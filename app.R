library(base64enc)
library(shiny)
library(shinydashboard)
library(cowplot)
library(MASS)
library(deming)
library(dplyr)
library(ggplot2)

# Defina a UI
ui <- dashboardPage(
  dashboardHeader(title = "GPAV/POLITEC/MT"),
  dashboardSidebar(
    # Barra lateral com abas recolhíveis e dispositivos de entrada
    sidebarMenu(
      id = "tabs",
      menuItem("Coordenadas", tabName = "app1", icon = icon("dashboard")),
      menuItem("Intervalo de confiança", tabName = "app2", icon = icon("bar-chart")),
      tags$hr()
    ),
    
    # Elementos de entrada para a primeira aba
    conditionalPanel(
      condition = "input.tabs === 'app1'",
      fileInput("upload", "Escolha uma Imagem", 
                buttonLabel = "Selecione...",
                placeholder = "Nenhum arquivo selecionado",
                accept = c("image/png", "image/jpeg")),
      sliderInput("zoom_level", "Nível de Zoom", min = 1, max = 7, value = 1, step = 0.5)
    ),
    
    # Elementos de entrada para a segunda aba
    conditionalPanel(
      condition = "input.tabs === 'app2'",
      fileInput("arq", "Arquivo com coordenadas dos pontos:",
                buttonLabel = "Selecione...",
                placeholder = "Nenhum arquivo selecionado",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      numericInput("inicio_quadro", "Marcação temporal do quadro inicial (s):", value = 42.02, min = 0),
      numericInput("fim_quadro", "Marcação temporal do quadro final (s):", value = 44.118, min = 0),
      numericInput("dist_referencia", "Distância de referência (mm):", value = 1894, min = 0),
      tags$hr(),
      numericInput("rep_mc", "Número de repetições pelo MMC:", value = 100, min = 1, max = 10000),
      numericInput("nc", "Nível de confiança:", value = 0.99, min = 0.00001, max = 0.99999),
      actionButton("calcular_botao", "Calcular!")
    )
  ),
  dashboardBody(
    # Conteúdo das saídas
    tabItems(
      tabItem(
        tabName = "app1",
        fluidRow(
          uiOutput("imgOutput"),
          verbatimTextOutput("coordsTxt"),
          plotOutput("scatter_plot"),
          tableOutput("pixel_coords"),
          downloadButton("download_data", "Download das Coordenadas")
        )
      ),
      tabItem(
        tabName = "app2",
        fluidRow(
          plotOutput("scatterPlot"),
          plotOutput("histogramPlot"),
          plotOutput("histogramDistancia")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Gera repeticoes usando uma distribuicao normal bivariada

gerar_repeticoes <- function(dados_originais, repeticoes) {
  grupos <- unique(dados_originais$ponto)  # Identificar os grupos de pontos
  repeticoes_geradas <- list()  # Lista para armazenar as repeticoes
  
  for (grupo in grupos) {
    dados_grupo <- dados_originais[dados_originais$ponto == grupo, ]  # Dados do grupo atual
    media <- colMeans(dados_grupo[, c("x", "y")])  # Media do grupo atual
    matriz_cov <- cov(dados_grupo[, c("x", "y")])  # Matriz de covariancia do grupo atual
    
    repeticoes_grupo <- lapply(1:repeticoes, function(i) {
      repeticao <- mvrnorm(1, mu = media, Sigma = matriz_cov)
      data.frame(serie = i, ponto = grupo, x = repeticao[1], y = repeticao[2])
    })
    
    repeticoes_geradas <- c(repeticoes_geradas, repeticoes_grupo)
  }
  # Junta todas as repeticoes em um unico data frame
  repeticoes_geradas <- do.call(rbind, repeticoes_geradas)  
  
  return(repeticoes_geradas)
}

# ------------------------------------------------------------------------------

# Calculo da distancia
distancia <- function(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l) {
  # Calcular as distancias euclidianas
  dAC <- sqrt((Ax - Cx)^2 + (Ay - Cy)^2)
  dBC <- sqrt((Bx - Cx)^2 + (By - Cy)^2)
  dAD <- sqrt((Ax - Dx)^2 + (Ay - Dy)^2)
  dBD <- sqrt((Bx - Dx)^2 + (By - Dy)^2)
  
  k <- (dAC/dBC)/(dAD/dBD)
  
  d <- (k/(k-1)*l^2)^0.5
  return(d)
} 

# ------------------------------------------------------------------------------

rotacionar_pontos <- function(df, angulo) {
  df_rotacionado <- df
  df_rotacionado$x <- df$x * cos(angulo) - df$y * sin(angulo)
  df_rotacionado$y <- df$x * sin(angulo) + df$y * cos(angulo)
  return(df_rotacionado)
}


# Defina o servidor
server <- function(session, input, output) {
  
  # Lógica da primeira aplicação Shiny
  coords <- reactiveVal(data.frame(ponto = integer(0), x = numeric(0), y = numeric(0), outlier = character(0)))
  
  observeEvent(input$reset_btn, {
    coords(data.frame(ponto = integer(0), x = numeric(0), y = numeric(0), outlier = character(0)))
    reset("upload")
  })
  
  observeEvent(input$zoom_level, {
    # Resetar dados e gráficos quando o nível de zoom for alterado
    coords(data.frame(ponto = integer(0), x = numeric(0), y = numeric(0), outlier = character(0)))
  })
  
  output$imgOutput <- renderUI({
    imagem <- input$upload
    if (is.null(imagem)) return(NULL)
    
    imgData <- dataURI(file = imagem$datapath, mime = imagem$type)
    tags$img(src = imgData, id = "uploaded_img", 
             width = paste0(500 * input$zoom_level, "px"), 
             onclick = "Shiny.setInputValue('img_click', [event.offsetX, event.offsetY], {priority: 'event'});")
  })
  
  observeEvent(input$img_click, {
    click <- input$img_click
    
    current_rows <- nrow(coords())
    
    # Calcula o ponto no ciclo de 1-4
    point_in_cycle <- (current_rows %% 4) + 1
    
    new_coords <- data.frame(ponto = point_in_cycle, x = click[1], y = click[2])
    
    # Determina se o ponto é um outlier em y
    if (nrow(coords()[coords()$ponto==new_coords$ponto,]) > 5) {
      Q1y <- quantile(coords()[coords()$ponto==new_coords$ponto,]$y, 0.25)
      Q3y <- quantile(coords()[coords()$ponto==new_coords$ponto,]$y, 0.75)
      IQRy <- Q3y - Q1y
      
      Q1x <- quantile(coords()[coords()$ponto==new_coords$ponto,]$x, 0.25)
      Q3x <- quantile(coords()[coords()$ponto==new_coords$ponto,]$x, 0.75)
      IQRx <- Q3x - Q1x
      
      if ((click[2] < Q1y - 1.5 * IQRy || click[2] > Q3y + 1.5 * IQRy) || 
          (click[1] < Q1x - 1.5 * IQRx || click[1] > Q3x + 1.5 * IQRx)) {
        new_coords$outlier <- "Sim"
      } else {
        new_coords$outlier <- "Não"
      }
      
    } else {
      new_coords$outlier <- "Calibração"
    }
    dados <<- coords(rbind(coords(), new_coords))
    dados
  })
  
  output$coordsTxt <- renderText({
    last_coord <- tail(coords(), 1)
    if(nrow(last_coord) == 0) return("Nenhum ponto clicado ainda.")
    paste("Ponto", last_coord$ponto, "- X:", last_coord$x, "Y:", last_coord$y)
  })
  
  output$scatter_plot <- renderPlot({
    dados <<- coords()
    if (nrow(dados) == 0) return(NULL)
    ggplot(dados, aes(x = x, y = y, color = as.factor(ponto))) +
      geom_point(size = 4) +
      geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
      labs(color = "Ponto") +
      scale_color_manual(values = c("red", "blue", "green", "purple")) +
      scale_y_reverse()  # Inverte o sentido do eixo y
  })
  
  # Download das coordenadas em CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("dados-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(coords(), file, row.names = FALSE)
    }
  )
  
  output$pixel_coords <- renderTable({
    coords()
  }, rownames = TRUE)
  
  # ------------------------------------------------------------------------------  

  # Lógica da segunda aplicação Shiny
  
  dataInput <- reactive({
    inFile <- input$arq
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = TRUE)
  })
  
  observeEvent(input$calcular_botao, {
    if (!exists('dados') || (nrow(dados)<5)) {
      dados <- dataInput()
      if (is.null(dados) || (nrow(dados)<5)) {
        return(NULL)
      } else if (!("x" %in% names(dados)) | !("y" %in% names(dados))) {
        return(print(dados))
      }}
    
    

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Cálculo em progresso:',
                 detail = 'aguarde um momento...')
    
    # Gráfico de dispersão
    
    # Gerar as repeticoes
    repeticoes_geradas <- gerar_repeticoes(dados, input$rep_mc)
    
    # Agrupar os dados por 'serie' e aplicar a regressao Deming a cada grupo
    resultados <- repeticoes_geradas %>% 
      group_by(serie) %>%
      do(modelo = deming(y ~ x, data = .))
    
    # Extrair os coeficientes de cada regressao e salva-los em uma lista
    coeficientes <- lapply(resultados$modelo, coefficients)
    
    # Converter a lista em um data frame
    df_coeficientes <- do.call(rbind, coeficientes)
    
    # Calcular a media e os intervalos de confianca dos coeficientes
    media_coeficientes <- colMeans(df_coeficientes)
    ic_coeficientes <- apply(df_coeficientes, 2, function(x) quantile(x, c(0.025, 0.975)))
    
    l <- input$dist_referencia/1000000 # Distância de referência (km)
    
    # Media dos coeficientes
    intercepto <- media_coeficientes[1]
    inclinacao <- media_coeficientes[2]
    
    # Calcular o centro de gravidade por 'ponto'
    centros_gravidade <- repeticoes_geradas %>%
      group_by(ponto) %>%
      summarise(centro_gravidade_x = mean(x), centro_gravidade_y = mean(y))
    
    
    # Calcular o ponto na reta de regressao mais proximo do centro de gravidade
    centros_gravidade <- centros_gravidade %>%
      mutate(
        projecao_x = (centro_gravidade_x + inclinacao * centro_gravidade_y - inclinacao * intercepto) / (1 + inclinacao ^ 2),
        projecao_y = inclinacao * projecao_x + intercepto
      )
    
    # Rotacao
    angulo_de_rotacao <- -atan(inclinacao)
    repeticoes_geradas_rotacionadas <- rotacionar_pontos(repeticoes_geradas, angulo_de_rotacao)
    
    # Calcular a media dos valores de y apos a rotacao
    intercepto <- mean(repeticoes_geradas_rotacionadas$y)
    
    # A inclinacao e 0, posto que a linha e agora horizontal
    inclinacao <- 0
    
    centros_gravidade_rotacionados <- centros_gravidade[,c(1,4,5)]
    colnames(centros_gravidade_rotacionados) <- c('ponto', 'x', 'y')
    centros_gravidade_rotacionados[,2:3] <- rotacionar_pontos(centros_gravidade_rotacionados[,2:3], 
                                                              angulo_de_rotacao)
    
    # Rotular os grupos como A, B, C e D
    repeticoes_geradas_rotacionadas$ponto <- factor(repeticoes_geradas_rotacionadas$ponto, 
                                                    levels = 1:4, 
                                                    labels = c("A", "B", "C", "D"))
    
    # Fazer o mesmo para os centros de gravidade rotacionados
    centros_gravidade_rotacionados$ponto <- factor(centros_gravidade_rotacionados$ponto, 
                                                   levels = 1:4, 
                                                   labels = c("A", "B", "C", "D"))
    
    # Plotar o grafico de dispersao com cores identificando os grupos e series
    output$scatterPlot <- renderPlot({
      ggplot(repeticoes_geradas_rotacionadas, aes(x = x, y = y)) +
        geom_hex(bins = 120, aes(fill = ..count..)) +
        scale_fill_gradient(name = "Frequencias", low = "gray", high = "black") +
        geom_abline(intercept = intercepto, slope = inclinacao, color = "black") +
        geom_point(data = centros_gravidade_rotacionados, aes(x = x, y = y), color = "lightblue", shape = 7, size = 1) +
        geom_text(data = centros_gravidade_rotacionados, aes(x = x, label = ponto), 
                  vjust = -0.25, hjust = 1.5, color = "black") +
        labs(x = "Coordenada X", y = "Coordenada Y")
    })
    
    output$histogramPlot <- renderPlot({
      # Histograma
      # Ordenar os dados por 'serie' e 'ponto'
      repeticoes_geradas <- repeticoes_geradas %>%
        arrange(serie, ponto)
      
      # Agrupar os dados por 'serie' e aplicar a funcao 'distancia' a cada grupo
      resultados <<- repeticoes_geradas %>%
        group_by(serie) %>%
        summarise(Ax = x[1], Ay = y[1], Bx = x[2], By = y[2], Cx = x[3], Cy = y[3], Dx = x[4], Dy = y[4]) %>%
        rowwise() %>%
        mutate(distancia = distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)) %>%
        ungroup()
      
      # Remove valores NA e calcula a velocidade
      dt <-  input$fim_quadro - input$inicio_quadro # Tempo entre os frames (s)
      dt <-  dt/3600 # h
      velocidade <- na.omit(resultados$distancia)/dt
      
      # Media
      media <- mean(velocidade)
      
      # Percentis
      percentis <- quantile(velocidade, c((1-input$nc)/2, 1-(1-input$nc)/2))
      
      velocidade <- as.data.frame(velocidade)
      names(velocidade) <- "v"
      p1 <- ggplot(velocidade, aes(x = v)) +
        geom_histogram(binwidth = 0.5, color = "black", fill = "lightgray") +
        geom_vline(aes(xintercept = media), color = "blue", linetype = "dashed", linewidth = 0.5) +
        geom_vline(aes(xintercept = percentis[1]), color = "red", linetype = "dashed", linewidth = 0.5) +
        geom_vline(aes(xintercept = percentis[2]), color = "red", linetype = "dashed", linewidth = 0.5) +
        annotate("text", x = media, y = Inf, label = paste(round(media, 1)), vjust = 2, color = "black", size = 4) +
        annotate("text", x = percentis[1], y = Inf, label = paste(round(percentis[1],1)), vjust = 3, color = "black", size = 4) +
        annotate("text", x = percentis[2], y = Inf, label = paste(round(percentis[2],1)), vjust = 3, color = "black", size = 4) +
        labs(x = "Velocidade (km/h)", y = "Frequencia") +
        theme_minimal()
      
      # Removendo o eixo x do histograma
      p1 <- p1 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      # Criando o boxplot
      p2 <- ggplot(velocidade, aes(x = v, y = 1)) +
        geom_boxplot() +
        labs(x = "Velocidade (km/h)", y = "Frequencia") +
        theme_minimal() +
        theme(axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line.y = element_blank(),
              axis.line.x = element_blank(),
              axis.title.y = element_blank())
      
      # Organizando os dois graficos em um unico plot
      p <- plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", rel_heights = c(6/7, 1/7))
      p
      
    })
  
  
  output$histogramDistancia <- renderPlot({
    deslocamento <- as.data.frame(resultados$distancia*1000)
    colnames(deslocamento) <- "d"
    
    # Media
    media <- mean(deslocamento$d, na.rm = TRUE)
    
    # ECDF
    ecdf_fun <- ecdf(deslocamento$d)
    
    # Percentil da média
    percentil_media <- ecdf_fun(media)
    
    # Percentis
    percentil <- quantile(deslocamento$d, 
                          c((1-input$nc)/2, 1-(1-input$nc)/2),
                          na.rm = TRUE)
    
    p <- ggplot(deslocamento, aes(x = d)) +
      stat_ecdf(geom = "line", colour = "blue", size = 1) +
      geom_hline(yintercept = percentil_media, 
                 colour = "green", 
                 linetype = "dashed", 
                 size = 0.5) +
      geom_hline(yintercept = (1-input$nc)/2, 
                 colour = "red", 
                 linetype = "dashed", 
                 size = 0.5) +
      geom_hline(yintercept = 1-(1-input$nc)/2, 
                 colour = "red", 
                 linetype = "dashed", 
                 size = 0.5) +
      labs(title = "Ogiva de Galton do deslocamento",
           x = "Deslocamento (m)",
           y = "Frequência Cumulativa") +
      theme_minimal()
    
    p <- p + geom_text(aes(x = percentil[1], y = (1-input$nc)/2, 
                           label = sprintf("%.2f", percentil[1])), 
                       vjust = 0,
                       hjust = 1)
    
    p <- p + geom_text(aes(x = media, y = percentil_media, 
                           label = sprintf("%.2f", media)), 
                       vjust = 0,
                       hjust = 1)
    
    p <- p + geom_text(aes(x = percentil[2], y = 1-(1-input$nc)/2, 
                           label = sprintf("%.2f", percentil[2])), 
                       vjust = 0,
                       hjust = 1)
    p
    
  })
  
  })
}

# Execute a aplicação Shiny
shinyApp(ui, server)
