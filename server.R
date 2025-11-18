# Servidor ---------------------------------
server <- function(session, input, output) {
  ## Objetos -------
  dimensoes <<- NULL
  imgData <<- NULL
  dados <<- NULL

  imgStore <- reactiveVal(NULL)

  tabela_parametros <- reactiveVal(NULL)

  coords <- reactiveVal(
    data.frame(
      ponto = integer(0),
      x = numeric(0),
      y = numeric(0),
      cor = character(0),
      cinza = numeric(0),
      outlier = character(0)
    )
  )

  dados_download <- reactiveVal(NULL)
  graficos_para_download <- reactiveVal(NULL)

  ## Lógica da primeira aba ----------------

  observeEvent(input$upload, {
    extensao <- tools::file_ext(input$upload$name)

    imagem_array <- switch(extensao,
      "jpg" = jpeg::readJPEG(input$upload$datapath),
      "jpeg" = jpeg::readJPEG(input$upload$datapath),
      "png" = png::readPNG(input$upload$datapath),
      {
        showNotification("Formato de imagem não suportado.", type = "error")
        return(NULL)
      }
    )

    imgStore(imagem_array) # Armazena imagem reativa

    coords(
      data.frame(
        ponto = integer(0),
        x = numeric(0),
        y = numeric(0),
        cor = character(0),
        cinza = numeric(0),
        outlier = character(0)
      )
    )
  })

  observeEvent(input$apagar_botao, {
    dados <- coords()
    if (nrow(dados) > 0) {
      dados <- dados[-nrow(dados), ]
      coords(dados)
    }
  })

  observeEvent(input$apagar_tudo, {
    coords(
      data.frame(
        ponto = integer(0),
        x = numeric(0),
        y = numeric(0),
        cor = character(0),
        cinza = numeric(0),
        outlier = character(0)
      )
    )

    imgData <<- NULL
    if (file.exists(input$upload$datapath)) {
      for (i in 1:7) {
        file_size <- file.info(input$upload$datapath)$size # Obtém o tamanho do arquivo em bytes
        writeBin(as.raw(runif(file_size, min = 0, max = 255)), input$upload$datapath)
      }
      file.remove(input$upload$datapath)
    }
    imgStore(NULL)
  })

  observeEvent(input$img_click, {
    click <- input$img_click

    # Validação de limites
    if (is.null(imgData)) {
      return()
    }

    largura_img <- dim(imgData)[2]
    altura_img <- dim(imgData)[1]

    if (click[1] < 1 || click[2] < 1 ||
      click[1] > largura_img || click[2] > altura_img) {
      showNotification("Clique fora da imagem válida.", type = "error")
      return()
    }

    current_rows <- nrow(coords())

    # Calcula o ponto no ciclo de 1-4
    point_in_cycle <- (current_rows %% 4) + 1

    # Extrair a cor
    clicked_color <- imgData[click[2], click[1], ]
    clicked_color_hex <- rgb(clicked_color[1], clicked_color[2], clicked_color[3], maxColorValue = 1)
    clicado_cinza <- mean(clicked_color)

    new_coords <- data.frame(ponto = point_in_cycle, x = click[1], y = click[2], cor = clicked_color_hex, cinza = clicado_cinza)

    # Determina se o ponto é um outlier em y
    if (nrow(coords()[coords()$ponto == new_coords$ponto, ]) > 5) {
      Q1y <- quantile(coords()[coords()$ponto == new_coords$ponto, ]$y, 0.25)
      Q3y <- quantile(coords()[coords()$ponto == new_coords$ponto, ]$y, 0.75)
      IQRy <- Q3y - Q1y

      Q1x <- quantile(coords()[coords()$ponto == new_coords$ponto, ]$x, 0.25)
      Q3x <- quantile(coords()[coords()$ponto == new_coords$ponto, ]$x, 0.75)
      IQRx <- Q3x - Q1x

      if ((click[2] < Q1y - 1.5 * IQRy || click[2] > Q3y + 1.5 * IQRy) ||
        (click[1] < Q1x - 1.5 * IQRx || click[1] > Q3x + 1.5 * IQRx)) {
        new_coords$outlier <- "Sim"
      } else {
        new_coords$outlier <- "Nao"
      }
    } else {
      new_coords$outlier <- "Calibracao"
    }
    dados <<- coords(rbind(coords(), new_coords))
    dados
  })

  output$coordsTxt <- renderText({
    dados <<- coords()
    dados$ponto <- factor(dados$ponto, levels = c(1, 2, 3, 4), labels = c("A", "B", "C", "D"))

    last_coord <- tail(dados, 1)
    if (nrow(last_coord) == 0) {
      return("-")
    } else {
      total_cliques <- nrow(dados)
      ciclos_completos <- floor(total_cliques / 4)

      paste0(
        "Dimensões:  ", paste(dimensoes, collapse = " x "), "\n",
        "Ciclos completos: ", ciclos_completos, " (", total_cliques, " cliques)", "\n",
        "Último ponto: Ponto ", last_coord$ponto,
        ", X: ", last_coord$x, " , Y: ", last_coord$y
      )
    }
  })

  ### Outputs ------------
  output$imgOutput <- renderUI({
    imagem <- imgStore()

    if (is.null(imagem)) {
      return(NULL)
    }
    imagem <- input$upload

    extensao <- tools::file_ext(input$upload$name)
    imgData <<- switch(extensao,
      "jpg" = jpeg::readJPEG(input$upload$datapath),
      "jpeg" = jpeg::readJPEG(input$upload$datapath),
      "png" = png::readPNG(input$upload$datapath),
      NULL
    )

    dimensoes_aux <<- dim(imgData)
    dimensoes <<- paste(dimensoes_aux[2], "x", dimensoes_aux[1])

    imgRaster <- base64enc::dataURI(file = imagem$datapath, mime = imagem$type)

    tags$div(
      style = "width: 100%; height: auto; overflow-x: auto; overflow-y: auto;",
      tags$img(
        src = imgRaster,
        id = "uploaded_img",
        style = paste0(
          "transform: scale(", input$zoom_nivel, ");",
          "transform-origin: top left; display: block; cursor: crosshair;"
        ),
        onclick = HTML("
        (function(event){
        const img = document.getElementById('uploaded_img');
        const rect = img.getBoundingClientRect();
        const zoom = parseFloat(getComputedStyle(img).transform.split(',')[0].replace('matrix(', ''));

        const realX = Math.round((event.clientX - rect.left) / zoom);
        const realY = Math.round((event.clientY - rect.top) / zoom);

        Shiny.setInputValue('img_click', [realX, realY], {priority: 'event'});
        })(event);
        ")
      )
    )
  })
  scatter_plot <- reactive({
    dados <<- coords()
    if (nrow(dados) == 0) {
      return(NULL)
    }

    dados$ponto <- factor(dados$ponto, levels = c(1, 2, 3, 4), labels = c("A", "B", "C", "D"))
    dados$y_mod <- dimensoes_aux[1] - dados$y

    imagem_grob <- grid::rasterGrob(imgStore(), width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

    xmin <- 0
    xmax <- dimensoes_aux[2]
    ymin <- 0
    ymax <- dimensoes_aux[1]

    aspect_ratio <- dimensoes_aux[1] / dimensoes_aux[2]

    # Plotar o gráfico com os valores de Y transformados
    p <- ggplot(dados, aes(x = x, y = y_mod)) +
      annotation_custom(imagem_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
      geom_smooth(formula = "y ~ x", method = "lm", se = FALSE, linetype = "dashed", color = "black", alpha = 0.5, aes(group = 1)) +
      geom_point(aes(shape = ponto, color = ponto), size = 7, alpha = 0.5) +
      labs(shape = "Pontos", color = "Pontos", x = "", y = "") +
      scale_shape_manual(values = c(16, 17, 18, 19)) +
      scale_color_manual(values = paleta) +
      xlim(xmin, xmax) +
      ylim(ymin, ymax) +
      theme_minimal(base_size = 13) +
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      theme(
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)
      ) +
      theme(aspect.ratio = aspect_ratio)

    p
  })
  output$scatter_plot <- renderPlot({
    scatter_plot()
  })

  output$regressaoTxt <- renderText({
    last_coord <- tail(coords(), 1)
    if (nrow(last_coord) == 0) {
      return("-")
    } else {
      dados <- coords() # Obtenha os dados do dataframe
      modelo <- lm(y ~ x, data = dados)
      eq <- paste(
        "Equação: y =",
        round(coef(modelo)[1], 5), " + (",
        round(coef(modelo)[2], 5), ")x"
      )
      r2 <- paste("R² =", round(summary(modelo)$r.squared, 5))
      aic <- paste("AIC =", round(AIC(modelo), 5))

      grupos <- split(dados, dados$ponto)

      resultados_mvn <- lapply(seq_along(grupos), function(i) {
        grupo <- grupos[[i]]
        letra <- LETTERS[i]

        mvn_result <- try(
          MVN::mvn(
            data = grupo[, c("x", "y")],
            mvn_test = "mardia"
          ),
          silent = TRUE
        ) |> withCallingHandlers(warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("Number of variables exceeds sample size", msg)) {
            invokeRestart("muffleWarning") # Suprime aviso de tamanho de amostra
          }
        })

        if (inherits(mvn_result, "try-error")) {
          return(paste0(letra, " - Erro no teste"))
        } else {
          skew <- mvn_result$multivariate_normality[1, ]
          kurt <- mvn_result$multivariate_normality[2, ]

          skew_p <- as.numeric(skew$p.value)
          kurt_p <- as.numeric(kurt$p.value)

          normal <- if (skew_p > 0.05 & kurt_p > 0.05) "✓ Normal" else "✗ Não normal"

          return(
            paste0(
              letra, " - ", normal,
              " (Skew = ", formatC(skew_p, digits = 3, format = "f"),
              "; Kurt = ", formatC(kurt_p, digits = 3, format = "f"), ")"
            )
          )
        }
      })

      paste(
        eq,
        r2,
        aic,
        "Teste de Mardia (por grupo):",
        paste(resultados_mvn, collapse = "\n"),
        sep = "\n"
      )
    }
  })

  # Download das coordenadas em CSV

  output$download_data <- downloadHandler(
    filename = function() {
      paste("pontos_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(coords())
      dados$ponto <- factor(
        dados$ponto,
        levels = c(1, 2, 3, 4),
        labels = c("A", "B", "C", "D")
      )
      write.csv2(dados, file, row.names = FALSE)
    }
  )

  output$pixel_coords <- renderTable(
    {
      dados <<- coords()
      dados$ponto <- factor(dados$ponto, levels = c(1, 2, 3, 4), labels = c("A", "B", "C", "D"))
      dados
    },
    rownames = FALSE
  )

  # ___________________________________________________________________

  ## Lógica da segunda aba --------------

  dataInput <- reactive({
    req(input$arq)
    inFile <- input$arq
    read.csv2(inFile$datapath, header = TRUE)
  })

  observeEvent(input$calcular_botao, {
    dados <- coords()

    # Se não tiver pontos marcados suficientes, tenta carregar o CSV
    if (is.null(dados) || nrow(dados) < 8) {
      dados_csv <- tryCatch(dataInput(), error = function(e) NULL)
      if (!is.null(dados_csv) && all(c("x", "y", "ponto") %in% names(dados_csv))) {
        dados <- dados_csv
      } else {
        showNotification("Dados ausentes. Marque pontos ou envie um CSV.", type = "error")
        return()
      }
    }

    # Garantindo formato
    dados$ponto <- tolower(dados$ponto) |>
      dplyr::recode(
        "a" = "1", "b" = "2", "c" = "3", "d" = "4"
      )

    dados_download(dados)

    withProgress(message = "Cálculo em progresso...", value = 0, {
      # Gráfico de dispersão

      # Gerar as repeticoes
      repeticoes_geradas <- gerar_repeticoes(dados, input$rep_mc)

      if (is.null(repeticoes_geradas) || nrow(repeticoes_geradas) == 0) {
        showNotification("Erro: não foi possível gerar repetições.", type = "error")
        return()
      }

      resultados <- repeticoes_geradas |>
        dplyr::group_by(serie) |>
        dplyr::group_map(
          ~ tryCatch(
            list(modelo = deming::deming(y ~ x, data = .x)),
            error = function(e) NULL
          ),
          .keep = TRUE
        )

      # Filtrar modelos válidos
      modelos_validos <- purrr::compact(resultados)

      if (length(modelos_validos) == 0) {
        showNotification("Erro: nenhum modelo Deming pôde ser ajustado.", type = "error")
        return()
      }

      # Extrair os coeficientes de cada regressao e salva-los em uma lista
      coeficientes <- purrr::map(modelos_validos, ~ tryCatch(
        coefficients(.x$modelo),
        error = function(e) NULL
      )) |> purrr::compact()

      if (length(coeficientes) == 0) {
        showNotification("Erro: Não foi possível extrair coeficientes dos modelos.", type = "error")
        return()
      }

      # Converter a lista em um data frame
      df_coeficientes <- tryCatch(
        do.call(rbind, coeficientes),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(df_coeficientes)) {
        return()
      }

      # Calcular a media e os intervalos de confianca dos coeficientes
      media_coeficientes <- colMeans(df_coeficientes)
      ic_coeficientes <- apply(df_coeficientes, 2, function(x) quantile(x, c(0.025, 0.975)))

      l <- input$dist_referencia / 1000000 # Distância de referência (km)

      # Media dos coeficientes
      intercepto <- media_coeficientes[1]
      inclinacao <- media_coeficientes[2]

      # Calcular o centro de gravidade por 'ponto'
      cg <<- repeticoes_geradas |>
        dplyr::group_by(ponto) |>
        dplyr::summarise(centro_gravidade_x = mean(x), centro_gravidade_y = mean(y))

      # Calcular o ponto na reta de regressao mais proximo do centro de gravidade
      centros_gravidade <- cg |>
        dplyr::mutate(
          projecao_x = (centro_gravidade_x + inclinacao * centro_gravidade_y - inclinacao * intercepto) / (1 + inclinacao^2),
          projecao_y = inclinacao * projecao_x + intercepto
        )

      # Rotacao
      angulo_de_rotacao <- -atan(inclinacao)
      repeticoes_geradas_rotacionadas <- rotacionar_pontos(repeticoes_geradas, angulo_de_rotacao)

      # Calcular a media dos valores de y apos a rotacao
      intercepto <- mean(repeticoes_geradas_rotacionadas$y)

      # A inclinacao e 0, posto que a linha e agora horizontal
      inclinacao <- 0

      centros_gravidade_rotacionados <- centros_gravidade[, c(1, 4, 5)]
      colnames(centros_gravidade_rotacionados) <- c("ponto", "x", "y")
      centros_gravidade_rotacionados[, 2:3] <- rotacionar_pontos(
        centros_gravidade_rotacionados[, 2:3],
        angulo_de_rotacao
      )

      # Rotular os grupos como A, B, C e D
      repeticoes_geradas_rotacionadas$ponto <- factor(repeticoes_geradas_rotacionadas$ponto,
        levels = 1:4,
        labels = c("A", "B", "C", "D")
      )

      # Fazer o mesmo para os centros de gravidade rotacionados
      centros_gravidade_rotacionados$ponto <- factor(centros_gravidade_rotacionados$ponto,
        levels = 1:4,
        labels = c("A", "B", "C", "D")
      )

      message("Processamento finalizado")
    })

    withProgress(message = "Carregando gráficos...", value = 0, {
      ### Outputs --------------
      #### Plotar o grafico de dispersao com cores identificando os grupos e series -----
      dispersaoCinza <- reactive({
        if (is.null(input$upload)) {
          showNotification("Nenhuma imagem carregada.", type = "warning")
          return(NULL)
        }

        imagem <- input$upload$datapath
        img <- tryCatch(
          imager::load.image(imagem),
          error = function(e) {
            showNotification("Erro ao carregar a imagem.", type = "error")
            return(NULL)
          }
        )
        if (is.null(img)) {
          return(NULL)
        }

        smoothed_img <- imager::isoblur(img, sigma = input$dp)


        temp_file <- tempfile(pattern = "aux", fileext = ".jpg")
        imager::save.image(smoothed_img, temp_file)

        if (!file.exists(temp_file)) {
          showNotification("Falha ao gerar imagem temporária.", type = "error")
          return(NULL)
        }

        imgData <<- tryCatch(
          as.array(jpeg::readJPEG(temp_file)),
          error = function(e) {
            showNotification("Erro ao ler imagem suavizada.", type = "error")
            return(NULL)
          }
        )
        unlink(temp_file)

        if (is.null(imgData)) {
          return(NULL)
        }

        dimensoes_aux <- dim(imgData)

        # Criando um data.frame para armazenar os resultados
        resultados <- data.frame(x = integer(), y = integer(), cinza = numeric())

        # Regressão
        regressao_deming <- deming::deming(y ~ x, data = dados)

        # Extraia os coeficientes
        beta_0 <- coef(regressao_deming)[1] # Intercepto
        beta_1 <- coef(regressao_deming)[2] # Inclinação

        # Loop sobre os pixels
        for (x_coord in 1:dimensoes_aux[2]) {
          # Prever o valor de y usando os coeficientes da regressão
          predito_y <- beta_0 + beta_1 * x_coord

          # Se o y previsto estiver dentro das dimensões da imagem, extrair o valor de cinza
          if (predito_y >= 1 & predito_y <= dimensoes_aux[1]) {
            # O valor de cinza pode ser extraído da matriz da imagem
            # Convertendo para cinza: utilizando a média dos 3 canais, se necessário
            cinza <- mean(imgData[predito_y, x_coord, 1:3])

            # Adicionando os resultados ao data.frame
            resultados <- rbind(resultados, data.frame(x = x_coord, y = round(predito_y), cinza = cinza))
          }
        }
        # Usando a inclinação para calcular o ângulo de rotação
        angulo <- -atan(beta_1)
        resultados_rotacionados <- rotacionar_pontos(resultados[, c(1, 2)], angulo)
        resultados$rot <- resultados_rotacionados$x

        cg_aux <- cg
        colnames(cg_aux) <- c("ponto", "x", "y")
        cg_aux <- rotacionar_pontos(cg_aux[, c(2, 3)], angulo)
        cg_aux <- cg_aux[, 1]
        colnames(cg_aux) <- "rot"
        cg_aux <- cg_aux |>
          dplyr::arrange(rot) |>
          dplyr::mutate(label = LETTERS[1:n()])

        spline_data <- smooth.spline(resultados$rot, resultados$cinza, cv = TRUE)

        p <- ggplot(resultados, aes(x = rot, y = cinza)) +
          geom_point(size = 0.3, color = "black") +
          geom_line(data = data.frame(x = spline_data$x, y = spline_data$y), aes(x = x, y = y), color = paleta[2]) +
          labs(
            x = "Abscissas rotacionadas",
            y = "Tom de cinza"
          ) +
          geom_vline(data = cg_aux, aes(xintercept = rot), linetype = "dashed", color = paleta[1]) +
          geom_text(data = cg_aux, aes(x = rot, label = label), vjust = 7, hjust = 1.5, size = 5, color = paleta[1]) +
          theme_minimal(base_size = 13)

        p
      })

      output$dispersaoCinza <- plotly::renderPlotly({
        req(dispersaoCinza())

        p <- dispersaoCinza()

        message("plot dispersao_cinza finalizado")
        p |>
          plotly::ggplotly() |>
          config_plotly()
      })

      #### Plotar o grafico de dispersao com cores identificando os grupos e series -------
      scatterPlot <- reactive({
        req(repeticoes_geradas_rotacionadas)
        p <- ggplot(repeticoes_geradas_rotacionadas, aes(x = x, y = y)) +
          geom_hex(bins = 120, aes(fill = ..count..)) +
          scale_fill_gradient(name = "Frequências", low = paleta[7], high = "black") +
          geom_abline(intercept = intercepto, slope = inclinacao, color = "black") +
          geom_point(data = centros_gravidade_rotacionados, aes(x = x, y = y), color = paleta[3], shape = 7, size = 1) +
          geom_text(
            data = centros_gravidade_rotacionados, aes(x = x, label = ponto),
            vjust = -0.25, hjust = 1.5, size = 5, color = paleta[1]
          ) +
          labs(x = "Abscissas rotacionadas", y = "Ordenadas rotacionadas") +
          theme_minimal(base_size = 13)
        p
      })

      output$scatterPlot <- plotly::renderPlotly({
        req(scatterPlot())
        p <- scatterPlot()

        message("scatterplot finalizado")
        p |>
          plotly::ggplotly() |>
          config_plotly()
      })

      #### Plot histograma -------
      histogramPlot <- reactive({
        req(repeticoes_geradas)
        # Histograma
        # Ordenar os dados por 'serie' e 'ponto'
        repeticoes_geradas <- repeticoes_geradas |>
          dplyr::arrange(serie, ponto)

        # Agrupar os dados por 'serie' e aplicar a funcao 'distancia' a cada grupo
        resultados <<- repeticoes_geradas |>
          dplyr::group_by(serie) |>
          dplyr::summarise(Ax = x[1], Ay = y[1], Bx = x[2], By = y[2], Cx = x[3], Cy = y[3], Dx = x[4], Dy = y[4]) |>
          dplyr::rowwise() |>
          dplyr::mutate(distancia = distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)) |>
          dplyr::ungroup()

        # Remove valores NA e calcula a velocidade
        dt <- input$fim_quadro - input$inicio_quadro # Tempo entre os frames (s)
        dt <- dt / 3600 # h

        # Proteção contra NULL nos inputs
        erro_medio_mt <- input$erro_medio_mt %||% 0
        dp_erro_medio_mt <- input$dp_erro_medio_mt %||% 0

        ruido <- tryCatch(
          {
            rnorm(1, mean = dt * erro_medio_mt, sd = abs(dt) * dp_erro_medio_mt) / 1000
          },
          error = function(e) {
            showNotification("Erro ao gerar ruído aleatório", type = "error")
            return(0)
          }
        )

        velocidade <- na.omit(resultados$distancia) / (dt + ruido)

        # Media
        media <- mean(velocidade)

        # Percentis
        percentis <- quantile(velocidade, c((1 - input$nc) / 2, 1 - (1 - input$nc) / 2))

        velocidade <- as.data.frame(velocidade)

        names(velocidade) <- "Velocidade"
        p1 <- ggplot(velocidade, aes(x = Velocidade)) +
          geom_histogram(binwidth = 0.5, color = "black", fill = paleta[7]) +
          geom_vline(aes(xintercept = media), color = paleta[6], linetype = "solid", linewidth = 0.5) +
          geom_vline(aes(xintercept = percentis[1]), color = paleta[1], linetype = "dashed", linewidth = 0.5) +
          geom_vline(aes(xintercept = percentis[2]), color = paleta[1], linetype = "dashed", linewidth = 0.5) +
          labs(x = "Velocidade (km/h)", y = "Frequência") +
          theme_minimal(base_size = 13)

        y_max <- max(ggplot_build(p1)$data[[1]]$count, na.rm = TRUE) * 1.05

        x_gap <- range(c(media, percentis), na.rm = TRUE) |>
          diff() * 0.04

        y_gap <- range(c(
          media,
          (1 - input$nc) / 2,
          1 - (1 - input$nc) / 2
        ), na.rm = TRUE) |>
          diff() * 0.04

        p1 <- p1 +
          annotate("text",
            x = media + x_gap, y = y_max, label = round(media, 1),
            vjust = 0, color = paleta[6], size = 5
          ) +
          annotate("text",
            x = percentis[1] + x_gap, y = y_max, label = round(percentis[1], 1),
            vjust = 0, color = paleta[1], size = 5
          ) +
          annotate("text",
            x = percentis[2] + x_gap, y = y_max, label = round(percentis[2], 1),
            vjust = 0, color = paleta[1], size = 5
          )

        # Removendo o eixo x do histograma
        p1 <- p1 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

        # Criando o boxplot
        p2 <- ggplot(velocidade, aes(x = 1, y = Velocidade)) +
          geom_boxplot() +
          labs(x = "Frequência", y = "Velocidade (km/h)") +
          theme_minimal(base_size = 13) +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.title.y = element_blank()
          ) +
          coord_flip()

        list(histograma = p1, boxplot = p2)
      })

      output$histogramPlot <- plotly::renderPlotly({
        req(histogramPlot())
        p1 <- histogramPlot()$histograma
        p2 <- histogramPlot()$boxplot
        # Organizando os dois graficos em um unico plot
        # p <- plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", rel_heights = c(6 / 7, 1 / 7))

        g1 <- plotly::ggplotly(p1)
        g2 <- plotly::ggplotly(p2)

        message("plot de histograma finalizado")
        plotly::subplot(g1, g2, nrows = 2, shareX = TRUE, heights = c(0.8, 0.2), titleY = TRUE) |>
          config_plotly()
      })

      #### Plot histogramaDistancia ---------
      histogramDistancia <- reactive({
        req(resultados)
        deslocamento <- as.data.frame(resultados$distancia * 1000)
        colnames(deslocamento) <- "d"

        # Media
        media <- mean(deslocamento$d, na.rm = TRUE)

        # ECDF
        ecdf_fun <- ecdf(deslocamento$d)

        # Percentil da média
        percentil_media <- ecdf_fun(media)

        # Percentis
        percentil <- quantile(deslocamento$d,
          c((1 - input$nc) / 2, 1 - (1 - input$nc) / 2),
          na.rm = TRUE
        )

        p <- ggplot(deslocamento, aes(x = d)) +
          stat_ecdf(geom = "line", colour = paleta[2], size = 1) +
          geom_hline(
            yintercept = percentil_media,
            colour = paleta[6],
            linetype = "solid",
            size = 0.5
          ) +
          geom_hline(
            yintercept = (1 - input$nc) / 2,
            colour = paleta[1],
            linetype = "dashed",
            size = 0.5
          ) +
          geom_hline(
            yintercept = 1 - (1 - input$nc) / 2,
            colour = paleta[1],
            linetype = "dashed",
            size = 0.5
          ) +
          labs(
            # title = "Ogiva de Galton do deslocamento",
            x = "Deslocamento (m)",
            y = "Frequência Acumulada"
          ) +
          theme_minimal(base_size = 13)

        x_gap <- range(c(media, percentil), na.rm = TRUE) |>
          diff() * 0.04

        y_gap <- range(c(
          percentil_media,
          (1 - input$nc) / 2,
          1 - (1 - input$nc) / 2
        ), na.rm = TRUE) |>
          diff() * 0.04

        p <- p + annotate("text",
          x = percentil[1],
          y = (1 - input$nc) / 2 - y_gap,
          label = sprintf("%.2f", percentil[1]),
          vjust = 0,
          hjust = 0,
          size = 5,
          colour = paleta[1]
        )

        p <- p + annotate("text",
          x = media - x_gap,
          y = percentil_media + y_gap,
          label = sprintf("%.2f", media),
          vjust = 0,
          hjust = 1,
          size = 5,
          colour = paleta[6]
        )

        p <- p + annotate("text",
          x = percentil[2],
          y = 1 - (1 - input$nc) / 2 + y_gap,
          label = sprintf("%.2f", percentil[2]),
          vjust = 0,
          hjust = 1,
          size = 5,
          colour = paleta[1]
        )
        p
      })

      output$histogramDistancia <- plotly::renderPlotly({
        req(histogramDistancia())
        p <- histogramDistancia()

        message("plot histogram_distance finalizado")
        p |>
          plotly::ggplotly() |>
          config_plotly()
      })

      tabela_parametros(tibble::tibble(
        parametro = c(
          "dp_filtro_gaussiano", "inicio_quadro_tempo", "fim_quadro_tempo",
          "erro_medio_tempo", "dp_erro_medio_tempo", "dist_referencia",
          "repeticoes_mc", "nivel_confianca"
        ),
        valor = c(
          input$dp,
          input$inicio_quadro,
          input$fim_quadro,
          input$erro_medio_mt,
          input$dp_erro_medio_mt,
          input$dist_referencia,
          input$rep_mc,
          input$nc
        )
      ))

      ## Exportar gráficos -----
      # Compor o gráfico histograma + boxplot

      hist_obj <- tryCatch(histogramPlot(), error = function(e) NULL)

      hist_p1 <- NULL
      hist_p2 <- NULL
      histograma_boxplot <- NULL

      if (!is.null(hist_obj) &&
        all(c("histograma", "boxplot") %in% names(hist_obj))) {
        hist_p1 <- tema_graficos_export(hist_obj$histograma, "Velocidade estimada") +
          theme(axis.title.x = element_blank())
        hist_p2 <- hist_obj$boxplot

        if (!is.null(hist_p1) && !is.null(hist_p2)) {
          histograma_boxplot <- cowplot::plot_grid(
            hist_p1, hist_p2,
            ncol = 1, align = "v", axis = "l", rel_heights = c(6 / 7, 1 / 7)
          )
        }
      }

      # Lista de gráficos com nome e objeto ggplot2
      graficos_export <- list(
        list(
          nome_arquivo = "1_pontos_marcados.png",
          grafico = tema_graficos_export(scatter_plot(), "Pontos marcados")
        ),
        list(
          nome_arquivo = "2_tom_de_cinza.png",
          grafico = tema_graficos_export(dispersaoCinza(), "Tom de cinza ao longo da trajetória")
        ),
        list(
          nome_arquivo = "3_densidade_pontos_simulados.png",
          grafico = tema_graficos_export(scatterPlot(), "Densidade dos pontos simulados")
        ),
        list(
          nome_arquivo = "4_velocidade_estimada.png",
          grafico = histograma_boxplot
        ),
        list(
          nome_arquivo = "5_deslocamento_estimado.png",
          grafico = tema_graficos_export(histogramDistancia(), "Deslocamento estimado")
        )
      )

      # Remove gráficos que falharam
      graficos_export <- purrr::keep(graficos_export, ~ !is.null(.x$grafico))

      graficos_para_download(graficos_export)
    })
  })

  ## Botão de exportação -----
  output$baixar_graficos <- downloadHandler(
    filename = function() {
      paste0("graficos_", Sys.Date(), ".zip")
    },
    content = function(file) {
      dir <- tempfile()
      fs::dir_create(dir)

      # Salvar os gráficos
      purrr::walk(graficos_para_download(), function(g) {
        caminho <- file.path(dir, g$nome_arquivo)
        ragg::agg_png(caminho, width = 20, height = 13, units = "cm", res = 200)
        print(g$grafico)
        grDevices::dev.off()
      })

      # Salvar a imagem original, se disponível
      if (!is.null(imgData)) {
        img <- imgData
        caminho_img <- file.path(dir, "imagem_original.png")

        # Converter array para imagem e salvar
        ragg::agg_png(caminho_img, width = dim(img)[2], height = dim(img)[1], res = 72)
        grid::grid.raster(img)
        grDevices::dev.off()
      }

      # Salvar a tabela CSV, se disponível
      if (!is.null(dados_download())) {
        tabela <- dados_download()
        caminho_csv <- file.path(dir, paste0("pontos_", Sys.Date(), ".csv"))
        readr::write_csv2(tabela, caminho_csv)
      }

      # Salvar a tabela de parâmetros utilizados
      if (!is.null(tabela_parametros())) {
        caminho_csv <- file.path(dir, paste0("parametros_utilizados_", Sys.Date(), ".csv"))
        readr::write_csv2(tabela_parametros(), caminho_csv)
      }

      # Compactar tudo
      zip::zipr(
        zipfile = file,
        files = fs::dir_ls(dir),
        root = dir
      )

      showNotification("Exportação concluída", type = "message", duration = 10)
    }
  )
}
