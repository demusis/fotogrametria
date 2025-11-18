# UI ---------------------
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "POLITEC/MT", .list = tagList(
    tags$li(
      class = "nav-item dropdown",
      tags$a(
        class = "nav-link",
        href = "https://github.com/demusis/fotogrametria",
        icon("github", class = "fab fa-lg"),
        target = "_blank"
      )
    )
  )),
  shinydashboard::dashboardSidebar(
    ## Barra lateral -----
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Coordenadas", tabName = "app1", icon = icon("images")),
      shinydashboard::menuItem("Processamento", tabName = "app2", icon = icon("bar-chart")),
      tags$hr()
    ),

    ### Elementos de entrada para a primeira aba --------
    conditionalPanel(
      condition = "input.tabs === 'app1'",
      input_com_ajuda(
        input_id = "upload",
        label_text = "Escolha uma imagem:",
        input_ui = fileInput(
          inputId = "upload",
          label = NULL,
          buttonLabel = "Selecione...",
          placeholder = "Nenhum arquivo selecionado",
          accept = c("image/jpeg", "image/png")
        ),
        ajuda_id = "ajuda_upload",
        ajuda_titulo = NULL,
        ajuda_texto = "Carregue uma imagem (<b>JPG</b> ou <b>PNG</b>) que contenha <b>dois quadros</b> (frames/momentos) da trajetória que deseja analisar."
      ),
      tags$div(
        style = "display: flex; align-items: center;",
        class = "form-group shiny-input-container",
        tags$label("Nível de zoom:", class = "control-label")
      ),
      sliderInput(
        inputId = "zoom_nivel",
        label = NULL,
        min = 0.1,
        max = 3,
        value = 1,
        step = 0.1
      ),
      tags$hr(),
      tags$div(
        style = "display: flex; align-items: center;",
        class = "form-group shiny-input-container",
        tags$label("Gerenciar marcações:", class = "control-label")
      ),
      actionButton("apagar_botao", "Apagar último ponto"),
      actionButton("apagar_tudo", "Apagar tudo")
    ),

    ### Elementos de entrada para a segunda aba ----------
    conditionalPanel(
      condition = "input.tabs === 'app2'",
      input_com_ajuda(
        input_id = "dp",
        label_text = "Filtro gaussiano isotrópico, desvio padrão:",
        input_ui = sliderInput("dp", NULL, min = 0, max = 100, value = 0, step = 1),
        ajuda_id = "ajuda_filtro",
        ajuda_titulo = NULL,
        ajuda_texto = "Aplica suavização à imagem antes do cálculo da linha de regressão, reduzindo ruídos locais."
      ),
      tags$hr(),
      input_com_ajuda(
        input_id = "arq",
        label_text = "Arquivo com coordenadas (opcional):",
        input_ui = fileInput("arq", NULL, buttonLabel = "Selecione...", placeholder = "Nenhum arquivo selecionado", accept = ".csv"),
        ajuda_id = "ajuda_arquivo",
        ajuda_titulo = NULL,
        ajuda_texto = "Se você não marcou os pontos na aba anterior, pode carregar um arquivo .CSV com as coordenadas."
      ),
      input_com_ajuda(
        input_id = "inicio_quadro",
        label_text = "Marcação temporal do quadro inicial (s):",
        input_ui = numericInput("inicio_quadro", NULL, value = 1.266, min = 0, step = 0.00001),
        ajuda_id = "ajuda_inicio",
        ajuda_titulo = NULL,
        ajuda_texto = "Tempo do primeiro frame utilizado na estimativa de velocidade."
      ),
      input_com_ajuda(
        input_id = "fim_quadro",
        label_text = "Marcação temporal do quadro final (s):",
        input_ui = numericInput("fim_quadro", NULL, value = 1.866, min = 0, step = 0.00001),
        ajuda_id = "ajuda_fim",
        ajuda_titulo = NULL,
        ajuda_texto = "Tempo do segundo frame utilizado na estimativa de velocidade."
      ),
      input_com_ajuda(
        input_id = "erro_medio_mt",
        label_text = "Erro médio da marcação temporal (ms/s):",
        input_ui = numericInput("erro_medio_mt", NULL, value = 0.881, min = 0.01, max = 5, step = 0.00001),
        ajuda_id = "ajuda_erro_medio",
        ajuda_titulo = NULL,
        ajuda_texto = "Erro médio (em milissegundos por segundo) estimado na marcação dos tempos."
      ),
      input_com_ajuda(
        input_id = "dp_erro_medio_mt",
        label_text = "DP do erro médio da marcação temporal (ms/s):",
        input_ui = numericInput("dp_erro_medio_mt", NULL, value = 0.287, min = 0.01, max = 5, step = 0.00001),
        ajuda_id = "ajuda_dp_erro",
        ajuda_titulo = NULL,
        ajuda_texto = "Desvio padrão do erro médio informado, para simulação de incertezas."
      ),
      input_com_ajuda(
        input_id = "dist_referencia",
        label_text = "Distância de referência (mm):",
        input_ui = numericInput("dist_referencia", NULL, value = 2002, min = 0.01, step = 0.00001),
        ajuda_id = "ajuda_dist",
        ajuda_titulo = NULL,
        ajuda_texto = "Distância real conhecida entre os centros das rodas do veículo."
      ),
      tags$hr(),
      input_com_ajuda(
        input_id = "rep_mc",
        label_text = "Número de repetições pelo MMC:",
        input_ui = numericInput("rep_mc", NULL, value = 100, min = 1, max = 10000, step = 1),
        ajuda_id = "ajuda_rep",
        ajuda_titulo = NULL,
        ajuda_texto = "Número de simulações de Monte Carlo realizadas para estimar a velocidade média e os intervalos de confiança."
      ),
      input_com_ajuda(
        input_id = "nc",
        label_text = "Nível de confiança:",
        input_ui = numericInput("nc", NULL, value = 0.99, min = 0.00001, max = 0.99999, step = 0.00001),
        ajuda_id = "ajuda_nc",
        ajuda_titulo = NULL,
        ajuda_texto = "Nível de confiança para os intervalos gerados da velocidade média estimada."
      ),
      actionButton("calcular_botao", "Calcular!"),
      tags$hr(),
      downloadButton("baixar_graficos", "Baixar gráficos e dados (.zip)")
    )
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(HTML("
      .btn.btn-default.shiny-download-link.shiny-bound-output {
        margin: 6px 5px 6px 15px;
        color: #444;
      }
    "))
    ),
    ## Conteúdo das saídas -----
    shinydashboard::tabItems(
      ### Página 1 -------
      shinydashboard::tabItem(
        tabName = "app1",
        fluidRow(
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$h4("Imagem enviada"),
            shinyBS::bsButton("ajuda_marcacao", label = NULL, icon = icon("info-circle"), size = "extra-small")
          )
        ),
        fluidRow(
          uiOutput("imgOutput")
        ),
        fluidRow(
          verbatimTextOutput("coordsTxt")
        ),
        fluidRow(
          # Resumo das estatísticas de regressão
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$h4("Resumo da Regressão"),
            shinyBS::bsButton("ajuda_regressao", label = NULL, icon = icon("info-circle"), size = "extra-small")
          ),
          verbatimTextOutput("regressaoTxt"),
          # Marcações feitas
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$h4("Pontos marcados")
          ),
          plotOutput("scatter_plot"),
          tableOutput("pixel_coords"),
          downloadButton("download_data", "Download das Coordenadas"),

          #### Botões de ajuda ------------
          shinyBS::bsPopover(
            id = "ajuda_regressao",
            title = "Interpretação dos resultados",
            content = paste0(
              "<b>Equação:</b> Ajuste linear ortogonal entre os pontos marcados (y = a + bx).<br/>",
              "<b>R²:</b> Mede o quanto a variação dos dados é explicada pela reta (0 a 1).<br/>",
              "<b>AIC:</b> Critério de informação – menor valor indica melhor ajuste.<br/><br/>",
              "<b>Teste de Mardia:</b> Verifica se os dados seguem distribuição normal bivariada:<br/>",
              "✓ Normal = não há evidência contra a normalidade (p > 0.05).<br/>",
              "✗ Não normal = evidência contra a normalidade (p ≤ 0.05) em assimetria (skew) ou curtose (kurt).<br/><br/>",
              "<b>É necessário que os dados sejam normais</b> para os pressupostos da segunda aba de cálculos."
            ),
            placement = "right",
            trigger = "hover",
            options = list(container = "body", trigger = "hover focus")
          ),
          shinyBS::bsPopover(
            id = "ajuda_marcacao",
            title = "Como marcar os pontos na imagem",
            content = HTML(paste0(
              "A imagem deve conter dois quadros (frames/momentos) da trajetória.<br/><br/>",
              "Em cada quadro, marque dois pontos: a roda traseira e a roda dianteira correspondente.<br/><br/>",
              "<b>Ordem recomendada de marcação:</b><br/>",
              "• Roda traseira do 1º momento (A)<br/>",
              "• Roda dianteira do 1º momento (B)<br/>",
              "• Roda traseira do 2º momento (C)<br/>",
              "• Roda dianteira do 2º momento (D)<br/><br/>",
              "<b>Repita esse ciclo pelo menos 7 vezes</b> (total de 28 cliques),<br/>",
              "para garantir que cada ponto tenha amostras suficientes para os testes estatísticos.<br/><br/>"
            )),
            placement = "right",
            trigger = "hover",
            options = list(container = "body", trigger = "hover focus")
          ),
        )
      ),
      ### Página 2 ------------
      shinydashboard::tabItem(
        tabName = "app2",
        fluidRow(
          titulo_com_ajuda(
            ajuda_id = "ajuda_dispersao_cinza",
            label_text = "Tom de cinza ao longo da trajetória",
            ajuda_titulo = "Como interpretar os resultados",
            ajuda_texto = HTML(paste0(
              "Este gráfico mostra a variação do tom de cinza ao longo da trajetória, ",
              "após a suavização da imagem com filtro gaussiano (parâmetro controlado à esquerda).<br/><br/>",
              "Os <b>pontos pretos</b> indicam os valores suavizados extraídos da imagem, enquanto a <b>linha azul</b> representa ",
              "uma curva de ajuste.<br/><br/>",
              "<b>Linhas vermelhas verticais (tracejadas)</b> indicam os pontos de referência, conforme as marcações (A-D). ",
              "Se forem visíveis como picos ou vales distintos, isso valida as marcações feitas na aba anterior."
            ))
          ),
          plotly::plotlyOutput("dispersaoCinza"),
          titulo_com_ajuda(
            ajuda_id = "ajuda_scatter",
            label_text = "Densidade dos pontos simulados",
            ajuda_titulo = "Como interpretar os resultados",
            ajuda_texto = HTML(paste0(
              "Cada ponto neste gráfico representa uma simulação da posição dos centros das rodas, ",
              "baseada no Método de Monte Carlo.<br/><br/>",
              "<b>O fundo em escala de cinza mostra a densidade dessas simulações</b>: áreas mais escuras ",
              "indicam maior concentração de pontos.<br/><br/>",
              "A <b>linha preta</b> é a regressão final ajustada. Os <b>símbolos vermelhos</b> (A-D) mostram os ",
              "pontos de referência, conforme marcações.<br/><br/>",
              "Esse gráfico ajuda a visualizar a dispersão e a consistência das marcações simuladas."
            ))
          ),
          plotly::plotlyOutput("scatterPlot"),
          titulo_com_ajuda(
            ajuda_id = "ajuda_hist_vel",
            label_text = "Velocidade estimada",
            ajuda_titulo = "Como interpretar os resultados",
            ajuda_texto = HTML(paste0(
              "Este gráfico mostra a <b>distribuição das velocidades estimadas a partir das simulações</b>. <br/>",
              "A sobreposição das informações permite verificar se existem assimetrias, concentrações ou valores extremos.<hr>",
              "As <b>barras cinzas</b> representa quantas simulações resultaram em cada faixa de velocidade.<br/>",
              "A <b>linha verde (sólida)</b> indica a média da velocidade estimada. <br/>",
              "<b>Linhas vermelhas (tracejadas)</b> marcam os limites do intervalo de confiança definido à esquerda (por exemplo, 99%).<br/><br/>",
              "Esses valores indicam a incerteza da estimativa: quanto mais estreito o intervalo, mais confiável o resultado.<hr>",
              "O <b>boxplot abaixo</b> resume as estimativas com os seguintes elementos:<br/>",
              "A <b>caixa</b> mostra onde estão os 50% valores centrais (a maior parte das simulações).<br/>",
              "A <b>linha no meio da caixa</b> é a <b>mediana</b>, que representa o valor central da distribuição.<br/>",
              "As <b>linhas horizontais que saem da caixa</b> mostram os 25% mais baixos à esquerda e os 25% mais altos à direita."
            ))
          ),
          plotly::plotlyOutput("histogramPlot"),
          titulo_com_ajuda(
            ajuda_id = "ajuda_hist_dist",
            label_text = "Deslocamento estimado",
            ajuda_titulo = "Como interpretar os resultados",
            ajuda_texto = HTML(paste0(
              "Este gráfico mostra a frequência acumulada do deslocamento estimado entre os quadros (em metros).<br/><br/>",
              "A <b>curva azul</b> representa a proporção de simulações com deslocamento até determinado valor.<br/><br/>",
              "<b>Linhas vermelhas (tracejadas)</b> indicam o intervalo de confiança e a <b>linha verde (sólida)</b> marca a média do deslocamento.<br/><br/>",
              "Com isso, é possível verificar se o deslocamento está bem definido ou se há muita variação nas estimativas."
            ))
          ),
          plotly::plotlyOutput("histogramDistancia")
        )
      )
    )
  )
)
