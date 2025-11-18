# Aplicativo para estimativa de velocidade veicular
# Autor Carlo Ralph De Musis
# Versão: 0.85

# Pacotes ------------------
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  # Interface Shiny
  shiny,
  shinydashboard,
  shinyBS, # popovers

  # Manipulação de imagem
  base64enc,
  jpeg,
  png,
  imager, # suavização de imagem

  # Estatística e modelagem ----------
  MASS, # simulação de distribuição normal multivariada (mvrnorm)
  MVN, # testes de normalidade multivariada (ex: Mardia)
  deming, # regressão Deming (erro em x e y)

  # Manipulação de dados -------------
  dplyr,
  tidyr,

  # Visualização ----------------------
  ggplot2,
  cowplot,
  grid,
  hexbin, # gráficos de densidade hexagonal
  plotly, # gráficos interativos
  shadowtext # adiciona borda a texto de gráficos exportados
)

# Objetos ------------------------------------

## Configurações do painel ------

### Gráficos ----

# Paleta de cores acessível para daltonismo definida por Paul Tol
paleta <- c("#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311", "#009988", "#BBBBBB")

config_plotly <- function(p) {
  plotly::config(p,
    scrollZoom = FALSE,
    locale = "pt-BR",
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("select", "select2d", "lasso2d")
  )
}

substituir_annotate_por_shadowtext <- function(p,
                                               bg.colour = "white",
                                               bg.r = 0.1) {
  if (!requireNamespace("shadowtext", quietly = TRUE)) {
    return(p)
  }

  novas_layers <- purrr::map(p$layers, function(layer) {
    if (inherits(layer$geom, "GeomText")) {
      mapping <- layer$mapping
      data <- layer$data
      aes_params <- layer$aes_params

      # Define cor padrão se não houver
      cor_final <- aes_params$colour %||% "black"

      # Remove cor de aes_params se presente, para evitar conflito
      aes_params$colour <- NULL

      nova <- do.call(shadowtext::geom_shadowtext, c(
        list(
          mapping = mapping,
          data = data,
          colour = cor_final,
          bg.colour = bg.colour,
          bg.r = bg.r
        ),
        aes_params
      ))

      return(nova)
    } else {
      return(layer)
    }
  })

  p$layers <- novas_layers
  p
}

tema_graficos_export <- function(p, titulo = NULL, fonte = "sans") {
  if (!inherits(p, "gg")) {
    return(NULL)
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 13, base_family = fonte) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, face = "bold", size = 16
      )
    )

  if (!is.null(titulo)) {
    p <- p + ggplot2::ggtitle(titulo)
  }

  p <- tryCatch(substituir_annotate_por_shadowtext(p), error = function(e) p)

  tryCatch(p, error = function(e) NULL)
}

### Botões de ajuda ------

input_com_ajuda <- function(input_id, label_text, input_ui, ajuda_id, ajuda_titulo = NULL, ajuda_texto) {
  tagList(
    tags$div(
      style = "display: flex; align-items: center;",
      class = "form-group shiny-input-container",
      tags$label(label_text, class = "control-label"),
      shinyBS::bsButton(ajuda_id, label = NULL, icon = icon("info-circle"), size = "extra-small")
    ),
    input_ui,
    shinyBS::bsPopover(
      ajuda_id, ajuda_titulo, ajuda_texto,
      placement = "right",
      options = list(container = "body", trigger = "hover focus")
    )
  )
}

titulo_com_ajuda <- function(ajuda_id, label_text, ajuda_texto, ajuda_titulo = NULL, centralizar = FALSE) {
  alinhamento <- if (centralizar) "center" else "flex-start"

  tagList(
    tags$div(
      style = paste0("display: flex; align-items: center; justify-content: ", alinhamento, "; gap: 8px;"),
      tags$h4(label_text),
      shinyBS::bsButton(ajuda_id, label = NULL, icon = icon("info-circle"), size = "extra-small")
    ),
    shinyBS::bsPopover(
      id = ajuda_id,
      title = ajuda_titulo,
      content = ajuda_texto,
      placement = "right",
      options = list(container = "body", trigger = "hover focus")
    )
  )
}

## Funções de processamento -----
# Gera repeticoes usando uma distribuicao normal bivariada

gerar_repeticoes <- function(dados_originais, repeticoes) {
  grupos <- unique(dados_originais$ponto) # Identificar os grupos de pontos
  repeticoes_geradas <- list() # Lista para armazenar as repeticoes

  for (grupo in grupos) {
    dados_grupo <- dados_originais[dados_originais$ponto == grupo, ] # Dados do grupo atual
    media <- colMeans(dados_grupo[, c("x", "y")]) # Media do grupo atual
    matriz_cov <- cov(dados_grupo[, c("x", "y")]) # Matriz de covariancia do grupo atual

    # Verificação de validade da matriz de covariância
    if (any(!is.finite(matriz_cov)) || any(is.na(matriz_cov))) {
      showNotification(
        paste0("Erro: matriz de covariância inválida para o grupo '", grupo, "'."),
        type = "error"
      )
      next
    }

    repeticoes_grupo <- tryCatch(
      {
        lapply(1:repeticoes, function(i) {
          repeticao <- MASS::mvrnorm(1, mu = media, Sigma = matriz_cov)
          data.frame(serie = i, ponto = grupo, x = repeticao[1], y = repeticao[2])
        })
      },
      error = function(e) {
        showNotification(
          paste0("Erro ao gerar repetições para grupo '", grupo, "': ", e$message),
          type = "error"
        )
        return(NULL)
      }
    )

    if (!is.null(repeticoes_grupo)) {
      repeticoes_geradas <- c(repeticoes_geradas, repeticoes_grupo)
    }
  }
  # Junta todas as repeticoes em um unico data frame
  repeticoes_geradas <- do.call(rbind, repeticoes_geradas)

  return(repeticoes_geradas)
}

## ______________________________________________________________________________

hex_cinza <- function(hex_colors) {
  # Aplica a conversão a cada cor no vetor usando sapply
  cinza_values <- sapply(hex_colors, function(hex_color) {
    # Converte hexadecimal para RGB
    rgb <- grDevices::col2rgb(hex_color)

    # Converte RGB para cinza usando a fórmula de luminosidade
    cinza <- (rgb[1, ] + rgb[2, ] + rgb[3, ]) / 3

    return(round(cinza))
  }, USE.NAMES = FALSE)

  return(cinza_values)
}

## ______________________________________________________________________________

# Calculo da distancia
distancia <- function(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l) {
  A <- complex(real = Ax, imaginary = Ay)
  B <- complex(real = Bx, imaginary = By)
  C <- complex(real = Cx, imaginary = Cy)
  D <- complex(real = Dx, imaginary = Dy)

  dAC <- C - A
  dBC <- C - B
  dAD <- D - A
  dBD <- D - B

  k <- (dAC / dBC) / (dAD / dBD)

  d_c <- sqrt(k / (k - 1) * l^2)
  d <- sqrt(Re(d_c)^2 + Im(d_c)^2)

  return(d)
}

## ______________________________________________________________________________

rotacionar_pontos <- function(df, angulo) {
  df_rotacionado <- df
  df_rotacionado$x <- df$x * cos(angulo) - df$y * sin(angulo)
  df_rotacionado$y <- df$x * sin(angulo) + df$y * cos(angulo)
  return(df_rotacionado)
}
