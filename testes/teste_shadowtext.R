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

      nova <- do.call(shadowtext::geom_shadowtext, c(
        list(
          mapping = mapping, data = data,
          bg.colour = bg.colour, bg.r = bg.r,
          colour = layer$aes_params$colour %||% "black"
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



# Base com pontos simples
p <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
  geom_point() +
  annotate("text", x = 5, y = 5, label = "Texto central", color = "red", size = 6) +
  annotate("text", x = 8, y = 8, label = "Outro texto", color = "blue", size = 5) +
  ggtitle("Exemplo com texto") +
  theme_minimal()

p <- p +
  annotate("text", x = 2, y = 2, label = "Outro texto2", color = "blue", size = 5)

# Plot original (sem sombra)
print(p)

# Plot com annotate substituído por shadowtext
substituir_annotate_por_shadowtext(p)
p2 <- tema_graficos_export(p)
p2
p2$layers[[3]] |> glimpse()
