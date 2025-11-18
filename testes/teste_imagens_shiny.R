library(shiny)
library(imager)

sobrepor_imagens <- function(img1, img2,
                             coluna_transicao = 200,
                             largura_linha = 2) {
  img1 <- imager::load.image(img1)
  img2 <- imager::load.image(img2)

  altura_final <- max(dim(img1)[2], dim(img2)[2])
  ajustar_altura <- function(img, altura_final) {
    alt_atual <- dim(img)[2]
    if (alt_atual == altura_final) return(img)

    margem <- altura_final - alt_atual
    topo <- floor(margem / 2)
    baixo <- margem - topo

    topo_borda <- imager::as.cimg(array(0, dim = c(dim(img)[1], topo, 1, dim(img)[4])))
    baixo_borda <- imager::as.cimg(array(0, dim = c(dim(img)[1], baixo, 1, dim(img)[4])))

    imager::imappend(list(topo_borda, img, baixo_borda), axis = "y")
  }

  img1 <- ajustar_altura(img1, altura_final)
  img2 <- ajustar_altura(img2, altura_final)

  if (coluna_transicao >= dim(img1)[1] || coluna_transicao >= dim(img2)[1]) {
    stop("coluna_transicao fora dos limites da largura das imagens")
  }

  recorte_img1 <- img1[1:coluna_transicao, , , , drop = FALSE]
  recorte_img2 <- img2[(coluna_transicao + 1):dim(img2)[1], , , , drop = FALSE]

  faixa_preta <- imager::as.cimg(array(0,
                                       dim = c(largura_linha, altura_final, 1, dim(img1)[4])
  ))

  imager::imappend(list(recorte_img1, faixa_preta, recorte_img2), axis = "x")
}

ui <- fluidPage(
  titlePanel("Combinar imagens horizontalmente"),
  sidebarLayout(
    sidebarPanel(
      fileInput("img1", "Imagem 1", accept = c(".jpg", ".png")),
      fileInput("img2", "Imagem 2", accept = c(".jpg", ".png")),
      numericInput("coluna", "Coluna de transição (px):", value = 200, min = 1),
      actionButton("processar", "Combinar imagens")
    ),
    mainPanel(
      imageOutput("resultado")
    )
  )
)

server <- function(input, output) {
  imagem_resultante <- eventReactive(input$processar, {
    req(input$img1, input$img2)

    tryCatch({
      resultado <- sobrepor_imagens(
        input$img1$datapath,
        input$img2$datapath,
        coluna_transicao = input$coluna
      )

      caminho <- tempfile(fileext = ".png")
      imager::save.image(resultado, caminho)
      caminho
    }, error = function(e) {
      showNotification("Erro ao combinar imagens.", type = "error")
      NULL
    })
  })

  output$resultado <- renderImage({
    req(imagem_resultante())
    list(src = imagem_resultante(), contentType = "image/png", alt = "Imagem combinada")
  }, deleteFile = TRUE)
}

shinyApp(ui, server)
