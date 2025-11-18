imagem <- "C:/Users/iliasmusis/Downloads/c1-240420155938.png"
imagem2 <- "C:/Users/iliasmusis/Downloads/c1-2.png"

imgData <<- png::readPNG(imagem)
imgData |> glimpse()

dimensoes_aux <<- dim(imgData)
dimensoes <<- paste(dimensoes_aux[2], "x", dimensoes_aux[1])

imgRaster <- base64enc::dataURI(file = imagem)
imgRaster |> glimpse()


img_magick <- magick::image_read(imagem)
img_magick |> glimpse()

img_imager <- imager::load.image(imagem)
img_imager |> glimpse()

juntar_quadros_horizontalmente <- function(img1, img2, largura_linha = 2) {
  img1 <- imager::load.image(img1)
  img2 <- imager::load.image(img2)

  alt1 <- dim(img1)[2]
  alt2 <- dim(img2)[2]
  max_altura <- max(alt1, alt2)

  # Função para padronizar altura
  ajustar_altura <- function(img, altura_final) {
    altura_atual <- dim(img)[2]
    if (altura_atual == altura_final) return(img)

    canais <- dim(img)[4]
    largura <- dim(img)[1]

    margem <- altura_final - altura_atual
    topo <- floor(margem / 2)
    baixo <- margem - topo

    topo_borda <- imager::as.cimg(array(0, dim = c(largura, topo, 1, canais)))
    baixo_borda <- imager::as.cimg(array(0, dim = c(largura, baixo, 1, canais)))

    imager::imappend(list(topo_borda, img, baixo_borda), axis = "y")
  }

  img1_pad <- ajustar_altura(img1, max_altura)
  img2_pad <- ajustar_altura(img2, max_altura)

  # Linha preta entre imagens
  canais <- dim(img1_pad)[4]
  faixa_preta <- imager::as.cimg(array(0, dim = c(largura_linha, max_altura, 1, canais)))

  imager::imappend(list(img1_pad, faixa_preta, img2_pad), axis = "x")
}

juntar_quadros_horizontalmente(imagem, imagem2) |> imager::save.image("testes/imagem_combinada.png")


sobrepor_imagens <- function(img1, img2,
                             coluna_transicao = 200,
                             largura_linha = 2) {
  img1 <- imager::load.image(img1)
  img2 <- imager::load.image(img2)

  # Padronizar alturas
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

  # Recorte horizontal (eixo x)
  if (coluna_transicao >= dim(img1)[1] || coluna_transicao >= dim(img2)[1]) {
    stop("coluna_transicao fora dos limites da largura das imagens")
  }

  recorte_img1 <- img1[1:coluna_transicao, , , , drop = FALSE]
  recorte_img2 <- img2[(coluna_transicao + 1):dim(img2)[1], , , , drop = FALSE]

  # Linha preta entre imagens
  faixa_preta <- imager::as.cimg(array(0,
                                       dim = c(largura_linha, altura_final, 1, dim(img1)[4])
  ))

  # Combinar
  imagem_final <- imager::imappend(
    list(recorte_img1, faixa_preta, recorte_img2),
    axis = "x"
  )

  return(imagem_final)
}
sobrepor_imagens(imagem, imagem2,
                            coluna_transicao = 500) |> imager::save.image("testes/imagem_combinada.png")
