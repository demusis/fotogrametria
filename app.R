# Título: Processamento Geométrico para Fotogrametria
# Data: 09/09/2025
# Autor: Carlo Ralph De Musis
# Versão Modificada: 09/09/2025

# --- Carregar Bibliotecas ---
library(shiny)
library(magick)
library(colourpicker)
library(shinycssloaders)
library(bslib)
library(imager)
library(dplyr)
library(fields)


# --- Opções Globais ---
options(shiny.maxRequestSize = 100 * 1024^2)

# --- Funções Auxiliares ---

# Função para aplicar distorção de barril a um conjunto de coordenadas
distort_coords <- function(points, k_params, center_xy, norm_factor) {
  k1 <- k_params[1]
  k2 <- k_params[2]
  k3 <- k_params[3]
  
  coords_centered <- data.frame(
    x = points$x - center_xy[1],
    y = points$y - center_xy[2]
  )
  
  r2 <- (coords_centered$x^2 + coords_centered$y^2) / norm_factor^2
  r4 <- r2^2
  r6 <- r2^3
  
  f <- 1 + k1 * r2 + k2 * r4 + k3 * r6
  
  distorted_points <- data.frame(
    x = coords_centered$x * f + center_xy[1],
    y = coords_centered$y * f + center_xy[2]
  )
  
  return(distorted_points)
}


# Função de transformação para a Aba 3
calculate_tps_transform <- function(source_points_matrix, target_points_df, lambda) {
  tps_fit_x <- Tps(target_points_df, source_points_matrix[, 1], lambda = lambda)
  tps_fit_y <- Tps(target_points_df, source_points_matrix[, 2], lambda = lambda)
  return(list(tps_x = tps_fit_x, tps_y = tps_fit_y))
}

apply_tps_warp <- function(img, tps_fits) {
  map_function <- function(x, y) {
    dest_points <- as.matrix(data.frame(x = x, y = y))
    source_x <- predict(tps_fits$tps_x, dest_points)
    source_y <- predict(tps_fits$tps_y, dest_points)
    return(list(x = source_x, y = source_y))
  }
  warped_img <- imwarp(img, map = map_function, direction = "backward", interpolation = "cubic")
  return(warped_img)
}


# --- Definição do Tema (bslib) ---
royal_blue_theme_v2 <- bs_theme(
  version = 5,
  bg = "#EFF2F5",
  fg = "#212529",
  primary = "#4169E1",
  secondary = "#ADB5BD",
  base_font = font_google("Roboto", local = FALSE),
  heading_font = font_google("Roboto", local = FALSE)
)


# --- Interface do Usuário (UI) ---
ui <- navbarPage(
  "Processamento Geométrico para Fotogrametria",
  theme = royal_blue_theme_v2,
  
  # --- Aba 1: Corretor de Distorção de Lente ---
  tabPanel(
    "Correção de Distorção de Lente",
    icon = icon("camera-retro"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Controles"),
          card_body(
            h5("1. Carregar Imagem"),
            fileInput("userImage", NULL, accept = c("image/png", "image/jpeg"), placeholder = "Selecione um arquivo"),
            hr(),
            h5("2. Ajuste Manual"),
            p("Use os sliders para corrigir a distorção manualmente."),
            sliderInput("k1", label = tags$label("k₁:", bslib::tooltip(shiny::icon("question-circle"), "Coeficiente de 1ª ordem.", placement = "right")), min = -1.5, max = 1.5, value = 0, step = 0.001),
            sliderInput("k2", label = tags$label("k₂:", bslib::tooltip(shiny::icon("question-circle"), "Coeficiente de 2ª ordem.", placement = "right")), min = -1.0, max = 1.0, value = 0, step = 0.001),
            sliderInput("k3", label = tags$label("k₃:", bslib::tooltip(shiny::icon("question-circle"), "Coeficiente de 3ª ordem.", placement = "right")), min = -0.5, max = 0.5, value = 0, step = 0.001),
            actionButton("dist_btn_apply_manual", "Aplicar Correção", icon = icon("play"), class = "btn-success", style = "width: 100%; margin-top: 10px;"),
            hr(),
            h5("3. Otimização Automática"),
            p("Com os sliders em zero, desenhe linhas sobre feições retas. Após, clique em 'Propor Ajuste'.", style = "font-size: 0.9em;"),
            uiOutput("dist_polyline_selector_ui"),
            div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                actionButton("dist_btn_delete", "Excluir Linha", icon = icon("trash")),
                actionButton("dist_btn_clear_all", "Limpar Todas", icon = icon("broom"))
            ),
            actionButton("dist_btn_optimize", "Propor Ajuste com Linhas", icon = icon("cogs"), class = "btn-primary"),
            hr(),
            p("Gestão das Linhas de Referência:"),
            downloadButton("dist_btn_save_polylines", "Salvar Linhas"),
            fileInput("dist_load_polylines_input", "Carregar Linhas (.rds)", accept = ".rds"),
            hr(),
            div(style = "display: flex; justify-content: space-between;",
                actionButton("reset_all", "Reiniciar Tudo", icon = icon("refresh")),
                downloadButton("save_image", "Salvar Imagem", icon = icon("save"))
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Visualização"),
          card_body(
            div(style = "margin-bottom: 10px;",
                sliderInput("dist_zoom", "Zoom:", min = 0.2, max = 5, value = 1, step = 0.1, width = "30%")
            ),
            uiOutput("dist_plot_container_ui") %>% withSpinner(color = "#4169E1")
          )
        )
      )
    )
  ),
  
  # --- Aba 2: Combinador de Imagens ---
  tabPanel(
    "Análise Comparativa",
    icon = icon("layer-group"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Controles"),
          card_body(
            h5("1. Carregar Imagens"),
            p("Devem possuir as mesmas dimensões.", style = "font-size: 0.9em;"),
            fileInput("file1", "Imagem Esquerda", accept = c("image/png", "image/jpeg")),
            fileInput("file2", "Imagem Direita", accept = c("image/png", "image/jpeg")),
            
            conditionalPanel(
              condition = "output.combiner_images_loaded == true",
              hr(),
              h5("2. Ajustar Sobreposição"),
              sliderInput("split_position", "Posição:", min = 0, max = 100, value = 50, post = "%"),
              sliderInput("line_thickness", "Espessura (px):", min = 0, max = 50, value = 2),
              colourInput("line_color", "Cor da Linha:", "black", allowTransparent = TRUE),
              
              hr(),
              h5("3. Legendas"),
              textInput("caption_left", "Legenda Esquerda:", placeholder = "Opcional"),
              textInput("caption_right", "Legenda Direita:", placeholder = "Opcional"),
              
              numericInput("caption_size", "Tamanho da Fonte:", value = 16, min = 8, max = 48, step = 1),
              colourInput("caption_color", "Cor da Fonte:", value = "black", allowTransparent = TRUE),
              
              selectInput("caption_gravity_y", "Posição Vertical:", 
                          choices = c("Topo" = "top", "Centro" = "center", "Base" = "bottom"), 
                          selected = "bottom"),
              
              hr(),
              actionButton("comb_btn_update", "Atualizar Visualização", icon = icon("sync"), class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
              downloadButton("save_combined_image", "Salvar Imagem", icon = icon("save"))
            )
          )
        )
      ),
      mainPanel(
        width = 10,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Visualização"),
          card_body(
            div(style = "margin-bottom: 10px;",
                sliderInput("comb_zoom", "Zoom:", min = 0.2, max = 5, value = 1, step = 0.1, width = "30%")
            ),
            uiOutput("comb_image_container_ui") %>% withSpinner(color = "#4169E1")
          )
        )
      )
    )
  ),
  
  # --- Aba 3: Corretor Numérico (TPS) ---
  tabPanel(
    "Correção por Pontos de Controle",
    icon = icon("ruler-combined"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Controles"),
          card_body(
            h5("1. Carregar Imagem"),
            fileInput("tps_file_input", NULL, accept = c("image/png", "image/jpeg"), placeholder = "Selecione um arquivo"),
            hr(),
            h5("2. Marcar Curvas"),
            p("Desenhe sobre as curvas que deveriam ser retas.", style = "font-size: 0.9em;"),
            p(tags$b("Clique"), " para adicionar pontos.", style = "font-size: 0.9em;"),
            p(tags$b("Duplo-clique"), " para finalizar a linha.", style = "font-size: 0.9em;"),
            uiOutput("tps_polyline_selector_ui"),
            actionButton("tps_btn_delete", "Excluir Linha", icon = icon("trash")),
            hr(),
            h5("3. Aplicar Correção"),
            sliderInput("tps_lambda_log", "Suavização (λ em log10):", min = -9, max = 2, value = -5, step = 1),
            actionButton("tps_btn_correct", "Corrigir Imagem", icon = icon("cogs"), class = "btn-primary"),
            actionButton("tps_btn_reset", "Reiniciar Desenho", icon = icon("undo")),
            hr(),
            h5("4. Gestão de Pontos"),
            downloadButton("tps_btn_save_polylines", "Salvar Linhas"),
            fileInput("tps_load_polylines_input", "Carregar Linhas (.rds)", accept = ".rds"),
            hr(),
            downloadButton("tps_btn_download", "Salvar Resultado")
          )
        )
      ),
      mainPanel(
        width = 10,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Visualização Interativa"),
          card_body(
            div(style = "margin-bottom: 10px;",
                sliderInput("tps_zoom", "Zoom:", min = 0.2, max = 5, value = 1, step = 0.1, width = "30%")
            ),
            uiOutput("tps_plot_container_ui")
          )
        )
      )
    )
  )
)

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Lógica do Corretor de Distorção (Aba 1) ---
  
  vals <- reactiveValues(
    original_image = NULL,
    image_to_display = NULL, # Armazena a imagem (original ou corrigida) a ser exibida
    dist_polylines = list(),
    dist_current_poly = data.frame(x = numeric(), y = numeric())
  )
  
  observeEvent(input$userImage, {
    req(input$userImage)
    vals$original_image <- magick::image_read(input$userImage$datapath)
    vals$image_to_display <- vals$original_image # Define a imagem inicial para exibição
    reset_all_dist()
  })
  
  reset_all_dist <- function(){
    updateSliderInput(session, "k1", value = 0)
    updateSliderInput(session, "k2", value = 0)
    updateSliderInput(session, "k3", value = 0)
    updateSliderInput(session, "dist_zoom", value = 1)
    vals$dist_polylines <- list()
    vals$dist_current_poly <- data.frame(x = numeric(), y = numeric())
    if (!is.null(vals$original_image)) {
      vals$image_to_display <- vals$original_image # Reseta a imagem de exibição
    }
  }
  
  observeEvent(input$reset_all, {
    req(vals$original_image)
    reset_all_dist()
    showNotification("Estado da aba reiniciado.", type = "message")
  })
  
  output$dist_plot_container_ui <- renderUI({
    req(vals$original_image)
    zoom <- if (is.null(input$dist_zoom)) 1 else input$dist_zoom
    
    info <- image_info(vals$original_image)
    plot_width <- info$width * zoom
    plot_height <- info$height * zoom
    
    div(style = "width: 100%; height: 700px; overflow: auto; border: 1px solid #ddd;",
        plotOutput("dist_plot",
                   width = plot_width, height = plot_height,
                   click = "dist_plot_click", dblclick = "dist_plot_dblclick")
    )
  })
  
  observeEvent(input$dist_btn_apply_manual, {
    req(vals$original_image)
    k_params <- c(input$k1, input$k2, input$k3)
    
    if (all(k_params == 0)) {
      vals$image_to_display <- vals$original_image
    } else {
      vals$image_to_display <- image_distort(
        vals$original_image, 'barrel',
        c(-k_params[1], -k_params[2], -k_params[3]),
        bestfit = TRUE
      )
    }
    showNotification("Correção aplicada.", type = "message")
  })
  
  output$dist_plot <- renderPlot({
    req(vals$image_to_display)
    
    plot(vals$image_to_display, axes = FALSE, ann = FALSE)
    
    # A verificação dos sliders foi encapsulada em isolate()
    # para que a alteração deles não dispare a re-renderização do plot.
    is_drawing_mode <- isolate(all(c(input$k1, input$k2, input$k3) == 0))
    
    if (is_drawing_mode) {
      for (poly in vals$dist_polylines) {
        if (nrow(poly) > 0) {
          points(poly$x, poly$y, col = "cyan", pch = 19, cex = 1.5); lines(poly$x, poly$y, col = "cyan", lwd = 2)
        }
      }
      if (nrow(vals$dist_current_poly) > 0) {
        points(vals$dist_current_poly$x, vals$dist_current_poly$y, col = "magenta", pch = 19, cex = 1.5); lines(vals$dist_current_poly$x, vals$dist_current_poly$y, col = "magenta", lwd = 2)
      }
    }
  })
  
  observeEvent(input$dist_plot_click, {
    req(vals$original_image)
    if (all(c(input$k1, input$k2, input$k3) == 0)) {
      vals$dist_current_poly <- rbind(vals$dist_current_poly,
                                      data.frame(x = input$dist_plot_click$x, y = input$dist_plot_click$y))
    } else {
      showNotification("Para desenhar, retorne os sliders k₁, k₂ e k₃ para 0.", type="warning")
    }
  })
  
  observeEvent(input$dist_plot_dblclick, {
    req(nrow(vals$dist_current_poly) > 1)
    vals$dist_polylines <- c(vals$dist_polylines, list(vals$dist_current_poly))
    vals$dist_current_poly <- data.frame(x = numeric(), y = numeric())
  })
  
  output$dist_polyline_selector_ui <- renderUI({
    if (length(vals$dist_polylines) > 0) {
      selectInput("dist_poly_to_delete", "Linhas de Referência:", choices = paste("Linha", 1:length(vals$dist_polylines)))
    }
  })
  
  observeEvent(input$dist_btn_delete, {
    req(input$dist_poly_to_delete)
    idx_to_remove <- as.numeric(gsub("Linha ", "", input$dist_poly_to_delete))
    if (!is.na(idx_to_remove) && idx_to_remove <= length(vals$dist_polylines)) {
      vals$dist_polylines[[idx_to_remove]] <- NULL
    }
  })
  
  observeEvent(input$dist_btn_clear_all, {
    vals$dist_polylines <- list()
    vals$dist_current_poly <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$dist_btn_optimize, {
    req(vals$original_image, length(vals$dist_polylines) > 0)
    
    showNotification("Otimizando parâmetros k... Isso pode levar um momento.", 
                     type = "message", duration = NULL, id = "optim_msg")
    
    info <- image_info(vals$original_image)
    center_xy <- c(info$width / 2, info$height / 2)
    norm_factor <- sqrt(info$width^2 + info$height^2) / 2
    
    error_function <- function(k_params, polylines, center, norm) {
      total_error <- 0; k_to_eval <- k_params
      for (poly in polylines) {
        start_pt <- poly[1, ]; end_pt <- poly[nrow(poly), ]
        v <- end_pt - start_pt; v_sq_len <- v$x^2 + v$y^2
        if(v_sq_len == 0) next
        
        w_list <- lapply(1:nrow(poly), function(i) as.numeric(poly[i,]) - as.numeric(start_pt))
        w_mat <- do.call(rbind, w_list)
        
        t <- (w_mat[,1]*v$x + w_mat[,2]*v$y) / v_sq_len
        projected_points <- data.frame(x=start_pt$x + t*v$x, y=start_pt$y + t*v$y)
        
        distorted_ideal_points <- distort_coords(projected_points, k_to_eval, center, norm)
        
        error <- sum((poly$x - distorted_ideal_points$x)^2 + (poly$y - distorted_ideal_points$y)^2)
        total_error <- total_error + error
      }
      return(total_error)
    }
    
    initial_k <- c(input$k1, input$k2, input$k3)
    optim_result <- optim(
      par = initial_k, fn = error_function,
      polylines = vals$dist_polylines, center = center_xy, norm = norm_factor,
      method = "L-BFGS-B",
      lower = c(-1.5, -1.0, -0.5), upper = c(1.5, 1.0, 0.5)
    )
    
    removeNotification("optim_msg")
    
    if(optim_result$convergence == 0){
      showNotification("Otimização concluída. Clique em 'Aplicar Correção' para ver o resultado.", type="message")
      updateSliderInput(session, "k1", value = optim_result$par[1])
      updateSliderInput(session, "k2", value = optim_result$par[2])
      updateSliderInput(session, "k3", value = optim_result$par[3])
      vals$dist_polylines <- list()
    } else {
      showNotification("A otimização não convergiu.", type="warning")
    }
  })
  
  output$dist_btn_save_polylines <- downloadHandler(
    filename = function() { "linhas_referencia_distorcao.rds" },
    content = function(file) {
      if (length(vals$dist_polylines) == 0) {
        showNotification("Não há linhas finalizadas para salvar.", type = "warning")
        return(NULL)
      }
      saveRDS(vals$dist_polylines, file)
    }
  )
  
  observeEvent(input$dist_load_polylines_input, {
    req(input$dist_load_polylines_input$datapath)
    tryCatch({
      loaded_polylines <- readRDS(input$dist_load_polylines_input$datapath)
      if (is.list(loaded_polylines)) {
        vals$dist_polylines <- loaded_polylines
        showNotification("Linhas de referência carregadas com sucesso.", type = "message")
      } else {
        showNotification("Arquivo inválido. O ficheiro deve ser uma lista de polilinhas salva em formato .rds.", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Erro ao carregar o ficheiro:", e$message), type = "error")
    })
  })
  
  output$save_image <- downloadHandler(
    filename = function() {
      original_filename <- tools::file_path_sans_ext(basename(input$userImage$name %||% "imagem.png"))
      paste0(original_filename, "_dist-corrigida.png")
    },
    content = function(file) {
      req(vals$image_to_display)
      image_write(vals$image_to_display, path = file, format = 'png')
    }
  )
  
  # --- Lógica do Combinador de Imagens (Aba 2) ---
  
  comb_vals <- reactiveValues(
    img1 = NULL,
    img2 = NULL,
    img_to_display = NULL
  )
  
  calculate_and_combine_image <- function() {
    req(comb_vals$img1, comb_vals$img2, input$split_position, input$line_thickness >= 0)
    
    img1 <- comb_vals$img1; img2 <- comb_vals$img2
    info <- magick::image_info(img1)
    img_w <- info$width; img_h <- info$height
    split_pos_px <- as.integer(round((input$split_position / 100) * img_w))
    line_thick <- as.integer(input$line_thickness)
    line_col <- input$line_color
    vertical_pos <- input$caption_gravity_y
    
    switch(vertical_pos,
           "top"    = { gravity_left <- "northwest"; gravity_right <- "northeast" },
           "center" = { gravity_left <- "west";      gravity_right <- "east" },
           "bottom" = { gravity_left <- "southwest"; gravity_right <- "southeast" }
    )
    
    if (input$split_position > 0 && input$split_position < 100) {
      geom_left <- sprintf("%dx%d+0+0", split_pos_px, img_h)
      img_left <- magick::image_crop(img1, geom_left)
      geom_right <- sprintf("%dx%d+%d+0", (img_w - split_pos_px), img_h, split_pos_px)
      img_right <- magick::image_crop(img2, geom_right)
      base_image <- magick::image_append(c(img_left, img_right))
      
      if (line_thick > 0) {
        divider <- magick::image_blank(width = line_thick, height = img_h, color = line_col)
        offset_string <- sprintf("+%d+0", (split_pos_px - round(line_thick / 2)))
        intermediate_img <- magick::image_composite(base_image, divider, offset = offset_string)
      } else {
        intermediate_img <- base_image
      }
      
      final_img <- intermediate_img
      if (!is.null(input$caption_left) && input$caption_left != "") {
        final_img <- magick::image_annotate(final_img, input$caption_left, gravity = gravity_left, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
      if (!is.null(input$caption_right) && input$caption_right != "") {
        final_img <- magick::image_annotate(final_img, input$caption_right, gravity = gravity_right, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
    } else if (input$split_position <= 0) {
      final_img <- img2
      if (!is.null(input$caption_right) && input$caption_right != "") {
        final_img <- magick::image_annotate(final_img, input$caption_right, gravity = gravity_right, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
    } else {
      final_img <- img1
      if (!is.null(input$caption_left) && input$caption_left != "") {
        final_img <- magick::image_annotate(final_img, input$caption_left, gravity = gravity_left, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
    }
    return(final_img)
  }
  
  observeEvent(c(input$file1, input$file2), {
    req(input$file1, input$file2)
    
    img1 <- magick::image_read(input$file1$datapath)
    img2 <- magick::image_read(input$file2$datapath)
    info1 <- magick::image_info(img1); info2 <- magick::image_info(img2)
    
    if (info1$width != info2$width || info1$height != info2$height) {
      showNotification("As imagens não possuem as mesmas dimensões.", type = "error", duration = 8)
      comb_vals$img_to_display <- NULL
    } else {
      comb_vals$img1 <- img1
      comb_vals$img2 <- img2
      comb_vals$img_to_display <- calculate_and_combine_image()
    }
    updateSliderInput(session, "comb_zoom", value = 1)
  })
  
  observeEvent(input$comb_btn_update, {
    comb_vals$img_to_display <- calculate_and_combine_image()
    showNotification("Visualização atualizada.", type = "message")
  })
  
  output$combiner_images_loaded <- reactive({ !is.null(input$file1) && !is.null(input$file2) })
  outputOptions(output, "combiner_images_loaded", suspendWhenHidden = FALSE)
  
  output$comb_image_container_ui <- renderUI({
    req(input$file1, input$file2)
    div(style = "width: 100%; height: 700px; overflow: auto; border: 1px solid #ddd;",
        imageOutput("combined_image", height = "auto")
    )
  })
  
  output$combined_image <- renderImage({
    req(comb_vals$img_to_display)
    
    final_img <- comb_vals$img_to_display
    tmpfile <- magick::image_write(final_img, tempfile(fileext = '.png'), format = 'png')
    
    zoom_level <- if(is.null(input$comb_zoom)) 1 else input$comb_zoom
    
    list(src = tmpfile, contentType = "image/png", 
         width = paste0(100 * zoom_level, "%"), 
         height = "auto", 
         alt = "Imagem combinada")
  }, deleteFile = TRUE)
  
  output$save_combined_image <- downloadHandler(
    filename = function() {
      paste0("imagem_combinada_com_legenda_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(comb_vals$img_to_display)
      magick::image_write(comb_vals$img_to_display, path = file, format = 'png')
    }
  )
  
  # --- Lógica do Corretor Numérico (TPS - Aba 3) ---
  
  tps_vals <- reactiveValues(
    original_img = NULL,
    display_img = NULL,
    polylines = list(),
    current_poly = data.frame(x = numeric(), y = numeric())
  )
  observeEvent(input$tps_file_input, {
    req(input$tps_file_input$datapath)
    tps_vals$original_img <- load.image(input$tps_file_input$datapath)
    tps_vals$display_img <- tps_vals$original_img
    tps_vals$polylines <- list()
    tps_vals$current_poly <- data.frame(x = numeric(), y = numeric())
    updateSliderInput(session, "tps_zoom", value = 1)
  })
  output$tps_plot_container_ui <- renderUI({
    req(tps_vals$original_img)
    zoom <- if (is.null(input$tps_zoom)) 1 else input$tps_zoom
    plot_width <- width(tps_vals$original_img) * zoom
    plot_height <- height(tps_vals$original_img) * zoom
    div(style = "width: 100%; height: 700px; overflow: auto; border: 1px solid #ddd;",
        plotOutput("tps_image_plot",
                   width = plot_width,
                   height = plot_height,
                   click = "tps_plot_click",
                   dblclick = "tps_plot_dblclick")
    )
  })
  observeEvent(input$tps_plot_click, {
    req(tps_vals$display_img)
    tps_vals$current_poly <- rbind(tps_vals$current_poly,
                                   data.frame(x = input$tps_plot_click$x,
                                              y = input$tps_plot_click$y))
  })
  observeEvent(input$tps_plot_dblclick, {
    req(nrow(tps_vals$current_poly) > 1)
    tps_vals$polylines <- c(tps_vals$polylines, list(tps_vals$current_poly))
    tps_vals$current_poly <- data.frame(x = numeric(), y = numeric())
  })
  observeEvent(input$tps_btn_correct, {
    req(tps_vals$original_img)
    if (length(tps_vals$polylines) > 0) {
      showNotification("Calculando transformação...", type = "message", duration = NULL, id = "tps_processing")
      source_points_user <- bind_rows(tps_vals$polylines)
      target_points_list <- list()
      for (poly in tps_vals$polylines) {
        start_point <- poly %>% slice(1); end_point <- poly %>% slice(n())
        n_points <- nrow(poly)
        t_param <- seq(0, 1, length.out = n_points)
        target_x <- (1 - t_param) * start_point$x + t_param * end_point$x
        target_y <- (1 - t_param) * start_point$y + t_param * end_point$y
        target_points_list <- c(target_points_list, list(data.frame(x = target_x, y = target_y)))
      }
      target_points_user <- bind_rows(target_points_list)
      w <- width(tps_vals$original_img); h <- height(tps_vals$original_img)
      corners <- data.frame(x = c(1, w, 1, w), y = c(1, 1, h, h))
      source_points <- rbind(source_points_user, corners)
      target_points <- rbind(target_points_user, corners)
      tryCatch({
        lambda_val <- 10^(input$tps_lambda_log)
        tps_fits <- calculate_tps_transform(as.matrix(source_points), target_points, lambda = lambda_val)
        tps_vals$display_img <- apply_tps_warp(tps_vals$original_img, tps_fits)
        tps_vals$polylines <- list()
        tps_vals$current_poly <- data.frame(x = numeric(), y = numeric())
        updateSliderInput(session, "tps_zoom", value = 1)
        removeNotification("tps_processing")
        showNotification("Correção aplicada! As linhas foram removidas.", type = "message")
      }, error = function(e) {
        removeNotification("tps_processing")
        showNotification("Informação de entrada não ideal. Tente usar mais pontos ou ajustar a suavização.", type = "warning")
      })
    } else {
      showNotification("Por favor, marque pelo menos uma linha antes de corrigir.", type = "warning")
    }
  })
  output$tps_image_plot <- renderPlot({
    req(tps_vals$display_img)
    plot(tps_vals$display_img, axes = FALSE, ann = FALSE)
    for (poly in tps_vals$polylines) {
      if (nrow(poly) > 0) {
        points(poly$x, poly$y, col = "blue", pch = 19, cex = 1.5)
        lines(poly$x, poly$y, col = "blue", lwd = 2)
      }
    }
    if (nrow(tps_vals$current_poly) > 0) {
      points(tps_vals$current_poly$x, tps_vals$current_poly$y, col = "red", pch = 19, cex = 1.5)
      lines(tps_vals$current_poly$x, tps_vals$current_poly$y, col = "red", lwd = 2)
    }
  }, res = 96)
  observeEvent(input$tps_btn_reset, {
    req(tps_vals$original_img)
    tps_vals$display_img <- tps_vals$original_img
    tps_vals$polylines <- list()
    tps_vals$current_poly <- data.frame(x = numeric(), y = numeric())
    updateSliderInput(session, "tps_zoom", value = 1)
    showNotification("Estado do desenho reiniciado.", type = "message")
  })
  output$tps_polyline_selector_ui <- renderUI({
    if (length(tps_vals$polylines) > 0) {
      selectInput("tps_poly_to_delete", "Linhas Finalizadas:", choices = paste("Linha", 1:length(tps_vals$polylines)))
    }
  })
  observeEvent(input$tps_btn_delete, {
    req(input$tps_poly_to_delete)
    idx_to_remove <- as.numeric(gsub("Linha ", "", input$tps_poly_to_delete))
    if (!is.na(idx_to_remove) && idx_to_remove <= length(vals$dist_polylines)) {
      tps_vals$polylines[[idx_to_remove]] <- NULL
    }
  })
  output$tps_btn_save_polylines <- downloadHandler(
    filename = function() { "linhas_tps.rds" },
    content = function(file) {
      if (length(tps_vals$polylines) == 0) {
        showNotification("Não há linhas finalizadas para salvar.", type = "warning")
        return(NULL)
      }
      saveRDS(tps_vals$polylines, file)
    }
  )
  observeEvent(input$tps_load_polylines_input, {
    req(input$tps_load_polylines_input$datapath)
    tryCatch({
      loaded_polylines <- readRDS(input$tps_load_polylines_input$datapath)
      if (is.list(loaded_polylines)) {
        tps_vals$polylines <- loaded_polylines
        showNotification("Linhas carregadas com sucesso.", type = "message")
      } else {
        showNotification("Arquivo inválido. O ficheiro deve ser uma lista salva em formato .rds.", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Erro ao carregar o ficheiro:", e$message), type = "error")
    })
  })
  output$tps_btn_download <- downloadHandler(
    filename = function() { paste0("tps_corrigida-", basename(input$tps_file_input$name %||% "imagem.png")) },
    content = function(file) {
      req(tps_vals$display_img)
      imager::save.image(tps_vals$display_img, file)
    }
  )
}


# --- Executar o Aplicativo ---
shinyApp(ui = ui, server = server)