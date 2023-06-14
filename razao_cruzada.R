# Estimativa de intervalo de confianca da velocidade escalar media
# Autor: Carlo Ralph De Musis

library(cowplot)
library(MASS)
library(deming)
library(dplyr)
library(ggplot2)

# Definir o numero de repeticoes desejadas
repeticoes <- 10000 # Repeicoes para o metodo de Monte Carlo
set.seed(42) # Semente aleatória

l <- 2373/1000000  # Distancia entre eixos (km)

pkt_dts_time_1 = 1678577738.267000 # CAM2, Frame 5073
pkt_dts_time_2 = 1678577738.534000 # CAM2, Frame 5077

dt <-  pkt_dts_time_2 - pkt_dts_time_1 # Tempo entre os frames (s)
dt <-  dt/3600 # Tempo entre os frames (h)

dados_originais <- data.frame(ponto = rep(1:4, each = 10),
                              x = c(223, 228, 219, 233, 231, 217, 220, 221, 218, 220,  # A
                                    443, 440, 446, 436, 440, 437, 442, 434, 431, 433,  # B
                                    652, 649, 649, 648, 647, 647, 648, 647, 647, 648,  # C
                                    767, 768, 765, 768, 769, 767, 768, 765, 767, 768), # D
                              y = c(527, 529, 526, 526, 532, 526, 523, 523, 523, 517,  # A
                                    475, 474, 474, 474, 476, 480, 473, 475, 471, 470,  # B
                                    414, 416, 419, 418, 420, 421, 417, 421, 418, 419,  # C
                                    396, 398, 399, 397, 397, 399, 399, 397, 398, 395)) # D


# Funcao para gerar repeticoes usando uma distribuicao normal bivariada
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
  
  repeticoes_geradas <- do.call(rbind, repeticoes_geradas)  # Juntar todas as repeticoes em um unico data frame
  
  return(repeticoes_geradas)
}

# Gerar as repeticoes
repeticoes_geradas <- gerar_repeticoes(dados_originais, repeticoes)

# Plotar o grafico de dispersao com cores identificando os grupos e series
ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  labs(x = "Coordenada X", y = "Coordenada Y", color = "Grupos") 

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

# Imprimir os resultados
print(paste("Equacao media de regressao: y =", round(media_coeficientes[1], 2), "+", round(media_coeficientes[2], 2), "*x"))
print(paste("Intervalo de confianca para o intercepto: [", round(ic_coeficientes[1, 1], 2), ",", round(ic_coeficientes[2, 1], 2), "]"))
print(paste("Intervalo de confianca para a inclinacao: [", round(ic_coeficientes[1, 2], 2), ",", round(ic_coeficientes[2, 2], 2), "]"))

# Media dos coeficientes
intercepto <- media_coeficientes[1]
inclinacao <- media_coeficientes[2]

# Criando o grafico de dispersao e adicionando a linha de regressao
ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  geom_abline(intercept = intercepto, slope = inclinacao, color = "black") +
  labs(x = "Coordenada X", y = "Coordenada Y", title = "Regressao Ortogonal Media", color = "Ponto") +
  theme_minimal()


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

ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  geom_abline(intercept = intercepto, slope = inclinacao, color = "black") +
  geom_point(data = centros_gravidade, aes(x = projecao_x, y = projecao_y), color = "darkgray", shape = 8, size = 3) +
  labs(x = "Coordenada X", y = "Coordenada Y", color = "Grupos") 

rotacionar_pontos <- function(df, angulo) {
  df_rotacionado <- df
  df_rotacionado$x <- df$x * cos(angulo) - df$y * sin(angulo)
  df_rotacionado$y <- df$x * sin(angulo) + df$y * cos(angulo)
  return(df_rotacionado)
}

# Rotacao
angulo_de_rotacao <- -atan(inclinacao)
repeticoes_geradas_rotacionadas <- rotacionar_pontos(repeticoes_geradas, angulo_de_rotacao)

# Calcular a media dos valores de y apos a rotacao
intercepto <- mean(repeticoes_geradas_rotacionadas$y)

# A inclinação e 0, já que a linha e agora horizontal
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

ggplot(repeticoes_geradas_rotacionadas, aes(x = x, y = y)) +
  geom_hex(bins = 120, aes(fill = ..count..)) +
  scale_fill_gradient(name = "Frequências", low = "gray", high = "black") +
  geom_abline(intercept = intercepto, slope = inclinacao, color = "black") +
  geom_point(data = centros_gravidade_rotacionados, aes(x = x, y = y), color = "lightblue", shape = 7, size = 1) +
  geom_text(data = centros_gravidade_rotacionados, aes(x = x, label = ponto), 
            vjust = -0.25, hjust = 1.5, color = "black") +
  labs(x = "Coordenada X", y = "Coordenada Y")

Ax <- as.numeric(centros_gravidade[1, 'projecao_x'])
Ay <- as.numeric(centros_gravidade[1, 'projecao_y'])
Bx <- as.numeric(centros_gravidade[2, 'projecao_x'])
By <- as.numeric(centros_gravidade[2, 'projecao_y'])
Cx <- as.numeric(centros_gravidade[3, 'projecao_x'])
Cy <- as.numeric(centros_gravidade[3, 'projecao_y'])
Dx <- as.numeric(centros_gravidade[4, 'projecao_x'])
Dy <- as.numeric(centros_gravidade[4, 'projecao_y'])

distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)

# Ordenar os dados por 'serie' e 'ponto'
repeticoes_geradas <- repeticoes_geradas %>%
  arrange(serie, ponto)

# Agrupar os dados por 'serie' e aplicar a funcao 'distancia' a cada grupo
resultados <- repeticoes_geradas %>%
  group_by(serie) %>%
  summarise(Ax = x[1], Ay = y[1], Bx = x[2], By = y[2], Cx = x[3], Cy = y[3], Dx = x[4], Dy = y[4]) %>%
  rowwise() %>%
  mutate(distancia = distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)) %>%
  ungroup()

# Remove valores NA e calcula a velocidade
velocidade <- na.omit(resultados$distancia)/dt

# Media
media <- mean(velocidade)
print(paste("Media da velocidade: ", round(media, 1)))

# Percentis
percentis <- quantile(velocidade, c(0.005, 0.995))
print(paste("Percentil 0.5%: ", round(percentis[1], 1)))
print(paste("Percentil 99.5%: ", round(percentis[2], 1)))

velocidade <- as.data.frame(velocidade)
names(velocidade) <- "v"

# Criando o histograma
p1 <- ggplot(velocidade, aes(x = v)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightgray") +
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

# Organizando os dois gráficos em um único plot
p <- plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", rel_heights = c(6/7, 1/7))
p