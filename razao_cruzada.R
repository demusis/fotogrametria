library(MASS)
library(deming)
library(dplyr)
library(ggplot2)

# Definir o número de repetições desejadas
repeticoes <- 100

# Dados originais
dados_originais <- data.frame(ponto = rep(1:4, each = 3), 
                              x = c(1, 2, 3, 
                                    6, 8, 10,
                                    20, 21, 23,
                                    25, 23, 26), 
                              y = c(4, 3, 5, 
                                    13, 13, 15,
                                    21, 21, 24,
                                    29, 28, 31))

# Função para gerar repetições usando uma distribuição normal bivariada
gerar_repeticoes <- function(dados_originais, repeticoes) {
  grupos <- unique(dados_originais$ponto)  # Identificar os grupos de pontos
  repeticoes_geradas <- list()  # Lista para armazenar as repetições
  
  for (grupo in grupos) {
    dados_grupo <- dados_originais[dados_originais$ponto == grupo, ]  # Dados do grupo atual
    media <- colMeans(dados_grupo[, c("x", "y")])  # Média do grupo atual
    matriz_cov <- cov(dados_grupo[, c("x", "y")])  # Matriz de covariância do grupo atual
    
    repeticoes_grupo <- lapply(1:repeticoes, function(i) {
      repeticao <- mvrnorm(1, mu = media, Sigma = matriz_cov)
      data.frame(serie = i, ponto = grupo, x = repeticao[1], y = repeticao[2])
    })
    
    repeticoes_geradas <- c(repeticoes_geradas, repeticoes_grupo)
  }
  
  repeticoes_geradas <- do.call(rbind, repeticoes_geradas)  # Juntar todas as repetições em um único data frame
  
  return(repeticoes_geradas)
}

# Gerar as repetições
repeticoes_geradas <- gerar_repeticoes(dados_originais, repeticoes)

# Plotar o gráfico de dispersão com cores identificando os grupos e series
ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  labs(x = "Coordenada X", y = "Coordenada Y", color = "Grupos") 

# Cálculo da distância
distancia <- function(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l) {
  # Calcular as distâncias euclidianas
  dAC <- sqrt((Ax - Cx)^2 + (Ay - Cy)^2)
  dBC <- sqrt((Bx - Cx)^2 + (By - Cy)^2)
  dAD <- sqrt((Ax - Dx)^2 + (Ay - Dy)^2)
  dBD <- sqrt((Bx - Dx)^2 + (By - Dy)^2)
  
  k <- (dAC/dBC)/(dAD/dBD)
  
  d <- (k/(k-1)*l^2)^0.5
  return(d)
}  

# Agrupar os dados por 'serie' e aplicar a regressão Deming a cada grupo
resultados <- repeticoes_geradas %>% 
  group_by(serie) %>%
  do(modelo = deming(y ~ x, data = .))

# Extrair os coeficientes de cada regressão e salvá-los em uma lista
coeficientes <- lapply(resultados$modelo, coefficients)

# Converter a lista em um data frame para facilitar a manipulação
df_coeficientes <- do.call(rbind, coeficientes)

# Calcular a média e os intervalos de confiança dos coeficientes
media_coeficientes <- colMeans(df_coeficientes)
ic_coeficientes <- apply(df_coeficientes, 2, function(x) quantile(x, c(0.025, 0.975)))

# Imprimir os resultados
print(paste("Equação média de regressão: y =", round(media_coeficientes[1], 2), "+", round(media_coeficientes[2], 2), "*x"))
print(paste("Intervalo de confiança para o intercepto: [", round(ic_coeficientes[1, 1], 2), ",", round(ic_coeficientes[2, 1], 2), "]"))
print(paste("Intervalo de confiança para a inclinação: [", round(ic_coeficientes[1, 2], 2), ",", round(ic_coeficientes[2, 2], 2), "]"))

# Média dos coeficientes
intercepto <- media_coeficientes[1]
inclinação <- media_coeficientes[2]

# Criando o gráfico de dispersão e adicionando a linha de regressão
ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  geom_abline(intercept = intercepto, slope = inclinação, color = "black") +
  labs(x = "Coordenada X", y = "Coordenada Y", title = "Regressão Ortogonal Média", color = "Ponto") +
  theme_minimal()


# Calcular o centro de gravidade por 'ponto'
centros_gravidade <- repeticoes_geradas %>%
  group_by(ponto) %>%
  summarise(centro_gravidade_x = mean(x), centro_gravidade_y = mean(y))

# Biblioteca necessária
library(dplyr)

# Calcular o ponto na reta de regressão mais próximo do centro de gravidade
centros_gravidade <- centros_gravidade %>%
  mutate(
    projeção_x = (centro_gravidade_x + inclinação * centro_gravidade_y - inclinação * intercepto) / (1 + inclinação ^ 2),
    projeção_y = inclinação * projeção_x + intercepto
  )

ggplot(repeticoes_geradas, aes(x = x, y = y, color = as.factor(ponto))) +
  geom_point(size = 3) +
  geom_abline(intercept = intercepto, slope = inclinação, color = "black") +
  geom_point(data = centros_gravidade, aes(x = projeção_x, y = projeção_y), color = "darkgray", shape = 8, size = 3) +
  labs(x = "Coordenada X", y = "Coordenada Y", color = "Grupos") 

# Distância entre eixos
l <- 2500  # mm

Ax <- as.numeric(centros_gravidade[1, 'projeção_x'])
Ay <- as.numeric(centros_gravidade[1, 'projeção_y'])
Bx <- as.numeric(centros_gravidade[2, 'projeção_x'])
By <- as.numeric(centros_gravidade[2, 'projeção_y'])
Cx <- as.numeric(centros_gravidade[3, 'projeção_x'])
Cy <- as.numeric(centros_gravidade[3, 'projeção_y'])
Dx <- as.numeric(centros_gravidade[4, 'projeção_x'])
Dy <- as.numeric(centros_gravidade[4, 'projeção_y'])

distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)

# Ordenar os dados por 'serie' e 'ponto'
repeticoes_geradas <- repeticoes_geradas %>%
  arrange(serie, ponto)

# Agrupar os dados por 'serie' e aplicar a função 'distancia' a cada grupo
resultados <- repeticoes_geradas %>%
  group_by(serie) %>%
  summarise(Ax = x[1], Ay = y[1], Bx = x[2], By = y[2], Cx = x[3], Cy = y[3], Dx = x[4], Dy = y[4]) %>%
  rowwise() %>%
  mutate(distancia = distancia(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy, l)) %>%
  ungroup()

# Remove valores NA da coluna 'distancia'
distancia_sem_na <- na.omit(resultados$distancia)

# Mediana da coluna 'distancia'
mediana <- median(distancia_sem_na)
print(paste("Mediana da distância: ", round(mediana, 2)))

# Percentis 2.5% e 97.5%
percentis <- quantile(distancia_sem_na, c(0.025, 0.975))
print(paste("Percentil 2.5%: ", round(percentis[1], 2)))
print(paste("Percentil 97.5%: ", round(percentis[2], 2)))
