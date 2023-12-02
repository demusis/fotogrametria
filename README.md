# Estimativa Forense da Velocidade Veicular Usando Fotogrametria, Modelos Estatísticos de Distribuição Normal Bivariada e Análise de Reflectância

Este repositório GitHub oferece uma aplicação inovador para a estimativa da velocidade de veículos em movimento linear, aplicando princípios matemáticos e estatísticos. Ideal para uso em investigações forenses e estudos de engenharia de tráfego, este método se destaca por sua precisão e aplicabilidade diversificada.

Autores: Carlo Ralph De Musis, Tadeu Gross e Ozlean Dantas.

## Como Utilizar a Fotogrametria na Estimação de Velocidade:
A fotogrametria, técnica de medição de distâncias e objetos a partir de imagens fotográficas, é essencial aqui. Você pode usar imagens de vídeo para determinar precisamente as coordenadas espaciais das marcações nas rodas dos veículos. Utilizando a "razão cruzada", um conceito fundamental na fotogrametria, você pode relacionar a posição destas marcações com o tempo decorrido entre os quadros de vídeo, estabelecendo uma base para a estimativa de velocidade.

## Integração de Marcações Temporais dos Quadros:
É importante que você forneça informações temporais exatas de cada quadro de vídeo analisado. Essas informações serão utilizadas para calcular a velocidade escalar média do veículo, combinando-as com a posição das marcações nas rodas.

## Aplicação da Distribuição Normal Bivariada e Método de Monte Carlo:
Você começa enviando dois quadros de vídeo que representem momentos-chave para a estimativa de velocidade e distância. Após corrigir distorções da câmera, os centros das rodas (A, B, C, D) são identificados nas imagens. Assumindo uma disposição colinear destas marcações, a aplicação apresenta um gráfico de dispersão com uma regressão ortogonal para avaliação da qualidade do ajuste.

A aplicação utiliza modelos de distribuição normal bivariada para as posições das rodas nos eixos X e Y, considerando a variabilidade e a covariância. O método de Monte Carlo é então aplicado, gerando múltiplas simulações que refletem a incerteza nas estimativas das posições das rodas. Essas simulações ajudam a calcular intervalos de confiança para a velocidade média do veículo e a distância entre os quadros.

## Análise da Reflectância dos Centros das Rodas:
È fornecida uma análise da reflectância dos centros das rodas, útil quando estes apresentam características de contraste ou refletância distintas do ambiente circundante.

## Determinação de Intervalos de Confiança:
Os intervalos de confiança, derivados dos pontos simulados pelo método de Monte Carlo, oferecem estimativas estatísticas confiáveis da velocidade média do veículo e a distância percorrida entre os quadros.

## Instalação:
Para instalar o aplicativo em seu próprio servidor Shiny, siga as instruções a seguir:

1. Baixe o código-fonte.

2. Abra a IDE RStudio, aqui adotado como sugestão, em seu computador, certificando-se de que você tenha uma distribuição padrão do R instalada.

3. No RStudio, abra o arquivo "app.R" que você baixou do GitHub.

4. Verifique se todas as dependências necessárias, como os pacotes Shiny, rgdal, rgeos, raster, sp, shinydashboard, shinyjs e shinyWidgets, estão instaladas em seu ambiente de desenvolvimento. Caso contrário, instale-as usando o as funcionalidade do RStudio ou utilizando no terminal o comando 'install.packages'.

5. Após ter todas as dependências instaladas e o código-fonte aberto no RStudio, você pode iniciar o aplicativo simplesmente clicando no botão "Run App" no painel superior direito do RStudio. 

Para fins de demonstração, a aplicação está disponível online no site https://demusis.shinyapps.io/fotogrametria/.

## Referência:
Rubinstein, R. Y., & Kroese, D. P. (2016). Simulation and the Monte Carlo Method. Wiley Series in Probability and Statistics. John Wiley & Sons, Inc.
