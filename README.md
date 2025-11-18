<h1 align="center">Estimativa Forense da Velocidade Veicular Usando Fotogrametria, Modelos Estatísticos de Distribuição Normal Bivariada e Análise de Reflectância</h1>

<p align="center">
	<img src="https://img.shields.io/badge/R-276DC3.svg?style=default&logo=R&logoColor=white" alt="R">
</p>

<br>

Este repositório GitHub oferece uma aplicação para a estimativa da velocidade de veículos em movimento linear, aplicando princípios matemáticos e estatísticos. Ideal para uso em investigações forenses e estudos de engenharia de tráfego, este método se destaca por sua precisão e aplicabilidade diversificada.

## Capturas de Tela

Abaixo, algumas imagens ilustrativas da interface e funcionalidades da aplicação:

<p align="center">
  <img src="https://github.com/idmusis/fotogrametria/blob/main/man/figures/captura_1.png" alt="Tela inicial do aplicativo" width="600">
  <br>
  <em>Interface para upload da imagem e marcação manual dos centros das rodas.</em>
</p>

<p align="center">
  <img src="https://github.com/idmusis/fotogrametria/blob/main/man/figures/captura_3.png" alt="Gráfico de regressão ortogonal" width="600">
  <br>
  <em>Gráfico gerado após processamento de dados, e sua mensagem de ajuda correspondente.</em>
</p>

<p align="center">
  <img src="https://github.com/idmusis/fotogrametria/blob/main/man/figures/captura_4.png" alt="Gráfico de regressão ortogonal" width="600">
  <br>
  <em>Resultados gerados pela simulação Monte Carlo, estimando deslocamento e velocidade.</em>
</p>

---
## Metodologia

A aplicação estima a velocidade de um veículo a partir da análise de uma imagem contendo dois quadros da sua trajetória. O usuário marca manualmente os centros das rodas nos dois momentos. Com base nessas marcações:

- A imagem é corrigida geometricamente, considerando distorções da câmera.
- É aplicada uma regressão ortogonal para estimar o deslocamento do veículo entre os quadros.
- Em seguida, são feitas simulações por Monte Carlo: a aplicação assume que as marcações feitas pelo usuário estão sujeitas a pequenas variações (incerteza). Ao simular diversas possíveis variações, é possível estimar o impacto dessas incertezas nos resultados e calcular intervalos de confiança estatísticos.

As análises geradas incluem:

- **Análise de reflectância:** Exibe a variação do tom de cinza ao longo da trajetória. Essa visualização ajuda a validar visualmente as marcações feitas nas rodas, observando se há contraste adequado em cada ponto marcado.

- **Dispersão dos pontos simulados:** Representa a distribuição espacial das simulações dos centros das rodas. Permite avaliar a consistência das marcações e o padrão de dispersão nas simulações.

- **Deslocamento estimado:** Mostra a distância percorrida pelo veículo entre os dois quadros. O valor final inclui média, percentis e uma curva acumulada das simulações.

- **Velocidade estimada:** Calculada a partir do deslocamento e do tempo informado entre os quadros. Um histograma com boxplot mostra a distribuição da velocidade estimada, permitindo interpretar se o valor é bem definido ou sujeito a grande variação.

O aplicativo requer que o usuário forneça:
- Uma imagem contendo dois momentos da trajetória (mesmo arquivo).
- A marcação manual de quatro pontos por ciclo: roda traseira e dianteira no primeiro quadro, e novamente no segundo quadro.
- Informações de tempo de cada quadro e distância de referência real.

---

## Instalação:
O aplicativo está disponível online no [shinyapps](https://demusis.shinyapps.io/fotogrametria/). Para instalar em seu próprio servidor Shiny, siga as instruções a seguir:

1. Faça o download deste repositório.

2. Abra a IDE [RStudio](https://posit.co/downloads/), aqui adotado como sugestão, em seu computador, certificando-se de que você tenha uma distribuição padrão do R instalada.

3. No RStudio, abra qualquer um dos arquivos `global.R`, `ui.R` ou `server.R` que você baixou do GitHub.

4. Você pode iniciar o aplicativo clicando no botão "Run App" no canto superior direito do painel principal do RStudio. Ele será executado após a instalação de dependências, se necessário.

Um pequeno tutorial está disponível em https://www.youtube.com/watch?v=tePeyTv-LN8.

---

## Referências:
Rubinstein, R. Y., & Kroese, D. P. (2016). Simulation and the Monte Carlo Method. Wiley Series in Probability and Statistics. John Wiley & Sons, Inc.

Wong, T. W.; Tao, C. H.; Cheng, Y. K.; Wong, K. H.; Tam, C. N. Application of cross-ratio in traffic accident reconstruction. Forensic Science International, v. 235, p. 19-23, 2014.