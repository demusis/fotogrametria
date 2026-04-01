# Fotogrametria — Manual de Operação e Modelos Estatísticos

## 1. Visão Geral

Este aplicativo estima a **velocidade de um veículo em movimento linear** a partir da análise fotogramétrica de uma imagem contendo dois quadros (frames) da sua trajetória. O método combina marcações manuais, correção geométrica e simulação estatística para produzir não apenas uma estimativa pontual, mas também intervalos de confiança robustos.

---

## 2. Fluxo de Trabalho

### 2.1 Entrada de Dados

O usuário fornece:

1. **Imagem** — arquivo JPG ou PNG contendo dois momentos (quadros) da trajetória do veículo.
2. **Marcações manuais** — cliques sobre a imagem identificando os centros das rodas:
   - **A** — Roda traseira no 1º quadro
   - **B** — Roda dianteira no 1º quadro
   - **C** — Roda traseira no 2º quadro
   - **D** — Roda dianteira no 2º quadro
3. **Parâmetros temporais** — marcação temporal de cada quadro (em segundos), erro médio e seu desvio padrão.
4. **Distância de referência** — distância real conhecida entre os centros das rodas do veículo (entre-eixos), em milímetros.

**Recomendação**: repita o ciclo A-B-C-D pelo menos **7 vezes** (28 cliques), para garantir amostras suficientes para os testes estatísticos.

### 2.2 Marcação dos Pontos

- Os pontos **não são exibidos visualmente sobre a imagem** durante a marcação. Essa decisão de projeto é intencional: a exibição dos pontos já marcados criaria um **viés de confirmação**, levando o operador a ajustar inconscientemente as marcações subsequentes em relação às anteriores.
- A tabela lateral registra todos os pontos marcados com suas coordenadas e cores.

### 2.3 Processamento

Ao clicar em **"Calcular"**, o aplicativo executa:

1. Regressão ortogonal (Deming) sobre as repetições simuladas.
2. Simulações de Monte Carlo para estimar a incerteza.
3. Cálculo de distância por razão cruzada.
4. Estimativa de velocidade com intervalos de confiança.

---

## 3. Modelos Estatísticos

### 3.1 Regressão Ortogonal (Deming)

A **regressão Deming** (também chamada de *Total Least Squares* quando a razão de variâncias é 1) é utilizada em vez da regressão ordinária (OLS) porque **ambas as variáveis (x e y) estão sujeitas a erro de medição**.

Na regressão OLS, assume-se que apenas *y* possui erro; na regressão Deming, erros em *x* e *y* são considerados simultaneamente.

**Modelo:**

```
y = β₀ + β₁·x
```

Onde a inclinação β₁ é calculada como:

```
β₁ = (S_yy − δ·S_xx + √((S_yy − δ·S_xx)² + 4δ·S_xy²)) / (2·S_xy)
```

Com:

- `S_xx = Σ(xᵢ − x̄)² / (n−1)` — variância amostral de x
- `S_yy = Σ(yᵢ − ȳ)² / (n−1)` — variância amostral de y
- `S_xy = Σ(xᵢ − x̄)(yᵢ − ȳ) / (n−1)` — covariância amostral
- `δ = σ²_ε / σ²_η` — razão das variâncias de erro (δ = 1 para regressão ortogonal)

O intercepto:

```
β₀ = ȳ − β₁·x̄
```

**Justificativa**: as marcações manuais sobre a imagem introduzem incerteza tanto na coordenada horizontal (x) quanto na vertical (y). A abordagem Deming/ortogonal é a mais adequada porque não privilegia nenhum dos dois eixos.

### 3.2 Teste de Normalidade Multivariada de Mardia

Para cada grupo de pontos (A, B, C, D), aplica-se o teste de Mardia, que verifica se as coordenadas (x, y) seguem uma **distribuição normal bivariada**. Essa é uma premissa necessária para a validade das simulações de Monte Carlo.

O teste avalia dois componentes:

#### 3.2.1 Assimetria Multivariada (Skewness)

```
b₁,p = (1/n²) · Σᵢ Σⱼ [(xᵢ − x̄)ᵀ S⁻¹ (xⱼ − x̄)]³
```

Estatística de teste: `κ₁ = n·b₁,p/6`, que segue uma distribuição χ² com `p(p+1)(p+2)/6` graus de liberdade, onde p = 2 (dimensões).

#### 3.2.2 Curtose Multivariada (Kurtosis)

```
b₂,p = (1/n) · Σᵢ [(xᵢ − x̄)ᵀ S⁻¹ (xᵢ − x̄)]²
```

Estatística de teste: `κ₂ = (b₂,p − p(p+2)) / √(8p(p+2)/n)`, que segue uma distribuição normal padrão sob H₀.

**Interpretação**:

- **p > 0.05** em ambos os testes → Não há evidência contra a normalidade bivariada (✓ Normal).
- **p ≤ 0.05** em qualquer teste → Evidência contra a normalidade (✗ Não normal).

### 3.3 Simulação de Monte Carlo

O **Método de Monte Carlo (MMC)** é utilizado para propagar as incertezas das marcações manuais até a estimativa final de velocidade.

#### 3.3.1 Geração de Repetições

Para cada grupo de pontos (A, B, C, D):

1. Calcula-se o **vetor de médias** μ = [x̄, ȳ] e a **matriz de covariância** Σ a partir das n marcações.
2. Geram-se *k* amostras aleatórias da distribuição **Normal Bivariada** N(μ, Σ).

```
(x*, y*) ~ N₂(μ, Σ)
```

Cada simulação produz uma nova configuração de 4 pontos (A*, B*, C*, D*), representando uma possível realização das marcações sob a incerteza observada.

#### 3.3.2 Distância por Cada Simulação

Para cada série simulada, calcula-se a distância usando a razão cruzada (seção 3.4).

#### 3.3.3 Velocidade por Cada Simulação

```
v = d / Δt
```

Onde Δt inclui um ruído aleatório representando a incerteza temporal:

```
Δt_efetivo = Δt + ε,  com  ε ~ N(Δt · erro_médio, |Δt| · dp_erro)
```

Esto produz uma **distribuição de velocidades** da qual se extraem:

- Média
- Percentis correspondentes ao nível de confiança escolhido

### 3.4 Cálculo de Distância por Razão Cruzada

A distância percorrida pelo veículo é calculada usando **aritmética de números complexos** para implementar a razão cruzada (*cross-ratio*) projeitva:

```
k = [(C−A)/(C−B)] / [(D−A)/(D−B)]
```

Onde A, B, C, D são representados como números complexos (x + iy).

A distância é então:

```
d_c = √(k/(k−1) · L²)
d = √(Re(d_c)² + Im(d_c)²)
```

Onde L é a distância de referência (entre-eixos do veículo).

**Justificativa**: a razão cruzada é um invariante projetivo. Isso significa que ela é preservada sob transformações projetivas (perspectiva), tornando desnecessária a calibração completa da câmera. A distância real pode ser recuperada usando apenas uma distância de referência conhecida (o entre-eixos).

### 3.5 Filtro Gaussiano Isotrópico

O filtro gaussiano é aplicado opcionalmente à imagem antes da análise de tons de cinza. Ele suaviza ruídos locais sem alterar a posição dos centros de cinza.

```
G(x, y) = (1 / 2πσ²) · exp(−(x² + y²) / 2σ²)
```

A imagem suavizada é a convolução da imagem original com o kernel gaussiano.

### 3.6 Rotação de Pontos

Para facilitar a visualização, os pontos e a reta de regressão são rotacionados para que a reta fique horizontal:

```
x' = x·cos(θ) − y·sin(θ)
y' = x·sin(θ) + y·cos(θ)
```

Onde θ = −arctan(β₁), sendo β₁ a inclinação da regressão.

---

## 4. Gráficos de Saída

### 4.1 Tom de Cinza ao Longo da Trajetória

Mostra a variação do tom de cinza (0 = preto, 1 = branco) ao longo da reta de regressão ajustada sobre a imagem. As linhas verticais tracejadas (A-D) indicam as posições dos centros de gravidade de cada grupo.

**Interpretação**: Picos ou vales distintos nas posições A-D validam a qualidade das marcações. Um contraste claro indica que as rodas foram marcadas corretamente.

### 4.2 Densidade dos Pontos Simulados

Gráfico hexagonal (hexbin) mostrando a concentração das simulações de Monte Carlo após rotação. Áreas mais escuras indicam maior concentração de simulações.

**Interpretação**: Os pontos devem formar agrupamentos compactos em torno de A, B, C e D. Dispersão excessiva indica incerteza alta nas marcações.

### 4.3 Velocidade Estimada

Histograma com boxplot mostrando a distribuição das velocidades estimadas. Inclui:

- **Linha verde sólida**: média
- **Linhas vermelhas tracejadas**: limites do intervalo de confiança

**Interpretação**: Distribuição estreita e simétrica indica alta confiança na estimativa. Assimetria ou valores extremos podem indicar problemas nas marcações.

### 4.4 Deslocamento Estimado

Curva ECDF (função de distribuição acumulada empírica) do deslocamento entre quadros. Inclui percentis e média.

**Interpretação**: Permite verificar se o deslocamento é bem definido ou se há grande variação entre simulações.

---

## 5. Exportação

O botão **"Exportar"** gera um arquivo ZIP contendo:

- Gráficos em formato PNG (alta resolução)
- Imagem da reta de regressão sobreposta à foto original
- Dados dos pontos marcados (CSV)
- Parâmetros utilizados (CSV)
- Resultados da regressão e teste de Mardia (CSV)
- Imagem original

---

## 6. Requisitos Técnicos

- **Sistema Operacional**: Windows 10/11
- **Formatos de imagem**: JPG, JPEG, PNG
- **Mínimo de marcações**: 8 pontos (2 ciclos A-B-C-D)
- **Recomendado**: ≥ 28 pontos (7 ciclos) para testes estatísticos confiáveis

---

## 7. Referências

- Deming, W. E. (1943). *Statistical Adjustment of Data*. Wiley.
- Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications. *Biometrika*, 57(3), 519-530.
- Hartley, R., & Zisserman, A. (2003). *Multiple View Geometry in Computer Vision*. Cambridge University Press.
- Robert, C. P., & Casella, G. (2004). *Monte Carlo Statistical Methods*. Springer.

---

*Versão 1.0*
