# Fotogrametria - Estimativa de Velocidade Veicular

Aplicativo desktop desenvolvido em Python (PySide6) para a estimativa da velocidade de veículos em movimento linear a partir da análise de imagens (fotogrametria). O sistema baseia-se em princípios matemáticos e estatísticos robustos para projetar incertezas de marcação manual em intervalos de confiança da velocidade final.

## Características Principais

* **Remoção de Viés de Confirmação**: A interface central evita exibir pontos marcados na imagem principal após cada clique, garantindo que o operador faça cada demarcação de forma independente.
* **Modelagem Estatística de Incertezas (Monte Carlo)**: Simula milhares de variações a partir das coordenadas marcadas, levando em conta o erro médio temporal e de coordenadas bivariadas, para entregar um intervalo de confiança realístico da estimativa final (ex: média de 60 km/h [55-65]).
* **Regressão de Deming (Total Least Squares)**: Diferente da regressão de Mínimos Quadrados Ordinários (OLS), a Regressão Ortogonal considera o erro em *ambos os eixos* (horizontal e vertical), ideal para trajetórias marcadas manualmente na tela.
* **Teste de Normalidade Multivariada (Mardia)**: Valida estatisticamente os grupos de marcações gerados, calculando o P-valor de assimetria e curtose para checar premissas antes da simulação.
* **Design e UX Avançados**: Temas visuais (Claro e Escuro) consistentes em toda a aplicação (incluindo gráficos matplotlib), botões de pop-up responsivos para revisar a regressão final, e exportação encapsulada de resultados (imagens em alta resolução, logs de texto puro, e dataframes do teste em CSV com separador vírgula e aspas literais).

## Como Instalar e Rodar a Aplicação

### 1. Usando o Instalador do Windows (Recomendado)

Você pode encontrar o instalador em:
`instalador/Fotogrametria_Setup_1.0.0.exe`

Esse é um pacote que instala o programa no `C:\Arquivos de Programas` de forma completamente autônoma, dispensando qualquer necessidade de ter o Python ou bibliotecas instaladas no computador.

### 2. Rodando pelo Código-Fonte

Pré-requisitos:
* Python 3.10 ou superior.

Clone este repositório e instale as dependências contidas no `requirements.txt`:

```bash
pip install -r requirements.txt
python main.py
```

## Guia Básico de Operação

O aplicativo espera a leitura de dois "frames" da posição original do veículo capturados a partir de gravações ou fotografias consecutivas no tempo.

1. **Importação**: Abra `Arquivo -> Abrir Imagem` e adicione a mesclagem da cena;
2. **Definição dos Parâmetros**: Altere os parâmetros físicos e de filmagem pela aba lateral (Distância entre eixos, Filtros, Marcadores de tempo, Simulador);
3. **Demarcação (A/B/C/D)**: 
   * A: Roda Traseira no instante 1.
   * B: Roda Dianteira no instante 1.
   * C: Roda Traseira no instante 2.
   * D: Roda Dianteira no instante 2.
   Recomenda-se formar pelo menos 7 a 8 desses ciclos a fim de estabilizar adequadamente a estatística do simulador de Monte Carlo.
4. **Resumo**: Clique em "Calcular!" para lançar a regressão, simular o tempo no processo iterativo e checar como a distribuição assinala a média da posição calculada pela Razão Cruzada na segunda aba.

Veja os detalhes matemáticos e teóricos profundos acessando o menu `Ajuda` dentro da própria aplicação final compilada (que lê a fundamentação localizada em `assets/ajuda.md`).

## Arquitetura do Projeto

* `app/core/` - Motor matemático independente e puramente procedural (`NumPy`, `SciPy`). Onde encontram-se a Deming, as simulações, rotações, o cálculo da razão cruzada e leitura dos tons de cinza.
* `app/widgets/` - Todo o aparato visual do software (`PySide6`). Trata os eventos e o encadeamento dos modais.
* `app/utils/` - Utilitários de manipulação I/O (Exportação padronizada do zip iterativo).
* `assets/` - Marcadores estáticos (fontes, manuais em markdown lidos via MEIPASS durante compilação do binário final).

## Créditos Originais
A aplicação originou-se de um algoritmo/dashboard na linguagem R construído pelo estatístico e perito oficial criminal **Carlo Ralph De Musis**. Esta versão foi portada, aprimorada e convertida para um aplicativo Windows com tecnologias modernas em Python.
