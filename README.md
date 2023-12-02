# Estimação Forense de Velocidade Veicular com Base em Modelos Estatísticos e Análise de Reflectância

A aplicação aqui apresentada  visa estimar a velocidade de veículos em movimento linear com base em princípios matemáticos e estatísticos. Esta aplicação é útil em investigações forenses, estudos de engenharia de tráfego e avaliação da qualidade dos dados em diversas áreas.

**Fotogrametria:**

A fotogrametria é um conceito fundamental nesta aplicação. Ela é uma técnica que envolve a medição de objetos e distâncias a partir de imagens fotográficas. No contexto da nossa aplicação, a fotogrametria é usada para estimar a posição das rodas de um veículo com base em imagens capturadas em quadros de vídeo. Através da análise das imagens, podemos determinar com precisão as coordenadas espaciais das marcações feitas nas rodas, o que é crucial para calcular a velocidade do veículo.

A razão cruzada é um procedimento utilizado na fotogrametria e, no caso, envolve a análise das posições relativas das marcações feitas nas rodas do veículo. Supõe-se que essas marcações estejam dispostas colinearmente em relação ao deslocamento do veículo. A razão cruzada permite relacionar a posição das marcações com o tempo decorrido entre os quadros de vídeo, fornecendo uma base sólida para a estimativa da velocidade.

Marcações Temporais dos Quadros:

Para realizar a análise, a aplicação requer que o usuário forneça informações temporais relacionadas a cada quadro de vídeo selecionado. Essas informações temporais são essenciais para calcular a velocidade do veículo, uma vez que representam a variação no tempo entre os quadros. Essas marcações temporais são usadas em conjunto com as posições das marcações das rodas para estimar a velocidade escalar média do veículo de maneira precisa.

**Estimativa da Distribuição Normal Bivariada e Método de Monte Carlo:**

O processo inicia com o envio do usuário de um composição de dois quadros de vídeo correspondentes aos montentos no qual deseja-se estimar a velocidade e distância de um veículo lá presente. Cabe também ao usuários a aplicação de correções geométricas para corrigir distorções da câmera. Em seguida, na aplicação, os centros das rodas do veículo (A, B, C e D) são, repetidas vezes, identificados e marcados nas imagens, com a suposição de que essas marcações estejam dispostas colinearmente (pressuposto da razão cruzada). A qualidade do ajuste é avaliada por uma regressão ortogonal

Com esses dados, estimamos distribuições normais bivariadas que modelam conjuntamente as posições das rodas nos eixos X e Y. Essas distribuições levam em consideração a variabilidade e a covariância das posições, capturando a incerteza nas estimativas.

Em seguida, aplicamos o método de Monte Carlo, gerando um grande número de pontos simulados com base nessas distribuições. Essas simulações criam cenários virtuais nos quais as posições das rodas variam de acordo com a incerteza das estimativas. Com repetidas simulações, acumulamos uma grande quantidade de pontos que representam possíveis configurações das rodas, permitindo-nos calcular intervalos de confiança para a velocidade média do veículo e a distância entre os frames.

**Análise da Reflectância dos Centros das Rodas:**

Além da estimativa das posições das rodas, a aplicação realiza uma análise adicional da reflectância dos centros das rodas. Isso é particularmente útil quando os centros das rodas apresentam características de contraste ou refletância distintas em relação ao ambiente circundante.

**Intervalos de Confiança:**

Por fim, os intervalos de confiança são calculados com base nos pontos simulados gerados pelo método de Monte Carlo. Esses intervalos fornecem estimativas estatísticas da faixa provável para a velocidade média do veículo e a distância do veículo analisado entre os quadros.
