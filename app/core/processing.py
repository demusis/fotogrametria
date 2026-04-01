"""
Módulo principal de processamento estatístico.

- Geração de repetições por distribuição normal multivariada (Monte Carlo)
- Regressão ortogonal (Deming)
- Teste de normalidade multivariada (Mardia)
- Cálculo de velocidade e deslocamento
"""

from __future__ import annotations

import numpy as np
from scipy import stats
from scipy.interpolate import UnivariateSpline
from dataclasses import dataclass, field

from .distance import distancia_vetorizada
from .image_ops import rotacionar_pontos


# ---------------------------------------------------------------------------
# Regressão Deming (ortogonal)
# ---------------------------------------------------------------------------

def regressao_deming(x: np.ndarray, y: np.ndarray, ratio: float = 1.0) -> tuple[float, float]:
    """Regressão Deming (Total Least Squares quando ratio=1).

    Parâmetros
    ----------
    x, y  : arrays de dados
    ratio : razão das variâncias de erro (delta = var_y / var_x). 1 = ortogonal.

    Retorna
    -------
    (intercepto, inclinação)
    """
    n = len(x)
    x_mean = np.mean(x)
    y_mean = np.mean(y)

    sxx = np.sum((x - x_mean) ** 2) / (n - 1)
    syy = np.sum((y - y_mean) ** 2) / (n - 1)
    sxy = np.sum((x - x_mean) * (y - y_mean)) / (n - 1)

    # Inclinação da regressão Deming
    b1 = (syy - ratio * sxx + np.sqrt((syy - ratio * sxx) ** 2 + 4 * ratio * sxy ** 2)) / (2 * sxy)

    # Intercepto
    b0 = y_mean - b1 * x_mean

    return float(b0), float(b1)


# ---------------------------------------------------------------------------
# Teste de Mardia para normalidade multivariada
# ---------------------------------------------------------------------------

@dataclass
class MardiaResult:
    """Resultado do teste de Mardia."""
    skewness_stat: float
    skewness_p: float
    kurtosis_stat: float
    kurtosis_p: float

    @property
    def is_normal(self) -> bool:
        return self.skewness_p > 0.05 and self.kurtosis_p > 0.05

    @property
    def label(self) -> str:
        return "✓ Normal" if self.is_normal else "✗ Não normal"


def teste_mardia(data: np.ndarray) -> MardiaResult:
    """Teste de Mardia para normalidade multivariada.

    Parâmetros
    ----------
    data : ndarray de shape (n, p) — n observações, p variáveis

    Retorna
    -------
    MardiaResult
    """
    n, p = data.shape
    mean = np.mean(data, axis=0)
    centered = data - mean
    S = np.cov(data, rowvar=False, ddof=1)

    try:
        S_inv = np.linalg.inv(S)
    except np.linalg.LinAlgError:
        return MardiaResult(0, 0, 0, 0)

    # Distâncias de Mahalanobis: D_ij = (x_i - mean) @ S_inv @ (x_j - mean)^T
    D = centered @ S_inv @ centered.T

    # Assimetria de Mardia
    b1p = np.sum(D ** 3) / (n ** 2)
    skew_stat = n * b1p / 6.0
    skew_df = p * (p + 1) * (p + 2) / 6.0
    skew_p = 1.0 - stats.chi2.cdf(skew_stat, skew_df)

    # Curtose de Mardia
    b2p = np.trace(D ** 2) / n
    kurt_expected = p * (p + 2)
    kurt_var = 8.0 * p * (p + 2) / n
    if kurt_var > 0:
        kurt_stat = (b2p - kurt_expected) / np.sqrt(kurt_var)
    else:
        kurt_stat = 0.0
    kurt_p = 2.0 * (1.0 - stats.norm.cdf(abs(kurt_stat)))

    return MardiaResult(
        skewness_stat=float(skew_stat),
        skewness_p=float(skew_p),
        kurtosis_stat=float(kurt_stat),
        kurtosis_p=float(kurt_p),
    )


# ---------------------------------------------------------------------------
# Geração de repetições Monte Carlo
# ---------------------------------------------------------------------------

def gerar_repeticoes(
    pontos_x: dict[int, np.ndarray],
    pontos_y: dict[int, np.ndarray],
    n_repeticoes: int,
) -> dict[int, np.ndarray]:
    """Gera repetições via distribuição normal bivariada para cada grupo de pontos.

    Parâmetros
    ----------
    pontos_x : dict {grupo: array de x}
    pontos_y : dict {grupo: array de y}
    n_repeticoes : número de simulações

    Retorna
    -------
    dict {grupo: ndarray (n_repeticoes, 2)} — colunas [x, y]
    """
    repeticoes = {}
    for grupo in pontos_x:
        x = pontos_x[grupo]
        y = pontos_y[grupo]
        if len(x) < 2:
            continue

        mean = np.array([np.mean(x), np.mean(y)])
        data = np.column_stack([x, y])
        cov_matrix = np.cov(data, rowvar=False)

        # Verificar validade
        if not np.all(np.isfinite(cov_matrix)):
            continue

        try:
            samples = np.random.multivariate_normal(mean, cov_matrix, size=n_repeticoes)
        except (np.linalg.LinAlgError, ValueError):
            continue

        repeticoes[grupo] = samples

    return repeticoes


# ---------------------------------------------------------------------------
# Resultado do processamento completo
# ---------------------------------------------------------------------------

@dataclass
class ResultadoProcessamento:
    """Contém todos os resultados da análise."""
    # Dados rotacionados para gráficos
    repeticoes_rotacionadas_x: np.ndarray = field(default_factory=lambda: np.array([]))
    repeticoes_rotacionadas_y: np.ndarray = field(default_factory=lambda: np.array([]))
    repeticoes_grupos: np.ndarray = field(default_factory=lambda: np.array([]))  # label do grupo (int)

    # Centros de gravidade rotacionados
    centros_x: np.ndarray = field(default_factory=lambda: np.array([]))
    centros_y: np.ndarray = field(default_factory=lambda: np.array([]))

    # Reta de regressão final
    intercepto: float = 0.0
    inclinacao: float = 0.0

    # Deslocamento e velocidade
    deslocamentos: np.ndarray = field(default_factory=lambda: np.array([]))  # em metros
    velocidades: np.ndarray = field(default_factory=lambda: np.array([]))    # em km/h

    # Cinza ao longo da trajetória
    cinza_rot_x: np.ndarray = field(default_factory=lambda: np.array([]))
    cinza_valores: np.ndarray = field(default_factory=lambda: np.array([]))
    cinza_spline_x: np.ndarray = field(default_factory=lambda: np.array([]))
    cinza_spline_y: np.ndarray = field(default_factory=lambda: np.array([]))
    centros_cinza_x: np.ndarray = field(default_factory=lambda: np.array([]))

    # Coeficientes da regressão sobre os dados originais
    coef_original_b0: float = 0.0
    coef_original_b1: float = 0.0

    # Estatísticas
    vel_media: float = 0.0
    vel_percentis: tuple[float, float] = (0.0, 0.0)
    desl_media: float = 0.0
    desl_percentis: tuple[float, float] = (0.0, 0.0)

    # Erros / avisos
    avisos: list[str] = field(default_factory=list)


def processar(
    pontos_x: dict[int, np.ndarray],
    pontos_y: dict[int, np.ndarray],
    n_repeticoes: int,
    dist_referencia_mm: float,
    inicio_quadro: float,
    fim_quadro: float,
    erro_medio_mt: float,
    dp_erro_medio_mt: float,
    nivel_confianca: float,
    img: np.ndarray | None = None,
    sigma_filtro: float = 0.0,
    callback_progresso=None,
) -> ResultadoProcessamento:
    """Executa o processamento completo de Monte Carlo.

    Parâmetros
    ----------
    pontos_x, pontos_y : dicts {grupo (1-4): array de coordenadas}
    n_repeticoes : nº de simulações Monte Carlo
    dist_referencia_mm : distância entre eixos em mm
    inicio_quadro, fim_quadro : tempo dos quadros (s)
    erro_medio_mt : erro médio da marcação temporal (ms/s)
    dp_erro_medio_mt : desvio padrão do erro (ms/s)
    nivel_confianca : nível de confiança (0-1)
    img : imagem (H,W,3) para análise de cinza (opcional)
    sigma_filtro : sigma do filtro gaussiano
    callback_progresso : callable(float) para atualizar progresso

    Retorna
    -------
    ResultadoProcessamento
    """
    from .image_ops import aplicar_filtro_gaussiano, extrair_cinza_ao_longo_da_reta

    resultado = ResultadoProcessamento()

    def progresso(valor: float):
        if callback_progresso:
            callback_progresso(valor)

    progresso(0.05)

    # ---------- 1. Regressão Deming sobre os dados originais ----------
    all_x = np.concatenate([pontos_x[g] for g in sorted(pontos_x)])
    all_y = np.concatenate([pontos_y[g] for g in sorted(pontos_y)])
    b0_orig, b1_orig = regressao_deming(all_x, all_y)
    resultado.coef_original_b0 = b0_orig
    resultado.coef_original_b1 = b1_orig

    progresso(0.10)

    # ---------- 2. Gerar repetições (Monte Carlo) ----------
    repeticoes = gerar_repeticoes(pontos_x, pontos_y, n_repeticoes)

    if len(repeticoes) < 4:
        resultado.avisos.append("Não foi possível gerar repetições para todos os 4 grupos.")
        return resultado

    progresso(0.25)

    # ---------- 3. Regressões Deming sobre cada série ----------
    coeficientes = []
    for i in range(n_repeticoes):
        try:
            xs = np.array([repeticoes[g][i, 0] for g in sorted(repeticoes)])
            ys = np.array([repeticoes[g][i, 1] for g in sorted(repeticoes)])
            b0, b1 = regressao_deming(xs, ys)
            coeficientes.append((b0, b1))
        except Exception:
            continue

    if not coeficientes:
        resultado.avisos.append("Nenhum modelo Deming pôde ser ajustado.")
        return resultado

    coefs = np.array(coeficientes)
    media_b0 = float(np.mean(coefs[:, 0]))
    media_b1 = float(np.mean(coefs[:, 1]))

    progresso(0.45)

    # ---------- 4. Centros de gravidade ----------
    centros = {}
    for g in sorted(repeticoes):
        centros[g] = np.mean(repeticoes[g], axis=0)  # [mean_x, mean_y]

    # Projeção na reta de regressão
    centros_proj = {}
    for g, (cx, cy) in centros.items():
        px = (cx + media_b1 * cy - media_b1 * media_b0) / (1 + media_b1**2)
        py = media_b1 * px + media_b0
        centros_proj[g] = (px, py)

    progresso(0.50)

    # ---------- 5. Rotação ----------
    angulo_rot = -np.arctan(media_b1)

    # Rotacionar todas as repetições
    all_rep_x = []
    all_rep_y = []
    all_rep_grupo = []
    for g in sorted(repeticoes):
        rx, ry = rotacionar_pontos(repeticoes[g][:, 0], repeticoes[g][:, 1], angulo_rot)
        all_rep_x.append(rx)
        all_rep_y.append(ry)
        all_rep_grupo.append(np.full(len(rx), g))

    resultado.repeticoes_rotacionadas_x = np.concatenate(all_rep_x)
    resultado.repeticoes_rotacionadas_y = np.concatenate(all_rep_y)
    resultado.repeticoes_grupos = np.concatenate(all_rep_grupo)

    # Intercepto e inclinação pós-rotação
    resultado.intercepto = float(np.mean(resultado.repeticoes_rotacionadas_y))
    resultado.inclinacao = 0.0

    # Centros de gravidade rotacionados
    centros_rx = []
    centros_ry = []
    for g in sorted(centros_proj):
        px, py = centros_proj[g]
        rx, ry = rotacionar_pontos(np.array([px]), np.array([py]), angulo_rot)
        centros_rx.append(float(rx[0]))
        centros_ry.append(float(ry[0]))
    resultado.centros_x = np.array(centros_rx)
    resultado.centros_y = np.array(centros_ry)

    progresso(0.60)

    # ---------- 6. Cálculo de distância e velocidade ----------
    l_km = dist_referencia_mm / 1_000_000.0  # mm → km
    dt_s = fim_quadro - inicio_quadro  # s
    dt_h = dt_s / 3600.0  # h

    # Construir array (N, 8) para distância vetorizada
    pontos_series = np.column_stack([
        repeticoes[1][:, 0], repeticoes[1][:, 1],  # A
        repeticoes[2][:, 0], repeticoes[2][:, 1],  # B
        repeticoes[3][:, 0], repeticoes[3][:, 1],  # C
        repeticoes[4][:, 0], repeticoes[4][:, 1],  # D
    ])

    distancias_km = distancia_vetorizada(pontos_series, l_km)

    # Ruído temporal
    ruido = np.random.normal(
        loc=dt_s * erro_medio_mt,
        scale=abs(dt_s) * dp_erro_medio_mt,
        size=len(distancias_km)
    ) / 1000.0  # ms → s, depois convertido a horas

    ruido_h = ruido / 3600.0

    # Velocidade (km/h)
    with np.errstate(divide="ignore", invalid="ignore"):
        velocidades = distancias_km / (dt_h + ruido_h)

    # Remover NaN/Inf
    mask = np.isfinite(velocidades) & (velocidades > 0)
    velocidades = velocidades[mask]
    distancias_m = distancias_km[mask] * 1000  # km → m

    resultado.deslocamentos = distancias_m
    resultado.velocidades = velocidades

    # Estatísticas
    if len(velocidades) > 0:
        alpha = (1 - nivel_confianca) / 2
        resultado.vel_media = float(np.mean(velocidades))
        resultado.vel_percentis = (
            float(np.quantile(velocidades, alpha)),
            float(np.quantile(velocidades, 1 - alpha)),
        )
        resultado.desl_media = float(np.mean(distancias_m))
        resultado.desl_percentis = (
            float(np.quantile(distancias_m, alpha)),
            float(np.quantile(distancias_m, 1 - alpha)),
        )

    progresso(0.75)

    # ---------- 7. Tom de cinza ao longo da trajetória ----------
    if img is not None:
        img_filtrada = aplicar_filtro_gaussiano(img, sigma_filtro)
        xs_cinza, ys_cinza, cinzas = extrair_cinza_ao_longo_da_reta(
            img_filtrada, b0_orig, b1_orig
        )

        if len(xs_cinza) > 4:
            # Rotacionar as coordenadas x para alinhá-las
            rot_x, _ = rotacionar_pontos(xs_cinza.astype(float), ys_cinza.astype(float), angulo_rot)
            resultado.cinza_rot_x = rot_x
            resultado.cinza_valores = cinzas

            # Spline suavizado
            try:
                sorted_idx = np.argsort(rot_x)
                rx_sorted = rot_x[sorted_idx]
                c_sorted = cinzas[sorted_idx]
                spline = UnivariateSpline(rx_sorted, c_sorted, s=len(rx_sorted) * 0.001)
                sp_x = np.linspace(rx_sorted[0], rx_sorted[-1], 500)
                sp_y = spline(sp_x)
                resultado.cinza_spline_x = sp_x
                resultado.cinza_spline_y = sp_y
            except Exception:
                pass

            # Centros de gravidade dos pontos originais rotacionados (para marcas no gráfico)
            cg_rot_x = []
            for g in sorted(pontos_x):
                mx = np.mean(pontos_x[g])
                my = np.mean(pontos_y[g])
                rx, _ = rotacionar_pontos(np.array([mx]), np.array([my]), angulo_rot)
                cg_rot_x.append(float(rx[0]))
            resultado.centros_cinza_x = np.array(sorted(cg_rot_x))

    progresso(1.0)

    return resultado
