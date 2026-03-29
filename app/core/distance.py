"""
Cálculo de distância por razão cruzada (cross-ratio) para fotogrametria.

Replica a função `distancia()` do R original, utilizando aritmética complexa
para calcular a distância percorrida pelo veículo entre dois quadros.
"""

import numpy as np


def distancia_cross_ratio(
    ax: float, ay: float,
    bx: float, by: float,
    cx: float, cy: float,
    dx: float, dy: float,
    l: float,
) -> float:
    """Calcula a distância usando razão cruzada com aritmética de números complexos.

    Parâmetros
    ----------
    ax, ay : coordenadas do ponto A (roda traseira, quadro 1)
    bx, by : coordenadas do ponto B (roda dianteira, quadro 1)
    cx, cy : coordenadas do ponto C (roda traseira, quadro 2)
    dx, dy : coordenadas do ponto D (roda dianteira, quadro 2)
    l      : distância de referência entre eixos (em km)

    Retorna
    -------
    float : distância estimada (em km)
    """
    A = complex(ax, ay)
    B = complex(bx, by)
    C = complex(cx, cy)
    D = complex(dx, dy)

    dAC = C - A
    dBC = C - B
    dAD = D - A
    dBD = D - B

    # Evitar divisão por zero
    if abs(dBC) < 1e-15 or abs(dBD) < 1e-15:
        return np.nan

    k = (dAC / dBC) / (dAD / dBD)

    if abs(k - 1.0) < 1e-15:
        return np.nan

    d_c = np.sqrt(k / (k - 1) * l**2)
    d = np.sqrt(d_c.real**2 + d_c.imag**2)

    return float(d)


def distancia_vetorizada(
    pontos: np.ndarray, l: float
) -> np.ndarray:
    """Calcula distância para múltiplas séries de simulação.

    Parâmetros
    ----------
    pontos : ndarray de shape (N, 8) onde cada linha é [Ax, Ay, Bx, By, Cx, Cy, Dx, Dy]
    l      : distância de referência (km)

    Retorna
    -------
    ndarray de shape (N,) com as distâncias calculadas
    """
    A = pontos[:, 0] + 1j * pontos[:, 1]
    B = pontos[:, 2] + 1j * pontos[:, 3]
    C = pontos[:, 4] + 1j * pontos[:, 5]
    D = pontos[:, 6] + 1j * pontos[:, 7]

    dAC = C - A
    dBC = C - B
    dAD = D - A
    dBD = D - B

    # Evitar divisão por zero
    with np.errstate(divide="ignore", invalid="ignore"):
        k = (dAC / dBC) / (dAD / dBD)
        d_c = np.sqrt(k / (k - 1) * l**2)
        d = np.sqrt(d_c.real**2 + d_c.imag**2)

    return np.where(np.isfinite(d), d, np.nan)
