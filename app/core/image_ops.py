"""
Operações de processamento de imagem.

- Filtro gaussiano isotrópico
- Extração de tons de cinza ao longo de uma reta
"""

import numpy as np
import cv2
from PIL import Image


def carregar_imagem(caminho: str) -> np.ndarray:
    """Carrega imagem como array RGB normalizado [0, 1].

    Parâmetros
    ----------
    caminho : str — caminho do arquivo (jpg/png)

    Retorna
    -------
    ndarray de shape (H, W, 3) com valores float64 em [0, 1]
    """
    img = Image.open(caminho).convert("RGB")
    return np.asarray(img, dtype=np.float64) / 255.0


def aplicar_filtro_gaussiano(img: np.ndarray, sigma: float) -> np.ndarray:
    """Aplica filtro gaussiano isotrópico à imagem.

    Parâmetros
    ----------
    img   : ndarray (H, W, 3) float64 [0, 1]
    sigma : desvio padrão do filtro (0 = sem filtro)

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] suavizada
    """
    if sigma <= 0:
        return img.copy()

    # ksize precisa ser ímpar; 0 faz o OpenCV calcular automaticamente
    img_uint8 = (img * 255).astype(np.uint8)
    suavizada = cv2.GaussianBlur(img_uint8, (0, 0), sigmaX=sigma, sigmaY=sigma)
    return suavizada.astype(np.float64) / 255.0


def extrair_cinza_ao_longo_da_reta(
    img: np.ndarray, beta_0: float, beta_1: float
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Extrai o tom de cinza médio dos pixels ao longo de uma reta y = beta_0 + beta_1 * x.

    Parâmetros
    ----------
    img    : ndarray (H, W, 3)
    beta_0 : intercepto da regressão
    beta_1 : inclinação da regressão

    Retorna
    -------
    tuple (x_coords, y_coords, cinza_values)
    """
    altura, largura = img.shape[:2]
    xs = []
    ys = []
    cinzas = []

    for x in range(largura):
        y_pred = beta_0 + beta_1 * x
        y_int = int(round(y_pred))
        if 0 <= y_int < altura:
            # Tom de cinza como média dos 3 canais
            cinza = float(np.mean(img[y_int, x, :3]))
            xs.append(x)
            ys.append(y_int)
            cinzas.append(cinza)

    return np.array(xs), np.array(ys), np.array(cinzas)


def rotacionar_pontos(x: np.ndarray, y: np.ndarray, angulo: float) -> tuple[np.ndarray, np.ndarray]:
    """Aplica rotação 2D a arrays de coordenadas.

    Parâmetros
    ----------
    x, y   : arrays de coordenadas
    angulo : ângulo de rotação em radianos

    Retorna
    -------
    tuple (x_rot, y_rot)
    """
    cos_a = np.cos(angulo)
    sin_a = np.sin(angulo)
    x_rot = x * cos_a - y * sin_a
    y_rot = x * sin_a + y * cos_a
    return x_rot, y_rot


def cor_pixel_hex(img: np.ndarray, x: int, y: int) -> str:
    """Retorna a cor do pixel (x, y) como string hexadecimal #RRGGBB."""
    altura, largura = img.shape[:2]
    if 0 <= x < largura and 0 <= y < altura:
        r, g, b = img[y, x, :3]
        return f"#{int(r * 255):02x}{int(g * 255):02x}{int(b * 255):02x}"
    return "#000000"


def cinza_pixel(img: np.ndarray, x: int, y: int) -> float:
    """Retorna o tom de cinza médio do pixel (x, y)."""
    altura, largura = img.shape[:2]
    if 0 <= x < largura and 0 <= y < altura:
        return float(np.mean(img[y, x, :3]))
    return 0.0
