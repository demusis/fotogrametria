"""
Filtros forenses de processamento de imagem.

Todos os filtros aceitam e retornam arrays numpy RGB float64 em [0, 1].
Internamente as conversões para uint8/BGR do OpenCV são feitas por helpers.

Filtros disponíveis:
    - histogram_percentile: Realce de contraste por corte de percentil L*a*b*
    - clahe: Equalização adaptativa limitada por contraste (CLAHE)
    - retinex_msr: Retinex Multi-Escala (MSR) — compressão de alcance dinâmico
    - wiener_deblur: Deconvolução de Wiener para desfoque direcional
    - white_balance: Correção de color cast pelo método Mundo Cinza (Gray World)
"""

import cv2
import numpy as np


# ---------------------------------------------------------------------------
# Helpers de conversão
# ---------------------------------------------------------------------------

def _rgb_float_to_bgr_uint8(img_rgb_float: np.ndarray) -> np.ndarray:
    """Converte RGB float64 [0,1] → BGR uint8 [0,255]."""
    return (np.clip(img_rgb_float, 0.0, 1.0) * 255).astype(np.uint8)[:, :, ::-1]


def _bgr_uint8_to_rgb_float(img_bgr_uint8: np.ndarray) -> np.ndarray:
    """Converte BGR uint8 [0,255] → RGB float64 [0,1]."""
    return img_bgr_uint8[:, :, ::-1].astype(np.float64) / 255.0


# ---------------------------------------------------------------------------
# Filtros
# ---------------------------------------------------------------------------

def histogram_percentile(
    frame_rgb_float: np.ndarray, percentile: float = 1.0
) -> np.ndarray:
    """Realce de contraste por corte de percentil no canal L* (L*a*b*).

    Parâmetros
    ----------
    frame_rgb_float : ndarray (H, W, 3) float64 [0, 1]
    percentile      : percentual de corte em ambos os extremos (0–49).
                      Valores maiores produzem um contraste mais agressivo.

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] com contraste ajustado.
    """
    if percentile <= 0:
        return frame_rgb_float.copy()

    percentile = min(percentile, 49.0)
    frame = _rgb_float_to_bgr_uint8(frame_rgb_float)
    lab = cv2.cvtColor(frame, cv2.COLOR_BGR2LAB)
    l_channel = lab[:, :, 0].astype(np.float32)

    low = np.percentile(l_channel, percentile)
    high = np.percentile(l_channel, 100 - percentile)

    if high <= low:
        return frame_rgb_float.copy()

    l_stretched = np.clip((l_channel - low) * (255.0 / (high - low)), 0, 255).astype(np.uint8)
    lab[:, :, 0] = l_stretched
    return _bgr_uint8_to_rgb_float(cv2.cvtColor(lab, cv2.COLOR_LAB2BGR))


def clahe(
    frame_rgb_float: np.ndarray,
    clip_limit: float = 2.0,
    tile_grid_size: tuple[int, int] = (8, 8),
) -> np.ndarray:
    """Equalização de Histograma Adaptativa Limitada por Contraste (CLAHE).

    Parâmetros
    ----------
    frame_rgb_float : ndarray (H, W, 3) float64 [0, 1]
    clip_limit      : limiar de contraste do CLAHE.
    tile_grid_size  : tamanho da grade de tiles.

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] equalizado.
    """
    frame = _rgb_float_to_bgr_uint8(frame_rgb_float)
    lab = cv2.cvtColor(frame, cv2.COLOR_BGR2LAB)

    clahe_obj = cv2.createCLAHE(clipLimit=clip_limit, tileGridSize=tile_grid_size)
    lab[:, :, 0] = clahe_obj.apply(lab[:, :, 0])

    return _bgr_uint8_to_rgb_float(cv2.cvtColor(lab, cv2.COLOR_LAB2BGR))


def retinex_msr(
    frame_rgb_float: np.ndarray,
    sigmas: list[float] | None = None,
) -> np.ndarray:
    """Retinex Multi-Escala (MSR) — compressão de alcance dinâmico.

    Parâmetros
    ----------
    frame_rgb_float : ndarray (H, W, 3) float64 [0, 1]
    sigmas          : lista de desvios-padrão para cada escala.
                      Padrão: [15, 80, 250].

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] com detalhes realçados.
    """
    if sigmas is None:
        sigmas = [15, 80, 250]

    frame = _rgb_float_to_bgr_uint8(frame_rgb_float)
    img = frame.astype(np.float32) + 1.0
    result = np.zeros_like(img)

    for i in range(3):
        channel = img[:, :, i]
        log_channel = np.log(channel)

        retinex = np.zeros_like(channel)
        for sigma in sigmas:
            blurred = cv2.GaussianBlur(channel, (0, 0), sigma)
            blurred = np.maximum(blurred, 1.0)
            retinex += log_channel - np.log(blurred)

        result[:, :, i] = retinex / len(sigmas)

    # Normalizar cada canal para [0, 255]
    for i in range(3):
        ch = result[:, :, i]
        min_val, max_val = np.min(ch), np.max(ch)
        if max_val > min_val:
            result[:, :, i] = (ch - min_val) / (max_val - min_val) * 255
        else:
            result[:, :, i] = 128

    return _bgr_uint8_to_rgb_float(np.clip(result, 0, 255).astype(np.uint8))


def wiener_deblur(
    frame_rgb_float: np.ndarray, angle: float = 0.0, length: int = 15, snr: float = 100.0
) -> np.ndarray:
    """Filtro de Deconvolução de Wiener para Desfoque de Movimento.

    Parâmetros
    ----------
    frame_rgb_float : ndarray (H, W, 3) float64 [0, 1]
    angle           : Ângulo do borrão em graus (0 a 180)
    length          : Extensão do borrão em pixels (> 0)
    snr             : Relação sinal-ruído (valores maiores preservam bordas)

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] restaurado.
    """
    try:
        from skimage import restoration
    except ImportError:
        print("scikit-image não encontrado. Filtro Wiener desativado.")
        return frame_rgb_float.copy()

    if length <= 1:
        return frame_rgb_float.copy()

    # Criar PSF de desfoque de movimento linear
    psf = np.zeros((length, length))
    center = length // 2
    cv2.line(psf, (0, center), (length - 1, center), 1.0, 1)
    
    M = cv2.getRotationMatrix2D((center, center), angle, 1.0)
    psf = cv2.warpAffine(psf, M, (length, length))
    
    psf_sum = np.sum(psf)
    if psf_sum > 0:
        psf /= psf_sum
    else:
        return frame_rgb_float.copy()

    balance = 1.0 / snr if snr > 0 else 0.01

    result = np.zeros_like(frame_rgb_float)
    for c in range(3):
        dec = restoration.wiener(frame_rgb_float[:, :, c], psf, balance)
        # Normalização min-max local
        min_v, max_v = np.min(dec), np.max(dec)
        if max_v > min_v:
            dec = (dec - min_v) / (max_v - min_v)
        result[:, :, c] = dec

    return np.clip(result, 0.0, 1.0)


def white_balance(
    frame_rgb_float: np.ndarray, intensity: float = 1.0
) -> np.ndarray:
    """Correção de color cast pelo método Mundo Cinza (Gray World).

    Parâmetros
    ----------
    frame_rgb_float : ndarray (H, W, 3) float64 [0, 1]
    intensity       : intensidade da correção (0 = sem efeito, 1 = total).

    Retorna
    -------
    ndarray (H, W, 3) float64 [0, 1] com balanço de branco corrigido.
    """
    intensity = max(0.0, min(1.0, intensity))
    if intensity <= 0:
        return frame_rgb_float.copy()

    frame = _rgb_float_to_bgr_uint8(frame_rgb_float)
    img = frame.astype(np.float32)

    means = [np.mean(img[:, :, c]) for c in range(3)]
    if any(m == 0 for m in means):
        return frame_rgb_float.copy()

    global_mean = sum(means) / 3.0
    gains = [global_mean / m for m in means]

    corrected = img.copy()
    for c in range(3):
        corrected[:, :, c] *= gains[c]

    if intensity < 1.0:
        corrected = img * (1 - intensity) + corrected * intensity

    return _bgr_uint8_to_rgb_float(np.clip(corrected, 0, 255).astype(np.uint8))
