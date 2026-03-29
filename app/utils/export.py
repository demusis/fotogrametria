"""
Utilitários de exportação: salvar gráficos, dados e parâmetros em ZIP.

CSVs utilizam vírgula como separador e campos de texto entre aspas.
"""

import io
import csv
import zipfile
import datetime
from pathlib import Path

import numpy as np
from matplotlib.figure import Figure


def salvar_figura_para_bytes(fig: Figure, dpi: int = 200) -> bytes:
    """Renderiza uma Figure matplotlib para bytes PNG."""
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=dpi, bbox_inches="tight", facecolor=fig.get_facecolor())
    buf.seek(0)
    return buf.read()


def exportar_zip(
    caminho_zip: str,
    figuras: list[tuple[str, Figure]],
    dados_pontos: list[dict] | None = None,
    parametros: dict | None = None,
    imagem_original: np.ndarray | None = None,
    fig_regressao: Figure | None = None,
    texto_regressao: str | None = None,
    resultados_mardia: list[dict] | None = None,
):
    """Cria um arquivo ZIP com gráficos, dados e parâmetros.

    Parâmetros
    ----------
    caminho_zip : caminho de saída do .zip
    figuras : lista de (nome_arquivo, Figure)
    dados_pontos : lista de dicts com as coordenadas marcadas
    parametros : dict com os parâmetros utilizados
    imagem_original : array (H,W,3) da imagem original
    fig_regressao : Figure matplotlib com regressão sobre a imagem
    texto_regressao : texto do resumo da regressão
    resultados_mardia : lista de dicts com resultados do teste de Mardia por grupo
    """
    data_str = datetime.date.today().isoformat()

    with zipfile.ZipFile(caminho_zip, "w", zipfile.ZIP_DEFLATED) as zf:
        # Gráficos de resultados
        for nome, fig in figuras:
            png_bytes = salvar_figura_para_bytes(fig)
            zf.writestr(nome, png_bytes)

        # Imagem da regressão sobre a foto
        if fig_regressao is not None:
            png_bytes = salvar_figura_para_bytes(fig_regressao, dpi=200)
            zf.writestr("0_regressao_sobre_imagem.png", png_bytes)

        # Dados dos pontos (CSV com vírgula e aspas)
        if dados_pontos:
            buf = io.StringIO()
            writer = csv.DictWriter(
                buf,
                fieldnames=["ponto", "x", "y", "cor", "cinza"],
                delimiter=",",
                quoting=csv.QUOTE_NONNUMERIC,
            )
            writer.writeheader()
            for row in dados_pontos:
                writer.writerow(row)
            zf.writestr(f"pontos_{data_str}.csv", buf.getvalue())

        # Parâmetros (CSV com vírgula e aspas)
        if parametros:
            buf = io.StringIO()
            writer = csv.writer(buf, delimiter=",", quoting=csv.QUOTE_NONNUMERIC)
            writer.writerow(["parametro", "valor"])
            for k, v in parametros.items():
                writer.writerow([k, v])
            zf.writestr(f"parametros_{data_str}.csv", buf.getvalue())

        # Resumo da regressão (texto plano)
        if texto_regressao:
            zf.writestr(f"regressao_{data_str}.txt", texto_regressao)

        # Resultados do teste de Mardia (CSV com vírgula e aspas)
        if resultados_mardia:
            buf = io.StringIO()
            writer = csv.DictWriter(
                buf,
                fieldnames=["grupo", "resultado", "skewness_stat", "skewness_p", "kurtosis_stat", "kurtosis_p"],
                delimiter=",",
                quoting=csv.QUOTE_NONNUMERIC,
            )
            writer.writeheader()
            for row in resultados_mardia:
                writer.writerow(row)
            zf.writestr(f"teste_mardia_{data_str}.csv", buf.getvalue())

        # Imagem original
        if imagem_original is not None:
            from PIL import Image
            img_pil = Image.fromarray((imagem_original * 255).astype(np.uint8))
            img_buf = io.BytesIO()
            img_pil.save(img_buf, format="PNG")
            img_buf.seek(0)
            zf.writestr("imagem_original.png", img_buf.read())
