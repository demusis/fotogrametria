"""
Painel de resultados com gráficos matplotlib embarcados no Qt.

Gráficos:
1. Tom de cinza ao longo da trajetória
2. Densidade dos pontos simulados (hex)
3. Velocidade estimada (histograma + boxplot)
4. Deslocamento estimado (ECDF)

Suporta temas claro e escuro.
"""

from __future__ import annotations

import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QLabel, QScrollArea, QFrame,
)
from PySide6.QtGui import QFont
from PySide6.QtCore import Qt

from ..core.processing import ResultadoProcessamento

# Paleta de cores acessível (Paul Tol)
PALETA = ["#EE7733", "#0077BB", "#33BBEE", "#EE3377", "#CC3311", "#009988", "#BBBBBB"]
LETRAS = ["A", "B", "C", "D"]

# Temas matplotlib
THEMES = {
    "dark": {
        "bg": "#0e1117",
        "face": "#161b22",
        "text": "#e0e0e0",
        "grid": "#30363d",
        "scatter": "white",
        "line_reg": "white",
        "qt_bg": "#0e1117",
        "qt_text": "#e0e0e0",
        "title_color": "#33BBEE",
        "desc_color": "#888888",
        "placeholder_color": "#555555",
        "hist_edge": "#333333",
    },
    "light": {
        "bg": "#ffffff",
        "face": "#f8f9fa",
        "text": "#333333",
        "grid": "#dee2e6",
        "scatter": "#333333",
        "line_reg": "#333333",
        "qt_bg": "#ffffff",
        "qt_text": "#333333",
        "title_color": "#0077BB",
        "desc_color": "#666666",
        "placeholder_color": "#999999",
        "hist_edge": "#aaaaaa",
    },
}


def _estilizar_eixo(ax, theme: dict):
    """Aplica estilo a um eixo matplotlib."""
    ax.set_facecolor(theme["face"])
    ax.tick_params(colors=theme["text"], labelsize=9)
    ax.xaxis.label.set_color(theme["text"])
    ax.yaxis.label.set_color(theme["text"])
    ax.title.set_color(theme["text"])
    for spine in ax.spines.values():
        spine.set_color(theme["grid"])
    ax.grid(True, alpha=0.2, color=theme["grid"])


class ChartWidget(QWidget):
    """Widget contendo um título e um canvas matplotlib."""

    def __init__(self, titulo: str, descricao: str = "", theme: dict | None = None, parent=None):
        super().__init__(parent)
        t = theme or THEMES["dark"]

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 16, 0, 0)

        self._lbl_titulo = QLabel(titulo)
        self._lbl_titulo.setFont(QFont("Segoe UI", 13, QFont.Weight.Bold))
        self._lbl_titulo.setStyleSheet(f"color: {t['title_color']};")
        layout.addWidget(self._lbl_titulo)

        self._lbl_desc = None
        if descricao:
            self._lbl_desc = QLabel(descricao)
            self._lbl_desc.setFont(QFont("Segoe UI", 9))
            self._lbl_desc.setStyleSheet(f"color: {t['desc_color']};")
            self._lbl_desc.setWordWrap(True)
            layout.addWidget(self._lbl_desc)

        self.figure = Figure(figsize=(10, 5), facecolor=t["bg"])
        self.canvas = FigureCanvas(self.figure)
        self.canvas.setMinimumHeight(350)
        layout.addWidget(self.canvas)

    def apply_theme(self, t: dict):
        self._lbl_titulo.setStyleSheet(f"color: {t['title_color']};")
        if self._lbl_desc:
            self._lbl_desc.setStyleSheet(f"color: {t['desc_color']};")
        self.figure.set_facecolor(t["bg"])
        self.canvas.draw_idle()


class ResultsPanel(QWidget):
    """Painel scrollável com todos os gráficos de resultado."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._theme_name = "dark"
        self._theme = THEMES["dark"]

        self._apply_qt_theme()

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)

        self._content = QWidget()
        self._layout = QVBoxLayout(self._content)
        self._layout.setSpacing(8)
        self._layout.setContentsMargins(20, 20, 20, 20)

        # Título principal
        self._titulo = QLabel("Resultados da Análise")
        self._titulo.setFont(QFont("Segoe UI", 18, QFont.Weight.Bold))
        self._titulo.setStyleSheet(f"color: {self._theme['title_color']};")
        self._titulo.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._layout.addWidget(self._titulo)

        # Placeholder quando não há resultados
        self._placeholder = QLabel(
            "Marque os pontos na imagem e clique em 'Calcular' para ver os resultados."
        )
        self._placeholder.setFont(QFont("Segoe UI", 11))
        self._placeholder.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._placeholder.setStyleSheet(f"color: {self._theme['placeholder_color']}; padding: 60px;")
        self._layout.addWidget(self._placeholder)

        # Charts (criados sob demanda)
        self._chart_cinza: ChartWidget | None = None
        self._chart_dispersao: ChartWidget | None = None
        self._chart_velocidade: ChartWidget | None = None
        self._chart_deslocamento: ChartWidget | None = None

        # Armazenar resultado e nc para re-render em troca de tema
        self._last_resultado: ResultadoProcessamento | None = None
        self._last_nc: float = 0.99

        self._layout.addStretch(1)

        scroll.setWidget(self._content)

        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.addWidget(scroll)

    def _apply_qt_theme(self):
        t = self._theme
        self.setStyleSheet(f"background-color: {t['qt_bg']}; color: {t['qt_text']};")

    def set_theme(self, theme_name: str):
        """Alterna o tema e re-renderiza os gráficos."""
        self._theme_name = theme_name
        self._theme = THEMES.get(theme_name, THEMES["dark"])
        self._apply_qt_theme()
        self._titulo.setStyleSheet(f"color: {self._theme['title_color']};")
        self._placeholder.setStyleSheet(f"color: {self._theme['placeholder_color']}; padding: 60px;")

        for chart in [self._chart_cinza, self._chart_dispersao, self._chart_velocidade, self._chart_deslocamento]:
            if chart:
                chart.apply_theme(self._theme)

        # Re-renderizar se houver resultados
        if self._last_resultado is not None:
            self.exibir_resultados(self._last_resultado, self._last_nc)

    def exibir_resultados(self, resultado: ResultadoProcessamento, nivel_confianca: float):
        """Atualiza todos os gráficos com os resultados do processamento."""
        self._last_resultado = resultado
        self._last_nc = nivel_confianca
        self._placeholder.hide()
        t = self._theme

        # --- 1. Tom de cinza ---
        if self._chart_cinza is None:
            self._chart_cinza = ChartWidget(
                "Tom de Cinza ao Longo da Trajetória",
                "Variação do tom de cinza ao longo da trajetória ajustada. "
                "Linhas vermelhas = posições de referência (A-D).",
                theme=t,
            )
            self._layout.insertWidget(1, self._chart_cinza)
        self._plot_cinza(resultado)

        # --- 2. Dispersão ---
        if self._chart_dispersao is None:
            self._chart_dispersao = ChartWidget(
                "Densidade dos Pontos Simulados",
                "Distribuição espacial das simulações dos centros das rodas. "
                "Áreas mais escuras = maior concentração.",
                theme=t,
            )
            self._layout.insertWidget(2, self._chart_dispersao)
        self._plot_dispersao(resultado)

        # --- 3. Velocidade ---
        if self._chart_velocidade is None:
            self._chart_velocidade = ChartWidget(
                "Velocidade Estimada",
                "Distribuição das velocidades estimadas com histograma e boxplot.",
                theme=t,
            )
            self._layout.insertWidget(3, self._chart_velocidade)
        self._plot_velocidade(resultado, nivel_confianca)

        # --- 4. Deslocamento ---
        if self._chart_deslocamento is None:
            self._chart_deslocamento = ChartWidget(
                "Deslocamento Estimado",
                "Frequência acumulada do deslocamento estimado entre os quadros (ECDF).",
                theme=t,
            )
            self._layout.insertWidget(4, self._chart_deslocamento)
        self._plot_deslocamento(resultado, nivel_confianca)

        # Refresh
        for chart in [self._chart_cinza, self._chart_dispersao, self._chart_velocidade, self._chart_deslocamento]:
            if chart:
                chart.canvas.draw_idle()

    def _plot_cinza(self, r: ResultadoProcessamento):
        t = self._theme
        fig = self._chart_cinza.figure
        fig.clear()
        fig.set_facecolor(t["bg"])
        ax = fig.add_subplot(111)
        _estilizar_eixo(ax, t)

        if len(r.cinza_rot_x) > 0:
            ax.scatter(r.cinza_rot_x, r.cinza_valores, s=1, c=t["scatter"], alpha=0.4, label="Pixels")
            if len(r.cinza_spline_x) > 0:
                ax.plot(r.cinza_spline_x, r.cinza_spline_y, color=PALETA[1], linewidth=1.5, label="Spline")

            for i, cx in enumerate(r.centros_cinza_x):
                letra = LETRAS[i] if i < 4 else str(i + 1)
                ax.axvline(cx, linestyle="--", color=PALETA[0], alpha=0.8, linewidth=1)
                ax.text(cx, ax.get_ylim()[0], f" {letra}", fontsize=11, color=PALETA[0],
                        ha="left", va="bottom", fontweight="bold")

        ax.set_xlabel("Abscissas rotacionadas", fontsize=10)
        ax.set_ylabel("Tom de Cinza", fontsize=10)
        fig.tight_layout()

    def _plot_dispersao(self, r: ResultadoProcessamento):
        t = self._theme
        fig = self._chart_dispersao.figure
        fig.clear()
        fig.set_facecolor(t["bg"])
        ax = fig.add_subplot(111)
        _estilizar_eixo(ax, t)

        if len(r.repeticoes_rotacionadas_x) > 0:
            hb = ax.hexbin(
                r.repeticoes_rotacionadas_x,
                r.repeticoes_rotacionadas_y,
                gridsize=60, cmap="Greys", mincnt=1,
            )
            cb = fig.colorbar(hb, ax=ax, label="Frequência", shrink=0.8)
            cb.ax.yaxis.label.set_color(t["text"])
            cb.ax.tick_params(colors=t["text"])

            # Reta de regressão
            x_min, x_max = ax.get_xlim()
            xs_line = np.linspace(x_min, x_max, 100)
            ys_line = r.intercepto + r.inclinacao * xs_line
            ax.plot(xs_line, ys_line, color=t["line_reg"], linewidth=1, alpha=0.7)

            # Centros de gravidade
            for i in range(len(r.centros_x)):
                letra = LETRAS[i] if i < 4 else str(i + 1)
                ax.plot(r.centros_x[i], r.centros_y[i], marker="x", color=PALETA[2],
                        markersize=8, markeredgewidth=2)
                ax.text(r.centros_x[i], r.centros_y[i], f"  {letra}", fontsize=11,
                        color=PALETA[0], fontweight="bold", va="center")

        ax.set_xlabel("Abscissas rotacionadas", fontsize=10)
        ax.set_ylabel("Ordenadas rotacionadas", fontsize=10)
        fig.tight_layout()

    def _plot_velocidade(self, r: ResultadoProcessamento, nc: float):
        t = self._theme
        fig = self._chart_velocidade.figure
        fig.clear()
        fig.set_facecolor(t["bg"])

        if len(r.velocidades) == 0:
            return

        gs = fig.add_gridspec(2, 1, height_ratios=[5, 1], hspace=0.05)
        ax1 = fig.add_subplot(gs[0])
        ax2 = fig.add_subplot(gs[1], sharex=ax1)
        _estilizar_eixo(ax1, t)
        _estilizar_eixo(ax2, t)

        media = r.vel_media
        p_inf, p_sup = r.vel_percentis

        ax1.hist(r.velocidades, bins=30, color=PALETA[6], edgecolor=t["hist_edge"], alpha=0.9)
        ax1.axvline(media, color=PALETA[5], linestyle="-", linewidth=1.5)
        ax1.axvline(p_inf, color=PALETA[0], linestyle="--", linewidth=1.2)
        ax1.axvline(p_sup, color=PALETA[0], linestyle="--", linewidth=1.2)

        y_max = ax1.get_ylim()[1]
        ax1.text(media, y_max * 0.95, f"  {media:.1f}", color=PALETA[5], fontsize=10, va="top")
        ax1.text(p_inf, y_max * 0.95, f"  {p_inf:.1f}", color=PALETA[0], fontsize=10, va="top")
        ax1.text(p_sup, y_max * 0.95, f"  {p_sup:.1f}", color=PALETA[0], fontsize=10, va="top")

        ax1.set_ylabel("Frequência", fontsize=10)
        ax1.tick_params(labelbottom=False)

        bp = ax2.boxplot(r.velocidades, vert=False, widths=0.6,
                         patch_artist=True,
                         boxprops=dict(facecolor=PALETA[6], edgecolor=t["text"]),
                         medianprops=dict(color=PALETA[5], linewidth=2),
                         whiskerprops=dict(color=t["text"]),
                         capprops=dict(color=t["text"]),
                         flierprops=dict(markeredgecolor=t["text"], markersize=3))
        ax2.set_xlabel("Velocidade (km/h)", fontsize=10)
        ax2.set_yticks([])

        fig.tight_layout()

    def _plot_deslocamento(self, r: ResultadoProcessamento, nc: float):
        t = self._theme
        fig = self._chart_deslocamento.figure
        fig.clear()
        fig.set_facecolor(t["bg"])

        if len(r.deslocamentos) == 0:
            return

        ax = fig.add_subplot(111)
        _estilizar_eixo(ax, t)

        sorted_d = np.sort(r.deslocamentos)
        ecdf_y = np.arange(1, len(sorted_d) + 1) / len(sorted_d)

        ax.plot(sorted_d, ecdf_y, color=PALETA[1], linewidth=1.5)

        media = r.desl_media
        p_inf, p_sup = r.desl_percentis
        alpha = (1 - nc) / 2

        from scipy.interpolate import interp1d
        if len(sorted_d) > 1:
            ecdf_func = interp1d(sorted_d, ecdf_y, bounds_error=False, fill_value=(0, 1))
            perc_media = float(ecdf_func(media))

            ax.axhline(perc_media, color=PALETA[5], linestyle="-", linewidth=1)
            ax.axhline(alpha, color=PALETA[0], linestyle="--", linewidth=1)
            ax.axhline(1 - alpha, color=PALETA[0], linestyle="--", linewidth=1)

            x_range = sorted_d[-1] - sorted_d[0]
            ax.text(sorted_d[0] + x_range * 0.02, perc_media + 0.02,
                    f"{media:.2f} m", color=PALETA[5], fontsize=10)
            ax.text(p_inf, alpha - 0.04,
                    f"{p_inf:.2f}", color=PALETA[0], fontsize=10, ha="center")
            ax.text(p_sup, 1 - alpha + 0.02,
                    f"{p_sup:.2f}", color=PALETA[0], fontsize=10, ha="center")

        ax.set_xlabel("Deslocamento (m)", fontsize=10)
        ax.set_ylabel("Frequência Acumulada", fontsize=10)
        fig.tight_layout()

    def get_figuras(self) -> list[tuple[str, Figure]]:
        """Retorna lista de (nome_arquivo, Figure) para exportação."""
        figuras = []
        if self._chart_cinza:
            figuras.append(("1_tom_de_cinza.png", self._chart_cinza.figure))
        if self._chart_dispersao:
            figuras.append(("2_densidade_pontos_simulados.png", self._chart_dispersao.figure))
        if self._chart_velocidade:
            figuras.append(("3_velocidade_estimada.png", self._chart_velocidade.figure))
        if self._chart_deslocamento:
            figuras.append(("4_deslocamento_estimado.png", self._chart_deslocamento.figure))
        return figuras
