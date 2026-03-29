"""
Janela principal do aplicativo Fotogrametria.

Integra:
- ImageViewer (aba Coordenadas) — sem overlay de pontos
- ResultsPanel (aba Processamento)
- Sidebar (controles, tema, regressão popup)
- Menu Ajuda com documentação completa
"""

from __future__ import annotations

import csv
import os
import sys
from pathlib import Path

import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QHBoxLayout, QVBoxLayout, QTabWidget,
    QLabel, QMessageBox, QProgressDialog, QFileDialog, QStatusBar,
    QTableWidget, QTableWidgetItem, QHeaderView, QSplitter,
    QTextEdit, QTextBrowser, QDialog, QDialogButtonBox, QScrollArea,
)
from PySide6.QtGui import QFont, QColor, QAction
from PySide6.QtCore import Qt, QThread, Signal

from .widgets.image_viewer import ImageViewer
from .widgets.sidebar import Sidebar
from .widgets.results_panel import ResultsPanel, PALETA
from .core.image_ops import carregar_imagem, cor_pixel_hex, cinza_pixel
from .core.processing import (
    processar, regressao_deming, teste_mardia, ResultadoProcessamento, MardiaResult,
)
from .utils.export import exportar_zip


# ===========================================================================
# Worker thread para processamento
# ===========================================================================

class ProcessingWorker(QThread):
    """Thread de processamento Monte Carlo."""
    progresso = Signal(float)
    concluido = Signal(object)  # ResultadoProcessamento
    erro = Signal(str)

    def __init__(self, kwargs: dict):
        super().__init__()
        self._kwargs = kwargs

    def run(self):
        try:
            resultado = processar(
                **self._kwargs,
                callback_progresso=lambda v: self.progresso.emit(v),
            )
            self.concluido.emit(resultado)
        except Exception as e:
            self.erro.emit(str(e))


# ===========================================================================
# Diálogo popup de regressão sobre imagem
# ===========================================================================

def criar_figura_regressao(
    img_array: np.ndarray, pontos: list[dict], theme: str = "dark"
) -> Figure:
    """Cria uma Figure matplotlib com regressão sobreposta à imagem.

    Retorna a Figure (sem canvas) para uso tanto no popup quanto na exportação.
    """
    if theme == "dark":
        fig_bg = "#0e1117"
        text_color = "#e0e0e0"
    else:
        fig_bg = "#ffffff"
        text_color = "#333333"

    h, w = img_array.shape[:2]
    aspect = h / w
    fig = Figure(figsize=(12, 12 * aspect), facecolor=fig_bg)

    ax = fig.add_subplot(111)
    ax.set_facecolor(fig_bg)

    # Exibir imagem
    ax.imshow(img_array, origin="upper", extent=[0, w, h, 0], aspect="auto")

    # Pontos marcados — agrupados por grupo A/B/C/D
    letras = ["A", "B", "C", "D"]
    cores = PALETA[:4]
    markers = ["o", "^", "D", "s"]
    for grupo_idx in range(1, 5):
        pts = [p for p in pontos if p["ponto"] == grupo_idx]
        if pts:
            xs = [p["x"] for p in pts]
            ys = [p["y"] for p in pts]
            ax.scatter(xs, ys, c=cores[grupo_idx - 1], marker=markers[grupo_idx - 1],
                       s=60, alpha=0.7, edgecolors="white", linewidth=0.5,
                       label=letras[grupo_idx - 1], zorder=5)

    # Regressão OLS
    all_x = np.array([p["x"] for p in pontos], dtype=float)
    all_y = np.array([p["y"] for p in pontos], dtype=float)

    if len(all_x) >= 2:
        coeffs = np.polyfit(all_x, all_y, 1)
        b1, b0 = coeffs[0], coeffs[1]

        x_line = np.linspace(0, w, 500)
        y_line = b0 + b1 * x_line
        mask = (y_line >= 0) & (y_line <= h)
        ax.plot(x_line[mask], y_line[mask], color=PALETA[4], linewidth=2,
                linestyle="--", alpha=0.9, label="Regressao", zorder=4)

    ax.set_xlim(0, w)
    ax.set_ylim(h, 0)
    ax.set_xlabel("")
    ax.set_ylabel("")
    ax.tick_params(colors=text_color, labelsize=8)
    ax.legend(loc="upper right", fontsize=9, framealpha=0.7)

    for spine in ax.spines.values():
        spine.set_visible(False)

    fig.tight_layout()
    return fig


class RegressionDialog(QDialog):
    """Popup com gráfico de regressão sobreposto à imagem.

    Sem botão X na barra de título — fecha apenas pelo botão 'Fechar'.
    """

    def __init__(self, img_array: np.ndarray, pontos: list[dict], theme: str = "dark", parent=None):
        super().__init__(parent)
        self.setWindowTitle("Regressao sobre a Imagem")
        # Remove o botão de fechar (X) da barra de título
        self.setWindowFlags(
            Qt.WindowType.Dialog
            | Qt.WindowType.WindowTitleHint
            | Qt.WindowType.CustomizeWindowHint
        )
        self.setMinimumSize(900, 600)
        self.resize(1000, 700)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)

        if theme == "dark":
            bg = "#0e1117"
            text_color = "#e0e0e0"
            self.setStyleSheet(f"background-color: {bg}; color: {text_color};")
        else:
            bg = "#ffffff"
            text_color = "#333333"
            self.setStyleSheet(f"background-color: {bg}; color: {text_color};")

        # Criar figura
        fig = criar_figura_regressao(img_array, pontos, theme)
        canvas = FigureCanvas(fig)
        canvas.setMinimumHeight(400)
        layout.addWidget(canvas)
        canvas.draw()

        # Botão fechar
        btn_box = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        btn_box.rejected.connect(self.close)
        btn_box.setStyleSheet(f"color: {text_color};")
        layout.addWidget(btn_box)


# ===========================================================================
# Diálogo de ajuda com conteúdo markdown
# ===========================================================================

class HelpDialog(QDialog):
    """Popup de ajuda com documentação detalhada carregada de arquivo markdown."""

    def __init__(self, theme: str = "dark", parent=None):
        super().__init__(parent)
        self.setWindowTitle("Ajuda - Fotogrametria")
        self.setMinimumSize(800, 600)
        self.resize(900, 700)

        layout = QVBoxLayout(self)

        if theme == "dark":
            bg = "#0e1117"
            text_color = "#e0e0e0"
            link_color = "#33BBEE"
            self.setStyleSheet(f"background-color: {bg}; color: {text_color};")
        else:
            bg = "#ffffff"
            text_color = "#333333"
            link_color = "#0077BB"
            self.setStyleSheet(f"background-color: {bg}; color: {text_color};")

        # Título
        lbl = QLabel("Manual de Operacao e Modelos Estatisticos")
        lbl.setFont(QFont("Segoe UI", 14, QFont.Weight.Bold))
        lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lbl.setStyleSheet(f"color: {link_color};")
        layout.addWidget(lbl)

        # Carregar conteúdo markdown
        text_browser = QTextBrowser()
        text_browser.setOpenExternalLinks(True)
        text_browser.setFont(QFont("Segoe UI", 10))
        text_browser.setStyleSheet(f"""
            QTextBrowser {{
                background-color: {bg};
                color: {text_color};
                border: none;
                padding: 16px;
            }}
        """)

        md_content = self._carregar_markdown()
        text_browser.setMarkdown(md_content)
        layout.addWidget(text_browser)

        # Botão fechar
        btn_box = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        btn_box.rejected.connect(self.close)
        btn_box.setStyleSheet(f"color: {text_color};")
        layout.addWidget(btn_box)

    @staticmethod
    def _carregar_markdown() -> str:
        """Carrega o arquivo ajuda.md do diretório assets."""
        # Tentar vários caminhos possíveis (incluindo PyInstaller bundle)
        candidates = [
            Path(__file__).parent.parent / "assets" / "ajuda.md",
            Path(sys.argv[0]).parent / "assets" / "ajuda.md",
            Path("assets") / "ajuda.md",
        ]
        # PyInstaller armazena dados em sys._MEIPASS
        if hasattr(sys, '_MEIPASS'):
            candidates.insert(0, Path(sys._MEIPASS) / "assets" / "ajuda.md")
        for path in candidates:
            if path.exists():
                return path.read_text(encoding="utf-8")
        return "# Ajuda\n\nArquivo de ajuda nao encontrado.\n\nVerifique se o arquivo `assets/ajuda.md` existe."


# ===========================================================================
# Janela Principal
# ===========================================================================

LETRAS = ["A", "B", "C", "D"]

# Temas Qt
DARK_THEME = {
    "main_bg": "#0e1117",
    "menubar": "QMenuBar { background-color: #0e1117; color: #e0e0e0; } QMenuBar::item:selected { background-color: #1a1a40; } QMenu { background-color: #16213e; color: #e0e0e0; } QMenu::item:selected { background-color: #0f3460; }",
    "tab_pane": "#0e1117",
    "tab_bg": "#16213e",
    "tab_fg": "#bbbbbb",
    "tab_sel_bg": "#0e1117",
    "tab_sel_fg": "#33BBEE",
    "tab_hover": "#1a1a40",
    "info_bg": "#0e1117",
    "info_fg": "#e0e0e0",
    "accent": "#33BBEE",
    "text_edit_bg": "#1a1a2e",
    "text_edit_fg": "#e0e0e0",
    "text_edit_border": "#1a1a40",
    "table_bg": "#1a1a2e",
    "table_fg": "#e0e0e0",
    "table_grid": "#30363d",
    "table_header_bg": "#16213e",
    "table_header_fg": "#33BBEE",
    "table_header_border": "#1a1a40",
    "status_bg": "#0e1117",
    "status_fg": "#888888",
}

LIGHT_THEME = {
    "main_bg": "#f5f5f5",
    "menubar": "QMenuBar { background-color: #f5f5f5; color: #333333; } QMenuBar::item:selected { background-color: #e0e0e0; } QMenu { background-color: #ffffff; color: #333333; } QMenu::item:selected { background-color: #d0e8ff; }",
    "tab_pane": "#f5f5f5",
    "tab_bg": "#e0e0e0",
    "tab_fg": "#666666",
    "tab_sel_bg": "#f5f5f5",
    "tab_sel_fg": "#0077BB",
    "tab_hover": "#d0d0d0",
    "info_bg": "#f5f5f5",
    "info_fg": "#333333",
    "accent": "#0077BB",
    "text_edit_bg": "#ffffff",
    "text_edit_fg": "#333333",
    "text_edit_border": "#cccccc",
    "table_bg": "#ffffff",
    "table_fg": "#333333",
    "table_grid": "#dee2e6",
    "table_header_bg": "#e8e8e8",
    "table_header_fg": "#0077BB",
    "table_header_border": "#cccccc",
    "status_bg": "#f5f5f5",
    "status_fg": "#666666",
}


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Fotogrametria — Estimativa de Velocidade Veicular v1.0")
        self.setMinimumSize(1280, 720)

        # Estado
        self._img_array: np.ndarray | None = None
        self._img_path: str | None = None
        self._pontos: list[dict] = []  # lista de {ponto, x, y, cor, cinza}
        self._csv_path: str | None = None
        self._resultado: ResultadoProcessamento | None = None
        self._worker: ProcessingWorker | None = None
        self._current_theme = "dark"

        self._init_ui()
        self._connect_signals()
        self._apply_theme("dark")

    # -------------------------------------------------------------------
    # Construção da UI
    # -------------------------------------------------------------------

    def _init_ui(self):
        # Menu bar
        self._menubar = self.menuBar()

        menu_arquivo = self._menubar.addMenu("Arquivo")
        act_abrir = QAction("Abrir Imagem...", self)
        act_abrir.setShortcut("Ctrl+O")
        act_abrir.triggered.connect(self._abrir_imagem_dialog)
        menu_arquivo.addAction(act_abrir)

        act_exportar = QAction("Exportar Resultados...", self)
        act_exportar.setShortcut("Ctrl+S")
        act_exportar.triggered.connect(self._exportar)
        menu_arquivo.addAction(act_exportar)

        menu_arquivo.addSeparator()
        act_sair = QAction("Sair", self)
        act_sair.setShortcut("Ctrl+Q")
        act_sair.triggered.connect(self.close)
        menu_arquivo.addAction(act_sair)

        # Menu Ajuda
        menu_ajuda = self._menubar.addMenu("Ajuda")
        act_ajuda = QAction("Manual de Operacao", self)
        act_ajuda.triggered.connect(self._mostrar_ajuda)
        menu_ajuda.addAction(act_ajuda)

        act_sobre = QAction("Sobre...", self)
        act_sobre.triggered.connect(self._mostrar_sobre)
        menu_ajuda.addAction(act_sobre)

        # Central widget
        central = QWidget()
        self.setCentralWidget(central)
        main_layout = QHBoxLayout(central)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)

        # Sidebar
        self.sidebar = Sidebar()
        main_layout.addWidget(self.sidebar)

        # Área de conteúdo com tabs
        content_widget = QWidget()
        content_layout = QVBoxLayout(content_widget)
        content_layout.setContentsMargins(0, 0, 0, 0)

        self.tabs = QTabWidget()

        # --- Tab 1: Coordenadas ---
        self._tab_coords = QWidget()
        tab_layout = QVBoxLayout(self._tab_coords)

        # Splitter: imagem à esquerda, info à direita
        splitter = QSplitter(Qt.Orientation.Horizontal)

        self.image_viewer = ImageViewer()
        splitter.addWidget(self.image_viewer)

        # Painel de info
        self._info_panel = QWidget()
        info_layout = QVBoxLayout(self._info_panel)
        info_layout.setContentsMargins(12, 12, 12, 12)

        self._lbl_info = QLabel("Informações")
        self._lbl_info.setFont(QFont("Segoe UI", 14, QFont.Weight.Bold))
        info_layout.addWidget(self._lbl_info)

        self.lbl_status_marcacao = QLabel("Nenhuma imagem carregada.")
        self.lbl_status_marcacao.setFont(QFont("Segoe UI", 10))
        self.lbl_status_marcacao.setWordWrap(True)
        info_layout.addWidget(self.lbl_status_marcacao)

        # Resumo da regressão
        self._lbl_reg = QLabel("Resumo da Regressão")
        self._lbl_reg.setFont(QFont("Segoe UI", 12, QFont.Weight.Bold))
        info_layout.addWidget(self._lbl_reg)

        self.txt_regressao = QTextEdit()
        self.txt_regressao.setReadOnly(True)
        self.txt_regressao.setMaximumHeight(200)
        self.txt_regressao.setFont(QFont("Consolas", 10))
        info_layout.addWidget(self.txt_regressao)

        # Tabela de pontos
        self._lbl_tab = QLabel("Pontos Marcados")
        self._lbl_tab.setFont(QFont("Segoe UI", 12, QFont.Weight.Bold))
        info_layout.addWidget(self._lbl_tab)

        self.tabela_pontos = QTableWidget()
        self.tabela_pontos.setColumnCount(5)
        self.tabela_pontos.setHorizontalHeaderLabels(["Ponto", "X", "Y", "Cor", "Cinza"])
        self.tabela_pontos.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        info_layout.addWidget(self.tabela_pontos)

        splitter.addWidget(self._info_panel)
        splitter.setSizes([700, 400])

        tab_layout.addWidget(splitter)
        self.tabs.addTab(self._tab_coords, "📍  Coordenadas")

        # --- Tab 2: Processamento ---
        self.results_panel = ResultsPanel()
        self.tabs.addTab(self.results_panel, "📊  Processamento")

        content_layout.addWidget(self.tabs)
        main_layout.addWidget(content_widget, stretch=1)

        # Status bar
        self.statusBar().showMessage("Pronto")

    # -------------------------------------------------------------------
    # Tema
    # -------------------------------------------------------------------

    def _apply_theme(self, theme: str):
        """Aplica o tema claro ou escuro a toda a janela."""
        self._current_theme = theme
        t = DARK_THEME if theme == "dark" else LIGHT_THEME

        # Janela
        self.setStyleSheet(f"QMainWindow {{ background-color: {t['main_bg']}; }}")

        # Menu bar
        self._menubar.setStyleSheet(t["menubar"])

        # Tabs
        self.tabs.setStyleSheet(f"""
            QTabWidget::pane {{
                border: none;
                background-color: {t['tab_pane']};
            }}
            QTabBar::tab {{
                background-color: {t['tab_bg']};
                color: {t['tab_fg']};
                padding: 10px 24px;
                margin-right: 2px;
                border-top-left-radius: 8px;
                border-top-right-radius: 8px;
                font-size: 12px;
                font-weight: bold;
            }}
            QTabBar::tab:selected {{
                background-color: {t['tab_sel_bg']};
                color: {t['tab_sel_fg']};
            }}
            QTabBar::tab:hover {{
                background-color: {t['tab_hover']};
            }}
        """)

        # Tab coordenadas
        self._tab_coords.setStyleSheet(f"background-color: {t['info_bg']};")
        self._info_panel.setStyleSheet(f"background-color: {t['info_bg']};")

        # Labels
        self._lbl_info.setStyleSheet(f"color: {t['accent']};")
        self.lbl_status_marcacao.setStyleSheet(f"color: {t['info_fg']};")
        self._lbl_reg.setStyleSheet(f"color: {t['accent']}; margin-top: 12px;")
        self._lbl_tab.setStyleSheet(f"color: {t['accent']}; margin-top: 12px;")

        # Text edit
        self.txt_regressao.setStyleSheet(f"""
            QTextEdit {{
                background-color: {t['text_edit_bg']};
                color: {t['text_edit_fg']};
                border: 1px solid {t['text_edit_border']};
                border-radius: 6px;
                padding: 8px;
            }}
        """)

        # Tabela
        self.tabela_pontos.setStyleSheet(f"""
            QTableWidget {{
                background-color: {t['table_bg']};
                color: {t['table_fg']};
                border: 1px solid {t['text_edit_border']};
                border-radius: 6px;
                gridline-color: {t['table_grid']};
            }}
            QHeaderView::section {{
                background-color: {t['table_header_bg']};
                color: {t['table_header_fg']};
                font-weight: bold;
                border: 1px solid {t['table_header_border']};
                padding: 4px;
            }}
        """)

        # Status bar
        self.statusBar().setStyleSheet(f"background-color: {t['status_bg']}; color: {t['status_fg']};")

        # Image viewer
        self.image_viewer._apply_theme(theme)

        # Results panel
        self.results_panel.set_theme(theme)

    # -------------------------------------------------------------------
    # Sinais
    # -------------------------------------------------------------------

    def _connect_signals(self):
        self.sidebar.imagem_selecionada.connect(self._carregar_imagem)
        self.sidebar.csv_selecionado.connect(self._carregar_csv)
        self.sidebar.apagar_ultimo.connect(self._apagar_ultimo_ponto)
        self.sidebar.apagar_tudo.connect(self._apagar_tudo)
        self.sidebar.calcular.connect(self._calcular)
        self.sidebar.exportar.connect(self._exportar)
        self.sidebar.zoom_alterado.connect(self._zoom_alterado)
        self.sidebar.tema_alterado.connect(self._apply_theme)
        self.sidebar.ver_regressao.connect(self._mostrar_regressao_popup)
        self.image_viewer.ponto_marcado.connect(self._on_ponto_marcado)

    # -------------------------------------------------------------------
    # Ações
    # -------------------------------------------------------------------

    def _abrir_imagem_dialog(self):
        caminho, _ = QFileDialog.getOpenFileName(
            self, "Selecionar Imagem", "",
            "Imagens (*.jpg *.jpeg *.png);;Todos (*)"
        )
        if caminho:
            self.sidebar.lbl_nome_imagem.setText(Path(caminho).name)
            self._carregar_imagem(caminho)

    def _carregar_imagem(self, caminho: str):
        try:
            self._img_array = carregar_imagem(caminho)
            self._img_path = caminho
            self.image_viewer.carregar_imagem(self._img_array)
            self._pontos.clear()
            self._atualizar_info()
            h, w = self._img_array.shape[:2]
            self.statusBar().showMessage(f"Imagem carregada: {w} × {h}")
        except Exception as e:
            QMessageBox.critical(self, "Erro", f"Falha ao carregar imagem:\n{e}")

    def _carregar_csv(self, caminho: str):
        self._csv_path = caminho
        self.statusBar().showMessage(f"CSV carregado: {Path(caminho).name}")

    def _on_ponto_marcado(self, x: int, y: int):
        if self._img_array is None:
            return

        h, w = self._img_array.shape[:2]
        if x < 0 or y < 0 or x >= w or y >= h:
            self.statusBar().showMessage("Clique fora da imagem válida.")
            return

        # Ciclo 1-4
        ponto_no_ciclo = (len(self._pontos) % 4) + 1

        cor = cor_pixel_hex(self._img_array, x, y)
        cinza = cinza_pixel(self._img_array, x, y)

        self._pontos.append({
            "ponto": ponto_no_ciclo,
            "x": x,
            "y": y,
            "cor": cor,
            "cinza": round(cinza, 4),
        })

        self._atualizar_info()

    def _apagar_ultimo_ponto(self):
        if self._pontos:
            self._pontos.pop()
            self._atualizar_info()

    def _apagar_tudo(self):
        self._pontos.clear()
        self._atualizar_info()

    def _zoom_alterado(self, nivel: float):
        self.image_viewer.set_zoom(nivel)

    def _mostrar_regressao_popup(self):
        """Abre popup com a reta de regressão sobre a imagem."""
        if self._img_array is None:
            QMessageBox.warning(self, "Aviso", "Nenhuma imagem carregada.")
            return
        if len(self._pontos) < 4:
            QMessageBox.warning(self, "Aviso", "Marque pelo menos 4 pontos para calcular a regressao.")
            return

        dialog = RegressionDialog(self._img_array, self._pontos, theme=self._current_theme, parent=self)
        dialog.exec()

    def _mostrar_ajuda(self):
        """Abre popup de ajuda com documentação completa."""
        dialog = HelpDialog(theme=self._current_theme, parent=self)
        dialog.exec()

    def _mostrar_sobre(self):
        """Abre diálogo Sobre."""
        QMessageBox.about(
            self,
            "Sobre Fotogrametria",
            "<h3>Fotogrametria v1.0</h3>"
            "<p>Estimativa de velocidade veicular por fotogrametria.</p>"
            "<p>Autor original (R/Shiny): Carlo Ralph De Musis</p>"
            "<p>Portagem para Python com PySide6.</p>"
            "<p>POLITEC/MT</p>"
        )

    # -------------------------------------------------------------------
    # Atualização visual
    # -------------------------------------------------------------------

    def _atualizar_info(self):
        n = len(self._pontos)
        ciclos = n // 4

        if n == 0:
            self.lbl_status_marcacao.setText("Nenhum ponto marcado.")
            self.txt_regressao.setText("-")
        else:
            ultimo = self._pontos[-1]
            letra = LETRAS[ultimo["ponto"] - 1]
            dims = ""
            if self._img_array is not None:
                h, w = self._img_array.shape[:2]
                dims = f"Dimensões: {w} × {h}\n"
            self.lbl_status_marcacao.setText(
                f"{dims}"
                f"Ciclos completos: {ciclos} ({n} cliques)\n"
                f"Último ponto: {letra}, X: {ultimo['x']}, Y: {ultimo['y']}"
            )

            # Regressão
            if n >= 4:
                self._atualizar_regressao()

        # Tabela
        self.tabela_pontos.setRowCount(n)
        for i, p in enumerate(self._pontos):
            self.tabela_pontos.setItem(i, 0, QTableWidgetItem(LETRAS[p["ponto"] - 1]))
            self.tabela_pontos.setItem(i, 1, QTableWidgetItem(str(p["x"])))
            self.tabela_pontos.setItem(i, 2, QTableWidgetItem(str(p["y"])))
            item_cor = QTableWidgetItem(p["cor"])
            item_cor.setBackground(QColor(p["cor"]))
            self.tabela_pontos.setItem(i, 3, item_cor)
            self.tabela_pontos.setItem(i, 4, QTableWidgetItem(f"{p['cinza']:.4f}"))

    def _atualizar_regressao(self):
        """Atualiza o resumo da regressão e teste de Mardia."""
        xs = np.array([p["x"] for p in self._pontos], dtype=float)
        ys = np.array([p["y"] for p in self._pontos], dtype=float)

        # Regressão OLS para resumo rápido
        coeffs = np.polyfit(xs, ys, 1)
        b1, b0 = coeffs[0], coeffs[1]
        ss_res = np.sum((ys - (b0 + b1 * xs))**2)
        ss_tot = np.sum((ys - np.mean(ys))**2)
        r2 = 1 - ss_res / ss_tot if ss_tot > 0 else 0

        n = len(xs)
        aic = n * np.log(ss_res / n) + 4 if n > 0 and ss_res > 0 else float('inf')

        linhas = [
            f"Equação: y = {b0:.5f} + ({b1:.5f})x",
            f"R² = {r2:.5f}",
            f"AIC = {aic:.5f}",
            "",
            "Teste de Mardia (por grupo):",
        ]

        # Teste por grupo
        grupos = {}
        for p in self._pontos:
            g = p["ponto"]
            if g not in grupos:
                grupos[g] = {"x": [], "y": []}
            grupos[g]["x"].append(p["x"])
            grupos[g]["y"].append(p["y"])

        for g in sorted(grupos):
            letra = LETRAS[g - 1]
            gx = np.array(grupos[g]["x"], dtype=float)
            gy = np.array(grupos[g]["y"], dtype=float)
            if len(gx) < 3:
                linhas.append(f"  {letra} - Dados insuficientes ({len(gx)} pontos)")
                continue

            data = np.column_stack([gx, gy])
            try:
                mr = teste_mardia(data)
                linhas.append(
                    f"  {letra} - {mr.label} "
                    f"(Skew = {mr.skewness_p:.3f}; Kurt = {mr.kurtosis_p:.3f})"
                )
            except Exception:
                linhas.append(f"  {letra} - Erro no teste")

        self.txt_regressao.setText("\n".join(linhas))

    # -------------------------------------------------------------------
    # Processamento
    # -------------------------------------------------------------------

    def _obter_pontos_para_processamento(self) -> tuple[dict, dict] | None:
        """Retorna (pontos_x, pontos_y) agrupados, ou None se insuficientes."""
        dados = self._pontos.copy()

        # Se não tiver pontos suficientes, tentar CSV
        if len(dados) < 8:
            if self._csv_path:
                try:
                    dados = self._carregar_pontos_csv(self._csv_path)
                except Exception as e:
                    QMessageBox.critical(self, "Erro", f"Falha ao ler CSV:\n{e}")
                    return None
            else:
                QMessageBox.warning(
                    self, "Dados Insuficientes",
                    "Marque pelo menos 8 pontos (2 ciclos completos) ou carregue um CSV."
                )
                return None

        # Agrupar
        pontos_x: dict[int, list] = {1: [], 2: [], 3: [], 4: []}
        pontos_y: dict[int, list] = {1: [], 2: [], 3: [], 4: []}
        for p in dados:
            g = p["ponto"]
            if g in pontos_x:
                pontos_x[g].append(p["x"])
                pontos_y[g].append(p["y"])

        # Converter para arrays
        pontos_x_arr = {g: np.array(v, dtype=float) for g, v in pontos_x.items() if len(v) >= 2}
        pontos_y_arr = {g: np.array(v, dtype=float) for g, v in pontos_y.items() if len(v) >= 2}

        if len(pontos_x_arr) < 4:
            QMessageBox.warning(
                self, "Dados Insuficientes",
                "Cada grupo (A, B, C, D) precisa de pelo menos 2 pontos."
            )
            return None

        return pontos_x_arr, pontos_y_arr

    def _carregar_pontos_csv(self, caminho: str) -> list[dict]:
        """Carrega pontos de um arquivo CSV."""
        dados = []
        with open(caminho, "r", encoding="utf-8") as f:
            # Tentar detectar separador
            sample = f.read(2048)
            f.seek(0)
            sep = ";" if ";" in sample else ","

            reader = csv.DictReader(f, delimiter=sep)
            for row in reader:
                ponto_str = row.get("ponto", "")
                mapa = {"a": 1, "b": 2, "c": 3, "d": 4, "1": 1, "2": 2, "3": 3, "4": 4}
                g = mapa.get(ponto_str.strip().lower())
                if g is None:
                    continue
                dados.append({
                    "ponto": g,
                    "x": float(row["x"].replace(",", ".")),
                    "y": float(row["y"].replace(",", ".")),
                    "cor": row.get("cor", "#000000"),
                    "cinza": float(row.get("cinza", "0").replace(",", ".")),
                })
        return dados

    def _calcular(self):
        result = self._obter_pontos_para_processamento()
        if result is None:
            return

        pontos_x, pontos_y = result
        params = self.sidebar.get_parametros()

        # Progress dialog
        progress = QProgressDialog("Processando simulações Monte Carlo...", "Cancelar", 0, 100, self)
        progress.setWindowTitle("Processamento")
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.setMinimumDuration(0)

        if self._current_theme == "dark":
            progress.setStyleSheet("""
                QProgressDialog { background-color: #16213e; color: #e0e0e0; }
                QProgressBar { background-color: #0a1628; border: 1px solid #1a1a40; border-radius: 4px; text-align: center; color: #e0e0e0; }
                QProgressBar::chunk { background-color: #009988; border-radius: 3px; }
            """)
        else:
            progress.setStyleSheet("""
                QProgressDialog { background-color: #ffffff; color: #333333; }
                QProgressBar { background-color: #e0e0e0; border: 1px solid #cccccc; border-radius: 4px; text-align: center; color: #333333; }
                QProgressBar::chunk { background-color: #009988; border-radius: 3px; }
            """)

        kwargs = {
            "pontos_x": pontos_x,
            "pontos_y": pontos_y,
            "n_repeticoes": params["repeticoes"],
            "dist_referencia_mm": params["dist_referencia"],
            "inicio_quadro": params["inicio_quadro"],
            "fim_quadro": params["fim_quadro"],
            "erro_medio_mt": params["erro_medio_mt"],
            "dp_erro_medio_mt": params["dp_erro_medio_mt"],
            "nivel_confianca": params["nivel_confianca"],
            "img": self._img_array,
            "sigma_filtro": params["sigma_filtro"],
        }

        self._worker = ProcessingWorker(kwargs)
        self._worker.progresso.connect(lambda v: progress.setValue(int(v * 100)))
        self._worker.concluido.connect(lambda r: self._on_processamento_concluido(r, progress))
        self._worker.erro.connect(lambda e: self._on_processamento_erro(e, progress))
        self._worker.start()

    def _on_processamento_concluido(self, resultado: ResultadoProcessamento, progress: QProgressDialog):
        progress.close()
        self._resultado = resultado

        if resultado.avisos:
            QMessageBox.warning(self, "Avisos", "\n".join(resultado.avisos))

        nc = self.sidebar.get_parametros()["nivel_confianca"]
        self.results_panel.exibir_resultados(resultado, nc)
        self.tabs.setCurrentIndex(1)  # Ir para aba de resultados
        self.statusBar().showMessage(
            f"Processamento concluído — Velocidade média: {resultado.vel_media:.1f} km/h"
        )

    def _on_processamento_erro(self, erro: str, progress: QProgressDialog):
        progress.close()
        QMessageBox.critical(self, "Erro no Processamento", f"Ocorreu um erro:\n{erro}")

    # -------------------------------------------------------------------
    # Exportação
    # -------------------------------------------------------------------

    def _gerar_figura_regressao(self) -> Figure | None:
        """Gera a figura de regressão sobre a imagem para exportação."""
        if self._img_array is None or len(self._pontos) < 4:
            return None
        return criar_figura_regressao(self._img_array, self._pontos, theme="light")

    def _obter_texto_regressao(self) -> str | None:
        """Retorna o texto atual do resumo da regressão."""
        texto = self.txt_regressao.toPlainText()
        return texto if texto and texto != "-" else None

    def _obter_resultados_mardia(self) -> list[dict] | None:
        """Calcula e retorna os resultados do teste de Mardia por grupo."""
        if len(self._pontos) < 4:
            return None

        grupos = {}
        for p in self._pontos:
            g = p["ponto"]
            if g not in grupos:
                grupos[g] = {"x": [], "y": []}
            grupos[g]["x"].append(p["x"])
            grupos[g]["y"].append(p["y"])

        resultados = []
        for g in sorted(grupos):
            letra = LETRAS[g - 1]
            gx = np.array(grupos[g]["x"], dtype=float)
            gy = np.array(grupos[g]["y"], dtype=float)
            if len(gx) < 3:
                resultados.append({
                    "grupo": letra,
                    "resultado": "Dados insuficientes",
                    "skewness_stat": "",
                    "skewness_p": "",
                    "kurtosis_stat": "",
                    "kurtosis_p": "",
                })
                continue

            data = np.column_stack([gx, gy])
            try:
                mr = teste_mardia(data)
                resultados.append({
                    "grupo": letra,
                    "resultado": "Normal" if mr.is_normal else "Nao normal",
                    "skewness_stat": round(mr.skewness_stat, 6),
                    "skewness_p": round(mr.skewness_p, 6),
                    "kurtosis_stat": round(mr.kurtosis_stat, 6),
                    "kurtosis_p": round(mr.kurtosis_p, 6),
                })
            except Exception:
                resultados.append({
                    "grupo": letra,
                    "resultado": "Erro no teste",
                    "skewness_stat": "",
                    "skewness_p": "",
                    "kurtosis_stat": "",
                    "kurtosis_p": "",
                })

        return resultados if resultados else None

    def _exportar(self):
        if self._resultado is None:
            QMessageBox.information(self, "Exportacao", "Nenhum resultado para exportar. Execute o calculo primeiro.")
            return

        caminho, _ = QFileDialog.getSaveFileName(
            self, "Salvar Exportacao", "fotogrametria_resultados.zip",
            "ZIP (*.zip)"
        )
        if not caminho:
            return

        try:
            figuras = self.results_panel.get_figuras()

            # Dados dos pontos
            dados_pontos = [
                {"ponto": LETRAS[p["ponto"] - 1], "x": p["x"], "y": p["y"], "cor": p["cor"], "cinza": p["cinza"]}
                for p in self._pontos
            ]

            parametros = self.sidebar.get_parametros()

            # Figura de regressão sobre a imagem
            fig_regressao = self._gerar_figura_regressao()

            # Texto da regressão
            texto_regressao = self._obter_texto_regressao()

            # Resultados do teste de Mardia
            resultados_mardia = self._obter_resultados_mardia()

            exportar_zip(
                caminho_zip=caminho,
                figuras=figuras,
                dados_pontos=dados_pontos if dados_pontos else None,
                parametros=parametros,
                imagem_original=self._img_array,
                fig_regressao=fig_regressao,
                texto_regressao=texto_regressao,
                resultados_mardia=resultados_mardia,
            )

            QMessageBox.information(self, "Exportacao", f"Dados exportados com sucesso!\n{caminho}")
            self.statusBar().showMessage(f"Exportado: {caminho}")
        except Exception as e:
            QMessageBox.critical(self, "Erro na Exportacao", f"Falha ao exportar:\n{e}")
