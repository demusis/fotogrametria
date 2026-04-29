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
import datetime
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
    QSpinBox, QDoubleSpinBox, QPushButton, QInputDialog, QFormLayout,
)
from PySide6.QtGui import QFont, QColor, QAction, QShortcut, QKeySequence
from PySide6.QtCore import Qt, QThread, Signal

from .widgets.image_viewer import ImageViewer
from .widgets.sidebar import Sidebar
from .widgets.results_panel import ResultsPanel, PALETA
from .widgets.lens_correction_tab import LensCorrectionTab
from .widgets.comparative_analysis_tab import ImageCombinerTab
from .widgets.video_player_tab import VideoPlayerTab
from .core.image_ops import carregar_imagem, cor_pixel_hex, cinza_pixel
from .core.processing import (
    processar, regressao_deming, teste_mardia, ResultadoProcessamento, MardiaResult,
)
from .utils.export import exportar_zip
from .utils.pdf_report import gerar_pdf_academico
from .core.filters import histogram_percentile, clahe, retinex_msr, wiener_deblur, white_balance

import cv2


# ===========================================================================
# Worker threads
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


class FilterWorker(QThread):
    """Thread para aplicação de filtros forenses pesados."""
    finished = Signal(np.ndarray)
    error = Signal(str)

    def __init__(self, filter_fn, img: np.ndarray, kwargs: dict):
        super().__init__()
        self._filter_fn = filter_fn
        self._img = img
        self._kwargs = kwargs

    def run(self):
        try:
            result = self._filter_fn(self._img, **self._kwargs)
            self.finished.emit(result)
        except Exception as e:
            self.error.emit(str(e))
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
    ax.imshow(img_array, origin="upper", extent=[0, w, h, 0])

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

    fig.tight_layout(pad=1.5)
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
        from app import __version__
        self.setWindowTitle(f"Fotogrametria — Estimativa de Velocidade Veicular v{__version__}")
        self.setMinimumSize(1280, 720)

        # Estado
        self._img_array: np.ndarray | None = None
        self._img_path: str | None = None
        self._pontos: list[dict] = []  # lista de {ponto, x, y, cor, cinza}
        self._csv_path: str | None = None
        self._resultado: ResultadoProcessamento | None = None
        self._worker: ProcessingWorker | None = None
        self._current_theme = "dark"
        self._undo_stack_coords: list[np.ndarray] = []
        self._undo_stack_video: list[np.ndarray] = []
        self._max_undo = 5
        self._audit_log: list[str] = []
        self._filter_worker: FilterWorker | None = None
        self._video_path_used: str | None = None
        self._regression_metrics: dict | None = None
        self.filter_chain: list[dict] = []

        self._init_ui()
        self._connect_signals()
        
        if hasattr(self.video_tab, 'set_filter_callback'):
            self.video_tab.set_filter_callback(self._apply_filter_chain)
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

        act_exportar = QAction("Exportar...", self)
        act_exportar.setShortcut("Ctrl+S")
        act_exportar.triggered.connect(self._exportar)
        menu_arquivo.addAction(act_exportar)

        menu_arquivo.addSeparator()
        act_sair = QAction("Sair", self)
        act_sair.setShortcut("Ctrl+Q")
        act_sair.triggered.connect(self.close)
        menu_arquivo.addAction(act_sair)
        
        # Menu Filtros Forenses
        menu_filtros = self._menubar.addMenu("Filtros Forenses")
        
        act_hist = QAction("Histograma (Percentil)", self)
        act_hist.triggered.connect(lambda: self._apply_filter("hist"))
        menu_filtros.addAction(act_hist)
        
        act_clahe = QAction("CLAHE (Equalização Limitada)", self)
        act_clahe.triggered.connect(lambda: self._apply_filter("clahe"))
        menu_filtros.addAction(act_clahe)
        
        act_retinex = QAction("Retinex Multi-Escala (MSR)", self)
        act_retinex.triggered.connect(lambda: self._apply_filter("retinex"))
        menu_filtros.addAction(act_retinex)
        
        act_wb = QAction("Balanço de Branco (Mundo Cinza)", self)
        act_wb.triggered.connect(lambda: self._apply_filter("wb"))
        menu_filtros.addAction(act_wb)
        
        act_wiener = QAction("Deconvolução de Wiener (Motion Deblur)", self)
        act_wiener.triggered.connect(lambda: self._apply_filter("wiener"))
        menu_filtros.addAction(act_wiener)
        
        menu_filtros.addSeparator()
        act_resize = QAction("Reescalonamento (Lanczos)", self)
        act_resize.triggered.connect(self._open_resize_dialog)
        menu_filtros.addAction(act_resize)
        
        act_aspect = QAction("Correção de Razão de Aspecto", self)
        act_aspect.triggered.connect(self._open_aspect_ratio_dialog)
        menu_filtros.addAction(act_aspect)
        
        act_manager = QAction("Gerenciador de Filtros (Vídeo)", self)
        act_manager.triggered.connect(self._open_filter_manager)
        menu_filtros.addAction(act_manager)
        
        act_undo = QAction("Desfazer Filtro", self)
        act_undo.setShortcut("Ctrl+Z")
        act_undo.triggered.connect(self._undo_filter)
        menu_filtros.addAction(act_undo)

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
        
        # --- Tab 1 (Agora): Processamento ---
        self.results_panel = ResultsPanel()
        self.tabs.addTab(self.results_panel, "📊  Processamento")

        # --- Tab 2 (Agora): Coordenadas ---
        self.tabs.addTab(self._tab_coords, "📍  Coordenadas")
        # --- Tab 3: Correção de Lente ---
        self.lens_correction_tab = LensCorrectionTab()
        self.tabs.addTab(self.lens_correction_tab, "📷  Correção de Lente")

        # --- Tab 4: Combinador ---
        self.combiner_tab = ImageCombinerTab()
        self.tabs.addTab(self.combiner_tab, "🖼️  Mesclar Imagens")

        # --- Tab 5: Forense Video ---
        self.video_tab = VideoPlayerTab()
        self.video_tab.enviar_par_imagens.connect(self._on_video_pair_sent)
        self.tabs.addTab(self.video_tab, "🎞️  Vídeo")
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
        if hasattr(self, 'lens_correction_tab'):
            self.lens_correction_tab.viewer._apply_theme(theme)
            self.lens_correction_tab.set_theme(theme)
        if hasattr(self, 'combiner_tab'):
            self.combiner_tab.viewer._apply_theme(theme)
            self.combiner_tab.set_theme(theme)
        if hasattr(self, 'video_tab'):
            if hasattr(self.video_tab, 'viewer'):
                self.video_tab.viewer._apply_theme(theme)
            self.video_tab.set_theme(theme)

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
        self.sidebar.gerar_relatorio.connect(self._gerar_relatorio_pdf)
        self.sidebar.zoom_alterado.connect(self._zoom_alterado)
        self.sidebar.tema_alterado.connect(self._apply_theme)
        self.sidebar.ver_regressao.connect(self._mostrar_regressao_popup)
        self.image_viewer.ponto_marcado.connect(self._on_ponto_marcado)
        
        self.combiner_tab.enviar_imagem.connect(self._on_combiner_send)
        self.lens_correction_tab.enviar_imagem.connect(self._on_lens_send)

    # -------------------------------------------------------------------
    # Helpers de imagem ativa (evita duplicação filter/undo/resize)
    # -------------------------------------------------------------------

    def _get_active_context(self) -> tuple[str, np.ndarray | None]:
        """Retorna (contexto, imagem_ativa) baseado na aba atual.
        
        contexto pode ser 'coords', 'video' ou 'other'.
        """
        txt = self.tabs.tabText(self.tabs.currentIndex())
        if "Coordenadas" in txt:
            return "coords", self._img_array
        elif "Vídeo" in txt:
            return "video", self.video_tab.current_frame_img
        return "other", None

    def _set_active_image(self, ctx: str, img: np.ndarray):
        """Atualiza a imagem ativa e recarrega o viewer."""
        if ctx == "coords":
            self._img_array = img
            self.image_viewer.carregar_imagem(img)
        elif ctx == "video":
            self.video_tab.current_frame_img = img
            self.video_tab.viewer.carregar_imagem(img)

    def _push_undo(self, ctx: str, img: np.ndarray):
        """Salva cópia na pilha de undo apropriada."""
        stack = self._undo_stack_coords if ctx == "coords" else self._undo_stack_video
        stack.append(img.copy())
        if len(stack) > self._max_undo:
            stack.pop(0)

    def _pop_undo(self, ctx: str) -> np.ndarray | None:
        """Remove e retorna o último estado da pilha de undo."""
        stack = self._undo_stack_coords if ctx == "coords" else self._undo_stack_video
        return stack.pop() if stack else None

    # -------------------------------------------------------------------
    # Ações
    # -------------------------------------------------------------------

    _FILTERS = {
        "hist": histogram_percentile,
        "clahe": clahe,
        "retinex": retinex_msr,
        "wb": white_balance,
        "wiener": wiener_deblur,
    }

    # Nomes legíveis para o audit log
    _FILTER_NAMES = {
        "hist": "Histograma (Percentil)",
        "clahe": "CLAHE",
        "retinex": "Retinex MSR",
        "wb": "Balanço de Branco",
        "wiener": "Deconvolução de Wiener",
    }

    def _log_audit(self, msg: str):
        """Registra uma entrada no log de auditoria com timestamp."""
        ts = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self._audit_log.append(f"[{ts}] {msg}")

    def _apply_filter_chain(self, frame: np.ndarray) -> np.ndarray:
        if not self.filter_chain:
            return frame
            
        import cv2
        
        res = frame.copy()
        for f in self.filter_chain:
            if not f.get("enabled", True):
                continue
            name = f["name"]
            kwargs = f["kwargs"]
            
            if name == "resize" or name == "aspect_ratio":
                new_w = kwargs.get("width")
                new_h = kwargs.get("height")
                if new_w and new_h:
                    res = cv2.resize(res, (new_w, new_h), interpolation=cv2.INTER_LANCZOS4)
            else:
                func = self._FILTERS.get(name)
                if func:
                    try:
                        res = func(res, **kwargs)
                    except Exception:
                        pass
        return res

    def _apply_filter(self, filter_name: str):
        """Aplica filtro forense à imagem ativa em thread separada."""
        ctx, img = self._get_active_context()
        if ctx == "other":
            QMessageBox.information(
                self, "Aviso",
                "Os filtros forenses são ideais para aplicação no 'Vídeo' ou 'Coordenadas'."
            )
            return
        if img is None:
            return

        # Parâmetros interativos
        kwargs = {}
        if filter_name == "hist":
            val, ok = QInputDialog.getDouble(
                self,
                "Histograma (Percentil)",
                "Percentil de corte de luz/sombra (0.0 a 49.0%):\n"
                "* Valores maiores forçam contraste mais agressivo.",
                1.0, 0.0, 49.0, 1,
            )
            if not ok:
                return
            kwargs["percentile"] = val

        elif filter_name == "retinex":
            dlg = QDialog(self)
            dlg.setWindowTitle("Retinex Multi-Escala (Ajuste Fino)")
            layout = QFormLayout(dlg)

            sigma1_spin = QDoubleSpinBox()
            sigma1_spin.setRange(1.0, 100.0)
            sigma1_spin.setValue(15.0)
            layout.addRow("Sigma 1 (fino):", sigma1_spin)

            sigma2_spin = QDoubleSpinBox()
            sigma2_spin.setRange(10.0, 200.0)
            sigma2_spin.setValue(80.0)
            layout.addRow("Sigma 2 (médio):", sigma2_spin)

            sigma3_spin = QDoubleSpinBox()
            sigma3_spin.setRange(50.0, 500.0)
            sigma3_spin.setValue(250.0)
            layout.addRow("Sigma 3 (grosso):", sigma3_spin)

            # Novos parâmetros de ajuste fino
            blend_spin = QDoubleSpinBox()
            blend_spin.setRange(0.0, 1.0)
            blend_spin.setSingleStep(0.1)
            blend_spin.setValue(0.5)
            blend_spin.setToolTip("Mistura entre luz natural (0.0) e Retinex puro (1.0)")
            layout.addRow("Intensidade do Filtro:", blend_spin)

            black_clip_spin = QDoubleSpinBox()
            black_clip_spin.setRange(0.0, 5.0)
            black_clip_spin.setSingleStep(0.1)
            black_clip_spin.setValue(1.0)
            black_clip_spin.setSuffix(" %")
            black_clip_spin.setToolTip("Corta os tons mais escuros para forçar um preto absoluto. Valores maiores aumentam o contraste nas sombras.")
            layout.addRow("Corte de Pretos:", black_clip_spin)

            btn_box = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
            btn_box.accepted.connect(dlg.accept)
            btn_box.rejected.connect(dlg.reject)
            layout.addRow(btn_box)

            if dlg.exec() != QDialog.Accepted:
                return
            
            kwargs["sigmas"] = [sigma1_spin.value(), sigma2_spin.value(), sigma3_spin.value()]
            kwargs["blend_factor"] = blend_spin.value()
            kwargs["black_clip"] = black_clip_spin.value()

        elif filter_name == "wiener":
            # Diálogo interativo para parâmetros de desfoque
            dlg = QDialog(self)
            dlg.setWindowTitle("Deconvolução de Wiener — Motion Deblur")
            layout = QVBoxLayout(dlg)
            layout.addWidget(QLabel(
                "Configure os parâmetros do filtro de Wiener.\n"
                "Use para restaurar imagens com desfoque de movimento."
            ))

            form = QHBoxLayout()

            form.addWidget(QLabel("Ângulo (°):"))
            spin_angle = QDoubleSpinBox()
            spin_angle.setRange(0, 180)
            spin_angle.setValue(0)
            spin_angle.setSingleStep(5)
            form.addWidget(spin_angle)

            form.addWidget(QLabel("Extensão (px):"))
            spin_len = QSpinBox()
            spin_len.setRange(2, 100)
            spin_len.setValue(15)
            form.addWidget(spin_len)

            form.addWidget(QLabel("SNR:"))
            spin_snr = QDoubleSpinBox()
            spin_snr.setRange(1, 10000)
            spin_snr.setValue(100)
            spin_snr.setSingleStep(10)
            form.addWidget(spin_snr)

            layout.addLayout(form)

            btns = QHBoxLayout()
            btn_ok = QPushButton("Aplicar")
            btn_cancel = QPushButton("Cancelar")
            btn_ok.clicked.connect(dlg.accept)
            btn_cancel.clicked.connect(dlg.reject)
            btns.addWidget(btn_ok)
            btns.addWidget(btn_cancel)
            layout.addLayout(btns)

            if dlg.exec() != int(QDialog.DialogCode.Accepted):
                return
            kwargs["angle"] = spin_angle.value()
            kwargs["length"] = spin_len.value()
            kwargs["snr"] = spin_snr.value()

        filter_fn = self._FILTERS.get(filter_name)
        if filter_fn is None:
            return

        label = "Coordenadas" if ctx == "coords" else "Vídeo"
        filter_readable = self._FILTER_NAMES.get(filter_name, filter_name)

        if ctx == "video":
            import uuid
            self.filter_chain.append({
                "id": str(uuid.uuid4()),
                "name": filter_name,
                "readable": filter_readable,
                "kwargs": kwargs,
                "enabled": True
            })
            params_str = ", ".join(f"{k}={v}" for k, v in kwargs.items()) if kwargs else "padrão"
            self._log_audit(f"Filtro '{filter_readable}' adicionado à pipeline de Vídeo. Params: {params_str}")
            self.statusBar().showMessage(f"Filtro '{filter_readable}' ativado no Vídeo.")
            if hasattr(self.video_tab, 'refresh_frame'):
                self.video_tab.refresh_frame()
            return

        self._push_undo(ctx, img)

        # Executar filtro em thread separada com diálogo de progresso
        progress = QProgressDialog(f"Aplicando {filter_readable}...", None, 0, 0, self)
        progress.setWindowTitle("Filtro Forense")
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.setMinimumDuration(0)
        progress.setCancelButton(None)
        progress.show()

        self._filter_worker = FilterWorker(filter_fn, img.copy(), kwargs)

        def on_finished(result: np.ndarray):
            progress.close()
            self._set_active_image(ctx, result)
            params_str = ", ".join(f"{k}={v}" for k, v in kwargs.items()) if kwargs else "padrão"
            self._log_audit(f"Filtro '{filter_readable}' aplicado ({label}). Params: {params_str}")
            self.statusBar().showMessage(f"Filtro {filter_readable} aplicado ({label}).")
            self._filter_worker = None

        def on_error(err: str):
            progress.close()
            QMessageBox.critical(self, "Erro no Filtro", f"Falha ao aplicar filtro:\n{err}")
            self._filter_worker = None

        self._filter_worker.finished.connect(on_finished)
        self._filter_worker.error.connect(on_error)
        self._filter_worker.start()

    def _undo_filter(self):
        """Desfaz o último filtro/reescalonamento."""
        ctx, _ = self._get_active_context()
        if ctx == "other":
            return

        if ctx == "video":
            if not self.filter_chain:
                self.statusBar().showMessage("Nenhum filtro na pipeline de vídeo.")
                return
            popped = self.filter_chain.pop()
            self._log_audit(f"Filtro '{popped['readable']}' removido da pipeline de Vídeo (Desfazer).")
            self.statusBar().showMessage(f"Desfez último filtro ({popped['readable']}).")
            if hasattr(self.video_tab, 'refresh_frame'):
                self.video_tab.refresh_frame()
            return

        img = self._pop_undo(ctx)
        if img is None:
            self.statusBar().showMessage("Nada para desfazer.")
            return

        self._set_active_image(ctx, img)
        label = "Coordenadas" if ctx == "coords" else "Vídeo"
        self._log_audit(f"Filtro/Reescalonamento desfeito (Aba {label}). Imagem restaurada para o estado anterior.")
        self.statusBar().showMessage(f"Desfez último filtro/reescalonamento ({label}).")

    def _open_resize_dialog(self):
        """Abre diálogo de reescalonamento com suporte a pixels e percentuais."""
        ctx, img = self._get_active_context()
        if ctx == "other":
            QMessageBox.information(self, "Aviso", "Reescalonamento no 'Vídeo' ou 'Coordenadas' apenas.")
            return
        if img is None:
            return

        h, w = img.shape[:2]
        
        dialog = QDialog(self)
        dialog.setWindowTitle("Reescalonar Imagem")
        lay = QVBoxLayout(dialog)
        
        lay.addWidget(QLabel("Defina a nova resolução (Algoritmo Lanczos4):"))
        
        hw_lay = QHBoxLayout()
        hw_lay.addWidget(QLabel("Largura:"))
        spin_w = QSpinBox()
        spin_w.setRange(1, 100000)
        spin_w.setValue(w)
        spin_w.setSuffix(" px")
        hw_lay.addWidget(spin_w)
        
        spin_pct_w = QDoubleSpinBox()
        spin_pct_w.setRange(0.1, 1000.0)
        spin_pct_w.setValue(100.0)
        spin_pct_w.setSuffix(" %")
        spin_pct_w.setDecimals(1)
        hw_lay.addWidget(spin_pct_w)
        
        hw_lay.addSpacing(15)
        
        hw_lay.addWidget(QLabel("Altura:"))
        spin_h = QSpinBox()
        spin_h.setRange(1, 100000)
        spin_h.setValue(h)
        spin_h.setSuffix(" px")
        hw_lay.addWidget(spin_h)
        
        spin_pct_h = QDoubleSpinBox()
        spin_pct_h.setRange(0.1, 1000.0)
        spin_pct_h.setValue(100.0)
        spin_pct_h.setSuffix(" %")
        spin_pct_h.setDecimals(1)
        hw_lay.addWidget(spin_pct_h)
        
        lay.addLayout(hw_lay)
        
        def update_from_w(val):
            spin_pct_w.blockSignals(True)
            spin_pct_w.setValue((val / w) * 100.0)
            spin_pct_w.blockSignals(False)
            
        def update_from_h(val):
            spin_pct_h.blockSignals(True)
            spin_pct_h.setValue((val / h) * 100.0)
            spin_pct_h.blockSignals(False)
            
        def update_from_pct_w(val):
            spin_w.blockSignals(True)
            spin_w.setValue(int(val / 100.0 * w))
            spin_w.blockSignals(False)
            
        def update_from_pct_h(val):
            spin_h.blockSignals(True)
            spin_h.setValue(int(val / 100.0 * h))
            spin_h.blockSignals(False)
            
        spin_w.valueChanged.connect(update_from_w)
        spin_h.valueChanged.connect(update_from_h)
        spin_pct_w.valueChanged.connect(update_from_pct_w)
        spin_pct_h.valueChanged.connect(update_from_pct_h)
        
        btn_lay = QHBoxLayout()
        btn_ok = QPushButton("Aplicar")
        btn_cancel = QPushButton("Cancelar")
        btn_ok.clicked.connect(dialog.accept)
        btn_cancel.clicked.connect(dialog.reject)
        btn_lay.addWidget(btn_ok)
        btn_lay.addWidget(btn_cancel)
        lay.addLayout(btn_lay)
        
        if dialog.exec() == int(QDialog.DialogCode.Accepted):
            new_w = spin_w.value()
            new_h = spin_h.value()
            if new_w == w and new_h == h: return
            
            if ctx == "video":
                import uuid
                self.filter_chain.append({
                    "id": str(uuid.uuid4()),
                    "name": "resize",
                    "readable": f"Reescalonamento ({new_w}x{new_h})",
                    "kwargs": {"width": new_w, "height": new_h},
                    "enabled": True
                })
                self._log_audit(f"Reescalonamento adicionado à pipeline de Vídeo: {w}x{h} → {new_w}x{new_h}.")
                self.statusBar().showMessage(f"Reescalonamento ({new_w}x{new_h}) ativado.")
                if hasattr(self.video_tab, 'refresh_frame'):
                    self.video_tab.refresh_frame()
            else:
                self._push_undo(ctx, img)
                resized = cv2.resize(img, (new_w, new_h), interpolation=cv2.INTER_LANCZOS4)
                self._set_active_image(ctx, resized)
                self._log_audit(f"Reescalonamento {w}x{h} → {new_w}x{new_h} (Lanczos4).")
                self.statusBar().showMessage(f"Reescalonado para {new_w}x{new_h} (Lanczos).")

    def _open_aspect_ratio_dialog(self):
        """Abre diálogo para correção da razão de aspecto mantendo a altura."""
        from PySide6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QDoubleSpinBox, QWidget, QPushButton, QMessageBox
        
        ctx, img = self._get_active_context()
        if ctx == "other":
            QMessageBox.information(self, "Aviso", "Correção de Razão de Aspecto no 'Vídeo' ou 'Coordenadas' apenas.")
            return
        if img is None:
            return

        h, w = img.shape[:2]
        
        dlg = QDialog(self)
        dlg.setWindowTitle("Correção de Razão de Aspecto")
        layout = QVBoxLayout(dlg)
        
        layout.addWidget(QLabel(f"Dimensões atuais: {w}x{h} (Razão: {w/h:.3f})"))
        
        form = QHBoxLayout()
        form.addWidget(QLabel("Nova Razão:"))
        
        combo = QComboBox()
        combo.addItems(["Original (DAR do Vídeo)", "4:3 (1.333)", "16:9 (1.778)", "1:1 (1.0)", "Personalizado"])
        form.addWidget(combo)
        layout.addLayout(form)
        
        custom_lay = QHBoxLayout()
        spin_w_ratio = QDoubleSpinBox()
        spin_w_ratio.setRange(0.1, 100.0)
        spin_w_ratio.setValue(16.0)
        spin_w_ratio.setDecimals(3)
        
        spin_h_ratio = QDoubleSpinBox()
        spin_h_ratio.setRange(0.1, 100.0)
        spin_h_ratio.setValue(9.0)
        spin_h_ratio.setDecimals(3)
        
        custom_lay.addWidget(spin_w_ratio)
        custom_lay.addWidget(QLabel(":"))
        custom_lay.addWidget(spin_h_ratio)
        
        custom_wgt = QWidget()
        custom_wgt.setLayout(custom_lay)
        custom_wgt.setVisible(False)
        layout.addWidget(custom_wgt)
        
        combo.currentIndexChanged.connect(lambda idx: custom_wgt.setVisible(idx == 4))
        
        btns = QHBoxLayout()
        btn_ok = QPushButton("Aplicar")
        btn_cancel = QPushButton("Cancelar")
        btn_ok.clicked.connect(dlg.accept)
        btn_cancel.clicked.connect(dlg.reject)
        btns.addWidget(btn_ok)
        btns.addWidget(btn_cancel)
        layout.addLayout(btns)
        
        if dlg.exec() != int(QDialog.DialogCode.Accepted):
            return
            
        idx = combo.currentIndex()
        if idx == 0:
            target_ratio = w / h
            if self._video_path_used:
                try:
                    import subprocess
                    cmd = ["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=display_aspect_ratio", "-of", "default=noprint_wrappers=1:nokey=1", self._video_path_used]
                    res = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=5)
                    dar_str = res.stdout.strip()
                    if dar_str and ":" in dar_str and dar_str != "0:1":
                        num, den = map(float, dar_str.split(":"))
                        if den > 0:
                            target_ratio = num / den
                except Exception:
                    pass
        elif idx == 1:
            target_ratio = 4.0 / 3.0
        elif idx == 2:
            target_ratio = 16.0 / 9.0
        elif idx == 3:
            target_ratio = 1.0
        else:
            if spin_h_ratio.value() > 0:
                target_ratio = spin_w_ratio.value() / spin_h_ratio.value()
            else:
                target_ratio = w / h
                
        if abs(target_ratio - (w/h)) < 0.01:
            return
            
        new_w = int(h * target_ratio)
        new_h = h
        
        if ctx == "video":
            import uuid
            self.filter_chain.append({
                "id": str(uuid.uuid4()),
                "name": "aspect_ratio",
                "readable": f"Correção Razão de Aspecto ({new_w}x{new_h})",
                "kwargs": {"width": new_w, "height": new_h},
                "enabled": True
            })
            self._log_audit(f"Correção de Aspect Ratio na pipeline de Vídeo: {new_w}x{new_h} (Razão {target_ratio:.3f}).")
            self.statusBar().showMessage(f"Razão de aspecto corrigida para {target_ratio:.3f}.")
            if hasattr(self.video_tab, 'refresh_frame'):
                self.video_tab.refresh_frame()
        else:
            self._push_undo(ctx, img)
            resized = cv2.resize(img, (new_w, new_h), interpolation=cv2.INTER_LANCZOS4)
            self._set_active_image(ctx, resized)
            self._log_audit(f"Correção de Razão de Aspecto: {w}x{h} → {new_w}x{new_h} (Razão {target_ratio:.3f}).")
            self.statusBar().showMessage(f"Razão de aspecto corrigida para {target_ratio:.3f} ({new_w}x{new_h}).")

    def _open_filter_manager(self):
        from PySide6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QListWidget, QListWidgetItem, QPushButton
        from PySide6.QtCore import Qt

        dlg = QDialog(self)
        dlg.setWindowTitle("Gerenciador de Filtros (Vídeo)")
        dlg.resize(400, 300)
        
        layout = QVBoxLayout(dlg)
        
        list_wgt = QListWidget()
        
        def refresh_list():
            list_wgt.clear()
            for f in self.filter_chain:
                item = QListWidgetItem(f["readable"])
                item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
                item.setCheckState(Qt.CheckState.Checked if f.get("enabled", True) else Qt.CheckState.Unchecked)
                item.setData(Qt.ItemDataRole.UserRole, f["id"])
                list_wgt.addItem(item)
                
        refresh_list()
        
        list_wgt.itemChanged.connect(self._on_filter_item_changed)
        layout.addWidget(list_wgt)
        
        btns_layout = QHBoxLayout()
        
        btn_remove_sel = QPushButton("Remover Selecionado")
        btn_remove_sel.clicked.connect(lambda: self._remove_selected_filter(list_wgt))
        btns_layout.addWidget(btn_remove_sel)
        
        btn_clear = QPushButton("Remover Todos")
        btn_clear.clicked.connect(lambda: self._clear_all_filters(dlg))
        btns_layout.addWidget(btn_clear)
        
        layout.addLayout(btns_layout)
        
        dlg.exec()
        
    def _remove_selected_filter(self, list_wgt):
        from PySide6.QtCore import Qt
        current_item = list_wgt.currentItem()
        if not current_item:
            return
        f_id = current_item.data(Qt.ItemDataRole.UserRole)
        for i, f in enumerate(self.filter_chain):
            if f["id"] == f_id:
                name = f['readable']
                del self.filter_chain[i]
                self._log_audit(f"Filtro '{name}' foi removido da pipeline de Vídeo.")
                break
        
        list_wgt.blockSignals(True)
        row = list_wgt.row(current_item)
        list_wgt.takeItem(row)
        list_wgt.blockSignals(False)
        
        if hasattr(self.video_tab, 'refresh_frame'):
            self.video_tab.refresh_frame()
        
    def _on_filter_item_changed(self, item):
        from PySide6.QtCore import Qt
        f_id = item.data(Qt.ItemDataRole.UserRole)
        enabled = (item.checkState() == Qt.CheckState.Checked)
        for f in self.filter_chain:
            if f["id"] == f_id:
                if f.get("enabled", True) != enabled:
                    f["enabled"] = enabled
                    self._log_audit(f"Filtro '{f['readable']}' {'habilitado' if enabled else 'desabilitado'} na pipeline de Vídeo.")
                    if hasattr(self.video_tab, 'refresh_frame'):
                        self.video_tab.refresh_frame()
                break
                
    def _clear_all_filters(self, dlg):
        if not self.filter_chain: return
        self.filter_chain.clear()
        self._log_audit("Todos os filtros da pipeline de Vídeo foram removidos.")
        if hasattr(self.video_tab, 'refresh_frame'):
            self.video_tab.refresh_frame()
        dlg.accept()

    def _on_combiner_send(self, img_array: np.ndarray):
        """Recebe imagem mesclada e a envia para a aba de correção."""
        self.lens_correction_tab.receber_imagem(img_array)
        if hasattr(self, '_last_time_esq') and hasattr(self, '_last_time_dir'):
            self.lens_correction_tab.set_suggested_texts(f"{self._last_time_esq:.3f}s", f"{self._last_time_dir:.3f}s")
        self.tabs.setCurrentWidget(self.lens_correction_tab)
        
    def _on_lens_send(self, img_array: np.ndarray):
        """Recebe imagem corrigida e a carrega na fotogrametria (aba coordenadas)."""
        self._img_array = img_array
        self.image_viewer.carregar_imagem(self._img_array)
        self._pontos.clear()
        self._atualizar_info()
        h, w = self._img_array.shape[:2]
        self.statusBar().showMessage(f"Imagem recebida da Correção: {w} × {h}")
        self.tabs.setCurrentWidget(self._tab_coords)

    def _on_video_pair_sent(self, img_esq: np.ndarray, img_dir: np.ndarray, time_esq: float, time_dir: float):
        """Recebe par do extrator de video e passa p/ Combinador, atualizando os campos de tempo"""
        self._last_time_esq = time_esq
        self._last_time_dir = time_dir
        
        self.combiner_tab.img1_array = img_esq.copy()
        self.combiner_tab.img2_array = img_dir.copy()
        self.combiner_tab.lbl_file1.setText(f"FFV: Esq (T={time_esq:.3f}s)")
        self.combiner_tab.lbl_file2.setText(f"FFV: Dir (T={time_dir:.3f}s)")
        self.combiner_tab._update_combined()
        
        # O quadro inicial deve ser o menor tempo
        q_inicio = min(time_esq, time_dir)
        q_fim = max(time_esq, time_dir)
        
        self.sidebar.spin_inicio.setValue(q_inicio)
        self.sidebar.spin_fim.setValue(q_fim)
        
        if getattr(self.video_tab, 'player', None):
            self._video_path_used = self.video_tab.player.path
        
        self.tabs.setCurrentWidget(self.combiner_tab)
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
            import hashlib
            sha = hashlib.sha256()
            with open(caminho, "rb") as fh:
                for chunk in iter(lambda: fh.read(4096 * 1024), b""):
                    sha.update(chunk)
            self._log_audit(f"Imagem aberta: {Path(caminho).name} ({w}x{h}). SHA-256: {sha.hexdigest()}")
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

        self._log_audit(f"Ponto marcado: Grupo {LETRAS[ponto_no_ciclo - 1]}, X: {x}, Y: {y}")
        self._atualizar_info()

    def _apagar_ultimo_ponto(self):
        if self._pontos:
            p = self._pontos.pop()
            self._log_audit(f"Último ponto apagado (Grupo {LETRAS[p['ponto'] - 1]}, X: {p['x']}, Y: {p['y']})")
            self._atualizar_info()

    def _apagar_tudo(self):
        self._pontos.clear()
        self._log_audit("Todos os pontos foram apagados.")
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
        from app import __version__
        QMessageBox.about(
            self,
            "Sobre Fotogrametria",
            f"<h3>Fotogrametria v{__version__}</h3>"
            "<p>Estimativa de velocidade veicular por fotogrametria.</p>"
            "<p>Autor original (R/Shiny): Carlo Ralph De Musis</p>"
            "<p>Portagem para Python com PySide6.</p>"
            "<p>Módulo de Processamento Pericial.</p>"
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

        self._regression_metrics = {
            "equacao": f"y = {b0:.5f} + ({b1:.5f})x",
            "r2": f"{r2:.5f}",
            "aic": f"{aic:.5f}"
        }

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
                    f"(N={len(gx)}; Assim={mr.skewness_p:.3f}; Curt={mr.kurtosis_p:.3f}; S-W X={mr.shapiro_x_p:.3f}; S-W Y={mr.shapiro_y_p:.3f})"
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

        self._log_audit(f"Iniciando cálculo de velocidade com {len(self._pontos)} pontos ou via CSV.")
        self._log_audit("Iniciada Simulação de Monte Carlo acoplada à Regressão Ortogonal de Deming.")
        self._log_audit("Metodologia Estatística: O intervalo de tempo entre quadros sofre perturbação estocástica (distribuição Normal iterada a partir do Erro Médio Temporal fornecido). Em cada iteração, as coordenadas projetadas são ajustadas para extração da velocidade instantânea baseada na inclinação da reta (Regressão de Deming). O conjunto empírico de velocidades extraídas delimita o intervalo de confiança reportado nos resultados.")
        
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
                    "N": len(gx),
                    "resultado": "Dados insuficientes",
                    "skewness_stat": "",
                    "skewness_p": "",
                    "kurtosis_stat": "",
                    "kurtosis_p": "",
                    "shapiro_x_p": "",
                    "shapiro_y_p": "",
                })
                continue

            data = np.column_stack([gx, gy])
            try:
                mr = teste_mardia(data)
                resultados.append({
                    "grupo": letra,
                    "N": len(gx),
                    "resultado": "Normal" if mr.is_normal else "Nao normal",
                    "skewness_stat": round(mr.skewness_stat, 6),
                    "skewness_p": round(mr.skewness_p, 6),
                    "kurtosis_stat": round(mr.kurtosis_stat, 6),
                    "kurtosis_p": round(mr.kurtosis_p, 6),
                    "shapiro_x_p": round(mr.shapiro_x_p, 6),
                    "shapiro_y_p": round(mr.shapiro_y_p, 6),
                })
            except Exception:
                resultados.append({
                    "grupo": letra,
                    "N": len(gx),
                    "resultado": "Erro no teste",
                    "skewness_stat": "",
                    "skewness_p": "",
                    "kurtosis_stat": "",
                    "kurtosis_p": "",
                    "shapiro_x_p": "",
                    "shapiro_y_p": "",
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

        if not caminho.lower().endswith('.zip'):
            caminho += '.zip'

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

            self._log_audit(f"Exportação iniciada: {Path(caminho).name}")
            exportar_zip(
                caminho_zip=caminho,
                figuras=figuras,
                dados_pontos=dados_pontos if dados_pontos else None,
                parametros=parametros,
                imagem_original=self._img_array,
                fig_regressao=fig_regressao,
                texto_regressao=texto_regressao,
                resultados_mardia=resultados_mardia,
                audit_log=self._audit_log if self._audit_log else None,
            )

            QMessageBox.information(self, "Exportacao", f"Dados exportados com sucesso!\n{caminho}")
            self.statusBar().showMessage(f"Exportado: {caminho}")
        except Exception as e:
            QMessageBox.critical(self, "Erro na Exportacao", f"Falha ao exportar:\n{e}")

    def _gerar_relatorio_pdf(self):
        if self._resultado is None:
            QMessageBox.information(self, "Relatorio", "Nenhum resultado para gerar relatorio. Execute o calculo primeiro.")
            return

        caminho, _ = QFileDialog.getSaveFileName(
            self, "Salvar Relatorio PDF", "Relatorio_Pericial.pdf",
            "PDF (*.pdf)"
        )
        if not caminho:
            return

        if not caminho.lower().endswith('.pdf'):
            caminho += '.pdf'

        try:
            self.statusBar().showMessage("Gerando relatorio PDF...")
            self._log_audit(f"Geracao de relatorio PDF iniciada: {Path(caminho).name}")
            
            figuras = self.results_panel.get_figuras()
            
            dados_pontos = [
                {"ponto": LETRAS[p["ponto"] - 1], "x": p["x"], "y": p["y"], "cinza": p["cinza"]}
                for p in self._pontos
            ]
            
            parametros = self.sidebar.get_parametros()
            fig_regressao = self._gerar_figura_regressao()
            resultados_mardia = self._obter_resultados_mardia()
            
            video_info = None
            if hasattr(self, '_video_path_used') and self._video_path_used:
                import hashlib
                import subprocess
                sha512 = hashlib.sha512()
                try:
                    with open(self._video_path_used, "rb") as f:
                        for chunk in iter(lambda: f.read(4096 * 1024), b""):
                            sha512.update(chunk)
                    hash_str = sha512.hexdigest()
                    
                    try:
                        mi_res = subprocess.run(["mediainfo", self._video_path_used], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=10)
                        if mi_res.returncode == 0 and mi_res.stdout.strip():
                            media_text = mi_res.stdout.strip()
                        else:
                            raise Exception()
                    except:
                        mi_res = subprocess.run([
                            "ffprobe", "-v", "quiet", "-show_format", "-show_streams", self._video_path_used
                        ], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=10)
                        media_text = mi_res.stdout.strip()
                    
                    video_info = {
                        "filename": os.path.basename(self._video_path_used),
                        "sha512": hash_str,
                        "mediainfo": media_text
                    }
                except Exception as e:
                    self._log_audit(f"Erro ao obter informacoes do video para relatorio: {e}")
            
            veiculo_id = ""
            if hasattr(self.video_tab, 'txt_veiculo'):
                veiculo_id = self.video_tab.txt_veiculo.text().strip()

            gerar_pdf_academico(
                caminho_saida=caminho,
                dados_pontos=dados_pontos if dados_pontos else None,
                parametros=parametros,
                resultado=self._resultado,
                figuras=figuras,
                fig_regressao=fig_regressao,
                resultados_mardia=resultados_mardia,
                audit_log=self._audit_log if self._audit_log else None,
                video_info=video_info,
                regression_metrics=getattr(self, '_regression_metrics', None),
                veiculo_id=veiculo_id
            )
            
            QMessageBox.information(self, "Relatorio", f"Relatorio gerado com sucesso!\n{caminho}")
            self.statusBar().showMessage(f"Relatorio PDF gerado: {caminho}")
        except Exception as e:
            QMessageBox.critical(self, "Erro no Relatorio", f"Falha ao gerar o relatorio:\n{e}")
