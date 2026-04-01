"""
Painel lateral (sidebar) com controles de entrada organizados por seção.
Inclui toggle de tema claro/escuro.
"""

from __future__ import annotations

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QDoubleSpinBox, QSpinBox, QSlider, QGroupBox, QFileDialog,
    QScrollArea, QFrame, QToolTip, QComboBox,
)
from PySide6.QtGui import QIcon, QFont, QCursor
from PySide6.QtCore import Qt, Signal


class HelpLabel(QLabel):
    """Label com ícone de ajuda (tooltip ao passar o mouse)."""

    def __init__(self, text: str, help_text: str, parent=None):
        super().__init__(text, parent)
        self._help_text = help_text
        self.setFont(QFont("Segoe UI", 9))
        self.setCursor(QCursor(Qt.CursorShape.WhatsThisCursor))
        self.setToolTip(help_text)


class LabeledSlider(QWidget):
    """Slider horizontal com label que mostra o valor atual."""
    valueChanged = Signal(int)

    def __init__(self, min_val: int, max_val: int, default: int, suffix: str = "", parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        self._slider = QSlider(Qt.Orientation.Horizontal)
        self._slider.setRange(min_val, max_val)
        self._slider.setValue(default)
        self._slider.setTickInterval(max(1, (max_val - min_val) // 10))
        layout.addWidget(self._slider, stretch=1)

        self._suffix = suffix
        self._label = QLabel(f"{default}{suffix}")
        self._label.setMinimumWidth(55)
        self._label.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        self._label.setFont(QFont("Segoe UI", 10, QFont.Weight.Bold))
        layout.addWidget(self._label)

        self._slider.valueChanged.connect(self._on_changed)

    def _on_changed(self, val: int):
        self._label.setText(f"{val}{self._suffix}")
        self.valueChanged.emit(val)

    def value(self) -> int:
        return self._slider.value()


class Sidebar(QWidget):
    """Painel lateral com todos os controles de entrada."""

    # Signals
    imagem_selecionada = Signal(str)
    csv_selecionado = Signal(str)
    apagar_ultimo = Signal()
    apagar_tudo = Signal()
    calcular = Signal()
    exportar = Signal()
    zoom_alterado = Signal(float)
    tema_alterado = Signal(str)  # "dark" ou "light"
    ver_regressao = Signal()  # Solicita popup de regressão sobre imagem

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setFixedWidth(340)

        self._current_theme = "dark"

        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)

        content = QWidget()
        layout = QVBoxLayout(content)
        layout.setSpacing(8)
        layout.setContentsMargins(12, 12, 12, 12)

        # ===== Título =====
        titulo = QLabel("Fotogrametria")
        titulo.setFont(QFont("Segoe UI", 16, QFont.Weight.Bold))
        titulo.setAlignment(Qt.AlignmentFlag.AlignCenter)
        titulo.setObjectName("lbl_titulo")
        layout.addWidget(titulo)

        subtitulo = QLabel("Estimativa da velocidade veicular por razão cruzada")
        subtitulo.setFont(QFont("Segoe UI", 10))
        subtitulo.setAlignment(Qt.AlignmentFlag.AlignCenter)
        subtitulo.setObjectName("lbl_subtitulo")
        subtitulo.setWordWrap(True)
        layout.addWidget(subtitulo)

        # ===== Seção: Aparência =====
        grp_aparencia = QGroupBox("Aparência")
        vbox = QVBoxLayout(grp_aparencia)

        hbox_tema = QHBoxLayout()
        lbl_tema = QLabel("Tema:")
        lbl_tema.setFont(QFont("Segoe UI", 9))
        hbox_tema.addWidget(lbl_tema)

        self.combo_tema = QComboBox()
        self.combo_tema.addItems(["🌙 Escuro", "☀️ Claro"])
        self.combo_tema.currentIndexChanged.connect(self._on_tema_changed)
        hbox_tema.addWidget(self.combo_tema)
        vbox.addLayout(hbox_tema)

        layout.addWidget(grp_aparencia)

        # ===== Seção: Imagem =====
        grp_img = QGroupBox("Imagem")
        vbox = QVBoxLayout(grp_img)

        btn_abrir = QPushButton("📂  Abrir Imagem...")
        btn_abrir.clicked.connect(self._selecionar_imagem)
        vbox.addWidget(btn_abrir)

        self.lbl_nome_imagem = QLabel("Nenhuma imagem selecionada")
        self.lbl_nome_imagem.setWordWrap(True)
        self.lbl_nome_imagem.setObjectName("lbl_secundario")
        vbox.addWidget(self.lbl_nome_imagem)

        vbox.addWidget(HelpLabel("Nível de Zoom:", "Ajuste o zoom da imagem."))
        self.slider_zoom = QSlider(Qt.Orientation.Horizontal)
        self.slider_zoom.setRange(10, 300)
        self.slider_zoom.setValue(100)
        self.slider_zoom.setTickInterval(10)
        self.slider_zoom.valueChanged.connect(lambda v: self.zoom_alterado.emit(v / 100.0))
        vbox.addWidget(self.slider_zoom)

        layout.addWidget(grp_img)

        # ===== Seção: Marcações =====
        grp_marc = QGroupBox("Gerenciar Marcações")
        vbox = QVBoxLayout(grp_marc)

        hbox = QHBoxLayout()
        btn_apagar = QPushButton("↩ Desfazer Último")
        btn_apagar.clicked.connect(self.apagar_ultimo.emit)
        hbox.addWidget(btn_apagar)

        btn_limpar = QPushButton("🗑 Limpar Tudo")
        btn_limpar.clicked.connect(self.apagar_tudo.emit)
        hbox.addWidget(btn_limpar)
        vbox.addLayout(hbox)

        # Botão de visualizar regressão sobre imagem
        btn_regressao = QPushButton("📈  Ver Regressão sobre Imagem")
        btn_regressao.setToolTip("Abre uma janela popup mostrando a reta de regressão ajustada sobre a imagem.")
        btn_regressao.clicked.connect(self.ver_regressao.emit)
        vbox.addWidget(btn_regressao)

        layout.addWidget(grp_marc)

        # ===== Seção: Parâmetros Temporais =====
        grp_tempo = QGroupBox("Parâmetros Temporais")
        vbox = QVBoxLayout(grp_tempo)

        vbox.addWidget(HelpLabel("Quadro inicial (s):", "Tempo do primeiro frame utilizado na estimativa."))
        self.spin_inicio = QDoubleSpinBox()
        self.spin_inicio.setRange(0, 999999)
        self.spin_inicio.setDecimals(5)
        self.spin_inicio.setValue(1.266)
        self.spin_inicio.setSingleStep(0.00001)
        vbox.addWidget(self.spin_inicio)

        vbox.addWidget(HelpLabel("Quadro final (s):", "Tempo do segundo frame utilizado na estimativa."))
        self.spin_fim = QDoubleSpinBox()
        self.spin_fim.setRange(0, 999999)
        self.spin_fim.setDecimals(5)
        self.spin_fim.setValue(1.866)
        self.spin_fim.setSingleStep(0.00001)
        vbox.addWidget(self.spin_fim)

        vbox.addWidget(HelpLabel("Erro médio marcação temporal (ms/s):", "Erro médio estimado na marcação dos tempos."))
        self.spin_erro_medio = QDoubleSpinBox()
        self.spin_erro_medio.setRange(0.01, 5)
        self.spin_erro_medio.setDecimals(5)
        self.spin_erro_medio.setValue(0.881)
        self.spin_erro_medio.setSingleStep(0.001)
        vbox.addWidget(self.spin_erro_medio)

        vbox.addWidget(HelpLabel("DP do erro médio (ms/s):", "Desvio padrão do erro médio, para simulação de incertezas."))
        self.spin_dp_erro = QDoubleSpinBox()
        self.spin_dp_erro.setRange(0.01, 5)
        self.spin_dp_erro.setDecimals(5)
        self.spin_dp_erro.setValue(0.287)
        self.spin_dp_erro.setSingleStep(0.001)
        vbox.addWidget(self.spin_dp_erro)

        layout.addWidget(grp_tempo)

        # ===== Seção: Parâmetros de Referência =====
        grp_ref = QGroupBox("Parâmetros de Referência")
        vbox = QVBoxLayout(grp_ref)

        vbox.addWidget(HelpLabel("Distância de referência (mm):", "Distância real conhecida entre os centros das rodas."))
        self.spin_dist_ref = QDoubleSpinBox()
        self.spin_dist_ref.setRange(0.01, 99999)
        self.spin_dist_ref.setDecimals(1)
        self.spin_dist_ref.setValue(2002.0)
        self.spin_dist_ref.setSingleStep(1)
        vbox.addWidget(self.spin_dist_ref)

        layout.addWidget(grp_ref)

        # ===== Seção: Simulação =====
        grp_sim = QGroupBox("Simulação Monte Carlo")
        vbox = QVBoxLayout(grp_sim)

        vbox.addWidget(HelpLabel("Filtro Gaussiano (σ):", "Suavização aplicada à imagem antes do cálculo da reflectância. 0 = sem filtro."))
        self.slider_sigma = LabeledSlider(0, 100, 0, suffix="")
        vbox.addWidget(self.slider_sigma)

        vbox.addWidget(HelpLabel("Nº de repetições:", "Número de simulações de Monte Carlo."))
        self.slider_repeticoes = LabeledSlider(10, 10000, 100, suffix="")
        vbox.addWidget(self.slider_repeticoes)

        vbox.addWidget(HelpLabel("Nível de confiança:", "Nível de confiança para os intervalos (0 a 1)."))
        self.spin_nc = QDoubleSpinBox()
        self.spin_nc.setRange(0.00001, 0.99999)
        self.spin_nc.setDecimals(5)
        self.spin_nc.setValue(0.99)
        self.spin_nc.setSingleStep(0.01)
        vbox.addWidget(self.spin_nc)

        layout.addWidget(grp_sim)

        # ===== Seção: CSV opcional =====
        grp_csv = QGroupBox("Arquivo CSV (opcional)")
        vbox = QVBoxLayout(grp_csv)

        btn_csv = QPushButton("📄  Carregar CSV...")
        btn_csv.clicked.connect(self._selecionar_csv)
        vbox.addWidget(btn_csv)

        self.lbl_csv = QLabel("Nenhum arquivo CSV")
        self.lbl_csv.setObjectName("lbl_secundario")
        vbox.addWidget(self.lbl_csv)

        layout.addWidget(grp_csv)

        # ===== Botões de ação =====
        btn_calc = QPushButton("⚡  Calcular!")
        btn_calc.setObjectName("btn_calcular")
        btn_calc.clicked.connect(self.calcular.emit)
        layout.addWidget(btn_calc)

        btn_exp = QPushButton("📦  Exportar Gráficos e Dados (.zip)")
        btn_exp.setObjectName("btn_exportar")
        btn_exp.clicked.connect(self.exportar.emit)
        layout.addWidget(btn_exp)

        # Spacer
        layout.addStretch(1)

        scroll_area.setWidget(content)

        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.addWidget(scroll_area)

        # Aplicar tema inicial
        self._apply_theme("dark")

    def _on_tema_changed(self, index: int):
        theme = "dark" if index == 0 else "light"
        self._current_theme = theme
        self._apply_theme(theme)
        self.tema_alterado.emit(theme)

    def _apply_theme(self, theme: str):
        """Aplica o tema claro ou escuro ao sidebar."""
        if theme == "dark":
            self.setStyleSheet("""
                QWidget {
                    background-color: #16213e;
                    color: #e0e0e0;
                }
                QGroupBox {
                    font-weight: bold;
                    font-size: 12px;
                    border: 1px solid #1a1a40;
                    border-radius: 8px;
                    margin-top: 12px;
                    padding-top: 18px;
                    background-color: #1a1a40;
                }
                QGroupBox::title {
                    subcontrol-origin: margin;
                    left: 12px;
                    padding: 0 6px;
                    color: #33BBEE;
                }
                #lbl_titulo {
                    color: #33BBEE;
                    margin-bottom: 8px;
                }
                #lbl_subtitulo {
                    color: #aaaaaa;
                    margin-bottom: 12px;
                }
                #lbl_secundario {
                    color: #888888;
                    font-size: 10px;
                }
                QLabel {
                    color: #e0e0e0;
                }
                QPushButton {
                    background-color: #0f3460;
                    color: #e0e0e0;
                    border: 1px solid #16213e;
                    border-radius: 6px;
                    padding: 8px 16px;
                    font-weight: bold;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background-color: #1a4a8a;
                }
                QPushButton:pressed {
                    background-color: #0a2540;
                }
                QPushButton#btn_calcular {
                    background-color: #009988;
                    font-size: 13px;
                    padding: 10px 20px;
                }
                QPushButton#btn_calcular:hover {
                    background-color: #00bfa5;
                }
                QPushButton#btn_exportar {
                    background-color: #EE7733;
                }
                QPushButton#btn_exportar:hover {
                    background-color: #ff9955;
                }
                QDoubleSpinBox, QSpinBox {
                    background-color: #0a1628;
                    color: #e0e0e0;
                    border: 1px solid #1a1a40;
                    border-radius: 4px;
                    padding: 4px;
                    font-size: 11px;
                }
                QComboBox {
                    background-color: #0a1628;
                    color: #e0e0e0;
                    border: 1px solid #1a1a40;
                    border-radius: 4px;
                    padding: 4px;
                    font-size: 11px;
                }
                QComboBox QAbstractItemView {
                    background-color: #0a1628;
                    color: #e0e0e0;
                    selection-background-color: #0f3460;
                }
                QSlider::groove:horizontal {
                    background: #0a1628;
                    height: 6px;
                    border-radius: 3px;
                }
                QSlider::handle:horizontal {
                    background: #33BBEE;
                    width: 16px;
                    margin: -5px 0;
                    border-radius: 8px;
                }
            """)
        else:
            self.setStyleSheet("""
                QWidget {
                    background-color: #f5f5f5;
                    color: #333333;
                }
                QGroupBox {
                    font-weight: bold;
                    font-size: 12px;
                    border: 1px solid #cccccc;
                    border-radius: 8px;
                    margin-top: 12px;
                    padding-top: 18px;
                    background-color: #ffffff;
                }
                QGroupBox::title {
                    subcontrol-origin: margin;
                    left: 12px;
                    padding: 0 6px;
                    color: #0077BB;
                }
                #lbl_titulo {
                    color: #0077BB;
                    margin-bottom: 8px;
                }
                #lbl_subtitulo {
                    color: #666666;
                    margin-bottom: 12px;
                }
                #lbl_secundario {
                    color: #999999;
                    font-size: 10px;
                }
                QLabel {
                    color: #333333;
                }
                QPushButton {
                    background-color: #e0e0e0;
                    color: #333333;
                    border: 1px solid #cccccc;
                    border-radius: 6px;
                    padding: 8px 16px;
                    font-weight: bold;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background-color: #d0d0d0;
                }
                QPushButton:pressed {
                    background-color: #c0c0c0;
                }
                QPushButton#btn_calcular {
                    background-color: #009988;
                    color: #ffffff;
                    font-size: 13px;
                    padding: 10px 20px;
                }
                QPushButton#btn_calcular:hover {
                    background-color: #00bfa5;
                }
                QPushButton#btn_exportar {
                    background-color: #EE7733;
                    color: #ffffff;
                }
                QPushButton#btn_exportar:hover {
                    background-color: #ff9955;
                }
                QDoubleSpinBox, QSpinBox {
                    background-color: #ffffff;
                    color: #333333;
                    border: 1px solid #cccccc;
                    border-radius: 4px;
                    padding: 4px;
                    font-size: 11px;
                }
                QComboBox {
                    background-color: #ffffff;
                    color: #333333;
                    border: 1px solid #cccccc;
                    border-radius: 4px;
                    padding: 4px;
                    font-size: 11px;
                }
                QComboBox QAbstractItemView {
                    background-color: #ffffff;
                    color: #333333;
                    selection-background-color: #d0e8ff;
                }
                QSlider::groove:horizontal {
                    background: #d0d0d0;
                    height: 6px;
                    border-radius: 3px;
                }
                QSlider::handle:horizontal {
                    background: #0077BB;
                    width: 16px;
                    margin: -5px 0;
                    border-radius: 8px;
                }
            """)

    def _selecionar_imagem(self):
        caminho, _ = QFileDialog.getOpenFileName(
            self, "Selecionar Imagem", "",
            "Imagens (*.jpg *.jpeg *.png);;Todos (*)"
        )
        if caminho:
            self.lbl_nome_imagem.setText(caminho.split("/")[-1].split("\\")[-1])
            self.imagem_selecionada.emit(caminho)

    def _selecionar_csv(self):
        caminho, _ = QFileDialog.getOpenFileName(
            self, "Selecionar CSV", "",
            "CSV (*.csv);;Todos (*)"
        )
        if caminho:
            self.lbl_csv.setText(caminho.split("/")[-1].split("\\")[-1])
            self.csv_selecionado.emit(caminho)

    def get_parametros(self) -> dict:
        """Retorna todos os parâmetros em um dict."""
        return {
            "inicio_quadro": self.spin_inicio.value(),
            "fim_quadro": self.spin_fim.value(),
            "erro_medio_mt": self.spin_erro_medio.value(),
            "dp_erro_medio_mt": self.spin_dp_erro.value(),
            "dist_referencia": self.spin_dist_ref.value(),
            "sigma_filtro": self.slider_sigma.value(),
            "repeticoes": self.slider_repeticoes.value(),
            "nivel_confianca": self.spin_nc.value(),
        }
