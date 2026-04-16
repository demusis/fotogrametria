from __future__ import annotations

import numpy as np
from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QSlider, QLabel, QFileDialog, QMessageBox, QSplitter, QGroupBox, QColorDialog
from PySide6.QtGui import QPixmap, QImage, QPainter, QColor, QPen
from PySide6.QtCore import Qt, Signal

from .image_viewer import ImageViewer
from app.core.image_ops import carregar_imagem

class ImageCombinerTab(QWidget):
    enviar_imagem = Signal(np.ndarray)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.img1_array = None
        self.img2_array = None
        self.combined_img = None
        self.combined_img_cropped = None
        self.line_color = (0.0, 0.0, 0.0) # RGB norm
        
        self._init_ui()

    def _init_ui(self):
        main_layout = QHBoxLayout(self)
        
        # Painel esquerdo (Controles)
        controls_panel = QWidget()
        controls_layout = QVBoxLayout(controls_panel)
        controls_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        
        import os
        
        # Botões de Carregar
        self.btn_load1 = QPushButton("Carregar Imagem Esquerda")
        self.btn_load1.clicked.connect(self._load_image1)
        controls_layout.addWidget(self.btn_load1)
        
        self.lbl_file1 = QLabel("Nenhum arquivo...")
        self.lbl_file1.setObjectName("lbl_file1")  # Para estilização específica se necessário
        controls_layout.addWidget(self.lbl_file1)
        
        self.btn_load2 = QPushButton("Carregar Imagem Direita")
        self.btn_load2.clicked.connect(self._load_image2)
        controls_layout.addWidget(self.btn_load2)
        
        self.lbl_file2 = QLabel("Nenhum arquivo...")
        self.lbl_file2.setObjectName("lbl_file2")
        controls_layout.addWidget(self.lbl_file2)
        
        self.btn_reset = QPushButton("🔄 Reiniciar")
        self.btn_reset.clicked.connect(self._reset_all)
        self.btn_reset.setStyleSheet("background-color: #B53A3A; color: white;")
        controls_layout.addWidget(self.btn_reset)
        
        # Sliders
        controls_layout.addWidget(QLabel("Posição da Divisão (%):"))
        self.slider_split = QSlider(Qt.Orientation.Horizontal)
        self.slider_split.setRange(0, 10000)
        self.slider_split.setSingleStep(10)
        self.slider_split.setPageStep(500)
        self.slider_split.setValue(5000)
        self.slider_split.valueChanged.connect(self._update_combined)
        controls_layout.addWidget(self.slider_split)
        
        controls_layout.addWidget(QLabel("Espessura da linha (px):"))
        self.slider_line = QSlider(Qt.Orientation.Horizontal)
        self.slider_line.setRange(0, 50)
        self.slider_line.setValue(2)
        self.slider_line.valueChanged.connect(self._update_combined)
        controls_layout.addWidget(self.slider_line)
        
        self.btn_line_color = QPushButton("Escolher Cor da Linha")
        self.btn_line_color.clicked.connect(self._choose_line_color)
        controls_layout.addWidget(self.btn_line_color)
        
        # Botão Salvar
        self.btn_save = QPushButton("Salvar Imagem Combinada")
        self.btn_save.clicked.connect(self._save_image)
        controls_layout.addWidget(self.btn_save)
        
        self.btn_send = QPushButton("⬅️ Enviar p/ Correção Lente")
        self.btn_send.clicked.connect(self._send_image)
        self.btn_send.setStyleSheet("background-color: #0077BB; color: #ffffff;")
        controls_layout.addWidget(self.btn_send)
        
        controls_panel.setMinimumWidth(250)
        controls_panel.setMaximumWidth(300)
        self.controls_panel = controls_panel
        
        # Painel direito (Visualização)
        self.viewer = ImageViewer()
        
        main_layout.addWidget(controls_panel)
        main_layout.addWidget(self.viewer, stretch=1)

    def set_theme(self, theme: str):
        """Aplica o tema (dark/light) aos controles internos."""
        if theme == "dark":
            style = """
                QWidget { background-color: #0e1117; color: #e0e0e0; }
                QLabel { color: #e0e0e0; background-color: transparent; }
                QGroupBox { color: #33BBEE; font-weight: bold; border: 1px solid #30363d; border-radius: 4px; margin-top: 10px; padding-top: 15px; }
                QGroupBox::title { subcontrol-origin: margin; left: 8px; padding: 0 3px 0 3px; }
                QPushButton { background-color: #1a1a2e; color: #e0e0e0; border: 1px solid #30363d; padding: 5px; border-radius: 3px; }
                QPushButton:hover { background-color: #2a2a4e; }
                #lbl_file1, #lbl_file2 { color: #888888; font-size: 10px; }
                QSlider::groove:horizontal { border: 1px solid #30363d; height: 4px; background: #1a1a2e; margin: 2px 0; border-radius: 2px; }
                QSlider::handle:horizontal { background: #33BBEE; border: 1px solid #33BBEE; width: 14px; height: 14px; margin: -5px 0; border-radius: 7px; }
            """
        else:
            style = """
                QWidget { background-color: #f5f5f5; color: #333333; }
                QLabel { color: #333333; background-color: transparent; }
                QGroupBox { color: #212529; font-weight: bold; border: 1px solid #cccccc; border-radius: 4px; margin-top: 10px; padding-top: 15px; }
                QGroupBox::title { subcontrol-origin: margin; left: 8px; padding: 0 3px 0 3px; }
                QPushButton { background-color: #e0e0e0; color: #333333; border: 1px solid #cccccc; padding: 5px; border-radius: 3px; }
                QPushButton:hover { background-color: #d0d0d0; }
                #lbl_file1, #lbl_file2 { color: #666666; font-size: 10px; }
                QSlider::groove:horizontal { border: 1px solid #cccccc; height: 4px; background: #e0e0e0; margin: 2px 0; border-radius: 2px; }
                QSlider::handle:horizontal { background: #0077BB; border: 1px solid #0077BB; width: 14px; height: 14px; margin: -5px 0; border-radius: 7px; }
            """
        self.controls_panel.setStyleSheet(style)
        self.viewer._apply_theme(theme)

    def _load_image1(self):
        path, _ = QFileDialog.getOpenFileName(self, "Selecionar Imagem 1", "", "Images (*.png *.jpg *.jpeg)")
        if path:
            import os
            self.lbl_file1.setText(os.path.basename(path))
            self.img1_array = carregar_imagem(path)
            self._update_combined()

    def _load_image2(self):
        path, _ = QFileDialog.getOpenFileName(self, "Selecionar Imagem 2", "", "Images (*.png *.jpg *.jpeg)")
        if path:
            import os
            self.lbl_file2.setText(os.path.basename(path))
            self.img2_array = carregar_imagem(path)
            self._update_combined()

    def _reset_all(self):
        self.img1_array = None
        self.img2_array = None
        self.combined_img = None
        self.combined_img_cropped = None
        
        self.lbl_file1.setText("Nenhum arquivo...")
        self.lbl_file2.setText("Nenhum arquivo...")
        
        # Bloquear sinais evita execuções desnecessárias durante o reset
        self.slider_split.blockSignals(True)
        self.slider_split.setValue(5000)
        self.slider_split.blockSignals(False)
        
        self.slider_line.blockSignals(True)
        self.slider_line.setValue(2)
        self.slider_line.blockSignals(False)
        
        self.viewer._scene.clear()

    def _choose_line_color(self):
        c = [int(v * 255) for v in self.line_color]
        color = QColorDialog.getColor(QColor(*c))
        if color.isValid():
            self.line_color = (color.red() / 255.0, color.green() / 255.0, color.blue() / 255.0)
            self._update_combined()

    def _update_combined(self):
        if self.img1_array is None or self.img2_array is None:
            return
            
        if self.img1_array.shape != self.img2_array.shape:
            QMessageBox.warning(self, "Erro", "As imagens devem ter as mesmas dimensões!")
            return
            
        h, w = self.img1_array.shape[:2]
        split_pct = self.slider_split.value() / 10000.0
        split_px = int(round(split_pct * w))
        
        line_thick = self.slider_line.value()
        
        self.combined_img = np.zeros_like(self.img1_array)
        if split_px > 0:
            self.combined_img[:, :split_px] = self.img1_array[:, :split_px]
        if split_px < w:
            self.combined_img[:, split_px:] = self.img2_array[:, split_px:]
            
        if line_thick > 0:
            half = line_thick // 2
            start_x = max(0, split_px - half)
            end_x = min(w, split_px + (line_thick - half))
            # preenche RGB
            self.combined_img[:, start_x:end_x, 0] = self.line_color[0]
            self.combined_img[:, start_x:end_x, 1] = self.line_color[1]
            self.combined_img[:, start_x:end_x, 2] = self.line_color[2]
            
        self.combined_img_cropped = self.combined_img
            
        self.viewer.carregar_imagem(self.combined_img_cropped)

    def _save_image(self):
        if self.combined_img_cropped is None:
            QMessageBox.warning(self, "Aviso", "Nenhuma imagem combinada gerada.")
            return
        path, _ = QFileDialog.getSaveFileName(self, "Salvar Imagem", "imagem_combinada.png", "PNG (*.png)")
        if path:
            try:
                import cv2
                img_uint8 = (self.combined_img_cropped * 255).astype(np.uint8)
                img_bgr = cv2.cvtColor(img_uint8, cv2.COLOR_RGB2BGR)
                success, encoded_image = cv2.imencode(".png", img_bgr)
                if success:
                    with open(path, "wb") as f:
                        f.write(encoded_image)
                    QMessageBox.information(self, "Sucesso", "Imagem salva com sucesso!")
                else:
                    QMessageBox.critical(self, "Erro", "Erro ao codificar a imagem.")
            except Exception as e:
                QMessageBox.critical(self, "Erro", f"Falha ao salvar a imagem:\n{e}")

    def _send_image(self):
        if self.combined_img_cropped is None:
            QMessageBox.warning(self, "Aviso", "Combine as imagens primeiro antes de enviar.")
            return
        
        self.enviar_imagem.emit(self.combined_img_cropped.copy())
