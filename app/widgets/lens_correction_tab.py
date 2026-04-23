import numpy as np
import cv2
from PySide6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QPushButton, 
                               QSlider, QLabel, QFileDialog, QMessageBox, QGroupBox,
                               QScrollArea, QLineEdit, QComboBox, QSpinBox, QColorDialog, QFrame)
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QColor
from .image_viewer import ImageViewer
from app.core.image_ops import carregar_imagem
from app.core.geom_ops import optimize_lens_distortion, distort_coords

class LensCorrectionTab(QWidget):
    enviar_imagem = Signal(np.ndarray)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.img_array = None
        self._base_corrected_img = None
        self.undistorted_img = None
        self.polylines = []
        self.current_poly = []
        self.k1 = 0.0
        self.k2 = 0.0
        self.k3 = 0.0
        self.p1 = 0.0
        self.p2 = 0.0
        self.custom_cx = None
        self.custom_cy = None
        self._center_select_mode = False
        
        # text configs
        self.txt_e_cfg = {"text": "", "size": 18, "pos": 0, "color": (255, 255, 255), "bg": (0, 0, 0)}
        self.txt_d_cfg = {"text": "", "size": 18, "pos": 0, "color": (255, 255, 255), "bg": (0, 0, 0)}
        
        self._init_ui()

    def _init_ui(self):
        main_layout = QHBoxLayout(self)
        
        # Area de Rolagem
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setMaximumWidth(420)
        scroll_area.setMinimumWidth(350)
        scroll_area.setFrameShape(QFrame.Shape.NoFrame)
        self.scroll_area = scroll_area
        
        # Painel Controles
        controls_panel = QWidget()
        self.controls_panel = controls_panel
        controls_layout = QVBoxLayout(controls_panel)
        controls_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        
        btn_load = QPushButton("Carregar Imagem")
        btn_load.clicked.connect(self._load_image)
        controls_layout.addWidget(btn_load)
        
        # Sliders Manual
        group_manual = QGroupBox("Ajuste Manual")
        man_layout = QVBoxLayout(group_manual)
        self.lbl_k1 = QLabel("k1: 0.000")
        self.sl_k1 = QSlider(Qt.Orientation.Horizontal)
        self.sl_k1.setRange(-3000, 3000)
        self.sl_k1.setValue(0)
        self.sl_k1.valueChanged.connect(self._on_sliders_changed)
        man_layout.addWidget(self.lbl_k1)
        man_layout.addWidget(self.sl_k1)
        
        self.lbl_k2 = QLabel("k2: 0.000")
        self.sl_k2 = QSlider(Qt.Orientation.Horizontal)
        self.sl_k2.setRange(-1500, 1500)
        self.sl_k2.setValue(0)
        self.sl_k2.valueChanged.connect(self._on_sliders_changed)
        man_layout.addWidget(self.lbl_k2)
        man_layout.addWidget(self.sl_k2)
        
        self.lbl_p1 = QLabel("p1 (Tangencial): 0.000")
        self.sl_p1 = QSlider(Qt.Orientation.Horizontal)
        self.sl_p1.setRange(-500, 500)
        self.sl_p1.setValue(0)
        self.sl_p1.valueChanged.connect(self._on_sliders_changed)
        man_layout.addWidget(self.lbl_p1)
        man_layout.addWidget(self.sl_p1)
        
        self.lbl_p2 = QLabel("p2 (Tangencial): 0.000")
        self.sl_p2 = QSlider(Qt.Orientation.Horizontal)
        self.sl_p2.setRange(-500, 500)
        self.sl_p2.setValue(0)
        self.sl_p2.valueChanged.connect(self._on_sliders_changed)
        man_layout.addWidget(self.lbl_p2)
        man_layout.addWidget(self.sl_p2)
        
        self.lbl_k3 = QLabel("k3: 0.000")
        self.sl_k3 = QSlider(Qt.Orientation.Horizontal)
        self.sl_k3.setRange(-1000, 1000)
        self.sl_k3.setValue(0)
        self.sl_k3.valueChanged.connect(self._on_sliders_changed)
        man_layout.addWidget(self.lbl_k3)
        man_layout.addWidget(self.sl_k3)
        
        btn_reset = QPushButton("Zerar Ajuste")
        btn_reset.clicked.connect(self._reset_sliders)
        man_layout.addWidget(btn_reset)
        
        self.lbl_center = QLabel("Centro Ótico: Padrão (centro da imagem)")
        self.lbl_center.setWordWrap(True)
        man_layout.addWidget(self.lbl_center)
        
        center_btns = QHBoxLayout()
        self.btn_select_center = QPushButton("🎯 Selecionar Centro")
        self.btn_select_center.setCheckable(True)
        self.btn_select_center.clicked.connect(self._toggle_center_mode)
        center_btns.addWidget(self.btn_select_center)
        
        btn_reset_center = QPushButton("Resetar Centro")
        btn_reset_center.clicked.connect(self._reset_center)
        center_btns.addWidget(btn_reset_center)
        man_layout.addLayout(center_btns)
        
        controls_layout.addWidget(group_manual)
        
        # Calibração com Linhas
        group_auto = QGroupBox("Otimização Automática")
        auto_layout = QVBoxLayout(group_auto)
        
        lbl_auto = QLabel("Clique para marcar pontos. Duplo clique ou botão p/ fechar a linha.")
        lbl_auto.setWordWrap(True)
        auto_layout.addWidget(lbl_auto)
        
        btn_finish = QPushButton("Finalizar Linha Atual")
        btn_finish.clicked.connect(self._finish_line)
        auto_layout.addWidget(btn_finish)
        
        btn_clear = QPushButton("Limpar Todas Linhas")
        btn_clear.clicked.connect(self._clear_lines)
        auto_layout.addWidget(btn_clear)
        
        btn_opt = QPushButton("Propor Ajuste (Otimizar)")
        btn_opt.clicked.connect(self._optimize)
        auto_layout.addWidget(btn_opt)
        
        controls_layout.addWidget(group_auto)
        # Crop
        group_crop = QGroupBox("Recorte (Crop) %")
        crop_layout = QVBoxLayout(group_crop)
        
        hbox1 = QHBoxLayout()
        hbox1.addWidget(QLabel("Topo:"))
        self.sl_crop_t = QSlider(Qt.Orientation.Horizontal)
        self.sl_crop_t.setRange(0, 49)
        self.sl_crop_t.setValue(0)
        self.sl_crop_t.valueChanged.connect(self._apply_correction)
        hbox1.addWidget(self.sl_crop_t)
        
        hbox1.addWidget(QLabel("Base:"))
        self.sl_crop_b = QSlider(Qt.Orientation.Horizontal)
        self.sl_crop_b.setRange(0, 49)
        self.sl_crop_b.setValue(0)
        self.sl_crop_b.valueChanged.connect(self._apply_correction)
        hbox1.addWidget(self.sl_crop_b)
        crop_layout.addLayout(hbox1)
        
        hbox2 = QHBoxLayout()
        hbox2.addWidget(QLabel("Esq:"))
        self.sl_crop_l = QSlider(Qt.Orientation.Horizontal)
        self.sl_crop_l.setRange(0, 49)
        self.sl_crop_l.setValue(0)
        self.sl_crop_l.valueChanged.connect(self._apply_correction)
        hbox2.addWidget(self.sl_crop_l)
        
        hbox2.addWidget(QLabel("Dir:"))
        self.sl_crop_r = QSlider(Qt.Orientation.Horizontal)
        self.sl_crop_r.setRange(0, 49)
        self.sl_crop_r.setValue(0)
        self.sl_crop_r.valueChanged.connect(self._apply_correction)
        hbox2.addWidget(self.sl_crop_r)
        crop_layout.addLayout(hbox2)
        
        controls_layout.addWidget(group_crop)
        
        # Textos
        controls_layout.addWidget(self._create_text_group("Texto Esquerda", True))
        controls_layout.addWidget(self._create_text_group("Texto Direita", False))

        # Salvar
        btn_save = QPushButton("Salvar Imagem Corrigida")
        btn_save.clicked.connect(self._save_image)
        controls_layout.addWidget(btn_save)
        
        self.btn_send = QPushButton("⬅️ Enviar p/ Fotogrametria")
        self.btn_send.clicked.connect(self._send_image)
        self.btn_send.setStyleSheet("background-color: #009988; color: #ffffff;")
        controls_layout.addWidget(self.btn_send)
        
        scroll_area.setWidget(controls_panel)
        
        # Visualização
        self.viewer = ImageViewer()
        self.viewer.ponto_marcado.connect(self._add_point)
        self.viewer.duplo_clique.connect(self._finish_line)
        self.viewer.set_marking_mode(True)
        
        main_layout.addWidget(scroll_area)
        main_layout.addWidget(self.viewer, stretch=1)

    def set_theme(self, theme: str):
        """Aplica o tema (dark/light) aos controles internos."""
        if theme == "dark":
            style = """
                QWidget { background-color: #0e1117; color: #e0e0e0; }
                QLabel { color: #e0e0e0; border: none; }
                QGroupBox { color: #33BBEE; font-weight: bold; border: 1px solid #30363d; border-radius: 4px; margin-top: 10px; }
                QGroupBox::title { subcontrol-origin: margin; left: 8px; padding: 0 3px 0 3px; }
                QScrollArea { background-color: transparent; border: none; }
            """
        else:
            style = """
                QWidget { background-color: #f5f5f5; color: #333333; }
                QLabel { color: #333333; border: none; }
                QGroupBox { color: #212529; font-weight: bold; border: 1px solid #cccccc; border-radius: 4px; margin-top: 10px; }
                QGroupBox::title { subcontrol-origin: margin; left: 8px; padding: 0 3px 0 3px; }
                QScrollArea { background-color: transparent; border: none; }
            """
        self.controls_panel.setStyleSheet(style)
        self.scroll_area.setStyleSheet(f"QScrollArea {{ background-color: {'#0e1117' if theme == 'dark' else '#f5f5f5'}; border: none; }}")

    def _load_image(self):
        path, _ = QFileDialog.getOpenFileName(self, "Selecionar Imagem", "", "Imagens (*.png *.jpg *.jpeg)")
        if path:
            self.img_array = carregar_imagem(path)
            self._reset_sliders()
            self._reset_center()
            self._clear_lines()
            self._apply_correction()
            
    def receber_imagem(self, img_array: np.ndarray):
        """Recebe o numpy array de outra aba (e.g. aba Mesclar)."""
        self.img_array = img_array
        self._reset_sliders()
        self._reset_center()
        self._clear_lines()
        self._apply_correction()

    def _on_sliders_changed(self):
        self.k1 = self.sl_k1.value() / 1000.0
        self.k2 = self.sl_k2.value() / 1000.0
        self.p1 = self.sl_p1.value() / 1000.0
        self.p2 = self.sl_p2.value() / 1000.0
        self.k3 = self.sl_k3.value() / 1000.0
        self.lbl_k1.setText(f"k1: {self.k1:.3f}")
        self.lbl_k2.setText(f"k2: {self.k2:.3f}")
        self.lbl_p1.setText(f"p1 (Tangencial): {self.p1:.3f}")
        self.lbl_p2.setText(f"p2 (Tangencial): {self.p2:.3f}")
        self.lbl_k3.setText(f"k3: {self.k3:.3f}")
        
        self._apply_correction()

    def _reset_sliders(self):
        self.sl_k1.setValue(0)
        self.sl_k2.setValue(0)
        self.sl_p1.setValue(0)
        self.sl_p2.setValue(0)
        self.sl_k3.setValue(0)
        
    def _toggle_center_mode(self, checked):
        self._center_select_mode = checked
        if checked:
            self.btn_select_center.setStyleSheet("background-color: #EE6677; color: #ffffff; font-weight: bold;")
            self.lbl_center.setText("⚠ Clique na imagem para definir o novo centro ótico.")
        else:
            self.btn_select_center.setStyleSheet("")
            if self.custom_cx is not None:
                self.lbl_center.setText(f"Centro Ótico: ({int(self.custom_cx)}, {int(self.custom_cy)})")
            else:
                self.lbl_center.setText("Centro Ótico: Padrão (centro da imagem)")

    def _reset_center(self):
        self.custom_cx = None
        self.custom_cy = None
        self._center_select_mode = False
        if hasattr(self, "btn_select_center"):
            self.btn_select_center.setChecked(False)
            self.btn_select_center.setStyleSheet("")
        if hasattr(self, "lbl_center"):
            self.lbl_center.setText("Centro Ótico: Padrão (centro da imagem)")
        self._apply_correction()

    def _apply_correction(self):
        if self.img_array is None:
            return
            
        h, w = self.img_array.shape[:2]
        
        if self.k1 == 0 and self.k2 == 0 and self.k3 == 0:
            self._base_corrected_img = self.img_array.copy()
        else:
            f = max(w, h)
            cx = self.custom_cx if self.custom_cx is not None else w/2.0
            cy = self.custom_cy if self.custom_cy is not None else h/2.0
            
            K = np.array([[f, 0, cx], [0, f, cy], [0, 0, 1]], dtype=np.float32)
            dist_coeffs = np.array([-self.k1, -self.k2, -self.p1, -self.p2, -self.k3], dtype=np.float32)
            new_K, roi = cv2.getOptimalNewCameraMatrix(K, dist_coeffs, (w, h), 1, (w, h))
            mapx, mapy = cv2.initUndistortRectifyMap(K, dist_coeffs, None, new_K, (w, h), cv2.CV_32FC1)
            
            img_uint8 = (self.img_array * 255).astype(np.uint8)
            corrected = cv2.remap(img_uint8, mapx, mapy, cv2.INTER_LINEAR, borderMode=cv2.BORDER_CONSTANT, borderValue=(255, 255, 255))
            self._base_corrected_img = corrected.astype(np.float64) / 255.0

        # Aplicar Crop (usar dimensões da imagem corrigida, não da original)
        ch, cw = self._base_corrected_img.shape[:2]
        crop_t = int((self.sl_crop_t.value() / 100.0) * ch)
        crop_b = int((self.sl_crop_b.value() / 100.0) * ch)
        crop_l = int((self.sl_crop_l.value() / 100.0) * cw)
        crop_r = int((self.sl_crop_r.value() / 100.0) * cw)
        
        h_end = ch - crop_b
        w_end = cw - crop_r
        
        if crop_t < h_end and crop_l < w_end:
            self._base_corrected_img = self._base_corrected_img[crop_t:h_end, crop_l:w_end]
            
        self._update_text()

    def _add_point(self, x, y):
        # Se estiver no modo de seleção de centro ótico
        if self._center_select_mode:
            self.custom_cx = float(x)
            self.custom_cy = float(y)
            self.lbl_center.setText(f"Centro Ótico: ({int(x)}, {int(y)})")
            # Desativar o modo após selecionar
            self._center_select_mode = False
            self.btn_select_center.setChecked(False)
            self.btn_select_center.setStyleSheet("")
            self._apply_correction()
            return
            
        # Modo normal: adicionar pontos para linhas de otimização
        if self.k1 != 0 or self.k2 != 0 or self.k3 != 0:
            QMessageBox.information(self, "Aviso", "Zere os sliders antes de desenhar linhas de calibração.")
            return
            
        self.current_poly.append((x, y))
        self._draw_overlay()

    def _finish_line(self):
        if len(self.current_poly) > 1:
            self.polylines.append(list(self.current_poly))
        self.current_poly.clear()
        self._draw_overlay()

    def _clear_lines(self):
        self.polylines.clear()
        self.current_poly.clear()
        self._draw_overlay()

    def _draw_overlay(self):
        if self.undistorted_img is None:
            return
            
        img_display = self.undistorted_img.copy()
        img_uint8 = (img_display * 255).astype(np.uint8)
        overlay = img_uint8.copy()

        if self.k1 == 0 and self.k2 == 0 and self.k3 == 0:
            # Desenha as linhas finalizadas
            for poly in self.polylines:
                for i in range(len(poly)-1):
                    cv2.line(overlay, poly[i], poly[i+1], (0, 255, 255), 2)
                for pt in poly:
                    cv2.circle(overlay, pt, 5, (0, 255, 255), -1)
            # Desenha a linha atual
            for i in range(len(self.current_poly)-1):
                cv2.line(overlay, self.current_poly[i], self.current_poly[i+1], (255, 0, 255), 2)
            for pt in self.current_poly:
                cv2.circle(overlay, pt, 5, (255, 0, 255), -1)

        # Textos são SEMPRE desenhados por cima de tudo (após correções/filtros)
        self._draw_texts_on(overlay)

        self.viewer.carregar_imagem(overlay.astype(np.float64)/255.0)

    def _optimize(self):
        if self.img_array is None:
            return
        if not self.polylines:
            self._finish_line()
            if not self.polylines:
                QMessageBox.warning(self, "Aviso", "Desenhe linhas para usar a otimização.")
                return
                
        h, w = self.img_array.shape[:2]
        cx = self.custom_cx if self.custom_cx is not None else w/2.0
        cy = self.custom_cy if self.custom_cy is not None else h/2.0
        
        norm = np.sqrt(w**2 + h**2) / 2.0
        
        QMessageBox.information(self, "Otimização", "Otimizando parâmetros. Aguarde um instante...")
        
        opt_k, success = optimize_lens_distortion(self.polylines, cx, cy, norm, (self.k1, self.k2, self.p1, self.p2, self.k3))
        if success:
            QMessageBox.information(self, "Sucesso", "Otimização concluída.")
            self.sl_k1.setValue(int(opt_k[0]*1000))
            self.sl_k2.setValue(int(opt_k[1]*1000))
            self.sl_p1.setValue(int(opt_k[2]*1000))
            self.sl_p2.setValue(int(opt_k[3]*1000))
            self.sl_k3.setValue(int(opt_k[4]*1000))
            self.polylines.clear()
            self._apply_correction()
        else:
            QMessageBox.warning(self, "Falha", "Não foi possível convergir.")

    def _save_image(self):
        if self.undistorted_img is None:
            QMessageBox.warning(self, "Aviso", "Nenhuma imagem para salvar.")
            return
        path, _ = QFileDialog.getSaveFileName(self, "Salvar Imagem", "imagem_corrigida.png", "PNG (*.png)")
        if path:
            try:
                # Aplicar textos por cima da imagem final no momento de salvar
                final_img = self._compose_final_image()
                img_uint8 = (final_img * 255).astype(np.uint8)
                # Converte para BGR para o padrão de visualizadores de imagem
                img_bgr = cv2.cvtColor(img_uint8, cv2.COLOR_RGB2BGR)
                success, encoded_image = cv2.imencode(".png", img_bgr)
                if success:
                    with open(path, "wb") as f:
                        f.write(encoded_image)
                    QMessageBox.information(self, "Sucesso", "Imagem salva com sucesso!")
                else:
                    QMessageBox.critical(self, "Erro", "Erro ao codificar a imagem para PNG.")
            except Exception as e:
                QMessageBox.critical(self, "Erro", f"Falha ao salvar a imagem:\n{e}")

    def _send_image(self):
        if self.undistorted_img is not None:
            # Aplicar textos por cima da imagem final no momento de enviar
            final_img = self._compose_final_image()
            self.enviar_imagem.emit(final_img)
        else:
            QMessageBox.warning(self, "Aviso", "Corrija ou carregue uma imagem primeiro antes de enviar.")

    def _compose_final_image(self) -> np.ndarray:
        """Compõe a imagem final: imagem corrigida + textos por cima."""
        img = self.undistorted_img.copy()
        img_uint8 = (img * 255).astype(np.uint8)
        self._draw_texts_on(img_uint8)
        return img_uint8.astype(np.float64) / 255.0

    def _create_text_group(self, title: str, is_left: bool) -> QGroupBox:
        grp = QGroupBox(title)
        ly = QVBoxLayout(grp)
        
        cfg = self.txt_e_cfg if is_left else self.txt_d_cfg
        
        txt_input = QLineEdit()
        txt_input.setPlaceholderText("Digite o texto...")
        
        sz_spin = QSpinBox()
        sz_spin.setRange(8, 200)
        sz_spin.setValue(18)
        
        pos_cb = QComboBox()
        pos_cb.addItems(["Topo", "Meio", "Base"])
        
        btn_cor_txt = QPushButton("Cor Letra")
        btn_cor_bg = QPushButton("Cor Fundo")
        
        def pick_color_txt():
            color = QColorDialog.getColor(QColor(*cfg["color"]))
            if color.isValid():
                cfg["color"] = (color.red(), color.green(), color.blue())
                self._update_text()
                
        def pick_color_bg():
            color = QColorDialog.getColor(QColor(*cfg["bg"]))
            if color.isValid():
                cfg["bg"] = (color.red(), color.green(), color.blue())
                self._update_text()
                
        def update_txt(text):
            cfg["text"] = text
            self._update_text()
            
        def update_sz(val):
            cfg["size"] = val
            self._update_text()
            
        def update_pos(idx):
            cfg["pos"] = idx
            self._update_text()
            
        txt_input.textChanged.connect(update_txt)
        sz_spin.valueChanged.connect(update_sz)
        pos_cb.currentIndexChanged.connect(update_pos)
        btn_cor_txt.clicked.connect(pick_color_txt)
        btn_cor_bg.clicked.connect(pick_color_bg)
        
        h1 = QHBoxLayout()
        h1.addWidget(QLabel("Tam:"))
        h1.addWidget(sz_spin)
        h1.addWidget(QLabel("Pos:"))
        h1.addWidget(pos_cb)
        
        h2 = QHBoxLayout()
        h2.addWidget(btn_cor_txt)
        h2.addWidget(btn_cor_bg)
        
        ly.addWidget(txt_input)
        ly.addLayout(h1)
        ly.addLayout(h2)
        return grp

    def _update_text(self):
        if self._base_corrected_img is None:
            return
        # undistorted_img armazena a imagem corrigida SEM texto
        self.undistorted_img = self._base_corrected_img.copy()
        # Textos são desenhados apenas na visualização (overlay)
        self._draw_overlay()

    def _draw_texts_on(self, img_uint8: np.ndarray):
        """Desenha os textos configurados diretamente sobre um array uint8 (in-place).
        
        Este método é usado tanto para exibição quanto para salvar/enviar,
        garantindo que os textos fiquem sempre por cima, sem distorção.
        """
        def draw_cfg(cfg, is_left):
            if not cfg["text"]: return
            font = cv2.FONT_HERSHEY_DUPLEX
            scale = cfg["size"] / 12.0
            thick = max(1, int(scale * 1.5))
            text = cfg["text"]
            
            (tw, th), baseline = cv2.getTextSize(text, font, scale, thick)
            h, w = img_uint8.shape[:2]
            
            # Y position
            margin_y = int(h * 0.05)
            if cfg["pos"] == 0:
                y = margin_y + th
            elif cfg["pos"] == 1:
                y = (h + th) // 2
            else:
                y = h - margin_y - baseline
                
            # X position
            margin_x = int(w * 0.02)
            if is_left:
                x = margin_x
            else:
                x = w - margin_x - tw
                
            # Background
            pad = int(10 * scale)
            bg_color = cfg["bg"]
            cv2.rectangle(img_uint8, (max(0, x - pad), max(0, y - th - pad)), 
                          (min(w, x + tw + pad), min(h, y + baseline + pad)), bg_color, -1)
            
            # Text
            txt_color = cfg["color"]
            cv2.putText(img_uint8, text, (x, y), font, scale, txt_color, thick, cv2.LINE_AA)
            
        draw_cfg(self.txt_e_cfg, True)
        draw_cfg(self.txt_d_cfg, False)
