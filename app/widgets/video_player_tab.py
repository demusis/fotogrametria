import os
import numpy as np

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QSlider, QLabel,
    QFileDialog, QMessageBox, QGroupBox, QLineEdit
)
from PySide6.QtCore import Qt, QTimer, Signal

from .image_viewer import ImageViewer
from app.core.ffmpeg_utils import FFmpegPlayer

class VideoPlayerTab(QWidget):
    """Aba de extração e navegação de vídeo forense via FFmpeg/FFprobe."""
    
    enviar_par_imagens = Signal(np.ndarray, np.ndarray, float, float)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.player = None
        self.current_time_sec = 0.0
        self.is_playing = False
        
        # Armazenar os dois quadros selecionados
        self.img_esq = None
        self.img_dir = None
        self.time_esq = 0.0
        self.time_dir = 0.0
        
        self._init_ui()
        
        self.timer = QTimer(self)
        self.timer.timeout.connect(self._next_frame)
        self.timer.setInterval(33) # ~30 fps default

    def _init_ui(self):
        main_layout = QHBoxLayout(self)
        
        controls_panel = QWidget()
        controls_layout = QVBoxLayout(controls_panel)
        controls_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        
        self.btn_load = QPushButton("Carregar Vídeo (FFmpeg)")
        self.btn_load.clicked.connect(self._load_video)
        controls_layout.addWidget(self.btn_load)

        self.txt_veiculo = QLineEdit()
        self.txt_veiculo.setPlaceholderText("Identificação do Veículo (ex: Placa, Modelo)")
        controls_layout.addWidget(self.txt_veiculo)
        
        self.lbl_info = QLabel("Nenhum vídeo carregado.")
        self.lbl_info.setWordWrap(True)
        controls_layout.addWidget(self.lbl_info)
        
        # Navegação
        nav_group = QGroupBox("Navegação")
        nav_layout = QVBoxLayout(nav_group)
        
        hbox_nav = QHBoxLayout()
        self.btn_prev = QPushButton("⏮️ -1 Qdo")
        self.btn_prev.clicked.connect(self._step_back)
        self.btn_play = QPushButton("▶️ Play")
        self.btn_play.clicked.connect(self._toggle_play)
        self.btn_next = QPushButton("⏭️ +1 Qdo")
        self.btn_next.clicked.connect(self._step_forward)
        
        hbox_nav.addWidget(self.btn_prev)
        hbox_nav.addWidget(self.btn_play)
        hbox_nav.addWidget(self.btn_next)
        nav_layout.addLayout(hbox_nav)
        
        self.slider = QSlider(Qt.Orientation.Horizontal)
        self.slider.setRange(0, 1000)
        self.slider.sliderReleased.connect(self._seek_slider)
        nav_layout.addWidget(self.slider)
        
        self.lbl_time = QLabel("00:00 / 00:00")
        nav_layout.addWidget(self.lbl_time)
        
        controls_layout.addWidget(nav_group)
        
        # Extração
        ext_group = QGroupBox("Extração")
        ext_layout = QVBoxLayout(ext_group)
        
        self.btn_mark_l = QPushButton("Salvar p/ Esq.")
        self.btn_mark_l.clicked.connect(self._mark_left)
        self.lbl_mark_l = QLabel("Esq: N/A")
        
        self.btn_mark_r = QPushButton("Salvar p/ Dir.")
        self.btn_mark_r.clicked.connect(self._mark_right)
        self.lbl_mark_r = QLabel("Dir: N/A")
        
        ext_layout.addWidget(self.btn_mark_l)
        ext_layout.addWidget(self.lbl_mark_l)
        ext_layout.addWidget(self.btn_mark_r)
        ext_layout.addWidget(self.lbl_mark_r)
        
        self.btn_hash = QPushButton("Hash e informações (pasta)")
        self.btn_hash.clicked.connect(self._generate_report)
        ext_layout.addWidget(self.btn_hash)
        
        controls_layout.addWidget(ext_group)
        
        self.btn_send = QPushButton("⬅️ Enviar p/ Mesclar Imagens")
        self.btn_send.clicked.connect(self._send_images)
        self.btn_send.setStyleSheet("background-color: #0077BB; color: white;")
        controls_layout.addWidget(self.btn_send)
        
        controls_panel.setMinimumWidth(260)
        controls_panel.setMaximumWidth(320)
        self.controls_panel = controls_panel
        
        self.viewer = ImageViewer()
        self.viewer.set_marking_mode(False)
        self.current_frame_img = None
        
        main_layout.addWidget(controls_panel)
        main_layout.addWidget(self.viewer, stretch=1)

        # Atalhos de Teclado restritos a esta aba
        from PySide6.QtGui import QShortcut, QKeySequence
        
        shortcut_left = QShortcut(QKeySequence(Qt.Key.Key_Left), self)
        shortcut_left.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
        shortcut_left.activated.connect(self._step_back)
        
        shortcut_right = QShortcut(QKeySequence(Qt.Key.Key_Right), self)
        shortcut_right.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
        shortcut_right.activated.connect(self._step_forward)
        
        shortcut_space = QShortcut(QKeySequence(Qt.Key.Key_Space), self)
        shortcut_space.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
        shortcut_space.activated.connect(self._toggle_play)

    def set_theme(self, theme: str):
        if theme == "dark":
            style = """
                QWidget { background-color: #0e1117; color: #e0e0e0; }
                QLabel { color: #e0e0e0; background-color: transparent; }
                QGroupBox { color: #33BBEE; font-weight: bold; border: 1px solid #30363d; border-radius: 4px; margin-top: 10px; padding-top: 15px; }
                QGroupBox::title { subcontrol-origin: margin; left: 8px; padding: 0 3px 0 3px; }
                QPushButton { background-color: #1a1a2e; color: #e0e0e0; border: 1px solid #30363d; padding: 5px; border-radius: 3px; }
                QPushButton:hover { background-color: #2a2a4e; }
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
                QSlider::groove:horizontal { border: 1px solid #cccccc; height: 4px; background: #e0e0e0; margin: 2px 0; border-radius: 2px; }
                QSlider::handle:horizontal { background: #0077BB; border: 1px solid #0077BB; width: 14px; height: 14px; margin: -5px 0; border-radius: 7px; }
            """
        self.controls_panel.setStyleSheet(style)
        
        # Correção direta forçada para as labels
        lbl_clr = "color: #e0e0e0;" if theme == "dark" else "color: #333333;"
        for lbl in [self.lbl_info, self.lbl_time, self.lbl_mark_l, self.lbl_mark_r]:
            lbl.setStyleSheet(lbl_clr)
        self.txt_veiculo.setStyleSheet("background-color: #1a1a2e; color: #e0e0e0; border: 1px solid #30363d;" if theme == "dark" else "")
            
        self.viewer._apply_theme(theme)

    def set_filter_callback(self, callback):
        self._filter_callback = callback

    def _display_frame(self, frame):
        if frame is None:
            return
        self._raw_frame = frame.copy()
        if hasattr(self, '_filter_callback') and self._filter_callback:
            frame = self._filter_callback(frame)
        self.current_frame_img = frame
        self.viewer.carregar_imagem(frame)

    def refresh_frame(self):
        """Re-exibe o quadro atual reaplicando os filtros ativos."""
        if hasattr(self, '_raw_frame') and self._raw_frame is not None:
            self._display_frame(self._raw_frame)

    def _load_video(self):
        path, _ = QFileDialog.getOpenFileName(self, "Selecionar Vídeo", "", "Videos (*.mp4 *.avi *.mkv *.mov)")
        if not path:
            return
            
        try:
            self.player = FFmpegPlayer(path)
            self.lbl_info.setText(f"Res: {self.player.width}x{self.player.height}\nFPS: {self.player.fps:.2f}\nDuração: {self.player.duration:.2f}s")
            
            # Reset
            self.current_time_sec = 0.0
            self.timer.setInterval(int(1000 / self.player.fps))
            self.slider.setRange(0, self.player.total_frames)
            
            self._seek_to(0.0)
            
        except Exception as e:
            QMessageBox.critical(self, "Erro", f"Falha ao carregar FFprobe/FFmpeg:\n{e}")

    def _seek_to(self, target_sec):
        if not self.player: return
        self.current_time_sec = max(0, min(target_sec, self.player.duration))
        self.player.start_stream(self.current_time_sec)
        self._next_frame()

    def _seek_slider(self):
        if not self.player: return
        frame_idx = self.slider.value()
        target_sec = frame_idx / self.player.fps
        self._seek_to(target_sec)

    def _next_frame(self):
        if not self.player: return
        arr = self.player.read_next_frame()
        if arr is not None:
            self._display_frame(arr)
            
            # Update slider sem disparar o signal
            current_frame_idx = int(self.current_time_sec * self.player.fps)
            self.slider.blockSignals(True)
            self.slider.setValue(current_frame_idx)
            self.slider.blockSignals(False)
            
            # Update label with ms
            cur_t = self.current_time_sec
            dur_t = self.player.duration
            m_c, s_c = divmod(cur_t, 60)
            m_d, s_d = divmod(dur_t, 60)
            self.lbl_time.setText(f"{int(m_c):02d}:{int(s_c):02d}.{int((s_c-int(s_c))*1000):03d} / {int(m_d):02d}:{int(s_d):02d}.{int((s_d-int(s_d))*1000):03d} (F: {current_frame_idx})")
            
            # Avanca o tempo interno
            self.current_time_sec += 1.0 / self.player.fps
        else:
            self.is_playing = False
            self.btn_play.setText("▶️ Play")
            self.timer.stop()

    def _toggle_play(self):
        if not self.player: return
        self.is_playing = not self.is_playing
        if self.is_playing:
            self.btn_play.setText("⏸️ Pause")
            self.timer.start()
        else:
            self.btn_play.setText("▶️ Play")
            self.timer.stop()

    def _step_forward(self):
        if self.is_playing: self._toggle_play()
        if not self.player: return
        self._next_frame()
        
    def _step_back(self):
        if self.is_playing: self._toggle_play()
        if not self.player: return
        # A volta rigorosa de -1 qdo é complexa no ffmpeg puro (fluxos iterativos), 
        # mas podemos buscar exatamente no tempo -2 frames:
        if self.current_time_sec >= 2.0 / self.player.fps:
            self._seek_to(self.current_time_sec - 2.0 / self.player.fps)
        else:
            self._seek_to(0.0)

    def _mark_left(self):
        if hasattr(self, '_raw_frame') and self._raw_frame is not None:
            self.img_esq = self._raw_frame.copy()
            frame_idx = int(self.current_time_sec * self.player.fps) - 1
            if frame_idx < 0: frame_idx = 0
            self.time_esq = self.player.get_time_for_frame(frame_idx)
            self.lbl_mark_l.setText(f"Esq: Frm {frame_idx} (T={self.time_esq:.3f}s)")

    def _mark_right(self):
        if hasattr(self, '_raw_frame') and self._raw_frame is not None:
            self.img_dir = self._raw_frame.copy()
            frame_idx = int(self.current_time_sec * self.player.fps) - 1
            if frame_idx < 0: frame_idx = 0
            self.time_dir = self.player.get_time_for_frame(frame_idx)
            self.lbl_mark_r.setText(f"Dir: Frm {frame_idx} (T={self.time_dir:.3f}s)")
            
    def _generate_report(self):
        folder_path = QFileDialog.getExistingDirectory(self, "Selecionar Pasta para Análise")
        if not folder_path:
            return

        out_path, _ = QFileDialog.getSaveFileName(self, "Salvar Relatório Forense", "relatorio_midia_pasta.pdf", "PDF (*.pdf)")
        if not out_path:
            return

        import hashlib
        import subprocess
        import os
        from PySide6.QtGui import QTextDocument
        from PySide6.QtPrintSupport import QPrinter
        from PySide6.QtWidgets import QApplication, QProgressDialog

        self.btn_hash.setText("Gerando...")
        self.btn_hash.setEnabled(False)
        
        # Encontrar todos os arquivos primeiro para a barra de progresso
        todos_arquivos = []
        for root, dirs, files in os.walk(folder_path):
            for f in files:
                todos_arquivos.append(os.path.join(root, f))
                
        total = len(todos_arquivos)
        if total == 0:
            QMessageBox.information(self, "Aviso", "Nenhum arquivo encontrado na pasta selecionada.")
            self.btn_hash.setEnabled(True)
            self.btn_hash.setText("Hash e informações (pasta)")
            return

        progress = QProgressDialog("Analisando arquivos...", "Cancelar", 0, total, self)
        progress.setWindowTitle("Relatório de Pasta")
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.show()

        try:
            html_parts = [
                "<html><head><style>",
                "body { font-family: Arial, sans-serif; font-size: 12pt; }",
                "h2 { font-size: 16pt; }",
                "p { font-size: 12pt; margin: 4pt 0; }",
                ".hash-text { font-family: 'Courier New', monospace; font-size: 8pt; word-wrap: break-word; }",
                ".metadata-block { font-family: 'Courier New', monospace; font-size: 8pt; background-color: #f8f9fa; border: 1px solid #ccc; padding: 6pt; white-space: pre-wrap; }",
                "</style></head><body>",
                "<h2>Relatório de Análise de Mídia Digital (Pasta)</h2>",
                f"<p><b>Pasta Analisada:</b> {folder_path}</p>",
                f"<p><b>Total de Arquivos:</b> {total}</p>",
                "<hr>"
            ]
            
            for i, file_path in enumerate(todos_arquivos):
                if progress.wasCanceled():
                    break
                    
                progress.setValue(i)
                progress.setLabelText(f"Analisando arquivo {i+1} de {total}...\n{os.path.basename(file_path)}")
                QApplication.processEvents()

                # Calcular hash
                sha512 = hashlib.sha512()
                try:
                    with open(file_path, "rb") as f:
                        for chunk in iter(lambda: f.read(4096 * 1024), b""):
                            sha512.update(chunk)
                    hash_str = sha512.hexdigest()
                except Exception as e:
                    hash_str = f"Erro de leitura: {e}"
                
                # Tentar mediainfo ou ffprobe
                media_text = "N/A"
                try:
                    mi_res = subprocess.run(["mediainfo", file_path], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=5)
                    if mi_res.returncode == 0 and mi_res.stdout.strip():
                        media_text = mi_res.stdout.strip()
                    else:
                        raise Exception()
                except:
                    try:
                        mi_res = subprocess.run([
                            "ffprobe", "-v", "quiet", "-show_format", "-show_streams", file_path
                        ], capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=5)
                        if mi_res.stdout.strip():
                            media_text = mi_res.stdout.strip()
                    except:
                        pass
                        
                html_parts.append(f"<div style='margin-bottom: 12pt; page-break-inside: avoid;'>")
                html_parts.append(f"<p><b>Arquivo:</b> {file_path}</p>")
                html_parts.append("<p><b>Hash SHA-512:</b></p>")
                html_parts.append(f"<p class='hash-text'>{hash_str}</p>")
                html_parts.append("<p><b>Metadados:</b></p>")
                html_parts.append(f"<pre class='metadata-block'>{media_text}</pre>")
                html_parts.append("</div><hr>")

            html_parts.append("</body></html>")

            progress.setLabelText("Gerando PDF...")
            progress.setValue(total)
            QApplication.processEvents()

            if not progress.wasCanceled():
                from PySide6.QtCore import QMarginsF
                from PySide6.QtGui import QPageSize, QPageLayout, QFont

                doc = QTextDocument()
                default_font = QFont("Arial", 12)
                doc.setDefaultFont(default_font)
                doc.setHtml("".join(html_parts))
                
                printer = QPrinter(QPrinter.PrinterMode.HighResolution)
                printer.setOutputFormat(QPrinter.OutputFormat.PdfFormat)
                printer.setOutputFileName(out_path)
                page_layout = QPageLayout(
                    QPageSize(QPageSize.PageSizeId.A4),
                    QPageLayout.Orientation.Portrait,
                    QMarginsF(15, 15, 15, 15)
                )
                printer.setPageLayout(page_layout)
                doc.setPageSize(printer.pageRect(QPrinter.Unit.Point).size())
                doc.print_(printer)

                QMessageBox.information(self, "Sucesso", "Relatório PDF da pasta gerado com sucesso!")
        except Exception as e:
            QMessageBox.critical(self, "Erro", f"Falha ao gerar o relatório:\n{e}")
        finally:
            progress.close()
            self.btn_hash.setEnabled(True)
            self.btn_hash.setText("Hash e informações (pasta)")
            
    def _send_images(self):
        if self.img_esq is None or self.img_dir is None:
            QMessageBox.warning(self, "Aviso", "Extraia e marque as duas imagens (Esq. e Dir.) antes de enviar!")
            return
            
        final_esq = self.img_esq.copy()
        final_dir = self.img_dir.copy()
        
        if hasattr(self, '_filter_callback') and self._filter_callback:
            final_esq = self._filter_callback(final_esq)
            final_dir = self._filter_callback(final_dir)
            
        self.enviar_par_imagens.emit(final_esq, final_dir, float(self.time_esq), float(self.time_dir))
