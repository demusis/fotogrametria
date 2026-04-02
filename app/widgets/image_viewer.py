"""
Widget de visualização de imagem com suporte a:
- Zoom com scroll do mouse
- Pan (arrastar)
- Marcação de pontos por clique (sem overlay visual para evitar viés de confirmação)
- Cursor discreto de cruz fina durante marcação
- Lupa de precisão (pixel magnifier) para marcação exata
"""

from __future__ import annotations

from PySide6.QtWidgets import (
    QGraphicsView, QGraphicsScene, QGraphicsPixmapItem,
)
from PySide6.QtGui import (
    QPixmap, QImage, QPainter, QWheelEvent, QMouseEvent,
    QCursor, QPen, QColor, QBrush, QFont,
)
from PySide6.QtCore import Qt, Signal, QPointF, QRectF

import numpy as np


def _criar_cursor_cruz(tamanho: int = 21, espessura: int = 1, cor: str = "#FF4444") -> QCursor:
    """Cria um cursor de cruz discreta (fino, com gap central).

    O cursor é um '+' fino com um pequeno espaço no centro para
    permitir visão precisa do ponto de clique.
    """
    pixmap = QPixmap(tamanho, tamanho)
    pixmap.fill(Qt.GlobalColor.transparent)

    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

    pen = QPen(QColor(cor), espessura)
    painter.setPen(pen)

    centro = tamanho // 2
    gap = 3  # gap central em pixels

    # Linha horizontal (com gap)
    painter.drawLine(0, centro, centro - gap, centro)
    painter.drawLine(centro + gap, centro, tamanho - 1, centro)

    # Linha vertical (com gap)
    painter.drawLine(centro, 0, centro, centro - gap)
    painter.drawLine(centro, centro + gap, centro, tamanho - 1)

    painter.end()

    return QCursor(pixmap, centro, centro)


class ImageViewer(QGraphicsView):
    """Widget interativo para visualização de imagem e marcação de pontos.

    Não exibe overlay dos pontos marcados sobre a imagem para evitar
    viés de confirmação nas marcações subsequentes.

    Inclui lupa de precisão (pixel magnifier) que exibe uma região
    ampliada ao redor do cursor durante o modo de marcação.
    """

    # Signal emitido quando um ponto é marcado: (x_pixel, y_pixel)
    ponto_marcado = Signal(int, int)
    duplo_clique = Signal()

    # Configurações da lupa
    _MAG_SIZE = 140        # Tamanho da lupa em pixels do viewport
    _MAG_REGION = 20       # Raio da região capturada em pixels da imagem
    _MAG_OFFSET = 20       # Offset da lupa em relação ao cursor

    def __init__(self, parent=None):
        super().__init__(parent)
        self._scene = QGraphicsScene(self)
        self.setScene(self._scene)

        self._pixmap_item: QGraphicsPixmapItem | None = None
        self._img_array: np.ndarray | None = None

        self._is_dragging = False
        self._did_drag = False
        self._last_drag_pos = None

        # Lupa de precisão
        self._magnifier_enabled = True
        self._mouse_scene_pos: QPointF | None = None
        self._mouse_viewport_pos: QPointF | None = None

        # Cursor personalizado para marcação
        self._cursor_cruz = _criar_cursor_cruz(tamanho=21, espessura=1, cor="#FF4444")

        # Configurações visuais
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
        self.setTransformationAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        self._zoom_factor = 1.0
        self._marking_mode = True

        # Habilitar rastreamento do mouse (sem precisar segurar botão)
        self.setMouseTracking(True)
        self.viewport().setMouseTracking(True)
        # Iniciar em modo marcação
        self.setDragMode(QGraphicsView.DragMode.NoDrag)
        self.setCursor(self._cursor_cruz)

        self._apply_theme("dark")

    def _apply_theme(self, theme: str):
        """Aplica estilo com base no tema."""
        if theme == "dark":
            self.setStyleSheet("""
                QGraphicsView {
                    background-color: #1a1a2e;
                    border: 2px solid #16213e;
                    border-radius: 8px;
                }
            """)
        else:
            self.setStyleSheet("""
                QGraphicsView {
                    background-color: #f0f0f0;
                    border: 2px solid #cccccc;
                    border-radius: 8px;
                }
            """)

    def carregar_imagem(self, img_array: np.ndarray):
        """Carrega imagem a partir de array numpy (H, W, 3) float [0,1]."""
        self._img_array = img_array
        h, w = img_array.shape[:2]
        img_uint8 = (img_array * 255).astype(np.uint8).copy()
        qimage = QImage(img_uint8.data, w, h, 3 * w, QImage.Format.Format_RGB888)
        pixmap = QPixmap.fromImage(qimage.copy())  # copy() para manter os dados vivos

        self._scene.clear()
        self._pixmap_item = QGraphicsPixmapItem(pixmap)
        self._scene.addItem(self._pixmap_item)
        self._scene.setSceneRect(0, 0, w, h)
        self.fitInView(self._pixmap_item, Qt.AspectRatioMode.KeepAspectRatio)
        self._zoom_factor = 1.0

    def set_zoom(self, nivel: float):
        """Define o nível de zoom absoluto."""
        if self._pixmap_item is None:
            return
        factor = nivel / self._zoom_factor
        self.scale(factor, factor)
        self._zoom_factor = nivel

    def wheelEvent(self, event: QWheelEvent):
        """Zoom com scroll do mouse."""
        if self._pixmap_item is None:
            return
        delta = event.angleDelta().y()
        if delta > 0:
            factor = 1.15
        else:
            factor = 1 / 1.15
        self._zoom_factor *= factor
        self.scale(factor, factor)
        
        # O mouse pode não mover, mas a lupa precisa renderizar o novo fator
        if self._marking_mode and self._magnifier_enabled:
            # Recalcula a posição da cena sob o cursor
            self._mouse_scene_pos = self.mapToScene(self.mapFromGlobal(QCursor.pos()))
            self.viewport().update()

    def mousePressEvent(self, event: QMouseEvent):
        """Inicia o arrasto ou o preparo para clique."""
        if event.button() == Qt.MouseButton.LeftButton:
            self._is_dragging = True
            self._did_drag = False
            self._last_drag_pos = event.position()
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent):
        """Arrasta a imagem e atualiza a posição da lupa."""
        # Atualizar posição da lupa
        self._mouse_viewport_pos = event.position()
        self._mouse_scene_pos = self.mapToScene(event.position().toPoint())

        if self._is_dragging:
            delta = event.position() - self._last_drag_pos
            if delta.manhattanLength() > 3:
                self._did_drag = True

                h_bar = self.horizontalScrollBar()
                v_bar = self.verticalScrollBar()
                h_bar.setValue(int(h_bar.value() - delta.x()))
                v_bar.setValue(int(v_bar.value() - delta.y()))

                self._last_drag_pos = event.position()

        # Forçar repaint para atualizar a lupa
        if self._marking_mode and self._magnifier_enabled:
            self.viewport().update()

        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent):
        """Avalia se foi um clique limpo para marcar ponto, ou se encerrou o arrasto."""
        if event.button() == Qt.MouseButton.LeftButton and self._is_dragging:
            self._is_dragging = False

            if not self._did_drag and self._marking_mode:
                scene_pos = self.mapToScene(event.position().toPoint())
                if self._pixmap_item and self._pixmap_item.contains(scene_pos):
                    x = int(round(scene_pos.x()))
                    y = int(round(scene_pos.y()))
                    self.ponto_marcado.emit(x, y)
        super().mouseReleaseEvent(event)

    def mouseDoubleClickEvent(self, event: QMouseEvent):
        if event.button() == Qt.MouseButton.LeftButton and self._marking_mode:
            self.duplo_clique.emit()
            return
        super().mouseDoubleClickEvent(event)

    def leaveEvent(self, event):
        """Limpa a posição da lupa quando o mouse sai do widget."""
        self._mouse_scene_pos = None
        self._mouse_viewport_pos = None
        self.viewport().update()
        super().leaveEvent(event)

    def paintEvent(self, event):
        """Renderiza a cena e desenha a lupa de precisão por cima."""
        super().paintEvent(event)

        if (not self._marking_mode
                or not self._magnifier_enabled
                or self._img_array is None
                or self._mouse_scene_pos is None
                or self._mouse_viewport_pos is None
                or self._pixmap_item is None):
            return

        sx = self._mouse_scene_pos.x()
        sy = self._mouse_scene_pos.y()
        h, w = self._img_array.shape[:2]

        # Só desenhar se o ponteiro estiver sobre a imagem
        if sx < 0 or sy < 0 or sx >= w or sy >= h:
            return

        ix = int(sx)
        iy = int(sy)
        # Calcula o crop ajustado para acompanhar dinamicamente o zoom da Scene
        r = max(2, int(self._MAG_REGION / self._zoom_factor))

        # Extrair região da imagem (com clamping nas bordas)
        x0 = max(0, ix - r)
        y0 = max(0, iy - r)
        x1 = min(w, ix + r)
        y1 = min(h, iy + r)

        if x1 <= x0 or y1 <= y0:
            return

        crop = self._img_array[y0:y1, x0:x1]
        crop_uint8 = (crop * 255).astype(np.uint8).copy()
        ch, cw = crop_uint8.shape[:2]

        qimg = QImage(crop_uint8.data, cw, ch, 3 * cw, QImage.Format.Format_RGB888)
        mag_pixmap = QPixmap.fromImage(qimg.copy())
        mag_pixmap = mag_pixmap.scaled(
            self._MAG_SIZE, self._MAG_SIZE,
            Qt.AspectRatioMode.IgnoreAspectRatio,
            Qt.TransformationMode.FastTransformation,
        )

        # Posicionar a lupa — canto superior direito do cursor por padrão
        vp = self.viewport()
        mx = int(self._mouse_viewport_pos.x()) + self._MAG_OFFSET
        my = int(self._mouse_viewport_pos.y()) - self._MAG_SIZE - self._MAG_OFFSET

        # Ajustar se sair da viewport
        if mx + self._MAG_SIZE > vp.width():
            mx = int(self._mouse_viewport_pos.x()) - self._MAG_SIZE - self._MAG_OFFSET
        if my < 0:
            my = int(self._mouse_viewport_pos.y()) + self._MAG_OFFSET

        painter = QPainter(vp)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Fundo da lupa com borda
        rect = QRectF(mx, my, self._MAG_SIZE, self._MAG_SIZE)
        painter.setPen(QPen(QColor("#33BBEE"), 2))
        painter.setBrush(QBrush(QColor(0, 0, 0)))
        painter.drawRoundedRect(rect, 4, 4)

        # Imagem ampliada
        painter.drawPixmap(int(rect.x()), int(rect.y()), mag_pixmap)

        # Cruz central na lupa
        center_x = mx + self._MAG_SIZE // 2
        center_y = my + self._MAG_SIZE // 2
        gap = 4
        pen_cross = QPen(QColor("#FF4444"), 1)
        painter.setPen(pen_cross)
        painter.drawLine(center_x - 12, center_y, center_x - gap, center_y)
        painter.drawLine(center_x + gap, center_y, center_x + 12, center_y)
        painter.drawLine(center_x, center_y - 12, center_x, center_y - gap)
        painter.drawLine(center_x, center_y + gap, center_x, center_y + 12)

        # Coordenadas do pixel
        coord_text = f"({ix}, {iy})"
        painter.setPen(QPen(QColor("#FFFFFF")))
        painter.setFont(QFont("Segoe UI", 8))
        painter.drawText(
            QRectF(mx, my + self._MAG_SIZE + 2, self._MAG_SIZE, 16),
            Qt.AlignmentFlag.AlignCenter,
            coord_text,
        )

        painter.end()

    def set_marking_mode(self, enabled: bool):
        """Ativa/desativa o modo de marcação (vs. modo de navegação)."""
        self._marking_mode = enabled
        if enabled:
            self.setCursor(self._cursor_cruz)
            self.setDragMode(QGraphicsView.DragMode.NoDrag)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)
            self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)

    def set_magnifier_enabled(self, enabled: bool):
        """Ativa/desativa a lupa de precisão."""
        self._magnifier_enabled = enabled
        self.viewport().update()
