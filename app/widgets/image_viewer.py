"""
Widget de visualização de imagem com suporte a:
- Zoom com scroll do mouse
- Pan (arrastar)
- Marcação de pontos por clique (sem overlay visual para evitar viés de confirmação)
- Cursor discreto de cruz fina durante marcação
"""

from __future__ import annotations

from PySide6.QtWidgets import (
    QGraphicsView, QGraphicsScene, QGraphicsPixmapItem,
)
from PySide6.QtGui import (
    QPixmap, QImage, QPainter, QWheelEvent, QMouseEvent,
    QCursor, QPen, QColor,
)
from PySide6.QtCore import Qt, Signal

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
    """

    # Signal emitido quando um ponto é marcado: (x_pixel, y_pixel)
    ponto_marcado = Signal(int, int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._scene = QGraphicsScene(self)
        self.setScene(self._scene)

        self._pixmap_item: QGraphicsPixmapItem | None = None

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

    def mousePressEvent(self, event: QMouseEvent):
        """Captura cliques para marcação de pontos."""
        if event.button() == Qt.MouseButton.LeftButton and self._marking_mode:
            scene_pos = self.mapToScene(event.position().toPoint())
            if self._pixmap_item and self._pixmap_item.contains(scene_pos):
                x = int(round(scene_pos.x()))
                y = int(round(scene_pos.y()))
                self.ponto_marcado.emit(x, y)
                return  # Não propagar para o drag
        super().mousePressEvent(event)

    def set_marking_mode(self, enabled: bool):
        """Ativa/desativa o modo de marcação (vs. modo de navegação)."""
        self._marking_mode = enabled
        if enabled:
            self.setCursor(self._cursor_cruz)
            self.setDragMode(QGraphicsView.DragMode.NoDrag)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)
            self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
