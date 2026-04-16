"""
Entry-point do aplicativo Fotogrametria.
Estimativa de velocidade veicular por fotogrametria.

Autor original (R/Shiny): Carlo Ralph De Musis
Portagem para Python: Versão 1.5.0
"""

import sys
import os

# Corrigir DPI em telas de alta resolução no Windows
os.environ["QT_ENABLE_HIGHDPI_SCALING"] = "1"

from PySide6.QtWidgets import QApplication
from PySide6.QtGui import QFont, QIcon
from PySide6.QtCore import Qt

from app.main_window import MainWindow

def get_resource_path(relative_path):
    """Resolve caminho de recursos, compatível com PyInstaller."""
    if hasattr(sys, '_MEIPASS'):
        return os.path.join(sys._MEIPASS, relative_path)
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), relative_path)

def main():
    app = QApplication(sys.argv)
    
    icon_path = get_resource_path(os.path.join("assets", "icon.ico"))
    if os.path.exists(icon_path):
        app.setWindowIcon(QIcon(icon_path))

    # Configurações globais
    app.setApplicationName("Fotogrametria")
    app.setOrganizationName("POLITEC-MT")
    app.setApplicationVersion("1.5.0")

    # Fonte padrão
    font = QFont("Segoe UI", 10)
    app.setFont(font)

    # Estilos globais mínimos (temas são aplicados pelo MainWindow)
    app.setStyleSheet("""
        QToolTip {
            padding: 6px;
            border-radius: 4px;
            font-size: 11px;
        }
        QScrollBar:vertical {
            width: 10px;
            border-radius: 5px;
        }
        QScrollBar::handle:vertical {
            min-height: 30px;
            border-radius: 5px;
        }
        QScrollBar:horizontal {
            height: 10px;
            border-radius: 5px;
        }
        QScrollBar::handle:horizontal {
            min-width: 30px;
            border-radius: 5px;
        }
        QScrollBar::add-line, QScrollBar::sub-line {
            height: 0px;
            width: 0px;
        }
    """)

    window = MainWindow()
    window.show()

    sys.exit(app.exec())


if __name__ == "__main__":
    main()
