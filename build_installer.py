"""
Script para gerar o executável e instalador Windows.

Uso:
    python build_installer.py

Pré-requisitos:
    pip install pyinstaller

Opcionalmente, para gerar instalador .exe profissional:
    - Instalar Inno Setup (https://jrsoftware.org/isinfo.php)
"""

import subprocess
import sys
import os
from pathlib import Path

ROOT = Path(__file__).parent
DIST = ROOT / "dist"
BUILD = ROOT / "build"
ICON = ROOT / "assets" / "icon.ico"


def build_exe():
    """Gera o executável com PyInstaller."""
    cmd = [
        sys.executable, "-m", "PyInstaller",
        "--noconfirm",
        "--onedir",
        "--windowed",
        "--name", "Fotogrametria",
        "--add-data", f"{ROOT / 'app'};app",
        "--add-data", f"{ROOT / 'assets'};assets",
    ]

    if ICON.exists():
        cmd.extend(["--icon", str(ICON)])

    # Hidden imports necessários
    hidden = [
        "PySide6.QtWidgets",
        "PySide6.QtGui",
        "PySide6.QtCore",
        "matplotlib.backends.backend_qtagg",
        "numpy",
        "scipy",
        "scipy.interpolate",
        "cv2",
        "PIL",
        "skimage",
    ]
    for h in hidden:
        cmd.extend(["--hidden-import", h])

    cmd.append(str(ROOT / "main.py"))

    print("=" * 60)
    print("Gerando executável com PyInstaller...")
    print("=" * 60)
    print(" ".join(cmd))
    print()

    result = subprocess.run(cmd, cwd=str(ROOT))

    if result.returncode == 0:
        print()
        print("=" * 60)
        print("Build concluído com sucesso!")
        print(f"Executável em: {DIST / 'Fotogrametria'}")
        print("=" * 60)
    else:
        print("ERRO: Build falhou!")
        sys.exit(1)


def generate_inno_script():
    """Gera o script .iss para o Inno Setup."""
    iss_content = f"""; Script Inno Setup para Fotogrametria
[Setup]
AppName=Fotogrametria
AppVersion=1.0.0
AppPublisher=POLITEC-MT
SetupIconFile={ICON}
DefaultDirName={{autopf}}\\Fotogrametria
DefaultGroupName=Fotogrametria
OutputDir={DIST}
OutputBaseFilename=Fotogrametria_Setup_1.0.0
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern

[Files]
Source: "{DIST / 'Fotogrametria' / '*'}"; DestDir: "{{app}}"; Flags: recursesubdirs

[Icons]
Name: "{{group}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"
Name: "{{commondesktop}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"

[Run]
Filename: "{{app}}\\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent
"""
    iss_path = ROOT / "fotogrametria_setup.iss"
    with open(iss_path, "w", encoding="utf-8") as f:
        f.write(iss_content)
    print(f"Script Inno Setup gerado: {iss_path}")
    print("Abra o script no Inno Setup Compiler para gerar o instalador.")


if __name__ == "__main__":
    build_exe()
    generate_inno_script()
