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
INSTALLER_DIR = ROOT / "instalador"
BUILD = ROOT / "build"
ICON = ROOT / "assets" / "icon.ico"

def get_version():
    """Extrai a versão do arquivo main.py."""
    main_py = ROOT / "main.py"
    if not main_py.exists():
        return "1.0.0"
    with open(main_py, "r", encoding="utf-8") as f:
        content = f.read()
        import re
        match = re.search(r'setApplicationVersion\("([^"]+)"\)', content)
        if match:
            return match.group(1)
        # Tenta no docstring se não achar no setApplicationVersion
        match = re.search(r'Versão ([\d\.]+)', content)
        if match:
            return match.group(1)
    return "1.0.0"

VERSION = get_version()

def build_exe():
    """Gera o executável com PyInstaller."""
    # Garantir que o diretório de saída do PyInstaller existe
    DIST.mkdir(exist_ok=True)
    
    cmd = [
        sys.executable, "-m", "PyInstaller",
        "--noconfirm",
        "--onefile",
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
        "PySide6.QtPrintSupport",
        "PySide6.QtMultimedia",
        "PySide6.QtMultimediaWidgets",
        "PySide6.QtSvg",
        "matplotlib.backends.backend_qtagg",
        "numpy",
        "scipy",
        "scipy.interpolate",
        "scipy.stats",
        "skimage",
        "cv2",
        "PIL",
        "psutil",
    ]
    for h in hidden:
        cmd.extend(["--hidden-import", h])

    cmd.append(str(ROOT / "main.py"))

    print("=" * 60)
    print(f"Gerando executável Fotogrametria v{VERSION} com PyInstaller...")
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
    # Garantir que o diretório do instalador existe
    INSTALLER_DIR.mkdir(exist_ok=True)
    
    iss_content = f"""; Script Inno Setup para Fotogrametria
[Setup]
AppName=Fotogrametria
AppVersion={VERSION}
AppPublisher=POLITEC-MT
SetupIconFile={ICON}
DefaultDirName={{autopf}}\\Fotogrametria
DefaultGroupName=Fotogrametria
OutputDir={INSTALLER_DIR}
OutputBaseFilename=Fotogrametria_Setup_{VERSION}
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Files]
Source: "{DIST / 'Fotogrametria.exe'}"; DestDir: "{{app}}"; Flags: ignoreversion

[Icons]
Name: "{{group}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"; WorkingDir: "{{app}}"
Name: "{{commondesktop}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"; WorkingDir: "{{app}}"

[Run]
Filename: "{{app}}\\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent
"""
    iss_path = ROOT / "fotogrametria_setup.iss"
    with open(iss_path, "w", encoding="utf-8") as f:
        f.write(iss_content)
    print(f"Script Inno Setup gerado: {iss_path}")
    print(f"Versão: {VERSION}")
    print(f"Destino do Instalador: {INSTALLER_DIR}")
    print("Abra o script no Inno Setup Compiler para gerar o instalador.")


if __name__ == "__main__":
    build_exe()
    generate_inno_script()
