import subprocess
import sys
import os
import shutil
import tempfile
from pathlib import Path

ROOT = Path(__file__).parent
ICON = ROOT / "assets" / "icon.ico"

def get_version():
    """Extrai a versão do arquivo main.py."""
    main_py = ROOT / "main.py"
    if not main_py.exists():
        return "1.5.0"
    with open(main_py, "r", encoding="utf-8") as f:
        content = f.read()
        import re
        match = re.search(r'setApplicationVersion\("([^"]+)"\)', content)
        if match:
            return match.group(1)
    return "1.5.0"

VERSION = get_version()

def build_all():
    # 1. Criar diretório temporário para o build (evita locks do Google Drive)
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        print(f"Copiando arquivos para diretório temporário: {temp_path}")
        
        # Copiar apenas os diretórios necessários
        for item in ["app", "assets", "main.py", "requirements.txt"]:
            src = ROOT / item
            dst = temp_path / item
            if src.is_dir():
                shutil.copytree(src, dst)
            else:
                shutil.copy2(src, dst)
        
        # 2. PyInstaller
        print("\n" + "="*60)
        print(f"Executando PyInstaller para Fotogrametria v{VERSION}...")
        print("="*60)
        
        cmd_pyi = [
            sys.executable, "-m", "PyInstaller",
            "--noconfirm",
            "--onefile",
            "--windowed",
            "--name", "Fotogrametria",
            "--add-data", "app;app",
            "--add-data", "assets;assets",
            "--icon", "assets/icon.ico" if (temp_path / "assets" / "icon.ico").exists() else "NONE",
            # Hidden imports para garantir que bibliotecas científicas sejam incluídas
            "--hidden-import", "PySide6.QtPrintSupport",
            "--hidden-import", "matplotlib.backends.backend_qtagg",
            "--hidden-import", "scipy.interpolate",
            "--hidden-import", "scipy.stats",
            "--hidden-import", "skimage.restoration",
            "--hidden-import", "cv2",
            "main.py"
        ]
        
        result = subprocess.run(cmd_pyi, cwd=temp_path)
        if result.returncode != 0:
            print("ERRO: PyInstaller falhou.")
            sys.exit(1)
            
        # 3. Copiar o executável de volta para a pasta dist do projeto
        dist_dir = ROOT / "dist"
        dist_dir.mkdir(exist_ok=True)
        shutil.copy2(temp_path / "dist" / "Fotogrametria.exe", dist_dir / "Fotogrametria.exe")
        print(f"\nExecutável gerado com sucesso em: {dist_dir}")

        # 4. Gerar e Executar Inno Setup
        generate_and_run_inno(dist_dir)

def generate_and_run_inno(dist_dir):
    installer_dir = ROOT / "instalador"
    installer_dir.mkdir(exist_ok=True)
    iss_path = ROOT / "fotogrametria_setup.iss"
    
    # Notar que usamos caminhos relativos ou caminhos absolutos baseados no ROOT
    iss_content = f"""; Script Inno Setup Gerado Automaticamente
[Setup]
AppName=Fotogrametria
AppVersion={VERSION}
AppPublisher=POLITEC-MT
DefaultDirName={{autopf}}\\Fotogrametria
DefaultGroupName=Fotogrametria
OutputDir={installer_dir}
OutputBaseFilename=Fotogrametria_Setup_{VERSION}
SetupIconFile={ROOT / "assets" / "icon.ico"}
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Files]
Source: "{dist_dir / "Fotogrametria.exe"}"; DestDir: "{{app}}"; Flags: ignoreversion

[Icons]
Name: "{{group}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"; WorkingDir: "{{app}}"
Name: "{{commondesktop}}\\Fotogrametria"; Filename: "{{app}}\\Fotogrametria.exe"; WorkingDir: "{{app}}"

[Run]
Filename: "{{app}}\\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent
"""
    with open(iss_path, "w", encoding="utf-8") as f:
        f.write(iss_content)
    print(f"Script Inno Setup atualizado: {iss_path}")

    # Tentar localizar ISCC.exe
    iscc = shutil.which("iscc")
    if not iscc:
        # Caminhos comuns no Windows
        common_paths = [
            r"C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
            r"C:\Program Files\Inno Setup 6\ISCC.exe",
        ]
        for p in common_paths:
            if os.path.exists(p):
                iscc = p
                break
    
    if iscc:
        print(f"Compilando instalador com {iscc}...")
        result = subprocess.run([iscc, str(iss_path)])
        if result.returncode == 0:
            print(f"\nINSTALADOR GERADO COM SUCESSO EM: {installer_dir}")
        else:
            print("\nERRO ao compilar o instalador com Inno Setup.")
    else:
        print("\nAVISO: ISCC.exe não encontrado. Por favor, compile o arquivo .iss manualmente no Inno Setup.")

if __name__ == "__main__":
    build_all()

