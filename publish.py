import os
import subprocess
import sys
import re
from pathlib import Path

ROOT = Path(__file__).parent
INIT_FILE = ROOT / "app" / "__init__.py"

def get_current_version():
    with open(INIT_FILE, "r", encoding="utf-8") as f:
        content = f.read()
        match = re.search(r'__version__ = "([^"]+)"', content)
        if match:
            return match.group(1)
    return "1.0.0"

def increment_version(version):
    parts = version.split(".")
    if len(parts) == 3:
        parts[2] = str(int(parts[2]) + 1)
    return ".".join(parts)

def update_version_file(new_version):
    with open(INIT_FILE, "r", encoding="utf-8") as f:
        content = f.read()
    
    new_content = re.sub(r'__version__ = "[^"]+"', f'__version__ = "{new_version}"', content)
    
    with open(INIT_FILE, "w", encoding="utf-8") as f:
        f.write(new_content)

def run_command(cmd):
    print(f"> {' '.join(cmd)}")
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print(f"ERRO ao executar: {' '.join(cmd)}")
        sys.exit(1)

def main():
    # 1. Obter e incrementar versão
    old_version = get_current_version()
    new_version = increment_version(old_version)
    print(f"Atualizando versão: {old_version} -> {new_version}")
    
    # 2. Atualizar arquivo __init__.py
    update_version_file(new_version)
    
    # 3. Git Commands
    commit_msg = input(f"Digite a mensagem do commit (padrão: 'Update to v{new_version}'): ")
    if not commit_msg:
        commit_msg = f"Update to v{new_version}"
    
    run_command(["git", "add", "."])
    run_command(["git", "commit", "-m", f'"{commit_msg}"'])
    run_command(["git", "push", "origin", "main"])
    
    print("\n" + "="*40)
    print(f"Sucesso! Versão {new_version} enviada ao GitHub.")
    print("="*40)

if __name__ == "__main__":
    main()
