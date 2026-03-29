; Inno Setup Script for Fotogrametria
; Generates a professional Windows installer (.exe)
;
; Prerequisites:
;   1. Install Inno Setup from https://jrsoftware.org/isinfo.php
;   2. Build the application with PyInstaller first:
;      python -m PyInstaller --noconfirm fotogrametria.spec
;   3. Open this file in Inno Setup Compiler and click "Compile"

[Setup]
AppId={{B6F3E2A1-4D8C-4F5A-9E7B-3C1D2E4F5A6B}
AppName=Fotogrametria
AppVersion=1.0.0
AppVerName=Fotogrametria 1.0.0
AppPublisher=POLITEC-MT
AppPublisherURL=https://www.politec.mt.gov.br
AppSupportURL=https://www.politec.mt.gov.br
DefaultDirName={autopf}\Fotogrametria
DefaultGroupName=Fotogrametria
AllowNoIcons=yes
OutputDir=instalador
OutputBaseFilename=Fotogrametria_Setup_1.0.0
SetupIconFile=
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=lowest
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
UninstallDisplayName=Fotogrametria

; Visual
WizardSmallImageFile=
WizardImageFile=

[Languages]
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 6.1; Check: not IsAdminInstallMode

[Files]
Source: "dist\Fotogrametria\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"
Name: "{group}\{cm:UninstallProgram,Fotogrametria}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{app}"
