; Script Inno Setup para Fotogrametria
[Setup]
AppName=Fotogrametria
AppVersion=1.0.0
AppPublisher=POLITEC-MT
SetupIconFile=G:\Meu Drive\POLITEC\fotogrametria\assets\icon.ico
DefaultDirName={autopf}\Fotogrametria
DefaultGroupName=Fotogrametria
OutputDir=G:\Meu Drive\POLITEC\fotogrametria\dist
OutputBaseFilename=Fotogrametria_Setup_1.0.0
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern

[Files]
Source: "G:\Meu Drive\POLITEC\fotogrametria\dist\Fotogrametria\*"; DestDir: "{app}"; Flags: recursesubdirs

[Icons]
Name: "{group}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"
Name: "{commondesktop}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"

[Run]
Filename: "{app}\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent
