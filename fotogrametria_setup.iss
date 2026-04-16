; Script Inno Setup para Fotogrametria
[Setup]
AppName=Fotogrametria
AppVersion=1.5.0
AppPublisher=POLITEC-MT
SetupIconFile=G:\Meu Drive\POLITEC\fotogrametria\assets\icon.ico
DefaultDirName={autopf}\Fotogrametria
DefaultGroupName=Fotogrametria
OutputDir=G:\Meu Drive\POLITEC\fotogrametria\instalador
OutputBaseFilename=Fotogrametria_Setup_1.5.0
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Files]
Source: "G:\Meu Drive\POLITEC\fotogrametria\dist\Fotogrametria.exe"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"; WorkingDir: "{app}"
Name: "{commondesktop}\Fotogrametria"; Filename: "{app}\Fotogrametria.exe"; WorkingDir: "{app}"

[Run]
Filename: "{app}\Fotogrametria.exe"; Description: "Executar Fotogrametria"; Flags: nowait postinstall skipifsilent
