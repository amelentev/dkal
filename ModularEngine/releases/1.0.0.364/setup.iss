[Setup]
AppName=DKAL
AppVersion=1.0.0.364
DefaultDirName={pf}\DKAL
DefaultGroupName=DKAL
UninstallDisplayIcon={app}\Binaries\GuiMain\DkalGui.exe
Compression=lzma2
SolidCompression=yes
ChangesEnvironment=true

[Tasks]
Name: modifypath; Description: Add DKAL directory to your PATH system variable

[Files]
Source: "Binaries\*.dll"; DestDir: "{app}\Binaries\GuiMain"
Source: "Binaries\GuiMain\*"; DestDir: "{app}\Binaries\GuiMain"
Source: "Binaries\*.dll"; DestDir: "{app}\Binaries\Main"
Source: "Binaries\Main\*"; DestDir: "{app}\Binaries\Main"
Source: "Binaries\*.dll"; DestDir: "{app}\Binaries\MultiMain"
Source: "Binaries\MultiMain\*"; DestDir: "{app}\Binaries\MultiMain"
Source: "..\..\LICENSE"; DestDir: "{app}"
Source: "..\..\samples\*"; DestDir: "{app}\Samples"; Flags: recursesubdirs
Source: "Misc\Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\Samples"; Filename: "{app}\Samples"

[Code]
const 
    ModPathName = 'modifypath'; 
    ModPathType = 'user'; 

function ModPathDir(): TArrayOfString; 
begin 
    setArrayLength(Result, 3) 
    Result[0] := ExpandConstant('{app}\Binaries\GuiMain'); 
    Result[1] := ExpandConstant('{app}\Binaries\Main'); 
    Result[2] := ExpandConstant('{app}\Binaries\MultiMain'); 
end; 
#include "modpath.iss"
