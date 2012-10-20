rm -r Binaries
mkdir Binaries
cd Binaries
cp ../../../src/Main/bin/Release/*.dll .
cp ../../../src/MultiMain/bin/Release/*.dll .
cp ../../../src/MultiMain/bin/Release/language.js .
cp ../../../src/GuiMain/bin/Release/*.dll .

cp "C:\Program Files (x86)\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.dll"  .
cp "C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll" .

mkdir Main
cp ../../../src/Main/bin/Release/NLog.config Main/
cp ../../../src/Main/bin/Release/Dkal.exe* Main/
mkdir MultiMain
cp ../../../src/MultiMain/bin/Release/NLog.config MultiMain/
cp ../../../src/MultiMain/bin/Release/DkalMulti.exe* MultiMain/
mkdir GuiMain
cp ../../../src/GuiMain/bin/x86/Release/NLog.config GuiMain/
cp ../../../src/GuiMain/bin/x86/Release/DkalGui.exe* GuiMain/
