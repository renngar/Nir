cd src\Nir
mkdir ..\..\Published
dotnet publish -c Release --self-contained -r win-x86 -o ..\..\Published\Nir
..\..\Tools\rcedit-x86.exe ..\..\Published\Nir\Nir.exe --set-icon Assets\Icons\Nir.ico
cd ..\..\Plugins\ModChecker
dotnet publish -c Release -r win-x86 -o ..\..\Published\Nir\Plugins\ModChecker
cd ..\..
