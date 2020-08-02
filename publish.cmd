cd Nir
mkdir ..\Published
dotnet publish -c Release --self-contained -r win-x86 -o ..\Published\Nir
cd ..
