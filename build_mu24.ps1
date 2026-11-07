$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$cad = Join-Path $env:ProgramFiles 'Autodesk\AutoCAD 2021'
if (!(Test-Path (Join-Path $cad 'acmgd.dll'))) {
  Write-Host 'Khong tim thay AutoCAD 2021 trong Program Files.' -ForegroundColor Red
  $cad = Read-Host 'Nhap thu muc cai AutoCAD 2021 (chua acmgd.dll)'
}
if (!(Test-Path (Join-Path $cad 'acmgd.dll'))) { throw 'Khong tim thay acmgd.dll' }
$csc = Join-Path $env:windir 'Microsoft.NET\Framework64\v4.0.30319\csc.exe'
if (!(Test-Path $csc)) { throw 'Khong tim thay csc.exe (.NET Framework). Cai .NET Framework 4.8 Developer Pack.' }
$out = Join-Path $here 'MU24Direct.dll'
$src = Join-Path $here 'MU24Direct.cs'
Write-Host "Bien dich DLL cho AutoCAD 2021: $cad" -ForegroundColor Cyan
& $csc /nologo /target:library /platform:x64 /optimize+ /utf8output "/out:$out" `
  "/reference:$(Join-Path $cad 'acmgd.dll')" `
  "/reference:$(Join-Path $cad 'acdbmgd.dll')" `
  "/reference:$(Join-Path $cad 'accoremgd.dll')" "$src"
if ($LASTEXITCODE -ne 0 -or !(Test-Path $out)) { throw 'Bien dich that bai. Gui thong bao do de sua dung loi.' }
Write-Host "DA TAO: $out" -ForegroundColor Green
Write-Host 'AutoCAD: NETLOAD -> chon MU24Direct.dll; APPLOAD -> chon MU_CADPDF_V24.lsp; go MUIN24'
