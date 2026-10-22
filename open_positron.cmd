@echo off

for /f "delims=" %%i in (
    'where /r "%LOCALAPPDATA%\Programs\Positron" Positron.exe 2^>nul'
) do (
    start "" "%%i" "%~dp0"

    powershell -NoProfile -WindowStyle Hidden -Command ^
        "$ws = New-Object -ComObject WScript.Shell; " ^
        "Start-Sleep -Milliseconds 800; " ^
        "$ws.AppActivate('Positron') | Out-Null"

    exit
)
