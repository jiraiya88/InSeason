@echo off
set /p "jpty=Enter Storm Name: "
set /p "eveid=Enter EventID: "
set /p "startid=Enter Storm Approaching Landfall Time in DDMMHH: "

cd D:\03_JapanAER\IN_SEASON_ANALYSIS
python.exe "D:\03_JapanAER\IN_SEASON_ANALYSIS\1_convert.py" --StormName %jpty% --EventID %eveid%
"C:\Program Files\R\R-4.0.4\bin\x64\Rscript.exe" "D:\03_JapanAER\IN_SEASON_ANALYSIS\2_interp1km.R"
"C:\Program Files\R\R-4.0.4\bin\x64\Rscript.exe" "D:\03_JapanAER\IN_SEASON_ANALYSIS\3_oasis.R"
python.exe "D:\03_JapanAER\IN_SEASON_ANALYSIS\4_JP_footprint_map.py" --Landfall %startid%
pause
