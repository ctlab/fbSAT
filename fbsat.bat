@chcp 866 >nul
@call gradlew.bat -q installDist && fbsat-cli\build\install\cli\bin\fbSAT.bat %*
