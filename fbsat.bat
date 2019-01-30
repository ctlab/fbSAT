@echo off

set DIRNAME=%~dp0
if "%DIRNAME%" == "" set DIRNAME=.

rem Jar from 'gradle jar'
java -jar "%~dp0/build/libs/fbSAT-1.0.jar" %*

rem Artifact from IDEA
rem java -jar "%~dp0/out/artifacts/fbSAT.jar" %*
