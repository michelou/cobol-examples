@echo off
setlocal

if defined JAVA_HOME ( set "_JAVA_CMD=%JAVA_HOME%\bin\java.exe"
) else ( set "_JAVA_CMD=java.exe"
)
if not exist "%_JAVA_CMD%" (
    echo Java command not found 1>&2
    exit /b 1
)
for /f "delims=" %%f in ("%~dp0.") do set "_ROOT_DIR=%%~dpf"
if not exist "%_ROOT_DIR%lib\opensourcecobol4j\libcobj.jar" (
    echo COBOL 4J library not found 1>&2
    exit /b 1
)
set _JAVA_OPTS=-cp "%_ROOT_DIR%lib\opensourcecobol4j\libcobj.jar"

@rem echo "%_JAVA_CMD%" %_JAVA_OPTS% jp.osscons.opensourcecobol.libcobj.user_util.indexed_file.IndexedFileUtilMain %* 1>&2
"%_JAVA_CMD%" %_JAVA_OPTS% jp.osscons.opensourcecobol.libcobj.user_util.indexed_file.IndexedFileUtilMain %*
