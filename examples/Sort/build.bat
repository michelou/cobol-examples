@echo off
setlocal enabledelayedexpansion

@rem only for interactive debugging !
set _DEBUG=0

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

if %_HELP%==1 (
    call :help
    exit /b !_EXITCODE!
)
if %_CLEAN%==1 (
    call :clean
    if not !_EXITCODE!==0 goto end
)
if %_COMPILE%==1 (
    call :compile
    if not !_EXITCODE!==0 goto end
)
if %_RUN%==1 (
    call :run
    if not !_EXITCODE!==0 goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

call :env_colors
set _DEBUG_LABEL=%_NORMAL_BG_CYAN%[%_BASENAME%]%_RESET%
set _ERROR_LABEL=%_STRONG_FG_RED%Error%_RESET%:
set _WARNING_LABEL=%_STRONG_FG_YELLOW%Warning%_RESET%:

set "_SOURCE_DIR=%_ROOT_DIR%src"
set "_SOURCE_MAIN_DIR=%_SOURCE_DIR%\main\cobol"
set "_TARGET_DIR=%_ROOT_DIR%target"

for /f "delims=" %%i in ("%~dp0\.") do set "_MAIN_NAME=%%~ni"
set "_EXE_FILE=%_TARGET_DIR%\%_MAIN_NAME%.exe"

@rem GnuCOBOL
if not exist "%COB_HOME%\bin\cobc.exe" (
    echo %_ERROR_LABEL% GnuCOBOL installation not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_COBC_CMD=%COB_HOME%\bin\cobc.exe"

@rem Micro Focus Visual COBOL
if %PROCESSOR_ARCHITECTURE%==AMD64 (
    set "_MFCOB_BIN_DIR=%COBDIR%\bin64"
    set _CCBL_NAME=ccbl64.exe
) else (
    set "_MFCOB_BIN_DIR=%COBDIR%\bin"
    set _CCBL_NAME=ccbl.exe
)
set _CCBL_CMD=
if exist "%_MFCOB_BIN_DIR%\%_CCBL_NAME%" (
    set "_CCBL_CMD=%_MFCOB_BIN_DIR%\%_CCBL_NAME%"
    set "_CBLLINK_CMD=%_MFCOB_BIN_DIR%\cbllink.exe"
    set "_CBLPROMP_CMD=%_MFCOB_BIN_DIR%\cblpromp.exe"
)

@rem COBOL 4J
if exist "%COBJ_HOME%\bin\cobj.exe" (
    set "_COBJ_CMD=%COBJ_HOME%\bin\cobj.exe"
)
if exist "%JAVA_HOME%\bin\java.exe" (
    set "_JAVA_CMD=%JAVA_HOME%\bin\java.exe"
)

@rem we use the newer PowerShell version if available
where /q pwsh.exe
if %ERRORLEVEL%==0 ( set _PWSH_CMD=pwsh.exe
) else ( set _PWSH_CMD=powershell.exe
)
@rem https://conemu.github.io/en/NewConsole.html
if defined ConEmuDir ( set _PWSH_OPTS=-cur_console:i
) else ( set _PWSH_OPTS=
)
goto :eof

:env_colors
@rem ANSI colors in standard Windows 10 shell
@rem see https://gist.github.com/mlocati/#file-win10colors-cmd

@rem normal foreground colors
set _NORMAL_FG_BLACK=[30m
set _NORMAL_FG_RED=[31m
set _NORMAL_FG_GREEN=[32m
set _NORMAL_FG_YELLOW=[33m
set _NORMAL_FG_BLUE=[34m
set _NORMAL_FG_MAGENTA=[35m
set _NORMAL_FG_CYAN=[36m
set _NORMAL_FG_WHITE=[37m

@rem normal background colors
set _NORMAL_BG_BLACK=[40m
set _NORMAL_BG_RED=[41m
set _NORMAL_BG_GREEN=[42m
set _NORMAL_BG_YELLOW=[43m
set _NORMAL_BG_BLUE=[44m
set _NORMAL_BG_MAGENTA=[45m
set _NORMAL_BG_CYAN=[46m
set _NORMAL_BG_WHITE=[47m

@rem strong foreground colors
set _STRONG_FG_BLACK=[90m
set _STRONG_FG_RED=[91m
set _STRONG_FG_GREEN=[92m
set _STRONG_FG_YELLOW=[93m
set _STRONG_FG_BLUE=[94m
set _STRONG_FG_MAGENTA=[95m
set _STRONG_FG_CYAN=[96m
set _STRONG_FG_WHITE=[97m

@rem strong background colors
set _STRONG_BG_BLACK=[100m
set _STRONG_BG_RED=[101m
set _STRONG_BG_GREEN=[102m
set _STRONG_BG_YELLOW=[103m
set _STRONG_BG_BLUE=[104m

@rem we define _RESET in last position with crazy console output with type command
set _BOLD=[1m
set _UNDERSCORE=[4m
set _INVERSE=[7m
set _RESET=[0m
goto :eof

@rem input parameter: %*
@rem output parameters: _CLEAN, _COMPILE, _DEBUG, _RUN, _VERBOSE
:args
set _CLEAN=0
set _COMPILE=0
set _DOC=0
set _FORMAT=free
set _HELP=0
set _LINT=0
set _RUN=0
@rem option -std:<name>, name=default, cobol2014, cobol2002, cobol85, xopen, ibm-strict,
@rem                          ibm, mvs-strict, mvs, mf-strict, mf, bs2000-strict, bs2000
set _STANDARD=cobol2014
set _TOOLSET=gnu
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG (
    if !__N!==0 set _HELP=1
    goto args_done
)
if "%__ARG:~0,1%"=="-" (
    @rem option
    if "%__ARG%"=="-cobj" ( set _TOOLSET=cobj
    ) else if "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if "%__ARG%"=="-fixed" ( set _FORMAT=fixed
    ) else if "%__ARG%"=="-free" ( set _FORMAT=free
    ) else if "%__ARG%"=="-gnu" ( set _TOOLSET=gnu
    ) else if "%__ARG%"=="-help" ( set _HELP=1
    ) else if "%__ARG%"=="-mf" ( set _TOOLSET=mf
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option "%__ARG%" 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    if "%__ARG%"=="clean" ( set _CLEAN=1
    ) else if "%__ARG%"=="compile" ( set _COMPILE=1
    ) else if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="run" ( set _COMPILE=1& set _RUN=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand "%__ARG%" 1>&2
        set _EXITCODE=1
        goto args_done
    )
    set /a __N+=1
)
shift
goto args_loop
:args_done
set _STDOUT_REDIRECT=1^>NUL
if %_DEBUG%==1 set _STDOUT_REDIRECT=

if %_TOOLSET%==mf if not defined _CCBL_CMD (
    echo %_WARNING_LABEL% Visual COBOL installation not found 1>&2
    set _TOOLSET=gnu
)
if %_TOOLSET%==cobj if not defined _COBJ_CMD (
    echo %_WARNING_LABEL% COBOL 4J installation not found 1>&2
    set _TOOLSET=gnu
)
if %_FORMAT%==fixed (
    if exist "%_SOURCE_MAIN_DIR%-fixed" ( set "_SOURCE_MAIN_DIR=%_SOURCE_MAIN_DIR%-fixed"
    ) else ( set _FORMAT=fixed2
    )
)
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options    : _FORMAT=%_FORMAT% _STANDARD=%_STANDARD% _TOOLSET=%_TOOLSET% _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Subcommands: _CLEAN=%_CLEAN% _COMPILE=%_COMPILE% _RUN=%_RUN% 1>&2
    echo %_DEBUG_LABEL% Variables  : "COB_HOME=%COB_HOME%" 1>&2
    if defined _CCBL_CMD echo %_DEBUG_LABEL% Variables  : "COBDIR=%COBDIR%" 1>&2
    if defined _COBJ_CMD echo %_DEBUG_LABEL% Variables  : "COBJ_HOME=%COBJ_HOME%" 1>&2
    echo %_DEBUG_LABEL% Variables  : "GIT_HOME=%GIT_HOME%" 1>&2
)
goto :eof

:help
if %_VERBOSE%==1 (
    set __BEG_P=%_STRONG_FG_CYAN%
    set __BEG_O=%_STRONG_FG_GREEN%
    set __BEG_N=%_NORMAL_FG_YELLOW%
    set __END=%_RESET%
) else (
    set __BEG_P=
    set __BEG_O=
    set __BEG_N=
    set __END=
)
echo Usage: %__BEG_O%%_BASENAME% { ^<option^> ^| ^<subcommand^> }%__END%
echo.
echo   %__BEG_P%Options:%__END%
echo     %__BEG_O%-cobj%__END%       select COBOL 4J tools
echo     %__BEG_O%-debug%__END%      print commands executed by this script
echo     %__BEG_O%-fixed%__END%      enable fixed-format code
echo     %__BEG_O%-free%__END%       enable free-format code ^(default^)
echo     %__BEG_O%-gnu%__END%        select GnuCOBOL tools
echo     %__BEG_O%-mf%__END%         select Visual COBOL tools
echo     %__BEG_O%-verbose%__END%    print progress messages
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%clean%__END%       delete generated files
echo     %__BEG_O%compile%__END%     generate executable files
echo     %__BEG_O%help%__END%        print this help message
echo     %__BEG_O%run%__END%         execute COBOL program "!_EXE_FILE:%_ROOT_DIR%=!"
goto :eof

:clean
call :rmdir "%_TARGET_DIR%"
goto :eof

@rem input parameter: %1=directory path
:rmdir
set "__DIR=%~1"
if not exist "%__DIR%\" goto :eof
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% rmdir /s /q "%__DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
)
rmdir /s /q "%__DIR%"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:compile
if not exist "%_TARGET_DIR%" mkdir "%_TARGET_DIR%"

call :action_required "%_EXE_FILE%" "%_SOURCE_MAIN_DIR%\*.cbl"
if %_ACTION_REQUIRED%==0 goto :eof

call :compile_%_TOOLSET%
goto :eof

:compile_cobj
if not exist "%_TARGET_DIR%\classes" mkdir "%_TARGET_DIR%\classes"
if not exist "%_TARGET_DIR%\src" mkdir "%_TARGET_DIR%\src"

set __SOURCE_FILES=
set __N=0
for /f "delims=" %%f in ('dir /s /b "%_SOURCE_MAIN_DIR%\*.cbl" "%_SOURCE_MAIN_DIR%\*.cob" 2^>NUL') do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
    set /a __N+=1
)
if %__N%==0 (
    echo %_WARNING_LABEL% No COBOL source file found 1>&2
    goto :eof
) else if %__N%==1 ( set __N_FILES=%__N% COBOL source file
) else ( set __N_FILES=%__N% COBOL source files
)
for /f "delims=" %%f in ("%_ROOT_DIR%.") do set "_CONFIG_FILE=%%~dpfdefault.conf"
if not exist "%_CONFIG_FILE%" (
    echo %_ERROR_LABEL% Configuration file "default.conf" not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__PATH=%PATH%"
set "PATH=%PATH%;%JAVA_HOME%\bin"
set "__CLASSPATH=%CLASSPATH%"
if defined CLASSPATH ( set "CLASSPATH=%CLASSPATH%;%COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar"
) else ( set "CLASSPATH=%COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar"
)
if %_DEBUG%==1 (
    for /f "delims=" %%i in ('where javac') do echo %_DEBUG_LABEL% %%i 1>&2
    echo %_DEBUG_LABEL% CLASSPATH=%CLASSPATH% 1>&2
)
set __COBJ_OPTS="-conf=%_CONFIG_FILE%" -o "%_TARGET_DIR:\=/%/classes" -j "%_TARGET_DIR:\=/%/src"
if %_FORMAT%==free set __COBJ_OPTS=-free %__COBJ_OPTS%

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_COBJ_CMD%" %__COBJ_OPTS% %__SOURCE_FILES:\=/%
) else if %_VERBOSE%==1 echo Compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(COBOL 4J^) 1>&2
)
call "%_COBJ_CMD%" %__COBJ_OPTS% %__SOURCE_FILES:\=/%
if not %ERRORLEVEL%==0 (
    set "PATH=%__PATH%"
    set "CLASSPATH=%__CLASSPATH%"
    echo %_ERROR_LABEL% Failed to compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(COBOL 4J^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "PATH=%__PATH%"
set "CLASSPATH=%__CLASSPATH%"
goto :eof

:compile_gnu
@rem create fixed-format code from COBOL source file
if %_FORMAT%==fixed2 (
    if not exist "%GIT_HOME%\usr\bin\awk.exe" (
        echo %_ERROR_LABEL% Command awk not found 1>&2
        set _EXITCODE=1
        goto :eof
    )
    if not exist "%_TARGET_DIR%\cobol" mkdir "%_TARGET_DIR%\cobol"
    for /f "delims=" %%f in ('dir /b /s "%_SOURCE_MAIN_DIR%\*.cbl" "%_SOURCE_MAIN_DIR%\*.cob"') do (
        (
            echo BEGIN { n=1 }
            echo { printf^("%%05d  %%s\n",n,$0^); n+=1 }
        ) > "%_TARGET_DIR%\%%~nf.awk"
        call "%GIT_HOME%\usr\bin\awk.exe" -f "%_TARGET_DIR%\%%~nf.awk" "%%f" > "%_TARGET_DIR%\cobol\%%~nxf"
    )
    set "_SOURCE_MAIN_DIR=%_TARGET_DIR%\cobol"
)
set __SOURCE_FILES=
set __N=0
for /f "delims=" %%f in ('dir /s /b "%_SOURCE_MAIN_DIR%\*.cbl" "%_SOURCE_MAIN_DIR%\*.cob" 2^>NUL') do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
    set /a __N+=1
)
if %__N%==0 (
    echo %_WARNING_LABEL% No COBOL source file found 1>&2
    goto :eof
) else if %__N%==1 ( set __N_FILES=%__N% COBOL source file
) else ( set __N_FILES=%__N% COBOL source files
)
setlocal
set "COB_CC=%COB_HOME%\mingw64\bin\gcc.exe"
@rem we need to specify -I "%MSYS_HOME%\mingw64\include" in the case we use the GMP library
set COB_CFLAGS=-I "%COB_HOME%\include" -pipe -Wno-unused -fsigned-char -Wno-pointer-sign
set COB_LIBS=-L "%COB_HOME%\lib" -lcob
set "COB_CONFIG_DIR=%COB_HOME%\config"
set "COB_COPY_DIR=%COB_HOME%\copy"
if %_DEBUG%==1 (
    @rem we print the customized environment variables
    echo %_DEBUG_LABEL% "%_COBC_CMD%" --info ^| findstr env: 1>&2
    for /f "delims=" %%i in ('call "%_COBC_CMD%" --info ^| findstr env:') do echo %_DEBUG_LABEL% %%i 1>&2
)
@rem option -x = build an executable program
set __COBC_OPTS=-std=%_STANDARD% -x -o "%_EXE_FILE%"
if %_FORMAT%==free set __COBC_OPTS=--free %__COBC_OPTS%
if %_DEBUG%==1 set __COBC_OPTS=--debug --verbose %__COBC_OPTS%

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_COBC_CMD%" %__COBC_OPTS% %__SOURCE_FILES%
) else if %_VERBOSE%==1 echo Compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(GnuCOBOL^) 1>&2
)
call "%_COBC_CMD%" %__COBC_OPTS% %__SOURCE_FILES% %_STDOUT_REDIRECT%
if not %ERRORLEVEL%==0 (
    endlocal
    echo %_ERROR_LABEL% Failed to compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(GnuCOBOL^) 1>&2
    set _EXITCODE=1
    goto :eof
)
endlocal
goto :eof

@rem Micro Focus Visual COBOL
:compile_mf
set __SOURCE_FILES=
set __N=0
for /f "delims=" %%f in ('dir /s /b "%_SOURCE_MAIN_DIR%\*.cbl" "%_SOURCE_MAIN_DIR%\*.cob" 2^>NUL') do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
    set /a __N+=1
)
if %__N%==0 (
    echo %_WARNING_LABEL% No COBOL source file found 1>&2
    goto :eof
) else if %__N%==1 ( set __N_FILES=%__N% COBOL source file
) else ( set __N_FILES=%__N% COBOL source files
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_CBLPROMP_CMD%" 1>&2
) else if %_VERBOSE%==1 ( echo Generate Micro Focus environment file 1>&2
)
call "%_CBLPROMP_CMD%" -n > "%_TARGET_DIR%\%_MAIN_NAME%_mfsetenv.bat"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL%% Failed to generate Micro Focus environment file 1>&2
    set _EXITCODE=1
    goto :eof
)
setlocal
call "%_TARGET_DIR%\%_MAIN_NAME%_mfsetenv.bat"

@rem https://web.cse.ohio-state.edu/~reeves.92/CSE314/COBOLmanpage.htm
@rem Option -a causes all warning messages to be displayed
set __CCBL_OPTS=-Sa -a -o "%_TARGET_DIR%\%_MAIN_NAME%.exe"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_CCBL_CMD%" %__CCBL_OPTS% %__SOURCE_FILES%
) else if %_VERBOSE%==1 ( echo Compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(Visual COBOL^) 1>&2
)
call "%_CCBL_CMD%" %__CCBL_OPTS% %__SOURCE_FILES%
if not %ERRORLEVEL%==0 (
    endlocal
    echo %_ERROR_LABEL% Failed to compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" ^(Visual COBOL^) 1>&2
    set _EXITCODE=1
    goto :eof
)
@rem Options: -K = Do not delete temporary files
set _CBLLINK_OPTS=-F -M%_MAIN_NAME% -O%_EXE_FILE%
if %_DEBUG%==1 set _CBLLINK_OPTS=-K -V %_CBLLINK_OPTS%

set __OBJECT_FILES=
set __N=0
for /f "delims=" %%f in ('dir /s /b "%_TARGET_DIR%\*.exe.int" 2^>NUL') do (
    set __OBJECT_FILES=!__OBJECT_FILES! "%%f"
    set /a __N+=1
)
if %__N%==1 ( set __N_FILES=%__N% COBOL object file
) else ( set __N_FILES=%__N% COBOL object files
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_CBLLINK_CMD%" %_CBLLINK_OPTS% %__OBJECT_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Link %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
)
pushd "%_TARGET_DIR%"
call "%_CBLLINK_CMD%" %_CBLLINK_OPTS% %__OBJECT_FILES% %_STDOUT_REDIRECT%
if not %ERRORLEVEL%==0 (
    popd
    endlocal
    echo %_ERROR_LABEL% Failed to link %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
popd
endlocal
goto :eof

@rem input parameter: 1=target file 2,3,..=path (wildcards accepted)
@rem output parameter: _ACTION_REQUIRED
:action_required
set "__TARGET_FILE=%~1"

set __PATH_ARRAY=
set __PATH_ARRAY1=
:action_path
shift
set __PATH=%~1
if not defined __PATH goto action_next
set __PATH_ARRAY=%__PATH_ARRAY%,'%__PATH%'
set __PATH_ARRAY1=%__PATH_ARRAY1%,'!__PATH:%_ROOT_DIR%=!'
goto action_path

:action_next
set __TARGET_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`call "%_PWSH_CMD%" -c "gci -path '%__TARGET_FILE%' -ea Stop | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
     set __TARGET_TIMESTAMP=%%i
)
set __SOURCE_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`call "%_PWSH_CMD%" -c "gci -recurse -path %__PATH_ARRAY:~1% -ea Stop | sort LastWriteTime | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
    set __SOURCE_TIMESTAMP=%%i
)
call :newer %__SOURCE_TIMESTAMP% %__TARGET_TIMESTAMP%
set _ACTION_REQUIRED=%_NEWER%
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% %__TARGET_TIMESTAMP% Target : '%__TARGET_FILE%' 1>&2
    echo %_DEBUG_LABEL% %__SOURCE_TIMESTAMP% Sources: %__PATH_ARRAY:~1% 1>&2
    echo %_DEBUG_LABEL% _ACTION_REQUIRED=%_ACTION_REQUIRED% 1>&2
) else if %_VERBOSE%==1 if %_ACTION_REQUIRED%==0 if %__SOURCE_TIMESTAMP% gtr 0 (
    echo No action required ^(%__PATH_ARRAY1:~1%^) 1>&2
)
goto :eof

@rem output parameter: _NEWER
:newer
set __TIMESTAMP1=%~1
set __TIMESTAMP2=%~2

set __DATE1=%__TIMESTAMP1:~0,8%
set __TIME1=%__TIMESTAMP1:~-6%

set __DATE2=%__TIMESTAMP2:~0,8%
set __TIME2=%__TIMESTAMP2:~-6%

if %__DATE1% gtr %__DATE2% ( set _NEWER=1
) else if %__DATE1% lss %__DATE2% ( set _NEWER=0
) else if %__TIME1% gtr %__TIME2% ( set _NEWER=1
) else ( set _NEWER=0
)
goto :eof

:run
call :run_%_TOOLSET%
goto :eof

:run_cobj
set __JAVA_OPTS=-cp "%COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar;%_TARGET_DIR%\classes"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_JAVA_CMD%" %__JAVA_OPTS% "%_MAIN_NAME%" 1>&2
) else if %_VERBOSE%==1 ( echo Execute Java program "%_MAIN_NAME%" 1>&2
)
call "%_JAVA_CMD%" %__JAVA_OPTS% "%_MAIN_NAME%"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to execute Java program "%_MAIN_NAME%" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:run_gnu
if not exist "%_EXE_FILE%" (
    echo %_DEBUG_LABEL% Main program "!_EXE_FILE:%_ROOT_DIR%=!" not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__PATH=%PATH%"
set "PATH=%COB_HOME%\bin;%PATH%"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% Execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
) else if %_VERBOSE%==1 ( echo Execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
)
call "%_EXE_FILE%"
if not %ERRORLEVEL%==0 (
    set "PATH=%__PATH%"
    echo %_DEBUG_LABEL% Failed to execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
set "PATH=%__PATH%"
goto :eof

:run_mf
if not exist "%_EXE_FILE%" (
    echo %_DEBUG_LABEL% Main program "!_EXE_FILE:%_ROOT_DIR%=!" not found 1>&2
    set _EXITCODE=1
    goto :eof
)
@rem we need clear the screen before output start at position 0,0
cls

set "__PATH=%PATH%"
set "PATH=%_MFCOB_BIN_DIR%"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% Execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
) else if %_VERBOSE%==1 ( echo Execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
)
call "%_EXE_FILE%"
if not %ERRORLEVEL%==0 (
    set "PATH=%__PATH%"
    echo %_DEBUG_LABEL% Failed to execute program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
set "PATH=%__PATH%"
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
