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

if not exist "%COB_HOME%\bin\cobc.exe" (
    echo %_ERROR_LABEL% COBOL installation not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_COBC_CMD=%COB_HOME%\bin\cobc.exe"

@rem Visual COBOL
if %PROCESSOR_ARCHITECTURE%==AMD64 ( set "_COB_BIN_DIR=%COBDIR%\bin64"
) else ( set "_COB_BIN_DIR=%COBDIR%\bin"
)
set _CCBL_CMD=
if exist "%_COB_BIN_DIR%\ccbl.exe" (
    set "_CCBL_CMD=%_COB_BIN_DIR%\ccbl.exe"
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
set _FORMAT=fixed
set _HELP=0
set _LINT=0
set _RUN=0
@rem option -std:<name>, name=default, cobol2014
set _STANDARD=default
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
    if "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if "%__ARG%"=="-help" ( set _HELP=1
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

if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options    : _FORMAT=%_FORMAT% _STANDARD=%_STANDARD% _TARGET=%_TARGET% _VERBOSE=%_VERBOSE% 1>&2
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
echo     %__BEG_O%-debug%__END%      print commands executed by this script
echo     %__BEG_O%-verbose%__END%    print progress messages
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%clean%__END%       delete generated files
echo     %__BEG_O%compile%__END%     generate executable files
echo     %__BEG_O%help%__END%        print this help message
echo     %__BEG_O%run%__END%         execute the generated program "!_EXE_FILE:%_ROOT_DIR%=!"
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
set COB_CFLAGS=-I "%COB_HOME%\include" -pipe -Wno-unused -fsigned-char -Wno-pointer-sign
set COB_LIBS=-L "%COB_HOME%\lib" -lcob
set "COB_CONFIG_DIR=%COB_HOME%\config"
set "COB_COPY_DIR=%COB_HOME%\copy"
if %_DEBUG%==1 (
    @rem we print the customized environment variables
    echo %_DEBUG_LABEL% "%_COBC_CMD%" --info ^| findstr env: 1>&2
    call "%_COBC_CMD%" --info | findstr env: 1>&2
)
@rem -x = build an executable program
set __COBC_OPTS=-std=%_STANDARD% -x -o "%_EXE_FILE%"
if %_FORMAT%==free set __COBC_OPTS=--free %__COBC_OPTS%
if %_DEBUG%==1 set __COBC_OPTS=--debug --verbose %__COBC_OPTS%

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_COBC_CMD%" %__COBC_OPTS% %__SOURCE_FILES%
) else if %_VERBOSE%==1 echo Compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
)
call "%_COBC_CMD%" %__COBC_OPTS% %__SOURCE_FILES% %_STDOUT_REDIRECT%
if not %ERRORLEVEL%==0 (
    endlocal
    echo %_ERROR_LABEL% Failed to compile %__N_FILES% into directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
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

@rem #########################################################################
@rem ## Cleanups

:end
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
