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

set __BUILD_OPTS=
if %_VERBOSE%==1 set __BUILD_OPTS=-verbose %__BUILD_OPTS%

for /f "delims=" %%f in ('dir /b /ad "%_ROOT_DIR%\*"') do (
    if exist "%_ROOT_DIR%%%f\build.bat" (
        if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_ROOT_DIR%%%f\build.bat" %__BUILD_OPTS% clean 1>&2
        ) else if %_VERBOSE%==1 ( echo Execute batch file "%%f\build.bat" %__BUILD_OPTS% clean 1>&2
        )
        call "%_ROOT_DIR%%%f\build.bat" %__BUILD_OPTS% clean
    )
)
goto end

@rem #########################################################################
@rem ## Subroutines

:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

set _DEBUG_LABEL=[%_BASENAME%]
set _ERROR_LABEL=Error:
set _WARNING_LABEL=Warning:
goto :eof


@rem input parameter: %*
:args
set _HELP=0
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG (
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
   echo %_ERROR_LABEL% Unknown subcommand "%__ARG%" 1>&2
   set _EXITCODE=1
   goto args_done
)
shift
goto args_loop
:args_done
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
