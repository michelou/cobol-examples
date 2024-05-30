@echo off
@rem
@rem cob-config
@rem
@rem Copyright (C) 2003,2004,2005,2006,2007 Keisuke Nishida
@rem Copyright (C) 2008-2011 Roger While
@rem
@rem This file is part of OpenCOBOL.
@rem
@rem The OpenCOBOL compiler is free software: you can redistribute it
@rem and/or modify it under the terms of the GNU General Public License
@rem as published by the Free Software Foundation, either version 2 of the
@rem License, or (at your option) any later version.
@rem
@rem OpenCOBOL is distributed in the hope that it will be useful,
@rem but WITHOUT ANY WARRANTY; without even the implied warranty of
@rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@rem GNU General Public License for more details.
@rem
@rem You should have received a copy of the GNU General Public License
@rem along with OpenCOBOL.  If not, see <http://www.gnu.org/licenses/>.

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

if %_ECHO_PREFIX%==yes echo %_PREFIX%

if %_ECHO_EXEC_PREFIX%==yes echo %_EXEC_PREFIX%

if %_ECHO_CFLAGS%==yes echo %_CFLAGS%

if %_ECHO_LIBS%==yes echo %_LIBS%

goto end

@rem #########################################################################
@rem ## Subroutines

:env
set _BASENAME=%~n0
for /f "delims=" %%f in ("%~dp0.") do set "__ROOT_DIR=%%~dpf"
@rem we remove the trailing file separator if present
if "%__ROOT_DIR:~-1%"=="\" set "__ROOT_DIR=%__ROOT_DIR:~0,-1%"

set "_PREFIX=%__ROOT_DIR%"
set "_EXEC_PREFIX=%_PREFIX%"

set "_LIBDIR=%_EXEC_PREFIX%\lib"
set "_INCLUDEDIR=%_PREFIX%\include"

for /f "tokens=1-3,4,*" %%i in ('"%_PREFIX%\bin\cobj.exe" --version ^| findstr /b opensource') do set _VERSION=%%~l
set _CFLAGS=-I"%_INCLUDEDIR%"
set _LIBS=-L"%_LIBDIR%" -lcob -lm
goto :eof

:usage
echo Usage: %~n0 [OPTIONS] 1>&2
echo Options: 1>&2
echo    [--prefix[=DIR]] 1>&2
echo    [--exec-prefix[=DIR]] 1>&2
echo    [--version] 1>&2
echo    [--libs] 1>&2
echo    [--cflags] 1>&2
goto :eof

:args
set _ECHO_CFLAGS=no
set _ECHO_LIBS=no
set _ECHO_PREFIX=no
set _ECHO_EXEC_PREFIX=no
set _EXEC_PREFIX=
set _EXEC_PREEFIX_SET=no
set _PREFIX=
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG (
    if !__N!==0 (
        call :usage
        set _EXITCODE=1
    )
    goto args_done
)
if "%__ARG:~0,2%"=="--" (
    @rem option
    if "%__ARG%"=="--prefix" ( set _ECHO_PREFIX=yes
    ) else if "%__ARG:~0,9%"=="--prefix=" (
        set "_PREFIX=%__ARG:~9%"
        if %_EXEC_PREEFIX_SET%==no set "_EXEC_PREFIX=%__ARG:~9%"
    ) else if "%__ARG%"=="--exec-prefix" ( set _ECHO_EXEC_PREFIX=yes
    ) else if "%__ARG:~0,14%"=="--exec-prefix=" (
        set "_EXEC_PREFIX=%__ARG:~14%"
        set _EXEC_PREEFIX_SET=yes
    ) else if "%__ARG%"=="--version" ( echo %_VERSION%
    ) else if "%__ARG%"=="--cflags" ( set _ECHO_CFLAGS=yes
    ) else if "%__ARG%"=="--libs" ( set _ECHO_LIBS=yes
    ) else (
        call :usage
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    call :usage
    set _EXITCODE=1
    goto args_done
)
set /a __N+=1
shift
goto args_loop
:args_done
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
