#!/usr/bin/env pwsh
#
# Copyright (c) 2018-2026 Stéphane Micheloud
#
# Licensed under the MIT License.
#

## https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_requires
#Requires -Version 5

## only for interactive debugging !
$DEBUG = $false

#########################################################################
## Environment setup

$EXITCODE = 0

$EXE = ""
if ($PSVersionTable.PSVersion -lt "6.0" -or $IsWindows) {
  # Fix case when both the Windows and Linux builds of this program
  # are installed in the same directory.
  $EXE = '.exe'
}

$BASENAME = (Get-Item $PSScriptRoot).Basename
$ROOT_DIR = $PSScriptRoot
$PATH_SEP = [IO.Path]::PathSeparator
$SEP = [IO.Path]::DirectorySeparatorChar

$SOURCE_DIR = $ROOT_DIR + $SEP + 'src'
$SOURCE_MAIN_DIR = $SOURCE_DIR + $SEP + 'main' + $SEP + 'cobol'
$TARGET_DIR = $ROOT_DIR + $SEP + 'target'
$TARGET_SRC_DIR = $TARGET_DIR + $SEP + 'src'
$CLASSES_DIR = $TARGET_DIR + $SEP + 'classes'

$COBC_CMD = $Env:COBC_HOME + $SEP + 'bin' + $SEP + 'cobc' + $EXE
if (! (Test-Path -PathType Leaf -Path $COBC_CMD)) {
    $COBC_CMD = $null
}
$COBJ_CMD = $Env:COBJ_HOME + $SEP + 'bin' + $SEP + 'cobj' + $EXE
if (! (Test-Path -PathType Leaf -Path $COBJ_CMD)) {
    $COBJ_CMD = $null
}
$COBJ_LIB_PATH = $Env:COBJ_HOME + $SEP + 'lib' + $SEP + 'opensourcecobol4j'
   
## $JAVA_HOME is used by COBOL 4J
$JAVA_CMD = $Env:JAVA_HOME + $SEP + 'bin' + $SEP + 'java' + $EXE
if (! (Test-Path -PathType Leaf -Path $JAVA_CMD)) {
    $JAVA_CMD = $null
}

$PROJECT_NAME = $BASENAME
$PROJECT_VERSION = '1.0-SNAPSHOT'

$TARGET_FILE = $TARGET_DIR + $SEP + $PROJECT_NAME + $EXE

#########################################################################
## Script arguments

$COMMANDS = @()

## Possible values: SilentlyContinue, Stop, Continue, Inquire, Ignore, Suspend
$DebugPreference = 'SilentlyContinue'
$VerbosePreference = 'SilentlyContinue'
$WarningPreference = 'Continue'

## option -std:<name>, name=default, cobol2014
$STANDARD = 'cobol2014'

$FORMAT = 'free'
$TIMER = $false
$TOOLSET = 'gnu'
$VERBOSE = $false
$N = 0
foreach ($ARG in $args) {
    if ($ARG.StartsWith("-")) {
        ## option
        
        if ($ARG -ieq '-cobj') { $TOOLSET = 'cobj'
        } elseif ($ARG -ieq '-debug') { $DEBUG = $true; $DebugPreference='Continue'
        } elseif ($ARG -ieq '-help' ) { $COMMANDS = 'Print-Help'
        } elseif ($ARG -ieq '-fixed') { $FORMAT = 'fixed'
        } elseif ($ARG -ieq '-free') { $FORMAT = 'free'
        } elseif ($ARG -ieq '-gnu') { $TOOLSET = 'gnu'
        } elseif ($ARG -ieq "-help") { $COMMANDS = 'Print-Help'
        } elseif ($ARG -ieq "-verbose") { $VERBOSE = $true; $VerbosePreference = 'Continue'
        } else {
            Write-Error "Unknown option ""$ARG"""
            $EXITCODE = 1
            break
        }
    } else {
        ## subcommand
        if ($ARG -ieq "clean") { $COMMANDS += 'Clean'
        } elseif ($ARG -ieq "compile") { $COMMANDS += 'Compile'
        } elseif ($ARG -ieq "help") { $COMMANDS = 'Print-Help'
        } elseif ($ARG -ieq "lint") { $COMMANDS += 'Lint'
        } elseif ($ARG -ieq "run" ) { $COMMANDS += 'Compile', 'Run'
        } elseif ($ARG -ieq "test") { $COMMANDS += 'Compile', 'Test'
        } else {
            Write-Error "Unknown subcommand ""$ARG"""
            $EXITCODE = 1
            break
        }
        $N++
    }
}
if ($FORMAT -eq 'fixed' -and (Test-Path -Path $SOURCE_MAIN_DIR + '-fixed')) {
    SOURCE_MAIN_DIR = $SOURCE_MAIN_DIR + '-fixed'
}
Write-Debug "Options    : FORMAT=$FORMAT STANDARD=$STANDARD TIMER=$TIMER TOOLSET=$TOOLSET VERBOSE=$VERBOSE"
Write-Debug "Subcommands: $COMMANDS"
Write-Debug "Variables  : ""COB_HOME=$Env:COB_HOME"""
Write-Debug "Variables  : ""COBJ_HOME=$Env:COBJ_HOME"""
Write-Debug "Variables  : ""GIT_HOME=$Env:GIT_HOME"""
Write-Debug "Variables  : ""JAVA_HOME=$Env:JAVA_HOME"""
Write-Debug "Variables  : MAIN_NAME=$MAIN_NAME MAIN_ARGS=$MAIN_ARGS"
Write-Debug "Variables  : PROJECT_NAME=$PROJECT_NAME"

if ($TIMER) { $TIMER_START = Get-Date }

#########################################################################
## Subroutines

function Main
{
    foreach($COMMAND in $COMMANDS) {
        &$COMMAND
        if ($EXITCODE -ne 0) { exit $EXITCODE }
    }
    if ($TIMER) {
        $DURATION = New-TimeSpan -Start $TIMER_START -End (Get-Date)
        Write-Output "Total execution time: $DURATION"
    }
    Cleanup $EXITCODE
}

function Print-Help
{
    Write-Output "Usage: $BASENAME { <option> | <subcommand> }"
    Write-Output ""
    Write-Output "   Options:"
    Write-Output "     -cobj       select COBOL 4J tools"
    Write-Output "     -debug      print commands executed by this script"
    Write-Output "     -fixed      enable fixed-format code"
    Write-Output "     -free       enable free-format code (default)"
    Write-Output "     -timer      print total execution time"
    Write-Output "     -verbose    print progress messages"
    Write-Output ""
    Write-Output "   Subcommands:"
    Write-Output "     clean       delete generated files"
    Write-Output "     compile     compile Cobol source files"
    Write-Output "     help        print this help message"
    Write-Output "     run         execute main program ""$MAIN_NAME"""
}

function Clean
{
    Delete-Dir $TARGET_DIR
}

function Delete-Dir
{
    param (
        [string]$dir
    )
    if (Test-Path -PathType Container -Path $dir) {
        Write-Debug "[System.IO.Directory]::Delete('$dir', $true)"
        Write-Verbose "Delete directory ""$($dir.Replace($ROOT_DIR + $SEP, ''))"""
        #Remove-Item -Path $dir -Force -Recurse
        try {
            [System.IO.Directory]::Delete($dir, $true)
        } catch {
            Write-Error "Failed to delete directory ""$($dir.Replace($ROOT_DIR + $SEP, ''))"""
            $EXITCODE = 1
            return
        }
    }
}

function Lint
{
}

function Compile
{
    if (! (Test-Path -PathType Container -Path $TARGET_DIR)) {
        $_ = New-Item -ItemType Directory -Path $TARGET_DIR
    }
    if (Test-Action-Required -FilePath "$TARGET_FILE" -DirPath "$SOURCE_MAIN_DIR" '*.cbl') {
        if ($TOOLSET -eq 'cobj') { Compile-Cobj
        } elseif ($TOOLSET -eq 'gnu') { Compile-Gnu
        } else { Write-Error "Unknown toolset ""$TOOLSET"""
        }
    }
}

function Compile-Cobj
{
    if (! (Test-Path -PathType Container -Path $CLASSES_DIR)) {
        $_ = New-Item -ItemType Directory -Path $CLASSES_DIR
    }
    if (! (Test-Path -PathType Container -Path $TARGET_SRC_DIR)) {
        $_ = New-Item -ItemType Directory -Path $TARGET_SRC_DIR
    }

    $SOURCE_FILES = (Get-ChildItem -Path $SOURCE_MAIN_DIR -Include "*.cbl" -Recurse).FullName
    $N = $SOURCE_FILES.Count
    if ($N -eq 0) {
        Write-Warning "No Cobol source file found"
        return
    } elseif ($N -eq 1) { $N_FILES = "$N Cobol source file"
    } else { $N_FILES = "$N Cobol source files"
    }
    $OLD_PATH = $PATH
    $OLD_CLASSPATH = $CLASSPATH

    $CONFIG_FILE = $(Split-Path -Path $ROOT_DIR -Parent) + $SEP + 'default.conf'
    if (! (Test-Path -PathType Leaf -Path $CONFIG_FILE)) {
        Write-Error "Configuration file ""default.conf"" not found"
        Cleanup 1
    }
    $Env:PATH = $PATH + $PATH_SEP + $JAVA_HOME + $SEP + 'bin'
    if ($CLASSPATH) { $CLASSPATH = $CLASSPATH + $PATH_SEP + $COBJ_LIB_PATH + $SEP + 'libcobj.jar'
    } else { $CLASSPATH = $COBJ_LIB_PATH + $SEP + 'libcobj.jar'
    }
    Write-Debug "CLASSPATH=$CLASSPATH"

    $COBJ_OPTS = "-conf=""$CONFIG_FILE"" -o ""$($TARGET_DIR + $SEP + 'classes')"" -j ""$($TARGET_DIR + $SEP + 'src')"""
    if ($FORMAT -eq 'free') { $COBJ_OPTS = "-free $COBJ_OPTS" }

    Write-Debug """$COBJ_CMD"" $COBJ_OPTS $SOURCE_FILES"
    Write-Verbose "Compile $n_files into directory ""$($TARGET_DIR.Replace($ROOT_DIR + '\', ''))"" (COBOL 4J)"
    &"$COBJ_CMD" -conf="$CONFIG_FILE" -o "$($TARGET_DIR + $SEP + 'classes')" -j "$($TARGET_DIR + $SEP + 'src')" $SOURCE_FILES
    if ($LASTEXITCODE -ne 0) {
        $Env:PATH = $OLD_PATH
        $Env:CLASSPATH = $OLD_CLASSPATH
        Write-Error "Failed to compile $n_files into directory ""$($TARGET_DIR.Replace($ROOT_DIR + '\', ''))"" (COBOL 4J)"
        Cleanup 1
    }
    $Env:PATH = $OLD_PATH
    $Env:CLASSPATH = $OLD_CLASSPATH
}

function Compile-Gnu
{
    echo "11111111"
    <#
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to compile $N_FILES to directory ""$($TARGET_DIR.Replace($ROOT_DIR +$SEP, ''))"""
        $EXITCODE = 1
        return
    }
    #>
}

function Test-Action-Required
{
    param (
        [string]$FilePath,
        [string]$DirPath,
        [string]$Pattern
    )
    $REQUIRED = $false
    if (Test-Path -PathType Container -Path $DirPath) {
        if (Test-Path -PathType Leaf -Path $FilePath) {
            $FILE_LAST_TIME = (Get-Item $FilePath).LastWriteTime
            $DIR_LAST_TIME = (Get-ChildItem -Path $DirPath -Include $Pattern -Recurse | Sort LastWriteTime | Select -Last 1).LastWriteTime
            $REQUIRED = $FILE_LAST_TIME -lt $DIR_LAST_TIME
        } else {
            $REQUIRED = $true
        }
    }
    Write-Debug "REQUIRED=$REQUIRED ($Pattern)"
    return $REQUIRED
}

function Run-Cobj
{
}

function Run-Gnu
{
    if (! (Test-Path -PathType Leaf -Path $TARGET_FILE)) {
        Write-Error "Main program ""$PROJECT_NAME"" not found ($TARGET_FILE)"
        $EXITCODE = 1
        return
    }
    Write-Debug """$TARGET_FILE"" $MAIN_ARGS"
    Write-Verbose "Execute Cobol program ""$($TARGET_FILE.Replace($ROOT_DIR + $SEP, ''))"""
    &$TARGET_FILE $MAIN_ARGS
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to execute Cobol program ""$($TARGET_FILE.Replace($ROOT_DIR, ''))"""
        $EXITCODE = 1
        return
    }
}

function Run
{
    if ($TOOLSET -eq 'cobj') { $(Run-Cobj)
    } elseif ($TOOLSET -eq 'gnu') { $(Run-Gnu)
    } else { Write-Error "Unknown toolset ""$TOOLSET"""
    }
}

function Compile-Test
{
    Write-Warning "Subcommand 'Compile-Test' is not yet implemented"
}

function Test
{
    Write-Warning "Subcommand 'Test' is not yet implemented"
}

function Cleanup
{
    param (
        [int]$ExitCode
    )
    Write-Debug "ExitCode=$ExitCode"
    exit $ExitCode
}

#########################################################################
## Entry-point

Main
