#!/usr/bin/env bash
#
# Copyright (c) 2018-2025 Stéphane Micheloud
#
# Licensed under the MIT License.
#

##############################################################################
## Subroutines

getHome() {
    local source="${BASH_SOURCE[0]}"
    while [[ -h "$source" ]]; do
        local linked="$(readlink "$source")"
        local dir="$( cd -P $(dirname "$source") && cd -P $(dirname "$linked") && pwd )"
        source="$dir/$(basename "$linked")"
    done
    ( cd -P "$(dirname "$source")" && pwd )
}

debug() {
    local DEBUG_LABEL="[46m[DEBUG][0m"
    [[ $DEBUG -eq 1 ]] && echo "$DEBUG_LABEL $1" 1>&2
}

warning() {
    local WARNING_LABEL="[46m[WARNING][0m"
    echo "$WARNING_LABEL $1" 1>&2
}

error() {
    local ERROR_LABEL="[91mError:[0m"
    echo "$ERROR_LABEL $1" 1>&2
}

# use variables EXITCODE, TIMER_START
cleanup() {
    [[ $1 =~ ^[0-1]$ ]] && EXITCODE=$1

    debug "EXITCODE=$EXITCODE"
    exit $EXITCODE
}

args() {
    [[ $# -eq 0 ]] && HELP=1 && return 1

    for arg in "$@"; do
        case "$arg" in
        ## options
        -debug)    DEBUG=1 ;;
        -fixed)    FORMAT=fixed ;;
        -free)     FORMAT=free ;;
        -help)     HELP=1 ;;
        -verbose)  VERBOSE=1 ;;
        -*)
            error "Unknown option $arg"
            EXITCODE=1 && return 0
            ;;
        ## subcommands
        clean)     CLEAN=1 ;;
        compile)   COMPILE=1 ;;
        help)      HELP=1 ;;
        run)       COMPILE=1 && RUN=1 ;;
        *)
            error "Unknown subcommand $arg"
            EXITCODE=1 && return 0
            ;;
        esac
    done
    debug "Options    : FORMAT=$FORMAT STANDARD=$STANDARD TOOLSET=$TOOLSET VERBOSE=$VERBOSE"
    debug "Subcommands: CLEAN=$CLEAN COMPILE=$COMPILE HELP=$HELP RUN=$RUN"
    debug "Variables  : COB_HOME=$COB_HOME"
    debug "Variables  : GIT_HOME=$GIT_HOME"
    # See http://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
    [[ $TIMER -eq 1 ]] && TIMER_START=$(date +"%s")
}

help() {
    cat << EOS
Usage: $BASENAME { <option> | <subcommand> }

  Options:
    -debug       print commands executed by this script
    -fixed       enable fixed-format code
    -free        enable free-format code (default)
    -verbose     print progress messages

  Subcommands:
    clean        delete generated files
    compile      compile COBOL source files
    help         print this help message
    run          execute COBOL program "$PROJECT_NAME$TARGET_EXT"
EOS
}

clean() {
    if [[ -d "$TARGET_DIR" ]]; then
        if [[ $DEBUG -eq 1 ]]; then
            debug "Delete directory \"$TARGET_DIR\""
        elif [[ $VERBOSE -eq 1 ]]; then
            echo "Delete directory \"${TARGET_DIR/$ROOT_DIR\//}\"" 1>&2
        fi
        rm -rf "$TARGET_DIR"
        [[ $? -eq 0 ]] || ( EXITCODE=1 && return 0 )
    fi
}

compile() {
    [[ -d "$TARGET_DIR" ]] || mkdir -p "$TARGET_DIR"

    local is_required=0
    is_required="$(action_required "$TARGET_FILE" "$SOURCE_MAIN_DIR/" "*.cbl")"
    if [[ $is_required -eq 1 ]]; then
        compile_cob
        [[ $? -eq 0 ]] || ( EXITCODE=1 && return 0 )
    fi
}

action_required() {
    local target_file=$1
    local search_path=$2
    local search_pattern=$3
    local source_file=
    for f in $(find "$search_path" -type f -name "$search_pattern" 2>/dev/null); do
        [[ $f -nt $source_file ]] && source_file=$f
    done
    if [[ -z "$source_file" ]]; then
        ## Do not compile if no source file
        echo 0
    elif [[ ! -f "$target_file" ]]; then
        ## Do compile if target file doesn't exist
        echo 1
    else
        ## Do compile if target file is older than most recent source file
        [[ $source_file -nt $target_file ]] && echo 1 || echo 0
    fi
}

compile_cob() {
    export COB_CC="$CC_CMD"
    export COB_CFLAGS="-I \"$COB_HOME/include\" -pipe -Wno-unused -fsigned-char -Wno-pointer-sign"
    export COB_LIBS="-L \"$COB_HOME/lib\" -lcob"
    export COB_CONFIG_DIR="$COB_HOME/config"
    export COB_COPY_DIR="$COB_HOME/copy"
    if [[ $DEBUG -eq 1 ]]; then
        ## we print the customized environment variables
        debug "\"$COBC_CMD"" --info | grep env:"
        eval "\"$COBC_CMD\" --info | grep env:"
    fi
    local cobc_opts="-std=$STANDARD -x -o \"$TARGET_FILE\""
    [[ $FORMAT = free ]] && cobc_opts="--free $cobc_opts"
    [[ $DEBUG -eq 1 ]] && cobc_opts="--debug --verbose $cobc_opts"

    local source_files=
    local n=0
    for f in $(find "$SOURCE_MAIN_DIR/" -type f -name "*.cbl" -o -name "*.cob" 2>/dev/null); do
        source_files="$source_files \"$f\""
        n=$((n + 1))
    done
    if [[ $n -eq 0 ]]; then
        warning "No COBOL source file found"
        return 1
    fi
    local s=; [[ $n -gt 1 ]] && s="s"
    local n_files="$n COBOL source file$s"
    if [[ $DEBUG -eq 1 ]]; then
        debug "\"$COBC_CMD\" $cobc_opts $source_files"
    elif [[ $VERBOSE -eq 1 ]]; then
        echo "Compile $n_files to directory \"${TARGET_DIR/$ROOT_DIR\//}\"" 1>&2
    fi
    eval "\"$COBC_CMD\" $cobc_opts $source_files"
    if [[ $? -ne 0 ]]; then
        error "Failed to compile $n_files to directory \"${TARGET_DIR/$ROOT_DIR\//}\""
        cleanup 1
    fi
}

mixed_path() {
    if [[ -x "$CYGPATH_CMD" ]]; then
        $CYGPATH_CMD -am $1
    elif [[ $(($mingw + $msys)) -gt 0 ]]; then
        echo $1 | sed 's|/|\\\\|g'
    else
        echo $1
    fi
}

run() {
    if [[ ! -f "$TARGET_FILE" ]]; then
        error "Executable \"${TARGET_FILE/$ROOT_DIR\//}\" not found"
        cleanup 1
    fi
    local saved_path="$PATH"
    export PATH="$COB_BIN_PATH:$PATH"

    if [[ $DEBUG -eq 1 ]]; then
        debug "$TARGET_FILE"
    elif [[ $VERBOSE -eq 1 ]]; then
        echo "Execute \"${TARGET_FILE/$ROOT_DIR\//}\"" 1>&2
    fi
    eval "$TARGET_FILE"
    if [[ $? -ne 0 ]]; then
        export PATH="$saved_path"
        error "Failed to execute \"${TARGET_FILE/$ROOT_DIR\//}\"" 1>&2
        cleanup 1
    fi
    export PATH="$saved_path"
}

##############################################################################
## Environment setup

BASENAME=$(basename "${BASH_SOURCE[0]}")

EXITCODE=0

ROOT_DIR="$(getHome)"

SOURCE_DIR="$ROOT_DIR/src"
SOURCE_MAIN_DIR="$SOURCE_DIR/main/cobol"
TARGET_DIR="$ROOT_DIR/target"

## We refrain from using `true` and `false` which are Bash commands
## (see https://man7.org/linux/man-pages/man1/false.1.html)
CLEAN=0
COMPILE=0
DEBUG=0
FORMAT=free
HELP=0
RUN=0
## option -std:<name>, name=default, cobol2014
STANDARD=cobol2014
TOOLSET=gnu
VERBOSE=0

cygwin=0
mingw=0
msys=0
darwin=0
case "$(uname -s)" in
    CYGWIN*) cygwin=1 ;;
    MINGW*)  mingw=1 ;;
    MSYS*)   msys=1 ;;
    Darwin*) darwin=1
esac
unset CYGPATH_CMD
PSEP=":"
if [[ $(($cygwin + $mingw + $msys)) -gt 0 ]]; then
    CYGPATH_CMD="$(which cygpath 2>/dev/null)"
    PSEP=";"
    [[ -n "$COB_HOME" ]] && COB_HOME="$(mixed_path $COB_HOME)"
    [[ -n "$GIT_HOME" ]] && GIT_HOME="$(mixed_path $GIT_HOME)"
    CC_CMD="$COB_HOME/mingw64/bin/gcc.exe"
    DIFF_CMD="$GIT_HOME/usr/bin/diff.exe"
    COB_BIN_PATH="$($CYGPATH_CMD -u $COB_HOME)/bin"
    TARGET_EXT=.exe
else
    CC_CMD="$(which gcc)"
    DIFF_CMD="$(which diff)"
    COB_BIN_PATH="$COB_HOME/bin"
    TARGET_EXT=
fi
if [[ ! -x "$COB_HOME/bin/cobc" ]]; then
    error "GnuCOBOL installation not found"
    cleanup 1
fi
COBC_CMD="$COB_HOME/bin/cobc"

PROJECT_NAME="$(basename $ROOT_DIR)"
PROJECT_URL="github.com/$USER/cobol-examples"
PROJECT_VERSION="1.0-SNAPSHOT"

TARGET_FILE="$TARGET_DIR/$PROJECT_NAME$TARGET_EXT"

args "$@"
[[ $EXITCODE -eq 0 ]] || cleanup 1

if [[ $FORMAT = fixed ]]; then
    if [[ -d "${SOURCE_MAIN_DIR}-fixed" ]]; then
        SOURCE_MAIN_DIR="${SOURCE_MAIN_DIR}-fixed"
    else
        FORMAT=fixed2
    fi
fi

##############################################################################
## Main

[[ $HELP -eq 1 ]] && help && cleanup

if [[ $CLEAN -eq 1 ]]; then
    clean || cleanup 1
fi
if [[ $COMPILE -eq 1 ]]; then
    compile || cleanup 1
fi
if [[ $RUN -eq 1 ]]; then
    run || cleanup 1
fi
cleanup
