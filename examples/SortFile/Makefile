##
## Copyright (c) 2018-2025 Stéphane Micheloud
##
## Licensed under the MIT License.
##
##############################################################################

ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

include ../Makefile.inc

PREFIX = $(subst \,/,$(COB_HOME))
PROJECT_NAME = $(shell basename $(ROOT_DIR))

SOURCE_DIR = src
TARGET_DIR = target

##############################################################################
## main rules

FORMAT = fixed
## option -std:<name>, name=default, cobol2014
STANDARD = cobol2014

SOURCES = $(shell $(FIND) "$(SOURCE_DIR)/main/cobol" -type f -name "*.cbl")
TARGET  = $(TARGET_DIR)/$(PROJECT_NAME)$(TARGET_EXT)

COBC_OPTS = -std=$(STANDARD) -x -o "$(TARGET)" -free

all: build

build: $(TARGET)

$(TARGET): $(SOURCES)
	[ -d "$(TARGET_DIR)" ] || "$(MKDIR)" -p "$(TARGET_DIR)"
	COB_CC="$(CC)" \
	COB_CFLAGS="-I \"$(PREFIX)/include\" -I \"$(MSYS_HOME)/mingw64/include\" -pipe -Wno-unused -fsigned-char -Wno-pointer-sign" \
	COB_LIBS="-L \"$(PREFIX)/lib\" -lcob" \
	COB_CONFIG_DIR="$(PREFIX)/config" \
	COB_COPY_DIR="$(PREFIX)/copy" \
	"$(COBC)" $(COBC_OPTS) $(SOURCES)

clean:
	"$(RM)" -rf "$(TARGET_DIR)"

run: build
	@if [ -f "$(TARGET)" ]; then \
	    PATH="$(COB_BIN_DIR):$(PATH)" && \
	    "$(TARGET)"; \
	else \
	    $(ECHO) "Target not found" 1>&2; \
	fi

help:
	@$(ECHO) "Usage: make all|build|clean|help|run"
	@$(ECHO) ""
	@$(ECHO) "  Subcommands:"
	@$(ECHO) "    all    alias for build"
	@$(ECHO) "    build  compile COBOL source files"
	@$(ECHO) "    clean  delete generated files"
	@$(ECHO) "    help   print this help message"
	@$(ECHO) "    run    execute main program \"$(PROJECT_NAME)$(TARGET_EXT)\""

##############################################################################
## test rules

##############################################################################
## phony

.PHONY: all build clean help run

.SUFFIXES:
.SUFFIXES: .cbl .cob .cpy .exe
