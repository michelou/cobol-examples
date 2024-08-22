ifndef COB_HOME
$(error Variable COB_HOME is not set)
endif
ifndef COBJ_HOME
$(error Variable COBJ_HOME is not set)
endif
ifndef MSYS_HOME
$(error Variable MSYS_HOME is not set)
endif

ifeq ($(OS),Windows_NT)
ifndef GIT_HOME
$(error Variable GIT_HOME is not set)
endif
PSEP       = ;
## $(MSYS_HOME)\usr\bin\{cp.exe, make.exe, wget.exe}
CP         = /usr/bin/cp.exe
CYGPATH    = /usr/bin/cygpath.exe
ECHO       = /usr/bin/echo.exe
FIND       = /usr/bin/find.exe
MKDIR      = /usr/bin/mkdir.exe
RM         = /usr/bin/rm.exe
TOUCH      = /usr/bin/touch.exe
TARGET_EXT = .exe
COB_BIN_DIR = $(shell cygpath -u $(COB_HOME)/bin)
CC         = $(subst \,/,$(COB_HOME))/mingw64/bin/gcc.exe
COBC       = $(subst \,/,$(COB_HOME))/bin/cobc.exe
COBJ       = $(subst \,/,$(COBJ_HOME))/bin/cobj.exe
else ifeq ($(OS),cygwin)
CP         = cp
CYGPATH    = cygpath
ECHO       = echo
FIND       = find
MKDIR      = mkdir
RM         = rm
TOUCH      = touch
TARGET_EXT = .exe
COB_BIN_DIR = $(shell cygpath -u $(COB_HOME)/bin)
CC         = $(subst \,/,$(COB_HOME))/mingw64/bin/gcc.exe
COBC       = $(subst \,/,$(COB_HOME))/bin/cobc.exe
COBJ       = $(subst \,/,$(COBJ_HOME))/bin/cobj.exe
else
PSEP       = :
CP         = cp
CYGPATH    =
ECHO       = echo
FIND       = find
MKDIR      = mkdir
RM         = rm
TOUCH      = touch
TARGET_EXT =
CC         = gcc
COBC       = cobc
COBJ       = cobj
endif

