.PHONY: default
default: all

MAKE_toolsDir ?= tools
MAKE_binDir   ?= bin
MAKE_objDir   ?= obj
MAKE_utilsDir ?= Makefiles

include $(MAKE_utilsDir)/Haskell.mk

vpath %.hs src

# Program
Main = $(call HASKELL_mkTarget,Main)

# Cabal dependencies

# Dependencies
$(Main): Main.hs

.PHONY: all
all: $(Main)

.PHONY: clean
clean: cleanall
