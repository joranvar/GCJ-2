.PHONY: default
default: all

MAKE_toolsDir ?= tools
MAKE_binDir   ?= bin
MAKE_objDir   ?= obj
MAKE_utilsDir ?= Makefiles

include $(MAKE_utilsDir)/Haskell.mk

vpath %.hs src

# Targets
Main = $(call HASKELL_mkTarget,Main)
Test = $(call HASKELL_mkTarget,Test)
Zip  = $(MAKE_binDir)/source.zip

# Cabal dependencies
HUnit = $(call HASKELL_mkCabalDep,HUnit)
QuickCheck = $(call HASKELL_mkCabalDep,QuickCheck)

# Groups
GCJ = GCJ.hs
Solution = GCJ/Y2016/Q/A.hs $(GCJ)

# Dependencies
$(Main): Main.hs $(Solution)
$(Test): Test.hs $(HUnit) $(Solution) $(QuickCheck)

.PHONY: test
test: $(Test)
	$(Test)

.PHONY: all
all: $(Main) $(Zip)

$(Zip): Main.hs $(Solution)
	zip -u1o $@ $^

.PHONY: clean
clean: cleanall
	-rm -f $(Zip)
