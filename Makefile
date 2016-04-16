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
Digits = $(call HASKELL_mkCabalDep,digits)
Parallel = $(call HASKELL_mkCabalDep,parallel)
Split = $(call HASKELL_mkCabalDep,split)

# Groups
GCJ = GCJ.hs
Solution = GCJ/Y2016/1A/B.hs $(GCJ) $(Digits) $(Parallel) $(Split)

# Dependencies
$(Main): Main.hs $(Solution)
$(Test): Test.hs $(HUnit) $(Solution) $(QuickCheck)

.PHONY: test
test: $(Test)
	$(Test) +RTS -N

.PHONY: all
all: $(Main) $(Zip)

$(Zip): Main.hs $(Solution)
	zip -u1o $@ $^

.PHONY: clean
clean: cleanall
	-rm -f $(Zip)
