.SUFFIXES:
EXE=gf_test

FC=gfortran
COMPILE.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c
MAKEMOD.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -fsyntax-only -c
LINK.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH)

SOURCES=cu_gf_deep.F90  cu_gf_driver.F90  cu_gf_driver_post.F90  cu_gf_driver_pre.F90  cu_gf_sh.F90  gf_test.F90  gf_utils.F90  machine.F90 mt19937.F90

default:
	$(MAKE) $(EXE)

$(EXE): $(subst .F90,.o,$(SOURCES))
	$(LINK.f08) -o $@ $+

%.o %.mod %.smod: %.F90
	$(COMPILE.f08) -o $*.o $<
	@touch $@

%.smod %.mod: %.F90
	$(MAKEMOD.f08) $<

.PHONY: clean
clean:
	rm -rf *.o *.mod *.smod $(EXE)

cu_gf_deep.o : machine.mod
cu_gf_driver.o : machine.mod cu_gf_sh.mod
cu_gf_driver_post.o : machine.mod
cu_gf_driver_pre.o : machine.mod
cu_gf_sh.o : machine.mod cu_gf_deep.mod
gf_utils.o : machine.mod
gf_test.o : machine.mod mt19937.mod gf_utils.mod cu_gf_driver.mod

