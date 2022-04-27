
include ../config/common.mk
.PHONY: clean all

EXE = dtype.x encap.x complex.x complex_new.x complex_mod.x

all:  $(EXE)

clean:
	rm -f *.[ox] *.mod

dtype.x: dtype.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

encap.x: encap.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

complex.x: complex.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

complex_new.x: complex_new.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

complex_mod.x: complex_mod.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)
