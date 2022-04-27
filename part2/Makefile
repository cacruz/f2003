include ../config/common.mk
.PHONY: clean all

EXE = timer_mod.x date_mod.x vector_mod.x

all:  $(EXE)

clean:
	rm -f *.[ox] *.mod


timer_mod.x: timer_mod.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

date_mod.x: date_mod.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

vector_mod.x: vector_mod.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)
