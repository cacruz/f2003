
include ../config/common.mk
.PHONY: clean all

EXE = comm_line_arg.x f_calls_strncpy.x

all:  $(EXE)

clean:
	rm -f *.[ox] *.mod

comm_line_arg.x: comm_line_arg.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

f_calls_strncpy.x: f_calls_strncpy.o 
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

