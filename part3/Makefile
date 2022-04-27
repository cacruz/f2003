include ../config/common.mk
.PHONY: clean all

EXE = employee_test.x select_type.x

all:  $(EXE)

clean:
	rm -f *.[ox] *.mod

select_type.x: select_type.o
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

employee_test.x: employee_test.o employee_mod.o  \
	salaried_employee_mod.o  hourly_employee_mod.o  
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)


employee_test.o: salaried_employee_mod.o hourly_employee_mod.o
salaried_employee_mod.o: employee_mod.o
hourly_employee_mod.o: employee_mod.o

