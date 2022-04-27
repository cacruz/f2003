.SUFFIXES: .f90 .F90

FC = gfortran
CC = gcc
CFLAGS = -O0 -g
FFLAGS = -O0 -g -fbacktrace
ifndef ARCH             # Architecture, e.g., Linux
  ARCH := $(shell uname -s)
endif
ifeq ($(ARCH), Linux)
  LDFLAGS = 
else
  LDFLAGS = -L/usr/lib -lSystem
endif

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -I.

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -I.
