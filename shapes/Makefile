include ../config/common.mk
.PHONY: clean all

all: test_shapes.x

SRCS := $(wildcard *.F90)
OALL := $(addsuffix .o, $(basename $(SRCS)))
OBJS := $(filter-out test_shapes.o,$(OALL))

clean:
	rm -f *.mod *.o *.x

test_shapes.x: $(OALL)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test_shapes.o:  $(OBJS)
polygon_mod.o:  shapes_mod.o 
ellipse_mod.o:  shapes_mod.o
rectangle_mod.o:  polygon_mod.o
circle_mod.o:  ellipse_mod.o
square_mod.o:  rectangle_mod.o
triangle_mod.o:  polygon_mod.o
