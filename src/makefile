target = ../main
object = \
  variablesMod.o \
  ioUtilityMod.o \
  ellipticIMod.o \
  bfd_ringIMod.o \
  main.o

#FC = ifort
FC = gfortran

FFLAGS  = -O3
LDFLAGS =


.SUFFIXES: .o .f90

%.o: %.f90
	$(FC) -c $< $(FFLAGS) $(LDFLAGS)

%.mod: %.f90 %.o
	@:

$(target): $(object)
	$(FC) -o $@ $(object) $(FFLAGS) $(LDFLAGS)


.PHONY:clean all

clean:
	$(RM) $(target) $(object) *.mod

all: clean $(target)
