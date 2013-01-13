#-------------------------------------------------------------------------------
#  MAKEFILE
#-------------------------------------------------------------------------------  
#
#  (C) James Wookey
#  Department of Earth Sciences, University of Bristol
#  Wills Memorial Building, Queen's Road, Bristol, BR8 1RJ, UK
#  j.wookey@bristol.ac.uk
#
#-------------------------------------------------------------------------------
#
#   Makefile for building sacio90 test program.
#
#-------------------------------------------------------------------------------
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
#-------------------------------------------------------------------------------

SACIO90 = sacio90.o

#-------------------------------------------------------------------------------
# Gnu Compilers 
FC = /usr/local/bin/gfortran
CC = /usr/local/bin/gcc
CFLAGS = -O2
FFLAGS = -O2

#-------------------------------------------------------------------------------
# Uncomment if your version sacio does not have the RSACH subroutine.  
#SACIO90_FLAGS += -DNORSACH
#-------------------------------------------------------------------------------

all:test_sacio90

test_sacio90: ${SACIO90} test_sacio90.o /usr/local/sac/lib/sacio.a
	$(FC) -o test_sacio90 ${SACIO90} test_sacio90.o /usr/local/sac/lib/sacio.a

sacio90.o: sacio90.F90
	$(FC) $(FFLAGS) $(SACIO90_FLAGS) -c sacio90.F90 
	
clean:
	rm -f *.o *.mod

# COMPILE INSTRUCTIONS
%.o: %.F90
	$(FC) $(FFLAGS) -c $*.F90 
%.o: %.f90
	$(FC) $(FFLAGS) -c $*.f90 
%.o: %.c
	$(CC) $(CFLAGS) -c $*.c
