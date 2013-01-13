#-------------------------------------------------------------------------------
#  MAKEFILE
#-------------------------------------------------------------------------------  
#
#  CVS: $Revision: 1.14 $ $Date: 2012/04/03 07:51:21 $
#
#  (C) James Wookey
#  Department of Earth Sciences, University of Bristol
#  Wills Memorial Building, Queen's Road, Bristol, BR8 1RJ, UK
#  j.wookey@bristol.ac.uk
#
#-------------------------------------------------------------------------------
#
#   Makefile for building sacio90
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
# Uncomment to use Intel Compilers (v8+) 
#FC = ifort
#CC = icc
#CFLAGS = -O2
#FFLAGS = -O2 -assume byterecl
#-------------------------------------------------------------------------------
# Uncomment to use Gnu Compilers 
FC = /usr/local/bin/gfortran
CC = /usr/local/bin/gcc
CFLAGS = -O2
FFLAGS = -O2
#-------------------------------------------------------------------------------
# Uncomment to use g95
#FC = /opt/local/bin/g95
#CC = gcc
#CFLAGS = -O2
#FFLAGS = -O2 -fno-second-underscore

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
