.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
srcdir = ./
objdir = .obj
bindir = ./
libdir = ./

# Command-line options at make call
debug    ?= 0

## COMPILER CONFIGURATION ##
# (should be loaded from config directory)

FC = gfortran

NC_CROOT=/opt/homebrew/Cellar/netcdf/4.9.2_2
NC_FROOT=/opt/homebrew/Cellar/netcdf-fortran/4.6.1_1
INC_NC  = -I${NC_FROOT}/include
LIB_NC  = -L${NC_FROOT}/lib -lnetcdff -L${NC_CROOT}/lib -lnetcdf

FFLAGS  = -ffree-line-length-none -I$(objdir) -J$(objdir)

LFLAGS  = $(LIB_NC)

DFLAGS_NODEBUG = -O2
DFLAGS_DEBUG   = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
DFLAGS_PROFILE = -O2 -pg

# Determine whether to use normal flags or debugging flags
DFLAGS   = $(DFLAGS_NODEBUG)
ifeq ($(debug), 1)
	DFLAGS   = $(DFLAGS_DEBUG)
endif

# Debugging flags with profiling output enabled
ifeq ($(debug), 2)
	DFLAGS   = $(DFLAGS_PROFILE)
endif

###############################################
##
## List of rules and source files
##
###############################################

$(objdir)/ismip6.o: $(libdir)/ismip6.f90 $(objdir)/nml.o \
							$(objdir)/ncio.o $(objdir)/varslice.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/ncio.o: $(libdir)/ncio.f90
	$(FC) $(DFLAGS) $(FFLAGS) $(INC_NC) -c -o $@ $<

$(objdir)/nml.o: $(libdir)/nml.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/varslice.o: $(libdir)/varslice.f90 $(objdir)/nml.o $(objdir)/ncio.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

ismip6_libs = 			$(objdir)/ismip6.o \
					    $(objdir)/ncio.o \
					    $(objdir)/nml.o \
			 		    $(objdir)/varslice.o


###############################################
##
## Compilation of complete programs
##
###############################################

test : $(ismip6_libs)
		$(FC) $(DFLAGS) $(FFLAGS) $(INC_LIS) $(INC_YELMO) -o $(bindir)/test.x test.f90 \
			$(LFLAGS) $(ismip6_libs) 
		@echo " "
		@echo "    test.x is ready."
		@echo " "

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles test.x, for testing ismip6 routines."
	@echo " make clean      : cleans object files."
	@echo ""

clean:
	rm -f $(bindir)/*.x
	rm -f  *.x gmon.out $(objdir)/*.o $(objdir)/*.mod $(objdir)/*.a $(objdir)/*.so
	rm -rf *.x.dSYM 

