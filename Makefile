# Makefile for WAM Cycle7
# created  27/02/2025
# modified 27/02/2025
#
#===============================================================================
# 1. Compiler Settings
#===============================================================================

#$(info !!! INFO: $(setENV) !!!)

ifdef setENV
$(info !!! INFO: Compilation for pre-loaded environment setENV=$(setENV) !!!)
$(info !!!       => Compilation starts without further variables/library setting !!!)
ifeq ($(findstring GCC, $(setENV)),GCC)
    MODOP=-J
else
    MODOP=-module 
endif


else ifneq ($(MAKECMDGOALS),clean)
$(info !!! WARNING: No environment specified !!!)
$(info !!!          Compilation based on the calling SHELL environment and !!!)
$(info !!!          Makefile-defined compiler variables and library paths  !!!)

#-------------------------------------------------------------------------------
# Fortran compiler & Basic FC arguments
#-------------------------------------------------------------------------------
#FC = ifort
FC = mpiifort
#FC = gfortran
#FC = mpifort

MODOP=-module 
#MODOP=-J

#-------------------------------------------------------------------------------
# Advanced Fortran compiler options
#-------------------------------------------------------------------------------
FFLAGS = -heap-arrays 64
FFLAGS+= -fp-model precise
#FFLAGS+= -O0
FFLAGS+= -O3
#FFLAGS+= -march=native
#FFLAGS+= -g -traceback -check all 

#-------------------------------------------------------------------------------
# System specific libraries
#-------------------------------------------------------------------------------
NCDFDIR=/project/opt/software/netcdf/4.9.2/intel_oneAPI
NCDFIN=-I${NCDFDIR}/include
NCDFLIB=-L${NCDFDIR}/lib
NCDFFLAGS=-lnetcdf -lnetcdff

LDOPT=${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}

#HDF5DIR=/sw/spack-levante/hdf5-1.12.1-kxfaux/include
#HDF5IN=-I${HDF5DIR}/include
#HDF5LIB=-L${HDF5LIB}/lib

#LDOPT+= ${HDF5IN} ${HDF5LIB} ${HDFFLAGS}

#OASISDIR=/home/g/g260237/Codes/oasis3-mct_forGCOAST_MR/oasis3-mct_LEVANTE
#OASISIN=-I${OASISDIR}/build/lib/psmile.MPI1
#OASISLIB=-L${OASISDIR}/lib
#OASISFLAGS=-lpsmile.MPI1 -lmct -lmpeu -lscrip

#FFLAGS+= ${OASISIN}
#LDOPT+= ${OASISLIB} ${OASISFLAGS}


endif


#===============================================================================
# 2. File & directory definitions
#===============================================================================

# Directories:
SRCDIR=./src
OBJDIR=./obj
EXEDIR=./bin

FFLAGS+=$(MODOP)$(OBJDIR)

# Objects for preproc
PREPROC_OBJS = \
$(OBJDIR)/wam_mpi_module.o \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_grid_module.o \
$(OBJDIR)/wam_timopt_module.o \
$(OBJDIR)/wam_fre_dir_module.o \
$(OBJDIR)/wam_jonswap_module.o \
$(OBJDIR)/wam_tables_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_special_module.o \
$(OBJDIR)/wam_output_set_up_module.o \
$(OBJDIR)/wam_interface_module.o \
$(OBJDIR)/wam_model_module.o \
$(OBJDIR)/wam_oasis_module.o \
$(OBJDIR)/wam_nest_module.o \
$(OBJDIR)/wam_mpi_comp_module.o \
\
$(OBJDIR)/preproc_module.o \
$(OBJDIR)/preproc_user_module.o \
\
$(OBJDIR)/preproc.o \
$(OBJDIR)/read_topography.o \
$(OBJDIR)/read_preproc_user.o

# Objects for wam
WAM_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_grid_module.o \
$(OBJDIR)/wam_timopt_module.o \
$(OBJDIR)/wam_fre_dir_module.o \
$(OBJDIR)/wam_jonswap_module.o \
$(OBJDIR)/wam_tables_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_mpi_module.o \
$(OBJDIR)/wam_special_module.o \
$(OBJDIR)/wam_output_set_up_module.o \
$(OBJDIR)/wam_nest_module.o \
$(OBJDIR)/wam_interface_module.o \
$(OBJDIR)/wam_mpi_comp_module.o \
$(OBJDIR)/wam_model_module.o \
$(OBJDIR)/wam_current_module.o \
$(OBJDIR)/wam_ice_module.o \
$(OBJDIR)/wam_wind_module.o \
$(OBJDIR)/wam_oasis_module.o \
$(OBJDIR)/wam_boundary_module.o \
$(OBJDIR)/wam_assi_set_up_module.o \
\
$(OBJDIR)/wam_swell_module.o \
$(OBJDIR)/wam_topo_module.o \
$(OBJDIR)/wam_propagation_module.o \
$(OBJDIR)/wam_radiation_module.o \
$(OBJDIR)/wam_flux_module.o \
$(OBJDIR)/wam_source_output_module.o \
$(OBJDIR)/wam_source_module.o \
$(OBJDIR)/wam_output_module.o \
$(OBJDIR)/preproc_module.o \
$(OBJDIR)/wam_coldstart_module.o \
$(OBJDIR)/wam_restart_module.o \
$(OBJDIR)/wam_initial_module.o \
$(OBJDIR)/wam_user_module.o \
$(OBJDIR)/wam_assi_module.o \
\
$(OBJDIR)/read_topo_input.o \
$(OBJDIR)/chief.o \
$(OBJDIR)/wavemdl.o \
$(OBJDIR)/initmdl.o \
$(OBJDIR)/read_wam_user.o \
$(OBJDIR)/print_wam_status.o \
$(OBJDIR)/read_wind_input.o  \
$(OBJDIR)/read_current_input.o \
$(OBJDIR)/wamodel.o \
$(OBJDIR)/read_boundary_input.o \
$(OBJDIR)/read_ice_input.o \
$(OBJDIR)/jafu.o \
$(OBJDIR)/readsat.o 

# Objects for pgrid
PGRID_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_print_module.o \
$(OBJDIR)/wam_print_user_module.o \
\
$(OBJDIR)/print_grid_file.o \
$(OBJDIR)/read_grid_file.o \
$(OBJDIR)/read_grid_user.o

# Objects for pspec
PSPEC_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_print_module.o \
$(OBJDIR)/wam_print_user_module.o \
\
$(OBJDIR)/print_spectra_file.o \
$(OBJDIR)/read_spectra_file.o \
$(OBJDIR)/read_spectra_user.o

# Objects for ptime
PTIME_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_print_module.o \
$(OBJDIR)/wam_print_user_module.o \
\
$(OBJDIR)/print_time.o \
$(OBJDIR)/read_time_user.o \
$(OBJDIR)/read_grid_file.o

# Objects for ptime_S
PTIMS_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_print_module.o \
$(OBJDIR)/wam_print_user_module.o \
\
$(OBJDIR)/print_time_S.o \
$(OBJDIR)/read_time_user_S.o \
$(OBJDIR)/read_grid_file.o

# Objects for pnetcdf (wam2netcdf)
PNCDF_OBJS = \
$(OBJDIR)/wam_mpi_module.o \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_grid_module.o \
$(OBJDIR)/wam_timopt_module.o \
$(OBJDIR)/wam_model_module.o \
$(OBJDIR)/wam_fre_dir_module.o \
$(OBJDIR)/wam_jonswap_module.o \
$(OBJDIR)/wam_tables_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_special_module.o \
$(OBJDIR)/wam_output_set_up_module.o \
$(OBJDIR)/wam_interface_module.o \
$(OBJDIR)/wam_nest_module.o \
$(OBJDIR)/wam_mpi_comp_module.o \
$(OBJDIR)/wam_current_module.o \
$(OBJDIR)/wam_ice_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_flux_module.o \
$(OBJDIR)/wam_topo_module.o \
$(OBJDIR)/wam_source_output_module.o \
$(OBJDIR)/wam_source_module.o \
$(OBJDIR)/wam_swell_module.o \
$(OBJDIR)/wam_propagation_module.o \
$(OBJDIR)/wam_radiation_module.o \
$(OBJDIR)/wam_output_module.o \
$(OBJDIR)/wam_print_module.o \
\
$(OBJDIR)/read_current_input.o \
$(OBJDIR)/read_ice_input.o \
$(OBJDIR)/read_topo_input.o \
$(OBJDIR)/jafu.o \
\
$(OBJDIR)/wam_netcdf_module.o \
$(OBJDIR)/make_netcdf.o

# Objects for psource
PSRC_OBJS = \
$(OBJDIR)/wam_file_module.o \
$(OBJDIR)/wam_coordinate_module.o \
$(OBJDIR)/wam_general_module.o \
$(OBJDIR)/wam_output_parameter_module.o \
$(OBJDIR)/wam_oasis_module.o \
\
$(OBJDIR)/wam_print_module.o \
$(OBJDIR)/wam_print_user_module.o \
\
$(OBJDIR)/print_scr_file.o \
$(OBJDIR)/read_scr_file.o \
$(OBJDIR)/read_scr_user.o


#===============================================================================
# 3. Make rules
#===============================================================================

#-------------------------------------------------------------------------------
# Make programs
#-------------------------------------------------------------------------------

all: preproc wam pgrid pspec ptime ptime_S pnetcdf psource

preproc : directories $(PREPROC_OBJS)
	$(FC) $(FFLAGS) $(PREPROC_OBJS) -o $(EXEDIR)/$@ $(LDOPT)

wam     : directories $(WAM_OBJS)
	$(FC) $(FFLAGS) $(WAM_OBJS)     -o $(EXEDIR)/$@ $(LDOPT)

pgrid   : directories $(PGRID_OBJS)
	$(FC) $(FFLAGS) $(PGRID_OBJS)   -o $(EXEDIR)/$@ $(LDOPT)

pspec   : directories $(PSPEC_OBJS) 
	$(FC) $(FFLAGS) $(PSPEC_OBJS)   -o $(EXEDIR)/$@ $(LDOPT)

ptime   : directories $(PTIME_OBJS)
	$(FC) $(FFLAGS) $(PTIME_OBJS)   -o $(EXEDIR)/$@ $(LDOPT)

ptime_S : directories $(PTIMS_OBJS)
	$(FC) $(FFLAGS) $(PTIMS_OBJS)   -o $(EXEDIR)/$@ $(LDOPT)

pnetcdf : directories $(PNCDF_OBJS)
	$(FC) $(FFLAGS) $(PNCDF_OBJS)   -o $(EXEDIR)/$@ $(LDOPT)

psource : directories $(PSRC_OBJS)
	$(FC) $(FFLAGS) $(PSRC_OBJS)    -o $(EXEDIR)/$@ $(LDOPT)

#-------------------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------------------
$(OBJDIR)/%.o : $(SRCDIR)/mod/%.f90
	$(FC) $(FFLAGS) -c $< $(LDOPT) -o $@
$(OBJDIR)/%.o : $(SRCDIR)/chief/%.f90
	$(FC) $(FFLAGS) -c $< $(LDOPT) -o $@
$(OBJDIR)/%.o : $(SRCDIR)/preproc/%.f90
	$(FC) $(FFLAGS) -c $< $(LDOPT) -o $@
$(OBJDIR)/%.o : $(SRCDIR)/print/%.f90
	$(FC) $(FFLAGS) -c $< $(LDOPT) -o $@

#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------

.PHONY: directories clean

directories:
	if [ ! -d $(OBJDIR) ]; then mkdir -p $(OBJDIR); fi
	if [ ! -d $(EXEDIR) ]; then mkdir -p $(EXEDIR); fi

clean:
	rm $(EXEDIR)/* $(OBJDIR)/*

