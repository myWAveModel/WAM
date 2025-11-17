#!/bin/bash

#===============================================================================
# Environment settings
#===============================================================================
if [ "$1" == strand ]; then
        #### STRAND - Intel2020 ####
        module purge
        module load compilers/intel/2020.1.217
        module load intelmpi/2020.1.217
        module load netcdf

        export FC=mpiifort
        export FFLAGS="-heap-arrays 64 -fp-model precise -O3"
        #export FFLAGS="-heap-arrays 64 -fp-model precise -O0 -g -fsanitize=address -fno-omit-frame-pointer -traceback -check bounds"
        NCDFDIR=/project/opt/software/netcdf/4.7.0/intel
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB=-L${NCDFDIR}/lib
        NCDFFLAGS='-lnetcdf -lnetcdff'
        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"


elif [ "$1" == strand-oneAPI ]; then
        #### STRAND - oneAPI ####
        module purge
        module load compilers/intel/oneAPI/2021.2.0
        module load netcdf

        export FC=mpiifort
        export FFLAGS='-heap-arrays 64 -fp-model precise -O3'
        #export FFLAGS="-heap-arrays 64 -fp-model precise -g -traceback -check bounds"
        NCDFDIR=/project/opt/software/netcdf/4.9.2/intel_oneAPI
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB=-L${NCDFDIR}/lib
        NCDFFLAGS='-lnetcdf -lnetcdff'
        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"


elif [ "$1" == strand-GCC ]; then
        #### STRAND - GCC ####
        module purge
        module load compilers/gnu/11.1.1
        module load netcdf/4.7.0

        export FC=mpifort
        export FFLAGS=' ' #'-O3 -march=native'
        NCDFDIR=/project/opt/software/netcdf/4.7.0/gcc_111
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB=-L${NCDFDIR}/lib
        NCDFFLAGS='-lnetcdf -lnetcdff'
        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"


elif [ "$1" == levante ]; then
        #### LEVANTE - OpenMPI (wk.make.LEVANTE) ####
        module purge
        module load openmpi/4.1.2-intel-2021.5.0
        module load netcdf-c/4.8.1-openmpi-4.1.2-intel-2021.5.0
        module load netcdf-fortran/4.5.3-openmpi-4.1.2-intel-2021.5.0

        export FC=mpifort
        export FFLAGS="-heap-arrays 64 -fp-model precise" #"-heap-arrays 64 -g -traceback -check bounds"
        #export MPIIN="-I/sw/spack-levante/openmpi-4.1.2-yfwe6t/include"
        NCDFDIR=/sw/spack-levante/netcdf-fortran-4.5.3-k6xq5g
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB="-L${NCDFDIR}/lib -Wl,-rpath,${NCDFDIR}/lib"
        NCDFFLAGS=-lnetcdff
        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"


elif [ "$1" == levante-oneAPI ]; then
        #### LEVANTE - oneAPI (NAAM) ####
        module purge
        module load intel-oneapi-compilers/2022.0.1-gcc-11.2.0
        module load intel-oneapi-mpi/2021.5.0-intel-2021.5.0
        module load netcdf-c/4.8.1-intel-oneapi-mpi-2021.5.0-intel-2021.5.0
        module load netcdf-fortran/4.5.3-intel-oneapi-mpi-2021.5.0-intel-2021.5.0

        export FC=mpiifort
        export FFLAGS="-heap-arrays 64 -fp-model precise" #"-heap-arrays 64 -g -traceback -check bounds"
        NCDFDIR=/sw/spack-levante/netcdf-fortran-4.5.3-r5r3ev
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB="-L${NCDFDIR}/lib -Wl,-rpath,${NCDFDIR}/lib"
        NCDFFLAGS=-lnetcdff
        #HDF5DIR=/sw/spack-levante/hdf5-1.12.1-jmeuy3
        #HDF5IN=-I${HDF5DIR}/include
        #HDF5LIB=-L${HDF5DIR}/lib

        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"
        #export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS} ${HDF5IN} ${HDF5LIB} ${HDFFLAGS}"


elif [ "$1" == levante-GCC ]; then
        #### LEVANTE - GCC ####
        module purge
        module load openmpi/4.1.2-gcc-11.2.0
        module load netcdf-c/4.8.1-openmpi-4.1.2-gcc-11.2.0
        module load netcdf-fortran/4.5.3-openmpi-4.1.2-gcc-11.2.0

        export FC=mpifort
        export FFLAGS=" " #"-march=native" #"-O3"
        NCDFDIR=/sw/spack-levante/netcdf-fortran-4.5.3-jlxcfz
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB="-L${NCDFDIR}/lib -Wl,-rpath,${NCDFDIR}/lib"
        NCDFFLAGS=-lnetcdff
        #HDF5DIR=/sw/spack-levante/hdf5-1.12.1-kxfaux/include"
        #HDF5IN=-I${HDF5DIR}/include
        #HDF5LIB=-L${HDF5DIR}/lib

        export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS}"
        #export LDOPT="${NCDFIN} ${NCDFLIB} ${NCDFFLAGS} ${HDF5IN} ${HDF5LIB} ${HDFFLAGS}"


elif [ "$1" == oman_nw ]; then
        #### OMAN - oneAPI ####
        source /opt/intel/oneapi/compiler/2022.2.0/env/vars.sh
        set oman_nw

        MPIFORT="/opt/openmpi/4.1.4/intel/bin"
        export LD_LIBRARY_PATH=/home01/netcdf/4.8.1/intel-2021.3.0-1/lib64:/home01/netcdf-fortran/4.5.2/intel-2021.3.0/lib64:/opt/intel/oneapi/compiler/2021.3.0/linux/compiler/lib/intel64_lin:/usr/lib64:${LD_LIBRARY_PATH}
        export PATH=/home01/netcdf/4.8.1/intel-2021.3.0-1/lib64:/home01/netcdf-fortran/4.5.2/intel-2021.3.0/lib64:/opt/intel/oneapi/compiler/2021.3.0/linux/compiler/lib/intel64_lin:/usr/lib64:${PATH}

        export FC=${MPIFORT}/mpifort
        export FFLAGS='-heap-arrays 64 -fp-model precise -O3'
        OMPIDIR='/opt/openmpi/4.1.4/intel'
        OMPIIN=-I${OMPIDIR}/include
        OMPILIB=-L${OMPIDIR}/lib
        NCDFDIR='/home01/netcdf-fortran/4.5.2/intel-2021.3.0'
        NCDFIN=-I${NCDFDIR}/include
        NCDFLIB=-L${NCDFDIR}/lib64
        NCDFDIR2='/home01/netcdf/4.8.1/intel-2021.3.0-1'
        NCDFIN2=-I${NCDFDIR2}/include
        NCDFLIB2=-L${NCDFDIR2}/lib64
        NCDFDIR3='/usr'
        NCDFIN3=-I${NCDFDIR3}/include
        NCDFLIB3=-L${NCDFDIR3}/lib64
        NCDFFLAGS='-lnetcdf -lnetcdff'
#         export LDOPT="${OMPIIN} ${OMPILIB} ${NCDFIN} ${NCDFLIB} ${NCDFFLAGS} "
        export LDOPT="${OMPIIN} ${OMPILIB} ${NCDFIN} ${NCDFLIB} ${NCDFIN2} ${NCDFLIB2} ${NCDFIN3} ${NCDFLIB3} ${NCDFFLAGS} "


elif [ "$1" == clean ]; then
        make clean
        exit
else
        echo '!!! ERROR: No known environment specified !!! '
        exit
fi


#===============================================================================
# OASIS settings
#===============================================================================
if [ "$2" == oasis ]; then
	# DEFINE THE PATH TO YOUR OASIS LIBRARY HERE !!
        OASISDIR=/home/g/g260237/Codes/oasis3-mct_forGCOAST_MR/oasis3-mct_LEVANTE
        
	OASISIN=-I${OASISDIR}/build/lib/psmile.MPI1
        OASISLIB=-L${OASISDIR}/lib
        OASISFLAGS="-lpsmile.MPI1 -lmct -lmpeu -lscrip"

        export FFLAGS="${FFLAGS} ${OASISIN}"
        export LDOPT="${LDOPT} ${OASISLIB} ${OASISFLAGS}"
fi


#===============================================================================
# A piori choice of Input Data Formats 
#===============================================================================
(cd src/chief
 #-> 'SWAMP' Test case
 ln -sf read_wind_input_arno.f90     read_wind_input.f90
 ln -sf read_boundary_input_arno.f90 read_boundary_input.f90
 ln -sf read_topo_input_arno.f90     read_topo_input.f90
 ln -sf read_current_input_arno.f90  read_current_input.f90
 ln -sf read_ice_input_arno.f90      read_ice_input.f90
 #-> 'NOSFOS', 'SNS', 'GCOAST-NSBS' setups
 #ln -sf read_wind_input_DWD.f90                  read_wind_input.f90
 #ln -sf read_boundary_input.f90_bound_from_4.5.3 read_boundary_input.f90
 #ln -sf read_topo_input_getm.f90                 read_topo_input.f90
 #ln -sf read_current_input_getm.f90              read_current_input.f90
 #-> Other options
 #ln -sf read_wind_input_netcdf_start_after00.f90 read_wind_input.f90
)


#===============================================================================
# Lets make it! 
#===============================================================================
module list
echo '==== Compilation Started ==='
if [ "$2" == oasis ]; then
	rm ./obj/wam_oasis_module.*
	(cd ./src/mod; ln -sf wam_oasis_active_module.f90 wam_oasis_module.f90)
        echo make wam "setENV=$1"
        #make wam "setENV=$1"
        make --debug=b wam "setENV=$1"

elif [ -n "$2" ]; then
        (cd ./src/mod; ln -sf wam_oasis_inactive_module.f90 wam_oasis_module.f90)
        echo make $2 "setENV=$1"
        make $2 "setENV=$1"

else
        (cd ./src/mod; ln -sf wam_oasis_inactive_module.f90 wam_oasis_module.f90)
        echo make all "setENV=$1"
        make all "setENV=$1"
fi

