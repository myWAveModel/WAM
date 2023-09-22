#!/bin/bash
# Kompilierung von WAM mit ifort mit mpi
#ccccccccccccccccccccccccccccccccccccccc
#	cd in das Verzeichnis von wk.make
#	./wk.make [first]
export LC_ALL=C
module unload all
module load compilers/intel/2019.4.243
module load intelmpi/2019.4.243
module load netcdf
module load hdf5
export FC=$(nf-config --fc)
export FFLAGS="$FFLAGS $(nf-config --fflags)"
export LIBS=$(nf-config --flibs)

#if which mpiifort 2>/dev/null;then
#  export FC=mpiifort
#else
#  export FC=mpif90
#  fi
#echo export PRODADMDIR=$PWD>mk/.dirset
mkdir -p abs obj
id=$(echo $MPIHOME|cut -d/ -f4)_hzg
#ASISLIB=/storage/kochw/oa-wam-getm/oasis3-mct/compile.$id/lib
#ASISLIB=/storage/kochw/oasis3-mct/compile.$id/lib
#ASISLIB=/storage/kochw/${oa+oa-wam-getm/}oasis3-mct/compile.$id/lib
#ASISLIB=/gpfs/work/kochw/storage/oasis3-mct/compile.$id/lib
#ASISLIB=/gpfs/work/grayek/COUPLING_DUMMY/oasis3-mct_3/STRAND/lib
OASISLIB=/gpfs/work/kochw/oasis3-mct/compile.$id/lib
if [ -d $OASISLIB ];then
  export FFLAGS="$FFLAGS -I ${OASISLIB%/*}/build/lib/psmile.MPI1"
  export LIBS="$LIBS -L$OASISLIB -lpsmile.MPI1 -lmct -lmpeu -lscrip"
else
  OASISLIB=
  fi
#cv=$(echo $NETCDFHOME|sed 's,.*netcdf/\([34]\).*,\1,')
#if [ "$cv" == "3" ];then
#  export NETCDFSUF=
#else
#  export NETCDFSUF=f
#  fi
#echo $OASISMOD $NETCDFHOME
#set -xv
if [ "$1" == first ];then
  cd mk
  ./create_binaries
else
  if [ "$1" == off ];then
    (cd src/mod;ln -sf wam_oasis_inactive_module wam_oasis_module)
    shift
  else
    (cd src/mod;ln -sf wam_oasis_active_module wam_oasis_module)
    fi
  cd obj
#  make --debug=b ../abs/wam
  make -r --debug=b ${1-../abs/wam}
  fi
date
