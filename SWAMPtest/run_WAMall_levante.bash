#!/bin/bash
#
#SBATCH --job-name=WAMnested         # Specify job name
#SBATCH --account=TOBESET            # Charge resources on this project account
#SBATCH --partition=compute          # Specify partition name
#SBATCH --ntasks=128                 # Specify number of parallel tasks
#SBATCH --cpus-per-task=1            # Specify number of CPUs per task: KEEP AT 1 !!!
#SBATCH --time=00:30:00              # Set a limit on the total run time
#SBATCH --output=WAMnested_o%j.log
#SBATCH --error=WAMnested_e%j.log
#


# ===================================================================
#### LEVANTE - OpenMPI-intel ####
module purge
module load openmpi/4.1.2-intel-2021.5.0
module load netcdf-c/4.8.1-openmpi-4.1.2-intel-2021.5.0
module load netcdf-fortran/4.5.3-openmpi-4.1.2-intel-2021.5.0

# - Recommended minimal environmental setting by DKRZ
export OMPI_MCA_osc="ucx"
export OMPI_MCA_pml="ucx"
export OMPI_MCA_btl="self"
export UCX_HANDLE_ERRORS="bt"
export OMPI_MCA_pml_ucx_opal_mem_hooks=1
# - Further recommended optimization settings by DKRZ
export OMPI_MCA_io="romio321"          # basic optimisation of I/O
export UCX_TLS="shm,rc_mlx5,rc_x,self" # for jobs using LESS than 150 nodes
#export UCX_TLS="shm,dc_mlx5,dc_x,self" # for jobs using MORE than 150 nodes
export UCX_UNIFIED_MODE="y"            # JUST for homogeneous jobs on CPUs, do not use for GPU nodes

# ===================================================================
#### LEVANTE - OpenMPI-GCC ####
#module purge
#module load openmpi/4.1.2-gcc-11.2.0
#module load netcdf-c/4.8.1-openmpi-4.1.2-gcc-11.2.0
#module load netcdf-fortran/4.5.3-openmpi-4.1.2-gcc-11.2.0
#
## - Recommended minimal environmental setting by DKRZ
#export OMPI_MCA_osc="ucx"
#export OMPI_MCA_pml="ucx"
#export OMPI_MCA_btl="self"
#export UCX_HANDLE_ERRORS="bt"
#export OMPI_MCA_pml_ucx_opal_mem_hooks=1
## - Further recommended optimization settings by DKRZ
#export OMPI_MCA_io="romio321"          # basic optimisation of I/O
#export UCX_TLS="shm,rc_mlx5,rc_x,self" # for jobs using LESS than 150 nodes
##export UCX_TLS="shm,dc_mlx5,dc_x,self" # for jobs using MORE than 150 nodes
#export UCX_UNIFIED_MODE="y"            # JUST for homogeneous jobs on CPUs, do not use for GPU nodes

# ===================================================================
#### LEVANTE - oneAPI ####
#module purge
#module load intel-oneapi-compilers/2022.0.1-gcc-11.2.0
#module load intel-oneapi-mpi/2021.5.0-intel-2021.5.0
#module load netcdf-c/4.8.1-intel-oneapi-mpi-2021.5.0-intel-2021.5.0
#module load netcdf-fortran/4.5.3-intel-oneapi-mpi-2021.5.0-intel-2021.5.0
#
#export I_MPI_PMI=pmi
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so
##export I_MPI_PMI=pmi2
##export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi2.so

# ===================================================================

# limit stacksize ... adjust to your programs need
# and core file size
ulimit -s unlimited
ulimit -c 0


WAMDIR='/home/X/USER/PATH/TO/WAM_Cycle7'

RUNDIR=.
INDIR=${RUNDIR}/input
OUTDIR=${RUNDIR}/output
GRDDIR=${RUNDIR}/grid
STOREDIR='/work/ACCOUNT/USER/PATH/TO/STOREDIR'

nproc=128
preproc='y'
srcout='y'
nofnest=2
#
if [ ! -d ${INDIR}/wam ] || [ ! -d ${INDIR}/config ]; then
    echo '!!!      ERROR: Input directories/files missing      !!!'
    echo '!!! Input data and configuration files expected in:  !!! '
    echo '!!! '${INDIR}'/wam and '${INDIR}'/config !!!'
    echo '!!! -------------------JOB ABORTS------------------- !!!'
    exit
fi
if [ ! -d ${GRDDIR} ]; then
    mkdir -p ${GRDDIR}
fi
if [ ! -d ${OUTDIR}/coarse/data ]; then
    mkdir -p ${OUTDIR}/coarse/data
fi
if [ $nofnest != 0 ]; then
    for ((i=1;$nofnest>=i;i++)); do
      if [ ! -d ${OUTDIR}/nest${i}/data ]; then
          mkdir -p ${OUTDIR}/nest${i}/data
      fi
    done
fi
#
set +k
cd ${RUNDIR}
#
# ===================================================================
# 1) ==>  WAM pre-processing preproc
# ===================================================================
echo '=== 1) WAM pre-processing preproc ==='
#
if [ $preproc = 'y' ]; then
    cp -ra ${WAMDIR}/bin/preproc preproc.exe
    # Main Grid
    cp -ra ${INDIR}/config/Preproc_User .
    srun -n 1 ./preproc.exe
    #srun -n 1 --mpi=pmi2 ./preproc.exe
    mv Preproc_Prot ${OUTDIR}/coarse/preproc_prot_coarse.log
    # Nested Grids
    if [ $nofnest != 0 ]; then
        echo '    --> coarse'
        for ((i=1;$nofnest>=i;i++)); do
            cp -ra ${INDIR}/config/Preproc_User_N${i} ./Preproc_User
            srun -n 1 ./preproc.exe
            #srun -n 1 --mpi=pmi2 ./preproc.exe
            mv Preproc_Prot ${OUTDIR}/nest${i}/preproc_prot_nest${i}.log
            echo '    --> nest '$i
        done
    fi
    #mv Grid_info_*_GRID ${GRDDIR}/
    rm Preproc_User
#
    echo '    --> DONE.'
#
else
    echo '    --> pre-processing switched OFF.'
fi
echo ' '
#
# ===================================================================
# 2) ==>  WAM model run
# ===================================================================
echo '=== 2) WAM model run ==='
#
cp -ra ${WAMDIR}/bin/wam wam.exe
# Main Grid
cp -ra ${INDIR}/config/WAM_User .
srun -n $nproc ./wam.exe
#srun -n $nproc --mpi=pmi2 ./wam.exe
if [ -f logfile.0 ]; then
    cp -ra logfile.0 ${OUTDIR}/coarse/wam_prot.log
    rm logfile.*
elif [ -f WAMLOGS/logfile.0 ]; then
    cp -ra WAMLOGS/logfile.0 ${OUTDIR}/coarse/wam_prot.log
    rm WAMLOGS/logfile.*
else
    cp -ra WAM_Prot ${OUTDIR}/coarse/wam_prot.log
    rm WAM_Prot
fi
mv BLS* MAP* OUT* C0* CB* SCR* ${OUTDIR}/coarse/data/
# Nested Grids
if [ $nofnest != 0 ]; then
    echo '    --> coarse'
    for ((i=1;$nofnest>=i;i++)); do
        cp -ra ${INDIR}/config/WAM_User_N${i} ./WAM_User
        srun -n $nproc ./wam.exe
        #srun -n $nproc --mpi=pmi2 ./wam.exe
        if [ -f logfile.0 ]; then
            cp -ra logfile.0 ${OUTDIR}/nest${i}/wam_prot.log
            rm logfile.*
        elif [ -f WAMLOGS/logfile.0 ]; then
            cp -ra WAMLOGS/logfile.0 ${OUTDIR}/nest${i}/wam_prot.log
            rm WAMLOGS/logfile.*
        else
            cp -ra WAM_Prot ${OUTDIR}/nest${i}/wam_prot.log
            rm WAM_Prot
        fi
        mv BLS* MAP* OUT* C0* CB* SCR* ${OUTDIR}/nest${i}/data/
        echo '    --> nest '$i
    done
fi
#
#
rm WAM_User
echo '    --> DONE.'
#
# ===================================================================
# 3i) ==>  WAM post-processing pnetcdf
# ===================================================================
echo '=== 3) WAM post-processing ==='
#
set -k
#
echo '--- i) NetCDF conversion ---'
#
cp -ra ${WAMDIR}/bin/pnetcdf pnetcdf.exe
# Main Grid
cp -ra ${INDIR}/config/NETCDF_User .
srun -n 1 ./pnetcdf.exe
#srun -n 1 --mpi=pmi2 ./pnetcdf.exe
mv pnetcdf_prot ${OUTDIR}/coarse/pnetcdf_prot.log
mv WAVE* ${OUTDIR}/coarse/data/
# Nested Grids
if [ $nofnest != 0 ]; then
    echo '    --> coarse'
    for ((i=1;$nofnest>=i;i++)); do
        cp -ra ${INDIR}/config/NETCDF_User_N${i} NETCDF_User
        srun -n 1 ./pnetcdf.exe
        #srun -n 1 --mpi=pmi2 ./pnetcdf.exe
        mv pnetcdf_prot ${OUTDIR}/nest${i}/pnetcdf_prot.log
        mv WAVE* ${OUTDIR}/nest${i}/data/
        echo '    --> nest '$i
    done
fi
rm NETCDF_User
#
echo '    --> DONE.'
#
# ===================================================================
# 3ii) ==>  WAM post-processing ptime
# ===================================================================
echo '--- ii) PTIME ---'
#
cp -ra ${WAMDIR}/bin/ptime ptime.exe
cp -ra ${INDIR}/config/Time_User .
#
./ptime.exe
mv Time_Prot ${OUTDIR}/coarse/ptime_prot.log
rm Time_User
#
echo '    --> DONE.'
#
# ===================================================================
# 3iii) ==>  WAM post-processing pgrid
# ===================================================================
echo '--- iii) PGRID ---'
#
cp -ra ${WAMDIR}/bin/pgrid pgrid.exe
cp -ra ${INDIR}/config/Grid_User .
#
./pgrid.exe
mv Grid_Prot ${OUTDIR}/coarse/pgrid_prot.log
rm Grid_User
#
echo '    --> DONE.'
#
# ===================================================================
# 3iv) ==>  WAM post-processing pspec
# ===================================================================
echo '--- iv) PSPEC ---'
#
cp -ra ${WAMDIR}/bin/pspec pspec.exe
cp -ra ${INDIR}/config/Spectra_User .
#
./pspec.exe
#
mv Spectra_Prot ${OUTDIR}/coarse/pspec_prot.log
rm Spectra_User
#
echo '    --> DONE.'
#
# ===================================================================
# 3v) ==>  WAM post-processing pspec
# ===================================================================
if [ $srcout = 'y' ]; then
    echo '--- v) PSOURCE ---'
#
    cp -ra ${WAMDIR}/bin/psource psource.exe
# Main Grid
    cp -ra ${INDIR}/config/Scr_User .
    ./psource.exe
    mv Scr_Prot ${OUTDIR}/coarse/psource_prot.log
# Nested Grids
    if [ $nofnest != 0 ]; then
        echo '    --> coarse'
        for ((i=1;$nofnest>=i;i++)); do
            cp -ra ${INDIR}/config/Scr_User_N${i} Scr_User
            ./psource.exe
            mv Scr_Prot ${OUTDIR}/nest${i}/psource_prot.log
            echo '    --> nest '$i
        done
    fi
    rm Scr_User
#
    echo '    --> DONE.'
fi
#
# ===================================================================
# 4) ==>  STORE DATA
# ===================================================================
echo '=== 4) Store output data ==='
#
if [ ! -d ${STOREDIR} ]; then
    mkdir -p ${STOREDIR}
fi
rsync -au ${OUTDIR} ${STOREDIR}/
rsync -au ${INDIR}  ${STOREDIR}/
rsync -au ${GRDDIR} ${STOREDIR}/
#
#rm -rf ${OUTDIR} ${GRDDIR} *.exe
#
echo '    --> DONE.'
#
echo '==> JOB FINALIZED.'
rsync -au *.log ${STOREDIR}/
#

