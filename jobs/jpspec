#!/bin/bash
#
#     Arno Behrens (May 2019)
#
#==>  WAM post-processing pspec
#
#SBATCH --job-name=pspec
#SBATCH --partition=pCluster
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:05:00
##SBATCH --mail-type=FAIL
##SBATCH --account=cluster
#SBATCH --output=pspec.o%j
#SBATCH --error=pspec.e%j
#
module load compilers/intel/2019.4.243
module load intelmpi/2019.4.243
module load netcdf
#
set -k
WAMDIR=/gpfs/home/ricker/WAM/WAM_Cycle7_test
WORK=/gpfs/work/ricker/storage/WAM/WAM_Cycle7_test
#
cd ${WORK}/tempsg
cp ${WAMDIR}/const/Spectra_User .
cp ${WAMDIR}/abs/pspec pspec.exe
#
./pspec.exe
#
mv Spectra_Prot ${WAMDIR}/dayfiles/pspec_prot_coarse_ST6
rm Spectra_User pspec.exe
#
# ===================================================================
#  GRID FILES HAVE BEEN CREATED AND SAVED.
#  END OF JOB PGRID.
# ===================================================================
#
