# WAM (Cycle 7.1)

Official repository of the third-generation spectral WAve Model WAM

This branch is the most recent offical stand-alone version of WAM.
For previous versions please visit: <https://github.com/mywave/WAM/>

New in Cycle 7.1:
  - assimilation of 2D wave spectra (IASSI_SPECTRA = 1)
  - direct NetCDF4 output of integrated parameters

New in Cycle 7:
  - ST6 (BYDBR) physics (IPHYS = 2)
  - bug fix in normalised wave stress
  - simplified usage of the SWAMP test case
  - betamax for ST4 adjustable via namelist
  - improved Netcdf conversion (in post-processing)
  - ! IMPROVED COMPILATION VIA NEW MAKEFILE !
  - merge with OASIS branch
  - source term output
  - ! UPDATED COUPLING INTERFACE FOLLOWING OASIS3-MCT CONVENTIONS (data exchange 
    at coupling times t=0,...,N-1 only; no data exchange at t=N) !
  - upgrade to modern Fortran 2008 MPI module mpi_f08
  - bug fix of uninitialised value in data assimilation routine

################################################################################
## License

WAM is free software: you can redistribute it and/or modify it under the terms 
of the GNU General Public License as published by the Free Software Foundation, 
either version 3 of the License, or at your option) any later version.

WAM is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with 
WAM. If not, see <http://www.gnu.org/licenses/>

################################################################################
## GPU WAM version

A GPU version of WAM Cycle 6 can be found here:
https://zenodo.org/records/13937876

Yuan, Y., Yu, F., Chen, Z., Li, X., Hou, F., Gao, Y., Gao, Z., & Pang, R. (2024). Towards a real-time modeling of global ocean waves by the fully GPU-accelerated spectral wave model WAM6-GPU v1.0. Geoscientific Model Development, 17(16), 6123–6136. https://doi.org/10.5194/gmd-17-6123-2024

################################################################################
## Installation - Quick build instructions

1. Install the following prerequisites of WAM on your system:
   * MPI
   * NetCDF
   * OASIS 3 MCT (if the OASIS coupling interface will be used)

2. Download the repository: <https://github.com/mywave/WAM/tree/WAM_Cycle7.1>

### OPTION A: individual compilation directly via make 

3. Link the compatible subroutines for the desired input data format in the 
   source code directories src/chief and src/preproc:
   ```
   [chief]   $ ln -sf read_*_input*.f90    read_*_input.f90 
   [preproc] $ ln -sf read_topography*.f90 read_topography.f90
   ```
   For the "SWAMP" test case:
   ```
   [chief]   $ ln -sf read_wind_input_arno.f90     read_wind_input.f90 
   [chief]   $ ln -sf read_topo_input_arno.f90     read_topo_input.f90 
   [chief]   $ ln -sf read_current_input_arno.f90  read_current_input.f90 
   [chief]   $ ln -sf read_boundary_input_arno.f90 read_boundary_input.f90 
   [chief]   $ ln -sf read_ice_input_arno.f90      read_ice_input.f90 
   [preproc] $ ln -sf read_topography_arno.f90     read_topography.f90
   ```

4. Choose between activated or deactivated OASIS coupling interface by linking 
   the corresponding module in directory src/mod.
   * deactivated OASIS interface for stand-alone WAM:
   ```
   [mod] $ ln -sf wam_oasis_inactive_module.f90 wam_oasis_module.f90
   ```
   * activated OASIS interface for both, coupled and stand-alone WAM 
     (requires OASIS 3 MCT library):
   ```
   [mod] $ ln -sf wam_oasis_active_module.f90   wam_oasis_module.f90
   ```

5. Load the required modules (MPI, NetCDF, and potential prerequisites).
   ```
   [WAM] $ module load NAMES
   ```
   Note: The exact NAMES can differ for each SYSTEM.

6. Set the compiler specifications and library paths in the preamble of the
   Makefile.

7. Compile the executables via make:
   ```
   [WAM] $ make clean 
   [WAM] $ make TARGET
   ```
   The optional argument TARGET specifies programm component to be compiled:
   * all       : All of the below are compiled (DEFAULT)
   * preproc   : Pre-processing program to create domain/grid files
   * wam       : Main program running the wave model 
   * pnetcdf   : Converter of binary wam output to NetCDF-format
   * pgrid     : Post-processing program for !!! TODO !!!
   * pspec     : Post-processing program for !!! TODO !!!
   * ptime[_S] : Post-processing program for !!! TODO !!!
   * psource   : Post-processing program for source-term output conversion
   
   Note that no TARGET is equivalent to TARGET=all.

8. DONE! The executables can be found in the directory bin/.

### OPTION B: full compilation via makeWAM.bash

3. Adjust the linking of read subroutines IN the file makeWAM.bash (2nd part).
   See step 3 of OPTION A for a linking example.

4. Ensure executable permission is set for makeWAM.bash.
   ```
   [WAM] $ chmod -u+x makeWAM.bash
   ```

5. Compile the executables by executing
   ```
   [WAM] $ ./makeWAM.bash clean 
   [WAM] $ ./makeWAM.bash SYSTEM TARGET
   ```
   The argument SYSTEM specifies the environment and compiler settings to 
   compile with. Currently pre-defined environments for SYSTEM are:
   * strand[-oneAPI|-GCC]
   * levante[-oneAPI|-GCC]
   If you are going to use WAM on a different system: Define the required 
   environment in the preamble of makeWAM.bash by adding another elseif block 
   for your SYSTEM, in which you define loaded modules, compiler settings, and 
   library paths, in the first part of makeWAM.bash.

   The optional argument TARGET specifies programm component to be compiled:
   * all       : All of the below are compiled (DEFAULT)
   * preproc   : Pre-processing program to create domain/grid files
   * wam       : Main program running the wave model (stand-alone)
   * oasis     : Main program running the wave model (coupled mode)
   * pnetcdf   : Converter of binary wam output to NetCDF-format
   * pgrid     : Post-processing program for !!! TODO !!!
   * pspec     : Post-processing program for !!! TODO !!!
   * ptime[_S] : Post-processing program for !!! TODO !!!
   * psource   : Post-processing program for source-term output conversion
 
   Notes:
   - no TARGET is equivalent to TARGET=all. Thus, the command 
     "./makeWAM.bash SYSTEM [all]" is equivalent to a "make [all]" in an 
     accordingly prepared SYSTEM environment without any OASIS libraries and 
     generally disabled OASIS coupling.
   - A call of "./makeWAM.bash SYSTEM oasis" (re)bulids the executable wam with 
     enabled OASIS coupling and is equivalent to a "make wam" in a prepared 
     SYSTEM environment including installed OASIS libraries. In this case one 
     needs to specify a path OASISDIR to the OASIS library in the OASIS 
     environment section.

6. DONE! The executables can be found in the directory bin.

################################################################################
## Execution (WAM stand-alone)

Each program requires a corresponding parameter file (namelist) named *_User in 
the chosen work directory for its execution. Examples *_User files (also used in 
the "SWAMP" test case) can be found in the directories const or 
SWAMPtest/input/config. 

### Manual step-by-step execution 

1. Copy the executable(s) and the parameter file(s) to any work directory of 
   your choice:
   ```
   [WRKDIR] $ cp -ra PATH/TO/WAM/bin/BINNAME ./BINNAME.exe
   [WRKDIR] $ cp -ra PATH/TO/WAM/const/BINNAME_User ./BINNAME_User
   ```

2. Set the general run parameters and the locations of input and output data 
   files in the format chosen at compilation stage in *_User.

3. Execute the pre-processing program preproc.exe (for now only as a singelton 
   process), e.g.:
   ```
   [WRKDIR] $ ./preproc.exe
   ```

4. Execute the main program wam.exe in the parallel enviroment of your choice, 
   e.g.:
   ```
   [WRKDIR] $ mpirun -n 48 ./wam.exe
   ```

5. Execute the post-processing program(s) (for now only as a singelton process), 
   e.g.:
   ```
   [WRKDIR] $ ./pnetcdf.exe
   [WRKDIR] $ ./pgrid.exe
   [WRKDIR] $ ./pspec.exe
   [WRKDIR] $ ./ptime[_S].exe
   ```

### Execution of the "SWAMP" test case:

An example case including nested grids to test and validate your compilation is 
provided in the directory SWAMPtest. It provides fully automated execution 
scripts for the Strand and Levante HPC environements:

0. Choose your environement. If not STRAND or LEVANTE, copy and modify your 
   run_WAMall_SYSTEM.bash accordingly.

1. Adjust in run_WAMall_SYSTEM.bash: The SLURM settings, the paths to your 
   executables (WAMDIR=) and your storage directory (STOREDIR=), and the number 
   of mpi processes used for wam (nproc=). Optionally, you can adjust whether 
   pre-processing is switched on/off (preproc=['y'/'n']) and how many nested 
   grids will be performed (nofnest=[0/1/2]) 

2. Run
   ```
   [SWAMPtest] $ sbatch run_WAMall_SYSTEM.bash
   ```

3. Compare your output and grid (preproc output) directories:
   ```
   [SWAMPtest] $ diff -r[qs] ./grid ./ref_grid 
   [SWAMPtest] $ diff -r[qs] ./output ./ref_output 
   ```
The content of the ref_output/ folder can accessed here:  
<https://share.hereon.de/index.php/s/mLoy75QbNBLasBa>

4. Preview the data (with ncview):
   ```
   [SWAMPtest] $ ncview [ref_]output/*/ST6/WAVE*.nc
   ```

################################################################################  
## Documentation

* A physical description:  
  The WAMDI Group. (1988). The WAM Model -- A Third Generation Ocean Wave
  Prediction Model. Journal of Physical Oceanography, 18(12), 1775–1810. 
  https://doi.org/10.1175/1520-0485(1988)018<1775:TWMTGO>2.0.CO;2

* Technical report:  
  Günther, H., Hasselmann, S., Janssen, P.A.E.M. (1992). The WAM Model cycle 4. 
  World Data Center for Climate (WDCC) at DKRZ. 
  <https://doi.org/10.2312/WDCC/DKRZ_Report_No04>

* An extensive physical and technical description of the WAM derivative ECWAM of 
  ECMWF:  
  ECMWF. (2019). IFS Documentation CY46R1. ECMWF. 
  <https://doi.org/10.21957/21g1hoiuo>

* A book about WAM and wave modelling in general:  
  Komen, G.J., Cavaleri, L., Donelan, M., Hasselmann, K., Hasselmann, S., & 
  Janssen, P.A.E.M. (1994). Dynamics and Modelling of Ocean Waves. Cambridge 
  University Press. <https://doi.org/10.1017/CBO9780511628955>

* Educational material including WAM:  
  Janssen, P.A.E.M. (2002). The wave model. ECMWF. 
  <https://www.ecmwf.int/en/elibrary/79883-wave-model>

################################################################################  
Version 7.1.1  
Marcel Ricker (marcel DOT ricker AT hereon DOT de)  
Arno Behrens  (arno DOT behrens AT hereon DOT de)  
13 January 2026

