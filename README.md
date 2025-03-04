# WAM (Cycle7)

Official repository of the third-generation spectral WAve Model WAM

This branch is the most recent offical stand-alone version of WAM.
For previous versions please visit: <https://github.com/mywave/WAM/>

New in Cycle 7:
  - ST6 (BYDBR) physics (IPHYS = 2)
  - bug fix in normalised wave stress
  - betamax for ST4 adjustable via namelist
  - improved Netcdf conversion
  - improved compilation
  - merge with OASIS branch
  - source term output
  - updated coupling interface following OASIS3-MCT conventions (data exchange 
    at coupling times t=0,...,N-1 only; no data exchange at t=N)
  - Upgrade to modern Fortran 2008 MPI module mpi_f08

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
## Installation - Quick build instructions

1. Install the following prerequisites of WAM on your system:
   * MPI
   * NetCDF
   * OASIS 3 (if the OASIS coupling interface will be used)

2. Download the repository: <https://github.com/mywave/WAM/tree/WAM_Cycle7>

### OPTION A: via mk/create_binaries as stand-alone WAM 

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

4. Set the library paths and modules to be loaded for your SYSTEM in the 
   preamble of mk/create_binaries.

5. Ensure executable permission is set for mk/create_binaries, mk/build_*, and 
   mk/make_* files.
   ```
   [mk] $ chmod -u+x create_binaries build_* make_* 
   ```

6. Compile the executables from the directory mk:
   ```
   [mk] $ rm ../abs/* ../obj/* 
   [mk] $ ./create_binaries SYSTEM
   ```
   The argument SYSTEM specifies the libraries and moduled set under 3. 
   Currently supported systems are: 
   * strand[-oneAPI]
   * levante[-oneAPI]
   * first  
   "first" does not specify any modules or libraries and should only be used for
   calls from super-scripts, which already set the SYSTEM environement.

7. DONE! The executables can be found in the directory abs.

### OPTION B: via make.SYSTEM for coupled models

3. Set the library paths and modules to be loaded for your SYSTEM in the 
   preamble of make.SYSTEM (e.g. make.LEVANTE).

4. Adjust the linking of read subroutines IN the file make.SYSTEM (e.g., 
   make.LEVANTE). See step 3 of OPTION A for a linking example.

5. Ensure executable permission is set for make.SYSTEM, mk/create_binaries, 
   mk/build_*, and mk/make_* files.
   ```
   [WAM] $ chmod -u+x make.SYSTEM mk/create_binaries mk/build_* mk/make_* 
   ```

6. Compile the executables by executing
   ```
   [WAM] $ ./make.SYSTEM clean 
   [WAM] $ ./make.SYSTEM first
   [WAM] $ ./make.SYSTEM
   ```
   Note that "./make.SYSTEM first" is equivalent to OPTION A "./create_binaries
   SYSTEM" with according manually-linked subroutines. The final call of 
   "./make.SYSTEM" rebulids the executable wam with activated OASIS coupling. 

7. DONE! The executables can be found in the directory abs.

After a successfull compilation abs should cointain the following executables 
(binaries):
   * preproc   : Pre-processing program to create domain/grid files
   * wam       : Main program running the wave model 
   * pnetcdf   : Converter of binary wam output to NetCDF-format
   * pgrid     : !!! TODO !!!
   * pspec     : !!! TODO !!!
   * ptime[_S] : !!! TODO !!!
   * psource   : !!! TODO !!!

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
   [WRKDIR] $ cp -ra PATH/TO/WAM/abs/BINNAME ./BINNAME.exe
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
Version 7.0.4  
Marcel Ricker   (marcel DOT ricker AT hereon DOT de)  
Robert Hartmann (robert DOT hartmann AT hereon DOT de)  
06 June 2024
