# WAM (Cycle6 OASIS)
Official repository of the third-generation spectral WAve Model WAM

This branch is the most recent OASIS-integrated version of WAM for the usage in
coupled models as well as stand-alone. For other versions please visit: 

https://github.com/mywave/WAM/tree/WAM_Cycle6_OASIS


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
   * OASIS 3

2. Download the repository: https://github.com/mywave/WAM/tree/WAM_Cycle6_OASIS

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

4. Set the libraries paths and modules to be loaded for your SYSTEM in the 
   preamble of mk/create_binaries.

5. Compile the executables from the directory mk:
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

6. DONE! The executables can be found in ./abs/
   * preproc: Pre-processor program to create domain/grid files
   * wam:     Main program running the stand-alone wave model 
   * pnetcdf: Converter of binary wam output to NetCDF-format    

### OPTION B: via make.SYSTEM for coupled models

3. Set the libraries paths and modules to be loaded for your SYSTEM in the 
   preamble of make.SYSTEM (e.g. make.LEVANTE).

4. Adjust the linking of read subroutines IN the file make.SYSTEM (e.g., 
   make.LEVANTE). See step 3 of OPTION A for example linking.
   
5. Compile the executables by executing
   ```
   [WAM] $ ./make.SYSTEM clean 
   [WAM] $ ./make.SYSTEM first
   [WAM] $ ./make.SYSTEM
   ```
   Note that "./make.SYSTEM first" is equivalent to OPTION A "./create_binaries
   SYSTEM" with according manually-linked subroutines. The final call of 
   "./make.SYSTEM" rebulids the executable wam with activated OASIS coupling. 

6. DONE! The executables can be found in ./abs/
   * preproc: Pre-processor program to create domain/grid files
   * wam:     Main program running the wave model with integrated OASIS coupling
   * pnetcdf: Converter of binary wam output to NetCDF-format    

################################################################################
## Execution

TODO!!!

################################################################################
## Documentation

A full documentation can be found at: ToDo!!!

################################################################################  
Version 6.0.0  
Marcel Ricker   (marcel DOT ricker AT hereon DOT de)  
Robert Hartmann (robert DOT hartmann AT hereon DOT de)  
28.05.2024