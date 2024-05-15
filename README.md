# WAM
Official repository of the third-generation spectral WAve Model WAM

This branch is the most recent offical stand-alone version of WAM.
For previous versions please visit: https://github.com/mywave/WAM/


New in Cycle 7:
  - ST6 (BYDBR) physics (IPHYS = 2)
  - Bug fix in normalised wave stress
  - betamax for ST4 adjustable via namelist

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

2. Download the repository: https://github.com/mywave/WAM/

3. Set the libraries paths and modules to be loaded for your SYSTEM in the 
   preamble of mk/create_binaries.

4. Compile the executables from the directory mk:
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

5. DONE! The executables can be found in ./abs/
   * preproc: Pre-processor program to create domain/grid files
   * wam:     Main program running the wave model 
   * pnetcdf: Converter of binary wam output to NetCDF-format    

################################################################################
## Execution

TODO!!!

################################################################################
## Documentation

A full documentation can be found at: ToDo!!!

################################################################################  
Version 4.7.0  
Marcel Ricker   (marcel DOT ricker AT hereon DOT de)  
Robert Hartmann (robert DOT hartmann AT hereon DOT de)  
15.05.2024