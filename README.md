# WAM
Official repository of the third-generation spectral WAve Model WAM

This branch is the most recent offical stand-alone version of WAM.
For previous versions please visit: https://github.com/mywave/WAM/

New in Cycle 7:
  - ST6 (BYDBR) physics (IPHYS = 2)
  - bug fix in normalised wave stress
  - betamax for ST4 adjustable via namelist
  - improved Netcdf conversion
  - improved compilation

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
   * wam:     Main program running the wave model 
   * pnetcdf: Converter of binary wam output to NetCDF-format    

################################################################################
## Execution

TODO!!!

################################################################################
## Documentation

A physical description:
The WAMDI Group. (1988). The WAM Model—A Third Generation Ocean Wave Prediction Model. Journal of Physical Oceanography, 18(12), 1775–1810. https://doi.org/10.1175/1520-0485(1988)018<1775:TWMTGO>2.0.CO;2

Technical report:
Günther, H., Hasselmann, S., Janssen, P.A.E.M. (1992). The WAM Model cycle 4. World Data Center for Climate (WDCC) at DKRZ. https://doi.org/10.2312/WDCC/DKRZ_Report_No04

An extensive physical and technical description of the WAM derivative ECWAM of ECmWF:
ECMWF. (2019). IFS Documentation CY46R1. ECMWF. https://doi.org/10.21957/21g1hoiuo.

################################################################################  
Version 7

Marcel Ricker   (marcel DOT ricker AT hereon DOT de)  
Robert Hartmann (robert DOT hartmann AT hereon DOT de)  
15 May 2024
