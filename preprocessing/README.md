The scripts in this folder make the pre-processing of the satellite spectra and prepare the SWI input files for the spectra assimilation in WAM.
Below a step-by-step is provided:

1.	read_cfo_nc.f90
- This script reads L3 CFOSat NetCDF files and converts wave number k to frequency f vector and interpolates the spectra from CFOSat spectral resolution (frequency and direction) to model’s spectral resolution. 
- Make sure to check beam angle if you are using other beam of CFOSat data. (The variable names and sizes will also be different.)
- Adjust the model specifications:
	* Number of frequencies (NFREQ_WAM), number of directions (NANG_WAM), and first frequency of WAM (f0_WAM).
	* Check beam angle from CFOSat data and the corresponding number in read_cfo_nc.f90, which will be used in lance_cfo_nc.sh.
		
2.	filtre_sar.for_ori
- This script filters the data for the prescribed model domain and creates hourly SWIYYYYMMDDHHSS00 files from CFOSat NetCDF files.
- Adjust:
	* Parameters: NGX, NGY (number of grid points in longitude and latitude direction), NF and ND (number of frequencies and directions as given in read_cfo_nc.f90).
	* Parameters: XDELLA, XDELLO, AMOSOP, AMONOP, AMOWEP and AMOEAP for your domain.
		
3.	lance_cfo_nc.sh
- This script runs the reading and filtering script. It first collects all read data into SWI_WV1 with read_cfo_nc.f90 and then creates hourly data for your domain with filtre_sar_for.ori.
- Adjust:
	* chemin_fabrique as the current folder and ensure that the CFOSat files are also in this directory.
	* list_fic = CFOSat file name, 3 is the number for beam angle 10 degrees (2 for 8 degrees and 1 for 6 degrees).

4. 	Run with: ./lance_cfo_nc.sh
