1.	read_cfo_nc.f90
- This script reads L3 CFOSat netcdf files and converts wave number k to frequency vector and interpolates the spectra from cfosat spectral resolution to (frequency and  direction) to model’s spectral resolution. 
- Make sure to compile it with your fortan compiler and create the read_cfo_nc file when you make changes.
- Make sure to check beam angle if you are using other level of CFOSat data. (The variable names and sizes will also be different.)
- Adjust the model specifications 
		i.	Number of frequencies, NFREQ_WAM, number of directions NANG_WAM, and first frequency of WAM f0_WAM
		ii.	Check beam angle from CFOSat data – and the corresponding number in read_cfo_nc.f90 which will be used in lance_cfo_nc.sh
		
2.	filtre_sar.for_ori
	a.	This script filters the data for the prescribed model domain. And creates hourly SWIYYYYMMDDHHSS00 files from CFOSat netcdf files.
	b.	Adjust :
		i.	Parameter: NGX, NGY (number of grid points in longitude and latitude direction), NF and ND (number of frequencies and directions as stated in read_cfo_nc.f90)
		ii.	Parameter: XDELLA, XDELLO, AMOSOP, AMONOP, AMOWEP and AMOEAP for your domain
		
3.	lance_cfo_nc.sh
	a.	This script runs the reading and filtering script. It first collects all read data into SWI_WV1 with read_cfo_nc.f90 and then creates hourly data for your domain with filtre_sar_for.ori
	b.	Adjust:
		i.	chemin_fabrique as the current folder and ensure that the CFOSat files are also in this directory.
		ii.	list_fic = CFOSat file name, 3 is the number for beam angle 10 degrees (2 for 8 degrees and 1 for 6 degrees) 
		
Running command = ./lance_cfo_nc.sh
