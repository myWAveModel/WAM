#FC	= mpif90
INC	= -I${OASISMOD}

NETCDF	= netcdf.inc netcdf.mod typesizes.mod

PREPROC	= wam_mpi_module.o wam_file_module.o wam_general_module.o wam_timopt_module.o wam_fre_dir_module.o \
	wam_jonswap_module.o wam_tables_module.o wam_interface_module.o wam_grid_module.o wam_model_module.o \
	wam_boundary_module.o preproc_module.o wam_special_module.o wam_oasis_module.o \
	preproc_user_module.o wam_nest_module.o read_boundary_input.o wam_output_set_up_module.o \
	preproc.o read_topography.o read_preproc_user.o wam_mpi_comp_module.o wam_coordinate_module.o \
	wam_output_parameter_module.o read_ice_input.o wam_ice_module.o \
	wam_current_module.o wam_topo_module.o wam_wind_module.o read_current_input.o read_topo_input.o read_wind_input.o

CHIEF	= wam_file_module.o wam_general_module.o wam_timopt_module.o wam_fre_dir_module.o \
	wam_jonswap_module.o wam_tables_module.o wam_swell_module.o \
	wam_interface_module.o wam_grid_module.o wam_current_module.o wam_model_module.o \
	wam_ice_module.o wam_output_module.o wam_wind_module.o wam_boundary_module.o \
	wam_flux_module.o wam_output_parameter_module.o \
	wam_source_module.o wam_propagation_module.o preproc_module.o wam_coldstart_module.o \
	wam_restart_module.o wam_initial_module.o wam_mpi_module.o wam_output_set_up_module.o \
	wam_topo_module.o wam_radiation_module.o wam_nest_module.o wam_user_module.o \
	wam_special_module.o read_topo_input.o chief.o wavemdl.o initmdl.o read_wam_user.o \
	print_wam_status.o read_wind_input.o read_current_input.o wamodel.o read_boundary_input.o \
	read_ice_input.o jafu.o wam_mpi_comp_module.o wam_assi_set_up_module.o wam_assi_module.o \
	wam_coordinate_module.o readsat.o wam_source_output_module.o wam_oasis_module.o

PGRID	= wam_general_module.o wam_print_module.o wam_file_module.o \
	wam_coordinate_module.o wam_oasis_inactive_module.o \
	wam_output_parameter_module.o \
	wam_print_user_module.o print_grid_file.o read_grid_file.o read_grid_user.o

PNETCDF = wam_mpi_module.o wam_file_module.o wam_general_module.o wam_timopt_module.o \
	wam_model_module.o wam_flux_module.o wam_source_module.o wam_fre_dir_module.o \
	wam_jonswap_module.o wam_tables_module.o wam_interface_module.o ingrid.o \
	wam_grid_module.o wam_current_module.o wam_special_module.o wam_nest_module.o \
	wam_ice_module.o wam_swell_module.o wam_output_module.o wam_print_module.o \
	wam_output_parameter_module.o wam_radiation_module.o wam_propagation_module.o \
	wam_output_set_up_module.o wam_mpi_comp_module.o wam_netcdf_module.o wam_coordinate_module.o \
	read_current_input.o read_ice_input.o wam_topo_module.o read_topo_input.o jafu.o \
	make_netcdf.o wam_source_output_module.o wam_oasis_inactive_module.o dtsec.o

PNETCDF_RAD = wam_mpi_module.o wam_file_module.o wam_general_module.o wam_timopt_module.o \
	wam_model_module.o wam_flux_module.o wam_source_module.o wam_fre_dir_module.o \
	wam_jonswap_module.o wam_tables_module.o wam_interface_module.o \
	wam_grid_module.o wam_current_module.o wam_special_module.o wam_nest_module.o \
	wam_ice_module.o wam_swell_module.o wam_output_module.o wam_print_module.o \
	wam_output_set_up_module.o wam_mpi_comp_module.o wam_rad_netcdf_module.o wam_coordinate_module.o \
	read_current_input.o read_ice_input.o wam_topo_module.o read_topo_input.o jafu.o \
	make_rad_netcdf.o wam_oasis_inactive_module.o wam_source_output_module.o dtsec.o

PRAD	= wam_general_module.o wam_print_module.o wam_file_module.o \
	wam_print_user_module.o print_radiation_file.o read_radiation_file.o \
	read_radiation_user.o wam_coordinate_module.o wam_oasis_inactive_module.o

PRAD_TIME = wam_general_module.o wam_print_module.o wam_file_module.o \
	read_radiation_file.o \
	wam_print_user_module.o print_rad_time.o read_time_user.o read_grid_file.o \
	wam_oasis_inactive_module.o wam_coordinate_module.o

PSOURCE = wam_general_module.o wam_print_module.o wam_file_module.o \
	wam_output_parameter_module.o \
	wam_print_user_module.o print_scr_file.o read_scr_file.o \
	read_scr_user.o wam_oasis_inactive_module.o wam_coordinate_module.o

PSPEC	= wam_general_module.o wam_print_module.o wam_file_module.o wam_output_parameter_module.o \
	wam_print_user_module.o print_spectra_file.o read_spectra_file.o \
	read_spectra_user.o wam_coordinate_module.o wam_oasis_inactive_module.o

PTIME	= wam_general_module.o wam_print_module.o wam_file_module.o wam_output_parameter_module.o \
	wam_print_user_module.o print_time.o read_time_user.o read_grid_file.o \
	wam_coordinate_module.o wam_oasis_inactive_module.o

PTIME_S	= wam_general_module.o wam_print_module.o wam_file_module.o \
	wam_print_user_module.o print_time_S.o read_time_user_S.o read_grid_file.o \
	wam_coordinate_module.o wam_output_parameter_module.o wam_oasis_inactive_module.o

BSFILE	= bsfile.o incdate.o

PRE_U10	= pre_u10.o incdate.o

# targets

../abs/wam:	$(CHIEF)
	$(FC) $(FFLAGS) $(CHIEF) -o $@ $(LIBS)
../abs/preproc:	$(PREPROC)
	$(FC) $(FFLAGS) $(PREPROC) -o $@ $(LIBS)
../abs/pgrid:	$(PGRID)
	$(FC) $(FFLAGS) $(PGRID) -o $@
../abs/pnetcdf:	$(PNETCDF)
	$(FC) $(FFLAGS) $(PNETCDF) -o $@ $(LIBS)
../abs/pnetcdf_rad: $(PNETCDF_RAD)
	$(FC) $(FFLAGS) $(PNETCDF_RAD) -o $@ $(LIBS)
../abs/prad:	$(PRAD)
	$(FC) $(FFLAGS) $(PRAD) -o $@
../abs/prad_time: $(PRAD_TIME)
	$(FC) $(FFLAGS) $(PRAD_TIME) -o $@
../abs/psource:	$(PSOURCE)
	$(FC) $(FFLAGS) $(PSOURCE) -o $@
../abs/pspec:	$(PSPEC)
	$(FC) $(FFLAGS) $(PSPEC) -o $@
../abs/ptime:	$(PTIME)
	$(FC) $(FFLAGS) $(PTIME) -o $@
../abs/ptime_S:	$(PTIME_S)
	$(FC) $(FFLAGS) $(PTIME_S) -o $@
../abs/bsfile:	$(BSFILE)
	$(FC) $(FFLAGS) $(BSFILE) -o $@
../abs/pre_u10:	$(PRE_U10)
	$(FC) $(FFLAGS) $(PRE_U10) -o $@
#all:	../abs/wam ../abs/preproc ../abs/pgrid ../abs/pnetcdf ../abs/pnetcdf_rad ../abs/prad \
#	../abs/prad_time ../abs/pspec ../abs/psource ../abs/ptime ../abs/bsfile ../abs/pre_u10
#all:	../abs/wam ../abs/preproc ../abs/pgrid ../abs/pnetcdf \
#	../abs/pspec ../abs/psource ../abs/ptime ../abs/bsfile ../abs/pre_u10
all:	../abs/wam ../abs/preproc ../abs/pgrid ../abs/pnetcdf \
	../abs/pspec ../abs/ptime ../abs/ptime_S
	exit
netcdf.inc:	.
	ln -sf ${NETCDFHOME}/include/$@
netcdf.mod:	.
	ln -sf ${NETCDFHOME}/include/$@
typesizes.mod:	.
	ln -sf ${NETCDFHOME}/include/$@
mpif.h:	${MPIHOME}/include/mpif.h
	ln -sf ${MPIHOME}/include/mpif.h
clean:
	rm *.o
	rm *.mod
realclean:	clean
	rm *.f90 netcdf.inc mpif.h

# explicit rules

$(PREPROC) $(CHIEF) $(PNETCDF) $(PNETCDF_RAD):	$(NETCDF)
chief.o: chief.f90 wam_file_module.mod wam_general_module.mod \
	wam_mpi_comp_module.mod wam_mpi_module.mod wam_oasis_module.mod \
	wam_timopt_module.mod
ingrid.o: ingrid.f90 wam_output_set_up_module.mod \
	wam_print_module.mod
initmdl.o initmdl.mod: initmdl.f90 wam_assi_set_up_module.mod \
	wam_boundary_module.mod wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_initial_module.mod wam_model_module.mod wam_mpi_comp_module.mod \
	wam_mpi_module.mod wam_nest_module.mod wam_oasis_module.mod wam_output_module.mod \
	wam_output_set_up_module.mod wam_propagation_module.mod \
	wam_radiation_module.mod wam_restart_module.mod wam_source_module.mod \
	wam_source_output_module.mod wam_timopt_module.mod
make_netcdf.o: make_netcdf.f90 \
	wam_coordinate_module.mod wam_general_module.mod wam_netcdf_module.mod \
	wam_output_set_up_module.mod wam_print_module.mod
make_rad_netcdf.o: make_rad_netcdf.f90 \
	wam_coordinate_module.mod wam_general_module.mod \
	wam_output_set_up_module.mod wam_print_module.mod \
	wam_rad_netcdf_module.mod
preproc.o: preproc.f90 preproc_module.mod \
	wam_file_module.mod wam_tables_module.mod
preproc_module.o preproc_module.mod: preproc_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_nest_module.mod \
	wam_tables_module.mod
preproc_user_module.o preproc_user_module.mod: preproc_user_module.f90 \
	preproc_module.mod wam_coordinate_module.mod wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_nest_module.mod \
	wam_tables_module.mod
print_grid_file.o: print_grid_file.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_module.mod
print_rad_time.o print_rad_time.mod: print_rad_time.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_print_module.mod
print_radiation_file.o: \
	print_radiation_file.f90 wam_file_module.mod wam_general_module.mod \
	wam_print_module.mod
print_scr_file.o: print_scr_file.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_module.mod
print_spectra_file.o: print_spectra_file.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_print_module.mod
print_time.o: print_time.f90 wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod wam_print_module.mod
print_time_S.o print_time_S.mod: print_time_S.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_print_module.mod
print_wam_status.o print_wam_status.mod: print_wam_status.f90 \
	wam_assi_set_up_module.mod wam_boundary_module.mod \
	wam_coldstart_module.mod wam_current_module.mod wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_ice_module.mod wam_nest_module.mod wam_output_set_up_module.mod \
	wam_propagation_module.mod wam_radiation_module.mod \
	wam_restart_module.mod wam_source_module.mod \
	wam_source_output_module.mod wam_tables_module.mod \
	wam_timopt_module.mod wam_topo_module.mod wam_wind_module.mod
read_boundary_input.o: read_boundary_input.f90 \
	wam_boundary_module.mod wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_nest_module.mod
read_current_input.o: read_current_input.f90 \
	wam_current_module.mod wam_file_module.mod wam_general_module.mod
read_current_input_arno.o: \
	read_current_input_arno.f90 wam_current_module.mod wam_file_module.mod \
	wam_general_module.mod
read_current_input_getm.o: \
	read_current_input_getm.f90 wam_current_module.mod \
	wam_file_module.mod wam_general_module.mod
read_grid_file.o: read_grid_file.f90 \
	wam_file_module.mod wam_print_module.mod
read_grid_user.o: read_grid_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_ice_input.o: read_ice_input.f90 \
	wam_file_module.mod wam_general_module.mod wam_ice_module.mod
read_preproc_user.o: read_preproc_user.f90 \
	preproc_user_module.mod wam_file_module.mod wam_general_module.mod
read_radiation_file.o: read_radiation_file.f90 \
	wam_file_module.mod wam_print_module.mod
read_radiation_user.o: read_radiation_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_scr_file.o: read_scr_file.f90 \
	wam_file_module.mod wam_print_module.mod
read_scr_user.o: read_scr_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_spectra_file.o: read_spectra_file.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_module.mod
read_spectra_user.o: read_spectra_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_spectrum.o: read_spectrum.f90 \
	wam_output_set_up_module.mod wam_print_module.mod
read_time_user.o: read_time_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_time_user_S.o read_time_user_S.mod: read_time_user_S.f90 \
	wam_file_module.mod wam_general_module.mod wam_print_user_module.mod
read_topo_input.o: read_topo_input.f90 \
	wam_file_module.mod wam_general_module.mod wam_topo_module.mod
read_topo_input_arno.o: \
	read_topo_input_arno.f90 wam_file_module.mod wam_general_module.mod \
	wam_topo_module.mod
read_topo_input_getm.o: \
	read_topo_input_getm.f90 wam_file_module.mod \
	wam_general_module.mod wam_topo_module.mod
read_topography.o: read_topography.f90 \
	preproc_module.mod wam_coordinate_module.mod wam_file_module.mod \
	wam_general_module.mod
read_topography_BSH.o read_topography_BSH.mod: read_topography_BSH.f90 \
	preproc_module.mod wam_coordinate_module.mod wam_file_module.mod \
	wam_general_module.mod
read_topography_ETOPO.o read_topography_ETOPO.mod: \
	read_topography_ETOPO.F90 preproc_module.mod wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod
read_topography_Heinz.o read_topography_Heinz.mod: \
	read_topography_Heinz.f90 preproc_module.mod wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod
read_topography_arno.o: \
	read_topography_arno.f90 preproc_module.mod wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod
read_topography_getm.o: \
	read_topography_getm.f90 preproc_module.mod wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod
read_wam_user.o: read_wam_user.f90 \
	wam_file_module.mod wam_general_module.mod wam_user_module.mod
read_wind_input.o: read_wind_input.f90 \
	wam_file_module.mod wam_general_module.mod wam_special_module.mod \
	wam_timopt_module.mod wam_wind_module.mod
read_wind_input_DWD.o: read_wind_input_DWD.f90 \
	wam_file_module.mod wam_general_module.mod wam_special_module.mod \
	wam_timopt_module.mod wam_wind_module.mod
read_wind_input_DWD_LSM.o: \
	read_wind_input_DWD_LSM.f90 wam_file_module.mod wam_general_module.mod \
	wam_special_module.mod wam_timopt_module.mod wam_wind_module.mod
read_wind_input_ecmwf.o: \
	read_wind_input_ecmwf.f90 wam_file_module.mod \
	wam_general_module.mod wam_special_module.mod wam_timopt_module.mod \
	wam_wind_module.mod
read_wind_input_swamp.o: \
	read_wind_input_swamp.f90 wam_file_module.mod wam_general_module.mod \
	wam_wind_module.mod
wam_assi_module.o wam_assi_module.mod: wam_assi_module.f90 \
	wam_assi_set_up_module.mod wam_coordinate_module.mod \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_ice_module.mod wam_interface_module.mod \
	wam_model_module.mod wam_mpi_comp_module.mod wam_mpi_module.mod \
	wam_output_module.mod wam_output_set_up_module.mod \
	wam_timopt_module.mod wam_topo_module.mod
wam_assi_set_up_module.o wam_assi_set_up_module.mod: \
	wam_assi_set_up_module.f90 wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_output_set_up_module.mod wam_timopt_module.mod
wam_boundary_module.o wam_boundary_module.mod: wam_boundary_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_interface_module.mod wam_model_module.mod \
	wam_mpi_comp_module.mod wam_mpi_module.mod wam_nest_module.mod \
	wam_output_set_up_module.mod wam_timopt_module.mod
wam_coldstart_module.o wam_coldstart_module.mod: \
	wam_coldstart_module.f90 wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_jonswap_module.mod \
	wam_model_module.mod wam_mpi_module.mod wam_oasis_module.mod \
	wam_tables_module.mod wam_timopt_module.mod wam_wind_module.mod
wam_coordinate_module.o wam_coordinate_module.mod: \
	wam_coordinate_module.f90 wam_file_module.mod
wam_current_module.o wam_current_module.mod: wam_current_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_mpi_comp_module.mod wam_model_module.mod \
	wam_timopt_module.mod
wam_fre_dir_module.o wam_fre_dir_module.mod: wam_fre_dir_module.f90 \
	wam_file_module.mod wam_general_module.mod
wam_general_module.o wam_general_module.mod: wam_general_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod
wam_grid_module.o wam_grid_module.mod: wam_grid_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod
wam_ice_module.o wam_ice_module.mod: wam_ice_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_mpi_module.mod \
	wam_timopt_module.mod
wam_initial_module.o wam_initial_module.mod: wam_initial_module.f90 \
	wam_coldstart_module.mod wam_current_module.mod wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_ice_module.mod wam_model_module.mod wam_mpi_module.mod \
	wam_nest_module.mod wam_oasis_module.mod wam_propagation_module.mod \
	wam_restart_module.mod wam_source_module.mod wam_special_module.mod \
	wam_tables_module.mod wam_timopt_module.mod \
	wam_topo_module.mod wam_wind_module.mod
wam_interface_module.o wam_interface_module.mod: \
	wam_interface_module.f90 wam_fre_dir_module.mod wam_general_module.mod \
	wam_output_set_up_module.mod wam_tables_module.mod
wam_jonswap_module.o wam_jonswap_module.mod: wam_jonswap_module.f90 \
	wam_general_module.mod
wam_mpi_comp_module.o wam_mpi_comp_module.mod: wam_mpi_comp_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_mpi_module.mod wam_nest_module.mod \
	wam_output_set_up_module.mod
wam_nest_module.o wam_nest_module.mod: wam_nest_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_grid_module.mod
wam_netcdf_module.o wam_netcdf_module.mod: wam_netcdf_module.f90 \
	wam_file_module.mod wam_output_set_up_module.mod \
	wam_print_module.mod
wam_netcdf_module_arno.o wam_netcdf_module_arno.mod: \
	wam_netcdf_module_arno.f90 wam_file_module.mod \
	wam_output_set_up_module.mod wam_print_module.mod
wam_netcdf_module_kw.o wam_netcdf_module_kw.mod: \
	wam_netcdf_module_kw.f90 wam_file_module.mod \
	wam_output_set_up_module.mod wam_print_module.mod
wam_oasis_module.o wam_oasis_module.mod: wam_oasis_module.f90 \
	wam_coordinate_module.mod wam_current_module.mod \
	wam_file_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_model_module.mod wam_mpi_module.mod \
	wam_output_parameter_module.mod wam_output_set_up_module.mod \
	wam_timopt_module.mod wam_topo_module.mod wam_wind_module.mod
wam_output_module.o wam_output_module.mod: wam_output_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_ice_module.mod wam_interface_module.mod \
	wam_model_module.mod wam_mpi_comp_module.mod wam_mpi_module.mod \
	wam_output_parameter_module.mod wam_output_set_up_module.mod \
	wam_radiation_module.mod wam_source_module.mod \
	wam_special_module.mod wam_swell_module.mod wam_tables_module.mod \
	wam_timopt_module.mod wam_topo_module.mod
wam_output_set_up_module.o wam_output_set_up_module.mod: \
	wam_output_set_up_module.f90 wam_output_parameter_module.mod \
	wam_coordinate_module.mod \
	wam_file_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_special_module.mod wam_timopt_module.mod
wam_print_module.o wam_print_module.mod: wam_print_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_output_parameter_module.mod
wam_print_user_module.o wam_print_user_module.mod: \
	wam_print_user_module.f90 wam_coordinate_module.mod wam_file_module.mod \
	wam_print_module.mod
wam_propagation_module.o wam_propagation_module.mod: \
	wam_propagation_module.f90 wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_model_module.mod \
	wam_mpi_comp_module.mod wam_mpi_module.mod wam_tables_module.mod \
	wam_timopt_module.mod wam_topo_module.mod
wam_propagation_module_hg.o wam_propagation_module_hg.mod: \
	wam_propagation_module_hg.f90 wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_model_module.mod wam_mpi_comp_module.mod wam_mpi_module.mod \
	wam_timopt_module.mod
wam_propagation_module_kw.o wam_propagation_module_kw.mod: \
	wam_propagation_module_kw.f90 wam_file_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_model_module.mod wam_mpi_comp_module.mod wam_mpi_module.mod \
	wam_timopt_module.mod wam_topo_module.mod
wam_rad_netcdf_module.o wam_rad_netcdf_module.mod: \
	wam_rad_netcdf_module.f90 wam_file_module.mod \
	wam_output_set_up_module.mod wam_print_module.mod
wam_radiation_module.o wam_radiation_module.mod: \
	wam_radiation_module.f90 wam_file_module.mod wam_flux_module.mod \
	wam_fre_dir_module.mod wam_general_module.mod wam_grid_module.mod \
	wam_ice_module.mod wam_interface_module.mod wam_model_module.mod \
	wam_mpi_comp_module.mod wam_mpi_module.mod wam_nest_module.mod \
	wam_output_set_up_module.mod wam_propagation_module.mod \
	wam_tables_module.mod wam_timopt_module.mod wam_topo_module.mod
wam_restart_module.o wam_restart_module.mod: wam_restart_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_model_module.mod wam_mpi_comp_module.mod \
	wam_mpi_module.mod wam_special_module.mod wam_timopt_module.mod
wam_source_module.o wam_source_module.mod: wam_source_module.f90 \
	wam_file_module.mod wam_flux_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_interface_module.mod wam_mpi_module.mod \
	wam_oasis_module.mod wam_source_output_module.mod wam_tables_module.mod \
	wam_timopt_module.mod
wam_source_output_module.o wam_source_output_module.mod: \
	wam_source_output_module.f90 wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_ice_module.mod \
	wam_interface_module.mod wam_model_module.mod wam_mpi_comp_module.mod \
	wam_mpi_module.mod wam_oasis_module.mod wam_output_set_up_module.mod \
	wam_timopt_module.mod wam_topo_module.mod
wam_special_module.o wam_special_module.mod: wam_special_module.f90 \
	wam_file_module.mod wam_general_module.mod wam_mpi_module.mod
wam_swell_module.o wam_swell_module.mod: wam_swell_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_interface_module.mod wam_model_module.mod wam_mpi_module.mod \
	wam_output_set_up_module.mod
wam_tables_module.o wam_tables_module.mod: wam_tables_module.f90 \
	wam_file_module.mod wam_fre_dir_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_jonswap_module.mod
wam_timopt_module.o wam_timopt_module.mod: wam_timopt_module.f90 \
	wam_file_module.mod wam_general_module.mod
wam_topo_module.o wam_topo_module.mod: wam_topo_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_fre_dir_module.mod \
	wam_general_module.mod wam_grid_module.mod wam_model_module.mod \
	wam_mpi_comp_module.mod wam_mpi_module.mod wam_tables_module.mod \
	wam_timopt_module.mod
wam_user_module.o wam_user_module.mod: wam_user_module.f90 \
	wam_assi_set_up_module.mod wam_boundary_module.mod \
	wam_coldstart_module.mod wam_coordinate_module.mod \
	wam_current_module.mod wam_file_module.mod wam_ice_module.mod \
	wam_nest_module.mod wam_oasis_module.mod wam_output_set_up_module.mod \
	wam_radiation_module.mod wam_restart_module.mod \
	wam_source_output_module.mod wam_timopt_module.mod wam_topo_module.mod \
	wam_wind_module.mod
wam_wind_module.o wam_wind_module.mod: wam_wind_module.f90 \
	wam_coordinate_module.mod wam_file_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_model_module.mod wam_mpi_module.mod \
	wam_special_module.mod wam_timopt_module.mod
wamodel.o wamodel.mod: wamodel.f90 wam_assi_module.mod \
	wam_assi_set_up_module.mod wam_boundary_module.mod \
	wam_current_module.mod wam_file_module.mod wam_general_module.mod \
	wam_grid_module.mod wam_ice_module.mod wam_model_module.mod \
	wam_mpi_module.mod wam_nest_module.mod wam_oasis_module.mod \
	wam_output_module.mod wam_output_set_up_module.mod \
	wam_propagation_module.mod wam_radiation_module.mod \
	wam_restart_module.mod wam_source_module.mod \
	wam_source_output_module.mod wam_timopt_module.mod wam_topo_module.mod \
	wam_wind_module.mod
wavemdl.o: wavemdl.f90 wam_current_module.mod \
	wam_file_module.mod wam_general_module.mod wam_oasis_module.mod \
	wam_timopt_module.mod wam_topo_module.mod wam_wind_module.mod

# pattern rules

%.f90: ../src
	find ../src -name $@ -exec ln -sf {} \;
%.o: %.f90
	$(FC) $(FFLAGS) -c $<
%.mod: %.f90
	$(FC) $(FFLAGS) -c $<
.PRECIOUS:	%.f90
.SILENT:	Makefile %.f90 .
.SUFFIXES:
