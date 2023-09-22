OBJECTS = wam_general_module.o wam_print_module.o wam_file_module.o \
read_radiation_file.o \
wam_print_user_module.o print_rad_time.o read_time_user.o read_grid_file.o \
wam_oasis_module.o wam_coordinate_module.o

prad_time:
	$(FC) $(OBJECTS) -o prad_time $(OASISLIB)
