OBJECTS = wam_general_module.o wam_print_module.o wam_file_module.o \
wam_print_user_module.o print_time_S.o read_time_user_S.o read_grid_file.o \
wam_coordinate_module.o wam_output_parameter_module.o wam_oasis_module.o

ptime_S:
	$(FC) $(OBJECTS) -o ptime_S $(OASISLIB)
