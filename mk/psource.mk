OBJECTS = wam_general_module.o wam_print_module.o wam_file_module.o \
wam_print_user_module.o wam_coordinate_module.o wam_output_parameter_module.o print_scr_file.o read_scr_file.o \
read_scr_user.o wam_oasis_module.o

psource:
	${FC} ${FFLAGS} $(OBJECTS) -o psource ${LDOPT}

