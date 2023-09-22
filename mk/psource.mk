OBJECTS = wam_general_module.o wam_print_module.o wam_file_module.o \
wam_print_user_module.o print_scr_file.o read_scr_file.o \
read_scr_user.o

psource:
	$(FC)  $(OBJECTS) -o psource $(OASISLIB)

