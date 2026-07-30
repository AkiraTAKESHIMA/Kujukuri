TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = c1.a

OBJ_F = \
c1_const.o\
c1_const_util.o\
c1_type_opt.o\
c1_type_gs.o\
c1_type_timer.o\
c1_file.o\
c1_set.o\
c1_opt_ctrl.o\
c1_opt_set.o\
c1_timer.o\
c1_gs_util.o\
c1_gs_grid_base.o\
c1_gs_base.o\
c1_gs_grid_util.o\
c1_gs_grid_core.o\
c1_gs_define_polygon.o\
c1_gs_define.o\
c1_gs_grid_io.o

OBJ_CMN = $(OBJ_F)

OBJ_TEST = main.o
TESTPROGRAM = main

.PHONY: makemod
makemod:
	$(MAKE) $(OBJ_F)
	$(MAKE) $(TARGET)

.PHONY: maketest
maketest:
	$(MAKE) $(OBJ_TEST)
	$(MAKE) $(TESTPROGRAM)

$(TARGET): $(OBJ_CMN)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)

