TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = c2.a

OBJ_F = \
c2_type_rt.o\
c2_type_rst.o\
c2_rt1d.o\
c2_area_raster.o\
c2_area_raster_polygon.o\
c2_rt_base.o\
c2_rt_set.o\
c2_rt_stats.o\
c2_rt_error.o\
c2_rt_main_io.o\
c2_rt_main_util.o\
c2_rt_main_coef.o\
c2_rt_main_finish.o\
c2_rt_vrf_core.o\
c2_rt_vrf_driv.o\
c2_rt_vrf_io.o

OBJ_CMN = $(OBJ_F)

TESTPROGRAM = main
OBJ_TEST = main.o

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

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(CMNDIR1)/c1.a $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)
