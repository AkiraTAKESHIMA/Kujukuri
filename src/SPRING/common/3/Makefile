TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = c3.a

OBJ_F = \
c3_rt_llbnds.o\
c3_rt_latlon_latlon.o\
c3_rt_latlon_raster.o\
c3_rt_latlon_polygon.o\
c3_rt_raster_raster.o\
c3_rt_raster_polygon.o\
c3_rt_polygon_polygon.o\
c3_rt_driv.o

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

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(CMNDIR2)/c2.a $(CMNDIR1)/c1.a $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)
