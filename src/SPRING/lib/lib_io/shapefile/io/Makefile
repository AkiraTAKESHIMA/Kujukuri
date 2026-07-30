TOPSRCDIR = ../../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
%.o: %.c
	$(CC) $(CCFLAGS) -c $<
%.mod: %.f90 %.o
	@:


OBJS = \
	safileio.o\
	shpopen.o\
	dbfopen.o\
	func_shapefile.o\
	lib_io_shapefile.o

MODS = \
	lib_io_shapefile.mod


.PHONY: all
all:
	$(MAKE) $(OBJS)
	$(INSTALL) $(OBJS) $(MODS) ../..

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod

