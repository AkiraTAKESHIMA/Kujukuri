TOPSRCDIR = ../../..
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

TARGET = main.exe
BINDIR = $(TOPSRCDIR)/../bin/std
BIN = $(BINDIR)/make_grid_data.exe

OBJ_F = \
def_type.o\
mod_set.o\
mod_main.o\
mod_finalize.o\
main.o

OBJ_ALL = $(OBJ_F)

all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_ALL) $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

