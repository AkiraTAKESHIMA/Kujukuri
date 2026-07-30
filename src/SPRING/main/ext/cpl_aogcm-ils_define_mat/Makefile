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
BINDIR = $(TOPSRCDIR)/../bin/ext
BIN = $(BINDIR)/cpl_aogcm-ils_define_mat.exe

OBJ_F = \
def_const.o\
def_type.o\
mod_set.o\
mod_check_input.o\
mod_define_mat.o\
mod_finalize.o\
main.o

OBJ_ALL = $(OBJ_F)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

.PHONY: install
install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)

$(TARGET): $(OBJ_ALL) $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

