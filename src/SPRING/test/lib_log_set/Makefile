TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = main.exe

OBJ_F = \
mod_main.o\
main.o

OBJ_ALL = $(OBJ_F)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_ALL) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

