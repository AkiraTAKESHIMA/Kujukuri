TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR) -I$(CMNDIR1) -I$(CMNDIR2) -I$(CMNDIR3)
%.mod: %.f90 %.o
	@:

OBJS = \
mod_io.o\
mod_make_basin.o\
main.o

TARGET = main

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(CMNDIR3)/*.o $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


