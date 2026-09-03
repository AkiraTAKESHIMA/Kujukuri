TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude
include $(TOPSRCDIR)/make_data/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(SPRINGMODDIR) -I$(CMNDIR1) -I$(CMNDIR2) -I$(CMNDIR3)
%.mod: %.f90 %.o
	@:

OBJS = \
mod_estimate_cross_section.o\
main.o

TARGET = main.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(CMNDIR3)/*.o $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(SPRINGLIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


