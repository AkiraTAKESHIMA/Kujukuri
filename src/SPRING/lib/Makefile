TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:

TARGET = $(LIB)

OBJ =\
$(LIBDIR_CONST)/*.o\
$(LIBDIR_BASE)/*.o\
$(LIBDIR_TIME)/*.o\
$(LIBDIR_LOG)/*.o\
$(LIBDIR_UTIL)/*.o\
$(LIBDIR_ARRAY)/*.o\
$(LIBDIR_MATH)/*.o\
$(LIBDIR_STATS)/*.o\
$(LIBDIR_IO)/*.o\
$(LIBDIR_PROJ)/*.o

.PHONY: all
all: 
	$(MAKE) -C $(LIBDIR_CONST)
	$(MAKE) -C $(LIBDIR_BASE)
	$(MAKE) -C $(LIBDIR_TIME)
	$(MAKE) -C $(LIBDIR_LOG)
	$(MAKE) -C $(LIBDIR_UTIL)
	$(MAKE) -C $(LIBDIR_ARRAY)
	$(MAKE) -C $(LIBDIR_MATH)
	$(MAKE) -C $(LIBDIR_STATS)
	$(MAKE) -C $(LIBDIR_IO)
	$(MAKE) -C $(LIBDIR_PROJ)

	$(RM) $(RMFLAGS) $(MODDIR)/*.mod && $(MKDIR) $(MKDIRFLAGS) $(MODDIR)
	$(INSTALL) $(LIBDIR_CONST)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_BASE)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_TIME)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_LOG)/*.mod    $(MODDIR)/.
	$(INSTALL) $(LIBDIR_UTIL)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_ARRAY)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_MATH)/*.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_STATS)/*.mod  $(MODDIR)/.
	$(INSTALL) $(LIBDIR_IO)/*.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_PROJ)/*.mod   $(MODDIR)/.

	$(MAKE) $(MKFLAGS) $(TARGET)

$(TARGET): $(OBJ)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(MAKE) -C $(LIBDIR_CONST)  clean
	$(MAKE) -C $(LIBDIR_BASE)   clean
	$(MAKE) -C $(LIBDIR_TIME)   clean
	$(MAKE) -C $(LIBDIR_LOG)    clean
	$(MAKE) -C $(LIBDIR_UTIL)   clean
	$(MAKE) -C $(LIBDIR_ARRAY)  clean
	$(MAKE) -C $(LIBDIR_MATH)   clean
	$(MAKE) -C $(LIBDIR_STATS)  clean
	$(MAKE) -C $(LIBDIR_IO)     clean
	$(MAKE) -C $(LIBDIR_PROJ)   clean
	$(RM) $(RMFLAGS) $(MODDIR)/*.mod $(TARGET)
