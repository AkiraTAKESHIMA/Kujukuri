
all:\
  make_lib\
  make_common\
  make_main\
  make_libspring\
  make_sample_libspring\
  make_ext\
  make_test

make_lib:
	$(MAKE) -C lib

make_common:
	$(MAKE) -C common

make_main:
	$(MAKE) -C main

make_libspring:
	$(MAKE) -C libspring

make_sample_libspring:
	$(MAKE) -C sample_libspring

make_ext:
	$(MAKE) -C ext

make_test:
	$(MAKE) -C test


install:\
  install_main

install_main:
	$(MAKE) -C main install


clean:\
  clean_lib\
  clean_common\
  clean_main\
  clean_libspring\
  clean_sample_libspring\
  clean_ext\
  clean_test


clean_lib:
	$(MAKE) -C lib clean

clean_common:
	$(MAKE) -C common clean

clean_main:
	$(MAKE) -C main clean

clean_libspring:
	$(MAKE) -C libspring clean

clean_sample_libspring:
	$(MAKE) -C sample_libspring clean

clean_ext:
	$(MAKE) -C ext clean

clean_test:
	$(MAKE) -C test clean
