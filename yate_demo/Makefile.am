SUBDIRS = src

all-local:
	test -L ebin || ln -s src ebin
#	test -L priv || ln -s c_src priv

clean-local:
	-test -L ebin && $(RM) ebin
#	-test -L priv && $(RM) priv
