SUBDIRS = yate .

ACLOCAL_AMFLAGS = -I m4

all-local:
	sinan build

clean-local:
	sinan clean

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name yate
