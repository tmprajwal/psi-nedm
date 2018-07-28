

#
# If pkg-config isn't installed on your system, comment the following lines and
# set the fasterac flags as indicated with your own paths:
#
# FASTERAC_CFLAGS = -I/usr/include
# FASTERAC_LIBS   = -L/usr/lib -lfasterac
#
FASTERAC_CFLAGS = $(shell pkg-config --cflags libfasterac)
FASTERAC_LIBS   = $(shell pkg-config --libs   libfasterac)
#


CC     = gcc
CFLAGS = ${FASTERAC_CFLAGS} -Wall
LIBS   = ${FASTERAC_LIBS} -lm
SRCEXE = $(shell ls *.c)
EXE    = $(SRCEXE:.c=)


all : $(EXE)

$(EXE): $(SRCEXE)
	${CC} $@.c -o $@ ${INCFLAGS} ${CFLAGS} ${LIBS}

test : $(EXE)
	@echo "------------------------------------------------------"
	./rate_example /usr/share/fasterac/data/small.fast
	@echo "------------------------------------------------------"
	./reader_example /usr/share/fasterac/data/small.fast
	@echo "------------------------------------------------------"

clean :
	rm -f *.o
	rm -f $(EXE)


