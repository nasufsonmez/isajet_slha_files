SOURCES := $(wildcard *.f)
OBJECTS = $(SOURCES:.f=.o)

# FFLAGS is set in the top-level Makefile
# -I directories are separate to get relative path correct
IFLAGS = -I../isainc

all: libisared.a

libisared.a: $(OBJECTS)
	ar r libisared.a $(OBJECTS)

.f.o:
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm *.o libisared.a
