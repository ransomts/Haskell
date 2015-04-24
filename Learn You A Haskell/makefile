CC=ghc
CFLAGS= --make -O2 -threaded

#all: foo bar

foo: foo.hs
	$(CC) $(CFLAGS) foo.hs

bar: bar.hs
	$(CC) $(CFLAGS) bar.hs

all: clean

clean:
	rm -rf *.o *.hi # foo bar
