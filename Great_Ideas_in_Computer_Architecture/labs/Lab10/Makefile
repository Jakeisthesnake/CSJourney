CC=gcc
CFLAGS=-fopenmp -g -std=gnu99 -pthread
LDFLAGS=-fopenmp -lm

all: v_add hello dotp

v_add: v_add.o
	$(CC) -o $@ $^ $(LDFLAGS)

hello: hello.o
	$(CC) -o $@ $^ $(LDFLAGS)

dotp: dotp.o
	$(CC) -o $@ $^ $(LDFLAGS)

clean:
	rm -f *.o dotp hello v_add