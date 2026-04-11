
LDFLAGS = $(shell pkg-config --libs raylib)


main: main.c
	cc -o main main.c $(LDFLAGS) -lm -Wall -Wextra
