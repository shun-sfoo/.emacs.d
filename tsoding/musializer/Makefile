CFLAGS = $(shell pkg-config --cflags raylib) -Wall -Wextra
LIBS = $(shell pkg-config --libs raylib) -lm -lpthread

debug: main.c libplug.so fft
	cc $(CFLAGS) -DHOTRELOAD -o musializer main.c  $(LIBS)

release: main.c libplug.so fft
	cc $(CFLAGS) -o musializer main.c plug.c $(LIBS)

libplug.so: plug.c
	cc $(CFLAGS) -fPIC -shared plug.c $(LIBS) -o libplug.so

fft: fft.c
	cc $(CFLAGS) -o fft fft.c $(LIBS)

clean:
	rm fft libplug.so musializer

