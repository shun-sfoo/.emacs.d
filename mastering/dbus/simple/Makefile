CXX = g++
CXXFLAGS = -std=c++17 $(shell pkg-config --cflags dbus-1) -Wall
LDFLAGS = $(shell pkg-config --libs dbus-1)

TARGET = client_simple
SRC = client_simple.cpp
HDR = dbus_simple.h

all: $(TARGET)

$(TARGET): $(SRC) $(HDR)
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f $(TARGET)

.PHONY: all clean
