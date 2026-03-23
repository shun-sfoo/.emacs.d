CFLAGS = $(shell pkg-config --cflags yaml-0.1) -Wall -Wextra
LDFLAGS = $(shell pkg-config --libs yaml-0.1)

TARGET = yaml_ecoding
SRC = yah_yaml_encoding.c
HDR = yah_yaml.h

$(TARGET): $(SRC) $(HDR)
	cc -o $(TARGET) $(SRC) $(CFLAGS) $(LDFLAGS)

clean:
	rm -rf $(TARGET) foo.yaml

.PHONY: clean
