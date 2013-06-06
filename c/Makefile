CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -fPIC
LDFLAGS = -shared -Wl,-soname,libsg.so.1 -lc
INCLUDES = -I.

LONG_VER=1.0.0
TARGET=libsg.so.$(LONG_VER)
SHORT_NAME=libsg.so

INSTALL_PREFIX=/usr

SOURCES = sgfile.c sgbitmap.c sgimage.c utils.c
HEADERS = $(SOURCES:.c=.h)

$(TARGET): $(SOURCES:.c=.o)
	$(CC) $(LDFLAGS) $(SOURCES:.c=.o) -o $(TARGTE)

sgfile.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c sgfile.c -o sgfile.o
sgbitmap.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c sgbitmap.c -o sgbitmap.o
sgimage.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c sgimage.c -o sgimage.o
utils.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c utils.c -o utils.o

install: $(TARGET)
	@install -m755 $(TARGET) $(INSTALL_PREFIX)/lib
	@mkdir $(INSTALL_PREFIX)/include/sg -p
	@install -m644 $(HEADERS:%.h=sg/%.h) -t $(INSTALL_PREFIX)/include/sg
	ln -s $(INSTALL_PREFIX)/lib/$(TARGET) $(INSTALL_PREFIX)/lib/$(SHORT_NAME)
