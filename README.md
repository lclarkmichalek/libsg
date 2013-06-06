LibSG
=====

A library for reading SG files. SG files are the data files that the art assets
for the City Builder games (Zeus, Caesar 3, Pharaoh etc) are stored in. The core
library is written in C and can be found in the "c" sub directory. It is based
on the SgReader program written by Pecunia. I simply removed the Qt dependency
and converted it to pure C.

Building
--------

    cd c
    make
    # Install to /usr
    sudo make install

The Makefile is pretty dysfunctional, contributions welcome.
