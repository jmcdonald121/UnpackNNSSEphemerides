# Unpack NNSS Ephemerides
An algorithm that decompresses NNSS (Transit) satellite precise ephemerides data.
This FORTRAN program takes compressed precise ephemerides satellite data (at 80 characters per line) and expands it to 102 characters per line, which is the normal format of the precise ephemeris.

This program is used to reconstruct the precise ephmeris when it is transferred from the Mainframe to the IBM PC-AT. The precise ephemeris on the Mainframe is compresses by taking all the blank characters our of the 102 character per line precise ephemeris file and writing into a 80 character per line file. This helps in tranferring the file as efficiently as possible using the file tranfer program KERMIT.This program then restores the precise epehmeris to its correct 102 character per line format.
