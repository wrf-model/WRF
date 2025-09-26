/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    mova2i      Moves a bit string from a char*1 to int
C   PRGMMR: Gilbert          ORG: W/NP11     DATE: 02-08-15
C
C ABSTRACT: This Function copies a bit string from a Character*1 variable
C   to an integer variable.  It is intended to replace the Fortran Intrinsic
C   Function ICHAR, which only supports 0 <= ICHAR(a) <= 127 on the
C   IBM SP.  If "a" is greater than 127 in the collating sequence,
C   ICHAR(a) does not return the expected bit value.
C   This function can be used for all values 0 <= ICHAR(a) <= 255.
C
C PROGRAM HISTORY LOG:
C   98-12-15  Gilbert
C
C USAGE:     I = mova2i(a)
C
C   INPUT ARGUMENT :
C
C          a - Character*1 variable that holds the bitstring to extract
C
C   RETURN ARGUMENT :
C
C          mova2i - Integer value of the bitstring in character a
C
C REMARKS:
C
C      None
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  IBM SP

C
C$$$i*/

#include "proto.h"

int MOVA2I(unsigned char *a)
{
    return (int)(*a);
}
