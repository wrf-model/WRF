#ifndef ADSTACK_LOADED
#define ADSTACK_LOADED 1

/** Push of int (4 bytes int) array */
extern void pushInteger4Array(int *x, int n) ;

/** Pop of int (4 bytes int) array */
extern void popInteger4Array(int *x, int n) ;

/** Push of long (8 bytes int) array */
extern void pushInteger8Array(long *x, int n) ;

/** Pop of long (8 bytes int) array */
extern void popInteger8Array(long *x, int n) ;

/** Push of float (4 bytes real) array */
extern void pushReal4Array(float *x, int n) ;

/** Pop of float (4 bytes real) array */
extern void popReal4Array(float *x, int n) ;

/** Push of double (8 bytes real) array */
extern void pushReal8Array(double *x, int n) ;

/** Pop of double (8 bytes real) array */
extern void popReal8Array(double *x, int n) ;

/** Push of char (byte) array */
extern void pushCharacterArray(char *x, int n) ;

/** Pop of char (byte) array */
extern void popCharacterArray(char *x, int n) ;

/*** There is no primitive complex type in C ***/
/* extern void pushComplex8Array(ccmplx *x, int n) ; */
/* extern void popComplex8Array(ccmplx *x, int n) ; */
/* extern void pushComplex16Array(cdcmplx *x, int n) ; */
/* extern void popComplex16Array(cdcmplx *x, int n) ; */

/** Utility for adBuffer.c (mostly)
 * Push an array of any type onto the main stack */
extern void pushNArray(char *x, unsigned int nbChars, int checkReadOnly) ;

/** Utility for adBuffer.c (mostly)
 * Pop an array of any type from the main stack */
extern void popNArray(char *x, unsigned int nbChars, int checkReadOnly) ;

/** Display the maximum size reached by the main AD stack */
extern void adStack_showPeakSize() ;

/** Utility for adBuffer.c (mostly)
 * Display the total amount of memory pushed */
extern void showTotalTraffic(unsigned long long int localtraffic) ;

/** Utility for adBuffer.c (mostly)
 * Display in detail the contents of the AD stack */
extern void showStack() ;

/** Utility for adBuffer.c */
extern void showStackSize(int i4i, int i8i, int r4i, int r8i, int c8i, int c16i, int s1i, int biti, int ptri) ;

/** Utility for adBuffer.c */
extern void startStackRepeat1() ;

/** Utility for adBuffer.c */
extern void startStackRepeat2() ;

/** Utility for adBuffer.c */
extern void resetStackRepeat1() ;

/** Utility for adBuffer.c */
extern void resetStackRepeat2() ;

/** Utility for adBuffer.c */
extern void endStackRepeat() ;

#endif
