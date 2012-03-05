/*
 * These are common definitions and macros.
 *
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 */

#ifndef COMMON_HDR_ALREADY_INCLUDED
# define COMMON_HDR_ALREADY_INCLUDED

#ifndef TRUE
# define TRUE 1
#endif

#ifndef FALSE
# define FALSE 0
#endif

#define max(x,y)		((x)>(y)?(x):(y))
#define min(x,y)		((x)<(y)?(x):(y))
#define absval(x)		((x)<0?-(x):(x))

/* Useful constants: */
#define		PI		(3.141592653589793)
#define		SR2		(1.414213562373095) /* sqrt(2.0)  */
#define		SRH		(0.707106781186547) /* sqrt(0.5)  */

#endif /* COMMON_HDR_ALREADY_INCLUDED */
