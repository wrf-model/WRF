/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/

#define MAXDOM_MAKE 1

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

/*#define A(I) b->[I]*/
#define A(I) x[I]

invoke_pf( nflds, f,
           parent_nl, child_nl,
	   p_i, p_j, p_ig, p_jg,
	   n_ig, n_jg,
	   cm1, cn1, buf, n )
  int nflds ;
  int (*f)() ;
  int_p    parent_nl, child_nl,
	   p_i, p_j, p_ig, p_jg,
	   n_ig, n_jg,
	   cm1, cn1, buf, n ;
{
  int x[900] ;		/* remove -- testing only */

  RSL_TEST_ERR( nflds > 50 || nflds < 0, "nflds out of range") ;
  /* the following case statement was genenerated by the script
     generate_invoke.csh (I did not type this in by hand!) */
  switch (nflds)
  {
case     0 : (*f)() ; break ;
case     1 : (*f)(A(0)); break ;
case     2 : (*f)(A(0),A(1)); break ;
case     3 : (*f)(A(0),A(1),A(2)); break ;
case     4 : (*f)(A(0),A(1),A(2),A(3)); break ;
case     5 : (*f)(A(0),A(1),A(2),A(3),A(4)); break ;
case     6 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5)); break ;
case     7 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6)); break ;
case     8 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7)); break ;
case     9 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8)); break ;
case    10 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9)); break ;
case    11 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10)); break ;
case    12 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11)); break ;
case    13 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12)); break ;
case    14 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13)); break ;
case    15 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14)); break ;
case    16 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15)); break ;
case    17 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16)); break ;
case    18 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17)); break ;
case    19 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18)); break ;
case    20 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19)); break ;
case    21 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20)); break ;
case    22 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21)); break ;
case    23 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22)); break ;
case    24 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23)); break ;
case    25 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24)); break ;
case    26 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25)); break ;
case    27 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26)); break ;
case    28 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27)); break ;
case    29 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28)); break ;
case    30 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29)); break ;
case    31 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30)); break ;
case    32 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31)); break ;
case    33 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32)); break ;
case    34 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33)); break ;
case    35 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34)); break ;
case    36 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35)); break ;
case    37 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36)); break ;
case    38 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37)); break ;
case    39 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38)); break ;
case    40 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39)); break ;
case    41 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40)); break ;
case    42 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41)); break ;
case    43 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42)); break ;
case    44 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43)); break ;
case    45 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44)); break ;
case    46 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44),A(45)); break ;
case    47 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44),A(45),A(46)); break ;
case    48 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44),A(45),A(46),A(47)); break ;
case    49 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44),A(45),A(46),A(47),A(48)); break ;
case    50 : (*f)(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),A(41),A(42),A(43),A(44),A(45),A(46),A(47),A(48),A(49)); break ;

    default:
	RSL_TEST_ERR(1,"bad number of arguments") ;
  }
}

