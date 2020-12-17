/* A Bison parser, made by GNU Bison 2.1.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EOSTMT = 258,
     ASSIGNMENT_OP = 259,
     GARBAGE = 260,
     CPP_INCLUDE = 261,
     F90PPR_INCLUDE = 262,
     COCO_INCLUDE = 263,
     F90PPR_DEFINE = 264,
     CPP_DEFINE = 265,
     F90PPR_UNDEF = 266,
     CPP_UNDEF = 267,
     CPP_IFDEF = 268,
     CPP_IFNDEF = 269,
     CPP_IF = 270,
     CPP_ELSE = 271,
     CPP_ELIF = 272,
     CPP_ENDIF = 273,
     F90PPR_IFDEF = 274,
     F90PPR_IFNDEF = 275,
     F90PPR_IF = 276,
     F90PPR_ELSE = 277,
     F90PPR_ELIF = 278,
     F90PPR_ENDIF = 279,
     CPP_TOENDL = 280,
     UNTERMINATED_STRING = 281,
     STRING = 282,
     WORD = 283
   };
#endif
/* Tokens.  */
#define EOSTMT 258
#define ASSIGNMENT_OP 259
#define GARBAGE 260
#define CPP_INCLUDE 261
#define F90PPR_INCLUDE 262
#define COCO_INCLUDE 263
#define F90PPR_DEFINE 264
#define CPP_DEFINE 265
#define F90PPR_UNDEF 266
#define CPP_UNDEF 267
#define CPP_IFDEF 268
#define CPP_IFNDEF 269
#define CPP_IF 270
#define CPP_ELSE 271
#define CPP_ELIF 272
#define CPP_ENDIF 273
#define F90PPR_IFDEF 274
#define F90PPR_IFNDEF 275
#define F90PPR_IF 276
#define F90PPR_ELSE 277
#define F90PPR_ELIF 278
#define F90PPR_ENDIF 279
#define CPP_TOENDL 280
#define UNTERMINATED_STRING 281
#define STRING 282
#define WORD 283




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 65 "find_dep.y"
typedef union YYSTYPE {
    char *string;
    int number;
} YYSTYPE;
/* Line 1447 of yacc.c.  */
#line 99 "y.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



