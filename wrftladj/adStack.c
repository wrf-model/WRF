#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* The size of a BLOCK in characters. Suggested 16384. Should try 2^16=65536 */
#define ONE_BLOCK_SIZE 65536

/* The main stack is a double-chain of DoubleChainedBlock objects.
 * Each DoubleChainedBlock holds an array[ONE_BLOCK_SIZE] of char. */
typedef struct _DoubleChainedBlock{
  unsigned int rank ;
  struct _DoubleChainedBlock *prev ;
  char                       *contents ;
  struct _DoubleChainedBlock *next ;
} DoubleChainedBlock ;

char initContents[ONE_BLOCK_SIZE] = {'\0'} ;
DoubleChainedBlock initBlock = {0,NULL,initContents,NULL} ;
static DoubleChainedBlock *curStack = &initBlock ;
static char               *curStackTop = initContents ;

static unsigned long int maintraffic = 0 ;

void setCurLocation(unsigned long int location) {
  unsigned int targetRank = (unsigned int)location/ONE_BLOCK_SIZE ;
  unsigned int targetOffset = (unsigned int)location%ONE_BLOCK_SIZE ;
  if (targetRank>curStack->rank)
    while (targetRank>curStack->rank) curStack = curStack->next ;
  else if (targetRank<curStack->rank)
    while (targetRank<curStack->rank) curStack = curStack->prev ;
  curStackTop = curStack->contents + targetOffset ;
}

unsigned long int getCurLocation() {
  return (curStackTop-curStack->contents)+curStack->rank*ONE_BLOCK_SIZE ;
}

void showLocation(unsigned long int location) {
  printf("%1i.%05i", (unsigned int)location/ONE_BLOCK_SIZE, (unsigned int)location%ONE_BLOCK_SIZE) ;
}

/*************** REPEATED ACCESS MECHANISM *********************/

typedef struct _StackRepeatCell {
  int hasBackPop ;
  unsigned long int backPop ;
  unsigned long int resume ;
  unsigned long int freePush ;
  struct _StackRepeatCell *previous ;
} StackRepeatCell ;

StackRepeatCell *stackRepeatTop = NULL ;

void showStackRepeatsRec(StackRepeatCell *inRepeatStack) {
  if (inRepeatStack->previous) {showStackRepeatsRec(inRepeatStack->previous) ; printf(" ; ") ;}
  printf("<") ;
  if (inRepeatStack->hasBackPop) showLocation(inRepeatStack->backPop) ;
  printf("|") ;
  showLocation(inRepeatStack->resume) ;
  printf("|") ;
  showLocation(inRepeatStack->freePush) ;
  printf(">") ;
}

void showStackRepeats() {
  showStackRepeatsRec(stackRepeatTop) ;
}

void showStack() {
  DoubleChainedBlock *inStack = &initBlock ;
  int i ;
  while (inStack) {
    printf("[%1i] ",inStack->rank) ;
    for (i=0 ; i<ONE_BLOCK_SIZE ; ++i) {
      if (i!=0 && i%4==0) printf(".") ;
      if (inStack==curStack && &(inStack->contents[i])==curStackTop) printf(" | ") ;
      printf("%02x",(unsigned char)inStack->contents[i]) ;
    }
    inStack = inStack->next ;
    if (inStack) printf("\n        ") ;
  }
  printf("\n        REPEATS:") ;
  if (stackRepeatTop)
    showStackRepeats() ;
  else
    printf(" none!") ;
  printf("\n") ;
}

void showStackSize(int i4i, int i8i, int r4i, int r8i, int c8i, int c16i, int s1i, int biti, int ptri) {
  printf(" --> <") ;
  showLocation(getCurLocation()) ;
  printf(">%1i.%1i.%1i.%1i.%1i.%1i.%1i.%1i.%1i\n",i4i, i8i, r4i, r8i, c8i, c16i, s1i, biti, ptri) ;
}

void adStack_showPeakSize() {
  DoubleChainedBlock *inStack = &initBlock ;
  int i = 0 ;
  while (inStack) {
    inStack = inStack->next ;
    ++i ;
  }
  printf("Peak stack size (%1i blocks): %1llu bytes\n",
         i, ((long long int)i)*((long long int)ONE_BLOCK_SIZE)) ;
}

void showTotalTraffic(unsigned long long int localtraffic) {
  printf("Total pushed traffic %1llu bytes\n", maintraffic+localtraffic) ;
}

/** If we are in a protected, read-only section, memorize location as "backPop"
 * and go to the "freePush" location */
void checkPushInReadOnly() {
  if (stackRepeatTop) {
    unsigned long int current = getCurLocation() ;
    if (current<stackRepeatTop->freePush) {
      stackRepeatTop->hasBackPop = 1 ;
      stackRepeatTop->backPop = current ;
      setCurLocation(stackRepeatTop->freePush) ;
/*       printf(" FREEPUSH(") ;                   //Trace */
/*       showLocation(stackRepeatTop->backPop) ;  //Trace */
/*       printf("=>") ;                           //Trace */
/*       showLocation(stackRepeatTop->freePush) ; //Trace */
/*       printf(")") ;                            //Trace */
    }
  }
}

/** If current location is the "freePush" location,
 * go back to its "backPop" location, which is in a protected, read-only section */
void checkPopToReadOnly() {
  if (stackRepeatTop && stackRepeatTop->hasBackPop) {
    unsigned long int current = getCurLocation() ;
    if (current==stackRepeatTop->freePush) {
      setCurLocation(stackRepeatTop->backPop) ;
      stackRepeatTop->hasBackPop = 0 ;
/*       printf(" BACKPOP(") ;                    //Trace */
/*       showLocation(stackRepeatTop->freePush) ; //Trace */
/*       printf("=>") ;                           //Trace */
/*       showLocation(stackRepeatTop->backPop) ;  //Trace */
/*       printf(")") ;                            //Trace */
    }
  }
}

// A global for communication from startStackRepeat1() to startStackRepeat2():
StackRepeatCell *newRepeatCell = NULL ;

void startStackRepeat1() {
  // Create (push) a new "stack" repeat level:
  newRepeatCell = (StackRepeatCell *)malloc(sizeof(StackRepeatCell)) ;
  newRepeatCell->previous = stackRepeatTop ;
  newRepeatCell->hasBackPop = 0 ;
  // Store current location as the "resume" location:
  unsigned long int current = getCurLocation() ;
  newRepeatCell->resume = current ;
  // Move to the "freePush" location if there is one:
  if (stackRepeatTop && current<stackRepeatTop->freePush)
    setCurLocation(stackRepeatTop->freePush) ;
}

void startStackRepeat2() {
  // Store current stack location as the "freePush" location:
  newRepeatCell->freePush = getCurLocation() ;
  // Reset current location to stored "resume" location:
  setCurLocation(newRepeatCell->resume) ;
  // Make this new repeat level the current repeat level:
  stackRepeatTop = newRepeatCell ;
/*   printf("\n+Rep ") ; showStackRepeats() ; printf("\n") ; //Trace */
}

void resetStackRepeat1() {
/*   printf("\n>Rep ") ; showStackRepeats() ; printf("\n") ; //Trace */
  // If we are in a nested checkpoint, force exit from it:
  if (stackRepeatTop->hasBackPop) {
    //setCurLocation(stackRepeatTop->backPop) ; //correct but useless code
    stackRepeatTop->hasBackPop = 0 ;
  }
  // Go to repeat location of current repeat level
  setCurLocation(stackRepeatTop->freePush) ;
}

void resetStackRepeat2() {
  // Reset current location to "ResumeLocation":
  setCurLocation(stackRepeatTop->resume) ;
}

void endStackRepeat() {
/*   printf("\n-Rep ") ; showStackRepeats() ; printf("\n") ; //Trace */
  // If we are in a nested checkpoint, go back to its "backPop" (read-only) location:
  if (stackRepeatTop->hasBackPop) {
    setCurLocation(stackRepeatTop->backPop) ;
    //stackRepeatTop->hasBackPop = 0 ; //correct but useless code
  }
  // Remove (pop) top "stack" repeat level:
  StackRepeatCell *oldRepeatCell = stackRepeatTop ;
  stackRepeatTop = stackRepeatTop->previous ;
  free(oldRepeatCell) ;
  // current location may have moved back ; check if we must move further back:
  checkPopToReadOnly() ;
}

/******************* PUSH/POP MECHANISM *******************/

/* PUSHes "nbChars" consecutive chars from a location starting at address "x".
 * Checks that there is enough space left to hold "nbChars" chars.
 * Otherwise, allocates the necessary space. */
void pushNArray(char *x, unsigned int nbChars, int checkReadOnly) {
  if (checkReadOnly) checkPushInReadOnly() ;
  if (checkReadOnly) maintraffic += nbChars ;
/* unsigned long int lfrom = getCurLocation() ; //Trace */
  unsigned int nbmax = ONE_BLOCK_SIZE-(curStackTop-(curStack->contents)) ;
  if (nbChars <= nbmax) {
    memcpy(curStackTop,x,nbChars) ;
    curStackTop+=nbChars ;
  } else {
    char *inx = x+(nbChars-nbmax) ;
    if (nbmax>0) memcpy(curStackTop,inx,nbmax) ;
    while (inx>x) {
      if (curStack->next)
        curStack = curStack->next ;
      else {
        /* Create new block: */
	DoubleChainedBlock *newStack ;
	char *contents = (char *)malloc(ONE_BLOCK_SIZE*sizeof(char)) ;
	newStack = (DoubleChainedBlock*)malloc(sizeof(DoubleChainedBlock)) ;
	if ((contents == NULL) || (newStack == NULL)) {
	  DoubleChainedBlock *stack = curStack ;
	  int nbBlocks = (stack?-1:0) ;
	  while(stack) {
	      stack = stack->prev ;
	      nbBlocks++ ;
	  }
	  printf("Out of memory (allocated %i blocks of %i bytes)\n",
		 nbBlocks, ONE_BLOCK_SIZE) ;
          exit(0);
	}
        curStack->next = newStack ;
	newStack->prev = curStack ;
        newStack->rank = curStack->rank + 1 ;
	newStack->next = NULL ;
	newStack->contents = contents ;
	curStack = newStack ;
        /* new block created! */
      }
      inx -= ONE_BLOCK_SIZE ;
      if(inx>x)
        memcpy(curStack->contents,inx,ONE_BLOCK_SIZE) ;
      else {
        unsigned int nbhead = (inx-x)+ONE_BLOCK_SIZE ;
        curStackTop = curStack->contents ;
        memcpy(curStackTop,x,nbhead) ;
        curStackTop += nbhead ;
      }
    }
  }
/* unsigned long int lto = getCurLocation() ; //Trace */
/* printf("pushNArray(") ;                    //Trace */
/* showLocation(lfrom) ;                      //Trace */
/* printf("=>") ;                             //Trace */
/* showLocation(lto) ;                        //Trace */
/* printf(")") ;                              //Trace */
}

/* POPs "nbChars" consecutive chars to a location starting at address "x".
 * Checks that there is enough data to fill "nbChars" chars.
 * Otherwise, pops as many blocks as necessary. */
void popNArray(char *x, unsigned int nbChars, int checkReadOnly) {
/* unsigned long int lfrom = getCurLocation() ; //Trace */
  unsigned int nbmax = curStackTop-(curStack->contents) ;
  if (nbChars <= nbmax) {
    curStackTop-=nbChars ;
    memcpy(x,curStackTop,nbChars);
  } else {
    char *tlx = x+nbChars ;
    if (nbmax>0) memcpy(x,curStack->contents,nbmax) ;
    x+=nbmax ;
    while (x<tlx) {
      curStack = curStack->prev ;
      if (curStack==NULL) printf("Popping from an empty stack!!!\n") ;
      if (x+ONE_BLOCK_SIZE<tlx) {
	memcpy(x,curStack->contents,ONE_BLOCK_SIZE) ;
	x += ONE_BLOCK_SIZE ;
      } else {
	unsigned int nbtail = tlx-x ;
	curStackTop = (curStack->contents)+ONE_BLOCK_SIZE-nbtail ;
	memcpy(x,curStackTop,nbtail) ;
	x = tlx ;
      }
    }
  }
/* unsigned long int lto = getCurLocation() ; //Trace */
/* printf("popNArray(") ;                     //Trace */
/* showLocation(lfrom) ;                      //Trace */
/* printf("=>") ;                             //Trace */
/* showLocation(lto) ;                        //Trace */
/* printf(")") ;                              //Trace */
  if (checkReadOnly) checkPopToReadOnly() ;
}

typedef struct {float r,i;} ccmplx ;
typedef struct {double dr, di;} cdcmplx ;

void pushInteger4Array(int *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*4), 1) ;
}

void popInteger4Array(int *x, int n) {
  popNArray((char *)x,(unsigned int)(n*4), 1) ;
}

void pushInteger8Array(long *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void popInteger8Array(long *x, int n) {
  popNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void pushReal4Array(float *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*4), 1) ;
}

void popReal4Array(float *x, int n) {
  popNArray((char *)x,(unsigned int)(n*4), 1) ;
}

void pushReal8Array(double *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void popReal8Array(double *x, int n) {
  popNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void pushComplex8Array(ccmplx *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void popComplex8Array(ccmplx *x, int n) {
  popNArray((char *)x,(unsigned int)(n*8), 1) ;
}

void pushComplex16Array(cdcmplx *x, int n) {
  pushNArray((char *)x,(unsigned int)(n*16), 1) ;
}

void popComplex16Array(cdcmplx *x, int n) {
  popNArray((char *)x,(unsigned int)(n*16), 1) ;
}

void pushCharacterArray(char *x, int n) {
  pushNArray(x,(unsigned int)n, 1) ;
}

void popCharacterArray(char *x, int n) {
  popNArray(x,(unsigned int)n, 1) ;
}

/* ********* Useful only for testpushpop.f90. Should go away! ********* */

void showpushpopsequence_(int *op, int *index, int* nbobjects, int* sorts, int* sizes) {
  char *prefix = "" ;
  if (*op==1) prefix = "+" ;
  else if (*op==-1) prefix = "-" ;
  else if (*op==2) prefix = "+s" ;
  else if (*op==-2) prefix = "-s" ;
  else if (*op==-3) prefix = "Ls" ;
  printf("%s%02i", prefix, *index) ;
  // Comment the rest for compact display:
  printf(":") ;
  int i ;
  for (i=0 ; i<*nbobjects ; ++i) {
    switch (sorts[i]) {
    case 1:
      printf(" I4") ;
      break ;
    case 2:
      printf(" I8") ;
      break ;
    case 3:
      printf(" R4") ;
      break ;
    case 4:
      printf(" R8") ;
      break ;
    case 5:
      printf(" C8") ;
      break ;
    case 6:
      printf(" C16") ;
      break ;
    case 7:
      printf(" char") ;
      break ;
    case 8:
      printf(" bit") ;
      break ;
    case 9:
      printf(" PTR") ;
      break ;
    }
    if (sizes[i]!=0) printf("[%1i]",sizes[i]) ;
  }
}

/****************** INTERFACE CALLED FROM FORTRAN *******************/

void showstack_() {
  showStack() ;
}

void showstacksize_(int *i4i, int *i8i, int *r4i, int *r8i, int *c8i, int *c16i, int *s1i, int *biti, int *ptri) {
  showStackSize(*i4i,*i8i,*r4i,*r8i,*c8i,*c16i,*s1i,*biti,*ptri) ;
}

void adstack_showpeaksize_() {
  adStack_showPeakSize() ;
}

void adstack_showpeaksize__() {
  adStack_showPeakSize() ;
}

void showtotaltraffic_(unsigned long long int *traffic) {
  showTotalTraffic(*traffic) ;
}

void startstackrepeat1_() {
  startStackRepeat1() ;
}

void startstackrepeat2_() {
  startStackRepeat2() ;
}

void resetstackrepeat1_() {
  resetStackRepeat1() ;
}

void resetstackrepeat2_() {
  resetStackRepeat2() ;
}

void endstackrepeat_() {
  endStackRepeat() ;
}

void pushnarray_(char *x, unsigned int *nbChars, int *checkReadOnly) {
  pushNArray(x, *nbChars, *checkReadOnly) ;
}

void popnarray_(char *x, unsigned int *nbChars, int *checkReadOnly) {
  popNArray(x, *nbChars, *checkReadOnly) ;
}

void pushinteger4array_(int *ii, int *ll) {
  pushInteger4Array(ii, *ll) ;
}

void popinteger4array_(int *ii, int *ll) {
  popInteger4Array(ii, *ll) ;
}

void pushinteger8array_(long *ii, int *ll) {
  pushInteger8Array(ii, *ll) ;
}

void popinteger8array_(long *ii, int *ll) {
  popInteger8Array(ii, *ll) ;
}

void pushreal4array_(float *ii, int *ll) {
  pushReal4Array(ii, *ll) ;
}

void popreal4array_(float *ii, int *ll) {
  popReal4Array(ii, *ll) ;
}

void pushreal8array_(double *ii, int *ll) {
  pushReal8Array(ii, *ll) ;
}

void popreal8array_(double *ii, int *ll) {
  popReal8Array(ii, *ll) ;
}

void pushcomplex8array_(ccmplx *ii, int *ll) {
  pushComplex8Array(ii, *ll) ;
}

void popcomplex8array_(ccmplx *ii, int *ll) {
  popComplex8Array(ii, *ll) ;
}

void pushcomplex16array_(cdcmplx *ii, int *ll) {
  pushComplex16Array(ii, *ll) ;
}

void popcomplex16array_(cdcmplx *ii, int *ll) {
  popComplex16Array(ii, *ll) ;
}

void pushcharacterarray_(char *ii, int *ll) {
  pushCharacterArray(ii, *ll) ;
}

void popcharacterarray_(char *ii, int *ll) {
  popCharacterArray(ii, *ll) ;
}

void pushbooleanarray_(char *x, unsigned int *n) {
  pushNArray(x,(*n*4), 1) ;
}

void popbooleanarray_(char *x, unsigned int *n) {
  popNArray(x,(*n*4), 1) ;
}
