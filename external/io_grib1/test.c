#include <stdio.h>
#include <stdlib.h>

int *test_alloc_(int *abc);

/*
int main()
{
  int *test;
  test=test_alloc();
  fprintf(stderr,"main: *test: %d\n",*test);
}
*/
int *test_alloc_(int *abc)
{
  int *test;
  //  int *test;
  fprintf(stderr,"in test_alloc:\n");
  test = (int *)calloc(1,sizeof(int));
  //  *test=995;
  //  return test;
  return test;
}
