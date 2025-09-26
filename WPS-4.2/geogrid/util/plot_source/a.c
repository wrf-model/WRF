#include <stdio.h>

#define N1 1200

int main(int argc, char ** argv)
{
  FILE * a;
  unsigned char * data;
  int i, j;
  int data1[N1*N1];

  data = (unsigned char *)malloc(N1*N1);
  a = fopen(argv[1],"r");
  fread(data, 1, N1*N1, a);
  fclose(a);

  for(i=0; i<N1*N1; i++)
  {
    printf("%i\n", (int)(data[i]));
  }

  free(data);

  return 0;
}
