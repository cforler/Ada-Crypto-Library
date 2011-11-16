#include<stdio.h>

int main(int args, char *argv[])
{

  int base = 16;

  while(*++argv)
    {
      printf("%i#%s#, ",base,*argv);
    }
  puts("");
  return 0;
}
