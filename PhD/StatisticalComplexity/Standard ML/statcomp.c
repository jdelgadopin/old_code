#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


main(argc,argv)
int argc;
char **argv;
{
   int i,j,k;
   char buf[100];

   int status;

   /*---------------------------------------------------------
     Command Line has to be like:
       StatComp <InputFile> <L> <delta> <NMoves> <OutputFile>
     ---------------------------------------------------------*/
     
   if (argc != 6) {
       printf("\nStatComp <InputFile> <L> <delta> <NMoves> <OutputFile>\n\n");
       return;
   }
    
   if (fork() == 0)
      execlp("./BTree","BTree",argv[1],argv[2],"SC0001.tmp",(char *)0);
   wait(&status);      

   if (status) {
      printf("\nSomething is wrong with BTree...\n\n");   
      return;
   }   
   
   if (fork() == 0)
      execlp("./crutchc","crutchc","SC0001.tmp",argv[3],argv[4],argv[5],(char *)0);
   wait(&status);      

   if (status) {
      printf("\nSomething is wrong with crutchc...\n\n");   
      return;
   }      
   
   close(fil_in);
   close(fil_out);
}

