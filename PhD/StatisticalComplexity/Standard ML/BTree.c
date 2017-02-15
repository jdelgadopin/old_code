#include <stdlib.h>
#include <math.h>
#include <alloc.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <io.h>

#define BUFFER  1000

#define NODE   struct node

NODE {
  double counter;
  NODE *left, *right;
};

double counter;

int fil_in, fil_out, L;

main(argc,argv)
int argc;
char **argv;
{
   NODE *Tree;
   char buf[BUFFER], temp[BUFFER], trav[100];
   int i,j,k,rd;
   int tmp;

   /*----------------------------------------------
     Command Line has to be like:
       BTree <Input_File> <L> <Output_File>
     ----------------------------------------------*/

   if (argc != 4) {
       printf("\nBTree <Input_File> <L> <Output_File>\n\n");
       return;
   }

   fil_in = open(argv[1],O_RDONLY,0600);
   fil_out = open(argv[3],O_CREAT|O_WRONLY|O_TRUNC,0600);
   L = atoi(argv[2]);

   /*----------------------------------------------
     Initializing Tree
     ----------------------------------------------*/

   Tree = (NODE *)malloc(sizeof(NODE));
   if (Tree == (NODE *)NULL) {
      printf("\nNo memory\n\n");
      return;
   }
   Tree->counter = 0.0;
   Tree->left = Tree->right = (NODE *)NULL;


   /*----------------------------------------------
     Now I read the input file while building the
     BTree
     ----------------------------------------------*/

   rd = read(fil_in,buf,BUFFER);
   printf("Llegeixo --> %u bytes\n",rd);
   for (i=0; i < rd-L+1; i++)
        if ((tmp = BTInsert(Tree,buf,i)) < 0) {
                printf("\nThis file is not made of 0's and 1's!!!\n");
                printf("No memory --> %d %d\n\n",tmp,i);
                return;
        };
   for (j=0; j < (L-1); j++, i++)
       buf[j] = buf[i];

   while (((rd = read(fil_in,temp,BUFFER-L+1))) != 0) {
        printf("Llegeixo --> %u bytes\n",rd);
        for (k=0; k < rd; k++)
            buf[k+L-1] = temp[k];
        for (i=0; i < rd; i++)
            if ((tmp = BTInsert(Tree,buf,i)) < 0) {
                printf("\nThis file is not made of 0's and 1's!!!\n");
                printf("No memory --> %d %d\n\n",tmp,i);
                return;
            }
        for (j=0; j < (L-1); j++, i++)
            buf[j] = buf[i];
   }

   counter = 0;
   BTSave(Tree,fil_out,0,trav);

   printf("\n%f\n\n",(float)counter);

   /*----------------------------------------------
     Closing files and terminating
     ---------------------------------------------*/

   close(fil_in);
   close(fil_out);

   exit(0);
}


BTInsert(NODE *Tree, char *b, int i)
{
     int j, k;
     NODE *path, *new;
     short side;
  
     path = Tree;
     path->counter += 1.0;
     for (j=i; j < (i+L); j++) {
       if ((b[j] != '0') && (b[j] != '1'))
          return(-2);
       else if (b[j] == '0') {
          side = 0;
          new = path->left;
       }
       else {
          side = 1;
          new = path->right;
       }
       if (new == (NODE *)NULL) {
          new = (NODE *)malloc(sizeof(NODE));
          if (new == (NODE *)NULL) {
              return(-1);
          }      
          new->counter = 0.0;
          new->left = new->right = (NODE *)NULL;
          if (side)
             path->right = new;   
          else
             path->left = new;
       }
       new->counter += 1.0;          
       path = new;
     }
     return(0);
}



BTSave (NODE *Tree, int fil, int level, char *trav)
{
 char buff[15];

 if (Tree != (NODE *)NULL) {
    if ((Tree->left == (NODE *)NULL) && 
        (Tree->right == (NODE *)NULL) && 
        (L == level)) {
        trav[level] = '\0';
        strcat(trav,"\n");
        strcat(trav,gcvt(Tree->counter,10,buff));
        counter += Tree->counter;
        strcat(trav,"\n");
        write(fil,trav,strlen(trav));
    } 
    else {
        trav[level] = '0';
        BTSave(Tree->left, fil, level+1, trav);
        trav[level] = '1';
        BTSave(Tree->right, fil, level+1, trav);
    }
 }
}
