/*************************************************************************************
*                      RECONSTRUCCIO D'epsilon MAQUINES                              *
*                        PER JAMES P. CRUTCHFIELD                                    *
*                                                                                    *
*                IMPLEMENTACIO DE JORDI DELGADO PIN, V 0.5 (Abril 1996)              *
*                                                                                    *
*                                                                                    *
*  Parametres d'entrada:   fil_in -----> fitxer d'entrada                            *
*                          L ----------> Creacio de l'arbre                          *
*                          D ----------> Profunditat del Morph                       *
*                          M ----------> Iteracions per trobar la distribucio        *
*                                        estacionaria del proces de Markov           *
*                          fil_out ----> fitxer d'output                             *
*                                                                                    *
*  Suposicions: Alfabet d'entrada ---> {0,1}                                         *
*               Delta ---------------> 1.0  (Si no, hauriem de modificar             * 
*                                              BTMorphCompare)                       *
*                                                                                    *
**************************************************************************************/


#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>




/********************************************************************************

   Rutines per al tractament de nombres aleatoris: RAN0 del Numerical Recipes

 ********************************************************************************/

#define randomize()         srand((unsigned)time(NULL))

#define IA   16807
#define IM   2147483647
#define AM   (1.0/IM)
#define IQ   127773
#define IR   2836
#define MASK 123459876

long idum;

float ran0(long *idum)
{
  long k;
  float ans;
  
  *idum ^= MASK;
  k=(*idum)/IQ;
  *idum=IA*(*idum-k*IQ)-IR*k;
  if (*idum < 0)  *idum += IM;
  ans=AM*(*idum);
  *idum ^= MASK;
  return ans;
}


void InicialitzarRan0(long *idum)
{
  randomize();
  *idum = rand()+1;
}


#define randd()            (int)floor(ran0(&idum)*32767)
#define rando(num)         (randd() % (num))



/***********************************************************************************

                        CONSTANTS DEL PROGRAMA

************************************************************************************/

#define BUFFER 10000   /* S'usa en BTreeConstruct per llegir del fitxer d'entrada */
#define DELTA  1.0     /* No s'utilitza, caldria canviar la comparacio de Morphs */




/***********************************************************************************

                        ESTRUCTURES DE DADES

************************************************************************************/

#define NODE struct node
NODE {
  double counter;            /* Probabilitat associada al node */
  int state;                 /* Estat de l'automata al que correspon el node */
  NODE *left,*right;
};

#define ELEM struct elem
ELEM {
  double element;
  NODE *nus;
  ELEM *prox;
};

#define AUTOM struct autom
AUTOM {
  double zero;
  double one;
};



/***********************************************************************************

                        DECLARACIONS DE FUNCIONS

************************************************************************************/

void Probabilitats(NODE *Tree, ELEM **List);
AUTOM ***StatComp(double *s);
double AutSum(AUTOM ***a, int i, int j);
NODE *BTreeConstruct(int fil, int P);
AUTOM ***BTCreateAutomata(NODE *Tree, int el);
void BTLabel(NODE *Tree);
void BTMorphLabel(NODE *Tree, ELEM **List);



/***********************************************************************************

                        VARIABLES GLOBALS

************************************************************************************/

int    fil_in, fil_out;       /* fitxers d'entrada i sortida */
int    L;                     /* Profunditat de l'arbre */
int    D;                     /* Profunditat dels Morphs */
double N;                     /* Nombre de dades a tractar, long. de fil_in */
double M;                     /* Iteracions per a la distr. estac. */

int leme;                     /* Var. auxiliar. Se li dona valor en StatComp() */
                              /* i s'utilitza en main()                        */
main(argc,argv)
int argc;
char **argv;
{
  char cadena[100];
  double StatisticalComplexity;
  int i,j;
  AUTOM ***S;

  if (argc != 6) {
    printf("La crida es: crutch <Input File> <L> <D> <Moves> <Output File>\n");
    return;
  }
  
  fil_in = open(argv[1], O_RDONLY, 0600);
  fil_out = open(argv[5], O_WRONLY|O_APPEND, 0600);
  L = atoi(argv[2]);
  D = atoi(argv[3]);
  M = atof(argv[4]);

  InicialitzarRan0(&idum);

  S = StatComp(&StatisticalComplexity);  /* Depenen de variables globals */

  cadena[0] = '\0';
  sprintf(cadena,"%16.9f\n",(float)StatisticalComplexity);
  write(fil_out,cadena,strlen(cadena));


  /********** Nomes per debugging ***************/

  if (S != (AUTOM ***)NULL) {
    for (i=0; i < leme; i++)
      for (j=0; j < leme; j++) {
         if (S[i][j] != (AUTOM *)NULL) {
            cadena[0] = '\0';
            if (S[i][j]->zero != -1.0) {
               sprintf(cadena,"(%u, 0, %5.4f, %u)\n",i+1,(float)(S[i][j]->zero),j+1);
               write(fil_out,cadena,strlen(cadena));
            }
            cadena[0] = '\0';
            if (S[i][j]->one != -1.0) {
               sprintf(cadena,"(%u, 1, %5.4f, %u)\n",i+1,(float)(S[i][j]->one),j+1);
               write(fil_out,cadena,strlen(cadena));
            }                 
         }
      }
  }
  
  /***********************************************/
  
      
  close(fil_in);
  close(fil_out);
}



/**************************************************************************************

        FUNCIO:     StatComp(*d)
        RETORNA:    Automata
        PARAMETRES: Variable on retornar la Complexitat Estadistica
        
***************************************************************************************/


AUTOM ***StatComp(double *d)
{
 NODE *Arbre;
 ELEM *Llista, *temp;
 int elements,state,j;
 AUTOM ***aut;
 double *prob,k,z,s;
 char cadena[20];
 
 Arbre  = (NODE *)NULL;
 Llista = (ELEM *)NULL;
 

 Arbre = BTreeConstruct(fil_in,L);
 
 BTMorphLabel(Arbre,&Llista);   
 
 elements = leme = DestrueixLlista(Llista);
 
 aut = BTCreateAutomata(Arbre, elements); 

 if (BTIndeterm(aut,elements)) {
   *d = -1.0;
   /*return((AUTOM ***)NULL);*/ return(aut);
 }    
 else if (!BTMarkovian(aut,elements)) {
   *d = -2.0;
   /*return((AUTOM ***)NULL);*/  return(aut);
 }    
 else {
   if (elements > 1) {
     prob = (double *)malloc(elements*sizeof(double));
     if (prob == (double *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        
     
     for (state = 0; state < elements; state++)
        prob[state] = 0.0;
        
     prob[0] = 1.0; state = 0;
     for (k=0.0; k < M; k += 1.0) {
        z = ran0(&idum);
        j = 0;
        s = AutSum(aut,state,j);
        while ((z >= s) && (j < elements)) {
           z -= s;
           j++;
           s = AutSum(aut,state,j);
        }
        prob[j] += 1.0;
        state = j;
     }
     
     for (state = 0; state < elements; state++)
        prob[state] /= M;

     s = 0;
     for (state=0; state < elements; state++)
         s += (prob[state] != 0.0) ? (-prob[state]*(log(prob[state])/log(2.0))) : 0.0;
   }
   else {
     s = 0.0;
   }        

   *d = s;
        
   return(aut);    
 }        
}




double AutSum(AUTOM ***a, int i, int j)
{
  double pr;
  
  pr = 0.0;
  if (a[i][j] != (AUTOM *)NULL) {
    pr += (a[i][j]->zero != -1.0) ? a[i][j]->zero : 0.0;
    pr += (a[i][j]->one != -1.0) ? a[i][j]->one : 0.0;    
  }
  
  return(pr); 
}



NODE *BTreeConstruct(int fil, int P)
{
   NODE *Tree;
   char buf[BUFFER], temp[BUFFER];
   int i,j,k,rd;

   Tree = (NODE *)malloc(sizeof(NODE));
   if (Tree == (NODE *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        

   Tree->counter = 0.0;
   Tree->state = -1;
   Tree->left = Tree->right = (NODE *)NULL;

   rd = read(fil,buf,BUFFER);
   for (i=0; i < rd-P+1; i++)         
        BTInsert(Tree,buf,i,P);
   for (j=0; j < (P-1); j++, i++)
        buf[j] = buf[i];

   while ((rd = read(fil,temp,BUFFER-P+1))) {
        for (k=0; k < rd; k++)
            buf[k+P-1] = temp[k];           
        for (i=0; i < rd; i++)         
            BTInsert(Tree,buf,i,P);
        for (j=0; j < (P-1); j++, i++)
            buf[j] = buf[i];
   }
   
   N = Tree->counter;   
   BTLabel(Tree);   
   return(Tree);
}


BTInsert(NODE *Tree, char *b, int i, int P)
{
     int j, k;
     NODE *path, *new;
     short side;
  
     path = Tree;
     path->counter += 1.0;
     for (j=i; j < (i+P); j++) {
       if (b[j] == '0') {
          side = 0;
          new = path->left;
       }
       else {
          side = 1;
          new = path->right;
       }
       if (new == (NODE *)NULL) {
          new = (NODE *)malloc(sizeof(NODE));          
          if (new == (NODE *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        

          new->counter = 0.0;
          new->state = -1;
          new->left = new->right = (NODE *)NULL;
          if (side)
             path->right = new;   
          else
             path->left = new;
       }
       new->counter += 1.0;          
       path = new;
     }
     return(1);
}



void BTLabel(NODE *Tree)
{
 if (Tree != (NODE *)NULL) {

   Tree->counter = Tree->counter/N;
 
   if (Tree->left != (NODE *)NULL)
      BTLabel(Tree->left);
 
   if (Tree->right != (NODE *)NULL)
      BTLabel(Tree->right);
 }
}



BTMorphCompare(NODE *Morph1, NODE *Morph2, int lg)
{ 
 if (lg == 0) {
      return(1);
 }     
 else if ( ((Morph1 != (NODE *)NULL) && (Morph2 == (NODE *)NULL)) ||
         ((Morph2 != (NODE *)NULL) && (Morph1 == (NODE *)NULL)) ) {
         return(0);
 }
 else if ((Morph1 != (NODE *)NULL) && (Morph2 != (NODE *)NULL)) {
         int dret1, esq1, dret2, esq2, fills;
 
         dret1 = (Morph1->right == (NODE *)NULL);
         esq1  = (Morph1->left  == (NODE *)NULL);
         dret2 = (Morph2->right == (NODE *)NULL);
         esq2  = (Morph2->left  == (NODE *)NULL);

         if ((dret1 != dret2) || (esq1 != esq2))
            return(0);
         else {
            fills = BTMorphCompare(Morph1->left, Morph2->left, lg-1) &&
                    BTMorphCompare(Morph1->right, Morph2->right, lg-1);
            return(fills);
         }   
 }
 else
      return(1);
}



void BTMorphLabel(NODE *Tree, ELEM **List)
{
 if ((Tree != (NODE *)NULL) && (BTDepth(Tree,D,0))) {
    if (*List == (ELEM *)NULL) {
       *List = (ELEM *)malloc(sizeof(ELEM));
       if ((*List) == (ELEM *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        

       (*List)->prox = (ELEM *)NULL;
       (*List)->nus = Tree;
       Tree->state = 1;
    }
    else {
       ELEM *temp, *temp0;
       int i = 1, found = 0;
    
       temp = (*List);
       while (temp != (ELEM *)NULL) {
         temp0 = temp;
         if (BTMorphCompare(Tree,temp->nus,D)) {
            Tree->state = i;
            temp = (ELEM *)NULL;
            found++;
         }
         else {
            temp = temp->prox;
         }
         i++;  
       }
       
       if (!found) {
         temp0->prox = (ELEM *)malloc(sizeof(ELEM));
         if (temp0->prox == (ELEM *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        

         temp0->prox->prox = (ELEM *)NULL;
         temp0->prox->nus = Tree;
         Tree->state = i;
       }
    }
 
    if (Tree->left != (NODE *)NULL)
       BTMorphLabel(Tree->left,List);

    if (Tree->right != (NODE *)NULL)
       BTMorphLabel(Tree->right,List); 
 }       
}



BTDepth (NODE *Tree, int len, int i)
{
 int l,r;
 
 l = r = 0;
 
 if (i < len) {
    if (Tree != (NODE *)NULL) {
       if (Tree->left != (NODE *)NULL)
          l = BTDepth(Tree->left, len, i+1);
       if ((Tree->right != (NODE *)NULL) && (!l))
          r = BTDepth(Tree->right, len, i+1);    
       return(l || r);   
    }
    else {
       return(0);
    }
 }
 else {
    return(1);
 }
}



AUTOM ***BTCreateAutomata(NODE *Tree, int elements)
{
 AUTOM ***a;
 ELEM *Fifo, *Last, *temp;
 int i,j,s,t,est1,est2;
 
 
 a = (AUTOM ***)malloc(elements*sizeof(AUTOM **));
 if (a == (AUTOM ***)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        

 for (i=0; i < elements; i++) {
    a[i] = (AUTOM **)malloc(elements*sizeof(AUTOM *));
    if (a[i] == (AUTOM **)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        
 }

 for (i=0; i < elements; i++)
   for (j=0; j < elements; j++)
     a[i][j] = (AUTOM *)NULL;


 Fifo = (ELEM *)malloc(sizeof(ELEM));
 if (Fifo == (ELEM *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        
 Fifo->nus = Tree;
 Fifo->prox = (ELEM *)NULL;
 Last = Fifo;
 
 while (Fifo != (ELEM *)NULL) {
 
    if (Fifo->nus->left != (NODE *)NULL) {
       if (Fifo->nus->left->state != -1) {
          Last->prox = (ELEM *)malloc(sizeof(ELEM));
          if (Last->prox == (ELEM *)NULL)
            printf("\n\nM'he quedat sense memoria!!\n\n");        
          Last = Last->prox;
          Last->nus = Fifo->nus->left;
          Last->prox = (ELEM *)NULL;
          est1 = Fifo->nus->state-1;
          est2 = Fifo->nus->left->state-1;
          if (a[est1][est2] == (AUTOM *)NULL) {
             a[est1][est2] = (AUTOM *)malloc(sizeof(AUTOM));
             if (a[est1][est2] == (AUTOM *)NULL)
                    printf("\n\nM'he quedat sense memoria!!\n\n");        
             a[est1][est2]->zero = -1.0;
             a[est1][est2]->one  = -1.0;
          }
          if (a[est1][est2]->zero == -1.0) {
             a[est1][est2]->zero = Fifo->nus->left->counter/Fifo->nus->counter;
          }
       }
    }
    if (Fifo->nus->right != (NODE *)NULL) {
       if (Fifo->nus->right->state != -1) {
          Last->prox = (ELEM *)malloc(sizeof(ELEM));
          if (Last->prox == (ELEM *)NULL)
                    printf("\n\nM'he quedat sense memoria!!\n\n");        
          Last = Last->prox;
          Last->nus = Fifo->nus->right;
          Last->prox = (ELEM *)NULL;    
          est1 = Fifo->nus->state-1;
          est2 = Fifo->nus->right->state-1;
          if (a[est1][est2] == (AUTOM *)NULL) {
             a[est1][est2] = (AUTOM *)malloc(sizeof(AUTOM));
             if (a[est1][est2] == (AUTOM *)NULL)
                    printf("\n\nM'he quedat sense memoria!!\n\n");        
             a[est1][est2]->zero = -1.0;
             a[est1][est2]->one  = -1.0;
          }
          if (a[est1][est2]->one == -1.0) { 
             a[est1][est2]->one = Fifo->nus->right->counter/Fifo->nus->counter;
          }
       }
    }

    temp = Fifo;
    Fifo = Fifo->prox;
    free((ELEM *)temp); 
 }

 return(a);
}



BTIndeterm(AUTOM ***a, int elems)
{
  int i,j,count0,count1,indet=0;

  for (i=0; ((!indet) && (i < elems)); i++) {
    count0 = count1 = 0;
    for (j=0; j < elems; j++) {
       if (a[i][j] != (AUTOM *)NULL) {
         if (a[i][j]->zero != -1.0)
           count0++;
         if (a[i][j]->one != -1.0)
           count1++;
       }
    }
    if ((count0 > 1) || (count1 > 1))
       indet++;
  } 
  
  return(indet == 1);
}



BTMarkovian(AUTOM ***a, int elems)
{
  int i,j,mark=1;
  double count;

  for (i=0; ((mark) && (i < elems)); i++) {
    count = 0.0;
    for (j=0; j < elems; j++) 
       if (a[i][j] != (AUTOM *)NULL)
         count += ((a[i][j]->zero != -1.0) ? a[i][j]->zero : 0.0) +
                  ((a[i][j]->one  != -1.0) ? a[i][j]->one  : 0.0);
    if (count < 0.9)
       mark--;
  } 
  
  return(mark);
}



DestrueixLlista(ELEM *List)
{
  ELEM *temp1, *temp2;
  int i = 0;
  
  temp1 = List;
  while (temp1 != (ELEM *)NULL) {
     i++;
     temp2 = temp1;
     temp1 = temp1->prox;
     free((ELEM *)temp2);
  }
  
  return(i);
}


