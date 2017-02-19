/*******************************************************************

Implementation of a regular graph
according to Kittock, J.E.,
             Emergent Conventions and the Structure of Multi-agent Systems,
             in 1993 Lectures in Complex Systems (L. Nadel \& D. Stein,
             eds.) (1995), SFI Studies in the sciences of complexity,
             Addison-Wesley.

********************************************************************
********************************************************************

Data Structures and Global Variables:

One needs to define:

#define AGENT   struct agent
AGENT {
  ...
}

before using this code.

Besides, one needs to  #include "random.h"

*******************************************************************
*******************************************************************/

#define NODE     struct node  /** Un graf sera una taula de nodes **/
#define LINK     struct link
#define GRAF     (NODE *)

NODE {
  AGENT *agent;
  double fitness; /** Fitness del node, nomes ho faig servir en un algorisme **/
  long numLinks;  /** Connectivitat del node **/
  LINK *link;     /** Apuntador a la llista de nodes adjacents **/
};

LINK {
  long node;     /** Index de la taula de nodes on es el receptor **/ 
  LINK *link;
};

long GrafN;      /*** Nombre d'elements del graf  ******************/
                 /*** (suposem que GrafN està inicialitzat abans de fer servir aquesta funcio) **/
/*******************************************************************
********************************************************************/

NODE *regular_graph (int k) {

  NODE *graf;
  long i, j, kmitjos, triat1, triat2, m;
  LINK *link, *auxLink, *antLink;
  int trobat;

  graf = (NODE *)malloc(GrafN*sizeof(NODE));
  for (i=0; i < GrafN; i++) {
    graf[i].agent = (AGENT *)NULL;
    graf[i].numLinks = 0;
    graf[i].link = (LINK *)NULL;
    graf[i].fitness = 0;    
  }

  if (k > 0) {
  
    /** Construeixo l'anell de k/2 veins per costat **/
    
    kmitjos = k/2;  /** k/2 << GrafN/2 **/

    for (i=0; i < GrafN; i++) {
      triat1 = i;
      for (j=1; j <= kmitjos; j++) {
	triat2 = (i + j) % GrafN; 
	
	link = (LINK *)malloc(sizeof(LINK));
	auxLink = (LINK *)malloc(sizeof(LINK));
	
	link->node = triat2;
	auxLink->node = triat1;
	
	link->link = graf[triat1].link;
	graf[triat1].link = link;
	
	auxLink->link = graf[triat2].link;
	graf[triat2].link = auxLink;
	
	graf[triat1].numLinks += 1;
	graf[triat2].numLinks += 1;	      
      }
    }
  }

  return graf;
}
