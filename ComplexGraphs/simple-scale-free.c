/*******************************************************************

Implementation of a simple scale free graph
according to Barabasi, Albert & Jeong, Phys. A 272, 173 (1999) 

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


NODE *simple_scale_free_graph (int m0, int m)  {
  long comptadorNodes;     /** comptadorNodes em dira quants nodes tinc **/
  NODE *graf;              /** ja que els vaig afegint gradualment...   **/  
  double y;
  long i, iter, triat1, triat2, passos, posMaxLinks;
  LINK *link, *auxLink;

  /** Creo i inicialitzo el graf, i considero m0 nodes inicials sense links **/

  graf = (NODE *)malloc(GrafN*sizeof(NODE));
  for (i=0; i < GrafN; i++) {
    graf[i].agent = (AGENT *)NULL;
    graf[i].numLinks = 0;
    graf[i].link = (LINK *)NULL;
    graf[i].fitness = 0;    
  }
  comptadorNodes = m0;
  
  posMaxLinks = 0;

  /** Repeteixo el proces fins que no tingui mes nodes a afegir **/

  while (comptadorNodes < GrafN) {

    triat1 = comptadorNodes; 
    
    for (iter=0; iter < m; iter++) {

      /*** Trio el desti del nou link ***/
	
      do {
	triat2 = random(comptadorNodes);
	y = ran2(&idum);
      } while ((triat2 == triat1) || 
	 (y > (graf[triat2].numLinks+1.0)/(graf[posMaxLinks].numLinks+1.0)));
		
      /*** Reorganitzo els links ***/
      
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
      if (graf[triat2].numLinks > graf[posMaxLinks].numLinks)
	posMaxLinks = triat2;
      
    }

    if (graf[comptadorNodes].numLinks > graf[posMaxLinks].numLinks)
      posMaxLinks = comptadorNodes;
    
    comptadorNodes++;
  }
  
  return graf;
}
