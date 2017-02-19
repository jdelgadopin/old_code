/*******************************************************************

Implementation of a scale free AND small world graph
according to Bianconi & Barabasi, Europhys. Lett., 54 (4), pp. 436–442 (2001)

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

NODE *scale_free_fitness (int m)  {
  long comptadorNodes;     /** comptadorNodes em dira quants nodes tinc **/
  NODE *graf;              /** ja que els vaig afegint gradualment...   **/  
  double y, tmpFit, tmpMaxFit;
  long i, j, iter, triat1, triat2, passos, posMaxLinks;
  LINK *link, *auxLink;
  double fitness;

  /** Creo i inicialitzo el graf **/

  graf = (NODE *)malloc(GrafN*sizeof(NODE));
  for (i=0; i < GrafN; i++) {
    graf[i].agent = (AGENT *)NULL;
    graf[i].numLinks = 0;
    graf[i].link = (LINK *)NULL;
    graf[i].fitness = 0;
  }

  /** Graf Inicial amb m0=m+1 nodes **/

  comptadorNodes = m+1; 

  posMaxLinks = 0;      /** Fitness inicials **/
  for (i=0; i < comptadorNodes; i++) {
    graf[i].fitness = ran2(&idum);
    if (graf[i].fitness > graf[posMaxLinks].fitness)
      posMaxLinks = i;
  }

  /** Cada node inicial tindra exactament m links **/
  for (i=0; i < (comptadorNodes-1); i++) {
    triat1 = i;
  
    for (j=(i+1); j < comptadorNodes; j++) {
      triat2 = j;

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


  /** Repeteixo el proces fins que no tingui mes nodes a afegir **/

  while (comptadorNodes < GrafN) {

    triat1 = comptadorNodes; 
    graf[triat1].fitness = ran2(&idum); /*** Distribucio de fitness uniforme **/
    
    for (iter=0; iter < m; iter++) {

      /*** Trio el desti del nou link ***/
      
      tmpMaxFit = graf[posMaxLinks].fitness * graf[posMaxLinks].numLinks;
      do {
	triat2 = random(comptadorNodes);
	tmpFit = graf[triat2].fitness * graf[triat2].numLinks;	
	y = ran2(&idum);
      } while ((triat2 == triat1) || (y > tmpFit/tmpMaxFit));
		
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

      if (graf[triat2].numLinks*graf[triat2].fitness > 
	  graf[posMaxLinks].numLinks*graf[posMaxLinks].fitness)
	posMaxLinks = triat2;
      
    }

    if (graf[comptadorNodes].numLinks*graf[comptadorNodes].fitness > 
	graf[posMaxLinks].numLinks*graf[posMaxLinks].fitness)
      posMaxLinks = comptadorNodes;
    
    comptadorNodes++;
  }
  
  return graf;
}
