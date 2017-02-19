/*******************************************************************

Implementation of a small world graph
according to Watts & Strogatz, Nature 393, 440 (1998)
             (where k => 4)

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

NODE *small_world_graph (int k, double beta) {

  NODE *graf;
  long i, j, kmitjos, triat1, triat2, m;
  LINK *link, *auxLink, *antLink;
  int trobat;

  kmitjos = k/2;  /** k/2 << GrafN/2 **/

  /** Inicialitzo el graf **/

  graf = (NODE *)malloc(GrafN*sizeof(NODE));
  for (i=0; i < GrafN; i++) {
    graf[i].agent = (AGENT *)NULL;
    graf[i].numLinks = 0;
    graf[i].link = (LINK *)NULL;
    graf[i].fitness = 0;    
  }

  /** Construeixo l'anell de k/2 veins per costat **/

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

  /** Faig el rewiring aleatori **/

  for (j=1; j <= kmitjos; j++) {
    
    for (i=0; i < GrafN; i++) {
      triat1 = i;
      triat2 = (i + j) % GrafN;

      if (ran2(&idum) < beta) {
	
	/*** Esborro el link triat1 --> triat2 ***/
	if ((graf[triat1].link)->node == triat2) {
	  auxLink = graf[triat1].link;
	  graf[triat1].link = auxLink->link;
	} else {
	  antLink = graf[triat1].link;
	  auxLink = antLink->link;
	  while ((auxLink->link != (LINK *)NULL) && (auxLink->node != triat2)) {
	    antLink = auxLink;
	    auxLink = auxLink->link;      
	  }
	  if (auxLink->node == triat2) {
	    antLink->link = auxLink->link;
	  } else {
	    printf ("\n\nAl tantu!!! ");
	    return (NODE *)NULL;
	  }
	}	
	free((LINK *)auxLink);
	graf[triat1].numLinks -= 1;


	/*** Esborro el link triat2 --> triat1 ***/
	if ((graf[triat2].link)->node == triat1) {
	  auxLink = graf[triat2].link;
	  graf[triat2].link = auxLink->link;
	} else {
	  antLink = graf[triat2].link;
	  auxLink = antLink->link;
	  while ((auxLink->link != (LINK *)NULL) && (auxLink->node != triat1)) {
	    antLink = auxLink;
	    auxLink = auxLink->link;      
	  }
	  if (auxLink->node == triat1) {
	    antLink->link = auxLink->link;
	  } else {
	    printf ("\n\nAl tantu!!! ");
	    return (NODE *)NULL;
	  }
	}	
	free((LINK *)auxLink);
	graf[triat2].numLinks -= 1;

	/** Ara ja estan els links esborrats. N'he de generar un a l'atzar **/

	do {
	  m = random(GrafN);
	  trobat = 0;
	  link = graf[triat1].link;
	  while ((link != (LINK *)NULL) && (trobat == 0)) {
	    trobat = (link->node == m) ? 1 : 0;
	    link = link->link;
	  }
	} while ((m==triat1) || (trobat != 0));
	triat2 = m;

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

