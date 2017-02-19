/*******************************************************************

Implementation of a scale free graph
according to Albert & Barabasi, Phys. Rev. Lett. 85, 5234 (2000) 

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

NODE *scale_free_graph (int m0, int m, double p, double q) {

  long comptadorNodes;     /** comptadorNodes em dira quants nodes tinc **/
  NODE *graf;              /** ja que els vaig afegint gradualment...   **/  
  double dau, y;
  long i, iter, triat1, triat2;
  long linkTriat, aux, num, posMaxLinks;
  LINK *link, *auxLink, *antLink;

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

    dau = ran2(&idum);
    if (dau < p) {   /*** Afegeixo m links nous ***/

      for (iter=0; iter < m; iter++) {

	triat1 = random(comptadorNodes);

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
	if (graf[triat1].numLinks > graf[posMaxLinks].numLinks) 
	  posMaxLinks = triat1;
	if (graf[triat2].numLinks > graf[posMaxLinks].numLinks)
	  posMaxLinks = triat2;

      }
      
    } else if (dau < p+q) {  /*** reconnecto m links ***/

      for (iter=0; iter < m; iter++) {
	
	triat1 = random(comptadorNodes);
	
	if (graf[triat1].numLinks > 0) {
	  
	  /*** Trio un link a l'atzar i l'esborro ***/
	  linkTriat = random(graf[triat1].numLinks);
	
	  if (linkTriat == 0) {
	    link = graf[triat1].link;
	    graf[triat1].link = link->link;
	  } else {
	    antLink = graf[triat1].link;
	    link = antLink->link;
	    for (i=1; i < linkTriat; i++) {
	      antLink = link;
	      link = link->link;      
	    }
	    antLink->link = link->link;
	  }
	  aux = link->node;

	  free((LINK *)link);

	  graf[triat1].numLinks--;
	  posMaxLinks = 0;
	  for (i=0; i < comptadorNodes; i++) 
	    if (graf[i].numLinks > graf[posMaxLinks].numLinks)
	      posMaxLinks = i;

	  /*** Esborro el link en la direccio oposada ***/
	  num = graf[aux].numLinks;  /*** aixo es > 0 per nassos ***/
	  
	  if ((graf[aux].link)->node == triat1) {
	    auxLink = graf[aux].link;
	    graf[aux].link = auxLink->link;
	  } else {
	    antLink = graf[aux].link;
	    auxLink = antLink->link;
	    for (i=1; ((i < num-1) && (auxLink->node != triat1)); i++) {
	      antLink = auxLink;
	      auxLink = auxLink->link;      
	    }
	    
	    if (auxLink->node == triat1) {
	      antLink->link = auxLink->link;
	    } else {
	      printf ("\n\nAl tantu!!! %u - %u - %u\n\n", comptadorNodes, triat1, aux);
	      return (NODE *)NULL;
	    }
	  }

	  free((LINK *)auxLink);

	  graf[aux].numLinks--;
	  posMaxLinks = 0;
	  for (i=0; i < comptadorNodes; i++) 
	    if (graf[i].numLinks > graf[posMaxLinks].numLinks)
	      posMaxLinks = i;
	  
	  /*** Trio el desti del link ***/

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
	  if (graf[triat1].numLinks > graf[posMaxLinks].numLinks)
	    posMaxLinks = triat1;
	  if (graf[triat2].numLinks > graf[posMaxLinks].numLinks)
	    posMaxLinks = triat2;
	  
	} 
      }
      
    } else {   /*** afegeixo un node nou amb m links ***/

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
	
	graf[triat1].numLinks += 1;  /** El node nou que afegeixo no compta **/

	graf[triat2].numLinks += 1;
	if (graf[triat2].numLinks > graf[posMaxLinks].numLinks)
	  posMaxLinks = triat2;
      }
      
      if (graf[comptadorNodes].numLinks > graf[posMaxLinks].numLinks)
	posMaxLinks = comptadorNodes;

      comptadorNodes++;
      
    }
    
  }

  /*** A tots els que tenen 0 links, els connecto a algu triat a l'atzar ***/

  for (i=0; i < GrafN; i++) {
    if (graf[i].numLinks == 0) {

      triat1 = i;

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
      if (graf[triat1].numLinks > graf[posMaxLinks].numLinks) 
	posMaxLinks = triat1;
      if (graf[triat2].numLinks > graf[posMaxLinks].numLinks)
	posMaxLinks = triat2;      
    } 
  }
  
  return graf;
}

