/********************************************************************
Measures on UNDIRECTED graphs:

double clustering (GRAF graf, long tamany)

void diameterAveragePathLength (NODE *graf, long tamany, double *pl, long *diam)  

double *correlations (NODE *graf, long tamany) 

********************************************************************

Data Structures and Global Variables:

One needs to define:

#define AGENT   struct agent
AGENT {
  ...
}

before using this code.

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

/*******************************************************************
********************************************************************/

/************* QUEUE implementation ********************************/

#define QUEUE   struct cua      /** Defineixo una cua de long's **/

QUEUE {
  long *cua;
  long entrada, sortida;
  long tamany;
};

QUEUE *CreaCua (long tamany) {
  QUEUE *queue;
  
  queue = (QUEUE *)malloc(sizeof(QUEUE));
  queue->cua = (long *)malloc(tamany*sizeof(long));
  queue->entrada = 0;
  queue->sortida = 0;
  queue->tamany  = tamany;

  return (queue);
}

void EliminarCua(QUEUE *q) {
  free((long *)q->cua);
  free((QUEUE *)q);
}

int CuaBuida(QUEUE *q) {
  return (q->entrada == q->sortida);
}

long CimCua(QUEUE *q) {
  return(q->cua[q->sortida]);
}

void DesCuar (QUEUE *q) {
  q->sortida = (q->sortida + 1) % q->tamany;
}

void EnCuar (QUEUE *q, long w) {
  q->cua[q->entrada] = w;
  q->entrada = (q->entrada + 1) % q->tamany;
}

/*******************************************************************/

double clustering (NODE *graf, long tamany)  {

  int trobat;
  long i, j, k, l, comptador, comptadorVei;
  LINK *link;
  double *arestes, num, suma;
  long *veins, *veinsVei;

  /** Construeixo la representacio senzilla del graf **/
  struct nodo {
    long *veins;
    long numveins;
  };

  struct nodo *nou_graf;

  veins    = (long *)malloc(tamany*sizeof(long));
  nou_graf = (struct nodo *)malloc(tamany*sizeof(struct nodo)); 

  for (i=0; i < tamany; i++) {

    /** Compto, pel vertex i, el nombre de veins diferents que te **/
    comptador = 0;
    link = graf[i].link;
    while (link != (LINK *)NULL) {
      for (j = 0, trobat = 0; (j < comptador) && !trobat; j++)
	trobat = (link->node == veins[j]);
      if ((j == comptador) && (link->node != i)) {
	veins[comptador] = link->node;
	comptador++;
      }
      link = link->link;      
    } /** Hi ha comptador veins diferents **/
    
    nou_graf[i].numveins = comptador;
    nou_graf[i].veins    = (long *)malloc(comptador*sizeof(long));
    for (j=0; j < comptador; j++)
      nou_graf[i].veins[j] = veins[j];
  }
  free((long *)veins);

  /** Ara calculo el clustering a partir de nou_graf **/
  arestes  = (double *)malloc(tamany*sizeof(double));

  for (i=0; i < tamany; i++) {
    
    num   = nou_graf[i].numveins;
    veins = nou_graf[i].veins;
     
    arestes[i] = 0;
    
    if (num > 0) {

      for (j=0; j < num; j++) {
	
	comptadorVei = nou_graf[veins[j]].numveins;
	veinsVei     = nou_graf[veins[j]].veins;
	
	/** Ara comparem veins i veinsVei (que son els veins de veins[j]) **/
	for (l=0; l < num; l++)	
	  for (k=0; k < comptadorVei; k++)
	    arestes[i] += (veins[l] == veinsVei[k]) ? 0.5 : 0;
	    /** aixo significa graf NO dirigit --------^ **/
      }
      
      arestes[i] /= (num > 1) ? (double)num*(num-1)/2.0 : 1;
    }    
  }

  for (i=0, suma=0; i < tamany; i++)
    suma += arestes[i];
  suma /= tamany;
  
  free((double *)arestes);
  for (i=0; i < tamany; i++)
    free((long *)(nou_graf[i].veins));
  free((struct nodo *)nou_graf);

  return suma;
}

/*******************************************************************/

void diameterAveragePathLength (NODE *graf, long tamany, double *pl, long *diam) {

  long i, j, v, vertex, comptador, d, maxim;
  LINK *link;
  double *arestes, num, suma, cami;
  long *veins;
  long *dist;
  QUEUE *q;
  
  /** Construeixo la representacio senzilla del graf **/
  struct nodo {
    long *veins;
    long numveins;
  };

  struct nodo *nou_graf;

  veins    = (long *)malloc(tamany*sizeof(long));
  nou_graf = (struct nodo *)malloc(tamany*sizeof(struct nodo)); 

  for (i=0; i < tamany; i++) {

    /** Compto, pel vertex i, el nombre de veins diferents que te **/
    comptador = 0;
    link = graf[i].link;
    while (link != (LINK *)NULL) {
      for (j=0; ((j < comptador) && (link->node != veins[j])); j++);
      if ((j == comptador) && (link->node != i)) {
	veins[comptador] = link->node;
	comptador++;
      }
      link = link->link;      
    } /** Hi ha comptador veins diferents **/
    
    nou_graf[i].numveins = comptador;
    nou_graf[i].veins    = (long *)malloc(comptador*sizeof(long));
    for (j=0; j < comptador; j++)
      nou_graf[i].veins[j] = veins[j];
  }
  free((long *)veins);

  /** Ara calculo el promig dels camins de cada vertex a tots els altres **/

  dist  = (long *)malloc(tamany*sizeof(long));

  cami  = 0;
  maxim = 0;

  for (i=0; i < (tamany-1); i++) {
    
    /** Inicialitzo taula **/
    for (j=0; j < tamany; j++) 
      dist[j]  = -1;
    dist[i] = 0;

    /** Calculo distancies pel vertex i **/
    q = CreaCua(tamany);
    EnCuar (q,i);

    while (!CuaBuida(q)) {
      vertex = CimCua(q);
      DesCuar(q);

      v = nou_graf[vertex].numveins;
      for (j=0; j < v; j++) {
	d = nou_graf[vertex].veins[j];
	if (dist[d] == -1) {
	  dist[d] = dist[vertex] + 1;
	  EnCuar(q,d);
	}
      }

    }
    EliminarCua(q);

    /** Ara ja tinc totes les distancies a i a la taula dist **/
    /** En calculo el promig i m'ho guardo **/
    for (j=i+1; j < tamany; j++) {
      cami += dist[j];
      maxim = (dist[j] > maxim) ? dist[j] : maxim;
    }

  }

  cami /= tamany*(tamany-1)/2.0;

  free((double *)dist );
  for (i=0; i < tamany; i++)
    free((long *)(nou_graf[i].veins));
  free((struct nodo *)nou_graf);

  *pl   = cami;
  *diam = maxim;
}

/*******************************************************************/

double *correlations (NODE *graf, long tamany)  {

  long i, j, connex;
  LINK *link;
  double *correlacions, suma;
  long *veins, comptador;
  
  /** Construeixo la representacio senzilla del graf **/
  struct nodo {
    long *veins;
    long numveins;
  };

  struct nodo *nou_graf;

  veins    = (long *)malloc(tamany*sizeof(long));
  nou_graf = (struct nodo *)malloc(tamany*sizeof(struct nodo)); 

  for (i=0; i < tamany; i++) {

    /** Compto, pel vertex i, el nombre de veins diferents que te **/
    comptador = 0;
    link = graf[i].link;
    while (link != (LINK *)NULL) {
      for (j=0; ((j < comptador) && (link->node != veins[j])); j++);
      if ((j == comptador) && (link->node != i)) {
	veins[comptador] = link->node;
	comptador++;
      }
      link = link->link;      
    } /** Hi ha comptador veins diferents **/
    
    nou_graf[i].numveins = comptador;
    nou_graf[i].veins    = (long *)malloc(comptador*sizeof(long));
    for (j=0; j < comptador; j++)
      nou_graf[i].veins[j] = veins[j];
  }
  /** free((long *)veins); NO destrueixo veins ja que el tornare a fer servir **/

  /** Ara calculo les correlacions **/
  correlacions = (double *)malloc(tamany*sizeof(double));
  for (i=0; i < tamany; i++) {
    correlacions[i] = 0;
    veins[i] = 0;
  }

  for (i=0; i < tamany; i++) {
    connex = nou_graf[i].numveins;
    
    veins[connex] += 1;
 
    for (suma = 0, j = 0; j < connex; j++)
      suma += nou_graf[nou_graf[i].veins[j]].numveins;
    suma /= (connex > 0) ? connex : 1;

    correlacions[connex] += suma;
  }

  for (i=0; i < tamany; i++) 
    correlacions[i] /= (veins[i] > 0) ? veins[i] : 1;
  
  free((long *)veins);

  return correlacions;	 
}

