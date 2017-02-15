# Epsilon-Machine reconstruction

At the end of the 80's the work by Prof. James P. Crutchfield et al. defined a computer-science related measure of complexity called *Statistical Complexity*. It involved, among other things, the reconstruction of stochastic finite automata from streams of 0's and 1's (a.k.a. epsilon-machines) .

The related papers that I used in my implementation were (back then when I was doing my PhD) :

J. P. Crutchfield and K. Young, *Inferring Statistical Complexity*, Physical Review Letters 63 (1989) 105-108.

K. Young, *The Grammar and Statistical Mechanics of Complex Physical Systems*, Ph.D. Dissertation, University of California, Santa Cruz (August 1991). 

J. E. Hanson, *Computational Mechanics of Cellular Automata*, Ph.D. Dissertation, University of California, Berkeley (August 1993). 

At the time I used this code in the research published in these papers:

J. Delgado, R. V. Sole, *Collective-induced computation*, Physical Review E 55 (1997), 2338-2344

J. Delgado, R. V. Sole, *Characterizing Turbulence in Globally Coupled Maps with Stochastic Finite Automata*, Physics Letters A 270 (2000), 314-319

I implemented two versions: One simple first version, using the functional language Standard ML, and another one, much faster, in C. I did this because the Standard ML version was, in fact, so slow that was unusable with files of more than 10^6 bits. This was around 1996, and the implementation of functional programming languages was not as advanced as it is today.

The algorithm of epsilon-machine reconstruction I implemented here is largely superseded by the work of Prof. [Cosma Shalizi](http://bactra.org/CSSR/):

C. R. Shalizi, K. L. Klinkner, *Blind Construction of Optimal Nonlinear Recursive Predictors for Discrete Sequences* in *Uncertainty in Artificial Intelligence: Proceedings of the Twentieth Conference (UAI 2004) *, Max Chickering and Joseph Halpern (eds.), AUAI Press 2004, 504-511

Thus, nowadays the code in this repository is *not* useful *at all* :smiley:
