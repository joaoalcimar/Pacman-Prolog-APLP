% Mapa do jogo pacman

% don't mess with this
:-dynamic pacman/3. 
:-dynamic ghost/3. 
:-dynamic diamond/2.  

grid([[1,1,1,1,1,3,1,1,1,1], 
      [1,0,0,0,0,0,0,0,0,1], 
      [1,0,1,0,0,0,0,1,0,1], 
      [1,0,1,1,1,1,1,1,0,1], 
      [2,0,0,0,0,0,0,0,0,2], 
      [1,0,0,0,0,0,0,0,0,1], 
      [1,0,1,1,0,0,1,1,0,1],
      [1,0,1,1,0,0,1,1,0,1],
      [1,0,0,0,0,0,0,0,0,1],
      [1,1,1,1,1,3,1,1,1,1]]).

% set de posicoes

pacman(2,2,normal). 
ghost(2,9,'Black'). 
ghost(9,9,'Red'). 
diamond(2,6).

/* README

1 - paredes
0 - posicoes livres
>1 - tuneis, qualquer numero maior que 1, o numero da saida deve combinar com o da entrada

*/
