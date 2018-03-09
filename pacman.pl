  :-use_module(library(lists)). 
  :-dynamic ball/2. 
  :-dynamic height/1. 
  :-dynamic width/1.
  :-dynamic the_end/1.
  :-dynamic win/1.
  
% Carrega mapa
start:-
  new_world('mapa'),
  iniciaRanking,
  draw_world.

% Reinicia Jogo
restart:-
  reconsult('pacman'),
  start.

% Evento de game over
the_end(Ghost):- write('GAME OVER'), nl, write('O monstro '), write(Ghost), write(' te matou'), nl, write('Sua pontuacao foi de '), count_points,
  write(' pontos'), nl, write('BAD END!!!'), nl, write('Digite seu nome de jogador (lembre do ponto final): '), read(N), count_points(P),add_score(N,P).

% Evento de zeramento de jogo
win:- nl, write('GREETZ!!!'), nl, write('Sua pontuacao foi de '), count_points_bonus, write(' pontos'), nl, 
  write('Digite seu nome de jogador (lembre do ponto final): '), read(N), count_points_bonus(P),add_score(N,P).

new_world(Whatever):-
  consult(Whatever),
  grid([P|G]), 
  length([P|G],H), 
  retract(height(_)), 
  assert(height(H)), 
  length(P,W), 
  retract(width(_)), 
  assert(width(W)), 
  create_balls. 

% Variaveis de dimensoes do jogo.
  width(0).
  height(0).

% Get de elemento pelas coordenadas
  element(X,Y,Whatever):- 
  grid(G), 
  nth1(Y,G,Line), 
  nth1(X,Line,Whatever). 
  
% Sistema de pontos

count_points :-  
  findall(element(X,Y,0),ball(X,Y),L),
  length(L,X),
  N is 96 - X,
  write(N).
  
count_points(N):-  
  findall(element(X,Y,0),ball(X,Y),L),
  length(L,X),
  N is 96 - X.
  
count_points_bonus :-  
  findall(element(X,Y,0),ball(X,Y),L),
  length(L,X),
  N is 100 + 96 - X,
  write(N).  
  
count_points_bonus(N) :-  
  findall(element(X,Y,0),ball(X,Y),L),
  length(L,X),
  N is 100 + 96 - X.
  
write_leaves([]).
write_leaves([H|T]) :- write_leaves(T), write_leaves(H).
write_leaves(X) :- write(X),nl.

iniciaRanking :-
  C = ([[5,'Robot'],[7,'Avenger'],[15,'GeekGuy'],[35,'Master666']]),
  nb_setval(counter, C).

ranking :-
  nb_getval(counter, L),
  write('      MELHORES PONTUACOES       '),nl,
  write_leaves(L).

add_score(N,P) :-
  N1 = [[P,N]],
  nb_getval(counter, L),
  append(L,N1,L1),
  sort_list(L1,S),
  nb_setval(counter, S).

sort_list(L,S) :-
  sort(0,  @=<, L,  S).
  
  
% Sistema de vidas

/*revamp :-
  pacman(2,2,normal),
  ghost(2,9,'Black'), 
  ghost(9,9,'Red'),
  diamond(2,6).
  %chamar o mapa do jeito que tava
  
life_verify(0) :-
  the_end(Ghost).
  
life_verify(N) :-
  ghost(X,Y,Ghost),
  pacman(X,Y,normal),
  draw_world,
  revamp,
  N1 is N - 1,
  life_verify(N1). */  
  
% Insercao das bolas brancas
  create_balls :- 
  element(X,Y,0), 
  assert(ball(X,Y)), 
  fail. 
  create_balls. 
  
% INTERFACE - desenha os elementos do jogo
% desenhar mapa do jogo
  draw_world :- 
  height(Lim), 
  draw_lines(1,Lim). 
  
% Desenha as linhas de jogo  
  draw_lines(Y,Lim) :- 
  Y > Lim,!. 
  draw_lines(Y,Lim) :- 
  draw_line(Y), 
  nl, 
  NY is Y+1, 
  draw_lines(NY,Lim). 
  
% Desenhar linha unica
  draw_line(Y) :- 
  width(Lim), 
  draw_a_line(1,Y,Lim). 
  
% Desenha caracters de uma linha
  draw_a_line(X,_,Lim) :- 
  X > Lim. 
  draw_a_line(X,Y,Lim) :- 
  X =< Lim, 
  draw(X,Y), 
  NX is X + 1, 
  draw_a_line(NX,Y,Lim). 
  
% Desenha os objetos do jogo em coordenadas X,Y
draw(X,Y):- 
  ghost(X,Y,_), 
  draw_ghost. 
  
draw(X,Y):- 
  pacman(X,Y,Type), 
  draw_pacman(Type). 
  
draw(X,Y):- 
  diamond(X,Y), 
  draw_diamond. 
  
draw(X,Y):- 
  element(X,Y,T), 
  draw(X,Y,T). 
  
draw_ghost:-write('A'). 
draw_diamond:-write('V'). 
draw_pacman(normal):-write('@'). 
draw_pacman(strong):-write('*'). 
draw(X,Y,0):-ball(X,Y),write('.'). 
draw(_,_,0):-write(' '). 
draw(_,_,1):-write('#').
draw(_,_,BlackHole) :- BlackHole > 1,write('~').

% Movimentos basicos
% Mudar a posicao do pac
  
move_pacman(X,Y):-
  ghost(X,Y,Ghost),
  retract(pacman(_,_,normal)),
  assert(pacman(X,Y,normal)),
  restart,
  the_end(Ghost).
  
move_pacman(X,Y):-
  blackhole(X,Y,NX,NY),  
  test_pos(NX,NY),
  retract(pacman(_,_,T)),
  assert(pacman(NX,NY,T)).

move_pacman(X,Y):-
  test_pos(X,Y),
  retract(pacman(_,_,T)),
  assert(pacman(X,Y,T)).

% Muda posicao dos fantasmas
move_ghosts:-
ghost(X,Y,'Red'),
ghost(W,Z,'Black'),
hunt(X,Y,NX,NY),
move_ghost(NX,NY,'Red'),
hunt(W,Z,NW,NZ),
move_ghost(NW,NZ,'Black').

move_ghost(X,Y,Z):-
  blackhole(X,Y,NX,NY),
  retract(ghost(_,_,Z)),
  assert(ghost(NX,NY,Z)).

move_ghost(X,Y,Z):-
  retract(ghost(_,_,Z)),
  assert(ghost(X,Y,Z)).

% reacao do jogo a vitoria ou derrota
play :-
  ghost(X,Y,Ghost),
  pacman(X,Y,_),
  restart,
  the_end(Ghost).
  
play :-
 findall(element(X,Y,0),ball(X,Y),L),
 L == [],
 win.

play:-
  read(S),move(S),!,move_ghosts,draw_world,play.
  play:-play.

move :-
  read(INPUT),
  move(INPUT).
  
% movimentacao do pacman
move(8):-
  pacman(X,Y,_),
  NY is Y - 1,
  move_pacman(X,NY).
  
move(2):-
  pacman(X,Y,_),
  NY is Y + 1,
  move_pacman(X,NY).
  
move(4):-
  pacman(X,Y,_),
  NX is X - 1,
  move_pacman(NX,Y).
  
move(6):-
  pacman(X,Y,_),
  NX is X + 1,
  move_pacman(NX,Y).
  
% testa posicao de paredes e fantasmas
test_pos(X,Y):-
  ghost(X,Y,_),
  assert(the_end).
  
test_pos(X,Y):-
  element(X,Y,1),
  write('movimento invalido - Error: Parede no caminho'),
  nl,!,fail.

test_pos(X,Y):-
  retract(ball(X,Y)).

test_pos(X,Y):-
  \+element(X,Y,_),
  write('movimento invalido - Error: Sem Espaço'),
  nl,!,fail.

test_pos(X,Y):-
  \+ball(X,Y).

% Tuneis especiais
 blackhole(X,Y,NX,NY):-
 element(X,Y,Z),
 Z > 1,
 element(NX,NY,Z),
 \+ (NX == X,NY == Y),!. 
  
% Inteligencia dos fantasmas

aux(X,Y,A,B):-
pacman(W,Z,normal),
A is W - X,
B is Z - Y.

% algoritmo de caça dos fantasmas

hunt(X,Y,NX,NY):-
aux(X,Y,A,B),
A == 0,
B == 0,
NX is X,
NY is Y.

hunt(X,Y,NX,NY):-
aux(X,Y,A,B),
A > 0,
abs(A) >= abs(B),
NX is X + 1,
NY is Y,
adjs((X,Y),P),
member((NX,NY),P).

hunt(X,Y,NX,NY):-
aux(X,Y,A,B),
A < 0,
abs(A) >= abs(B),
NX is X - 1,
NY is Y,
adjs((X,Y),P),
member((NX,NY),P).

hunt(X,Y,NX,NY):-
aux(X,Y,A,B),
B > 0,
abs(B) > abs(A),
NY is Y + 1,
NX is X,
adjs((X,Y),P),
member((NX,NY),P).

hunt(X,Y,NX,NY):-
aux(X,Y,A,B),
B < 0,
abs(B) > abs(A),
NY is Y - 1,
NX is X,
adjs((X,Y),P),
member((NX,NY),P).

hunt(X,Y,NX,NY):-
adjs((X,Y),P),
member((NX,NY),P).

% posicoes adjacentes livres
adjs(P,As) :-
  findall((X,Y),(adj(P,(X,Y)),\+ element(X,Y,1)),As).

adj((X,Y),(X,NY)) :-
  height(H),
  Y < H,
  NY is Y + 1.

adj((X,Y),(X,NY)) :-
    Y > 1,
    NY is Y - 1.
  
adj((X,Y),(NX,Y)) :-
    width(W),
    X < W,
    NX is X + 1.
  
adj((X,Y),(NX,Y)) :- 
    Y > 1,
    NX is X - 1.
  
distance((X,Y),(X2,Y2),D) :-
  D is abs(X - X2) + abs(Y - Y2).
