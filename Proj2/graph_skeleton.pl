% FLP Project 2 (Prolog) - Skeleton of a graph
% Author: Michal Pysik (xpysik00)

:- [input2].


solve :- prompt(_, ''), read_lines(LL), split_lines(LL, SLL), create_edges(SLL), print(SLL).

:- dynamic edge/2.

create_edges([]).
create_edges([[[X],[Y]]|Es]) :- assert(edge(X, Y)), create_edges(Es).