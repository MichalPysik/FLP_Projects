% FLP Project 2 (Prolog) - Skeleton of a graph
% Author: Michal Pysik (xpysik00)

% Load input2.pl for parsing stdin
:- [input2].


% The main goal
solve :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL, SLL),
    create_edges(SLL),
    print(SLL).


% edge(A, B) <= there is an edge from A to B
:- dynamic edge/2.

% create_edges(LLL) creates edges from a list of lists of "singleton-list tuples" LLL
create_edges([]).
create_edges([[[X],[Y]]|Es]) :-
    assert(edge(X, Y)),
    create_edges(Es).

% all_edges(Edges) <= Edges is a list of all edges
all_edges(Edges) :-
    findall(X-Y, edge(X, Y), Edges).


% subset(X, Y) <= Y is a subset of X
subset([], []).
subset([X|Xs], [X|Ys]) :- subset(Xs, Ys).
subset([_|Xs], Ys) :- subset(Xs, Ys).

% all_subsets(Edges, Subsets) <= Subsets is a list of all possible subsets of Edges
all_subsets(Edges, Subsets) :- findall(Subset, subset(Edges, Subset), Subsets).


% connected(X, Y, Edges) <= there is a path from X to Y in graph consiting of Edges
connected(X, Y, Edges) :-
    all_edges(Edges),
    dfs(X, Y, [X,Y], Edges), !.

% -- Depth-first search algorithm for reaching node Y from node X in graph consiting of Edges --
% Terminating condition when X and Y are directly connected
dfs(X, Y, _, Edges) :-
    member(X-Y, Edges); member(Y-X, Edges).
% Recursive case where new node Z is added to Visited, search continues from Z
dfs(X, Y, Visited, Edges) :-
    (member(X-Z, Edges); member(Z-X, Edges)),
    \+ member(Z, Visited),
    dfs(Z, Y, [Z|Visited], Edges).
 

% all_complete_skeletons :- Comp_skelets is list of all complete skeletons in graph consisting of Edges
%all_complete_skeletons(Edges, CompSkelets) :-
%    all_edges(Edges),
%    bagof(CompSkelet, (

 %   ))


