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
    findall(CS, complete_skeleton(CS), CSS),
    print(CSS).


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

% all_vertices(Vertices) <= Vertices is a list of all vertices
all_vertices(Vertices) :-
    findall(X, (edge(X, _); edge(_, X)), Vs),
    list_to_set(Vs, Vertices).


% subset(X, Y) <= Y is a subset of X
subset([], []).
subset([X|Xs], [X|Ys]) :- subset(Xs, Ys).
subset([_|Xs], Ys) :- subset(Xs, Ys).

% all_subsets(Edges, Subsets) <= Subsets is a list of all possible subsets of Edges
all_subsets(Edges, Subsets) :- findall(Subset, subset(Edges, Subset), Subsets).


% connected(X, Y, Edges) <= there is a path from X to Y in graph consiting of Edges
connected(X, Y, Edges) :-
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


% complete_subgraph(Subgraph) <= Subgraph is a complete subgraph
complete_subgraph(Subgraph) :-
    all_edges(Edges),
    all_vertices(Vertices),
    all_subsets(Edges, Subsets),
    member(Subgraph, Subsets),
    % Check if all vertices are contained in the subgraph
    forall(member(V, Vertices), (member(V-_, Subgraph); member(_-V, Subgraph))),
    % Check if all vertices are connected
    forall(
        (
            (member(X-_, Subgraph); member(_-X, Subgraph)), 
            (member(Y-_, Subgraph); member(_-Y, Subgraph)),
            X \= Y
        ),
        connected(X, Y, Subgraph)
    ).

% complete_skeleton(Subgraph) <= Subgraph is a complete skeleton
complete_skeleton(Subgraph) :-
    all_vertices(Vertices),
    length(Vertices, N),
    N1 is N - 1,
    complete_subgraph(Subgraph),
    % For a connected graph with n vertices, its complete skeleton is its complete subgraph with n-1 edges
    length(Subgraph, N1).

    
