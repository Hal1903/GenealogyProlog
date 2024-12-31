% Some simple test Prolog programs
% --------------------------------
% *taisho, *showa, *takahito, *akihito, masahito, 
% naruhito, fumihito, *aiko, *kako, hisahito, *mako
% tomohito, norihito, akiko, yoko, tsuguko: 16
% 
% Knowledge bases 2024, 
% parent(parent_name, child)
parent(yoshihito, hirohito).
parent(yoshihito, takahito). % shin-ou

parent(takahito, tomohito).
parent(takahito, norihito).

parent(tomohito, yoko).
parent(tomohito, akiko).

parent(norihito, tsuguko).

parent(hirohito, akihito). % previous emperors
parent(hirohito, masahito).

parent(akihito, naruhito). % prev, current emperor
parent(akihito, fumihito).

parent(naruhito, aiko).

parent(fumihito, kako).
parent(fumihito, mako).
parent(fumihito, hisahito).

% added creatures for a testing purpose of nth_cousins(3, _)
parent(tsuguko, tsathoggua).
parent(aiko, nyarlathotep).
parent(masahito, shoggoth).
parent(shoggoth, mi-go).

age(0, tsathoggua).
age(1, nyarlathotep).
age(2, shoggoth).
age(-1, mi-go).

age(145, yoshihito).
age(123, hirohito). % dead
age(109, takahito). % dead at 100
age(89, masahito).

age(91, akihito).
age(78, tomohito). % dead
age(70, norihito). % dead at 47
age(64, naruhito). 
age(59, fumihito).

age(33, mako). % married and not a member of emperor structure now
age(30, kako).
age(23, aiko).
age(18, hisahito).

age(43, akiko).
age(41, yoko).
age(38, tsuguko).

grandparent(X, Z) :-
    parent(X, Y),   % X is a parent of Y
    parent(Y, Z).   % Y is a parent of Z

great_grand_parent(V, Z) :-
    parent(V, X),   % V is a parent of X
    parent(X, Y),   % X is a parent of Y
    parent(Y, Z).   % Y is a parent of Z

sortByAge(Arr, Sorted):-
    findall(A, (
    member(M, Arr),
    age(A, M)), ArrAge),
    
    sort(ArrAge, SortedAge),
    
    findall(S, (
    member(M, SortedAge),
    age(M, S)), Sorted).

sibling(X, S) :-
    findall( Y, (
    parent(V, X), % V is a parent of X
    parent(V, Y) % V is a parent of Y
    ), S1),
    delete(S1, X, S).

cousin(X, Cousins) :-
    parent(X1, X),             % X1 is the parent of X
    sibling(X1, Siblings),     % Find all siblings of X1 (X's aunts and uncles)
    findall(Y, (
        member(Y1, Siblings),  % For each sibling Y1 of X1
        parent(Y1, Y)          % Y1 is a parent of Y (Y is a cousin of X)
    ), Cousins).

nth_parent(1, X, Y) :-
    parent(X, Y).

nth_parent(N, X, Y) :- % X is return, uniquely determined.
% careful: variable must have a capital letter or underscore first
    N > 1,
    parent(X, Z), 
    N1 is N - 1,                
    nth_parent(N1, Z, Y).  

% list all children
all_children(X, Children) :-
    findall(Y, 				% a variable to return
            parent(X, Y), 	% collect all satisfying X is parent of Y relationship
            Children		% result
            ).

%nth_children() :- % list all children

gather(MEMBER, All) :- % to gather all n+1 th gen member's children
    findall(Child, (
        member(M, MEMBER),              % BF each M in MEMBER
        all_children(M, Children),      % Check all children of M
        member(Child, Children)         % Collect each child in Children
    ), All).

%nth_children(1, X, Children) :- % base case
kthchild(C,X,1) :-
    gather(X, C).
    %findall(Y, parent(X, Y), Children).

% Recursive case: For N > 1, find the children of the (N-1)th generation children
kthchild(C,X,N) :-
%nth_children(N, X, Children) :- % X is starting parent, Q is queued children, Children is return
    N > 1,
    N1 is N-1,
    gather(X, X1),
    %length(X1, L),
    kthchild(C, X1, N1).  % Find the Nth generation child Y from Z

nthcousin(1, X, Cousins):-
    cousin(X, Cousins).

% Recursive case: Nth cousins for N > 1, X is starting point
nthcousin(N0, X, Cousins) :-
    N is N0 + 1,
    
    % Find the Nth and (N-1)th ancestors of X
    nth_parent(N, Ancestor0, X),
    nth_parent(N0, Ancestor1, X),
    
    % Get all children of the Nth ancestor (same generation as Ancestor1)
    all_children(Ancestor0, Ch),
    
    % Remove Ancestor1 from Ch to avoid including X's branch
    delete(Ch, Ancestor1, REST),
    
    % Collect Nth-generation descendants from each member in REST
    findall(Descendant, (
        member(R, REST),
        kthchild(Descendants, [R], N0),
        member(Descendant, Descendants)
    ), PotentialCousins),
    
    % Remove X from the list if present and return as Cousins
    delete(PotentialCousins, X, Cousins).

nthcousinkremoved(X,Y,N,K) :-
    nthcousin(N, X, Cousins),
    kthchild(Y, Cousins, K)
    . %(C,X,N)

/*
nthcousin(1, X, Cousins) :-
	cousin(X, Cousins).
    
% Nth cousins are descendants of the first cousins
nthcousin(N, X, Cousins) :- % starts from X
    N1 is N - 1,
    cousin(X, C),
    nth_children(N1, C, Cousins).
*/





