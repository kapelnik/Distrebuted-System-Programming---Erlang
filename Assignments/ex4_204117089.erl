-module(ex4_204117089).
-export([flatten/1, smaller/2, replace/3, mapSub/2]).


flatten(L) -> lists:reverse(flatten(L,[])).
flatten([],L) -> L;							      %%finishes with empty list
flatten([Head|Tail],L) when is_list(Head) -> flatten(Tail, flatten(Head,L));  %%guard: checks if the list is a list or a variable
flatten([Head|Tail],L) -> flatten(Tail,[Head|L]).			      %%Head is a variable, no need to flatten


smaller(L,K)-> lists:map(fun(X)-> X  =< K end, L).			      %%map takes each cell in the list as X and uses fun to check if smaller and return a boolean

replace(L,Old,New) -> lists:reverse(replace(L,Old,New,[])).		      		
replace([],_,_,L) -> L;							   		%%finished, return L
replace([Head|Tail],Old,New,L) when Head =:= Old -> replace(Tail,Old,New,[New|L]);	%%using pattern maching, take each head and check if need for replacement
replace([Head|Tail],O,N,L) -> replace(Tail,O,N,[Head|L]).


mapSub(L,[])->isNumber(L,L);
mapSub(L1,L2)   when is_list(L2) and (length(L1) =/= length(L2)) -> lenError;		%%check with guards if length is equal and if L2 is a list
mapSub(L1,L2)  when is_list(L2)  -> isNumbers(L1,L2);					%%check with guards if L2 is a number, else an error atom will be returned
mapSub(L,K) when is_number(K)  -> lists:map(fun(X) -> X-K end, L);			
mapSub(_,_) -> error.
sub([Head,Head2|_]) ->Head-Head2.		%%zip returns a tuple of each cell in the list and after making it a list, here we calculate head1-head2

isNumbers(L1,L2) -> isNumbers(L1,L2,L1,L2).							%%following functions checks if all the numbers in the lists givenare numbers(if in equal length)
isNumbers([],[],L1,L2) ->   lists:map(fun(X) -> sub(tuple_to_list(X)) end, lists:zip(L1,L2));	%%zip returnes a list of tuples -[{L1(0),L2(0)},{L1(1),L2(1)},{L1(2),L2(2)}...]
isNumbers([Head1|_],[Head2|_],_,_) when not is_number(Head1) or not is_number(Head2) -> error;
isNumbers([_|Tail1],[_|Tail2],L1,L2) ->isNumbers(Tail1,Tail2,L1,L2).

isNumber([],L) -> L;		%%same check when only one list
isNumber([Head|Tail],L) when is_number(Head) -> isNumber(Tail,L);
isNumber([Head|Tail],L) -> error.
