-module(ex2_204117089).
-export([findKelem/2, reverse/1, deleteKelem/2, addKelem/3, union/2]).

findKelem([Head|_],0)->Head;						%%look for K'th element in list, error if list too short
findKelem([_|Tail],K) -> findKelem(Tail,K-1);
findKelem([],_) -> error_no_k_elem.


reverse(L)->reverse(L,[]).						%%reversing a list
reverse([],L)->L;
reverse([Head|Tail],L) -> reverse(Tail,[Head|L]).


deleteKelem(L,Elem)->deleteKelem(L,Elem,[]).				%%delete the K'th element, return the new list without it
deleteKelem([],_,L) -> reverse(L);
deleteKelem([Elem|Tail],Elem,L)->deleteKelem(Tail,Elem,L);
deleteKelem([Head|Tail],Elem,L)->deleteKelem(Tail,Elem,[Head|L]).


addKelem(L,K,Elem)->reverse(addKelem(L,K-1,Elem,[])).			%%adding an element to the K'th place
addKelem(List,0,Elem,Seen)->continue(List,[Elem|Seen]);
addKelem([Head|Tail],K,Elem,Seen)->addKelem(Tail,K-1,Elem,[Head|Seen]).

continue([],L2)->L2;							%%function that combines two lists
continue([Head|Tail],L2)->continue(Tail,[Head|L2]).


union(L1,L2)->reverse(union(L1,L2,[])).	%%union of two lists, without double apearance
union([],[],L)->L;
union([],[Head|Tail],L)->union([],deleteKelem(Tail,Head),[Head|L]);
union([Head|Tail],L2,L)->union(deleteKelem(Tail,Head),deleteKelem(L2,Head),[Head|L]).

