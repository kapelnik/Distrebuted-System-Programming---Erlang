-module(ex3_204117089).
-export([sortResLC/1, sortResPM/1, sortResLM/1, mSort/1, qSort/1, filter_g/2, filter_p/2, even/1, fiboR/1, fiboT/1, matElemMult/2]).

sortResLC(L)-> sortResLC(L,2,[]).                                                           %%sorting using lists comprehansion
sortResLC(L1,0,L) -> [X || X<-L1, ((X rem 3) == 0)]++L;                                     %%adding to L all memmbers that X%3=K
sortResLC(L1,K,L) -> sortResLC(L1 ,K-1, [X ||  X <-L1, ((X rem 3) == K)]++L).               %%continue until K=0


sortResPM(L)->sortResPM(L,0, [])++sortResPM(L,1, [])++sortResPM(L,2, []).                   %%chaining 3 types of numbers by rem from 3
sortResPM([],_,L)->L;                                                                       %%using pattern maching
sortResPM([Head|Tail],K,L) -> sortResPM(Tail,K,checkrem(Head,K,Head rem 3,L)).

checkrem(Elem,K,K,L) -> [Elem|L];                                                           %%instead of guard..
checkrem(_,_,_,L) -> L.


                                                                                            %%sort array using lists module
sortResLM(L) -> lists:filter(fun(X)-> X rem 3 == 0 end, L)++lists:filter(fun(X)-> X rem 3 == 1 end, L)++lists:filter(fun(X)-> X rem 3 == 2 end, L).


mSort([Cell]) -> [Cell];                                                                    %%return the sorted array
mSort(L) -> {L1,L2} = lists:split(length(L) div 2,L) , mSort(mSort(L1),mSort(L2)).          %%splitting
mSort(L1,L2) -> mSort(L1,L2,[]).
mSort([],L2,L) ->merge(L, L2);
mSort(L1,[],L) ->merge(L, L1);                                                              %%one list is empty, union lists and return
mSort([Head1|Tail1],[Head2|Tail2],L)  when (Head1=<Head2) ->                                %%adding the smallest first memmber from each
      mSort(Tail1,[Head2|Tail2],[Head1|L]);                                                 %%array using guards
mSort([Head1|Tail1],[Head2|Tail2],L)  when (Head2<Head1) ->                                 %%adding from two sorted lists, the smallest head to the merged list
      mSort([Head1|Tail1],Tail2,[Head2|L]).

merge([],L)->L;                                                                             %% a union of L1 and L2
merge([Head|Tail],L)->merge(Tail,[Head|L]).

qSort([]) ->[];                                                                             %%pivot is first memmber in the list
qSort([Pivot|Tail]) -> qSort([X||X<-Tail, X=<Tail])++[Pivot] ++ qSort([X||X<-Tail, X>Tail]).


matElemMult(M1,M2) -> matElemMult(M1,M2,[]).                                                %%multiplying two matrices using zipwith
matElemMult([],[],M)->M;
matElemMult([Head1|Tail1],[Head2|Tail2],M)->matElemMult(Tail1,Tail2,M++[lists:zipwith(fun(X,Y) -> 1.0*X*Y end,Head1,Head2)]).        %mult ROW*COL



filter_g(L,Filter) when Filter==atoms -> [X||X<-L, is_atom(X)];                             %%clearing a list from atoms
filter_g(L,Filter) when Filter==numbers-> [X||X<-L, is_number(X)];                          %%or numbers using guards
filter_g(L,_) -> L.



filter_p(L,atoms) -> [X||X<-L, is_atom(X)];                                                 %%clearing a list from atoms
filter_p(L,numbers) -> [X||X<-L, is_number(X)];                                             %%or numbers using pattern matching
filter_p(L,_) -> L.

even([])->[];                                                                               %%clearing a list from odd numbers
even([Head|Tail]) when Head rem 2 ==0 -> [Head]++ even(Tail);                               %%using guards
even([_|Tail]) -> even(Tail).


fiboR(N) -> fiboR(1,N).                                                                     %%returning the Nth Fibonacci
fiboR(_,2)->1;                                                                              %%number using recursive
fiboR(_,1)->1;
fiboR(_,0)->0;
fiboR(S,K)->fiboR(S,K-1)+fiboR(S,K-2).

fiboT(1)->1;                                                                                %%returning the Nth Fibonacci
fiboT(2)->1;                                                                                %%number using recursive
fiboT(N) -> fiboT(1,1,N-1).
fiboT(_,Sum,2) -> Sum+1;
fiboT(Prev,Sum,N) -> fiboT(Prev+Sum,Sum+Prev,N-1).
