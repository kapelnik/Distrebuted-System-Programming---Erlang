-module(ex6_204117089).
-export([songList/1, songGen/3]).

%songList receiving a list of songs, and adds all of their first and last letters. i.e for the song "Peanut butter jelly timE" the new vertices will be "P" and "E".
%in addition, for each song an edge with the song label will be added, connecting the first and last letter of the song.
songList(L)-> G=digraph:new(), songList(L,G).
songList([],G)->io:format("Number of edges in G: ~p~n", [digraph:no_edges(G)]),G;
songList([Head|Tail],G)->addTograph(G,getfirstletter(Head),getlastletter(Head,[]),Head),songList(Tail,G).

%adding first letter as a vertex as well as last letter, and connecting them with an edge labeled as "Song"
addTograph(G,First,Last,Song) ->V1=digraph:add_vertex(G,[First]),V2=digraph:add_vertex(G,[Last]),digraph:add_edge(G,V1,V2,Song).

%songGen returning a list of songs starting with Start and ending with end when each song have a starting letter same as the previous song last letter.
%first, need to check theres those two songs in graph G edges set, and that theres a connection between the last letter of Start and the first letter of End
songGen(G,Start,End) -> songGen(G,Start,End, digraph:get_short_path(G,[getlastletter(Start,[])],[getfirstletter(End)]),checksong(G,Start),checksong(G,End)).
%if one of the cases mentioned above happened, return an error atom
songGen(_,_,_,false,_,_)-> wrong_input;
songGen(_,_,_,_,X,Y) when (not X) or (not Y) -> wrong_input;
%songs found and a path found: chain the first letter + all the songs in the path for end + ending song
songGen(G,Start,End, _,_,_) ->  [Start]++lists:reverse(getEdges(G,getlastletter(Start,[]),getfirstletter(End)))++[End].

%getEdges returning a list of songs in the path from vertex(start ending letter) and vertex(end starting letter).
getEdges(G,Start,End)-> getEdgeslabel(G,digraph:get_short_path(G,[Start],[End]),[]).

%just in case of a wrong input
getEdgeslabel(_,false,_)->wrong_input;
%getEdgeslabel returning all the songs label from the edges between:  digraph:get_short_path(G,[Start],[End])
getEdgeslabel(_,[],L)->L;
%last letter, return the list built
getEdgeslabel(_,[_|[]],L)->L;
%for each set of vertices Head, Head2, look for an edge connecting them(already checked there is one for sure)
getEdgeslabel(G,[Head|[Head2|Tail]],L) -> getEdgeslabel(G,[Head2|Tail],[getSong(Head,Head2,[digraph:edge(G,X)||X<-digraph:edges(G)])]++L).

%check with pattern matching for an edge with vertices X and Y and return its label(song name)
getSong(X,Y,[{_,X,Y,Label}|_]) -> Label;
getSong(X,Y,[{_,_,_,_}|Tail]) -> getSong(X,Y,Tail).

%check song reading all the edges labels(songs names) and returning true if a song Song was found and false otherwise.
checksong(G,Song)-> lookfor(Song,[digraph:edge(G,X)||X<-digraph:edges(G)]).

%lookfor checks if an element Song is in the list, all the variables in the list are edges:{edge, vertex1, vertex2, edge label(song name))
lookfor(_,[])->false;
lookfor(Song,[{_,_,_,Song}|_])-> true;
lookfor(Song,[_|Tail])->lookfor(Song,Tail).

%return the last and first letter of a  given string
getfirstletter([Head|_])->Head.
getlastletter([],X)->X;
getlastletter([Head|Tail],_)->getlastletter(Tail,Head).

