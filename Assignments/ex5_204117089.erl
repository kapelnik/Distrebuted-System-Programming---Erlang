-module(ex5_204117089).
-export([ring_parallel/2, ring_serial/2, mesh_parallel/3, mesh_serial/3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ring parallel create a chain of N procceses, when finished creating the ring the first node sends M messages to his following node, and the message passes through
%the other nodes until it reaches the first node. the procces is finished when all M messages passed in the ring
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ring_parallel(N,M) ->
  Pid = self(),
  P1 = get_pid_Name(1),
  StartTime = os:timestamp(),
  register(P1,spawn(fun() -> send_M_Messages(N,M,StartTime,Pid) end)),
  _=P1!{"Start",2},
  receive
    {"DONE",Time,Sent,Received}->{Time,Sent,Received}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ring serial sends itself M messages and pass them to himself N times
ring_serial(V,M) ->
  Pid = self(),

  StartTime = os:timestamp(),
  _=register(get_pid_Name(1),spawn(fun() -> send_M_Messages(V,M,StartTime,Pid) end)),
  _=send_serial(V,M,StartTime),
  receive
    {"DONE",Time,Sent,Received}->{Time,Sent,Received}
  end.

%send serial sends the  message V to the first node M times
send_serial(1,M,_)-> get_pid_Name(1)!{"Send myself",1,M};
send_serial(V,M,StartTime)-> get_pid_Name(1)!{"Send myself",V,M}, send_serial(V-1,M,StartTime).

%Send M Messages is the main loop of ring parallel and serial. receives messages sent to current process by pattern maching
send_M_Messages(N,M,StartTime,Pid) ->
  receive
    %when all messages sent, a DONE message is sent to the main proccess that called ring parallel or serial a message with the tuple asked
    {"Finish",M} ->
      _=Pid!{"DONE",round(timer:now_diff(os:timestamp(), StartTime) / 1000),M,M},
      _=destroy_Ring(N);

    {"Finish",_} ->send_M_Messages(N,M,StartTime,Pid);
    
    %make the next node in the ring and message it to open another node. finished when N process opened
    {"Start",N} -> %"Node1 creating procces N,
      register(get_pid_Name(N),spawn(fun() ->send_M_Messages(N,M,StartTime,Pid) end)),
      get_pid_Name(1) ! {"Send",0},
      send_M_Messages(N,M,StartTime,Pid);

    {"Start",X} ->   %Node1 creating procces X,
      register(get_pid_Name(X),spawn(fun() ->send_M_Messages(N,M,StartTime,Pid) end)),
      get_pid_Name(X) ! {"Start", X+1},
      send_M_Messages(N,M,StartTime,Pid);

    %This pattern is for ring serial. sending itself messages and finished when V*M mesages sent
    {"Send myself", 1,1} -> Pid!{"DONE",round(timer:now_diff(os:timestamp(), StartTime) / 1000),M,M};
    {"Send myself",_,0} -> send_M_Messages(N,M,StartTime,Pid);

    {"Send myself",V,K} ->
      _=get_pid_Name(1)!{"Send myself",V,K-1},
      send_M_Messages(N,M,StartTime,Pid);

    %Send message is sent by the main process to itself to continue making M messages. and forwarding them to the next node
    {"Send",M} ->
      get_pid_Name(2) ! {"forward",2,M},
      send_M_Messages(N,M,StartTime,Pid);

    {"Send",X} ->
      get_pid_Name(1) ! {"Send",X+1},
      get_pid_Name(2) ! {"forward",2,X},
      send_M_Messages(N,M,StartTime,Pid);

    %forward is a pattern for a node in the ring to receive a message and forward it to the next node. when the last node receives the Mth message, it sends the
    %main process a finish call
    {"forward",N,K} ->
      get_pid_Name(1) ! {"Finish",K},
      send_M_Messages(N,M,StartTime,Pid);

    {"forward",X,K} ->   %Node X sending message K
      get_pid_Name(X+1) ! {"forward",X+1,K},
      send_M_Messages(N,M,StartTime,Pid)
  end.

%destroy ring unregisters all processes in the ring
destroy_Ring(N) -> destroy_Ring(1,N).
destroy_Ring(N,N) ->unregister(get_pid_Name(N));
destroy_Ring(X,N) ->unregister(get_pid_Name(X)), destroy_Ring(X+1,N).

%get pid name returns an atom - pidX when X is an integer
get_pid_Name(X) -> list_to_atom(lists:flatten(string:concat("pid",integer_to_list(X)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mesh parallel creates a grid of SqrtN*SqrtN processes and makes the Cth node the master that creates the messages
%than Master process start sending M messages to all its neighbours.
%when a node in the grid receives a message, it checks if he already received it, and if not, an ACK message is sent from him to the master and the message
%received is forwarded to its neighbours. when all the nodes received M messages, Master process call terminate and return a tuple with the answer to the main process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mesh_parallel(SqrtN,M,C)->
  Pid=self(),
  {X,Y} = master(C,SqrtN),
  Master = get_pid_Name2D(X,Y),
  StartTime = os:timestamp(),
  _=register(Master,spawn(fun() -> send_M_Messages2D(SqrtN,M, X,Y,StartTime,[Master|[1]],Pid) end)),
  _=Master!{"Start",1,1},
  receive
    {"DONE",Time,Sent,Received}->terminate(SqrtN),{Time,Sent,Received}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mesh serial create a process that creates a map of {X,Y}=>[received messages] and like mesh parallel send itself by neighbours M messages
mesh_serial(SqrtN,M,C)->
  Pid=self(),
  {X,Y} = master(C,SqrtN),
  Master = get_pid_Name2D(X,Y),
  StartTime = os:timestamp(),
  Map =createMap(1,1,#{},SqrtN),

  _=register(Master,spawn(fun() -> send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,0,Pid) end)),
  _=Master!{"begin",1},
  receive
    {"DONE",Time,Sent,Received}->{Time,Sent,Received}
  end.


%createmap returns a map with all the tuples nXn=>[0] as an empty list of received messages
createMap(SqrtN,SqrtN,M,SqrtN)->M#{{SqrtN,SqrtN}=>[0]};
createMap(X,SqrtN,M,SqrtN) -> createMap(X+1,1,M#{{X,SqrtN}=>[0]},SqrtN);
createMap(X,Y,M,SqrtN) -> createMap(X,Y+1,M#{{X,Y}=>[0]},SqrtN).

%send_M_Messages2D is the main loop for receiving messages for the processes of
send_M_Messages2D(SqrtN,M,X,Y,StartTime,[MyPid|Data],Pid) ->
  receive
    {"lunch"} ->
      Nibrs = toSend(SqrtN,X,Y),
      MyPid!{"Send",Nibrs,1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);


    {"Send",[{A,B}|[]],M} -> get_pid_Name2D(A,B)!{"Forward",M,A,B},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

    {"Send",[{A,B}|[]],K} -> get_pid_Name2D(X,Y)!{"Forward",K,A,B},
      Nibrs = toSend(SqrtN,X,Y),
      MyPid!{"Send",Nibrs,K+1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);


    {"Send",[{A,B}|Tail],K} -> get_pid_Name2D(A,B)!{"Forward",K,A,B},
      MyPid!{"Send",Tail,K}, % io:format("~p,~p sending ~p ~n",[A,B,K]),
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %%Starting Mesh grid, When X and Y are 1,1. just continue from 1,2 node..
    {"Start",X,Y} when (X == 1) and (Y == 1) ->
      MyPid ! {"Start",1,2},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %done creating the grid, and Master is the last node. begin message sending.
    {"Start",X,Y} when (X == SqrtN) and (Y == SqrtN) ->
      MyPid!{"lunch"},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %this is master, continue to next node
    {"Start",X,Y} when Y == SqrtN->
      MyPid ! {"Start",X+1,1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %this is master, continue to next node
    {"Start",X,Y} ->
      MyPid ! {"Start",X,Y+1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %creating last node in the grid and lunching
    {"Start",SqrtN,SqrtN} ->
      ToSend=get_pid_Name2D(SqrtN,SqrtN),  %io:format("creating - ~p ~n",[Pid]),
      register(ToSend,spawn(fun() ->send_M_Messages2D(SqrtN,M, X,Y,StartTime,[ToSend|[0]],Pid) end)),
      MyPid ! {"lunch"},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %creating last node in line and continue to next line
    {"Start",Ln,SqrtN} ->
      ToSend=get_pid_Name2D(Ln,SqrtN),  %io:format("creating - ~p ~n",[Pid]),
      register(ToSend,spawn(fun() ->send_M_Messages2D(SqrtN,M, X,Y,StartTime,[ToSend|[0]],Pid) end)),
      MyPid ! {"Start",Ln+1,1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

  %creating a node, and continue to next in line
    {"Start",Ln,Cl} ->
      ToSend=get_pid_Name2D(Ln,Cl),  %io:format("creating - ~p ~n",[Pid]),
      register(ToSend,spawn(fun() ->send_M_Messages2D(SqrtN,M, X,Y,StartTime,[ToSend|[0]],Pid) end)),
      MyPid ! {"Start",Ln,Cl+1},
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);

    %A NODE receives a message to forword to his neighbors if the node is master than the message is ignored
    {"Forward",_,X,Y}  -> send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|Data],Pid);
    {"Forward",K,A,B} ->
      Tmp = lists:member(K,Data),
      if
        not(Tmp) -> NewData = send(SqrtN,X,Y,A,B,K,Data);
        true -> NewData = Data
      end,
      send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|NewData],Pid);

    %an ACK message is sent from each node to the master when a NEW message has arrived
    {"ACK"} ->
      Counter = getfirst(Data)+1,
      if

        Counter == 1+M*((SqrtN*SqrtN)-1) ->terminate(SqrtN),
          Pid!{"DONE",round(timer:now_diff(os:timestamp(), StartTime) / 1000),M*((SqrtN*SqrtN)-1),Counter-1};
        true ->
          send_M_Messages2D(SqrtN,M, X,Y,StartTime,[MyPid|[Counter]],Pid)
      end


  end.

%send_M_Messages2D_single is the main loop for mesh serial
send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid)->
  receive
  %Start sending messages to all neighbours(in the Map)
    {"begin",M} -> Nibrs = toSend(SqrtN,X,Y),
      rise(Nibrs,Master,M),
      send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid);

    {"begin",K} -> Nibrs = toSend(SqrtN,X,Y),
      rise(Nibrs,Master,K),
      Master!{"begin",K+1},
      send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid);

    %each node in the Map receives a rise message to add the Kth message to his list in the map. than an ACK message is sent
    {"rise",_,X,Y}->	send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid);

    {"rise",K,A,B}->
      Sent = maps:get({A,B},Map),
      if
        length(Sent)==M+1-> send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid);
        true -> 		Tmp = lists:member(K,Sent),
          if
            not(Tmp) ->Master!{"ACK"},
              rise(toSend(SqrtN,A,B),Master,K),
              send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map#{{A,B}=>[K|Sent]},Counter,Pid);
            true-> 	send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Counter,Pid)
          end
      end;

    %each node receiving a new message sends an ACK message to master. the process is finished when all node received all M messages.
    {"ACK"} ->
      Tmp = Counter+1,
      if
        Tmp==M*((SqrtN*SqrtN)-1) ->
          unregister(Master),
          Pid!{"DONE",round(timer:now_diff(os:timestamp(), StartTime) / 1000),M,Tmp};
        true -> send_M_Messages2D_single(SqrtN,M, X,Y,StartTime,Master,Map,Tmp,Pid)
      end

  end.
  %rise sends a rise message to all neighbours
rise([{A,B}|[]],Master,K) -> Master!{"rise",K,A,B};
rise([{A,B}|Tail],Master,K) -> Master!{"rise",K,A,B}, rise(Tail,Master,K).

%send sends a message to all neighbours of {A,B}, and checks that the message wasnt sent already
send(SqrtN,X,Y,A,B,K,[0]) ->
  [sendtoNibrs(SqrtN,X,Y,A,B,K,false),0];
send(SqrtN,X,Y,A,B,K,Data) ->
  removeatoms([sendtoNibrs(SqrtN,X,Y,A,B,K,lists:member(K,Data))|Data]).

sendtoNibrs(_,_,_,_,_,_,true)-> no;
sendtoNibrs(SqrtN,X,Y,A,B,K,false) ->
        Nibrs = lists:delete({X,Y},toSend(SqrtN,A,B)),
        Master = get_pid_Name2D(X,Y),
        Tmp = lists:member(Master,registered()), %Check if master has not been unregistered
        if
        Tmp->  Master!{"ACK"};
        true -> no
        end,
        sendNibrs(Nibrs,K),
        K.
sendNibrs([{A,B}|[]],K)->%io:format("forwarding ~p to: ~p,~p~n",[K,A,B]),
  Tmp =lists:member(get_pid_Name2D(A,B),registered()),
  if
    Tmp  -> sendifReg(get_pid_Name2D(A,B),{"Forward",K,A,B},Tmp);
    true -> no
  end;
sendNibrs([{A,B}|Tail],K)->
  sendifReg(get_pid_Name2D(A,B),{"Forward",K,A,B},lists:member(get_pid_Name2D(A,B),registered())), sendNibrs(Tail,K).

%send a message if Pid is registered
sendifReg(_,_,false) ->no;
sendifReg(Pid,Message,true) ->
  Tmp = lists:member(Pid,registered()),
  if
    Tmp-> Pid!Message;
    true -> no
  end.
%unregister all processes pidXoX
terminate(SqrtN) ->terminate(1,1,SqrtN).
terminate(SqrtN,SqrtN,SqrtN)-> Pid =get_pid_Name2D(SqrtN,SqrtN), unreg(Pid,lists:member(Pid,registered()));
terminate(A,SqrtN,SqrtN) -> Pid =get_pid_Name2D(A,SqrtN),  unreg(Pid,lists:member(Pid,registered())), terminate(A+1,1,SqrtN);
terminate(A,B,SqrtN) -> Pid =get_pid_Name2D(A,B),  unreg(Pid,lists:member(Pid,registered())) , terminate(A,B+1,SqrtN).
unreg(Pid,true)-> unregister(Pid);
unreg(_,_)->ok.

%return an atom pidXoX when X is an integer
get_pid_Name2D(X,Y)-> list_to_atom(lists:flatten(string:concat(string:concat("pid",integer_to_list(X)),string:concat("_",integer_to_list(Y))))).

%toSend returns a list of tuples of neighbours location in a grid [{1,2},{2,1}] are the neighbours of {1,1}
toSend(SqrtN,X,Y)-> removeatoms([up(SqrtN,X,Y)]++[down(X,Y)]++[left(X,Y)]++[right(SqrtN,X,Y)]).
down(1,_)->no;
down(X,Y)->{X-1,Y}.
up(N,N,_)->no;
up(_,X,Y)->{X+1,Y}.
right(N,_,N)->no;
right(_,X,Y)->{X,Y+1}.
left(_,1)->no;
left(X,Y)->{X,Y-1}.

%calculate the location of master in the mesh grid
master(C,SqrtN) when (C rem SqrtN) == 0 ->{round(C/SqrtN),SqrtN};
master(C,SqrtN) ->{1+floor((C-1)/SqrtN),C rem SqrtN}.

getfirst(Head) when is_integer(Head) -> Head;
getfirst([Head|_])->Head;
getfirst(_)->0.

removeatoms(L)->removeatoms(L,[]).
removeatoms([],L)->L;
removeatoms([no|Tail],L)  ->  removeatoms(Tail,L);
removeatoms([Head|Tail],L)  ->  removeatoms(Tail,[Head|L]).

