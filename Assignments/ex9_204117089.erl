%%%-------------------------------------------------------------------
%%% @author tal kapelnik
%%% @copyright (C) 2020, tal kapelnik
%%% @doc  ets intro.
%%%
%%% @end
%%% Created : 21. Jun 2020 12:42
%%%-------------------------------------------------------------------
-module(ex9_204117089).
-author("kapelnik").
%% API
-export([etsBot/0]).
%c(ex9_204117089). ex9_204117089:etsBot().

%%%-------------------------------------------------------------------
%etsBot takes a given commands file to insert and manipulate an ets table,
%this functions reads each command from the input and activates them sequentially.
%after all commands are finished, prints the ets to a file named etsRes.ets and deletes the ets table from the RAM.
%%%-------------------------------------------------------------------
etsBot() ->
  %open the input file
  {ok, Data} = file:open("etsCommands.txt",[read]),
  Input = readFile(Data),
  %first line in the file is the type of the requested ets
  [Ets_Type|Commands] = remove_enter(Input),
  %Create an ETS table named mytable
  Ets = ets:new(mytable,[list_to_atom(Ets_Type),named_table]),
  %if something is wrong with the input, the program will not crash
  %start executing commends:


  try process_Commands(Commands,Ets_Type) of
    _->
      %if all commands successfully executed, write the ets table to the log file
      file:open("etsRes_204117089.ets",[read, write, append]),
      dump_to_file(ets:tab2list(mytable))
  catch
    error:E->  io:format("~p~n~p~n",[E,ets:tab2list(mytable)]),
      input_error
  end ,



  %when finished, delete the table from the RAM memory
  ets:delete(Ets).

%%%-------------------------------------------------------------------
%helpful functions:
%%%-------------------------------------------------------------------

process_Commands([],_)->okay;
process_Commands([FirstLine|Commands],Ets_Type)->
  {Command,Keys_Values} = lists:split(getindex(FirstLine,0),FirstLine),
  process(Command,split_by_space(Keys_Values),Ets_Type),
  process_Commands(Commands,Ets_Type).

process(Command,Keys_Values,Ets_Type)->
  case Command of
    "insert"-> insert(Keys_Values,Ets_Type);
    "update"-> update(Keys_Values,Ets_Type);
    "delete"-> delete(Keys_Values);
    "lookup"-> lookup(Keys_Values);
    _->input_error
  end.

lookup([])->ok;
lookup(Keys_Values) ->
  [Key|L]=Keys_Values,
  Print = ets:lookup(mytable, Key),
  print(Print),
  lookup(L).

delete([])->ok;
delete(Keys_Values) ->
[Key|L]=Keys_Values,
ets:delete(mytable, Key),
delete(L).

%inserts a key-value to the table if needed- meaning if this is a bag ets we will add
insert([],_)->ok;
insert(Keys_Values,Ets_Type) ->
  [Key|Temp]=Keys_Values,
  [Value|L] = Temp,
  Bool = ets:member(mytable,Key),
  case Ets_Type of
    "bag" -> ets:insert(mytable, {Key,Value});
    "set"  ->
      case Bool of
        false -> ets:insert(mytable, {Key,Value});
        true -> ok
      end;
    "ordered_set"  ->
      case Bool of
        false -> ets:insert(mytable, {Key,Value});
        true -> ok
      end

  end,
  insert(L,Ets_Type).

%updates a key-value in the table if needed- meaning no missing key will be added. only updates an existing key.
update([],_)->ok;
update(Keys_Values,Ets_Type) ->
  [Key|Temp]=Keys_Values,
  [Value|L] = Temp,
  Bool = ets:member(mytable,Key),
  case Bool of
    true ->  ets:insert(mytable, {Key,Value});
    false ->ok
  end,
  update(L,Ets_Type).

%write to file line by line
dump_to_file([])-> ok;
dump_to_file([{Key,Value}|L])->
  file:write_file("etsRes_204117089.ets",[Key," ",Value,"\n"], [append]),

%%  file:write_file("etsRes_204117089.ets", io_lib:format("~p ~p~n", [Key,Value]), [append]),
  dump_to_file(L).

%reading all lines from input file
readFile(Input) ->
  case file:read_line(Input) of
    % Data
    {ok, Lines} -> [Lines | readFile(Input)]; %appending another line from input file to list
    eof -> [] %finished reading
  end.


split_by_space([_|L])->Bin = list_to_binary(L),
  Tmp=binary:split(Bin, <<" ">>, [global]),
  [binary_to_list(X)||X<-Tmp].

print([])->ok;
print([{Key,Value}|L]) ->
  io:format("key: ~p val:~p~n",[Key,Value]),
  print(L).

getindex([],_)->-1;
getindex([32|_],K) ->K;
getindex([_|Tail],K) ->getindex(Tail,K+1).

%remove /n from the lists arguments
remove_enter(L)-> remove_enter(L,[]).
remove_enter([],L)->L;
remove_enter([Head|Tail],L)->remove_enter(Tail,L++[lists:sublist(Head,length(Head)-1)]).