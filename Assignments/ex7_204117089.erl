-module(ex7_204117089).
-export([steady/1,calc/3]).

%steady try the function received and logs into myLog_204117089.elog all actions, weather succeeded or failed by by any type of error.
steady(F) -> file:open("myLog_204117089.elog",[read, write, append]),
             try F() of
                Value-> file:write_file("myLog_204117089.elog", io_lib:format("{~p,success,~p}~n", [erlang:localtime(), Value]), [append]), Value
             catch
                %%catch the right type of error by pattern maching. append the new tuple to the existing file created.
                error:Error-> file:write_file("myLog_204117089.elog", io_lib:format("{~p,error,~p}~n", [erlang:localtime(), Error]), [append]), Error;
                throw:Throw-> file:write_file("myLog_204117089.elog", io_lib:format("{~p,throw,~p}~n", [erlang:localtime(), Throw]), [append]), Throw;
                exit:Exit-> file:write_file("myLog_204117089.elog", io_lib:format("{~p,exit,~p}~n", [erlang:localtime(), Exit]), [append]), Exit
                end.
%safe devision by using the function steady above.
%if the devider is zero, an error will be logged. otherwise, the value of the devision will be added to a seccess tuple.
calc(division,A,B) ->
    try A/B
        catch
        error:Error -> Error
        end.