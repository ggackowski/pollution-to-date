%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 12:45 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("grzegorz").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, startServer/0,
         getMaxDifference/2, getDailyMean/2, getStationByName/1, getTypeMean/1,
          removeValue/3, getStationMean/2]).

start() ->
  register(pollutionServer, spawn(fun() -> startServer() end)).

stop() -> pollutionServer ! kill.

startServer() ->
  Monitor = init(),
  loop(Monitor).

init() ->
  pollution:createMonitor().

loop(Monitor) ->
  io:format("loop again~n"),
  receive

    {add_station, StationName, {X, Y}, Pid} ->

      MonitorNew = pollution:addStation(StationName, {X, Y}, Monitor),
      Pid ! {ok, self()},
      loop(MonitorNew);


    {add_value, {X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, Pid} ->

      MonitorNew = pollution:addValue({X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, Monitor),
      Pid ! {ok, self()},
      loop(MonitorNew);


    {add_value, Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, Pid} ->

      MonitorNew = pollution:addValue(Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, Monitor),
      Pid ! ok,
      loop(MonitorNew);


    {get_one_value, {X, Y}, Data, Type, Pid} ->

      Value = pollution:getOneValue({X, Y}, Data, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_one_value, Name, Data, Type, Pid} ->

      Value = pollution:getOneValue(Name, Data, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_station_by_name, {X, Y}, Pid} ->

      Value = pollution:getStationByName({X, Y}, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);

    {remove_value, {X, Y}, Data, Type, Pid} ->

      pollution:removeValue({X, Y}, Data, Type, Monitor),
      Pid ! {ok, self()},
      loop(Monitor);


    {remove_value, Name, Data, Type, Pid} ->

      pollution:removeValue(Name, Data, Type, Monitor),
      Pid ! {ok, self()},
      loop(Monitor);


    {get_station_mean, {X, Y}, Type, Pid} ->

      Value = pollution:getStationMean({X, Y}, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_station_mean, Name, Type, Pid} ->

      Value = pollution:getStationMean(Name, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_daily_mean, Type, {Y, M, D}, Pid} ->

      Value = pollution:getDailyMean(Type, {Y, M, D}, Pid),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_type_mean, Type, Pid} ->

      Value = pollution:getTypeMean(Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_max_difference, {X, Y}, Type, Pid} ->

      Value = pollution:getMaxDifference({X, Y}, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);


    {get_max_difference, Name, Type, Pid} ->

      Value = pollution:getMaxDifference(Name, Type, Monitor),
      Pid ! {Value, self()},
      loop(Monitor);



    kill -> ok



  end.

addStation(StationName, {X, Y}) ->
  pollutionServer ! {add_station, StationName, {X, Y}, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
    after 1000 -> err
  end.

addValue({X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value) ->
  pollutionServer ! {add_value, {X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
    after 1000 -> err
  end.

getOneValue({X, Y}, Data, Type) ->
  pollutionServer ! {get_one_value, {X, Y}, Data, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

getStationByName({X, Y}) ->
  pollutionServer ! {get_station_by_name, {X, Y}, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

removeValue({X, Y}, Data, Type) ->
  pollutionServer ! {remove_value, {X, Y}, Data, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end;
removeValue(Name, Data, Type) ->
  pollutionServer ! {remove_value, Name, Data, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

getStationMean({X, Y}, Type) ->
  pollutionServer ! {get_station_mean, {X, Y}, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end;
getStationMean(Name, Type) ->
  pollutionServer ! {get_station_mean, Name, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

getDailyMean(Type, {Y, M, D}) ->
  pollutionServer ! {get_daily_mean, Type, {Y, M, D}, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

getTypeMean(Type) ->
  pollutionServer ! {get_type_mean, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.

getMaxDifference({X, Y}, Type) ->
  pollutionServer ! {get_max_difference, {X, Y}, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end;
getMaxDifference(Name, Type) ->
  pollutionServer ! {get_max_difference, Name, Type, self()},
  receive
    {Value, _} -> io:format("~w~n", [Value])
  after 1000 -> err
  end.
