%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2020 6:29 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("grzegorz").

%% API
-export([createMonitor/0, addStation/3, addValue/5, getStationByName/2, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getTypeMean/2, getMaxDifference/3]).

% informacje o stacjach pogodowych
% wspolrzedne geograficzne
% nazwy stacji pomiarowych
% zmierzone wartosci paraetrow (stezenia pylow, wartosci temperatury wraz z
% data i godzina pomiaru.

createMonitor() -> [[], []].

addStation(StationName, {X, Y}, [Stations, Values]) ->
  case lists:member(StationName, [Name || {Name, _} <- Stations]) of
    true -> error_nostation;
    false ->
      case lists:member({X, Y}, [A || {_, A} <-  Stations]) of
        true -> error_duplicate;
        false -> [[{StationName, {X, Y}} | Stations], Values]
      end
  end.


addValue({X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values]) ->
  Name = getStationByName({X, Y}, [Stations, Values]),
  case Name of
    error -> error_nostation;
    _ -> addValue(Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values])
  end;
addValue(Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values]) ->
  case lists:member({Name, {{Year, Month, Day}, Hour, Minute, Sec}, Type},
                    [{N, Tm, Tp} || {N, Tm, Tp, _} <- Values]) of
    true -> error_duplicate;
    false ->
      case lists:member(Name, [N || {N, _} <- Stations]) of
        true -> [Stations, [{Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value} | Values]];
        false -> error_nostation
      end
  end.



getStationByName({X, Y}, [Stations, _]) ->
  Res = lists:filter(fun({_, {Xx, Yy}}) -> (Xx == X) and (Yy == Y) end,  Stations),
  case Res of
    [] -> error;
    [{Name, _}] -> Name
  end.

removeValue({X, Y}, Data, Type, [Stations, Values]) ->
  Name = getStationByName({X, Y}, [Stations, Values]),
  case Name of
    error -> error_nostation;
    _ -> NewValues = lists:filter(fun({NName, DData, TType, _}) -> (NName /= Name) or (DData /= Data) or (TType /= Type) end, Values),
        [Stations, NewValues]
  end;
removeValue(Name, Data, Type, [Stations, Values]) ->
  case lists:member(Name, [N || {N, _} <- Stations]) of
    false -> error_nostation;
    true -> NewValues = lists:filter(fun({NName, DData, TType, _}) -> (NName /= Name) or (DData /= Data) or (TType /= Type) end, Values),
      [Stations, NewValues]
  end.

getOneValue({X, Y}, Data, Type, [Stations, Values]) ->
  Name = getStationByName({X, Y}, [Stations, Values]),
  case Name of
    error -> error_nostation;
    _ -> [NewValues | _ ] = lists:dropwhile(fun({NName, DData, TType, _}) -> (NName /= Name) or (DData /= Data) or (TType /= Type) end, Values),
      NewValues

  end;
getOneValue(Name, Data, Type, [Stations, Values]) ->
  case lists:member(Name, [N || {N, _} <- Stations]) of
    error -> error_nostation;
    _ -> [NewValues | _ ] = lists:dropwhile(fun({NName, DData, TType, _}) -> (NName /= Name) or (DData /= Data) or (TType /= Type) end, Values),
      NewValues

  end.

average([], Sum, Cnt) -> Sum / Cnt;
average([H|T], Sum, Cnt) -> average(T, Sum + H, Cnt + 1).



getStationMean({X, Y}, Type, [Stations, Values]) ->
  Name = getStationByName({X, Y}, [Stations, Values]),
  Vals = [V || {NName, _, TType, V} <- Values, Type == TType, NName == Name],
  case Vals of
    [] -> error_nodata;
    _ -> average(Vals, 0, 0)
  end;
getStationMean(Name, Type, [_, Values]) ->
  Vals = [V || {NName, _, TType, V} <- Values, Type == TType, NName == Name],
  case Vals of
    [] -> error_nodata;
    _ -> average(Vals, 0, 0)
  end.

getDailyMean(Type, {Y, M, D}, [_, Values]) ->
  Vals = [V || {_, {{Yy, Mm, Dd}, _}, TType, V} <- Values, Yy == Y, Mm == M, Dd == D, TType == Type],
 case Vals of
    [] -> error_nodata;
    _ -> average(Vals, 0, 0)
 end.

%%% dodatkowe

getTypeMean(Type, [_, Values]) ->
  Vals = [V || {_, _, T, V} <- Values, T == Type],
  case Vals of
    [] -> error_nodata;
    _ -> average(Vals, 0, 0)
  end.

getMaxDifference({X, Y}, Type, [Stations, Values]) ->
  Name = getStationByName({X, Y}, [Stations, Values]),
  case Name of
    error -> error_nostation;
    _ -> Vals = [V || {NName, _, TType, V} <- Values, NName == Name, TType == Type],
        lists:max(Vals) - lists:min(Vals)
  end;
getMaxDifference(Name, Type, [_, Values]) ->
  case Name of
    error -> error_nostation;
    _ -> Vals = [V || {NName, _, TType, V} <- Values, NName == Name, TType == Type],
      lists:max(Vals) - lists:min(Vals)
  end.


% removeValue(Name, )
% stations [{"name", {x, y}}, {}, {} ]
% Values [{Name, Time, Type, Value}]
