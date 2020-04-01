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
-export([createMonitor/0, addStation/3, addValue/5, getStationByName/2]).

% informacje o stacjach pogodowych
% wspolrzedne geograficzne
% nazwy stacji pomiarowych
% zmierzone wartosci paraetrow (stezenia pylow, wartosci temperatury wraz z
% data i godzina pomiaru.

createMonitor() -> [[], []].

addStation(StationName, {X, Y}, [Stations, Values]) ->
  case lists:member(StationName, [Name || {Name, _} <- Stations]) of
    true -> error;
    false ->
      case lists:member({X, Y}, [A || {_, A} <-  Stations]) of
        true -> error;
        false -> [[{StationName, {X, Y}} | Stations], Values]
      end
  end.

addValue(Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values]) ->
  case lists:member({Name, {{Year, Month, Day}, Hour, Minute, Sec}, Type},
                    [{N, Tm, Tp} || {N, Tm, Tp, _} <- Values]) of
    true -> error_repeat;
    false ->
      case lists:member(Name, [N || {N, _} <- Stations]) of
        true -> [Stations, [{Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value} | Values]];
        false -> error_nostation
      end
  end;
addValue({X, Y}, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values]) ->
  Name = getStationByName(X, Y),
  case Name of
    [] -> error_noname;
    _ -> addValue(Name, {{Year, Month, Day}, {Hour, Minute, Sec}}, Type, Value, [Stations, Values])
  end.

getStationByName({X, Y}, [Stations, _]) ->
  Res = lists:filter(fun({_, {Xx, Yy}}) -> (Xx == X) and (Yy == Y) end,  Stations),
  case Res of
    [] -> error;
    _ -> Res
  end.





% removeValue(Name, )
% stations [{"name", {x, y}}, {}, {} ]
% Values [{Name, Time, Type, Value}]
