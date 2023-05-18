%%%-------------------------------------------------------------------
%% @doc process_quest API
%% @end
%%%-------------------------------------------------------------------
-module(process_quest).
%%%-------------------------------------------------------------------
-export([fetch/0]).
%%%-------------------------------------------------------------------
fetch() ->
    Quests = quests(),
    lists:nth(rand:uniform(length(Quests)), Quests).

quests() ->
    [{<<"Cancel the festival">>,    [{experience, 65},   {kills, 8}]},
     {<<"Fetch me a nut">>,         [{experience, 150},  {kills, 20}]},
     {<<"Meet the invisible man">>, [{experience, 200},  {kills, 25}]},
     {<<"Find quest ideas">>,       [{experience, 340},  {kills, 32}]},
     {<<"Slay the Bieber">>,        [{experience, 500},  {kills, 45}]},
     {<<"Summon the dragon">>,      [{experience, 1000}, {kills, 100}]},
     {<<"Invent maple syrup">>,     [{experience, 1500}, {kills, 175}]}].
