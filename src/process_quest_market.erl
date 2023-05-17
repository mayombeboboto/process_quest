%%%-------------------------------------------------------------------
%% @doc process_quest_market API
%% @end
%%%-------------------------------------------------------------------
-module(process_quest_market).
%%%-------------------------------------------------------------------
-export([weapon/2]).
-export([helmet/2]).
-export([shield/2]).
-export([armor/2]).
%%%-------------------------------------------------------------------

weapon(CombinedLevel, Money) ->
    Materials = [
     {<<"plastic knife">>,  -1, 1, 2},
     {<<"plastic knife">>,   0, 1, 3},
     {<<"plastic knife">>,   1, 1, 5},
     {<<"metal spoon">>,    -1, 4, 3},
     {<<"metal spoon">>,     0, 4, 4},
     {<<"butter knife">>,   -1, 6, 5},
     {<<"butter knife">>,    0, 6, 7},
     {<<"butter knife">>,    1, 6, 9},
     {<<"machete">>,        -1, 9, 15},
     {<<"machete">>,         0, 9, 20},
     {<<"machete">>,         1, 9, 25},
     {<<"broad sword">>,    -1, 12, 23},
     {<<"broad sword">>,     0, 12, 30},
     {<<"broad sword">>,     1, 12, 38},
     {<<"lance">>,          -1, 15, 32},
     {<<"lance">>,           0, 15, 44},
     {<<"lance">>,           1, 15, 57},
     {<<"pistol">>,         -1, 25, 95},
     {<<"pistol">>,          0, 25, 105},
     {<<"pistol">>,          1, 25, 155},
     {<<"submachine gun">>, -1, 40, 200},
     {<<"submachine gun">>,  0, 40, 245},
     {<<"submachine gun">>,  1, 40, 365}],
    Fun = fun(Material) -> lambda(Material, CombinedLevel, Money) end,
    first_match(Fun, Materials).

helmet(CombinedLevel, Money) ->
    pick_material(CombinedLevel, Money).

shield(CombinedLevel, Money) ->
    pick_material(CombinedLevel, Money).

armor(CombinedLevel, Money)  ->
    pick_material(CombinedLevel, Money).

pick_material(CombinedLevel, Money) ->
    Materials = materials(),
    Fun = fun(Material) -> lambda(Material, CombinedLevel, Money) end,
    first_match(Fun, Materials).

first_match(_Fun, []) ->
    undefined;
first_match(Fun, [Head|Tail]) ->
    case Fun(Head) of
        continue -> first_match(Fun,Tail);
        Value -> Value
    end.

lambda(Material={_Tag, Modifier, Level, Price}, CombinedLevel, Money) ->
    if Modifier+Level > CombinedLevel, Price =< Money -> Material;
       true -> continue
    end.

materials() ->
    [{<<"wool">>,        0,  1,  25},
     {<<"pleather">>,    0,  2,  45},
     {<<"pleather">>,    1,  2,  50},
     {<<"pleather">>,    2,  2,  65},
     {<<"leather">>,    -2,  7,  30},
     {<<"leather">>,    -1,  7,  35},
     {<<"leather">>,     0,  7,  45},
     {<<"leather">>,     2,  7,  65},
     {<<"chain mail">>, -2, 12,  70},
     {<<"chain mail">>,  0, 12,  85},
     {<<"chain mail">>,  1, 12,  95},
     {<<"chain mail">>,  2, 12, 105},
     {<<"plate mail">>, -2, 17,  90},
     {<<"plate mail">>, -1, 17,  95},
     {<<"plate mail">>,  0, 17, 105},
     {<<"plate mail">>,  1, 17, 115},
     {<<"plate mail">>,  2, 17, 135}].