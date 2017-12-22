%% Title: Revenge of the Towers of Hanoi
%% Author: Harry Baines
-module(hanoi).
-export([create_towers/1, display_towers/1, move/5, solve/1, get_stack_containing/2, 
         get_contig_stack/2, get_via/3, get_top/1, is_empty/1]).

%% Creates the towers with number of disks
create_towers(Disks) -> 
	[{tower1, lists:seq(1, Disks)}, {tower2, []}, {tower3, []}].

%% Displays the tower names alongside disks
display_towers([Tower|Rest]) ->
    io:format("~n~p: ~p~n", [element(1, Tower), lists:reverse(element(2, Tower))]),
    display_towers(Rest);

%% Terminating case for displaying towers
display_towers([]) ->
    io:format("~n").

%% Terminating condition for move() = allows a disk to be moved from one tower to another
move(Source, Destination, _, 1, Towers) ->
    
    %% Obtains top disk from the source tower disk list (separates top from rest)
    [TopDisk|NewSource] = proplists:get_value(Source, Towers),

    %% Adds top disk from source to destination (adds top to existing destination disk list)
    NewDestination = [TopDisk|proplists:get_value(Destination, Towers)],

    io:format("--- Moved disk ~p from ~p to ~p ---~n", [TopDisk, Source, Destination]),

    %% Updates the list of towers containing new source tower disks
    UpdatedSource = lists:keyreplace(Source, 1, Towers, {Source, NewSource}),

    %% Updates the list of towers containing new source tower disks AND new destination tower disks
    UpdatedTowers = lists:keyreplace(Destination, 1, UpdatedSource, {Destination, NewDestination}),

    %% Display to user
    display_towers(UpdatedTowers),
    UpdatedTowers;

%% Move function - general case for moving all disks from source to destination
move(Source, Destination, Via, NumDisks, Towers) ->

    %% Step 1: move NumDisks-1 disks from source to via
    TowersAfterMoveToVia = move(Source, Via, Destination, NumDisks - 1, Towers),

    %% Step 2: move 1 Disk from source to destination
    TowersAfterMoveToDest = move(Source, Destination, Via, 1, TowersAfterMoveToVia),

    %% Step 3: move NumDisks-1 disks from via to destination
    move(Via, Destination, Source, NumDisks - 1, TowersAfterMoveToDest).

%% Obtains the largest contiguous stack containing 1
get_contig_stack(Disks, Count) ->

    %% Get top disk (smallest)
    Top = get_top(Disks),

    io:format("Current tower state: ~p~n", [Disks]),
    io:format("Next disk: ~p~n", [Top]),

    %% Recurse for next disk in list, otherwise return contiguous stack
    if
        Count == Top ->
            TowerDisks = lists:droplast(lists:reverse(Disks)),
            get_contig_stack(lists:reverse(TowerDisks), Count + 1);
        true ->
            io:format("Nope. Not contiguous.~n"),
            ContigStack = lists:seq(1, Count-1),
            ContigStack
    end.

get_top([Head|Tail]) ->
    case Tail of
      [] ->
        Head;
      _ -> 
        Head
    end;

get_top([]) ->
    ok.

%% Obtains the largest contiguous stack using previous function
get_stack_containing(ToFind, [Tower|Rest]) ->
    
    %% Initialise count - used to find contiguous stack containing 1
    Count = 1,

    %% Get tower disks
    TowerDisks = element(2, Tower),

    %% Reverse disks
    TopDisk = get_top(TowerDisks),
    % TopDisk = lists:last(lists:reverse(TowerDisks)),

    %% Check if disk is 1
    if
        TopDisk == ToFind ->
           %% Found a 1, so find largest contiguous stack
           io:format("Found!~n"),
           ContigStack = {element(1, Tower), get_contig_stack(TowerDisks, Count)},
           ContigStack;
        true ->
           %% Look at next tower to find a 1 
           io:format("Not found yet!~n"),
           get_stack_containing(ToFind, Rest)
    end;

%% Terminating case for obtaining 1 from tower list
get_stack_containing(_, []) ->
    ok.

get_via(SourceName, DestName, Towers) ->

    [CurTower|Rest] = Towers,

    CurTowerStr = atom_to_list(element(1, CurTower)),
    SourceStr = atom_to_list(SourceName),
    DestStr = atom_to_list(DestName),
    
    if
        SourceStr /= CurTowerStr ->

            if
                DestStr /= CurTowerStr ->
                    ViaName = CurTowerStr,
                    ViaName;

                true ->
                    get_via(SourceName, DestName, Rest)
            end;
            
        true ->
            get_via(SourceName, DestName, Rest)
    end.


is_empty(Disks) ->
    if
        length(Disks) == 0 ->
            1;
        true ->
            0
    end.

get_num_empty_towers(Towers) ->
    [Head1|Tail1] = Towers,
    Tower1C = 0 + is_empty(element(2, Head1)),
    [Head2|Tail2] = Tail1,
    Tower2C = 0 + is_empty(element(2, Head2)),
    [Head3|Tail3] = Tail2,
    Tower3C = 0 + is_empty(element(2, Head3)),
    Count = Tower1C + Tower2C + Tower3C,
    io:format("Count = ~p~n", [Count]),
    Count.
    

get_final_source(Towers) ->
    [Head|Tail] = Towers,
    DisksLen = length(element(2, Head)),
    if
        DisksLen == 0 ->
            get_final_source(Tail);
        true ->
            Head
    end.

%% Solve function - solves the towers from start state
%% Begins recursive calls to move all disks from tower1 to tower3
solve(Towers) ->

    display_towers(Towers),

    %% Find largest contiguous stack containing 1
    StackContainingOne = get_stack_containing(1, Towers),
    io:format("~nLargest stack containing one: ~p~n", [element(2, StackContainingOne)]),

    %% Move stack onto next largest disk - standard algorithm
    LargestInContig = lists:max(element(2, StackContainingOne)),
    io:format("~nLargest disk in contig: ~p~n", [LargestInContig]),
    DiskToFind = LargestInContig + 1,
    io:format("Disk to move to: ~p~n", [DiskToFind]),
    
    %% Get tower names of source (contig containing 1) and dest (4)
    StackContainingDest = get_stack_containing(DiskToFind, Towers),
    SourceName = element(1, StackContainingOne),
    DestName = element(1, StackContainingDest),
    ViaName = list_to_atom(get_via(SourceName, DestName, Towers)),

    io:format("~nSource for contig stack: ~p~n", [SourceName]),
    io:format("Destination for contig stack: ~p~n", [DestName]),
    io:format("Via for contig stack: ~p~n", [ViaName]),

    NumDisks = length(element(2, StackContainingOne)),
    OneMove = move(SourceName, DestName, ViaName, NumDisks, Towers),


    NumTowers = get_num_empty_towers(OneMove),
    if
        %% Partially finished (2 towers empty) - find tower, move all to tower3
        NumTowers == 2 ->

            io:format("Partially finished!!~n"),
            FinalSource = get_final_source(OneMove),
            FinalDisks = element(2, FinalSource),
            NumDisks2 = length(FinalDisks),
            FinalVia = list_to_atom(get_via(element(1, FinalSource), tower3, OneMove)),

            io:format("Num Disks: ~p~n", [NumDisks2]),

            FinalSourceName = atom_to_list(element(1, FinalSource)),

            if
                FinalSourceName == "tower3" ->
                    display_towers(OneMove),
                    OneMove;
                true ->
                    move(element(1, FinalSource), tower3, FinalVia, NumDisks2, OneMove)
            end;

        true ->
            io:format("Not partially finished!~n"),
            solve(OneMove)
    end.
