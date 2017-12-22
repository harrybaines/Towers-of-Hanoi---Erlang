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

    %% Recurse for next disk in list if contiguous, otherwise return contiguous stack
    if
        Count == Top ->
            TowerDisks = lists:droplast(lists:reverse(Disks)),
            get_contig_stack(lists:reverse(TowerDisks), Count + 1);
        true ->
            ContigStack = lists:seq(1, Count-1),
            ContigStack
    end.

%% Returns the head of the disk list (top disk)
get_top([Head|_]) ->
    Head;

%% Case for attempting to get top disk on empty disk list
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

    %% Check if disk is 1
    if
        %% Find largest contiguous stack
        TopDisk == ToFind ->
           ContigStack = {element(1, Tower), get_contig_stack(TowerDisks, Count)},
           ContigStack;

        %% 1 not found yet - look at other towers to find it
        true ->
           get_stack_containing(ToFind, Rest)
    end;

%% Terminating case for obtaining 1 from tower list
get_stack_containing(_, []) ->
    ok.

%% Obtains the atom for the via tower name
get_via(SourceName, DestName, Towers) ->

    %% Obtain current tower atom
    [CurTower|Rest] = Towers,
    CurTowerName = element(1, CurTower),

    %% Check if tower is not source and not destination
    if
        SourceName /= CurTowerName ->
            if
                %% Found via name
                DestName /= CurTowerName ->
                    ViaName = CurTowerName,
                    ViaName;

                %% Haven't found yet, recursively search
                true ->
                    get_via(SourceName, DestName, Rest)
            end;

        %% Haven't found yet, recursively search    
        true ->
            get_via(SourceName, DestName, Rest)
    end.

%% Returns 1 if the disk list is empty, 0 otherwise
is_empty(Disks) ->
    if
        length(Disks) == 0 ->
            1;
        true ->
            0
    end.

%% Returns the total number of empty towers
get_num_empty_towers(Towers) ->
    [T1H|T1T] = Towers,
    Tower1C = is_empty(element(2, T1H)),
    [T2H|T2T] = T1T,
    Tower2C = is_empty(element(2, T2H)),
    [T3H|_] = T2T,
    Tower3C = is_empty(element(2, T3H)),
    Count = Tower1C + Tower2C + Tower3C,
    Count.
    
%% Returns the atom name of the final source tower (then moved to tower3)
get_final_source(Towers) ->
    [Head|Tail] = Towers,
    DisksLen = length(element(2, Head)),
    if
        DisksLen == 0 ->
            get_final_source(Tail);
        true ->
            Head
    end.

%% Solve function: solves the towers from start state and begins recursive calls to move all disks
solve(Towers) ->

    display_towers(Towers),

    %% Find largest contiguous stack containing 1
    StackContainingOne = get_stack_containing(1, Towers),

    %% Check if 2 towers are empty in initial game state
    EmptyTowers = get_num_empty_towers(Towers),
    if
        %% Can move disks to final destination here
        EmptyTowers == 2 ->

            %% Obtain details to call move function or simply display finished game
            FinalSource = get_final_source(Towers),
            FinalDisks = element(2, FinalSource),
            NumDisks2 = length(FinalDisks),
            FinalVia = get_via(element(1, FinalSource), tower3, Towers),

            %% Check if game is finished - just display
            FinalSourceName = atom_to_list(element(1, FinalSource)),
            if
                FinalSourceName == "tower3" ->
                    display_towers(Towers),
                    Towers;

                %% Otherwise, recursively move disks to tower3
                true ->
                    move(element(1, FinalSource), tower3, FinalVia, NumDisks2, Towers)
            end;

        %% Recursively solve the game until 2 towers are empty
        true ->

            %% Move stack onto next largest disk - standard algorithm
            LargestInContig = lists:max(element(2, StackContainingOne)),
            
            %% Find next sequential disk number to move contig stack to
            DiskToFind = LargestInContig + 1,
            
            %% Get tower names of source (contig containing 1), dest, via and NumDisks
            StackContainingDest = get_stack_containing(DiskToFind, Towers),
            SourceName = element(1, StackContainingOne),
            DestName = element(1, StackContainingDest),
            ViaName = get_via(SourceName, DestName, Towers),
            NumDisks = length(element(2, StackContainingOne)),

            %% Make a move and recursively solve again
            Moved = move(SourceName, DestName, ViaName, NumDisks, Towers),
            solve(Moved)
    end.