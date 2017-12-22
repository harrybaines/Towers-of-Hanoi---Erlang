Towers of Hanoi - Erlang:


This implementation solves the game from any valid game state (i.e. not just when all
disks are stacked on Tower 1).

When creating the list of 3 towers, the towers need not be in any particular order, however
all the disks on each tower MUST be stacked in size order (i.e. 1,4,7 and not 6,4,2).

The game also assumes you include all disks numbered 1..<=N (i.e. using 5 disks means
disks 1,2,3,4,5 must be included somewhere in the tower list).


Example in the Erlang shell:

> c(hanoi).

> T = [{tower1, [4,5,6,7,8]}, {tower2, [2,3]}, {tower3, [1]}].

> hanoi:solve(T).


Further tower list examples:

- T1 = [{tower1, [3,4]}, {tower2, [2]}, {tower3, [1]}].

- T2 = [{tower1, [1,5,9]}, {tower2, [2,3,4]}, {tower3, [6,7,8,10]}].

- T3 = [{tower1, [6,9]}, {tower2, [1,8]}, {tower3, [2,3,4,5,7,10]}].

- T4 = [{tower2, [1,7]}, {tower3, [2,4,6]}, {tower1, [3,5]}].

- T5 = [{tower1, []}, {tower2, [1,2]}, {tower3,[]}]

- T6 = [{tower1, []}, {tower2, [1]}, {tower3, []}].

- T7 = [{tower3, [3]}, {tower2, [1,5]}, {tower1, [2,4]}].