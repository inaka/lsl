# Last Stick Loses
## Introduction
**Last Stick Loses** is a pen-and-paper game where a pyramid of sticks is drown as the board

2 players take turns to strike out adjacent sticks. Each player must strike out at least one stick in each turn and she can strike as many as she wants as long as they are on the same row. The player who strikes the last stick loses the game.

### Example
|   Board   |  Turn 1   |  Turn 2   |  Turn 3   |  Turn 4   |  Turn 5   |
|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|
|`    I    `|`    I    `|`    I    `|`    I    `|`    I    `|`    +    `|
|`   I I   `|`   I I   `|`   I I   `|`   I +   `|`   +-+   `|`   +-+   `|
|`  I I I  `|`  I I I  `|`  +-+-+  `|`  +-+-+  `|`  +-+-+  `|`  +-+-+  `|
|` I I I I `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|
_Player 2 lost the game_

## This application
This project is an Erlang application that hosts a RESTful API through which you can play **Last Stick Loses** either against a machine (also coded in Erlang) or against another user.

## Additional Goals
This project will also be used as an excercise for those candidates who want to work at [Inaka](inaka.net)
