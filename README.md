# Last Stick Loses
## Introduction
**Last Stick Loses** is a pen-and-paper game where a pyramid of sticks is drown as the board

2 players take turns to strike out adjacent sticks. Each player must strike out at least one stick in each turn and she can strike as many as she wants as long as they are on the same row. The player who strikes the last stick loses the game.

### Example
|   Board   |  P1 Turn  |  P2 Turn  |  P1 Turn  |  P2 Turn  |  P1 Turn  |
|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|
|`    I    `|`    I    `|`    I    `|`    I    `|`    I    `|`    +    `|
|`   I I   `|`   I I   `|`   I I   `|`   I +   `|`   +-+   `|`   +-+   `|
|`  I I I  `|`  I I I  `|`  +-+-+  `|`  +-+-+  `|`  +-+-+  `|`  +-+-+  `|
|` I I I I `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|` I +-+-+ `|
_P2 lost the game_

## This application
This project is an Erlang application that hosts a RESTful API through which you can play **Last Stick Loses** either against a machine (also coded in Erlang) or against another user.

## Additional Goals
This project will also be used as an excercise for those candidates who want to work at [Inaka](http://inaka.net)

## RESTful API
Check the documentation of the system's [RESTful API](RESTful-API.md) in its own file.

## Contact Us
For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/galgo/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

