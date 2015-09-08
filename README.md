# Kaxo

Kaxo is a nim-like game invented by Alex Voak. 

## Overview

Kaxo is an impartial game best played with a misere winning rule - if you play last, you lose. In the normal play version, there are some simple symmetry strategies open to the players which will make the game less interesting once they have been discovered. Pursuing symmetry strategies to the bitter end in a misere game is doomed to failure.

Another nim-like game with a normal play symmetry strategy is Kayles. A winning strategy for the misere version of Kayles has recently been published. A general solution for asymmetric positions in Kayles has been found using Sprague-Grundy theory. It should be possible to apply Sprague-Grundy theory to Kaxo too. 

This code was developed for https://wild.maths.org in order to provoke work on finding a winning strategy for Kaxo - which is currently an open problem.

## Rules

Play on a square grid of dots - a 4x4 grid is probably optimal. Players take turns to cross out dots singly, or by drawing line segments through them. Lines must start and end on a dot. Lines must be parallel to the grid axes or at 45 degrees. Lines may not intersect, and may not intersect already crossed out dots. The loser is the player forced to play last.

## Demo
Under development

## Development setup

This is a ClojureScript project. To create a development environment from scratch you will need to install:

* A Java virtual machine. (e.g. A [recent JRE from Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
* Leiningen. Follow [these installation instructions](http://leiningen.org/), which will provide the `lein` command.
* A suitable text editor. I use emacs set up according to [this guide](http://www.braveclojure.com/basic-emacs/), but you may prefer Sublime or LightTable or IntelliJ with the Cursive plugin for Clojure. 

To get an interactive development environment run:
```
lein figwheel
```
in a terminal or command line window. This will download other dependencies
and open your browser at <http://localhost:3449/>.
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## Other dependencies

The project uses ReactJS to provide a fast virtual DOM, together with the Reagent flavour
of [tonsky/rum](https://github.com/tonsky/rum) to drive React. To create an initial fresh, empty figwheel/Rum/ReactJS project I used [this Leiningen template](https://github.com/gmp26/fwrum).

## Copyright and License

Game ©2015 Alex Voak all rights reserved.  
Source code ©2015 University of Cambridge (NRICH project) 2015.

The source code is distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
