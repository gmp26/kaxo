# Kaxo

Kaxo is a nim-like game invented by Alex Voak. 

## Overview

Kaxo is an impartial game best played with a misere winning rule - if you play last, you lose. In the normal play version, there are some simple symmetry strategies open to the players which will make the game less interesting once they have been discovered. Pursuing symmetry strategies to the bitter end in a misere game is doomed to failure.

Another nim-like game with a normal play symmetry strategy is Kayles. A winning strategy for the misere version of Kayles has recently been published. A general solution for asymmetric positions in Kayles has recently been found. It should be possible to adapt Sprague-Grundy theory to misere Kaxo too. 

This code was developed for https://wild.maths.org in order to provoke work on finding a winning strategy for Kaxo - which is currently an open problem.

## Rules

Play on a square grid of dots - a 4x4 grid is probably optimal. Players take turns to cross out dots singly, or by drawing line segments through them. Lines must start and end on a dot. Lines must be parallel to the grid axes or at 45 degrees. Lines may not intersect, and may not intersect already crossed out dots. The loser is the player forced to play last.

## Demo
See https://nrich.maths.org/kaxo. This version has an incredibly dumb AI which simply chooses a free dot at random. We're hoping to open the game up to school students through https://wild.maths.org for collaborative improvement of the AI. There are already some functions in the source code that should help this process along.

## Development setup

This is a ClojureScript project. To create a development environment from scratch you will need to install:

* A Java virtual machine. (e.g. A [recent JRE from Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
* Leiningen. Follow [these installation instructions](http://leiningen.org/), which will provide the `lein` command.
* A suitable text editor. I use emacs with an [emacs-live set up](http://overtone.github.io/emacs-live/) for clojurescript development. You may prefer Sublime or LightTable or IntelliJ with the Cursive plugin for Clojure. 

Once you have leiningen installed, get an interactive development environment for kaxo running by typing:
```
git clone https://github.com/gmp26/kaxo.git
cd kaxo
lein figwheel
```
in a terminal or command line window. This will download other dependencies
and open your browser at http://localhost:3449. Once a browser connects you will have a REPL executing there allowing you to test cljs functions in situ. Use clojure's `in-ns` function to switch namespace to the source file you are working on. Source maps are provided so you are able to step through clojurescript code in Chrome Developer Tools.

[Figwheel](https://github.com/bhauman/lein-figwheel) gives you live reloading into the browser. It's brilliant.

Production build
----------------

To clean all compiled files:

    lein clean

To create a production build run:

    lein cljsbuild once min

Test by opening your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## Other dependencies

The project uses ReactJS to provide a fast virtual DOM, together with the Reagent flavour
of [tonsky/rum](https://github.com/tonsky/rum) to drive React. To create an initial fresh, empty figwheel/Rum/ReactJS project I used [this Leiningen template](https://github.com/gmp26/fwrum).

## Copyright and License

Game ©2015 Alex Voak all rights reserved.  
Source code ©2015 University of Cambridge (NRICH project) 2015.

The source code is distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
