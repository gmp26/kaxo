# kaxo

FIXME: Write a one-line description of your library/project.

## Overview

FIXME: Write a paragraph about the library/project and highlight its goals.

## Setup

This is a clojurescript project. To create a development environment from scratch you will need to install:

* A java virtual machine. (e.g. A [recent JRE from Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
* Leiningen. Follow [these installation instructions](http://leiningen.org/), which will provide the `lein` command.
* A suitable text editor. I use emacs set up according to [this guide](http://www.braveclojure.com/basic-emacs/), but you may prefer Sublime or LightTable or IntelliJ with the Cursive plugin for clojure. 

To get an interactive development environment run:
```
lein figwheel
```
in a terminal or command line window. This will download other dependencies
and open your browser at [localhost:3449](http://localhost:3449/).
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

The project uses ReactJS to provide a fast virtual DOM, and the Reagent flavour
of [tonsky/rum](https://github.com/tonsky/rum) to drive React. To create an initial fresh, empty figwheel/Rum/ReactJS project I used [this Leiningen template](https://github.com/gmp26/fwrum).

## License

Copyright © 2014 Alex Voak, Mike Pearson

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
