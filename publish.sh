#!/bin/bash

cd ~/clojure/kaxo
lein clean
lein cljsbuild once min
rsync -av resources/public/ gmp26@maths.org:/www/nrich/html/kaxo
