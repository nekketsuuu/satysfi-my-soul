#!/bin/bash

set -eu

build () {
    set -x ;
    ./build.hs
    git add docs ;
    git status ;
    set +x ;
}

push () {
    set -x ;
    git commit -m "publish" ;
    git push ;
    set +x ;
}

build
read -p 'Is it OK to commit & push? [y/N]: ' ans
case $ans in
    [Yy]* )
	;;
    '' | [Nn]* )
	exit 0 ;
	;;
    * )
	echo 'Please answer yes or no. Aborted.' ;
	exit 1 ;
esac
push
