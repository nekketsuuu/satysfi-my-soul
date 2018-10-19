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
    git commit ;
    git push ;
    set +x ;
}

build
read -p 'Is it OK to commit & push? [y/N]: ' ans
case $ans in
    [Yy]* )
	break ;
	;;
    '' | [Nn]* )
	exit 0 ;
	break ;
	;;
    * )
	echo 'Please answer yes or no. Aborted.' ;
	exit 1 ;
esac
push
