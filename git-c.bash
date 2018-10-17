#!/bin/bash

set -eu

while true ; do
  read -p 'Run ./build.sh and add ./docs before commit? [y/N]: ' ans
  case $ans in
    [Yy]* )
      set -x ;
      ./build.hs ;
      git add docs ;
      git status ;
      set +x ;
      echo 'Build finished. Please run this command again.'
      break ;
      ;;
    '' | [Nn]* )
      set -x ;
      git commit "$@"
      set +x ;
      break ;
      ;;
    * )
      echo 'Please answer yes or no'
  esac
done
