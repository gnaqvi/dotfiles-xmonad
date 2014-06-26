#!/bin/bash
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles

set -e
set -u

dots=$(ls -da .* | grep -Pxv '\.git|\.+')

for dot in $dots
do
  if [[ -f $dot && ! -f $HOME/$dot ]]; then
    ln -s "$(realpath "$dot")" $HOME
  elif [[ -d $dot && ! -d $HOME/$dot ]]; then
    ln -s "$(realpath "$dot")" $HOME
  elif [[ $dot == ".config" ]]; then
    for sub_entry in .config/*
    do
      ln -s $(realpath "$sub_entry") $HOME/.config
    done
  fi
done

