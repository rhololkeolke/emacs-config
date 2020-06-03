#!/bin/bash

git clone -b develop https://github.com/syl20bnr/spacemacs.git ~/.emacs.d

stow -t $HOME spacemacs
