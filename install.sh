#!/bin/bash

# prepare kde for xmonad
mkdir -p ~/.config/plasma-workspace/env
echo "export KDEWM=xmonad" > ~/.config/plasma-workspace/env/wm.sh

# install xmonad
sudo apt install xmonad
mkdir -p ~/.xmonad
cp xmonad.hs ~/.xmonad/
xmonad --recompile

# install stack
#curl -sSL https://get.haskellstack.org/ | sh
