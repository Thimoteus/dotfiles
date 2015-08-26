# performs ls after cd
function cd
  if test (count $argv) = 0
    set new_directory $HOME
  else
    set new_directory $argv[1]
  end
  builtin cd $new_directory
  ls
end

# enable vi mode
fish_vi_mode

# set theme
set BASE16_THEME "chalk"
set BASE16_SHELL "$HOME/.config/base16-shell/base16-$BASE16_THEME.dark.sh"
eval sh $BASE16_SHELL

# aliases
alias edit="nvim"

function vim
  nvim
end

# worthless
source ~/.config/fish/nvm-wrapper/nvm.fish

# start X at login
if status --is-login
    if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
        exec startx -- -keeptty
    end
end
