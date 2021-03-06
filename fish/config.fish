# set theme
set BASE16_THEME "embers"
set BASE16_INTENSITY "dark"
set BASE16_SHELL "$HOME/.config/base16-shell/base16-$BASE16_THEME.$BASE16_INTENSITY.sh"
#eval sh $BASE16_SHELL
eval sh "/home/thimoteus/.nvim/plugged/gruvbox/gruvbox_256palette.sh"

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

# check the blacklist
function blacklist
  cat /usr/share/doc/your-freedom/blacklist.txt | grep $argv
end

# enable vi mode
fish_vi_key_bindings

# worthless
#bass source ~/.nvm/nvm.sh ';' nvm use stable

# promptline.vim
function fish_prompt
  env FISH_VERSION=$FISH_VERSION PROMPTLINE_LAST_EXIT_CODE=$status bash ~/.promptline.sh left
end

function fish_right_prompt
  env FISH_VERSION=$FISH_VERSION PROMPTLINE_LAST_EXIT_CODE=$status bash ~/.promptline.sh right
end

# source pass stuff
source ~/dotfiles/fish/pass.fish-completion

# add bin path
set PATH ~/bin/ $PATH
set PATH /usr/bin/core_perl $PATH
#set PATH ~/.local/bin/ $PATH

# smlnj stuff
set PATH /usr/local/sml/bin/ $PATH

#bass source ~/dotfiles/base16-chalk.dark.sh

set EDITOR nvim
set RANGER_LOAD_DEFAULT_RC FALSE

#aliases
alias pastebin "curl -F 'sprunge=<-' http://sprunge.us"
alias irc "tmux new-session -A -s irc"
alias zirc "ssh -t thimotron@192.168.1.5 tmux new-session -A -s irc"
alias vps "ssh -t thimoteus@144.217.6.205 -p 51413 tmux new-session -A -s evante"
alias gotosleep "xautolock -time 60 -locker 'systemctl poweroff'"
