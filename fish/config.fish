# set theme
set BASE16_THEME "grayscale"
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
fish_vi_mode

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
set PATH ~/.local/bin/ $PATH
set PATH ~/.psvm/current/bin/ $PATH

#bass source ~/dotfiles/base16-chalk.dark.sh

set EDITOR nvim
set RANGER_LOAD_DEFAULT_RC FALSE

#aliases
alias spacemacs "emacs -nw"
#72.202.184.126
alias vps "ssh -t -p 1005 thimoteus@unixbird.windowskrill.me tmux new-session -A -s thimotron"
alias vps-command "ssh -p 1005 thimoteus@unixbird.windowskrill.me "
alias pastebin "curl -F 'sprunge=<-' http://sprunge.us"
alias irc "tmux new-session -A -s irc"
alias zirc "ssh -t thimotron@192.168.1.5 tmux new-session -A -s irc"
alias gotosleep "xautolock -time 60 -locker 'systemctl poweroff'"
