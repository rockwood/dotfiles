set fish_greeting
set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x HOMEBREW_EDITOR "vim"
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
source /opt/homebrew/opt/asdf/libexec/asdf.fish
