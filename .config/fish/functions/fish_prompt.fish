# fish git prompt
set __fish_git_prompt_showcolorhints 1
set __fish_git_prompt_show_informative_status 1

function fish_prompt
  set -l current_dir (printf '[%s%s%s]' (set_color blue) (prompt_pwd) (set_color normal))
  set -l current_git (printf '%s' (__fish_git_prompt '[%s]'))

  printf '%s%s » %s' $current_dir $current_git (set_color normal)
end
