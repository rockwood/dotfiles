function fish_mode_prompt --description "Displays the current mode"
  if test "$fish_key_bindings" = "fish_vi_key_bindings"
    or test "$fish_key_bindings" = "fish_hybrid_key_bindings"
    switch $fish_bind_mode
      case default
        printf '[%s%s%s]' (set_color red) 'N' (set_color normal)
      case insert
        printf '[%s%s%s]' (set_color green) 'I' (set_color normal)
      case replace_one
        printf '[%s%s%s]' (set_color green) 'R' (set_color normal)
      case visual
        printf '[%s%s%s]' (set_color magenta) 'V' (set_color normal)
    end
  end
end
