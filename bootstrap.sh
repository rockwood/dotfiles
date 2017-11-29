#!/bin/bash

# This symlinks all dotfiles to ~/. It's safe to run multiple times and will prompt you about
# anything unclear.

answer_is_yes() {
    [[ "$REPLY" =~ ^[Yy]$ ]] \
        && return 0 \
        || return 1
}

ask_for_confirmation() {
    print_question "$1 (y/n) "
    read -n 1
    printf "\n"
}

execute() {
    $1 &> /dev/null
    print_result $? "${2:-$1}"
}

print_error() {
    # Print output in red
    printf "\e[0;31m  [✖] $1 $2\e[0m\n"
}

print_info() {
    # Print output in purple
    printf "\n\e[0;35m $1\e[0m\n\n"
}

print_question() {
    # Print output in yellow
    printf "\e[0;33m  [?] $1\e[0m"
}

print_result() {
    [ $1 -eq 0 ] \
        && print_success "$2" \
        || print_error "$2"

    [ "$3" == "true" ] && [ $1 -ne 0 ] \
        && exit
}

print_success() {
    # Print output in green
    printf "\e[0;32m  [✔] $1\e[0m\n"
}

#
# Actual symlink stuff
#

# Finds all .dotfiles in this folder
declare -a FILES_TO_SYMLINK=$(find . -type f -maxdepth 1 -name ".*" -not -name .DS_Store | sed -e 's|//|/|' | sed -e 's|./.|.|')

# Finds all files in these extra config dirs
for dir in $(echo .config .emacs.d .vim bin); do
    FILES_TO_SYMLINK="$FILES_TO_SYMLINK $(find $dir -type f -not -name .DS_Store)"
done

main() {
    local file=""
    local sourcePath=""
    local targetPath=""

    for file in ${FILES_TO_SYMLINK[@]}; do
        sourcePath="$(pwd)/$file"
        targetPath="$HOME/$file"

        mkdir -p "$(dirname $targetPath)"

        if [ -e "$targetPath" ]; then
            if [ "$(readlink "$targetPath")" != "$sourcePath" ]; then
                ask_for_confirmation "'$targetPath' already exists, do you want to overwrite it?"
                if answer_is_yes; then
                    rm "$targetPath"
                    execute "ln -fs $sourcePath $targetPath" "$targetPath → $sourcePath"
                else
                    print_error "$targetPath → $sourcePath"
                fi
            else
                print_success "$targetPath → $sourcePath"
            fi
        else
            execute "ln -fs $sourcePath $targetPath" "$targetPath → $sourcePath"
        fi
    done
}

main
