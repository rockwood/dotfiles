# Rockwood's dotfiles

## Installation

### Using Git and the bootstrap script

You can clone the repository wherever you want. (I like to keep it in `~/devel/dotfiles`) The
bootstrap script will create symlinks in your home folder.

    git clone https://github.com/rockwood/dotfiles.git
    cd dotfiles
    source bootstrap.sh

### Specify the `$PATH`

If `~/.path` exists, it will be sourced along with the other files, before any feature testing

Here’s an example `~/.path` file that adds `~/utils` to the `$PATH`:

    export PATH="$HOME/utils:$PATH"

### Add custom commands without creating a new fork

If `~/.extra` exists, it will be sourced along with the other files. You can use this to add a few
custom commands without the need to fork this entire repository, or to add commands you don’t want
to commit to a public repository.

## Scripts

### Sensible OS X defaults

When setting up a new Mac, you may want to set some sensible OS X defaults:

```bash script/osx_setup ```

### Install Homebrew formulae

When setting up a new Mac, you may want to install some common [Homebrew](http://brew.sh/) formulae
(after installing Homebrew, of course):

    cd script
    brew bundle

### Run ssh setup

This will walk through creating an ssh key:

    bash script/ssh_setup

### Print bash colors

Helpful if you're tweeking your iTerm theme

    bash script/colors

## Author

| [![twitter/kevinrockwood](http://www.gravatar.com/avatar/180276ea8f2063445af08593e20f53ec?s=70)](http://twitter.com/kevinrockwood "Follow @kevinrockwood on Twitter") |
|---|
| [Kevin Rockwood](http://rockwood.me/) |


| [![twitter/mathias](http://gravatar.com/avatar/24e08a9ea84deb17ae121074d0f17125?s=70)](http://twitter.com/mathias "Follow @mathias on Twitter") |
|---|
| [Mathias Bynens](http://mathiasbynens.be/) |

## Thanks to…

* @ptb and [his _OS X Lion Setup_ repository](https://github.com/ptb/Mac-OS-X-Lion-Setup)
* [Ben Alman](http://benalman.com/) and his [dotfiles repository](https://github.com/cowboy/dotfiles)
* [Chris Gerke](http://www.randomsquared.com/) and his [tutorial on creating an OS X SOE master
  image](http://chris-gerke.blogspot.com/2012/04/mac-osx-soe-master-image-day-7.html) + [_Insta_
  repository](https://github.com/cgerke/Insta)
* [Cãtãlin Mariş](https://github.com/alrra) and his [dotfiles repository](https://github.com/alrra/dotfiles)
* [Gianni Chiappetta](http://gf3.ca/) for sharing his [amazing collection of dotfiles](https://github.com/gf3/dotfiles)
* [Jan Moesen](http://jan.moesen.nu/) and his [ancient `.bash_profile`](https://gist.github.com/1156154) + [shiny
  _tilde_ repository](https://github.com/janmoesen/tilde)
* [Lauri ‘Lri’ Ranta](http://lri.me/) for sharing [loads of hidden preferences](http://osxnotes.net/defaults.html)
* [Matijs Brinkhuis](http://hotfusion.nl/) and his [dotfiles repository](https://github.com/matijs/dotfiles)
* [Nicolas Gallagher](http://nicolasgallagher.com/) and his [dotfiles repository](https://github.com/necolas/dotfiles)
* [Sindre Sorhus](http://sindresorhus.com/)
* [Tom Ryder](http://blog.sanctum.geek.nz/) and his [dotfiles repository](https://github.com/tejr/dotfiles)

* anyone who [contributed a patch](https://github.com/mathiasbynens/dotfiles/contributors) or [made a helpful
  suggestion](https://github.com/mathiasbynens/dotfiles/issues)
