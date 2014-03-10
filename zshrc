[ -r /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && . /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
[ -r /usr/share/doc/pkgfile/command-not-found.zsh ] && . /usr/share/doc/pkgfile/command-not-found.zsh
autoload -U compinit promptinit colors
compinit
promptinit
colors

function zle-line-init zle-keymap-select {
	PROMPT="%(?..[%{$fg[red]%}%?%{$reset_color%}]) ${${KEYMAP/vicmd/"%{$fg[white]%}"}/(main|viins)/"%{$fg[red]%}"}» %{$reset_color%}"
	RPROMPT="%B%{$fg[cyan]%}%~%{$reset_color%} %{$fg[blue]%}%!%{$reset_color%}"
	zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# vim-like keybindings
# modified from sharat87@github / zsh-vim-mode
bindkey -v

bindkey '^k' vi-cmd-mode # <C-k> for going to command mode
bindkey -M vicmd ' ' execute-named-cmd # Space for command line mode

# Home key variants
bindkey '\e[1~' vi-beginning-of-line
bindkey '\eOH' vi-beginning-of-line

# End key variants
bindkey '\e[4~' vi-end-of-line
bindkey '\eOF' vi-end-of-line

bindkey -M viins '^o' vi-backward-kill-word

bindkey -M vicmd 'yy' vi-yank-whole-line
bindkey -M vicmd 'Y' vi-yank-eol

bindkey -M vicmd 'y.' vi-yank-whole-line
bindkey -M vicmd 'c.' vi-change-whole-line
bindkey -M vicmd 'd.' kill-whole-line

bindkey -M vicmd 'u' undo
bindkey -M vicmd 'U' redo

bindkey -M vicmd 'H' run-help
bindkey -M viins '\eh' run-help

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

bindkey '^p' history-substring-search-up
bindkey '^n' history-substring-search-down

bindkey -M vicmd '\-' vi-repeat-find
bindkey -M vicmd '_' vi-rev-repeat-find

bindkey -M viins '\e.' insert-last-word
bindkey -M vicmd '\e.' insert-last-word

bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
# end of vim bindings
bindkey '^R' history-incremental-search-backward

[[ -t 1 ]] || return
case $TERM in
	*xterm*|*rxvt*|(dt|k|E|a)term)
		preexec () {
			print -Pn "\e]2;$1\a"    # edited; %n@%m omitted, as I know who and where I am
		}
		;;
esac

setopt auto_cd
setopt correct
setopt completealiases
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
setopt no_clobber
setopt nohup
COMPLETION_WAITING_DOTS="true"
export HISTFILE="${HOME}"/.zsh-history
export HISTSIZE=2000
export SAVEHIST=$HISTSIZE
# When offering typo corrections, do not propose anything which starts with an
# underscore (such as many of zsh's shell functions).
export CORRECT_IGNORE='_*'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
# Ignore completion functions for commands you don’t have
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' menu select
# Completing process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }

conf() {
	case $1 in
		xmonad)		vim ~/.config/dotfiles/xmonad.hs ;;
		conky)		vim ~/.xmonad/statusbar_conkyrc ;;
		mpd)		vim ~/.mpdconf ;;
		compton)	vim ~/.config/compton.conf ;;
		ncmpcpp)	vim ~/.ncmpcpp/config ;;
		pacman)		svim /etc/pacman.conf ;;
		ranger)		vim ~/.config/ranger/rc.conf ;;
		rifle)		vim ~/.config/ranger/rifle.conf ;;
		tmux)		vim ~/.tmux.conf ;;
		vim)		vim ~/.vimrc ;;
		xinit)		vim ~/.xinitrc ;;
		xresources)	vim ~/.Xresources && xrdb ~/.Xresources ;;
		zathura)	vim ~/.config/zathura/zathurarc ;;
		gtk2)		vim ~/.gtkrc-2.0 ;;
		gtk3)		vim ~/.config/gtk-3.0/settings.ini ;;
		zsh)		vim ~/.zshrc && source ~/.zshrc ;;
		hosts)		sudoedit /etc/hosts ;;
		vhosts)		sudoedit /etc/httpd/conf/extra/httpd-vhosts.conf ;;
		httpd)		sudoedit /etc/httpd/conf/httpd.conf ;;
		*)			echo "Unknown application: $1" ;;
	esac
}

# Programs
alias installfont='sudo fc-cache -f -v'
alias archey='archey3 --config=${XDG_CONFIG_HOME}/archey3.cfg'

# Shortcuts
alias rmi='rm -i'
alias h='history | tail -n 10'
alias hgrep='history | grep '
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias ll='ls -AlF'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'
alias vi='vim'

# enable color support of ls and also add handy aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

man() {
	env LESS_TERMCAP_mb=$'\E[01;31m' \
	LESS_TERMCAP_md=$'\E[01;38;5;74m' \
	LESS_TERMCAP_me=$'\E[0m' \
	LESS_TERMCAP_se=$'\E[0m' \
	LESS_TERMCAP_so=$'\E[38;5;246m' \
	LESS_TERMCAP_ue=$'\E[0m' \
	LESS_TERMCAP_us=$'\E[04;38;5;146m' \
	man "$@"

}

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi
export PATH=${PATH}:/usr/local/texlive/2013/bin/x86_64-linux
export XDG_CONFIG_HOME="/home/tlw/.config"
# Tell Java apps that I'm using a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1
