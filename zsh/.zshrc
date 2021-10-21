# options
setopt correct # Auto correct mistakes
setopt extendedglob # Extended globbing. Allows using regular expressions with *
setopt rcexpandparam # Array expension with parameters
setopt numericglobsort # Sort filenames numerically when it makes sense
setopt nobeep
setopt appendhistory # Immediately append history instead of overwriting
setopt histignorealldups # If a new command is a duplicate, remove the older one
setopt no_auto_remove_slash # don't remove trailing slash after command and arrow up
setopt promptsubst # enable substitution for prompt

# exports
export VISUAL="/usr/bin/emacs"
export EDITOR="/usr/bin/emacs -nw"

export LS_COLORS="no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:"

export PATH="$HOME/.cargo/bin:$PATH"
export GOPATH="$HOME/go"

# completions
autoload -U compinit && compinit -d

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case insensitive tab completion
zstyle ":completion:*:default" list-colors ${(s.:.)LS_COLORS} 'ma=0;7' # ls colors for directory completions
zstyle ':completion:*' menu select # Selection menu
zstyle ':completion:*:*:*:*:descriptions' format '%F{blue}%d: %f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands

# Speed up completions
zstyle ":completion:*" accept-exact '*(N)' # magic?
zstyle ":completion:*" cache-path ~/.zsh/cache
zstyle ":completion:*" use-cache on

# history
HISTFILE=~/.zsh/history
HISTSIZE=1000
SAVEHIST=500

WORDCHARS="" # don't consider any special characters part of the word

# aliases
# color
alias ls='ls --color=auto'
alias grep='grep --color=auto'
# confirm before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias df='df -h' # Human-readable sizes
alias free='free -m' # Show sizes in MB

# Plugins
# Use syntax highlighting
if [ -r /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
# Use history substring search
if [ -r /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh ]; then
    source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
    # bind UP and DOWN arrow keys to history substring search
    bindkey "^[[A" history-substring-search-up
    bindkey "^[[B" history-substring-search-down
fi

# keybindings
bindkey -e
bindkey "^[[2~" overwrite-mode # Insert key
bindkey "^[[3~" delete-char # Delete key
bindkey '^[[Z' reverse-menu-complete # shift + TAB to go backwards in completion menu

# Theming section
autoload -U colors && colors

# prompt
user="%(!.%F{red}.%F{yellow})%n%f"

case "$HOST" in
    "manjaro")
        hostname=""
        ;;
    *)
        hostname="%m"
        ;;
esac
host="%F{green}$hostname%f"

directory="%F{blue}%1~%f"

symbol="%(!.%F{red}.)%#%f"

# [username@machine] directory % <- `#` if root and red
#  ^- red if root    ^- top directory
PROMPT="[$user@$host] $directory $symbol "

# Prompt on right:
#  - if inside git repository: git branch and status (adapted from https://gist.github.com/joshdick/4415470)
#  - if previous command finished with an error: exit status
#  - else: nothing

# Show Git branch/tag, or name-rev if on detached head
parse_git_branch() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

parse_git_state() {
  # Show different symbols as appropriate for various Git repository states
  # Compose this value via multiple conditional appends.
  local git_state=""

  local git_dir="$(git rev-parse --git-dir 2>/dev/null)"
  if [ -n $git_dir ] && test -r $git_dir/MERGE_HEAD; then
    git_state="$git_state%F{magenta}⚡%f"
  fi
  if [[ -n "$(git ls-files --other --exclude-standard 2>/dev/null)" ]]; then
    git_state="$git_state%F{red}●%f"
  fi
  if ! git diff --quiet 2> /dev/null; then
    git_state="$git_state%F{yellow}●%f"
  fi
  if ! git diff --cached --quiet 2> /dev/null; then
    git_state="$git_state%F{green}●%f"
  fi
  if [[ -n $git_state ]]; then
    echo " [$git_state]"
  fi
}

git_prompt_string() {
  local branch="$(parse_git_branch)"

  if [ -n "$branch" ]; then
      # If inside a Git repository, print its branch and state
      local n_ahead="$(git rev-list origin..HEAD --count 2>/dev/null)"
      if [ "$n_ahead" -gt 0 ]; then
          n_ahead=":%F{blue}+$n_ahead%f"
      else
          n_ahead=""
      fi
      local n_behind="$(git rev-list HEAD..origin --count 2>/dev/null)"
      if [ "$n_behind" -gt 0 ]; then
          n_behind=":%F{cyan}-$n_behind%f"
      else
          n_behind=""
      fi

      echo "%F{yellow} ${branch#(refs/heads/|tags/)}%f$n_ahead$n_behind$(parse_git_state)"
  else
      # If not inside the Git repo, print exit codes of last command (only if it failed)
      echo "%{$fg[red]%}%(?..(╯°□°）╯︵[%?])%{$reset_color%}"
  fi
}

#  branch:+ahead:-behind [status]
RPROMPT='$(git_prompt_string)'

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r
