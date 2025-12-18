# Alacritty shell integration for zsh
# Source this file in your zshrc when running inside alacritty

alacritty_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer
if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    alias clear='alacritty_printf "51;Ealacritty-clear-scrollback"; tput clear'
fi

# With alacritty_cmd you can execute Emacs commands directly from the shell.
alacritty_cmd() {
    local elisp
    elisp=""
    while [ $# -gt 0 ]; do
        elisp="$elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    alacritty_printf "51;E$elisp"
}

# Set the title
alacritty_title() {
    alacritty_printf "0;$*"
}

# Sync directory with Emacs
alacritty_prompt_end() {
    alacritty_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# Set up the prompt hook
if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ alacritty_prompt_end }
    
    # Also run on prompt display
    precmd() {
        alacritty_prompt_end
        # Set title to user@host:path for directory tracking
        print -Pn "\e]0;%n@%m:%~\a"
    }
fi
