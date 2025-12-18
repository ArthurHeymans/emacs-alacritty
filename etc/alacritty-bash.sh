# Alacritty shell integration for bash
# Source this file in your bashrc when running inside alacritty

function alacritty_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    function clear(){
        alacritty_printf "51;Ealacritty-clear-scrollback";
        tput clear;
    }
fi

# With alacritty_cmd you can execute Emacs commands directly from the shell.
# For example, alacritty_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# alacritty-eval-cmds.
alacritty_cmd() {
    local elisp
    elisp=""
    while [ $# -gt 0 ]; do
        elisp="$elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    alacritty_printf "51;E$elisp"
}

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
# Format: user@host:path - this allows alacritty to track the directory
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;$(whoami)@${HOSTNAME}:${PWD}\007"'

# Note: Directory tracking is done via the title (OSC 0) which includes user@host:path
# The OSC 51;A sequence is kept for compatibility but may not work with all terminal backends
alacritty_prompt_end(){
    alacritty_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    PS1=$PS1'\[$(alacritty_prompt_end)\]'
fi
