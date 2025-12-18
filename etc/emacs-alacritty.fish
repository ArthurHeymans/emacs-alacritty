# Emacs-alacritty shell integration for fish
# Source this file in your fish config when running inside emacs-alacritty

function alacritty_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [ "$INSIDE_EMACS" = 'alacritty' ]
    function clear
        alacritty_printf "51;Ealacritty-clear-scrollback";
        tput clear;
    end
end

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
# Format: user@host:path - this allows emacs-alacritty to track the directory
function fish_title
    whoami
    echo "@"
    hostname
    echo ":"
    pwd
end

# With alacritty_cmd you can execute Emacs commands directly from the shell.
# For example, alacritty_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# emacs-alacritty-eval-cmds.
function alacritty_cmd --description 'Run an Emacs command among the ones defined in emacs-alacritty-eval-cmds.'
    set -l elisp ()
    for arg in $argv
        set -a elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
    end
    alacritty_printf '51;E'(string join '' $elisp)
end

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
function alacritty_prompt_end;
    alacritty_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

# We are going to add a portion to the prompt, so we copy the old one
if [ "$INSIDE_EMACS" = 'alacritty' ]
    functions --copy fish_prompt alacritty_old_fish_prompt

    function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
        # Remove the trailing newline from the original prompt. This is done
        # using the string builtin from fish, but to make sure any escape codes
        # are correctly interpreted, use %b for printf.
        printf "%b" (string join "\n" (alacritty_old_fish_prompt))
        alacritty_prompt_end
    end
end
