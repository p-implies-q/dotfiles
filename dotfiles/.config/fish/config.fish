# Define some useful aliases
alias ls "ls --group-directories-first --color"

# Set the theme
omf theme scorphish
# set -x theme_color_scheme gruvbox

# Disable the greeting
set fish_greeting ""

# Emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

function fish_title
    true
end
