# Define some useful aliases
alias ls "ls --group-directories-first --color"

# Set the theme
# omf theme scorphish
# set -x theme_color_scheme gruvbox

# Set dircolors
eval (dircolors -c ~/.dircolors)


# Disable the greeting
set fish_greeting ""

eval (dircolors -c ~/.dircolors)

set PATH "/home/david/proj/neurospin/bin" $PATH

# Emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

function fish_title
    true
end
