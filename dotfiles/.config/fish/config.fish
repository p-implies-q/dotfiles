# Define some useful aliases
alias ls "ls --group-directories-first --color"
alias nix-hask 'nix-env -f "<nixpkgs>" -qaP -A haskellPackages | grep '
alias bgset "feh --bg-scale"
# Set the theme
# omf theme scorphish
# set -x theme_color_scheme gruvbox

# Set dircolors
eval (dircolors -c ~/.dircolors)

# Run keychain
eval (keychain --eval --agents ssh -Q --quiet id_rsa --nogui)

# Disable the greeting
set fish_greeting ""

eval (dircolors -c ~/.dircolors)

set PATH "/home/david/proj/neurospin/bin" $PATH
set PATH "/home/david/.emacs.d/bin" $PATH
set PATH "/home/david/.local/bin" $PATH
set PATH "/home/david/.emacs.d/bin" $PATH

# Emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

function fish_title
    true
end


if [ "$TERM" = "linux" ]
    then
    echo -en "\e]P0222222" #black
    echo -en "\e]P8222222" #darkgrey
    echo -en "\e]P1803232" #darkred
    echo -en "\e]P9982b2b" #red
    echo -en "\e]P25b762f" #darkgreen
    echo -en "\e]PA89b83f" #green
    echo -en "\e]P3aa9943" #brown
    echo -en "\e]PBefef60" #yellow
    echo -en "\e]P4324c80" #darkblue
    echo -en "\e]PC2b4f98" #blue
    echo -en "\e]P5706c9a" #darkmagenta
    echo -en "\e]PD826ab1" #magenta
    echo -en "\e]P692b19e" #darkcyan
    echo -en "\e]PEa1cdcd" #cyan
    echo -en "\e]P7ffffff" #lightgrey
    echo -en "\e]PFdedede" #white
    clear #for background artifacting
end
