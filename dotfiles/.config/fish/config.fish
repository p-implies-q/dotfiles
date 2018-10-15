# Define some useful aliases
alias ls       "ls --group-directories-first --color"
alias nix-hask "nix-env -f '<nixpkgs>' -qaP -A haskellPackages | grep"
alias bgset    "feh --bg-scale"
alias e        "emacsclient -nw"
alias se       "sudo emacsclient -nw"
alias magit    "emacsclient -e '(call-interactively #\'magit-status)'"
alias keys     "eval (keychain --eval -Q --quiet id_rsa --nogui ^/dev/null)"

# Setup the path
set -gx PATH ~/.emacs.d/bin $PATH
set -gx PATH ~/.bin         $PATH

# Set dircolors
eval (dircolors -c ~/.dircolors)

# Disable the greeting
set fish_greeting ""

# Emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

function fish_title
    true
end

# Disable __pycache__ directories
set PYTHONDONTWRITEBYTECODE 1

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
