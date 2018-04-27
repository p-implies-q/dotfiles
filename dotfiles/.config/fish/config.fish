
# Set theme based on circumstances
if set -q IN_NIX_SHELL
   omf theme cbjohnson
else
    omf theme agnoster
end

set fish_greeting ""
set -gx DEF_THEME agnoster
set -gx NIX_SHELL_THEME cbjohnson
