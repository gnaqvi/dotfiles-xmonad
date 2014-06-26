#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# ANSI-COLOR-CODES
Color_Off="\[\033[0m\]"

###-Regular-###
Yellow="\[\033[0;33m\]"
Red="\[\033[0;31m\]"
Green="\[\033[0;32m\]"
Purple="\[\033[0;35m\]"

####-Bold-####
BRed="\[\033[1;31m\]"
BPurple="\[\033[1;35m\]"

# Set up command prompt
function __prompt_command()
{
    PS1=""

    # If logged in via ssh shows the ip of the client
    if [ -n "$SSH_CLIENT" ]; then PS1+="$Yellow("${$SSH_CLIENT%% *}")$Color_Off"; fi

    # debian chroot stuff (take it or leave it)
    PS1+="${debian_chroot:+($debian_chroot)}"

    # Basic information (user@host:path)
    PS1+="$BRed\u$Color_Off@$BRed\h$Color_Off:$BPurple\w$Color_Off "

    # Check if inside git repo
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        # Parse the porcelain output of git status
        if [[ "$git_status" =~ nothing\ to\ commit ]]; then
            local Color_On=$Green
        elif [[ "$git_status" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
            local Color_On=$Purple
        else
            local Color_On=$Red
        fi

        if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
            branch=${BASH_REMATCH[1]}
        else
            # Detached HEAD. (branch=HEAD is a faster alternative.)
            branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null || echo HEAD`)"
        fi

        # Add the result to prompt
        PS1+="$Color_On($branch) "
    fi

    # Prompt $ or # for root
    PS1+="$Color_Off\$ "
}
PROMPT_COMMAND=__prompt_command

# Add CABAL bin/ and .xmonad/bin/ to PATH
export PATH=$PATH:$HOME/.cabal/bin:$HOME/.xmonad/bin

# Set Sublime Text as default editor
export EDITOR="subl3"

# Set Firefox as default browser
export BROWSER="firefox"
