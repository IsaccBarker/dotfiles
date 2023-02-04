# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set out default editor
export EDITOR="helix"

function get_cowfile() {
    dir=/usr/share/cows

    if [ -d /usr/share/cowsay/cows ]; then
        dir=/usr/share/cowsay/cows
    fi

    f=$(find $dir -type f | shuf -n 1)
    if [[ "$(cat $f)" == *"Acme::Cow"* ]]; then
        echo $(get_cowfile)
    fi

    echo $f
}

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
if [[ -n $DISPLAY ]];
then
    ZSH_THEME="bira"
else 
    ZSH_THEME="gallois"
fi

# source $ZSH/oh-my-zsh.sh

alias vim=nvim # Use the supperior vim
alias hx=helix # Use the supperior modal text editor.

# Enable Starship
eval "$(starship init zsh)"

# Get time since midnight
zmodload zsh/datetime
now=$EPOCHSECONDS
strftime -s today %F $now
strftime -rs midnight %F $today
since_midnight=$((now - midnight))

# Neofetch!
# if [[ "$OSTYPE" != *darwin* ]]; then
#     neofetch --ascii_distro `fortune neofetch-os` --disable terminal --disable packages --disable host --disable --disable uptime --disable shell --disable resolution --disable de --disable wm --disable "wm theme"   --disable theme --disable icons
# else
#     neofetch --disable terminal --disable packages --disable host --disable --disable uptime --disable shell --disable resolution --disable de --disable wm --disable "wm theme" --disable theme --disable icons
# fi

echo

if [[ "$OSTYPE" != *darwin* ]]; then
    # Quotes
    if [ "$since_midnight" -ge "75600" ]; then
        cowsay -f $(get_cowfile) $(fortune -a -s) # No one is looking, make offensive jokes
    else
        cowsay -f $(get_cowfile) $(fortune -a -s) # People might be looking
    fi
fi

echo

# Disable palm detection
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    xinput set-prop 13 365 0 1>/dev/null 2>/dev/null
fi

export PATH="$PATH:$HOME/Developer/opt/cross/bin:$HOME/bin:$HOME/.local/bin"
alias mux=tmuxinator
alias starwars="telnet towel.blinkenlights.nl"
alias telehack="telnet telehack.com"
alias playchess="telnet telnet freechess.org"
alias playgo="telnet igs.joyjoy.net 6969"
alias bbs="telnet gt.gamingmuseum.com"
alias dnd="telnet nethack.alt.org"
alias backgammon="telnet fibs.com 4321"

export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

bindkey -v # Vi keybindings

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Let tmux see the terminal databse on MacOS Ventura.
export TERM=screen-256color
