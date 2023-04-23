export PROMPT="[%n@%m %~]$ " # PS1='\e[37;1m[\u@\h \W]$ \e[0m'
export EDITOR=nvim
export PATH="$PATH:/opt/local/bin:$HOME/Developer/opt/cross/bin:$HOME/bin:$HOME/.local/bin"
export GOPATH="$HOME/.local/go"
# Let tmux see the terminal databse on MacOS Ventura.
export TERM=xterm-256color
# Weird macOS behaviour
export SHELL=$(which zsh)

alias vim=nvim

function fun_aliases() {
    alias starwars="telnet towel.blinkenlights.nl"
    alias telehack="telnet telehack.com"
    alias playchess="telnet telnet freechess.org"
    alias playgo="telnet igs.joyjoy.net 6969"
    alias bbs="telnet gt.gamingmuseum.com"
    alias dnd="telnet nethack.alt.org"
    alias backgammon="telnet fibs.com 4321"
}

function do_nvm() {
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
}

function do_xmake() {
    if [ -d "$HOME/.xmake" ]; then
        source $HOME/.xmake/profile
    fi
}

function do_welcome() {
    echo "Welcome. If you are intruder, please log out now."
    echo
    echo "\"Threading future through the past... There is no reason to believe.\""
}

fun_aliases
do_nvm
do_xmake
do_welcome
