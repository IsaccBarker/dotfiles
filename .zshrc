export PROMPT="[%n@%m %~]$ " # PS1='\e[37;1m[\u@\h \W]$ \e[0m'
export EDITOR=nvim
export PATH="$PATH:$HOME/Developer/opt/cross/bin:$HOME/bin:$HOME/.local/bin"
# Let tmux see the terminal databse on MacOS Ventura.
export TERM=xterm-256color

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

function do_neofetch_if_avail() {
    if type "neofetch" > /dev/null; then
        neofetch
    fi
}

fun_aliases
do_neofetch_if_avail

