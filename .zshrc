export BASH_SILENCE_DEPRECATION_WARNING=1
export PROMPT="[%n@%m %~]$ " # PS1='\e[37;1m[\u@\h \W]$ \e[0m'
export EDITOR=nvim
export PATH="$PATH:$HOME/Developer/opt/cross/bin:$HOME/bin:$HOME/.local/bin"
# Let tmux see the terminal databse on MacOS Ventura.
export TERM=xterm-256color

alias vim=nvim

function get_cowfile() {
    dir=""

    if [ -d /usr/share/cows ]; then
        dir=/usr/share/cows
    else
        dir=$(ls -d /nix/store/* | grep "cowsay-" | sed -n 2p)/share/cowsay/cows
    fi

    echo $(find $dir -type f | sort -R | head -n1)
}

function cowdo() {
    cowsay -f $(get_cowfile) $(fortune -a -s)
}

function fun_aliases() {
    alias starwars="telnet towel.blinkenlights.nl"
    alias telehack="telnet telehack.com"
    alias playchess="telnet telnet freechess.org"
    alias playgo="telnet igs.joyjoy.net 6969"
    alias bbs="telnet gt.gamingmuseum.com"
    alias dnd="telnet nethack.alt.org"
    alias backgammon="telnet fibs.com 4321"
}

fun_aliases
cowdo

