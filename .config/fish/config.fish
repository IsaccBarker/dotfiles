function get_cowfile
    set dir ""
    switch (uname)
        case Linux
            set dir /usr/share/cows
        case Darwin
            set dir (ls -d /nix/store/* | grep "cowsay-" | sed -n 2p)/share/cowsay/cows
    end

    echo (ls $dir | sort -R | head -n1)
end

function cowdo
    switch (uname)
        case Linux Darwin
            cowsay -f (get_cowfile) (fortune -a -s)
        case '*'
            echo 'What in the hell... (uname)?'
    end
end

function fun_aliases
    alias starwars="telnet towel.blinkenlights.nl"
    alias telehack="telnet telehack.com"
    alias playchess="telnet telnet freechess.org"
    alias playgo="telnet igs.joyjoy.net 6969"
    alias bbs="telnet gt.gamingmuseum.com"
    alias dnd="telnet nethack.alt.org"
    alias backgammon="telnet fibs.com 4321"
end

if status is-interactive
    # Editor related stuff
    export EDITOR=nvim
    alias vim=nvim

    # Print something funny.
    cowdo

    # Mutate the path
    export PATH="$PATH:$HOME/Developer/opt/cross/bin:$HOME/bin:$HOME/.local/bin"

    # Have some fun!
    fun_aliases

    # Let tmux see the terminal databse on MacOS Ventura.
    export TERM=screen-256color
end

