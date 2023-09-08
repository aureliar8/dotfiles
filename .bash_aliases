alias o='xdg-open'
alias c='clear'
alias tree='tree -C'
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
if command -v exa &>/dev/null
then
	alias ls='exa -a --color=always --group-directories-first' # my preferred listing
	alias ll='exa -l --color=always --group-directories-first'  # long format
	alias lt='exa -aT --color=always --group-directories-first' # tree listing
fi

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'


### ARCHIVE EXTRACTION
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.jar) 	   unzip $1	;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;      
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}


function toolbox() {
    docker run --rm -ti -v ~/.cloudstack.ini:/root/.cloudstack.ini -v ~/.config/exoscale/toolbox:/root/.config/exoscale/toolbox/ registry.internal.exoscale.ch/exoscale/toolbox:latest $*
}

alias findhost='ssh exoadmin@infra-dns003.gv2.p.exoscale.net findhost $1'

function tgo() {
    tmp="$(mktemp -p /tmp -d "tgo_$(date +%Y%m%d)_XXXXXXXX")"
    printf 'package main\n\nfunc main() {\n\n}\n' > "$tmp/main.go"
    printf 'package main\nimport "testing"\nfunc TestMain(t *testing.T) {\n\n}\n\n' > "$tmp/main_test.go"
    printf 'func BenchmarkMain(b *testing.B) {\n\tb.ReportAllocs()\n\tfor n := 0; n < b.N; n++ {\n\t}\n}\n' >> "$tmp/main_test.go"

    printf 'module %s\n' "$(basename "$tmp")" > "$tmp/go.mod"
    (
        cd "$tmp"
        emacs main.go main_test.go
        echo "$tmp"
    )
}

alias rg='rg --hidden'
