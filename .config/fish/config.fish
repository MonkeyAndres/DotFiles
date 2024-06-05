if status is-interactive
    # Commands to run in interactive sessions can go here
    
    # Remove greeting
    set -U fish_greeting

    # Setup Atuin for history sync
    atuin init fish | source
end

# Created by `pipx` on 2024-03-31 23:48:51
set PATH $PATH /home/monkey/.local/bin

# Deno setup
set -Ux DENO_INSTALL "/home/monkey/.deno"
set -U fish_user_paths "$DENO_INSTALL/bin" $fish_user_paths

# eval '/home/linuxbrew/.linuxbrew/bin/brew shellenv'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /home/monkey/mambaforge/bin/conda
    eval /home/monkey/mambaforge/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/home/monkey/mambaforge/etc/fish/conf.d/conda.fish"
        . "/home/monkey/mambaforge/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/home/monkey/mambaforge/bin" $PATH
    end
end

if test -f "/home/monkey/mambaforge/etc/fish/conf.d/mamba.fish"
    source "/home/monkey/mambaforge/etc/fish/conf.d/mamba.fish"
end
# <<< conda initialize <<<

# Redirect ollama commands to Docker container
# alias ollama="docker exec ollama ollama"

# GOPATH
set -gx GOPATH /home/monkey/go
set -U fish_user_paths "$GOPATH/bin" $fish_user_paths
