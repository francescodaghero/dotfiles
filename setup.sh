#!/usr/bin/env bash

# Setup dei file dopo il git clone.

# Z-SHELL
echo "Linking oh-my-zshrc config files"
if [[ -f "$HOME/.zshrc" ]]
then
   mv $HOME/.zshrc $HOME/.zshrc_old
fi
ln -sr .zshrc $HOME/.zshrc

#Oh my zsh
if [[ -d "$HOME/.oh-my-zsh/custom/" ]]
then
    ln -sr .oh-my-zsh/custom/ $HOME/.oh-my-zsh/custom/
else
    echo "No zsh installation found"
fi

# Tmux
echo "Linking tmux config files"
if [[ -f "$HOME/.tmux.conf" ]]
then
   mv $HOME/.tmux.conf $HOME/.tmux_old.conf  
fi
ln -sr .tmux.conf $HOME/.tmux.conf


# Neovim
echo "Linking nvim setup"
if [[ -d "$HOME/.config/nvim" ]]
then
    mv $HOME/.config/nvim $HOME/.config/nvim_old
else
    echo "No nvim found, is it in a different path?"
fi
ln -sr .config/nvim $HOME/.config/nvim
