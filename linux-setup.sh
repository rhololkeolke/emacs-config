#!/bin/bash

set -e

confirm () {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case $response in
        [yY][eE][sS]|[yY]) 
            true
            ;;
        *)
            false
            ;;
    esac
}

# install cask if not already present
if [ -z ~/.cask ]; then
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

# add the cask command to the path
which cask || echo 'export PATH=~/.cask/bin/:$PATH' >> ~/.bashrc

# add the cask command to this sessions path
export PATH=~/.cask/bin:$PATH

# install the emacs packages via Cask
pushd emacs.d
cask install
popd

# check if .emacs symlink exists

repo_dir=$(realpath $(dirname $0))

if [[ -L ~/.emacs ]]; then
	if [[ "$(stat -L -c "%d:%i" ~/.emacs)" != "$(stat -c "%d:%i" $repo_dir/emacs)" ]]; then
		echo "~/.emacs exists and points to a different location"
		echo "Going to delete and replace the old symlink"
		if confirm; then
			rm ~/.emacs
			ln -s $repo_dir/emacs ~/.emacs
		else
			echo "denied. Quitting install..."
			exit 1
		fi
	else
		echo "~/.emacs already points to correct location"
	fi
else
	if [[ -e ~/.emacs ]]; then
		echo "~/.emacs exists and is not a symlink"
		echo "Going to delete the existing .emacs. If you proceed you will lose whatever is currently in the file"
		if confirm; then
			rm -rf ~/.emacs
			ln -s $repo_dir/emacs ~/.emacs
		else
			echo "denied. Quitting install..."
			exit 1
		fi
	else
		echo "No ~/.emacs exists. Creating the symlink"
		ln -s $repo_dir/emacs ~/.emacs
	fi
fi

if [[ -L ~/.emacs.d ]]; then
	if [[ "$(stat -L -c "%d:%i" ~/.emacs.d)" != "$(stat -c "%d:%i" $repo_dir/emacs.d)" ]]; then
		echo "~/.emacs.d exists and points to a different location"
		echo "Going to delete and replace the old symlink"
		if confirm; then
			rm ~/.emacs
			ln -s $repo_dir/emacs.d ~/.emacs.d
		else
			echo "denied. Quitting install..."
			exit 1
		fi
	else
		echo "~/.emacs.d already points to the correct location"
	fi
else
	if [[ -e ~/.emacs ]]; then
		echo "~/.emacs.d exists and is not a symlink"
		echo "Going to delete the existing .emacs.d. If you proceed you will lose whatevedr is currently in .emacs.d"
		if confirm; then
			rm -rf ~/.emacs.d
			ln -s $repo_dir/emacs.d ~/.emacs.d
		else
			echo "denied. Quitting install..."
			exit 1
		fi
	else
		echo "No ~/.emacs.d exists. Creating the symlink"
		ln -s $repo_dir/emacs.d ~/.emacs.d
	fi
fi

echo "Finished installing. You may now launch emacs"
