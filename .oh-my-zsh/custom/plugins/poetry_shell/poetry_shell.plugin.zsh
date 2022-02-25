# Codice adattato da https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/pipenv/pipenv.plugin.zsh
_togglePoetryShell() {
  # deactivate shell if Pipfile doesn't exist and not in a subdir
  if [[ ! -f "$PWD/poetry.lock" ]]; then
    if [[ "$POETRY_ACTIVE" == 1 ]]; then
      if [[ "$PWD" != "$poetry_dir"* ]]; then
        exit
      fi
    fi
  fi

  # activate the shell if Pipfile exists
  if [[ "$POETRY_ACTIVE" != 1 ]]; then
    if [[ -f "$PWD/poetry.lock" ]]; then
      export poetry_dir="$PWD"
      poetry shell
    fi
  fi
}
autoload -U add-zsh-hook
add-zsh-hook chpwd _togglePoetryShell
_togglePoetryShell
