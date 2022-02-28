import os
import shutil
from datetime import datetime
from glob import glob

ERROR_MESSAGE='xxx Error xxx'
SUCCESS_MESSAGE='~~~ Success ~~~'
REINSTALL = True

HOME=os.path.expanduser('~')
BACKUP_FOLDER=f"{HOME}/.backup"

# Symlink the folders
MAPPINGS = (
        #From:To
        (".scripts",f"{HOME}/.scripts"),
        (".zshrc",f"{HOME}/.zshrc"),
        (".tmux.conf",f"{HOME}/.tmux.conf"),
        (".vimrc",f"{HOME}/.vimrc"),
        (".config/lf",f"{HOME}/.config/lf")
)

# Symlink all the subfolders, without overwriting 
SUB_MAPPINGS = (
        #From: To
        (".oh-my-zsh/custom/plugins/",f"{HOME}/.oh-my-zsh/custom/plugins/"),
        ("modules/",f"{HOME}/.config/")
)

# Dependencies
EXECUTABLES = (
        # Executable name, path
        ('zsh', 'Zsh'),
        ('lf', 'Lf file manager'),
        ('vim', 'Vim'),
        ('bat', 'Bat'),
        ('fzf', 'Fzf'),
        ('rg', 'Ripgrep')
)

DATA = (
        (f'{HOME}/.oh-my-zsh','Oh-My-Zsh'),
        #(f'{HOME}/.local/share/lscolors.sh','LS-COLORS')
)

def has_previous_version(full_path):
    # Check the target is a file or a directory
    exists = (os.path.isdir(full_path) or os.path.isfile(full_path))
    if os.path.islink(full_path):
        assert REINSTALL,   "Errore: Trovato un symlink, settare la flag REINSTALL a TRUE per rimuoverli"
    return exists

def backup(output_path, backup_folder):
    '''
    Esegue il backup della cartella o del file e lo RIMUOVE dalla posizione precedente
    '''
    os.makedirs(backup_folder, exist_ok=True)
    print(f"Creo una copia di backup di {output_path}...")
    now=datetime.now()
    date_string=f'{now.year}_{now.month}_{now.day}'
    name=os.path.split(output_path)[-1]
    timestamp_name=name+'_'+date_string
    backup_fullpath=os.path.join(backup_folder,timestamp_name)
    assert not os.path.exists(backup_fullpath), \
            f"Trovato un backup con lo stesso nome nella cartella {backup_fullpath}, errore"
    shutil.move(output_path,backup_fullpath)
    print(f">> File spostato/i in {backup_fullpath}")
    return backup_fullpath


def symlink(full_path, output_path):
    print(f"Symlinking {full_path} to {output_path}")
    if REINSTALL and os.path.islink(output_path):
        os.unlink(output_path)
    abs_full_path=os.path.abspath(full_path)
    os.symlink(abs_full_path, output_path)

def symlink_subelements(full_path, output_path, max_depth=0):
    path_at_depth=glob(os.path.join(full_path,*["*" for _ in
        range(max_depth+1)]))
    folders_at_depth=filter(os.path.isdir, path_at_depth)
    for folder in folders_at_depth:
        destination_folder=folder.replace(full_path,"")
        output_path_folder=os.path.join(output_path,destination_folder)
        symlink(folder, output_path_folder)



def check_install(exec_name):
    """Check whether `name` is on PATH and marked as executable.
    """
    is_installed= shutil.which(exec_name) is not None
    print(ERROR_MESSAGE if not is_installed else SUCCESS_MESSAGE)
    return is_installed

def check_existance(exec_name):
    """Check whether `name` is on PATH and marked as executable.
    """
    is_installed= shutil.which(exec_name) is not None
    print(ERROR_MESSAGE if not is_installed else SUCCESS_MESSAGE)
    return is_installed


def check_dependencies(executables, data):
    '''
    Checks if each dependency is met, executables are checked with which, data (e.g. folders, files) with os
    executables: tuple of tuples containing (executable_name, program_name) . E.g ("nvim","neovim")
    data: tuple of tuples containing (file_required, dependency_name)
    '''
    print('Checking dependencies...')
    print('> Executables...')
    for executable, program in executables:
        print(f'>> {program}')
        is_installed = check_install(executable)
        assert is_installed, "Missing dependency, stopping execution..."
    for position, name in data:
        print(f'>> {name}')
        is_present= os.path.exists(position)
        print(ERROR_MESSAGE if not is_present else SUCCESS_MESSAGE)
        assert is_present, "Missing dependency, stopping execution..."
    print("All dependencies are met")



def setup(mappings, sub_folder_mappings):
    print("Starting the setup...")
    for source, target in mappings:
        if has_previous_version(target):
            backup(target,BACKUP_FOLDER)
        symlink(source, target)
    for source, target in sub_folder_mappings:
        symlink_subelements(source, target)
    if "vim" in mappings:
        os.makedirs(f"{HOME}/.vim/undodir", exist_ok=True)
    print("Setup successful")

if __name__=='__main__':
    check_dependencies(EXECUTABLES, DATA)
    setup(MAPPINGS, SUB_MAPPINGS)

