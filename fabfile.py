from fabric.api import *
import os, os.path
import fabric.contrib.project as project

PROD = 'eugen@eugenkiss.com:223'
DEST_PATH = '/var/www/loopgotowhile/'
ROOT_PATH = os.path.abspath(os.path.dirname(__file__))
DEPLOY_PATH = os.path.join(ROOT_PATH, 'deploy')
TEST_PATH = os.path.join(ROOT_PATH, 'test')
NGINX_CONF_PATH = '/etc/nginx/sites-available/loopgotowhile'

@hosts(PROD)
def update_nginx_conf():
    put('nginx.conf', NGINX_CONF_PATH, use_sudo=True)
    sudo('/etc/init.d/nginx reload')

def backup():
    local('git push -f ~/Dropbox/backup/loopgotowhile-site.git')

def compile(profiling):
    local('rm -rf ' + os.path.join(ROOT_PATH, 'dist/'))
    if profiling:
        local('cabal configure --enable-executable-profiling --enable-library-profiling --ghc-option=-auto-all')
    else:
        local('cabal configure --disable-executable-profiling --disable-library-profiling')
    local('cabal build')

def copy(path):
    # make sure the directory is there!
    local('mkdir -p ' + path)
    # remove old contents
    local('rm -r ' + path + '/*')
    local('cp ' + os.path.join(ROOT_PATH, 'index.html') + ' ' +
                  os.path.join(path, 'index.html'))
    local('cp ' + os.path.join(ROOT_PATH, 'style.css') + ' ' +
                  os.path.join(path, 'style.css'))
    local('cp ' + os.path.join(ROOT_PATH, 'bg.jpg') + ' ' +
                  os.path.join(path, 'bg.jpg'))
    local('cp ' + os.path.join(ROOT_PATH, 'dist/build/LGWServer/LGWServer') + ' ' +
                  os.path.join(path, 'LGWServer'))
    local('cp -r ' + os.path.join(ROOT_PATH, 'codemirror') + ' ' +
                  os.path.join(path, 'codemirror'))

def test():
    """Test the server locally"""
    compile(False) # Profiling and using more than one thread is not possible
    # make sure the directory is there!
    local('mkdir -p ' + TEST_PATH)
    copy(TEST_PATH)
    with lcd(TEST_PATH): 
        local('LGWServer test +RTS -hd -p')
        #local('LGWServer test +RTS -N')

@hosts(PROD)
def update_static_files():
    copy(DEPLOY_PATH)
    project.rsync_project(
        remote_dir=DEST_PATH,
        local_dir=DEPLOY_PATH.rstrip('/') + '/',
        delete=True
    )
    update_nginx_conf()

@hosts(PROD)
def publish():
    # Kill old LGWServer instances
    # Note: For some magical reason I have to kill the processes here instead
    # of directly before restarting them at the bottom of this function. 
    # Otherwise LGWServer is hardly ever started on the remote. I guess the
    # killall has some delay/aftermath... but I also tried to put a `sleep 5s`
    # between the kills and the restarts without any luck...
    with settings(warn_only=True):
        run('killall -9 -v LGWServer ')
        run('killall -9 -v cpulimit ')
    backup()
    compile(False)
    copy(DEPLOY_PATH)
    project.rsync_project(
        remote_dir=DEST_PATH,
        local_dir=DEPLOY_PATH.rstrip('/') + '/',
        delete=True
    )
    update_nginx_conf()
    # Limit cpu usage (cpulimit must be installed on remote)
    run('nohup cpulimit -e LGWServer -l 50 >& /dev/null < /dev/null &')
    run('sleep 1s')
    # Run LGWServer in background and limit heap size
    run('nohup ' + os.path.join(DEST_PATH, 'LGWServer') + ' +RTS -N -M40m -RTS >& /dev/null < /dev/null &')
    run('sleep 1s')
