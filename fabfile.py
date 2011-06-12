from fabric.api import *
import os, os.path
import fabric.contrib.project as project

PROD = 'root@eugenkiss.com:223'
DEST_PATH = '/var/www/loopgotowhile/'
ROOT_PATH = os.path.abspath(os.path.dirname(__file__))
DEPLOY_PATH = os.path.join(ROOT_PATH, 'deploy')
TEST_PATH = os.path.join(ROOT_PATH, 'test')
NGINX_CONF_PATH = '/etc/nginx/sites-available/loopgotowhile'

@hosts(PROD)
def update_nginx_conf():
    put('nginx.conf', NGINX_CONF_PATH)
    run('/etc/init.d/nginx reload')

def backup():
    local('git push -f ~/Dropbox/backup/loopgotowhile-site.git')

def compile():
    local('cabal configure')
    local('cabal build')

def copy():
    # make sure the directory is there!
    local('mkdir -p ' + DEPLOY_PATH)
    local('cp ' + os.path.join(ROOT_PATH, 'index.html') + ' ' +
                  os.path.join(DEPLOY_PATH, 'index.html'))
    local('cp ' + os.path.join(ROOT_PATH, 'style.css') + ' ' +
                  os.path.join(DEPLOY_PATH, 'style.css'))
    local('cp ' + os.path.join(ROOT_PATH, 'dist/build/LGWServer/LGWServer') + ' ' +
                  os.path.join(DEPLOY_PATH, 'LGWServer'))

def test():
    with settings(warn_only=True):
        local('killall -9 -v LGWServer ')
    compile()
    local('mkdir -p ' + TEST_PATH)
    local('cp ' + os.path.join(ROOT_PATH, 'index.html') + ' ' +
                  os.path.join(TEST_PATH, 'index.html'))
    local('cp ' + os.path.join(ROOT_PATH, 'style.css') + ' ' +
                  os.path.join(TEST_PATH, 'style.css'))
    local('cp ' + os.path.join(ROOT_PATH, 'dist/build/LGWServer/LGWServer') + ' ' +
                  os.path.join(TEST_PATH, 'LGWServer'))
    local(os.path.join(TEST_PATH, 'LGWServer'))


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
    compile()
    copy()
    project.rsync_project(
        remote_dir=DEST_PATH,
        local_dir=DEPLOY_PATH.rstrip('/') + '/',
        delete=True
    )
    update_nginx_conf()
    # Limit cpu usage (cpulimit must be installed on remote)
    run('nohup cpulimit -e LGWServer -l 20 >& /dev/null < /dev/null &')
    # Run LGWServer in background and limit heap size
    run('nohup ' + os.path.join(DEST_PATH, 'LGWServer') + ' +RTS M30m -RTS >& /dev/null < /dev/null &')
