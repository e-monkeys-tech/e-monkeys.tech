# Migrate a Tomcat application 

```bash
# Globals
SRC=${1}
TARGET=${2}
JAVA_HOME=${3}
APP_HOME=${4}
TOMCAT_HOME=${5}
ENV=${6}
SSL_HOME=${7}

function init_temp() {
  mkdir -p /tmp/{java,ssl,tomcat,app}
  TMP_APP=/tmp/app
  TMP_JAVA=/tmp/java
  TMP_SSL=/tmp/ssl
  TMP_TOMCAT=/tmp/tomcat
}

# Test de la présence des paramètres
function start_usage() {
  if !([ -z ${6+x} ]); then 
    echo
  else 
    echo -e "Ce script prend en arguments positionnels les paramètres suivants : 
    <SRC> <TARGET> <JAVA_HOME> <APP_HOME> <TOMCAT_HOME> <ENV> <[SSL_HOME]>
    specimen : #./self.sh s-coca-q-1 s-coca-p-1 /usr/java /usr/local/apps/tomcat /home/tomcat pre /usr/ssl"
    exit 
  fi
}

function src_copy() {
  ssh root@${TARGET} 'mkdir ${APP_HOME} ${JAVA_HOME} ${SSL_HOME}; useradd tomcat'
  scp -r  root@${SRC}:${APP_HOME}/* ${TMP_APP}/
  scp -r  ${TMP_APP}/* root@:${TARGET}/
  scp -r  root@${SRC}:${JAVA_HOME}/* ${TMP_JAVA}/
  scp -r  ${TMP_JAVA}/*  root@${TARGET}:${JAVA_HOME}/
  scp -r  root@${SRC}:${TOMCAT_HOME}/* ${TMP_TOMCAT}/
  scp -r  ${TMP_TOMCAT}/* root@${TARGET}:${TOMCAT_HOME}/
}

function prepare_tomcat() {
  ssh root@${TARGET} "export TOMCAT_USER=tomcat ; \
  export JAVA_HOME=${JAVA_HOME} ; \
  cd ${APP_HOME} ; \
  rm -rf logs/* temp/* webapps/* ; \
  chown -R tomcat: ${APP_HOME} ${TOMCAT_HOME}"
}

function init_service() {
  ssh root@${TARGET} "cat << EOF > /usr/lib/systemd/system/tomcat.service
  [Unit]
  Description=Apache Tomcat Web Application Container
  After=syslog.target network.target
  
  [Service]
  Type=forking
  
  Environment=JAVA_HOME=${JAVA_HOME}
  Environment=CATALINA_PID=/usr/local/apps/tomcat/temp/tomcat.pid
  Environment=CATALINA_HOME=${APP_HOME}
  Environment=CATALINA_BASE=${APP_HOME}
  Environment='JAVA_OPTS=-Djava.awt.headless=true -Djava.security.egd=file:/dev/./urandom'
  Environment='ENV_TYPE=${ENV}'
  
  ExecStart=/usr/local/apps/tomcat/bin/startup.sh
  ExecStop=/bin/kill -15 $MAINPID
  
  User=tomcat
  Group=tomcat
  
  [Install]
  WantedBy=multi-user.target
  EOF"
  fi
  
  ssh root@${TARGET} "systemctl daemon-reload && systemctl enable tomcat.service"
}

function is_ok() {
  if [ $? -ne 0 ]; then
    exit
  else
    continue
  fi 
}

# 
function clean() {
  rm -rf /tmp/{java,ssl,tomcat,app}
  shopt -u $TMP_APP $TMP_JAVA $TMP_SSL $TMP_TOMCAT
}

echo -e "Vérification des paramètres de démarrage ..."
start_usage
is_ok

echo -e "Initialisation des fichiers temporaires ..."
init_temp
is_ok

echo -e "Copie des sources ..."
src_copy
is_ok

echo -e "Préparation du tomcat sur la cible distante ..."
prepare_tomcat
is_ok

echo -e "Initialisation du service tomcat ..."
init_service
is_ok

echo -e "Nettoyage des répertoires et des variables temporaires ..."
clean
is_ok
```