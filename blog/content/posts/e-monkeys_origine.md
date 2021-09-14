+++
title = "e-monkeys.tech à l'origine ..."
slug = "origine"
+++

# A nerd adventure : e-monkeys.tech site building

## Architecture

- Choix d'une infrastructure multiservices monohost portée par un VPS. Utilisation d'un reverse proxy apache.

### Evolutions possibles 

Sur le même hôte : 

- architecture dockersisée multicontainers monohost. Le reverse proxy apache est conservé.

Si passage sur un cluster managé : 

- containers de microservices portés par un cluster Kubernetes à 3 noeuds. Un reverse proxy Traefik
serait utilisé.

Analyses des traces : 

- Mettre en place Jaeger (si migration des applications vers des containers)

## Prérequis
  
- Acheter un nom de domaine
- Activer DNSSEC lors de l'achat du nom de domaine
- Disposer d'un serveur (mutualisé,VPS ou bare-metal)

## Configurer les sous-domaines de sa zone DNS

Faire la configuration relative au sous-domaines qui seront activés comme sites joignables au travers du reverse proxy.

## Hardenning

```bash
sudo su -
apt-get install fail2ban
```

Une fois le paquet installé, il faut modifier le fichier de configuration de ce dernier pour l’adapter à la vôtre. Avant toute modification, il est recommandé d’effectuer une sauvegarde du fichier de configuration :

```bash
cp -a /etc/fail2ban/jail.conf /etc/fail2ban/jail.conf.backup
```

Apportez ensuite vos modifications au fichier :

```bash
vim /etc/fail2ban/jail.conf
```

Une fois ces modifications terminées, redémarrez le service :

```bash
/etc/init.d/fail2ban restart
```

## Installation d'un VPS Debian 10 chez OVH

Se référer à la documentation officielle.

### Configuration des locales

```bash
dpkg-reconfigure locales
locale-gen
#Config NTP
```

### Connexion au VPS

```bash
ssh -i pbackz@${HOST_IP} -p 2022
```

### Récupérer ou envoyer des données depuis le VPS 

```bash
scp -P 2022 pbackz@${HOST_IP}:/usr/local/apps/blog/blog.e-monkeys.tech/config.toml .
scp -P 2022 'C:\Users\pbaconnier\Downloads\AgensGraph_v2.1.2_linux_CE.tar.gz'  pbackz@${HOST_IP}:/usr/local/apps/sources/
```

### Suppression du user par défaut et création d'un user sudo

Detailed in https://docs.ovh.com/fr/vps/conseils-securisation-vps/

### Installation d'une instance PostgreSQL 12 

```bash
sudo su -
cat << EOF > /etc/apt/sources.list.d/pgdg.list
deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main
EOF
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
apt-get update
apt-get install postgresql-12
```

## Création du site blog.e-monkeys.tech step-by-step

###  Hugo, générateur de site statique

#### Installation et configuration

```bash
sudo su -
adduser usr_blog
usermod -a -G sudo usr_blog
mkdir -p /usr/local/apps/blog/
wget https://github.com/gohugoio/hugo/releases/download/v0.72.0/hugo_0.72.0_Linux-64bit.deb
dpkg -i hugo_0.72.0_Linux-64bit.deb
rm -f hugo_0.72.0_Linux-64bit.deb
chmod -R 0770 /usr/local/apps/blog/
chown -R usr_blog: /usr/local/apps/blog
su - usr_blog
hugo create new site blog.e-monkeys.tech
cd /usr/local/apps/blog/blog.e-monkeys.tech
git init
git clone https://github.com/your-identity/hugo-theme-dimension.git themes/dimension
git submodule add https://github.com/your-identity/hugo-theme-dimension.git themes/dimension
echo 'theme = "dimension"' >> config.toml
vim config.toml
hugo server -D
hugo server --bind=$PUBLIC_VPS_IP_ADDRESS --baseURL=http://127.0.0.1:1313 &
```

#### Création d'un service linux Systemd

```bash
cat << EOF > /usr/lib/systemd/system/hugo-blog.service
[Unit]
Description=Hugo Blog site for e-monkeys.tech
After=syslog.target
After=network.target
[Service]
#RestartSec=2s
Type=simple
User=usr_blog
Group=usr_blog
WorkingDirectory=/usr/local/apps/blog/blog.e-monkeys.tech
ExecStart=/usr/local/bin/hugo server --bind=${HOST_IP} --baseURL=http://127.0.0.1:1313
Restart=always
Environment=USER=usr_blog HOME=/home/usr_blog
[Install]
WantedBy=multi-user.target
EOF

systemctl enable hugo-blog.service
```

### Ouverture du service

#### Configuration du vhost Apache

```bash
sudo su -
apt-get install -y apache2
```

Note from debian.org::
__On Debian systems, Apache Virtual Hosts configuration files are located in */etc/apache2/sites-available* directory and can be enabled by creating symbolic links to the */etc/apache2/sites-enabled* directory.__

```bash
cat << EOF > /etc/apache2/sites-available/blog.e-monkeys.tech.conf
<VirtualHost *:80>
    ServerName blog.e-monkeys.tech
    ServerAlias www.blog.e-monkeys.tech

    ProxyPreserveHost On

    ProxyPass / http://127.0.0.1:1313/
    ProxyPassReverse / http://127.0.0.1:1313/

    ErrorLog ${APACHE_LOG_DIR}/blog.e-monkeys.tech-error.log
    CustomLog ${APACHE_LOG_DIR}/blog.e-monkeys.tech-access.log combined
</VirtualHost>
EOF
```

#### Première configuration des modules Apache

```bash
a2enmod proxy
a2enmod proxy_http
a2enmod proxy_balancer
a2enmod lbmethod_byrequests

a2ensite blog.e-monkeys.tech
ln -s /etc/apache2/sites-available/blog.e-monkeys.tech.conf /etc/apache2/sites-enabled/

systemctl restart apache2
```

#### Tests de bon fonctionnement

```bash
curl -X GET https://blog.e-monkeys.tech
```

OR 

```nu
# Nushell
fetch https://blog.e-monkeys.tech | from html | lines
```

## Création du site docs.e-monkeys.tech

### Antora : la plateforme documentaire pour les writers asciidoc

### Installation et configuration

```bash
sudo su -
adduser usr_node
usermod -a -G sudo usr_node
mkdir -p /usr/local/apps/docs/antora/docs.e-monkeys.tech
chmod -R 0770 /usr/local/apps/docs/
chown -R usr_node: /usr/local/apps/docs
su - usr_node

sudo curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash || wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | sudo bash
```

Copy this in *_~/.bash_profile_* or *_~/.bashrc_* :

```bash
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Verify
command -v nvm
```

#### Installation de NodeJS

```bash
nvm install --lts
nvm alias default 12
```

#### Installation de la CLI Antora

```bash
npm i -g @antora/cli@2.2 @antora/site-generator-default@2.2
antora version
```

#### Déploiement demo

```bash
cd /usr/local/apps/docs/antora/docs.e-monkeys.tech

cat << EOF > antora-playbook_docs.e-monkeys.tech.yaml
site:
  title: Antora Demo Site
  # the 404 page and sitemap files only get generated when the url property is set
  url: https://antora.gitlab.io/demo/docs-site
  start_page: component-b::index.adoc
content:
  sources:
  # embedding empty credentials in the URL disables the Edit this Page link for any page created from this repository
  - url: https://@gitlab.com/antora/demo/demo-component-a.git
    branches: master
  - url: https://gitlab.com/antora/demo/demo-component-b.git
    branches: [v2.0, v1.0]
    start_path: docs
ui:
  bundle:
    url: https://gitlab.com/antora/antora-ui-default/-/jobs/artifacts/master/raw/build/ui-bundle.zip?job=bundle-stable
    snapshot: true
EOF

antora --stacktrace antora-playbook_docs.e-monkeys.tech.yaml

cd build/site/

```nu
# Nushell
open index.html
```

#### Installation des binaires du serveur d'application NodeJS

Install the node-srv package globally using npm:

```bash
npm i -g node-srv
node-srv -r build/site
```

#### Création d'un service linux Systemd

```bash
cat << EOF > /usr/lib/systemd/system/antora.service
[Unit]
Description=Antora
After=syslog.target
After=network.target
[Service]
#RestartSec=2s
Type=simple
User=usr_node
Group=usr_node
WorkingDirectory=/usr/local/apps/docs/antora
ExecStart=/usr/local/bin/node-srv -r build/site
Restart=always
Environment=USER=usr_node HOME=/home/usr_node
[Install]
WantedBy=multi-user.target
EOF

systemctl enable antora.service
```

### Ouverture du service

```bash
systemctl start antora.service
```

#### Configuration du vhost Apache

Identique à la configuration de la section précédente hormis le port et l'url correspondante.

#### Tests de bon fonctionnement

```bash
curl -X GET https://docs.e-monkeys.tech
```

OR 

```nu
# Nushell
fetch https://docs.e-monkeys.tech | from html | lines
```

## Création du site git.e-monkeys.tech

### Création de la database applicative

#### Encryption

Dans le fichier postgresql.conf vérfier la directive suivante: +
_password_encryption = scram-sha-256_

Modifier également la directive _listen_addresses_ et _port_

Restarter l'instance postgres pour prendre en compte les nouveaux paramètres

```bash
#su -c "psql" - postgres
su - postgres
$(which pg_ctl) restart -D $PGDATA
```

#### Création d'un user applicatif

```sql
CREATE ROLE gitea WITH LOGIN PASSWORD 'gitea';
```

#### Configuration de l'encoding et du charset

```sql
CREATE DATABASE db_gitea WITH OWNER gitea TEMPLATE template0 ENCODING UTF8 LC_COLLATE 'fr_FR.UTF-8' LC_CTYPE 'fr_FR.UTF-8';
```

#### Configuration des accès *pg_hba.conf*

.Local Database
```
local    db_gitea    gitea    scram-sha-256
```

.Remote Database
```
host    db_gitea    gitea    12.34.56.78/32    scram-sha-256
```

#### Test de la connexion SQL

.Local Database
```bash
psql -U gitea -d db_gitea
```

.Remote Database:
```bash
psql "postgres://gitea@${POSTGRES_HOST_IP}/giteadb"
```

### Gitea : plateforme git collaborative open source

#### Installation du binaire

```bash
sudo su -
wget -O gitea https://dl.gitea.io/gitea/1.11.0/gitea-1.11.0-linux-amd64 && \
chmod +x gitea && \
cp -a gitea /usr/local/bin/gitea && \
source ~/.bashrc && rm gitea
```

#### Création du propriétaire de service Gitea

```bash
sudo su -
adduser \
   --system \
   --shell /bin/bash \
   --home /home/usr_git \
   usr_git && \
addgroup git && \
usermod -a -G usr_git usr_git
chown -R usr_git: /home/usr_git
```

#### Créer l'arborescence requise

```bash
mkdir -p /usr/local/apps/git/gitea/{custom,data,log}
chown -R usr_git: /usr/local/apps/git/gitea
chmod -R 750 /usr/local/apps/git/gitea
mkdir /etc/gitea
chown root:usr_git /etc/gitea
chmod 770 /etc/gitea
```

NOTE:  */etc/gitea* est un répertoire temporaire avec des droits d'écriture pour l'utilisateur git afin que le programme d'installation Web puisse écrire le fichier de configuration. Une fois l'installation terminée, il est recommandé de définir les droits en lecture seule.

```bash
chmod 750 /etc/gitea
chmod 640 /etc/gitea/app.ini
```

#### Running Gitea

WARNING: Le user 'usr_git' doit être configuré dans le fichier ini pour une installation en mode service +

```bash
grep -r RUN /usr/local/apps/git/gitea/custom/conf/app.ini
RUN_USER = usr_git
```

#### Création d'un service Linux Systemd

```bash
cat << EOF > /usr/lib/systemd/system/gitea.service
[Unit]
Description=Gitea (Git with a cup of tea)
After=syslog.target
After=network.target
Requires=postgresql.service
[Service]
#LimitMEMLOCK=infinity
#LimitNOFILE=65535
RestartSec=2s
Type=simple
User=usr_git
Group=usr_git
WorkingDirectory=/usr/local/apps/git/gitea
ExecStart=/usr/local/bin/gitea web --config /usr/local/apps/git/gitea/custom/conf/app.ini
Restart=always
Environment=USER=usr_git HOME=/home/usr_git GITEA_WORK_DIR=/usr/local/apps/git/gitea
[Install]
WantedBy=multi-user.target
EOF
```

#### Configuration du logging

Dans le fichier _/usr/local/apps/git/gitea/custom/conf/app.ini_
```toml
[log]
MODE      = file
LEVEL     = info
ROOT_PATH = /usr/local/apps/git/gitea/logs
```

#### Activation du service

```bash
systemctl enable gitea
systemctl start gitea

systemctl enable gitea --now
```

#### Démarrage depuis la ligne de commande

```bash
GITEA_WORK_DIR=/usr/local/apps/git/gitea/ /usr/local/bin/gitea web -c /etc/gitea/app.ini
```

#### Upgrade vers une nouvelle version

Vous pouvez mettre à jour Gitea vers une nouvelle version en arrêtant le service Gitea, en remplaçant le binaire dans */usr/local/bin/gitea* et en redémarrant l'instance.

Il est recommandé d'effectuer un backup.

.Backup
```bash
su git
cd /usr/local/apps/git/gitea
gitea dump -c /etc/gitea/app.ini
```

Dans l'archive générée gitea-dump-*.zip on devrait voir ce qui suit:

* *app.ini* - Optional copy of configuration file if originally stored outside of the default custom/ directory
* *custom/* - All config or customization files in custom/.
* *data/* - Data directory in , except sessions if you are using file session. This directory includes attachments, avatars, lfs, indexers, sqlite file if you are using sqlite.
* *gitea-db.sql* - SQL dump of database
* *gitea-repo.zip* - Complete copy of the repository directory.
    log/ - Various logs. They are not needed for a recovery or migration.

#### Démarrage de Gitea sur un autre port

```bash
gitea web -p $PORT
```

#### Support Asciidoc Gitea 

```bash
sudo su -
adduser usr_rb
addgroup apps
usermod -a -G sudo usr_rb
usermod -a -G apps usr_rb usr_node
su - usr_rb 
apt-get install -y gcc-c++ patch readline readline-devel zlib zlib-devel
apt-get install -y libyaml-devel libffi-devel openssl-devel make
apt-get install -y bzip2 autoconf automake libtool bison iconv-devel sqlite-devel
curl -L get.rvm.io | bash -s stable
curl -sSL https://rvm.io/mpapis.asc | gpg2 --import -
curl -sSL https://rvm.io/pkuczynski.asc | gpg2 --import -
source /etc/profile.d/rvm.sh
rvm install 2.5.0
rvm use 2.5.0 --default; ruby --version
sudo chown -R usr_rb: /home/usr_rb/.gem
rvm docs generate-ri
gem install asciidoctor
export GEM_PATH=$GEM_PATH:/usr/local/rvm/gems/ruby-2.5.0/
gem sources; gem env
gem sources --add https://rubygems.org/
```

Ajouter ce qui suit au fichier _app.ini_:
```toml
[markup.asciidoc]
ENABLED = true
; List of file extensions that should be rendered by an external command
FILE_EXTENSIONS = .adoc,.asciidoc
; ; External command to render all matching extensions
RENDER_COMMAND = "asciidoctor --out-file=- -"
; ; Input is not a standard input but a file
; IS_INPUT_FILE = false
```

### Ouverture du service

#### Configuration du vhost Apache

Identique à la configuration de la section précédente hormis le port et l'url correspondante.

#### Tests de bon fonctionnement

Identique à la configuration de la section précédente hormis le port et l'url correspondante.