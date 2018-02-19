#!/bin/bash

# installing host Scheme implementation (latest Sagittarius) and scheme env
SCHEME_ENV_INSTALL_PACKAGE=${SCHEME_ENV_INSTALL_PACKAGE:-"yes"}

stop()
{
    while true; do
	read -rep $'\nAre you sure you want to stop? (y/n)' yn
	case $yn in
	    [Yy]*) exit 1;;
	    [Nn]*) break ;;
	    *) echo 'Please enter (y/n)';;
	esac
    done
}
trap 'stop' SIGINT

SCHEME_ENV_HOME=~/.scheme-env
mkdir -p ${SCHEME_ENV_HOME}/bin
mkdir -p ${SCHEME_ENV_HOME}/scripts
mkdir -p ${SCHEME_ENV_HOME}/implementations
mkdir -p ${SCHEME_ENV_HOME}/work

cd ${SCHEME_ENV_HOME}

# directories are created. so install host implementation

ubuntu_package()
{
    name=$1
    echo -n "Checking package '${name}' ... "
    installed=`dpkg --get-selections | grep ${name}`
    if [ $? -ne 0 ]; then
	echo "Installing package ${name}"
	sudo apt-get install ${name}
    else
	echo 'ok'
    fi
}

linux_command()
{
    LINUX_DISTRIBUTION=`lsb_release -i`
    case ${LINUX_DISTRIBUTION} in
	*Ubuntu*)
	    PACKAGE_COMMAND='ubuntu_package'
	    ;;
	*)
	    echo "${LINUX_DISTRIBUTION} is not supported yet"
	    exit 1
	    ;;
    esac
    
}    

install_package()
{
    for name in $@
    do
	${PACKAGE_COMMAND} ${name}
    done
}

init_commands() {
    PLATFORM_OS=`uname -s`
    case ${PLATFORM_OS} in
	Linux)
	    linux_command
	    ;;
	*)
	    echo "************************WARNING*************************"
	    echo "* Package manager of '${PLATFORM_OS}' is not supported.*"
	    echo "* So required package must manually be installed.      *"
	    echo "********************************************************"
	    SCHEME_ENV_INSTALL_PACKAGE=no
	    ;;
    esac
}

usage()
{
    echo <<EOF 1>&2
install.sh [-l]
  -l, Using local files instead of Github (for developers)
EOF
    exit 1
}    

while getopts "l:" o; do
    case "${o}" in
	l)
	    USE_LOCAL=yes
	    LOCAL_REPOSITORY=${OPTARG}
	    ;;
	*)
	    usage
	    ;;
    esac
done
shift $((OPTIND-1))

init_commands
echo "Should install packages ... ${SCHEME_ENV_INSTALL_PACKAGE}"
case ${SCHEME_ENV_INSTALL_PACKAGE} in
    1|yes)
	# TODO absorb the different names
	install_package gcc g++ make curl cmake libgc-dev libffi-dev zlib1g-dev
	;;
esac

check_downloader()
{
    # curl first, then wget
    echo -n "Checking curl ... "
    c=`command -v curl`
    if [ $? -eq 0 ]; then
	echo "yes"
	CURL="curl -sL -o"
    else
	echo "no"
	echo -n "Checking wget ... "
	c=`command -v wget`
	if [ $? -eq 0 ]; then
	    echo "yes"
	    CURL="wget -q -O"
	else
	    echo "no"
	    echo "curl or wget is required"
	    exit 1
	fi
    fi
}
check_downloader

REPOSITORY_URL=https://bitbucket.org/ktakashi/sagittarius-scheme/downloads

echo -n "Downloading latest-version.txt ... "
${CURL} work/version ${REPOSITORY_URL}/latest-version.txt
echo "done!"

VERSION=`cat work/version`
echo "Host Sagittarius version ... ${VERSION}"

echo -n "Downloading Sagittarius ${VERSION} ... "
LATEST_TAR=sagittarius-${VERSION}.tar.gz
${CURL} work/${LATEST_TAR} ${REPOSITORY_URL}/${LATEST_TAR}
echo "done!"

cd work
echo -n "Expanding Sagittarius $VERSION ... "
tar xf ${LATEST_TAR}
echo "done!"

SAGITTARIUS_DIR=$SCHEME_ENV_HOME/implementations/sagittarius
INSTALL_DIR=${SAGITTARIUS_DIR}/${VERSION}

cd sagittarius-${VERSION}
echo -n "Pre-build process ... "
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} . >  build.log 2>&1
echo "done!"

progress()
{
    first=1
    indicator='|'
    msg=$1
    while read line; do
	progres_regex='\[(.*)%\].*'
	install_regex='^--.*:.*'
	if [[ ${line} =~ ${progres_regex} ]]; then
	    percent=${BASH_REMATCH[1]}
	    let count=percent/10
	    echo -ne "${msg} ... #"
	    i=0
	    while [ $i -lt $count ]; do
		echo -ne '###'
		let i++
	    done
	    while [ $i -lt 10 ]; do
		echo -ne '   '
		let i++
	    done
	    echo -ne "  (${percent}%)\r"	    
	elif [[ ${line} =~ ${install_regex} ]]; then
	    if [ ${first} -eq 1 ]; then
		echo -ne '\n'
		first=0
	    fi
	    echo -ne "Installing files ... ${indicator}\r"
	    if [ x"${indicator}" == x"|" ]; then
		indicator='-'
	    else
		indicator='|'
	    fi
	fi
    done
    if [ ${first} -eq 1 ]; then
	echo -ne "\n"
    else
	echo "Installing files ... done!"
    fi
}
make -j8 2>&1     | tee -a build.log | progress "Building host Sagittarius  "
make install 2>&1 | tee -a build.log | progress "Installing host Sagittarius"

HOST_SCHEME=`pwd`
# back to work
cd ..
case `uname -s` in
    *CYGWIN*)
	make rebase > /dev/null 2>&1
	echo "****************************************************"
	echo "*   PLEASE EXECUTE /bin/rebaseall -v -T dlls.txt   *"
	echo "****************************************************"
	echo "Command on Ash (or Dash)"
	echo "cd ${HOST_SCHEME}; /bin/rebaseall -v -T dlls.txt"
	echo "Reinstall command"
	echo "cd ${HOST_SCHEME}; make install"
    ;;
    *)
	# remove work
	rm -rf *
esac

remove_if_exists()
{
    for file in "$@"
    do
	if [ -e ${file} ]; then
	    rm -rf ${file}
	fi
    done
}

remove_if_exists ${INSTALL_DIR}/sagittarius ${SCHEME_ENV_HOME}/bin/sagittarius

echo -n "Creating symblic links ... "
cat << EOF > ${INSTALL_DIR}/sagittarius
#!/bin/sh
LD_LIBRARY_PATH=${INSTALL_DIR}/lib ${INSTALL_DIR}/bin/sagittarius "\$@"
EOF

chmod +x ${INSTALL_DIR}/sagittarius

remove_if_exists ${SCHEME_ENV_HOME}/bin/default \
		 ${SCHEME_ENV_HOME}/bin/host-scheme

LINK_NAME=${SCHEME_ENV_HOME}/bin/sagittarius@${VERSION}

ln -s ${INSTALL_DIR}/sagittarius ${LINK_NAME}
ln -s ${LINK_NAME} ${SCHEME_ENV_HOME}/bin/default
ln -s ${LINK_NAME} ${SCHEME_ENV_HOME}/bin/host-scheme
echo "done!"

cd ${SCHEME_ENV_HOME}

echo -n "Installing execution script ... "
cat <<EOF > bin/scheme-env
#!/bin/sh

case \$1 in
    run)
	shift
	exec ${SCHEME_ENV_HOME}/bin/default "\$@"
	;;
esac

exec env SCHEME_ENV_HOME=${SCHEME_ENV_HOME} \
     ${SCHEME_ENV_HOME}/bin/host-scheme \
     ${SCHEME_ENV_HOME}/bin/scheme-env.scm "\$@"
EOF

chmod +x bin/scheme-env
echo "done!"

case ${USE_LOCAL} in
    yes)
	cp ${LOCAL_REPOSITORY}/bin/scheme-env.scm bin/scheme-env.scm 
	;;
    *)
	${CURL} bin/scheme-env.scm https://raw.githubusercontent.com/ktakashi/scheme-env/master/bin/scheme-env.scm
	;;
esac

cat <<EOF
Host Scheme system is installed. Please add the following to your resource file:
PATH=~/.scheme-env/bin:\${PATH}

For help run the following command:
\$ scheme-env help
EOF
