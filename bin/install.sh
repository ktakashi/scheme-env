#!/bin/bash

# installing host Scheme implementation (latest Sagittarius) and scheme env
SCHEME_ENV_HOME=~/.scheme-env
mkdir -p $SCHEME_ENV_HOME/bin
mkdir -p $SCHEME_ENV_HOME/scripts
mkdir -p $SCHEME_ENV_HOME/implementations
mkdir -p $SCHEME_ENV_HOME/work

cd $SCHEME_ENV_HOME

# directories are created. so install host implementation

ubuntu_package()
{
    name=$1
    echo -n "Checking package '$name' ... "
    installed=`dpkg --get-selections | grep $name`
    if [ $? -ne 0 ]; then
	echo "Installing package $name"
	sudo apt-get install $name
    else
	echo 'ok'
    fi
}

linux_command()
{
    LINUX_DISTRIBUTION=`lsb_release -i`
    case $LINUX_DISTRIBUTION in
	*Ubuntu*)
	    PACKAGE_COMMAND='ubuntu_package'
	    ;;
	*)
	    echo "$LINUX_DISTRIBUTION is not supported yet"
	    exit 1
	    ;;
    esac
    
}    

install_package()
{
    for name in $@
    do
	$PACKAGE_COMMAND $name
    done
}

init_commands() {
    PLATFORM_OS=`uname -s`
    case $PLATFORM_OS in
	Linux)
	    linux_command
	    ;;
	*)
	    echo "$PLATFORM_OS is not supported"
	    exit 1
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

# TODO absorb the different names
install_package gcc g++ make curl cmake libgc-dev libffi-dev zlib1g-dev

REPOSITORY_URL=https://bitbucket.org/ktakashi/sagittarius-scheme/downloads

curl -L -o work/version $REPOSITORY_URL/latest-version.txt
VERSION=`cat work/version`
LATEST_TAR=sagittarius-$VERSION.tar.gz
curl -L -o work/$LATEST_TAR $REPOSITORY_URL/$LATEST_TAR

cd work
tar xf $LATEST_TAR
SAGITTARIUS_DIR=$SCHEME_ENV_HOME/implementations/sagittarius
INSTALL_DIR=$SAGITTARIUS_DIR/$VERSION

cd sagittarius-$VERSION
cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR .
make -j8
make install

# back to work
cd ..
rm -rf *

if [ -e $INSTALL_DIR/sagittarius ]; then
    rm -rf $INSTALL_DIR/sagittarius
fi
if [ -e $SCHEME_ENV_HOME/bin/sagittarius ]; then
    rm -rf $SCHEME_ENV_HOME/bin/sagittarius
fi

cat << EOF > $INSTALL_DIR/sagittarius
#!/bin/sh
LD_LIBRARY_PATH=$INSTALL_DIR/lib $INSTALL_DIR/bin/sagittarius "\$@"
EOF

chmod +x $INSTALL_DIR/sagittarius

ln -s $INSTALL_DIR/sagittarius $SCHEME_ENV_HOME/bin/sagittarius
ln -s $INSTALL_DIR/sagittarius $SCHEME_ENV_HOME/bin/default
ln -s $INSTALL_DIR/sagittarius $SCHEME_ENV_HOME/bin/host-scheme

cd $SCHEME_ENV_HOME

cat <<EOF > bin/scheme-env
#!/bin/bash

case \$1 in
    run)
	shift
	exec $SCHEME_ENV_HOME/bin/default "\$@"
	;;
esac

exec env SCHEME_ENV_HOME=$SCHEME_ENV_HOME $SCHEME_ENV_HOME/bin/host-scheme $SCHEME_ENV_HOME/bin/scheme-env.scm "\$@"
EOF

chmod +x bin/scheme-env
case $USE_LOCAL in
    yes)
	cp $LOCAL_REPOSITORY/bin/scheme-env.scm bin/scheme-env.scm 
	;;
    *)
	curl -L -o bin/scheme-env.scm https://raw.githubusercontent.com/ktakashi/scheme-env/master/bin/scheme-env.scm
	;;
esac

echo <<EOF
Host Scheme system is installed. Please add the following to your resource file:
PATH=~/.scheme-env/bin:\$PATH

For help run the following command:
\$ scheme-env help
EOF
