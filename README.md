# Scheme environment

This will be a simple Scheme implementation switcher.

# How to install

```
$ curl https://raw.githubusercontent.com/ktakashi/scheme-env/master/bin/install.sh | sh
```

After the installation you need to add the following to your shell
resource file:

```
PATH=~/.scheme-env/bin:$PATH
```

# How to use

The basic command id `scheme-env` if you want to run the default
implementation then use the following comment

```
$ scheme-env run
```

