# Scheme environment

This will be a simple Scheme implementation switcher.

# How to install

```
$ curl https://raw.githubusercontent.com/ktakashi/scheme-env/master/bin/install.sh | bash
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

# Installing implementations

To install implementations, you can run the following command

```
$ scheme-env install implementation
```
Currently the followings are the supported implementation 

- Chibi Scheme (chibi-scheme)
- Sagittarius Scheme (sagittarius)
- Gauche (gauche)
- Foment (foment)
- Chicken (chicken-scheme)
- Larceny (larceny)
- Chez Scheme (chez)

You can also specify the version by adding `@` and version number.
For example:

```
$ scheme-env install sagittarius@0.8.9
```

## For Chicken Scheme

The installation process of Chicken Scheme creates 3 aliases, `csi`, `csc` and
`chicken-scheme` followed by `@{version}` suffix. These are the standard
entry points for Chicken Scheme.

## macOS

On macOS, there are following prerequisites:

- Installing Xcode (For Chicken Scheme)
- Installing [XQuartz](https://www.xquartz.org/) (For Chez Scheme)

# Switch implementations

To swich default implementation, you can run the following command

```
$ scheme-env switch implementation
```
If this command isn't run, then `sagitarius` is set to default.

# Acknowledgement

* Shiro Kawai for the installation script of Gauche
