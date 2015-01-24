# Conventional Git Tools

Git Hooks and Changelog Generator based on Conventional Changelog

[![Build Status](https://travis-ci.org/shanewilson/cgtools.svg)](https://travis-ci.org/shanewilson/cgtools)

- [Why](#why)
- [What](#what)
- [Install](#install)
- [Git Hooks](#git-hooks)
- [Change Log](#change-log)

## Why
... Standardize git commits to run tools against later ...

## What
... A single file that can be added to your project that will install git hooks to standardize git commits and generate change logs automagically ...

## Install

To install **cgtools** simply drop the bin into your project directory and run `./cgtools install -v`.

```
$ chmod u+x cgtools
$ ./cgtools install -v
Checking dependencies...
OK
Generating Git Hooks...
OK
```

### Bash Completion

**cgtools** can generate *Bash Completion* with the flag `-b` or `--bash-completion`. You can test it out by sourcing the file: `source cgtools-completion.sh` or adding it to your `.bashrc`.

```
$ chmod u+x cgtools
$ ./cgtools install -b -v
Checking dependencies...
OK
Generating Git Hooks...
OK
Generating Bash Completion...
OK
$ source cgtools-completion.sh
$ ./cgtools <tab>
--help     --verbose  --version  -h         -v         install    logs       validate
```

Note: If you use `zsh` you might need to add the following to your `.zshrc`:

```
autoload bashcompinit
bashcompinit
```

### Source

To build from source you will need to install Haskell

```
$ git clone https://github.com/shanewilson/cgtools.git
$ cabal sandbox init
$ cabal install --enable-tests --enable-benchmarks --force-reinstalls
$ .cabal-sandbox/bin/cgtools --help
```

## Git Hooks

### Prepare Commit Message

Talk about this next time

## Change Log

TODO