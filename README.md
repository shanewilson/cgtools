# Conventional Git Tools

Git Hooks and Changelog Generator based on Conventional Changelog

[![Build Status](https://travis-ci.org/shanewilson/cgtools.svg)](https://travis-ci.org/shanewilson/cgtools)

- [Install](#install)
- [Git Hooks](#git-hooks)
- [Change Log](#change-log)

## Install

To install **cgtools** simply drop the bin into your project directory, run `./cgtools install -v`.

```
$ chmod u+x cgtools
$ ./cgtools install -v
Checking dependencies...
OK
Generating Git Hooks...
OK
```

### Bash Completion

**cgtools** can generate *Bash Completion* with the flag `-b` or `--bash-completion`.

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

You can test it out by sourcing the file: `source cgtools-completion.sh` or adding it to your `.bashrc`.
Note: If you use `zsh` you might need to add the following to your `.zshrc`:

```
autoload bashcompinit
bashcompinit
```

## Git Hooks

### Prepare Commit Message

## Change Log