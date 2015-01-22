# Conventional Git Tools

Git Hooks and Changelog Generator based on Conventional Changelog

[![Build Status](https://travis-ci.org/shanewilson/cgtools.svg)](https://travis-ci.org/shanewilson/cgtools)

- [Install](#install)
- [Git Hooks](#git-hooks)
- [Change Log](#change-log)

## Setup

To install **cgtools** simply drop the bin into your project directory, run `./cgtools install` and follow the prompts.

### Bash Completion

**cgtools** comes with *Bash Completion*. You can test it out by sourcing the file: `source cgtools-completion.sh` or adding it to your `.bashrc`. 
Note: If you use `zsh` you might need to add the following to your `.zshrc`:

```
autoload bashcompinit
bashcompinit
```

### Install

```
$ chmod u+x cgtools
$ ./cgtools install
generate bash completion? (y/n [y]): y
$ source cgtools-completion.sh
```

## Git Hooks

### Prepare Commit Message

## Change Log