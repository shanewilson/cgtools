# Conventional Git Tools

Git Hooks and Changelog Generator based on Conventional Changelog

[![Build Status](https://travis-ci.org/shanewilson/cgtools.svg)](https://travis-ci.org/shanewilson/cgtools)

- [Why](#why)
- [What](#what)
- [Installation](#installion)
- [Git Hooks](#git-hooks)
- [Change Log](#change-log)

## Why
... Standardize git commits to run tools against later ...

## What
... A single file that can be added to your project that will install git hooks to standardize git commits and generate change logs automagically ...

## Installation

To install **cgtools** simply drop the [bin](https://github.com/shanewilson/cgtools/releases/download/0.1.0/cgtools) into your project directory and run `./cgtools install -v`.

```
> chmod +x cgtools          # Make cgtools executable
> ./cgtools install -v      # Install cgtools
Checking dependencies...
OK
Generating Git Hooks...
OK
```

### Tab completion in bash and zsh

**cgtools** can generate *tab completion* for bash and zsh with the flag `-b` or `--bash-completion`.
You can test it out by sourcing the file: `source cgtools-completion.sh`.

```
> ./cgtools install -b -v         # Install cgtools with bash completion
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

#### bash instructions

#### zsh instructions



**Note**: If you use `zsh` you might need to add the following to your `.zshrc`:

```
autoload bashcompinit
bashcompinit
```

### Source

To build from source you will need to install [Haskell](https://www.haskell.org/haskellwiki/Haskell)

- [Haskell Guide](https://github.com/bitemyapp/learnhaskell#getting-started)

```
$ git clone https://github.com/shanewilson/cgtools.git
$ cd cgtools
$ cabal sandbox init
$ cabal install --enable-tests --enable-benchmarks --force-reinstalls
$ .cabal-sandbox/bin/cgtools --help
```

## Git Hooks

### Prepare Commit Message

Talk about this next time

## Change Log

TODO