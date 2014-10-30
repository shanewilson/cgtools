# Git Release Tools

Git Hooks and Changelog Generator based on Conventional Changelog

## Setup
## Git Hooks
### Commit Message

```
gt hooks commitmsg $1
```

## Log

```
gt log --version=... --subtitle=... --file=... --from=... --to=... --repo=... 
```

## Release
### Prepare

- bump version
- generate changelog

```
// Version 1.0.0-SNAPSHOT
gt prerelease
// Version bump to 1.0.0-RC1
// CHANGELOG updates
// <Custom activities here>
gt publish
// creates tag for 1.0.0-RC1
// creates git release from CHANGELOG
// 
```

### Publish

```
gt release publish 1.0.0
```

### Next
gt release next 1.1.0-SNAPSHOT
