# SATySFi My Soul

## Prerequisite

* Haskell Stack
* Pandoc >= 2.3
* [SatysfiFilter](https://github.com/nekketsuuu/SatysfiFilter)
  * Run `stack install` to generate `SatysfiFilter-exe`.

## Build

```sh
./build.hs
```

Don't foreget to build before committing. A script `git-c.bash` is useful for automatical builds.

```sh
git config --local alias.c '!bash ./git-c.bash'
git c
```

## License

* Contents written by Markdown: [CC BY 3.0 Unported](https://creativecommons.org/licenses/by/3.0/)
* Build script & templates: Apache 2.0

See [LICENSE](https://github.com/nekketsuuu/how-to-satysfi/blob/master/LICENSE) for details.
