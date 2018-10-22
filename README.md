# SATySFi My Soul

## Prerequisites

* Haskell Stack
* Pandoc >= 2.3
* [SatysfiFilter](https://github.com/nekketsuuu/SatysfiFilter) >= 0.6.0.0
  * Run `stack install` to generate `SatysfiFilter-exe`.
* [PandocPagetitle](https://github.com/nekketsuuu/PandocPagetitle)
  * Run `stack install` to generate `PandocPagetitle-exe`.

## Build

```sh
./build.hs
```

Don't foreget to build before `git push`. A script `git-publish.bash` is useful for automatical builds.

```sh
git config --local alias.publish '!bash ./git-publish.bash'
git publish
```

Alternatively, [codeskyblue/fswatch](https://github.com/codeskyblue/fswatch) can be used to auto-builds.

```sh
fswatch  # settings: .fsw.yml
```

## License

* Contents written by Markdown: [CC BY 3.0 Unported](https://creativecommons.org/licenses/by/3.0/)
* Build script & templates: [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)
    * The original HTML & CSS templates is [diversen/pandoc-bootstrap-adaptive-template](https://github.com/diversen/pandoc-bootstrap-adaptive-template). Thank you @diversen :)

See [LICENSE](https://github.com/nekketsuuu/how-to-satysfi/blob/master/LICENSE) for details.
