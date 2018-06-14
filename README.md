# My Emacs configuration

It's here so that I can synchronize it across computers I work with.

I have copied or adapted the contents of some of the files from other
people. For small chunks of code, I have embedded the source URL in a
comment inside the file. When copying has been extensive, I have
retained the original copyright in the file. Thank you all!

## Installation

Installing Emacs itself, using [Homebrew] on macOS:

``` bash
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-official-icon
```

You'll need [Node.js] and [npm] for some 3rd party tools (listed
below). For installing Node.js, I recommend using [nodenv]:

``` bash
brew install nodenv

# setup nodenv, probably with `eval "$(nodenv init -)"`

nodenv install $nodejs_version_you_want
nodenv global $nodejs_version_you_want
```

## 3rd party tools in use

### GNU Global

[GNU Global] is used by [ggtags] minor mode to generate and find source
code symbols. It's configured in [etc/tk-dev.el]. `ggtags` mode gets
enabled in selected major modes, such as for `.scss` and `.rb` sources,
automatically.

Configure Global to use Exuberant Ctags for finding symbol definitions
and Pygments for symbol references.

Installation, on macOS:

``` bash
brew install ctags
brew install global --with-ctags --with-pygments
```

I use configuration files for [Ctags][conf-ctags] and
[Global][conf-globalrc].

### jq

[jq] is used by [Flycheck] in [json-mode] to check json syntax. It's
configured in [etc/tk-dev.el].

Installation, on macOS:

``` bash
brew install jq --devel
```

### LibreSSL

I use [LibreSSL] to establish TLS connections in Emacs, because I want
to use a nonobscure TLS implementation.

Installation, on macOS:

``` bash
brew install libressl
export PATH="$(brew --prefix libressl)/bin:$PATH"
```

In [etc/tk-network.el], I customize variable `gnutls-trustfiles` to
point to the cert store file of LibreSSL.

### Marked

[Marked] generates the html output from Markdown sources, used by
[markdown-mode]. It's configured in [etc/tk-dev.el].

``` bash
npm install -g marked
```

### TypeScript

I use [`tsserver`][tsserver] CLI tool of [TypeScript] via [Tide] minor
mode, configured in [etc/tk-dev.el]. Tide gets enabled
for `.js` and `.jsx` sources automatically.

Installation:

``` bash
npm install -g typescript
```

[Flycheck]: http://www.flycheck.org/en/latest/
[GNU Global]: https://www.gnu.org/software/global/
[Homebrew]: https://brew.sh/
[LibreSSL]: https://www.libressl.org/
[Marked]: https://github.com/markedjs/marked
[Node.js]: https://nodejs.org/
[Tide]: https://github.com/ananthakumaran/tide
[TypeScript]: https://github.com/Microsoft/TypeScript
[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[etc/tk-dev.el]: etc/tk-dev.el
[etc/tk-network.el]: etc/tk-network.el
[ggtags]: https://github.com/leoliu/ggtags
[jq]: https://stedolan.github.io/jq/
[json-mode]: https://github.com/joshwnj/json-mode
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
[nodenv]: https://github.com/nodenv/nodenv
[npm]: https://www.npmjs.com/
[tsserver]: https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29
