# My Emacs configuration

It's here so that I can synchronize it to computers I work with.

I have copied or adapted some contents from others. For small chunks of
code, I have embedded the source URL in a comment inside the file. When
copying has been extensive, I have retained the original copyright in
the file. Thank you all!

## Installation

Installing Emacs itself, using [Homebrew] on macOS:

``` bash
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-official-icon
```

You'll need [Node.js] and [npm] for some 3rd party tools (listed
below). For managing Node.js versions, I recommend using [chnode]
together with [node-build]:

``` bash
curl 'https://raw.githubusercontent.com/tkareine/chnode/master/chnode.sh' > chnode.sh
brew install node-build
mkdir -p ~/.nodes
node-build 10.11.0 ~/.nodes/node-10.11.0

# put these into shell init script:
source chnode.sh
chnode node-10
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

### Ag (The Silver Searcher)

I use two frontends for [Ag], the search tool:

1. [ag.el] for situations when I want to persist the search results, and
2. [Ivy]'s `counsel-projectile-ag` when I want to discard search results
   right away.

Installation, on macOS:

``` bash
brew install the_silver_searcher
```

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
mode, configured in [etc/tk-dev.el]. Tide gets enabled for `.ts`,
`.tsx`, `.js`, and `.jsx` sources automatically.

Installation:

``` bash
npm install -g typescript
```

[Ag]: https://github.com/ggreer/the_silver_searcher
[Flycheck]: http://www.flycheck.org/en/latest/
[GNU Global]: https://www.gnu.org/software/global/
[Homebrew]: https://brew.sh/
[Ivy]: https://github.com/abo-abo/swiper
[LibreSSL]: https://www.libressl.org/
[Marked]: https://github.com/markedjs/marked
[Node.js]: https://nodejs.org/
[Tide]: https://github.com/ananthakumaran/tide
[TypeScript]: https://github.com/Microsoft/TypeScript
[ag.el]: https://github.com/Wilfred/ag.el
[chnode]: https://github.com/tkareine/chnode
[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[etc/tk-dev.el]: etc/tk-dev.el
[etc/tk-network.el]: etc/tk-network.el
[ggtags]: https://github.com/leoliu/ggtags
[jq]: https://stedolan.github.io/jq/
[json-mode]: https://github.com/joshwnj/json-mode
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
[node-build]: https://github.com/nodenv/node-build
[npm]: https://www.npmjs.com/
[tsserver]: https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29
