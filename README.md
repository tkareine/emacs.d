# My Emacs configuration

It's here so that I can synchronize it across computers I work with.

I have copied or adapted the contents of some of the files from other
people. For small chunks of code, I have embedded the source URL in a
comment inside the file. When copying has been extensive, I have
retained the original copyright in the file. Thank you all!

## Installation

On macOS, using [Homebrew]:

``` bash
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-official-icon
```

## 3rd party tools in use

### GNU Global

Used by [ggtags], configured in [etc/tk-dev.el][etc-tk-dev.el].

Configure Global to use Exuberant Ctags for finding symbol definitions
and Pygments for symbol references.

Installation, on MacOS:

``` bash
brew install ctags
brew install global --with-ctags --with-pygments
```

I use configuration files for [Ctags][conf-ctags] and
[Global][conf-globalrc].

### LibreSSL

Used by TLS connections established by Emacs.

Installation, on MacOS:

``` bash
brew install libressl
export PATH="$(brew --prefix libressl)/bin:$PATH"
```

In [etc/tk-network.el][etc-tk-network.el], I customize variable
`gnutls-trustfiles` to point to the cert store file of LibreSSL.

[Homebrew]: https://brew.sh/
[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[etc-tk-dev.el]: etc/tk-dev.el
[etc-tk-network.el]: etc/tk-network.el
[ggtags]: https://github.com/leoliu/ggtags
