# My Emacs configuration

It's here so that I can synchronize it across computers I work with.

I have copied or adapted the contents of some of the files from other
people. For small chunks of code, I have embedded the source URL in a
comment inside the file. When copying has been extensive, I have
retained the original copyright in the file. Thank you all!

## 3rd party tools in use

### GNU Global

Used by [ggtags.el][ggtags.el], configured in
[etc/minor-modes.el][etc-minor-modes.el].

Configure Global to use Exuberant Ctags for finding symbol definitions
Pygments for symbol references.

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

In [etc/network.el][etc-network.el], I customize variable
`gnutls-trustfiles` to point to the cert store file of LibreSSL.

[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[etc-minor-modes.el]: etc/minor-modes.el
[etc-network.el]: etc/network.el
[ggtags.el]: https://github.com/leoliu/ggtags
