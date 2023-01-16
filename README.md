# My Emacs setup

My public Emacs configuration, in order to synchronize them to computers
I work with. The configuration for selected command line tools is in [my
dotfiles] repository.

I have copied or adapted some contents from others. For small chunks of
code, I have embedded the source URL in a comment inside the file. When
copying has been extensive, I have retained the original copyright in
the file. Thank you all!

## Setup highlights

A screenshot, featuring [magit] (the right buffer) and [Ivy]'s
`counsel-projectile-ag` (the bottom buffer), in [Zenburn] theme:

<img src="https://github.com/tkareine/emacs.d/raw/master/images/setup-showcase.png" title="Setup showcase" alt="Setup showcase" width="864">

The font in use is [Input][Input font]
([customization](https://input.fontbureau.com/download/index.html?size=14&language=python&theme=solarized-dark&family=InputMono&width=300&weight=400&line-height=1.1&a=ss&g=ss&i=serifs_round&l=serifs_round&zero=0&asterisk=height&braces=straight&preset=consolas&customize=please)).

### Show relevant information in mode-line

I've tuned the mode-line to show only information that I think is
relevant. For instance, only a selection of enabled minor modes is
shown. See [etc/tk-looks.el], grep for `mode-line-format`.

### JavaScript, TypeScript, and JSX source file editing

I have customized [js2-mode] (for `.js` sources), [rjsx-mode] (`.jsx`),
and [typescript-mode] (`.ts`, `.tsx`) major modes to work together with
[lsp-mode], [Prettier] (via [prettier.el]), [company-mode], and
[Flycheck]. For instance, when I save a `.ts` buffer, Flycheck validates
the file and Prettier reformats it. See [etc/tk-dev.el].

### Small editing improvements

There are a bunch of small improvements to editing in
[etc/tk-editing.el], such as:

* Key binding `C-a`
  (`tk-editing/back-to-indentation-or-move-beginning-of-line`) switches
  the point between the start of line content or the beginning of the
  line.
* Key binding `M-/` (`tk-editing/comment-or-uncomment-region-or-line`)
  comments or uncomments either the current line the point is on (if no
  region is active) or active region. And if the point was on the
  beginning of the line, automatically move point to the next line.
* By default, highlight trailing whitespace with high-key color and show
  tabs with low-key color (easy to see, but not to disturb you). Toggle
  showing trailing whitespace with `C-x W`
  (`tk-editing/toggle-show-trailing-whitespace`).
* Copy the path of the current buffer to the OS clipboard with `C-c P`
  (`tk-editing/file-path-to-clipboard`), helping you to pass the path to
  other programs quickly.
* Use [Smartparens] globally, so that it's easy to copy strings within
  quotes, for example.
* Save the history of recent files periodically, in order to avoid
  losing the information if Emacs crashes.

## Installation

Installing Mitsuharu Yamamoto's [Emacs Mac port], using [Homebrew] on
macOS:

``` bash
brew tap railwaycat/emacsmacport
brew install railwaycat/emacsmacport/emacs-mac --with-modern-icon
```

You'll need [Node.js] and [npm] for some of the 3rd party tools (see
below). For managing Node.js versions, I recommend using [chnode]
together with the latest LTS version of Node.js:

``` bash
brew tap tkareine/chnode
brew install tkareine/chnode/chnode
brew install node@18
mkdir -p ~/.nodes
ln -s /usr/local/opt/node@18 ~/.nodes/node-18
```

Then, put the following into your shell's (Bash or Zsh) init script:

``` bash
source chnode.sh
chnode node-18
```

## 3rd party tools in use

### GNU Global

[GNU Global] is used by the [ggtags] minor mode to generate and find
source code symbols. It's configured in [etc/tk-dev.el]. Ggtags mode
gets enabled in selected major modes, such as [enhanced-ruby-mode],
`scss-mode`, and [yaml-mode], automatically. It's especially useful for
navigating vars in Ansible playbooks.

Configure Global to use Exuberant Ctags for finding symbol definitions
and Pygments for symbol references.

Installation, with Homebrew on macOS:

``` bash
brew install global
```

I use configuration files for [Ctags][conf-ctags] and
[Global][conf-globalrc].

### ripgrep (rg)

I use two frontends for [ripgrep], the fast search tool:

1. [deadgrep] for situations when I want to persist the search results
   (`C-c a` for `deadgrep`), and
2. [Ivy]'s `counsel-projectile-rg` (`C-c s`) when I want to discard
   search results right away.

Installation with Homebrew:

``` bash
brew install ripgrep
```

### jq

[jq] is used by [Flycheck] in [json-mode] to check JSON syntax. It's
configured in [etc/tk-dev.el].

Installation with Homebrew:

``` bash
brew install jq
```

### Marked

[Marked] generates the HTML output from Markdown sources, used by
[markdown-mode]. It's configured in [etc/tk-dev.el].

Installation with npm:

``` bash
npm install -g marked
```

### Prettier

[Prettier] is a popular code formatter, used by [prettier.el] to
reformat the buffer upon save. It's configured in [etc/tk-dev.el] and
enabled for [js2-mode], [typescript-mode], `html-mode`, [json-mode], and
[yaml-mode] automatically.

Installation with npm:

``` bash
npm install -g prettier
```

### TypeScript

I use the [`tsserver`][tsserver] CLI tool of of [TypeScript] and
[typescript-language-server] as the server for [lsp-mode], configured in
[etc/tk-dev.el]. [lsp-mode] gets enabled for [js2-mode] and
[typescript-mode] automatically.

Installation with npm:

``` bash
npm install -g typescript typescript-language-server
```

[Emacs Mac port]: https://bitbucket.org/mituharu/emacs-mac/src/master/
[Flycheck]: https://www.flycheck.org/
[GNU Global]: https://www.gnu.org/software/global/
[Homebrew]: https://brew.sh/
[Input font]: http://input.fontbureau.com/
[Ivy]: https://github.com/abo-abo/swiper
[Marked]: https://github.com/markedjs/marked
[Node.js]: https://nodejs.org/
[Prettier]: https://prettier.io/
[Smartparens]: https://github.com/Fuco1/smartparens
[TypeScript]: https://github.com/Microsoft/TypeScript
[Zenburn]: https://github.com/bbatsov/zenburn-emacs
[chnode]: https://github.com/tkareine/chnode
[company-mode]: https://company-mode.github.io/
[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[deadgrep]: https://github.com/Wilfred/deadgrep
[enhanced-ruby-mode]: https://github.com/zenspider/enhanced-ruby-mode
[etc/tk-dev.el]: etc/tk-dev.el
[etc/tk-editing.el]: etc/tk-editing.el
[etc/tk-looks.el]: etc/tk-looks.el
[etc/tk-packages.el]: etc/tk-packages.el
[ggtags]: https://github.com/leoliu/ggtags
[jq]: https://stedolan.github.io/jq/
[js2-mode]: https://github.com/mooz/js2-mode
[json-mode]: https://github.com/joshwnj/json-mode
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode/
[magit]: https://magit.vc/
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
[my dotfiles]: https://github.com/tkareine/dotfiles/
[node-build]: https://github.com/nodenv/node-build
[npm]: https://www.npmjs.com/
[prettier.el]: https://github.com/jscheid/prettier.el
[ripgrep]: https://github.com/BurntSushi/ripgrep
[rjsx-mode]: https://github.com/felipeochoa/rjsx-mode
[tsserver]: https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29
[typescript-language-server]: https://github.com/theia-ide/typescript-language-server
[typescript-mode]: https://github.com/emacs-typescript/typescript.el
[yaml-mode]: https://github.com/yoshiki/yaml-mode
