# My Emacs setup

Here is my public Emacs configuration, which I checkout to computers I
work with. The configuration for selected command line tools is in [my
dotfiles] repository.

I have copied or adapted some contents from others. For small chunks of
code, I have embedded the source URL in a comment inside the file. When
copying has been extensive, I have retained the original copyright in
the file. Thank you all!

## Setup highlights

A screenshot, featuring syntax coloring of a shell script file (the left
buffer), [magit] (the right buffer), and [Consult]'s `consult-ripgrep`
(the bottom buffer, with [Vertico] used as the completion UI and
[Orderless] for completion matching), in the `base16-gruvbox-dark-pale`
color theme of [base16-theme]:

<img src="https://github.com/tkareine/emacs.d/raw/master/images/setup-showcase-v4.png" title="Setup showcase" alt="Setup showcase" width="916">

The font in use is [Input][Input font]
([customization](https://input.djr.com/download/?customize&fontSelection=fourStyleFamily&regular=InputMonoNarrow-Regular&italic=InputMonoNarrow-Italic&bold=InputMonoNarrow-Bold&boldItalic=InputMonoNarrow-BoldItalic&a=ss&g=ss&i=serifs_round&l=serifs_round&zero=0&asterisk=height&braces=straight&preset=default&line-height=1.2)).

### Show relevant information in mode-line

I've tuned the mode-line to show only information that I think is
relevant. For instance, only a selection of enabled minor modes is
shown. See [etc/tk-looks.el], grep for `mode-line-format`.

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

Installing Boris Buliga's (d12frosted) [Emacs+], using [Homebrew] on
macOS:

``` bash
brew tap d12frosted/emacs-plus
brew install d12frosted/emacs-plus/emacs-plus --with-modern-icon --with-native-comp
```

You'll need [Node.js] and [npm] for some of the 3rd party tools (see
below). For managing Node.js versions, I recommend using [chnode]
together with the latest LTS version of Node.js:

``` bash
brew tap tkareine/chnode
brew install tkareine/chnode/chnode
brew install node@22
mkdir -p ~/.nodes
ln -s /usr/local/opt/node@22 ~/.nodes/node-22
```

Then, put the following into your shell's (Bash or Zsh) init script:

``` bash
source chnode.sh
chnode node-22
```

## 3rd party tools in use

### GNU Global

[GNU Global] is used by the [ggtags] minor mode to generate and find
source code symbols. It's configured in [etc/tk-dev.el]. Ggtags mode
gets enabled in selected major modes, such as `sh-mode`, `scss-mode`,
and [yaml-mode], automatically. It's especially useful for navigating
vars in Ansible playbooks.

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
2. [Consult]'s `consult-ripgrep` (`C-c s`) when I want to discard search
   results right away.

Installation with Homebrew:

``` bash
brew install ripgrep
```

### jq

[jq] is used by [Flycheck] in [json-mode] to check JSON syntax.

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

[Prettier] is a popular code formatter, used by [Apheleia] in selected
major modes to reformat buffers upon save.

Installation with npm:

``` bash
npm install -g prettier
```

### TypeScript

I use the [`tsserver`][tsserver] CLI tool of of [TypeScript] and
[typescript-language-server] as the server for [lsp-mode], configured in
[etc/tk-dev.el].

Installation with npm:

``` bash
npm install -g typescript typescript-language-server
```

[Apheleia]: https://github.com/radian-software/apheleia
[Consult]: https://github.com/minad/consult
[Emacs+]: https://github.com/d12frosted/homebrew-emacs-plus
[Flycheck]: https://www.flycheck.org/
[GNU Global]: https://www.gnu.org/software/global/
[Homebrew]: https://brew.sh/
[Input font]: https://input.djr.com/
[Marked]: https://github.com/markedjs/marked
[Node.js]: https://nodejs.org/
[Orderless]: https://github.com/oantolin/orderless
[Prettier]: https://prettier.io/
[Smartparens]: https://github.com/Fuco1/smartparens
[TypeScript]: https://github.com/Microsoft/TypeScript
[Vertico]: https://github.com/minad/vertico
[base16-theme]: https://github.com/tinted-theming/base16-emacs
[chnode]: https://github.com/tkareine/chnode
[conf-ctags]: https://github.com/tkareine/dotfiles/blob/master/.ctags
[conf-globalrc]: https://github.com/tkareine/dotfiles/blob/master/.globalrc
[deadgrep]: https://github.com/Wilfred/deadgrep
[etc/tk-dev.el]: etc/tk-dev.el
[etc/tk-editing.el]: etc/tk-editing.el
[etc/tk-looks.el]: etc/tk-looks.el
[etc/tk-packages.el]: etc/tk-packages.el
[ggtags]: https://github.com/leoliu/ggtags
[jq]: https://stedolan.github.io/jq/
[json-mode]: https://github.com/joshwnj/json-mode
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode/
[magit]: https://magit.vc/
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
[my dotfiles]: https://github.com/tkareine/dotfiles/
[node-build]: https://github.com/nodenv/node-build
[npm]: https://www.npmjs.com/
[ripgrep]: https://github.com/BurntSushi/ripgrep
[tsserver]: https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29
[typescript-language-server]: https://github.com/theia-ide/typescript-language-server
[yaml-mode]: https://github.com/yoshiki/yaml-mode
