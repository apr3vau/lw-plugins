# LispWorks Plugins by April & May

Here's the LispWorks Plugins created by April & May. All plugins are
zero-dependency, tested under the LispWorks 8.0.1 Windows & Macintosh,
safe for delivery, and have been used for a long time in my own. All
the codes are licensed under
[0BSD](https://spdx.org/licenses/0BSD.html)), allowing any kind of
usage without any limitation. Feel free to include any part of them in
your own product.

Here we provide:

- A Sly-style flexible fuzzy-matching in-place code completion for LispWorks Editor ([flex-complete.lisp](./editor-markdown.lisp))
- A decent syntax highlight method of Markdown for LispWorks Editor ([editor-markdown.lisp](./editor-markdown.lisp))
- A decent & elaborate syntax highlight method of Lisp for LispWorks Editor ([colourful.lisp](./colourful.lisp))
- A enhancing / bugfixing / edge-case completing plugin for LispWorks Directory Mode, make it more similar with Emacs's dired ([directory.lisp](./directory.lisp))
- A expand-region plugin for Lisp editing in LispWorks Editor, similar with [expand-region.el](https://github.com/magnars/expand-region.el) ([expand-region.lisp](./expand-region.lisp))
- A simple pair-editing facility like Emacs's electric-pair-mode, with just enough of functions ([pair.lisp](./pair.lisp))

## Usage

Basically, just simply load any file you want to include. Some plugins
has specific requirement for delivery (like
[editor-markdown](editor-markdown.lisp)). For details, please visit
each source file and read the top comment lines of them.

Here's a sample code to load all of them into your LispWorks IDE. We
assume that you clone this repo into `~/common-lisp/lw-plugins`:

	(dolist (file (directory #P"~/common-lisp/lw-plugins/*.lisp"))
	  (compile-file file :load t))

And we also provide an ASDF system file. You can load it using ASDF:

	(require "asdf")
	(asdf:load-system :lw-plugins)

----------------

## Acknowledgements

Thanks to the LispWorks Ltd. to provide us the exellent product.

Thanks my headmate, May, who helps and supports me.

Supporting Neurodiversity & Transgender & Plurality!

🏳️‍🌈🏳️‍⚧️

----------------

## Screenshots

### Flex completion

![Flex completion](./images/completion.png)

### Markdown

![Markdown](./images/markdown.png)

