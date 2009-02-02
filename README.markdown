Spiffy Textmate Mode
====================

There are a bunch of Textmate minor modes available to Emacs users. This is one of them.
If you don't like this one, try [defunkt's textmate.el](http://github.com/defunkt/textmate.el/tree/master).

Installation
============

To use it, put spiffy-textmate-mode.el somewhere that Emacs can find it and add
`(require 'spiffy-textmate-mode)`
`(spiffy-textmate-mode t)`
to your ~/.emacs file.

Functionality
=============
* cmd-l (M-l): Go to line
* shift+arrows: select text
  Unlike Textmate, typing plain arrows does not cause all the selected text to be unselected. Hit ctrl-g to unselect text.
* cmd-t (M-t): choose a file in the project (the project root is determined by the presence of a .git directory)
* Textmate-style parens: typing a left delimiter (that is, "(", "[", or "{") also inserts the right delimiter and leaves the point between them. Typing the right delimiter moves over the ), ], or } instead of inserting a new one. If the cursor is between an empty pair of balanced delimiters, typing backspace will eliminate both of them.
* ctrl-w selects the current word if no text is selected (Textmate behavior). If some text is selected, then ctrl-w kills it (Emacs behavior).
* ctrl-shift-k kills the entire line.
* backspace kills any selected text, otherwise it just acts like normal backspace.
* cmd-/ (M-/) comments/uncomments the current line/region.
* cmd-return (M-return) skips over the rest of the line and inserts a newline.
* cmd-s (M-s) saves the current buffer.