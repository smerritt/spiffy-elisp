Spiffy Textmate Mode
====================

There are a bunch of Textmate minor modes available to Emacs users. This is one of them.
If you don't like this one, try [defunkt's textmate.el](http://github.com/defunkt/textmate.el/tree/master).

Installation
============

To use it, put spiffy-textmate-mode.el somewhere that Emacs can find it and add

    (require 'spiffy-textmate-mode)
    (spiffy-textmate-mode t)

to your ~/.emacs file.

Also, you'll want to make sure that your command key is bound to the 'meta' modifier and your option key to the 'super' modifier.

You can accomplish this in Carbon Emacs thusly:

    (setq mac-pass-command-to-system nil)
    (setq mac-option-modifier 'super)


Keybindings
===========
* __shift-up__: move up a line and select
* __shift-down__: move down a line and select
* __shift-left__: move left one character and select
* __shift-right__: move right one character and select
* __control-shift-left__: move left one word and select
* __control-shift-right__: move right one word and select
* __meta-up__: move to the beginning of the buffer
* __meta-down__: move to the end of the buffer
* __meta-left__: move to the beginning of the line
* __meta-right__: move to the end of the line
* __meta-shift-up__: move to the beginning of the buffer and select
* __meta-shift-down__: move to the end of the buffer and select
* __meta-shift-left__: move to the beginning of the line and select
* __meta-shift-right__: move to the end of the line and select
* __control-meta-up__: scoot the region or current line up one line
* __control-meta-down__: scoot the region or current line down one line
* __control-meta-left__: scoot the region left one character
* __control-meta-right__: scoot the region right one character
* __super-up__: move up a paragraph
* __super-down__: move down a a paragraph
* __super-left__: move left a word, considering underscore as a word constituent
* __super-right__: move right a word, considering underscore as a word constituent
* __super-shift-up__: move up a paragraph and select
* __super-shift-down__: move down a a paragraph and select
* __super-shift-left__: move left a word, considering underscore as a word constituent and select
* __super-shift-right__: move right a word, considering underscore as a word constituent and select
* __Textmate-style parens__: typing a left delimiter (that is, "(", "[", or "{") also inserts the right delimiter and leaves the point between them. Typing the right delimiter moves over the ), ], or } instead of inserting a new one. If the cursor is between an empty pair of balanced delimiters, typing backspace will eliminate both of them.
* __meta-F__: grep in project, where the project root is the directory with the .git directory in it
* __control-K__: kill the current line
* __meta-l__: Go to line *(overrides downcase-word)*
* __meta-L__: highlight the current line or move the region's boundaries outward to line boundaries.
* __meta-t__: choose a file in the project, where the project root is the directory with the .git directory in it. *(overrides transpose-words)*
* __control-x 4 meta-t__: choose a file in the project and open it in another window (Emacs window, not GUI window)
* __control-x 5 meta-t__: choose a file in the project and open it in another frame (GUI window)
* __control-w__: selects the current word if no text is selected (Textmate behavior). If text is selected, control-w kills it (Emacs behavior). *(overrides and enhances kill-region)*
* __control-y__: yank and indent text. *(overrides and enhances yank)*
* __meta-y__: yank-pop and indent result. *(overrides and enhances yank-pop)*
* __meta-/__: comment/uncomment the current line/region. *(overrides dabbrev-expand)*
* __meta-[__: outdent the region or current line
* __meta-]__: indent the region or current line
* __meta-s-[__: fix indentation in region
* __meta-s-]__: align the region
* __meta-delete__: kill to end of line
* __meta-return__: skip to the end of the line and insert a newline.
* __meta-s-return__: open and indent a new line above the current one.
* __backspace__: delete selected text or character before point. In the middle of (), [], or {}, it deletes the pair. *(overrides and enhances delete-backward-char)*
* __meta-backspace__: kill to beginning of line *(overrides backward-kill-word)*
