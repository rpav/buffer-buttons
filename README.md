# buffer-buttons

Working in CL, particularly with slime, I often wish I could simply
click and evaluate a block of code:

```lisp
(progn
  (some-function)
  (some-other-function)) [<- Eval]
```

That is exactly what `buffer-buttons` is for.  This will let you
easily define, save, and load button definitions in files.  For
instance, if the above buffer were saved to a file, it would look like
this:

```lisp
(progn
  (some-function)
  (some-other-function)) ;;#button:slime-eval
```

On load this would be restored to the emacs "button" that lets you
eval the block.

## Usage

Using this is easy; copy or link `buffer-buttons.el` somewhere emacs
can find them, and put this in your `.emacs`:

```elisp
(load "buffer-buttons")
```

This will add the appropriate hook for loading.

### Inserting a button

Inserting a button is as simple as `M-x buffer-button-insert` followed
by the button type.  The following are predefined:

* `slime-eval`: Call `slime-eval-last-expression`.
* `slime-connect`: Call `(slime-connect "127.0.0.1" 4005)`.  You may
  need to alter this for your configuration!
* `elisp-eval`: Call `eval-last-sexp`, similar to the slime variant,
  but for elisp buffers.

This will insert the "button" at the point, which you may then click
on to activate.

### Defining buttons

Defining buttons is fairly simple as well.  Here is the definition for
the `slime-eval` button:

```elisp
(define-buffer-button (slime-eval b
                       :label "<- Eval"
                       :prefix ";;")
  (save-excursion
    (goto-char (button-start b))
    (slime-eval-last-expression)))
```

This defines the `slime-eval` button type, the function
`buffer-button-fn-slime-eval`, and sets the button label to `<- Eval`.
The variable `b` is the emacs button object.  The body is the function
is the body executed when the button is clicked.

When a button is inserted via `buffer-button-insert`, the following
text is generated in the buffer: `;;#button:slime-eval`.  Overlay
properties are then used to convert this into a functional and
visually-appealing link.

You may also specify `:prefix` and `:suffix`, which get prepended and
appended respectively to the generated text.  This is useful for
inserting buttons into source, such that any compilation or evaluation
does not error.
