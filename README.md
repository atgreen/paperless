# paperless.el
Emacs assisted PDF document filing

You've just scanned a stack of papers that have be cluttering up your
desk.  Now what?  Paperless-mode is an Emacs major mode to assist with
filing scanned documents into a tree of folders.

Here's how it works...

## Step 1

Scan your documents and dump them into a holding directory.  Set the
variable `*paperless-capture-dir*` to reference this directory.  For
instance, my ~/.emacs/init.el file includes:

```lisp
(setq *paperless-capture-dir* "/home/green/TOL/CAPTURE")
```

## Step 2

Tell paperless-mode where to file the documents by setting
`*paperless-root-dir*`, like so:

```lisp
(setq *paperless-root-dir* "/home/green/Documents")
```

Under `*paperless-root-dir*`, create a hierarchy of directories that
makes sense for you.  For example, you might create directories to
sort documents like so:

```
*paperless-root-dir*/Finance
                       /Taxes
                         /2015
                         /2016
                       /Insurance
                         /Car
                         /Home
                       /Work
                         /Expenses
                         /2016-FOSDEM
                         /20170102-NYC
                         /20170202-SFO
                       /Medical
                         /Receipts
                         /2016
                         /2017
                       /Utilities
                         /Mobile
                         /Electricity
```

At the time of this writing, the author's directory tree contains over
620 folders.

## Step 3

WIP.  Load and run (paperless).  Use `f`, `d` and `x` keys. 

## Installation

## Licensing

Copyright 2016 by Anthony Green

Paperless is provided under the terms of the 3-clause BSD license.
See paperless.el for details.
