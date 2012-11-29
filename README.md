# erc-image.el #

Show inlined images (png/jpg) in erc buffers.

usage:

```lisp
(require 'erc-image)
```

This plugin subscribes to hooks `erc-insert-modify-hook` and `erc-send-modify-hook` to download and show images. In this early version it's doing this synchronously.
