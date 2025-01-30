# nasm-mode
`nasm-mode` is a major mode for editing [NASM][nasm] x86 assembly
programs. It includes syntax highlighting, automatic indentation, and
imenu integration. Unlike Emacs' generic `asm-mode`, it understands
NASM-specific syntax. Requires Emacs 24.3 or higher.

The instruction and keyword lists are from NASM 2.16.03.

# Installing with Quelpa
If you prefer to use a package manager, you can use [quelpa-use-package].

```elisp
;; Install NASM
(use-package nasm-mode
  :quelpa (nasm-mode :fetcher github :repo "GabrielFrigo4/nasm-mode"))
```

## Known Issues
* Due to limitations of Emacs' syntax tables, like many other major
  modes, double and single quoted strings don't properly handle
  backslashes, which, unlike backquoted strings, aren't escapes in
  NASM syntax.


[nasm]: http://www.nasm.us/
[quelpa-use-package]: https://github.com/quelpa/quelpa-use-package