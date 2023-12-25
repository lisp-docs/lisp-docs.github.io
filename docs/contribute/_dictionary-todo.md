# Common Lisp Dictionary Entries To Do's

Here are all the dicionary entry pages for which we need expanded explanations and examples. If you write an exaplantion on one and submit a pull request, please strike the list items here by surrounding it with `~~` and move it to the "Done" list in the bottom of the page.

## Missing Pages

- Missing Pages and where they are:
  - Check Dictionaries
    - Chapter 9:
      - `simple-condition-format-control, simple- condition-format-arguments` are in `simple-condition`
    - Chapter 12:
      - `=`, `+`, `-`, `1+`, `1-`, missing
      - `*` is in `_sinh.md`
      - `boole-1` etc are in `boole`
      - `arithmetic-error-operands` in `arithmetic-error`
      - `most-positive-short-float` in `floatp`
    - Chapter 14:
      - `cons`  `System Class` missing
      - `atom` `Type` missing
      - `null` `System Class` missing
    - Chapter 15:
      - The "See Also" section of `adjust-array` is missing. See `adjustable-array-p`
      - `vector` `System Class` is missing
      - `bit-and` etc in `bit, sbit`
    - Chapters 16 and on need to check
    - Chapter 22:
      - Allmost everything is between 22.3 and intro... need to parse again...
    - Chapter 23:
      - `set-dispatch-macro-character, get-dispatch-macro character` in `readtablep`
      - `∗readtable∗`, `*read-suppress*`, `∗read-default-float-format∗`, `*read-base*`, `*read-eval*` are in `with-standard-io-syntax`
    - Chapter 24:
      - `∗compile-print∗**,** ∗compile-verbose∗`, `∗load-print∗**` are in `with-compilation-unit`
    - Chapter 25:
      - `-`, `+`, `*` are missing


## Changes

- Epsilon character

## Done

- Missing Pages and where they are:
  - Check Dictionaries
    - Chapter 5:
      - ~~defun~~
      - ~~catch~~
      - ~~funcall~~
      - ~~let, let* (in defstructuring-bind)~~
      - ~~nil (not in `not` nor in `unwind-protect`)~~
      - ~~prog, prog* (in `nth-value`)~~
    - Chapter 7:
      - ~~`(setf class-name)` missing~~
    - Chapter 8:
      - ~~both `defstruct` and `copy-structure` are in the intro...~~
    - Chapter 9:
      - ~~`*debugger-hook∗` and `∗break-on-signals∗` are on `break`~~
      - ~~`abort, continue, muffle-warning, store-value, use value` in `use-value`~~
    - Chapter 10:
      - ~~`*gensym-counter*` in `gensym`~~
    - Chapter 11:
      - ~~`∗package∗` in `packagep`~~
    - Chapter 24:
      - ~~`∗features∗`~~, ~~`∗compile-file-pathname∗**,** ∗compile-file-truename∗`~~, ~~`∗load-pathname∗**,** ∗load-truename∗`~~, ~~`** ∗load-verbose∗`~~, ~~`∗modules∗`~~, are in `with-compilation-unit`
    - Chapter 25:
      - ~~`encode-universal-time` is in `decode-universal-time`~~
      - ~~`lisp-implementation-type, lisp-implementation version` are in `/`~~
