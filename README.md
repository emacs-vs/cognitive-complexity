[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/cognitive-complexity.svg)](https://jcs-emacs.github.io/jcs-elpa/#/cognitive-complexity)

# Cognitive Complexity for Emacs
Show the cognitive complexity of the code

[![CI](https://github.com/emacs-vs/cognitive-complexity/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-vs/cognitive-complexity/actions/workflows/test.yml)

<img src="./etc/demo.gif" />

This package is a direct `treesit` port of the `tree-sitter` based
[codemetrics](https://github.com/emacs-vs/codemetrics). It implements live
calculation of the **Cognitive Complexity** metric, which was proposed by G. Ann
Campbell in [Cognitive Complexity - A new way of measuring
understandability](https://www.sonarsource.com/docs/CognitiveComplexity.pdf) (c)
SonarSource S.A. 2016-2021, Switzerland.

> **Abstract:** Cyclomatic Complexity was initially formulated as a measurement
> of the "testability and maintainability" of the control flow of a module.
> While it excels at measuring the former, its underlying mathematical model is
> unsatisfactory at producing a value that measures the latter. This white paper
> describes a new metric that breaks from the use of mathematical models to
> evaluate code in order to remedy Cyclomatic Complexity’s shortcomings and
> produce a measurement that more accurately reflects the relative difficulty of
> understanding, and therefore of maintaining methods, classes, and
> applications.

Please note that this documentation is not up-to-date (it is basically the same
as `codemetrics`'), I will try to enrich it with `cognitive-complexity` specifc
information later!

## Installation

### Using `straight.el` and `use-package`:

```elisp
(use-package cognitive-complexity
  :straight (:host github :repo "emacs-vs/cognitive-complexity"))
```

### Manually

```sh
git clone https://github.com/emacs-vs/cognitive-complexity /path/to/lib
```

then in Emacs:

```elisp
(add-to-list 'load-path "/path/to/lib")
(require 'cognitive-complexity)
```

or

```elisp
(use-package cognitive-complexity
  :load-path "/path/to/lib")
```

## Usage

The simplest way to start using this package:

```elisp
(cognitive-complexity-mode 1)
```

### Use it as a library

These are functions you can use to analyze:

| Functions                      | Description                        |
|--------------------------------|------------------------------------|
| `cognitive-complexity-analyze` | Analyze a string with `major-mode` |
| `cognitive-complexity-region`  | Analyze region                     |
| `cognitive-complexity-buffer`  | Analyze the whole buffer           |

All these functions return the score data indicating the complexity.

## Supported languages
These languages are fairly complete:

- Bash
- C / C++ / C#
- Elisp
- Go
- Java / JavaScript / JSX / Julia
- Kotlin
- Lua
- PHP / Python
- Ruby / Rust
- Swift
- TypeScript / TSX

These languages are in development:

- Agda
- Elm
- Elixir
- OCaml
- Scala (upstream, kinda buggy)

## Customization

Although `cognitive-complexity` aims to have good analysis rules out of the box
for all supported definitions, people will indubitably have their own
preferences or desired functionality. The following section outlines how to add
your own analysis definitions and analysis functions to make
cognitive-complexity work for you. If there are any improvements you find for
existing or new languages, please do raise a PR so that others may benefit from
better analysis in the future!

### Analysis on new nodes

Code-Metrics defines all its analysis definitions in the variable
`cognitive-complexity-rules` which is an alist with the key of the alist being the
mode and the value being another alist of analysis definitions.

```elisp
;; Example of cognitive-complexity-rules' structure
'((c-mode      . c-analysis-definitions)  ; <language>-analysis-definitions is structured as shown below
  (csharp-mode . csharp-analysis-definitions)
  (go-mode     . go-analysis-definitions)
  (scala-mode  . scala-analysis-definitions)
  ...)

;; Examle of a analysis definition alist
(setq csharp-analysis-definitions
    (if_statement   . (1 t))
    ("&&"           . cognitive-complexity-rules--logical-operators))
```

So you can select whatever node that you want to analyze on it.

To find what node you'll want to analyze, refer to the
[tree-sitter documentation](https://emacs-tree-sitter.github.io/getting-started/#view-the-syntax-tree)
about viewing nodes. `tree-sitter-debug` and `tree-sitter-query-builder`
are both very useful for this.

#### Example

Let's look at a quick example of adding a new analysis definition. Let's say you
want to add analysis to `go-mode`'s `if_statement`. The analysis definition that
is needed will be `'("if_statement" . (1 t))`. To add this to the
`cognitive-complexity-rules`, you can do something like the following.

```emacs-lisp
(push '("if_statement" . (1 t)) (alist-get 'go-mode cognitive-complexity-rules))
```

Now the new analysis definition should be usable by cognitive-complexity!

### Writing new analysis functions

For more complex analysis, you can write your own analysis rules!

- `node` - (optional) the targeted tree-sitter node, in this example,
`if_statement` will be the targeting node.
- `depth` - (optional) current depth of from the root tree.
- `nested` - (optional) current nested level apply from current complexity
algorithm.

Then the function needs to return an integer represent the score and a
boolean represent increment of the nested level in the form
`(score-to-add . nested?)`. This can be useful if you want to add extra
conditional logic onto your analysis.

As an example of an analysis function, take a look at the definition of the
basic `cognitive-complexity-rules--class-declaration`.

```elisp
(defun cognitive-complexity-rules--class-declaration (_node depth _nested)
  "..."
  (cognitive-complexity-with-metrics
    (if (< 1 depth)  ; if class inside class,
        '(1 nil)     ; we score 1, but don't increase nested level
      '(0 nil))
    '(1 nil)))
```

## References

- [`codemetrics`](https://github.com/emacs-vs/codemetrics)
- [Code Metrics - Visual Studio Code Extension](https://github.com/kisstkondoros/codemetrics)
- [CognitiveComplexity for Rider and ReSharper](https://github.com/matkoch/resharper-cognitivecomplexity)
- [`gocognit`](https://github.com/uudashr/gocognit)

## Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## How to add an analysis rules?

When adding a new analysis rules, add the analysis definition function to
`cognitive-complexity.el` itself near where the other rules functions live and
then add the parser to `cognitive-complexity-rules.el` file. Finally, if you are
adding support for a new language, remember to add it to the
`cognitive-complexity-rules` variable.

When creating a new parser, name it `cognitive-complexity-rules-<language>`.

When creating a new analysis function, name it
`cognitive-complexity-rules-<language>-<feature>` or something similar.

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](/LICENSE) for details.
