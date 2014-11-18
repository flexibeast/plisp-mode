# picolisp-mode - major mode for PicoLisp programming

*Author:* Alexis <flexibeast@gmail.com><br>
*URL:* [https://github.com/flexibeast/picolisp-mode](https://github.com/flexibeast/picolisp-mode)<br>

`picolisp-mode` provides a major mode for PicoLisp programming.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Issues](#issues)
- [License](#license)

## Features

* Syntax highlighting of PicoLisp code.

* Comint-based `pil` REPL buffers.

* Quick access to documentation for function at point.

## Installation

Install [picolisp-mode from MELPA](http://melpa.org/#/picolisp-mode), or put the `picolisp-mode` folder in your load-path and do a `(require 'picolisp-mode)`.

## Usage

Enable syntax highlighting for a PicoLisp source buffer with `M-x picolisp-mode`.

Start a `pil` REPL session with `M-x picolisp-repl`.

Access documentation for the function at point with `C-c C-f` (`picolisp-describe-function`). The browser used by `pil` is specified by the variable `picolisp-browser`. By default, it is set to the `eww` helper shell script in the `picolisp-mode` source directory, which opens an `eww` buffer via `emacsclient`. If you're using a pre-24.4 version of Emacs (and thus don't have `eww` available), or don't want to run an Emacs daemon/server, you can set `picolisp-browser` to any other HTML browser on your system, e.g. `/usr/bin/iceweasel`.

Various customisation options, including the faces used for syntax highlighting, are available via the `picolisp` customize-group.

<a name="issues"></a>

## Issues / bugs

If you discover an issue or bug in `picolisp-mode` not already noted:

* as a TODO item, or

* in [the project's 'Issues' section on GitHub](https://github.com/flexibeast/picolisp-mode/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `picolisp-mode`.

## License

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.


---
Converted from `picolisp-mode.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
