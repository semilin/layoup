# Layoup

## Usage
Run the `./layoup` binary. Type `help` for a list of commands, which
should tell you most of what you need to know!

## Installation
Prebuilt binaries don't exist yet since layoup isn't release
ready. For the time being, you can build it from source. This might be
a little confusing if you're not experienced with Common Lisp, so I'll
draw out the steps here.
### SBCL
First, you need SBCL, a Common Lisp compiler. Other compilers might
work (CCL has been tested to work), but I don't guarantee any support
for them. If you're on Linux, you should install it through your
package manager. Otherwise, download a prebuilt binary from the
[platform table on their
website](https://www.sbcl.org/platform-table.html). See the [getting
started page](https://www.sbcl.org/getting.html) for information on
installation.

### Quicklisp
Quicklisp is the package manager for common lisp. To install it, first
download
[quicklisp.lisp](https://beta.quicklisp.org/quicklisp.lisp). Then open
a terminal, and in the same directory as `quicklisp.lisp`, start SBCL
with `sbcl --load quicklisp.lisp`.

In the Lisp REPL, run `(quicklisp-quickstart:install)`, and wait for
that to finish. After it's done, run `(ql:add-to-init-file)` to have
quicklisp be loaded automatically. Then run `(quit)` to close the REPL.

### Building Layoup
```sh
git clone https://github.com/semilin/layoup
cd layoup
./pull-dependencies
./make
./layoup
```
## Author

* semi (semilin@disroot.org)

## Copyright

Copyright (c) 2022 semi (semilin@disroot.org)

## License

Licensed under the GPLv3 License.
