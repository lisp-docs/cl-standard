#!/bin/sh
# CB this script can be used to build the common lisp standard draft

build() {
	pdftex includer.tex
}

view() {
	evince includer.pdf
}

solidify() {
	cp includer.pdf cl-ansi-standard-draft-w-sidebar.pdf
}

eval "$1" "$2"
