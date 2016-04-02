index.html: index.org Makefile init.el
	emacs -Q -l init.el $< -f doexport
