index.html: index.org Makefile init.el
	emacs -Q -l init.el $< -f doexport

parinfer_index.html: parinfer_index.org Makefile init.el
	emacs -Q -l init.el $< -f doexport
