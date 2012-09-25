lisp-basenames := packages principles
asdfs := object-theory
lisp-files := $(addsuffix .lisp,$(lisp-basenames)) $(addsuffix .asd,$(asdfs))
editable-files := $(lisp-files) Makefile README.mkd todo.org
emacs-backups := $(addsuffix ~,$(editable-files))

# functions
force-delete = rm -f $(1)

.PHONY: clean

clean:
	$(call force-delete,$(emacs-backups))
