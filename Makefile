EMACS_ROOT	= $(HOME)/src/emacs/
EMACS_SRC	= $(EMACS_ROOT)/src/
EMACS		= $(EMACS_ROOT)/src/emacs

all: scws-module.so

scws-module.o: scws-module.c
	$(CC) -std=c99 -Wall -I$(EMACS_SRC) -c $<

scws-module.so: scws-module.o
	$(CC) -shared -lscws $< -o $@

test: scws-module.so
	$(EMACS) --batch -Q -L . -l scws --eval '(message "%S" (scws "我来到北京清华大学"))'
