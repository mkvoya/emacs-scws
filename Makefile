EMACS_ROOT	= $(HOME)/code/readonly/emacs/
EMACS_SRC	= $(EMACS_ROOT)/src/
EMACS		= $(EMACS_ROOT)/src/emacs

all: jieba-module.so

jieba-module.o: jieba-module.cc
	$(CXX) -std=c++11 -Wall -I$(EMACS_SRC) -Icppjieba/deps -c $<

jieba-module.so: jieba-module.o
	$(CXX) -shared $< -o $@

test: jieba-module.so
	$(EMACS) --batch -Q --eval '(add-to-list (quote load-suffixes) ".so")' -L . -l jieba --eval '(message "%S" (jieba "我来到北京清华大学"))'
