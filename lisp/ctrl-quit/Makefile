CC=gcc
CFLAGS=-fPIC -I$(shell emacs -Q --batch --eval '(princ (expand-file-name "include" installation-directory))' 2>/dev/null) -Wall -Wextra
LDFLAGS=-shared -lX11

all: ctrl-quit-module.so

ctrl-quit-module.so: ctrl-quit-module.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f ctrl-quit-module.so
