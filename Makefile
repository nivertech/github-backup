BASEFLAGS=-Wall -fno-warn-orphans
GHCFLAGS=-O2 $(BASEFLAGS)
bins=github-backup
mans=github-backup.1
all=$(bins)

ifdef PROFILE
GHCFLAGS=-prof -auto-all -rtsopts -caf-all -fforce-recomp
endif

GHCMAKE=ghc $(GHCFLAGS) --make

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

all: $(all)

# Disables optimisation. Not for production use.
fast: GHCFLAGS=$(BASEFLAGS)
fast: $(bins)

$(bins):
	$(GHCMAKE) $@

install: all
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1

clean:
	rm -f $(bins)
	find . \( -name \*.o -or -name \*.hi \) -exec rm {} \;

.PHONY: $(bins)
