GHCFLAGS=-O2 -Wall
bins=github-backup
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
fast: GHCFLAGS=-Wall
fast: $(bins)

$(bins):
	$(GHCMAKE) $@

clean:
	rm -f $(bins)
	find . \( -name \*.o -or -name \*.hi \) -exec rm {} \;

.PHONY: $(bins)
