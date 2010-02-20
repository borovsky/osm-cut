###-*-makefile-*-   ; force emacs to enter makefile-mode


PREFIX             = /usr/local
prefix             = ${PREFIX}
ETCDIR             = ${prefix}/etc
VARDIR             = ${prefix}/var

ifeq ($(DESTDIR),)
 DESTDIR=/
endif

## DESTDIR is handled separately in the individual Makefiles

ERL=/usr/bin/erl
WERL=
ERLC=/usr/bin/erlc
EMULATOR=beam
DEFAULT_CHARSET=
EPAM = ../priv/epam
EXTRAINCLUDE = 
ERLBINDIR = /usr/lib/erlang/bin


ifdef debug
  ERLC_FLAGS+=-Ddebug
endif

ifdef trace
  ERLC_FLAGS=+trace
endif

ifdef export_all
  ERLC_FLAGS+=-Dexport_all
endif

ifdef debug_info
  ERLC_FLAGS+=+debug_info
endif


ERLDIR=/usr/lib/erlang
APPDIR=$(ERLDIR)/lib/erlsom-$(ERLSOM_VSN)

INSTALL=/bin/install -c
INSTALL_DATA=${INSTALL} -m 644



EMULATOR=beam
ifdef debug
  ERLC_FLAGS+=-Ddebug
endif

ERLC_FLAGS+=+"{hipe, o3}" +inline +native

ifdef trace
  ERLC_FLAGS=+trace
endif

ifdef export_all
  ERLC_FLAGS+=-Dexport_all
endif


# Hmm, don't know if you are supposed to like this better... ;-)
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

# Targets

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.appup: %.appup 
	cp $< $@

../ebin/%.$(EMULATOR): %.erl
	"$(ERLC)" $(ERLC_FLAGS) -o ../ebin $<

%.$(EMULATOR): %.erl
	"$(ERLC)" $(ERLC_FLAGS) $<



