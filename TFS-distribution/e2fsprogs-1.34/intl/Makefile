# Makefile for directory with message catalog handling in GNU NLS Utilities.
# Copyright (C) 1995-1998, 2000-2002 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

PACKAGE = e2fsprogs
VERSION = 1.34

SHELL = /bin/sh

srcdir = .
top_srcdir = ..
top_builddir = ..


prefix = /usr
exec_prefix = ${prefix}
transform = s,x,x,
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include
datadir = ${prefix}/share
localedir = $(datadir)/locale
gettextsrcdir = $(datadir)/gettext/intl
aliaspath = $(localedir)
subdir = intl

INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
MKINSTALLDIRS = ./mkinstalldirs
mkinstalldirs = $(SHELL) `case "$(MKINSTALLDIRS)" in /*) echo "$(MKINSTALLDIRS)" ;; *) echo "$(top_builddir)/$(MKINSTALLDIRS)" ;; esac`

l = 

AR = ar
CC = cc
LIBTOOL = @LIBTOOL@
RANLIB = ranlib
YACC = bison -y -d
YFLAGS = --name-prefix=__gettext

DEFS = -DLOCALEDIR=\"$(localedir)\" -DLOCALE_ALIAS_PATH=\"$(aliaspath)\" \
-DLIBDIR=\"$(libdir)\" -DIN_LIBINTL -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DENABLE_HTREE=1 -DENABLE_SWAPFS=1 -DPACKAGE=\"e2fsprogs\" -DVERSION=\"1.34\" -DHAVE_ALLOCA_H=1 -DHAVE_ALLOCA=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_GETPAGESIZE=1 -DHAVE_MMAP=1 -DINTDIV0_RAISES_SIGFPE=1 -DHAVE_INTTYPES_H_WITH_UINTMAX=1 -DHAVE_STDINT_H_WITH_UINTMAX=1 -DHAVE_UNSIGNED_LONG_LONG=1 -DHAVE_INTTYPES_H=1 -DHAVE_ARGZ_H=1 -DHAVE_LIMITS_H=1 -DHAVE_LOCALE_H=1 -DHAVE_NL_TYPES_H=1 -DHAVE_MALLOC_H=1 -DHAVE_STDDEF_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_UNISTD_H=1 -DHAVE_SYS_PARAM_H=1 -DHAVE_FEOF_UNLOCKED=1 -DHAVE_FGETS_UNLOCKED=1 -DHAVE_GETC_UNLOCKED=1 -DHAVE_GETCWD=1 -DHAVE_GETEGID=1 -DHAVE_GETEUID=1 -DHAVE_GETGID=1 -DHAVE_GETUID=1 -DHAVE_MEMPCPY=1 -DHAVE_MUNMAP=1 -DHAVE_PUTENV=1 -DHAVE_SETENV=1 -DHAVE_SETLOCALE=1 -DHAVE_STPCPY=1 -DHAVE_STRCASECMP=1 -DHAVE_STRDUP=1 -DHAVE_STRTOUL=1 -DHAVE_TSEARCH=1 -DHAVE___ARGZ_COUNT=1 -DHAVE___ARGZ_STRINGIFY=1 -DHAVE___ARGZ_NEXT=1 -DHAVE_ICONV=1 -DICONV_CONST= -DHAVE_LANGINFO_CODESET=1 -DHAVE_LC_MESSAGES=1 -DENABLE_NLS=1 -DHAVE_GETTEXT=1 -DHAVE_DCGETTEXT=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_ERRNO_H=1 -DHAVE_MALLOC_H=1 -DHAVE_MNTENT_H=1 -DHAVE_PATHS_H=1 -DHAVE_DIRENT_H=1 -DHAVE_GETOPT_H=1 -DHAVE_SETJMP_H=1 -DHAVE_SIGNAL_H=1 -DHAVE_TERMIOS_H=1 -DHAVE_LINUX_FD_H=1 -DHAVE_LINUX_MAJOR_H=1 -DHAVE_SYS_IOCTL_H=1 -DHAVE_SYS_MOUNT_H=1 -DHAVE_SYS_SOCKET_H=1 -DHAVE_SYS_SYSMACROS_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_RESOURCE_H=1 -DHAVE_NET_IF_H=1 -DHAVE_NETINET_IN_H=1 -DHAVE_VPRINTF=1 -DHAVE_RECLEN_DIRENT=1 -DHAVE_TYPE_SSIZE_T=1 -DHAVE_LSEEK64_PROTOTYPE=1 -DSIZEOF_SHORT=2 -DSIZEOF_INT=4 -DSIZEOF_LONG=4 -DSIZEOF_LONG_LONG=8 -DHAVE_GETRUSAGE=1 -DHAVE_LLSEEK=1 -DHAVE_LSEEK64=1 -DHAVE_OPEN64=1 -DHAVE_STRTOULL=1 -DHAVE_STRCASECMP=1 -DHAVE_SRANDOM=1 -DHAVE_FCHOWN=1 -DHAVE_MALLINFO=1 -DHAVE_FDATASYNC=1 -DHAVE_STRNLEN=1 -DHAVE_STRPTIME=1 -DHAVE_SYSCONF=1 -DHAVE_PATHCONF=1 -DHAVE_POSIX_MEMALIGN=1 -DHAVE_MEMALIGN=1 -DHAVE_VALLOC=1 -DHAVE_DLOPEN=1 -DHAVE_EXT2_IOCTLS=1 
CPPFLAGS = 
CFLAGS = -g -O2
LDFLAGS = 

COMPILE = $(CC) -c $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(XCFLAGS)

HEADERS = $(COMHDRS) libgnuintl.h loadinfo.h
COMHDRS = gmo.h gettextP.h hash-string.h plural-exp.h eval-plural.h os2compat.h
SOURCES = $(COMSRCS) intl-compat.c
COMSRCS = bindtextdom.c dcgettext.c dgettext.c gettext.c \
finddomain.c loadmsgcat.c localealias.c textdomain.c l10nflist.c \
explodename.c dcigettext.c dcngettext.c dngettext.c ngettext.c plural.y \
plural-exp.c localcharset.c localename.c osdep.c os2compat.c
OBJECTS =  bindtextdom.$lo dcgettext.$lo dgettext.$lo gettext.$lo \
finddomain.$lo loadmsgcat.$lo localealias.$lo textdomain.$lo l10nflist.$lo \
explodename.$lo dcigettext.$lo dcngettext.$lo dngettext.$lo ngettext.$lo \
plural.$lo plural-exp.$lo localcharset.$lo localename.$lo osdep.$lo
GETTOBJS = intl-compat.$lo
DISTFILES.common = Makefile.in \
config.charset locale.alias ref-add.sin ref-del.sin $(HEADERS) $(SOURCES)
DISTFILES.generated = plural.c
DISTFILES.normal = VERSION
DISTFILES.gettext = COPYING.LIB-2.0 COPYING.LIB-2.1 libintl.glibc
DISTFILES.obsolete = xopen-msg.sed linux-msg.sed po2tbl.sed.in cat-compat.c \
COPYING.LIB-2 gettext.h libgettext.h plural-eval.c

# Libtool's library version information for libintl.
# Before making a gettext release, the gettext maintainer must change this
# according to the libtool documentation, section "Library interface versions".
# Maintainers of other packages that include the intl directory must *not*
# change these values.
LTV_CURRENT=4
LTV_REVISION=0
LTV_AGE=2

.SUFFIXES:
.SUFFIXES: .c .y .o .lo .sin .sed
.c.o:
	$(COMPILE) $<
.c.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) $<

.y.c:
	$(YACC) $(YFLAGS) --output $@ $<
	rm -f $*.h

.sin.sed:
	sed -e '/^#/d' -e 's/@''PACKAGE''@/e2fsprogs/g' $< > t-$@
	mv t-$@ $@

INCLUDES = -I.. -I. -I$(top_srcdir)/intl

all: all-no
all-yes: libintl.$la libintl.h charset.alias ref-add.sed ref-del.sed
all-no: all-no-no
all-no-yes: libgnuintl.$la
all-no-no:

libintl.a libgnuintl.a: $(OBJECTS)
	rm -f $@
	$(AR) cru $@ $(OBJECTS)
	$(RANLIB) $@

libintl.la libgnuintl.la: $(OBJECTS)
	$(LIBTOOL) --mode=link \
	  $(CC) $(CPPFLAGS) $(CFLAGS) $(XCFLAGS) $(LDFLAGS) -o $@ \
	  $(OBJECTS)  -lc \
	  -version-info $(LTV_CURRENT):$(LTV_REVISION):$(LTV_AGE) \
	  -rpath $(libdir) \
	  -no-undefined

libintl.h: libgnuintl.h
	cp $(srcdir)/libgnuintl.h libintl.h

charset.alias: config.charset
	$(SHELL) $(srcdir)/config.charset 'i686-pc-linux-gnu' > t-$@
	mv t-$@ $@

check: all

# This installation goal is only used in GNU gettext.  Packages which
# only use the library should use install instead.

# We must not install the libintl.h/libintl.a files if we are on a
# system which has the GNU gettext() function in its C library or in a
# separate library.
# If you want to use the one which comes with this version of the
# package, you have to use `configure --with-included-gettext'.
install: install-exec install-data
install-exec: all
	if test "$(PACKAGE)" = "gettext" \
	   && test '' = '$(GETTOBJS)'; then \
	  $(mkinstalldirs) $(DESTDIR)$(libdir) $(DESTDIR)$(includedir); \
	  $(INSTALL_DATA) libintl.h $(DESTDIR)$(includedir)/libintl.h; \
	  $(LIBTOOL) --mode=install \
	    $(INSTALL_DATA) libintl.$la $(DESTDIR)$(libdir)/libintl.$la; \
	else \
	  : ; \
	fi
	if test 'no' = yes; then \
	  test yes != no || $(mkinstalldirs) $(DESTDIR)$(libdir); \
	  temp=$(DESTDIR)$(libdir)/t-charset.alias; \
	  dest=$(DESTDIR)$(libdir)/charset.alias; \
	  if test -f $(DESTDIR)$(libdir)/charset.alias; then \
	    orig=$(DESTDIR)$(libdir)/charset.alias; \
	    sed -f ref-add.sed $$orig > $$temp; \
	    $(INSTALL_DATA) $$temp $$dest; \
	    rm -f $$temp; \
	  else \
	    if test yes = no; then \
	      orig=charset.alias; \
	      sed -f ref-add.sed $$orig > $$temp; \
	      $(INSTALL_DATA) $$temp $$dest; \
	      rm -f $$temp; \
	    fi; \
	  fi; \
	  $(mkinstalldirs) $(DESTDIR)$(localedir); \
	  test -f $(DESTDIR)$(localedir)/locale.alias \
	    && orig=$(DESTDIR)$(localedir)/locale.alias \
	    || orig=$(srcdir)/locale.alias; \
	  temp=$(DESTDIR)$(localedir)/t-locale.alias; \
	  dest=$(DESTDIR)$(localedir)/locale.alias; \
	  sed -f ref-add.sed $$orig > $$temp; \
	  $(INSTALL_DATA) $$temp $$dest; \
	  rm -f $$temp; \
	else \
	  : ; \
	fi
install-data: all
	if test "$(PACKAGE)" = "gettext"; then \
	  $(mkinstalldirs) $(DESTDIR)$(gettextsrcdir); \
	  $(INSTALL_DATA) VERSION $(DESTDIR)$(gettextsrcdir)/VERSION; \
	  $(INSTALL_DATA) ChangeLog.inst $(DESTDIR)$(gettextsrcdir)/ChangeLog; \
	  dists="COPYING.LIB-2.0 COPYING.LIB-2.1 $(DISTFILES.common)"; \
	  for file in $$dists; do \
	    $(INSTALL_DATA) $(srcdir)/$$file \
			    $(DESTDIR)$(gettextsrcdir)/$$file; \
	  done; \
	  chmod a+x $(DESTDIR)$(gettextsrcdir)/config.charset; \
	  dists="$(DISTFILES.generated)"; \
	  for file in $$dists; do \
	    if test -f $$file; then dir=.; else dir=$(srcdir); fi; \
	    $(INSTALL_DATA) $$dir/$$file \
			    $(DESTDIR)$(gettextsrcdir)/$$file; \
	  done; \
	  dists="$(DISTFILES.obsolete)"; \
	  for file in $$dists; do \
	    rm -f $(DESTDIR)$(gettextsrcdir)/$$file; \
	  done; \
	else \
	  : ; \
	fi

install-strip: install

installdirs:
	if test "$(PACKAGE)" = "gettext" \
	   && test '' = '$(GETTOBJS)'; then \
	  $(mkinstalldirs) $(DESTDIR)$(libdir) $(DESTDIR)$(includedir); \
	else \
	  : ; \
	fi
	if test 'no' = yes; then \
	  test yes != no || $(mkinstalldirs) $(DESTDIR)$(libdir); \
	  $(mkinstalldirs) $(DESTDIR)$(localedir); \
	else \
	  : ; \
	fi
	if test "$(PACKAGE)" = "gettext"; then \
	  $(mkinstalldirs) $(DESTDIR)$(gettextsrcdir); \
	else \
	  : ; \
	fi

# Define this as empty until I found a useful application.
installcheck:

install-shlibs:

uninstall:
	if test "$(PACKAGE)" = "gettext" \
	   && test '' = '$(GETTOBJS)'; then \
	  rm -f $(DESTDIR)$(includedir)/libintl.h; \
	  $(LIBTOOL) --mode=uninstall \
	    rm -f $(DESTDIR)$(libdir)/libintl.$la; \
	else \
	  : ; \
	fi
	if test 'no' = yes; then \
	  if test -f $(DESTDIR)$(libdir)/charset.alias; then \
	    temp=$(DESTDIR)$(libdir)/t-charset.alias; \
	    dest=$(DESTDIR)$(libdir)/charset.alias; \
	    sed -f ref-del.sed $$dest > $$temp; \
	    if grep '^# Packages using this file: $$' $$temp > /dev/null; then \
	      rm -f $$dest; \
	    else \
	      $(INSTALL_DATA) $$temp $$dest; \
	    fi; \
	    rm -f $$temp; \
	  fi; \
	  if test -f $(DESTDIR)$(localedir)/locale.alias; then \
	    temp=$(DESTDIR)$(localedir)/t-locale.alias; \
	    dest=$(DESTDIR)$(localedir)/locale.alias; \
	    sed -f ref-del.sed $$dest > $$temp; \
	    if grep '^# Packages using this file: $$' $$temp > /dev/null; then \
	      rm -f $$dest; \
	    else \
	      $(INSTALL_DATA) $$temp $$dest; \
	    fi; \
	    rm -f $$temp; \
	  fi; \
	else \
	  : ; \
	fi
	if test "$(PACKAGE)" = "gettext"; then \
	  for file in VERSION ChangeLog COPYING.LIB-2.0 COPYING.LIB-2.1 $(DISTFILES.common) $(DISTFILES.generated); do \
	    rm -f $(DESTDIR)$(gettextsrcdir)/$$file; \
	  done; \
	else \
	  : ; \
	fi

info dvi:

$(OBJECTS): libgnuintl.h
bindtextdom.$lo dcgettext.$lo dcigettext.$lo dcngettext.$lo dgettext.$lo dngettext.$lo finddomain.$lo gettext.$lo intl-compat.$lo loadmsgcat.$lo localealias.$lo ngettext.$lo textdomain.$lo: gettextP.h gmo.h loadinfo.h
dcigettext.$lo: hash-string.h
explodename.$lo l10nflist.$lo: loadinfo.h
dcigettext.$lo loadmsgcat.$lo plural.$lo plural-exp.$lo: plural-exp.h
dcigettext.$lo: eval-plural.h

tags: TAGS

TAGS: $(HEADERS) $(SOURCES)
	here=`pwd`; cd $(srcdir) && etags -o $$here/TAGS $(HEADERS) $(SOURCES)

id: ID

ID: $(HEADERS) $(SOURCES)
	here=`pwd`; cd $(srcdir) && mkid -f$$here/ID $(HEADERS) $(SOURCES)


mostlyclean:
	rm -f *.a *.la *.o *.lo core core.*
	rm -f libintl.h charset.alias ref-add.sed ref-del.sed
	rm -f -r .libs _libs

clean: mostlyclean

distclean: clean
	rm -f Makefile ID TAGS
	if test "$(PACKAGE)" = gettext; then \
	  rm -f ChangeLog.inst $(DISTFILES.normal); \
	else \
	  : ; \
	fi

maintainer-clean: distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."


# GNU gettext needs not contain the file `VERSION' but contains some
# other files which should not be distributed in other packages.
distdir = ../$(PACKAGE)-$(VERSION)/$(subdir)
dist distdir: Makefile
	if test "$(PACKAGE)" = gettext; then \
	  additional="$(DISTFILES.gettext)"; \
	else \
	  additional="$(DISTFILES.normal)"; \
	fi; \
	$(MAKE) $(DISTFILES.common) $(DISTFILES.generated) $$additional; \
	for file in ChangeLog $(DISTFILES.common) $(DISTFILES.generated) $$additional; do \
	  if test -f $$file; then dir=.; else dir=$(srcdir); fi; \
	  cp -p $$dir/$$file $(distdir); \
	done

Makefile: Makefile.in ../config.status
	cd .. \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
