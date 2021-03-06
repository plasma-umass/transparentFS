# This file is included by the global makefile so that you can add your own
# architecture-specific flags and dependencies. Remember to do have actions
# for "archclean" and "archdep" for cleaning up and making dependencies for
# this architecture
#
# This file is subject to the terms and conditions of the GNU General Public
# License.  See the file "COPYING" in the main directory of this archive
# for more details.
#
# Copyright (C) 1994 by Linus Torvalds
# Changes for PPC by Gary Thomas
# Rewritten by Cort Dougan and Paul Mackerras
# Adjusted for PPC64 by Tom Gall
#

KERNELLOAD	:= 0xc000000000000000

# Set default 32 bits cross compilers for vdso and boot wrapper
CROSS32_COMPILE ?=

CROSS32CC		:= $(CROSS32_COMPILE)gcc
CROSS32AS		:= $(CROSS32_COMPILE)as
CROSS32LD		:= $(CROSS32_COMPILE)ld
CROSS32OBJCOPY		:= $(CROSS32_COMPILE)objcopy

# If we have a biarch compiler, use it for 32 bits cross compile if
# CROSS32_COMPILE wasn't explicitely defined, and add proper explicit
# target type to target compilers

HAS_BIARCH      := $(call cc-option-yn, -m64)
ifeq ($(HAS_BIARCH),y)
ifeq ($(CROSS32_COMPILE),)
CROSS32CC	:= $(CC) -m32
CROSS32AS	:= $(AS) -a32
CROSS32LD	:= $(LD) -m elf32ppc
CROSS32OBJCOPY	:= $(OBJCOPY)
endif
override AS	+= -a64
override LD	+= -m elf64ppc
override CC	+= -m64
endif

export CROSS32CC CROSS32AS CROSS32LD CROSS32OBJCOPY

new_nm := $(shell if $(NM) --help 2>&1 | grep -- '--synthetic' > /dev/null; then echo y; else echo n; fi)

ifeq ($(new_nm),y)
NM		:= $(NM) --synthetic

endif

CHECKFLAGS	+= -m64 -D__powerpc__

LDFLAGS		:= -m elf64ppc
LDFLAGS_vmlinux	:= -Bstatic -e $(KERNELLOAD) -Ttext $(KERNELLOAD)
CFLAGS		+= -msoft-float -pipe -mminimal-toc -mtraceback=none \
		   -mcall-aixdesc

GCC_VERSION     := $(call cc-version)
GCC_BROKEN_VEC	:= $(shell if [ $(GCC_VERSION) -lt 0400 ] ; then echo "y"; fi ;)

ifeq ($(CONFIG_POWER4_ONLY),y)
ifeq ($(CONFIG_ALTIVEC),y)
ifeq ($(GCC_BROKEN_VEC),y)
	CFLAGS += $(call cc-option,-mcpu=970)
else
	CFLAGS += $(call cc-option,-mcpu=power4)
endif
else
	CFLAGS += $(call cc-option,-mcpu=power4)
endif
else
	CFLAGS += $(call cc-option,-mtune=power4)
endif

# Enable unit-at-a-time mode when possible. It shrinks the
# kernel considerably.
CFLAGS += $(call cc-option,-funit-at-a-time)

head-y := arch/ppc64/kernel/head.o

libs-y				+= arch/ppc64/lib/
core-y				+= arch/ppc64/kernel/
core-y				+= arch/ppc64/mm/
core-$(CONFIG_XMON)		+= arch/ppc64/xmon/
drivers-$(CONFIG_OPROFILE)	+= arch/ppc64/oprofile/

boot := arch/ppc64/boot

boottarget-$(CONFIG_PPC_PSERIES) := zImage zImage.initrd
boottarget-$(CONFIG_PPC_MAPLE) := zImage zImage.initrd
boottarget-$(CONFIG_PPC_ISERIES) := vmlinux.sminitrd vmlinux.initrd vmlinux.sm
boottarget-$(CONFIG_PPC_BPA) := zImage zImage.initrd
$(boottarget-y): vmlinux
	$(Q)$(MAKE) $(build)=$(boot) $(boot)/$@

bootimage-$(CONFIG_PPC_PSERIES) := $(boot)/zImage
bootimage-$(CONFIG_PPC_PMAC) := vmlinux
bootimage-$(CONFIG_PPC_MAPLE) := $(boot)/zImage
bootimage-$(CONFIG_PPC_BPA) := zImage
bootimage-$(CONFIG_PPC_ISERIES) := vmlinux
BOOTIMAGE := $(bootimage-y)
install: vmlinux
	$(Q)$(MAKE) $(build)=$(boot) BOOTIMAGE=$(BOOTIMAGE) $@

defaultimage-$(CONFIG_PPC_PSERIES) := zImage
defaultimage-$(CONFIG_PPC_PMAC) := vmlinux
defaultimage-$(CONFIG_PPC_MAPLE) := zImage
defaultimage-$(CONFIG_PPC_ISERIES) := vmlinux
KBUILD_IMAGE := $(defaultimage-y)
all: $(KBUILD_IMAGE)

archclean:
	$(Q)$(MAKE) $(clean)=$(boot)

prepare: include/asm-ppc64/offsets.h

arch/ppc64/kernel/asm-offsets.s: include/asm include/linux/version.h \
				   include/config/MARKER

include/asm-ppc64/offsets.h: arch/ppc64/kernel/asm-offsets.s
	$(call filechk,gen-asm-offsets)

define archhelp
  echo  '* zImage       - Compressed kernel image (arch/$(ARCH)/boot/zImage)'
  echo  '  zImage.initrd- Compressed kernel image with initrd attached,'
  echo  '		   sourced from arch/$(ARCH)/boot/ramdisk.image.gz'
  echo  '		   (arch/$(ARCH)/boot/zImage.initrd)'
endef

CLEAN_FILES += include/asm-ppc64/offsets.h
