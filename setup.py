#!/usr/bin/env python
# setup.py
# setup program for clips module

# (c) 2002-2008 Francesco Garosi/JKS
#  The author's copyright is expressed through the following notice, thus
#  giving effective rights to copy and use this software to anyone, as shown
#  in the license text.
#
# NOTICE:
#  This software is released under the terms of the GNU Lesser General Public
#  license; a copy of the text has been released with this package (see file
#  _license.py, where the license text also appears), and can be found on the
#  GNU web site, at the following address:
#
#           http://www.gnu.org/copyleft/lesser.html
#
#  Please refer to the license text for any license information. This notice
#  has to be considered part of the license, and should be kept on every copy
#  integral or modified, of the source files. The removal of the reference to
#  the license will be considered an infringement of the license itself.


"""PyCLIPS
A Python module to interface the CLIPS expert system shell library."""


from glob import glob
import os, sys, re, tempfile
from subprocess import *    # we require version >= 2.4, which supports it

# selective definitions per Python version
PYTHON_VERSION = sys.version[:3]


# this printing function is compatible with python 2.x and 3.0
def write(s):
    sys.stdout.write("%s\n" % s)
        

__revision__ = "$Id: setup.py 371 2008-12-24 00:47:54Z Franz $"
write("Module 'clips': Python to CLIPS interface")
write("Setup revision: %s" % __revision__)


# this is used to build the version number according to beta state 
BETA = True

# the following values generate the version number and some of them
#  are modified via an automatic process; version information has been
#  made this detailed in order to help setuptools to better decide
#  whether or not to replace an existing package
PYCLIPS_MAJOR = 2
PYCLIPS_MINOR = 0
PYCLIPS_PATCHLEVEL = 0
PYCLIPS_INCREMENTAL = 369

if BETA:
    PYCLIPS_VERSION = "%s.%sb_%s" % (
        PYCLIPS_MAJOR,
        PYCLIPS_MINOR,
        PYCLIPS_INCREMENTAL)
else:
    PYCLIPS_VERSION = "%s.%s.%s.%s" % (
        PYCLIPS_MAJOR,
        PYCLIPS_MINOR,
        PYCLIPS_PATCHLEVEL,
        PYCLIPS_INCREMENTAL)


# remove unwanted patch sets from this list (not recommended)
APPLY_PATCHSETS = [
    'ia64',     # ia_64 related fixes
    'bgfx',     # official patches to the CLIPS source
    'test',     # "experimental" (unofficial) but useful patches
    ]

# this will be used to download CLIPS source if not found
CLIPS_SRC_URL = "http://pyclips.sourceforge.net/files/CLIPSSrc.zip"

# standard indentation for conversion (4 spaces)
INDENT = " " * 4


# a shortcut, since it appears many times
_p = os.path.join

# find Clips source directory and zip file
zl, zd = os.listdir("."), {}
for x in zl:
    zd[x.lower()] = x
try:
    ClipsSrcZIP = _p('.', zd['clipssrc.zip'])
except:
    ClipsSrcZIP = "CLIPSSrc.zip"    # will obviously produce an error
ClipsLIB_dir = _p('.', 'clipssrc')
ClipsPATCH_dir = _p('.', 'optpatch')


# make a new setup.h based on current platform (note: this file is a copy
#  of the original setup.h file, and is subject to the license terms that
#  can be found in all the original CLIPS source files). This is the only
#  chunk of code which may not be subject to the same license as the others
#  (although all of this code is released under an Open Source license)
setup_h_templ = """\
/*************************************************************/
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/* Contributing Programmer(s):                               */
/* Revision History:                                         */
/*************************************************************/
#ifndef _H_setup
#define _H_setup

#define GENERIC 0
#define UNIX_V  %s
#define UNIX_7  0
#define MAC_MCW 0
#define IBM_MCW 0
#define IBM_MSC %s
#define IBM_TBC 0
#define IBM_GCC %s

#define IBM_ZTC 0
#define IBM_ICB 0
#define IBM_SC  0
#define MAC_SC6 0
#define MAC_SC7 0
#define MAC_SC8 0
#define MAC_MPW 0
#define VAX_VMS 0
#define MAC_XCD 0

#if IBM_ZTC || IBM_MSC || IBM_TBC || IBM_ICB || IBM_SC || IBM_MCW
#define IBM 1
#else
#define IBM 0
#endif

#if MAC_SC6 || MAC_SC7 || MAC_SC8
#define MAC_SC 1
#else
#define MAC_SC 0
#endif

#if MAC_SC || MAC_MPW || MAC_MCW || MAC_XCD
#define MAC 1
#else
#define MAC 0
#endif


#if CLIPS_MAJOR >= 6

#if CLIPS_MINOR < 24
#error "Cannot build using CLIPS version less than 6.24"
#endif /* CLIPS_MINOR < 24 */

#define VOID     void
#define VOID_ARG void
#define STD_SIZE size_t

#define intBool int

#define globle

#define ALLOW_ENVIRONMENT_GLOBALS 1
#define BASIC_IO 1
#define BLOAD 0
#define BLOAD_AND_BSAVE 1
#define BLOAD_ONLY 0
#define BLOAD_INSTANCES 1
#define BLOCK_MEMORY 0
#define BSAVE_INSTANCES 1
#define CONFLICT_RESOLUTION_STRATEGIES 1
#define CONSTRUCT_COMPILER 0
#define DEBUGGING_FUNCTIONS 1
#define DEFFACTS_CONSTRUCT 1
#define DEFFUNCTION_CONSTRUCT 1
#define DEFGENERIC_CONSTRUCT 1
#define DEFGLOBAL_CONSTRUCT 1
#define DEFINSTANCES_CONSTRUCT 1
#define DEFMODULE_CONSTRUCT 1
#define DEFRULE_CONSTRUCT 1
#define DEFTEMPLATE_CONSTRUCT 1
#define EMACS_EDITOR 0
#define ENVIRONMENT_API_ONLY 1
#define EX_MATH 1
#define EXT_IO 1
#define FACT_SET_QUERIES 1
#define HELP_FUNCTIONS 0
#define INSTANCE_SET_QUERIES 1
#define MULTIFIELD_FUNCTIONS 1
#define OBJECT_SYSTEM 1
#define PROFILING_FUNCTIONS 1
#define RUN_TIME 0
#define STRING_FUNCTIONS 1
#define TEXTPRO_FUNCTIONS 1
#define WINDOW_INTERFACE 1

#define DEVELOPER 0

#else   /* CLIPS_MAJOR < 6 */
#error "Cannot build using CLIPS version less than 6.24"
#endif

#include "envrnmnt.h"

#define Bogus(x)
#define PrintCLIPS(x,y) EnvPrintRouter(GetCurrentEnvironment(),x,y)
#define GetcCLIPS(x,y) EnvGetcRouter(GetCurrentEnvironment(),x)
#define UngetcCLIPS(x,y) EnvUngetcRouter(GetCurrentEnvironment(),x,y)
#define ExitCLIPS(x) EnvExitRouter(GetCurrentEnvironment(),x)
#define CLIPSSystemError(x,y) SystemError(x,y)
#define CLIPSFunctionCall(x,y,z) FunctionCall(x,y,z)
#define InitializeCLIPS() InitializeEnvironment()
#define WCLIPS WPROMPT
#define CLIPSTrueSymbol SymbolData(GetCurrentEnvironment())->TrueSymbol
#define CLIPSFalseSymbol SymbolData(GetCurrentEnvironment())->FalseSymbol
#define EnvCLIPSTrueSymbol(theEnv) SymbolData(theEnv)->TrueSymbol
#define EnvCLIPSFalseSymbol(theEnv) SymbolData(theEnv)->FalseSymbol
#define CLIPS_FALSE 0
#define CLIPS_TRUE 1

#if BLOCK_MEMORY
#define INITBLOCKSIZE 32000
#define BLOCKSIZE 32000
#endif

#include "usrsetup.h"

#endif  /* _H_setup */
"""





# This retrieves the CLIPS version looking up headers
def get_clips_version(fn):
    li = open(fn).readlines()
    cre = re.compile(r"\#define\ VERSION_STRING\ \"([0-9]+\.[0-9]+)\"")
    vno = "unknown"
    for s in li:
        f = cre.search(s)
        if f:
            vno = f.groups(1)
            break
    return "%s" % vno



# Actual distutils setup routine

# check for CLIPS library files, and eventually uncompress them from
#  the CLIPS source - to be found at CLIPS website. In case no ZIP
#  file is even present here, notice the user that it has to be
#  downloaded and put here, in the directory where this file resides
nozip_notice = """\
ERROR: setup.py could not find the CLIPS source files. These files
       are necessary to build the CLIPS interface module: please
       download the archive (%s) and copy it to the directory
       where setup.py resides. The setup program will take care of
       unpacking the necessary source files in an appropriate place.
"""

badzip_notice = """\
ERROR: setup.py could not read one or more file(s) from the source
       archive. Please provide a good copy of the archive (%s).
"""


# this is used to normalize end-of-line characters across systems, since we
#  use the ZIP version of the archive (which is for Win32/DOS systems). The
#  use of TemporaryFile is a hack, but so we can read lines letting Python
#  do the dirty job of removing bad EOLs.
def normalize_eols(t):
    def _remove_badchars(x):
        t = ""
        x = x.replace('\t', " " * 8)
        for c in x:
            if ord(c) >= 32 and ord(c) < 128:
                t += c
        return t
    tf = tempfile.TemporaryFile()
    tf.write(t)
    tf.seek(0)
    li = tf.readlines()
    tf.close()
    li = map(lambda x: x.rstrip(), li)
    li = map(_remove_badchars, li)
    return li


if not os.path.exists(ClipsLIB_dir):
    if not os.path.exists(ClipsSrcZIP):
        # try to download file from official site
        import urllib
        write("CLIPS source archive (%s) not found, " \
              "trying to download it for you..." % ClipsSrcZIP)
        try:
            f = urllib.urlopen(CLIPS_SRC_URL)
            s = f.read()
            if not s:
                raise   # anyway we'll answer that the source wasn't found
            f = open(ClipsSrcZIP, 'wb')
            f.write(s)
            f.close()
            write("Download successful, continuing build.")
            write("Please review CLIPS license in the downloaded ZIP file!")
        except:
            write("Download FAILED!")
            write(nozip_notice % ClipsSrcZIP)
            sys.exit(2)
    import zipfile
    try:
        write("Opening CLIPS source archive (%s)..." % ClipsSrcZIP)
        zf = zipfile.ZipFile(ClipsSrcZIP)
        os.mkdir(ClipsLIB_dir)
        li = zf.namelist()
        for x in li:
            n = _p(ClipsLIB_dir, os.path.basename(x))
            if n.endswith('.h') or n.endswith('.c'):
                sys.stdout.write("\tExtracting %s... " % n)
                li = normalize_eols(zf.read(x))
                f = open(n, 'w')
                for t in li:
                    f.write("%s\n" % t)
                f.close()
                sys.stdout.write("done.\n")
        zf.close()
        write("All CLIPS source files extracted, continuing build.")
    except zipfile.error:
        write(badzip_notice % ClipsSrcZIP)
        sys.exit(2)
    except:
        write(nozip_notice % ClipsSrcZIP)
        sys.exit(2)



# ----------------------------------------------------------------------------
# -- 8-< -- CUT FROM HERE --------------------------------------------- >-8 --

bad_platform = """\
setup.py: setup provided only for posix/win32 systems
NOTE: If you want to provide your own setup.h file, please remove the lines
      between the CUT ... HERE markers and edit your setup.h file using the
      original file (setup.h or setup.h.m_ORIG). Please be cautious doing so,
      since the module expects all the constructs and functions defined in
      the ad-hoc configuration file to be available: modifying setup.h by
      yourself may lead to an inconsistent module or compilation errors.
"""
if sys.platform == "win32":
    defines = (0, 1, 0)
elif os.name == "posix":
    defines = (0, 0, 1)
else:
    sys.stderr.write(bad_platform)
    sys.exit(2)

setup_h = setup_h_templ % defines

# rename setup.h to setup.h.ORIG to keep a copy of original file; if the
#  copy already exists, we are supposed to already have built the new
#  CLIPS setup file, thus we do not
setup_orig_name = _p(ClipsLIB_dir, 'setup.h')
setup_copy_name = _p(ClipsLIB_dir, 'setup.h.m_ORIG')
if os.path.exists(setup_orig_name) and not os.path.exists(setup_copy_name):
    sys.stdout.write("copying setup.h to setup.h.m_ORIG, making new header...")
    os.rename(setup_orig_name, setup_copy_name)
    f = open(setup_orig_name, 'w')
    f.write(setup_h)
    f.close()
    sys.stdout.write("Done.\n")

# -- 8-< -- CUT UP TO HERE -------------------------------------------- >-8 --
# ----------------------------------------------------------------------------


# here we remove the (exit) function from CLIPS to prevent aborting Python
routerc_orig_name = _p(ClipsLIB_dir, 'router.c')
routerc_copy_name = _p(ClipsLIB_dir, 'router.c.m_ORIG')
if os.path.exists(routerc_orig_name) and not os.path.exists(routerc_copy_name):
    sys.stdout.write("copying router.c to router.c.m_ORIG, making new source...")
    f = open(routerc_orig_name)
    li = f.readlines()
    f.close()
    os.rename(routerc_orig_name, routerc_copy_name)
    badline_re = re.compile(
        "\s*EnvDefineFunction2.+exit.+ExitCommand.+ExitCommand.+")
    f = open(routerc_orig_name, 'w')
    for l in li:
        if badline_re.match(l):
            f.write("/* INTENTIONALLY SKIPPED */\n")
            f.write("#ifndef PYCLIPS\n")
            f.write(l)
            f.write("#endif /* PYCLIPS */\n")
        else:
            f.write(l)
    f.close()
    sys.stdout.write("Done.\n")


# remove some source files as they interfere with clipsmodule.c
TO_REMOVE = [
    'main.c',
    'userfunctions.c',
    ]

all_clipssrc = glob(_p(ClipsLIB_dir, '*.c'))
main_clipssrc = ['clipsmodule.c', 'clips_or.c']
for x in all_clipssrc:
    if os.path.basename(x) in TO_REMOVE:
        all_clipssrc.remove(x)


# retrieve used CLIPS version
clips_version = get_clips_version(_p("clipssrc", "constant.h"))
write("Found CLIPS version: %s" % clips_version)
maj, min = clips_version.split('.', 1)
CFLAGS = [
    '-D_CRT_SECURE_NO_WARNINGS',
    '-DPYCLIPS',
    '-DCLIPS_MAJOR=%s' % maj,
    '-DCLIPS_MINOR=%s' % min,
    '-DPYCLIPS_MAJOR=%s' % PYCLIPS_MAJOR,
    '-DPYCLIPS_MINOR=%s' % PYCLIPS_MINOR,
    '-DPYCLIPS_PATCHLEVEL=%s' % PYCLIPS_PATCHLEVEL,
    '-DPYCLIPS_INCREMENTAL=%s' % PYCLIPS_INCREMENTAL,
    ]


# if we are using GCC we must set an extra option; for now we assume that
#  GCC is used unless, on Win32, compiler is explicitly specified as one
#  of 'unix', 'mingw32', 'cygwin'; note that this will force to *always*
#  specify compiler even when using setuptools and the default compiler
#  is set to be one of the above; maybe one day I will discover a better
#  way to determine which compiler is used without having to rewrite all
#  of distutils
if sys.platform == 'win32':
    try:
        find_compiler = sys.argv.index('-c') + 1
    except ValueError:
        find_compiler = 0
    uses_gcc = (
        (find_compiler and
            find_compiler < len(sys.argv) and
            sys.argv[find_compiler] in (
                'unix', 'mingw32', 'cygwin')) or
        ('--compiler=unix' in sys.argv) or
        ('--compiler=mingw32' in sys.argv) or
        ('--compiler=cygwin' in sys.argv))
else:
    uses_gcc = True
if uses_gcc:
    CFLAGS.append('-fno-strict-aliasing')


# apply "optional" patches
o = Popen("patch --version", shell=True, stdout=PIPE).stdout
vs = o.read()
o.close()
if vs:
    write("'patch' utility found, applying selected patchsets...")
    import shutil
    def apply_patchset(ps):
        write("Applying patchset '%s':" % ps)
        pattern = "*.[ch]-??.v%s-%s.diff" % (clips_version, ps)
        for x in glob(_p(ClipsPATCH_dir, pattern)):
            pfn = os.path.basename(x)
            sourcefile = pfn.split('-', 1)[0]
            if not os.path.exists(_p(ClipsLIB_dir, "%s.ORIG" % sourcefile)):
                sys.stdout.write(
                    "patching %s (original in %s.ORIG)... " % (
                        sourcefile, sourcefile))
                shutil.copy(
                    _p(ClipsLIB_dir, sourcefile),
                    _p(ClipsLIB_dir, "%s.ORIG" % sourcefile)
                    )
                patchcmd = "patch -l -s -p0 %s < %s" % (
                    _p(ClipsLIB_dir, sourcefile), x)
                p = Popen(patchcmd, shell=True)
                if not os.waitpid(p.pid, 0):
                    write("ok.")
                else:
                    write("FAILED")
    for x in APPLY_PATCHSETS:
        apply_patchset(x)


# create the version submodule
sys.stdout.write("Creating version number: ")
f = open(_p('clips', '_version.py'), 'w')
if BETA:
    f.write("""# version number
version_string = "%s"
version = (%s, %s, "BETA", %s)
""" % (PYCLIPS_VERSION, PYCLIPS_MAJOR,
       PYCLIPS_MINOR,
       PYCLIPS_INCREMENTAL))
else:
    f.write("""# version number
version_string = "%s"
version = (%s, %s, %s, %s)
""" % (PYCLIPS_VERSION, PYCLIPS_MAJOR,
       PYCLIPS_MINOR, PYCLIPS_PATCHLEVEL,
       PYCLIPS_INCREMENTAL))
f.close()

# start setup
write("Standard setup in progress:")


# The following is a warning to users of ez_setup when using GCC (for
# instance MinGW) and it is set as the default compiler, since it is
# difficult for me to detect which compiler is used.
warn_default_gcc = """
WARNING: if you are using setuptools and GCC (eg. MinGW) as your default
         compiler, it has not been detected. This can lead to unexpected
         behaviours such as crashes. If you experience such behaviours,
         please rebuild the module specifying, for instance, the option
         '--compiler=mingw32' on the setup command line.
"""

# possibly use ez_setup/setuptools, or standard distutils if not possible
# and if not using a debug executable (this is mainly because it would
# pollute MY setup): the following way to discover under what circumstances
# we are running is definitely hideous
DEBUGGING = bool(
    sys.executable.endswith('_d')
    or sys.executable.endswith('_d.exe'))
if not DEBUGGING:
    try:
        import ez_setup
        ez_setup.use_setuptools()
        from setuptools import setup, Extension
        write("Using setuptools instead of distutils...")
        if not uses_gcc:
            write(warn_default_gcc)
    except:
        from distutils.core import setup, Extension
else:
    from distutils.core import setup, Extension


setup(name="pyclips",
      version="%s-clips_%s" % (PYCLIPS_VERSION, clips_version),
      description="Python CLIPS interface",
      long_description=__doc__,
      author="Francesco Garosi",
      author_email="franzg@users.sourceforge.net",
      url="http://pyclips.sourceforge.net",
      packages=['clips'],
      ext_modules=[
          Extension('clips._clips',
                    main_clipssrc + all_clipssrc,
                    extra_compile_args=CFLAGS,
                    include_dirs=[ClipsLIB_dir]),
          ],
     )


# end.
