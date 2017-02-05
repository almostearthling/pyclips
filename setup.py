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


__revision__ = "$Id: setup.py 342 2008-02-22 01:17:23Z Franz $"
print "Module 'clips': Python to CLIPS interface"
print "Setup revision: %s" % __revision__


# the following values generate the version number and some of them
#  are modified via an automatic process; version information has been
#  made this detailed in order to help setuptools to better decide
#  whether or not to replace an existing package
PYCLIPS_MAJOR = 1
PYCLIPS_MINOR = 0
PYCLIPS_PATCHLEVEL = 7
PYCLIPS_INCREMENTAL = 343
PYCLIPS_VERSION = "%s.%s.%s.%s" % (
    PYCLIPS_MAJOR,
    PYCLIPS_MINOR,
    PYCLIPS_PATCHLEVEL,
    PYCLIPS_INCREMENTAL)


from glob import glob
import os, sys, re, tempfile


# remove unwanted patch sets from this list (not recommended)
APPLY_PATCHSETS = [
    'ia64',     # ia_64 related fixes
    'bgfx',     # official patches to the CLIPS source
    'test',     # "experimental" (unofficial) but useful patches
    ]

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


#define VOID     void
#define VOID_ARG void
#define STD_SIZE size_t

#define intBool int
#define globle

#define ALLOW_ENVIRONMENT_GLOBALS 1
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
#define EXTENDED_MATH_FUNCTIONS 1
#define FACT_SET_QUERIES 1
#define HELP_FUNCTIONS 0
#define INSTANCE_SET_QUERIES 1
#define IO_FUNCTIONS 1
#define MULTIFIELD_FUNCTIONS 1
#define OBJECT_SYSTEM 1
#define PROFILING_FUNCTIONS 1
#define RUN_TIME 0
#define STRING_FUNCTIONS 1
#define TEXTPRO_FUNCTIONS 1
#define WINDOW_INTERFACE 1

#define DEVELOPER 0

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



# find out symbols that are imported from clipsmodule.c: if we keep
#  using this "protocol" for declaring symbols, it could be used also
#  to read other possible C source files; we define IMPORTED_SYMBOLS
#  here just because it is used as global in some functions below
IMPORTED_SYMBOLS = []
def find_imported_symbols(filename):
    f = open(filename)
    li = [x for x in map(str.strip, f.readlines())
          if x.startswith('ADD_MANIFEST_CONSTANT(')
          or x.startswith('MMAP_ENTRY(')]
    f.close()
    li1 = []
    for x in li:
        if x.startswith('ADD_MANIFEST_CONSTANT('):
            x = x.replace('ADD_MANIFEST_CONSTANT(d,', '')
            x = x.replace(');', '').strip()
        else:
            x = x.replace('MMAP_ENTRY(', '')
            x = x.split(',')[0].strip()
        li1.append(x)
    return li1




# parts of the companion module file
MODULE_TEMPLATE = '''\
# _eclips_wrap.py
# environment aware functions for CLIPS, embedded in an Environment class

# (c) 2002-2008 Francesco Garosi/JKS
#  The Author's copyright is expressed through the following notice, thus
#  giving actual rights to copy and use this software to anyone, as expressed
#  in the license text.
#
# NOTICE:
# This software is released under the terms of the GNU Lesser General Public
#  license; a copy of the text has been released with this package (see file
#  license.py), and can be found on the GNU web site, at the following
#  address:
#
#           http://www.gnu.org/copyleft/lesser.html
#
#  Please refer to the license text for any license information. This notice
#  has to be considered part of the license, and should be kept on every copy
#  integral or modified, of the source files. The removal of the reference to
#  the license will be considered an infringement of the license itself.

"""\
clips - high-level interface to the CLIPS engine module
        (c) 2002-2008 Francesco Garosi/JKS
"""

# standard imports
import sys as _sys

import os as  _os
import types as _types

# the low-level module
import _clips as _c


# ========================================================================== #
# globals

# bring the CLIPS Exception object at top level
ClipsError = _c.ClipsError


# redeclare manifest constants here in order to avoid having to
#  reference the ones defined in te low-level module _clips

# check Python version, and issue an exception if not supported
if _sys.version[:3] < "2.4":
    raise _c.ClipsError("M99: Python 2.4 or higher required")


# these globals are redefined instead of reimported for sake of speed
LOCAL_SAVE = _c.LOCAL_SAVE
VISIBLE_SAVE = _c.VISIBLE_SAVE

WHEN_DEFINED = _c.WHEN_DEFINED
WHEN_ACTIVATED = _c.WHEN_ACTIVATED
EVERY_CYCLE = _c.EVERY_CYCLE

NO_DEFAULT = _c.NO_DEFAULT
STATIC_DEFAULT = _c.STATIC_DEFAULT
DYNAMIC_DEFAULT = _c.DYNAMIC_DEFAULT

DEPTH_STRATEGY = _c.DEPTH_STRATEGY
BREADTH_STRATEGY = _c.BREADTH_STRATEGY
LEX_STRATEGY = _c.LEX_STRATEGY
MEA_STRATEGY = _c.MEA_STRATEGY
COMPLEXITY_STRATEGY = _c.COMPLEXITY_STRATEGY
SIMPLICITY_STRATEGY = _c.SIMPLICITY_STRATEGY
RANDOM_STRATEGY = _c.RANDOM_STRATEGY

CONVENIENCE_MODE = _c.CONVENIENCE_MODE
CONSERVATION_MODE = _c.CONSERVATION_MODE


# import adequate symbols from _clips_wrap
from _clips_wrap import Nil, Integer, Float, String, Symbol, InstanceName, \\
                        Multifield, _cl2py, _py2cl, _py2clsyntax, \\
                        ClipsIntegerType, ClipsFloatType, ClipsStringType, \\
                        ClipsSymbolType, ClipsInstanceNameType, \\
                        ClipsMultifieldType, ClipsNilType, \\
                        _setStockClasses, _accepts_method, _forces_method, \\
                        AROUND, BEFORE, PRIMARY, AFTER, CLIPS_VERSION



# environment class:
class Environment(object):
    """class representing an environment: implements all global classes"""

%(MEMBER_CLASSES)s

    # constructor possibly sets the "borrowed" flag, to state that this
    #  is a Python class around an existing object: in this case the
    #  underlying CLIPS environment is not attempted to be destroyed on
    #  deletion
    def __init__(self, o=None):
        """environment constructor"""
        if o is None:
            self.__env = _c.createEnvironment()
            self.__borrowed = False
        else:
            if _c.isEnvironment(o):
                self.__env = o
                self.__borrowed = True
            else:
                raise TypeError("invalid argument for constructor")
%(CLASS_INIT)s
        # if o is not None, then this is an internal object and its status
        #  should not be modified by the user, nor the stock objects be
        #  accessible for direct inspection or subclassing (as this could
        #  be the current environment and might be corrupted)
        if o is None:
            self.EngineConfig = self._clips_Status()
            self.DebugConfig = self._clips_Debug()

%(MEMBER_FUNCTIONS)s

    def __del__(self):
        """environment destructor"""
        if not self.__borrowed:
            try:
                _c.destroyEnvironment(self.__env)
            except ClipsError:
                pass

    def __repr__(self):
        """representation of environment, borrowed by underlying object"""
        return "<Environment: " + repr(self.__env)[1:-1] + ">"

    def __property_getIndex(self):
        return _c.getEnvironmentIndex(self.__env)
    Index = property(__property_getIndex, None, None,
                     "Return index of this Environment")

    def SetCurrent(self):
        """Make this Environment the current Environment"""
        _c.setCurrentEnvironment(self.__env)
        _setStockClasses()



# A function that returns current Environment
def CurrentEnvironment():
    """Return current Environment"""
    cenv = _c.getCurrentEnvironment()
    env = Environment(cenv)
    env.EngineConfig = env._clips_Status()
    env.DebugConfig = env._clips_Debug()
    return env



# end.
'''


# this is what all the functions in the low-level module look like
func_re = re.compile(r"(_c\.\w+\()")
func_re_NA = re.compile(r"(_c\.\w+\(\))")
class_re = re.compile(r"^class\ ")
def_re = re.compile(r"def\ (\w+\()")
def_re_NA = re.compile(r"def\ (\w+\(\))")


# ========================================================================== #

ALL_CLASSES = {}
ALL_FUNCTIONS = {}

def useful_line(s):
    s1 = s.strip()
    return bool(s1 and not s1.startswith('#'))

def remove_leading_underscore(s):
    while s[0] == "_":
        s = s[1:]
    return s

# read the module (which contains the special class/function markers) and
#  extract all top-level functions and classes to be put in Environment
def _i_read_module(f):
    global ALL_CLASSES, ALL_FUNCTIONS
    li = f.readlines()
    for i in range(len(li)):
        l = li[i]
        if l.strip() == "#{{CLASS":
            while not li[i].strip().startswith('class'):
                i += 1
            classname = li[i].split('(', 1)[0].replace('class', '').strip()
            text = [li[i]]
            i += 1
            while li[i].strip() != "#}}":
                if useful_line(li[i]):
                    text.append(li[i])
                i += 1
            ALL_CLASSES[classname] = text
        elif l.strip() == "#{{FUNCTION":
            text = []
            while not li[i].strip().startswith('def'):
                if li[i].strip().startswith('@'):
                    text.append(li[i])
                i += 1
            funcname = li[i].split('(', 1)[0].replace('def', '').strip()
            text.append(li[i])
            i += 1
            while li[i].strip() != "#}}":
                if useful_line(li[i]):
                    text.append(li[i])
                i += 1
            ALL_FUNCTIONS[funcname] = text
        i += 1

# hack to convert direct instantiations of classes in return values
def _i_convert_classinstantiation_c(s):
    if s.split()[0] == 'class':
        return s
    for x in ALL_CLASSES.keys():
        s = s.replace(" %s(" % x, " self.__envobject.%s(" % x)
        s = s.replace("(%s(" % x, "(self.__envobject.%s(" % x)
    return s

def _i_convert_classinstantiation(s):
    if s.split()[0] == 'class':
        return s
    for x in ALL_CLASSES.keys():
        s = s.replace(" %s(" % x, " self.%s(" % x)
        s = s.replace("(%s(" % x, "(self.%s(" % x)
    return s


# convert a single line of code and return it indented
def _i_convert_line(s, cvtdef=False):
    if s.strip() == "":
        return ""
    if s.strip()[0] == "#":
        return ""
    s1 = s
    if not s1.strip().startswith("def "):
        s1 = s1.replace(" _cl2py(", " self._cl2py(")
        s1 = s1.replace(" _py2cl(", " self._py2cl(")
        s1 = s1.replace("(_cl2py(", "(self._cl2py(")
        s1 = s1.replace("(_py2cl(", "(self._py2cl(")
    if s1.strip().startswith("@"):
        s1 = s1.replace("@_accepts(", "@_accepts_method(")
        s1 = s1.replace("@_forces(", "@_forces_method(")
    mo = func_re.search(s1)
    if mo:
        li = mo.groups()
        for x in li:
            t = x[3:-1]
            if "env_%s" % t in IMPORTED_SYMBOLS:
                if func_re_NA.search(s1):
                    s1 = s1.replace(x, "_c.env_%s(self.__env" % t)
                else:
                    s1 = s1.replace(x, "_c.env_%s(self.__env, " % t)
    if cvtdef:
        mo = def_re.search(s1)
        if mo:
            if def_re_NA.search(s1):
                s1 = def_re.sub(mo.group(0) + "self", s1)
            else:
                s1 = def_re.sub(mo.group(0) + "self, ", s1)
        for x in ALL_CLASSES.keys():
            s1 = s1.replace(" %s(" % x, " self.%s(" % x)
            s1 = s1.replace(" %s:" % x, " self.%s:" % x)
            s1 = s1.replace("(%s(" % x, "(self.%s(" % x)
    return INDENT + _i_convert_classinstantiation(s1)

def _i_convert_classline(s):
    if s.strip() == "":
        return ""
    if s.strip()[0] == "#":
        return ""
    s1 = s
    if not s1.strip().startswith("def "):
        s1 = s1.replace(" _cl2py(", " self.__envobject._cl2py(")
        s1 = s1.replace(" _py2cl(", " self.__envobject._py2cl(")
        s1 = s1.replace("(_cl2py(", "(self.__envobject._cl2py(")
        s1 = s1.replace("(_py2cl(", "(self.__envobject._py2cl(")
    if s1.strip().startswith("@"):
        s1 = s1.replace("@_accepts(", "@_accepts_method(")
        s1 = s1.replace("@_forces(", "@_forces_method(")
    mo = func_re.search(s1)
    if mo:
        li = mo.groups()
        for x in li:
            t = x[3:-1]
            if "env_%s" % t in IMPORTED_SYMBOLS:
                if func_re_NA.search(s1):
                    s1 = s1.replace(x, "_c.env_%s(self.__env" % t)
                else:
                    s1 = s1.replace(x, "_c.env_%s(self.__env, " % t)
    return INDENT + INDENT + _i_convert_classinstantiation_c(s1)

# convert an entire class, provided as a list of lines
def _i_convert_fullclass(name, li):
    li1 = li[1:]
    docs = []
    while li1[0].strip()[0] in ['"', "'"]:
        docs.append(li1[0])
        li1 = li1[1:]
    head1 = INDENT + "def %s(self, private_environment):\n" % name
    head2 = INDENT + INDENT + "environment_object = self\n"
    head3 = INDENT + INDENT + "class %s(object):\n" % name
    head4 = INDENT + INDENT + INDENT + "__env = private_environment\n"
    head5 = INDENT + INDENT + INDENT + "__envobject = environment_object\n"
    foot1 = INDENT + INDENT + "return %s\n"  % name
    return [head1, head2, head3] + map(_i_convert_classline, docs) \
       + [head4, head5] + map(_i_convert_classline, li1) + [foot1]

# convert an entire function, provided as a list of lines
def _i_convert_fullfunction(name, li):
    return map(lambda x: _i_convert_line(x, True), li)

# create the list of lines that build inner classes in Environment.__init__
def _i_create_inner_classes():
    inner = []
    kclasses = ALL_CLASSES.keys()
    kclasses.sort()
    for x in kclasses:
        inner.append(
            INDENT + INDENT + "self.%s = self.%s(self.__env)\n" % (x, x))
    return inner


# macro to convert all the read module
def convert_module(filename):
    f = open(filename)
    _i_read_module(f)
    f.close()
    classes = []
    kclasses = ALL_CLASSES.keys()
    kclasses.sort()
    for x in kclasses:
        classes += _i_convert_fullclass(x, ALL_CLASSES[x])
    initclasses = _i_create_inner_classes()
    functions = []
    kfunctions = ALL_FUNCTIONS.keys()
    kfunctions.sort()
    for x in kfunctions:
        functions += _i_convert_fullfunction(x, ALL_FUNCTIONS[x])
    return MODULE_TEMPLATE % {
        'MEMBER_CLASSES': "".join(classes),
        'CLASS_INIT': "".join(initclasses),
        'MEMBER_FUNCTIONS': "".join(functions),
        }

# ========================================================================== #


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


# actually build "companion" module: first we calculate the list of
#  all symbols that are imported from the low-level module, and then
#  we read the environment-unaware high-level module to build the
#  environment-aware part using the above helpers
sys.stdout.write("finding low-level module symbols... ")
IMPORTED_SYMBOLS = find_imported_symbols(_p('.', 'clipsmodule.c'))
sys.stdout.write("Done!\nbuilding environment-aware submodule... ")
s = convert_module(_p('clips', '_clips_wrap.py'))
f = open(_p('clips', '_eclips_wrap.py'), 'w')
f.write(s)
f.close()
sys.stdout.write("Done!\n")


# retrieve used CLIPS version
clips_version = get_clips_version(_p("clipssrc", "constant.h"))
print "Found CLIPS version: %s" % clips_version
maj, min = clips_version.split('.', 1)
CFLAGS = [
    '-DPYCLIPS',
    '-DCLIPS_MAJOR=%s' % maj,
    '-DCLIPS_MINOR=%s' % min,
    '-DPYCLIPS_MAJOR=%s' % PYCLIPS_MAJOR,
    '-DPYCLIPS_MINOR=%s' % PYCLIPS_MINOR,
    '-DPYCLIPS_PATCHLEVEL=%s' % PYCLIPS_PATCHLEVEL,
    '-DPYCLIPS_INCREMENTAL=%s' % PYCLIPS_INCREMENTAL,
    '-DWIN_MVC'
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
i, o, e = os.popen3("patch --version")
vs = o.read()
o.close()
e.close()
if vs:
    print "'patch' utility found, applying selected patchsets..."
    import shutil
    def apply_patchset(ps):
        print "Applying patchset '%s':" % ps
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
                if not os.system(patchcmd):
                    print "ok."
                else:
                    print "FAILED"
    for x in APPLY_PATCHSETS:
        apply_patchset(x)


# create the version submodule
sys.stdout.write("Creating version number: ")
f = open(_p('clips', '_version.py'), 'w')
f.write("""# version number
version_string = "%s"
version = (%s, %s, %s, %s)
""" % (PYCLIPS_VERSION, PYCLIPS_MAJOR,
       PYCLIPS_MINOR, PYCLIPS_PATCHLEVEL,
       PYCLIPS_INCREMENTAL))
f.close()

# start setup
print "Standard setup in progress:"


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
        print "Using setuptools instead of distutils..."
        if not uses_gcc:
            print warn_default_gcc
    except:
        from distutils.core import setup, Extension
else:
    from distutils.core import setup, Extension


setup(name="pyclips",
      version=PYCLIPS_VERSION,
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
