/*
 * clipsmodule.h
 *
 * common header file for CLIPS python module
 * $Id: clipsmodule.h 340 2008-02-21 00:39:34Z Franz $
 * (c) 2002-2008 - Francesco Garosi/JKS
 */

/* LICENSE INFORMATION

# (c) 2002-2008 Francesco Garosi/JKS
# The author's copyright is expressed through the following notice, thus
# giving effective rights to copy and use this software to anyone, as shown
# in the license text.
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

*/

#ifdef WIN32
#include <windows.h>
#endif

#ifndef _CLIPS_H_
#include <clips.h>
#include <prcdrfun.h>
#include <strngfun.h>
#include <factfun.h>
#include "clips_or.h"
#define _CLIPS_H_
#endif

#ifndef __PYTHON_H_CLIPS__
#include <Python.h>
#define __PYTHON_H_CLIPS__
#endif

#ifndef __SETJMP_H_CLIPS__
#include <setjmp.h>
#define __SETJMP_H_CLIPS__
#endif

#ifdef __cplusplus
#define PYFUNC extern "C"
#else
#define PYFUNC
#endif

#if (CLIPS_MAJOR == 6 && CLIPS_MINOR < 23)
#error "at least CLIPS v6.23 is required"
#endif

/* these are needed to define manifest constants */
#if (CLIPS_MAJOR == 6 && CLIPS_MINOR < 24)
#define NO_DEFAULT 0
#define STATIC_DEFAULT  1
#define DYNAMIC_DEFAULT 2
#endif


/* True/False symbols */
#define CLIPS_TRUE_SYMBOL TrueSymbol()
#define CLIPS_FALSE_SYMBOL FalseSymbol()
#define ECLIPS_TRUE_SYMBOL(_e) EnvTrueSymbol(_e)
#define ECLIPS_FALSE_SYMBOL(_e) EnvFalseSymbol(_e)


/* configuration: initial and default values that could be modified */
#define INITIAL_PPBUFFER_SIZE 8192      /* pretty print buffer size */
#define INITIAL_MAX_ENVIRONMENTS 256    /* maximum number of environments */

/* size of the hash table used to keep track of facts: should be prime */
#define STRAY_FACTS_TABLE_SIZE 9973


/* always inhibit CLIPS garbage collection except when it's forced */
/* #define USE_CLIPSGCLOCK */

/* lock CLIPS garbage collection when there are non-asserted facts around */
#define USE_NONASSERT_CLIPSGCLOCK

/* allow Clear() to reset PyCLIPS garbage collection counters */
#define USE_CLEAR_RESETGCCOUNTERS

/* use 'dummy' unaccessible system objects to force memory release at exit */
#define USE_TEST_DEALLOC_ENV

/* declare some variables as 'register' in hope to speed up */
#define USE_REGISTER_VARIABLES


/* ATTENTION:
 * The following flags only have effect when the 'test' patch set is applied
 * to the CLIPS source (this is enabled by default); otherwise even defining
 * the following symbols does not impact the module functionality.
 */

/* use Python memory allocator in CLIPS too (affects performance) */
#define USE_PYTHON_MEMMGR

/* handle memory errors using setjmp/longjmp (affects performance) */
#define USE_MEMORY_ERROR_HANDLER

/* enable suppression of fatal errors written on stderr (stdout) */
#define USE_FATAL_ERROR_INHIBITION


/* end */
