/* clipsmodule.c
 *
 * Python module to embed CLIPS into Python.
 * $Id: clipsmodule.c 345 2008-02-22 17:44:54Z Franz $
 * (c) 2002-2008 - Francesco Garosi/JKS
 */

/* LICENSE INFORMATION

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

*/


#include "clipsmodule.h"


/* some configuration values that should generally not be changed */
#define MIN_PPBUFFER_SIZE 256   /* minimum pretty print buffer size */


/* the module documentation string, in perfect Python style */
static char clips__doc__[] =
"_clips - a low-level portable interface to the CLIPS system for Python\n"
"         (c) 2002-2008 Francesco Garosi/JKS";

/* the release string, in case it should be used someday */
static char *clips__revision__ =
    "$Id: clipsmodule.c 345 2008-02-22 17:44:54Z Franz $";


/* module scope exception */
PyObject *PyExc_ClipsError = NULL;
PyObject *PyExc_ClipsMemoryError = NULL;


/* module scope router dictionary */
static PyDictObject *clips_Streams = NULL;


/* module scope functions dictionary */
static PyDictObject *clips_PythonFunctions = NULL;


/* literal helpers */

/* this is to fail uniformly, assuming there is a "fail:" label */
#define FAIL() do { goto fail; } while(0)
#define BEGIN_FAIL fail:
#define END_FAIL return NULL;

/* to add manifest integer constants to module dictionary */
#define ADD_MANIFEST_CONSTANT(dict, name) \
    PyDict_SetItemString((dict), #name, PyInt_FromLong((long)(name)))

/* I always define this */
#define SKIP()

/* this also might be useful */
#define TRACE(s) fprintf(stderr, s "\n");

/* perform all the job to return the None object and other standard things */
#define RETURN_PYOBJECT(_p) do { return (PyObject *)(_p); } while(0)
#define RETURN_INT(_i) do { return Py_BuildValue("i", (_i)); } while(0)
#define RETURN_BOOL(_i) do { return Py_BuildValue("i", (_i) ? 1 : 0); } while(0)
#define RETURN_STR(_s) do { return Py_BuildValue("s", (_s)); } while(0)

/* this is not defined for Python < 2.4 */
#ifdef Py_RETURN_NONE
#define RETURN_NONE() Py_RETURN_NONE
#else
#define RETURN_NONE() do { Py_INCREF(Py_None); return Py_None; } while(0)
#endif /* Py_RETURN_NONE */

/* inlining might be useful in some cases */
#ifdef __GNUC__
#define F_INLINE __inline__
#else
#ifdef _MSC_VER
#define F_INLINE __inline
#else
#define F_INLINE
#endif /* _MSC_VER */
#endif /* __GNUC__ */


/* some common strings to save space */
static char _error_object_creation[] = "P01: object could not be created";
/* static char _error_clips_generic[] = "C00: generic clips error"; */ /* NOT USED */
static char _error_clips_creation[] = "C01: system object could not be created";
static char _error_clips_notfound[] = "C02: construct or object could not be found";
static char _error_clips_readonly[] = "C03: object cannot be modified";
static char _error_clips_fileio[] = "C04: could not open file";
static char _error_clips_noenv[] = "C05: could not get current environment";
static char _error_clips_retval[] = "C06: unable to retrieve value";
static char _error_clips_parsefile[] = "C07: unable to parse file";
static char _error_clips_parseexpr[] = "C08: syntax error, or unable to parse expression";
static char _error_clips_parsearg[] = "C09: unable to understand argument";
static char _error_clips_evalexpr[] = "C10: unable to evaluate expression";
static char _error_clips_remove[] = "C11: could not remove construct or object";
static char _error_clips_assert[] = "C12: could not assert";
/* static char _error_clips_beyond[] = "C13: beyond last element"; */ /* NOT USED */
static char _error_clips_funccall[] = "C14: unsuccessful function call";
static char _error_clips_reassert[] = "C15: cannot reassert or modify fact";
static char _error_clips_unimplemented[] = "C98: unimplemented feature/function";
static char _error_clips_impossible[] = "C99: could not perform operation";

/* static char _error_clipssys_generic[] = "S00: generic engine error"; */ /* NOT USED */
static char _error_clipssys_garbfact[] = "S01: fact does not exist anymore";
static char _error_clipssys_garbinstance[] = "S02: instance does not exist anymore";
static char _error_clipssys_envnoclear[] = "S03: environment could not be cleared";
static char _error_clipssys_badenv[] = "S04: environment is invalid";
static char _error_clipssys_curenv[] = "S05: aliasing current environment might corrupt system";
static char _error_clipssys_maxenv[] = "S06: maximum number of environments reached";
static char _error_clipssys_cleanup[] = "S07: cannot force cleanup while rules are executing";

/* static char _error_clipsmem_generic[] = "X00: generic memory error"; */ /* NOT USED */
static char _error_clipsmem_out[] = "X01: out of memory, system may be inconsistent";

static char _error_router_invalid[] = "R01: invalid logical buffer operation";
static char _error_router_nostream[] = "R02: logical buffer not found";
static char _error_router_readonly[] = "R03: buffer is read-only";

/* common helpers for common exceptions */
#define ERROR_CLIPS(s) PyErr_SetString(PyExc_ClipsError, (s))
#define ERROR_CLIPS_MEMORY(s) PyErr_SetString(PyExc_ClipsMemoryError, (s))
#define ERROR_VALUE(s) PyErr_SetString(PyExc_ValueError, (s))
#define ERROR_TYPE(s) PyErr_SetString(PyExc_TypeError, (s))
#define ERROR_IO(s) PyErr_SetString(PyExc_IOError, (s))
#define ERROR_MEMORY(s) PyErr_SetString(PyExc_MemoryError, (s))

#define ERROR_MEMORY_CREATION() ERROR_MEMORY(_error_object_creation)
#define ERROR_UNIMPLEMENTED() ERROR_CLIPS(_error_clips_unimplemented)
#define ERROR_CLIPS_IMPOSSIBLE() ERROR_CLIPS(_error_clips_impossible)
#define ERROR_CLIPS_GENERIC() ERROR_CLIPS(_error_clips_generic)
#define ERROR_CLIPS_CREATION() ERROR_CLIPS(_error_clips_creation)
#define ERROR_CLIPS_NOTFOUND() ERROR_CLIPS(_error_clips_notfound)
#define ERROR_CLIPS_READONLY() ERROR_CLIPS(_error_clips_readonly)
#define ERROR_CLIPS_IO() ERROR_IO(_error_clips_fileio)
#define ERROR_CLIPS_NOENV() ERROR_CLIPS(_error_clips_noenv)
#define ERROR_CLIPS_RETVAL() ERROR_CLIPS(_error_clips_retval)
#define ERROR_CLIPS_PARSEF() ERROR_CLIPS(_error_clips_parsefile)
#define ERROR_CLIPS_PARSEX() ERROR_CLIPS(_error_clips_parseexpr)
#define ERROR_CLIPS_PARSEA() ERROR_CLIPS(_error_clips_parsearg)
#define ERROR_CLIPS_EVALX() ERROR_CLIPS(_error_clips_evalexpr)
#define ERROR_CLIPS_REMOVE() ERROR_CLIPS(_error_clips_remove)
#define ERROR_CLIPS_ASSERT() ERROR_CLIPS(_error_clips_assert)
#define ERROR_CLIPS_FUNCCALL() ERROR_CLIPS(_error_clips_funccall)
#define ERROR_CLIPS_REASSERT() ERROR_CLIPS(_error_clips_reassert)
#define ERROR_CLIPS_OTHER(s) ERROR_CLIPS("C90: " s)

#define ERROR_CLIPSSYS_GENERIC() ERROR_CLIPS(_error_clipssys_generic)
#define ERROR_CLIPSSYS_GARBFACT() ERROR_CLIPS(_error_clipssys_garbfact)
#define ERROR_CLIPSSYS_GARBINSTANCE() ERROR_CLIPS(_error_clipssys_garbinstance)
#define ERROR_CLIPSSYS_ENVUNINIT() ERROR_CLIPS(_error_clipssys_envuninit)
#define ERROR_CLIPSSYS_ENVNOCLEAR() ERROR_CLIPS(_error_clipssys_envnoclear)
#define ERROR_CLIPSSYS_BADENV() ERROR_CLIPS(_error_clipssys_badenv)
#define ERROR_CLIPSSYS_CURENV() ERROR_CLIPS(_error_clipssys_curenv)
#define ERROR_CLIPSSYS_MAXENV() ERROR_CLIPS(_error_clipssys_maxenv)
#define ERROR_CLIPSSYS_CLEANUP() ERROR_CLIPS(_error_clipssys_cleanup)

#define ERROR_CLIPSMEM_OUT() ERROR_CLIPS_MEMORY(_error_clipsmem_out)

#define ERROR_ROUTER_INVALID() ERROR_CLIPS(_error_router_invalid)
#define ERROR_ROUTER_NOSTREAM() ERROR_CLIPS(_error_router_nostream)
#define ERROR_ROUTER_READONLY() ERROR_CLIPS(_error_router_readonly)


/* unimplemented function builder */
#define UNIMPLEMENT(s, sn) \
    static char sn##__doc__[] = \
    "unimplemented feature/function"; \
    static PyObject *sn(PyObject *_self, PyObject *_args) { \
        ERROR_UNIMPLEMENTED(); \
        return NULL; \
    }

/* invalid CLIPS version function builder: the error string is hardcoded
 * in order to avoid a warning
 */
#define UNIMPLEMENT_VERSION(s, sn) \
    static char sn##__doc__[] = \
    "higher engine version required for feature/function"; \
    static PyObject *sn(PyObject *_self, PyObject *_args) { \
        ERROR_CLIPS("C97: higher engine version required"); \
        return NULL; \
    }

/* simplify writing entries in the method map */
#define MMAP_ENTRY(s, sn) { #s, sn, METH_VARARGS, sn##__doc__ }

/* check for no argument and fail if not */
#define CHECK_NOARGS(_a) do { \
    if(!PyArg_ParseTuple((_a), "")) FAIL(); } while(0)

#ifdef ALLOW_CURRENT_ENVIRONMENT_ALIASING
#define CHECK_NOCURENV(_e)
#else
/* check for environment not to be current and fail if it is */
#define CHECK_NOCURENV(_e) do { \
        if(clips_environment_value(_e) == GetCurrentEnvironment()) { \
            ERROR_CLIPSSYS_CURENV(); \
            FAIL(); \
        } \
    } while(0)
#endif /* ALLOW_CURRENT_ENVIRONMENT_ALIASING */


/* Macros to enable/disable memory checking on a per-function basis:
 *  note that the second macro must always be used when the first is
 *  used, and immediately after each memory allocating function has
 *  been called. A good point to call the RELEASE part is just before
 *  returning control to Python, while a good point to call ACQUIRE
 *  is at the beginning of the memory allocating function.
 */
#ifdef USE_MEMORY_ERROR_HANDLER
#define ACQUIRE_MEMORY_ERROR() do { \
        if(setjmp(env_OutOfMemory)) { \
            env_OutOfMemory_isSet = FALSE; \
            ERROR_CLIPSMEM_OUT(); \
            FAIL(); \
        } else \
            env_OutOfMemory_isSet = TRUE; \
    } while(0)
#define RELEASE_MEMORY_ERROR() env_OutOfMemory_isSet = FALSE
#else
#define ACQUIRE_MEMORY_ERROR()
#define RELEASE_MEMORY_ERROR()
#endif /* USE_MEMORY_ERROR_HANDLER */


/* status function builder */
#define FUNC_GET_ONLY(_py, _sn, _api, _type) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        int _i = 0; \
        CHECK_NOARGS(_args); \
        ACQUIRE_MEMORY_ERROR(); \
        _i = _api(); \
        RELEASE_MEMORY_ERROR(); \
        return Py_BuildValue(_type, _i); \
        BEGIN_FAIL \
        END_FAIL \
    }
#define STATUS_FUNC_GET FUNC_GET_ONLY
#define STATUS_FUNC_GET_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "() -> bool\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        int _i = 0; \
        CHECK_NOARGS(_args); \
        ACQUIRE_MEMORY_ERROR(); \
        _i = _api(); \
        RELEASE_MEMORY_ERROR(); \
        return Py_BuildValue("i", _i ? 1 : 0); \
        BEGIN_FAIL \
        END_FAIL \
    }

#define STATUS_FUNC_SET_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "(bool)"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        PyObject *_o = NULL; \
        if(!PyArg_ParseTuple(_args, "O", &_o)) \
            FAIL(); \
        ACQUIRE_MEMORY_ERROR(); \
        _api(PyObject_IsTrue(_o)); \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }

#define FUNC_VOID_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        CHECK_NOARGS(_args); \
        ACQUIRE_MEMORY_ERROR(); \
        if(!_api()) { \
            RELEASE_MEMORY_ERROR(); \
            ERROR_CLIPS(_error_clips_impossible); \
            FAIL(); \
        } \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }
#define FUNC_VOID_ONLY(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        CHECK_NOARGS(_args); \
        ACQUIRE_MEMORY_ERROR(); \
        _api(); \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }


/* environment-version macros */
/* status function builder */
#define E_FUNC_GET_ONLY(_py, _sn, _api, _type) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        clips_EnvObject *_env = NULL; \
        int _i = 0; \
        if(!PyArg_ParseTuple(_args, "O!", &clips_EnvType, &_env)) \
            FAIL(); \
        ACQUIRE_MEMORY_ERROR(); \
        _i = _api(clips_environment_value(_env)); \
        RELEASE_MEMORY_ERROR(); \
        return Py_BuildValue(_type, _i); \
        BEGIN_FAIL \
        END_FAIL \
    }
#define E_STATUS_FUNC_GET E_FUNC_GET_ONLY
#define E_STATUS_FUNC_GET_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "() -> bool\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        clips_EnvObject *_env = NULL; \
        int _i = 0; \
        if(!PyArg_ParseTuple(_args, "O!", &clips_EnvType, &_env)) \
            FAIL(); \
        ACQUIRE_MEMORY_ERROR(); \
        _i = _api(clips_environment_value(_env)); \
        RELEASE_MEMORY_ERROR(); \
        return Py_BuildValue("i", _i ? 1 : 0); \
        BEGIN_FAIL \
        END_FAIL \
    }

#define E_STATUS_FUNC_SET_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "(bool)"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        PyObject *_o = NULL; \
        clips_EnvObject *_env = NULL; \
        if(!PyArg_ParseTuple(_args, "O!O", &clips_EnvType, &_env, &_o)) \
            FAIL(); \
        CHECK_NOCURENV(_env); \
        ACQUIRE_MEMORY_ERROR(); \
        _api(clips_environment_value(_env), PyObject_IsTrue(_o)); \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }

#define E_FUNC_VOID_BOOL(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        clips_EnvObject *_env = NULL; \
        if(!PyArg_ParseTuple(_args, "O!", &clips_EnvType, &_env)) \
            FAIL(); \
        CHECK_NOCURENV(_env); \
        ACQUIRE_MEMORY_ERROR(); \
        if(!_api(clips_environment_value(_env))) { \
            ERROR_CLIPS(_error_clips_impossible); \
            RELEASE_MEMORY_ERROR(); \
            FAIL(); \
        } \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }
#define E_FUNC_VOID_ONLY(_py, _sn, _api) \
    static char _sn##__doc__[] = \
    "" #_py "()\nequivalent of C API " #_api "()"; \
    static PyObject *_sn(PyObject *_self, PyObject *_args) { \
        clips_EnvObject *_env = NULL; \
        if(!PyArg_ParseTuple(_args, "O!", &clips_EnvType, &_env)) \
            FAIL(); \
        CHECK_NOCURENV(_env); \
        ACQUIRE_MEMORY_ERROR(); \
        _api(clips_environment_value(_env)); \
        RELEASE_MEMORY_ERROR(); \
        RETURN_NONE(); \
        BEGIN_FAIL \
        END_FAIL \
    }


/* macros used to verify if objects are being garbaged */
#define CHECK_VALID_FACT(_o) do { \
        if(!FactExistp(clips_fact_value(_o))) { \
            ERROR_CLIPSSYS_GARBFACT(); \
            FAIL(); \
        } \
    } while(0)
#define ENV_CHECK_VALID_FACT(_e, _o) do { \
        if(!EnvFactExistp(_e, clips_fact_value(_o))) { \
            ERROR_CLIPSSYS_GARBFACT(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_VALID_INSTANCE(_o) do { \
        if(!ValidInstanceAddress(clips_instance_value(_o))) { \
            ERROR_CLIPSSYS_GARBINSTANCE(); \
            FAIL(); \
        } \
    } while(0)
#define ENV_CHECK_VALID_INSTANCE(_e, _o) do { \
        if(!EnvValidInstanceAddress(_e, clips_instance_value(_o))) { \
            ERROR_CLIPSSYS_GARBINSTANCE(); \
            FAIL(); \
        } \
    } while(0)


/* the following can be helpful when allocating objects */
#ifdef USE_PYTHON_MEMMGR
#ifdef NEW
#undef NEW
#endif /* NEW */
#define NEW(x) PyMem_New(x, 1)
#ifdef NEW_ARRAY
#undef NEW_ARRAY
#endif /* NEW_ARRAY */
#define NEW_ARRAY(x, n) PyMem_New(x, (n))
#ifdef DELETE
#undef DELETE
#endif /* DELETE */
#define DELETE(x) PyMem_Del(x)
#ifdef REALLOC
#undef REALLOC
#endif /* REALLOC */
#define REALLOC(p, n) PyMem_Realloc((p), (n))
#ifdef MALLOC
#undef MALLOC
#endif /* MALLOC */
#define MALLOC(s) PyMem_Malloc(s)
#ifdef FREE
#undef FREE
#endif /* FREE */
#define FREE(p) PyMem_Free(p)
#else
#ifdef NEW
#undef NEW
#endif /* NEW */
#define NEW(x) (((x) *)malloc(sizeof(x)))
#ifdef NEW_ARRAY
#undef NEW_ARRAY
#endif /* NEW_ARRAY */
#define NEW_ARRAY(x, n) ((x *)malloc((n) * sizeof(x)))
#ifdef DELETE
#undef DELETE
#endif /* DELETE */
#define DELETE(x) free(x)
#ifdef REALLOC
#undef REALLOC
#endif /* REALLOC */
#define REALLOC(p, n) realloc((p), (n))
#ifdef MALLOC
#undef MALLOC
#endif /* MALLOC */
#define MALLOC(s) malloc(s)
#ifdef FREE
#undef FREE
#endif /* FREE */
#define FREE(p) free(p)
#endif


#ifdef BYTE
#undef BYTE
#define BYTE unsigned char
#endif


/* the boolean values */
#ifndef BOOL
#define BOOL int
#define TRUE 1
#define FALSE 0
#endif

/* maximum supported references */
#define MAX_REFERENCES (INT_MAX - 1)

/* position of stray fact pointer in CLIPS environment */
#ifdef USE_NONASSERT_CLIPSGCLOCK
#define STRAYFACTS_DATA (USER_ENVIRONMENT_DATA + 0)
#endif /* USE_NONASSERT_CLIPSGCLOCK */

/* length of buffers used for string operations */
static size_t ppbuffer_size = INITIAL_PPBUFFER_SIZE;

/* flag to state whether or not fatal environment errors must be shown */
static BOOL clips_ShowEnvironmentFatalErrors = FALSE;


/* Part One: module internals and helpers */


/* The following jump point is used to ensure memory error handling */
#ifdef USE_MEMORY_ERROR_HANDLER
static jmp_buf env_OutOfMemory;
static BOOL env_OutOfMemory_isSet = FALSE;
#endif /* USE_MEMORY_ERROR_HANDLER */


/* This function should be used instead of the standard memory allocator
 *  within CLIPS in order to let memory errors to be correctly reported by
 *  Python instead of exiting abnormally. This function is tightly coupled
 *  with the "out of memory function" implemented here, that jumps back to
 *  the above defined jump point.
 */
void *PyCLIPS_Malloc(size_t s) {
#ifdef USE_MEMORY_ERROR_HANDLER
    void *p = MALLOC(s);
    if(!p && env_OutOfMemory_isSet)
        longjmp(env_OutOfMemory, 1);
    return p;
#else
    return MALLOC(s);
#endif /* USE_MEMORY_ERROR_HANDLER */
}
void PyCLIPS_Free(void *p) {
    FREE(p);
}


/* The following function queries the above defined flag to test whether or
 *  not to show fatal environment errors on stderr (on a patched CLIPS source,
 *  of course: the unpatched source will have the usual behaviour to write a
 *  message on stdout) and is called directly by CLIPS before writing anything.
 */
int PyCLIPS_EnableFatal(void) {
#ifdef USE_FATAL_ERROR_INHIBITION
    return clips_ShowEnvironmentFatalErrors;
#else
    return TRUE;
#endif /* USE_FATAL_ERROR_INHIBITION */
}


/* THE FOLLOWING ARE DEFINED HERE AS IN CLIPS main.c */

/*********************************************************/
/* UserFunctions: Informs the expert system environment  */
/*   of any user defined functions. In the default case, */
/*   there are no user defined functions. To define      */
/*   functions, either this function must be replaced by */
/*   a function with the same name within this file, or  */
/*   this function can be deleted from this file and     */
/*   included in another file.                           */
/*********************************************************/
void UserFunctions(void) { }


/***********************************************************/
/* EnvUserFunctions: Informs the expert system environment */
/*   of any user defined functions. In this case, there is */
/*   only a function which invokes real Python functions.  */
/***********************************************************/
void EnvPythonExternalCall(void *, DATA_OBJECT_PTR);
void EnvUserFunctions(void *env) {
    EnvDefineFunction2(env, "python-call", 'u', PTIEF EnvPythonExternalCall,
                       "EnvPythonExternalCall", "1*uw");
}


/* NOTE:
 *  The following literally included file contains a rough implementation
 *  of an hash-table driven method of storing pointers in order to possibly
 *  apply a function to all contained elements. This is done in order to
 *  avoid access violations when accessing fact objects after a call to the
 *  CLIPS Clear() function. The behaviour of CLIPS in this occasion is known
 *  and not considered a bug; however there is no way to determine whether
 *  or not a fact has still valid slots or not. Keeping track of which facts
 *  have still not been asserted seemed the only way to determine when they
 *  could be corrupted. Of course this is a time consuming operation, but
 *  handling it via the structure define in "loptr.c" seemed a way to keep
 *  the time overhead as small as possible. Most of the "time waste" occurs
 *  when either Assert() or CreateFact() or Clear() are called.
 */
#ifdef USE_NONASSERT_CLIPSGCLOCK
#include "loptr.c"
#endif /* USE_NONASSERT_CLIPSGCLOCK */



/* a Python type representing a standalone CLIPS environment */
staticforward PyTypeObject clips_EnvType;

typedef struct {
    PyObject_HEAD
    void *value;
    BOOL valid;
#ifdef USE_NONASSERT_CLIPSGCLOCK
    unsigned long clips_NotAssertedFacts;
    BOOL clips_GCLocked;
    LOPTR_HASH_TABLE(clips_StrayFacts);
#endif /* USE_NONASSERT_CLIPSGCLOCK */
} clips_EnvObject;

#define clips_environment_value(v) (((clips_EnvObject *)(v))->value)
#define clips_environment_valid(v) (((clips_EnvObject *)(v))->valid)
#define clips_environment_check(v) \
    (((v)->ob_type == &clips_EnvType) && clips_environment_valid(v))

#ifdef USE_NONASSERT_CLIPSGCLOCK
#define clips_environment_New(p) \
    do { \
        p = PyObject_New(clips_EnvObject, &clips_EnvType); \
        if(p) { \
            clips_environment_valid(p) = TRUE; \
            p->clips_NotAssertedFacts = 0;\
            p->clips_GCLocked = FALSE; \
            INIT_LOPTR_HASH_TABLE(p->clips_StrayFacts); \
        } \
    } while(0)
#define CLEAR_LOST_FACTS() LOPTR_reset_hash_table(clips_StrayFacts)
#define ENV_CLEAR_LOST_FACTS(_pe) \
    LOPTR_reset_hash_table((_pe)->clips_StrayFacts)
#else
#define clips_environment_New(p) \
    do { \
        p = PyObject_New(clips_EnvObject, &clips_EnvType); \
        if(p) \
            clips_environment_valid(p) = TRUE; \
    } while(0)
#define CLEAR_LOST_FACTS()
#define ENV_CLEAR_LOST_FACTS(_pe)
#endif /* USE_NONASSERT_CLIPSGCLOCK */

static void clips_EnvObject_dealloc(PyObject *self) {
    /* only the Python object is destroyed: environments are forever */
    ENV_CLEAR_LOST_FACTS((clips_EnvObject *)self);
    PyObject_Del(self);
}

static PyTypeObject clips_EnvType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "environment",
    sizeof(clips_EnvObject),
    0,
    clips_EnvObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS deftemplate object */
staticforward PyTypeObject clips_DeftemplType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DeftemplObject;

#define clips_deftemplate_check(v) ((v)->ob_type == &clips_DeftemplType)
#define clips_deftemplate_value(v) (((clips_DeftemplObject *)(v))->value)

#define clips_deftemplate_New(p) \
    p = PyObject_New(clips_DeftemplObject, &clips_DeftemplType)

static void clips_DeftemplObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DeftemplType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "deftemplate",
    sizeof(clips_DeftemplObject),
    0,
    clips_DeftemplObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS fact object */
staticforward PyTypeObject clips_FactType;

typedef struct {
    PyObject_HEAD
    BOOL readonly;
    BOOL locked;
    void *value;
    void *creation_env;
#ifdef USE_NONASSERT_CLIPSGCLOCK
    BOOL lost;
#endif /* USE_NONASSERT_CLIPSGCLOCK */
} clips_FactObject;

#define clips_fact_check(v) ((v)->ob_type == &clips_FactType)
#define clips_fact_value(v) (((clips_FactObject *)(v))->value)
#define clips_fact_env(v) (((clips_FactObject *)(v))->creation_env)
#define clips_fact_readonly(v) (((clips_FactObject *)(v))->readonly)
#define clips_fact_verify(v) (!((struct fact *) \
                              (((clips_FactObject *)(v))->value))->garbage)

/* assign a pointer to a CLIPS fact to this object */
#define clips_fact_assign(e, p) do { \
        EnvIncrementFactCount(clips_fact_env(e), p); \
        clips_fact_value(e) = p; \
    } while(0)

/* lock/unlock a fact object */
#define clips_fact_locked(v) (((clips_FactObject *)(v))->locked)
#define clips_fact_lock(v) do { \
        ((clips_FactObject *)(v))->locked = TRUE; \
    } while(0)
#define clips_fact_unlock(v) do { \
        ((clips_FactObject *)(v))->locked = FALSE; \
    } while(0)

/* facts behave differently than instances upon call to Clear */
#ifdef USE_NONASSERT_CLIPSGCLOCK
#define clips_fact_lost(v) (((clips_FactObject *)(v))->lost)
#define clips_fact_New(e, p) do { \
        p = PyObject_New(clips_FactObject, &clips_FactType); \
        clips_fact_readonly(p) = FALSE; \
        clips_fact_lost(p) = FALSE; \
        clips_fact_env(p) = e; \
        clips_fact_value(p) = NULL; \
        clips_fact_locked(p) = FALSE; \
    } while(0)

/* This function will be called for every not asserted fact upon clear:
 *  it has to be a function (and not an inlineable one) because its address
 *  will be passed to a function that calls it for each fact that is
 *  supposedly lost. Moreover, although we actually don't need it, the
 *  function has to be a BOOL()(void *), since this is the accepted type
 *  for the final calling function.
 */
static BOOL lose_fact(void *p) {
    clips_fact_lost(p) = TRUE;  /* explicitly casts to clips_FactObject */
    return TRUE;
}

#else
#define clips_fact_New(e, p) do { \
        p = PyObject_New(clips_FactObject, &clips_FactType); \
        clips_fact_readonly(p) = FALSE; \
        clips_fact_env(p) = e; \
        clips_fact_value(p) = NULL; \
        clips_fact_locked(p) = FALSE; \
    } while(0)
#endif /* USE_NONASSERT_CLIPSGCLOCK */

#ifdef USE_NONASSERT_CLIPSGCLOCK
/* allow to assert facts without losing defined but non asserted ones */

static unsigned long clips_NotAssertedFacts = 0;
static BOOL clips_GCLocked = FALSE;
LOPTR_HASH_TABLE(clips_StrayFacts) = { 0 };

F_INLINE void clips_lock_gc(clips_EnvObject *pyenv) {
    if(pyenv) {
        if(!pyenv->clips_GCLocked && pyenv->clips_NotAssertedFacts > 0) {
            EnvIncrementGCLocks(clips_environment_value(pyenv));
            pyenv->clips_GCLocked = TRUE;
        }
    } else {
        if(!clips_GCLocked && clips_NotAssertedFacts > 0) {
            IncrementGCLocks();
            clips_GCLocked = TRUE;
        }
    }
}
F_INLINE void clips_unlock_gc(clips_EnvObject *pyenv) {
    if(pyenv) {
        if(pyenv->clips_GCLocked && pyenv->clips_NotAssertedFacts == 0) {
            pyenv->clips_GCLocked = FALSE;
            EnvDecrementGCLocks(clips_environment_value(pyenv));
        }
    } else {
        if(clips_GCLocked && clips_NotAssertedFacts == 0) {
            clips_GCLocked = FALSE;
            DecrementGCLocks();
        }
    }
}
F_INLINE BOOL add_FactObject_lock(clips_EnvObject *pyenv) {
    if(pyenv)
        pyenv->clips_NotAssertedFacts++;
    else
        clips_NotAssertedFacts++;
    return TRUE;
}
F_INLINE BOOL remove_FactObject_lock(clips_EnvObject *pyenv) {
    if(pyenv) {
        if(pyenv->clips_NotAssertedFacts > 0) {
            pyenv->clips_NotAssertedFacts--;
            return TRUE;
        }
    } else {
        if(clips_NotAssertedFacts > 0) {
            clips_NotAssertedFacts--;
            return TRUE;
        }
    }
    return FALSE;
}
F_INLINE BOOL reset_FactObject_lock(clips_EnvObject *pyenv) {
    if(pyenv) {
        if(pyenv->clips_NotAssertedFacts > 0) {
            pyenv->clips_NotAssertedFacts = 0;
            if(pyenv->clips_GCLocked) {
                pyenv->clips_GCLocked = FALSE;
                EnvDecrementGCLocks(clips_environment_value(pyenv));
            }
            return TRUE;
        }
    } else {
        if(clips_NotAssertedFacts > 0) {
            clips_NotAssertedFacts = 0;
            if(clips_GCLocked) {
                clips_GCLocked = FALSE;
                DecrementGCLocks();
            }
            return TRUE;
        }
    }
    return FALSE;
}

/* through the loptr.c utility we can check whether or not a fact is lost */
#define APPEND_HASH_FACT(_p) LOPTR_append(clips_StrayFacts, (void *)(_p))
#define REMOVE_HASH_FACT(_p) LOPTR_remove(clips_StrayFacts, (void *)(_p))
#define LOSE_HASH_FACTS() LOPTR_apply(clips_StrayFacts, lose_fact)
#define ENV_APPEND_HASH_FACT(_pe, _p) \
    LOPTR_append((_pe)->clips_StrayFacts, (void *)(_p))
#define ENV_REMOVE_HASH_FACT(_pe, _p) \
    LOPTR_remove((_pe)->clips_StrayFacts, (void *)(_p))
#define ENV_LOSE_HASH_FACTS(_pe) \
    LOPTR_apply((_pe)->clips_StrayFacts, lose_fact)
/* the following is needed for facts to auto-deregister */
#define SPEC_REMOVE_HASH_FACT(_sfl, _p) LOPTR_remove((_sfl), (void *)(_p))

/* check if a fact is lost: the ENV version is for completeness only */
#define CHECK_LOST_FACT(_o) do { \
        if(clips_fact_lost(_o)) { \
            ERROR_CLIPSSYS_GARBFACT(); \
            FAIL(); \
        } \
    } while(0)
#define ENV_CHECK_LOST_FACT(_e, _o) do { \
        if(clips_fact_lost(_o)) { \
            ERROR_CLIPSSYS_GARBFACT(); \
            FAIL(); \
        } \
    } while(0)

/* and these are needed to inform the system about creation or assertion */
#define ADD_NONASSERTED_FACT() add_FactObject_lock(NULL)
#define REMOVE_JUSTASSERTED_FACT() remove_FactObject_lock(NULL)
#define ENV_ADD_NONASSERTED_FACT(_pe) add_FactObject_lock(_pe)
#define ENV_REMOVE_JUSTASSERTED_FACT(_pe) remove_FactObject_lock(_pe)

#define RESET_ASSERTED_FACTS() reset_FactObject_lock(NULL)
#define ENV_RESET_ASSERTED_FACTS(_pe) reset_FactObject_lock(_pe)

#define CLIPS_LOCK_GC() clips_lock_gc(NULL)
#define CLIPS_UNLOCK_GC() clips_unlock_gc(NULL)
#define ENV_CLIPS_LOCK_GC(_pe) clips_lock_gc(_pe)
#define ENV_CLIPS_UNLOCK_GC(_pe) clips_unlock_gc(_pe)

#define COPY_ADDITIONAL_ENVIRONMENT_DATA(_pe) do \
    { \
        clips_NotAssertedFacts = (_pe)->clips_NotAssertedFacts; \
        clips_GCLocked = (_pe)->clips_GCLocked; \
        COPY_LOPTR_HASH_TABLE(clips_StrayFacts, (_pe)->clips_StrayFacts); \
    } while(0)
#define INJECT_ADDITIONAL_ENVIRONMENT_DATA(_pe) do \
    { \
        (_pe)->clips_NotAssertedFacts = clips_NotAssertedFacts; \
        (_pe)->clips_GCLocked = clips_GCLocked; \
        COPY_LOPTR_HASH_TABLE((_pe)->clips_StrayFacts, clips_StrayFacts); \
    } while(0)

#else

/* symbols to always force that garbage collector in CLIPS to be locked */
#ifdef USE_CLIPSGCLOCK
#define CLIPS_LOCK_GC() IncrementGCLocks()
#define CLIPS_UNLOCK_GC() DecrementGCLocks()
#define ENV_CLIPS_LOCK_GC(_pe) EnvIncrementGCLocks(clips_environment_value(_pe))
#define ENV_CLIPS_UNLOCK_GC(_pe) EnvDecrementGCLocks(clips_environment_value(_pe))
#else
#define CLIPS_LOCK_GC()
#define CLIPS_UNLOCK_GC()
#define ENV_CLIPS_LOCK_GC(_pe)
#define ENV_CLIPS_UNLOCK_GC(_pe)
#endif /* USE_CLIPSGCLOCK */

#define ADD_NONASSERTED_FACT()
#define REMOVE_JUSTASSERTED_FACT()
#define RESET_ASSERTED_FACTS()
#define ENV_ADD_NONASSERTED_FACT(_pe)
#define ENV_REMOVE_JUSTASSERTED_FACT(_pe)
#define ENV_RESET_ASSERTED_FACTS(_pe)

#define COPY_ADDITIONAL_ENVIRONMENT_DATA(_pe)
#define INJECT_ADDITIONAL_ENVIRONMENT_DATA(_pe)

#define APPEND_HASH_FACT(_p)
#define REMOVE_HASH_FACT(_p)
#define LOSE_HASH_FACTS()
#define ENV_APPEND_HASH_FACT(_pe, _p)
#define ENV_REMOVE_HASH_FACT(_pe, _p)
#define SPEC_REMOVE_HASH_FACT(_sfl, _p)
#define ENV_LOSE_HASH_FACTS(_pe)
#define CHECK_LOST_FACT(_o)
#define ENV_CHECK_LOST_FACT(_e, _o)

#endif /* USE_NONASSERT_CLIPSGCLOCK */

/* The fact removal process might seem tricky. However there is a reason
 *  for this complicated routine: when a fact is created, and before its
 *  assertion, it is put in the fact hash map in order to be invalidated
 *  during actions that would cause a CLIPS garbage collection (see comments
 *  above). Whenever a Python Fact object is destroyed, it has to deregister
 *  itself from the corresponding hash map. Information about the said hash
 *  map is carried along with the CLIPS managed environment, via specifically
 *  allocated data - which just contains a pointer to the corresponding map.
 *  That a fact at the end contains information about its hash map is not a
 *  problem when environments are made current: all data in the Environment
 *  object (note the capital E, for Python corresponding objects) is copied
 *  to the current Environment, including the hash map (clips_StrayFacts),
 *  and everything remains consistent.
 */
static void clips_FactObject_dealloc(PyObject *self) {
    void *p = clips_fact_value(self);
#ifdef USE_NONASSERT_CLIPSGCLOCK
    LOPTR_ITEM ***hm = 
        (LOPTR_ITEM ***)GetEnvironmentData(
            clips_fact_env(self), STRAYFACTS_DATA);
    SPEC_REMOVE_HASH_FACT(*hm, self);
#endif /* USE_NONASSERT_CLIPSGCLOCK */
    if(p)
        EnvDecrementFactCount(clips_fact_env(self), p);
    clips_fact_unlock(self);
    PyObject_Del(self);
}

static PyTypeObject clips_FactType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "fact",
    sizeof(clips_FactObject),
    0,
    clips_FactObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS address object */
staticforward PyTypeObject clips_AddressType;

typedef struct {
    PyObject_HEAD
    int ob_addrtype;
    void *value;
} clips_AddressObject;

#define clips_address_check(v) ((v)->ob_type == &clips_AddressType)
#define clips_address_value(v) (((clips_AddressObject *)(v))->value)
#define clips_address_addrtype(v) (((clips_AddressObject *)(v))->ob_addrtype)

#define clips_address_New(p) do { \
        p = PyObject_New(clips_AddressObject, &clips_AddressType); \
        clips_address_addrtype(p) = 0; \
    } while(0)

static void clips_AddressObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_AddressType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "address",
    sizeof(clips_AddressObject),
    0,
    clips_AddressObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS deffacts object */
staticforward PyTypeObject clips_DeffactsType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DeffactsObject;

#define clips_deffacts_check(v) ((v)->ob_type == &clips_DeffactsType)
#define clips_deffacts_value(v) (((clips_DeffactsObject *)(v))->value)

#define clips_deffacts_New(p) \
    p = PyObject_New(clips_DeffactsObject, &clips_DeffactsType)

static void clips_DeffactsObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DeffactsType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "deffacts",
    sizeof(clips_DeffactsObject),
    0,
    clips_DeffactsObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defrule object */
staticforward PyTypeObject clips_DefruleType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefruleObject;

#define clips_defrule_check(v) ((v)->ob_type == &clips_DeffactsType)
#define clips_defrule_value(v) (((clips_DefruleObject *)(v))->value)

#define clips_defrule_New(p) \
    p = PyObject_New(clips_DefruleObject, &clips_DefruleType)

static void clips_DefruleObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefruleType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defrule",
    sizeof(clips_DefruleObject),
    0,
    clips_DefruleObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS activation object */
staticforward PyTypeObject clips_ActivationType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_ActivationObject;

#define clips_activation_check(v) ((v)->ob_type == &clips_ActivationType)
#define clips_activation_value(v) (((clips_ActivationObject *)(v))->value)

#define clips_activation_New(p) \
    p = PyObject_New(clips_ActivationObject, &clips_ActivationType)

static void clips_ActivationObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_ActivationType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "activation",
    sizeof(clips_ActivationObject),
    0,
    clips_ActivationObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defglobal object */
staticforward PyTypeObject clips_DefglobalType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefglobalObject;

#define clips_defglobal_check(v) ((v)->ob_type == &clips_DefglobalType)
#define clips_defglobal_value(v) (((clips_DefglobalObject *)(v))->value)

#define clips_defglobal_New(p) \
    p = PyObject_New(clips_DefglobalObject, &clips_DefglobalType)

static void clips_DefglobalObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefglobalType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defglobal",
    sizeof(clips_DefglobalObject),
    0,
    clips_DefglobalObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS deffunction object */
staticforward PyTypeObject clips_DeffunctionType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DeffunctionObject;

#define clips_deffunction_check(v) ((v)->ob_type == &clips_DeffunctionType)
#define clips_deffunction_value(v) (((clips_DeffunctionObject *)(v))->value)

#define clips_deffunction_New(p) \
    p = PyObject_New(clips_DeffunctionObject, &clips_DeffunctionType)

static void clips_DeffunctionObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DeffunctionType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "deffunction",
    sizeof(clips_DeffunctionObject),
    0,
    clips_DeffunctionObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defgeneric object */
staticforward PyTypeObject clips_DefgenericType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefgenericObject;

#define clips_defgeneric_check(v) ((v)->ob_type == &clips_DefgenericType)
#define clips_defgeneric_value(v) (((clips_DefgenericObject *)(v))->value)

#define clips_defgeneric_New(p) \
    p = PyObject_New(clips_DefgenericObject, &clips_DefgenericType)

static void clips_DefgenericObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefgenericType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defgeneric",
    sizeof(clips_DefgenericObject),
    0,
    clips_DefgenericObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defmethod object */
staticforward PyTypeObject clips_DefmethodType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefmethodObject;

#define clips_defmethod_check(v) ((v)->ob_type == &clips_DefmethodType)
#define clips_defmethod_value(v) (((clips_DefmethodObject *)(v))->value)

#define clips_defmethod_New(p) \
    p = PyObject_New(clips_DefmethodObject, &clips_DefmethodType)

static void clips_DefmethodObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefmethodType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defmethod",
    sizeof(clips_DefmethodObject),
    0,
    clips_DefmethodObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defclass object */
staticforward PyTypeObject clips_DefclassType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefclassObject;

#define clips_defclass_check(v) ((v)->ob_type == &clips_DefclassType)
#define clips_defclass_value(v) (((clips_DefclassObject *)(v))->value)

#define clips_defclass_New(p) \
    p = PyObject_New(clips_DefclassObject, &clips_DefclassType)

static void clips_DefclassObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefclassType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defclass",
    sizeof(clips_DefclassObject),
    0,
    clips_DefclassObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS instance object */
staticforward PyTypeObject clips_InstanceType;

typedef struct {
    PyObject_HEAD
    BOOL locked;
    void *value;
    void *creation_env;
} clips_InstanceObject;

#define clips_instance_check(v) ((v)->ob_type == &clips_InstanceType)
#define clips_instance_value(v) (((clips_InstanceObject *)(v))->value)
#define clips_instance_env(v) (((clips_InstanceObject *)(v))->creation_env)
#define clips_instance_verify(v) (!((struct instance *) \
                                  (((clips_InstanceObject *)(v))->value))->garbage)

/* assign a pointer to a CLIPS instance to this object */
#define clips_instance_assign(v, p) do { \
        EnvIncrementInstanceCount(clips_instance_env(v), p); \
        clips_instance_value(v) = p; \
    } while(0)

/* lock/unlock an instance object */
#define clips_instance_locked(v) (((clips_InstanceObject *)(v))->locked)
#define clips_instance_lock(v) do { \
        ((clips_InstanceObject *)(v))->locked = TRUE; \
    } while(0)
#define clips_instance_unlock(v) do { \
        ((clips_InstanceObject *)(v))->locked = FALSE; \
    } while(0)

#define clips_instance_New(env, p) do { \
        p = PyObject_New(clips_InstanceObject, &clips_InstanceType); \
        clips_instance_env(p) = env; \
        clips_instance_value(p) = NULL; \
        ((clips_InstanceObject *)(p))->locked = FALSE; \
    } while(0)

static void clips_InstanceObject_dealloc(PyObject *self) {
    void *p = clips_instance_value(self);
    if(p)
        EnvDecrementInstanceCount(clips_instance_env(self), p);
    clips_instance_unlock(self);
    PyObject_Del(self);
}

static PyTypeObject clips_InstanceType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "instance",
    sizeof(clips_InstanceObject),
    0,
    clips_InstanceObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS definstances object */
staticforward PyTypeObject clips_DefinstancesType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefinstancesObject;

#define clips_definstances_check(v) ((v)->ob_type == &clips_DefinstancesType)
#define clips_definstances_value(v) (((clips_DefinstancesObject *)(v))->value)

#define clips_definstances_New(p) \
    p = PyObject_New(clips_DefinstancesObject, &clips_DefinstancesType)

static void clips_DefinstancesObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefinstancesType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "definstances",
    sizeof(clips_DefinstancesObject),
    0,
    clips_DefinstancesObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* a Python Type representing a CLIPS defmodule object */
staticforward PyTypeObject clips_DefmoduleType;

typedef struct {
    PyObject_HEAD
    void *value;
} clips_DefmoduleObject;

#define clips_defmodule_check(v) ((v)->ob_type == &clips_DefmoduleType)
#define clips_defmodule_value(v) (((clips_DefmoduleObject *)(v))->value)

#define clips_defmodule_New(p) \
    p = PyObject_New(clips_DefmoduleObject, &clips_DefmoduleType)

static void clips_DefmoduleObject_dealloc(PyObject *self) {
    PyObject_Del(self);
}

static PyTypeObject clips_DefmoduleType = {
    PyObject_HEAD_INIT(NULL)
    0,
    "defmodule",
    sizeof(clips_DefmoduleObject),
    0,
    clips_DefmoduleObject_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* ATTENTION!
 *  all the following helpers do not acquire the memory error handler function
 *  because they are internal helpers: however all functions that invoke these
 *  helpers (in both directions) should actually take care of the acquisition.
 */

/* helpers to convert DATA_OBJECTs into Python objects */
PyObject *i_do2py_mfhelp_e(void *env, void *ptr, int pos) {
    PyObject *p = NULL;
    int t = GetMFType(ptr, pos);
    clips_AddressObject *a = NULL;
    clips_FactObject *fo = NULL;
    clips_InstanceObject *io = NULL;

    /* check for return type and build a PyObject to return */
    switch(t) {
    case INTEGER:
        p = Py_BuildValue("(ii)", t, ValueToLong(GetMFValue(ptr, pos)));
        break;
    case FLOAT:
        p = Py_BuildValue("(id)", t, ValueToDouble(GetMFValue(ptr, pos)));
        break;
    case STRING:
    case SYMBOL:
    case INSTANCE_NAME:
        p = Py_BuildValue("(is)", t, ValueToString(GetMFValue(ptr, pos)));
        break;
    case INSTANCE_ADDRESS:
        if(!ptr)
            FAIL();
        if(env) {
            clips_instance_New(env, io);
            if(!io)
                FAIL();
            clips_instance_assign(io, GetMFValue(ptr, pos));
            ENV_CHECK_VALID_INSTANCE(env, io);
        } else {
            clips_instance_New(GetCurrentEnvironment(), io);
            if(!io)
                FAIL();
            clips_instance_assign(io, GetMFValue(ptr, pos));
            CHECK_VALID_INSTANCE(io);
        }
        clips_instance_lock(io);
        p = Py_BuildValue("(iO)", t, clips_instance_value(io));
        break;
    case EXTERNAL_ADDRESS:
        if(!ptr)
            FAIL();
        clips_address_New(a);
        if(!a)
            FAIL();
        clips_address_addrtype(a) = t;
        clips_address_value(a) = GetMFValue(ptr, pos);
        p = Py_BuildValue("(iO)", t, a);
        break;
    case FACT_ADDRESS:
        if(!ptr)
            FAIL();
        if(env) {
            clips_fact_New(env, fo);
            if(!fo)
                FAIL();
            clips_fact_assign(fo, GetMFValue(ptr, pos));
            ENV_CHECK_VALID_FACT(env, fo);
        } else {
            clips_fact_New(GetCurrentEnvironment(), fo);
            if(!fo)
                FAIL();
            clips_fact_assign(fo, GetMFValue(ptr, pos));
            CHECK_VALID_FACT(fo);
        }
        clips_fact_readonly(fo) = TRUE;
        clips_fact_lock(fo);
        p = Py_BuildValue("(iO)", t, fo);
        break;
    case MULTIFIELD:
        FAIL();     /* this should not be called for MFs */
        break;
    default:
        FAIL();     /* something wrong? */
        break;
    }

    return p;

BEGIN_FAIL
    Py_XDECREF(p);
    Py_XDECREF(a);
    Py_XDECREF(io);
    Py_XDECREF(fo);
END_FAIL
}


PyObject *i_do2py_e(void *env, DATA_OBJECT *o) {
    PyObject *p = NULL, *p1 = NULL, *q = NULL;
    void *ptr = NULL;
    int i = 0, n = 0, begin = 0, t = GetpType(o);
    clips_AddressObject *a = NULL;
    clips_FactObject *fo = NULL;
    clips_InstanceObject *io = NULL;

    /* check for return type and build a PyObject to return */
    switch(t) {
    case INTEGER:
        p = Py_BuildValue("(il)", t, DOPToLong(o));
        break;
    case FLOAT:
        p = Py_BuildValue("(id)", t, DOPToDouble(o));
        break;
    case STRING:
    case SYMBOL:
    case INSTANCE_NAME:
        p = Py_BuildValue("(is)", t, DOPToString(o));
        break;
    case MULTIFIELD:
        n = GetpDOLength(o);
        begin = GetpDOBegin(o);
        ptr = GetpValue(o);
        if(!ptr)
            FAIL();
        q = PyList_New(n);
        if(!q)
            FAIL();
        for(i=0; i<n; i++) {
            if(!(p1 = i_do2py_mfhelp_e(env, ptr, begin + i)))
                FAIL();
            PyList_SET_ITEM(q, i, p1);
        }
        p = Py_BuildValue("(iO)", t, q);
        break;
    case INSTANCE_ADDRESS:
        ptr = DOPToPointer(o);
        if(!ptr)
            FAIL();
        if(env) {
            clips_instance_New(env, io);
            if(!io)
                FAIL();
            clips_instance_assign(io, ptr);
            ENV_CHECK_VALID_INSTANCE(env, io);
        } else {
            clips_instance_New(GetCurrentEnvironment(), io);
            if(!io)
                FAIL();
            clips_instance_assign(io, ptr);
            CHECK_VALID_INSTANCE(io);
        }
        clips_instance_lock(io);
        p = Py_BuildValue("(iO)", t, io);
        break;
    case EXTERNAL_ADDRESS:
        ptr = DOPToPointer(o);
        if(!ptr)
            FAIL();
        clips_address_New(a);
        if(!a)
            FAIL();
        clips_address_addrtype(a) = t;
        clips_address_value(a) = ptr;
        p = Py_BuildValue("(iO)", t, a);
        break;
    case FACT_ADDRESS:
        ptr = DOPToPointer(o);
        if(!ptr)
            FAIL();
        if(env) {
            clips_fact_New(env, fo);
            if(!fo)
                FAIL();
            clips_fact_assign(fo, ptr);
            ENV_CHECK_VALID_FACT(env, fo);
        } else {
            clips_fact_New(GetCurrentEnvironment(), fo);
            if(!fo)
                FAIL();
            clips_fact_assign(fo, ptr);
            CHECK_VALID_FACT(fo);
        }
        clips_fact_readonly(fo) = TRUE;
        clips_fact_lock(fo);
        p = Py_BuildValue("(iO)", t, fo);
        break;
    default:
        FAIL();     /* something wrong? */
        break;
    }

    return p;

BEGIN_FAIL
    Py_XDECREF(p);
    Py_XDECREF(a);
    Py_XDECREF(io);
    Py_XDECREF(fo);
END_FAIL
}

#define i_do2py(_o) i_do2py_e(NULL, _o)


/* helpers to convert couples (type, value) to DATA_OBJECTs:
 *  first, a "simple" macro to check if something can be really
 *  converted to a DATA_OBJECT: that is, it should be a pair and
 *  its first element has to be an integer. Then, macros to extract
 *  the elements from the tuple itself.
 */
#define IS_PY_DATA_OBJECT(_p) \
    (PyTuple_Check(_p) && PyTuple_Size(_p) == 2 \
     && PyInt_Check(PyTuple_GetItem(_p, 0)))
#define PY_DATA_OBJECT_TYPE(_p) ((int)PyInt_AsLong(PyTuple_GetItem(_p, 0)))
#define PY_DATA_OBJECT_VALUE(_p) ((PyObject *)PyTuple_GetItem(_p, 1))
/* then the actual functions to put data in the DATA_OBJECT structure */
BOOL i_py2do_mfhelp_e(void *env, PyObject *p, void *mfptr, int fieldpos) {
    int type = 0;
    PyObject *value = NULL;
    void *do_value = NULL;
    long i = 0;
    double d = 0;
    char *s = NULL;

    if(!IS_PY_DATA_OBJECT(p)) goto fail;
    type = PY_DATA_OBJECT_TYPE(p);
    value = PY_DATA_OBJECT_VALUE(p);
    switch(type) {
    case INTEGER:
        if(!PyInt_Check(value))
            goto fail;
        i = PyInt_AsLong(value);
        do_value = EnvAddLong(env, i);
        break;
    case FLOAT:
        if(!PyFloat_Check(value))
            goto fail;
        d = PyFloat_AsDouble(value);
        do_value = EnvAddDouble(env, d);
        break;
    case STRING:
    case SYMBOL:
    case INSTANCE_NAME:
        if(!PyString_Check(value))
            goto fail;
        s = PyString_AsString(value);
        do_value = EnvAddSymbol(env, s);
        break;
    case INSTANCE_ADDRESS:
        if(!clips_instance_check(value))
            goto fail;
        do_value = clips_instance_value(value);
        break;
    case FACT_ADDRESS:
        if(!clips_fact_check(value))
            goto fail;
        do_value = clips_fact_value(value);
        break;
    case EXTERNAL_ADDRESS:
        if(!clips_address_check(value))
            goto fail;
        do_value = clips_address_value(value);
        break;
    default:
        goto fail;
    }

    SetMFType(mfptr, fieldpos, type);
    SetMFValue(mfptr, fieldpos, do_value);
    return TRUE;

fail:
    return FALSE;
}


BOOL i_py2do_e(void *env, PyObject *p, DATA_OBJECT *o) {
    int type = 0;
    PyObject *value = NULL, *item = NULL;
    void *do_value = NULL;
    long i = 0, n = 0;
    double d = 0;
    char *s = NULL;

    if(!IS_PY_DATA_OBJECT(p)) goto fail;
    type = PY_DATA_OBJECT_TYPE(p);
    value = PY_DATA_OBJECT_VALUE(p);
    switch(type) {
    case INTEGER:
        if(!PyInt_Check(value))
            goto fail;
        i = PyInt_AsLong(value);
        do_value = EnvAddLong(env, i);
        break;
    case FLOAT:
        if(!PyFloat_Check(value))
            goto fail;
        d = PyFloat_AsDouble(value);
        do_value = EnvAddDouble(env, d);
        break;
    case STRING:
    case SYMBOL:
    case INSTANCE_NAME:
        if(!PyString_Check(value))
            goto fail;
        s = PyString_AsString(value);
        do_value = EnvAddSymbol(env, s);
        break;
    case MULTIFIELD:
        if(!PyList_Check(value))
            goto fail;
        n = PyList_Size(value);
        if(n == 0) goto fail;
        if(!(do_value = EnvCreateMultifield(env, n)))
            goto fail;
        for(i = 0; i < n; i++) {
            item = PyList_GetItem(value, i);
            if(!item)
                goto fail;
            if(!i_py2do_mfhelp_e(env, item, do_value, (int)(i + 1)))
                goto fail;
        }
        break;  /* n is still good for below */
    case INSTANCE_ADDRESS:
        if(!clips_instance_check(value))
            goto fail;
        do_value = clips_instance_value(value);
        break;
    case FACT_ADDRESS:
        if(!clips_fact_check(value))
            goto fail;
        do_value = clips_fact_value(value);
        break;
    case EXTERNAL_ADDRESS:
        if(!clips_address_check(value))
            goto fail;
        do_value = clips_address_value(value);
        break;
    default:
        goto fail;
    }

    SetpType(o, type);
    SetpValue(o, do_value);
    if(type == MULTIFIELD) {
        SetpDOBegin(o, 1);
        SetpDOEnd(o, n);
    }

    return TRUE;

fail:
    return FALSE;
}

#define i_py2do(_p, _o) i_py2do_e(GetCurrentEnvironment(), _p, _o)


/* Part Two: the implementation */

/* Part Two - Section 1: Globals */


/* 4.1 - Environment Functions */

/* AddClearFunction */
UNIMPLEMENT(addClearFunction, g_addClearFunction)
/* AddPeriodicFunction */
UNIMPLEMENT(addPeriodicFunction, g_addPeriodicFunction)
/* AddResetFunction */
UNIMPLEMENT(addResetFunction, g_addResetFunction)

/* bload */
static char g_bload__doc__[] = "\
bload(filename)\n\
load a binary image of environment constructs\n\
arguments:\n\
  filename (str) - the name of binary file to load from";
static PyObject *g_bload(PyObject *self, PyObject *args) {
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Bload(fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* bsave */
static char g_bsave__doc__[] = "\
bsave(filename)\n\
save a binary image of environment constructs\n\
arguments:\n\
  filename (str) - the name of binary file to save to";
static PyObject *g_bsave(PyObject *self, PyObject *args) {
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Bsave(fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* clear */
static char g_clear__doc__[] = "\
clear()\n\
clear environment";
static PyObject *g_clear(PyObject *self, PyObject *args) {

    CHECK_NOARGS(args);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Clear_PY()) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPSSYS_ENVNOCLEAR();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
#ifdef USE_CLEAR_RESETGCCOUNTERS
    RESET_ASSERTED_FACTS();
#endif /* USE_CLEAR_RESETGCCOUNTERS */
    LOSE_HASH_FACTS();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* functionCall */
static char g_functionCall__doc__[] = "\
functionCall(funcname [, args]) -> (type, value)\n\
call an internal function\n\
returns: the function result, as a pair (type, return value)\n\
arguments:\n\
  funcname (str) - the internal function or operator name\n\
  args (str) - string containing a blank separated list of arguments";
static PyObject *g_functionCall(PyObject *self, PyObject *args) {
    char *func = NULL, *fargs = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "s|s", &func, &fargs))
        FAIL();
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(FunctionCall(func, fargs, &o)) {
        SetEvaluationError(GetCurrentEnvironment(), FALSE);
        SetHaltExecution(GetCurrentEnvironment(), FALSE);
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_FUNCCALL();
        FAIL();
    }
    SetEvaluationError(GetCurrentEnvironment(), FALSE);
    SetHaltExecution(GetCurrentEnvironment(), FALSE);
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    p = i_do2py(&o);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    SKIP();
    Py_XDECREF(p);
END_FAIL
}


/* getAutoFloatDividend */
STATUS_FUNC_GET_BOOL(getAutoFloatDividend,
                     g_getAutoFloatDividend,
                     GetAutoFloatDividend)

/* getDynamicConstraintChecking */
STATUS_FUNC_GET_BOOL(getDynamicConstraintChecking,
                     g_getDynamicConstraintChecking,
                     GetDynamicConstraintChecking)

/* getSequenceOperatorRecognition */
STATUS_FUNC_GET_BOOL(getSequenceOperatorRecognition,
                     g_getSequenceOperatorRecognition,
                     GetSequenceOperatorRecognition)

/* getStaticConstraintChecking */
STATUS_FUNC_GET_BOOL(getStaticConstraintChecking,
                     g_getStaticConstraintChecking,
                     GetStaticConstraintChecking)

/* initializeEnvironment: NEVER TO BE IMPLEMENTED
 *  the main environment is initialized at module load time
 */

/* load */
static char g_load__doc__[] = "\
load(filename)\n\
load constructs into environment\n\
arguments:\n\
  filename (str) - the name of file to load constructs from";
static PyObject *g_load(PyObject *self, PyObject *args) {
    char *fn = NULL;
    int rv;


    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    rv = Load(fn);
    RELEASE_MEMORY_ERROR();
    if(rv == 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    if(rv < 0) {
        ERROR_CLIPS_PARSEF();
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* removeClearFunction */
UNIMPLEMENT(removeClearFunction, g_removeClearFunction)

/* removePeriodicFunction */
UNIMPLEMENT(removePeriodicFunction, g_removePeriodicFunction)

/* removeResetFunction */
UNIMPLEMENT(removeResetFunction, g_removeResetFunction)

/* reset */
static char g_reset__doc__[] = "\
reset()\n\
reset environment";
static PyObject *g_reset(PyObject *self, PyObject *args) {

    /* this function may cause CLIPS to allocate memory */

    CHECK_NOARGS(args);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    Reset();
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* save */
static char g_save__doc__[] = "\
save(filename)\n\
save constructs to a file\n\
arguments:\n\
  filename (str) - the name of file to save constructs to";
static PyObject *g_save(PyObject *self, PyObject *args) {
    char *fn = NULL;
    void *env = NULL;

    /* this function may cause CLIPS to allocate memory */

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!EnvSave(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setAutoFloatDividend */
STATUS_FUNC_SET_BOOL(setAutoFloatDividend,
                     g_setAutoFloatDividend,
                     SetAutoFloatDividend)

/* setDynamicConstraintChecking */
STATUS_FUNC_SET_BOOL(setDynamicConstraintChecking,
                     g_setDynamicConstraintChecking,
                     SetDynamicConstraintChecking)

/* setSequenceOperatorRecognition */
STATUS_FUNC_SET_BOOL(setSequenceOperatorRecognition,
                     g_setSequenceOperatorRecognition,
                     SetSequenceOperatorRecognition)

/* setStaticConstraintChecking */
STATUS_FUNC_SET_BOOL(setStaticConstraintChecking,
                     g_setStaticConstraintChecking,
                     SetStaticConstraintChecking)

/* batchStar */
static char g_batchStar__doc__[] = "\
batchStar(filename)\n\
batch execute commands stored in specified file\n\
arguments:\n\
  filename (str) - the name of file to read commands from";
static PyObject *g_batchStar(PyObject *self, PyObject *args) {
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!BatchStar(fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* build */
static char g_build__doc__[] = "\
build(construct)\n\
define specified construct\n\
arguments:\n\
  construct (str) - the construct to be added";
static PyObject *g_build(PyObject *self, PyObject *args) {
    char *cons = NULL;

    if(!PyArg_ParseTuple(args, "s", &cons))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Build(cons)) {
        SetEvaluationError(GetCurrentEnvironment(), FALSE);
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEX();
        FAIL();
    }
    SetEvaluationError(GetCurrentEnvironment(), FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* eval */
static char g_eval__doc__[] = "\
eval(expression) -> (type, value)\n\
evaluate the provided expression\n\
returns: a pair holding the result in the form (type, return value)\n\
arguments:\n\
  expression (str) - the expression to evaluate";
static PyObject *g_eval(PyObject *self, PyObject *args) {
    char *expr = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "s", &expr))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Eval(expr, &o)) {
        SetEvaluationError(GetCurrentEnvironment(), FALSE);
        SetHaltExecution(GetCurrentEnvironment(), FALSE);
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_EVALX();
        FAIL();
    }
    SetEvaluationError(GetCurrentEnvironment(), FALSE);
    SetHaltExecution(GetCurrentEnvironment(), FALSE);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p)
        RETURN_NONE();
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* 4.2 - Debugging Functions */

/* dribbleActive */
STATUS_FUNC_GET_BOOL(dribbleActive, g_dribbleActive, DribbleActive)

/* dribbleOff */
FUNC_VOID_BOOL(dribbleOff, g_dribbleOff, DribbleOff)

/* dribbleOn */
static char g_dribbleOn__doc__[] = "\
dribbleOn(filename)\n\
turn the dribble function on\n\
arguments:\n\
  filename (str) - the name of file to write dribble information to";
static PyObject *g_dribbleOn(PyObject *self, PyObject *args) {
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!DribbleOn(fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getWatchItem */
static char g_getWatchItem__doc__[] = "\
getWatchItem(item) -> bool\n\
tell whether the specified item is watched or not\n\
returns: True if specified item is being watched, False otherwise\n\
arguments:\n\
  item (str) - the item to monitor the status of, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *g_getWatchItem(PyObject *self, PyObject *args) {
    char *item = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "s", &item))
        FAIL();
    rv = GetWatchItem(item);
    if(rv < 0) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    return Py_BuildValue("i", rv ? 1 : 0);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* unwatch */
static char g_unwatch__doc__[] = "\
unwatch(item)\n\
turn off tracing for specified item\n\
arguments:\n\
  item (str) - the item to disable tracing for, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *g_unwatch(PyObject *self, PyObject *args) {
    char *item = NULL;

    if(!PyArg_ParseTuple(args, "s", &item))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Unwatch(item)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* watch */
static char g_watch__doc__[] = "\
watch(item)\n\
turn on tracing for specified item\n\
arguments:\n\
  item (str) - the item to enable tracing for, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *g_watch(PyObject *self, PyObject *args) {
    char *item = NULL;

    if(!PyArg_ParseTuple(args, "s", &item))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!Watch(item)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.3 - Deftemplate functions */

/* helper to check whether there is still this deftemplate */
F_INLINE void *deftemplateExists(void *ptr) {
	void *rv = GetNextDeftemplate(NULL);
	while(rv != NULL) {
		if(rv == ptr) return rv;
		else rv = GetNextDeftemplate(rv);
	}
	return NULL;
}
#define PYDEFTEMPLATE_EXISTS(_p) deftemplateExists(clips_deftemplate_value(_p))
#define CHECK_DEFTEMPLATE(_p) do { \
        if(!PYDEFTEMPLATE_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFTEMPLATE(_p) do { \
        if(_p && !PYDEFTEMPLATE_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* deftemplateModule */
static char g_deftemplateModule__doc__[] = "\
deftemplateModule(deftemplate) -> str\n\
retrieve the name of the module where the provided deftemplate resides\n\
returns: a string containing a module name\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect";
static PyObject *g_deftemplateModule(PyObject *self, PyObject *args) {
    char *module = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    module = DeftemplateModule(clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!module) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(module);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#if CLIPS_MINOR > 23

/* deftemplateSlotAllowedValues */
static char g_deftemplateSlotAllowedValues__doc__[] = "\
deftemplateSlotAllowedValues(deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the allowed values for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotAllowedValues(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotAllowedValues(clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotCardinality */
static char g_deftemplateSlotCardinality__doc__[] = "\
deftemplateSlotCardinality(deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the cardinality for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotCardinality(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotCardinality(clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotDefaultP */
static char g_deftemplateSlotDefaultP__doc__[] = "\
deftemplateSlotCardinality(deftemplate, name) -> int\n\
tell whether or not a slot of given deftemplate has a default value\n\
returns: one of NO_DEFAULT, STATIC_DEFAULT or DYNAMIC_DEFAULT\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotDefaultP(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    rv = DeftemplateSlotDefaultP(clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotDefaultValue */
static char g_deftemplateSlotDefaultValue__doc__[] = "\
deftemplateSlotDefaultValue(deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve default value(s) for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotDefaultValue(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotDefaultValue(clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotExistP */
static char g_deftemplateSlotExistP__doc__[] = "\
deftemplateSlotExistP(deftemplate, name) -> bool\n\
tell whether or not the given deftemplate has the specified slot\n\
returns: True if the slot is present, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotExistP(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    rv = DeftemplateSlotExistP(clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotMultiP */
static char g_deftemplateSlotMultiP__doc__[] = "\
deftemplateSlotMultiP(deftemplate, name) -> bool\n\
tell whether or not the specified slot of given deftemplate is multifield\n\
returns: True if it is multifield, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotMultiP(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    rv = DeftemplateSlotMultiP(clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotNames */
static char g_deftemplateSlotNames__doc__[] = "\
deftemplateSlotNames(deftemplate) -> (MULTIFIELD, list)\n\
retrieve the names of slots in given deftemplate (special case if implied)\n\
returns: MULTIFIELD and a list of pairs or a pair (SYMBOL, 'implied')\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect";
static PyObject *g_deftemplateSlotNames(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotNames(clips_deftemplate_value(p), &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotRange */
static char g_deftemplateSlotRange__doc__[] = "\
deftemplateSlotRange(deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the numeric range information for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotRange(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotRange(clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotSingleP */
static char g_deftemplateSlotSingleP__doc__[] = "\
deftemplateSlotSingleP(deftemplate, name) -> bool\n\
tell whether or not the specified slot of given deftemplate is single field\n\
returns: True if it is single field, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotSingleP(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    rv = DeftemplateSlotSingleP(clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotTypes */
static char g_deftemplateSlotTypes__doc__[] = "\
deftemplateSlotTypes(deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve names of the data types allowed for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_deftemplateSlotTypes(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    DeftemplateSlotTypes(clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(deftemplateSlotAllowedValues,
                    g_deftemplateSlotAllowedValues)
UNIMPLEMENT_VERSION(deftemplateSlotCardinality,
                    g_deftemplateSlotCardinality)
UNIMPLEMENT_VERSION(deftemplateSlotDefaultP,
                    g_deftemplateSlotDefaultP)
UNIMPLEMENT_VERSION(deftemplateSlotDefaultValue,
                    g_deftemplateSlotDefaultValue)
UNIMPLEMENT_VERSION(deftemplateSlotExistP,
                    g_deftemplateSlotExistP)
UNIMPLEMENT_VERSION(deftemplateSlotMultiP,
                    g_deftemplateSlotMultiP)
UNIMPLEMENT_VERSION(deftemplateSlotNames,
                    g_deftemplateSlotNames)
UNIMPLEMENT_VERSION(deftemplateSlotRange,
                    g_deftemplateSlotRange)
UNIMPLEMENT_VERSION(deftemplateSlotSingleP,
                    g_deftemplateSlotSingleP)
UNIMPLEMENT_VERSION(deftemplateSlotTypes,
                    g_deftemplateSlotTypes)
#endif /* CLIPS_MINOR > 23 */

/* findDeftemplate */
static char g_findDeftemplate__doc__[] = "\
findDeftemplate(name) -> deftemplate\n\
retrieve deftemplate object corresponding to the specified name\n\
returns: the deftemplate as a new object\n\
arguments:\n\
  name (str) - the name of the deftemplate to look for";
static PyObject *g_findDeftemplate(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDeftemplate(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deftemplate_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeftemplateList */
static char g_getDeftemplateList__doc__[] = "\
getDeftemplateList([module]) -> (MULTIFIELD, list)\n\
retrieve list of deftemplate names in specified defmodule\n\
returns: value MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDeftemplateList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDeftemplateList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeftemplateName */
static char g_getDeftemplateName__doc__[] = "\
getDeftemplateName(deftemplate) -> str\n\
retrieve the name of given deftemplate object\n\
returns: a string containing the name\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *g_getDeftemplateName(PyObject *self, PyObject *args) {
    char *name = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDeftemplateName(clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* getDeftemplatePPForm */
static char g_getDeftemplatePPForm__doc__[] = "\
getDeftemplatePPForm(deftemplate) -> str\n\
retrieve the pretty-print form of given deftemplate object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *g_getDeftemplatePPForm(PyObject *self, PyObject *args) {
    char *s = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDeftemplatePPForm(clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* getDeftemplateWatch */
static char g_getDeftemplateWatch__doc__[] = "\
getDeftemplateWatch(deftemplate) -> bool\n\
tell if deftemplate is being watched\n\
returns: True if the deftemplate is being watched, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *g_getDeftemplateWatch(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    i = GetDeftemplateWatch(clips_deftemplate_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* getNextDeftemplate */
static char g_getNextDeftemplate__doc__[] = "\
getNextDeftemplate([deftemplate]) -> deftemplate\n\
find next deftemplate in the list, first if argument is omitted\n\
returns: next deftemplate object, None if already at last deftemplate\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to start from";
static PyObject *g_getNextDeftemplate(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DeftemplType, &p))
        FAIL();
    if(p)
        CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDeftemplate(p ? clips_deftemplate_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_deftemplate_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDeftemplateDeletable */
static char g_isDeftemplateDeletable__doc__[] = "\
isDeftemplateDeletable(deftemplate) -> bool\n\
tell whether or not given deftemplate object can be deleted\n\
returns: True when deftemplate can be deleted, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *g_isDeftemplateDeletable(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    i = IsDeftemplateDeletable(clips_deftemplate_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDeftemplates */
static char g_listDeftemplates__doc__[] = "\
listDeftemplates(logicalname [, module])\n\
list deftemplates to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDeftemplates(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDeftemplates(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDeftemplateWatch */
static char g_setDeftemplateWatch__doc__[] = "\
setDeftemplateWatch(state, deftemplate)\n\
set the specified deftemplate to a new watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *g_setDeftemplateWatch(PyObject *self, PyObject *args) {
    PyObject *state = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DeftemplType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(p);
    ACQUIRE_MEMORY_ERROR();
    SetDeftemplateWatch(PyObject_IsTrue(state), clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undeftemplate */
static char g_undeftemplate__doc__[] = "\
undeftemplate([deftemplate])\n\
remove a deftemplate or all deftemplates from the system\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to remove, all if omitted";
static PyObject *g_undeftemplate(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DeftemplType, &p)) FAIL();
    CHECK_RM_DEFTEMPLATE(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undeftemplate(p ? clips_deftemplate_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.4 - Fact Functions */

/* assertFact */
static char g_assertFact__doc__[] = "\
assertFact(fact) -> fact\n\
add a fact to the fact list\n\
returns: the asserted fact as a new object\n\
arguments:\n\
  fact (fact) - the fact to assert";
static PyObject *g_assertFact(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p)) FAIL();
    CHECK_VALID_FACT(p);
    /* if fact is read-only, it was previously asserted and can't be reused */
    if(clips_fact_readonly(p)) {
        ERROR_CLIPS_REASSERT();
        FAIL();
    }
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    ptr = Assert(clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    if(!ptr) {
        ERROR_CLIPS_ASSERT();
        FAIL();
    }
    REMOVE_JUSTASSERTED_FACT();
    REMOVE_HASH_FACT(p);
    /* now the old fact cannot be modified anymore, even on possible failure */
    clips_fact_readonly(p) = TRUE;
    clips_fact_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    CHECK_VALID_FACT(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* assertString */
static char g_assertString__doc__[] = "\
assertString(expr) -> fact\n\
add a fact to the fact list using a string\n\
returns: the asserted fact as a new object\n\
arguments:\n\
  expr (str) - string containing a list of primitive datatypes";
static PyObject *g_assertString(PyObject *self, PyObject *args) {
    char *expr = NULL;
    clips_FactObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "s", &expr))
        FAIL();
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    ptr = AssertString(expr);
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    if(!ptr) {
        ERROR_CLIPS_ASSERT();
        FAIL();
    }
    clips_fact_New(GetCurrentEnvironment(), p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(p) = TRUE;
    clips_fact_assign(p, ptr);
    clips_fact_lock(p);
    CHECK_VALID_FACT(p);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* assignFactSlotDefaults */
static char g_assignFactSlotDefaults__doc__[] = "\
assignFactSlotDefaults(fact)\n\
assign default values to the slots of a fact\n\
arguments:\n\
  fact (fact) - the fact whose slots are to reset to default values";
static PyObject *g_assignFactSlotDefaults(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    if(clips_fact_readonly(p)) {
        ERROR_CLIPS_READONLY();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!AssignFactSlotDefaults(clips_fact_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_OTHER("could not assign default values to fact");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* createFact */
static char g_createFact__doc__[] = "\
createFact(deftemplate) -> fact\n\
create a new fact object using the provided deftemplate\n\
returns: a new fact object\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate defining the fact type";
static PyObject *g_createFact(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    clips_DeftemplObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeftemplType, &q)) FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = CreateFact(clips_deftemplate_value(q));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_fact_New(GetCurrentEnvironment(), p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_assign(p, ptr);
    clips_fact_lock(p);
    CHECK_VALID_FACT(p);
    ADD_NONASSERTED_FACT();
    APPEND_HASH_FACT(p);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* decrementFactCount */
UNIMPLEMENT(decrementFactCount, g_decrementFactCount)

/* factIndex */
static char g_factIndex__doc__[] = "\
factIndex(fact) -> int\n\
retrieve the index of specified fact in fact list\n\
arguments:\n\
  fact (fact) - the fact to look for";
static PyObject *g_factIndex(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    long int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    i = FactIndex(clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* facts */
static char g_facts__doc__[] = "\
facts(logicalname [, module [, start [, end [, max]]]])\n\
list facts to the output stream identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule, None for all modules\n\
  start (int) - first fact, -1 for no restriction\n\
  end (int) - last fact, -1 for no restriction\n\
  max (int) - maximum number of facts, -1 for no restriction";
static PyObject *g_facts(PyObject *self, PyObject *args) {
    PyObject *module = NULL;
    char *lname = NULL;
    int start = -1, end = -1, max = -1;

    if(!PyArg_ParseTuple(args, "s|O!iii", &lname,
                         &clips_DefmoduleType, &module,
                         &start, &end, &max))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    Facts(lname,
          module && module != Py_None ? clips_defmodule_value(module) : NULL,
          start, end, max);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getFactDuplication */
STATUS_FUNC_GET_BOOL(getFactDuplication,
                     g_getFactDuplication,
                     GetFactDuplication)

/* getFactListChanged */
STATUS_FUNC_GET_BOOL(getFactListChanged,
                     g_getFactListChanged,
                     GetFactListChanged)

/* getFactPPForm */
static char g_getFactPPForm__doc__[] = "\
getFactPPForm(fact) -> str\n\
retrieve the pretty-print form of given fact\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  fact (fact) - the fact object to inspect";
static PyObject *g_getFactPPForm(PyObject *self, PyObject *args) {
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    clips_FactObject *p = NULL;
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    GetFactPPForm(buffer, ppbuffer_size-1, clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getFactSlot */
static char g_getFactSlot__doc__[] = "\
getFactSlot(fact [, slotname]) -> (type, value)\n\
get the slot value for the specified fact given the slot name\n\
returns: a value or a multifield in the form (type, return value)\n\
arguments:\n\
  fact (fact) - the fact to inspect\n\
  slotname (str) - the name of the slot to retrieve, should be omitted\n\
                   for the implied multifield slot of an implied\n\
                   deftemplate";
static PyObject *g_getFactSlot(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    char *slotname = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!|s", &clips_FactType, &p, &slotname))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    /* we make some considerations about this call, to avoid nasty errors */
    /* check that the slot name can be really omitted (see docstring) */
    if(!slotname && !
        ((struct deftemplate *)
            ((struct fact *)clips_fact_value(p))->whichDeftemplate
        )->implied) {
            ERROR_VALUE("cannot omit slot name using this fact");
            FAIL();
        }
    /* end of considerations */
    ACQUIRE_MEMORY_ERROR();
    if(!GetFactSlot(clips_fact_value(p), slotname, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    Py_XDECREF(rv);
END_FAIL
}

/* getNextFact */
static char g_getNextFact__doc__[] = "\
getNextFact([fact]) -> fact\n\
retrieve next fact object, first if argument is omitted\n\
returns: a fact object, None if already at last fact\n\
arguments:\n\
  fact (fact) - the fact to start from";
static PyObject *g_getNextFact(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_FactType, &p))
        FAIL();
    if(p)
        CHECK_VALID_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextFact(p ? clips_fact_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_fact_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    CHECK_VALID_FACT(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* 4.4.12a - getNextFactInTemplate */
static char g_getNextFactInTemplate__doc__[] = "\
getNextFactInTemplate(deftemplate [, fact]) -> fact\n\
retrieve next fact object for a deftemplate, first if fact is omitted\n\
returns: a fact object, None if already at last fact\n\
arguments:\n\
  deftemplate (deftemplate) - the template to find facts of\n\
  fact (fact) - the fact to start from";
static PyObject *g_getNextFactInTemplate(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL, *q = NULL;
    clips_DeftemplObject *t = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_DeftemplType, &t, &clips_FactType, &p))
        FAIL();
    CHECK_DEFTEMPLATE(t);
    if(p)
        CHECK_VALID_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextFactInTemplate(
        clips_deftemplate_value(t), p ? clips_fact_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_fact_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    CHECK_VALID_FACT(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* incrementFactCount */
UNIMPLEMENT(incrementFactCount, g_incrementFactCount)

/* loadFacts */
static char g_loadFacts__doc__[] = "\
loadFacts(filename)\n\
load facts from specified file\n\
arguments:\n\
  filename (str) - the name of file to load facts from";
static PyObject *g_loadFacts(PyObject *self, PyObject *args) {
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!LoadFacts(fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

#if CLIPS_MINOR > 23

/* ppFact */
static char g_ppFact__doc__[] = "\
ppFact(fact, output [, ignoredefault])\n\
write the pretty-print form of given fact to logical output\n\
arguments:\n\
  fact (fact) - the fact to write\n\
  output (str) - logical name of stream to output to\n\
  ignoredefault (bool) - True to skip slots whose values equal defaults";
static PyObject *g_ppFact(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    char *s = NULL;
    PyObject *o = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O", &clips_FactType, &p, &s, &o))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    PPFact(clips_fact_value(p), s, o ? PyObject_IsTrue(o) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(ppFact, g_ppFact)
#endif /* CLIPS_MINOR > 23 */

/* putFactSlot */
static char g_putFactSlot__doc__[] = "\
putFactSlot(fact, name, value)\n\
changes the value of specified slot in given fact\n\
arguments:\n\
  fact (fact) - the fact to change: must have been created with createFact\n\
  name (str) - the name of the slot to change\n\
  value (pair) - a pair (type, value) containing the value to assign";
static PyObject *g_putFactSlot(PyObject *self, PyObject *args) {
    DATA_OBJECT o = { 0 };
    clips_FactObject *f = NULL;
    PyObject *p = NULL;
    char *s;

    /* note that function i_py2do checks for last argument validity */
    if(!PyArg_ParseTuple(args, "O!sO", &clips_FactType, &f, &s, &p))
        FAIL();
    CHECK_VALID_FACT(f);
    CHECK_LOST_FACT(f);
    if(clips_fact_readonly(f)) {
        ERROR_CLIPS_READONLY();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!i_py2do(p, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    if(!PutFactSlot(clips_fact_value(f), s, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_OTHER("fact slot could not be modified");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* retract */
static char g_retract__doc__[] = "\
retract(fact)\n\
retract provided fact\n\
arguments:\n\
  fact (fact) - the fact to retract";
static PyObject *g_retract(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    clips_fact_lock(p);
    /* if the fact was actually asserted it must now be read-only */
    if(!clips_fact_readonly(p)) {
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!Retract(clips_fact_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* saveFacts */
static char g_saveFacts__doc__[] = "\
saveFacts(filename, scope)\n\
save the facts in the specified scope\n\
arguments:\n\
  filename (str) - the name of the file to save to\n\
  scope (int) - can be one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *g_saveFacts(PyObject *self, PyObject *args) {
    int scope = 0;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "si", &fn, &scope))
        FAIL();
    if(scope != LOCAL_SAVE && scope != VISIBLE_SAVE) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!SaveFacts(fn, scope, NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setFactDuplication */
STATUS_FUNC_SET_BOOL(setFactDuplication,
                     g_setFactDuplication,
                     SetFactDuplication)

/* setFactListChanged */
STATUS_FUNC_SET_BOOL(setFactListChanged,
                     g_setFactListChanged,
                     SetFactListChanged)

/* factDeftemplate */
static char g_factDeftemplate__doc__[] = "\
factDeftemplate(fact) -> deftemplate\n\
return the deftemplate associated with a particular fact\n\
returns: a deftemplate object, None if no deftemplate associated\n\
arguments:\n\
  fact (fact) - the fact to examine";
static PyObject *g_factDeftemplate(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    clips_DeftemplObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = FactDeftemplate(clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr || !deftemplateExists(ptr))
        RETURN_NONE();
    clips_deftemplate_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* factExistp */
static char g_factExistp__doc__[] = "\
factExistp(fact) -> bool\n\
tell whether a fact is in the list or has been retracted\n\
returns: True if the fact exixts, False if it was retracted\n\
arguments:\n\
  fact (fact) - the fact to check";
static PyObject *g_factExistp(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    i = FactExistp(clips_fact_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* factSlotNames */
static char g_factSlotNames__doc__[] = "\
factSlotNames(fact) -> (MULTIFIELD, list)\n\
get the slot names for the specified fact\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  fact (fact) - the fact to inspect";
static PyObject *g_factSlotNames(PyObject *self, PyObject *args) {
    clips_FactObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!", &clips_FactType, &p))
        FAIL();
    CHECK_VALID_FACT(p);
    CHECK_LOST_FACT(p);
    ACQUIRE_MEMORY_ERROR();
    FactSlotNames(clips_fact_value(p), &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getFactList */
static char g_getFactList__doc__[] = "\
getFactList([module]) -> (type, list)\n\
retrieve list of fact identifiers in specified defmodule\n\
returns:  MULTIFIELD and a list of pairs (STRING, identifier)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getFactList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetFactList(&o, module ? clips_defmodule_value(module) : NULL);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* loadFactsFromString */
static char g_loadFactsFromString__doc__[] = "\
loadFactsFromString(str)\n\
load facts from specified string into the fact list\n\
arguments:\n\
  string (str) - string to load facts from";
static PyObject *g_loadFactsFromString(PyObject *self, PyObject *args) {
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s", &s))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!LoadFactsFromString(s, -1)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEX();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.5 - Deffacts functions */

/* helper to check whether there is still this construct */
F_INLINE void *deffactsExists(void *ptr) {
	void *rv = GetNextDeffacts(NULL);
	while(rv != NULL) {
		if(rv == ptr) return rv;
		else rv = GetNextDeffacts(rv);
	}
	return NULL;
}
#define PYDEFFACTS_EXISTS(_p) deffactsExists(clips_deffacts_value(_p))
#define CHECK_DEFFACTS(_p) do { \
        if(!PYDEFFACTS_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFFACTS(_p) do { \
        if(_p && !PYDEFFACTS_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* deffactsModule */
static char g_deffactsModule__doc__[] = "\
deffactsModule(deffacts) -> str\n\
retrieve the module where the specified deffacts object is defined\n\
returns: the module name\n\
arguments:\n\
  deffacts (deffacts) - the deffacts object to inspect";
static PyObject *g_deffactsModule(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffactsType, &p))
        FAIL();
    CHECK_DEFFACTS(p);
    ACQUIRE_MEMORY_ERROR();
    s = DeffactsModule(clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDeffacts */
static char g_findDeffacts__doc__[] = "\
findDeffacts(name) -> deffacts\n\
find the deffacts object whose name is specified\n\
returns: the deffacts as a new object\n\
arguments:\n\
  name (str) - the name of the deffacts to look for";
static PyObject *g_findDeffacts(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DeffactsObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDeffacts(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deffacts_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffacts_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeffactsList */
static char g_getDeffactsList__doc__[] = "\
getDeffactsList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of deffacts in specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDeffactsList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDeffactsList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeffactsName */
static char g_getDeffactsName__doc__[] = "\
getDeffactsName(deffacts) -> str\n\
retrieve the name of deffacts object\n\
returns: the deffacts name\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to inspect";
static PyObject *g_getDeffactsName(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffactsType, &p))
        FAIL();
    CHECK_DEFFACTS(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDeffactsName(clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDeffactsPPForm */
static char g_getDeffactsPPForm__doc__[] = "\
getDeffactsPPForm(deffacts) -> str\n\
retrieve the pretty-print form of deffacts object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to inspect";
static PyObject *g_getDeffactsPPForm(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffactsType, &p))
        FAIL();
    CHECK_DEFFACTS(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDeffactsPPForm(clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDeffacts */
static char g_getNextDeffacts__doc__[] = "\
getNextDeffacts([deffacts]) -> deffacts\n\
retrieve next deffacts object in list, first if argument is omitted\n\
returns: a deffacts object, None if already at last deffacts\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to start from";
static PyObject *g_getNextDeffacts(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DeffactsType, &p))
        FAIL();
    if(p)
        CHECK_DEFFACTS(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDeffacts(p ? clips_deffacts_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_deffacts_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffacts_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDeffactsDeletable */
static char g_isDeffactsDeletable__doc__[] = "\
isDeffactsDeletable(deffacts) -> bool\n\
tell whether or not given deffacts object can be deleted\n\
returns: True when deffacts can be deleted, False otherwise\n\
arguments:\n\
  deffacts (deffacts) - the deffacts object";
static PyObject *g_isDeffactsDeletable(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffactsType, &p))
        FAIL();
    CHECK_DEFFACTS(p);
    i = IsDeffactsDeletable(clips_deffacts_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDeffacts */
static char g_listDeffacts__doc__[] = "\
listDeffacts(output [, module])\n\
list deffacts objects in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDeffacts(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &s, &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDeffacts(s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undeffacts */
static char g_undeffacts__doc__[] = "\
undeffacts([deffacts])\n\
delete a deffacts object or all deffacts objects\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to be deleted, all if omitted";
static PyObject *g_undeffacts(PyObject *self, PyObject *args) {
    clips_DeffactsObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffactsType, &p))
        FAIL();
    CHECK_RM_DEFFACTS(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undeffacts(p ? clips_deffacts_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.6 - Defrule functions */

/* helper to check whether there is still this construct */
F_INLINE void *defruleExists(void *ptr) {
    void *rv = GetNextDefrule(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDefrule(rv);
    }
    return NULL;
}
#define PYDEFRULE_EXISTS(_p) defruleExists(clips_defrule_value(_p))
#define CHECK_DEFRULE(_p) do { \
        if(!PYDEFRULE_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFRULE(_p) do { \
        if(_p && !PYDEFRULE_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* defruleHasBreakpoint */
static char g_defruleHasBreakpoint__doc__[] = "\
defruleHasBreakpoint(defrule) -> bool\n\
test whether or not the given defrule has a breakpoint\n\
returns: True if the defrule has a breakpoint, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_defruleHasBreakpoint(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    i = DefruleHasBreakpoint(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* defruleModule */
static char g_defruleModule__doc__[] = "\
defruleModule(defrule) -> str\n\
retrieve the module where the defrule is defined\n\
returns: the requested name of module\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_defruleModule(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    name = DefruleModule(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDefrule */
static char g_findDefrule__doc__[] = "\
findDefrule(name) -> defrule\n\
find the defrule with the specified name\n\
returns: the defrule as a new object\n\
arguments:\n\
  name (str) - the name of defrule to look for";
static PyObject *g_findDefrule(PyObject *self, PyObject *args) {
    char *name = NULL;
    clips_DefruleObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefrule(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defrule_New(p);
    clips_defrule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefruleList */
static char g_getDefruleList__doc__[] = "\
getDefruleList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of defrule objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDefruleList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDefruleList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefruleName */
static char g_getDefruleName__doc__[] = "\
getDefruleName(defrule) -> str\n\
retrieve the name of specified defrule\n\
returns: a string containing the defrule name\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_getDefruleName(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDefruleName(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefrulePPForm */
static char g_getDefrulePPForm__doc__[] = "\
getDefrulePPForm(defrule) -> str\n\
retrieve the pretty-print form of specified defrule\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_getDefrulePPForm(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefrulePPForm(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefruleWatchActivations */
static char g_getDefruleWatchActivations__doc__[] = "\
getDefruleWatchActivations(defrule) -> bool\n\
tell whether the specified defrule is watched for activations\n\
returns: True if activations are watched, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_getDefruleWatchActivations(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    i = GetDefruleWatchActivations(clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefruleWatchFirings */
static char g_getDefruleWatchFirings__doc__[] = "\
getDefruleWatchFirings(defrule) -> bool\n\
tell whether the specified defrule is watched for firings\n\
returns: True if rule firings are watched, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_getDefruleWatchFirings(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    i = GetDefruleWatchFirings(clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getIncrementalReset */
STATUS_FUNC_GET_BOOL(getIncrementalReset,
                     g_getIncrementalReset,
                     GetIncrementalReset)

/* getNextDefrule */
static char g_getNextDefrule__doc__[] = "\
getNextDefrule([defrule]) -> defrule\n\
retrieve next defrule object in list, first if argument is omitted\n\
returns: a defrule object, None if already at last defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to start from";
static PyObject *g_getNextDefrule(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefruleType, &p))
        FAIL();
    if(p)
        CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefrule(p ? clips_defrule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defrule_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defrule_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDefruleDeletable */
static char g_isDefruleDeletable__doc__[] = "\
isDefruleDeletable(defrule) -> bool\n\
tell whether or not given defrule object can be deleted\n\
returns: True when defrule can be deleted, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule object";
static PyObject *g_isDefruleDeletable(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    i = IsDefruleDeletable(clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefrules */
static char g_listDefrules__doc__[] = "\
listDefrules(output [, module])\n\
list defrule objects in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDefrules(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &s, &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefrules(s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* matches */
static char g_matches__doc__[] = "\
matches(output, defrule)\n\
list defrule partial matches\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *g_matches(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "sO!", &s, &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    if(!Matches_PY(s, clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* refresh */
static char g_refresh__doc__[] = "\
refresh(defrule)\n\
refresh a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to refresh";
static PyObject *g_refresh(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    if(!Refresh(clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* removeBreak */
static char g_removeBreak__doc__[] = "\
removeBreak(defrule)\n\
remove the breakpoint from a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to access";
static PyObject *g_removeBreak(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    if(!RemoveBreak(clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setBreak */
static char g_setBreak__doc__[] = "\
setBreak(defrule)\n\
set a breakpoint to a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to access";
static PyObject *g_setBreak(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    ACQUIRE_MEMORY_ERROR();
    SetBreak(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefruleWatchActivations */
static char g_setDefruleWatchActivations__doc__[] = "\
setDefruleWatchActivations(state, defrule)\n\
set activations watch of a defrule to a new state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defrule (defrule) - the defrule to access";
static PyObject *g_setDefruleWatchActivations(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    SetDefruleWatchActivations(
        PyObject_IsTrue(state), clips_defrule_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefruleWatchFirings */
static char g_setDefruleWatchFirings__doc__[] = "\
setDefruleWatchFirings(state, defrule)\n\
set firings watch of a defrule to a new state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defrule (defrule) - the defrule to access";
static PyObject *g_setDefruleWatchFirings(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefruleType, &p))
        FAIL();
    CHECK_DEFRULE(p);
    SetDefruleWatchFirings(PyObject_IsTrue(state), clips_defrule_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setIncrementalReset */
STATUS_FUNC_SET_BOOL(setIncrementalReset,
                     g_setIncrementalReset,
                     SetIncrementalReset)

/* showBreaks */
static char g_showBreaks__doc__[] = "\
showBreaks(output [, module])\n\
list breakpoints in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_showBreaks(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &s, &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ShowBreaks(s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefrule */
static char g_undefrule__doc__[] = "\
undefrule([defrule])\n\
delete a defrule object or all defrule objects\n\
arguments:\n\
  defrule (defrule) - the defrule to be deleted, all if omitted";
static PyObject *g_undefrule(PyObject *self, PyObject *args) {
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefruleType, &p)) FAIL();
    CHECK_RM_DEFRULE(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefrule(p ? clips_defrule_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.7 - Agenda Functions */

/* helper to check whether there is still this construct */
F_INLINE void *activationExists(void *ptr) {
    void *rv = GetNextActivation(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextActivation(rv);
    }
    return NULL;
}
#define PYACTIVATION_EXISTS(_p) activationExists(clips_activation_value(_p))
#define CHECK_ACTIVATION(_p) do { \
        if(!PYACTIVATION_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_ACTIVATION(_p) do { \
        if(_p && !PYACTIVATION_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* addRunFunction */
UNIMPLEMENT(addRunFunction, g_addRunFunction)

/* agenda */
static char g_agenda__doc__[] = "\
agenda(output [, module])\n\
list agenda rules in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_agenda(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &s, &clips_DefmoduleType, &p))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvAgenda(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* clearFocusStack */
FUNC_VOID_ONLY(clearFocusStack, g_clearFocusStack, ClearFocusStack)

/* deleteActivation */
static char g_deleteActivation__doc__[] = "\
deleteActivation(activation)\n\
remove activation from agenda\n\
arguments:\n\
  activation (activation) - the activation to delete";
static PyObject *g_deleteActivation(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_ActivationType, &p))
        FAIL();
    CHECK_RM_ACTIVATION(p);
    ACQUIRE_MEMORY_ERROR();
    if(!DeleteActivation(p ? clips_activation_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* focus */
static char g_focus__doc__[] = "\
focus(module)\n\
set current focus\n\
arguments:\n\
  module (defmodule) - the defmodule to set focus to";
static PyObject *g_focus(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    Focus(clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getActivationName */
static char g_getActivationName__doc__[] = "\
getActivationName(activation) -> str\n\
retrieve the name of specified activation\n\
returns: name as a string\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *g_getActivationName(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_ActivationType, &p))
        FAIL();
    CHECK_ACTIVATION(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetActivationName(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getActivationPPForm */
static char g_getActivationPPForm__doc__[] = "\
getActivationPPForm(activation) -> str\n\
retrieve the pretty-print form of specified activation\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *g_getActivationPPForm(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!", &clips_ActivationType, &p))
        FAIL();
    CHECK_ACTIVATION(p);
    ACQUIRE_MEMORY_ERROR();
    GetActivationPPForm(buffer, ppbuffer_size-1, clips_activation_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getActivationSalience */
static char g_getActivationSalience__doc__[] = "\
getActivationSalience(activation) -> int\n\
retrieve the salience value of specified activation\n\
returns: salience as integer\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *g_getActivationSalience(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_ActivationType, &p))
        FAIL();
    CHECK_ACTIVATION(p);
    ACQUIRE_MEMORY_ERROR();
    i = GetActivationSalience(clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getAgendaChanged */
STATUS_FUNC_GET_BOOL(getAgendaChanged, g_getAgendaChanged, GetAgendaChanged)

/* getFocus */
static char g_getFocus__doc__[] = "\
getFocus() -> defmodule\n\
retrieve the module with associated focus\n\
returns: the defmodule with focus as a new object";
static PyObject *g_getFocus(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    void *ptr = NULL;

    CHECK_NOARGS(args);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetFocus();
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getFocusStack */
static char g_getFocusStack__doc__[] = "\
getFocusStack() -> (MULTIFIELD, list)\n\
retrieve the module names in the focus stack\n\
returns: MULTIFIELD and a list of pairs (STRING, name)";
static PyObject *g_getFocusStack(PyObject *self, PyObject *args) {
    DATA_OBJECT o = { 0 };
    PyObject *p = NULL;
    void *env = NULL;

    CHECK_NOARGS(args);
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvGetFocusStack(env, &o);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getNextActivation */
static char g_getNextActivation__doc__[] = "\
getNextActivation([activation]) -> activation\n\
retrieve next activation object in list, first if argument is omitted\n\
returns: an activation object, None if already at last activation\n\
arguments:\n\
  activation (activation) - the activation to start from";
static PyObject *g_getNextActivation(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_ActivationType, &p))
        FAIL();
    if(p) { CHECK_ACTIVATION(p); }
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextActivation(p ? clips_activation_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_activation_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_activation_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getSalienceEvaluation */
STATUS_FUNC_GET(getSalienceEvaluation,
                g_getSalienceEvaluation,
                GetSalienceEvaluation,
                "i")

/* getStrategy */
FUNC_GET_ONLY(getStrategy, g_getStrategy, GetStrategy, "i")

/* listFocusStack */
static char g_listFocusStack__doc__[] = "\
listFocusStack(output)\n\
print current focus stack\n\
arguments:\n\
  output (str) - logical name of output stream";
static PyObject *g_listFocusStack(PyObject *self, PyObject *args) {
    char *output = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "s", &output))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvListFocusStack(env, output);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* popFocus */
FUNC_VOID_ONLY(popFocus, g_popFocus, PopFocus)

/* refreshAgenda */
static char g_refreshAgenda__doc__[] = "\
refreshAgenda([module])\n\
refresh agenda for specified defmodule\n\
arguments:\n\
  module (defmodule) - the defmodule to process, all if omitted";
static PyObject *g_refreshAgenda(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvRefreshAgenda(env, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* removeRunFunction */
UNIMPLEMENT(removeRunFunction, g_removeRunFunction)

/* reorderAgenda */
static char g_reorderAgenda__doc__[] = "\
reorderAgenda([module])\n\
reorder agenda for specified defmodule\n\
arguments:\n\
  module (defmodule) - the defmodule to process, all if omitted";
static PyObject *g_reorderAgenda(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvReorderAgenda(env, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* run */
static char g_run__doc__[] = "\
run([limit]) -> fired\n\
execute rules\n\
returns: the number of fired rules\n\
arguments:\n\
  limit (int) - number of rules to fire, all if omitted";
static PyObject *g_run(PyObject *self, PyObject *args) {
    long runlimit = -1;

    if(!PyArg_ParseTuple(args, "|i", &runlimit))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    runlimit = Run(runlimit);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(runlimit);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setActivationSalience */
static char g_setActivationSalience__doc__[] = "\
setActivationSalience(activation, salience) -> int\n\
set the new activation salience\n\
returns: the old value\n\
arguments:\n\
  activation (activation) - an activation object\n\
  salience (int) - the new activation salience";
static PyObject *g_setActivationSalience(PyObject *self, PyObject *args) {
    clips_ActivationObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i", &clips_ActivationType, &p, &i))
        FAIL();
    CHECK_ACTIVATION(p);
    ACQUIRE_MEMORY_ERROR();
    i = SetActivationSalience(clips_activation_value(p), i);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setAgendaChanged */
STATUS_FUNC_SET_BOOL(setAgendaChanged, g_setAgendaChanged, SetAgendaChanged)

/* setSalienceEvaluation */
static char g_setSalienceEvaluation__doc__[] = "\
setSalienceEvaluation(mode)\n\
set the new salience evaluation mode\n\
arguments:\n\
  mode (int) - the new evaluation mode";
static PyObject *g_setSalienceEvaluation(PyObject *self, PyObject *args) {
    int i = 0;

    if(!PyArg_ParseTuple(args, "i", &i))
        FAIL();
    if(i != WHEN_DEFINED && i != WHEN_ACTIVATED && i != EVERY_CYCLE) {
        ERROR_VALUE("invalid evaluation mode");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    SetSalienceEvaluation(i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setStrategy */
static char g_setStrategy__doc__[] = "\
setStrategy(mode)\n\
set the new strategy\n\
arguments:\n\
  strategy (int) - the new strategy";
static PyObject *g_setStrategy(PyObject *self, PyObject *args) {
    int i = 0;

    if(!PyArg_ParseTuple(args, "i", &i)) FAIL();
    if(i != DEPTH_STRATEGY && i != BREADTH_STRATEGY && i != LEX_STRATEGY &&
       i != MEA_STRATEGY && i != COMPLEXITY_STRATEGY &&
       i != SIMPLICITY_STRATEGY && i != RANDOM_STRATEGY) {
        ERROR_VALUE("invalid strategy");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    SetStrategy(i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.8 Defglobal Functions */

/* helper to check whether there is still this construct */
F_INLINE void *defglobalExists(void *ptr) {
    void *rv = GetNextDefglobal(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDefglobal(rv);
    }
    return NULL;
}
#define PYDEFGLOBAL_EXISTS(_p) defglobalExists(clips_defglobal_value(_p))
#define CHECK_DEFGLOBAL(_p) do { \
        if(!PYDEFGLOBAL_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFGLOBAL(_p) do { \
        if(_p && !PYDEFGLOBAL_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* defglobalModule */
static char g_defglobalModule__doc__[] = "\
defglobalModule(defglobal) -> str\n\
retrieve the module name where specified defglobal is defined\n\
returns: module name as string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *g_defglobalModule(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    char *module = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    ACQUIRE_MEMORY_ERROR();
    module = DefglobalModule(clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    if(!module) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    return Py_BuildValue("s", module);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDefglobal */
static char g_findDefglobal__doc__[] = "\
findDefglobal(name) -> defglobal\n\
return the defglobal object associated with name\n\
returns: the defglobal as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *g_findDefglobal(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefglobal(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defglobal_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defglobal_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefglobalList */
static char g_getDefglobalList__doc__[] = "\
getDefglobalList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of defglobal objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDefglobalList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDefglobalList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefglobalName */
static char g_getDefglobalName__doc__[] = "\
getDefglobalName(defglobal) -> str\n\
retrieve the name of specified defglobal\n\
returns: name as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *g_getDefglobalName(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    name = GetDefglobalName(clips_defglobal_value(p));
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefglobalPPForm */
static char g_getDefglobalPPForm__doc__[] = "\
getDefglobalPPForm(defglobal) -> str\n\
retrieve the pretty-print form of specified defglobal\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *g_getDefglobalPPForm(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefglobalPPForm(clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefglobalValue */
static char g_getDefglobalValue__doc__[] = "\
getDefglobalValue(name) -> (type, value)\n\
retrieve the value of specified defglobal\n\
returns: value as a pair (type, value)\n\
arguments:\n\
  name (str) - the name of the defglobal";
static PyObject *g_getDefglobalValue(PyObject *self, PyObject *args) {
    char *name = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!EnvGetDefglobalValue(env, name, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefglobalValueForm */
static char g_getDefglobalValueForm__doc__[] = "\
getDefglobalValueForm(defglobal) -> str\n\
retrieve the pretty-print form of specified defglobal\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *g_getDefglobalValueForm(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    ACQUIRE_MEMORY_ERROR();
    GetDefglobalValueForm(buffer, ppbuffer_size-1, clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getDefglobalWatch */
static char g_getDefglobalWatch__doc__[] = "\
getDefglobalWatch(defglobal) -> bool\n\
tell whether or not the defglobal is being watched\n\
returns: True if the defglobal is being watched, False otherwise\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *g_getDefglobalWatch(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    i = GetDefglobalWatch(clips_defglobal_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getGlobalsChanged */
STATUS_FUNC_GET_BOOL(getGlobalsChanged, g_getGlobalsChanged, GetGlobalsChanged)

/* getNextDefglobal */
static char g_getNextDefglobal__doc__[] = "\
getNextDefglobal([defglobal]) -> defglobal\n\
retrieve next defglobal object in list, first if argument is omitted\n\
returns: a defglobal object, None if already at last defglobal\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to start from";
static PyObject *g_getNextDefglobal(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefglobalType, &p))
        FAIL();
    if(p)
        CHECK_DEFGLOBAL(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefglobal(p ? clips_defglobal_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defglobal_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defglobal_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getResetGlobals */
STATUS_FUNC_GET_BOOL(getResetGlobals, g_getResetGlobals, GetResetGlobals)

/* isDefglobalDeletable */
static char g_isDefglobalDeletable__doc__[] = "\
isDefglobalDeletable(defglobal) -> bool\n\
tell whether or not the defglobal is deletable\n\
returns: True if the defglobal can be deleted, False otherwise\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to be inspected";
static PyObject *g_isDefglobalDeletable(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    i = IsDefglobalDeletable(clips_defglobal_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefglobals */
static char g_listDefglobals__doc__[] = "\
listDefglobals(logicalname [, module])\n\
list defglobals to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDefglobals(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefglobals(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefglobalValue */
static char g_setDefglobalValue__doc__[] = "\
setDefglobalValue(name, value)\n\
set the value of passed in defglobal\n\
arguments:\n\
  name (str) - the name of defglobal to change\n\
  value (pair) - a pair (type, value)";
static PyObject *g_setDefglobalValue(PyObject *self, PyObject *args) {
    char *name = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "sO", &name, &p))
        FAIL();
    if(!i_py2do(p, &o)) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!SetDefglobalValue(name, &o)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    Py_XDECREF(p);
    RETURN_NONE();

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* setDefglobalWatch */
static char g_setDefglobalWatch__doc__[] = "\
setDefglobalWatch(state, defglobal)\n\
set the specified defglobal to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defglobal (defglobal) - the defglobal object";
static PyObject *g_setDefglobalWatch(PyObject *self, PyObject *args) {
    PyObject *state = NULL;
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefglobalType, &p))
        FAIL();
    CHECK_DEFGLOBAL(p);
    SetDefglobalWatch(PyObject_IsTrue(state), clips_defglobal_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setGlobalsChanged */
STATUS_FUNC_SET_BOOL(setGlobalsChanged, g_setGlobalsChanged, SetGlobalsChanged)

/* setResetGlobals */
STATUS_FUNC_SET_BOOL(setResetGlobals, g_setResetGlobals, SetResetGlobals)

/* showDefglobals */
static char g_showDefglobals__doc__[] = "\
showDefglobals(output [, module])\n\
list defglobals in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_showDefglobals(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &s, &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ShowDefglobals(s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefglobal */
static char g_undefglobal__doc__[] = "\
undefglobal([defglobal])\n\
delete a defglobal object or all defglobal objects\n\
arguments:\n\
  defglobal (defglobal) - object to be deleted, all if omitted";
static PyObject *g_undefglobal(PyObject *self, PyObject *args) {
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefglobalType, &p))
        FAIL();
    CHECK_RM_DEFGLOBAL(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefglobal(p ? clips_defglobal_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.9 Deffunction Functions */

/* helper to check whether there is still this construct */
F_INLINE void *deffunctionExists(void *ptr) {
    void *rv = GetNextDeffunction(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDeffunction(rv);
    }
    return NULL;
}
#define PYDEFFUNCTION_EXISTS(_p) deffunctionExists(clips_deffunction_value(_p))
#define CHECK_DEFFUNCTION(_p) do { \
        if(!PYDEFFUNCTION_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFFUNCTION(_p) do { \
        if(_p && !PYDEFFUNCTION_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* deffunctionModule */
static char g_deffunctionModule__doc__[] = "\
deffunctionModule(deffunction) -> str\n\
retrieve the module name where specified deffunction is defined\n\
returns: module name as string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *g_deffunctionModule(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;
    char *module = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    CHECK_DEFFUNCTION(p);
    ACQUIRE_MEMORY_ERROR();
    module = DeffunctionModule(clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!module) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    return Py_BuildValue("s", module);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDeffunction */
static char g_findDeffunction__doc__[] = "\
findDeffunction(name) -> deffunction\n\
return the deffunction object associated with name\n\
returns: the deffunction as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *g_findDeffunction(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDeffunction(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deffunction_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffunction_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeffunctionList */
static char g_getDeffunctionList__doc__[] = "\
getDeffunctionList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of deffunction objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDeffunctionList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDeffunctionList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDeffunctionName */
static char g_getDeffunctionName__doc__[] = "\
getDeffunctionName(deffunction) -> str\n\
retrieve the name of specified deffunction\n\
returns: name as a string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *g_getDeffunctionName(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    CHECK_DEFFUNCTION(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDeffunctionName(clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDeffunctionPPForm */
static char g_getDeffunctionPPForm__doc__[] = "\
getDeffunctionPPForm(deffunction) -> str\n\
retrieve the pretty-print form of specified deffunction\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *g_getDeffunctionPPForm(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    CHECK_DEFFUNCTION(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDeffunctionPPForm(clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDeffunctionWatch */
static char g_getDeffunctionWatch__doc__[] = "\
getDeffunctionWatch(deffunction) -> bool\n\
tell whether or not the deffunction is being watched\n\
returns: True if the deffunction is being watched, False otherwise\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *g_getDeffunctionWatch(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    i = GetDeffunctionWatch(clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDeffunction */
static char g_getNextDeffunction__doc__[] = "\
getNextDeffunction([deffunction]) -> deffunction\n\
retrieve next deffunction object in list, first if argument is omitted\n\
returns: a deffunction object, None if already at last deffunction\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to start from";
static PyObject *g_getNextDeffunction(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DeffunctionType, &p))
        FAIL();
    if(p) CHECK_DEFFUNCTION(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDeffunction(p ? clips_deffunction_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_deffunction_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffunction_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDeffunctionDeletable */
static char g_isDeffunctionDeletable__doc__[] = "\
isDeffunctionDeletable(deffunction) -> bool\n\
tell whether or not the deffunction is deletable\n\
returns: True if the deffunction can be deleted, False otherwise\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to be inspected";
static PyObject *g_isDeffunctionDeletable(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    CHECK_DEFFUNCTION(p);
    i = IsDeffunctionDeletable(clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDeffunctions */
static char g_listDeffunctions__doc__[] = "\
listDeffunctions(logicalname [, module])\n\
list deffunctions to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDeffunctions(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDeffunctions(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDeffunctionWatch */
static char g_setDeffunctionWatch__doc__[] = "\
setDeffunctionWatch(state, deffunction)\n\
set the specified deffunction to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  deffunction (deffunction) - the deffunction object";
static PyObject *g_setDeffunctionWatch(PyObject *self, PyObject *args) {
    PyObject *state = NULL;
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_DEFFUNCTION(p);
    ACQUIRE_MEMORY_ERROR();
    SetDeffunctionWatch(PyObject_IsTrue(state), clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undeffunction */
static char g_undeffunction__doc__[] = "\
undeffunction([deffunction])\n\
delete a deffunction object or all deffunction objects\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to delete, all if omitted";
static PyObject *g_undeffunction(PyObject *self, PyObject *args) {
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DeffunctionType, &p))
        FAIL();
    CHECK_RM_DEFFUNCTION(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undeffunction(p ? clips_deffunction_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.10 Defgeneric Functions */

/* helper to check whether there is still this construct */
F_INLINE void *defgenericExists(void *ptr) {
    void *rv = GetNextDefgeneric(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDefgeneric(rv);
    }
    return NULL;
}
#define PYDEFGENERIC_EXISTS(_p) defgenericExists(clips_defgeneric_value(_p))
#define CHECK_DEFGENERIC(_p) do { \
        if(!PYDEFGENERIC_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFGENERIC(_p) do { \
        if(_p && !PYDEFGENERIC_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* defgenericModule */
static char g_defgenericModule__doc__[] = "\
defgenericModule(defgeneric) -> str\n\
retrieve the module name where specified defgeneric is defined\n\
returns: module name as string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_defgenericModule(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    char *module = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    module = DefgenericModule(clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    if(!module) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    return Py_BuildValue("s", module);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDefgeneric */
static char g_findDefgeneric__doc__[] = "\
findDefgeneric(name) -> defgeneric\n\
return the defgeneric object associated with name\n\
returns: the defgeneric as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *g_findDefgeneric(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefgeneric(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defgeneric_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defgeneric_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefgenericList */
static char g_getDefgenericList__doc__[] = "\
getDefgenericList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of defgeneric objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDefgenericList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDefgenericList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefgenericName */
static char g_getDefgenericName__doc__[] = "\
getDefgenericName(defgeneric) -> str\n\
retrieve the name of specified defgeneric\n\
returns: name as a string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefgenericName(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDefgenericName(clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefgenericPPForm */
static char g_getDefgenericPPForm__doc__[] = "\
getDefgenericPPForm(defgeneric) -> str\n\
retrieve the pretty-print form of specified defgeneric\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefgenericPPForm(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefgenericPPForm(clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefgenericWatch */
static char g_getDefgenericWatch__doc__[] = "\
getDefgenericWatch(defgeneric) -> bool\n\
tell whether or not the defgeneric is being watched\n\
returns: True if the defgeneric is being watched, False otherwise\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefgenericWatch(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    i = GetDefgenericWatch(clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDefgeneric */
static char g_getNextDefgeneric__doc__[] = "\
getNextDefgeneric([defgeneric]) -> defgeneric\n\
retrieve next defgeneric object in list, first if argument is omitted\n\
returns: a defgeneric object, None if already at last defgeneric\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to start from";
static PyObject *g_getNextDefgeneric(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefgenericType, &p))
        FAIL();
    if(p)
        CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefgeneric(p ? clips_defgeneric_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defgeneric_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defgeneric_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDefgenericDeletable */
static char g_isDefgenericDeletable__doc__[] = "\
isDefgenericDeletable(defgeneric) -> bool\n\
tell whether or not the defgeneric is deletable\n\
returns: True if the defgeneric can be deleted, False otherwise\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to be inspected";
static PyObject *g_isDefgenericDeletable(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    i = IsDefgenericDeletable(clips_defgeneric_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefgenerics */
static char g_listDefgenerics__doc__[] = "\
listDefgenerics(logicalname [, module])\n\
list defgeneric objects to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDefgenerics(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefgenerics(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefgenericWatch */
static char g_setDefgenericWatch__doc__[] = "\
setDefgenericWatch(state, defgeneric)\n\
set the specified defgeneric to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defgeneric (defgeneric) - the defgeneric object";
static PyObject *g_setDefgenericWatch(PyObject *self, PyObject *args) {
    PyObject *state = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    SetDefgenericWatch(PyObject_IsTrue(state), clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefgeneric */
static char g_undefgeneric__doc__[] = "\
undefgeneric([defgeneric])\n\
delete a defgeneric object or all defgeneric objects\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to delete, all if omitted";
static PyObject *g_undefgeneric(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefgenericType, &p))
        FAIL();
    CHECK_RM_DEFGENERIC(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefgeneric(p ? clips_defgeneric_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.11 - Defmethod functions */

/* getDefmethodDescription */
static char g_getDefmethodDescription__doc__[] = "\
getDefmethodDescription(index, defgeneric) -> str\n\
describe method parameter restrictions\n\
returns: the description as a string\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefmethodDescription(PyObject *self, PyObject *args) {
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;
    int i = 0;
    clips_DefgenericObject *p = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    GetDefmethodDescription(buffer, ppbuffer_size-1, clips_defgeneric_value(p), i);
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getDefmethodList */
static char g_getDefmethodList__doc__[] = "\
getDefmethodList([defgeneric]) -> (MULTIFIELD, list)\n\
retrieve the list of defmethods in the specified defgeneric\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect, all if omitted";
static PyObject *g_getDefmethodList(PyObject *self, PyObject *args) {
    clips_DefgenericObject *g = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefgenericType, &g))
        FAIL();
    if(g)
        CHECK_DEFGENERIC(g);
    ACQUIRE_MEMORY_ERROR();
    GetDefmethodList(g ? clips_defgeneric_value(g) : NULL, &o);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefmethodPPForm */
static char g_getDefmethodPPForm__doc__[] = "\
getDefmethodPPForm(index, defgeneric) -> str\n\
retrieve the pretty-print form of specified defmethod\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefmethodPPForm(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefmethodPPForm(clips_defgeneric_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmethodWatch */
static char g_getDefmethodWatch__doc__[] = "\
getDefmethodWatch(index, defgeneric) -> bool\n\
tell whether or not a defgeneric method is being watched\n\
returns: True if the method is being watched, False otherwise\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getDefmethodWatch(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    int i = 0, j = 0;

    if(!PyArg_ParseTuple(args, "iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    j = GetDefmethodWatch(clips_defgeneric_value(p), i);
    RETURN_BOOL(j);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getMethodRestrictions */
static char g_getMethodRestrictions__doc__[] = "\
getMethodRestrictions(index, defgeneric) -> (MULTIFIELD, list)\n\
retrieve restriction for specified defmethod\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getMethodRestrictions(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    PyObject *q = NULL;
    int i = 0;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "i|O!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    GetMethodRestrictions(p ? clips_defgeneric_value(p) : NULL, i, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getNextDefmethod */
static char g_getNextDefmethod__doc__[] = "\
getNextDefmethod(index, defgeneric) -> int\n\
return index of next defmethod in defgeneric object\n\
returns: an integer value\n\
arguments:\n\
  index (int) - index of method, zero for first method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_getNextDefmethod(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    int i = 0, j = 0;

    if(!PyArg_ParseTuple(args, "iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();     /* needed? */
    j = GetNextDefmethod(p ? clips_defgeneric_value(p) : NULL, i);
    RELEASE_MEMORY_ERROR();
    return Py_BuildValue("i", j);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* isDefmethodDeletable */
static char g_isDefmethodDeletable__doc__[] = "\
isDefmethodDeletable(index, defgeneric) -> bool\n\
tell whether the specified method is deletable or not\n\
returns: True if the method can be deleted, False otherwise\n\
arguments:\n\
  index (int) - index of method, zero for first method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_isDefmethodDeletable(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    int i = 0, j = 0;

    if(!PyArg_ParseTuple(args, "iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    j = IsDefmethodDeletable(p ? clips_defgeneric_value(p) : NULL, i);
    RETURN_BOOL(j);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefmethods */
static char g_listDefmethods__doc__[] = "\
listDefmethods(output, defgeneric)\n\
list the defmethod in the defgeneric object\n\
arguments:\n\
  output (str) - logical name of output stream\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *g_listDefmethods(PyObject *self, PyObject *args) {
    clips_DefgenericObject *p = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefgenericType, &p))
        FAIL();
    CHECK_DEFGENERIC(p);
    ACQUIRE_MEMORY_ERROR();
    ListDefmethods(lname, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefmethodWatch */
static char g_setDefmethodWatch__doc__[] = "\
setDefmethodWatch(state, index, defgeneric)\n\
set the specified defgeneric to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defgeneric (defgeneric) - the defgeneric object";
static PyObject *g_setDefmethodWatch(PyObject *self, PyObject *args) {
    int i = 0;
    PyObject *state = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "OiO!", &state, &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFGENERIC(p);
    SetDefmethodWatch(PyObject_IsTrue(state), clips_defgeneric_value(p), i);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefmethod */
static char g_undefmethod__doc__[] = "\
undefmethod([index, defgeneric])\n\
delete a defmethod object or all defmethod objects\n\
arguments:\n\
  index (int) - index of defmethod to delete, all if omitted\n\
  defgeneric (defgeneric) - referred defgeneric, all if omitted";
static PyObject *g_undefmethod(PyObject *self, PyObject *args) {
    int i = 0;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "|iO!", &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    if(i != 0 && !p) {
        ERROR_VALUE("both arguments must be omitted or specified");
        FAIL();
    }
    if(p)
        CHECK_DEFGENERIC(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefmethod(p ? clips_defgeneric_value(p) : NULL, i)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.12 - Defclass Functions */

/* helper to check whether there is still this construct */
F_INLINE void *defclassExists(void *ptr) {
    void *rv = GetNextDefclass(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDefclass(rv);
    }
    return NULL;
}
#define PYDEFCLASS_EXISTS(_p) defclassExists(clips_defclass_value(_p))
#define CHECK_DEFCLASS(_p) do { \
        if(!PYDEFCLASS_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFCLASS(_p) do { \
        if(_p && !PYDEFCLASS_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual function with documentation */

/* browseClasses */
static char g_browseClasses__doc__[] = "\
browseClasses(output, defclass)\n\
print the classes who inherit from specified one in a graph form\n\
arguments:\n\
  output (str) - the name of logical output\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *g_browseClasses(PyObject *self, PyObject *args) {
    char *s = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "sO!", &s, &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    BrowseClasses(s, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* classAbstractP */
static char g_classAbstractP__doc__[] = "\
classAbstractP(defclass) -> bool\n\
tell if class is abstract or concrete\n\
returns: True if the class is abstract, False if concrete\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *g_classAbstractP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    i = ClassAbstractP(clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* classReactiveP */
static char g_classReactiveP__doc__[] = "\
classReactiveP(defclass) -> bool\n\
tell if class is reactive (matches object patterns) or not\n\
returns: True if the class is reactive, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *g_classReactiveP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    i = ClassReactiveP(clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* classSlots */
static char g_classSlots__doc__[] = "\
classSlots(defclass, inherit) -> (MULTIFIELD, list)\n\
return names of slots of the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *g_classSlots(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O", &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    ClassSlots(clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* classSubclasses */
static char g_classSubclasses__doc__[] = "\
classSubclasses(defclass, inherit) -> (MULTIFIELD, list)\n\
return names of subclasses for the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *g_classSubclasses(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O", &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    ClassSubclasses(clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* classSuperclasses */
static char g_classSuperclasses__doc__[] = "\
classSuperclasses(defclass, inherit) -> (MULTIFIELD, list)\n\
return names of superclasses for the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *g_classSuperclasses(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O", &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    ClassSuperclasses(clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* defclassModule */
static char g_defclassModule__doc__[] = "\
defclassModule(defclass) -> str\n\
retrieve the name of the module where the provided defclass resides\n\
returns: a string containing a module name\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *g_defclassModule(PyObject *self, PyObject *args) {
    char *module = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    module = DefclassModule(clips_defclass_value(p));
    if(!module) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    return Py_BuildValue("s", module);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* describeClass */
static char g_describeClass__doc__[] = "\
describeClass(output, defclass)\n\
print a descriptive summary of class\n\
arguments:\n\
  output (str) - logical name of output stream\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *g_describeClass(PyObject *self, PyObject *args) {
    char *lname = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "sO!", &lname, &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    DescribeClass(lname, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDefclass */
static char g_findDefclass__doc__[] = "\
findDefclass(name) -> defclass\n\
retrieve defclass object corresponding to the specified name\n\
returns: the defclass as a new object\n\
arguments:\n\
  name (str) - the name of the defclass to look for";
static PyObject *g_findDefclass(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefclass(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defclass_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getClassDefaultsMode */
FUNC_GET_ONLY(getClassDefaultsMode,
              g_getClassDefaultsMode,
              GetClassDefaultsMode,
              "i")

/* getDefclassList */
static char g_getDefclassList__doc__[] = "\
getDefclassList([module]) -> (MULTIFIELD, list)\n\
retrieve list of defclass objects names in specified defmodule\n\
returns:  MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDefclassList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDefclassList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefclassName */
static char g_getDefclassName__doc__[] = "\
getDefclassName(defclass) -> str\n\
retrieve the name of given defclass object\n\
returns: a string containing the name\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *g_getDefclassName(PyObject *self, PyObject *args) {
    char *name = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDefclassName(clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefclassPPForm */
static char g_getDefclassPPForm__doc__[] = "\
getDefclassPPForm(defclass) -> str\n\
retrieve the pretty-print form of given defclass object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *g_getDefclassPPForm(PyObject *self, PyObject *args) {
    char *s = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefclassPPForm(clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefclassWatchInstances */
static char g_getDefclassWatchInstances__doc__[] = "\
getDefclassWatchInstances(defclass) -> bool\n\
tell if defclass instances are being watched\n\
returns: True if defclass instances are being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *g_getDefclassWatchInstances(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    i = GetDefclassWatchInstances(clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefclassWatchSlots */
static char g_getDefclassWatchSlots__doc__[] = "\
getDefclassWatchSlots(defclass) -> bool\n\
tell if defclass slots are being watched\n\
returns: True if defclass slots are being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *g_getDefclassWatchSlots(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    i = GetDefclassWatchSlots(clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDefclass */
static char g_getNextDefclass__doc__[] = "\
getNextDefclass([defclass]) -> defclass\n\
find next defclass in the list, first if argument is omitted\n\
returns: next defclass object, None if already at last defclass\n\
arguments:\n\
  defclass (defclass) - the defclass to start from";
static PyObject *g_getNextDefclass(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefclassType, &p))
        FAIL();
    if(p)
        CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefclass(p ? clips_defclass_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defclass_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDefclassDeletable */
static char g_isDefclassDeletable__doc__[] = "\
isDefclassDeletable(defclass) -> bool\n\
tell whether or not the defclass can be deleted\n\
returns: True if the defclass can be deleted, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *g_isDefclassDeletable(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p)) FAIL();
    CHECK_DEFCLASS(p);
    i = IsDefclassDeletable(clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefclasses */
static char g_listDefclasses__doc__[] = "\
listDefclasses(logicalname [, module])\n\
list defclasses to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDefclasses(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefclasses(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setClassDefaultsMode */
static char g_setClassDefaultsMode__doc__[] = "\
setClassDefaultsMode(mode)\n\
set default mode for classes\n\
arguments:\n\
  mode (int) - the new default mode";
static PyObject *g_setClassDefaultsMode(PyObject *self, PyObject *args) {
    int i = 0;

    if(!PyArg_ParseTuple(args, "i", &i))
        FAIL();
    if(i != CONVENIENCE_MODE && i != CONSERVATION_MODE) {
        ERROR_VALUE("invalid mode value");
        FAIL();
    }
    SetClassDefaultsMode((unsigned short)i);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefclassWatchInstances */
static char g_setDefclassWatchInstances__doc__[] = "\
setDefclassWatchInstances(state, defclass)\n\
tell to system if defclass instances are to be watched\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass object";
static PyObject *g_setDefclassWatchInstances(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    SetDefclassWatchInstances(
        PyObject_IsTrue(state), clips_defclass_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefclassWatchSlots */
static char g_setDefclassWatchSlots__doc__[] = "\
setDefclassWatchSlots(state, defclass)\n\
tell to system if defclass slots are to be watched\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass object";
static PyObject *g_setDefclassWatchSlots(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "OO!", &state, &clips_DefclassType, &p))
        FAIL();
    CHECK_DEFCLASS(p);
    SetDefclassWatchSlots(PyObject_IsTrue(state), clips_defclass_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotAllowedValues */
static char g_slotAllowedValues__doc__[] = "\
slotAllowedValues(defclass, name) -> (MULTIFIELD, list)\n\
retrieve allowed values for specified slot\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotAllowedValues(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotAllowedValues(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* slotCardinality */
static char g_slotCardinality__doc__[] = "\
slotCardinality(defclass, name) -> (MULTIFIELD, list)\n\
retrieve cardinality information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotCardinality(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotCardinality(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

#if CLIPS_MINOR > 23

/* slotAllowedClasses */
static char g_slotAllowedClasses__doc__[] = "\
slotAllowedClasses(defclass, name) -> (MULTIFIELD, list)\n\
retrieve the allowed classes for a slot of given class\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_slotAllowedClasses(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &name))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotAllowedClasses(clips_defclass_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotDefaultValue */
static char g_slotDefaultValue__doc__[] = "\
slotDefaultValue(defclass, name) -> (MULTIFIELD, list)\n\
retrieve default value(s) for a slot of given defclass\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *g_slotDefaultValue(PyObject *self, PyObject *args) {
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &name))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotDefaultValue(clips_defclass_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(slotAllowedClasses, g_slotAllowedClasses)
UNIMPLEMENT_VERSION(slotDefaultValue, g_slotDefaultValue)
#endif /* CLIPS_MINOR > 23 */

/* slotDirectAccessP */
static char g_slotDirectAccessP__doc__[] = "\
slotDirectAccessP(defclass, name) -> bool\n\
tell if slot is directly accessible\n\
returns: True if slot is directly accessible, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotDirectAccessP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    i = SlotDirectAccessP(clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotExistP */
static char g_slotExistP__doc__[] = "\
slotExistP(defclass, name, inherit) -> bool\n\
tell if slot exists\n\
returns: True if slot exists, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotExistP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!sO", &clips_DefclassType, &p, &s, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    i = SlotExistP(clips_defclass_value(p), s, PyObject_IsTrue(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotFacets */
static char g_slotFacets__doc__[] = "\
slotFacets(defclass, name) -> (MULTIFIELD, list)\n\
retrieve facet values information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotFacets(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotFacets(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* slotInitableP */
static char g_slotInitableP__doc__[] = "\
slotInitableP(defclass, name) -> bool\n\
tell if slot is initializable\n\
returns: True if slot can be initialized, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotInitableP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    i = SlotInitableP(clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotPublicP */
static char g_slotPublicP__doc__[] = "\
slotPublicP(defclass, name) -> bool\n\
tell if slot is public\n\
returns: True if slot is public, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotPublicP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    i = SlotPublicP(clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotRange */
static char g_slotRange__doc__[] = "\
slotRange(defclass, name) -> (MULTIFIELD, list)\n\
retrieve numeric range information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotRange(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotRange(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* slotSources */
static char g_slotSources__doc__[] = "\
slotSources(defclass, name) -> (MULTIFIELD, list)\n\
retrieve the name of class sources for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotSources(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotSources(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* slotTypes */
static char g_slotTypes__doc__[] = "\
slotTypes(defclass, name) -> (MULTIFIELD, list)\n\
retrieve cardinality information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotTypes(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SlotTypes(clips_defclass_value(p), s, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* slotWritableP */
static char g_slotWritableP__doc__[] = "\
slotWritableP(defclass, name) -> bool\n\
tell whether slot can be overwritten or not\n\
returns: True if slot is writeable, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *g_slotWritableP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_DEFCLASS(p);
    i = SlotWritableP(clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* subclassP */
static char g_subclassP__doc__[] = "\
subclassP(defclass1, defclass2) -> bool\n\
tell if defclass1 is a subclass of defclass2\n\
returns: True if relationship is satisfied, False otherwise\n\
arguments:\n\
  defclass1, defclass2 (defclass) - test objects";
static PyObject *g_subclassP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL, *q = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_DefclassType, &p, &clips_DefclassType, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    CHECK_DEFCLASS(q);
    i = SubclassP(clips_defclass_value(p), clips_defclass_value(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* superclassP */
static char g_superclassP__doc__[] = "\
superclassP(defclass1, defclass2) -> bool\n\
tell if defclass1 is a superclass of defclass2\n\
returns: True if relationship is satisfied, False otherwise\n\
arguments:\n\
  defclass1, defclass2 (defclass) - test objects";
static PyObject *g_superclassP(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL, *q = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_DefclassType, &p, &clips_DefclassType, &q))
        FAIL();
    CHECK_DEFCLASS(p);
    CHECK_DEFCLASS(q);
    i = SuperclassP(clips_defclass_value(p), clips_defclass_value(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefclass */
static char g_undefclass__doc__[] = "\
undefclass(defclass)\n\
remove a class and all its subclasses from system\n\
arguments:\n\
  defclass (defclass) - the defclass to remove";
static PyObject *g_undefclass(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefclassType, &p))
        FAIL();
    CHECK_RM_DEFCLASS(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefclass(clips_defclass_value(p))) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.13 - Instances Functions */

/* binaryLoadInstances */
static char g_binaryLoadInstances__doc__[] = "\
binaryLoadInstances(filename) -> int\n\
binary load a set of instances from named file\n\
returns: the number of loaded instances\n\
arguments:\n\
  filename (str) - the name of binary file to load from";
static PyObject *g_binaryLoadInstances(PyObject *self, PyObject *args) {
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = BinaryLoadInstances(fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    else RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* binarySaveInstances */
static char g_binarySaveInstances__doc__[] = "\
binarySaveInstances(filename, scope) -> int\n\
dump instances to file\n\
returns: the number of saved instances\n\
arguments:\n\
  filename (str) - the name of binary file to save to\n\
  scope (int) - one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *g_binarySaveInstances(PyObject *self, PyObject *args) {
    char *fn = NULL;
    long i = 0;

    if(!PyArg_ParseTuple(args, "si", &fn, &i)) FAIL();
    if(i != LOCAL_SAVE && i != VISIBLE_SAVE) {
        ERROR_VALUE("invalid scope");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    i = BinarySaveInstances(fn, i, NULL, TRUE);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    } /* do not know */
    else RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* createRawInstance */
static char g_createRawInstance__doc__[] = "\
createRawInstance(defclass, name) -> instance\n\
create an instance of specified class with given name\n\
returns: the instance as a new object\n\
arguments:\n\
  defclass (defclass) - the defclass to create an instance of\n\
  name (str) - the name of the instance";
static PyObject *g_createRawInstance(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    clips_InstanceObject *q = NULL;
    void *ptr = 0;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_DefclassType, &p, &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = CreateRawInstance(clips_defclass_value(p), name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* decrementInstanceCount */
UNIMPLEMENT(decrementInstanceCount, g_decrementInstanceCount)

/* deleteInstance */
static char g_deleteInstance__doc__[] = "\
deleteInstance([instance])\n\
delete specified instance\n\
arguments:\n\
  instance (instance) - the instance to delete, all if omitted";
static PyObject *g_deleteInstance(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!DeleteInstance(p ? clips_instance_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* directGetSlot */
static char g_directGetSlot__doc__[] = "\
directGetSlot(instance, slotname) -> (type, value)\n\
get value in specified slot of given instance\n\
returns: a pair (type, value)\n\
arguments:\n\
  instance (instance) - the instance to inspect\n\
  slotname (str) - the slot to retrieve";
static PyObject *g_directGetSlot(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    PyObject *q = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_InstanceType, &p, &name))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    DirectGetSlot(clips_instance_value(p), name, &o);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* directPutSlot */
static char g_directPutSlot__doc__[] = "\
directPutSlot(instance, slotname, (type, value))\n\
put a value in specified slot of given instance\n\
arguments:\n\
  instance (instance) - the instance to inspect\n\
  slotname (str) - the slot to retrieve\n\
  type (int) - the type of value to assign\n\
  value (object or list of pairs) - the value to assign";
static PyObject *g_directPutSlot(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    PyObject *q = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!sO", &clips_InstanceType, &p, &name, &q))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    if(!i_py2do(q, &o)) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!DirectPutSlot(clips_instance_value(p), name, &o)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_OTHER("instance slot could not be modified");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findInstance */
static char g_findInstance__doc__[] = "\
findInstance(name, srchimports [, module]) -> instance\n\
find the instance with the specified name\n\
returns: the instance as a new object\n\
arguments:\n\
  name (str) - instance name\n\
  srchimports (bool) - True if imported modules have to be inspected\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_findInstance(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    clips_InstanceObject *q = NULL;
    char *name = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "sO|O!",
                         &name, &p, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindInstance(
        module ? clips_defmodule_value(module) : NULL,
        name, PyObject_IsTrue(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getInstanceClass */
static char g_getInstanceClass__doc__[] = "\
getInstanceClass(instance) -> defclass\n\
retrieve the class of specified instance\n\
returns: the instance defclass as a new object\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *g_getInstanceClass(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    clips_DefclassObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetInstanceClass(clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defclass_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getInstanceName */
static char g_getInstanceName__doc__[] = "\
getInstanceName(instance) -> str\n\
retrieve the name of specified instance\n\
returns: the requested name of instance\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *g_getInstanceName(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetInstanceName(clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getInstancePPForm */
static char g_getInstancePPForm__doc__[] = "\
getInstancePPForm(instance) -> str\n\
retrieve the pretty-print form of specified instance\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *g_getInstancePPForm(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    GetInstancePPForm(buffer, ppbuffer_size-1, clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getInstancesChanged */
STATUS_FUNC_GET_BOOL(getInstancesChanged,
                     g_getInstancesChanged,
                     GetInstancesChanged)

/* getNextInstance */
static char g_getNextInstance__doc__[] = "\
getNextInstance([instance]) -> instance\n\
retrieve next instance object in list, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  instance (defrule) - the instance to start from";
static PyObject *g_getNextInstance(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_InstanceType, &p))
        FAIL();
    if(p)
        CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextInstance(p ? clips_instance_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getNextInstanceInClass */
static char g_getNextInstanceInClass__doc__[] = "\
getNextInstanceInClass(defclass [, instance]) -> instance\n\
retrieve next instance object in class, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  instance (instance) - the instance to start from";
static PyObject *g_getNextInstanceInClass(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL, *q = NULL;
    clips_DefclassObject *c = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_DefclassType, &c, &clips_InstanceType, &p))
        FAIL();
    CHECK_DEFCLASS(c);
    if(p)
        CHECK_VALID_INSTANCE(p);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextInstanceInClass(
        clips_defclass_value(c), p ? clips_instance_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getNextInstanceInClassAndSubclasses */
static char g_getNextInstanceInClassAndSubclasses__doc__[] = "\
getNextInstanceInClassAndSubclasses(defclass [, instance]) -> instance\n\
retrieve next instance in class/subclasses, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  instance (instance) - the instance to start from";
static PyObject *g_getNextInstanceInClassAndSubclasses(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL, *q = NULL;
    clips_DefclassObject *c = NULL;
    DATA_OBJECT o = { 0 };
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_DefclassType, &c, &clips_InstanceType, &p))
        FAIL();
    CHECK_DEFCLASS(c);
    if(p)
        CHECK_VALID_INSTANCE(p);
    /* we should iterate from the start in order to keep the iteration data */
    ACQUIRE_MEMORY_ERROR();
    if(p) {
        ptr = GetNextInstanceInClassAndSubclasses_PY(
            clips_defclass_value(c), NULL, &o);
        /* move cursor to the instance we passed in */
        while(ptr && ptr != clips_instance_value(p))
            ptr = GetNextInstanceInClassAndSubclasses_PY(
                clips_defclass_value(c), ptr, &o);
        /* move cursor one step forward if conditions met (may return NULL) */
        if(ptr)
            ptr = GetNextInstanceInClassAndSubclasses_PY(
                clips_defclass_value(c), ptr, &o);
    } else
        ptr = GetNextInstanceInClassAndSubclasses_PY(
            clips_defclass_value(c), NULL, &o);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}


/* incrementInstanceCount */
UNIMPLEMENT(incrementInstanceCount, g_incrementInstanceCount)

/* instances */
static char g_instances__doc__[] = "\
instances(output [, module] [, class [, subclassflag]]])\n\
list instances in specified defmodule to logical output\n\
arguments:\n\
  output (str) - the name of logical output stream\n\
  module (defmodule) - the defmodule to inspect, all if omitted\n\
  class (str) - the class name to inspect, all if omitted\n\
  subclassflag (bool) - True if all subclasses must be recursed";
static PyObject *g_instances(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = Py_None;
    char *name = NULL, *output = NULL;

    if(!PyArg_ParseTuple(args, "s|O!sO", &output,
       &clips_DefmoduleType, &module, &name, &p)) {
        PyErr_Clear();
        module = NULL;
        if(!PyArg_ParseTuple(args, "s|sO", &output, &name, &p)) {
            PyErr_Clear();
            ERROR_TYPE("invalid argument(s) or invalid argument order");
            FAIL();
        }
    }
    ACQUIRE_MEMORY_ERROR();
    Instances(
        output, module ? clips_defmodule_value(module) : NULL,
        name, p ? PyObject_IsTrue(p) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* loadInstances */
static char g_loadInstances__doc__[] = "\
loadInstances(filename) -> int\n\
load instance from specified file\n\
returns: the number of loaded instances\n\
arguments:\n\
  filename (str) - the name of file to load instances from";
static PyObject *g_loadInstances(PyObject *self, PyObject *args) {
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = LoadInstances(fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* makeInstance */
static char g_makeInstance__doc__[] = "\
makeInstance(command) -> instance\n\
create and initialize an instance using given command\n\
returns: a new instance\n\
arguments:\n\
  command (str) - command used to create instance";
static PyObject *g_makeInstance(PyObject *self, PyObject *args) {
    clips_InstanceObject *q = NULL;
    char *s = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "s", &s))
        FAIL();
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    ptr = MakeInstance(s);
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_instance_New(GetCurrentEnvironment(), q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    CHECK_VALID_INSTANCE(q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* restoreInstances */
static char g_restoreInstances__doc__[] = "\
restoreInstances(filename) -> int\n\
restore instance from specified file\n\
returns: number of restored instances\n\
arguments:\n\
  filename (str) - the name of file to restore instances from";
static PyObject *g_restoreInstances(PyObject *self, PyObject *args) {
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "s", &fn))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = RestoreInstances(fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* saveInstances */
static char g_saveInstances__doc__[] = "\
saveInstances(filename, scope) -> int\n\
save instances to specified file\n\
returns: the number of saved instances\n\
arguments:\n\
  filename (str) - the name of file to save instances to\n\
  scope (int) - one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *g_saveInstances(PyObject *self, PyObject *args) {
    char *fn = NULL;
    long i = 0;

    if(!PyArg_ParseTuple(args, "si", &fn, &i))
        FAIL();
    if(i != LOCAL_SAVE && i != VISIBLE_SAVE) {
        ERROR_VALUE("invalid scope");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    i = SaveInstances(fn, i, NULL, TRUE);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    } /* do not know */
    else
        RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* send */
static char g_send__doc__[] = "\
send(instance, message [, arguments])\n\
send a message to specified object\n\
returns: the result value for the operation if any\n\
arguments:\n\
  instance (instance) - object to send message to\n\
  message (str) - message to send\n\
  arguments (str) - blank separated constant arguments";
static PyObject *g_send(PyObject *self, PyObject *args) {
    PyObject *p = NULL, *q = NULL;
    char *msg = NULL, *msa = NULL;
    DATA_OBJECT o = { 0 }, rv = { 0 };

    if(!PyArg_ParseTuple(args, "O!s|s", &clips_InstanceType, &p, &msg, &msa))
        FAIL();
    CHECK_VALID_INSTANCE(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    SetType(o, INSTANCE_ADDRESS);
    SetValue(o, clips_instance_value(p));
    Send(&o, msg, msa, &rv);
    q = i_do2py(&rv);
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    if(q) {
        RETURN_PYOBJECT(q);
    } else RETURN_NONE();

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* setInstancesChanged */
STATUS_FUNC_SET_BOOL(setInstancesChanged,
                     g_setInstancesChanged,
                     SetInstancesChanged)

/* unmakeInstance */
static char g_unmakeInstance__doc__[] = "\
unmakeInstance([instance])\n\
delete specified instance (passing a message)\n\
arguments:\n\
  instance (instance) - instance to delete, all if omitted";
static PyObject *g_unmakeInstance(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    if(p)
        CHECK_VALID_INSTANCE(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!UnmakeInstance(p ? clips_instance_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* validInstanceAddress */
static char g_validInstanceAddress__doc__[] = "\
validInstanceAddress(instance) -> bool\n\
tell whether the instance still exists or not\n\
returns: True if instance exists, False otherwise\n\
arguments:\n\
  instance (instance) - istance to test";
static PyObject *g_validInstanceAddress(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_InstanceType, &p))
        FAIL();
    i = ValidInstanceAddress(clips_instance_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* loadInstancesFromString */
static char g_loadInstancesFromString__doc__[] = "\
loadInstancesFromString(string [, maxpos]) -> int\n\
load instances from a string\n\
returns: the number of loaded instances\n\
arguments:\n\
  string (str) - string to read from\n\
  maxpos (int) - last char to read, all string if omitted";
static PyObject *g_loadInstancesFromString(PyObject *self, PyObject *args) {
    char *s = NULL;
    int i = -1;

    if(!PyArg_ParseTuple(args, "s|i", &s, &i))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = LoadInstancesFromString(s, i);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* restoreInstancesFromString */
static char g_restoreInstancesFromString__doc__[] = "\
restoreInstancesFromString(string [, maxpos]) -> int\n\
restore instances from a string\n\
returns: number of restored instances\n\
arguments:\n\
  string (str) - string to read from\n\
  maxpos (int) - last char to read, all string if omitted";
static PyObject *g_restoreInstancesFromString(PyObject *self, PyObject *args) {
    char *s = NULL;
    int i = -1;

    if(!PyArg_ParseTuple(args, "s|i", &s, &i))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = RestoreInstancesFromString(s, i);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.14 - DefmessageHandler Functions */

/* findDefmessageHandler */
static char g_findDefmessageHandler__doc__[] = "\
findDefmessageHandler(defclass, name, type) -> int\n\
find the matching message handler attached to defclass\n\
returns: index of message handler\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect object\n\
  name (str) - message handler name\n\
  type (str) - one of 'around', 'before', 'primary', 'after'";
static PyObject *g_findDefmessageHandler(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *name = NULL, *type = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!ss", &clips_DefclassType, &p, &name, &type))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    i = (int)FindDefmessageHandler(clips_defclass_value(p), name, type);
    RELEASE_MEMORY_ERROR();
    CHECK_DEFCLASS(p);
    if(i == 0) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmessageHandlerList */
static char g_getDefmessageHandlerList__doc__[] = "\
getDefmessageHandlerList([defclass [, inh]]) -> (MULTIFIELD, list)\n\
retrieve list of message handlers attached to defclass\n\
returns: MULTIFIELD and a list of pairs (STRING, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect, all if omitted\n\
  inh (bool) - True if inherited handlers are to list";
static PyObject *g_getDefmessageHandlerList(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *p1 = NULL, *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!O", &clips_DefclassType, &p, &p1))
        FAIL();
    if(p)
        CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    GetDefmessageHandlerList(
        p ? clips_defclass_value(p) : NULL,
        &o, p1 ? PyObject_IsTrue(p1) : FALSE);
    q = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getDefmessageHandlerName */
static char g_getDefmessageHandlerName__doc__[] = "\
getDefmessageHandlerName(defclass, index) -> str\n\
retrieve the name of specified message handler\n\
returns: name as string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_getDefmessageHandlerName(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *name = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i",  &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDefmessageHandlerName(clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmessageHandlerPPForm */
static char g_getDefmessageHandlerPPForm__doc__[] = "\
getDefmessageHandlerPPForm(defclass, index) -> str\n\
retrieve the pretty-print form of specified message handler\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_getDefmessageHandlerPPForm(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i",  &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefmessageHandlerPPForm(clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmessageHandlerType */
static char g_getDefmessageHandlerType__doc__[] = "\
getDefmessageHandlerType(defclass, index) -> str\n\
retrieve type of specified message handler\n\
returns: type as string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_getDefmessageHandlerType(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i",  &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefmessageHandlerType(clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmessageHandlerWatch */
static char g_getDefmessageHandlerWatch__doc__[] = "\
getDefmessageHandlerWatch(defclass, index) -> bool\n\
tell if specified message handler is being watched\n\
returns: True if the message handler is being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_getDefmessageHandlerWatch(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i",  &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    i = GetDefmessageHandlerWatch(clips_defclass_value(p), (unsigned int)i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDefmessageHandler */
static char g_getNextDefmessageHandler__doc__[] = "\
getNextDefmessageHandler(defclass [, index]) -> int\n\
return index of next message handler for specified class\n\
returns: index as an integer, None if already at last handler\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of current handler, 0 or omitted for first";
static PyObject *g_getNextDefmessageHandler(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!|i", &clips_DefclassType, &p, &i))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    i = GetNextDefmessageHandler(clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    if(i == 0)
        RETURN_NONE();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* isDefmessageHandlerDeletable */
static char g_isDefmessageHandlerDeletable__doc__[] = "\
isDefmessageHandlerDeletable(defclass, index) -> bool\n\
tell whether or not the specified message handler can be deleted\n\
returns: True if the message handler can be deleted, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_isDefmessageHandlerDeletable(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i", &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    i = IsDefmessageHandlerDeletable(clips_defclass_value(p), (unsigned int)i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefmessageHandlers */
static char g_listDefmessageHandlers__doc__[] = "\
listDefmessageHandlers(output [, defclass [, inhflag]])\n\
list message handlers to logical output\n\
arguments:\n\
  output (str) - the name of output stream\n\
  defclass (defclass) - the defclass to inspect\n\
  inhflag (bool) - True to list inherited handlers";
static PyObject *g_listDefmessageHandlers(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    PyObject *p1 = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "s|O!O", &s, &clips_DefclassType, &p, &p1))
        FAIL();
    if(p)
        CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    ListDefmessageHandlers(s,
        p ? clips_defclass_value(p) : NULL, p1 ? PyObject_IsTrue(p1) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* previewSend */
static char g_previewSend__doc__[] = "\
previewSend(output, defclass, messagename)\n\
list message handlers applicable to instances to logical output\n\
arguments:\n\
  output (str) - logical output stream name\n\
  defclass (defclass) - the defclass to inspect\n\
  messagename (str) - the name of message";
static PyObject *g_previewSend(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    char *s = NULL, *m = NULL;

    if(!PyArg_ParseTuple(args, "sO!s", &s, &clips_DefclassType, &p, &m))
        FAIL();
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    PreviewSend(s, clips_defclass_value(p), m);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setDefmessageHandlerWatch */
static char g_setDefmessageHandlerWatch__doc__[] = "\
setDefmessageHandlerWatch(state, defclass, index)\n\
set watch on message handler to specified state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_setDefmessageHandlerWatch(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "OO!i", &state, &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    SetDefmessageHandlerWatch(
        PyObject_IsTrue(state), clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefmessageHandler */
static char g_undefmessageHandler__doc__[] = "\
undefmessageHandler(defclass, index)\n\
remove specified message handler from system\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *g_undefmessageHandler(PyObject *self, PyObject *args) {
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i", &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_DEFCLASS(p);
    ACQUIRE_MEMORY_ERROR();
    if(!UndefmessageHandler(clips_defclass_value(p), (unsigned int)i)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.15 - Definstances Functions */

/* helper to check whether there is still this construct */
F_INLINE void *definstancesExists(void *ptr) {
    void *rv = GetNextDefinstances(NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = GetNextDefinstances(rv);
    }
    return NULL;
}
#define PYDEFINSTANCES_EXISTS(_p) \
    definstancesExists(clips_definstances_value(_p))
#define CHECK_DEFINSTANCES(_p) do { \
        if(!PYDEFINSTANCES_EXISTS(_p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define CHECK_RM_DEFINSTANCES(_p) do { \
        if(_p && !PYDEFINSTANCES_EXISTS(_p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* actual functions with documentation */

/* definstancesModule */
static char g_definstancesModule__doc__[] = "\
definstancesModule(definstances) -> str\n\
retrieve the module name where specified definstances is defined\n\
returns: module name as string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *g_definstancesModule(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefinstancesType, &p))
        FAIL();
    CHECK_DEFINSTANCES(p);
    ACQUIRE_MEMORY_ERROR();
    s = DefinstancesModule(clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* findDefinstances */
static char g_findDefinstances__doc__[] = "\
findDefinstances(name) -> definstances\n\
return the definstances object associated with name\n\
returns: the definstances as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *g_findDefinstances(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DefinstancesObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name)) FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefinstances(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) { ERROR_CLIPS_NOTFOUND(); FAIL(); }
    clips_definstances_New(p);
    if(!p) { ERROR_MEMORY_CREATION(); FAIL(); }
    clips_definstances_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefinstancesList */
static char g_getDefinstancesList__doc__[] = "\
getDefinstancesList([module]) -> (MULTIFIELD, list)\n\
retrieve the list of definstances objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_getDefinstancesList(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    GetDefinstancesList(&o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefinstancesName */
static char g_getDefinstancesName__doc__[] = "\
getDefinstancesName(definstances) -> str\n\
retrieve the name of specified definstances\n\
returns: name as a string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *g_getDefinstancesName(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefinstancesType, &p))
        FAIL();
    CHECK_DEFINSTANCES(p);
    ACQUIRE_MEMORY_ERROR();
    name = GetDefinstancesName(clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefinstancesPPForm */
static char g_getDefinstancesPPForm__doc__[] = "\
getDefinstancesPPForm(definstances) -> str\n\
retrieve the pretty-print form of specified definstances\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *g_getDefinstancesPPForm(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefinstancesType, &p))
        FAIL();
    CHECK_DEFINSTANCES(p);
    ACQUIRE_MEMORY_ERROR();
    s = GetDefinstancesPPForm(clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDefinstances */
static char g_getNextDefinstances__doc__[] = "\
getNextDefinstances([definstances]) -> definstances\n\
retrieve next definstances object in list, first if argument is omitted\n\
returns: a definstances object, None if already at last definstances\n\
arguments:\n\
  definstances (definstances) - the definstances to start from";
static PyObject *g_getNextDefinstances(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefinstancesType, &p))
        FAIL();
    if(p) { CHECK_DEFINSTANCES(p); }
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefinstances(p ? clips_definstances_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_definstances_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_definstances_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* isDefinstancesDeletable */
static char g_isDefinstancesDeletable__doc__[] = "\
isDefinstancesDeletable(definstances) -> bool\n\
tell whether or not the definstances can be deleted\n\
returns: True if the definstances can be deleted, False otherwise\n\
arguments:\n\
  definstances (definstances) - the definstances to be inspected";
static PyObject *g_isDefinstancesDeletable(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefinstancesType, &p))
        FAIL();
    CHECK_DEFINSTANCES(p);
    i = IsDefinstancesDeletable(clips_definstances_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* listDefinstances */
static char g_listDefinstances__doc__[] = "\
listDefinstances(logicalname [, module])\n\
list definstances to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *g_listDefinstances(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "s|O!", &lname, &clips_DefmoduleType, &module))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefinstances(lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* undefinstances */
static char g_undefinstances__doc__[] = "\
undefinstances([definstances])\n\
delete a definstances object or all definstances objects\n\
arguments:\n\
  definstances (definstances) - object to be deleted, all if omitted";
static PyObject *g_undefinstances(PyObject *self, PyObject *args) {
    clips_DefinstancesObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefinstancesType, &p))
        FAIL();
    CHECK_RM_DEFINSTANCES(p);
    CLIPS_LOCK_GC();
    ACQUIRE_MEMORY_ERROR();
    if(!Undefinstances(p ? clips_definstances_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        CLIPS_UNLOCK_GC();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    CLIPS_UNLOCK_GC();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.16 - Defmodule functions */

/* findDefmodule */
static char g_findDefmodule__doc__[] = "\
findDefmodule(name) -> defmodule\n\
return the defmodule object associated with name\n\
returns: the defmodule as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *g_findDefmodule(PyObject *self, PyObject *args) {
    char *name = NULL;
    void *ptr = NULL;
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = FindDefmodule(name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getCurrentModule */
static char g_getCurrentModule__doc__[] = "\
getCurrentModule() -> defmodule\n\
return current module\n\
returns: current module as a new object";
static PyObject *g_getCurrentModule(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    void *ptr = NULL;

    CHECK_NOARGS(args);
    ACQUIRE_MEMORY_ERROR();
    ptr = GetCurrentModule();
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefmoduleList */
static char g_getDefmoduleList__doc__[] = "\
getDefmoduleList() -> (MULTIFIELD, list)\n\
return the list of modules in system\n\
returns: MULTIFIELD and a list of pairs (STRING, name)";
static PyObject *g_getDefmoduleList(PyObject *self, PyObject *args) {
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    CHECK_NOARGS(args);
    ACQUIRE_MEMORY_ERROR();
    GetDefmoduleList(&o);
    p = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getDefmoduleName */
static char g_getDefmoduleName__doc__[] = "\
getDefmoduleName(defmodule) -> str\n\
retrieve the name of specified defmodule\n\
returns: name as a string\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to inspect";
static PyObject *g_getDefmoduleName(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    name = GetDefmoduleName(clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) { ERROR_CLIPS_RETVAL(); FAIL(); }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getDefmodulePPForm */
static char g_getDefmodulePPForm__doc__[] = "\
getDefmodulePPForm(defmodule) -> str\n\
retrieve the pretty-print form of specified defmodule\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to inspect";
static PyObject *g_getDefmodulePPForm(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    s = GetDefmodulePPForm(clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getNextDefmodule */
static char g_getNextDefmodule__doc__[] = "\
getNextDefmodule([defmodule]) -> defmodule\n\
retrieve next defmodule object in list, first if argument is omitted\n\
returns: a defmodule object, None if already at last defmodule\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to start from";
static PyObject *g_getNextDefmodule(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "|O!", &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ptr = GetNextDefmodule(p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defmodule_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* listDefmodules */
static char g_listDefmodules__doc__[] = "\
listDefmodules(output)\n\
list modules to logical output\n\
arguments:\n\
  output (str) - logical name of output stream";
static PyObject *g_listDefmodules(PyObject *self, PyObject *args) {
    char *output = NULL;

    if(!PyArg_ParseTuple(args, "s", &output))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    ListDefmodules(output);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setCurrentModule */
static char g_setCurrentModule__doc__[] = "\
setCurrentModule(defmodule)\n\
set current module to the one specified\n\
arguments:\n\
  defmodule (defmodule) - new current defmodule";
static PyObject *g_setCurrentModule(PyObject *self, PyObject *args) {
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_DefmoduleType, &p))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    SetCurrentModule(clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* sendCommand [undocumented] */
static char g_sendCommand__doc__[] = "\
sendCommand(command [, verbose])\n\
send a full command to the engine as if it was typed at the prompt\n\
arguments:\n\
  command (str) - the complete command to send\n\
  verbose (bool) - if True command can produce output";
static PyObject *g_sendCommand(PyObject *self, PyObject *args) {
    void *env = NULL;
    char *command = NULL;
    int res = 0, verbose = FALSE;
    PyObject *v = NULL;

    if(!PyArg_ParseTuple(args, "s|O", &command, &v))
        FAIL();
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    verbose = (v && PyObject_IsTrue(v)) ? TRUE : FALSE;
    ACQUIRE_MEMORY_ERROR();
    FlushPPBuffer(env);
    SetPPBufferStatus(env, OFF);
    RouteCommand(env, command, verbose);
    res = GetEvaluationError(env);
    FlushPPBuffer(env);
    SetHaltExecution(env, FALSE);
    SetEvaluationError(env, FALSE);
    FlushBindList(env);
    RELEASE_MEMORY_ERROR();
    if(res) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* forceCleanup() [undocumented] */
static char g_forceCleanup__doc__[] = "\
forceCleanup([alldepths, heuristics])\n\
attempt to force a garbage collection\n\
arguments:\n\
  alldepths (bool) - True to clean up all depths (default)\n\
  heuristics (bool) - True to use heuristics (default)";
static PyObject *g_forceCleanup(PyObject *self, PyObject *args) {
    void *env = GetCurrentEnvironment();
    PyObject *alldepths = NULL, *heuristics = NULL;

    if(!PyArg_ParseTuple(args, "|OO", &alldepths, &heuristics))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(EngineData(env)->ExecutingRule != NULL) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPSSYS_CLEANUP();
        FAIL();
    }
    PeriodicCleanup(env,
        alldepths ? TRUE : PyObject_IsTrue(alldepths),
        heuristics ? TRUE : PyObject_IsTrue(heuristics));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* ======================================================================== */



/* helper to check if an environment is safe to use */
#define CHECK_VALID_ENVIRONMENT(_e) do { \
        if(!clips_environment_valid(_e)) { \
            ERROR_CLIPSSYS_BADENV(); \
            FAIL(); \
        } \
    } while(0)

/* AddClearFunction */
UNIMPLEMENT(env_addClearFunction, e_addClearFunction)
/* AddPeriodicFunction */
UNIMPLEMENT(env_addPeriodicFunction, e_addPeriodicFunction)
/* AddResetFunction */
UNIMPLEMENT(env_addResetFunction, e_addResetFunction)

/* env_bload */
static char e_bload__doc__[] = "\
env_bload(env, filename)\n\
load a binary image of environment constructs\n\
arguments:\n\
  filename (str) - the name of binary file to load from";
static PyObject *e_bload(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvBload(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_bsave */
static char e_bsave__doc__[] = "\
env_bsave(env, filename)\n\
save a binary image of environment constructs\n\
arguments:\n\
  filename (str) - the name of binary file to save to";
static PyObject *e_bsave(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvBsave(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_clear */
static char e_clear__doc__[] = "\
env_clear(env)\n\
clear environment";
static PyObject *e_clear(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvClear_PY(env)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPSSYS_ENVNOCLEAR();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
#ifdef USE_CLEAR_RESETGCCOUNTERS
    ENV_RESET_ASSERTED_FACTS(pyenv);
#endif /* USE_CLEAR_RESETGCCOUNTERS */
    ENV_LOSE_HASH_FACTS(pyenv);
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* functionCall */
static char e_functionCall__doc__[] = "\
env_functionCall(funcname [, args]) -> (type, value)\n\
call an internal function\n\
returns: the function result, as a pair (type, return value)\n\
arguments:\n\
  funcname (str) - the internal function or operator name\n\
  args (str) - string containing a blank separated list of arguments";
static PyObject *e_functionCall(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *func = NULL, *fargs = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s|s",
                         &clips_EnvType, &pyenv, &func, &fargs))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(EnvFunctionCall(env, func, fargs, &o)) {
        SetEvaluationError(env, FALSE);
        SetHaltExecution(env, FALSE);
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_FUNCCALL();
        FAIL();
    }
    p = i_do2py_e(env, &o);
    SetEvaluationError(env, FALSE);
    SetHaltExecution(env, FALSE);
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getAutoFloatDividend */
E_STATUS_FUNC_GET_BOOL(env_getAutoFloatDividend,
                       e_getAutoFloatDividend,
                       EnvGetAutoFloatDividend)

/* getDynamicConstraintChecking */
E_STATUS_FUNC_GET_BOOL(env_getDynamicConstraintChecking,
                       e_getDynamicConstraintChecking,
                       EnvGetDynamicConstraintChecking)

/* getSequenceOperatorRecognition */
E_STATUS_FUNC_GET_BOOL(env_getSequenceOperatorRecognition,
                       e_getSequenceOperatorRecognition,
                       EnvGetSequenceOperatorRecognition)

/* getStaticConstraintChecking */
E_STATUS_FUNC_GET_BOOL(env_getStaticConstraintChecking,
                       e_getStaticConstraintChecking,
                       EnvGetStaticConstraintChecking)

/* initializeEnvironment: NEVER TO BE IMPLEMENTED */
/*        the environment is initialized at module load time */


/* env_load */
static char e_load__doc__[] = "\
env_load(env, filename)\n\
load constructs into environment\n\
arguments:\n\
  filename (str) - the name of file to load constructs from";
static PyObject *e_load(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    int rv;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    rv = EnvLoad(env, fn);
    RELEASE_MEMORY_ERROR();
    if(rv == 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    if(rv < 0) {
        ERROR_CLIPS_PARSEF();
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* removeClearFunction */
UNIMPLEMENT(env_removeClearFunction, e_removeClearFunction)

/* removePeriodicFunction */
UNIMPLEMENT(env_removePeriodicFunction, e_removePeriodicFunction)

/* removeResetFunction */
UNIMPLEMENT(env_removeResetFunction, e_removeResetFunction)


/* env_reset */
static char e_reset__doc__[] = "\
env_reset(env)\n\
reset environment";
static PyObject *e_reset(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvReset(env);
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_save */
static char e_save__doc__[] = "\
env_save(env, filename)\n\
save constructs to a file\n\
arguments:\n\
  filename (str) - the name of file to save constructs to";
static PyObject *e_save(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    char *fn = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvSave(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* setAutoFloatDividend */
E_STATUS_FUNC_SET_BOOL(env_setAutoFloatDividend,
                       e_setAutoFloatDividend,
                       EnvSetAutoFloatDividend)

/* setDynamicConstraintChecking */
E_STATUS_FUNC_SET_BOOL(env_setDynamicConstraintChecking,
                       e_setDynamicConstraintChecking,
                       EnvSetDynamicConstraintChecking)

/* setSequenceOperatorRecognition */
E_STATUS_FUNC_SET_BOOL(env_setSequenceOperatorRecognition,
                       e_setSequenceOperatorRecognition,
                       EnvSetSequenceOperatorRecognition)

/* setStaticConstraintChecking */
E_STATUS_FUNC_SET_BOOL(env_setStaticConstraintChecking,
                       e_setStaticConstraintChecking,
                       EnvSetStaticConstraintChecking)


/* env_batchStar */
static char e_batchStar__doc__[] = "\
env_batchStar(env, filename)\n\
batch execute commands stored in specified file\n\
arguments:\n\
  filename (str) - the name of file to read commands from";
static PyObject *e_batchStar(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvBatchStar(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_build */
static char e_build__doc__[] = "\
env_build(env, construct)\n\
define specified construct\n\
arguments:\n\
  construct (str) - the construct to be added";
static PyObject *e_build(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *cons = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &cons))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvBuild(env, cons)) {
        SetEvaluationError(env, FALSE);
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEX();
        FAIL();
    }
    SetEvaluationError(env, FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_eval */
static char e_eval__doc__[] = "\
env_eval(env, expression) -> (type, value)\n\
evaluate the provided expression\n\
returns: a pair holding the result in the form (type, return value)\n\
arguments:\n\
  expression (str) - the expression to evaluate";
static PyObject *e_eval(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *expr = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &expr))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvEval(env, expr, &o)) {
        SetEvaluationError(env, FALSE);
        SetHaltExecution(env, FALSE);
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_EVALX();
        FAIL();
    }
    SetEvaluationError(env, FALSE);
    SetHaltExecution(env, FALSE);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p)
        RETURN_NONE();
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}


/* dribbleActive */
E_STATUS_FUNC_GET_BOOL(env_dribbleActive, e_dribbleActive, EnvDribbleActive)

/* dribbleOff */
E_FUNC_VOID_BOOL(env_dribbleOff, e_dribbleOff, EnvDribbleOff)



/* env_dribbleOn */
static char e_dribbleOn__doc__[] = "\
env_dribbleOn(env, filename)\n\
turn the dribble function on\n\
arguments:\n\
  filename (str) - the name of file to write dribble information to";
static PyObject *e_dribbleOn(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvDribbleOn(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getWatchItem */
static char e_getWatchItem__doc__[] = "\
env_getWatchItem(env, item) -> bool\n\
tell whether the specified item is watched or not\n\
returns: True if specified item is being watched, False otherwise\n\
arguments:\n\
  item (str) - the item to monitor the status of, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *e_getWatchItem(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *item = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &item))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    rv = EnvGetWatchItem(env, item);
    if(rv < 0) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    return Py_BuildValue("i", rv ? 1 : 0);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_unwatch */
static char e_unwatch__doc__[] = "\
env_unwatch(env, item)\n\
turn off tracing for specified item\n\
arguments:\n\
  item (str) - the item to disable tracing for, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *e_unwatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *item = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &item))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUnwatch(env, item)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_watch */
static char e_watch__doc__[] = "\
env_watch(env, item)\n\
turn on tracing for specified item\n\
arguments:\n\
  item (str) - the item to enable tracing for, can be one of\n\
               the following: facts, rules, focus, activations,\n\
               compilations, statistics, globals, slots, instances,\n\
               messages, message-handlers, generic-functions,\n\
               method or deffunctions.";
static PyObject *e_watch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *item = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &item)) FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvWatch(env, item)) {
         RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.3 [E] - Deftemplate Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_deftemplateExists(void *env, void *ptr) {
    void *rv = EnvGetNextDeftemplate(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDeftemplate(env, rv);
    }
    return NULL;
}
#define EPYDEFTEMPLATE_EXISTS(_e, _p) \
    env_deftemplateExists(_e, clips_deftemplate_value(_p))
#define ECHECK_DEFTEMPLATE(_e, _p) do { \
        if(!EPYDEFTEMPLATE_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFTEMPLATE(_e, _p) do { \
        if(_p && !EPYDEFTEMPLATE_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_deftemplateModule */
static char e_deftemplateModule__doc__[] = "\
env_deftemplateModule(env, deftemplate) -> str\n\
retrieve the name of the module where the provided deftemplate resides\n\
returns: a string containing a module name\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect";
static PyObject *e_deftemplateModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDeftemplateModule(env, clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#if CLIPS_MINOR > 23

/* deftemplateSlotAllowedValues */
static char e_deftemplateSlotAllowedValues__doc__[] = "\
env_deftemplateSlotAllowedValues(env, deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the allowed values for a slot of the specified deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotAllowedValues(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotAllowedValues(env, clips_deftemplate_value(p), name, &o);
    rv = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotCardinality */
static char e_deftemplateSlotCardinality__doc__[] = "\
env_deftemplateSlotCardinality(env, deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the cardinality for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotCardinality(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotCardinality(env, clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotDefaultP */
static char e_deftemplateSlotDefaultP__doc__[] = "\
env_deftemplateSlotCardinality(env, deftemplate, name) -> int\n\
tell whether or not a slot of given deftemplate has a default value\n\
returns: one of NO_DEFAULT, STATIC_DEFAULT or DYNAMIC_DEFAULT\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotDefaultP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    rv = EnvDeftemplateSlotDefaultP(env, clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotDefaultValue */
static char e_deftemplateSlotDefaultValue__doc__[] = "\
env_deftemplateSlotDefaultValue(env, deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve default value(s) for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotDefaultValue(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotDefaultValue(env, clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotExistP */
static char e_deftemplateSlotExistP__doc__[] = "\
env_deftemplateSlotExistP(env, deftemplate, name) -> bool\n\
tell whether or not the given deftemplate has the specified slot\n\
returns: True if the slot is present, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotExistP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    rv = EnvDeftemplateSlotExistP(env, clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotMultiP */
static char e_deftemplateSlotMultiP__doc__[] = "\
env_deftemplateSlotMultiP(env, deftemplate, name) -> bool\n\
tell whether or not the specified slot of given deftemplate is multifield\n\
returns: True if it is multifield, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotMultiP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    rv = EnvDeftemplateSlotMultiP(env, clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotNames */
static char e_deftemplateSlotNames__doc__[] = "\
env_deftemplateSlotNames(env, deftemplate) -> (MULTIFIELD, list)\n\
retrieve the names of slots in given deftemplate (special case if implied)\n\
returns: MULTIFIELD and a list of pairs or a pair (SYMBOL, 'implied')\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect";
static PyObject *e_deftemplateSlotNames(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotNames(env, clips_deftemplate_value(p), &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotRange */
static char e_deftemplateSlotRange__doc__[] = "\
env_deftemplateSlotRange(env, deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve the numeric range information for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotRange(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotRange(env, clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotSingleP */
static char e_deftemplateSlotSingleP__doc__[] = "\
env_deftemplateSlotSingleP(env, deftemplate, name) -> bool\n\
tell whether or not the specified slot of given deftemplate is single field\n\
returns: True if it is single field, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotSingleP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    int rv = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    rv = EnvDeftemplateSlotSingleP(env, clips_deftemplate_value(p), name);
    RELEASE_MEMORY_ERROR();
    RETURN_BOOL(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* deftemplateSlotTypes */
static char e_deftemplateSlotTypes__doc__[] = "\
env_deftemplateSlotTypes(env, deftemplate, name) -> (MULTIFIELD, list)\n\
retrieve names of the data types allowed for a slot of given deftemplate\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_deftemplateSlotTypes(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDeftemplateSlotTypes(env, clips_deftemplate_value(p), name, &o);
    rv = i_do2py(&o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(env_deftemplateSlotAllowedValues,
                    e_deftemplateSlotAllowedValues)
UNIMPLEMENT_VERSION(env_deftemplateSlotCardinality,
                    e_deftemplateSlotCardinality)
UNIMPLEMENT_VERSION(env_deftemplateSlotDefaultP,
                    e_deftemplateSlotDefaultP)
UNIMPLEMENT_VERSION(env_deftemplateSlotDefaultValue,
                    e_deftemplateSlotDefaultValue)
UNIMPLEMENT_VERSION(env_deftemplateSlotExistP,
                    e_deftemplateSlotExistP)
UNIMPLEMENT_VERSION(env_deftemplateSlotMultiP,
                    e_deftemplateSlotMultiP)
UNIMPLEMENT_VERSION(env_deftemplateSlotNames,
                    e_deftemplateSlotNames)
UNIMPLEMENT_VERSION(env_deftemplateSlotRange,
                    e_deftemplateSlotRange)
UNIMPLEMENT_VERSION(env_deftemplateSlotSingleP,
                    e_deftemplateSlotSingleP)
UNIMPLEMENT_VERSION(env_deftemplateSlotTypes,
                    e_deftemplateSlotTypes)
#endif /* CLIPS_MINOR > 23 */

/* env_findDeftemplate */
static char e_findDeftemplate__doc__[] = "\
env_findDeftemplate(env, name) -> deftemplate\n\
retrieve deftemplate object corresponding to the specified name\n\
returns: the deftemplate as a new object\n\
arguments:\n\
  name (str) - the name of the deftemplate to look for";
static PyObject *e_findDeftemplate(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDeftemplate(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deftemplate_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeftemplateList */
static char e_getDeftemplateList__doc__[] = "\
env_getDeftemplateList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve list of deftemplate names in specified defmodule\n\
returns: value MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDeftemplateList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDeftemplateList(env,
        &o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeftemplateName */
static char e_getDeftemplateName__doc__[] = "\
env_getDeftemplateName(env, deftemplate) -> str\n\
retrieve the name of given deftemplate object\n\
returns: a string containing the name\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *e_getDeftemplateName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDeftemplateName(env, clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* env_getDeftemplatePPForm */
static char e_getDeftemplatePPForm__doc__[] = "\
env_getDeftemplatePPForm(env, deftemplate) -> str\n\
retrieve the pretty-print form of given deftemplate object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *e_getDeftemplatePPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDeftemplatePPForm(env, clips_deftemplate_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* env_getDeftemplateWatch */
static char e_getDeftemplateWatch__doc__[] = "\
env_getDeftemplateWatch(env, deftemplate) -> bool\n\
tell if deftemplate is being watched\n\
returns: True if the deftemplate is being watched, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *e_getDeftemplateWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    i = EnvGetDeftemplateWatch(env, clips_deftemplate_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* env_getNextDeftemplate */
static char e_getNextDeftemplate__doc__[] = "\
env_getNextDeftemplate(env [, deftemplate]) -> deftemplate\n\
find next deftemplate in the list, first if argument is omitted\n\
returns: next deftemplate object, None if already at last deftemplate\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to start from";
static PyObject *e_getNextDeftemplate(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFTEMPLATE(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDeftemplate(env, p ? clips_deftemplate_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_deftemplate_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDeftemplateDeletable */
static char e_isDeftemplateDeletable__doc__[] = "\
env_isDeftemplateDeletable(env, deftemplate) -> bool\n\
tell whether or not given deftemplate object can be deleted\n\
returns: True when deftemplate can be deleted, False otherwise\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *e_isDeftemplateDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    i = EnvIsDeftemplateDeletable(env, clips_deftemplate_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* env_listDeftemplates */
static char e_listDeftemplates__doc__[] = "\
env_listDeftemplates(env, logicalname [, module])\n\
list deftemplates to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDeftemplates(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDeftemplates(env, lname,
        module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDeftemplateWatch */
static char e_setDeftemplateWatch__doc__[] = "\
env_setDeftemplateWatch(env, state, deftemplate)\n\
set the specified deftemplate to a new watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  deftemplate (deftemplate) - the deftemplate object";
static PyObject *e_setDeftemplateWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *state = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DeftemplType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, p);
    EnvSetDeftemplateWatch(env,
        PyObject_IsTrue(state), clips_deftemplate_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP()
END_FAIL
}

/* env_undeftemplate */
static char e_undeftemplate__doc__[] = "\
env_undeftemplate(env [, deftemplate])\n\
remove a deftemplate or all deftemplates from the system\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate to remove, all if omitted";
static PyObject *e_undeftemplate(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFTEMPLATE(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndeftemplate(env, p ? clips_deftemplate_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP()
END_FAIL
}


/* 4.4 [E] - Fact Functions */

/* env_assertFact */
static char e_assertFact__doc__[] = "\
env_assertFact(env, fact) -> fact\n\
add a fact to the fact list\n\
returns: the asserted fact as a new object\n\
arguments:\n\
  fact (fact) - the fact to assert";
static PyObject *e_assertFact(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    /* if fact is read-only, it was previously asserted and can't be reused */
    if(clips_fact_readonly(p)) {
        ERROR_CLIPS_REASSERT();
        FAIL();
    }
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvAssert(env, clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    if(!ptr) {
        ERROR_CLIPS_ASSERT();
        FAIL();
    }
    /* now the old fact cannot be modified anymore, even on future failure */
    ENV_REMOVE_JUSTASSERTED_FACT(pyenv);
    ENV_REMOVE_HASH_FACT(pyenv, p);
    clips_fact_readonly(p) = TRUE;
    clips_fact_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    ENV_CHECK_VALID_FACT(env, q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_assertString */
static char e_assertString__doc__[] = "\
env_assertString(env, expr) -> fact\n\
add a fact to the fact list using a string\n\
returns: the asserted fact as a new object\n\
arguments:\n\
  expr (str) - string containing a list of primitive datatypes";
static PyObject *e_assertString(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *expr = NULL;
    clips_FactObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &expr))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvAssertString(env, expr);
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    if(!ptr) {
        ERROR_CLIPS_ASSERT();
        FAIL();
    }
    clips_fact_New(env, p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(p) = TRUE;
    clips_fact_assign(p, ptr);
    clips_fact_lock(p);
    ENV_CHECK_VALID_FACT(env, p);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_assignFactSlotDefaults */
static char e_assignFactSlotDefaults__doc__[] = "\
env_assignFactSlotDefaults(env, fact)\n\
assign default values to the slots of a fact\n\
arguments:\n\
  fact (fact) - the fact whose slots are to reset to default values";
static PyObject *e_assignFactSlotDefaults(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    if(clips_fact_readonly(p)) {
        ERROR_CLIPS_READONLY();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!EnvAssignFactSlotDefaults(env, clips_fact_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_OTHER("could not assign default values to fact");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_createFact */
static char e_createFact__doc__[] = "\
env_createFact(env, deftemplate) -> fact\n\
create a new fact object using the provided deftemplate\n\
returns: a new fact object\n\
arguments:\n\
  deftemplate (deftemplate) - the deftemplate defining the fact type";
static PyObject *e_createFact(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    clips_DeftemplObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeftemplType, &q))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvCreateFact(env, clips_deftemplate_value(q));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_fact_New(env, p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_assign(p, ptr);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_ADD_NONASSERTED_FACT(pyenv);
    ENV_APPEND_HASH_FACT(pyenv, p);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_decrementFactCount */
UNIMPLEMENT(env_decrementFactCount, e_decrementFactCount)

/* env_factIndex */
static char e_factIndex__doc__[] = "\
env_factIndex(env, fact) -> int\n\
retrieve the index of specified fact in fact list\n\
arguments:\n\
  fact (fact) - the fact to look for";
static PyObject *e_factIndex(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    long int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();     /* needed? */
    i = EnvFactIndex(env, clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    if(p) DELETE(p);
    SKIP();
END_FAIL
}

/* env_facts */
static char e_facts__doc__[] = "\
env_facts(env, logicalname [, module [, start [, end [, max]]]])\n\
list facts to the output stream identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule, None for all modules\n\
  start (int) - first fact, -1 for no restriction\n\
  end (int) - last fact, -1 for no restriction\n\
  max (int) - maximum number of facts, -1 for no restriction";
static PyObject *e_facts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *module = NULL;
    char *lname = NULL;
    int start = -1, end = -1, max = -1;

    if(!PyArg_ParseTuple(args, "O!s|O!iii",
                         &clips_EnvType, &pyenv, &lname,
                         &clips_DefmoduleType, &module,
                         &start, &end, &max))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvFacts(
        env, lname,
        module && module != Py_None ? clips_defmodule_value(module) : NULL,
        start, end, max);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* getFactDuplication */
E_STATUS_FUNC_GET_BOOL(env_getFactDuplication,
                       e_getFactDuplication,
                       EnvGetFactDuplication)

/* getFactListChanged */
E_STATUS_FUNC_GET_BOOL(env_getFactListChanged,
                       e_getFactListChanged,
                       EnvGetFactListChanged)


/* env_getFactPPForm */
static char e_getFactPPForm__doc__[] = "\
env_getFactPPForm(env, fact) -> str\n\
retrieve the pretty-print form of given fact\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  fact (fact) - the fact object to inspect";
static PyObject *e_getFactPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;
    clips_FactObject *p = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvGetFactPPForm(env, buffer, ppbuffer_size-1, clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* env_getFactSlot */
static char e_getFactSlot__doc__[] = "\
env_getFactSlot(env, fact [, slotname]) -> (type, value)\n\
get the slot value for the specified fact given the slot name\n\
returns: a value or a multifield in the form (type, return value)\n\
arguments:\n\
  fact (fact) - the fact to inspect\n\
  slotname (str) - the name of the slot to retrieve, should be omitted\n\
                   for the implied multifield slot of an implied\n\
                   deftemplate";
static PyObject *e_getFactSlot(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    char *slotname = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!|s",
                         &clips_EnvType, &pyenv,
                         &clips_FactType, &p, &slotname))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    /* we make some considerations about this call, to avoid nasty errors */
    /* check that the slot name can be really omitted (see docstring) */
    if(!slotname && !
       ((struct deftemplate *)
           ((struct fact *)clips_fact_value(p))->whichDeftemplate
       )->implied) {
        ERROR_VALUE("cannot omit slot name using this fact");
        FAIL();
    }
    /* end of considerations */
    ACQUIRE_MEMORY_ERROR();
    if(!EnvGetFactSlot(env, clips_fact_value(p), slotname, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    rv = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    Py_XDECREF(rv);
END_FAIL
}

/* env_getNextFact */
static char e_getNextFact__doc__[] = "\
env_getNextFact(env [, fact]) -> fact\n\
retrieve next fact object, first if argument is omitted\n\
returns: a fact object, None if already at last fact\n\
arguments:\n\
  fact (fact) - the fact to start from";
static PyObject *e_getNextFact(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ENV_CHECK_VALID_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextFact(env, p ? clips_fact_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_fact_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    ENV_CHECK_VALID_FACT(env, q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* 4.4.12a [E] - getNextFactInTemplate */
static char e_getNextFactInTemplate__doc__[] = "\
env_getNextFactInTemplate(env, deftemplate [, fact]) -> fact\n\
retrieve next fact object for a deftemplate, first if fact is omitted\n\
returns: a fact object, None if already at last fact\n\
arguments:\n\
  deftemplate (deftemplate) - the template to find facts of\n\
  fact (fact) - the fact to start from";
static PyObject *e_getNextFactInTemplate(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL, *q = NULL;
    clips_DeftemplObject *t = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DeftemplType, &t, &clips_FactType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFTEMPLATE(env, t);
    if(p)
        ENV_CHECK_VALID_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextFactInTemplate(env,
        clips_deftemplate_value(t), p ? clips_fact_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_fact_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_fact_readonly(q) = TRUE;
    clips_fact_assign(q, ptr);
    clips_fact_lock(q);
    ENV_CHECK_VALID_FACT(env, q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_incrementFactCount */
UNIMPLEMENT(env_incrementFactCount, e_incrementFactCount)

/* env_loadFacts */
static char e_loadFacts__doc__[] = "\
env_loadFacts(env, filename)\n\
load facts from specified file\n\
arguments:\n\
  filename (str) - the name of file to load facts from";
static PyObject *e_loadFacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvLoadFacts(env, fn)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

#if CLIPS_MINOR > 23

/* ppFact */
static char e_ppFact__doc__[] = "\
env_ppFact(env, fact, output [, ignoredefault])\n\
write the pretty-print form of given fact to logical output\n\
arguments:\n\
  fact (fact) - the fact to write\n\
  output (str) - logical name of stream to output to\n\
  ignoredefault (bool) - True to skip slots whose values equal defaults";
static PyObject *e_ppFact(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    char *s = NULL;
    PyObject *o = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s|O",
                         &clips_EnvType, &pyenv, &clips_FactType, &p, &s, &o))
        FAIL();
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvPPFact(env, clips_fact_value(p), s, o ? PyObject_IsTrue(o) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(env_ppFact, e_ppFact)
#endif /* CLIPS_MINOR > 23 */

/* env_putFactSlot */
static char e_putFactSlot__doc__[] = "\
env_putFactSlot(env, fact, name, value)\n\
changes the value of specified slot in given fact\n\
arguments:\n\
  fact (fact) - the fact to change: must have been created with createFact\n\
  name (str) - the name of the slot to change\n\
  value (pair) - a pair (type, value) containing the value to assign";
static PyObject *e_putFactSlot(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    DATA_OBJECT o = { 0 };
    clips_FactObject *f = NULL;
    PyObject *p = NULL;
    char *s;

    /* note that function i_py2do checks for last argument validity */
    if(!PyArg_ParseTuple(args, "O!O!sO",
                         &clips_EnvType, &pyenv,
                         &clips_FactType, &f, &s, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, f);
    ENV_CHECK_LOST_FACT(env, f);
    if(clips_fact_readonly(f)) {
        ERROR_CLIPS_READONLY();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!i_py2do_e(env, p, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    if(!EnvPutFactSlot(env, clips_fact_value(f), s, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_OTHER("fact slot could not be modified");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_retract */
static char e_retract__doc__[] = "\
env_retract(env, fact)\n\
retract provided fact\n\
arguments:\n\
  fact (fact) - the fact to retract";
static PyObject *e_retract(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    clips_fact_lock(p);
    /* if the fact was actually asserted it must now be read-only */
    if(!clips_fact_readonly(p)) {
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!EnvRetract(env, clips_fact_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_saveFacts */
static char e_saveFacts__doc__[] = "\
env_saveFacts(env, filename, scope)\n\
save the facts in the specified scope\n\
arguments:\n\
  filename (str) - the name of the file to save to\n\
  scope (int) - can be one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *e_saveFacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    int scope = 0;
    char *fn = NULL;

    if(!PyArg_ParseTuple(args, "O!si",
                         &clips_EnvType, &pyenv, &fn, &scope))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(scope != LOCAL_SAVE && scope != VISIBLE_SAVE) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    if(!EnvSaveFacts(env, fn, scope, NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_IO();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* setFactDuplication */
E_STATUS_FUNC_SET_BOOL(env_setFactDuplication,
                       e_setFactDuplication,
                       EnvSetFactDuplication)

/* setFactListChanged */
E_STATUS_FUNC_SET_BOOL(env_setFactListChanged,
                       e_setFactListChanged,
                       EnvSetFactListChanged)

/* env_factDeftemplate */
static char e_factDeftemplate__doc__[] = "\
env_factDeftemplate(env, fact) -> deftemplate\n\
return the deftemplate associated with a particular fact\n\
returns: a deftemplate object, None if no deftemplate associated\n\
arguments:\n\
  fact (fact) - the fact to examine";
static PyObject *e_factDeftemplate(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    clips_DeftemplObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFactDeftemplate(env, clips_fact_value(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr || !env_deftemplateExists(env, ptr))
        RETURN_NONE();
    clips_deftemplate_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deftemplate_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_factExistp */
static char e_factExistp__doc__[] = "\
env_factExistp(env, fact) -> bool\n\
tell whether a fact is in the list or has been retracted\n\
returns: True if the fact exixts, False if it was retracted\n\
arguments:\n\
  fact (fact) - the fact to check";
static PyObject *e_factExistp(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    clips_FactObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    i = FactExistp(clips_fact_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_factSlotNames */
static char e_factSlotNames__doc__[] = "\
env_factSlotNames(env, fact) -> (MULTIFIELD, list)\n\
get the slot names for the specified fact\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  fact (fact) - the fact to inspect";
static PyObject *e_factSlotNames(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_FactObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_FactType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_FACT(env, p);
    ENV_CHECK_LOST_FACT(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvFactSlotNames(env, clips_fact_value(p), &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    return q;

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getFactList */
static char e_getFactList__doc__[] = "\
env_getFactList(env [, module]) -> (type, list)\n\
retrieve list of fact identifiers in specified defmodule\n\
returns:  MULTIFIELD and a list of pairs (STRING, identifier)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getFactList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetFactList(env, &o, module ? clips_defmodule_value(module) : NULL);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    return q;

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_loadFactsFromString */
static char e_loadFactsFromString__doc__[] = "\
env_loadFactsFromString(env, string)\n\
load facts from specified string into the fact list\n\
arguments:\n\
  string (str) - string to load facts from";
static PyObject *e_loadFactsFromString(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &s)) FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvLoadFactsFromString(env, s, -1)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_PARSEX();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.5 [E] - Deffacts Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_deffactsExists(void *env, void *ptr) {
    void *rv = EnvGetNextDeffacts(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDeffacts(env, rv);
    }
    return NULL;
}
#define EPYDEFFACTS_EXISTS(_e, _p) \
    env_deffactsExists(_e, clips_deffacts_value(_p))
#define ECHECK_DEFFACTS(_e, _p) do { \
        if(!EPYDEFFACTS_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFFACTS(_e, _p) do { \
        if(_p && !EPYDEFFACTS_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_deffactsModule */
static char e_deffactsModule__doc__[] = "\
env_deffactsModule(env, deffacts) -> str\n\
retrieve the module where the specified deffacts object is defined\n\
returns: the module name\n\
arguments:\n\
  deffacts (deffacts) - the deffacts object to inspect";
static PyObject *e_deffactsModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFACTS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDeffactsModule(env, clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDeffacts */
static char e_findDeffacts__doc__[] = "\
env_findDeffacts(env, name) -> deffacts\n\
find the deffacts object whose name is specified\n\
returns: the deffacts as a new object\n\
arguments:\n\
  name (str) - the name of the deffacts to look for";
static PyObject *e_findDeffacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DeffactsObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDeffacts(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deffacts_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffacts_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeffactsList */
static char e_getDeffactsList__doc__[] = "\
env_getDeffactsList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of deffacts in specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDeffactsList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDeffactsList(env, &o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeffactsName */
static char e_getDeffactsName__doc__[] = "\
env_getDeffactsName(env, deffacts) -> str\n\
retrieve the name of deffacts object\n\
returns: the deffacts name\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to inspect";
static PyObject *e_getDeffactsName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFACTS(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDeffactsName(env, clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDeffactsPPForm */
static char e_getDeffactsPPForm__doc__[] = "\
env_getDeffactsPPForm(env, deffacts) -> str\n\
retrieve the pretty-print form of deffacts object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to inspect";
static PyObject *e_getDeffactsPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFACTS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDeffactsPPForm(env, clips_deffacts_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDeffacts */
static char e_getNextDeffacts__doc__[] = "\
env_getNextDeffacts(env [, deffacts]) -> deffacts\n\
retrieve next deffacts object in list, first if argument is omitted\n\
returns: a deffacts object, None if already at last deffacts\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to start from";
static PyObject *e_getNextDeffacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFFACTS(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDeffacts(env, p ? clips_deffacts_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_deffacts_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffacts_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDeffactsDeletable */
static char e_isDeffactsDeletable__doc__[] = "\
env_isDeffactsDeletable(env, deffacts) -> bool\n\
tell whether or not given deffacts object can be deleted\n\
returns: True when deffacts can be deleted, False otherwise\n\
arguments:\n\
  deffacts (deffacts) - the deffacts object";
static PyObject *e_isDeffactsDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFACTS(env, p);
    i = EnvIsDeffactsDeletable(env, clips_deffacts_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDeffacts */
static char e_listDeffacts__doc__[] = "\
env_listDeffacts(env, output [, module])\n\
list deffacts objects in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDeffacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDeffacts(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undeffacts */
static char e_undeffacts__doc__[] = "\
env_undeffacts(env [, deffacts])\n\
delete a deffacts object or all deffacts objects\n\
arguments:\n\
  deffacts (deffacts) - the deffacts to be deleted, all if omitted";
static PyObject *e_undeffacts(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffactsObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffactsType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFFACTS(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndeffacts(env, p ? clips_deffacts_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.6 [E] - Defrule Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_defruleExists(void *env, void *ptr) {
    void *rv = EnvGetNextDefrule(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDefrule(env, rv);
    }
    return NULL;
}
#define EPYDEFRULE_EXISTS(_e, _p) \
    env_defruleExists(_e, clips_defrule_value(_p))
#define ECHECK_DEFRULE(_e, _p) do { \
        if(!EPYDEFRULE_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFRULE(_e, _p) do { \
        if(_p && !EPYDEFRULE_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_defruleHasBreakpoint */
static char e_defruleHasBreakpoint__doc__[] = "\
env_defruleHasBreakpoint(env, defrule) -> bool\n\
test whether or not the given defrule has a breakpoint\n\
returns: True if the defrule has a breakpoint, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_defruleHasBreakpoint(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    i = EnvDefruleHasBreakpoint(env, clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_defruleModule */
static char e_defruleModule__doc__[] = "\
env_defruleModule(env, defrule) -> str\n\
retrieve the module where the defrule is defined\n\
returns: the requested name of module\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_defruleModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvDefruleModule(env, clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefrule */
static char e_findDefrule__doc__[] = "\
env_findDefrule(env, name) -> defrule\n\
find the defrule with the specified name\n\
returns: the defrule as a new object\n\
arguments:\n\
  name (str) - the name of defrule to look for";
static PyObject *e_findDefrule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    clips_DefruleObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefrule(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defrule_New(p);
    clips_defrule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefruleList */
static char e_getDefruleList__doc__[] = "\
env_getDefruleList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of defrule objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDefruleList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefruleList(env, &o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefruleName */
static char e_getDefruleName__doc__[] = "\
env_getDefruleName(env, defrule) -> str\n\
retrieve the name of specified defrule\n\
returns: a string containing the defrule name\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_getDefruleName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefruleName(env, clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefrulePPForm */
static char e_getDefrulePPForm__doc__[] = "\
env_getDefrulePPForm(env, defrule) -> str\n\
retrieve the pretty-print form of specified defrule\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_getDefrulePPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    s = EnvGetDefrulePPForm(env, clips_defrule_value(p));
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefruleWatchActivations */
static char e_getDefruleWatchActivations__doc__[] = "\
env_getDefruleWatchActivations(env, defrule) -> bool\n\
tell whether the specified defrule is watched for activations\n\
returns: True if activations are watched, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_getDefruleWatchActivations(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    i = EnvGetDefruleWatchActivations(env, clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefruleWatchFirings */
static char e_getDefruleWatchFirings__doc__[] = "\
env_getDefruleWatchFirings(env, defrule) -> bool\n\
tell whether the specified defrule is watched for firings\n\
returns: True if rule firings are watched, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_getDefruleWatchFirings(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    i = EnvGetDefruleWatchFirings(env, clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* getIncrementalReset */
E_STATUS_FUNC_GET_BOOL(env_getIncrementalReset,
                       e_getIncrementalReset,
                       EnvGetIncrementalReset)


/* env_getNextDefrule */
static char e_getNextDefrule__doc__[] = "\
env_getNextDefrule(env [, defrule]) -> defrule\n\
retrieve next defrule object in list, first if argument is omitted\n\
returns: a defrule object, None if already at last defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to start from";
static PyObject *e_getNextDefrule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefrule(env, p ? clips_defrule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_defrule_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defrule_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDefruleDeletable */
static char e_isDefruleDeletable__doc__[] = "\
env_isDefruleDeletable(env, defrule) -> bool\n\
tell whether or not given defrule object can be deleted\n\
returns: True when defrule can be deleted, False otherwise\n\
arguments:\n\
  defrule (defrule) - the defrule object";
static PyObject *e_isDefruleDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    i = EnvIsDefruleDeletable(env, clips_defrule_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefrules */
static char e_listDefrules__doc__[] = "\
env_listDefrules(env, output [, module])\n\
list defrule objects in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDefrules(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv, &s, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefrules(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_matches */
static char e_matches__doc__[] = "\
env_matches(env, output, defrule)\n\
list defrule partial matches\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  defrule (defrule) - the defrule to inspect";
static PyObject *e_matches(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!sO!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvMatches_PY(env, s, clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_refresh */
static char e_refresh__doc__[] = "\
env_refresh(env, defrule)\n\
refresh a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to refresh";
static PyObject *e_refresh(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvRefresh(env, clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_removeBreak */
static char e_removeBreak__doc__[] = "\
env_removeBreak(env, defrule)\n\
remove the breakpoint from a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to access";
static PyObject *e_removeBreak(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvRemoveBreak(env, clips_defrule_value(p))) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setBreak */
static char e_setBreak__doc__[] = "\
env_setBreak(env, defrule)\n\
set a breakpoint to a defrule\n\
arguments:\n\
  defrule (defrule) - the defrule to access";
static PyObject *e_setBreak(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetBreak(env, clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefruleWatchActivations */
static char e_setDefruleWatchActivations__doc__[] = "\
env_setDefruleWatchActivations(env, state, defrule)\n\
set activations watch of a defrule to a new state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defrule (defrule) - the defrule to access";
static PyObject *e_setDefruleWatchActivations(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    EnvSetDefruleWatchActivations(env,
        PyObject_IsTrue(state), clips_defrule_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefruleWatchFirings */
static char e_setDefruleWatchFirings__doc__[] = "\
env_setDefruleWatchFirings(env, state, defrule)\n\
set firings watch of a defrule to a new state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defrule (defrule) - the defrule to access";
static PyObject *e_setDefruleWatchFirings(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFRULE(env, p);
    EnvSetDefruleWatchFirings(env,
        PyObject_IsTrue(state), clips_defrule_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setIncrementalReset */
E_STATUS_FUNC_SET_BOOL(env_setIncrementalReset,
                       e_setIncrementalReset,
                       EnvSetIncrementalReset)

/* env_showBreaks */
static char e_showBreaks__doc__[] = "\
env_showBreaks(env, output [, module])\n\
list breakpoints in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_showBreaks(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvShowBreaks(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefrule */
static char e_undefrule__doc__[] = "\
env_undefrule(env [, defrule])\n\
delete a defrule object or all defrule objects\n\
arguments:\n\
  defrule (defrule) - the defrule to be deleted, all if omitted";
static PyObject *e_undefrule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefruleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefruleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p && !env_defruleExists(env, clips_defrule_value(p))) {
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefrule(env, p ? clips_defrule_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.7 [E] - Agenda functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_activationExists(void *env, void *ptr) {
    void *rv = EnvGetNextActivation(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextActivation(env, rv);
    }
    return NULL;
}
#define EPYACTIVATION_EXISTS(_e, _p) \
    env_activationExists(_e, clips_activation_value(_p))
#define ECHECK_ACTIVATION(_e, _p) do { \
        if(!EPYACTIVATION_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_ACTIVATION(_e, _p) do { \
        if(_p && !EPYACTIVATION_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* addRunFunction */
UNIMPLEMENT(env_addRunFunction, e_addRunFunction)

/* env_agenda */
static char e_agenda__doc__[] = "\
env_agenda(env, output [, module])\n\
list agenda rules in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_agenda(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvAgenda(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* clearFocusStack */
E_FUNC_VOID_ONLY(env_clearFocusStack, e_clearFocusStack, EnvClearFocusStack)

/* env_deleteActivation */
static char e_deleteActivation__doc__[] = "\
env_deleteActivation(env, activation)\n\
remove activation from agenda\n\
arguments:\n\
  activation (activation) - the activation to delete";
static PyObject *e_deleteActivation(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_ActivationType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvDeleteActivation(env, p ? clips_activation_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_focus */
static char e_focus__doc__[] = "\
env_focus(env, module)\n\
set current focus\n\
arguments:\n\
  module (defmodule) - the defmodule to set focus to";
static PyObject *e_focus(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvFocus(env, clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getActivationName */
static char e_getActivationName__doc__[] = "\
env_getActivationName(env, activation) -> str\n\
retrieve the name of specified activation\n\
returns: name as a string\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *e_getActivationName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_ActivationType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetActivationName(env, clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getActivationPPForm */
static char e_getActivationPPForm__doc__[] = "\
env_getActivationPPForm(env, activation) -> str\n\
retrieve the pretty-print form of specified activation\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *e_getActivationPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_ActivationType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvGetActivationPPForm(env, buffer,
        ppbuffer_size-1, clips_activation_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* env_getActivationSalience */
static char e_getActivationSalience__doc__[] = "\
env_getActivationSalience(env, activation) -> int\n\
retrieve the salience value of specified activation\n\
returns: salience as integer\n\
arguments:\n\
  activation (activation) - the activation to inspect";
static PyObject *e_getActivationSalience(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_ActivationType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    i = EnvGetActivationSalience(env, clips_defrule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getAgendaChanged */
E_STATUS_FUNC_GET_BOOL(env_getAgendaChanged,
                       e_getAgendaChanged,
                       EnvGetAgendaChanged)

/* env_getFocus */
static char e_getFocus__doc__[] = "\
env_getFocus(env) -> defmodule\n\
retrieve the module with associated focus\n\
returns: the defmodule with focus as a new object";
static PyObject *e_getFocus(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetFocus(env);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getFocusStack */
static char e_getFocusStack__doc__[] = "\
env_getFocusStack(env) -> (MULTIFIELD, list)\n\
retrieve the module names in the focus stack\n\
returns: MULTIFIELD and a list of pairs (STRING, name)";
static PyObject *e_getFocusStack(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetFocusStack(env, &o);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getNextActivation */
static char e_getNextActivation__doc__[] = "\
env_getNextActivation(env [, activation]) -> activation\n\
retrieve next activation object in list, first if argument is omitted\n\
returns: an activation object, None if already at last activation\n\
arguments:\n\
  activation (activation) - the activation to start from";
static PyObject *e_getNextActivation(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_ActivationType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextActivation(env, p ? clips_activation_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr) RETURN_NONE();
    clips_activation_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_activation_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getSalienceEvaluation */
E_STATUS_FUNC_GET(env_getSalienceEvaluation,
                  e_getSalienceEvaluation,
                  EnvGetSalienceEvaluation,
                  "i")

/* getStrategy */
E_FUNC_GET_ONLY(env_getStrategy, e_getStrategy, EnvGetStrategy, "i")

/* env_listFocusStack */
static char e_listFocusStack__doc__[] = "\
env_listFocusStack(env, output)\n\
print current focus stack\n\
arguments:\n\
  output (str) - logical name of output stream";
static PyObject *e_listFocusStack(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *output = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &output))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListFocusStack(env, output);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* popFocus */
E_FUNC_VOID_ONLY(env_popFocus, e_popFocus, EnvPopFocus)

/* env_refreshAgenda */
static char e_refreshAgenda__doc__[] = "\
env_refreshAgenda(env [, module])\n\
refresh agenda for specified defmodule\n\
arguments:\n\
  module (defmodule) - the defmodule to process, all if omitted";
static PyObject *e_refreshAgenda(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvRefreshAgenda(env, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* removeRunFunction */
UNIMPLEMENT(env_removeRunFunction, e_removeRunFunction)

/* env_reorderAgenda */
static char e_reorderAgenda__doc__[] = "\
env_reorderAgenda(env [, module])\n\
reorder agenda for specified defmodule\n\
arguments:\n\
  module (defmodule) - the defmodule to process, all if omitted";
static PyObject *e_reorderAgenda(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvReorderAgenda(env, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_run */
static char e_run__doc__[] = "\
env_run(env [, limit]) -> fired\n\
execute rules\n\
returns: the number of fired rules\n\
arguments:\n\
  limit (int) - number of rules to fire, all if omitted";
static PyObject *e_run(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    long runlimit = -1;

    if(!PyArg_ParseTuple(args, "O!|i", &clips_EnvType, &pyenv, &runlimit))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    runlimit = EnvRun(env, runlimit);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(runlimit);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setActivationSalience */
static char e_setActivationSalience__doc__[] = "\
env_setActivationSalience(env, activation, salience) -> int\n\
set the new activation salience\n\
returns: the old value\n\
arguments:\n\
  activation (activation) - an activation object\n\
  salience (int) - the new activation salience";
static PyObject *e_setActivationSalience(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_ActivationObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_ActivationType, &p, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_ACTIVATION(env, p);
    ACQUIRE_MEMORY_ERROR();
    i = EnvSetActivationSalience(env, clips_activation_value(p), i);
    RELEASE_MEMORY_ERROR();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setAgendaChanged */
E_STATUS_FUNC_SET_BOOL(env_setAgendaChanged,
                       e_setAgendaChanged,
                       EnvSetAgendaChanged)

/* env_setSalienceEvaluation */
static char e_setSalienceEvaluation__doc__[] = "\
env_setSalienceEvaluation(env, mode)\n\
set the new salience evaluation mode\n\
arguments:\n\
  mode (int) - the new evaluation mode";
static PyObject *e_setSalienceEvaluation(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i", &clips_EnvType, &pyenv, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != WHEN_DEFINED && i != WHEN_ACTIVATED && i != EVERY_CYCLE) {
        ERROR_VALUE("invalid evaluation mode");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvSetSalienceEvaluation(env, i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setStrategy */
static char e_setStrategy__doc__[] = "\
env_setStrategy(env, mode)\n\
set the new strategy\n\
arguments:\n\
  strategy (int) - the new strategy";
static PyObject *e_setStrategy(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!i", &clips_EnvType, &pyenv, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != DEPTH_STRATEGY && i != BREADTH_STRATEGY && i != LEX_STRATEGY &&
       i != MEA_STRATEGY && i != COMPLEXITY_STRATEGY &&
       i != SIMPLICITY_STRATEGY && i != RANDOM_STRATEGY) {
        ERROR_VALUE("invalid strategy");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvSetStrategy(env, i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.8 [E] - Defglobal Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_defglobalExists(void *env, void *ptr) {
    void *rv = EnvGetNextDefglobal(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDefglobal(env, rv);
    }
    return NULL;
}
#define EPYDEFGLOBAL_EXISTS(_e, _p) \
    env_defglobalExists(_e, clips_defglobal_value(_p))
#define ECHECK_DEFGLOBAL(_e, _p) do { \
        if(!EPYDEFGLOBAL_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFGLOBAL(_e, _p) do { \
        if(_p && !EPYDEFGLOBAL_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_defglobalModule */
static char e_defglobalModule__doc__[] = "\
env_defglobalModule(env, defglobal) -> str\n\
retrieve the module name where specified defglobal is defined\n\
returns: module name as string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *e_defglobalModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDefglobalModule(env, clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefglobal */
static char e_findDefglobal__doc__[] = "\
env_findDefglobal(env, name) -> defglobal\n\
return the defglobal object associated with name\n\
returns: the defglobal as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *e_findDefglobal(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefglobal(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defglobal_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defglobal_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefglobalList */
static char e_getDefglobalList__doc__[] = "\
env_getDefglobalList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of defglobal objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDefglobalList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefglobalList(env, &o,
        module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefglobalName */
static char e_getDefglobalName__doc__[] = "\
env_getDefglobalName(env, defglobal) -> str\n\
retrieve the name of specified defglobal\n\
returns: name as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *e_getDefglobalName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefglobalName(env, clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefglobalPPForm */
static char e_getDefglobalPPForm__doc__[] = "\
env_getDefglobalPPForm(env, defglobal) -> str\n\
retrieve the pretty-print form of specified defglobal\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *e_getDefglobalPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefglobalPPForm(env, clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefglobalValue */
static char e_getDefglobalValue__doc__[] = "\
env_getDefglobalValue(env, name) -> (type, value)\n\
retrieve the value of specified defglobal\n\
returns: value as a pair (type, value)\n\
arguments:\n\
  name (str) - the name of the defglobal";
static PyObject *e_getDefglobalValue(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvGetDefglobalValue(env, name, &o)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefglobalValueForm */
static char e_getDefglobalValueForm__doc__[] = "\
env_getDefglobalValueForm(env, defglobal) -> str\n\
retrieve the pretty-print form of specified defglobal\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *e_getDefglobalValueForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefglobalValueForm(
        env, buffer, ppbuffer_size-1, clips_defglobal_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* env_getDefglobalWatch */
static char e_getDefglobalWatch__doc__[] = "\
env_getDefglobalWatch(env, defglobal) -> bool\n\
tell whether or not the defglobal is being watched\n\
returns: True if the defglobal is being watched, False otherwise\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to inspect";
static PyObject *e_getDefglobalWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    i = EnvGetDefglobalWatch(env, clips_defglobal_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* getGlobalsChanged */
E_STATUS_FUNC_GET_BOOL(env_getGlobalsChanged,
                       e_getGlobalsChanged,
                       EnvGetGlobalsChanged)

/* env_getNextDefglobal */
static char e_getNextDefglobal__doc__[] = "\
env_getNextDefglobal(env [, defglobal]) -> defglobal\n\
retrieve next defglobal object in list, first if argument is omitted\n\
returns: a defglobal object, None if already at last defglobal\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to start from";
static PyObject *e_getNextDefglobal(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFGLOBAL(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefglobal(env, p ? clips_defglobal_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defglobal_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defglobal_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getResetGlobals */
E_STATUS_FUNC_GET_BOOL(env_getResetGlobals,
                       e_getResetGlobals,
                       EnvGetResetGlobals)

/* env_isDefglobalDeletable */
static char e_isDefglobalDeletable__doc__[] = "\
env_isDefglobalDeletable(env, defglobal) -> bool\n\
tell whether or not the defglobal is deletable\n\
returns: True if the defglobal can be deleted, False otherwise\n\
arguments:\n\
  defglobal (defglobal) - the defglobal to be inspected";
static PyObject *e_isDefglobalDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    i = EnvIsDefglobalDeletable(env, clips_defglobal_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefglobals */
static char e_listDefglobals__doc__[] = "\
env_listDefglobals(env, logicalname [, module])\n\
list defglobals to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDefglobals(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefglobals(env, lname,
        module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefglobalValue */
static char e_setDefglobalValue__doc__[] = "\
env_setDefglobalValue(env, name, value)\n\
set the value of passed in defglobal\n\
arguments:\n\
  name (str) - the name of defglobal to change\n\
  value (pair) - a pair (type, value)";
static PyObject *e_setDefglobalValue(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!sO", &clips_EnvType, &pyenv, &name, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(!i_py2do_e(env, p, &o)) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvSetDefglobalValue(env, name, &o)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    Py_XDECREF(p);
    RETURN_NONE();

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_setDefglobalWatch */
static char e_setDefglobalWatch__doc__[] = "\
env_setDefglobalWatch(env, state, defglobal)\n\
set the specified defglobal to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defglobal (defglobal) - the defglobal object";
static PyObject *e_setDefglobalWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *state = NULL;
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefglobalType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGLOBAL(env, p);
    EnvSetDefglobalWatch(env,
        PyObject_IsTrue(state), clips_defglobal_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setGlobalsChanged */
E_STATUS_FUNC_SET_BOOL(env_setGlobalsChanged,
                       e_setGlobalsChanged,
                       EnvSetGlobalsChanged)

/* setResetGlobals */
E_STATUS_FUNC_SET_BOOL(env_setResetGlobals,
                       e_setResetGlobals,
                       EnvSetResetGlobals)

/* env_showDefglobals */
static char e_showDefglobals__doc__[] = "\
env_showDefglobals(env, output [, module])\n\
list defglobals in specified defmodule to logical output\n\
arguments:\n\
  output (str) - logical name of stream to output to\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_showDefglobals(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvShowDefglobals(env, s, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefglobal */
static char e_undefglobal__doc__[] = "\
env_undefglobal(env [, defglobal])\n\
delete a defglobal object or all defglobal objects\n\
arguments:\n\
  defglobal (defglobal) - object to be deleted, all if omitted";
static PyObject *e_undefglobal(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefglobalObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefglobalType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFGLOBAL(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefglobal(env, p ? clips_defglobal_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.9 [E] - Deffunction functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_deffunctionExists(void *env, void *ptr) {
    void *rv = EnvGetNextDeffunction(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDeffunction(env, rv);
    }
    return NULL;
}
#define EPYDEFFUNCTION_EXISTS(_e, _p) \
    env_deffunctionExists(_e, clips_deffunction_value(_p))
#define ECHECK_DEFFUNCTION(_e, _p) do { \
        if(!EPYDEFFUNCTION_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFFUNCTION(_e, _p) do { \
        if(_p && !EPYDEFFUNCTION_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_deffunctionModule */
static char e_deffunctionModule__doc__[] = "\
env_deffunctionModule(env, deffunction) -> str\n\
retrieve the module name where specified deffunction is defined\n\
returns: module name as string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *e_deffunctionModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDeffunctionModule(env, clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDeffunction */
static char e_findDeffunction__doc__[] = "\
env_findDeffunction(env, name) -> deffunction\n\
return the deffunction object associated with name\n\
returns: the deffunction as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *e_findDeffunction(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDeffunction(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_deffunction_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffunction_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeffunctionList */
static char e_getDeffunctionList__doc__[] = "\
env_getDeffunctionList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of deffunction objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDeffunctionList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDeffunctionList(env, &o,
        module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDeffunctionName */
static char e_getDeffunctionName__doc__[] = "\
env_getDeffunctionName(env, deffunction) -> str\n\
retrieve the name of specified deffunction\n\
returns: name as a string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *e_getDeffunctionName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDeffunctionName(env, clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDeffunctionPPForm */
static char e_getDeffunctionPPForm__doc__[] = "\
env_getDeffunctionPPForm(env, deffunction) -> str\n\
retrieve the pretty-print form of specified deffunction\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *e_getDeffunctionPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDeffunctionPPForm(env, clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDeffunctionWatch */
static char e_getDeffunctionWatch__doc__[] = "\
env_getDeffunctionWatch(env, deffunction) -> bool\n\
tell whether or not the deffunction is being watched\n\
returns: True if the deffunction is being watched, False otherwise\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to inspect";
static PyObject *e_getDeffunctionWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    i = EnvGetDeffunctionWatch(env, clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDeffunction */
static char e_getNextDeffunction__doc__[] = "\
env_getNextDeffunction(env [, deffunction]) -> deffunction\n\
retrieve next deffunction object in list, first if argument is omitted\n\
returns: a deffunction object, None if already at last deffunction\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to start from";
static PyObject *e_getNextDeffunction(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFFUNCTION(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDeffunction(env, p ? clips_deffunction_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_deffunction_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_deffunction_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDeffunctionDeletable */
static char e_isDeffunctionDeletable__doc__[] = "\
env_isDeffunctionDeletable(env, deffunction) -> bool\n\
tell whether or not the deffunction is deletable\n\
returns: True if the deffunction can be deleted, False otherwise\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to be inspected";
static PyObject *e_isDeffunctionDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    i = EnvIsDeffunctionDeletable(env, clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDeffunctions */
static char e_listDeffunctions__doc__[] = "\
env_listDeffunctions(env, logicalname [, module])\n\
list deffunctions to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDeffunctions(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDeffunctions(env, lname,
        module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDeffunctionWatch */
static char e_setDeffunctionWatch__doc__[] = "\
env_setDeffunctionWatch(env, state, deffunction)\n\
set the specified deffunction to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  deffunction (deffunction) - the deffunction object";
static PyObject *e_setDeffunctionWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *state = NULL;
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFFUNCTION(env, p);
    EnvSetDeffunctionWatch(env,
        PyObject_IsTrue(state), clips_deffunction_value(p));
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undeffunction */
static char e_undeffunction__doc__[] = "\
env_undeffunction(env [, deffunction])\n\
delete a deffunction object or all deffunction objects\n\
arguments:\n\
  deffunction (deffunction) - the deffunction to delete, all if omitted";
static PyObject *e_undeffunction(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeffunctionObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DeffunctionType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFFUNCTION(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndeffunction(env, p ? clips_deffunction_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.10 [E] - Defgeneric Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_defgenericExists(void *env, void *ptr) {
    void *rv = EnvGetNextDefgeneric(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDefgeneric(env, rv);
    }
    return NULL;
}
#define EPYDEFGENERIC_EXISTS(_e, _p) \
    env_defgenericExists(_e, clips_defgeneric_value(_p))
#define ECHECK_DEFGENERIC(_e, _p) do { \
        if(!EPYDEFGENERIC_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFGENERIC(_e, _p) do { \
        if(_p && !EPYDEFGENERIC_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_defgenericModule */
static char e_defgenericModule__doc__[] = "\
env_defgenericModule(env, defgeneric) -> str\n\
retrieve the module name where specified defgeneric is defined\n\
returns: module name as string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_defgenericModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDefgenericModule(env, clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefgeneric */
static char e_findDefgeneric__doc__[] = "\
env_findDefgeneric(env, name) -> defgeneric\n\
return the defgeneric object associated with name\n\
returns: the defgeneric as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *e_findDefgeneric(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefgeneric(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defgeneric_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defgeneric_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefgenericList */
static char e_getDefgenericList__doc__[] = "\
env_getDefgenericList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of defgeneric objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDefgenericList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefgenericList(env, &o,
        module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefgenericName */
static char e_getDefgenericName__doc__[] = "\
env_getDefgenericName(env, defgeneric) -> str\n\
retrieve the name of specified defgeneric\n\
returns: name as a string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefgenericName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefgenericName(env, clips_deffunction_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefgenericPPForm */
static char e_getDefgenericPPForm__doc__[] = "\
env_getDefgenericPPForm(env, defgeneric) -> str\n\
retrieve the pretty-print form of specified defgeneric\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefgenericPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefgenericPPForm(env, clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefgenericWatch */
static char e_getDefgenericWatch__doc__[] = "\
env_getDefgenericWatch(env, defgeneric) -> bool\n\
tell whether or not the defgeneric is being watched\n\
returns: True if the defgeneric is being watched, False otherwise\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefgenericWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    i = EnvGetDefgenericWatch(env, clips_deffunction_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDefgeneric */
static char e_getNextDefgeneric__doc__[] = "\
env_getNextDefgeneric(env [, defgeneric]) -> defgeneric\n\
retrieve next defgeneric object in list, first if argument is omitted\n\
returns: a defgeneric object, None if already at last defgeneric\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to start from";
static PyObject *e_getNextDefgeneric(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefgeneric(env, p ? clips_defgeneric_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defgeneric_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defgeneric_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDefgenericDeletable */
static char e_isDefgenericDeletable__doc__[] = "\
env_isDefgenericDeletable(env, defgeneric) -> bool\n\
tell whether or not the defgeneric is deletable\n\
returns: True if the defgeneric can be deleted, False otherwise\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to be inspected";
static PyObject *e_isDefgenericDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    i = IsDefgenericDeletable(clips_defgeneric_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefgenerics */
static char e_listDefgenerics__doc__[] = "\
env_listDefgenerics(env, logicalname [, module])\n\
list defgeneric objects to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDefgenerics(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefgenerics(env, lname,
        module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefgenericWatch */
static char e_setDefgenericWatch__doc__[] = "\
env_setDefgenericWatch(env, state, defgeneric)\n\
set the specified defgeneric to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defgeneric (defgeneric) - the defgeneric object";
static PyObject *e_setDefgenericWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *state = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefgenericType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetDefgenericWatch(env,
        PyObject_IsTrue(state), clips_defgeneric_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefgeneric */
static char e_undefgeneric__doc__[] = "\
env_undefgeneric(env [, defgeneric])\n\
delete a defgeneric object or all defgeneric objects\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to delete, all if omitted";
static PyObject *e_undefgeneric(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefgeneric(env, p ? clips_defgeneric_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.11 [E] - Defmethod function */

/* env_getDefmethodDescription */
static char e_getDefmethodDescription__doc__[] = "\
env_getDefmethodDescription(env, index, defgeneric) -> str\n\
describe method parameter restrictions\n\
returns: the description as a string\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefmethodDescription(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;
    int i = 0;
    clips_DefgenericObject *p = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefmethodDescription(env, buffer,
        ppbuffer_size-1, clips_defgeneric_value(p), i);
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* env_getDefmethodList */
static char e_getDefmethodList__doc__[] = "\
env_getDefmethodList(env [, defgeneric]) -> (MULTIFIELD, list)\n\
retrieve the list of defmethods in the specified defgeneric\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defgeneric (defgeneric) - the defgeneric to inspect, all if omitted";
static PyObject *e_getDefmethodList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *g = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefgenericType, &g))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(g)
        ECHECK_DEFGENERIC(env, g);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefmethodList(env, g ? clips_defgeneric_value(g) : NULL, &o);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefmethodPPForm */
static char e_getDefmethodPPForm__doc__[] = "\
env_getDefmethodPPForm(env, index, defgeneric) -> str\n\
retrieve the pretty-print form of specified defmethod\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefmethodPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefmethodPPForm(env, clips_defgeneric_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmethodWatch */
static char e_getDefmethodWatch__doc__[] = "\
env_getDefmethodWatch(env, index, defgeneric) -> bool\n\
tell whether or not a defgeneric method is being watched\n\
returns: True if the method is being watched, False otherwise\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getDefmethodWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    i = EnvGetDefmethodWatch(env, clips_defgeneric_value(p), i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getMethodRestrictions */
static char e_getMethodRestrictions__doc__[] = "\
env_getMethodRestrictions(env, index, defgeneric) -> (MULTIFIELD, list)\n\
retrieve restriction for specified defmethod\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  index (int) - index of method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getMethodRestrictions(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    PyObject *q = NULL;
    int i = 0;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!i|O!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvGetMethodRestrictions(env, p ? clips_defgeneric_value(p) : NULL, i, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_getNextDefmethod */
static char e_getNextDefmethod__doc__[] = "\
env_getNextDefmethod(env, index, defgeneric) -> int\n\
return index of next defmethod in defgeneric object\n\
returns: an integer value\n\
arguments:\n\
  index (int) - index of method, zero for first method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_getNextDefmethod(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    i = EnvGetNextDefmethod(env, p ? clips_defgeneric_value(p) : NULL, i);
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_isDefmethodDeletable */
static char e_isDefmethodDeletable__doc__[] = "\
env_isDefmethodDeletable(env, index, defgeneric) -> bool\n\
tell whether the specified method is deletable or not\n\
returns: True if the method can be deleted, False otherwise\n\
arguments:\n\
  index (int) - index of method, zero for first method\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_isDefmethodDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    i = EnvIsDefmethodDeletable(
        env, p ? clips_defgeneric_value(p) : NULL, i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefmethods */
static char e_listDefmethods__doc__[] = "\
env_listDefmethods(env, output, defgeneric)\n\
list the defmethod in the defgeneric object\n\
arguments:\n\
  output (str) - logical name of output stream\n\
  defgeneric (defgeneric) - the defgeneric to inspect";
static PyObject *e_listDefmethods(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefgenericObject *p = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefgenericType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefmethods(env, lname, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefmethodWatch */
static char e_setDefmethodWatch__doc__[] = "\
env_setDefmethodWatch(env, state, index, defgeneric)\n\
set the specified defgeneric to a particular watch state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defgeneric (defgeneric) - the defgeneric object";
static PyObject *e_setDefmethodWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *state = NULL;
    int i = 0;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!OiO!",
                         &clips_EnvType, &pyenv,
                         &state, &i, &clips_DefgenericType, &p))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFGENERIC(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetDefmethodWatch(env,
        PyObject_IsTrue(state), clips_defgeneric_value(p), i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefmethod */
static char e_undefmethod__doc__[] = "\
env_undefmethod(env [, index, defgeneric])\n\
delete a defmethod object or all defmethod objects\n\
arguments:\n\
  index (int) - index of defmethod to delete, all if omitted\n\
  defgeneric (defgeneric) - referred defgeneric, all if omitted";
static PyObject *e_undefmethod(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    int i = 0;
    clips_DefgenericObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!|iO!",
                         &clips_EnvType, &pyenv,
                         &i, &clips_DefgenericType, &p))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != 0 && !p) {
        ERROR_VALUE("both arguments must be omitted or specified");
        FAIL();
    }
    if(p)
        ECHECK_DEFGENERIC(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefmethod(env, p ? clips_defgeneric_value(p) : NULL, i)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.12 [E] - Defclass functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_defclassExists(void *env, void *ptr) {
    void *rv = EnvGetNextDefclass(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDefclass(env, rv);
    }
    return NULL;
}
#define EPYDEFCLASS_EXISTS(_e, _p) \
    env_defclassExists(_e, clips_defclass_value(_p))
#define ECHECK_DEFCLASS(_e, _p) do { \
        if(!EPYDEFCLASS_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFCLASS(_e, _p) do { \
        if(_p && !EPYDEFCLASS_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_browseClasses */
static char e_browseClasses__doc__[] = "\
env_browseClasses(env, output, defclass)\n\
print the classes who inherit from specified one in a graph form\n\
arguments:\n\
  output (str) - the name of logical output\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *e_browseClasses(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!sO!",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvBrowseClasses(env, s, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_classAbstractP */
static char e_classAbstractP__doc__[] = "\
env_classAbstractP(env, defclass) -> bool\n\
tell if class is abstract or concrete\n\
returns: True if the class is abstract, False if concrete\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *e_classAbstractP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvClassAbstractP(env, clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_classReactiveP */
static char e_classReactiveP__doc__[] = "\
env_classReactiveP(env, defclass) -> bool\n\
tell if class is reactive (matches object patterns) or not\n\
returns: True if the class is reactive, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *e_classReactiveP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvClassReactiveP(env, clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_classSlots */
static char e_classSlots__doc__[] = "\
env_classSlots(env, defclass, inherit) -> (MULTIFIELD, list)\n\
return names of slots of the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *e_classSlots(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!O",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvClassSlots(env, clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_classSubclasses */
static char e_classSubclasses__doc__[] = "\
env_classSubclasses(env, defclass, inherit) -> (MULTIFIELD, list)\n\
return names of subclasses for the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *e_classSubclasses(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!O",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvClassSubclasses(env, clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_classSuperclasses */
static char e_classSuperclasses__doc__[] = "\
env_classSuperclasses(env, defclass, inherit) -> (MULTIFIELD, list)\n\
return names of superclasses for the passed in class\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  inherit (bool) - True to include inherited slots, False otherwise";
static PyObject *e_classSuperclasses(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!O",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvClassSuperclasses(env, clips_defclass_value(p), &o, PyObject_IsTrue(q));
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_defclassModule */
static char e_defclassModule__doc__[] = "\
env_defclassModule(env, defclass) -> str\n\
retrieve the name of the module where the provided defclass resides\n\
returns: a string containing a module name\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *e_defclassModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDefclassModule(env, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_describeClass */
static char e_describeClass__doc__[] = "\
env_describeClass(env, output, defclass)\n\
print a descriptive summary of class\n\
arguments:\n\
  output (str) - logical name of output stream\n\
  defclass (defclass) - the defclass to inspect";
static PyObject *e_describeClass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *lname = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!sO!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvDescribeClass(env, lname, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefclass */
static char e_findDefclass__doc__[] = "\
env_findDefclass(env, name) -> defclass\n\
retrieve defclass object corresponding to the specified name\n\
returns: the defclass as a new object\n\
arguments:\n\
  name (str) - the name of the defclass to look for";
static PyObject *e_findDefclass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefclass(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defclass_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* getClassDefaultsMode */
E_FUNC_GET_ONLY(env_getClassDefaultsMode,
                e_getClassDefaultsMode,
                EnvGetClassDefaultsMode,
                "i")

/* env_getDefclassList */
static char e_getDefclassList__doc__[] = "\
env_getDefclassList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve list of defclass objects names in specified defmodule\n\
returns:  MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDefclassList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefclassList(env, &o,
        module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefclassName */
static char e_getDefclassName__doc__[] = "\
env_getDefclassName(env, defclass) -> str\n\
retrieve the name of given defclass object\n\
returns: a string containing the name\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *e_getDefclassName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefclassName(env, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefclassPPForm */
static char e_getDefclassPPForm__doc__[] = "\
env_getDefclassPPForm(env, defclass) -> str\n\
retrieve the pretty-print form of given defclass object\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *e_getDefclassPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefclassPPForm(env, clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefclassWatchInstances */
static char e_getDefclassWatchInstances__doc__[] = "\
env_getDefclassWatchInstances(env, defclass) -> bool\n\
tell if defclass instances are being watched\n\
returns: True if defclass instances are being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *e_getDefclassWatchInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvGetDefclassWatchInstances(env, clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefclassWatchSlots */
static char e_getDefclassWatchSlots__doc__[] = "\
env_getDefclassWatchSlots(env, defclass) -> bool\n\
tell if defclass slots are being watched\n\
returns: True if defclass slots are being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *e_getDefclassWatchSlots(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvGetDefclassWatchSlots(env, clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDefclass */
static char e_getNextDefclass__doc__[] = "\
env_getNextDefclass(env [, defclass]) -> defclass\n\
find next defclass in the list, first if argument is omitted\n\
returns: next defclass object, None if already at last defclass\n\
arguments:\n\
  defclass (defclass) - the defclass to start from";
static PyObject *e_getNextDefclass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefclass(env, p ? clips_defclass_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defclass_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDefclassDeletable */
static char e_isDefclassDeletable__doc__[] = "\
env_isDefclassDeletable(env, defclass) -> bool\n\
tell whether or not the defclass can be deleted\n\
returns: True if the defclass can be deleted, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass object";
static PyObject *e_isDefclassDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvIsDefclassDeletable(env, clips_defclass_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefclasses */
static char e_listDefclasses__doc__[] = "\
env_listDefclasses(env, logicalname [, module])\n\
list defclasses to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDefclasses(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefclasses(env, lname,
        module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setClassDefaultsMode */
static char e_setClassDefaultsMode__doc__[] = "\
env_setClassDefaultsMode(env, mode)\n\
set default mode for classes\n\
arguments:\n\
  mode (int) - the new default mode";
static PyObject *e_setClassDefaultsMode(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    int i = 0;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "O!i", &clips_EnvType, &pyenv, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != CONVENIENCE_MODE && i != CONSERVATION_MODE) {
        ERROR_VALUE("invalid mode value");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    EnvSetClassDefaultsMode(env, (unsigned short)i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefclassWatchInstances */
static char e_setDefclassWatchInstances__doc__[] = "\
env_setDefclassWatchInstances(env, state, defclass)\n\
tell to system if defclass instances are to be watched\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass object";
static PyObject *e_setDefclassWatchInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefclassType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetDefclassWatchInstances(env,
        PyObject_IsTrue(state), clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefclassWatchSlots */
static char e_setDefclassWatchSlots__doc__[] = "\
env_setDefclassWatchSlots(env, state, defclass)\n\
tell to system if defclass slots are to be watched\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass object";
static PyObject *e_setDefclassWatchSlots(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefclassType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetDefclassWatchSlots(env,
        PyObject_IsTrue(state), clips_defclass_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_slotAllowedValues */
static char e_slotAllowedValues__doc__[] = "\
env_slotAllowedValues(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve allowed values for specified slot\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotAllowedValues(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotAllowedValues(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_slotCardinality */
static char e_slotCardinality__doc__[] = "\
env_slotCardinality(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve cardinality information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotCardinality(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotCardinality(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

#if CLIPS_MINOR > 23

/* slotAllowedClasses */
static char e_slotAllowedClasses__doc__[] = "\
env_slotAllowedClasses(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve the allowed classes for a slot of given class\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_slotAllowedClasses(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotAllowedClasses(env, clips_defclass_value(p), name, &o);
    rv = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* slotDefaultValue */
static char e_slotDefaultValue__doc__[] = "\
env_slotDefaultValue(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve default value(s) for a slot of given defclass\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - name of the slot to inspect";
static PyObject *e_slotDefaultValue(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DeftemplObject *p = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };
    PyObject *rv = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &name))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotDefaultValue(env, clips_defclass_value(p), name, &o);
    rv = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!rv) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(rv);

BEGIN_FAIL
    SKIP();
END_FAIL
}

#else
UNIMPLEMENT_VERSION(env_slotAllowedClasses, e_slotAllowedClasses)
UNIMPLEMENT_VERSION(env_slotDefaultValue, e_slotDefaultValue)
#endif /* CLIPS_MINOR > 23 */

/* env_slotDirectAccessP */
static char e_slotDirectAccessP__doc__[] = "\
env_slotDirectAccessP(env, defclass, name) -> bool\n\
tell if slot is directly accessible\n\
returns: True if slot is directly accessible, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotDirectAccessP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvSlotDirectAccessP(env, clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_slotExistP */
static char e_slotExistP__doc__[] = "\
env_slotExistP(env, defclass, name, inherit) -> bool\n\
tell if slot exists\n\
returns: True if slot exists, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotExistP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!sO",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvSlotExistP(env, clips_defclass_value(p), s, PyObject_IsTrue(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_slotFacets */
static char e_slotFacets__doc__[] = "\
env_slotFacets(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve facet values information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotFacets(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotFacets(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_slotInitableP */
static char e_slotInitableP__doc__[] = "\
env_slotInitableP(env, defclass, name) -> bool\n\
tell if slot is initializable\n\
returns: True if slot can be initialized, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotInitableP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvSlotInitableP(env, clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_slotPublicP */
static char e_slotPublicP__doc__[] = "\
env_slotPublicP(env, defclass, name) -> bool\n\
tell if slot is public\n\
returns: True if slot is public, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotPublicP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvSlotPublicP(env, clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_slotRange */
static char e_slotRange__doc__[] = "\
env_slotRange(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve numeric range information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotRange(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotRange(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_slotSources */
static char e_slotSources__doc__[] = "\
env_slotSources(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve the name of class sources for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotSources(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotSources(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_slotTypes */
static char e_slotTypes__doc__[] = "\
env_slotTypes(env, defclass, name) -> (MULTIFIELD, list)\n\
retrieve cardinality information for specified slot\n\
returns: MULTIFIELD and a list of pairs (type, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotTypes(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *q = NULL;
    char *s = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSlotTypes(env, clips_defclass_value(p), s, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_slotWritableP */
static char e_slotWritableP__doc__[] = "\
env_slotWritableP(env, defclass, name) -> bool\n\
tell whether slot can be overwritten or not\n\
returns: True if slot is writeable, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  name (str) - the slot name";
static PyObject *e_slotWritableP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &s))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvSlotWritableP(env, clips_defclass_value(p), s);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_subclassP */
static char e_subclassP__doc__[] = "\
env_subclassP(env, defclass1, defclass2) -> bool\n\
tell if defclass1 is a subclass of defclass2\n\
returns: True if relationship is satisfied, False otherwise\n\
arguments:\n\
  defclass1, defclass2 (defclass) - test objects";
static PyObject *e_subclassP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL, *q = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &clips_DefclassType, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ECHECK_DEFCLASS(env, q);
    i = EnvSubclassP(env, clips_defclass_value(p), clips_defclass_value(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_superclassP */
static char e_superclassP__doc__[] = "\
env_superclassP(env, defclass1, defclass2) -> bool\n\
tell if defclass1 is a superclass of defclass2\n\
returns: True if relationship is satisfied, False otherwise\n\
arguments:\n\
  defclass1, defclass2 (defclass) - test objects";
static PyObject *e_superclassP(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL, *q = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &clips_DefclassType, &q))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ECHECK_DEFCLASS(env, q);
    i = EnvSuperclassP(env, clips_defclass_value(p), clips_defclass_value(q));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefclass */
static char e_undefclass__doc__[] = "\
env_undefclass(env, defclass)\n\
remove a class and all its subclasses from system\n\
arguments:\n\
  defclass (defclass) - the defclass to remove";
static PyObject *e_undefclass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefclassType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFCLASS(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefclass(env, clips_defclass_value(p))) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.13 [E] - Instance functions */

/* env_binaryLoadInstances */
static char e_binaryLoadInstances__doc__[] = "\
env_binaryLoadInstances(env, filename) -> numinstances\n\
binary load a set of instances from named file\n\
returns: the number of loaded instances\n\
arguments:\n\
  filename (str) - the name of binary file to load from";
static PyObject *e_binaryLoadInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    i = EnvBinaryLoadInstances(env, fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    else RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_binarySaveInstances */
static char e_binarySaveInstances__doc__[] = "\
env_binarySaveInstances(env, filename, scope) -> int\n\
dump instances to file\n\
returns: the number of saved instances\n\
arguments:\n\
  filename (str) - the name of binary file to save to\n\
  scope (int) - one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *e_binarySaveInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    long i = 0;

    if(!PyArg_ParseTuple(args, "O!si", &clips_EnvType, &pyenv, &fn, &i))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != LOCAL_SAVE && i != VISIBLE_SAVE) {
        ERROR_VALUE("invalid scope");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    i = EnvBinarySaveInstances(env, fn, i, NULL, TRUE);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    } /* do not know */
    else RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_createRawInstance */
static char e_createRawInstance__doc__[] = "\
env_createRawInstance(env, defclass, name) -> instance\n\
create an instance of specified class with given name\n\
returns: the instance as a new object\n\
arguments:\n\
  defclass (defclass) - the defclass to create an instance of\n\
  name (str) - the name of the instance";
static PyObject *e_createRawInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    clips_InstanceObject *q = NULL;
    void *ptr = 0;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvCreateRawInstance(env, clips_defclass_value(p), name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_decrementInstanceCount */
UNIMPLEMENT(env_decrementInstanceCount, e_decrementInstanceCount)

/* env_deleteInstance */
static char e_deleteInstance__doc__[] = "\
env_deleteInstance(env [, instance])\n\
delete specified instance\n\
arguments:\n\
  instance (instance) - the instance to delete, all if omitted";
static PyObject *e_deleteInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    if(p)
        ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvDeleteInstance(env, p ? clips_instance_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_directGetSlot */
static char e_directGetSlot__doc__[] = "\
env_directGetSlot(env, instance, slotname) -> (type, value)\n\
get value in specified slot of given instance\n\
returns: a pair (type, value)\n\
arguments:\n\
  instance (instance) - the instance to inspect\n\
  slotname (str) - the slot to retrieve";
static PyObject *e_directGetSlot(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    PyObject *q = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s",
                         &clips_EnvType, &pyenv,
                         &clips_InstanceType, &p, &name))
        FAIL();
    ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvDirectGetSlot(env, clips_instance_value(p), name, &o);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_directPutSlot */
static char e_directPutSlot__doc__[] = "\
env_directPutSlot(env, instance, slotname, (type, value))\n\
put a value in specified slot of given instance\n\
arguments:\n\
  instance (instance) - the instance to inspect\n\
  slotname (str) - the slot to retrieve\n\
  type (int) - the type of value to assign\n\
  value (object or list of pairs) - the value to assign";
static PyObject *e_directPutSlot(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    PyObject *q = NULL;
    char *name = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!sO",
                         &clips_EnvType, &pyenv,
                         &clips_InstanceType, &p, &name, &q))
        FAIL();
    ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(!i_py2do_e(env, q, &o)) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvDirectPutSlot(env, clips_instance_value(p), name, &o)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_OTHER("instance slot could not be modified");
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findInstance */
static char e_findInstance__doc__[] = "\
env_findInstance(env, name, srchimports [, module]) -> instance\n\
find the instance with the specified name\n\
returns: the instance as a new object\n\
arguments:\n\
  name (str) - instance name\n\
  srchimports (bool) - True if imported modules have to be inspected\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_findInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    clips_InstanceObject *q = NULL;
    char *name = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!sO|O!",
                         &clips_EnvType, &pyenv,
                         &name, &p, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindInstance(env,
        module ? clips_defmodule_value(module) : NULL, name,
        PyObject_IsTrue(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_getInstanceClass */
static char e_getInstanceClass__doc__[] = "\
env_getInstanceClass(env, instance) -> defclass\n\
retrieve the class of specified instance\n\
returns: the instance defclass as a new object\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *e_getInstanceClass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    clips_DefclassObject *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetInstanceClass(env, clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defclass_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defclass_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_getInstanceName */
static char e_getInstanceName__doc__[] = "\
env_getInstanceName(env, instance) -> str\n\
retrieve the name of specified instance\n\
returns: the requested name of instance\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *e_getInstanceName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetInstanceName(env, clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getInstancePPForm */
static char e_getInstancePPForm__doc__[] = "\
env_getInstancePPForm(env, instance) -> str\n\
retrieve the pretty-print form of specified instance\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  instance (instance) - the instance to inspect";
static PyObject *e_getInstancePPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    char *buffer = NEW_ARRAY(char, ppbuffer_size);
    PyObject *rv = NULL;

    if(!buffer) {
        ERROR_MEMORY("cannot allocate buffer");
        FAIL();
    }
    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetInstancePPForm(env, buffer,
        ppbuffer_size-1, clips_instance_value(p));
    RELEASE_MEMORY_ERROR();
    rv = Py_BuildValue("s", buffer);
    DELETE(buffer);
    return rv;

BEGIN_FAIL
    if(buffer) DELETE(buffer);
    SKIP();
END_FAIL
}

/* getInstancesChanged */
E_STATUS_FUNC_GET_BOOL(env_getInstancesChanged,
                       e_getInstancesChanged,
                       EnvGetInstancesChanged)

/* env_getNextInstance */
static char e_getNextInstance__doc__[] = "\
env_getNextInstance(env [, instance]) -> instance\n\
retrieve next instance object in list, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  instance (defrule) - the instance to start from";
static PyObject *e_getNextInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    if(p)
        ENV_CHECK_VALID_INSTANCE(env, p);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextInstance(env, p ? clips_instance_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_getNextInstanceInClass */
static char e_getNextInstanceInClass__doc__[] = "\
env_getNextInstanceInClass(env, defclass [, instance]) -> instance\n\
retrieve next instance object in class, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  instance (instance) - the instance to start from";
static PyObject *e_getNextInstanceInClass(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL, *q = NULL;
    clips_DefclassObject *c = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!O!|O!",
        &clips_EnvType, &pyenv, &clips_DefclassType, &c,
        &clips_InstanceType, &p)) FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    if(p) { ENV_CHECK_VALID_INSTANCE(env, p); }
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, c);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextInstanceInClass(env,
        clips_defclass_value(c), p ? clips_instance_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* getNextInstanceInClassAndSubclasses */
static char e_getNextInstanceInClassAndSubclasses__doc__[] = "\
getNextInstanceInClassAndSubclasses(env, defclass [, instance]) -> instance\n\
retrieve next instance in class/subclasses, first if argument is omitted\n\
returns: a instance object, None if already at last instance\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  instance (instance) - the instance to start from";
static PyObject *e_getNextInstanceInClassAndSubclasses(PyObject *self, PyObject *args) {
    clips_InstanceObject *p = NULL, *q = NULL;
    clips_DefclassObject *c = NULL;
    DATA_OBJECT o = { 0 };
    void *ptr = NULL;
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;

    if(!PyArg_ParseTuple(args, "O!O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &c, &clips_InstanceType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    if(p)
        ENV_CHECK_VALID_INSTANCE(env, p);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, c);
    /* we should iterate from the start in order to keep the iteration data */
    ACQUIRE_MEMORY_ERROR();
    if(p) {
        ptr = EnvGetNextInstanceInClassAndSubclasses_PY(env,
            clips_defclass_value(c), NULL, &o);
        /* move cursor to the instance we passed in */
        while(ptr && ptr != clips_instance_value(p))
            ptr = EnvGetNextInstanceInClassAndSubclasses_PY(env,
                clips_defclass_value(c), ptr, &o);
        /* move cursor one step forward if conditions met (may return NULL) */
        if(ptr)
            ptr = EnvGetNextInstanceInClassAndSubclasses_PY(env,
                clips_defclass_value(c), ptr, &o);
    } else
        ptr = EnvGetNextInstanceInClassAndSubclasses_PY(env,
            clips_defclass_value(c), NULL, &o);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_incrementInstanceCount */
UNIMPLEMENT(env_incrementInstanceCount, e_incrementInstanceCount)

/* env_instances */
static char e_instances__doc__[] = "\
env_instances(env, output [, module] [, class [, subclassflag]]])\n\
list instances in specified defmodule to logical output\n\
arguments:\n\
  output (str) - the name of logical output stream\n\
  module (defmodule) - the defmodule to inspect, all if omitted\n\
  class (str) - the class name to inspect, all if omitted\n\
  subclassflag (bool) - True if all subclasses must be recursed";
static PyObject *e_instances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = Py_None;
    char *name = NULL, *output = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!sO",
                         &clips_EnvType, &pyenv, &output,
                         &clips_DefmoduleType, &module, &name, &p)) {
        PyErr_Clear();
        module = NULL;
        if(!PyArg_ParseTuple(args, "O!s|sO",
                             &clips_EnvType, &pyenv, &output, &name, &p)) {
            PyErr_Clear();
            ERROR_TYPE("invalid argument(s) or invalid argument order");
            FAIL();
        }
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvInstances(env, output,
        module ? clips_defmodule_value(module) : NULL,
        name, p ? PyObject_IsTrue(p) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_loadInstances */
static char e_loadInstances__doc__[] = "\
env_loadInstances(env, filename) -> int\n\
load instance from specified file\n\
returns: the number of loaded instances\n\
arguments:\n\
  filename (str) - the name of file to load instances from";
static PyObject *e_loadInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    i = EnvLoadInstances(env, fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_makeInstance */
static char e_makeInstance__doc__[] = "\
env_makeInstance(env, command) -> instance\n\
create and initialize an instance using given command\n\
returns: a new instance\n\
arguments:\n\
  command (str) - command used to create instance";
static PyObject *e_makeInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *q = NULL;
    char *s = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &s))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvMakeInstance(env, s);
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    if(!ptr) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_instance_New(env, q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_instance_assign(q, ptr);
    ENV_CHECK_VALID_INSTANCE(env, q);
    clips_instance_lock(q);
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_restoreInstances */
static char e_restoreInstances__doc__[] = "\
env_restoreInstances(env, filename) -> numinstances\n\
restore instance from specified file\n\
returns: number of restored instances\n\
arguments:\n\
  filename (str) - the name of file to restore instances from";
static PyObject *e_restoreInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &fn))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    i = EnvRestoreInstances(env, fn);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_saveInstances */
static char e_saveInstances__doc__[] = "\
env_saveInstances(env, filename, scope)\n\
save instances to specified file\n\
returns: the number of saved instances\n\
arguments:\n\
  filename (str) - the name of file to save instances to\n\
  scope (int) - one of LOCAL_SAVE or VISIBLE_SAVE";
static PyObject *e_saveInstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *fn = NULL;
    long i = 0;

    if(!PyArg_ParseTuple(args, "O!si", &clips_EnvType, &pyenv, &fn, &i))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(i != LOCAL_SAVE && i != VISIBLE_SAVE) {
        ERROR_VALUE("invalid scope");
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    i = EnvSaveInstances(env, fn, i, NULL, TRUE);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_IO();
        FAIL();
    } /* do not know */
    else RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_send */
static char e_send__doc__[] = "\
env_send(env, instance, type, message [, arguments])\n\
send a message to specified object\n\
returns: the result value for the operation if any\n\
arguments:\n\
  instance (instance) - instance to send message to\n\
  message (str) - message to send\n\
  arguments (str) - blank separated constant arguments";
static PyObject *e_send(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *p = NULL, *q = NULL;
    char *msg = NULL, *msa = NULL;
    DATA_OBJECT o = { 0 }, rv = { 0 };

    if(!PyArg_ParseTuple(args, "O!O!s|s",
                         &clips_EnvType, &pyenv,
                         &clips_InstanceType, &p, &msg, &msa))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CHECK_VALID_INSTANCE(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    SetType(o, INSTANCE_ADDRESS);
    SetValue(o, clips_instance_value(p));
    EnvSend(env, &o, msg, msa, &rv);
    q = i_do2py(&rv);
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    if(q) {
        RETURN_PYOBJECT(q);
    } else RETURN_NONE();

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* setInstancesChanged */
E_STATUS_FUNC_SET_BOOL(env_setInstancesChanged,
                       e_setInstancesChanged,
                       EnvSetInstancesChanged)

/* env_unmakeInstance */
static char e_unmakeInstance__doc__[] = "\
env_unmakeInstance(env [, instance])\n\
delete specified instance (passing a message)\n\
arguments:\n\
  instance (instance) - instance to delete, all if omitted";
static PyObject *e_unmakeInstance(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    if(p)
        ENV_CHECK_VALID_INSTANCE(env, p);
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUnmakeInstance(env, p ? clips_instance_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_validInstanceAddress */
static char e_validInstanceAddress__doc__[] = "\
env_validInstanceAddress(env, instance) -> bool\n\
tell whether the instance still exists or not\n\
returns: True if instance exists, False otherwise\n\
arguments:\n\
  instance (instance) - istance to test";
static PyObject *e_validInstanceAddress(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_InstanceObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_InstanceType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    i = EnvValidInstanceAddress(env, clips_instance_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_loadInstancesFromString */
static char e_loadInstancesFromString__doc__[] = "\
env_loadInstancesFromString(env, string [, maxpos]) -> int\n\
load instances from a string\n\
returns: the number of loaded instances\n\
arguments:\n\
  string (str) - string to read from\n\
  maxpos (int) - last char to read, all string if omitted";
static PyObject *e_loadInstancesFromString(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    int i = -1;

    if(!PyArg_ParseTuple(args, "O!s|i", &clips_EnvType, &pyenv, &s, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    i = EnvLoadInstancesFromString(env, s, i);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_restoreInstancesFromString */
static char e_restoreInstancesFromString__doc__[] = "\
env_restoreInstancesFromString(env, string [, maxpos]) -> int\n\
restore instances from a string\n\
returns: number of restored instances\n\
arguments:\n\
  string (str) - string to read from\n\
  maxpos (int) - last char to read, all string if omitted";
static PyObject *e_restoreInstancesFromString(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *s = NULL;
    int i = -1;

    if(!PyArg_ParseTuple(args, "O!s|i", &clips_EnvType, &pyenv, &s, &i))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    i = EnvRestoreInstancesFromString(env, s, i);
    RELEASE_MEMORY_ERROR();
    if(i < 0) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.14 [E] - DefmessageHandler functions */

/* env_findDefmessageHandler */
static char e_findDefmessageHandler__doc__[] = "\
env_findDefmessageHandler(env, defclass, name, type) -> int\n\
find the matching message handler attached to defclass\n\
returns: index of message handler\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect object\n\
  name (str) - message handler name\n\
  type (str) - one of 'around', 'before', 'primary', 'after'";
static PyObject *e_findDefmessageHandler(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *name = NULL, *type = NULL;
    unsigned int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!ss",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &name, &type))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    i = EnvFindDefmessageHandler(env, clips_defclass_value(p), name, type);
    RELEASE_MEMORY_ERROR();
    if(i == 0) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmessageHandlerList */
static char e_getDefmessageHandlerList__doc__[] = "\
env_getDefmessageHandlerList(env, [defclass [, inh]]) -> (MULTIFIELD, list)\n\
retrieve list of message handlers attached to defclass\n\
returns: MULTIFIELD and a list of pairs (STRING, value)\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect, all if omitted\n\
  inh (bool) - True if inherited handlers are to list";
static PyObject *e_getDefmessageHandlerList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *p1 = NULL, *q = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!O",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &p1))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p) { ECHECK_DEFCLASS(env, p); }
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefmessageHandlerList(env,
        p ? clips_defclass_value(p) : NULL,
        &o, p1 ? PyObject_IsTrue(p1) : FALSE);
    q = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!q) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_getDefmessageHandlerName */
static char e_getDefmessageHandlerName__doc__[] = "\
env_getDefmessageHandlerName(env, defclass, index) -> str\n\
retrieve the name of specified message handler\n\
returns: name as string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_getDefmessageHandlerName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *name = NULL;
    unsigned int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefmessageHandlerName(env, clips_defclass_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmessageHandlerPPForm */
static char e_getDefmessageHandlerPPForm__doc__[] = "\
env_getDefmessageHandlerPPForm(env, defclass, index) -> str\n\
retrieve the pretty-print form of specified message handler\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_getDefmessageHandlerPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    unsigned int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefmessageHandlerPPForm(env, clips_defclass_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmessageHandlerType */
static char e_getDefmessageHandlerType__doc__[] = "\
env_getDefmessageHandlerType(env, defclass, index) -> str\n\
retrieve type of specified message handler\n\
returns: type as string\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_getDefmessageHandlerType(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL;
    unsigned int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvGetDefmessageHandlerType(env, clips_defclass_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmessageHandlerWatch */
static char e_getDefmessageHandlerWatch__doc__[] = "\
env_getDefmessageHandlerWatch(env, defclass, index) -> bool\n\
tell if specified message handler is being watched\n\
returns: True if the message handler is being watched, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_getDefmessageHandlerWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvGetDefmessageHandlerWatch(
        env, clips_defclass_value(p), (unsigned int)i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDefmessageHandler */
static char e_getNextDefmessageHandler__doc__[] = "\
env_getNextDefmessageHandler(env, defclass [, index]) -> int\n\
return index of next message handler for specified class\n\
returns: index as an integer, None if already at last handler\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of current handler, 0 or omitted for first";
static PyObject *e_getNextDefmessageHandler(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!|i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i < 0) {
        ERROR_VALUE("index must be positive or zero");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    i = EnvGetNextDefmessageHandler(env, clips_defclass_value(p), i);
    RELEASE_MEMORY_ERROR();
    if(i == 0)
        RETURN_NONE();
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_isDefmessageHandlerDeletable */
static char e_isDefmessageHandlerDeletable__doc__[] = "\
env_isDefmessageHandlerDeletable(env, defclass, index) -> bool\n\
tell whether or not the specified message handler can be deleted\n\
returns: True if the message handler can be deleted, False otherwise\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_isDefmessageHandlerDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    i = EnvIsDefmessageHandlerDeletable(
        env, clips_defclass_value(p), (unsigned int)i);
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefmessageHandlers */
static char e_listDefmessageHandlers__doc__[] = "\
env_listDefmessageHandlers(env, output, [, defclass [, inhflag]])\n\
list message handlers to logical output\n\
arguments:\n\
  output (str) - the name of output stream\n\
  defclass (defclass) - the defclass to inspect\n\
  inhflag (bool) - True to list inherited handlers";
static PyObject *e_listDefmessageHandlers(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    PyObject *p1 = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!O",
                         &clips_EnvType, &pyenv,
                         &s, &clips_DefclassType, &p, &p1))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefmessageHandlers(env, s,
        p ? clips_defclass_value(p) : NULL, p1 ? PyObject_IsTrue(p1) : FALSE);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_previewSend */
static char e_previewSend__doc__[] = "\
env_previewSend(env, output, defclass, messagename)\n\
list message handlers applicable to instances to logical output\n\
arguments:\n\
  output (str) - logical output stream name\n\
  defclass (defclass) - the defclass to inspect\n\
  messagename (str) - the name of message";
static PyObject *e_previewSend(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    char *s = NULL, *m = NULL;

    if(!PyArg_ParseTuple(args, "O!sO!s",
                         &clips_EnvType, &pyenv, &s,
                         &clips_DefclassType, &p, &m))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvPreviewSend(env, s, clips_defclass_value(p), m);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setDefmessageHandlerWatch */
static char e_setDefmessageHandlerWatch__doc__[] = "\
env_setDefmessageHandlerWatch(env, state, defclass, index)\n\
set watch on message handler to specified state\n\
arguments:\n\
  state (bool) - the new watch state\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_setDefmessageHandlerWatch(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    int i = 0;
    PyObject *state = NULL;

    if(!PyArg_ParseTuple(args, "O!OO!i",
                         &clips_EnvType, &pyenv,
                         &state, &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    EnvSetDefmessageHandlerWatch(env, PyObject_IsTrue(state),
        clips_defclass_value(p), (unsigned int)i);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefmessageHandler */
static char e_undefmessageHandler__doc__[] = "\
env_undefmessageHandler(env, defclass, index)\n\
remove specified message handler from system\n\
arguments:\n\
  defclass (defclass) - the defclass to inspect\n\
  index (int) - index of handler";
static PyObject *e_undefmessageHandler(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefclassObject *p = NULL;
    unsigned int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!i",
                         &clips_EnvType, &pyenv,
                         &clips_DefclassType, &p, &i))
        FAIL();
    if(i <= 0) {
        ERROR_VALUE("index must be positive");
        FAIL();
    }
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFCLASS(env, p);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefmessageHandler(env, clips_defclass_value(p), i)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* 4.15 [E] - Definstances Functions */

/* helper to check whether there is still this construct */
F_INLINE void *env_definstancesExists(void *env, void *ptr) {
    void *rv = EnvGetNextDefinstances(env, NULL);
    while(rv != NULL) {
        if(rv == ptr) return rv;
        else rv = EnvGetNextDefinstances(env, rv);
    }
    return NULL;
}
#define EPYDEFINSTANCES_EXISTS(_e, _p) \
    env_definstancesExists(_e, clips_definstances_value(_p))
#define ECHECK_DEFINSTANCES(_e, _p) do { \
        if(!EPYDEFINSTANCES_EXISTS(_e, _p)) { \
            ERROR_CLIPS_NOTFOUND(); \
            FAIL(); \
        } \
    } while(0)
#define ECHECK_RM_DEFINSTANCES(_e, _p) do { \
        if(_p && !EPYDEFINSTANCES_EXISTS(_e, _p)) { \
            ERROR_CLIPS_REMOVE(); \
            FAIL(); \
        } \
    } while(0)
/* API functions with documentation */

/* env_definstancesModule */
static char e_definstancesModule__doc__[] = "\
env_definstancesModule(env, definstances) -> str\n\
retrieve the module name where specified definstances is defined\n\
returns: module name as string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *e_definstancesModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFINSTANCES(env, p);
    ACQUIRE_MEMORY_ERROR();
    s = EnvDefinstancesModule(env, clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefinstances */
static char e_findDefinstances__doc__[] = "\
env_findDefinstances(env, name) -> definstances\n\
return the definstances object associated with name\n\
returns: the definstances as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *e_findDefinstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DefinstancesObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefinstances(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_definstances_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_definstances_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefinstancesList */
static char e_getDefinstancesList__doc__[] = "\
env_getDefinstancesList(env [, module]) -> (MULTIFIELD, list)\n\
retrieve the list of definstances objects in the specified defmodule\n\
returns: MULTIFIELD and a list of pairs (STRING, name)\n\
arguments:\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_getDefinstancesList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefinstancesList(
        env, &o, module ? clips_defmodule_value(module) : NULL);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefinstancesName */
static char e_getDefinstancesName__doc__[] = "\
env_getDefinstancesName(env, definstances) -> str\n\
retrieve the name of specified definstances\n\
returns: name as a string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *e_getDefinstancesName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFINSTANCES(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefinstancesName(env, clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefinstancesPPForm */
static char e_getDefinstancesPPForm__doc__[] = "\
env_getDefinstancesPPForm(env, definstances) -> str\n\
retrieve the pretty-print form of specified definstances\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  definstances (definstances) - the definstances to inspect";
static PyObject *e_getDefinstancesPPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFINSTANCES(env, p);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefinstancesPPForm(env, clips_definstances_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDefinstances */
static char e_getNextDefinstances__doc__[] = "\
env_getNextDefinstances(env [, definstances]) -> definstances\n\
retrieve next definstances object in list, first if argument is omitted\n\
returns: a definstances object, None if already at last definstances\n\
arguments:\n\
  definstances (definstances) - the definstances to start from";
static PyObject *e_getNextDefinstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(p)
        ECHECK_DEFINSTANCES(env, p);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefinstances(env, p ? clips_definstances_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_definstances_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_definstances_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_isDefinstancesDeletable */
static char e_isDefinstancesDeletable__doc__[] = "\
env_isDefinstancesDeletable(env, definstances) -> bool\n\
tell whether or not the definstances can be deleted\n\
returns: True if the definstances can be deleted, False otherwise\n\
arguments:\n\
  definstances (definstances) - the definstances to be inspected";
static PyObject *e_isDefinstancesDeletable(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_DEFINSTANCES(env, p);
    i = EnvIsDefinstancesDeletable(env, clips_definstances_value(p));
    RETURN_BOOL(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_listDefinstances */
static char e_listDefinstances__doc__[] = "\
env_listDefinstances(env, logicalname [, module])\n\
list definstances to output identified by logicalname\n\
arguments:\n\
  logicalname (str) - the logical name of output\n\
  module (defmodule) - the defmodule to inspect, all if omitted";
static PyObject *e_listDefinstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *module = NULL;
    char *lname = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O!",
                         &clips_EnvType, &pyenv,
                         &lname, &clips_DefmoduleType, &module))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefinstances(
        env, lname, module ? clips_defmodule_value(module) : NULL);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_undefinstances */
static char e_undefinstances__doc__[] = "\
env_undefinstances(env [, definstances])\n\
delete a definstances object or all definstances objects\n\
arguments:\n\
  definstances (definstances) - object to be deleted, all if omitted";
static PyObject *e_undefinstances(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefinstancesObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv,
                         &clips_DefinstancesType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ECHECK_RM_DEFINSTANCES(env, p);
    ENV_CLIPS_LOCK_GC(pyenv);
    ACQUIRE_MEMORY_ERROR();
    if(!EnvUndefinstances(env, p ? clips_definstances_value(p) : NULL)) {
        RELEASE_MEMORY_ERROR();
        ENV_CLIPS_UNLOCK_GC(pyenv);
        ERROR_CLIPS_REMOVE();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    ENV_CLIPS_UNLOCK_GC(pyenv);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_findDefmodule */
static char e_findDefmodule__doc__[] = "\
env_findDefmodule(env, name) -> defmodule\n\
return the defmodule object associated with name\n\
returns: the defmodule as a new object\n\
arguments:\n\
  name (str) - the name to look for";
static PyObject *e_findDefmodule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *name = NULL;
    void *ptr = NULL;
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &name))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvFindDefmodule(env, name);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getCurrentModule */
static char e_getCurrentModule__doc__[] = "\
env_getCurrentModule(env) -> defmodule\n\
return current module\n\
returns: current module as a new object";
static PyObject *e_getCurrentModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetCurrentModule(env);
    RELEASE_MEMORY_ERROR();
    if(!ptr) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_defmodule_New(p);
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(p) = ptr;
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefmoduleList */
static char e_getDefmoduleList__doc__[] = "\
env_getDefmoduleList(env) -> (MULTIFIELD, list)\n\
return the list of modules in system\n\
returns: MULTIFIELD and a list of pairs (STRING, name)";
static PyObject *e_getDefmoduleList(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *p = NULL;
    DATA_OBJECT o = { 0 };

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvGetDefmoduleList(env, &o);
    p = i_do2py_e(env, &o);
    RELEASE_MEMORY_ERROR();
    if(!p) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* env_getDefmoduleName */
static char e_getDefmoduleName__doc__[] = "\
env_getDefmoduleName(env, defmodule) -> str\n\
retrieve the name of specified defmodule\n\
returns: name as a string\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to inspect";
static PyObject *e_getDefmoduleName(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    name = EnvGetDefmoduleName(env, clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!name) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(name);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getDefmodulePPForm */
static char e_getDefmodulePPForm__doc__[] = "\
env_getDefmodulePPForm(env, defmodule) -> str\n\
retrieve the pretty-print form of specified defmodule\n\
returns: the requested pretty-print form as a string\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to inspect";
static PyObject *e_getDefmodulePPForm(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;
    char *s = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    ACQUIRE_MEMORY_ERROR();
    env = clips_environment_value(pyenv);
    s = EnvGetDefmodulePPForm(env, clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    if(!s) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    RETURN_STR(s);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_getNextDefmodule */
static char e_getNextDefmodule__doc__[] = "\
env_getNextDefmodule(env [, defmodule]) -> defmodule\n\
retrieve next defmodule object in list, first if argument is omitted\n\
returns: a defmodule object, None if already at last defmodule\n\
arguments:\n\
  defmodule (defmodule) - the defmodule to start from";
static PyObject *e_getNextDefmodule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL, *q = NULL;
    void *ptr = NULL;

    if(!PyArg_ParseTuple(args, "O!|O!",
                         &clips_EnvType, &pyenv, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    ptr = EnvGetNextDefmodule(env, p ? clips_defmodule_value(p) : NULL);
    RELEASE_MEMORY_ERROR();
    if(!ptr)
        RETURN_NONE();
    clips_defmodule_New(q);
    if(!q) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_defmodule_value(q) = ptr;
    RETURN_PYOBJECT(q);

BEGIN_FAIL
    Py_XDECREF(q);
END_FAIL
}

/* env_listDefmodules */
static char e_listDefmodules__doc__[] = "\
env_listDefmodules(env, output)\n\
list modules to logical output\n\
arguments:\n\
  output (str) - logical name of output stream";
static PyObject *e_listDefmodules(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *output = NULL;

    if(!PyArg_ParseTuple(args, "O!s", &clips_EnvType, &pyenv, &output))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvListDefmodules(env, output);
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_setCurrentModule */
static char e_setCurrentModule__doc__[] = "\
env_setCurrentModule(env, defmodule)\n\
set current module to the one specified\n\
arguments:\n\
  defmodule (defmodule) - new current defmodule";
static PyObject *e_setCurrentModule(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    clips_DefmoduleObject *p = NULL;

    if(!PyArg_ParseTuple(args, "O!O!",
                         &clips_EnvType, &pyenv, &clips_DefmoduleType, &p))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvSetCurrentModule(env, clips_defmodule_value(p));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_sendCommand [undocumented] */
static char e_sendCommand__doc__[] = "\
env_sendCommand(env, command [, verbose])\n\
send a full command to the engine as if it was typed at the prompt\n\
arguments:\n\
  command (str) - the complete command to send\n\
  verbose (bool) - if True command can produce output";
static PyObject *e_sendCommand(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    char *command = NULL;
    int res = 0, verbose = FALSE;
    PyObject *v = NULL;

    if(!PyArg_ParseTuple(args, "O!s|O",
                         &clips_EnvType, &pyenv, &command, &v))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    verbose = (v && PyObject_IsTrue(v)) ? TRUE : FALSE;
    ACQUIRE_MEMORY_ERROR();
    FlushPPBuffer(env);
    SetPPBufferStatus(env, OFF);
    RouteCommand(env, command, verbose);
    res = GetEvaluationError(env);
    FlushPPBuffer(env);
    SetHaltExecution(env, FALSE);
    SetEvaluationError(env, FALSE);
    FlushBindList(env);
    RELEASE_MEMORY_ERROR();
    if(res) {
        ERROR_CLIPS_PARSEA();
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* env_forceCleanup() [undocumented] */
static char e_forceCleanup__doc__[] = "\
env_forceCleanup(env [, alldepths, heuristics])\n\
attempt to force a garbage collection\n\
arguments:\n\
  alldepths (bool) - true to clean up all depths (default)\n\
  heuristics (bool) - true to use heuristics (default)";
static PyObject *e_forceCleanup(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;
    PyObject *alldepths = NULL, *heuristics = NULL;

    if(!PyArg_ParseTuple(args, "O!|OO",
                         &clips_EnvType, &pyenv,
                         &alldepths, &heuristics))
        FAIL();
    CHECK_NOCURENV(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    env = clips_environment_value(pyenv);
    if(EngineData(env)->ExecutingRule != NULL) {
        ERROR_CLIPSSYS_CLEANUP();
        FAIL();
    }
    ACQUIRE_MEMORY_ERROR();
    PeriodicCleanup(env,
        alldepths ? TRUE : PyObject_IsTrue(alldepths),
        heuristics ? TRUE : PyObject_IsTrue(heuristics));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* ======================================================================== */

/* 8.2 - Memory Management */

/* getConserveMemory */
STATUS_FUNC_GET_BOOL(getConserveMemory, m_getConserveMemory, GetConserveMemory)

/* memRequests */
STATUS_FUNC_GET(memRequests, m_memRequests, MemRequests, "i")

/* memUsed */
STATUS_FUNC_GET(memUsed, m_memUsed, MemUsed, "i")

/* releaseMem */
static char m_releaseMem__doc__[] = "\
releaseMem(tell [, bytes]) -> int\n\
release specified amount of memory\n\
returns: the effective amount of freed memory\n\
arguments:\n\
  tell (bool) - True to print out a message\n\
  bytes (int) - bytes to free, all if omitted";
static PyObject *m_releaseMem(PyObject *self, PyObject *args) {
    PyObject *tell = NULL;
    int b = -1;
    long b1 = 0;

    if(!PyArg_ParseTuple(args, "O|i", &tell, &b))
        FAIL();
    ACQUIRE_MEMORY_ERROR();
    b1 = ReleaseMem(b, PyObject_IsTrue(tell));
    RELEASE_MEMORY_ERROR();
    RETURN_INT(b1);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setConserveMemory */
STATUS_FUNC_SET_BOOL(setConserveMemory, m_setConserveMemory, SetConserveMemory)

/* setOutOfMemoryFunction */
UNIMPLEMENT(setOutOfMemoryFunction, m_setOutOfMemoryFunction)


/* functions related to pretty-print (and similar) buffers can be put here */
static char m_getPPBufferSize__doc__[] = "\
getPPBufferSize() -> int\n\
retrieve current size of pretty-print form buffers\n\
returns: the above said size";
static PyObject *m_getPPBufferSize(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    RETURN_INT((unsigned long)ppbuffer_size);

BEGIN_FAIL
    SKIP();
END_FAIL
}

static char m_setPPBufferSize__doc__[] = "\
setPPBufferSize(bytes)\n\
specify new size for pretty-print form buffers\n\
arguments:\n\
  size (long) - the number of bytes: it has a lower limit";
static PyObject *m_setPPBufferSize(PyObject *self, PyObject *args) {
    int l = 0;

    if(!PyArg_ParseTuple(args, "i", &l))
        FAIL();
    if(l < MIN_PPBUFFER_SIZE) {
        ERROR_VALUE("size is too small");
        FAIL();
    }
    ppbuffer_size = (size_t)l;
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}



/* ======================================================================== */

/* Standard Environment Functions */

/* number of created environments and maximum number of environments */
static int num_environments = 1;
static int max_environments = INITIAL_MAX_ENVIRONMENTS;


/* retrieve current number of enviroments allowing index based retrieval */
static char v_getNumberOfEnvironments__doc__[] = "\
getNumberOfEnvironments() -> int\n\
retrieve current number of all created environments\n\
returns: number of created environments";
static PyObject *v_getNumberOfEnvironments(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    RETURN_INT(num_environments);

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* offer a way to modify maximum number of environments */
static char v_getMaxEnvironments__doc__[] = "\
getMaxEnvironments() -> int\n\
retrieve current limit for number of environments\n\
returns: current maximum number of environments";
static PyObject *v_getMaxEnvironments(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    RETURN_INT(max_environments);

BEGIN_FAIL
    SKIP();
END_FAIL
}

static char v_setMaxEnvironments__doc__[] = "\
setMaxEnvironments(limit)\n\
specify new limit for number of environments\n\
arguments:\n\
  limit (int) - maximum number of allowed environments";
static PyObject *v_setMaxEnvironments(PyObject *self, PyObject *args) {
    int l = 0;

    if(!PyArg_ParseTuple(args, "i", &l))
        FAIL();
    if(l < num_environments) {
        ERROR_VALUE("limit is less than current amount");
        FAIL();
    }
    max_environments = l;
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* addEnvironmentCleanupFunction */
UNIMPLEMENT(addEnvironmentCleanupFunction, v_addEnvironmentCleanupFunction)

/* allocateEnvironmentData */
UNIMPLEMENT(allocateEnvironmentData, v_allocateEnvironmentData)


/* prototypes for router handling functions - see below */
int clips_env_queryFunction(void *, char *);
int clips_env_printFunction(void *, char *, char *);
int clips_env_getcFunction(void *, char *);
int clips_env_ungetcFunction(void *, int, char *);
int clips_env_exitFunction(void *, int);

/* createEnvironment */
static char v_createEnvironment__doc__[] = "\
createEnvironment() -> environment\n\
create a new CLIPS execution environment separated from current one\n\
returns: a new environment object";
static PyObject *v_createEnvironment(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *oldptr = NULL;
    void *env = NULL;
#ifdef USE_NONASSERT_CLIPSGCLOCK
    LOPTR_ITEM ***hm = NULL;
#endif /* USE_NONASSERT_CLIPSGCLOCK */

    CHECK_NOARGS(args);
    if(num_environments >= max_environments) {
        ERROR_CLIPSSYS_MAXENV();
        FAIL();
    }
    /* save current environment, we need to reset it before we are back */
    if(!(oldptr = GetCurrentEnvironment())) {
        ERROR_CLIPS_NOENV();
        FAIL();
    }
    env = CreateEnvironment();
    if(!env) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    clips_environment_New(pyenv);
#ifdef USE_NONASSERT_CLIPSGCLOCK
    if(!AllocateEnvironmentData(
       env, STRAYFACTS_DATA, sizeof(LOPTR_ITEM ***), NULL)) {
        ERROR_CLIPS_CREATION();
        FAIL();
    }
    /* STRAYFACTS_DATA will contain just a copy of the pointer to the map */
    hm = (LOPTR_ITEM ***)GetEnvironmentData(env, STRAYFACTS_DATA);
    *hm = pyenv->clips_StrayFacts;  /* no real thin ice, it was allocated */
#endif /* USE_NONASSERT_CLIPSGCLOCK */
    if(!pyenv) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    clips_environment_value(pyenv) = env;
    CHECK_VALID_ENVIRONMENT(pyenv);
    ACQUIRE_MEMORY_ERROR();
    EnvAddRouter(env, "python", 0,
                 clips_env_queryFunction,
                 clips_env_printFunction,
                 clips_env_getcFunction,
                 clips_env_ungetcFunction,
                 clips_env_exitFunction);
    EnvActivateRouter(env, "python");
    /* as said above restore environment in order to avoid problems */
    SetCurrentEnvironment(oldptr);
    /* if we are here the environment creation was successful */
    RELEASE_MEMORY_ERROR();
    num_environments++;
    RETURN_PYOBJECT(pyenv);

BEGIN_FAIL
    if(env) {
        DestroyEnvironment(env);
        SetCurrentEnvironment(oldptr);
    }
    Py_XDECREF(pyenv);
END_FAIL
}

/* destroyEnvironment */
UNIMPLEMENT(destroyEnvironment, v_destroyEnvironment)

/* getCurrentEnvironment */
static char v_getCurrentEnvironment__doc__[] = "\
getCurrentEnvironment() -> environment\n\
return current environment\n\
returns: an environment object";
static PyObject *v_getCurrentEnvironment(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    void *env = NULL;

    CHECK_NOARGS(args);
    env = GetCurrentEnvironment();
    if(!env) {
        ERROR_CLIPS_RETVAL();
        FAIL();
    }
    clips_environment_New(pyenv);
    clips_environment_value(pyenv) = env;
    INJECT_ADDITIONAL_ENVIRONMENT_DATA(pyenv);
    CHECK_VALID_ENVIRONMENT(pyenv);
    RETURN_PYOBJECT(pyenv);

BEGIN_FAIL
    Py_XDECREF(pyenv);
END_FAIL
}

/* getEnvironmentData */
UNIMPLEMENT(getEnvironmentData, v_getEnvironmentData)

/* getEnvironmentIndex */
static char v_getEnvironmentIndex__doc__[] = "\
getEnvironmentIndex(environment) -> int\n\
return unique index of specified environment\n\
returns: an integer value\n\
arguments:\n\
  environment (environment) - the environment to inspect";
static PyObject *v_getEnvironmentIndex(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;
    int i = 0;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    i = GetEnvironmentIndex(clips_environment_value(pyenv));
    RETURN_INT(i);

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setCurrentEnvironment */
static char v_setCurrentEnvironment__doc__[] = "\
setCurrentEnvironment(environment)\n\
switch to specified environment\n\
arguments:\n\
  environment (environment) - the environment to switch to";
static PyObject *v_setCurrentEnvironment(PyObject *self, PyObject *args) {
    clips_EnvObject *pyenv = NULL;

    if(!PyArg_ParseTuple(args, "O!", &clips_EnvType, &pyenv))
        FAIL();
    CHECK_VALID_ENVIRONMENT(pyenv);
    /* we cannot preserve GC counter status for current environment */
    /* RESET_ASSERTED_FACTS(); */ /* reset GC counter for old environment... */
    COPY_ADDITIONAL_ENVIRONMENT_DATA(pyenv);
    ACQUIRE_MEMORY_ERROR();
    SetCurrentEnvironment(clips_environment_value(pyenv));
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* setCurrentEnvironmentByIndex */
/* if GC locking is enabled we cannot consider this function safe */
#ifndef USE_NONASSERT_CLIPSGCLOCK
static char v_setCurrentEnvironmentByIndex__doc__[] = "\
setCurrentEnvironmentByIndex(index)\n\
switch to specified environment passing its index\n\
arguments:\n\
  index (int) - unique index of environment to switch to";
static PyObject *v_setCurrentEnvironmentByIndex(PyObject *self, PyObject *args) {
    int i = 0;

    if(!PyArg_ParseTuple(args, "i", &i)) FAIL();
    ACQUIRE_MEMORY_ERROR();
    if(!SetCurrentEnvironmentByIndex(i)) {
        RELEASE_MEMORY_ERROR();
        ERROR_CLIPS_NOTFOUND();
        FAIL();
    }
    RELEASE_MEMORY_ERROR();
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}
#else
UNIMPLEMENT(setCurrentEnvironmentByIndex, v_setCurrentEnvironmentByIndex)
#endif /* USE_NONASSERT_CLIPSGCLOCK */



/* ======================================================================== */

/* 7 - I/O Routers
 *  Now, we have to deal with it somehow.
 *  I/O Routers will probably be not easy to implement in Python, and there
 *  is the strong need for ideas. One method could be the following:
 *  - we build some buffers of variable length, which can be written and
 *    read using the functions required by CLIPS
 *  - we store them in Python objects, and in a Python structure (which will
 *    only be accessed through C functions, thus invisible to the user), and
 *    let Python manage all the memory for it - except for buffer destruction
 *  At the end of this, we will have the standard CLIPS streams connected to
 *  as many Python file-like objects, and the user can send input and read
 *  output to/from these, thus being able to use CLIPS just as a pipe.
 */


/* treat an I/O buffer (a file-like behaving object) as a Python object */
staticforward PyTypeObject buffer_Type;

typedef struct {
    PyObject_HEAD
    char *name;
    char *buffer;
    char *readptr;
    size_t size;
    BOOL py_readonly;   /* this flag is only used by Python interface */
} buffer_Object;

#define buffer_Check(v) ((v)->ob_type == &buffer_Type)
#define buffer_Name(v) (((buffer_Object *)(v))->name)
#define buffer_Buffer(v) (((buffer_Object *)(v))->buffer)
#define buffer_Readptr(v) (((buffer_Object *)(v))->readptr)
#define buffer_Size(v) (((buffer_Object *)(v))->size)
#define buffer_Readonly(v) (((buffer_Object *)(v))->py_readonly)

/* allocate a named initial buffer with size 0 */
static buffer_Object *buffer_create(const char *name, BOOL readonly) {
    buffer_Object *p = PyObject_New(buffer_Object, &buffer_Type);
    size_t namelen = strlen(name);

    buffer_Name(p) = NEW_ARRAY(char, namelen + 1);
    if(!p)
        return NULL;
    strncpy(buffer_Name(p), name, namelen);
    buffer_Name(p)[namelen] = '\0';
    buffer_Size(p) = 0;
    buffer_Buffer(p) = NULL;
    buffer_Readptr(p) = NULL;
    buffer_Readonly(p) = readonly;
    return p;
}

/* destroy all memory related to the buffer */
static void buffer_destroy(buffer_Object *o) {
    if(buffer_Buffer(o) != NULL)
        DELETE(buffer_Buffer(o));
    if(buffer_Name(o) != NULL)
        DELETE(buffer_Name(o));
    PyObject_Del(o);
}

/* same as above, with a cast to delete from Python interpreter */
static void buffer_dealloc(PyObject *o) {
    buffer_destroy((buffer_Object *)o);
}

/* clear the buffer */
static void buffer_clear(buffer_Object *o) {
    if(buffer_Buffer(o) != NULL)
        DELETE(buffer_Buffer(o));
    buffer_Size(o) = 0;
    buffer_Buffer(o) = NULL;
    buffer_Readptr(o) = NULL;
}

/* append a string to the buffer */
static BOOL buffer_append(buffer_Object *o, const char *str) {
    char *newbuffer = NULL; /* should be the result of a realloc() */
    size_t len = strlen(str);
    size_t readindex = 0;
    BOOL rv = FALSE, usedbuffer = buffer_Buffer(o) ? TRUE : FALSE;

    /* in this case realloc() may behave as malloc() */
    newbuffer = (char *)REALLOC(buffer_Buffer(o), buffer_Size(o) + len + 1);
    if(!usedbuffer && newbuffer)
        newbuffer[0] = '\0';
    if(newbuffer != NULL) {
        strncat(newbuffer, str, len);
        buffer_Size(o) += len;
        newbuffer[buffer_Size(o)] = '\0';
        if(!buffer_Readptr(o))
            buffer_Readptr(o) = newbuffer;
        else {
            readindex = (size_t)(buffer_Readptr(o) - buffer_Buffer(o));
            buffer_Readptr(o) = (char *)(newbuffer + readindex);
        }
        buffer_Buffer(o) = newbuffer;   /* could have changed */
        rv = TRUE;
    }
    return rv;
}

/* pick first n characters from string: out must be already allocated */
static BOOL buffer_head(const buffer_Object *o, char *out, size_t n) {
    strncpy(out, buffer_Buffer(o), n);
    out[n] = '\0';
    return TRUE;
}

/* remove first n characters from buffer */
static BOOL buffer_remove(buffer_Object *o, size_t n) {
    char *newbuffer = NULL;
    size_t readindex = 0, newsize = 0;

    if(n > buffer_Size(o) || buffer_Size(o) == 0) return FALSE;
    else {
        newsize = buffer_Size(o) - n;
        readindex = (size_t)(buffer_Readptr(o) - buffer_Buffer(o)) - n;
        newbuffer = NEW_ARRAY(char, newsize + 1);
        if(newbuffer != NULL) {
            strncpy(newbuffer,
                (char *)(buffer_Buffer(o) + n), buffer_Size(o) - n);
            DELETE(buffer_Buffer(o));
            buffer_Size(o) = newsize;
            buffer_Buffer(o) = newbuffer;
            buffer_Buffer(o)[newsize] = '\0';
            if(readindex < 0)
                readindex = 0;
            buffer_Readptr(o) = (char *)(buffer_Buffer(o) + readindex);
            return TRUE;
        }
        return FALSE;
    }
}

/* find the first occurrence of a character in buffer as index */
static long buffer_chrindex(const buffer_Object *o, int c) {
    char *ptr = NULL;

    if(buffer_Size(o) == 0) return -1;
    else {
        ptr = strchr(buffer_Buffer(o), c);
        if(!ptr)
            return -1;
        else
            return (long)(ptr - buffer_Buffer(o));
    }
}

/* putchar function */ /* NOT USED */ /*
static BOOL buffer_putchar(buffer_Object *o, int c) {
    char fakestr[2] = { (char)c, 0 };

    return buffer_append(o, fakestr);
}
*/

/* getchar function */
static int buffer_getchar(buffer_Object *o) {
    int c = 0;

    if((size_t)(buffer_Readptr(o) - buffer_Buffer(o)) < buffer_Size(o)) {
        c = (int)(*buffer_Readptr(o));
        buffer_Readptr(o)++;
        return c;
    } else return -1;
}

/* ungetchar function */
static int buffer_ungetchar(buffer_Object *o, int c) {
    char *ptr = (char *)(buffer_Readptr(o) - 1);

    if(ptr[0] == (char)c && ptr >= buffer_Buffer(o)) {
        buffer_Readptr(o)--;
        return c;
    } else return -1;
}

/* the Python buffer Type */
static PyTypeObject buffer_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "streambuffer",
    sizeof(buffer_Object),
    0,
    buffer_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};


/* stream dictionary handlers */
static BOOL bufdict_Add(char *name, BOOL readonly) {
    buffer_Object *o = NULL;

    o = buffer_create(name, readonly);
    if(!o)
        return FALSE;
    else {
        PyDict_SetItemString((PyObject *)clips_Streams, name, (PyObject *)o);
        return TRUE;
    }
}

static buffer_Object *bufdict_Get(char *name) {
    PyObject *o = PyDict_GetItemString((PyObject *)clips_Streams, name);
    if(o != NULL && buffer_Check(o))
        return (buffer_Object *)o;
    else return NULL;
}

static BOOL bufdict_Remove(char *name) {
    PyObject *o = NULL;

    if(!(o = PyDict_GetItemString((PyObject *)clips_Streams, name)))
        return FALSE;
    if(PyDict_DelItemString((PyObject *)clips_Streams, name) == -1)
        return FALSE;
    else {
        /* we know that we are the last ones to use this */
        buffer_clear((buffer_Object *)o);
        Py_DECREF(o);
        return TRUE;
    }
}

/* clips router helpers (page 161..163 of apg) */
int clips_queryFunction(char *logicalName) {
    return bufdict_Get(logicalName) != NULL ? TRUE : FALSE;
}

int clips_printFunction(char *logicalName, char *str) {
    buffer_Object *o = bufdict_Get(logicalName);

    if(o != NULL) {
        if(!buffer_append(o, str))
            return FALSE;
        else return TRUE;
    } else return FALSE;
}

int clips_getcFunction(char *logicalName) {
    buffer_Object *o = bufdict_Get(logicalName);
    int c = 0;

    if(o != NULL) {
        c = buffer_getchar(o);
        if(c < 0)
            return EOF;
        else return c;
    } else return EOF;
}

int clips_ungetcFunction(int ch, char *logicalName) {
    buffer_Object *o = bufdict_Get(logicalName);
    int c = 0;

    if(o != NULL) {
        c = buffer_ungetchar(o, ch);
        if(c < 0)
            return EOF;
        else return c;
    } else return EOF;
}

int clips_exitFunction(int exitCode) {
    return 0;
}


/* clips router helpers [environmental] (page 161..163 of apg) */
int clips_env_queryFunction(void *env, char *logicalName) {
    return bufdict_Get(logicalName) != NULL ? TRUE : FALSE;
}

int clips_env_printFunction(void *env, char *logicalName, char *str) {
    buffer_Object *o = bufdict_Get(logicalName);

    if(o != NULL) {
        if(!buffer_append(o, str))
            return FALSE;
        else return TRUE;
    } else return FALSE;
}

int clips_env_getcFunction(void *env, char *logicalName) {
    buffer_Object *o = bufdict_Get(logicalName);
    int c = 0;

    if(o != NULL) {
        c = buffer_getchar(o);
        if(c < 0)
            return EOF;
        else return c;
    } else return EOF;
}

int clips_env_ungetcFunction(void *env, int ch, char *logicalName) {
    buffer_Object *o = bufdict_Get(logicalName);
    int c = 0;

    if(o != NULL) {
        c = buffer_ungetchar(o, ch);
        if(c < 0)
            return EOF;
        else return c;
    } else return EOF;
}

int clips_env_exitFunction(void *env, int exitCode) {
    return 0;
}


/* Python router interface functions */

/* routerWrite - write to Python router */
static char g_routerWrite__doc__[] = "\
routerWrite(streamname, string)\n\
write a string to logical stream\n\
arguments:\n\
  streamname (str) - the name of logical stream\n\
  string (str) - data to write";
static PyObject *g_routerWrite(PyObject *self, PyObject *args) {
    buffer_Object *o = NULL;
    char *name = NULL, *str = NULL;

    if(!PyArg_ParseTuple(args, "ss", &name, &str))
        FAIL();
    o = bufdict_Get(name);
    if(!o) {
        ERROR_ROUTER_NOSTREAM();
        FAIL();
    }
    if(buffer_Readonly(o)) {
        ERROR_ROUTER_READONLY();
        FAIL();
    }
    if(!buffer_append(o, str)) {
        ERROR_ROUTER_INVALID();
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* routerClear - clear Python router */
static char g_routerClear__doc__[] = "\
routerClear(streamname)\n\
clear logical stream\n\
arguments:\n\
  streamname (str) - the name of logical stream";
static PyObject *g_routerClear(PyObject *self, PyObject *args) {
    buffer_Object *o = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    o = bufdict_Get(name);
    if(!o) {
        ERROR_ROUTER_NOSTREAM();
        FAIL();
    }
    buffer_clear(o);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* routerRead - read from Python router */
static char g_routerRead__doc__[] = "\
routerRead(streamname)\n\
read from logical stream\n\
arguments:\n\
  streamname (str) - the name of logical stream";
static PyObject *g_routerRead(PyObject *self, PyObject *args) {
    buffer_Object *o = NULL;
    PyObject *p = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    o = bufdict_Get(name);
    if(!o) {
        ERROR_ROUTER_NOSTREAM();
        FAIL();
    }
    p = Py_BuildValue("s", buffer_Buffer(o));
    if(!p) {
        ERROR_MEMORY_CREATION();
        FAIL();
    }
    buffer_clear(o);
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    Py_XDECREF(p);
END_FAIL
}

/* routerReadline - read a line from Python router */
static char g_routerReadline__doc__[] = "\
routerReadline(streamname)\n\
read a single text line from logical stream\n\
arguments:\n\
  streamname (str) - the name of logical stream";
static PyObject *g_routerReadline(PyObject *self, PyObject *args) {
    buffer_Object *o = NULL;
    PyObject *p = NULL;
    char *name = NULL, *tmp = NULL;
    int n = 0;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    o = bufdict_Get(name);
    if(!o) {
        ERROR_ROUTER_NOSTREAM();
        FAIL();
    }

    n = buffer_chrindex(o, '\n');
    if(n < 0) {
        p = Py_BuildValue("s", buffer_Buffer(o));
        buffer_clear(o);
        if(!p) {
            ERROR_MEMORY_CREATION();
            FAIL();
        }
    } else {
        tmp = NEW_ARRAY(char, n + 2);
        if(!tmp || !buffer_head(o, tmp, n + 1)) {
            ERROR_MEMORY_CREATION();
            FAIL();
        }
        p = Py_BuildValue("s", tmp);
        if(!p) {
            ERROR_MEMORY_CREATION();
            FAIL();
        }
        DELETE(tmp);
        buffer_remove(o, n);
    }
    RETURN_PYOBJECT(p);

BEGIN_FAIL
    if(tmp != NULL)
        DELETE(tmp);
    Py_XDECREF(p);
END_FAIL
}



/* ======================================================================== */

/* external functions definition system */

/* when FALSE, do not print tracebacks from within CLIPS */
static BOOL clips_EnableExternalTraceback = FALSE;


/* we allow only alphanumerics and hyphens for function names (sorry...) */
static BOOL checkFuncName(char *name) {
    if(isalpha(*name) || *name == '_') name++;
    while(*name) {
        if(isalnum(*name) || *name == '-' || *name == '_')
            name++;
        else return FALSE;
    }
    return TRUE;
}

/* addPythonFunction - register a new Python function for CLIPS */
static char f_addPythonFunction__doc__[] = "\
addPythonFunction(name, function)\n\
add a Python external function to call from CLIPS: the passed in function\n\
should expect arguments as (type, value) pairs and return such a pair\n\
arguments:\n\
  name (str) - name that the function will have in CLIPS\n\
  function (callable) - the function to be called from within CLIPS";
static PyObject *f_addPythonFunction(PyObject *self, PyObject *args) {
    PyObject *pfunc = NULL;
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "sO", &name, &pfunc))
        FAIL();
    if (!PyCallable_Check(pfunc)) {
        ERROR_TYPE("callable expected as second argument");
        FAIL();
    }
    if(!checkFuncName(name)) {
        ERROR_VALUE("invalid function name");
        FAIL();
    }
    if(PyDict_SetItemString((PyObject *)clips_PythonFunctions,
                            name, (PyObject *)pfunc)) {
        ERROR_CLIPS_OTHER("could not register external function");
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* removePythonFunction - deregister a Python function from CLIPS */
static char f_removePythonFunction__doc__[] = "\
removePythonFunction(name)\n\
remove a previously registered external Python function\n\
arguments:\n\
  name (str) - name that the function has in CLIPS";
static PyObject *f_removePythonFunction(PyObject *self, PyObject *args) {
    char *name = NULL;

    if(!PyArg_ParseTuple(args, "s", &name))
        FAIL();
    if(PyDict_DelItemString((PyObject *)clips_PythonFunctions, name)) {
        ERROR_CLIPS_OTHER("could not remove external function");
        FAIL();
    }
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

/* clearPythonFunctions - deregister all registered Python functions */
static char f_clearPythonFunctions__doc__[] = "\
clearPythonFunctions()\n\
remove all previously registered external Python functions";
static PyObject *f_clearPythonFunctions(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    PyDict_Clear((PyObject *)clips_PythonFunctions);
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* allow/disallow printing a trace on bad functions or exceptions */
static char f_setPrintExternalTraceback__doc__[] = "\
setPrintExternalTraceback(bool)\n\
allow or disallow printing a standard traceback on calls within CLIPS\n\
arguments:\n\
  bool (bool) - allow when True and disallow otherwise";
static PyObject *f_setPrintExternalTraceback(PyObject *self, PyObject *args) {
    PyObject *bval = NULL;

    if(!PyArg_ParseTuple(args, "O", &bval))
        FAIL();
    clips_EnableExternalTraceback = PyObject_IsTrue(bval) ? TRUE : FALSE;
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

static char f_getPrintExternalTraceback__doc__[] = "\
getPrintExternalTraceback() -> bool\n\
report whether printing a traceback from within CLIPS is allowed or not";
static PyObject *f_getPrintExternalTraceback(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    return Py_BuildValue("i", clips_EnableExternalTraceback ? 1 : 0);

BEGIN_FAIL
    SKIP();
END_FAIL
}


/* functions that print an error to CLIPS */
static void InvalidFunctionError(void *env, char *name) {
    PrintErrorID(env, "PYTHONFN", 1, TRUE);
    EnvPrintRouter(env, WERROR, "External function ");
    EnvPrintRouter(env, WERROR, name);
    EnvPrintRouter(env, WERROR, " not found.\n");
}
static void FunctionExceptionError(void *env, char *name) {
    PrintErrorID(env, "PYTHONXC", 1, TRUE);
    EnvPrintRouter(env, WERROR, "Call to function ");
    EnvPrintRouter(env, WERROR, name);
    EnvPrintRouter(env, WERROR, " failed.\n");
}

/* the following is the function that is actually called by CLIPS
 * declared as
 * EnvDefineFunction2("python-call", 'u', PTIF EnvPythonExternalCall,
 *                    "EnvPythonExternalCall", "1*uw");
 */
/* some notes about this:
 *  this function will be called with an undefined number of arguments by
 *  CLIPS, and will store its return value in an unknown DATA_OBJECT; when
 *  the Python call fails, it will return the symbol FALSE after sending
 *  an error message to the 'werror' output router, as all functions in
 *  CLIPS should return a value also in case of error
 */
#define ECLIPS_RETURN_FALSE(_e, _v) do { \
    SetpType(_v, SYMBOL); SetpValue(_v, ECLIPS_FALSE_SYMBOL(_e)); return; \
    }  while(0)
void EnvPythonExternalCall(void *env, DATA_OBJECT_PTR retval) {
    char *funcname = NULL;
    int argcnt = 0, i = 0;
    DATA_OBJECT arg;
    PyObject *to_call = NULL, *result = NULL, *o = NULL;
    PyTupleObject *pyargs = NULL;

    if(EnvArgCountCheck(env, "python-call", AT_LEAST, 1) < 0)
        ECLIPS_RETURN_FALSE(env, retval);
    if(EnvArgTypeCheck(env, "python-call", 1, SYMBOL, &arg) < 0)
        ECLIPS_RETURN_FALSE(env, retval);
    argcnt = EnvRtnArgCount(env);
    /* now arg contains the function name */
    funcname = DOToString(arg);
    if(!(to_call = PyDict_GetItemString((PyObject *)clips_PythonFunctions,
                                        funcname))) {
        InvalidFunctionError(env, funcname);
        if(clips_EnableExternalTraceback) {
            ERROR_VALUE("invalid function name");
            PyErr_Print();
        }
        SetEvaluationError(env, TRUE);
        SetHaltExecution(env, TRUE);
        ECLIPS_RETURN_FALSE(env, retval);
    }
    pyargs = (PyTupleObject *)PyTuple_New(argcnt - 1);
    if(!pyargs) {
        FunctionExceptionError(env, funcname);
        if(clips_EnableExternalTraceback) {
            ERROR_MEMORY("can not pass parameters");
            PyErr_Print();
        }
        SetEvaluationError(env, TRUE);
        SetHaltExecution(env, TRUE);
        ECLIPS_RETURN_FALSE(env, retval);
    }
    for(i = 2; i <= argcnt; i++) {
        EnvRtnUnknown(env, i, &arg);
        o = i_do2py_e(env, &arg);
        if(!o) {
            Py_DECREF(pyargs);
            FunctionExceptionError(env, funcname);
            if(clips_EnableExternalTraceback) {
                ERROR_VALUE("can not convert parameters");
                PyErr_Print();
            }
            SetEvaluationError(env, TRUE);
            SetHaltExecution(env, TRUE);
            ECLIPS_RETURN_FALSE(env, retval);
        }
        PyTuple_SetItem((PyObject *)pyargs, i - 2, o);
    }
    result = PyEval_CallObject(to_call, (PyObject *)pyargs);
    Py_DECREF(pyargs);
    if(!result) {
        FunctionExceptionError(env, funcname);
        if(PyErr_Occurred()) {
            if(clips_EnableExternalTraceback)
                PyErr_Print();
            else PyErr_Clear();
            SetEvaluationError(env, TRUE);
            SetHaltExecution(env, TRUE);
            ECLIPS_RETURN_FALSE(env, retval);
        }
    } else {
        i_py2do_e(env, result, retval);
        Py_DECREF(result);
    }
}

void PythonExternalCall(DATA_OBJECT_PTR retval) {
    EnvPythonExternalCall(GetCurrentEnvironment(), retval);
}



/* ======================================================================== */

/* specific utilities */

/* test for types, for we cannot expose type objects as objects */
#define TYPE_TEST_FUNC(func, type) \
    static char func##__doc__[] = "type test function"; \
    static PyObject *func(PyObject *self, PyObject *args) { \
        PyObject *p = NULL; \
        if(!PyArg_ParseTuple(args, "O", &p)) \
            FAIL(); \
        return Py_BuildValue("i", PyObject_TypeCheck(p, &type)); \
        BEGIN_FAIL \
        END_FAIL \
    }

TYPE_TEST_FUNC(g_isEnvironment, clips_EnvType)
TYPE_TEST_FUNC(g_isDeftemplate, clips_DeftemplType)
TYPE_TEST_FUNC(g_isFact, clips_FactType)
TYPE_TEST_FUNC(g_isInstance, clips_InstanceType)
TYPE_TEST_FUNC(g_isDefmodule, clips_DefmoduleType)
TYPE_TEST_FUNC(g_isDeffacts, clips_DeffactsType)
TYPE_TEST_FUNC(g_isDefrule, clips_DefruleType)
TYPE_TEST_FUNC(g_isActivation, clips_ActivationType)
TYPE_TEST_FUNC(g_isDefglobal, clips_DefglobalType)
TYPE_TEST_FUNC(g_isDeffunction, clips_DeffunctionType)
TYPE_TEST_FUNC(g_isDefgeneric, clips_DefgenericType)
TYPE_TEST_FUNC(g_isDefmethod, clips_DefmethodType)
TYPE_TEST_FUNC(g_isDefclass, clips_DefclassType)
TYPE_TEST_FUNC(g_isDefinstances, clips_DefinstancesType)


/* set/reset suppression of fatal environment messages on stderr (stdout) */
static char g_setEnableEnvironmentFatalMessages__doc__[] = "\
setEnableEnvironmentFatalMessages(bool)\n\
allow or disallow printing fatal CLIPS environment error messages\n\
arguments:\n\
  bool (bool) - allow when True and disallow otherwise";
static PyObject *g_setEnableEnvironmentFatalMessages(PyObject *self, PyObject *args) {
    PyObject *bval = NULL;

    if(!PyArg_ParseTuple(args, "O", &bval))
        FAIL();
    clips_ShowEnvironmentFatalErrors = PyObject_IsTrue(bval) ? TRUE : FALSE;
    RETURN_NONE();

BEGIN_FAIL
    SKIP();
END_FAIL
}

static char g_getEnableEnvironmentFatalMessages__doc__[] = "\
getEnableEnvironmentFatalMessages() -> bool\n\
report whether printing fatal CLIPS environment errors is allowed or not";
static PyObject *g_getEnableEnvironmentFatalMessages(PyObject *self, PyObject *args) {
    CHECK_NOARGS(args);
    RETURN_BOOL(clips_ShowEnvironmentFatalErrors);

BEGIN_FAIL
    SKIP();
END_FAIL
}



/* ======================================================================== */

#ifdef USE_TEST_DEALLOC_ENV
/* this is a stupid hack to try to force the module to call the environment
 * data deallocation function once it's unloaded, in order to ask the CLIPS
 * engine to release all its allocated memory; it relies on an object that
 * is only created once at module initialization and can never be created by
 * a module user; it is also not guaranteed to function asexpected
 */

staticforward PyTypeObject guard_Type;

typedef struct {
    PyObject_HEAD
} guard_Object;


/* guard destruction: as a "side effect" perform module finalization */
static void guard_dealloc(PyObject *o) {
    /* this is a good place to perform every kind of unhandled cleanup */

    /* destroy all open CLIPS I/O buffers */
    bufdict_Remove("t");
    bufdict_Remove("stdin");
    bufdict_Remove("stdout");
    bufdict_Remove("wprompt");
    bufdict_Remove("wdialog");
    bufdict_Remove("wdisplay");
    bufdict_Remove("werror");
    bufdict_Remove("wwarning");
    bufdict_Remove("wtrace");
    bufdict_Remove("temporary");

    /* invoke CLIPS specific all environments destruction */
    DeallocateEnvironmentData();

    /* and only finally destroy the object itself */
    PyObject_Del(o);

    /* other things like removal of external functions are not needed here */
}


/* the Python internal guard Type */
static PyTypeObject guard_Type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "__PyCLIPS_$iGuardType__",  /* the name is intentionally unusable */
    sizeof(guard_Object),
    0,
    guard_dealloc, /*tp_dealloc*/
    0,          /*tp_print*/
    0,          /*tp_getattr*/
    0,          /*tp_setattr*/
    0,          /*tp_compare*/
    0,          /*tp_repr*/
    0,          /*tp_as_number*/
    0,          /*tp_as_sequence*/
    0,          /*tp_as_mapping*/
    0,          /*tp_hash */
};

/* also prepare an object living in module namespace with an unusable name */
#define PREPARE_DEALLOC_ENV() guard_Object *_ig = NULL;
#define INSTALL_DEALLOC_ENV(_m) do { \
        guard_Type.ob_type = &PyType_Type; \
        _ig = PyObject_New(guard_Object, &guard_Type); \
        PyModule_AddObject(_m, "__PyCLIPS_$iGuardObject__", (PyObject *)_ig); \
    } while(0)

#else

#define PREPARE_DEALLOC_ENV()
#define INSTALL_DEALLOC_ENV()

#endif /* USE_TEST_DEALLOC_ENV */

/* ======================================================================== */

/* global methods map */
static PyMethodDef g_methods[] = {
    MMAP_ENTRY(addClearFunction, g_addClearFunction),
    MMAP_ENTRY(addPeriodicFunction, g_addPeriodicFunction),
    MMAP_ENTRY(addResetFunction, g_addResetFunction),
    MMAP_ENTRY(bload, g_bload),
    MMAP_ENTRY(bsave, g_bsave),
    MMAP_ENTRY(clear, g_clear),
    MMAP_ENTRY(functionCall, g_functionCall),
    MMAP_ENTRY(getAutoFloatDividend, g_getAutoFloatDividend),
    MMAP_ENTRY(getDynamicConstraintChecking, g_getDynamicConstraintChecking),
    MMAP_ENTRY(getSequenceOperatorRecognition, g_getSequenceOperatorRecognition),
    MMAP_ENTRY(getStaticConstraintChecking, g_getStaticConstraintChecking),
    MMAP_ENTRY(load, g_load),
    MMAP_ENTRY(reset, g_reset),
    MMAP_ENTRY(save, g_save),
    MMAP_ENTRY(setAutoFloatDividend, g_setAutoFloatDividend),
    MMAP_ENTRY(setDynamicConstraintChecking, g_setDynamicConstraintChecking),
    MMAP_ENTRY(setSequenceOperatorRecognition, g_setSequenceOperatorRecognition),
    MMAP_ENTRY(setStaticConstraintChecking, g_setStaticConstraintChecking),
    MMAP_ENTRY(batchStar, g_batchStar),
    MMAP_ENTRY(build, g_build),
    MMAP_ENTRY(eval, g_eval),
    MMAP_ENTRY(dribbleActive, g_dribbleActive),
    MMAP_ENTRY(dribbleOff, g_dribbleOff),
    MMAP_ENTRY(dribbleOn, g_dribbleOn),
    MMAP_ENTRY(getWatchItem, g_getWatchItem),
    MMAP_ENTRY(unwatch, g_unwatch),
    MMAP_ENTRY(watch, g_watch),
    MMAP_ENTRY(deftemplateModule, g_deftemplateModule),
    MMAP_ENTRY(deftemplateSlotAllowedValues, g_deftemplateSlotAllowedValues),
    MMAP_ENTRY(deftemplateSlotCardinality, g_deftemplateSlotCardinality),
    MMAP_ENTRY(deftemplateSlotDefaultP, g_deftemplateSlotDefaultP),
    MMAP_ENTRY(deftemplateSlotDefaultValue, g_deftemplateSlotDefaultValue),
    MMAP_ENTRY(deftemplateSlotExistP, g_deftemplateSlotExistP),
    MMAP_ENTRY(deftemplateSlotMultiP, g_deftemplateSlotMultiP),
    MMAP_ENTRY(deftemplateSlotNames, g_deftemplateSlotNames),
    MMAP_ENTRY(deftemplateSlotRange, g_deftemplateSlotRange),
    MMAP_ENTRY(deftemplateSlotSingleP, g_deftemplateSlotSingleP),
    MMAP_ENTRY(deftemplateSlotTypes, g_deftemplateSlotTypes),
    MMAP_ENTRY(findDeftemplate, g_findDeftemplate),
    MMAP_ENTRY(getDeftemplateList, g_getDeftemplateList),
    MMAP_ENTRY(getDeftemplateName, g_getDeftemplateName),
    MMAP_ENTRY(getDeftemplatePPForm, g_getDeftemplatePPForm),
    MMAP_ENTRY(getDeftemplateWatch, g_getDeftemplateWatch),
    MMAP_ENTRY(getNextDeftemplate, g_getNextDeftemplate),
    MMAP_ENTRY(isDeftemplateDeletable, g_isDeftemplateDeletable),
    MMAP_ENTRY(listDeftemplates, g_listDeftemplates),
    MMAP_ENTRY(setDeftemplateWatch, g_setDeftemplateWatch),
    MMAP_ENTRY(undeftemplate, g_undeftemplate),
    MMAP_ENTRY(assertFact, g_assertFact),
    MMAP_ENTRY(assertString, g_assertString),
    MMAP_ENTRY(assignFactSlotDefaults, g_assignFactSlotDefaults),
    MMAP_ENTRY(createFact, g_createFact),
    MMAP_ENTRY(decrementFactCount, g_decrementFactCount),
    MMAP_ENTRY(factIndex, g_factIndex),
    MMAP_ENTRY(facts, g_facts),
    MMAP_ENTRY(getFactDuplication, g_getFactDuplication),
    MMAP_ENTRY(getFactListChanged, g_getFactListChanged),
    MMAP_ENTRY(getFactPPForm, g_getFactPPForm),
    MMAP_ENTRY(getFactSlot, g_getFactSlot),
    MMAP_ENTRY(getNextFact, g_getNextFact),
    MMAP_ENTRY(getNextFactInTemplate, g_getNextFactInTemplate),
    MMAP_ENTRY(incrementFactCount, g_incrementFactCount),
    MMAP_ENTRY(loadFacts, g_loadFacts),
    MMAP_ENTRY(ppFact, g_ppFact),
    MMAP_ENTRY(putFactSlot, g_putFactSlot),
    MMAP_ENTRY(retract, g_retract),
    MMAP_ENTRY(saveFacts, g_saveFacts),
    MMAP_ENTRY(setFactDuplication, g_setFactDuplication),
    MMAP_ENTRY(setFactListChanged, g_setFactListChanged),
    MMAP_ENTRY(factDeftemplate, g_factDeftemplate),
    MMAP_ENTRY(factExistp, g_factExistp),
    MMAP_ENTRY(factSlotNames, g_factSlotNames),
    MMAP_ENTRY(getFactList, g_getFactList),
    MMAP_ENTRY(loadFactsFromString, g_loadFactsFromString),
    MMAP_ENTRY(deffactsModule, g_deffactsModule),
    MMAP_ENTRY(findDeffacts, g_findDeffacts),
    MMAP_ENTRY(getDeffactsList, g_getDeffactsList),
    MMAP_ENTRY(getDeffactsName, g_getDeffactsName),
    MMAP_ENTRY(getDeffactsPPForm, g_getDeffactsPPForm),
    MMAP_ENTRY(getNextDeffacts, g_getNextDeffacts),
    MMAP_ENTRY(isDeffactsDeletable, g_isDeffactsDeletable),
    MMAP_ENTRY(listDeffacts, g_listDeffacts),
    MMAP_ENTRY(undeffacts, g_undeffacts),
    MMAP_ENTRY(defruleHasBreakpoint, g_defruleHasBreakpoint),
    MMAP_ENTRY(defruleModule, g_defruleModule),
    MMAP_ENTRY(findDefrule, g_findDefrule),
    MMAP_ENTRY(getDefruleList, g_getDefruleList),
    MMAP_ENTRY(getDefruleName, g_getDefruleName),
    MMAP_ENTRY(getDefrulePPForm, g_getDefrulePPForm),
    MMAP_ENTRY(getDefruleWatchActivations, g_getDefruleWatchActivations),
    MMAP_ENTRY(getDefruleWatchFirings, g_getDefruleWatchFirings),
    MMAP_ENTRY(getIncrementalReset, g_getIncrementalReset),
    MMAP_ENTRY(getNextDefrule, g_getNextDefrule),
    MMAP_ENTRY(isDefruleDeletable, g_isDefruleDeletable),
    MMAP_ENTRY(listDefrules, g_listDefrules),
    MMAP_ENTRY(matches, g_matches),
    MMAP_ENTRY(refresh, g_refresh),
    MMAP_ENTRY(removeBreak, g_removeBreak),
    MMAP_ENTRY(setBreak, g_setBreak),
    MMAP_ENTRY(setDefruleWatchActivations, g_setDefruleWatchActivations),
    MMAP_ENTRY(setDefruleWatchFirings, g_setDefruleWatchFirings),
    MMAP_ENTRY(setIncrementalReset, g_setIncrementalReset),
    MMAP_ENTRY(showBreaks, g_showBreaks),
    MMAP_ENTRY(undefrule, g_undefrule),
    MMAP_ENTRY(addRunFunction, g_addRunFunction),
    MMAP_ENTRY(agenda, g_agenda),
    MMAP_ENTRY(clearFocusStack, g_clearFocusStack),
    MMAP_ENTRY(deleteActivation, g_deleteActivation),
    MMAP_ENTRY(focus, g_focus),
    MMAP_ENTRY(getActivationName, g_getActivationName),
    MMAP_ENTRY(getActivationPPForm, g_getActivationPPForm),
    MMAP_ENTRY(getActivationSalience, g_getActivationSalience),
    MMAP_ENTRY(getAgendaChanged, g_getAgendaChanged),
    MMAP_ENTRY(getFocus, g_getFocus),
    MMAP_ENTRY(getFocusStack, g_getFocusStack),
    MMAP_ENTRY(getNextActivation, g_getNextActivation),
    MMAP_ENTRY(getSalienceEvaluation, g_getSalienceEvaluation),
    MMAP_ENTRY(getStrategy, g_getStrategy),
    MMAP_ENTRY(listFocusStack, g_listFocusStack),
    MMAP_ENTRY(popFocus, g_popFocus),
    MMAP_ENTRY(refreshAgenda, g_refreshAgenda),
    MMAP_ENTRY(removeRunFunction, g_removeRunFunction),
    MMAP_ENTRY(reorderAgenda, g_reorderAgenda),
    MMAP_ENTRY(run, g_run),
    MMAP_ENTRY(setActivationSalience, g_setActivationSalience),
    MMAP_ENTRY(setAgendaChanged, g_setAgendaChanged),
    MMAP_ENTRY(setSalienceEvaluation, g_setSalienceEvaluation),
    MMAP_ENTRY(setStrategy, g_setStrategy),
    MMAP_ENTRY(defglobalModule, g_defglobalModule),
    MMAP_ENTRY(findDefglobal, g_findDefglobal),
    MMAP_ENTRY(getDefglobalList, g_getDefglobalList),
    MMAP_ENTRY(getDefglobalName, g_getDefglobalName),
    MMAP_ENTRY(getDefglobalPPForm, g_getDefglobalPPForm),
    MMAP_ENTRY(getDefglobalValue, g_getDefglobalValue),
    MMAP_ENTRY(getDefglobalValueForm, g_getDefglobalValueForm),
    MMAP_ENTRY(getDefglobalWatch, g_getDefglobalWatch),
    MMAP_ENTRY(getGlobalsChanged, g_getGlobalsChanged),
    MMAP_ENTRY(getNextDefglobal, g_getNextDefglobal),
    MMAP_ENTRY(getResetGlobals, g_getResetGlobals),
    MMAP_ENTRY(isDefglobalDeletable, g_isDefglobalDeletable),
    MMAP_ENTRY(listDefglobals, g_listDefglobals),
    MMAP_ENTRY(setDefglobalValue, g_setDefglobalValue),
    MMAP_ENTRY(setDefglobalWatch, g_setDefglobalWatch),
    MMAP_ENTRY(setGlobalsChanged, g_setGlobalsChanged),
    MMAP_ENTRY(setResetGlobals, g_setResetGlobals),
    MMAP_ENTRY(showDefglobals, g_showDefglobals),
    MMAP_ENTRY(undefglobal, g_undefglobal),
    MMAP_ENTRY(deffunctionModule, g_deffunctionModule),
    MMAP_ENTRY(findDeffunction, g_findDeffunction),
    MMAP_ENTRY(getDeffunctionList, g_getDeffunctionList),
    MMAP_ENTRY(getDeffunctionName, g_getDeffunctionName),
    MMAP_ENTRY(getDeffunctionPPForm, g_getDeffunctionPPForm),
    MMAP_ENTRY(getDeffunctionWatch, g_getDeffunctionWatch),
    MMAP_ENTRY(getNextDeffunction, g_getNextDeffunction),
    MMAP_ENTRY(isDeffunctionDeletable, g_isDeffunctionDeletable),
    MMAP_ENTRY(listDeffunctions, g_listDeffunctions),
    MMAP_ENTRY(setDeffunctionWatch, g_setDeffunctionWatch),
    MMAP_ENTRY(undeffunction, g_undeffunction),
    MMAP_ENTRY(defgenericModule, g_defgenericModule),
    MMAP_ENTRY(findDefgeneric, g_findDefgeneric),
    MMAP_ENTRY(getDefgenericList, g_getDefgenericList),
    MMAP_ENTRY(getDefgenericName, g_getDefgenericName),
    MMAP_ENTRY(getDefgenericPPForm, g_getDefgenericPPForm),
    MMAP_ENTRY(getDefgenericWatch, g_getDefgenericWatch),
    MMAP_ENTRY(getNextDefgeneric, g_getNextDefgeneric),
    MMAP_ENTRY(isDefgenericDeletable, g_isDefgenericDeletable),
    MMAP_ENTRY(listDefgenerics, g_listDefgenerics),
    MMAP_ENTRY(setDefgenericWatch, g_setDefgenericWatch),
    MMAP_ENTRY(undefgeneric, g_undefgeneric),
    MMAP_ENTRY(getDefmethodDescription, g_getDefmethodDescription),
    MMAP_ENTRY(getDefmethodList, g_getDefmethodList),
    MMAP_ENTRY(getDefmethodPPForm, g_getDefmethodPPForm),
    MMAP_ENTRY(getDefmethodWatch, g_getDefmethodWatch),
    MMAP_ENTRY(getMethodRestrictions, g_getMethodRestrictions),
    MMAP_ENTRY(getNextDefmethod, g_getNextDefmethod),
    MMAP_ENTRY(isDefmethodDeletable, g_isDefmethodDeletable),
    MMAP_ENTRY(listDefmethods, g_listDefmethods),
    MMAP_ENTRY(setDefmethodWatch, g_setDefmethodWatch),
    MMAP_ENTRY(undefmethod, g_undefmethod),
    MMAP_ENTRY(browseClasses, g_browseClasses),
    MMAP_ENTRY(classAbstractP, g_classAbstractP),
    MMAP_ENTRY(classReactiveP, g_classReactiveP),
    MMAP_ENTRY(classSlots, g_classSlots),
    MMAP_ENTRY(classSubclasses, g_classSubclasses),
    MMAP_ENTRY(classSuperclasses, g_classSuperclasses),
    MMAP_ENTRY(defclassModule, g_defclassModule),
    MMAP_ENTRY(describeClass, g_describeClass),
    MMAP_ENTRY(findDefclass, g_findDefclass),
    MMAP_ENTRY(getClassDefaultsMode, g_getClassDefaultsMode),
    MMAP_ENTRY(getDefclassList, g_getDefclassList),
    MMAP_ENTRY(getDefclassName, g_getDefclassName),
    MMAP_ENTRY(getDefclassPPForm, g_getDefclassPPForm),
    MMAP_ENTRY(getDefclassWatchInstances, g_getDefclassWatchInstances),
    MMAP_ENTRY(getDefclassWatchSlots, g_getDefclassWatchSlots),
    MMAP_ENTRY(getNextDefclass, g_getNextDefclass),
    MMAP_ENTRY(isDefclassDeletable, g_isDefclassDeletable),
    MMAP_ENTRY(listDefclasses, g_listDefclasses),
    MMAP_ENTRY(setClassDefaultsMode, g_setClassDefaultsMode),
    MMAP_ENTRY(setDefclassWatchInstances, g_setDefclassWatchInstances),
    MMAP_ENTRY(setDefclassWatchSlots, g_setDefclassWatchSlots),
    MMAP_ENTRY(slotAllowedValues, g_slotAllowedValues),
    MMAP_ENTRY(slotCardinality, g_slotCardinality),
    MMAP_ENTRY(slotAllowedClasses, g_slotAllowedClasses),
    MMAP_ENTRY(slotDefaultValue, g_slotDefaultValue),
    MMAP_ENTRY(slotDirectAccessP, g_slotDirectAccessP),
    MMAP_ENTRY(slotExistP, g_slotExistP),
    MMAP_ENTRY(slotFacets, g_slotFacets),
    MMAP_ENTRY(slotInitableP, g_slotInitableP),
    MMAP_ENTRY(slotPublicP, g_slotPublicP),
    MMAP_ENTRY(slotRange, g_slotRange),
    MMAP_ENTRY(slotSources, g_slotSources),
    MMAP_ENTRY(slotTypes, g_slotTypes),
    MMAP_ENTRY(slotWritableP, g_slotWritableP),
    MMAP_ENTRY(subclassP, g_subclassP),
    MMAP_ENTRY(superclassP, g_superclassP),
    MMAP_ENTRY(undefclass, g_undefclass),
    MMAP_ENTRY(binaryLoadInstances, g_binaryLoadInstances),
    MMAP_ENTRY(binarySaveInstances, g_binarySaveInstances),
    MMAP_ENTRY(createRawInstance, g_createRawInstance),
    MMAP_ENTRY(decrementInstanceCount, g_decrementInstanceCount),
    MMAP_ENTRY(deleteInstance, g_deleteInstance),
    MMAP_ENTRY(directGetSlot, g_directGetSlot),
    MMAP_ENTRY(directPutSlot, g_directPutSlot),
    MMAP_ENTRY(findInstance, g_findInstance),
    MMAP_ENTRY(getInstanceClass, g_getInstanceClass),
    MMAP_ENTRY(getInstanceName, g_getInstanceName),
    MMAP_ENTRY(getInstancePPForm, g_getInstancePPForm),
    MMAP_ENTRY(getInstancesChanged, g_getInstancesChanged),
    MMAP_ENTRY(getNextInstance, g_getNextInstance),
    MMAP_ENTRY(getNextInstanceInClass, g_getNextInstanceInClass),
    MMAP_ENTRY(getNextInstanceInClassAndSubclasses, g_getNextInstanceInClassAndSubclasses),
    MMAP_ENTRY(incrementInstanceCount, g_incrementInstanceCount),
    MMAP_ENTRY(instances, g_instances),
    MMAP_ENTRY(loadInstances, g_loadInstances),
    MMAP_ENTRY(makeInstance, g_makeInstance),
    MMAP_ENTRY(restoreInstances, g_restoreInstances),
    MMAP_ENTRY(saveInstances, g_saveInstances),
    MMAP_ENTRY(send, g_send),
    MMAP_ENTRY(setInstancesChanged, g_setInstancesChanged),
    MMAP_ENTRY(unmakeInstance, g_unmakeInstance),
    MMAP_ENTRY(validInstanceAddress, g_validInstanceAddress),
    MMAP_ENTRY(loadInstancesFromString, g_loadInstancesFromString),
    MMAP_ENTRY(restoreInstancesFromString, g_restoreInstancesFromString),
    MMAP_ENTRY(findDefmessageHandler, g_findDefmessageHandler),
    MMAP_ENTRY(getDefmessageHandlerList, g_getDefmessageHandlerList),
    MMAP_ENTRY(getDefmessageHandlerName, g_getDefmessageHandlerName),
    MMAP_ENTRY(getDefmessageHandlerPPForm, g_getDefmessageHandlerPPForm),
    MMAP_ENTRY(getDefmessageHandlerType, g_getDefmessageHandlerType),
    MMAP_ENTRY(getDefmessageHandlerWatch, g_getDefmessageHandlerWatch),
    MMAP_ENTRY(getNextDefmessageHandler, g_getNextDefmessageHandler),
    MMAP_ENTRY(isDefmessageHandlerDeletable, g_isDefmessageHandlerDeletable),
    MMAP_ENTRY(listDefmessageHandlers, g_listDefmessageHandlers),
    MMAP_ENTRY(previewSend, g_previewSend),
    MMAP_ENTRY(setDefmessageHandlerWatch, g_setDefmessageHandlerWatch),
    MMAP_ENTRY(undefmessageHandler, g_undefmessageHandler),
    MMAP_ENTRY(definstancesModule, g_definstancesModule),
    MMAP_ENTRY(findDefinstances, g_findDefinstances),
    MMAP_ENTRY(getDefinstancesList, g_getDefinstancesList),
    MMAP_ENTRY(getDefinstancesName, g_getDefinstancesName),
    MMAP_ENTRY(getDefinstancesPPForm, g_getDefinstancesPPForm),
    MMAP_ENTRY(getNextDefinstances, g_getNextDefinstances),
    MMAP_ENTRY(isDefinstancesDeletable, g_isDefinstancesDeletable),
    MMAP_ENTRY(listDefinstances, g_listDefinstances),
    MMAP_ENTRY(undefinstances, g_undefinstances),
    MMAP_ENTRY(findDefmodule, g_findDefmodule),
    MMAP_ENTRY(getCurrentModule, g_getCurrentModule),
    MMAP_ENTRY(getDefmoduleList, g_getDefmoduleList),
    MMAP_ENTRY(getDefmoduleName, g_getDefmoduleName),
    MMAP_ENTRY(getDefmodulePPForm, g_getDefmodulePPForm),
    MMAP_ENTRY(getNextDefmodule, g_getNextDefmodule),
    MMAP_ENTRY(listDefmodules, g_listDefmodules),
    MMAP_ENTRY(setCurrentModule, g_setCurrentModule),
    MMAP_ENTRY(setCurrentModule, g_setCurrentModule),
    MMAP_ENTRY(sendCommand, g_sendCommand),
    MMAP_ENTRY(removeClearFunction, g_removeClearFunction),
    MMAP_ENTRY(removePeriodicFunction, g_removePeriodicFunction),
    MMAP_ENTRY(removeResetFunction, g_removeResetFunction),
    MMAP_ENTRY(forceCleanup, g_forceCleanup),

/* -------------------------------------------------------------------- */

    MMAP_ENTRY(env_addClearFunction, e_addClearFunction),
    MMAP_ENTRY(env_addPeriodicFunction, e_addPeriodicFunction),
    MMAP_ENTRY(env_addResetFunction, e_addResetFunction),
    MMAP_ENTRY(env_bload, e_bload),
    MMAP_ENTRY(env_bsave, e_bsave),
    MMAP_ENTRY(env_clear, e_clear),
    MMAP_ENTRY(env_functionCall, e_functionCall),
    MMAP_ENTRY(env_getAutoFloatDividend, e_getAutoFloatDividend),
    MMAP_ENTRY(env_getDynamicConstraintChecking, e_getDynamicConstraintChecking),
    MMAP_ENTRY(env_getSequenceOperatorRecognition, e_getSequenceOperatorRecognition),
    MMAP_ENTRY(env_getStaticConstraintChecking, e_getStaticConstraintChecking),
    MMAP_ENTRY(env_load, e_load),
    MMAP_ENTRY(env_reset, e_reset),
    MMAP_ENTRY(env_save, e_save),
    MMAP_ENTRY(env_setAutoFloatDividend, e_setAutoFloatDividend),
    MMAP_ENTRY(env_setDynamicConstraintChecking, e_setDynamicConstraintChecking),
    MMAP_ENTRY(env_setSequenceOperatorRecognition, e_setSequenceOperatorRecognition),
    MMAP_ENTRY(env_setStaticConstraintChecking, e_setStaticConstraintChecking),
    MMAP_ENTRY(env_batchStar, e_batchStar),
    MMAP_ENTRY(env_build, e_build),
    MMAP_ENTRY(env_eval, e_eval),
    MMAP_ENTRY(env_dribbleActive, e_dribbleActive),
    MMAP_ENTRY(env_dribbleOff, e_dribbleOff),
    MMAP_ENTRY(env_dribbleOn, e_dribbleOn),
    MMAP_ENTRY(env_getWatchItem, e_getWatchItem),
    MMAP_ENTRY(env_unwatch, e_unwatch),
    MMAP_ENTRY(env_watch, e_watch),
    MMAP_ENTRY(env_deftemplateModule, e_deftemplateModule),
    MMAP_ENTRY(env_deftemplateSlotAllowedValues, e_deftemplateSlotAllowedValues),
    MMAP_ENTRY(env_deftemplateSlotCardinality, e_deftemplateSlotCardinality),
    MMAP_ENTRY(env_deftemplateSlotDefaultP, e_deftemplateSlotDefaultP),
    MMAP_ENTRY(env_deftemplateSlotDefaultValue, e_deftemplateSlotDefaultValue),
    MMAP_ENTRY(env_deftemplateSlotExistP, e_deftemplateSlotExistP),
    MMAP_ENTRY(env_deftemplateSlotMultiP, e_deftemplateSlotMultiP),
    MMAP_ENTRY(env_deftemplateSlotNames, e_deftemplateSlotNames),
    MMAP_ENTRY(env_deftemplateSlotRange, e_deftemplateSlotRange),
    MMAP_ENTRY(env_deftemplateSlotSingleP, e_deftemplateSlotSingleP),
    MMAP_ENTRY(env_deftemplateSlotTypes, e_deftemplateSlotTypes),
    MMAP_ENTRY(env_findDeftemplate, e_findDeftemplate),
    MMAP_ENTRY(env_getDeftemplateList, e_getDeftemplateList),
    MMAP_ENTRY(env_getDeftemplateName, e_getDeftemplateName),
    MMAP_ENTRY(env_getDeftemplatePPForm, e_getDeftemplatePPForm),
    MMAP_ENTRY(env_getDeftemplateWatch, e_getDeftemplateWatch),
    MMAP_ENTRY(env_getNextDeftemplate, e_getNextDeftemplate),
    MMAP_ENTRY(env_isDeftemplateDeletable, e_isDeftemplateDeletable),
    MMAP_ENTRY(env_listDeftemplates, e_listDeftemplates),
    MMAP_ENTRY(env_setDeftemplateWatch, e_setDeftemplateWatch),
    MMAP_ENTRY(env_undeftemplate, e_undeftemplate),
    MMAP_ENTRY(env_assertFact, e_assertFact),
    MMAP_ENTRY(env_assertString, e_assertString),
    MMAP_ENTRY(env_assignFactSlotDefaults, e_assignFactSlotDefaults),
    MMAP_ENTRY(env_createFact, e_createFact),
    MMAP_ENTRY(env_decrementFactCount, e_decrementFactCount),
    MMAP_ENTRY(env_factIndex, e_factIndex),
    MMAP_ENTRY(env_facts, e_facts),
    MMAP_ENTRY(env_getFactDuplication, e_getFactDuplication),
    MMAP_ENTRY(env_getFactListChanged, e_getFactListChanged),
    MMAP_ENTRY(env_getFactPPForm, e_getFactPPForm),
    MMAP_ENTRY(env_getFactSlot, e_getFactSlot),
    MMAP_ENTRY(env_getNextFact, e_getNextFact),
    MMAP_ENTRY(env_getNextFactInTemplate, e_getNextFactInTemplate),
    MMAP_ENTRY(env_incrementFactCount, e_incrementFactCount),
    MMAP_ENTRY(env_loadFacts, e_loadFacts),
    MMAP_ENTRY(env_ppFact, e_ppFact),
    MMAP_ENTRY(env_putFactSlot, e_putFactSlot),
    MMAP_ENTRY(env_retract, e_retract),
    MMAP_ENTRY(env_saveFacts, e_saveFacts),
    MMAP_ENTRY(env_setFactDuplication, e_setFactDuplication),
    MMAP_ENTRY(env_setFactListChanged, e_setFactListChanged),
    MMAP_ENTRY(env_factDeftemplate, e_factDeftemplate),
    MMAP_ENTRY(env_factExistp, e_factExistp),
    MMAP_ENTRY(env_factSlotNames, e_factSlotNames),
    MMAP_ENTRY(env_getFactList, e_getFactList),
    MMAP_ENTRY(env_loadFactsFromString, e_loadFactsFromString),
    MMAP_ENTRY(env_deffactsModule, e_deffactsModule),
    MMAP_ENTRY(env_findDeffacts, e_findDeffacts),
    MMAP_ENTRY(env_getDeffactsList, e_getDeffactsList),
    MMAP_ENTRY(env_getDeffactsName, e_getDeffactsName),
    MMAP_ENTRY(env_getDeffactsPPForm, e_getDeffactsPPForm),
    MMAP_ENTRY(env_getNextDeffacts, e_getNextDeffacts),
    MMAP_ENTRY(env_isDeffactsDeletable, e_isDeffactsDeletable),
    MMAP_ENTRY(env_listDeffacts, e_listDeffacts),
    MMAP_ENTRY(env_undeffacts, e_undeffacts),
    MMAP_ENTRY(env_defruleHasBreakpoint, e_defruleHasBreakpoint),
    MMAP_ENTRY(env_defruleModule, e_defruleModule),
    MMAP_ENTRY(env_findDefrule, e_findDefrule),
    MMAP_ENTRY(env_getDefruleList, e_getDefruleList),
    MMAP_ENTRY(env_getDefruleName, e_getDefruleName),
    MMAP_ENTRY(env_getDefrulePPForm, e_getDefrulePPForm),
    MMAP_ENTRY(env_getDefruleWatchActivations, e_getDefruleWatchActivations),
    MMAP_ENTRY(env_getDefruleWatchFirings, e_getDefruleWatchFirings),
    MMAP_ENTRY(env_getIncrementalReset, e_getIncrementalReset),
    MMAP_ENTRY(env_getNextDefrule, e_getNextDefrule),
    MMAP_ENTRY(env_isDefruleDeletable, e_isDefruleDeletable),
    MMAP_ENTRY(env_listDefrules, e_listDefrules),
    MMAP_ENTRY(env_matches, e_matches),
    MMAP_ENTRY(env_refresh, e_refresh),
    MMAP_ENTRY(env_removeBreak, e_removeBreak),
    MMAP_ENTRY(env_setBreak, e_setBreak),
    MMAP_ENTRY(env_setDefruleWatchActivations, e_setDefruleWatchActivations),
    MMAP_ENTRY(env_setDefruleWatchFirings, e_setDefruleWatchFirings),
    MMAP_ENTRY(env_setIncrementalReset, e_setIncrementalReset),
    MMAP_ENTRY(env_showBreaks, e_showBreaks),
    MMAP_ENTRY(env_undefrule, e_undefrule),
    MMAP_ENTRY(env_addRunFunction, e_addRunFunction),
    MMAP_ENTRY(env_agenda, e_agenda),
    MMAP_ENTRY(env_clearFocusStack, e_clearFocusStack),
    MMAP_ENTRY(env_deleteActivation, e_deleteActivation),
    MMAP_ENTRY(env_focus, e_focus),
    MMAP_ENTRY(env_getActivationName, e_getActivationName),
    MMAP_ENTRY(env_getActivationPPForm, e_getActivationPPForm),
    MMAP_ENTRY(env_getActivationSalience, e_getActivationSalience),
    MMAP_ENTRY(env_getAgendaChanged, e_getAgendaChanged),
    MMAP_ENTRY(env_getFocus, e_getFocus),
    MMAP_ENTRY(env_getFocusStack, e_getFocusStack),
    MMAP_ENTRY(env_getNextActivation, e_getNextActivation),
    MMAP_ENTRY(env_getSalienceEvaluation, e_getSalienceEvaluation),
    MMAP_ENTRY(env_getStrategy, e_getStrategy),
    MMAP_ENTRY(env_listFocusStack, e_listFocusStack),
    MMAP_ENTRY(env_popFocus, e_popFocus),
    MMAP_ENTRY(env_refreshAgenda, e_refreshAgenda),
    MMAP_ENTRY(env_removeRunFunction, e_removeRunFunction),
    MMAP_ENTRY(env_reorderAgenda, e_reorderAgenda),
    MMAP_ENTRY(env_run, e_run),
    MMAP_ENTRY(env_setActivationSalience, e_setActivationSalience),
    MMAP_ENTRY(env_setAgendaChanged, e_setAgendaChanged),
    MMAP_ENTRY(env_setSalienceEvaluation, e_setSalienceEvaluation),
    MMAP_ENTRY(env_setStrategy, e_setStrategy),
    MMAP_ENTRY(env_defglobalModule, e_defglobalModule),
    MMAP_ENTRY(env_findDefglobal, e_findDefglobal),
    MMAP_ENTRY(env_getDefglobalList, e_getDefglobalList),
    MMAP_ENTRY(env_getDefglobalName, e_getDefglobalName),
    MMAP_ENTRY(env_getDefglobalPPForm, e_getDefglobalPPForm),
    MMAP_ENTRY(env_getDefglobalValue, e_getDefglobalValue),
    MMAP_ENTRY(env_getDefglobalValueForm, e_getDefglobalValueForm),
    MMAP_ENTRY(env_getDefglobalWatch, e_getDefglobalWatch),
    MMAP_ENTRY(env_getGlobalsChanged, e_getGlobalsChanged),
    MMAP_ENTRY(env_getNextDefglobal, e_getNextDefglobal),
    MMAP_ENTRY(env_getResetGlobals, e_getResetGlobals),
    MMAP_ENTRY(env_isDefglobalDeletable, e_isDefglobalDeletable),
    MMAP_ENTRY(env_listDefglobals, e_listDefglobals),
    MMAP_ENTRY(env_setDefglobalValue, e_setDefglobalValue),
    MMAP_ENTRY(env_setDefglobalWatch, e_setDefglobalWatch),
    MMAP_ENTRY(env_setGlobalsChanged, e_setGlobalsChanged),
    MMAP_ENTRY(env_setResetGlobals, e_setResetGlobals),
    MMAP_ENTRY(env_showDefglobals, e_showDefglobals),
    MMAP_ENTRY(env_undefglobal, e_undefglobal),
    MMAP_ENTRY(env_deffunctionModule, e_deffunctionModule),
    MMAP_ENTRY(env_findDeffunction, e_findDeffunction),
    MMAP_ENTRY(env_getDeffunctionList, e_getDeffunctionList),
    MMAP_ENTRY(env_getDeffunctionName, e_getDeffunctionName),
    MMAP_ENTRY(env_getDeffunctionPPForm, e_getDeffunctionPPForm),
    MMAP_ENTRY(env_getDeffunctionWatch, e_getDeffunctionWatch),
    MMAP_ENTRY(env_getNextDeffunction, e_getNextDeffunction),
    MMAP_ENTRY(env_isDeffunctionDeletable, e_isDeffunctionDeletable),
    MMAP_ENTRY(env_listDeffunctions, e_listDeffunctions),
    MMAP_ENTRY(env_setDeffunctionWatch, e_setDeffunctionWatch),
    MMAP_ENTRY(env_undeffunction, e_undeffunction),
    MMAP_ENTRY(env_defgenericModule, e_defgenericModule),
    MMAP_ENTRY(env_findDefgeneric, e_findDefgeneric),
    MMAP_ENTRY(env_getDefgenericList, e_getDefgenericList),
    MMAP_ENTRY(env_getDefgenericName, e_getDefgenericName),
    MMAP_ENTRY(env_getDefgenericPPForm, e_getDefgenericPPForm),
    MMAP_ENTRY(env_getDefgenericWatch, e_getDefgenericWatch),
    MMAP_ENTRY(env_getNextDefgeneric, e_getNextDefgeneric),
    MMAP_ENTRY(env_isDefgenericDeletable, e_isDefgenericDeletable),
    MMAP_ENTRY(env_listDefgenerics, e_listDefgenerics),
    MMAP_ENTRY(env_setDefgenericWatch, e_setDefgenericWatch),
    MMAP_ENTRY(env_undefgeneric, e_undefgeneric),
    MMAP_ENTRY(env_getDefmethodDescription, e_getDefmethodDescription),
    MMAP_ENTRY(env_getDefmethodList, e_getDefmethodList),
    MMAP_ENTRY(env_getDefmethodPPForm, e_getDefmethodPPForm),
    MMAP_ENTRY(env_getDefmethodWatch, e_getDefmethodWatch),
    MMAP_ENTRY(env_getMethodRestrictions, e_getMethodRestrictions),
    MMAP_ENTRY(env_getNextDefmethod, e_getNextDefmethod),
    MMAP_ENTRY(env_isDefmethodDeletable, e_isDefmethodDeletable),
    MMAP_ENTRY(env_listDefmethods, e_listDefmethods),
    MMAP_ENTRY(env_setDefmethodWatch, e_setDefmethodWatch),
    MMAP_ENTRY(env_undefmethod, e_undefmethod),
    MMAP_ENTRY(env_browseClasses, e_browseClasses),
    MMAP_ENTRY(env_classAbstractP, e_classAbstractP),
    MMAP_ENTRY(env_classReactiveP, e_classReactiveP),
    MMAP_ENTRY(env_classSlots, e_classSlots),
    MMAP_ENTRY(env_classSubclasses, e_classSubclasses),
    MMAP_ENTRY(env_classSuperclasses, e_classSuperclasses),
    MMAP_ENTRY(env_defclassModule, e_defclassModule),
    MMAP_ENTRY(env_describeClass, e_describeClass),
    MMAP_ENTRY(env_findDefclass, e_findDefclass),
    MMAP_ENTRY(env_getClassDefaultsMode, e_getClassDefaultsMode),
    MMAP_ENTRY(env_getDefclassList, e_getDefclassList),
    MMAP_ENTRY(env_getDefclassName, e_getDefclassName),
    MMAP_ENTRY(env_getDefclassPPForm, e_getDefclassPPForm),
    MMAP_ENTRY(env_getDefclassWatchInstances, e_getDefclassWatchInstances),
    MMAP_ENTRY(env_getDefclassWatchSlots, e_getDefclassWatchSlots),
    MMAP_ENTRY(env_getNextDefclass, e_getNextDefclass),
    MMAP_ENTRY(env_isDefclassDeletable, e_isDefclassDeletable),
    MMAP_ENTRY(env_listDefclasses, e_listDefclasses),
    MMAP_ENTRY(env_setClassDefaultsMode, e_setClassDefaultsMode),
    MMAP_ENTRY(env_setDefclassWatchInstances, e_setDefclassWatchInstances),
    MMAP_ENTRY(env_setDefclassWatchSlots, e_setDefclassWatchSlots),
    MMAP_ENTRY(env_slotAllowedValues, e_slotAllowedValues),
    MMAP_ENTRY(env_slotCardinality, e_slotCardinality),
    MMAP_ENTRY(env_slotAllowedClasses, e_slotAllowedClasses),
    MMAP_ENTRY(env_slotDefaultValue, e_slotDefaultValue),
    MMAP_ENTRY(env_slotDirectAccessP, e_slotDirectAccessP),
    MMAP_ENTRY(env_slotExistP, e_slotExistP),
    MMAP_ENTRY(env_slotFacets, e_slotFacets),
    MMAP_ENTRY(env_slotInitableP, e_slotInitableP),
    MMAP_ENTRY(env_slotPublicP, e_slotPublicP),
    MMAP_ENTRY(env_slotRange, e_slotRange),
    MMAP_ENTRY(env_slotSources, e_slotSources),
    MMAP_ENTRY(env_slotTypes, e_slotTypes),
    MMAP_ENTRY(env_slotWritableP, e_slotWritableP),
    MMAP_ENTRY(env_subclassP, e_subclassP),
    MMAP_ENTRY(env_superclassP, e_superclassP),
    MMAP_ENTRY(env_undefclass, e_undefclass),
    MMAP_ENTRY(env_binaryLoadInstances, e_binaryLoadInstances),
    MMAP_ENTRY(env_binarySaveInstances, e_binarySaveInstances),
    MMAP_ENTRY(env_createRawInstance, e_createRawInstance),
    MMAP_ENTRY(env_decrementInstanceCount, e_decrementInstanceCount),
    MMAP_ENTRY(env_deleteInstance, e_deleteInstance),
    MMAP_ENTRY(env_directGetSlot, e_directGetSlot),
    MMAP_ENTRY(env_directPutSlot, e_directPutSlot),
    MMAP_ENTRY(env_findInstance, e_findInstance),
    MMAP_ENTRY(env_getInstanceClass, e_getInstanceClass),
    MMAP_ENTRY(env_getInstanceName, e_getInstanceName),
    MMAP_ENTRY(env_getInstancePPForm, e_getInstancePPForm),
    MMAP_ENTRY(env_getInstancesChanged, e_getInstancesChanged),
    MMAP_ENTRY(env_getNextInstance, e_getNextInstance),
    MMAP_ENTRY(env_getNextInstanceInClass, e_getNextInstanceInClass),
    MMAP_ENTRY(env_getNextInstanceInClassAndSubclasses, e_getNextInstanceInClassAndSubclasses),
    MMAP_ENTRY(env_incrementInstanceCount, e_incrementInstanceCount),
    MMAP_ENTRY(env_instances, e_instances),
    MMAP_ENTRY(env_loadInstances, e_loadInstances),
    MMAP_ENTRY(env_makeInstance, e_makeInstance),
    MMAP_ENTRY(env_restoreInstances, e_restoreInstances),
    MMAP_ENTRY(env_saveInstances, e_saveInstances),
    MMAP_ENTRY(env_send, e_send),
    MMAP_ENTRY(env_setInstancesChanged, e_setInstancesChanged),
    MMAP_ENTRY(env_unmakeInstance, e_unmakeInstance),
    MMAP_ENTRY(env_validInstanceAddress, e_validInstanceAddress),
    MMAP_ENTRY(env_loadInstancesFromString, e_loadInstancesFromString),
    MMAP_ENTRY(env_restoreInstancesFromString, e_restoreInstancesFromString),
    MMAP_ENTRY(env_findDefmessageHandler, e_findDefmessageHandler),
    MMAP_ENTRY(env_getDefmessageHandlerList, e_getDefmessageHandlerList),
    MMAP_ENTRY(env_getDefmessageHandlerName, e_getDefmessageHandlerName),
    MMAP_ENTRY(env_getDefmessageHandlerPPForm, e_getDefmessageHandlerPPForm),
    MMAP_ENTRY(env_getDefmessageHandlerType, e_getDefmessageHandlerType),
    MMAP_ENTRY(env_getDefmessageHandlerWatch, e_getDefmessageHandlerWatch),
    MMAP_ENTRY(env_getNextDefmessageHandler, e_getNextDefmessageHandler),
    MMAP_ENTRY(env_isDefmessageHandlerDeletable, e_isDefmessageHandlerDeletable),
    MMAP_ENTRY(env_listDefmessageHandlers, e_listDefmessageHandlers),
    MMAP_ENTRY(env_previewSend, e_previewSend),
    MMAP_ENTRY(env_setDefmessageHandlerWatch, e_setDefmessageHandlerWatch),
    MMAP_ENTRY(env_undefmessageHandler, e_undefmessageHandler),
    MMAP_ENTRY(env_definstancesModule, e_definstancesModule),
    MMAP_ENTRY(env_findDefinstances, e_findDefinstances),
    MMAP_ENTRY(env_getDefinstancesList, e_getDefinstancesList),
    MMAP_ENTRY(env_getDefinstancesName, e_getDefinstancesName),
    MMAP_ENTRY(env_getDefinstancesPPForm, e_getDefinstancesPPForm),
    MMAP_ENTRY(env_getNextDefinstances, e_getNextDefinstances),
    MMAP_ENTRY(env_isDefinstancesDeletable, e_isDefinstancesDeletable),
    MMAP_ENTRY(env_listDefinstances, e_listDefinstances),
    MMAP_ENTRY(env_undefinstances, e_undefinstances),
    MMAP_ENTRY(env_findDefmodule, e_findDefmodule),
    MMAP_ENTRY(env_getCurrentModule, e_getCurrentModule),
    MMAP_ENTRY(env_getDefmoduleList, e_getDefmoduleList),
    MMAP_ENTRY(env_getDefmoduleName, e_getDefmoduleName),
    MMAP_ENTRY(env_getDefmodulePPForm, e_getDefmodulePPForm),
    MMAP_ENTRY(env_getNextDefmodule, e_getNextDefmodule),
    MMAP_ENTRY(env_listDefmodules, e_listDefmodules),
    MMAP_ENTRY(env_setCurrentModule, e_setCurrentModule),
    MMAP_ENTRY(env_sendCommand, e_sendCommand),
    MMAP_ENTRY(env_removeClearFunction, e_removeClearFunction),
    MMAP_ENTRY(env_removePeriodicFunction, e_removePeriodicFunction),
    MMAP_ENTRY(env_removeResetFunction, e_removeResetFunction),
    MMAP_ENTRY(env_forceCleanup, e_forceCleanup),

    /* -------------------------------------------------------------------- */

    MMAP_ENTRY(getConserveMemory, m_getConserveMemory),
    MMAP_ENTRY(memRequests, m_memRequests),
    MMAP_ENTRY(memUsed, m_memUsed),
    MMAP_ENTRY(releaseMem, m_releaseMem),
    MMAP_ENTRY(setConserveMemory, m_setConserveMemory),
    MMAP_ENTRY(setOutOfMemoryFunction, m_setOutOfMemoryFunction),
    MMAP_ENTRY(getPPBufferSize, m_getPPBufferSize),
    MMAP_ENTRY(setPPBufferSize, m_setPPBufferSize),

    MMAP_ENTRY(addEnvironmentCleanupFunction, v_addEnvironmentCleanupFunction),
    MMAP_ENTRY(allocateEnvironmentData, v_allocateEnvironmentData),
    MMAP_ENTRY(createEnvironment, v_createEnvironment),
    MMAP_ENTRY(destroyEnvironment, v_destroyEnvironment),
    MMAP_ENTRY(getCurrentEnvironment, v_getCurrentEnvironment),
    MMAP_ENTRY(getEnvironmentData, v_getEnvironmentData),
    MMAP_ENTRY(getEnvironmentIndex, v_getEnvironmentIndex),
    MMAP_ENTRY(setCurrentEnvironment, v_setCurrentEnvironment),
    MMAP_ENTRY(setCurrentEnvironmentByIndex, v_setCurrentEnvironmentByIndex),

    MMAP_ENTRY(getNumberOfEnvironments, v_getNumberOfEnvironments),
    MMAP_ENTRY(getMaxEnvironments, v_getMaxEnvironments),
    MMAP_ENTRY(setMaxEnvironments, v_setMaxEnvironments),

    /* -------------------------------------------------------------------- */

    MMAP_ENTRY(isEnvironment, g_isEnvironment),
    MMAP_ENTRY(isDeftemplate, g_isDeftemplate),
    MMAP_ENTRY(isFact, g_isFact),
    MMAP_ENTRY(isInstance, g_isInstance),
    MMAP_ENTRY(isDefmodule, g_isDefmodule),
    MMAP_ENTRY(isDeffacts, g_isDeffacts),
    MMAP_ENTRY(isDefrule, g_isDefrule),
    MMAP_ENTRY(isActivation, g_isActivation),
    MMAP_ENTRY(isDefglobal, g_isDefglobal),
    MMAP_ENTRY(isDeffunction, g_isDeffunction),
    MMAP_ENTRY(isDefgeneric, g_isDefgeneric),
    MMAP_ENTRY(isDefmethod, g_isDefmethod),
    MMAP_ENTRY(isDefclass, g_isDefclass),
    MMAP_ENTRY(isDefinstances, g_isDefinstances),

    /* -------------------------------------------------------------------- */

    MMAP_ENTRY(setEnableEnvironmentFatalMessages, g_setEnableEnvironmentFatalMessages),
    MMAP_ENTRY(getEnableEnvironmentFatalMessages, g_getEnableEnvironmentFatalMessages),

    /* -------------------------------------------------------------------- */

    MMAP_ENTRY(routerWrite, g_routerWrite),
    MMAP_ENTRY(routerClear, g_routerClear),
    MMAP_ENTRY(routerRead, g_routerRead),
    MMAP_ENTRY(routerReadline, g_routerReadline),

    /* -------------------------------------------------------------------- */

    MMAP_ENTRY(addPythonFunction, f_addPythonFunction),
    MMAP_ENTRY(removePythonFunction, f_removePythonFunction),
    MMAP_ENTRY(clearPythonFunctions, f_clearPythonFunctions),
    MMAP_ENTRY(setPrintExternalTraceback, f_setPrintExternalTraceback),
    MMAP_ENTRY(getPrintExternalTraceback, f_getPrintExternalTraceback),

    // ...

    {NULL, NULL, 0, NULL},
};


/* initialization function */
PYFUNC
PyMODINIT_FUNC
init_clips(void) {
    PyObject *m = NULL, *d = NULL;
#ifdef USE_NONASSERT_CLIPSGCLOCK
    void *e = NULL;
    LOPTR_ITEM ***hm = NULL;
#endif /* USE_NONASSERT_CLIPSGCLOCK */
    PREPARE_DEALLOC_ENV();

    /* give the module a method map */
    m = Py_InitModule3("_clips", g_methods, clips__doc__);
    d = PyModule_GetDict(m);

    /* possibly install the environment deallocator */
    INSTALL_DEALLOC_ENV(m);

    /* set the item version string */
    PyDict_SetItemString(d, "__revision__",
                         PyString_FromString(clips__revision__));

    /* build the actual exception objects */
    PyExc_ClipsError = PyErr_NewException("_clips.ClipsError", NULL, NULL);
    PyDict_SetItemString(d, "ClipsError", PyExc_ClipsError);
    
    PyExc_ClipsMemoryError = PyErr_NewException(
        "_clips.ClipsMemoryError", NULL, NULL);
    PyDict_SetItemString(d, "ClipsMemoryError", PyExc_ClipsMemoryError);

    /* setup ob_type for types defined here */
    clips_EnvType.ob_type = &PyType_Type;
    clips_DeftemplType.ob_type = &PyType_Type;
    clips_FactType.ob_type = &PyType_Type;
    clips_DefmoduleType.ob_type = &PyType_Type;
    clips_DeffactsType.ob_type = &PyType_Type;
    clips_ActivationType.ob_type = &PyType_Type;
    clips_DefglobalType.ob_type = &PyType_Type;
    clips_DeffunctionType.ob_type = &PyType_Type;
    clips_DefgenericType.ob_type = &PyType_Type;
    clips_DefmethodType.ob_type = &PyType_Type;
    clips_DefclassType.ob_type = &PyType_Type;

    buffer_Type.ob_type = &PyType_Type;

    /* initialize the router system */
    clips_Streams = (PyDictObject *)PyDict_New();
    bufdict_Add("t", TRUE);
    bufdict_Add("stdin", FALSE);
    bufdict_Add("stdout", TRUE);
    bufdict_Add("wprompt", TRUE);
    bufdict_Add("wdialog", TRUE);
    bufdict_Add("wdisplay", TRUE);
    bufdict_Add("werror", TRUE);
    bufdict_Add("wwarning", TRUE);
    bufdict_Add("wtrace", TRUE);
    bufdict_Add("temporary", TRUE);     /* used by the wrapper module */

    /* adding clips_Streams to module dict will free it on finalization */
    PyModule_AddObject(m,
        "__PyCLIPS_$iRouterSystem__", (PyObject *)clips_Streams);

    /* initialize dictionary of Python functions and mark for finalization */
    clips_PythonFunctions = (PyDictObject *)PyDict_New();
    PyModule_AddObject(m,
        "__PyCLIPS_$iPythonFunctions__", (PyObject *)clips_PythonFunctions);

    /* setup manifest constants */
    ADD_MANIFEST_CONSTANT(d, SYMBOL);
    ADD_MANIFEST_CONSTANT(d, STRING);
    ADD_MANIFEST_CONSTANT(d, INTEGER);
    ADD_MANIFEST_CONSTANT(d, FLOAT);
    ADD_MANIFEST_CONSTANT(d, EXTERNAL_ADDRESS);
    ADD_MANIFEST_CONSTANT(d, INSTANCE_NAME);
    ADD_MANIFEST_CONSTANT(d, INSTANCE_ADDRESS);
    ADD_MANIFEST_CONSTANT(d, FACT_ADDRESS);
    ADD_MANIFEST_CONSTANT(d, MULTIFIELD);

    ADD_MANIFEST_CONSTANT(d, LOCAL_SAVE);
    ADD_MANIFEST_CONSTANT(d, VISIBLE_SAVE);

    ADD_MANIFEST_CONSTANT(d, WHEN_DEFINED);
    ADD_MANIFEST_CONSTANT(d, WHEN_ACTIVATED);
    ADD_MANIFEST_CONSTANT(d, EVERY_CYCLE);

    ADD_MANIFEST_CONSTANT(d, DEPTH_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, BREADTH_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, LEX_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, MEA_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, COMPLEXITY_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, SIMPLICITY_STRATEGY);
    ADD_MANIFEST_CONSTANT(d, RANDOM_STRATEGY);

    ADD_MANIFEST_CONSTANT(d, CONVENIENCE_MODE);
    ADD_MANIFEST_CONSTANT(d, CONSERVATION_MODE);

    ADD_MANIFEST_CONSTANT(d, CLIPS_MAJOR);
    ADD_MANIFEST_CONSTANT(d, CLIPS_MINOR);

    ADD_MANIFEST_CONSTANT(d, PYCLIPS_MAJOR);
    ADD_MANIFEST_CONSTANT(d, PYCLIPS_MINOR);
    ADD_MANIFEST_CONSTANT(d, PYCLIPS_PATCHLEVEL);
    ADD_MANIFEST_CONSTANT(d, PYCLIPS_INCREMENTAL);

    ADD_MANIFEST_CONSTANT(d, NO_DEFAULT);
    ADD_MANIFEST_CONSTANT(d, STATIC_DEFAULT);
    ADD_MANIFEST_CONSTANT(d, DYNAMIC_DEFAULT);

    /* let us hope that no memory errors happen right here, because
     *  memory handler has not been enabled; however the flag that
     *  indicates that memory errors have been enabled is still not
     *  set: a memory allocation failure here will thus cause the
     *  Python interpreter to exit due to the CLIPS way of treating
     *  the problem.
     *  TODO: a "more elegant way" to notify the user.
     */

    /* we should initialize CLIPS environment once module is loaded */
    InitializeEnvironment();

#ifdef USE_NONASSERT_CLIPSGCLOCK
    e = GetCurrentEnvironment();
    AllocateEnvironmentData(e, STRAYFACTS_DATA, sizeof(LOPTR_ITEM ***), NULL);
    /* STRAYFACTS_DATA will contain just a copy of the pointer to the map */
    hm = (LOPTR_ITEM ***)GetEnvironmentData(e, STRAYFACTS_DATA);
    *hm = clips_StrayFacts;     /* no real thin ice, it was allocated */
#endif /* USE_NONASSERT_CLIPSGCLOCK */

    /* add the Python router to the engine */
    AddRouter("python", 0,
              clips_queryFunction,
              clips_printFunction,
              clips_getcFunction,
              clips_ungetcFunction,
              clips_exitFunction);
    ActivateRouter("python");

}


/* end */
