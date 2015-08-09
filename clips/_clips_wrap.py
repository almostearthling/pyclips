# _clips_wrap.py
# higher-level interface for the _clips module

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

"""\
clips - high-level interface to the CLIPS engine module
        (c) 2002-2008 Francesco Garosi/JKS
"""

__revision__ = "$Id: _clips_wrap.py 342 2008-02-22 01:17:23Z Franz $"

# ========================================================================== #
# imports - these are hidden to module user

# standard imports
import sys as _sys
import os as  _os

# the low-level module
import _clips as _c

# check Python version, and issue an exception if not supported
if _sys.version[:3] < "2.4":
    raise _c.ClipsError("M99: Python 2.4 or higher required")


# ========================================================================== #
# globals

# clips version is defined
CLIPS_VERSION = "%s.%s" % (_c.CLIPS_MAJOR, _c.CLIPS_MINOR)
PYCLIPS_VERSION = "%s.%s.%s.%s" % (
    _c.PYCLIPS_MAJOR,
    _c.PYCLIPS_MINOR,
    _c.PYCLIPS_PATCHLEVEL,
    _c.PYCLIPS_INCREMENTAL)

# bring the CLIPS exception objects at top level
ClipsError = _c.ClipsError
ClipsMemoryError = _c.ClipsMemoryError


# redeclare manifest constants here in order to avoid having to
#  reference the ones defined in the low-level module _clips

# These manifest constants are commented out, since the user has to rely
#  on the class constructors defined below in order to build values to
#  pass to the CLIPS engine. Also, these names are used to implement
#  the stock class objects (see below)
##INTEGER = _c.INTEGER
##FLOAT = _c.FLOAT
##STRING = _c.STRING
##SYMBOL = _c.SYMBOL
##INSTANCE_NAME = _c.INSTANCE_NAME
##MULTIFIELD = _c.MULTIFIELD
##INSTANCE_ADDRESS = _c.INSTANCE_ADDRESS
##EXTERNAL_ADDRESS = _c.EXTERNAL_ADDRESS
##FACT_ADDRESS = _c.FACT_ADDRESS

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

AFTER = 'after'
AROUND = 'around'
BEFORE = 'before'
PRIMARY = 'primary'


# ========================================================================== #
# these decorators allow to verify the types of data passed to the decorated
#  functions and methods (_accepts* decorators) and to force the input types
#  to some defined ones (_force* decorators)

# verify that the types passed to the decorated functions are exactly the
#  ones passed as arguments to the decorator: if not, raise an appropriate
#  TypeError. If a type is specified as None, then the type in the same
#  position will not be checked. If a tuple containing types is passed to
#  the decorator, then the decorator will verify that the function name is
#  of one of the types in the tuple. Examples:
#
# @_accepts(int, int)   # will raise TypeError if either x or y is not int
# def add(x, y):
#   return x + y
#
# @_accepts(int, (int, float))  # will accept if y is either int or float
# def add(x, y):
#   return x + y
#
# @_accepts(None, int)  # will raise if y is not int, but won't check x
# def add(x, y):
#   return x + y
#
def _accepts(*types):
    def _DECO(f):
        def _WRAPPER(*args):
            i = 0   # start counting arguments at 0
            for a in args:
                t = types[i]
                if t is not None:   # otherwise no type checking
                    # please note that isinstance already accepts a tuple
                    if not isinstance(a, t):
                        if type(t) == tuple:
                            errorstr = \
                                "one of %s expected in %s, parameter %s" \
                                % (", ".join(map(lambda x: str(x)[1:-1], t)),
                                   f.__name__, i + 1)
                        else:
                            errorstr = \
                                "%s expected in %s, parameter %s" \
                                % (str(t)[1:-1], f.__name__, i + 1)
                        raise TypeError(errorstr)
                i += 1
            return f(*args)
        _WRAPPER.__name__ = f.__name__
        _WRAPPER.__doc__ = f.__doc__
        return _WRAPPER
    return _DECO

# same as above, but for class methods: takes the implicit self in account
def _accepts_method(*types):
    def _DECO(f):
        def _WRAPPER(self, *args):
            i = 0
            for a in args:
                t = types[i]
                if t is not None:
                    if not isinstance(a, t):
                        if type(t) == tuple:
                            errorstr = \
                                "one of %s expected in %s, parameter %s" \
                                % (", ".join(map(lambda x: str(x)[1:-1], t)),
                                   f.__name__, i + 1)
                        else:
                            errorstr = \
                                "%s expected in %s, parameter %s" \
                                % (str(t)[1:-1], f.__name__, i + 1)
                        raise TypeError(errorstr)
                i += 1
            return f(self, *args)
        _WRAPPER.__name__ = f.__name__
        _WRAPPER.__doc__ = f.__doc__
        return _WRAPPER
    return _DECO


# given a list of types to the decorator, the arguments of the decorated
#  function are converted (cast) to the corresponding type in the list. If
#  None is given as an argument to the decorator, in the decorated function
#  the corresponding parameter is left alone. Example:
#
# @_forces(None, int)   # x is left as it is, while y is converted to int
# def add(x, y):
#   return x + y
#
#  a dict can be specified as a conversion map: in this case the keys are
#  the types that will be converted, the values are the types to convert to
#  and None acts differently when used as key (in which case it converts
#  every type as a last resort) or as a value (when used here there is no
#  conversion). An example:
#
# @_forces(None, {float: long, long: None, None: int})
# def add(x, y):
#   return x + y
#
def _forces(*types):
    def _DECO(f):
        def _WRAPPER(*args):
            newargs = []
            i = 0
            for a in args:
                t = types[i]
                # when None is used as a type to convert to, no conversion
                #  performed at all (the argument is left as it is)
                if t is None:
                    newargs.append(a)
                # pass a dict to perform selective conversions, where...
                elif type(t) == dict:
                    type_a = type(a)
                    # ...if the type of the argument is taken into account...
                    if t.has_key(type_a):
                        conv = t[type_a]
                        # ...when it is not None, the argument is converted
                        if conv is not None:
                            newargs.append(conv(a))
                        # ...when it is None, the argument is left as it is
                        else:
                            newargs.append(a)
                    # ...if no other specification was found, but there is
                    #  None as a possible type to convert to another, then
                    #  the argument is converted anyway (ie. None acts as a
                    #  sink that converts any type as a last resort)
                    elif t.has_key(None):
                        newargs.append(t[None](a))
                    # ...but when there is no suitable specific conversion
                    #  and None is not given the argument is left as it is
                    else:
                        newargs.append(a)
                # otherwise the argument is converted to the specified type
                else:
                    newargs.append(t(a))
                i += 1
            return f(*newargs)
        _WRAPPER.__name__ = f.__name__
        _WRAPPER.__doc__ = f.__doc__
        return _WRAPPER
    return _DECO

# same as above, but for class methods: takes the implicit self in account
def _forces_method(*types):
    def _DECO(f):
        def _WRAPPER(self, *args):
            newargs = []
            i = 0
            for a in args:
                t = types[i]
                if t is None:
                    newargs.append(a)
                elif type(t) == dict:
                    type_a = type(a)
                    if t.has_key(type_a):
                        conv = t[type_a]
                        if conv is not None:
                            newargs.append(conv(a))
                        else:
                            newargs.append(a)
                    elif t.has_key(None):
                        newargs.append(t[None](a))
                    else:
                        newargs.append(a)
                else:
                    newargs.append(t(a))
                i += 1
            return f(self, *newargs)
        _WRAPPER.__name__ = f.__name__
        _WRAPPER.__doc__ = f.__doc__
        return _WRAPPER
    return _DECO

# the decorators have underscored names, because they are too raw and too
#  unspecific to the module to be used outside this scope; please note that
#  the environment aware module will only have to use the method specific
#  versions


# ========================================================================== #
# High-level classes to embed clips internal types

# 1) numeric types
class Integer(int):
    """extend an int for use with CLIPS"""
    def __repr__(self):
        return "<Integer %s>" % int.__repr__(self)
    def __add__(self, o):
        return Integer(int(self) + int(o))
    def __sub__(self, o):
        return Integer(int(self) - int(o))
    def __mul__(self, o):
        return Integer(int(self) * int(o))
    def __floordiv__(self, o):
        return Integer(int(self) // int(o))
    def __truediv__(self, o):
        return Integer(int(self) / int(o))
    def __div__(self, o):
        return Integer(int(self) / int(o))
    def __mod__(self, o):
        return Integer(int(self) % int(o))
    def __lshift__(self, o):
        return Integer(int(self) << int(o))
    def __rshift__(self, o):
        return Integer(int(self) >> int(o))
    def __and__(self, o):
        return Integer(int(self) & int(o))
    def __xor__(self, o):
        return Integer(int(self) ^ int(o))
    def __or__(self, o):
        return Integer(int(self) | int(o))
    def __pow__(self, o, m=None):
        if m is not None:
            return Integer((int(self) ** int(o)) % int(m))
        return Integer(int(self) ** int(o))
    def clrepr(self):
        """represent this Integer for CLIPS"""
        return (_c.INTEGER, int(self))
    def clsyntax(self):
        """represent this Integer as it would be in CLIPS syntax"""
        return str(self)
    def cltypename(self):
        """name of this type in CLIPS"""
        return "INTEGER"
ClipsIntegerType = type(Integer(0))

class Float(float):
    """extend a float for use with CLIPS"""
    def __repr__(self):
        return "<Float %s>" % float.__repr__(self)
    def __add__(self, o):
        return Float(float(self) + float(o))
    def __sub__(self, o):
        return Float(float(self) - float(o))
    def __mul__(self, o):
        return Float(float(self) * float(o))
    def __floordiv__(self, o):
        return Float(float(self) // float(o))
    def __truediv__(self, o):
        return Float(float(self) / float(o))
    def __div__(self, o):
        return Float(float(self) / float(o))
    def __pow__(self, o, m=None):
        if m is not None:
            return Float((float(self) ** float(o)) % float(m))
        return Float(float(self) ** float(o))
    def clrepr(self):
        """represent this Float for CLIPS"""
        return (_c.FLOAT, float(self))
    def clsyntax(self):
        """represent this Float as it would be in CLIPS syntax"""
        return str(self)
    def cltypename(self):
        """name of this type in CLIPS"""
        return "FLOAT"
ClipsFloatType = type(Float(0.0))

# 2) string types
class String(str):
    """extend a str for use with CLIPS"""
    def __repr__(self):
        return "<String %s>" % str.__repr__(self)
    def __add__(self, o):
        return String(str(self) + str(o))
    def clrepr(self):
        """represent this String for CLIPS"""
        return (_c.STRING, str(self))
    def clsyntax(self):
        """represent this String as it would be in CLIPS syntax"""
        return '"%s"' % str(self).replace("\\", "\\\\").replace('"', '\\"')
    def cltypename(self):
        """name of this type in CLIPS"""
        return "STRING"
ClipsStringType = type(String(""))

class Symbol(str):
    """extend a str for use with CLIPS as symbol"""
    def __repr__(self):
        return "<Symbol %s>" % str.__repr__(self)
    def __nonzero__(self):
        return bool(self not in ('FALSE', 'nil', ''))
    def __add__(self, o):
        return Symbol(str(self) + str(o))
    def clrepr(self):
        """represent this Symbol for CLIPS"""
        return (_c.SYMBOL, str(self))
    def clsyntax(self):
        """represent this Symbol as it would be in CLIPS syntax"""
        return str(self)
    def cltypename(self):
        """name of this type in CLIPS"""
        return "SYMBOL"
ClipsSymbolType = type(Symbol(""))

class InstanceName(str):
    """extend a str for use with CLIPS as instance name"""
    def __repr__(self):
        return "<InstanceName %s>" % str.__repr__(self)
    def __add__(self, o):
        return InstanceName(str(self) + str(o))
    def clrepr(self):
        """represent this InstanceName for CLIPS"""
        return (_c.INSTANCE_NAME, str(self))
    def clsyntax(self):
        """represent this InstanceName as it would be in CLIPS syntax"""
        return "[%s]" % str(self)
    def cltypename(self):
        """name of this type in CLIPS"""
        return "INSTANCE-NAME"
ClipsInstanceNameType = type(InstanceName(""))

# a Nil object that might be useful in comparisons and assignments: after
#  its creation the constructor is no longer necessary and is later deleted
class NilObject(Symbol):
    """represent the CLIPS nil symbol"""
    __created = False
    def __init__(self):
        if self.__created:
            raise TypeError("Nil object cannot be created")
        _NilObject__created = True
    def __repr__(self):
        return "<Nil>"
    def __str__(self):
        return "nil"
    def __eq__(self, o):
        return o is self or o == Symbol("nil")
    def __ne__(self, o):
        return o is not self and o != Symbol("nil")
    def __nonzero__(self):
        return False
    def clrepr(self):
        """represent the nil symbol for CLIPS"""
        return (_c.SYMBOL, "nil")
    def clsyntax(self):
        """represent Nil as it would be in CLIPS syntax"""
        return "nil"
    def cltypename(self):
        """name of this type in CLIPS"""
        return "SYMBOL"
Nil = NilObject()
ClipsNilType = type(Nil)
del NilObject

# the multifield type is a little bit more complex, since a list
#  can contain elements of various types: at conversion time we must
#  check that all elements are suitable for building a multivalue
class Multifield(list):
    """extend a list for use with CLIPS as Multifield value"""
    def __repr__(self):
        return "<Multifield %s>" % list.__repr__(self)
    def __add__(self, o):
        return Multifield(list(self) + list(o))
    def clrepr(self):
        """represent this Multifield for CLIPS"""
        li = []
        for x in self:
            t = type(x)
            if t in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                     ClipsSymbolType, ClipsNilType, ClipsInstanceNameType):
                li.append(x.clrepr())
            elif t in (int, long):
                li.append(Integer(x).clrepr())
            elif t == float:
                li.append(Float(x).clrepr())
            elif t in (str, unicode):
                li.append(String(x).clrepr())
            elif isinstance(x, int):
                li.append(Integer(x).clrepr())
            elif isinstance(x, long):
                li.append(Integer(x).clrepr())
            elif isinstance(x, float):
                li.append(Float(x).clrepr())
            elif isinstance(x, str):
                li.append(String(x).clrepr())
            elif isinstance(x, unicode):
                li.append(String(x).clrepr())
            else:
                raise TypeError(
                    "list element of type %s cannot be converted" % t)
        return (_c.MULTIFIELD, li)
    def clsyntax(self):
        """represent this Multifield as it would be in CLIPS syntax"""
        li = []
        for x in self:
            t = type(x)
            if t in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                     ClipsSymbolType, ClipsNilType, ClipsInstanceNameType):
                li.append(x.clsyntax())
            elif t in (int, long):
                li.append(Integer(x).clsyntax())
            elif t == float:
                li.append(Float(x).clsyntax())
            elif t in (str, unicode):
                li.append(String(x).clsyntax())
            elif isinstance(x, int):
                li.append(Integer(x).clsyntax())
            elif isinstance(x, long):
                li.append(Integer(x).clsyntax())
            elif isinstance(x, float):
                li.append(Float(x).clsyntax())
            elif isinstance(x, str):
                li.append(String(x).clsyntax())
            elif isinstance(x, unicode):
                li.append(String(x).clsyntax())
            else:
                raise TypeError(
                    "list element of type %s cannot be converted" % t)
        return "(create$ %s)" % " ".join(li)    # only createable via this
    def cltypename(self):
        """name of this type in CLIPS"""
        return "MULTIFIELD"
ClipsMultifieldType = type(Multifield([]))


# ========================================================================== #
# NOTICE:
#  as of version 1.0.6 (incremental 331) we use the special markers around
#  _cl2py and _py2cl, namely #{{FUNCTION and #}} in order to publish the
#  above functions to the Environment class and allow passing and returning
#  Fact and Instance objects from the respective pointers
# ========================================================================== #

# Converter from internal form (type, value) of CLIPS data to the
#  wrappers provided above, in order to simplify transparent conversions
#{{FUNCTION
def _cl2py(o):
    """convert a well-formed tuple to one of the CLIPS wrappers"""
    if o is None: return None
    elif type(o) == tuple and len(o) == 2:
        if o[0] == _c.INTEGER:
            return Integer(o[1])
        elif o[0] == _c.FLOAT:
            return Float(o[1])
        elif o[0] == _c.STRING:
            return String(o[1])
        elif o[0] == _c.INSTANCE_NAME:
            return InstanceName(o[1])
        elif o[0] == _c.SYMBOL:
            if o[1] == "nil":
                return Nil
            else:
                return Symbol(o[1])
        elif o[0] == _c.INSTANCE_ADDRESS:
            return Instance(o[1])
        elif o[0] == _c.FACT_ADDRESS:
            return Fact(o[1])
        elif o[0] == _c.MULTIFIELD:
            li = []
            for (x, v) in o[1]:
                if x == _c.INTEGER:
                    li.append(Integer(v))
                elif x == _c.FLOAT:
                    li.append(Float(v))
                elif x == _c.STRING:
                    li.append(String(v))
                elif x == _c.SYMBOL:
                    li.append(Symbol(v))
                elif x == _c.INSTANCE_NAME:
                    li.append(InstanceName(v))
                elif x == _c.INSTANCE_ADDRESS:
                    li.append(Instance(v))
                elif x == _c.FACT_ADDRESS:
                    li.append(Fact(v))
                else:
                    raise TypeError("list cannot be converted")
            return Multifield(li)
        else:
            raise TypeError("malformed tuple value")
    else:
        raise TypeError("wrong argument type")
#}}

# same as above, but from Python to CLIPS
#{{FUNCTION
def _py2cl(o):
    """convert Python data to a well-formed tuple"""
    t1 = type(o)
    if t1 in (int, long):
        return (_c.INTEGER, int(o))
    elif t1 == float:
        return (_c.FLOAT, float(o))
    elif t1 in (str, unicode):
        return (_c.STRING, str(o))
    elif t1 in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                ClipsSymbolType, ClipsInstanceNameType, ClipsNilType,
                ClipsMultifieldType):
        return o.clrepr()
    elif t1 == Fact:
        return (_c.FACT_ADDRESS, o._Fact__fact)
    elif t1 == Instance:
        return (_c.INSTANCE_ADDRESS, o._Instance__instance)
    elif isinstance(o, int):
        return (_c.INTEGER, int(o))
    elif isinstance(o, long):
        return (_c.INTEGER, int(o))
    elif isinstance(o, float):
        return (_c.FLOAT, float(o))
    elif isinstance(o, str):
        return (_c.STRING, str(o))
    elif isinstance(o, unicode):
        return (_c.STRING, str(o))
    elif t1 in (list, tuple):
        li = []
        for x in o:
            t0 = type(x)
            if t0 in (int, long):
                li.append((_c.INTEGER, int(x)))
            elif t0 == float:
                li.append((_c.FLOAT, float(x)))
            elif t0 in (str, unicode):
                li.append((_c.STRING, str(x)))
            elif t0 in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                        ClipsSymbolType, ClipsInstanceNameType, ClipsNilType):
                li.append(x.clrepr())
            elif t0 == Fact:
                li.append((_c.FACT_ADDRESS, o._Fact__fact))
            elif t0 == Instance:
                li.append((_c.INSTANCE_ADDRESS, o._Instance__instance))
            elif isinstance(x, int):
                li.append((_c.INTEGER, int(o)))
            elif isinstance(x, long):
                li.append((_c.INTEGER, int(o)))
            elif isinstance(x, float):
                li.append((_c.FLOAT, float(o)))
            elif isinstance(x, str):
                li.append((_c.STRING, str(o)))
            elif isinstance(x, unicode):
                li.append((_c.STRING, str(o)))
            else:
                raise TypeError(
                    "list element of type %s cannot be converted" % t0)
        return (_c.MULTIFIELD, li)
    else:
        raise TypeError("value of type %s cannot be converted" % t1)
#}}

# convert a Python value to what the python value would be in CLIPS syntax
def _py2clsyntax(o):
    """convert Python data to CLIPS syntax"""
    t1 = type(o)
    if t1 in (int, long):
        return Integer(int(o)).clsyntax()
    elif t1 == float:
        return Float(o).clsyntax()
    elif t1 in (str, unicode):
        return String(o).clsyntax()
    elif t1 in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                ClipsSymbolType, ClipsInstanceNameType, ClipsNilType,
                ClipsMultifieldType):
        return o.clsyntax()
    elif isinstance(o, int):
        return Integer(int(o)).clsyntax()
    elif isinstance(o, long):
        return Integer(int(o)).clsyntax()
    elif isinstance(o, float):
        return Float(o).clsyntax()
    elif isinstance(o, str):
        return String(o).clsyntax()
    elif t1 in (list, tuple):
        li = []
        for x in o:
            t0 = type(x)
            if t0 in (int, long):
                li.append(Integer(int(x)).clsyntax())
            elif t0 == float:
                li.append(Float(x).clsyntax())
            elif t0 == str:
                li.append(String(x).clsyntax())
            elif t0 in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                        ClipsSymbolType, ClipsInstanceNameType, ClipsNilType):
                li.append(x.clsyntax())
            elif isinstance(x, int):
                li.append(Integer(int(x)).clsyntax())
            elif isinstance(x, long):
                li.append(Integer(int(x)).clsyntax())
            elif isinstance(x, float):
                li.append(Float(x).clsyntax())
            elif isinstance(x, str):
                li.append(String(x).clsyntax())
            elif isinstance(x, unicode):
                li.append(String(x).clsyntax())
            else:
                raise TypeError(
                    "list element of type %s cannot be converted" % t0)
        return Multifield(li).clsyntax()
    else:
        raise TypeError("value of type %s cannot be converted" % t1)



# ========================================================================== #
# NOTICE:
#  as of version 1.0.5 (incremental 324) every class that should also appear
#  in the environment-aware submodule must be surrounded by special comments,
#  namely '#{{CLASS' and '#}}'; the same has to be done for functions: the
#  surrounding comments in this case are '#{{FUNCTION' and '#}}'; this allows
#  the setup process to be more readable and avoid certain 'tricks'
# ========================================================================== #


# ========================================================================== #
# 0.1) Status functions and classes - as of APG section 4.1

# as we did above, we group all the status functions under a class and then
#  create a single instance of the class itself prohibiting further instances
#{{CLASS
class _clips_Status(object):
    """object to access global status functions"""

    __created = False

    def __init__(self):
        """raise an exception if an object of this type has been created"""
        if(self.__created):
            raise TypeError("cannot create this object twice")
        self.__created = True

    def __repr__(self):
        return "<Configuration Management Object>"

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle engine status")

    def __property_setFactDuplication(self, v):
        _c.setFactDuplication(v)
    def __property_getFactDuplication(self, v):
        return bool(_c.getFactDuplication())
    FactDuplication = property(
        __property_getFactDuplication,
        __property_setFactDuplication,
        None, "Fact duplication behaviour")

    def __property_setAutoFloatDividend(self, v):
        _c.setAutoFloatDividend(v)
    def __property_getAutoFloatDividend(self):
        return bool(_c.getAutoFloatDividend())
    AutoFloatDividend = property(
        __property_getAutoFloatDividend,
        __property_setAutoFloatDividend,
        None, "AutoFloatDividend behaviour")

    def __property_setDynamicConstraintChecking(self, v):
        _c.setDynamicConstraintChecking(v)
    def __property_getDynamicConstraintChecking(self):
        return bool(_c.getDynamicConstraintChecking())
    DynamicConstraintChecking = property(
        __property_getDynamicConstraintChecking,
        __property_setDynamicConstraintChecking,
        None, "Dynamic constraint checking behaviour")

    def __property_setSequenceOperatorRecognition(self, v):
        _c.setSequenceOperatorRecognition(v)
    def __property_getSequenceOperatorRecognition(self):
        return bool(_c.getSequenceOperatorRecognition())
    SequenceOperatorRecognition = property(
        __property_getSequenceOperatorRecognition,
        __property_setSequenceOperatorRecognition,
        None, "Sequence operator recognition behaviour")

    def __property_setStaticConstraintChecking(self, v):
        _c.setStaticConstraintChecking(v)
    def __property_getStaticConstraintChecking(self):
        return bool(_c.getStaticConstraintChecking())
    StaticConstraintChecking = property(
        __property_getStaticConstraintChecking,
        __property_setStaticConstraintChecking,
        None, "Static constraint checking behaviour")

    def __property_setIncrementalReset(self, v):
        _c.setIncrementalReset(v)
    def __property_getIncrementalReset(self):
        return bool(_c.getIncrementalReset())
    IncrementalReset = property(
        __property_getIncrementalReset,
        __property_setIncrementalReset,
        None, "Incremental reset behaviour")

    def __property_setResetGlobals(self, v):
        _c.setResetGlobals(v)
    def __property_getResetGlobals(self):
        return bool(_c.getResetGlobals())
    ResetGlobals = property(
        __property_getResetGlobals,
        __property_setResetGlobals,
        None, "ResetGlobals behaviour")

    def __property_setStrategy(self, v):
        _c.setStrategy(v)
    def __property_getStrategy(self):
        return _c.getStrategy()
    Strategy = property(
        __property_getStrategy,
        __property_setStrategy,
        None, "strategy behaviour")

    def __property_setSalienceEvaluation(self, v):
        _c.setSalienceEvaluation(v)
    def __property_getSalienceEvaluation(self):
        return _c.getSalienceEvaluation()
    SalienceEvaluation = property(
        __property_getSalienceEvaluation,
        __property_setSalienceEvaluation,
        None, "salience evaluation behaviour")

    def __property_setClassDefaultsMode(self, v):
        _c.setClassDefaultsMode(v)
    def __property_getClassDefaultsMode(self):
        return _c.getClassDefaultsMode()
    ClassDefaultsMode = property(
        __property_getClassDefaultsMode,
        __property_setClassDefaultsMode,
        None, "class defaults mode")
#}}



# ========================================================================== #
# 0.2) Debugging functions and classes - as of APG section 4.2

# we group all debugging function under a class, then we prohibit
#  creation of items of that class but provide an object able to
#  access debugging status and to toggle debugging features
#{{CLASS
class _clips_Debug(object):
    """object to enable/disable debugging features"""

    __created = False

    def __init__(self):
        """one-time initializer"""
        if(self.__created):
            raise TypeError("cannot create this object twice")
        self.__created = True
        self.__watchitems = ['facts', 'rules', 'activations', 'compilations',
                             'statistics', 'globals', 'slots', 'instances',
                             'messages', 'message-handlers',
                             'generic-functions', 'methods', 'deffunctions',]
        # the following would modify the engine status on instantiation,
        #  which would disallow storing current environment for swapping
        ##for x in self.__watchitems: _c.unwatch(x)
        ##_c.dribbleOff()

    def __repr__(self):
        return "<Debug Management Object>"

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle debug status")

    def DribbleOn(self, fn):
        """enable dribble on given file"""
        _c.dribbleOn(fn)
    def DribbleOff(self):
        """turn off dribble"""
        _c.dribbleOff()
    def DribbleActive(self):
        """tell whether or not dribble is active"""
        return bool(_c.dribbleActive())

    def __property_setFactsWatched(self, v):
        if(v):
            _c.watch("facts")
        else:
            _c.unwatch("facts")
    def __property_getFactsWatched(self):
        return bool(_c.getWatchItem("facts"))
    FactsWatched = property(__property_getFactsWatched,
                            __property_setFactsWatched,
                            None, "Facts watch status")

    def __property_setRulesWatched(self, v):
        if(v):
            _c.watch("rules")
        else:
            _c.unwatch("rules")
    def __property_getRulesWatched(self):
        return bool(_c.getWatchItem("rules"))
    RulesWatched = property(__property_getRulesWatched,
                            __property_setRulesWatched,
                            None, "Rules watch status")

    def __property_setActivationsWatched(self, v):
        if(v):
            _c.watch("activations")
        else:
            _c.unwatch("activations")
    def __property_getActivationsWatched(self):
        return bool(_c.getWatchItem("activations"))
    ActivationsWatched = property(__property_getActivationsWatched,
                                  __property_setActivationsWatched,
                                  None, "Activations watch status")

    def __property_setCompilationsWatched(self, v):
        if(v):
            _c.watch("compilations")
        else:
            _c.unwatch("compilations")
    def __property_getCompilationsWatched(self):
        return bool(_c.getWatchItem("compilations"))
    CompilationsWatched = property(__property_getCompilationsWatched,
                                   __property_setCompilationsWatched,
                                   None, "compilations watch status")

    def __property_setStatisticsWatched(self, v):
        if(v):
            _c.watch("statistics")
        else:
            _c.unwatch("statistics")
    def __property_getStatisticsWatched(self):
        return bool(_c.getWatchItem("statistics"))
    StatisticsWatched = property(__property_getStatisticsWatched,
                                 __property_setStatisticsWatched,
                                 None, "statistics watch status")

    def __property_setGlobalsWatched(self, v):
        if(v):
            _c.watch("globals")
        else:
            _c.unwatch("globals")
    def __property_getGlobalsWatched(self):
        return bool(_c.getWatchItem("globals"))
    GlobalsWatched = property(__property_getGlobalsWatched,
                              __property_setGlobalsWatched,
                              None, "Globals watch status")

    def __property_setSlotsWatched(self, v):
        if(v):
            _c.watch("slots")
        else:
            _c.unwatch("slots")
    def __property_getSlotsWatched(self):
        return bool(_c.getWatchItem("slots"))
    SlotsWatched = property(__property_getSlotsWatched,
                            __property_setSlotsWatched,
                            None, "Slots watch status")

    def __property_setMessagesWatched(self, v):
        if(v):
            _c.watch("messages")
        else:
            _c.unwatch("messages")
    def __property_getMessagesWatched(self):
        return bool(_c.getWatchItem("messages"))
    MessagesWatched = property(__property_getMessagesWatched,
                               __property_setMessagesWatched,
                               None, "messages watch status")

    def __property_setMessageHandlersWatched(self, v):
        if(v):
            _c.watch("message-handlers")
        else:
            _c.unwatch("message-handlers")
    def __property_getMessageHandlersWatched(self):
        return bool(_c.getWatchItem("message-handlers"))
    MessageHandlersWatched = property(__property_getMessageHandlersWatched,
                                      __property_setMessageHandlersWatched,
                                      None, "MessageHandlers watch status")

    def __property_setGenericFunctionsWatched(self, v):
        if(v):
            _c.watch("generic-functions")
        else:
            _c.unwatch("generic-functions")
    def __property_getGenericFunctionsWatched(self):
        return bool(_c.getWatchItem("generic-functions"))
    GenericFunctionsWatched = property(__property_getGenericFunctionsWatched,
                                       __property_setGenericFunctionsWatched,
                                       None, "Generic functions watch status")

    def __property_setMethodsWatched(self, v):
        if(v):
            _c.watch("methods")
        else:
            _c.unwatch("methods")
    def __property_getMethodsWatched(self):
        return bool(_c.getWatchItem("methods"))
    MethodsWatched = property(__property_getMethodsWatched,
                              __property_setMethodsWatched,
                              None, "Methods watch status")

    def __property_setFunctionsWatched(self, v):
        if(v):
            _c.watch("deffunctions")
        else:
            _c.unwatch("deffunctions")
    def __property_getFunctionsWatched(self):
        return bool(_c.getWatchItem("deffunctions"))
    FunctionsWatched = property(__property_getFunctionsWatched,
                                __property_setFunctionsWatched,
                                None, "Deffunctions watch status")

    def __property_setExternalTraceback(self, v):
        _c.setPrintExternalTraceback(bool(v))
    def __property_getExternalTraceback(self):
        return bool(_c.getPrintExternalTraceback())
    ExternalTraceback = property(__property_getExternalTraceback,
                                 __property_setExternalTraceback,
                                 None,
                                 "traceback of Python functions in CLIPS")

    def WatchAll(self):
        """watch all items"""
        for x in self.__watchitems:
            _c.watch(x)
    def UnwatchAll(self):
        """unwatch all items"""
        for x in self.__watchitems:
            _c.unwatch(x)
#}}



# ========================================================================== #
# High-level class for deftemplate objects
# Treat a deftemplate as an object having an Object-Oriented interface.
#  Implements all the functions needed to access deftemplate objects.
#{{CLASS
class Template(object):
    """high-level Template class (represents: deftemplate)"""

    def __init__(self, o=None):
        """create a Template object (internal)"""
        if _c.isDeftemplate(o):
            self.__deftemplate = o
        else:
            raise _c.ClipsError("M01: cannot directly create Template")

        class __template_Slots:
            """define a structure for Class Slots"""
            def __init__(self, o):
                self.__deftemplate = o
            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle template slots")

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def AllowedValues(self, name):
                """return allowed values for specified Slot"""
                rv = _cl2py(
                    _c.deftemplateSlotAllowedValues(self.__deftemplate, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Cardinality(self, name):
                """return cardinality for specified Slot"""
                rv = _cl2py(
                    _c.deftemplateSlotCardinality(self.__deftemplate, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def HasDefault(self, name):
                """one of NO_DEFAULT, STATIC_DEFAULT or DYNAMIC_DEFAULT"""
                return _c.deftemplateSlotDefaultP(self.__deftemplate, name)

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def DefaultValue(self, name):
                """return default value for specified Slot"""
                rv = _cl2py(
                    _c.deftemplateSlotDefaultValue(self.__deftemplate, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Exists(self, name):
                """return True if specified Slot exists"""
                return bool(
                    _c.deftemplateSlotExistP(self.__deftemplate, name))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def IsMultifield(self, name):
                """return True if specified Slot is a multifield one"""
                return bool(
                    _c.deftemplateSlotMultiP(self.__deftemplate, name))

            def Names(self):
                """return the list of Slot names"""
                rv = _cl2py(_c.deftemplateSlotNames(self.__deftemplate))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Range(self, name):
                """return numeric range information of specified Slot"""
                rv = _cl2py(_c.deftemplateSlotRange(self.__deftemplate, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def IsSinglefield(self, name):
                """return True if specified Slot is a single field one"""
                return bool(
                    _c.deftemplateSlotSingleP(self.__deftemplate, name))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Types(self, name):
                """return names of primitive types for specified Slot"""
                rv = _cl2py(_c.deftemplateSlotTypes(self.__deftemplate, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

        self.__Slots = __template_Slots(self.__deftemplate)
        try:
            self.__Slots._template_Slots__env = self.__env
        except AttributeError: pass
        try:
            self.__Slots._template_Slots__envobject = self.__envobject
        except AttributeError: pass

    def __str__(self):
        """string form of Template"""
        return _c.getDeftemplateName(self.__deftemplate)

    def __repr__(self):
        """representation of Template"""
        s = repr(self.__deftemplate)[1:-1]
        return "<Template '%s': %s>" % (
            _c.getDeftemplateName(self.__deftemplate), s)

    def __getstate__(self):
        raise _c.ClipsError(
            "M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Template"""
        o = _c.getNextDeftemplate(self.__deftemplate)
        if(o):
            return Template(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Template"""
        return _c.getDeftemplatePPForm(self.__deftemplate)

    def Remove(self):
        """remove Template"""
        _c.undeftemplate(self.__deftemplate)

    def BuildFact(self):
        """create a fact from this Template without asserting it"""
        return Fact(self.__deftemplate)

    def InitialFact(self):
        """find initial Fact for this Template"""
        return Fact(_c.getNextFactInTemplate(self.__deftemplate))

    def NextFact(self, fact):
        """find initial Fact for this Template"""
        return Fact(
            _c.getNextFactInTemplate(self.__deftemplate, fact._Fact__fact))

    def __property_getDeletable(self):
        return bool(_c.isDeftemplateDeletable(self.__deftemplate))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Template can be deleted")

    def __property_getName(self):
        return Symbol(_c.getDeftemplateName(self.__deftemplate))
    Name = property(__property_getName, None, None, "retrieve Template name")

    def __property_getModule(self):
        return Symbol(_c.deftemplateModule(self.__deftemplate))
    Module = property(__property_getModule, None, None,
                      "retrieve Template Module name")

    # access class slots through the internal object
    def __property_getSlots(self): return self.__Slots
    Slots = property(__property_getSlots, None, None,
                     "Template Slots information")

    # debugging functions and properties
    def __property_setWatch(self, v):
        _c.setDeftemplateWatch(v, self.__deftemplate)
    def __property_getWatch(self):
        return _c.getDeftemplateWatch(self.__deftemplate)
    Watch = property(__property_getWatch, __property_setWatch,
                     None, "watch status of this Template")
#}}



# ========================================================================== #
# High-level class for fact objects
# Treat a fact as an object having an Object-Oriented interface. All functions
#  that normally refer to a fact use the underlying low-level fact object to
#  interact with the system.
#{{CLASS
class Fact(object):
    """high-level Fact class (represents: fact)"""

    # class constructor - we want to initialize the fact in several ways, ie.
    #  by creation (using a Template or its underlying __deftemplate), by
    #  copy (using a Fact or its underlying __fact), or by assertion using a
    #  string; besides this, we also want to initialize some internal structs
    #  that help use the fact "the Python way" (eg. the fact slots should be
    #  grouped in a string-addressed dictionary, as it would naturally be)
    def __init__(self, o):
        """create a Fact object"""
        # this on-the-fly class takes the underlying fact object, which
        #  should already exist, and accesses its slots through the functions
        #  provided by the low-level module, thus exposing a dictionary-like
        #  interface that can be used to access slots at high level
        #  NOTE: there is a hack that allows the environment version to work
        #        by trying to access the underlying environment object
        class __fact_Slots:
            """access fact Slots"""
            def __init__(self, fo):
                self.__fact = fo

            @_accepts_method((str, unicode), None)
            @_forces_method(str, None)
            def __setitem__(self, name, v):
                _c.putFactSlot(self.__fact, name, _py2cl(v))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def __getitem__(self, name):
                if not name:
                    return _cl2py(_c.getFactSlot(self.__fact))
                else:
                    return _cl2py(_c.getFactSlot(self.__fact, name))

            def keys(self):
                return _cl2py(_c.factSlotNames(self.__fact))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def has_key(self, k):
                return k in map(str, _cl2py(_c.factSlotNames(self.__fact)))

            def __repr__(self):
                return "<Fact '%s' Slots>" \
                       % _c.getFactPPForm(self.__fact).split()[0]

            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle fact slots")

        # now we can instance an object of this kind, and throw the class
        #  away; however the instance must be created at the end of function
        #  body since the fact has to be created at lower level
        if _c.isFact(o):
            self.__fact = o
        elif '_Fact__fact' in dir(o) and _c.isFact(o.__fact):
            self.__fact = o.__fact
        elif _c.isDeftemplate(o):
            self.__fact = _c.createFact(o)
        elif '_Template__deftemplate' in dir(o) and \
           _c.isDeftemplate(o._Template__deftemplate):
            self.__fact = _c.createFact(o._Template__deftemplate)
        elif type(o) == str:
            try:
                self.__fact = _c.assertString(o)
            except:
                raise ValueError("invalid assertion string")
        else:
            raise TypeError("argument should be Fact, Template or str")
        # here the fact is created: we create an instance of it and do not
        #  care about internal class definition destiny, since it's useful
        #  that this class definition disappears from Fact dictionary
        self.__Slots = __fact_Slots(self.__fact)
        try:
            self.__Slots._fact_Slots__env = self.__env
        except AttributeError: pass
        try:
            self.__Slots._fact_Slots__envobject = self.__envobject
        except AttributeError: pass

    def __str__(self):
        """string form of Fact"""
        return _c.getFactPPForm(self.__fact).split()[0]

    def __repr__(self):
        """representation of Fact"""
        s = repr(self.__fact)[1:-1]
        return "<Fact '%s': %s>" % (
            _c.getFactPPForm(self.__fact).split()[0], s)

    def __getstate__(self):
        raise _c.ClipsError(
            "M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # interface
    def Assert(self):
        """assert this Fact"""
        self.__fact = _c.assertFact(self.__fact)

    def Retract(self):
        """retract this Fact"""
        _c.retract(self.__fact)

    def AssignSlotDefaults(self):
        """assign Fact Slot defaults"""
        _c.assignFactSlotDefaults(self.__fact)

    def Next(self):
        """return next Fact"""
        o = _c.getNextFact(self.__fact)
        if(o):
            return Fact(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Fact"""
        return _c.getFactPPForm(self.__fact)

    def PPrint(self, ignoredefaults=True):
        """pretty-print fact, possibly including slot default values"""
        _c.routerClear("temporary")
        _c.ppFact(self.__fact, "temporary", ignoredefaults)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def CleanPPForm(self):
        """return the pretty-print form of Fact"""
        return _c.getFactPPForm(self.__fact).split(None, 1)[1].strip()

    # return the relation field
    def __property_getRelation(self):
        return Symbol(
            _c.getFactPPForm(self.__fact).split(
                None, 1)[1].strip()[1:-1].split(None, 1)[0])
    Relation = property(__property_getRelation, None, None,
                        "fact relation symbol")

    # the list of implied slots
    def __property_getImpliedSlots(self):
        try:
            mli = _cl2py(_c.getFactSlot(self.__fact))
        except:
            mli = Multifield([])
        return mli
    ImpliedSlots = property(__property_getImpliedSlots, None, None,
                            "list of implied Slots")

    # access fact index, read only property
    def __property_getIndex(self):
        return _c.factIndex(self.__fact)
    Index = property(__property_getIndex, None, None, "index of this Fact")

    # access fact slots through the internal object
    def __property_getSlots(self):
        return self.__Slots
    Slots = property(__property_getSlots, None, None,
                     """Fact Slots dictionary""")

    # access Template of this Fact, read only property
    def __property_getTemplate(self):
        return Template(_c.factDeftemplate(self.__fact))
    Template = property(__property_getTemplate, None, None,
                        """Template for this Fact""")

    # tell whether or not this Fact has been retracted (if asserted)
    def __property_getExists(self):
        return bool(_c.factExistp(self.__fact))
    Exists = property(__property_getExists, None, None,
                      "determine if Fact has been asserted and not retracted")
#}}



# ========================================================================== #
# High-level class for deffacts objects
# Treat a deffacts as an object having an Object-Oriented interface.
#  Implements all the functions needed to access deffacts objects.
#{{CLASS
class Deffacts(object):
    """high-level Deffacts class (represents: deffacts)"""

    def __init__(self, o):
        """create a Deffacts object (internal)"""
        if _c.isDeffacts(o):
            self.__deffacts = o
        else:
            raise _c.ClipsError("M01: cannot directly create Deffacts")

    def __str__(self):
        """string form of Deffacts"""
        return _c.getDeffactsName(self.__deffacts)

    def __repr__(self):
        """representation of Deffacts"""
        s = repr(self.__deffacts)[1:-1]
        return "<Deffacts '%s': %s>" % (
            _c.getDeffactsName(self.__deffacts), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
              % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Deffacts"""
        o = _c.getNextDeffacts(self.__deffacts)
        if(o):
            return Deffacts(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Deffacts"""
        return _c.getDeffactsPPForm(self.__deffacts)

    def Remove(self):
        """remove Deffacts"""
        _c.undeffacts(self.__deffacts)

    def __property_getName(self):
        return Symbol(_c.getDeffactsName(self.__deffacts))
    Name = property(__property_getName, None, None, "retrieve Deffacts name")

    def __property_getModule(self):
        return Symbol(_c.deffactsModule(self.__deffacts))
    Module = property(__property_getModule, None, None,
                      "retrieve Deffacts Module name")

    def __property_getDeletable(self):
        return bool(_c.isDeffactsDeletable(self.__deffacts))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Deffacts can be deleted")
#}}



# ========================================================================== #
# High-level class for defrule objects
# Treat a defrule as an object having an Object-Oriented interface.
#  Implements all the functions needed to access defrule objects.
#{{CLASS
class Rule(object):
    """high-level Rule class (represents: defrule)"""

    def __init__(self, o):
        """create a Rule object (internal)"""
        if _c.isDefrule(o):
            self.__defrule = o
        else:
            raise _c.ClipsError("M01: cannot directly create Rule")

    def __str__(self):
        """string form of Rule"""
        return _c.getDefruleName(self.__defrule)

    def __repr__(self):
        """representation of Rule"""
        s = repr(self.__defrule)[1:-1]
        return "<Rule '%s': %s>" % (
            _c.getDefruleName(self.__defrule), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Rule"""
        o = _c.getNextDefrule(self.__defrule)
        if o:
            return Rule(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Rule"""
        return _c.getDefrulePPForm(self.__defrule)

    def Refresh(self):
        """refresh Rule"""
        _c.refresh(self.__defrule)

    def PrintMatches(self):
        """print partial matches to standard output"""
        _c.routerClear("temporary")
        _c.matches("temporary", self.__defrule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def Remove(self):
        """remove Rule"""
        _c.undefrule(self.__defrule)

    def __property_getName(self):
        return Symbol(_c.getDefruleName(self.__defrule))
    Name = property(__property_getName, None, None, "retrieve Rule name")

    def __property_getModule(self):
        return Symbol(_c.defruleModule(self.__defrule))
    Module = property(__property_getModule, None, None,
                      "retrieve Rule Module name")

    def __property_setBreak(self, v):
        if v:
            _c.setBreak(self.__defrule)
        else:
            if _c.defruleHasBreakpoint(self.__defrule):
                _c.removeBreak(self.__defrule)
    def __property_getBreak(self):
        return bool(_c.defruleHasBreakpoint(self.__defrule))
    Breakpoint = property(__property_getBreak, __property_setBreak,
                          None, "set or remove breakpoint from Rule")

    def __property_getDeletable(self):
        return bool(_c.isDefruleDeletable(self.__defrule))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Rule can be deleted")

    def __property_setWatchActivations(self, v):
        _c.setDefruleWatchActivations(self.__defrule, v)
    def __property_getWatchActivations(self):
        return bool(_c.getDefruleWatchActivations(self.__defrule))
    WatchActivations = property(__property_getWatchActivations,
                                __property_setWatchActivations,
                                None, "Rule Activations debug status")

    def __property_setWatchFirings(self, v):
        _c.setDefruleWatchFirings(self.__defrule, v)
    def __property_getWatchFirings(self):
        return bool(_c.getDefruleWatchFirings(self.__defrule))
    WatchFirings = property(__property_getWatchFirings,
                            __property_setWatchFirings,
                            None, "Rule firings debug status")
#}}



# ========================================================================== #
# High-level class for activation objects
# Treat an activation as an object having an Object-Oriented interface.
#  Implements all the functions needed to access activation objects.
#{{CLASS
class Activation(object):
    """high-level Activation class (represents: activation)"""

    def __init__(self, o):
        """create an Activation object (internal)"""
        if _c.isActivation(o):
            self.__activation = o
        else:
            raise _c.ClipsError("M01: cannot directly create Activation")

    def __str__(self):
        """string form of Activation"""
        return _c.getActivationName(self.__activation)

    def __repr__(self):
        """representation of Activation"""
        return "<Activation '%s'>" % _c.getActivationName(self.__activation)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Activation"""
        o = _c.getNextActivation(self.__activation)
        if o:
            return Activation(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Activation"""
        return _c.getActivationPPForm(self.__activation)

    def Remove(self):
        """remove this Activation"""
        _c.deleteActivation(self.__activation)

    def __property_getName(self):
        return Symbol(_c.getActivationName(self.__activation))
    Name = property(__property_getName, None, None,
                    "retrieve Activation name")

    def __property_setSalience(self, v):
        _c.setActivationSalience(self.__activation, v)
    def __property_getSalience(self):
        return _c.getActivationSalience(self.__activation)
    Salience = property(__property_getSalience, __property_setSalience,
                        None, "retrieve Activation salience")
#}}



# ========================================================================== #
# High-level class for defglobal objects
# Treat a defglobal as an object having an Object-Oriented interface.
#  Implements all the functions needed to access defglobal objects.
#{{CLASS
class Global(object):
    """high-level Global class (represents: defglobal)"""

    def __init__(self, o):
        """create a Global object (internal)"""
        if _c.isDefglobal(o):
            self.__defglobal = o
        else:
            raise _c.ClipsError("M01: cannot directly create Global")

    def __str__(self):
        """string form of Global"""
        return _c.getDefglobalName(self.__defglobal)

    def __repr__(self):
        """representation of Global"""
        s = repr(self.__defglobal)[1:-1]
        return "<Global '%s': %s>" % (
            _c.getDefglobalName(self.__defglobal), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Global"""
        o = _c.getNextDefglobal(self.__defglobal)
        if o:
            return Global(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Global"""
        return _c.getDefglobalPPForm(self.__defglobal)

    def ValueForm(self):
        """return a 'printed' form of Global value"""
        return _c.getDefglobalValueForm(self.__defglobal)

    def Remove(self):
        """remove this Global"""
        _c.undefglobal(self.__defglobal)

    def __property_getName(self):
        return Symbol(_c.getDefglobalName(self.__defglobal))
    Name = property(__property_getName, None, None, "retrieve Global name")

    def __property_getModule(self):
        return Symbol(_c.defglobalModule(self.__defglobal))
    Module = property(__property_getModule, None, None,
                      "retrieve Global Module name")

    def __property_setValue(self, v):
        _c.setDefglobalValue(self.Name, _py2cl(v))
    def __property_getValue(self):
        return _cl2py(_c.getDefglobalValue(self.Name))
    Value = property(__property_getValue, __property_setValue,
                     None, "set/retrieve Global value")

    def __property_getDeletable(self):
        return bool(_c.isDefglobalDeletable(self.__defglobal))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Global can be deleted")

    def __property_setWatch(self, v):
        _c.setDefglobalWatch(v, self.__defglobal)
    def __property_getWatch(self):
        return _c.getDefglobalWatch(self.__defglobal)
    Watch = property(__property_getWatch, __property_setWatch,
                     None, "set/retrieve Global debug status")
#}}



# ========================================================================== #
# High-level class for deffunction objects
# Treat a deffunction as an object having an Object-Oriented interface.
#  Implements all the functions needed to access deffunction objects.
#{{CLASS
class Function(object):
    """high-level Function class (represents: deffunction)"""

    def __init__(self, o):
        """create a Function object (internal)"""
        if _c.isDeffunction(o):
            self.__deffunction = o
        else:
            raise _c.ClipsError("M01: cannot directly create Function")

    def __str__(self):
        """string form of Function"""
        return _c.getDeffunctionName(self.__deffunction)

    def __repr__(self):
        """representation of Function"""
        s = repr(self.__deffunction)[1:-1]
        return "<Function '%s': %s>" % (
            _c.getDeffunctionName(self.__deffunction), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Function"""
        o = _c.getNextDeffunction(self.__deffunction)
        if o:
            return Function(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Function"""
        return _c.getDeffunctionPPForm(self.__deffunction)

    def Remove(self):
        """remove this Function"""
        _c.undeffunction(self.__deffunction)

    def Call(self, *args):
        """call this Function with given arguments"""
        func = _c.getDeffunctionName(self.__deffunction)
        if args:
            if(len(args) == 1 and type(args[0]) == str):
                sargs = args[0]
            else:
                li = []
                for x in args:
                    t1 = type(x)
                    if t1 in (ClipsIntegerType, ClipsFloatType,
                              ClipsStringType, ClipsSymbolType, ClipsNilType,
                              ClipsInstanceNameType, ClipsMultifieldType):
                        li.append(_py2clsyntax(x))
                    elif t1 in (int, long):
                        li.append(Integer(x).clsyntax())
                    elif t1 == float:
                        li.append(Float(x).clsyntax())
                    elif t1 in (str, unicode):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, int):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, long):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, float):
                        li.append(Float(x).clsyntax())
                    elif isinstance(x, str):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, unicode):
                        li.append(String(x).clsyntax())
                    else:
                        li.append(str(x))
                sargs = " ".join(li)
            return _cl2py(_c.functionCall(func, sargs))
        else:
            return _cl2py(_c.functionCall(func))
    __call__ = Call

    def __property_getName(self):
        return Symbol(_c.getDeffunctionName(self.__deffunction))
    Name = property(__property_getName, None, None, "retrieve Function name")

    def __property_getModule(self):
        return Symbol(_c.deffunctionModule(self.__deffunction))
    Module = property(__property_getModule, None, None,
                      "retrieve Function Module name")

    def __property_getDeletable(self):
        return bool(_c.isDeffunctionDeletable(self.__deffunction))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Function can be deleted")

    def __property_setWatch(self, v):
        _c.setDeffunctionWatch(v, self.__deffunction)
    def __property_getWatch(self):
        return bool(_c.getDeffunctionWatch(self.__deffunction))
    Watch = property(__property_getWatch, __property_setWatch,
                     None, "set/retrieve Function debug status")
#}}



# ========================================================================== #
# High-level class for defgeneric objects
# Treat a defgeneric as an object having an Object-Oriented interface.
#  Implements all the functions needed to access defgeneric objects.
#{{CLASS
class Generic(object):
    """high-level Generic class (represents: defgeneric)"""

    def __init__(self, o):
        """create a Generic function object (internal)"""
        if _c.isDefgeneric(o):
            self.__defgeneric = o
        else:
            raise _c.ClipsError("M01: cannot directly create Generic")

    def __str__(self):
        """string form of Generic"""
        return _c.getDefgenericName(self.__defgeneric)

    def __repr__(self):
        """representation of Generic"""
        s = repr(self.__defgeneric)[1:-1]
        return "<Generic '%s': %s>" % (
            _c.getDefgenericName(self.__defgeneric), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Generic"""
        o = _c.getNextDefgeneric(self.__defgeneric)
        if o:
            return Generic(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Generic"""
        return _c.getDefgenericPPForm(self.__defgeneric)

    def Remove(self):
        """remove this Generic"""
        _c.undefgeneric(self.__defgeneric)

    def Call(self, *args):
        """call this Generic with given arguments"""
        func = _c.getDefgenericName(self.__defgeneric)
        if args:
            if(len(args) == 1 and type(args[0]) in (str, unicode)):
                sargs = str(args[0])
            else:
                li = []
                for x in args:
                    t1 = type(x)
                    if t1 in (ClipsIntegerType, ClipsFloatType,
                              ClipsStringType, ClipsSymbolType, ClipsNilType,
                              ClipsInstanceNameType, ClipsMultifieldType):
                        li.append(_py2clsyntax(x))
                    elif t1 in (int, long):
                        li.append(Integer(int(x)).clsyntax())
                    elif t1 == float:
                        li.append(Float(x).clsyntax())
                    elif t1 in (str, unicode):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, int):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, long):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, float):
                        li.append(Float(x).clsyntax())
                    elif isinstance(x, str):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, unicode):
                        li.append(String(x).clsyntax())
                    else:
                        li.append(str(x))
                sargs = " ".join(li)
            return _cl2py(_c.functionCall(func, sargs))
        else:
            return _cl2py(_c.functionCall(func))
    __call__ = Call

    def __property_getName(self):
        return Symbol(_c.getDefgenericName(self.__defgeneric))
    Name = property(__property_getName, None, None, "retrieve Generic name")

    def __property_getModule(self):
        return Symbol(_c.defgenericModule(self.__defgeneric))
    Module = property(__property_getModule, None, None,
                      "retrieve Generic Module name")

    def __property_getDeletable(self):
        return bool(_c.isDefgenericDeletable(self.__defgeneric))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Generic can be deleted")

    def __property_setWatch(self, v):
        _c.setDefgenericWatch(v, self.__defgeneric)
    def __property_getWatch(self):
        return bool(_c.getDefgenericWatch(self.__defgeneric))
    Watch = property(__property_getWatch, __property_setWatch,
                     None, "set/retrieve Generic debug status")

    # Method functions
    def MethodList(self):
        """return the list of Method indices for this Generic"""
        o = _c.getDefmethodList(self.__defgeneric)
        li, mli = Multifield(_cl2py(o)), Multifield([])
        l = len(li) / 2
        for x in range(0, l):
            mli.append(li[2 * x + 1])
        return mli

    def MethodDescription(self, midx):
        """return the synopsis of specified Method restrictions"""
        return _c.getDefmethodDescription(midx, self.__defgeneric)

    def MethodPPForm(self, midx):
        """return the pretty-print form of specified Method"""
        return _c.getDefmethodPPForm(midx, self.__defgeneric)

    def MethodRestrictions(self, midx):
        """return the restrictions of specified Method"""
        return Multifield(
            _cl2py(_c.getMethodRestrictions(midx, self.__defgeneric)))

    def InitialMethod(self):
        """return the index of first Method in this Generic"""
        try:
            return _c.getNextDefmethod(0, self.__defgeneric)
        except:
            raise _c.ClipsError("M02: could not find any Method")

    def NextMethod(self, midx):
        """return the index of next Method in this Generic"""
        return _c.getNextDefmethod(midx, self.__defgeneric)

    def PrintMethods(self):
        """print out Method list for this Generic"""
        _c.routerClear("temporary")
        _c.listDefmethods("temporary", self.__defgeneric)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_accepts_method(None, None, (int, long), None)
    def AddMethod(self, restrictions, actions, midx=None, comment=None):
        """Add a method to this Generic, given restrictions and actions"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        if midx:
            indstr = str(midx)
        else:
            indstr = ""
        if type(restrictions) in (tuple, list):
            rstr = ""
            for x in restrictions:
                if type(x) not in (tuple, str, unicode):
                    raise TypeError("tuple or string expected as restriction")
                if type(x) == str:
                    rstr += "(%s)" % x
                elif type(x) == unicode:
                    rstr += "(%s)" % str(x)
                else:
                    if len(x) < 2:
                        raise ValueError("tuple must be at least a pair")
                    v1, v2 = str(x[0]), []
                    for y in range(1, len(x)):
                        z = x[y]
                        if z == str:
                            v2.append("STRING")
                        elif z == ClipsStringType:
                            v2.append("STRING")
                        elif z == ClipsSymbolType:
                            v2.append("SYMBOL")
                        elif z == ClipsInstanceNameType:
                            v2.append("INSTANCE-NAME")
                        elif z == int:
                            v2.append("INTEGER")
                        elif z == ClipsIntegerType:
                            v2.append("INTEGER")
                        elif z == float:
                            v2.append("FLOAT")
                        elif z == ClipsFloatType:
                            v2.append("FLOAT")
                        elif z == list:
                            v2.append("MULTIFIELD")
                        elif z == ClipsMultifieldType:
                            v2.append("MULTIFIELD")
                        elif type(z) == str:
                            v2.append(z)
                        elif type(z) == unicode:
                            v2.append(str(z))
                        else:
                            raise TypeError("unexpected value '%s'" % z)
                        rstr += "(%s %s)" % (v1, " ".join(v2))
        elif type(restrictions) == str:
            rstr = restrictions
        else:
            raise TypeError("tuple or string expected as restriction")
        _c.build("(defmethod %s %s %s (%s) %s)" % (
            self.Name, indstr, cmtstr, rstr, actions))

    def RemoveMethod(self, midx):
        """remove specified Method"""
        _c.undefmethod(midx, self.__defgeneric)

    # these are peculiar, since defmethods cannot be rendered as classes
    def WatchMethod(self, midx):
        """activate watch on specified Method"""
        _c.setDefmethodWatch(True, midx, self.__defgeneric)

    def UnwatchMethod(self, midx):
        """deactivate watch on specified Method"""
        _c.setDefmethodWatch(False, midx, self.__defgeneric)

    def MethodWatched(self, midx):
        """test whether or not specified Method is being watched"""
        return bool(_c.getDefmethodWatch(midx, self.__defgeneric))

    def MethodDeletable(self, midx):
        """test whether or not specified Method can be deleted"""
        return bool(_c.isDefmethodDeletable(midx, self.__defgeneric))
#}}



# ========================================================================== #
# High-level class for defclass objects
# Treat a defclass as an object having an Object-Oriented interface.
#  Implements all the functions needed to access defclass objects.
#{{CLASS
class Class(object):
    """high-level Class class (represents: defclass)"""

    def __init__(self, o):
        """create a Class object (internal)"""
        if _c.isDefclass(o):
            self.__defclass = o
        else:
            raise _c.ClipsError("M01: cannot directly create Class")
        # define a class to group slots information
        #  NOTE: there is a hack that allows the environment version to work
        #        by trying to access the underlying environment object
        class __class_Slots:
            """define a structure for Class Slots"""
            def __init__(self, o):
                self.__defclass = o

            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle class slots")

            def Names(self):
                """return the list of Slot names"""
                rv = _cl2py(_c.classSlots(self.__defclass, 1))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            def NamesDefined(self):
                """return the list of Slot names"""
                rv = _cl2py(_c.classSlots(self.__defclass, 0))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def AllowedValues(self, name):
                """return allowed values for specified Slot"""
                rv = _cl2py(_c.slotAllowedValues(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def AllowedClasses(self, name):
                """return allowed classes for specified Slot"""
                rv = _cl2py(_c.slotAllowedClasses(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Cardinality(self, name):
                """return cardinality for specified Slot"""
                rv = _cl2py(_c.slotCardinality(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def DefaultValue(self, name):
                """return default value for specified Slot"""
                rv = _cl2py(_c.slotDefaultValue(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Facets(self, name):
                """return facet values for specified Slot"""
                rv = _cl2py(_c.slotFacets(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Range(self, name):
                """return numeric range information of specified Slot"""
                rv = _cl2py(_c.slotRange(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Sources(self, name):
                """return source class names for specified Slot"""
                rv = _cl2py(_c.slotSources(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Types(self, name):
                """return names of primitive types for specified Slot"""
                rv = _cl2py(_c.slotTypes(self.__defclass, name))
                if type(rv) in (tuple, list):
                    return Multifield(rv)
                else:
                    return rv

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def HasDirectAccess(self, name):
                """return True if specified Slot is directly accessible"""
                return bool(_c.slotDirectAccessP(self.__defclass, name))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def Exists(self, name):
                """return True if specified Slot exists or is inherited"""
                return bool(_c.slotExistP(self.__defclass, name, 1))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def ExistsDefined(self, name):
                """return True if specified Slot is defined in this Class"""
                return bool(_c.slotExistP(self.__defclass, name, 0))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def IsInitable(self, name):
                """return True if specified Slot is initable"""
                return bool(_c.slotInitableP(self.__defclass, name))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def IsPublic(self, name):
                """return True if specified Slot is public"""
                return bool(_c.slotPublicP(self.__defclass, name))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def IsWritable(self, name):
                """return True if specified Slot is writable"""
                return bool(_c.slotWritableP(self.__defclass, name))

        self.__Slots = __class_Slots(self.__defclass)
        # the following try/except blocks are to enable companion versions
        try:
            self.__Slots._class_Slots__env = self.__env
        except AttributeError: pass
        try:
            self.__Slots._class_Slots__envobject = self.__envobject
        except AttributeError: pass

    def __str__(self):
        """string form of Class"""
        return _c.getDefclassName(self.__defclass)

    def __repr__(self):
        """representation of Class"""
        s = repr(self.__defclass)[1:-1]
        return "<Class '%s': %s>" % (
            _c.getDefclassName(self.__defclass), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Class"""
        o = _c.getNextDefclass(self.__defclass)
        if o:
            return Class(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Class"""
        return _c.getDefclassPPForm(self.__defclass)

    def Description(self):
        """return a summary of Class description"""
        _c.routerClear("temporary")
        _c.describeClass("temporary", self.__defclass)
        return _c.routerRead("temporary").strip()

    def IsSubclassOf(self, o):
        """test whether this Class is a subclass of specified Class"""
        return bool(_c.subclassP(self.__defclass, o.__defclass))

    def IsSuperclassOf(self, o):
        """test whether this Class is a superclass of specified Class"""
        return bool(_c.superclassP(self.__defclass, o.__defclass))

    def Subclasses(self, inherit=True):
        """return the names of subclasses"""
        return Multifield(
            _cl2py(_c.classSubclasses(self.__defclass, inherit)))

    def Superclasses(self, inherit=True):
        """return the names of superclasses"""
        return Multifield(
            _cl2py(_c.classSuperclasses(self.__defclass, inherit)))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def RawInstance(self, name):
        """create an empty Instance of this Class with specified name"""
        return Instance(_c.createRawInstance(self.__defclass, name))

    def InitialInstance(self):
        """return initial Instance of this Class"""
        try:
            return Instance(_c.getNextInstanceInClass(self.__defclass))
        except:
            raise _c.ClipsError("M02: could not find any Instance")

    def NextInstance(self, instance):
        """return next Instance of this Class"""
        i = _c.getNextInstanceInClass(
            self.__defclass, instance._Instance__instance)
        if _c.isInstance(i):
            return Instance(i)
        else:
            return None

    def InitialSubclassInstance(self):
        """return initial instance of this Class and subclasses"""
        try:
            return Instance(_c.getNextInstanceInClassAndSubclasses(
                self.__defclass))
        except:
            raise _c.ClipsError("M02: could not find any Instance")

    def NextSubclassInstance(self, instance):
        """return next instance of this Class and subclasses"""
        i = _c.getNextInstanceInClassAndSubclasses(
            self.__defclass, instance._Instance__instance)
        if _c.isInstance(i):
            return Instance(i)
        else:
            return None

    def Remove(self):
        """remove this Class"""
        _c.undefclass(self.__defclass)

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildSubclass(self, name, text="", comment=None):
        """build a subclass of this Class with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        clname = _c.getDefclassName(self.__defclass)
        cltext = "(is-a %s)" % clname + text
        construct = "(defclass %s %s %s)" % (name, cmtstr, cltext)
        _c.build(construct)
        return Class(_c.findDefclass(name))

    @_accepts_method((str, unicode), (str, unicode))
    @_forces_method(str, str)
    def BuildInstance(self, name, overrides=""):
        """build an instance of this class overriding specified slots"""
        clname = _c.getDefclassName(self.__defclass)
        cmdstr = "(%s of %s %s)" % (name, clname, overrides)
        return Instance(_c.makeInstance(cmdstr))

    def __property_getName(self):
        return Symbol(_c.getDefclassName(self.__defclass))
    Name = property(__property_getName, None, None, "retrieve Class name")

    def __property_getModule(self):
        return Symbol(_c.defclassModule(self.__defclass))
    Module = property(__property_getModule, None, None,
                      "retrieve Class Module name")

    def __property_getDeletable(self):
        return bool(_c.isDefclassDeletable(self.__defclass))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Class can be deleted")

    def __property_getAbstract(self):
        return bool(_c.classAbstractP(self.__defclass))
    Abstract = property(__property_getAbstract, None, None,
                        "verify if this Class is abstract or not")

    def __property_getReactive(self):
        return bool(_c.classReactiveP(self.__defclass))
    Reactive = property(__property_getReactive, None, None,
                        "verify if this Class is reactive or not")

    def __property_setWatchSlots(self, v):
        _c.setDefclassWatchSlots(v, self.__defclass)
    def __property_getWatchSlots(self):
        return bool(_c.getDefclassWatchSlots(self.__defclass))
    WatchSlots = property(__property_getWatchSlots, __property_setWatchSlots,
                          None, "set/retrieve Slot debug status")

    def __property_setWatchInstances(self, v):
        _c.setDefclassWatchInstances(v, self.__defclass)
    def __property_getWatchInstances(self):
        return bool(_c.getDefclassWatchInstances(self.__defclass))
    WatchInstances = property(__property_getWatchInstances,
                              __property_setWatchInstances,
                              None, "set/retrieve Instance debug status")

    # access class slots through the internal object
    def __property_getSlots(self): return self.__Slots
    Slots = property(__property_getSlots, None, None,
                     "Class Slots information")

    # message-handler functions
    @_accepts_method((str, unicode), (str, unicode), (str, unicode), None, None)
    @_forces_method(str, str, str, None, None)
    def AddMessageHandler(self, name, args, text, htype=PRIMARY, comment=None):
        """build a MessageHandler for this class with arguments and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        htype = htype.lower()
        if not htype in (AROUND, BEFORE, PRIMARY, AFTER):
            raise ValueError("htype must be AROUND, BEFORE, PRIMARY or AFTER")
        if type(args) in (tuple, list):
            sargs = " ".join(args)
        elif args is None:
            sargs = ""
        else:
            sargs = str(args)
        hclass = _c.getDefclassName(self.__defclass)
        construct = "(defmessage-handler %s %s %s %s (%s) %s)" % (
            hclass, name, htype, cmtstr, sargs, text)
        _c.build(construct)
        return _c.findDefmessageHandler(self.__defclass, name, htype)

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def MessageHandlerIndex(self, name, htype=PRIMARY):
        """find the specified MessageHandler"""
        htype = htype.lower()
        if htype in (AROUND, BEFORE, PRIMARY, AFTER):
            return _c.findDefmessageHandler(self.__defclass, name, htype)
        else:
            raise ValueError(
                "htype must be in AROUND, BEFORE, PRIMARY, AFTER")

    def MessageHandlerName(self, index):
        """return name of specified MessageHandler"""
        return Symbol(_c.getDefmessageHandlerName(self.__defclass, index))

    def MessageHandlerPPForm(self, index):
        """return the pretty-print form of specified MessageHandler"""
        return _c.getDefmessageHandlerPPForm(self.__defclass, index)

    def MessageHandlerType(self, index):
        """return type of specified MessageHandler"""
        return _c.getDefmessageHandlerType(self.__defclass, index)

    def MessageHandlerWatched(self, index):
        """return watch status of specified MessageHandler"""
        return bool(_c.getDefmessageHandlerWatch(self.__defclass, index))

    def MessageHandlerDeletable(self, index):
        """return True if specified MessageHandler can be deleted"""
        return bool(_c.isDefmessageHandlerDeletable(self.__defclass, index))

    def NextMessageHandlerIndex(self, index):
        """return index of next MessageHandler wrt. specified"""
        return _c.getNextDefmessageHandler(self.__defclass, index)

    def RemoveMessageHandler(self, index):
        """remove the specified MessageHandler"""
        return _c.undefmessageHandler(self.__defclass, index)

    def WatchMessageHandler(self, index):
        """watch specified MessageHandler"""
        return _c.setDefmessageHandlerWatch(True, self.__defclass, index)

    def UnwatchMessageHandler(self, index):
        """unwatch specified MessageHandler"""
        return _c.setDefmessageHandlerWatch(False, self.__defclass, index)

    def MessageHandlerList(self):
        """return list of MessageHandler constructs of this Class"""
        o = _c.getDefmessageHandlerList(self.__defclass, False)
        li, rv = Multifield(_cl2py(o)), []
        l = len(li) / 3
        for x in range(0, l):
            rv.append(Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
        return Multifield(rv)

    def AllMessageHandlerList(self):
        """return list of MessageHandlers of this Class and superclasses"""
        o = _c.getDefmessageHandlerList(self.__defclass, True)
        li, rv = Multifield(_cl2py(o)), []
        l = len(li) / 3
        for x in range(0, l):
            rv.append(Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
        return Multifield(rv)

    def PrintMessageHandlers(self):
        """print list of all MessageHandlers of this Class"""
        _c.routerClear("temporary")
        _c.listDefmessageHandlers("temporary", self.__defclass)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def PrintAllMessageHandlers(self):
        """print list of MessageHandlers of this Class and superclasses"""
        _c.routerClear("temporary")
        _c.listDefmessageHandlers("temporary", self.__defclass, 1)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def PreviewSend(self, msgname):
        """print list of MessageHandlers suitable for specified message"""
        _c.routerClear("temporary")
        _c.previewSend("temporary", self.__defclass, msgname)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)
#}}



# ========================================================================== #
# High-level class for instance objects
# Treat an instance as an object having an Object-Oriented interface.
#  Implements all the functions needed to access instance objects.
#{{CLASS
class Instance(object):
    """high-level Instance class (represents: instance)"""

    def __init__(self, o):
        """create an Instance object (internal)"""
        # this on-the-fly class takes the underlying instance object, which
        #  should already exist, and accesses its slots through the functions
        #  provided by the low-level module, thus exposing a dictionary-like
        #  interface that can be used to access slots at high level
        #  NOTE: there is a hack that allows the environment version to work
        #        by trying to access the underlying environment object
        class __instance_Slots:
            """access instance Slots"""
            def __init__(self, io):
                self.__instance = io

            @_accepts_method((str, unicode), None)
            @_forces_method(str, None)
            def __setitem__(self, name, v):
                _c.directPutSlot(self.__instance, name, _py2cl(v))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def __getitem__(self, name):
                return _cl2py(_c.directGetSlot(self.__instance, name))

            def keys(self):
                return map(
                    str, list(Instance(self.__instance).Class.Slots.Names()))

            @_accepts_method((str, unicode))
            @_forces_method(str)
            def has_key(self, k):
                return bool(
                    k in map(str, list(
                        Instance(self.__instance).Class.Slots.Names())))

            def __repr__(self):
                return "<Instance [%s] Slots>" \
                       % _c.getInstanceName(self.__instance)

            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle instance slots")
        if _c.isInstance(o): self.__instance = o
        else:
            raise _c.ClipsError("M01: cannot directly create Instance")
        self.__Slots = __instance_Slots(self.__instance)
        # the following try/except blocks are to enable companion versions
        try:
            self.__Slots._instance_Slots__env = self.__env
        except AttributeError: pass
        try:
            self.__Slots._instance_Slots__envobject = self.__envobject
        except AttributeError: pass

    def __str__(self):
        """string form of Instance"""
        return _c.getInstanceName(self.__instance)

    def __repr__(self):
        """representation of Instance"""
        s = repr(self.__instance)[1:-1]
        return "<Instance [%s]: %s>" % (
            _c.getInstanceName(self.__instance), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Instance"""
        o = _c.getNextInstance(self.__instance)
        if o:
            return Instance(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Instance"""
        return _c.getInstancePPForm(self.__instance)

    def IsValid(self):
        """determine if this Instance is still valid"""
        return bool(_c.validInstanceAddress(self.__instance))

    def Remove(self):
        """remove this Instance"""
        _c.unmakeInstance(self.__instance)

    def DirectRemove(self):
        """directly remove this Instance"""
        _c.deleteInstance(self.__instance)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def GetSlot(self, slotname):
        """retrieve value of specified Slot"""
        return _cl2py(_c.directGetSlot(self.__instance, slotname))
    SlotValue = GetSlot

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def PutSlot(self, slotname, value):
        """set value of specified Slot"""
        _c.directPutSlot(self.__instance, slotname, _py2cl(value))
    SetSlotValue = PutSlot

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def Send(self, msg, args=None):
        """send specified message with the given arguments to Instance"""
        if args is not None:
            t = type(args)
            if t == str:
                sargs = args
            elif t == unicode:
                sargs = str(args)
            elif isinstance(args, str):
                sargs = str(args)
            elif isinstance(args, unicode):
                sargs = str(args)
            elif t in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                       ClipsSymbolType, ClipsNilType, ClipsInstanceNameType,
                       ClipsMultifieldType):
                sargs = _py2clsyntax(args)
            elif t in (tuple, list):
                li = []
                for x in args:
                    t1 = type(x)
                    if t1 in (ClipsIntegerType, ClipsFloatType,
                              ClipsStringType, ClipsSymbolType, ClipsNilType,
                              ClipsInstanceNameType, ClipsMultifieldType):
                        li.append(_py2clsyntax(x))
                    elif t1 in (int, long):
                        li.append(Integer(int(x)).clsyntax())
                    elif t1 == float:
                        li.append(Float(x).clsyntax())
                    elif t1 in (str, unicode):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, int):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, long):
                        li.append(Integer(x).clsyntax())
                    elif isinstance(x, float):
                        li.append(Float(x).clsyntax())
                    elif isinstance(x, str):
                        li.append(String(x).clsyntax())
                    elif isinstance(x, unicode):
                        li.append(String(x).clsyntax())
                    else:
                        li.append(str(x))
                sargs = " ".join(li)
            elif t in (int, long):
                sargs = Integer(args).clsyntax()
            elif t == float:
                sargs = Float(args).clsyntax()
            elif isinstance(args, str):
                sargs = str(args)
            elif isinstance(args, unicode):
                sargs = str(args)
            elif isinstance(args, int):
                sargs = Integer(args).clsyntax()
            elif isinstance(args, long):
                sargs = Integer(args).clsyntax()
            elif isinstance(args, float):
                sargs = Float(args).clsyntax()
            else:
                sargs = str(args)
            return _cl2py(_c.send(self.__instance, msg, sargs))
        else:
            return _cl2py(_c.send(self.__instance, msg))

    def __property_getName(self):
        return InstanceName(_c.getInstanceName(self.__instance))
    Name = property(__property_getName, None, None, "retrieve Instance name")

    def __property_getClass(self):
        return Class(_c.getInstanceClass(self.__instance))
    Class = property(__property_getClass, None, None,
                     "retrieve Instance class")

    # access instance slots through the internal object
    def __property_getSlots(self): return self.__Slots
    Slots = property(__property_getSlots, None, None,
                     "Instance Slots information")
#}}



# ========================================================================== #
# High-level class for definstances objects
# Treat definstances as an object having an Object-Oriented interface.
#  Implements all the functions needed to access definstances objects.
#{{CLASS
class Definstances(object):
    """high-level Definstances class (represents: definstances)"""

    def __init__(self, o):
        """create a Definstances object (internal)"""
        if _c.isDefinstances(o): self.__definstances = o
        else: raise _c.ClipsError("M01: cannot directly create Definstances")

    def __str__(self):
        """string form of Definstances"""
        return _c.getDefinstancesName(self.__definstances)

    def __repr__(self):
        """representation of Definstances"""
        s = repr(self.__definstances)[1:-1]
        return "<Definstances '%s': %s>" % (
            _c.getDefinstancesName(self.__definstances), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
            % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Definstances"""
        o = _c.getNextDefinstances(self.__definstances)
        if o:
            return Definstances(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Definstances"""
        return _c.getDefinstancesPPForm(self.__definstances)

    def Remove(self):
        """delete this Definstances object"""
        _c.undefinstances(self.__definstances)

    def __property_getModule(self):
        return Symbol(_c.definstancesModule(self.__definstances))
    Module = property(__property_getModule, None, None,
                      "retrieve Definstances module")

    def __property_getName(self):
        return Symbol(_c.getDefinstancesName(self.__definstances))
    Name = property(__property_getName, None, None,
                    "retrieve Definstances name")

    def __property_getDeletable(self):
        return bool(_c.isDefinstancesDeletable(self.__definstances))
    Deletable = property(__property_getDeletable, None, None,
                         "verify if this Definstances can be deleted")
#}}



# ========================================================================== #
# High-level class for defmodule objects
# Treat a defmodule as an object having an Object-Oriented interface.
#  Implements all the functions needed to access defmodule objects.
#{{CLASS
class Module(object):
    """high-level Module class (represents: defmodule)"""

    def __init__(self, o):
        """create a Module object (internal)"""
        if _c.isDefmodule(o):
            self.__defmodule = o
        else: raise _c.ClipsError("M01: cannot directly create Module")

    def __str__(self):
        """string form of Module"""
        return _c.getDefmoduleName(self.__defmodule)

    def __repr__(self):
        """representation of Module"""
        s = repr(self.__defmodule)[1:-1]
        return "<Module '%s': %s>" % (
            _c.getDefmoduleName(self.__defmodule), s)

    def __getstate__(self):
        raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
              % self.__class__.__name__)

    # Interface
    def Next(self):
        """return next Module"""
        o = _c.getNextDefmodule(self.__defmodule)
        if(o):
            return Module(o)
        else:
            return None

    def PPForm(self):
        """return the pretty-print form of Module"""
        return _c.getDefmodulePPForm(self.__defmodule)

    def SetCurrent(self):
        """make this the current Module"""
        _c.setCurrentModule(self.__defmodule)

    def SetFocus(self):
        """set focus to this Module"""
        _c.focus(self.__defmodule)

    def __property_getName(self):
        return Symbol(_c.getDefmoduleName(self.__defmodule))
    Name = property(__property_getName, None, None, "return Module name")

    # Functions involving other entities

    # Templates
    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildTemplate(self, name, text, comment=None):
        """build a Template object with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(deftemplate %s::%s %s %s)" % (mname, name, cmtstr, text)
        _c.build(construct)
        return Template(_c.findDeftemplate("%s::%s" % (mname, name)))

    def TemplateList(self):
        """return list of Template names"""
        o = _c.getDeftemplateList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintTemplates(self):
        """print Templates to standard output"""
        _c.routerClear("temporary")
        _c.listDeftemplates("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Facts
    def FactList(self):
        """return list of Facts in this Module"""
        o, li = _c.getFactList(self.__defmodule), []
        if o is not None:
            for x in o[1]:
                if x[0] == _c.FACT_ADDRESS:
                    li.append(Fact(x[1]))
        return li

    # Deffacts
    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildDeffacts(self, name, text, comment=None):
        """build a Deffacts object with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(deffacts %s::%s %s %s)" % (mname, name, cmtstr, text)
        _c.build(construct)
        return Deffacts(_c.findDeffacts("%s::%s" % (mname, name)))

    def DeffactsList(self):
        """return a list of Deffacts names in this Module"""
        o = _c.getDeffactsList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintDeffacts(self):
        """print Deffacts to standard output"""
        _c.routerClear("temporary")
        _c.listDeffacts("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Rules
    @_accepts_method((str, unicode), (str, unicode), (str, unicode), None)
    @_forces_method(str, str, str, None)
    def BuildRule(self, name, lhs, rhs, comment=None):
        """build a Rule object with specified name and LHS/RHS"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(defrule %s::%s %s %s => %s)" % (
            mname, name, cmtstr, lhs, rhs)
        _c.build(construct)
        return Rule(_c.findDefrule("%s::%s" % (mname, name)))

    def RuleList(self):
        """return a list of Rule names in this Module"""
        o = _c.getDefruleList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintRules(self):
        """print Rules to standard output"""
        _c.routerClear("temporary")
        _c.listDefrules("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def PrintBreakpoints(self):
        """print breakpoints to standard output"""
        _c.routerClear("temporary")
        _c.showBreaks("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Agenda
    def PrintAgenda(self):
        """print Agenda Rules to standard output"""
        _c.routerClear("temporary")
        _c.agenda("temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def RefreshAgenda(self):
        """refresh Agenda for this Module"""
        _c.refreshAgenda(self.__defmodule)

    def ReorderAgenda(self):
        """reorder Agenda for this Module"""
        _c.reorderAgenda(self.__defmodule)

    # Globals
    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def BuildGlobal(self, name, value=Nil):
        """build a Global variable with specified name and value"""
        mname = self.Name
        if type(value)  in (str, ClipsStringType):
            value = '"%s"' % value
        construct = "(defglobal %s ?*%s* = %s)" % (mname, name, value)
        _c.build(construct)
        return Global(_c.findDefglobal("%s::%s" % (mname, name)))

    def GlobalList(self):
        """return the list of Global variable names"""
        o = _c.getDefglobalList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintGlobals(self):
        """print list of Global variables to standard output"""
        _c.routerClear("temporary")
        _c.listDefglobals("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def ShowGlobals(self):
        """print list of Global variables and values to standard output"""
        _c.routerClear("temporary")
        _c.showDefglobals("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Functions
    @_accepts_method((str, unicode), None, (str, unicode), None)
    @_forces_method(str, None, str, None)
    def BuildFunction(self, name, args, text, comment=None):
        """build a Function with specified name, body and arguments"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        if type(args) in (tuple, list):
            args = " ".join(args)
        elif args is None:
            args = ""
        construct = "(deffunction %s::%s %s (%s) %s)" % (
            mname, name, cmtstr, args, text)
        _c.build(construct)
        return Function(_c.findDeffunction("%s::%s" % (mname, name)))

    def FunctionList(self):
        """return the list of Function names"""
        o = _c.getDeffunctionList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintFunctions(self):
        """print list of Functions to standard output"""
        _c.routerClear("temporary")
        _c.listDeffunctions("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Generics
    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def BuildGeneric(self, name, comment=None):
        """build a Generic with specified name"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(defgeneric %s::%s %s)" % (mname, name, cmtstr)
        _c.build(construct)
        return Generic(_c.findDefgeneric("%s::%s" % (mname, name)))

    def GenericList(self):
        """return the list of Generic names"""
        o = _c.getDefgenericList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintGenerics(self):
        """print list of Generics to standard output"""
        _c.routerClear("temporary")
        _c.listDefgenerics("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Classes
    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildClass(self, name, text, comment=None):
        """build a Class with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(defclass %s::%s %s %s)" % (mname, name, cmtstr, text)
        _c.build(construct)
        return Class(_c.findDefclass("%s::%s" % (mname, name)))

    def ClassList(self):
        """return the list of Class names"""
        o = _c.getDefclassList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintClasses(self):
        """print list of Class to standard output"""
        _c.routerClear("temporary")
        _c.listDefclasses("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Instances
    @_accepts_method((str, unicode), None, None)
    @_forces_method(str, str, None)
    def BuildInstance(self, name, defclass, overrides=""):
        """build an Instance of given Class overriding specified Slots"""
        mname = self.Name
        cmdstr = "(%s::%s of %s %s)" % (mname, name, defclass, overrides)
        return Instance(_c.makeInstance(cmdstr))

    @_forces_method(str)
    def PrintInstances(self, classname=None):
        """print Instances to standard output"""
        _c.routerClear("temporary")
        if classname:
            _c.instances("temporary", self.__defmodule, classname, False)
        else:
            _c.instances("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_forces_method(str)
    def PrintSubclassInstances(self, classname):
        """print Instances to standard output"""
        _c.routerClear("temporary")
        _c.instances("temporary", self.__defmodule, classname, True)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # Definstances
    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildDefinstances(self, name, text, comment=None):
        """build a Definstances with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        mname = self.Name
        construct = "(definstances %s::%s %s %s)" % (
            mname, name, cmtstr, text)
        _c.build(construct)
        return Definstances(_c.findDefinstances(name))

    def DefinstancesList(self):
        """retrieve list of all Definstances names"""
        o = _c.getDefinstancesList(self.__defmodule)
        return Multifield(_cl2py(o))

    def PrintDefinstances(self):
        """print list of all Definstances to standard output"""
        _c.routerClear("temporary")
        _c.listDefinstances("temporary", self.__defmodule)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)
#}}



# ========================================================================== #
# some toplevel functions
# ========================================================================== #


# ========================================================================== #
# 1) functions involving Templates

#{{FUNCTION
def InitialTemplate():
    """return first Template in environment"""
    try:
        return Template(_c.getNextDeftemplate())
    except:
        raise _c.ClipsError("M02: could not find any Template")
#}}

#{{FUNCTION
def PrintTemplates():
    """print Templates to standard output"""
    _c.routerClear("temporary")
    _c.listDeftemplates("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def TemplateList():
    """return a list of Template names"""
    o = _c.getDeftemplateList()
    return Multifield(_cl2py(o))    # should be all strings
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindTemplate(s):
    """find a Template by name"""
    return Template(_c.findDeftemplate(s))
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), None)
@_forces(str, str, None)
def BuildTemplate(name, text, comment=None):
    """build a Template object with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(deftemplate %s %s %s)" % (name, cmtstr, text)
    _c.build(construct)
    return Template(_c.findDeftemplate(name))
#}}



# ========================================================================== #
# 2) functions involving facts

#{{FUNCTION
def Assert(o):
    """assert a Fact from a string or constructed Fact object"""
    if '_Fact__fact' in dir(o) and _c.isFact(o._Fact__fact):
        return o.Assert()
    elif type(o) in (str, unicode):
        return Fact(_c.assertString(str(o)))
    else:
        raise TypeError("expected a string or a Fact")
#}}

#{{FUNCTION
def InitialFact():
    """return first Fact in environment"""
    try:
        return Fact(_c.getNextFact())
    except:
        raise _c.ClipsError("M02: could not find any Fact")
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def LoadFacts(filename):
    """load Facts from file"""
    _c.loadFacts(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def LoadFactsFromString(s):
    """load Fact objects from a string"""
    _c.loadFactsFromString(s)
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode))
@_forces(str, str)
def SaveFacts(filename, mode=LOCAL_SAVE):
    """save current Facts to file"""
    _c.saveFacts(_os.path.normpath(filename), mode)
#}}

#{{FUNCTION
def PrintFacts():
    """print Facts to standard output"""
    _c.routerClear("temporary")
    _c.facts("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def FactListChanged():
    """test whether Fact list is changed since last call"""
    rv = bool(_c.getFactListChanged())
    _c.setFactListChanged(False)
    return rv
#}}

#{{FUNCTION
def FactList():
    """return list of Facts in current module"""
    o, li = _c.getFactList(), []
    if o is not None:
        for x in o[1]:
            if x[0] == _c.FACT_ADDRESS:
                li.append(Fact(x[1]))
    return li
#}}



# ========================================================================== #
# 3) functions involving deffacts

#{{FUNCTION
def InitialDeffacts():
    """return first Deffacts"""
    try:
        return Deffacts(_c.getNextDeffacts())
    except:
        raise _c.ClipsError("M02: could not find any Deffacts")
#}}

#{{FUNCTION
def DeffactsList():
    """return a list of Deffacts names in current module"""
    o = _c.getDeffactsList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindDeffacts(s):
    """find a Deffacts by name"""
    try:
        return Deffacts(_c.findDeffacts(s))
    except:
        raise _c.ClipsError("M02: could not find Deffacts '%s'" % s)
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), None)
@_forces(str, str, None)
def BuildDeffacts(name, text, comment=None):
    """build a Deffacts object with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(deffacts %s %s %s)" % (name, cmtstr, text)
    _c.build(construct)
    return Deffacts(_c.findDeffacts(name))
#}}

#{{FUNCTION
def PrintDeffacts():
    """print Deffacts to standard output"""
    _c.routerClear("temporary")
    _c.listDeffacts("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 4) functions involving Rules

#{{FUNCTION
def InitialRule():
    """return first Rule"""
    try:
        return Rule(_c.getNextDefrule())
    except:
        raise _c.ClipsError("M02: could not find any Rule")
#}}

#{{FUNCTION
def RuleList():
    """return a list of Rule names in current module"""
    o = _c.getDefruleList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindRule(s):
    """find a Rule by name"""
    try:
        return Rule(_c.findDefrule(s))
    except:
        raise _c.ClipsError("M02: could not find defrule '%s'" % s)
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), (str, unicode), None)
@_forces(str, str, str, None)
def BuildRule(name, lhs, rhs, comment=None):
    """build a Rule object with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(defrule %s %s %s => %s)" % (name, cmtstr, lhs, rhs)
    _c.build(construct)
    return Rule(_c.findDefrule(name))
#}}

#{{FUNCTION
def PrintRules():
    """print Rules to standard output"""
    _c.routerClear("temporary")
    _c.listDefrules("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def PrintBreakpoints():
    """print breakpoints to standard output"""
    _c.routerClear("temporary")
    _c.showBreaks("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 5) functions involving Modules

#{{FUNCTION
def InitialModule():
    """return first Module"""
    try:
        return Module(_c.getNextDefmodule())
    except:
        raise _c.ClipsError("M02: could not find any Module")
#}}

#{{FUNCTION
def ModuleList():
    """return the list of Module names"""
    o = _c.getDefmoduleList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindModule(name):
    """find a Module by name"""
    return Module(_c.findDefmodule(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), None)
@_forces(str, str, None)
def BuildModule(name, text="", comment=None):
    """build a Module with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(defmodule %s %s %s)" % (name, cmtstr, text)
    _c.build(construct)
    return Module(_c.findDefmodule(name))
#}}

#{{FUNCTION
def PrintModules():
    """print list of Modules to standard output"""
    _c.routerClear("temporary")
    _c.listDefmodules("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 6) functions involving defglobals

#{{FUNCTION
def InitialGlobal():
    """return first Global variable"""
    try:
        return Global(_c.getNextDefglobal())
    except:
        raise _c.ClipsError("M02: could not find any Global")
#}}

#{{FUNCTION
def GlobalList():
    """return the list of Global variable names"""
    o = _c.getDefglobalList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindGlobal(name):
    """find a Global variable by name"""
    return Global(_c.findDefglobal(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def BuildGlobal(name, value=Nil):
    """build a Global variable with specified name and body"""
    if type(value) in (str, unicode, ClipsStringType):
        value = '"%s"' % str(value)
    construct = "(defglobal ?*%s* = %s)" % (name, value)
    _c.build(construct)
    return Global(_c.findDefglobal("%s" % name))
#}}

#{{FUNCTION
def GlobalsChanged():
    """test whether or not Global variables have changed since last call"""
    rv = bool(_c.getGlobalsChanged())
    _c.setGlobalsChanged(False)
    return rv
#}}

#{{FUNCTION
def PrintGlobals():
    """print list of Global variables to standard output"""
    _c.routerClear("temporary")
    _c.listDefglobals("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def ShowGlobals():
    """print list of Global variables and values to standard output"""
    _c.routerClear("temporary")
    _c.showDefglobals("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 7) functions involving Functions

#{{FUNCTION
def InitialFunction():
    """return first Function"""
    try:
        return Function(_c.getNextDeffunction())
    except:
        raise _c.ClipsError("M02: could not find any Function")
#}}

#{{FUNCTION
def FunctionList():
    """return the list of Function names"""
    o = _c.getDeffunctionList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindFunction(name):
    """find a Function by name"""
    return Function(_c.findDeffunction(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), None, (str, unicode), None)
@_forces(str, None, str, None)
def BuildFunction(name, args, text, comment=None):
    """build a Function with specified name, body and arguments"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    if type(args) in (tuple, list):
        args = " ".join(args)
    elif args is None:
        args = ""
    construct = "(deffunction %s %s (%s) %s)" % (name, cmtstr, args, text)
    _c.build(construct)
    return Function(_c.findDeffunction(name))
#}}

#{{FUNCTION
def PrintFunctions():
    """print list of Functions to standard output"""
    _c.routerClear("temporary")
    _c.listDeffunctions("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 8) functions involving Generics

#{{FUNCTION
def InitialGeneric():
    """return first Generic"""
    try:
        return Generic(_c.getNextDefgeneric())
    except:
        raise _c.ClipsError("M02: could not find any Generic")
#}}

#{{FUNCTION
def GenericList():
    """return the list of Generic names"""
    o = _c.getDefgenericList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindGeneric(name):
    """find a Generic by name"""
    return Generic(_c.findDefgeneric(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def BuildGeneric(name, comment=None):
    """build a Generic with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(defgeneric %s %s)" % (name, cmtstr)
    _c.build(construct)
    return Generic(_c.findDefgeneric(name))
#}}

#{{FUNCTION
def PrintGenerics():
    """print list of Generics to standard output"""
    _c.routerClear("temporary")
    _c.listDefgenerics("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def MethodList():
    """return the list of all Methods"""
    o = _cl2py(_c.getDefmethodList())
    li = Multifield([])
    l = len(o) / 2
    for x in range(l):
        li.append(Multifield([o[2 * x], o[2 * x + 1]]))
    return li
#}}



# ========================================================================== #
# 9) functions involving Classes

#{{FUNCTION
def InitialClass():
    """retrieve first Class"""
    try:
        return Class(_c.getNextDefclass())
    except:
        raise _c.ClipsError("M02: could not find any Class")
#}}

#{{FUNCTION
def ClassList():
    """return the list of Class names"""
    o = _c.getDefclassList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindClass(name):
    """find a Class by name"""
    return Class(_c.findDefclass(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), None)
@_forces(str, str, None)
def BuildClass(name, text, comment=None):
    """build a Class with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(defclass %s %s %s)" % (name, cmtstr, text)
    _c.build(construct)
    return Class(_c.findDefclass(name))
#}}

#{{FUNCTION
def PrintClasses():
    """print list of Classes to standard output"""
    _c.routerClear("temporary")
    _c.listDefclasses("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def BrowseClasses(classname):
    """print list of Classes that inherit from specified one"""
    _c.routerClear("temporary")
    defclass = _c.findDefclass(str(classname))
    _c.browseClasses("temporary", defclass)
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
@_accepts((str, unicode), None, None, (str, unicode), None, None)
@_forces(str, str, None, str, None, None)
def BuildMessageHandler(name, hclass, args, text, htype=PRIMARY, comment=None):
    """build a MessageHandler for specified class with arguments and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else: cmtstr = ""
    htype = htype.lower()
    if not htype in (AROUND, BEFORE, PRIMARY, AFTER):
        raise ValueError("htype must be in AROUND, BEFORE, PRIMARY, AFTER")
    if type(args) in (tuple, list):
        sargs = " ".join(args)
    elif args is None:
        sargs = ""
    else:
        sargs = str(args)
    construct = "(defmessage-handler %s %s %s %s (%s) %s)" % (
        hclass, name, htype, cmtstr, sargs, text)
    _c.build(construct)
    defclass = _c.findDefclass(hclass)
    return _c.findDefmessageHandler(defclass, name, htype)
#}}

#{{FUNCTION
def MessageHandlerList():
    """return list of MessageHandler constructs"""
    o = _c.getDefmessageHandlerList()
    li, rv = Multifield(_cl2py(o)), []
    l = len(li) / 3
    for x in range(0, l):
        rv.append(Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
    return Multifield(rv)
#}}

#{{FUNCTION
def PrintMessageHandlers():
    """print list of all MessageHandlers"""
    _c.routerClear("temporary")
    _c.listDefmessageHandlers("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 10) functions involving instances

#{{FUNCTION
def InitialInstance():
    """retrieve first Instance"""
    try:
        return Instance(_c.getNextInstance())
    except:
        raise _c.ClipsError("M02: could not find any Instance")
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def BLoadInstances(filename):
    """load Instances from binary file"""
    _c.binaryLoadInstances(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def BSaveInstances(filename, mode=LOCAL_SAVE):
    """save Instances to binary file"""
    _c.binarySaveInstances(_os.path.normpath(filename), mode)
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def LoadInstances(filename):
    """load Instances from file"""
    _c.loadInstances(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def SaveInstances(filename, mode=LOCAL_SAVE):
    """save Instances to file"""
    _c.saveInstances(_os.path.normpath(filename), mode)
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def LoadInstancesFromString(s):
    """load Instances from the specified string"""
    _c.loadInstancesFromString(s)
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def RestoreInstancesFromString(s):
    """restore Instances from the specified string"""
    _c.restoreInstancesFromString(s)
#}}

#{{FUNCTION
def InstancesChanged():
    """test if Instances have changed since last call"""
    rv = bool(_c.getInstancesChanged())
    _c.setInstancesChanged(False)
    return rv
#}}

#{{FUNCTION
@_accepts((str, unicode), None, (str, unicode))
@_forces(str, str, str)
def BuildInstance(name, defclass, overrides=""):
    """build an Instance of given class overriding specified slots"""
    cmdstr = "(%s of %s %s)" % (name, str(defclass), overrides)
    return Instance(_c.makeInstance(cmdstr))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindInstance(name):
    """find an Instance in all modules (including imported)"""
    return Instance(_c.findInstance(name, True))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindInstanceLocal(name):
    """find an Instance in non imported modules"""
    return Instance(_c.findInstance(name, False))
#}}

#{{FUNCTION
@_forces(str)
def PrintInstances(classname=None):
    """print Instances to standard output"""
    _c.routerClear("temporary")
    if classname:
        _c.instances("temporary", classname, False)
    else:
        _c.instances("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
@_forces(str)
def PrintSubclassInstances(classname):
    """print subclass Instances to standard output"""
    _c.routerClear("temporary")
    if classname:
        _c.instances("temporary", classname, True)
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 11) functions involving definstances

#{{FUNCTION
def InitialDefinstances():
    """retrieve first Definstances"""
    try:
        return Definstances(_c.getNextDefinstances())
    except:
        raise _c.ClipsError("M02: could not find any Definstances")
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def FindDefinstances(name):
    """find Definstances by name"""
    return Definstances(_c.findDefinstances(name))
#}}

#{{FUNCTION
@_accepts((str, unicode), (str, unicode), None)
@_forces(str, str, None)
def BuildDefinstances(name, text, comment=None):
    """build a Definstances with specified name and body"""
    if comment:
        cmtstr = '"%s"' % str(comment).replace('"', '\\"')
    else:
        cmtstr = ""
    construct = "(definstances %s %s %s)" % (name, cmtstr, text)
    _c.build(construct)
    return Definstances(_c.findDefinstances(name))
#}}

#{{FUNCTION
def DefinstancesList():
    """retrieve list of all Definstances names"""
    o = _c.getDefinstancesList()
    return Multifield(_cl2py(o))
#}}

#{{FUNCTION
def PrintDefinstances():
    """print list of all Definstances to standard output"""
    _c.routerClear("temporary")
    _c.listDefinstances("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}



# ========================================================================== #
# 12) Agenda functions

#{{FUNCTION
def PrintAgenda():
    """print Agenda Rules to standard output"""
    _c.routerClear("temporary")
    _c.agenda("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def AgendaChanged():
    """test whether or not Agenda is changed since last call"""
    rv = bool(_c.getAgendaChanged())
    _c.setAgendaChanged(False)
    return rv
#}}

#{{FUNCTION
def RefreshAgenda():
    """refresh Agenda Rules for current Module"""
    _c.refreshAgenda()
#}}

#{{FUNCTION
def ReorderAgenda():
    """reorder Agenda Rules for current Module"""
    _c.reorderAgenda()
#}}

#{{FUNCTION
def Run(limit=None):
    """execute Rules up to limit (if any)"""
    if limit is None:
        return _c.run()
    else:
        return _c.run(limit)
#}}

#{{FUNCTION
def ClearFocusStack():
    """clear focus stack"""
    _c.clearFocusStack()
#}}

#{{FUNCTION
def FocusStack():
    """return list of Module names in focus stack"""
    return _cl2py(_c.getFocusStack())
#}}

#{{FUNCTION
def PrintFocusStack():
    """print focus stack to standard output"""
    _c.routerClear("temporary")
    _c.listFocusStack("temporary")
    s = _c.routerRead("temporary")
    if s:
        _sys.stdout.write(s)
#}}

#{{FUNCTION
def PopFocus():
    """pop focus"""
    _c.popFocus()
#}}

#{{FUNCTION
def InitialActivation():
    """return first Activation object"""
    try:
        return Activation(_c.getNextActivation())
    except:
        raise _c.ClipsError("M02: could not find any Activation")
#}}

#{{FUNCTION
def CurrentModule():
    """return current Module"""
    return Module(_c.getCurrentModule())
#}}



# ========================================================================== #
# 13) True "current environment" functions - as of APG section 4.1

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def BLoad(filename):
    """binary load the constructs from a file"""
    _c.bload(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def BSave(filename):
    """binary save constructs to a file"""
    _c.bsave(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def Load(filename):
    """load constructs from a file"""
    _c.load(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def Save(filename):
    """save constructs to a file"""
    _c.save(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def BatchStar(filename):
    """execute commands stored in file"""
    _c.batchStar(_os.path.normpath(filename))
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def Build(construct):
    """build construct given in argument"""
    _c.build(construct)
#}}

#{{FUNCTION
@_accepts((str, unicode))
@_forces(str)
def Eval(expr):
    """evaluate expression passed as argument"""
    return _cl2py(_c.eval(expr))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def Call(func, args=None):
    """call a function with the given argument string or tuple"""
    if args is not None:
        t = type(args)
        if t == str:
            sargs = args
        if t == unicode:
            sargs = str(args)
        elif t in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                   ClipsSymbolType, ClipsNilType, ClipsInstanceNameType,
                   ClipsMultifieldType):
            sargs = _py2clsyntax(args)
        elif isinstance(args, str):
            sargs = str(args)
        elif isinstance(args, unicode):
            sargs = str(args)
        elif t in (tuple, list):
            li = []
            for x in args:
                t1 = type(x)
                if t1 in (ClipsIntegerType, ClipsFloatType, ClipsStringType,
                          ClipsSymbolType, ClipsNilType,
                          ClipsInstanceNameType, ClipsMultifieldType):
                    li.append(_py2clsyntax(x))
                elif t1 in (int, long):
                    li.append(Integer(int(x)).clsyntax())
                elif t1 == float:
                    li.append(Float(x).clsyntax())
                elif t1 in (str, unicode):
                    li.append(String(x).clsyntax())
                elif isinstance(x, int):
                    li.append(Integer(x).clsyntax())
                elif isinstance(x, long):
                    li.append(Integer(x).clsyntax())
                elif isinstance(x, float):
                    li.append(Float(x).clsyntax())
                elif isinstance(x, str):
                    li.append(String(x).clsyntax())
                elif isinstance(x, unicode):
                    li.append(String(x).clsyntax())
                else:
                    li.append(str(x))
            sargs = " ".join(li)
        elif t in (int, long):
            sargs = Integer(int(args)).clsyntax()
        elif t == float:
            sargs = Float(args).clsyntax()
        elif isinstance(args, int):
            sargs = Integer(args).clsyntax()
        elif isinstance(args, long):
            sargs = Integer(args).clsyntax()
        elif isinstance(args, float):
            sargs = Float(args).clsyntax()
        else:
            sargs = str(args)
        return _cl2py(_c.functionCall(func, sargs))
    else:
        return _cl2py(_c.functionCall(func))
#}}

#{{FUNCTION
@_accepts((str, unicode), None)
@_forces(str, None)
def SendCommand(command, verbose=False):
    """send a command to the engine as if typed at the CLIPS prompt"""
    _c.sendCommand(command, verbose)
#}}

#{{FUNCTION
def Reset():
    """reset Environment"""
    _c.reset()

# the environment-aware and toplevel versions of Clear() behave differently
# as only the toplevel version reinitializes stock classes (see below): this
# is why a check is performed to test whether or not the 'self' identifier
# is present (which only happens in the environment-aware version)#}}

#{{FUNCTION
def Clear():
    """clear Environment"""
    _c.clear()
    if not 'self' in locals().keys():
        _setStockClasses()
#}}




# define the only object of the Status type and remove the class definition
EngineConfig = _clips_Status()
del _clips_Status

# define the only object of the Debug type and remove the class definition
DebugConfig = _clips_Debug()
del _clips_Debug



# the following is a mechanism to keep stock class names up to date:
# when the importing module sets this, then its dictionary is modified
# by the _setStockClasses() function directly; please note that, since
# the first time this module is imported the stock class names are
# correct, the __parent_module_dict__ should be set up after importing
__parent_module_dict__ = None
def _setParentModuleDict(d):
    global __parent_module_dict__
    __parent_module_dict__ = d


# provide a way for Environments to do the same as they become current
def _setStockClasses():
    """reset stock classes to the ones of current Environment"""
    global FLOAT_CLASS, INTEGER_CLASS, SYMBOL_CLASS, STRING_CLASS, \
           MULTIFIELD_CLASS, EXTERNAL_ADDRESS_CLASS, FACT_ADDRESS_CLASS, \
           INSTANCE_ADDRESS_CLASS, INSTANCE_NAME_CLASS, OBJECT_CLASS, \
           PRIMITIVE_CLASS, NUMBER_CLASS, LEXEME_CLASS, ADDRESS_CLASS, \
           INSTANCE_CLASS, USER_CLASS, INITIAL_OBJECT_CLASS
    # the following definitions are only valid at submodule level
    FLOAT_CLASS = Class(_c.findDefclass("FLOAT"))
    INTEGER_CLASS = Class(_c.findDefclass("INTEGER"))
    SYMBOL_CLASS = Class(_c.findDefclass("SYMBOL"))
    STRING_CLASS = Class(_c.findDefclass("STRING"))
    MULTIFIELD_CLASS = Class(_c.findDefclass("MULTIFIELD"))
    EXTERNAL_ADDRESS_CLASS = Class(_c.findDefclass("EXTERNAL-ADDRESS"))
    FACT_ADDRESS_CLASS = Class(_c.findDefclass("FACT-ADDRESS"))
    INSTANCE_ADDRESS_CLASS = Class(_c.findDefclass("INSTANCE-ADDRESS"))
    INSTANCE_NAME_CLASS = Class(_c.findDefclass("INSTANCE-NAME"))
    OBJECT_CLASS = Class(_c.findDefclass("OBJECT"))
    PRIMITIVE_CLASS = Class(_c.findDefclass("PRIMITIVE"))
    NUMBER_CLASS = Class(_c.findDefclass("NUMBER"))
    LEXEME_CLASS = Class(_c.findDefclass("LEXEME"))
    ADDRESS_CLASS = Class(_c.findDefclass("ADDRESS"))
    INSTANCE_CLASS = Class(_c.findDefclass("INSTANCE"))
    USER_CLASS = Class(_c.findDefclass("USER"))
    INITIAL_OBJECT_CLASS = Class(_c.findDefclass("INITIAL-OBJECT"))
    # modify the importing package namespace using the provided dictionary
    if __parent_module_dict__:
        __parent_module_dict__['FLOAT_CLASS'] = FLOAT_CLASS
        __parent_module_dict__['INTEGER_CLASS'] = INTEGER_CLASS
        __parent_module_dict__['SYMBOL_CLASS'] = SYMBOL_CLASS
        __parent_module_dict__['STRING_CLASS'] = STRING_CLASS
        __parent_module_dict__['MULTIFIELD_CLASS'] = MULTIFIELD_CLASS
        __parent_module_dict__['EXTERNAL_ADDRESS_CLASS'] = EXTERNAL_ADDRESS_CLASS
        __parent_module_dict__['FACT_ADDRESS_CLASS'] = FACT_ADDRESS_CLASS
        __parent_module_dict__['INSTANCE_ADDRESS_CLASS'] = INSTANCE_ADDRESS_CLASS
        __parent_module_dict__['INSTANCE_NAME_CLASS'] = INSTANCE_NAME_CLASS
        __parent_module_dict__['OBJECT_CLASS'] = OBJECT_CLASS
        __parent_module_dict__['PRIMITIVE_CLASS'] = PRIMITIVE_CLASS
        __parent_module_dict__['NUMBER_CLASS'] = NUMBER_CLASS
        __parent_module_dict__['LEXEME_CLASS'] = LEXEME_CLASS
        __parent_module_dict__['ADDRESS_CLASS'] = ADDRESS_CLASS
        __parent_module_dict__['INSTANCE_CLASS'] = INSTANCE_CLASS
        __parent_module_dict__['USER_CLASS'] = USER_CLASS
        __parent_module_dict__['INITIAL_OBJECT_CLASS'] = INITIAL_OBJECT_CLASS



# set up stock classes now for the module level; please notice that this only
# is useful when the module is imported directly, thus in the "import clips"
# form, as it is impossible to modify names defined in the global namespace
_setStockClasses()



# ========================================================================== #
# 14) Functions and classes to access CLIPS input/output

# the simple class to access CLIPS output
class _clips_Stream(object):
    """object to access CLIPS output streams"""

    def __init__(self, stream, name=None):
        """stream object constructor"""
        self.__stream = stream
        if name is None:
            self.__name = 'Internal'
        else:
            self.__name = name

    def __repr__(self):
        return "<%s Stream>" % self.__name

    def Read(self):
        """read current output from stream"""
        return _c.routerRead(self.__stream)


# the class to write to CLIPS standard input
class _clips_WriteStream(object):
    """object to access CLIPS input streams"""

    def __init__(self, stream, name=None):
        """stream object constructor"""
        self.__stream = stream
        if name is None:
            self.__name = 'Internal'
        else:
            self.__name = name

    def __repr__(self):
        return "<%s Stream>" % self.__name

    def Write(self, s):
        """write string to stream"""
        _c.routerWrite(self.__stream, str(s))


# actual objects the module user can read from
StdoutStream = _clips_Stream("stdout", "General Output")
StdinStream = _clips_WriteStream("stdin", "General Input")
PromptStream = _clips_Stream("wprompt")     # should not be used
DialogStream = _clips_Stream("wdialog")     # should not be used
DisplayStream = _clips_Stream("wdisplay")   # should not be used
ErrorStream = _clips_Stream("werror", "Error Output")
WarningStream = _clips_Stream("wwarning", "Warning Output")
TraceStream = _clips_Stream("wtrace", "Trace Output")

# class definitions can be removed as all possible objects have been built
del _clips_Stream
del _clips_WriteStream



# ========================================================================== #
# 15) Memory Management - for CLIPS gurus
class _clips_Memory(object):
    """object for memory management"""

    __created = False

    def __init__(self):
        """raise an exception if an object of this type has been created"""
        if(_clips_Memory.__created):
            raise TypeError("cannot recreate this object")
        _clips_Memory.__created = True

    def __repr__(self):
        return "<Memory Management Object>"

    def __property_getUsed(self):
        return _c.memUsed()
    Used = property(__property_getUsed, None, None,
                    "amount in bytes of memory used by CLIPS")

    def __property_getRequests(self):
        return _c.memRequests()
    Requests = property(__property_getRequests, None, None,
                        "number of requests for memory made by CLIPS")

    def __property_setConserve(self, v):
        _c.setConserveMemory(v)
    def __property_getConserve(self):
        return bool(_c.getConserveMemory())
    Conserve = property(__property_getConserve, __property_setConserve,
                        None, "enable/disable caching of some informations")

    def __property_setPPBufferSize(self, v):
        _c.setPPBufferSize(v)
    def __property_getPPBufferSize(self):
        return _c.getPPBufferSize()
    PPBufferSize = property(__property_getPPBufferSize,
                            __property_setPPBufferSize,
                            None, "size of pretty-print buffers")

    def __property_setEnvironmentErrorsEnabled(self, v):
        _c.setEnableEnvironmentFatalMessages(v)
    def __property_getEnvironmentErrorsEnabled(self):
        return bool(_c.getEnableEnvironmentFatalMessages())
    EnvironmentErrorsEnabled = property(
        __property_getEnvironmentErrorsEnabled,
        __property_setEnvironmentErrorsEnabled,
        None, "whether or not fatal environment errors are printed")

    def __property_getNumberOfEnvironments(self):
        return _c.getNumberOfEnvironments()
    NumberOfEnvironments = property(__property_getNumberOfEnvironments, None,
                                    None, "current number of Environments")

    def Free(self):
        """free up unneeded memory"""
        _c.releaseMem(False)


# define one only actual object of this type to access memory functions
Memory = _clips_Memory()
del _clips_Memory



# ========================================================================== #
# 16) External Functions - "all sorts of new and shiny evil"
@_accepts(None, (str, unicode))
@_forces(None, str)
def RegisterPythonFunction(func, name=None):
    """register an external (Python) function to call from within CLIPS"""
    def _extcall_retval(rv):
        if rv is None:
            return Nil.clrepr()
        else:
            return _py2cl(rv)
    if not name:
        name = func.__name__
    f = lambda *args: _extcall_retval(func(*tuple(map(_cl2py, list(args)))))
    _c.addPythonFunction(name, f)

def UnregisterPythonFunction(name):
    """unregister the given Python function from CLIPS"""
    if type(name) in (str, unicode):
        _c.removePythonFunction(str(name))
    else:
        _c.removePythonFunction(name.__name__)

def ClearPythonFunctions():
    """unregister all Python functions from CLIPS"""
    _c.clearPythonFunctions()

# set or test whether CLIPS python calls should print a traceback or not
def ExternalTracebackEnabled():
    """return True if printing tracebacks from within CLIPS is enabled"""
    return bool(_c.getPrintExternalTraceback())
def SetExternalTraceback(enable=True):
    """call with a True value to enable printing tracebacks from CLIPS"""
    _c.setPrintExternalTraceback(bool(enable))



# end.
