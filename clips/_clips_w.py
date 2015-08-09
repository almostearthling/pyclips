# _clips_w.py
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

__revision__ = "$Id: _clips_w.py 371 2008-12-24 00:47:54Z Franz $"

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
    _c.PYCLIPS_INCREMENTAL
    )

# bring the CLIPS exception objects at top level
ClipsError = _c.ClipsError
ClipsMemoryError = _c.ClipsMemoryError


# redeclare manifest constants here in order to avoid having to
#  reference the ones defined in the low-level module _clips

# these manifest constants are commented out, since the user has to rely
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


# these class decorators are the ones that allow definition of both dynamic
#  and static inner classes (such as the ones in Java); this code is not mine
#  but comes from a recipe that can be found at the following web address:
#
#           http://code.activestate.com/recipes/409366/
#
#  Thanks to George Sakkis for the recipe. I slightly modified it, mostly in
#  a cosmetic way: the underscore in front of the name states that these
#  functions are actually not in the purpose (and scope) of the module and
#  not to be exported. Moreover, the outer attributes are not implicitly
#  reachable as attributes of the inner class, but only via __outer__

# this decorator allows dynamic inner classes, such as Java inner classes:
#  inner classes reference an instance of the enclosing outer class, and
#  outer instance members can be reached using the __outer__ attribute
def _innerclass(cls):
    """class decorator for a class to behave as an inner class"""
    if hasattr(cls, '__outer__'):
        raise TypeError('Cannot set attribute "__outer__" in inner class')
    class InnerDescriptor(object):
        def __get__(self, outer, outercls):
            if outer is None:
                raise AttributeError(
                    'An enclosing instance that contains %s.%s is required'
                    % (cls.__name__, cls.__name__))
            clsdict = cls.__dict__.copy()
            # explicit read-only reference to the outer instance
            clsdict['__outer__'] = property(lambda self: outer)
            ## implicit lookup in the outer instance
            ##clsdict['__getattr__'] = lambda self, attr: getattr(outer, attr)
            def __setattr__(this, attr, value):
                # setting an attribute in the inner instance sets the
                # respective attribute in the outer instance if and only if
                # the attribute is already defined in the outer instance
                if hasattr(outer, attr):
                    setattr(outer, attr, value)
                else:
                    super(this.__class__, this).__setattr__(attr, value)
            clsdict['__setattr__'] = __setattr__
            return type(cls.__name__, cls.__bases__, clsdict)
    return InnerDescriptor()

# this decorator allows dynamic inner classes, such as Java inner classes:
#  inner classes directly reference the enclosing outer class, and outer
#  class members can be reached using the __outer__ attribute
def _nestedclass(cls):
    """class decorator for a class to behave as a static inner class"""
    if hasattr(cls, '__outer__'):
        raise TypeError('Cannot set attribute "__outer__" in nested class')
    class NestedDescriptor(object):
        def __get__(self, outer, outercls):
            clsdict = cls.__dict__.copy()
            # explicit read-only reference the outer class
            clsdict['__outer__'] = outercls
            ## implicit lookup in the outer class
            ##clsdict['__getattr__'] = lambda self, attr: getattr(outercls, attr)
            def __setattr__(this, attr, value):
                # setting an attribute in the inner instance sets the
                # respective attribute in the outer class if and only if the
                # attribute is already defined in the outer class
                if hasattr(outercls, attr):
                    setattr(outercls, attr, value)
                else:
                    super(this.__class__, this).__setattr__(attr, value)
            clsdict['__setattr__'] = __setattr__
            return type(cls.__name__, cls.__bases__, clsdict)
    return NestedDescriptor()

# class decorators don't (still) support the '@' syntax, and will be invoked
#  using the classic "Inner = _deco(Inner)" syntax at least for now ;-)



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
#  its creation the constructor is no longer necessary and is later deleted;
#  we have to create in this a unique object whose hash is unique in order
#  to be able to return a single value with the __hash__ method
class NilObject(Symbol):
    """represent the CLIPS nil symbol"""
    __created = False
    __hash_placeholder = lambda : None  # see above, ugly but functional
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
    def __hash__(self):
        return NilObject.__hash_placeholder.__hash__()  # thus *not* None
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

# Converter from internal form (type, value) of CLIPS data to the
#  wrappers provided above, in order to simplify transparent conversions
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
                # facts and instances do not exist at top level anymore, thus
                #  these conversions should not be supported in this place
                #elif x == _c.INSTANCE_ADDRESS:
                #    li.append(Instance(v))
                #elif x == _c.FACT_ADDRESS:
                #    li.append(Fact(v))
                else:
                    raise TypeError("list cannot be converted")
            return Multifield(li)
        else:
            raise TypeError("malformed tuple value")
    else:
        raise TypeError("wrong argument type")

# same as above, but from Python to CLIPS
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
    # facts and instances do not exist at top level anymore, thus these
    #  conversions should not be supported in this place; the same happens
    #  below (see commented lines)
    #elif t1 == Fact:
    #    return (_c.FACT_ADDRESS, o._Fact__fact)
    #elif t1 == Instance:
    #    return (_c.INSTANCE_ADDRESS, o._Instance__instance)
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
            elif t0 in (ClipsIntegerType, ClipsFloatType,
                        ClipsStringType, ClipsSymbolType,
                        ClipsInstanceNameType, ClipsNilType):
                li.append(x.clrepr())
            #elif t0 == Fact:
            #    li.append((_c.FACT_ADDRESS, o._Fact__fact))
            #elif t0 == Instance:
            #    li.append((_c.INSTANCE_ADDRESS, o._Instance__instance))
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
                raise TypeError("list element of type %s cannot be converted" % t0)
        return (_c.MULTIFIELD, li)
    else:
        raise TypeError("value of type %s cannot be converted" % t1)

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
            elif t0 in (ClipsIntegerType, ClipsFloatType,
                        ClipsStringType, ClipsSymbolType,
                        ClipsInstanceNameType, ClipsNilType):
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
                raise TypeError("list element of type %s cannot be converted" % t0)
        return Multifield(li).clsyntax()
    else:
        raise TypeError("value of type %s cannot be converted" % t1)



# the Environment class is at the center of the whole new implementation
#  of PyCLIPS, and most of the "command" functions as well as most of the
#  classes will only belong to Environment objects (once created); please
#  note that there are no static methods and/or classes: the decorators
#  above are used to make all inner classes non-static and depending on
#  the initialization of an an instance of Environment
class Environment(object):

    # utility functions

    # conversion from the internal representation of values (see above)
    # 1) from CLIPS to Python
    def _cl2py(self, o):
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
                return self.Instance(o[1])
            elif o[0] == _c.FACT_ADDRESS:
                return self.Fact(o[1])
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
                        li.append(self.Instance(v))
                    elif x == _c.FACT_ADDRESS:
                        li.append(self.Fact(v))
                    else:
                        raise TypeError("list cannot be converted")
                return Multifield(li)
            else:
                raise TypeError("malformed tuple value")
        else:
            raise TypeError("wrong argument type")

    # 2) from Python to CLIPS
    def _py2cl(self, o):
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
        #elif t1 == self.Fact:
        elif hasattr(o, '_Fact__fact'):
            return (_c.FACT_ADDRESS, o._Fact__fact)
        #elif t1 == self.Instance:
        elif hasattr(o, '_Instance__instance'):
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
                #elif t0 == self.Fact:
                elif hasattr(o, '_Fact__fact'):
                    li.append((_c.FACT_ADDRESS, o._Fact__fact))
                #elif t0 == self.Instance:
                elif hasattr(o, '_Instance__instance'):
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
                    raise TypeError("list element of type %s cannot be converted" % t0)
            return (_c.MULTIFIELD, li)
        else:
            raise TypeError("value of type %s cannot be converted" % t1)


    # the following objects (status and debug) are created within the outer
    #  class constructor, so the outer class still does not exist when these
    #  are built, thus they cannot follow the inner class pattern; moreover
    #  the objects can only be created once, and the creation functions must
    #  not be available to the user

    # debug subsystem configuration
    def __clips_Debug(self, private_environment):
        environment_object = self
        class _clips_Debug(object):
            """object to enable/disable debugging features"""
            __env = private_environment
            __envobject = environment_object
            __created = False

            def __init__(self):
                """one-time initializer"""
                if(self.__created):
                    raise TypeError("cannot create this object twice")
                self.__created = True
                self.__watchitems = ['facts', 'rules', 'activations',
                                     'compilations', 'statistics', 'globals',
                                     'slots', 'instances', 'messages',
                                     'message-handlers', 'generic-functions',
                                     'methods', 'deffunctions',]
                # the following would modify the engine status on instantiation,
                #  which would disallow storing current environment for swapping
                for x in self.__watchitems:
                    _c.env_unwatch(self.__env, x)
                _c.env_dribbleOff(self.__env)

            def __repr__(self):
                return "<Debug Management Object>"

            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle debug status")

            def DribbleOn(self, fn):
                """enable dribble on given file"""
                _c.env_dribbleOn(self.__env, fn)

            def DribbleOff(self):
                """turn off dribble"""
                _c.env_dribbleOff(self.__env)

            def DribbleActive(self):
                """tell whether or not dribble is active"""
                return bool(_c.env_dribbleActive(self.__env))

            def __property_setFactsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "facts")
                else:
                    _c.env_unwatch(self.__env, "facts")
            def __property_getFactsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "facts"))
            FactsWatched = property(__property_getFactsWatched,
                                    __property_setFactsWatched,
                                    None, "Facts watch status")

            def __property_setRulesWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "rules")
                else:
                    _c.env_unwatch(self.__env, "rules")
            def __property_getRulesWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "rules"))
            RulesWatched = property(__property_getRulesWatched,
                                    __property_setRulesWatched,
                                    None, "Rules watch status")

            def __property_setActivationsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "activations")
                else:
                    _c.env_unwatch(self.__env, "activations")
            def __property_getActivationsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "activations"))
            ActivationsWatched = property(__property_getActivationsWatched,
                                          __property_setActivationsWatched,
                                          None, "Activations watch status")

            def __property_setCompilationsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "compilations")
                else:
                    _c.env_unwatch(self.__env, "compilations")
            def __property_getCompilationsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "compilations"))
            CompilationsWatched = property(__property_getCompilationsWatched,
                                           __property_setCompilationsWatched,
                                           None, "compilations watch status")

            def __property_setStatisticsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "statistics")
                else:
                    _c.env_unwatch(self.__env, "statistics")
            def __property_getStatisticsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "statistics"))
            StatisticsWatched = property(__property_getStatisticsWatched,
                                         __property_setStatisticsWatched,
                                         None, "statistics watch status")

            def __property_setGlobalsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "globals")
                else:
                    _c.env_unwatch(self.__env, "globals")
            def __property_getGlobalsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "globals"))
            GlobalsWatched = property(__property_getGlobalsWatched,
                                      __property_setGlobalsWatched,
                                      None, "Globals watch status")

            def __property_setSlotsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "slots")
                else:
                    _c.env_unwatch(self.__env, "slots")
            def __property_getSlotsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "slots"))
            SlotsWatched = property(__property_getSlotsWatched,
                                    __property_setSlotsWatched,
                                    None, "Slots watch status")

            def __property_setMessagesWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "messages")
                else:
                    _c.env_unwatch(self.__env, "messages")
            def __property_getMessagesWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "messages"))
            MessagesWatched = property(__property_getMessagesWatched,
                                       __property_setMessagesWatched,
                                       None, "messages watch status")

            def __property_setMessageHandlersWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "message-handlers")
                else:
                    _c.env_unwatch(self.__env, "message-handlers")
            def __property_getMessageHandlersWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "message-handlers"))
            MessageHandlersWatched = property(__property_getMessageHandlersWatched,
                                              __property_setMessageHandlersWatched,
                                              None, "MessageHandlers watch status")

            def __property_setGenericFunctionsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "generic-functions")
                else:
                    _c.env_unwatch(self.__env, "generic-functions")
            def __property_getGenericFunctionsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "generic-functions"))
            GenericFunctionsWatched = property(__property_getGenericFunctionsWatched,
                                               __property_setGenericFunctionsWatched,
                                               None, "Generic functions watch status")

            def __property_setMethodsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "methods")
                else:
                    _c.env_unwatch(self.__env, "methods")
            def __property_getMethodsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "methods"))
            MethodsWatched = property(__property_getMethodsWatched,
                                      __property_setMethodsWatched,
                                      None, "Methods watch status")

            def __property_setFunctionsWatched(self, v):
                if(v):
                    _c.env_watch(self.__env, "deffunctions")
                else:
                    _c.env_unwatch(self.__env, "deffunctions")
            def __property_getFunctionsWatched(self):
                return bool(_c.env_getWatchItem(self.__env, "deffunctions"))
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
                    _c.env_watch(self.__env, x)
            def UnwatchAll(self):
                """unwatch all items"""
                for x in self.__watchitems:
                    _c.env_unwatch(self.__env, x)
        return _clips_Debug

    # engine subsystem configuration
    def __clips_Status(self, private_environment):
        environment_object = self
        class _clips_Status(object):
            """object to access global status functions"""
            __env = private_environment
            __envobject = environment_object
            __created = False

            def __init__(self):
                """one-time initializer"""
                if(self.__created):
                    raise TypeError("cannot create this object twice")
                self.__created = True

            def __repr__(self):
                return "<Configuration Management Object>"

            def __getstate__(self):
                raise _c.ClipsError("M03: cannot pickle engine status")

            def __property_setFactDuplication(self, v):
                _c.env_setFactDuplication(self.__env, v)
            def __property_getFactDuplication(self):
                return bool(_c.env_getFactDuplication(self.__env))
            FactDuplication = property(__property_getFactDuplication,
                                       __property_setFactDuplication,
                                       None, "Fact duplication behaviour")

            def __property_setAutoFloatDividend(self, v):
                _c.env_setAutoFloatDividend(self.__env, v)
            def __property_getAutoFloatDividend(self):
                return bool(_c.env_getAutoFloatDividend(self.__env))
            AutoFloatDividend = property(__property_getAutoFloatDividend,
                                         __property_setAutoFloatDividend,
                                         None, "AutoFloatDividend behaviour")

            def __property_setDynamicConstraintChecking(self, v):
                _c.env_setDynamicConstraintChecking(self.__env, v)
            def __property_getDynamicConstraintChecking(self):
                return bool(_c.env_getDynamicConstraintChecking(self.__env))
            DynamicConstraintChecking = property(__property_getDynamicConstraintChecking,
                                                 __property_setDynamicConstraintChecking,
                                                 None,
                                                 "Dynamic constraint checking behaviour")

            def __property_setSequenceOperatorRecognition(self, v):
                _c.env_setSequenceOperatorRecognition(self.__env, v)
            def __property_getSequenceOperatorRecognition(self):
                return bool(_c.env_getSequenceOperatorRecognition(self.__env))
            SequenceOperatorRecognition = property(__property_getSequenceOperatorRecognition,
                                                   __property_setSequenceOperatorRecognition,
                                                   None,
                                                   "Sequence operator recognition behaviour")

            def __property_setStaticConstraintChecking(self, v):
                _c.env_setStaticConstraintChecking(self.__env, v)
            def __property_getStaticConstraintChecking(self):
                return bool(_c.env_getStaticConstraintChecking(self.__env))
            StaticConstraintChecking = property(__property_getStaticConstraintChecking,
                                                __property_setStaticConstraintChecking,
                                                None,
                                                "Static constraint checking behaviour")

            def __property_setIncrementalReset(self, v):
                _c.env_setIncrementalReset(self.__env, v)
            def __property_getIncrementalReset(self):
                return bool(_c.env_getIncrementalReset(self.__env))
            IncrementalReset = property(__property_getIncrementalReset,
                                        __property_setIncrementalReset,
                                        None, "Incremental reset behaviour")

            def __property_setResetGlobals(self, v):
                _c.env_setResetGlobals(self.__env, v)
            def __property_getResetGlobals(self):
                return bool(_c.env_getResetGlobals(self.__env))
            ResetGlobals = property(__property_getResetGlobals,
                                    __property_setResetGlobals,
                                    None, "ResetGlobals behaviour")

            def __property_setStrategy(self, v):
                _c.env_setStrategy(self.__env, v)
            def __property_getStrategy(self):
                return _c.env_getStrategy(self.__env)
            Strategy = property(__property_getStrategy,
                                __property_setStrategy,
                                None, "strategy behaviour")

            def __property_setSalienceEvaluation(self, v):
                _c.env_setSalienceEvaluation(self.__env, v)
            def __property_getSalienceEvaluation(self):
                return _c.env_getSalienceEvaluation(self.__env)
            SalienceEvaluation = property(__property_getSalienceEvaluation,
                                          __property_setSalienceEvaluation,
                                          None,
                                          "salience evaluation behaviour")

            def __property_setClassDefaultsMode(self, v):
                _c.env_setClassDefaultsMode(self.__env, v)
            def __property_getClassDefaultsMode(self):
                return _c.env_getClassDefaultsMode(self.__env)
            ClassDefaultsMode = property(__property_getClassDefaultsMode,
                                         __property_setClassDefaultsMode,
                                         None, "class defaults mode")
        return _clips_Status

    # constructor: it just performs basic initialization operations, because
    #  the innerclass protocol saves us from having to behave in a particular
    #  way with respect to inner classes, and inner object initialization is
    #  performed at the moment of their creation
    def __init__(self):
        """environment constructor"""
        self.__env = _c.createEnvironment()

        # create the status and debug structures: these objects are
        #  singletons; since they are built here, they can't follow
        #  the innerclass protocol
        self.EngineConfig = self.__clips_Status(self.__env)()
        self.DebugConfig = self.__clips_Debug(self.__env)()


    # the index remains as a read-only attribute
    def __property_getIndex(self):
        return _c.getEnvironmentIndex(self.__env)
    Index = property(__property_getIndex, None, None,
                     "Return index of this Environment")


    # =================================================================== #
    # High-level class for deftemplate objects
    # Treat a deftemplate as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access deftemplate objects.
    class Template(object):
        """high-level Template class (represents: deftemplate)"""

        def __init__(self, o=None):
            """create a Template object (internal)"""
            if _c.isDeftemplate(o):
                self.__deftemplate = o
            else:
                raise _c.ClipsError("M01: cannot directly create Template")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

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
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotAllowedValues(
                            self.__env, self.__deftemplate, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Cardinality(self, name):
                    """return cardinality for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotCardinality(
                            self.__env, self.__deftemplate, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def HasDefault(self, name):
                    """one of NO_DEFAULT, STATIC_DEFAULT or DYNAMIC_DEFAULT"""
                    return _c.env_deftemplateSlotDefaultP(
                        self.__env, self.__deftemplate, name)

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def DefaultValue(self, name):
                    """return default value for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotDefaultValue(
                            self.__env, self.__deftemplate, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Exists(self, name):
                    """return True if specified Slot exists"""
                    return bool(
                        _c.env_deftemplateSlotExistP(
                            self.__env, self.__deftemplate, name))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def IsMultifield(self, name):
                    """return True if specified Slot is a multifield one"""
                    return bool(
                        _c.env_deftemplateSlotMultiP(
                            self.__env, self.__deftemplate, name))

                def Names(self):
                    """return the list of Slot names"""
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotNames(
                            self.__env, self.__deftemplate))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Range(self, name):
                    """return numeric range information of specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotRange(
                            self.__env, self.__deftemplate, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def IsSinglefield(self, name):
                    """return True if specified Slot is a single field one"""
                    return bool(
                        _c.env_deftemplateSlotSingleP(
                            self.__env, self.__deftemplate, name))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Types(self, name):
                    """return names of primitive types for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_deftemplateSlotTypes(
                            self.__env, self.__deftemplate, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

            self.__Slots = __template_Slots(self.__deftemplate)
            self.__Slots._template_Slots__env = self.__env
            self.__Slots._template_Slots__envobject = self.__envobject

        def __str__(self):
            """string form of Template"""
            return _c.env_getDeftemplateName(self.__env, self.__deftemplate)

        def __repr__(self):
            """representation of Template"""
            s = repr(self.__deftemplate)[1:-1]
            return "<Template '%s': %s>" % (
                _c.env_getDeftemplateName(self.__env, self.__deftemplate), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Template"""
            o = _c.env_getNextDeftemplate(self.__env, self.__deftemplate)
            if(o):
                return self.__envobject.Template(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Template"""
            return _c.env_getDeftemplatePPForm(self.__env, self.__deftemplate)

        def Remove(self):
            """remove Template"""
            _c.env_undeftemplate(self.__env, self.__deftemplate)

        def BuildFact(self):
            """create a fact from this Template without asserting it"""
            return self.__envobject.Fact(self.__deftemplate)

        def InitialFact(self):
            """find initial Fact for this Template"""
            return self.__envobject.Fact(
                _c.env_getNextFactInTemplate(self.__env, self.__deftemplate))

        def NextFact(self, fact):
            """find initial Fact for this Template"""
            return self.__envobject.Fact(
                _c.env_getNextFactInTemplate(
                    self.__env, self.__deftemplate, fact._Fact__fact))

        def __property_getDeletable(self):
            return bool(
                _c.env_isDeftemplateDeletable(self.__env, self.__deftemplate))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Template can be deleted")

        def __property_getName(self):
            return Symbol(
                _c.env_getDeftemplateName(self.__env, self.__deftemplate))
        Name = property(__property_getName, None, None,
                        "retrieve Template name")

        def __property_getModule(self):
            return Symbol(
                _c.env_deftemplateModule(self.__env, self.__deftemplate))
        Module = property(__property_getModule,
                          None, None, "retrieve Template Module name")

        # access class slots through the internal object
        def __property_getSlots(self):
            return self.__Slots
        Slots = property(__property_getSlots,
                         None, None, "Template Slots information")

        # debugging functions and properties
        def __property_setWatch(self, v):
            _c.env_setDeftemplateWatch(self.__env, v, self.__deftemplate)
        def __property_getWatch(self):
            return _c.env_getDeftemplateWatch(self.__env, self.__deftemplate)
        Watch = property(__property_getWatch,
                         __property_setWatch,
                         None, "watch status of this Template")

    Template = _innerclass(Template)


    # =================================================================== #
    # High-level class for fact objects
    # Treat a fact as an object having an Object-Oriented interface. All
    #  functions that normally refer to a fact use the underlying low-level
    #  fact object to interact with the system.
    class Fact(object):
        """high-level Fact class (represents: fact)"""

        # class constructor - we want to initialize the fact in several
        #  ways, ie. by creation (using a Template or its underlying
        #  __deftemplate), by copy (using a Fact or its underlying __fact),
        #  or by assertion using a string; besides this, we also want to
        #  initialize some internal structs that help use the fact "the
        #  Python way" (eg. the fact slots should be grouped in a string
        #  addressed dictionary, as it would naturally be)
        def __init__(self, o):
            """create a Fact object"""
            # this on-the-fly class takes the underlying fact object, which
            #  should already exist, and accesses its slots through the
            #  functions provided by the low-level module, thus exposing a
            #  dictionary-like interface that can be used to access slots at
            #  high level
            #  NOTE: there is a hack that allows the environment version to
            #        work by trying to access the underlying environment
            #        object
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

            class __fact_Slots:
                """access fact Slots"""
                def __init__(self, fo):
                    self.__fact = fo

                @_accepts_method((str, unicode), None)
                @_forces_method(str, None)
                def __setitem__(self, name, v):
                    _c.env_putFactSlot(
                        self.__env, self.__fact, name,
                        self.__envobject._py2cl(v))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def __getitem__(self, name):
                    if not name:
                        return self.__envobject._cl2py(
                            _c.env_getFactSlot(self.__env, self.__fact))
                    else:
                        return self.__envobject._cl2py(
                            _c.env_getFactSlot(self.__env, self.__fact, name))

                def keys(self):
                    return self.__envobject._cl2py(
                        _c.env_factSlotNames(self.__env, self.__fact))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def has_key(self, k):
                    return k in map(
                        str, self.__envobject._cl2py(
                            _c.env_factSlotNames(self.__env, self.__fact)))

                def __repr__(self):
                    return "<Fact '%s' Slots>" \
                            % _c.env_getFactPPForm(
                                self.__env, self.__fact).split()[0]

                def __getstate__(self):
                    raise _c.ClipsError("M03: cannot pickle fact slots")

            # now we can instance an object of this kind, and throw the
            #  class away; however the instance must be created at the
            #  end of function body since the fact has to be created at
            #  lower level
            if _c.isFact(o):
                self.__fact = o
            elif '_Fact__fact' in dir(o) and _c.isFact(o.__fact):
                self.__fact = o.__fact
            elif _c.isDeftemplate(o):
                self.__fact = _c.env_createFact(self.__env, o)
            elif type(o) == str:
                try:
                    self.__fact = _c.env_assertString(self.__env, o)
                except:
                    raise ValueError("invalid assertion string")
            else:
                try:
                    self.__fact = _c.env_createFact(
                        self.__env, o._Template__deftemplate)
                except AttributeError:
                    raise TypeError(
                        "argument should be Fact, Template or str")
            # here the fact is created: we create an instance of it and do
            #  not care about internal class definition destiny, since it's
            #  useful that this class definition disappears from Fact
            #  dictionary
            self.__Slots = __fact_Slots(self.__fact)
            self.__Slots._fact_Slots__env = self.__env
            self.__Slots._fact_Slots__envobject = self.__envobject

        def __str__(self):
            """string form of Fact"""
            return _c.env_getFactPPForm(self.__env, self.__fact).split()[0]

        def __repr__(self):
            """representation of Fact"""
            s = repr(self.__fact)[1:-1]
            return "<Fact '%s': %s>" % (
                _c.env_getFactPPForm(self.__env, self.__fact).split()[0], s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # interface
        def Assert(self):
            """assert this Fact"""
            self.__fact = _c.env_assertFact(self.__env, self.__fact)

        def Retract(self):
            """retract this Fact"""
            _c.env_retract(self.__env, self.__fact)

        def AssignSlotDefaults(self):
            """assign Fact Slot defaults"""
            _c.env_assignFactSlotDefaults(self.__env, self.__fact)

        def Next(self):
            """return next Fact"""
            o = _c.env_getNextFact(self.__env, self.__fact)
            if(o):
                return self.__envobject.Fact(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Fact"""
            return _c.env_getFactPPForm(self.__env, self.__fact)

        def PPrint(self, ignoredefaults=True):
            """pretty-print fact, possibly including slot default values"""
            _c.routerClear("temporary")
            _c.env_ppFact(
                self.__env, self.__fact, "temporary", ignoredefaults)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def CleanPPForm(self):
            """return the pretty-print form of Fact"""
            return _c.env_getFactPPForm(
                self.__env, self.__fact).split(None, 1)[1].strip()

        # return the relation field
        def __property_getRelation(self):
            return Symbol(
                _c.env_getFactPPForm(
                    self.__env, self.__fact).split(
                        None, 1)[1].strip()[1:-1].split(None, 1)[0])
        Relation = property(__property_getRelation,
                            None, None, "fact relation symbol")

        # the list of implied slots
        def __property_getImpliedSlots(self):
            try:
                mli = self.__envobject._cl2py(
                    _c.env_getFactSlot(self.__env, self.__fact))
            except:
                mli = Multifield([])
            return mli
        ImpliedSlots = property(__property_getImpliedSlots,
                                None, None, "list of implied Slots")

        # access fact index, read only property
        def __property_getIndex(self):
            return _c.env_factIndex(self.__env, self.__fact)
        Index = property(__property_getIndex,
                         None, None, "index of this Fact")

        # access fact slots through the internal object
        def __property_getSlots(self):
            return self.__Slots
        Slots = property(__property_getSlots,
                         None, None, """Fact Slots dictionary""")

        # access Template of this Fact, read only property
        def __property_getTemplate(self):
            return self.__envobject.Template(
                _c.env_factDeftemplate(self.__env, self.__fact))
        Template = property(__property_getTemplate,
                            None, None, """Template for this Fact""")

        # tell whether or not this Fact has been retracted (if asserted)
        def __property_getExists(self):
            return bool(_c.env_factExistp(self.__env, self.__fact))
        Exists = property(__property_getExists,
                          None, None,
                          "determine if Fact has been asserted and not retracted")

    Fact = _innerclass(Fact)


    # =================================================================== #
    # High-level class for deffacts objects
    # Treat a deffacts as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access deffacts objects.
    class Deffacts(object):
        """high-level Deffacts class (represents: deffacts)"""

        def __init__(self, o):
            """create a Deffacts object (internal)"""
            if _c.isDeffacts(o):
                self.__deffacts = o
            else:
                raise _c.ClipsError("M01: cannot directly create Deffacts")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Deffacts"""
            return _c.env_getDeffactsName(self.__env, self.__deffacts)

        def __repr__(self):
            """representation of Deffacts"""
            s = repr(self.__deffacts)[1:-1]
            return "<Deffacts '%s': %s>" % (
                _c.env_getDeffactsName(self.__env, self.__deffacts), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Deffacts"""
            o = _c.env_getNextDeffacts(self.__env, self.__deffacts)
            if(o):
                return self.__envobject.Deffacts(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Deffacts"""
            return _c.env_getDeffactsPPForm(self.__env, self.__deffacts)

        def Remove(self):
            """remove Deffacts"""
            _c.env_undeffacts(self.__env, self.__deffacts)

        def __property_getName(self):
            return Symbol(_c.env_getDeffactsName(self.__env, self.__deffacts))
        Name = property(__property_getName,
                        None, None, "retrieve Deffacts name")

        def __property_getModule(self):
            return Symbol(_c.env_deffactsModule(self.__env, self.__deffacts))
        Module = property(__property_getModule,
                          None, None, "retrieve Deffacts Module name")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDeffactsDeletable(self.__env, self.__deffacts))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Deffacts can be deleted")

    Deffacts = _innerclass(Deffacts)


    # =================================================================== #
    # High-level class for defrule objects
    # Treat a defrule as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access defrule objects.
    class Rule(object):
        """high-level Rule class (represents: defrule)"""

        def __init__(self, o):
            """create a Rule object (internal)"""
            if _c.isDefrule(o):
                self.__defrule = o
            else:
                raise _c.ClipsError("M01: cannot directly create Rule")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Rule"""
            return _c.env_getDefruleName(self.__env, self.__defrule)

        def __repr__(self):
            """representation of Rule"""
            s = repr(self.__defrule)[1:-1]
            return "<Rule '%s': %s>" % (
                _c.env_getDefruleName(self.__env, self.__defrule), s)

        def __getstate__(self):
            raise _c.ClipsError(
                "M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Rule"""
            o = _c.env_getNextDefrule(self.__env, self.__defrule)
            if o:
                return self.__envobject.Rule(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Rule"""
            return _c.env_getDefrulePPForm(self.__env, self.__defrule)

        def Refresh(self):
            """refresh Rule"""
            _c.env_refresh(self.__env, self.__defrule)

        def PrintMatches(self):
            """print partial matches to standard output"""
            _c.routerClear("temporary")
            _c.env_matches(self.__env, "temporary", self.__defrule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def Remove(self):
            """remove Rule"""
            _c.env_undefrule(self.__env, self.__defrule)

        def __property_getName(self):
            return Symbol(_c.env_getDefruleName(self.__env, self.__defrule))
        Name = property(__property_getName, None, None, "retrieve Rule name")

        def __property_getModule(self):
            return Symbol(_c.env_defruleModule(self.__env, self.__defrule))
        Module = property(__property_getModule,
                          None, None, "retrieve Rule Module name")

        def __property_setBreak(self, v):
            if v:
                _c.env_setBreak(self.__env, self.__defrule)
            else:
                if _c.env_defruleHasBreakpoint(self.__env, self.__defrule):
                    _c.env_removeBreak(self.__env, self.__defrule)
        def __property_getBreak(self):
            return bool(
                _c.env_defruleHasBreakpoint(self.__env, self.__defrule))
        Breakpoint = property(__property_getBreak,
                              __property_setBreak,
                              None, "set or remove breakpoint from Rule")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDefruleDeletable(self.__env, self.__defrule))
        Deletable = property(__property_getDeletable,
                             None, None, "verify if this Rule can be deleted")

        def __property_setWatchActivations(self, v):
            _c.env_setDefruleWatchActivations(self.__env, self.__defrule, v)
        def __property_getWatchActivations(self):
            return bool(
                _c.env_getDefruleWatchActivations(self.__env, self.__defrule))
        WatchActivations = property(__property_getWatchActivations,
                                    __property_setWatchActivations,
                                    None, "Rule Activations debug status")

        def __property_setWatchFirings(self, v):
            _c.env_setDefruleWatchFirings(self.__env, self.__defrule, v)
        def __property_getWatchFirings(self):
            return bool(
                _c.env_getDefruleWatchFirings(self.__env, self.__defrule))
        WatchFirings = property(__property_getWatchFirings,
                                __property_setWatchFirings,
                                None, "Rule firings debug status")

    Rule = _innerclass(Rule)


    # =================================================================== #
    # High-level class for activation objects
    # Treat an activation as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access activation objects.
    class Activation(object):
        """high-level Activation class (represents: activation)"""

        def __init__(self, o):
            """create an Activation object (internal)"""
            if _c.isActivation(o):
                self.__activation = o
            else:
                raise _c.ClipsError("M01: cannot directly create Activation")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Activation"""
            return _c.env_getActivationName(self.__env, self.__activation)

        def __repr__(self):
            """representation of Activation"""
            return "<Activation '%s'>" % _c.env_getActivationName(
                self.__env, self.__activation)

        def __getstate__(self):
            raise _c.ClipsError(
                "M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Activation"""
            o = _c.env_getNextActivation(self.__env, self.__activation)
            if o:
                return self.__envobject.Activation(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Activation"""
            return _c.env_getActivationPPForm(self.__env, self.__activation)

        def Remove(self):
            """remove this Activation"""
            _c.env_deleteActivation(self.__env, self.__activation)

        def __property_getName(self):
            return Symbol(
                _c.env_getActivationName(self.__env, self.__activation))
        Name = property(__property_getName,
                        None, None, "retrieve Activation name")

        def __property_setSalience(self, v):
            _c.env_setActivationSalience(self.__env, self.__activation, v)
        def __property_getSalience(self):
            return _c.env_getActivationSalience(self.__env, self.__activation)
        Salience = property(__property_getSalience,
                            __property_setSalience,
                            None, "retrieve Activation salience")

    Activation = _innerclass(Activation)


    # =================================================================== #
    # High-level class for defglobal objects
    # Treat a defglobal as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access defglobal objects.
    class Global(object):
        """high-level Global class (represents: defglobal)"""

        def __init__(self, o):
            """create a Global object (internal)"""
            if _c.isDefglobal(o):
                self.__defglobal = o
            else:
                raise _c.ClipsError("M01: cannot directly create Global")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Global"""
            return _c.env_getDefglobalName(self.__env, self.__defglobal)

        def __repr__(self):
            """representation of Global"""
            s = repr(self.__defglobal)[1:-1]
            return "<Global '%s': %s>" % (
                _c.env_getDefglobalName(self.__env, self.__defglobal), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Global"""
            o = _c.env_getNextDefglobal(self.__env, self.__defglobal)
            if o:
                return self.__envobject.Global(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Global"""
            return _c.env_getDefglobalPPForm(self.__env, self.__defglobal)

        def ValueForm(self):
            """return a 'printed' form of Global value"""
            return _c.env_getDefglobalValueForm(self.__env, self.__defglobal)

        def Remove(self):
            """remove this Global"""
            _c.env_undefglobal(self.__env, self.__defglobal)

        def __property_getName(self):
            return Symbol(
                _c.env_getDefglobalName(self.__env, self.__defglobal))
        Name = property(__property_getName, None, None, "retrieve Global name")

        def __property_getModule(self):
            return Symbol(
                _c.env_defglobalModule(self.__env, self.__defglobal))
        Module = property(__property_getModule,
                          None, None, "retrieve Global Module name")

        def __property_setValue(self, v):
            _c.env_setDefglobalValue(
                self.__env, self.Name, self.__envobject._py2cl(v))
        def __property_getValue(self):
            return self.__envobject._cl2py(
                _c.env_getDefglobalValue(self.__env, self.Name))
        Value = property(__property_getValue,
                         __property_setValue,
                         None, "set/retrieve Global value")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDefglobalDeletable(self.__env, self.__defglobal))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Global can be deleted")

        def __property_setWatch(self, v):
            _c.env_setDefglobalWatch(self.__env, v, self.__defglobal)
        def __property_getWatch(self):
            return _c.env_getDefglobalWatch(self.__env, self.__defglobal)
        Watch = property(__property_getWatch,
                         __property_setWatch,
                         None, "set/retrieve Global debug status")

    Global = _innerclass(Global)


    # =================================================================== #
    # High-level class for deffunction objects
    # Treat a deffunction as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access deffunction objects.
    class Function(object):
        """high-level Function class (represents: deffunction)"""

        def __init__(self, o):
            """create a Function object (internal)"""
            if _c.isDeffunction(o):
                self.__deffunction = o
            else:
                raise _c.ClipsError("M01: cannot directly create Function")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Function"""
            return _c.env_getDeffunctionName(self.__env, self.__deffunction)

        def __repr__(self):
            """representation of Function"""
            s = repr(self.__deffunction)[1:-1]
            return "<Function '%s': %s>" % (
                _c.env_getDeffunctionName(self.__env, self.__deffunction), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Function"""
            o = _c.env_getNextDeffunction(self.__env, self.__deffunction)
            if o:
                return self.__envobject.Function(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Function"""
            return _c.env_getDeffunctionPPForm(self.__env, self.__deffunction)

        def Remove(self):
            """remove this Function"""
            _c.env_undeffunction(self.__env, self.__deffunction)

        def Call(self, *args):
            """call this Function with given arguments"""
            func = _c.env_getDeffunctionName(self.__env, self.__deffunction)
            if args:
                if(len(args) == 1 and type(args[0]) == str):
                    sargs = args[0]
                else:
                    li = []
                    for x in args:
                        t1 = type(x)
                        if t1 in (ClipsIntegerType, ClipsFloatType,
                                  ClipsStringType, ClipsSymbolType,
                                  ClipsNilType, ClipsInstanceNameType,
                                  ClipsMultifieldType):
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
                return self.__envobject._cl2py(
                    _c.env_functionCall(self.__env, func, sargs))
            else:
                return self.__envobject._cl2py(
                    _c.env_functionCall(self.__env, func))
        __call__ = Call

        def __property_getName(self):
            return Symbol(
                _c.env_getDeffunctionName(self.__env, self.__deffunction))
        Name = property(__property_getName,
                        None, None, "retrieve Function name")

        def __property_getModule(self):
            return Symbol(
                _c.env_deffunctionModule(self.__env, self.__deffunction))
        Module = property(__property_getModule,
                          None, None, "retrieve Function Module name")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDeffunctionDeletable(self.__env, self.__deffunction))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Function can be deleted")

        def __property_setWatch(self, v):
            _c.env_setDeffunctionWatch(self.__env, v, self.__deffunction)
        def __property_getWatch(self):
            return bool(
                _c.env_getDeffunctionWatch(self.__env, self.__deffunction))
        Watch = property(__property_getWatch,
                         __property_setWatch,
                         None, "set/retrieve Function debug status")

    Function = _innerclass(Function)


    # =================================================================== #
    # High-level class for defgeneric objects
    # Treat a defgeneric as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access defgeneric objects.
    class Generic(object):
        """high-level Generic class (represents: defgeneric)"""

        def __init__(self, o):
            """create a Generic function object (internal)"""
            if _c.isDefgeneric(o):
                self.__defgeneric = o
            else:
                raise _c.ClipsError("M01: cannot directly create Generic")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Generic"""
            return _c.env_getDefgenericName(self.__env, self.__defgeneric)

        def __repr__(self):
            """representation of Generic"""
            s = repr(self.__defgeneric)[1:-1]
            return "<Generic '%s': %s>" % (
                _c.env_getDefgenericName(self.__env, self.__defgeneric), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Generic"""
            o = _c.env_getNextDefgeneric(self.__env, self.__defgeneric)
            if o:
                return self.__envobject.Generic(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Generic"""
            return _c.env_getDefgenericPPForm(self.__env, self.__defgeneric)

        def Remove(self):
            """remove this Generic"""
            _c.env_undefgeneric(self.__env, self.__defgeneric)

        def Call(self, *args):
            """call this Generic with given arguments"""
            func = _c.env_getDefgenericName(self.__env, self.__defgeneric)
            if args:
                if(len(args) == 1 and type(args[0]) in (str, unicode)):
                    sargs = str(args[0])
                else:
                    li = []
                    for x in args:
                        t1 = type(x)
                        if t1 in (ClipsIntegerType, ClipsFloatType,
                                  ClipsStringType, ClipsSymbolType,
                                  ClipsNilType, ClipsInstanceNameType,
                                  ClipsMultifieldType):
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
                return self.__envobject._cl2py(
                    _c.env_functionCall(self.__env, func, sargs))
            else:
                return self.__envobject._cl2py(
                    _c.env_functionCall(self.__env, func))
        __call__ = Call

        def __property_getName(self):
            return Symbol(
                _c.env_getDefgenericName(self.__env, self.__defgeneric))
        Name = property(__property_getName,
                        None, None, "retrieve Generic name")

        def __property_getModule(self):
            return Symbol(
                _c.env_defgenericModule(self.__env, self.__defgeneric))
        Module = property(__property_getModule,
                          None, None, "retrieve Generic Module name")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDefgenericDeletable(self.__env, self.__defgeneric))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Generic can be deleted")

        def __property_setWatch(self, v):
            _c.env_setDefgenericWatch(self.__env, v, self.__defgeneric)
        def __property_getWatch(self):
            return bool(
                _c.env_getDefgenericWatch(self.__env, self.__defgeneric))
        Watch = property(__property_getWatch,
                         __property_setWatch,
                         None, "set/retrieve Generic debug status")

        # Method functions
        def MethodList(self):
            """return the list of Method indices for this Generic"""
            o = _c.env_getDefmethodList(self.__env, self.__defgeneric)
            li, mli = Multifield(self.__envobject._cl2py(o)), Multifield([])
            l = len(li) / 2
            for x in range(0, l):
                mli.append(li[2 * x + 1])
            return mli

        def MethodDescription(self, midx):
            """return the synopsis of specified Method restrictions"""
            return _c.env_getDefmethodDescription(
                self.__env, midx, self.__defgeneric)

        def MethodPPForm(self, midx):
            """return the pretty-print form of specified Method"""
            return _c.env_getDefmethodPPForm(
                self.__env, midx, self.__defgeneric)

        def MethodRestrictions(self, midx):
            """return the restrictions of specified Method"""
            return Multifield(
                self.__envobject._cl2py(
                    _c.env_getMethodRestrictions(
                        self.__env, midx, self.__defgeneric)))

        def InitialMethod(self):
            """return the index of first Method in this Generic"""
            try:
                return _c.env_getNextDefmethod(
                    self.__env, 0, self.__defgeneric)
            except:
                raise _c.ClipsError("M02: could not find any Method")

        def NextMethod(self, midx):
            """return the index of next Method in this Generic"""
            return _c.env_getNextDefmethod(
                self.__env, midx, self.__defgeneric)

        def PrintMethods(self):
            """print out Method list for this Generic"""
            _c.routerClear("temporary")
            _c.env_listDefmethods(self.__env, "temporary", self.__defgeneric)
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
                        raise TypeError(
                            "tuple or string expected as restriction")
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
            _c.env_build(self.__env, "(defmethod %s %s %s (%s) %s)" % (
                self.Name, indstr, cmtstr, rstr, actions))

        def RemoveMethod(self, midx):
            """remove specified Method"""
            _c.env_undefmethod(self.__env, midx, self.__defgeneric)

        # these are peculiar, since defmethods cannot be rendered as classes
        def WatchMethod(self, midx):
            """activate watch on specified Method"""
            _c.env_setDefmethodWatch(
                self.__env, True, midx, self.__defgeneric)

        def UnwatchMethod(self, midx):
            """deactivate watch on specified Method"""
            _c.env_setDefmethodWatch(
                self.__env, False, midx, self.__defgeneric)

        def MethodWatched(self, midx):
            """test whether or not specified Method is being watched"""
            return bool(
                _c.env_getDefmethodWatch(self.__env, midx, self.__defgeneric))

        def MethodDeletable(self, midx):
            """test whether or not specified Method can be deleted"""
            return bool(
                _c.env_isDefmethodDeletable(
                    self.__env, midx, self.__defgeneric))

    Generic = _innerclass(Generic)


    # =================================================================== #
    # High-level class for defclass objects
    # Treat a defclass as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access defclass objects.
    class Class(object):
        """high-level Class class (represents: defclass)"""

        def __init__(self, o):
            """create a Class object (internal)"""
            if _c.isDefclass(o):
                self.__defclass = o
            else:
                raise _c.ClipsError("M01: cannot directly create Class")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__
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
                    rv = self.__envobject._cl2py(
                        _c.env_classSlots(self.__env, self.__defclass, 1))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                def NamesDefined(self):
                    """return the list of Slot names"""
                    rv = self.__envobject._cl2py(
                        _c.env_classSlots(self.__env, self.__defclass, 0))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def AllowedValues(self, name):
                    """return allowed values for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotAllowedValues(
                            self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def AllowedClasses(self, name):
                    """return allowed classes for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotAllowedClasses(
                            self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Cardinality(self, name):
                    """return cardinality for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotCardinality(
                            self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def DefaultValue(self, name):
                    """return default value for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotDefaultValue(
                            self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Facets(self, name):
                    """return facet values for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotFacets(self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Range(self, name):
                    """return numeric range information of specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotRange(self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Sources(self, name):
                    """return source class names for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotSources(self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Types(self, name):
                    """return names of primitive types for specified Slot"""
                    rv = self.__envobject._cl2py(
                        _c.env_slotTypes(self.__env, self.__defclass, name))
                    if type(rv) in (tuple, list):
                        return Multifield(rv)
                    else:
                        return rv

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def HasDirectAccess(self, name):
                    """return True if specified Slot is directly accessible"""
                    return bool(
                        _c.env_slotDirectAccessP(
                            self.__env, self.__defclass, name))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def Exists(self, name):
                    """return True if specified Slot exists or is inherited"""
                    return bool(
                        _c.env_slotExistP(
                            self.__env, self.__defclass, name, 1))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def ExistsDefined(self, name):
                    """return True if specified Slot is defined in this Class"""
                    return bool(
                        _c.env_slotExistP(
                            self.__env, self.__defclass, name, 0))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def IsInitable(self, name):
                    """return True if specified Slot is initable"""
                    return bool(
                        _c.env_slotInitableP(
                            self.__env, self.__defclass, name))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def IsPublic(self, name):
                    """return True if specified Slot is public"""
                    return bool(
                        _c.env_slotPublicP(self.__env, self.__defclass, name))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def IsWritable(self, name):
                    """return True if specified Slot is writable"""
                    return bool(
                        _c.env_slotWritableP(
                            self.__env, self.__defclass, name))

            self.__Slots = __class_Slots(self.__defclass)
            # the following try/except blocks are to enable companion versions
            self.__Slots._class_Slots__env = self.__env
            self.__Slots._class_Slots__envobject = self.__envobject

        def __str__(self):
            """string form of Class"""
            return _c.env_getDefclassName(self.__env, self.__defclass)

        def __repr__(self):
            """representation of Class"""
            s = repr(self.__defclass)[1:-1]
            return "<Class '%s': %s>" % (
                _c.env_getDefclassName(self.__env, self.__defclass), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Class"""
            o = _c.env_getNextDefclass(self.__env, self.__defclass)
            if o:
                return self.__envobject.Class(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Class"""
            return _c.env_getDefclassPPForm(self.__env, self.__defclass)

        def Description(self):
            """return a summary of Class description"""
            _c.routerClear("temporary")
            _c.env_describeClass(self.__env, "temporary", self.__defclass)
            return _c.routerRead("temporary").strip()

        def IsSubclassOf(self, o):
            """test whether this Class is a subclass of specified Class"""
            return bool(
                _c.env_subclassP(self.__env, self.__defclass, o.__defclass))

        def IsSuperclassOf(self, o):
            """test whether this Class is a superclass of specified Class"""
            return bool(
                _c.env_superclassP(self.__env, self.__defclass, o.__defclass))

        def Subclasses(self, inherit=True):
            """return the names of subclasses"""
            return Multifield(
                self.__envobject._cl2py(
                    _c.env_classSubclasses(
                        self.__env, self.__defclass, inherit)))

        def Superclasses(self, inherit=True):
            """return the names of superclasses"""
            return Multifield(
                self.__envobject._cl2py(
                    _c.env_classSuperclasses(
                        self.__env, self.__defclass, inherit)))

        @_accepts_method((str, unicode))
        @_forces_method(str)
        def RawInstance(self, name):
            """create an empty Instance of this Class with specified name"""
            return self.__envobject.Instance(
                _c.env_createRawInstance(self.__env, self.__defclass, name))

        def InitialInstance(self):
            """return initial Instance of this Class"""
            try:
                return self.__envobject.Instance(
                    _c.env_getNextInstanceInClass(
                        self.__env, self.__defclass))
            except:
                raise _c.ClipsError("M02: could not find any Instance")

        def NextInstance(self, instance):
            """return next Instance of this Class"""
            i = _c.env_getNextInstanceInClass(
                self.__env, self.__defclass, instance._Instance__instance)
            if _c.isInstance(i):
                return self.__envobject.Instance(i)
            else:
                return None

        def InitialSubclassInstance(self):
            """return initial instance of this Class and subclasses"""
            try:
                return self.__envobject.Instance(
                    _c.env_getNextInstanceInClassAndSubclasses(
                        self.__env, self.__defclass))
            except:
                raise _c.ClipsError("M02: could not find any Instance")

        def NextSubclassInstance(self, instance):
            """return next instance of this Class and subclasses"""
            i = _c.env_getNextInstanceInClassAndSubclasses(
                self.__env, self.__defclass, instance._Instance__instance)
            if _c.isInstance(i):
                return self.__envobject.Instance(i)
            else:
                return None

        def Remove(self):
            """remove this Class"""
            _c.env_undefclass(self.__env, self.__defclass)

        @_accepts_method((str, unicode), (str, unicode), None)
        @_forces_method(str, str, None)
        def BuildSubclass(self, name, text="", comment=None):
            """build a subclass of this Class with specified name and body"""
            if comment:
                cmtstr = '"%s"' % str(comment).replace('"', '\\"')
            else:
                cmtstr = ""
            clname = _c.env_getDefclassName(self.__env, self.__defclass)
            cltext = "(is-a %s)" % clname + text
            construct = "(defclass %s %s %s)" % (name, cmtstr, cltext)
            _c.env_build(self.__env, construct)
            return self.__envobject.Class(
                _c.env_findDefclass(self.__env, name))

        @_accepts_method((str, unicode), (str, unicode))
        @_forces_method(str, str)
        def BuildInstance(self, name, overrides=""):
            """build an instance of this class overriding specified slots"""
            clname = _c.env_getDefclassName(self.__env, self.__defclass)
            cmdstr = "(%s of %s %s)" % (name, clname, overrides)
            return self.__envobject.Instance(
                _c.env_makeInstance(self.__env, cmdstr))

        def __property_getName(self):
            return Symbol(
                _c.env_getDefclassName(self.__env, self.__defclass))
        Name = property(__property_getName,
                        None, None, "retrieve Class name")

        def __property_getModule(self):
            return Symbol(
                _c.env_defclassModule(self.__env, self.__defclass))
        Module = property(__property_getModule,
                          None, None, "retrieve Class Module name")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDefclassDeletable(self.__env, self.__defclass))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Class can be deleted")

        def __property_getAbstract(self):
            return bool(_c.env_classAbstractP(self.__env, self.__defclass))
        Abstract = property(__property_getAbstract,
                            None, None,
                            "verify if this Class is abstract or not")

        def __property_getReactive(self):
            return bool(_c.env_classReactiveP(self.__env, self.__defclass))
        Reactive = property(__property_getReactive,
                            None, None,
                            "verify if this Class is reactive or not")

        def __property_setWatchSlots(self, v):
            _c.env_setDefclassWatchSlots(self.__env, v, self.__defclass)
        def __property_getWatchSlots(self):
            return bool(
                _c.env_getDefclassWatchSlots(self.__env, self.__defclass))
        WatchSlots = property(__property_getWatchSlots,
                              __property_setWatchSlots,
                              None, "set/retrieve Slot debug status")

        def __property_setWatchInstances(self, v):
            _c.env_setDefclassWatchInstances(self.__env, v, self.__defclass)
        def __property_getWatchInstances(self):
            return bool(
                _c.env_getDefclassWatchInstances(
                    self.__env, self.__defclass))
        WatchInstances = property(__property_getWatchInstances,
                                  __property_setWatchInstances,
                                  None, "set/retrieve Instance debug status")

        # access class slots through the internal object
        def __property_getSlots(self):
            return self.__Slots
        Slots = property(__property_getSlots,
                         None, None, "Class Slots information")

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
                raise ValueError(
                    "htype must be AROUND, BEFORE, PRIMARY or AFTER")
            if type(args) in (tuple, list):
                sargs = " ".join(args)
            elif args is None:
                sargs = ""
            else:
                sargs = str(args)
            hclass = _c.env_getDefclassName(self.__env, self.__defclass)
            construct = "(defmessage-handler %s %s %s %s (%s) %s)" % (
                hclass, name, htype, cmtstr, sargs, text)
            _c.env_build(self.__env, construct)
            return _c.env_findDefmessageHandler(
                self.__env, self.__defclass, name, htype)

        @_accepts_method((str, unicode), None)
        @_forces_method(str, None)
        def MessageHandlerIndex(self, name, htype=PRIMARY):
            """find the specified MessageHandler"""
            htype = htype.lower()
            if htype in (AROUND, BEFORE, PRIMARY, AFTER):
                return _c.env_findDefmessageHandler(
                    self.__env, self.__defclass, name, htype)
            else:
                raise ValueError(
                    "htype must be in AROUND, BEFORE, PRIMARY, AFTER")

        def MessageHandlerName(self, index):
            """return name of specified MessageHandler"""
            return Symbol(
                _c.env_getDefmessageHandlerName(
                    self.__env, self.__defclass, index))

        def MessageHandlerPPForm(self, index):
            """return the pretty-print form of specified MessageHandler"""
            return _c.env_getDefmessageHandlerPPForm(
                self.__env, self.__defclass, index)

        def MessageHandlerType(self, index):
            """return type of specified MessageHandler"""
            return _c.env_getDefmessageHandlerType(
                self.__env, self.__defclass, index)

        def MessageHandlerWatched(self, index):
            """return watch status of specified MessageHandler"""
            return bool(
                _c.env_getDefmessageHandlerWatch(
                    self.__env, self.__defclass, index))

        def MessageHandlerDeletable(self, index):
            """return True if specified MessageHandler can be deleted"""
            return bool(
                _c.env_isDefmessageHandlerDeletable(
                    self.__env, self.__defclass, index))

        def NextMessageHandlerIndex(self, index):
            """return index of next MessageHandler wrt. specified"""
            return _c.env_getNextDefmessageHandler(
                self.__env, self.__defclass, index)

        def RemoveMessageHandler(self, index):
            """remove the specified MessageHandler"""
            return _c.env_undefmessageHandler(
                self.__env, self.__defclass, index)

        def WatchMessageHandler(self, index):
            """watch specified MessageHandler"""
            return _c.env_setDefmessageHandlerWatch(
                self.__env, True, self.__defclass, index)

        def UnwatchMessageHandler(self, index):
            """unwatch specified MessageHandler"""
            return _c.env_setDefmessageHandlerWatch(
                self.__env, False, self.__defclass, index)

        def MessageHandlerList(self):
            """return list of MessageHandler constructs of this Class"""
            o = _c.env_getDefmessageHandlerList(
                self.__env, self.__defclass, False)
            li, rv = Multifield(self.__envobject._cl2py(o)), []
            l = len(li) / 3
            for x in range(0, l):
                rv.append(
                    Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
            return Multifield(rv)

        def AllMessageHandlerList(self):
            """return list of MessageHandlers of this Class and superclasses"""
            o = _c.env_getDefmessageHandlerList(
                self.__env, self.__defclass, True)
            li, rv = Multifield(self.__envobject._cl2py(o)), []
            l = len(li) / 3
            for x in range(0, l):
                rv.append(
                    Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
            return Multifield(rv)

        def PrintMessageHandlers(self):
            """print list of all MessageHandlers of this Class"""
            _c.routerClear("temporary")
            _c.env_listDefmessageHandlers(
                self.__env, "temporary", self.__defclass)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def PrintAllMessageHandlers(self):
            """print list of MessageHandlers of this Class and superclasses"""
            _c.routerClear("temporary")
            _c.env_listDefmessageHandlers(
                self.__env, "temporary", self.__defclass, 1)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        @_accepts_method((str, unicode))
        @_forces_method(str)
        def PreviewSend(self, msgname):
            """print list of MessageHandlers suitable for specified message"""
            _c.routerClear("temporary")
            _c.env_previewSend(
                self.__env, "temporary", self.__defclass, msgname)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

    Class = _innerclass(Class)


    # =================================================================== #
    # High-level class for instance objects
    # Treat an instance as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access instance objects.
    class Instance(object):
        """high-level Instance class (represents: instance)"""

        def __init__(self, o):
            """create an Instance object (internal)"""
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__
            # this on-the-fly class takes the underlying instance object,
            #  which should already exist, and accesses its slots through
            #  the functions provided by the low-level module, thus exposing
            #  a dictionary-like interface that can be used to access slots
            #  at high level
            #  NOTE: there is a hack that allows the environment version to
            #        work by trying to access the underlying environment
            #        object
            class __instance_Slots:
                """access instance Slots"""
                def __init__(self, io):
                    self.__instance = io

                @_accepts_method((str, unicode), None)
                @_forces_method(str, None)
                def __setitem__(self, name, v):
                    _c.env_directPutSlot(
                        self.__env, self.__instance, name,
                        self.__envobject._py2cl(v))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def __getitem__(self, name):
                    return self.__envobject._cl2py(
                        _c.env_directGetSlot(
                            self.__env, self.__instance, name))

                def keys(self):
                    return map(
                        str, list(self.__envobject.Instance(
                            self.__instance).Class.Slots.Names()))

                @_accepts_method((str, unicode))
                @_forces_method(str)
                def has_key(self, k):
                    return bool(
                        k in map(
                            str, list(self.__envobject.Instance(
                                self.__instance).Class.Slots.Names())))

                def __repr__(self):
                    return "<Instance [%s] Slots>" \
                           % _c.env_getInstanceName(
                               self.__env, self.__instance)

                def __getstate__(self):
                    raise _c.ClipsError("M03: cannot pickle instance slots")
            if _c.isInstance(o):
                self.__instance = o
            else:
                raise _c.ClipsError("M01: cannot directly create Instance")
            self.__Slots = __instance_Slots(self.__instance)
            self.__Slots._instance_Slots__env = self.__env
            self.__Slots._instance_Slots__envobject = self.__envobject

        def __str__(self):
            """string form of Instance"""
            return _c.env_getInstanceName(self.__env, self.__instance)

        def __repr__(self):
            """representation of Instance"""
            s = repr(self.__instance)[1:-1]
            return "<Instance [%s]: %s>" % (
                _c.env_getInstanceName(self.__env, self.__instance), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Instance"""
            o = _c.env_getNextInstance(self.__env, self.__instance)
            if o:
                return self.__envobject.Instance(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Instance"""
            return _c.env_getInstancePPForm(self.__env, self.__instance)

        def IsValid(self):
            """determine if this Instance is still valid"""
            return bool(
                _c.env_validInstanceAddress(self.__env, self.__instance))

        def Remove(self):
            """remove this Instance"""
            _c.env_unmakeInstance(self.__env, self.__instance)

        def DirectRemove(self):
            """directly remove this Instance"""
            _c.env_deleteInstance(self.__env, self.__instance)

        @_accepts_method((str, unicode))
        @_forces_method(str)
        def GetSlot(self, slotname):
            """retrieve value of specified Slot"""
            return self.__envobject._cl2py(
                _c.env_directGetSlot(self.__env, self.__instance, slotname))
        SlotValue = GetSlot

        @_accepts_method((str, unicode), None)
        @_forces_method(str, None)
        def PutSlot(self, slotname, value):
            """set value of specified Slot"""
            _c.env_directPutSlot(
                self.__env, self.__instance, slotname,
                self.__envobject._py2cl(value))
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
                elif t in (ClipsIntegerType, ClipsFloatType,
                           ClipsStringType, ClipsSymbolType,
                           ClipsNilType, ClipsInstanceNameType,
                           ClipsMultifieldType):
                    sargs = _py2clsyntax(args)
                elif t in (tuple, list):
                    li = []
                    for x in args:
                        t1 = type(x)
                        if t1 in (ClipsIntegerType, ClipsFloatType,
                                  ClipsStringType, ClipsSymbolType,
                                  ClipsNilType, ClipsInstanceNameType,
                                  ClipsMultifieldType):
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
                return self.__envobject._cl2py(
                    _c.env_send(self.__env, self.__instance, msg, sargs))
            else:
                return self.__envobject._cl2py(
                    _c.env_send(self.__env, self.__instance, msg))

        def __property_getName(self):
            return InstanceName(
                _c.env_getInstanceName(self.__env, self.__instance))
        Name = property(__property_getName,
                        None, None, "retrieve Instance name")

        def __property_getClass(self):
            return self.__envobject.Class(
                _c.env_getInstanceClass(self.__env, self.__instance))
        Class = property(__property_getClass,
                         None, None, "retrieve Instance class")

        # access instance slots through the internal object
        def __property_getSlots(self):
            return self.__Slots
        Slots = property(__property_getSlots,
                         None, None, "Instance Slots information")

    Instance = _innerclass(Instance)


    # =================================================================== #
    # High-level class for definstances objects
    # Treat definstances as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access definstances objects.
    class Definstances(object):
        """high-level Definstances class (represents: definstances)"""

        def __init__(self, o):
            """create a Definstances object (internal)"""
            if _c.isDefinstances(o):
                self.__definstances = o
            else:
                raise _c.ClipsError(
                    "M01: cannot directly create Definstances")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Definstances"""
            return _c.env_getDefinstancesName(
                self.__env, self.__definstances)

        def __repr__(self):
            """representation of Definstances"""
            s = repr(self.__definstances)[1:-1]
            return "<Definstances '%s': %s>" % (
                _c.env_getDefinstancesName(
                    self.__env, self.__definstances), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Definstances"""
            o = _c.env_getNextDefinstances(self.__env, self.__definstances)
            if o:
                return self.__envobject.Definstances(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Definstances"""
            return _c.env_getDefinstancesPPForm(
                self.__env, self.__definstances)

        def Remove(self):
            """delete this Definstances object"""
            _c.env_undefinstances(self.__env, self.__definstances)

        def __property_getModule(self):
            return Symbol(
                _c.env_definstancesModule(self.__env, self.__definstances))
        Module = property(__property_getModule,
                          None, None, "retrieve Definstances module")

        def __property_getName(self):
            return Symbol(
                _c.env_getDefinstancesName(self.__env, self.__definstances))
        Name = property(__property_getName,
                        None, None, "retrieve Definstances name")

        def __property_getDeletable(self):
            return bool(
                _c.env_isDefinstancesDeletable(
                    self.__env, self.__definstances))
        Deletable = property(__property_getDeletable,
                             None, None,
                             "verify if this Definstances can be deleted")

    Definstances = _innerclass(Definstances)


    # =================================================================== #
    # High-level class for defmodule objects
    # Treat a defmodule as an object having an Object-Oriented interface.
    #  Implements all the functions needed to access defmodule objects.
    class Module(object):
        """high-level Module class (represents: defmodule)"""

        def __init__(self, o):
            """create a Module object (internal)"""
            if _c.isDefmodule(o):
                self.__defmodule = o
            else:
                raise _c.ClipsError("M01: cannot directly create Module")
            self.__env = self.__outer__._Environment__env
            self.__envobject = self.__outer__

        def __str__(self):
            """string form of Module"""
            return _c.env_getDefmoduleName(self.__env, self.__defmodule)

        def __repr__(self):
            """representation of Module"""
            s = repr(self.__defmodule)[1:-1]
            return "<Module '%s': %s>" % (
                _c.env_getDefmoduleName(self.__env, self.__defmodule), s)

        def __getstate__(self):
            raise _c.ClipsError("M03: cannot pickle objects of type '%s'"
                  % self.__class__.__name__)

        # Interface
        def Next(self):
            """return next Module"""
            o = _c.env_getNextDefmodule(self.__env, self.__defmodule)
            if(o):
                return self.__envobject.Module(o)
            else:
                return None

        def PPForm(self):
            """return the pretty-print form of Module"""
            return _c.env_getDefmodulePPForm(self.__env, self.__defmodule)

        def SetCurrent(self):
            """make this the current Module"""
            _c.env_setCurrentModule(self.__env, self.__defmodule)

        def SetFocus(self):
            """set focus to this Module"""
            _c.env_focus(self.__env, self.__defmodule)

        def __property_getName(self):
            return Symbol(
            _c.env_getDefmoduleName(self.__env, self.__defmodule))
        Name = property(__property_getName, None, None, "return Module name")

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
            construct = "(deftemplate %s::%s %s %s)" % (
                mname, name, cmtstr, text)
            _c.env_build(self.__env, construct)
            return self.__envobject.Template(
                _c.env_findDeftemplate(self.__env, "%s::%s" % (mname, name)))

        def TemplateList(self):
            """return list of Template names"""
            o = _c.env_getDeftemplateList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintTemplates(self):
            """print Templates to standard output"""
            _c.routerClear("temporary")
            _c.env_listDeftemplates(self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        # Facts
        def FactList(self):
            """return list of Facts in this Module"""
            o, li = _c.env_getFactList(self.__env, self.__defmodule), []
            if o is not None:
                for x in o[1]:
                    if x[0] == _c.FACT_ADDRESS:
                        li.append(self.__envobject.Fact(x[1]))
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
            construct = "(deffacts %s::%s %s %s)" % (
                mname, name, cmtstr, text)
            _c.env_build(self.__env, construct)
            return self.__envobject.Deffacts(
                _c.env_findDeffacts(self.__env, "%s::%s" % (mname, name)))

        def DeffactsList(self):
            """return a list of Deffacts names in this Module"""
            o = _c.env_getDeffactsList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintDeffacts(self):
            """print Deffacts to standard output"""
            _c.routerClear("temporary")
            _c.env_listDeffacts(self.__env, "temporary", self.__defmodule)
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
            _c.env_build(self.__env, construct)
            return self.__envobject.Rule(
                _c.env_findDefrule(self.__env, "%s::%s" % (mname, name)))

        def RuleList(self):
            """return a list of Rule names in this Module"""
            o = _c.env_getDefruleList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintRules(self):
            """print Rules to standard output"""
            _c.routerClear("temporary")
            _c.env_listDefrules(self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def PrintBreakpoints(self):
            """print breakpoints to standard output"""
            _c.routerClear("temporary")
            _c.env_showBreaks(self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        # Agenda
        def PrintAgenda(self):
            """print Agenda Rules to standard output"""
            _c.routerClear("temporary")
            _c.env_agenda(self.__env, "temporary")
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def RefreshAgenda(self):
            """refresh Agenda for this Module"""
            _c.env_refreshAgenda(self.__env, self.__defmodule)

        def ReorderAgenda(self):
            """reorder Agenda for this Module"""
            _c.env_reorderAgenda(self.__env, self.__defmodule)

        # Globals
        @_accepts_method((str, unicode), None)
        @_forces_method(str, None)
        def BuildGlobal(self, name, value=Nil):
            """build a Global variable with specified name and value"""
            mname = self.Name
            if type(value)  in (str, ClipsStringType):
                value = '"%s"' % value
            construct = "(defglobal %s ?*%s* = %s)" % (mname, name, value)
            _c.env_build(self.__env, construct)
            return self.__envobject.Global(
                _c.env_findDefglobal(self.__env, "%s::%s" % (mname, name)))

        def GlobalList(self):
            """return the list of Global variable names"""
            o = _c.env_getDefglobalList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintGlobals(self):
            """print list of Global variables to standard output"""
            _c.routerClear("temporary")
            _c.env_listDefglobals(self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        def ShowGlobals(self):
            """print list of Global variables and values to standard output"""
            _c.routerClear("temporary")
            _c.env_showDefglobals(self.__env, "temporary", self.__defmodule)
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
            _c.env_build(self.__env, construct)
            return self.__envobject.Function(
                _c.env_findDeffunction(self.__env, "%s::%s" % (mname, name)))

        def FunctionList(self):
            """return the list of Function names"""
            o = _c.env_getDeffunctionList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintFunctions(self):
            """print list of Functions to standard output"""
            _c.routerClear("temporary")
            _c.env_listDeffunctions(
                self.__env, "temporary", self.__defmodule)
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
            _c.env_build(self.__env, construct)
            return self.__envobject.Generic(
                _c.env_findDefgeneric(self.__env, "%s::%s" % (mname, name)))

        def GenericList(self):
            """return the list of Generic names"""
            o = _c.env_getDefgenericList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintGenerics(self):
            """print list of Generics to standard output"""
            _c.routerClear("temporary")
            _c.env_listDefgenerics(self.__env, "temporary", self.__defmodule)
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
            construct = "(defclass %s::%s %s %s)" % (
                mname, name, cmtstr, text)
            _c.env_build(self.__env, construct)
            return self.__envobject.Class(
                _c.env_findDefclass(self.__env, "%s::%s" % (mname, name)))

        def ClassList(self):
            """return the list of Class names"""
            o = _c.env_getDefclassList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintClasses(self):
            """print list of Class to standard output"""
            _c.routerClear("temporary")
            _c.env_listDefclasses(self.__env, "temporary", self.__defmodule)
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
            return self.__envobject.Instance(
                _c.env_makeInstance(self.__env, cmdstr))

        @_forces_method(str)
        def PrintInstances(self, classname=None):
            """print Instances to standard output"""
            _c.routerClear("temporary")
            if classname:
                _c.env_instances(
                    self.__env, "temporary", self.__defmodule, classname,
                    False)
            else:
                _c.env_instances(self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

        @_forces_method(str)
        def PrintSubclassInstances(self, classname):
            """print Instances to standard output"""
            _c.routerClear("temporary")
            _c.env_instances(
                self.__env, "temporary", self.__defmodule, classname, True)
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
            _c.env_build(self.__env, construct)
            return self.__envobject.Definstances(
                _c.env_findDefinstances(self.__env, name))

        def DefinstancesList(self):
            """retrieve list of all Definstances names"""
            o = _c.env_getDefinstancesList(self.__env, self.__defmodule)
            return Multifield(self.__envobject._cl2py(o))

        def PrintDefinstances(self):
            """print list of all Definstances to standard output"""
            _c.routerClear("temporary")
            _c.env_listDefinstances(
                self.__env, "temporary", self.__defmodule)
            s = _c.routerRead("temporary")
            if s:
                _sys.stdout.write(s)

    Module = _innerclass(Module)


    # =================================================================== #
    # functions involving Templates

    def InitialTemplate(self):
        """return first Template in environment"""
        try:
            return self.Template(_c.env_getNextDeftemplate(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Template")

    def PrintTemplates(self):
        """print Templates to standard output"""
        _c.routerClear("temporary")
        _c.env_listDeftemplates(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def TemplateList(self):
        """return a list of Template names"""
        o = _c.env_getDeftemplateList(self.__env)
        return Multifield(self._cl2py(o))    # should be all strings

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindTemplate(self, s):
        """find a Template by name"""
        return self.Template(_c.env_findDeftemplate(self.__env, s))

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildTemplate(self, name, text, comment=None):
        """build a Template object with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(deftemplate %s %s %s)" % (name, cmtstr, text)
        _c.env_build(self.__env, construct)
        return self.Template(_c.env_findDeftemplate(self.__env, name))


    # =================================================================== #
    # functions involving facts

    def Assert(self, o):
        """assert a Fact from a string or constructed Fact object"""
        if '_Fact__fact' in dir(o) and _c.isFact(o._Fact__fact):
            return o.Assert()
        elif type(o) in (str, unicode):
            return self.Fact(_c.env_assertString(self.__env, str(o)))
        else:
            raise TypeError("expected a string or a Fact")

    def InitialFact(self):
        """return first Fact in environment"""
        try:
            return self.Fact(_c.env_getNextFact(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Fact")

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def LoadFacts(self, filename):
        """load Facts from file"""
        _c.env_loadFacts(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def LoadFactsFromString(self, s):
        """load Fact objects from a string"""
        _c.env_loadFactsFromString(self.__env, s)

    @_accepts_method((str, unicode), (str, unicode))
    @_forces_method(str, str)
    def SaveFacts(self, filename, mode=LOCAL_SAVE):
        """save current Facts to file"""
        _c.env_saveFacts(self.__env, _os.path.normpath(filename), mode)

    def PrintFacts(self):
        """print Facts to standard output"""
        _c.routerClear("temporary")
        _c.env_facts(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def FactListChanged(self):
        """test whether Fact list is changed since last call"""
        rv = bool(_c.env_getFactListChanged(self.__env))
        _c.env_setFactListChanged(self.__env, False)
        return rv

    def FactList(self):
        """return list of Facts in current module"""
        o, li = _c.env_getFactList(self.__env), []
        if o is not None:
            for x in o[1]:
                if x[0] == _c.FACT_ADDRESS:
                    li.append(self.Fact(x[1]))
        return li


    # =================================================================== #
    # functions involving deffacts

    def InitialDeffacts(self):
        """return first Deffacts"""
        try:
            return self.Deffacts(_c.env_getNextDeffacts(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Deffacts")

    def DeffactsList(self):
        """return a list of Deffacts names in current module"""
        o = _c.env_getDeffactsList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindDeffacts(self, s):
        """find a Deffacts by name"""
        try:
            return self.Deffacts(_c.env_findDeffacts(self.__env, s))
        except:
            raise _c.ClipsError("M02: could not find Deffacts '%s'" % s)

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildDeffacts(self, name, text, comment=None):
        """build a Deffacts object with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(deffacts %s %s %s)" % (name, cmtstr, text)
        _c.env_build(self.__env, construct)
        return self.Deffacts(_c.env_findDeffacts(self.__env, name))

    def PrintDeffacts(self):
        """print Deffacts to standard output"""
        _c.routerClear("temporary")
        _c.env_listDeffacts(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Rules

    def InitialRule(self):
        """return first Rule"""
        try:
            return self.Rule(_c.env_getNextDefrule(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Rule")

    def RuleList(self):
        """return a list of Rule names in current module"""
        o = _c.env_getDefruleList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindRule(self, s):
        """find a Rule by name"""
        try:
            return self.Rule(_c.env_findDefrule(self.__env, s))
        except:
            raise _c.ClipsError("M02: could not find defrule '%s'" % s)

    @_accepts_method((str, unicode), (str, unicode), (str, unicode), None)
    @_forces_method(str, str, str, None)
    def BuildRule(self, name, lhs, rhs, comment=None):
        """build a Rule object with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(defrule %s %s %s => %s)" % (name, cmtstr, lhs, rhs)
        _c.env_build(self.__env, construct)
        return self.Rule(_c.env_findDefrule(self.__env, name))

    def PrintRules(self):
        """print Rules to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefrules(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def PrintBreakpoints(self):
        """print breakpoints to standard output"""
        _c.routerClear("temporary")
        _c.env_showBreaks(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Modules

    def InitialModule(self):
        """return first Module"""
        try:
            return self.Module(_c.env_getNextDefmodule(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Module")

    def ModuleList(self):
        """return the list of Module names"""
        o = _c.env_getDefmoduleList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindModule(self, name):
        """find a Module by name"""
        return self.Module(_c.env_findDefmodule(self.__env, name))

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildModule(self, name, text="", comment=None):
        """build a Module with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(defmodule %s %s %s)" % (name, cmtstr, text)
        _c.env_build(self.__env, construct)
        return self.Module(_c.env_findDefmodule(self.__env, name))

    def PrintModules(self):
        """print list of Modules to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefmodules(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Defglobals

    def InitialGlobal(self):
        """return first Global variable"""
        try:
            return self.Global(_c.env_getNextDefglobal(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Global")

    def GlobalList(self):
        """return the list of Global variable names"""
        o = _c.env_getDefglobalList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindGlobal(self, name):
        """find a Global variable by name"""
        return self.Global(_c.env_findDefglobal(self.__env, name))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def BuildGlobal(self, name, value=Nil):
        """build a Global variable with specified name and body"""
        if type(value) in (str, unicode, ClipsStringType):
            value = '"%s"' % str(value)
        construct = "(defglobal ?*%s* = %s)" % (name, value)
        _c.env_build(self.__env, construct)
        return self.Global(_c.env_findDefglobal(self.__env, "%s" % name))

    def GlobalsChanged(self):
        """test whether or not Global variables have changed since last call"""
        rv = bool(_c.env_getGlobalsChanged(self.__env))
        _c.env_setGlobalsChanged(self.__env, False)
        return rv

    def PrintGlobals(self):
        """print list of Global variables to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefglobals(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def ShowGlobals(self):
        """print list of Global variables and values to standard output"""
        _c.routerClear("temporary")
        _c.env_showDefglobals(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Functions

    def InitialFunction(self):
        """return first Function"""
        try:
            return self.Function(_c.env_getNextDeffunction(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Function")

    def FunctionList(self):
        """return the list of Function names"""
        o = _c.env_getDeffunctionList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindFunction(self, name):
        """find a Function by name"""
        return self.Function(_c.env_findDeffunction(self.__env, name))

    @_accepts_method((str, unicode), None, (str, unicode), None)
    @_forces_method(str, None, str, None)
    def BuildFunction(self, name, args, text, comment=None):
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
        _c.env_build(self.__env, construct)
        return self.Function(_c.env_findDeffunction(self.__env, name))

    def PrintFunctions(self):
        """print list of Functions to standard output"""
        _c.routerClear("temporary")
        _c.env_listDeffunctions(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    # =================================================================== #
    # functions involving Generics

    def InitialGeneric(self):
        """return first Generic"""
        try:
            return self.Generic(_c.env_getNextDefgeneric(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Generic")

    def GenericList(self):
        """return the list of Generic names"""
        o = _c.env_getDefgenericList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindGeneric(self, name):
        """find a Generic by name"""
        return self.Generic(_c.env_findDefgeneric(self.__env, name))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def BuildGeneric(self, name, comment=None):
        """build a Generic with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(defgeneric %s %s)" % (name, cmtstr)
        _c.env_build(self.__env, construct)
        return self.Generic(_c.env_findDefgeneric(self.__env, name))

    def PrintGenerics(self):
        """print list of Generics to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefgenerics(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def MethodList(self):
        """return the list of all Methods"""
        o = self._cl2py(_c.env_getDefmethodList(self.__env))
        li = Multifield([])
        l = len(o) / 2
        for x in range(l):
            li.append(Multifield([o[2 * x], o[2 * x + 1]]))
        return li


    # =================================================================== #
    # functions involving Classes

    def InitialClass(self):
        """retrieve first Class"""
        try:
            return self.Class(_c.env_getNextDefclass(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Class")

    def ClassList(self):
        """return the list of Class names"""
        o = _c.env_getDefclassList(self.__env)
        return Multifield(self._cl2py(o))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindClass(self, name):
        """find a Class by name"""
        return self.Class(_c.env_findDefclass(self.__env, name))

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildClass(self, name, text, comment=None):
        """build a Class with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(defclass %s %s %s)" % (name, cmtstr, text)
        _c.env_build(self.__env, construct)
        return self.Class(_c.env_findDefclass(self.__env, name))

    def PrintClasses(self):
        """print list of Classes to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefclasses(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def BrowseClasses(self, classname):
        """print list of Classes that inherit from specified one"""
        _c.routerClear("temporary")
        defclass = _c.env_findDefclass(self.__env, str(classname))
        _c.env_browseClasses(self.__env, "temporary", defclass)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_accepts_method((str, unicode), None, None, (str, unicode), None, None)
    @_forces_method(str, str, None, str, None, None)
    def BuildMessageHandler(self, name, hclass, args, text, htype=PRIMARY, comment=None):
        """build a MessageHandler for specified class with arguments and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else: cmtstr = ""
        htype = htype.lower()
        if not htype in (AROUND, BEFORE, PRIMARY, AFTER):
            raise ValueError(
                "htype must be in AROUND, BEFORE, PRIMARY, AFTER")
        if type(args) in (tuple, list):
            sargs = " ".join(args)
        elif args is None:
            sargs = ""
        else:
            sargs = str(args)
        construct = "(defmessage-handler %s %s %s %s (%s) %s)" % (
            hclass, name, htype, cmtstr, sargs, text)
        _c.env_build(self.__env, construct)
        defclass = _c.env_findDefclass(self.__env, hclass)
        return _c.env_findDefmessageHandler(
            self.__env, defclass, name, htype)

    def MessageHandlerList(self):
        """return list of MessageHandler constructs"""
        o = _c.env_getDefmessageHandlerList(self.__env)
        li, rv = Multifield(self._cl2py(o)), []
        l = len(li) / 3
        for x in range(0, l):
            rv.append(Multifield([li[x * 3], li[x * 3 + 1], li[x * 3 + 2]]))
        return Multifield(rv)

    def PrintMessageHandlers(self):
        """print list of all MessageHandlers"""
        _c.routerClear("temporary")
        _c.env_listDefmessageHandlers(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Instances

    def InitialInstance(self):
        """retrieve first Instance"""
        try:
            return self.Instance(_c.env_getNextInstance(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Instance")

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def BLoadInstances(self, filename):
        """load Instances from binary file"""
        _c.env_binaryLoadInstances(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def BSaveInstances(self, filename, mode=LOCAL_SAVE):
        """save Instances to binary file"""
        _c.env_binarySaveInstances(
            self.__env, _os.path.normpath(filename), mode)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def LoadInstances(self, filename):
        """load Instances from file"""
        _c.env_loadInstances(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def SaveInstances(self, filename, mode=LOCAL_SAVE):
        """save Instances to file"""
        _c.env_saveInstances(self.__env, _os.path.normpath(filename), mode)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def LoadInstancesFromString(self, s):
        """load Instances from the specified string"""
        _c.env_loadInstancesFromString(self.__env, s)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def RestoreInstancesFromString(self, s):
        """restore Instances from the specified string"""
        _c.env_restoreInstancesFromString(self.__env, s)

    def InstancesChanged(self):
        """test if Instances have changed since last call"""
        rv = bool(_c.env_getInstancesChanged(self.__env))
        _c.env_setInstancesChanged(self.__env, False)
        return rv

    @_accepts_method((str, unicode), None, (str, unicode))
    @_forces_method(str, str, str)
    def BuildInstance(self, name, defclass, overrides=""):
        """build an Instance of given class overriding specified slots"""
        cmdstr = "(%s of %s %s)" % (name, str(defclass), overrides)
        return self.Instance(_c.env_makeInstance(self.__env, cmdstr))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindInstance(self, name):
        """find an Instance in all modules (including imported)"""
        return self.Instance(_c.env_findInstance(self.__env, name, True))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindInstanceLocal(self, name):
        """find an Instance in non imported modules"""
        return self.Instance(_c.env_findInstance(self.__env, name, False))

    @_forces_method(str)
    def PrintInstances(self, classname=None):
        """print Instances to standard output"""
        _c.routerClear("temporary")
        if classname:
            _c.env_instances(self.__env, "temporary", classname, False)
        else:
            _c.env_instances(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    @_forces_method(str)
    def PrintSubclassInstances(self, classname):
        """print subclass Instances to standard output"""
        _c.routerClear("temporary")
        if classname:
            _c.env_instances(self.__env, "temporary", classname, True)
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # functions involving Definstances

    def InitialDefinstances(self):
        """retrieve first Definstances"""
        try:
            return self.Definstances(_c.env_getNextDefinstances(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Definstances")

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def FindDefinstances(self, name):
        """find Definstances by name"""
        return self.Definstances(_c.env_findDefinstances(self.__env, name))

    @_accepts_method((str, unicode), (str, unicode), None)
    @_forces_method(str, str, None)
    def BuildDefinstances(self, name, text, comment=None):
        """build a Definstances with specified name and body"""
        if comment:
            cmtstr = '"%s"' % str(comment).replace('"', '\\"')
        else:
            cmtstr = ""
        construct = "(definstances %s %s %s)" % (name, cmtstr, text)
        _c.env_build(self.__env, construct)
        return self.Definstances(_c.env_findDefinstances(self.__env, name))

    def DefinstancesList(self):
        """retrieve list of all Definstances names"""
        o = _c.env_getDefinstancesList(self.__env)
        return Multifield(self._cl2py(o))

    def PrintDefinstances(self):
        """print list of all Definstances to standard output"""
        _c.routerClear("temporary")
        _c.env_listDefinstances(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)


    # =================================================================== #
    # Agenda functions

    def PrintAgenda(self):
        """print Agenda Rules to standard output"""
        _c.routerClear("temporary")
        _c.env_agenda(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def AgendaChanged(self):
        """test whether or not Agenda is changed since last call"""
        rv = bool(_c.env_getAgendaChanged(self.__env))
        _c.env_setAgendaChanged(self.__env, False)
        return rv

    def RefreshAgenda(self):
        """refresh Agenda Rules for current Module"""
        _c.env_refreshAgenda(self.__env)

    def ReorderAgenda(self):
        """reorder Agenda Rules for current Module"""
        _c.env_reorderAgenda(self.__env)

    def Run(self, limit=None):
        """execute Rules up to limit (if any)"""
        if limit is None:
            return _c.env_run(self.__env)
        else:
            return _c.env_run(self.__env, limit)

    def ClearFocusStack(self):
        """clear focus stack"""
        _c.env_clearFocusStack(self.__env)

    def FocusStack(self):
        """return list of Module names in focus stack"""
        return self._cl2py(_c.env_getFocusStack(self.__env))

    def PrintFocusStack(self):
        """print focus stack to standard output"""
        _c.routerClear("temporary")
        _c.env_listFocusStack(self.__env, "temporary")
        s = _c.routerRead("temporary")
        if s:
            _sys.stdout.write(s)

    def PopFocus(self):
        """pop focus"""
        _c.env_popFocus(self.__env)

    def InitialActivation(self):
        """return first Activation object"""
        try:
            return self.Activation(_c.env_getNextActivation(self.__env))
        except:
            raise _c.ClipsError("M02: could not find any Activation")

    def CurrentModule(self):
        """return current Module"""
        return self.Module(_c.env_getCurrentModule(self.__env))


    # =================================================================== #
    # basic environment functions

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def BLoad(self, filename):
        """binary load the constructs from a file"""
        _c.env_bload(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def BSave(self, filename):
        """binary save constructs to a file"""
        _c.env_bsave(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def Load(self, filename):
        """load constructs from a file"""
        _c.env_load(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def Save(self, filename):
        """save constructs to a file"""
        _c.env_save(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def BatchStar(self, filename):
        """execute commands stored in file"""
        _c.env_batchStar(self.__env, _os.path.normpath(filename))

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def Build(self, construct):
        """build construct given in argument"""
        _c.env_build(self.__env, construct)

    @_accepts_method((str, unicode))
    @_forces_method(str)
    def Eval(self, expr):
        """evaluate expression passed as argument"""
        return self._cl2py(_c.env_eval(self.__env, expr))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def Call(self, func, args=None):
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
                    if t1 in (ClipsIntegerType, ClipsFloatType,
                              ClipsStringType, ClipsSymbolType,
                              ClipsNilType, ClipsInstanceNameType,
                              ClipsMultifieldType):
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
            return self._cl2py(_c.env_functionCall(self.__env, func, sargs))
        else:
            return self._cl2py(_c.env_functionCall(self.__env, func))

    @_accepts_method((str, unicode), None)
    @_forces_method(str, None)
    def SendCommand(self, command, verbose=False):
        """send a command to the engine as if typed at the CLIPS prompt"""
        _c.env_sendCommand(self.__env, command, verbose)

    def Reset(self):
        """reset Environment"""
        _c.env_reset(self.__env)

    def Clear(self):
        """clear Environment"""
        _c.env_clear(self.__env)




# ========================================================================== #
# Functions and classes to access CLIPS input/output

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


# ========================================================================== #
# streams are singletons also in this implementation, hence the definition
#  at toplevel and the deletion of the class definitions after enumerating
#  all supported streams

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
# the following stuff remains at the top level, as both memory management
#  functions and external Python functions are defined for the whole CLIPS
#  engine at once; also, we mantain toplevel versions of both the (_py2cl,
#  _cl2py) pair of helpers, and the test and coercion decorators in order
#  to be able to use them here and keep these functions symmetrical to the
#  Environment related ones

# ========================================================================== #
# memory management - for CLIPS gurus ;-)
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
    Used = property(__property_getUsed,
                    None, None,
                    "amount in bytes of memory used by CLIPS")

    def __property_getRequests(self):
        return _c.memRequests()
    Requests = property(__property_getRequests,
                        None, None,
                        "number of requests for memory made by CLIPS")

    def __property_setConserve(self, v):
        _c.setConserveMemory(v)
    def __property_getConserve(self):
        return bool(_c.getConserveMemory())
    Conserve = property(__property_getConserve,
                        __property_setConserve,
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
    EnvironmentErrorsEnabled = property(__property_getEnvironmentErrorsEnabled,
                                        __property_setEnvironmentErrorsEnabled,
                                        None,
                                        "whether or not fatal environment errors are printed")

    def __property_getNumberOfEnvironments(self):
        return _c.getNumberOfEnvironments()
    NumberOfEnvironments = property(__property_getNumberOfEnvironments,
                                    None, None,
                                    "current number of Environments")

    def Free(self):
        """free up unneeded memory"""
        _c.releaseMem(False)


# define one only actual object of this type to access memory functions
Memory = _clips_Memory()
del _clips_Memory



# ========================================================================== #
# external functions - "all sorts of new and shiny evil"
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
