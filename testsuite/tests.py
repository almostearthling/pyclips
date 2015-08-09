# tests.py
# perform some unit tests
# revision $Id: tests.py 372 2008-12-24 01:03:45Z Franz $

import unittest
import glob
import sys
PYTHON_VERSION = sys.version[:3]


if PYTHON_VERSION >= "2.6":
    def exec_file(x):
        f = open(x)
        exec(f, globals(), globals())   # we *want* to modify global space
        f.close()
else:
    def exec_file(x):
        execfile(x, globals(), globals())
        

exec_file('test_00.py')
for x in glob.glob("test_[a-z]*.py"):
    exec_file(x)
def is_test_class(x):
    try:
        return issubclass(eval(x), ctestcase)
    except:
        return False
def is_test_function(x):
    try:
        return x.startswith('ctf_')
    except:
        return False

suite = unittest.TestSuite()
for x in filter(is_test_class, dir()):
    for y in filter(is_test_function, dir(eval(x))):
        suite.addTest(eval("%s('%s')" % (x, y)))

unittest.TextTestRunner(verbosity=2).run(suite)


# end.
