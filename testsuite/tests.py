# tests.py
# perform some unit tests
# revision $Id: tests.py 188 2004-11-10 20:04:34Z Franz $

import unittest
import glob

execfile('test_00.py')
for x in glob.glob("test_[a-z]*.py"): execfile(x)
def is_test_class(x):
    try: return issubclass(eval(x), ctestcase)
    except: return False
def is_test_function(x):
    try: return x.startswith('ctf_')
    except: return False

suite = unittest.TestSuite()
for x in filter(is_test_class, dir()):
    for y in filter(is_test_function, dir(eval(x))):
        suite.addTest(eval("%s('%s')" % (x, y)))

unittest.TextTestRunner(verbosity=2).run(suite)


# end.
