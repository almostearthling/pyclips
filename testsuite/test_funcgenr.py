# test_funcgenr.py


"""revision: $Id: test_funcgenr.py 215 2004-11-26 16:10:56Z Franz $
TESTS:
BuildFunction
RegisterPythonFunction
UnregisterPythonFunction
BuildGeneric
FindFunction
FindGeneric

Function:
  Name
  Module
  Deletable
  Watch

Generic:
  AddMethod
  RemoveMethod
  MethodList
  Module
  Watch
  MethodWatched
  WatchMethod
  UnwatchMethod
  MethodDeletable

"""



class ctc_Function(ctestcase):
    """test Function objects"""

    def ctf_Function_01(self):
        """Testing: BuildFunction, Function.Name, Function.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f1 = e.BuildFunction("tf", "?a ?b", "(str-cat ?a ?b)")
            self.assertEqual(f1.Module, "MAIN")
            self.assertEqual(e.Eval('(%s "s1" "s2")' % f1.Name), "s1s2")

    def ctf_Function_02(self):
        """Testing: RegisterPythonFunction, UnregisterPythonFunction"""
        def tfunc(a, b):
            return a + b
        clips.RegisterPythonFunction(tfunc)
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            self.assertEqual(e.Eval("(python-call tfunc 13 29)"), 42)
            self.assertEqual(e.Eval('(python-call tfunc "4" "2")'), "42")
        clips.UnregisterPythonFunction(tfunc)

    def ctf_Function_03(self):
        """Testing: FindFunction, Function.Deletable, Function.Watch, {...}"""
        """         Function.PPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f1 = e.BuildFunction("tf", "?a ?b", "(str-cat ?a ?b)")
            f2 = e.BuildFunction("tfd", "?a ?b", "(tf ?a ?b)")
            self.assert_(f2.Deletable)
            self.assert_(not f1.Deletable)
            self.assert_(f1.Watch)
            self.assert_(f2.Watch)
            self.assertEqual(e.FindFunction("tfd").PPForm(), f2.PPForm())


class ctc_Generic(ctestcase):
    """test Generic objects"""

    def ctf_Generic_01(self):
        """Testing: BuildGeneric, Generic.AddMethod, Generic.Name, Generic.Watch"""
        def mulstr(s, n):
            return clips.String(s * n)
        clips.RegisterPythonFunction(mulstr)
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            self.assertEqual(g1.Name, "g1")
            self.assert_(g1.Watch)
            g1.AddMethod(
                [('?p1', clips.ClipsStringType), ('?p2', clips.ClipsStringType)],
                '(str-cat ?p1 ?p2)')
            g1.AddMethod(
                ['?p1 STRING', '?p2 INTEGER'], "(python-call mulstr ?p1 ?p2)")
            g1.AddMethod("(?p1 NUMBER)(?p2 NUMBER)", "(+ ?p1 ?p2)")
            self.assertEqual(e.Eval("(g1 13 29)"), clips.Integer(42))
            self.assertEqual(e.Eval("(g1 13.0 29.0)"), clips.Float(42.0))
            self.assertEqual(e.Eval('(g1 "sp" "am")'), "spam")
            self.assertEqual(len(e.Eval('(g1 "spam" 42)')) / len("spam"), 42)
            g1.RemoveMethod(0)
            self.assertEqual(len(g1.MethodList()), 0)

    def ctf_Generic_02(self):
        """Testing: Generic.MethodList, Generic.RemoveMethod, Generic.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            self.assertEqual(g1.Module, "MAIN")
            g1.AddMethod('(?p1 STRING)(?p2 STRING)', "(str-cat ?p1 ?p2)")
            g1.AddMethod("(?p1 NUMBER)(?p2 NUMBER)", "(+ ?p1 ?p2)")
            self.assertEqual(len(g1.MethodList()), 2)
            for i in g1.MethodList():
                g1.RemoveMethod(int(i))
            self.assertEqual(len(g1.MethodList()), 0)

    def ctf_Generic_03(self):
        """Testing: FindGeneric, Generic.Deletable, Generic.PPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            g1.AddMethod("(?p1 NUMBER)(?p2 NUMBER)", "(+ ?p1 ?p2)")
            self.assert_(g1.Deletable)
            f1 = e.BuildFunction("f1", "?a ?b", "(g1 ?a ?b)")
            self.assert_(not g1.Deletable)
            self.assertEqual(e.FindGeneric("g1").PPForm(), g1.PPForm())

    def ctf_Generic_04(self):
        """Testing: Generic.MethodWatched, Generic.WatchMethod, {...}"""
        """         Generic.UnwatchMethod, Generic.MethodDeletable"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            g1.AddMethod("(?p1 NUMBER)(?p2 NUMBER)", "(+ ?p1 ?p2)")
            self.assert_(g1.MethodDeletable(1))
            g1.UnwatchMethod(1)
            self.assert_(not g1.MethodWatched(1))
            g1.WatchMethod(1)
            self.assert_(g1.MethodWatched(1))

    def ctf_Generic_05(self):
        """Testing: Generic.MethodRestrictions, Generic.MethodDescription"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            g1.AddMethod("(?p1 NUMBER)(?p2 NUMBER)", "(+ ?p1 ?p2)")
            self.assertEqual(
                e.Eval("(get-method-restrictions g1 1)"),
                g1.MethodRestrictions(1))
            mdesc = g1.MethodDescription(1).split(None, 1)[1]
            self.assertEqual(mdesc, "(NUMBER) (NUMBER)")



# end.
