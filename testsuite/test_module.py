# test_module.py

"""revision $Id: test_module.py 297 2006-06-15 00:46:44Z Franz $
TESTS:
BuildModule
FocusStack
PopFocus
ClearFocusStack

Module
  SetCurrent
  BuildTemplate
  BuildRule
  FactList
  BuildFunction
  BuildGeneric
  FunctionList
  GenericList
  TemplateList
  BuildClass
  ClassList
  BuildInstance
  RuleList
  BuildDeffacts
  DeffactsList
  BuildDefinstances
  DefinstancesList
  RefreshAgenda
  ReorderAgenda

"""


class ctc_Module(ctestcase):
    """test Module objects"""

    def ctf_Module_01(self):
        """Testing: BuildModule, FocusStack, PopFocus, Module.SetCurrent"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST")
            mo.SetCurrent()
            self.assertEqual(e.FocusStack()[0], "MAIN")
            e.PopFocus()
            self.assertEqual(len(e.FocusStack()), 0)

    def ctf_Module_02(self):
        """Testing: ClearFocusStack"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST")
            self.assertEqual(e.CurrentModule().Name, "TEST")
            mo.SetCurrent()
            self.assertEqual(len(e.FocusStack()), 1)
            e.ClearFocusStack()
            self.assertEqual(len(e.FocusStack()), 0)

    def ctf_Module_03(self):
        """Testing: Module.BuildTemplate, Module.BuildRule, Module.FactList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            MAIN = e.BuildModule("MAIN", "(export ?ALL)")
            e.Reset()
            t = MAIN.BuildTemplate("t", "(slot s)")
            r = MAIN.BuildRule("r", "(start)", "(assert (t (s 42))) (focus TEST)")
            TEST = e.BuildModule("TEST", "(import MAIN deftemplate t)")
            tr = TEST.BuildRule("tr", "(t (s 42))", "(assert (success))")
            MAIN.SetCurrent()
            e.Assert("(start)")
            self.assertEqual(e.Run(), 2)
            f2 = e.FactList()[-1]
            self.assertEqual(f2.Relation, "success")
            f1 = TEST.FactList()[-1]
            self.assertEqual(f1.Relation, "success")

    def ctf_Module_04(self):
        """Testing: Module.BuildFunction, Module.FunctionList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            f = mo.BuildFunction("f", "?a", "(+ ?a ?a)")
            self.assertEqual(e.Eval("(f 21)"), 42)
            self.assertEqual(f.Module, "TEST")
            li = mo.FunctionList()
            self.assertEqual(li[-1], f.Name)

    def ctf_Module_05(self):
        """Testing: Module.BuildGeneric, Module.GenericList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            g = mo.BuildGeneric("g")
            self.assertEqual(g.Module, "TEST")
            li = mo.GenericList()
            self.assertEqual(li[-1], g.Name)

    def ctf_Module_06(self):
        """Testing: Module.BuildGlobal, Module.GlobalList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            g = mo.BuildGlobal("g", 42)
            self.assertEqual(g.Module, "TEST")
            self.assertEqual(g.Value, 42)
            li = mo.GlobalList()
            self.assertEqual(li[-1], g.Name)

    def ctf_Module_07(self):
        """Testing: Module.TemplateList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST")
            t = mo.BuildTemplate("t", "(slot s)")
            self.assertEqual(t.Module, "TEST")
            li = mo.TemplateList()
            self.assertEqual(li[-1], t.Name)

    def ctf_Module_08(self):
        """Testing: Module.BuildClass, Module.ClassList, Module.BuildInstance"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            C = mo.BuildClass("C", "(is-a USER)")
            self.assertEqual(C.Module, "TEST")
            li = mo.ClassList()
            self.assertEqual(li[-1], C.Name)
            i = mo.BuildInstance("i", C)
            self.assert_(i)

    def ctf_Module_09(self):
        """Testing: Module.RuleList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            r = mo.BuildRule("r", "(duck)", "(assert (quack))")
            li = mo.RuleList()
            self.assertEqual(li[-1], r.Name)

    def ctf_Module_10(self):
        """Testing: Module.BuildDeffacts, Module.DeffactsList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            df = mo.BuildDeffacts("df", "(duck)")
            li = mo.DeffactsList()
            self.assertEqual(li[-1], df.Name)

    def ctf_Module_11(self):
        """Testing: Module.BuildDefinstances, Module.DefinstancesList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            C = mo.BuildClass("C", "(is-a USER)")
            di = mo.BuildDefinstances("di", "([c] of C)")
            li = mo.DefinstancesList()
            self.assertEqual(li[-1], di.Name)

    def ctf_Module_12(self):
        """Testing: Module.RefreshAgenda, Module.ReorderAgenda"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            dr1 = mo.BuildRule("dr1", "(duck)", "(assert (quack1))")
            dr2 = mo.BuildRule("dr2", "(duck)", "(assert (quack1))(assert (quack2))")
            e.Assert("(duck)")
            e.EngineConfig.Strategy = clips.COMPLEXITY_STRATEGY
            e.RefreshAgenda()
            a1 = e.InitialActivation()
            e.EngineConfig.Strategy = clips.DEPTH_STRATEGY
            e.ReorderAgenda()
            a2 = e.InitialActivation()
            self.assert_(a1.Name != a2.Name)

    def ctf_Module_13(self):
        """Testing: Module.FocusStack"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEST", "(export ?ALL)")
            mo.SetFocus()
            self.assertEqual(e.FocusStack()[0], "TEST")

# end.
