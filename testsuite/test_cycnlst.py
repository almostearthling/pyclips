# test_cycnlst.py

import sys, re

"""revision $Id: test_cycnlst.py 277 2006-05-27 17:04:33Z Franz $
TESTS:
FactList
InitialFact
InitialClass
FindClass
ClassList
InstancesChanged
InitialInstance
InitialRule
InitialDeffacts
FindDeffacts
RuleList
FindRule
InitialTemplate
TemplateList
FindTemplate
BuildGeneric
BuildDeffacts
MethodList
InitialDefinstances
DefinstancesList
BuildDefinstances
BuildGeneric
InitialGeneric
GenericList
FindGeneric
InitialGlobal
GlobalList
FindGlobal
BuildGlobal
GlobalsChanged
InitialFunction
FunctionList
FindFunction

Function
  Next
  Name
  PPForm
  Watch
  Deletable
  Module

Global
  Watch
  Deletable
  Next
  Name
  PPForm
  Value
  ValueForm
  Module

Generic
  Name
  PPForm
  Watch
  Deletable
  Module
  MethodList
  AddMethod
  InitialMethod
  NextMethod
  MethodPPForm

Definstances
  Name
  Next
  PPForm
  Deletable
  Module

Deffacts:
  Name
  Next
  PPForm
  Deletable
  Module

Template:
  Name
  Next
  Module
  Deletable
  Watch

Rule:
  Next
  Name
  PPForm
  Name
  Deletable
  Remove
  Module

Instance:
  Next
  FindInstance
  Class
  Slots
  Send
  IsValid
  Remove
  DirectRemove

Class:
  Next
  InitialInstance
  InitialSubclassInstance
  NextInstance
  NextSubclassInstance

Fact:
  Next
  Index
"""


class ctc_CyclesAndLists(ctestcase):
    """test Initial, Next and List functions"""

    def ctf_CyclesFact_01(self):
        """Testing: FactList, InitialFact, Fact.Next, Fact.Index"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Assert("(tf1)")
            e.Assert("(tf2)")
            e.Assert("(tf3)")
            e.Assert("(tf4)")
            li = e.FactList()
            f0 = e.InitialFact()
            for f in li:
                self.assertEqual(f.PPForm(), f0.PPForm())
                f0 = f0.Next()
            self.assertEqual(li[-1].Index, 4)
            self.assert_(f0 is None)

    def ctf_CyclesClass_01(self):
        """Testing: ClassList, InitialClass, FindClass, Class.Next"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            D = e.BuildClass("D", "(is-a C)")
            E = e.BuildClass("E", "(is-a D)")
            li = e.ClassList()
            cl0 = e.InitialClass()
            for cln in li:
                if cln in ['C', 'D', 'E']:
                    cl = e.FindClass(cln)
                    self.assertEqual(cl.PPForm(), cl0.PPForm())
                cl0 = cl0.Next()
            self.assert_(cl0 is None)

    def ctf_CyclesInstance_01(self):
        """Testing: InstancesChanged, InitialInstance, FindInstance, Instance.Next"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            e.InstancesChanged()
            io = e.FindInstance('initial-object')
            c1 = e.BuildInstance("c1", "C")
            c2 = e.BuildInstance("c2", "C")
            c3 = e.BuildInstance("c3", "C")
            self.assert_(e.InstancesChanged())
            li = [io, c1, c2, c3]
            i0 = e.InitialInstance()
            for i in li:
                self.assertEqual(i.PPForm(), i0.PPForm())
                i0 = i0.Next()
            self.assert_(i0 is None)

    def ctf_CyclesInstance_02(self):
        """Testing: Instance.IsValid, Instance.Remove, Instance.DirectRemove"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            e.InstancesChanged()
            c1 = e.BuildInstance("c1", "C")
            c2 = e.BuildInstance("c2", "C")
            self.assert_(c2.IsValid())
            c2.Remove()
            self.assert_(not c2.IsValid())
            self.assert_(c1.IsValid())
            c1.DirectRemove()
            self.assert_(not c1.IsValid())

    def ctf_CyclesInstance_03(self):
        """Testing: Instance.Class, Instance.Slots, Instance.Send"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C",
                             """(is-a USER)
                                (slot s1 (default 0)
                                         (create-accessor read-write))""")
            e.InstancesChanged()
            c1 = e.BuildInstance("c1", "C")
            self.assertEqual(c1.Class.Name, C.Name)
            self.assertEqual(c1.Send('get-s1'), 0)
            c1.Send('put-s1', '42')
            self.assertEqual(c1.Slots['s1'], 42)

    def ctf_CyclesInstance_04(self):
        """Testing: Class.InitialInstance, Class.InitialSubclassInstance, {...}"""
        """         Class.NextInstance, Class.NextSubclassInstance"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            D = e.BuildClass("D", "(is-a C)")
            d1 = e.BuildInstance("d1", "D")
            d2 = e.BuildInstance("d2", "D")
            c1 = e.BuildInstance("c1", "C")
            c2 = e.BuildInstance("c2", "C")
            i00 = C.InitialInstance()
            i01 = C.NextInstance(i00)
            i10 = C.InitialSubclassInstance()
            i11 = C.NextSubclassInstance(i10)
            i12 = C.NextSubclassInstance(i11)
            i13 = C.NextSubclassInstance(i12)
            i20 = D.InitialInstance()
            i21 = D.NextInstance(i20)
            i30 = D.InitialSubclassInstance()
            i31 = D.NextSubclassInstance(i30)
            self.assert_(i01 is not None)
            self.assert_(i13 is not None)
            self.assert_(i21 is not None)
            self.assert_(i31 is not None)
            self.assertEqual(i01.PPForm(), c2.PPForm())
            self.assertEqual(i31.PPForm(), d2.PPForm())
            self.assertEqual(i20.PPForm(), i30.PPForm())
            self.assertEqual(i21.PPForm(), i31.PPForm())
            lppf = [d1.PPForm(), d2.PPForm(), c1.PPForm(), c2.PPForm()]
            for i in [i10, i11, i12, i13]:
                ippf = i.PPForm()
                if ippf in lppf:
                    lppf.remove(ippf)
            self.assert_(not lppf)
            self.assert_(C.NextInstance(i01) is None)
            self.assert_(D.NextInstance(i21) is None)
            self.assert_(C.NextSubclassInstance(i13) is None)
            self.assert_(D.NextSubclassInstance(i31) is None)

    def ctf_CyclesRule_01(self):
        """Testing: InitialRule, RuleList, FindRule, Rule.Next, Rule.PPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.BuildRule("r1", "(f1)", "")
            e.BuildRule("r2", "(f2)", "")
            li = e.RuleList()
            r0 = e.InitialRule()
            for r in li:
                self.assertEqual(r0.PPForm(), e.FindRule(r).PPForm())
                r0 = r0.Next()
            self.assert_(r0 is None)

    def ctf_CyclesRule_02(self):
        """Testing: Rule.Name, Rule.Deletable, Rule.Remove, Rule.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            r1 = e.BuildRule("r1", "(f1)", "")
            r2 = e.BuildRule("r2", "(f2)", "")
            self.assert_(r1.Deletable)
            self.assert_(r2.Deletable)
            li = e.RuleList()
            r0 = e.InitialRule()
            for r in li:
                self.assertEqual(r0.Name, e.FindRule(r).Name)
                self.assertEqual(r0.Module, "MAIN")
                r0 = r0.Next()
            self.assert_(r0 is None)
            r1.Remove()
            r2.Remove()

    def ctf_CyclesRule_03(self):
        """Testing: Rule.WatchActivations, Rule.WatchFirings"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            r1 = e.BuildRule("r1", "(f1)", "")
            r2 = e.BuildRule("r2", "(f2)", "")
            li = e.RuleList()
            r0 = e.InitialRule()
            for r in li:
                self.assert_(r0.WatchActivations)
                self.assert_(r0.WatchFirings)
                r0 = r0.Next()
            self.assert_(r0 is None)

    def ctf_CyclesTemplate_01(self):
        """Testing: InitialTemplate, TemplateList, FindTemplate, Template.Next"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate("tf1", "(slot s1)")
            t2 = e.BuildTemplate("tf2", "(slot s2)")
            li = e.TemplateList()
            t0 = e.InitialTemplate()
            for t in li:
                self.assertEqual(t0.Name, e.FindTemplate(t).Name)
                t0 = t0.Next()

    def ctf_CyclesTemplate_02(self):
        """Testing: Template.Deletable, Template.Module, Template.Watch"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate("tf1", "(slot s1)")
            t2 = e.BuildTemplate("tf2", "(slot s2)")
            self.assert_(t1.Deletable)
            self.assert_(t2.Deletable)
            li = e.TemplateList()
            t0 = e.InitialTemplate()
            for t in li:
                self.assertEqual(t0.Name, e.FindTemplate(t).Name)
                self.assertEqual(t0.Module, "MAIN")
                self.assert_(t0.Watch)
                t0 = t0.Next()

    def ctf_CyclesMessageHandler_01(self):
        """Testing: MessageHandlerList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            ws = """init
                    delete
                    create
                    print
                    direct-modify
                    message-modify
                    direct-duplicate
                    message-duplicate""".split()
            li = e.MessageHandlerList()
            for s in li:
                w = str(s[1])
                if w in ws:
                    ws.remove(w)
            self.assert_(not ws)

    def ctf_CyclesMethod_01(self):
        """Testing: BuildGeneric, MethodList, Generic.MethodList, Generic.AddMethod"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGeneric("g")
            g.AddMethod("(?a INTEGER)", "(+ ?a ?a)")
            g.AddMethod("(?a STRING)", "(str-cat ?a ?a)")
            li = e.MethodList()
            lig = g.MethodList()
            self.assertEqual(len(li), 2)
            self.assertEqual(len(lig), 2)
            for i in range(len(li)):
                self.assertEqual(li[i][0], clips.Symbol("g"))
                self.assertEqual(li[i][1], lig[i])
                if li[i][1] == 1: self.assertEqual(e.Call("g", 21), 42)
                elif li[i][1] == 2:
                    self.assertEqual(
                        e.Call("g", clips.String("spam")), "spam" * 2)
                else: self.assert_(False)

    def ctf_CyclesMethod_02(self):
        """Testing: Generic.InitialMethod, Generic.NextMethod, Generic.MethodPPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGeneric("g")
            g.AddMethod("(?a INTEGER)", "(+ ?a ?a)")
            g.AddMethod("(?a STRING)", "(str-cat ?a ?a)")
            lig = g.MethodList()
            m0 = g.InitialMethod()
            for m in lig:
                self.assertEqual(g.MethodPPForm(m), g.MethodPPForm(m0))
                m0 = g.NextMethod(m0)
            self.assertEqual(m0, 0)

    def ctf_CyclesDeffacts_01(self):
        """Testing: InitialDeffacts, DeffactsList, Deffacts.Next, Deffacts.Name"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            df1 = e.BuildDeffacts("df1", "(f11)(f12)")
            df2 = e.BuildDeffacts("df2", "(f21)(f22)")
            li = e.DeffactsList()
            df0 = e.InitialDeffacts()
            for d in li:
                self.assertEqual(d.replace("MAIN::", ""), df0.Name)
                df0 = df0.Next()
            self.assert_(df0 is None)

    def ctf_CyclesDeffacts_02(self):
        """Testing: FindDeffacts, Deffacts.PPForm, Deffacts.Deletable, {...}"""
        """         Deffacts.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            df1 = e.BuildDeffacts("df1", "(f11)(f12)")
            df2 = e.BuildDeffacts("df2", "(f21)(f22)")
            li = e.DeffactsList()
            df0 = e.InitialDeffacts()
            for d in li:
                if d.replace("MAIN::", "") != "initial-fact":
                    self.assertEqual(e.FindDeffacts(d).PPForm(), df0.PPForm())
                self.assert_(df0.Deletable)
                self.assertEqual(df0.Module, "MAIN")
                df0 = df0.Next()
            self.assert_(df0 is None)

    def ctf_CyclesDefinstances_01(self):
        """Testing: InitialDefinstances, DefinstancesList, Definstances.Next, {...}"""
        """         Definstances.PPForm, BuildDefinstances"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            di1 = e.BuildDefinstances("di1", "(di11 of C)(di12 of C)")
            di2 = e.BuildDefinstances("di2", "(di21 of C)(di22 of C)")
            li = e.DefinstancesList()
            di0 = e.InitialDefinstances()
            for d in li:
                if d.replace("MAIN::", "") != "initial-object":
                    self.assertEqual(e.FindDefinstances(d).PPForm(), di0.PPForm())
                self.assert_(di0.Deletable)
                di0 = di0.Next()
            self.assert_(di0 is None)

    def ctf_CyclesDefinstances_02(self):
        """Testing: Definstances.Name, Definstances.Module, Definstances.Deletable"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C1 = e.BuildClass("C1", "(is-a USER)")
            di1 = e.BuildDefinstances("di1", "(di11 of C1)(di12 of C1)")
            di2 = e.BuildDefinstances("di2", "(di21 of C1)(di22 of C1)")
            li = e.DefinstancesList()
            di0 = e.InitialDefinstances()
            for d in li:
                self.assertEqual(d.replace("MAIN::", ""), di0.Name)
                self.assertEqual(e.FindDefinstances(d).Module, di0.Module)
                self.assert_(di0.Deletable)
                di0 = di0.Next()
            self.assert_(di0 is None)

    def ctf_CyclesGeneric_01(self):
        """Testing: BuildGeneric, Generic.Name, Generic.PPForm, Generic.Watch"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            g2 = e.BuildGeneric("g2")
            li = e.GenericList()
            g0 = e.InitialGeneric()
            for g in li:
                self.assertEqual(g.replace("MAIN::", ""), g0.Name)
                self.assertEqual(e.FindGeneric(g).PPForm(), g0.PPForm())
                self.assert_(g0.Watch)
                g0 = g0.Next()
            self.assert_(g0 is None)

    def ctf_CyclesGeneric_02(self):
        """Testing: InitialGeneric, GenericList, FindGeneric, {...}"""
        """         Generic.Deletable, Generic.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGeneric("g1")
            g2 = e.BuildGeneric("g2")
            li = e.GenericList()
            g0 = e.InitialGeneric()
            for g in li:
                self.assert_(g0.Deletable)
                self.assertEqual(e.FindGeneric(g).Module, g0.Module)
                g0 = g0.Next()
            self.assert_(g0 is None)

    def ctf_CyclesGlobal_01(self):
        """Testing: BuildGlobal, Global.Name, Global.PPForm, {...}"""
        """         GlobalsChanged, Global.ValueForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.GlobalsChanged()
            self.assert_(not e.GlobalsChanged())
            g1 = e.BuildGlobal("g1", clips.Integer(42))
            g2 = e.BuildGlobal("g2", clips.Symbol("FORTY-TWO"))
            self.assert_(e.GlobalsChanged())
            li = e.GlobalList()
            g0 = e.InitialGlobal()
            for g in li:
                self.assertEqual(g.replace("MAIN::", ""), g0.Name)
                self.assertEqual(e.FindGlobal(g).PPForm(), g0.PPForm())
                self.assertEqual(e.FindGlobal(g).ValueForm(), g0.ValueForm())
                g0 = g0.Next()
            self.assert_(g0 is None)

    def ctf_CyclesGlobal_02(self):
        """Testing: InitialGlobal, GlobalList, FindGlobal, Global.Watch, {...}"""
        """         Global.Deletable, Global.Next, Global.Module, Global.Value"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g1 = e.BuildGlobal("g1", clips.Integer(42))
            g2 = e.BuildGlobal("g2", clips.Symbol("FORTY-TWO"))
            li = e.GlobalList()
            g0 = e.InitialGlobal()
            for g in li:
                self.assertEqual(e.FindGlobal(g).Value, g0.Value)
                self.assertEqual(e.FindGlobal(g).Module, g0.Module)
                self.assert_(g0.Watch)
                self.assert_(g0.Deletable)
                g0 = g0.Next()
            self.assert_(g0 is None)

    def ctf_CyclesFunction_01(self):
        """Testing: InitialFunction, FunctionList, Function.Name, Function.Next"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f1 = e.BuildFunction("f1", ("?a", "?b"), "(+ ?a ?b)")
            f2 = e.BuildFunction("f2", ("?a", "?b"), "(+ ?a ?b)")
            li = e.FunctionList()
            f0 = e.InitialFunction()
            for f in li:
                self.assertEqual(e.Eval("(%s 21 21)" % f.replace("MAIN::", "")), 42)
                self.assertEqual(e.Eval("(%s 21 21)" % f0.Name), 42)
                f0 = f0.Next()
            self.assert_(f0 is None)

    def ctf_CyclesFunction_02(self):
        """Testing: FindFunction, Function.PPForm, Function.Watch, {...}"""
        """         Function.Deletable, Function.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f1 = e.BuildFunction("f1", ("?a", "?b"), "(+ ?a ?b)")
            f2 = e.BuildFunction("f2", ("?a", "?b"), "(+ ?a ?b)")
            li = e.FunctionList()
            f0 = e.InitialFunction()
            for f in li:
                self.assertEqual(e.FindFunction(f).Name, f0.Name)
                self.assertEqual(e.FindFunction(f).Module, f0.Module)
                self.assertEqual(e.FindFunction(f).PPForm(), f0.PPForm())
                self.assert_(f0.Watch)
                self.assert_(f0.Deletable)
                f0 = f0.Next()
            self.assert_(f0 is None)

    def ctf_CyclesModule_01(self):
        """Testing: InitialModule, BuildModule, FindModule, Module.Name"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            m = e.InitialModule()
            self.assertEqual(m.Name, "MAIN")
            e.BuildModule("NEW_MODULE")
            m1 = e.FindModule("NEW_MODULE")
            self.assertEqual(m1.Name, "NEW_MODULE")

    def ctf_CyclesModule_02(self):
        """Testing: ModuleList, Module.PPForm, Module.Next"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            M1 = e.BuildModule("M1")
            M2 = e.BuildModule("M2")
            li = e.ModuleList()
            M0 = e.InitialModule()
            for m in li:
                self.assertEqual(m, M0.Name)
                if m != "MAIN":
                    self.assertEqual(e.FindModule(m).PPForm(), M0.PPForm())
                M0 = M0.Next()
            self.assert_(M0 is None)



# end.
