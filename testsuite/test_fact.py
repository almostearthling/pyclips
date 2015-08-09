# test_fact.py

"""revision $Id: test_fact.py 321 2006-10-10 16:22:00Z Franz $
TESTS:

Assert
BuildTemplate
BuildRule
FactList
StdoutStream
FactListChanged
InitialFact
LoadFactsFromString
RefreshAgenda
ReorderAgenda

Rule
  Refresh

Fact:
  Fact
  Assert
  Index
  PPForm
  CleanPPForm
  Retract
  Exists
  Slots
  AssignSlotDefaults
  Next
  ImpliedSlots

Template:
  Name
  Deletable
  Module
  InitialFact
  NextFact

Template.Slots:
  AllowedValues
  Cardinality
  HasDefault
  DefaultValue
  Exists
  Range
  Names
  Types
  IsSinglefield
  IsMultifield

"""


class ctc_Fact(ctestcase):
    """test Fact objects"""

    def ctf_Fact_01(self):
        """Testing: Assert, FactList, Fact.PPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Assert("(duck)")
            li = e.FactList()
            for f in li:
                if str(f) == 'f-1':
                    self.assertEqual(f.PPForm().split(None, 1)[1], "(duck)")

    def ctf_Fact_02(self):
        """Testing: Fact.Index, Fact.Exists, Fact.Retract"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Assert("(duck)")
            li = e.FactList()
            for f in li:
                if f.Index == 1:
                    f1 = f
            self.assert_(f1.Exists)
            f1.Retract()
            self.assert_(not f1.Exists)
            li = e.FactList()
            self.assertEqual(len(li), 1)

    def ctf_Fact_03(self):
        """Testing: FactListChanged"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f = e.Assert("(duck)")
            self.assert_(e.FactListChanged())
            f.Retract()
            self.assert_(e.FactListChanged())

    def ctf_Fact_04(self):
        """Testing: InitialFact, Fact.Next, Fact.CleanPPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f = e.Assert("(duck)")
            f1 = e.InitialFact()
            f2 = f1.Next()
            self.assertEqual(f2.CleanPPForm(), "(duck)")

    def ctf_Fact_05(self):
        """Testing: LoadFactsFromString"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f = e.LoadFactsFromString("(duck)(quack)")
            f1 = e.InitialFact()
            f2 = f1.Next()
            f3 = f2.Next()
            self.assertEqual(f3.CleanPPForm(), "(quack)")

    def ctf_Fact_06(self):
        """Testing: Fact.ImpliedSlots"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            f = e.Assert("(duck 42)")
            self.assertEqual(f.ImpliedSlots[0], 42)

    def ctf_Fact_07(self):
        """Testing: Rule.Refresh"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            dr = e.BuildRule("dr", "(duck)", """
                (bind ?fn (sym-cat q- (gensym*)))
                (assert (quack ?fn))""")
            e.Assert("(duck)")
            self.assert_(e.Run())
            self.assert_(not e.Run())
            dr.Refresh()
            self.assert_(e.Run())
            self.assert_(not e.Run())

    def ctf_Fact_08(self):
        """Testing: RefreshAgenda, ReorderAgenda"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            dr1 = e.BuildRule("dr1", "(duck)", "(assert (quack1))")
            dr2 = e.BuildRule("dr2", "(duck)", "(assert (quack1))(assert (quack2))")
            e.Assert("(duck)")
            e.EngineConfig.Strategy = clips.COMPLEXITY_STRATEGY
            e.RefreshAgenda()
            a1 = e.InitialActivation()
            e.EngineConfig.Strategy = clips.DEPTH_STRATEGY
            e.ReorderAgenda()
            a2 = e.InitialActivation()
            self.assert_(a1.Name != a2.Name)

    def ctf_Template_01(self):
        """Testing: BuildTemplate, BuildRule, Run, Fact, Fact.Slots, Fact.Assert"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", "(slot s1) (slot s2)", "test template")
            r1 = e.BuildRule("test-rule", """
                ?f <- (t1 (s1 ?a) (s2 ?b))
                (test (eq ?a ?b))
            """, """
                (retract ?f)
                (assert (it-works))
            """, "arises on two equal slots")
            f1 = e.Fact(t1)
            f1.Slots['s1'] = clips.String("test1")
            f1.Slots['s2'] = clips.String("test1")
            f1.Assert()
            e.Run()
            w0 = e.FactList()
            fs = w0[-1].CleanPPForm()
            self.assertEqual(fs, "(it-works)")

    def ctf_Template_02(self):
        """Testing: Fact.AssignSlotDefaults, Template.Module"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1 (default 1))
                (slot s2 (default 1))
                """, "test template")
            self.assertEqual(t1.Module, "MAIN")
            r1 = e.BuildRule("test-rule", """
                ?f <- (t1 (s1 ?a) (s2 ?b))
                (test (eq ?a ?b))
            """, """
                (retract ?f)
                (assert (it-works))
            """, "arises on two equal slots")
            f1 = e.Fact(t1)
            f1.AssignSlotDefaults()
            self.assertEqual(int(f1.Slots['s1']), int(f1.Slots['s2']))
            self.assertEqual(int(f1.Slots['s1']), 1)
            f1.Assert()
            e.Run()
            w0 = e.FactList()
            fs = w0[-1].CleanPPForm()
            self.assertEqual(fs, "(it-works)")

    def ctf_Template_03(self):
        """Testing: Template.Name, Template.Deletable, StdoutStream.Read"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1 (default 1))
                (slot s2 (default 1))
                """, "test template")
            self.assert_(t1.Deletable)
            r1 = e.BuildRule("test-rule", """
                ?f <- (t1 (s1 ?a) (s2 ?b))
                (test (eq ?a ?b))
            """, """
                (retract ?f)
                (printout t (+ ?a ?b))
            """, "arises on two equal slots")
            self.assert_(not t1.Deletable)
            self.assertEqual(t1.Name, "t1")
            f1 = e.Fact(t1)
            f1.AssignSlotDefaults()
            f1.Assert()
            clips.StdoutStream.Read()
            e.Run()
            i = int(clips.StdoutStream.Read())
            self.assertEqual(i, 2)

    def ctf_Template_04(self):
        """Testing: Template.InitialFact, Template.NextFact"""
        if clips.CLIPS_VERSION < "6.23":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1 (default 1))
                (slot s2 (default 1))
                """, "test template")
            f1 = e.Fact(t1)
            f1.AssignSlotDefaults()
            f1.Assert()
            f2 = e.Fact(t1)
            f2.Slots['s1'] = 2
            f2.Slots['s2'] = 2
            f2.Assert()
            ff1 = t1.InitialFact()
            self.assertEqual(ff1.Slots['s1'], 1)
            self.assertEqual(ff1.Slots['s2'], 1)
            ff2 = t1.NextFact(ff1)
            self.assertEqual(ff2.Slots['s1'], 2)
            self.assertEqual(ff2.Slots['s2'], 2)

    def ctf_Template_05(self):
        """Testing: Template.Slots.AllowedValues, Template.Slots.Cardinality"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1)
                (slot s2 (allowed-integers 42 13)
                         (allowed-symbols foo)
                         )
                (multislot s3 (cardinality ?VARIABLE 42))
                """, "test template")
            c = t1.Slots.Cardinality("s3")
            self.assert_(not t1.Slots.AllowedValues("s1"))
            self.assertEqual(len(t1.Slots.AllowedValues("s2")), 3)
            self.assertEqual(c[0], 0)
            self.assertEqual(c[1], 42)

    def ctf_Template_06(self):
        """Testing: Template.Slots.HasDefault, Template.Slots.DefaultValue"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s0)
                (slot s1 (default ?NONE))
                (slot s2 (default 42))
                (slot s3 (default-dynamic (gensym)))
                """, "test template")
            self.assertEqual(t1.Slots.HasDefault("s0"), clips.STATIC_DEFAULT)
            self.assertEqual(t1.Slots.HasDefault("s1"), clips.NO_DEFAULT)
            self.assertEqual(t1.Slots.HasDefault("s2"), clips.STATIC_DEFAULT)
            self.assertEqual(t1.Slots.DefaultValue("s2"), 42)
            self.assertEqual(t1.Slots.HasDefault("s3"), clips.DYNAMIC_DEFAULT)

    def ctf_Template_07(self):
        """Testing: Template.Slots.Exists, Template.Slots.Range"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s0)
                (slot s1 (type SYMBOL))
                (slot s2 (range 13 42))
                """, "test template")
            self.assert_(t1.Slots.Exists("s0"))
            self.assert_(t1.Slots.Exists("s1"))
            self.assert_(t1.Slots.Exists("s2"))
            self.assert_(not t1.Slots.Exists("s3"))
            rs0 = t1.Slots.Range("s0")
            self.assertEqual(rs0[0], '-oo')
            self.assertEqual(rs0[1], '+oo')
            self.assert_(not t1.Slots.Range("s1"))
            rs2 = t1.Slots.Range("s2")
            self.assertEqual(rs2[0], 13)
            self.assertEqual(rs2[1], 42)

    def ctf_Template_08(self):
        """Testing: Template.Slots.Names, Template.Slots.Types"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s0)
                (slot s1 (type LEXEME))
                (slot s2 (type INTEGER))
                """, "test template")
            n = map(str, t1.Slots.Names())
            self.assert_('s0' in n)
            self.assert_('s1' in n)
            self.assert_('s2' in n)
            self.assert_('s3' not in n)
            t = map(str, t1.Slots.Types("s1"))
            self.assert_('STRING' in t)
            self.assert_('SYMBOL' in t)
            self.assert_('INTEGER' not in t)
            t = map(str, t1.Slots.Types("s2"))
            self.assert_('SYMBOL' not in t)
            self.assert_('INTEGER' in t)

    def ctf_Template_09(self):
        """Testing: Template.Slots.IsSinglefield, Template.Slots.IsMultifield"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1)
                (multislot s2)
                """, "test template")
            self.assert_(t1.Slots.IsSinglefield('s1'))
            self.assert_(not t1.Slots.IsMultifield('s1'))
            self.assert_(not t1.Slots.IsSinglefield('s2'))
            self.assert_(t1.Slots.IsMultifield('s2'))

    def ctf_Template_10(self):
        """Testing: Template Facts coherence between assertions"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            t1 = e.BuildTemplate(
                "t1", """
                (slot s1)
                """, "test template")
            f11 = t1.BuildFact()
            f11.Slots['s1'] = clips.Symbol("sym13")
            f12 = t1.BuildFact()
            f12.Slots['s1'] = clips.Symbol("sym42")
            f11.Assert()
            self.assertEqual(f11.Slots['s1'], clips.Symbol("sym13"))
            self.assertEqual(f12.Slots['s1'], clips.Symbol("sym42"))


# end.
