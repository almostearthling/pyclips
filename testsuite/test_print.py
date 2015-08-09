# test_print.py

import sys, re, StringIO

"""revision $Id: test_print.py 247 2005-02-23 00:28:47Z Franz $
TESTS:
BuildGlobal
PrintFacts
PrintRules
PrintAgenda
PrintClasses
PrintDeffacts
PrintDefinstances
PrintModules
PrintInstances
PrintGlobals
PrintGenerics
PrintSubclassInstances
PrintAgenda
PrintBreakpoints
PrintMessageHandlers
BrowseClasses

Generic
  PrintMethods

Rule
  PrintMatches

Class
  PrintMessageHandlers
  PrintAllMessageHandlers
  PreviewSend

"""


def i_returnOutput(func, args=None, kwargs=None):
    """execute a function while redirecting stdout"""
    io = StringIO.StringIO()
    save_stdout = sys.stdout
    sys.stdout = io
    try:
        if args is None:
            if kwargs is None: r = func()
            else: r = func(**kwargs)
        else:
            if kwargs is None: r = func(*args)
            else: r = func(*args, **kwargs)
        s = io.getvalue().strip()
        sys.stdout = save_stdout
        return r, s
    except:
        sys.stdout = save_stdout
        raise


def i_checkContains(s, rex):
    """check whether or not the given string contains the supplied RE"""
    return bool(re.search(rex, s) is not None)



class ctc_Print(ctestcase):
    """test debug output functions"""

    def ctf_Print_01(self):
        """Testing: PrintFacts"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Assert("(f1)")
            r, s = i_returnOutput(e.PrintFacts)
            self.assert_(i_checkContains(s, r"f\-\d\s+\(initial\-fact\)"))
            self.assert_(i_checkContains(s, r"f\-\d\s+\(f1\)"))

    def ctf_Print_02(self):
        """Testing: PrintRules, PrintAgenda"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.BuildRule("f1-rule", "(f1)", "(assert (f2))")
            e.Assert("(f1)")
            r, s = i_returnOutput(e.PrintRules)
            self.assert_(i_checkContains(s, r"f1\-rule"))
            r, s = i_returnOutput(e.PrintAgenda)
            self.assert_(i_checkContains(s, r"\d\s+f1\-rule"))

    def ctf_Print_03(self):
        """Testing: PrintClasses, PrintDeffacts, PrintDefinstances, PrintModules"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            r, s = i_returnOutput(e.PrintClasses)
            self.assert_(i_checkContains(s, r"INITIAL\-OBJECT"))
            self.assert_(i_checkContains(s, r"USER"))
            self.assert_(i_checkContains(s, r"INSTANCE"))
            self.assert_(i_checkContains(s, r"ADDRESS"))
            self.assert_(i_checkContains(s, r"LEXEME"))
            self.assert_(i_checkContains(s, r"NUMBER"))
            self.assert_(i_checkContains(s, r"PRIMITIVE"))
            self.assert_(i_checkContains(s, r"OBJECT"))
            # ...
            self.assert_(i_checkContains(s, r"FLOAT"))
            r, s = i_returnOutput(e.PrintDeffacts)
            self.assert_(i_checkContains(s, r"initial\-fact"))
            r, s = i_returnOutput(e.PrintDefinstances)
            self.assert_(i_checkContains(s, r"initial\-object"))
            r, s = i_returnOutput(e.PrintModules)
            self.assert_(i_checkContains(s, r"MAIN"))

    def ctf_Print_04(self):
        """Testing: PrintTemplates, PrintMessageHandlers"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            r, s = i_returnOutput(e.PrintTemplates)
            self.assert_(i_checkContains(s, r"initial\-fact"))
            r, s = i_returnOutput(e.PrintMessageHandlers)
            self.assert_(i_checkContains(s, r"message\-duplicate"))
            self.assert_(i_checkContains(s, r"direct\-duplicate"))
            self.assert_(i_checkContains(s, r"message\-modify"))
            self.assert_(i_checkContains(s, r"direct\-modify"))
            self.assert_(i_checkContains(s, r"print"))
            self.assert_(i_checkContains(s, r"create"))
            self.assert_(i_checkContains(s, r"delete"))
            self.assert_(i_checkContains(s, r"init"))

    def ctf_Print_05(self):
        """Testing: BuildGlobal, PrintGlobals, PrintGenerics"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGlobal("g", 42)
            r, s = i_returnOutput(e.PrintGlobals)
            self.assert_(i_checkContains(s, "g"))
            g1 = e.BuildGeneric("g1")
            r, s = i_returnOutput(e.PrintGenerics)
            self.assert_(i_checkContains(s, "g1"))

    def ctf_Print_06(self):
        """Testing: PrintInstances, PrintSubclassInstances"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            c = e.BuildInstance("c", C)
            D = e.BuildClass("D", "(is-a C)")
            d = e.BuildInstance("d", D)
            r, s2 = i_returnOutput(e.PrintInstances, (C,))
            r, s3 = i_returnOutput(e.PrintInstances, (D,))
            r, s4 = i_returnOutput(e.PrintSubclassInstances, (C,))
            self.assert_(not i_checkContains(s2, re.escape(d.PPForm())))
            self.assert_(i_checkContains(s3, re.escape(d.PPForm())))
            self.assert_(not i_checkContains(s3, re.escape(c.PPForm())))
            self.assert_(i_checkContains(s4, re.escape(c.PPForm())))
            self.assert_(i_checkContains(s4, re.escape(d.PPForm())))

    def ctf_Print_07(self):
        """Testing: PrintAgenda, PrintBreakpoints, PrintFocusStack"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            dr = e.BuildRule("dr", "(duck)", "(assert (quack))")
            f0 = e.Assert("(duck)")
            rex = r"0\s+%s:\s+f-%s" % (dr.Name, f0.Index)
            r, s = i_returnOutput(e.PrintAgenda)
            self.assert_(i_checkContains(s, rex))
            r, s = i_returnOutput(e.PrintBreakpoints)
            self.assertEqual(s.strip(), "MAIN:")
            dr.Breakpoint = True
            r, s = i_returnOutput(e.PrintBreakpoints)
            self.assert_(i_checkContains(s, "dr"))
            m = e.BuildModule("MNEW", "")
            m.SetFocus()
            r, s = i_returnOutput(e.PrintFocusStack)
            self.assert_(i_checkContains(s, "MNEW"))
            self.assert_(i_checkContains(s, "MAIN"))

    def ctf_Print_08(self):
        """Testing: Generic.PrintMethods"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGeneric("g")
            g.AddMethod("(?a NUMBER)", "(+ ?a ?a)")
            g.AddMethod("(?a STRING)", "(str-cat ?a \"+\" ?a)")
            r, s = i_returnOutput(g.PrintMethods)
            self.assert_(i_checkContains(s, r"g.+\(NUMBER\)"))
            self.assert_(i_checkContains(s, r"g.+\(STRING\)"))

    def ctf_Print_09(self):
        """Testing: Rule.PrintMatches"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            r = e.BuildRule("dr", "(duck)", "(assert (quack))")
            f = e.Assert("(duck)")
            r, s = i_returnOutput(r.PrintMatches)
            self.assert_(i_checkContains(s, r"Matches for Pattern 1\s+f\-1"))
            self.assert_(i_checkContains(s, r"Activations\s+f\-1"))

    def ctf_Print_10(self):
        """Testing: Class.PrintMessageHandlers, Class.PrintAllMessageHandlers"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            D = e.BuildClass(
                "D", "(is-a C)(slot s2 (create-accessor read-write))")
            r, s = i_returnOutput(D.PrintMessageHandlers)
            self.assert_(i_checkContains(s, r"get-s2\ .*\ class"))
            self.assert_(i_checkContains(s, r"put-s2\ .*\ class"))
            self.assert_(not i_checkContains(s, r"get-s1\ .*\ class"))
            self.assert_(not i_checkContains(s, r"put-s1\ .*\ class"))
            r, s = i_returnOutput(D.PrintAllMessageHandlers)
            self.assert_(i_checkContains(s, r"get-s2\ .*\ class"))
            self.assert_(i_checkContains(s, r"put-s2\ .*\ class"))
            self.assert_(i_checkContains(s, r"get-s1\ .*\ class"))
            self.assert_(i_checkContains(s, r"put-s1\ .*\ class"))

    def ctf_Print_11(self):
        """Testing: BrowseClasses, Class.PreviewSend"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            C.AddMessageHandler("mh", "", "(return nil)")
            r, s = i_returnOutput(C.PreviewSend, ("mh",))
            self.assert_(i_checkContains(s, r"\>\> mh primary in class C"))
            self.assert_(i_checkContains(s, r"\<\< mh primary in class C"))
            r, s = i_returnOutput(e.BrowseClasses, ("USER",))
            self.assert_(i_checkContains(s, r"USER\s+INITIAL\-OBJECT\s+C"))

    def ctf_Print_12(self):
        """Testing: Module.BuildGlobal, Module.ShowGlobals"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            mo = e.BuildModule("TEst", "(export ?ALL)")
            g = mo.BuildGlobal("g", 42)
            r, s = i_returnOutput(mo.ShowGlobals)
            self.assert_(i_checkContains(s, re.escape("?*g* = 42")))

# end.
