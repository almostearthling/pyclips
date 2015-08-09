# test_retpass.py

"""revision: $Id: test_retpass.py 335 2008-01-13 01:50:50Z Franz $
TESTS:
passing Fact objects as arguments
passing Instance objects as arguments
Fact objects as return values
Instance objects as return values
"""

class ctc_RetPass(ctestcase):
    """test Class objects"""

    def ctf_PassFacts_01(self):
        """Testing: Fact objects as arguments"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGlobal("g")
            f = e.Assert("(a)")
            g.Value = f
            e.SendCommand("(ppfact ?*g*)")
            self.assertEqual(clips.StdoutStream.Read().strip(), "(a)")

    def ctf_PassInstances_01(self):
        """Testing: Instance objects as arguments"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            g = e.BuildGlobal("g")
            c = e.BuildClass("C", "(is-a USER)", "New Class")
            i = c.BuildInstance("i")
            g.Value = i
            e.SendCommand("(send ?*g* print)")
            self.assertEqual(
                e.Eval("(instance-name ?*g*)"), clips.InstanceName('i'))

    def ctf_RetFacts_01(self):
        """Testing: Fact objects as return values"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.SendCommand("(defglobal ?*g* = nil)")
            e.SendCommand("""(defrule r
                                ?f <- (a)
                            =>
                                (bind ?*g* ?f))""")
            e.SendCommand("(deffunction f () (return ?*g*))")
            e.Assert("(a)")
            e.Run()
            f = clips.Eval("(f)")
            self.assertEqual(f.Relation, clips.Symbol('a'))

    def ctf_RetInstances_01(self):
        """Testing: Instance objects as return values"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.SendCommand("(defclass C (is-a USER))")
            e.SendCommand("(make-instance [i] of C)")
            i = clips.Eval("(instance-address [i])")
            self.assertEqual(i.Name, clips.InstanceName('i'))



# end.
