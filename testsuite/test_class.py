# test_classes.py


"""revision $Id: test_class.py 306 2006-06-22 00:15:55Z Franz $
TESTS:

BuildClass
BuildInstance
ClassList
FindClass
LoadInstancesFromString
FindInstance
BuildMessageHandler

Class:
  IsSuperclassOf
  IsSubclassOf
  Subclasses
  Abstract
  Reactive
  Deletable
  Description
  PPForm
  Name
  Module
  BuildSubclass
  Deletable
  WatchSlots
  WatchInstances
  MessageHandlerIndex
  MessageHandlerWatched
  UnwatchMessageHandler
  WatchMessageHandler
  MessageHandlerName
  MessageHandlerType
  NextMessageHandlerIndex
  MessageHandlerDeletable
  AddMessageHandler
  RemoveMessageHandler
  MessageHandlerPPForm
  BuildInstance
  RawInstance

Class.Slots
  Names
  NamesDefined
  Exists
  ExistsDefined
  Cardinality
  AllowedValues
  AllowedClasses
  Types, Slots.Sources
  IsPublic
  IsInitable
  Range
  IsWritable
  HasDirectAccess
  Facets
  DefaultValue

Instance:
  Slots
  GetSlot
  PutSlot
  PPForm
  Class
  Name
"""


default_classes = """\
MAIN::FLOAT
MAIN::INTEGER
MAIN::SYMBOL
MAIN::STRING
MAIN::MULTIFIELD
MAIN::EXTERNAL-ADDRESS
MAIN::FACT-ADDRESS
MAIN::INSTANCE-ADDRESS
MAIN::INSTANCE-NAME
MAIN::OBJECT
MAIN::PRIMITIVE
MAIN::NUMBER
MAIN::LEXEME
MAIN::ADDRESS
MAIN::INSTANCE
MAIN::USER
MAIN::INITIAL-OBJECT""".split()


class ctc_Class(ctestcase):
    """test Class objects"""

    def ctf_Class_01(self):
        """Testing: BuildClass, FindClass, ClassList, {...}\n""" \
        """         Class.IsSubclassOf, Class.IsSuperclassOf,\n""" \
        """         Class.Subclasses, Class.Superclasses,\n""" \
        """         Class.Abstract, Class.Reactive"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            li = e.ClassList()
            self.assertEqual(default_classes, map(str, li))
            c0 = e.FindClass("USER")
            c1 = e.FindClass("OBJECT")
            c2 = e.BuildClass("C", "(is-a USER)", "New Class")
            for name in c2.Superclasses():
                c = e.FindClass(name)
                self.assert_(c.IsSuperclassOf(c2))
                self.assert_(c2.IsSubclassOf(c))
            for name in c0.Subclasses():
                c = e.FindClass(name)
                self.assert_(c.IsSubclassOf(c0))
                self.assert_(c0.IsSuperclassOf(c))
            self.assert_(not c2.Abstract)
            self.assert_(c2.Reactive)
            self.assert_(c1.Abstract)
            self.assert_(not c1.Reactive)

    def ctf_Class_02(self):
        """Testing: Clear, Reset, Class.PPForm, Class.Description, Class.Module"""
        e0 = self.envdict['clips']
        e2 = self.envdict['env']
        for e in [e0, e2]:
            e.Clear()
            e.Reset()
        c0 = e0.BuildClass("C2", "(is-a USER)", "New Class")
        e2.Build(c0.PPForm())
        c2 = e2.FindClass("C2")
        self.assertEqual(c0.Description(), c2.Description())
        self.assertEqual(c0.Module, c2.Module)

    def ctf_Class_03(self):
        """Testing: Class.BuildSubclass"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            D = C.BuildSubclass("D")
            self.assert_(C.IsSuperclassOf(D))

    def ctf_Class_04(self):
        """Testing: Class.WatchSlots, Class.WatchInstances"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            self.assert_(C.WatchSlots)
            self.assert_(C.WatchInstances)

    def ctf_Class_05(self):
        """Testing: Class.MessageHandlerIndex, Class.MessageHandlerWatched"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            self.assertEqual(C.MessageHandlerIndex("get-s1", "primary"), 1)
            self.assertEqual(C.MessageHandlerIndex("put-s1", "primary"), 2)
            self.assert_(C.MessageHandlerWatched(1))
            self.assert_(C.MessageHandlerWatched(2))

    def ctf_Class_06(self):
        """Testing: Class.UnwatchMessageHandler, Class.WatchMessageHandler"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            C.UnwatchMessageHandler(1)
            self.assert_(not C.MessageHandlerWatched(1))
            C.WatchMessageHandler(1)
            self.assert_(C.MessageHandlerWatched(1))

    def ctf_Class_07(self):
        """Testing: Class.MessageHandlerName, Class.MessageHandlerType"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            self.assertEqual(C.MessageHandlerName(1), "get-s1")
            self.assertEqual(C.MessageHandlerName(2), "put-s1")
            self.assertEqual(C.MessageHandlerType(1), "primary")
            self.assertEqual(C.MessageHandlerType(2), "primary")

    def ctf_Class_08(self):
        """Testing: Class.NextMessageHandlerIndex, Class.MessageHandlerDeletable"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            self.assertEqual(C.NextMessageHandlerIndex(1), 2)
            self.assert_(not C.MessageHandlerDeletable(1))
            self.assert_(not C.MessageHandlerDeletable(2))

    def ctf_Class_09(self):
        """Testing: BuildMessageHandler, Class.AddMessageHandler, {...}"""
        """         Class.RemoveMessageHandler, Class.MessageHandlerPPForm"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            self.assertEqual(C.AddMessageHandler("m1", "", "(return nil)"), 1)
            self.assertEqual(e.BuildMessageHandler("m2", C, "", "(return nil)"), 2)
            anonppf1 = C.MessageHandlerPPForm(1).replace("1", "??")
            anonppf2 = C.MessageHandlerPPForm(2).replace("2", "??")
            self.assertEqual(anonppf1, anonppf2)
            self.assert_(C.MessageHandlerDeletable(1))
            C.RemoveMessageHandler(2)

    def ctf_Class_10(self):
        """Testing: Class.BuildInstance, Class.RawInstance"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            i1 = C.BuildInstance("i1")
            i2 = C.RawInstance("i2")
            self.assertEqual(i1.Class.Name, i2.Class.Name)

    def ctf_Class_11(self):
        """Testing: Class.MessageHandlerList, Class.AllMessageHandlerList"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            C.AddMessageHandler("m1", "", "(return nil)")
            D = C.BuildSubclass("D")
            D.AddMessageHandler("m2", "", "(return nil)")
            liD = D.MessageHandlerList()
            liAD = D.AllMessageHandlerList()
            self.assert_(len(liD) < len(liAD))

    def ctf_Instance_01(self):
        """Testing: BuildInstance, Class.Deletable, Instance.Slots, Instance.PPForm"""
        d1 = []
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            c = e.BuildClass("C3", """
                (is-a USER)
                (slot ts1 (type SYMBOL))
                (multislot ts2)
            """)
            self.assert_(c.Deletable)
            s1 = clips.Symbol(e.Eval("(gensym*)"))
            s2 = clips.Symbol(e.Eval("(gensym*)"))
            s3 = clips.Symbol(e.Eval("(gensym*)"))
            m1 = clips.Multifield([s2, s3])
            i = e.BuildInstance("test", c)
            self.assert_(not c.Deletable)
            i.Slots['ts1'] = s1
            i.PutSlot('ts2', m1)
            rs1 = i.GetSlot('ts1')
            rm1 = i.Slots['ts2']
            self.assertEqual(rs1, s1)
            self.assertEqual(rm1, m1)
            self.assertEqual(len(i.Slots.keys()), 2)
            d1.append(i.PPForm())
        self.assertEqual(d1[0], d1[-1])

    def ctf_Instance_02(self):
        """Testing: FindInstance, Instance.Class, Instance.Name"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            c = e.BuildClass("C4", "(is-a USER)")
            i = e.BuildInstance("test-%s" % x, c)
            i0 = e.FindInstance("test-%s" % x)
            self.assertEqual(i.Name, "test-%s" % x)
            self.assertEqual(i.Class.Name, "C4")
            self.assertEqual(i.PPForm(), i0.PPForm())

    def ctf_Instance_03(self):
        """Testing: LoadInstancesFromString"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            c = e.BuildClass("C4", "(is-a USER)")
            e.LoadInstancesFromString("(d of C4)(f of C4)")
            d = e.FindInstance('d')
            f = e.FindInstance('f')
            self.assertEqual(d.Class.Name, "C4")
            self.assertEqual(f.Class.Name, "C4")

    def ctf_Slots_01(self):
        """Testing: Slots.Names, Slots.Exists, Slots.ExistsDefined, {...}"""
        """         Slots.NamesDefined"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (type INTEGER))(slot s2 (type STRING))")
            D = C.BuildSubclass("D", "(slot s3 (type INTEGER)(default 1))")
            li = D.Slots.Names()
            li1 = D.Slots.NamesDefined()
            self.assertEqual(len(li), 3)
            self.assertEqual(len(li1), 1)
            self.assert_(not C.Slots.Exists(li[2]))
            self.assert_(D.Slots.Exists(li[2]))

    def ctf_Slots_02(self):
        """Testing: Slots.Cardinality, Slots.AllowedValues"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (type INTEGER))(slot s2 (type STRING))")
            D = C.BuildSubclass("D", "(multislot s3 (type INTEGER)(default 1))")
            self.assertEqual(len(D.Slots.Cardinality("s3")), 2)
            self.assertEqual(len(D.Slots.Cardinality("s1")), 0)
            self.assert_(not D.Slots.AllowedValues("s1"))

    def ctf_Slots_03(self):
        """Testing: Slots.Types, Slots.Sources"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (type INTEGER))(slot s2 (type STRING))")
            D = C.BuildSubclass("D", "(slot s3 (type INTEGER)(default 1))")
            self.assertEqual(D.Slots.Types("s1")[0], "INTEGER")
            self.assertEqual(D.Slots.Sources("s1")[0], "C")

    def ctf_Slots_04(self):
        """Testing: Slots.IsPublic, Slots.IsInitable, Slots.Range"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", """(is-a USER)
                        (slot s1 (create-accessor read-write)
                                 (type INTEGER) (range 0 1)
                                 (visibility public))
                        """)
            self.assert_(C.Slots.IsPublic("s1"))
            self.assert_(C.Slots.IsInitable("s1"))
            self.assertEqual(int(C.Slots.Range("s1")[0]), 0)
            self.assertEqual(int(C.Slots.Range("s1")[1]), 1)

    def ctf_Slots_05(self):
        """Testing: Slots.IsWritable, Slots.HasDirectAccess, Slots.Facets"""
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", "(is-a USER)(slot s1 (create-accessor read-write))")
            self.assert_(C.Slots.IsWritable("s1"))
            self.assert_(C.Slots.HasDirectAccess("s1"))
            self.assert_(clips.Symbol("put-s1") in C.Slots.Facets("s1"))

    def ctf_Slots_06(self):
        """Testing: Slots.DefaultValue, Slots.AllowedClasses"""
        if clips.CLIPS_VERSION < "6.24":
            sys.stderr.write("SKIPPED ")
            return
        for x in self.envdict.keys():
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass(
                "C", """(is-a USER)
                        (slot s1 (allowed-classes INTEGER STRING)
                                 (default 42))
                        """)
            self.assertEqual(len(C.Slots.AllowedClasses("s1")), 2)
            self.assert_('INTEGER' in map(str, C.Slots.AllowedClasses("s1")))
            self.assert_('STRING' in map(str, C.Slots.AllowedClasses("s1")))
            self.assertEqual(42, int(C.Slots.DefaultValue("s1")))




# end.
