# PyCLIPS Roadmap

This has to be a new edition of PyCLIPS, and should comply with some constraints, in order to make the code lighter, more readable, and more consistent. The main goal is to make the module easier to use, and this will be achieved mostly in two ways:

1. Remove the "current environment" feature from PyCLIPS: as it might be useful in CLIPS when no environments are needed, it's mostly a complication in Python since it's easy to treat an *Environment* the same way the top-level module is treated. For instance,

  ```
  import clips

  e = clips.Environment()
  e.Assert("(something)")
  ```

  is not much longer to type than

  ```
  import clips

  clips.Assert("(something)")
  ```

  especially in the long run. This has many advantage, as it will reduce the code (and the library size) and make the setup process much simpler.

2. Create a simpler interface on top of the "traditional" PyCLIPS.

Where the second goal was the main one for the old 2.0 release, the first point will be the main focus of this one. It will require the following steps:

* rewrite the Environment class directly, not as an automatic translation of the top-level `clips` module
* remove the top-level module and integrate top-level utilities in the `Environment` class module
* rewrite the setup script in a simpler way (it doesn't have to translate the top-level code)
* remove the current environment code from the *C* module

Also, the test suite has to be simplified to only contain `Environment` class related tests.
