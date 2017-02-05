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
* remove the top-level module and integrate top-level utilities in the `Environment` class module: most of the top-level utilities that are not ES related are also influent on Environments
* rewrite the setup script in a simpler way (it doesn't have to translate the top-level code)
* possibly separate the process of building CLIPS exploiting it as a library -- and possibly a shared one -- in order to make the process of developing PyCLIPS more independent from CLIPS itself, and possibly adapt PyCLIPS to more versions of the library. However this might be a difficult part especially for pieces of code in CLIPS that have to be patched
* remove the current environment code from the *C* module
* configure the `setup.h` header to have `ENVIRONMENT_API_ONLY` (or something similar for CLIPS 6.30) set to *TRUE* in order to remove all non *Environment* related code from the CLIPS library.

Also, the test suite has to be simplified to only contain `Environment` class related tests.

The first part of the new developement course will only affect Python code: the C code has to be left untouched. Removing current-environment-related code from the C module should be considered optional.
