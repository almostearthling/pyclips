PyCLIPS - a Python module to integrate CLIPS into Python
(c) 2002-2008 Francesco Garosi/JKS

Version 1.0 (release)



RELEASE NOTES
=============

This is a C module embedding CLIPS functionality. To build it, you need to
download the source code for CLIPS, which can be found on its web site, at

    http://clipsrules.sourceforge.net/

(you must follow the link to the download page). Please note that the ZIP
version of the source archive is needed, since the setup procedure unpacks
it by itself. Recent versions of PyCLIPS will try to connect to SourceForge
automatically to download the latest source package supported by PyCLIPS,
but in case it does not work you will have to download it manually. If the
source package is provided, no attempt to connect to the internet will be
made.

The module fully embeds the CLIPS engine, with COOL (Clips Object Oriented
Language) and environments support. It does not require a CLIPS installation
to function. It also supports the file formats produced (and interpreted) by
CLIPS itself. Documentation is provided in source (TeX) format, as a PDF
file and as an HTML tarball.

This is a new stable release of the 1.0 branch. Not all the approved
enhancements have been completed, but all basic module functionality appears
to be working and stable. The test suite is also almost complete and helps
to find possible flaws and weaknesses. From now on I will try to enhance the
module with additional (optional) packages, keeping the base version as
close as possible to the 1.0 stable branch. This release is named 1.0.X.Y
where X is a number indicating the patch level (previously it was preceded
by an 'R') and Y is an incremental value. The change in version numbering
has been introduced in order to disambiguate the version string, so that
setuptools can better decide whether or not to replace a package.

Further beta releases will be named 1.1_nn, where the integer number nn will
increase until the final 1.1 version is reached. The release policies are
the same: only "stable enough" versions will be uploaded to the file release
system, while the SVN repository will also provide possibly unstable changes.



REQUIREMENTS
============

PyCLIPS requires Python 2.4 or higher to work. Recently it has been tested
on Python 2.5. Previous versions of Python are not supported.

PyCLIPS also requires the CLIPS 6.23 or CLIPS 6.24 sources to compile.
Compilation will fail against CLIPS version 6.22 or earlier: although the
CLIPS API is quite stable, there are some improvements at this level in
CLIPS 6.23 which are required for PyCLIPS. If you don't need to stick to
the former CLIPS version, please use CLIPS 6.24: the previous version lacks
some features and PyCLIPS is mostly developed against the more recent
release.

I use GCC 3.x to compile PyCLIPS both on UNIX (Linux and Solaris) and on
Win32. If Python was compiled using Visual C 9.0 (the one released with
Visual Studio 2008 Express) and the Platform SDK, this environment does also
build the module successfully, although with warnings due to deprecation of
insecure functions. Win32 users are encouraged to use the prebuilt package.
PyCLIPS can also be successfully compiled using the free Microsoft Visual
C++ Toolkit 2003. However also this compiler will issue some warnings (see
below) which are mostly safe to ignore.

I do not know other dependencies or requirements to build PyCLIPS, but feel
free to contact me for any annoyance.



INSTALLATION
============


1) from the source
------------------

Installing from the source should be simple, as the module uses distutils
in the usual way. As said before, the setup script will try to download
the CLIPS source code.

The sequence of operations should be as follows:

    $ gunzip -c pyclips-1.0.X.Y.tar.gz | tar xvf -
    $ cd pyclips
    $ python setup.py build
    $ su -c "python setup.py install"

This could not work, and you might still receive a message saying that the
CLIPS source package has not been found: you will need to download it
manually following the instructions on the above mentioned web site, and
copy the package in the base directory of the source tree ('pyclips').

The setup procedure should run out of the box. It has been tested on Linux,
Solaris (with GCC and a self compiled version of Python) and Win32 (using
MinGW 3.1 and MSYS 1.0, as well as some flavours of MS Visual C). Recent
versions of PyCLIPS use setuptools instead of distutils, if found: the
setup script falls back to distutils if setuptools are not installed. The
standard setup procedure will also take care of performing additional steps
such as applying patches to the CLIPS source if possible (see below).

To use setuptools instead of distutils you will have to download the
'ez_setup.py' file from http://peak.telecommunity.com, for instance using
the following command:

    $ wget http://peak.telecommunity.com/dist/ez_setup.py

when you are in the base directory of the source tree ('pyclips'). The
main advantage in using setuptools is that the resulting installation is
less "sparse", and will consist in a single file in your $PYTHONPATH. I
found myself this to be a big advantage. Moreover, recent PyCLIPS binary
distributions will be packaged as "eggs", as I verified that this gives
more compatibility across different Linux distributions: the binaries for
Linux present on SourceForge (as .egg) have been built on a Debian-based
distribution and installed and successfully tested on a Slackware-based
one (respectively: Ubuntu and Zenwalk, even though on Ubuntu I completely
recompiled Python for the build system and debugging).

A small note for MS Visual C users: during compilation the compiler will
warn about not recognizing the '-fno-strict-aliasing' option, but it will
continue the build process anyway: this is not a problem, as Microsoft C
does not try to aggressively optimize code in a way that would be unsafe
for CLIPS. This parameter is necessary for CLIPS when using GCC, as stated
in the CLIPS Advanced Programming Guide for CLIPS 6.24: in fact, omission
of this flag produces a module that might lead to obscure crashes.


2) using the prebuilt installer
-------------------------------

For Win32 I also provide prebuilt packages: to use them, just double-click
the installer you downloaded. It will just find out where Python is located
and copy the module in the right place. If your  distribution supports
setuptools, you might also be able to use the "easy_install" program to
install a prebuilt binary distribution (of the ones whose filenames have a
".egg" extension) suitable to your needs. Linux ".egg" packages may also be
available for some Python versions.



LICENSE
=======

PyCLIPS is released under the Library General Public License (LGPL): a copy
of the license text is included in the manual. Also, if you install the
module, the license text can be viewed by typing:

    >>> import clips
    >>> print clips.license

at the Python prompt. However the license can be found in the usual place,
that is at the GNU's web site (http://www.gnu.org/copyleft/lesser.html).

Please take your time to review the CLIPS license, especially in case the
automatic CLIPS source download succeeds (because if it happens, it means
that you have not read the notice on the CLIPS web site): CLIPS is free for
everyone, but the Author suggests that if you derive any commercial or
monetary benefit from CLIPS, a donation might be appreciated. This applies
to CLIPS only, however: I give my "translation" work for free in the spirit
of the LGPL.



DOCUMENTATION
=============

The documentation can be found on SourceForge, at the PyCLIPS download page.
It is distributed as a single PDF file, generated from the TeX sources that
are included in the source distribution. The PyCLIPS documentation does not
cover the CLIPS language: CLIPS is very well documented, the manuals for
CLIPS are available as PDF on its web site.



PATCHES
=======

Recent versions of PyCLIPS allow the possibility to easily apply optional
patches to the official CLIPS source. Mandatory patches are however always
applied by the setup script at the first build. Optional patches can be
used to solve specific problems on some platforms or to test experimental
bug fixes. At the time of writing there are in fact three patchsets:

    - bgfx: fixes provided by the CLIPS maintainers
    - test: "experimental" bug or annoyance fixes
    - ia64: fixes needed for the x86_64 platforms

Patches marked as "experimental" in fact derive from considerations found
in developer forums and from contributions provided by other developers
after addressing particular issues that were found after the release of
the official CLIPS source. On the other hand, patches written for special
platforms are normally mandatory on those: for example, the "ia64" patch
set is needed to successfully pass the test suite on x86_64 platforms. 
The other fixes (marked as "bgfx") should be considered mandatory, as they
have officially been provided (sometimes as files to be replaced) by the
people who invented CLIPS, and will hopefully be removed as soon as the
next CLIPS release is out.

If your system has a working GNU 'patch' command (it can be compiled for
Win32 as well), the setup script will use it to apply all patch sets; in
fact for 32-bit platforms the 'ia64' patchset is optional but not harmful.
The setup script will skip files that have already been patched, that is,
for which a copy of the original file exists: do not attempt to manually
patch the files.



TO DO
=====

Testing thoroughly has to be considered a primary goal. Also, in the same
spirit, writing more tests for the test suite is needed: I will be grateful
to everyone that would volunteer for helping me in this task.

Probably both the C and the Python code in the module need some cleanup.
Also the installation script is quite confused - probably it's not easy to
understand what it does when it "creates the environment-aware submodule".

The documentation is fairly complete, I think. However, if someone speaking
english is reading this, probably she or he will notice that it's definitely
not my mother tongue: even though I asked a friend to help me to find errors
in current manual release, I think that especially for further additions and
integrations I will introduce some clues of my "english illiteracy". So feel
free to comment and criticize documentation as well.

There are also other goals: they will pop up from time to time in the RFE
list of the SourceForge.net PyCLIPS page. As the application I'm writing
using the module goes on, I discover bugs and missing features, as well as
concepts that can be implemented in different ways: every time it happens
I post a request on SourceForge, as an external developer would do. Every
developer with a SourceForge account can do the same, and this is also a
contribution to the project.



ACKNOWLEDGEMENTS
================

I'd like to thank the CLIPS crew for writing CLIPS, putting it in the Public
Domain, and for writing the extensive documentation. And the people at CLIPS
Developer Forum (http://groups.google.com/group/CLIPSESG/) for solving many
of my doubts.

I really have to thank Johan Lindberg (you can visit his blog at the address
http://commentsarelies.blogspot.com/) who also develops the module with me.
His help has been invaluable in testing, finding bugs and inconsistencies,
as well as writing the example suite - it was heavily needed, and his idea
to translate Gary's work for CLIPSJNI into PyCLIPS also gives an opportunity
to compare the two different interfaces.

Also thanks to people who have supported my work, especially Vladimir Ulogov
who also worked on a module like this.

A very big "Thank You" goes to all friends and people who have been near me,
sometimes flooded by my words when I have been speaking about PyCLIPS (and
more generally about computers) in front of some beverage...

I would like as well to thank the people who found either bugs or unexpected
behaviours for helping me in the hard task of finding a solution, as well as
those who showed interest in using PyCLIPS for their projects and extending
it to make it more useful.

I am looking forward to having many people to thank here. :)



CONTACT INFORMATION
===================

I can be reached for suggestions, criticisms and reports about any annoyance
at the following e-mail address:

    franzg -at- users -dot- sourceforge -dot- net

I also have a web site, where I will occasionally write some news about this
module as well:

    http://www.jks.it

Every kind of help is really welcome.

Francesco Garosi


---

$Id: README 343 2008-02-22 01:35:38Z Franz $
