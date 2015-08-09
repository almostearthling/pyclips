# __init__.py
# clips wrapper module loader

# (c) 2002-2008 Francesco Garosi/JKS
#  The author's copyright is expressed through the following notice, thus
#  giving effective rights to copy and use this software to anyone, as shown
#  in the license text.
#
# NOTICE:
#  This software is released under the terms of the GNU Lesser General Public
#  license; a copy of the text has been released with this package (see file
#  _license.py, where the license text also appears), and can be found on the
#  GNU web site, at the following address:
#
#           http://www.gnu.org/copyleft/lesser.html
#
#  Please refer to the license text for any license information. This notice
#  has to be considered part of the license, and should be kept on every copy
#  integral or modified, of the source files. The removal of the reference to
#  the license will be considered an infringement of the license itself.



"""\
clips - high-level interface to the CLIPS engine module
        (c) 2002-2008 Francesco Garosi/JKS

This work is based on the CLIPS library and interpreter, by Gary Riley and
others. Please visit its homepage at http://clipsrules.sourceforge.net
for further information and to obtain the full source code.

Please issue 'print clips.license' at the prompt for licensing information.
"""


from _clips_wrap import *
from _eclips_wrap import Environment, CurrentEnvironment
from _license import license
from _version import version, version_string


# provide our __dict__ to the _clips_wrap in order to set up stock classes:
# the name _setParentModuleDict will be removed later
from _clips_wrap import _setParentModuleDict
_setParentModuleDict(globals())
del _setParentModuleDict



# define the __all__ list so that the module can avoid useless names: in
#  fact all useful names that this part of the module begin with a letter
__all__ = filter(lambda x: x[0] != '_', dir())



# end.
