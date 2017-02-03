  1. git clone https://github.com/almostearthling/pyclips.git
  2. cd pyclips
  3. git checkout pyclips-1.1_clips-6.30
  4. delete file `clipssrc`
  5. create new folder `clipssrc` inside the `pyclips` folder
  4. download CLIPS 6.30 source code from the official repository at https://sourceforge.net/projects/clipsrules/files/CLIPS/6.30/
  5. browse archive and extract contents of `core` folder into `clipssrc` folder
  5. in`setup.py`, make sure that around line 738 `'-DWIN_MVC'` is part of the `CFLAGS` list
  6. install patch utility for windows and add it to PATH
  6. deactivate user account control pop up windows for patch
  7. download visual studio express 2008, http://download.microsoft.com/download/E/8/E/E8EEB394-7F42-4963-A2D8-29559B738298/VS2008ExpressWithSP1ENUX1504728.iso
  7. start visual studio express 2008 (32 bit) command prompt
  8. cd into the `pyclips` folder
  8. run `python setup.py build`
  8. install via `python setup.py install`
