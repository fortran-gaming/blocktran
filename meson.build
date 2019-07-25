project('tetran', 'c', 'fortran',
  default_options : ['default_library=static', 'buildtype=release'],
  meson_version : '>= 0.51')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

#--- Curses

if os == 'windows'
  curses_path = 'c:/pdcurses'
else
  curses_path = []
endif

curses = dependency('ncurses', required: false)
if not curses.found()
    curses_lib = cc.find_library('curses', dirs : curses_path, required : false)
    if curses_lib.found()
      curses = declare_dependency(dependencies : curses_lib, include_directories : curses_path)
    endif
endif

subdir('src')
