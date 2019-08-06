project('tetran', 'c', 'fortran',
  default_options : ['default_library=static', 'buildtype=release'],
  meson_version : '>= 0.51')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

if fc.get_id() == 'gcc'
  add_project_arguments('-march=native', '-Wall', '-Wextra', '-Wpedantic',
    '-Werror=array-bounds', '-finit-real=nan', '-Wconversion', '-fimplicit-none',
    language: 'fortran')

  if fc.version().version_compare('>=8')
    add_project_arguments('-std=f2018', language: 'fortran')
  endif
elif fc.get_id() == 'intel'
  add_project_arguments('-stand f18', '-warn all', '-traceback', language: 'fortran')
elif fc.get_id() == 'intel-cl'
  add_project_arguments('/stand:f18', '/traceback', '/warn:all', language: 'fortran')
elif fc.get_id() == 'pgi'
  add_project_arguments('-C', '-Mdclchk', language: 'fortran')
endif
#--- Curses

if os == 'windows'
  curses_path = 'c:/pdcurses'
else
  curses_path = ''
endif

curses = dependency('ncurses', required: false)
if not curses.found() and run_command('python', '-c', 'import os; assert os.path.isdir("' + curses_path + '")').returncode() == 0
  curses = cc.find_library('curses', dirs : curses_path, required : false)
endif
if curses.found() and not fc.links('''
module foo
use, intrinsic:: iso_c_binding
interface
subroutine endwin() bind(C)
end subroutine endwin
end interface
end module foo
program a
use foo
call endwin()
end program''',
dependencies : curses, include_directories: include_directories(curses_path), name: 'Curses OK')
  curses = disabler()
endif

subdir('src')
