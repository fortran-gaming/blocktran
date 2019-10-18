project('tetran', 'c', 'fortran',
  default_options : ['default_library=static', 'buildtype=release'],
  version: '1.2.0',
  meson_version : '>= 0.51')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

f18flag = fc.first_supported_argument(['-std=f2018', '-stand f18', '/stand:f18'])
if fc.get_id() == 'gcc'
  add_project_arguments('-march=native', '-Wall', '-Wextra', '-Wpedantic',
    '-Werror=array-bounds', '-finit-real=nan', '-Wconversion', '-fimplicit-none', f18flag,
    language: 'fortran')

elif fc.get_id() == 'intel'
  add_project_arguments(f18flag, '-warn all', '-traceback', language: 'fortran')
elif fc.get_id() == 'intel-cl'
  add_project_arguments(f18flag, '/traceback', '/warn:all', language: 'fortran')
elif fc.get_id() == 'pgi'
  add_project_arguments('-C', '-Mdclchk', language: 'fortran')
endif
#--- Curses

curses_root = get_option('curses_root')
if curses_root == ''
  curses_root = []
endif

curses = dependency('curses', required: false)
if not curses.found()
  curses = cc.find_library('curses', dirs : curses_root, required : false, disabler: true)
endif
if not fc.links('''
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
dependencies : curses, include_directories: include_directories(curses_root), name: 'Curses')
  curses = disabler()
endif

subdir('src')

tetran = executable('tetran', 'src/main.f90',
  link_with: [cinter, menu, shapes, blocks, keys, AI, errs],  # need AI for fla$
  install: true,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'
  test('ShortGame', tetran, args : ['-d', '100', '-s', '6', '6'],
    timeout : 30,
    suite: 'gui')
endif
