f18flag = fc.first_supported_argument(['-std=f2018', '-stand f18', '/stand:f18'])
if fc.get_id() == 'gcc'
  add_project_arguments('-Werror=array-bounds', f18flag, language: 'fortran')
elif fc.get_id() == 'intel'
  add_project_arguments(f18flag, language: 'fortran')
elif fc.get_id() == 'intel-cl'
  add_project_arguments(f18flag, language: 'fortran')
endif
#--- Curses

curses_root = get_option('curses_root')
if curses_root == ''
  curses_root = []
endif

code = '''
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
end program'''
# dependency needs to be "ncurses", while find_library is "curses"
curses = dependency('ncurses', required: false, disabler: true)
if curses.found() and not fc.links(code, dependencies : curses, name: 'Curses')
  curses = disabler()
endif
if not curses.found()
  curses_lib = cc.find_library('curses', dirs : curses_root, required : false, disabler: true)
  curses = declare_dependency(include_directories: curses_root, dependencies: curses_lib)
  if curses.found() and not fc.links(code, dependencies : curses_lib, name: 'Curses')
    curses = disabler()
  endif
endif
if not curses.found()
  curses_proj = subproject('pdcurses')
  curses = declare_dependency(link_with: curses_proj.get_variable('curses_lib'), include_directories: curses_proj.get_variable('incdir'))
endif