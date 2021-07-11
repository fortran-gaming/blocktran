
#--- Curses

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

foreach k : ['curses', 'ncurses', 'ncursesw']
  curses = dependency(k, required: false, disabler: true)
  if curses.found()
    break
  endif
endforeach

if not fc.links(code, dependencies : curses, name: 'Curses')
  curses = disabler()
endif
if not curses.found()
  curses_proj = subproject('pdcurses')
  curses = declare_dependency(link_with: curses_proj.get_variable('curses_lib'), include_directories: curses_proj.get_variable('incdir'))
endif
