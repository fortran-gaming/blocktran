if fc.links('call random_init(.false., .false.); end', name: 'F2018 random_init')
  rsrc = 'rand.f90'
else
  rsrc = 'rand_legacy.f90'
endif

shapes = library('shapes', 'shapes.f90', 'fields.f90','rot90.f90',
  'random.f90', rsrc)

AI = library('AI', 'ai.f90',
  link_with: shapes)

cinterc = library('cinterc', 'cinter.c',
  dependencies : curses)

cinter_src = os == 'windows' ? files('wincinter.f90') : cinter_src = files('unixcinter.f90')
cinter = library('cinter', 'cinter.f90', cinter_src,
  link_with: [cinterc],
  dependencies : curses)

errs = library('errs', 'err.f90',
  link_with: cinter, dependencies: curses)

blocks = library('blocks', 'blocks.f90',
  link_with: [cinter, shapes])

menu = library('menu', 'menu.f90',
  link_with: [cinter, shapes, blocks])


keys = library('keys', 'keys.f90',
  link_with: [AI, cinter, blocks, shapes])

#  executables
curses_test = executable('curses_test', 'tests/test_curses.f90',
  link_with: cinter,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'  # PDcurses doesn't work with ctest
  test('CursesLib', curses_test,
    suite: 'gui')
endif

testkeys_exe = executable('testkeys', 'tests/keytest.f90',
  link_with: [errs, cinter, keys],
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04

menu_test = executable('menu_test', 'tests/test_menu.f90',
  link_with: [cinter, menu, blocks, shapes],  # need blocks, shapes for flang
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'
  test('Menu', menu_test,
    timeout: 15,
    suite: 'gui')
endif


blockrand = executable('blockrand', 'tests/randtest.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('RandomBlocks', blockrand,
  timeout: 10,
  suite: 'nogui')

shapetest = executable('shapetest', 'tests/test_shapes.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('BlockShapes', shapetest,
  timeout: 15,
  suite: 'nogui')

motiontest = executable('motiontest', 'tests/test_motion.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('BlockMotion', motiontest,
  timeout: 10,
  suite: 'nogui')
