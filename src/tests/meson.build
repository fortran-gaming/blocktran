curses_test = executable('curses_test', 'test_curses.f90',
  link_with: cinter,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'  # PDcurses doesn't work with ctest
  test('CursesLib', curses_test,
    suite: 'gui')
endif

testkeys_exe = executable('testkeys', 'keytest.f90',
  link_with: [errs, cinter, keys],
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04

menu_test = executable('menu_test', 'test_menu.f90',
  link_with: [cinter, menu, blocks, shapes],  # need blocks, shapes for flang
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'
  test('Menu', menu_test,
    timeout: 15,
    suite: 'gui')
endif


blockrand = executable('blockrand', 'randtest.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('RandomBlocks', blockrand,
  timeout: 10,
  suite: 'nogui')

shapetest = executable('shapetest', 'test_shapes.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('BlockShapes', shapetest,
  timeout: 15,
  suite: 'nogui')

motiontest = executable('motiontest', 'test_motion.f90',
  link_with: shapes,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
test('BlockMotion', motiontest,
  timeout: 10,
  suite: 'nogui')