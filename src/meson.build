if fc.run('call random_init(.false., .false.); end').returncode() == 0
  rsrc = 'rand.f90'
else
  rsrc = 'rand_legacy.f90'
endif

shapes = library('shapes', 'shapes.f90', 'fields.f90','rot90.f90',
  'random.f90', rsrc)

AI = library('AI', 'ai.f90',
  link_with: shapes)


if curses.found()
  cinterc = library('cinterc', 'cinter.c', dependencies : curses)

  if os == 'windows'
    cinter_src = files('wincinter.f90')
  else
    cinter_src = files('unixcinter.f90')
  endif
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
  tetran = executable('tetran', 'main.f90',
    link_with: [cinter, menu, shapes, blocks, keys, errs])
  if os != 'windows'
    test('ShortGame', tetran, args : ['-d', '100', '-s', '6', '6'], timeout : 30)
  endif

  curses_test = executable('curses_test', 'tests/test_curses.f90',
    link_with: cinter)
  if os != 'windows'  # PDcurses doesn't work with ctest
    test('CursesLib', curses_test)
  endif

  testkeys_exe = executable('testkeys', 'tests/keytest.f90',
    link_with: [errs, cinter, keys])

  menu_test = executable('menu_test', 'tests/test_menu.f90',
    link_with: [cinter, menu])
  if os != 'windows'
    test('Menu', menu_test, timeout: 15)
  endif
endif

blockrand = executable('blockrand', 'tests/randtest.f90',
    link_with: shapes)
  test('RandomBlocks', blockrand, timeout: 10)

shapetest = executable('shapetest', 'tests/test_shapes.f90',
  link_with: shapes)
test('BlockShapes', shapetest, timeout: 15)

motiontest = executable('motiontest', 'tests/test_motion.f90',
  link_with: shapes)
test('BlockMotion', motiontest, timeout: 10)