project('tetran', 'c', 'fortran',
  default_options : ['default_library=static'],
  meson_version : '>= 0.50.999')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

#---- libraries
shapes = library('shapes', 'src/shapes.f90', 'src/fields.f90','src/rot90.f90',
  'src/random.f90', 'src/rand_legacy.f90')

AI = library('AI', 'src/ai.f90',
  link_with: shapes)

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
if curses.found()
  cinterc = library('cinterc', 'src/cinter.c', dependencies : curses)

  if os == 'windows'
    cinter_src = files('src/wincinter.f90')
  else
    cinter_src = files('src/unixcinter.f90')
  endif
  cinter = library('cinter', 'src/cinter.f90', cinter_src,
    link_with: [cinterc],
    dependencies : curses)

  errs = library('errs', 'src/err.f90',
    link_with: cinter, dependencies: curses)

  blocks = library('blocks', 'src/blocks.f90',
    link_with: [cinter, shapes])

  menu = library('menu', 'src/menu.f90',
    link_with: [cinter, shapes, blocks])


  keys = library('keys', 'src/keys.f90',
    link_with: [AI, cinter, blocks, shapes])

  #  executables
  tetran = executable('tetran', 'src/main.f90',
    link_with: [cinter, menu, shapes, blocks, keys, errs],
    link_language: 'fortran')
  test('ShortGame', tetran, args : ['-d', '100', '-s', '6', '6'], timeout : 30)

  testkeys_exe = executable('testkeys', 'src/tests/keytest.f90',
    link_with: [errs, cinter, keys],
    link_langauge: 'fortran')

  blockrand = executable('blockrand', 'src/tests/randtest.f90',
    link_with: shapes)
  test('RandomBlocks', blockrand)

  menu_test = executable('menu_test', 'src/tests/test_menu.f90',
    link_with: [cinter, menu],
    link_language: 'fortran')
  test('Menu', menu_test)
endif

shapetest = executable('shapetest', 'src/tests/test_shapes.f90',
  link_with: shapes)
test('BlockShapes', shapetest)

motiontest = executable('motiontest', 'src/tests/test_motion.f90',
  link_with: shapes)
test('BlockMotion', motiontest)
