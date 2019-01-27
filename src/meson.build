project('tetran', 'c', 'fortran', default_options : ['default_library=static'])

#---- libraries
shapes= library('shapes', 'shapes.f90', 'fields.f90','rot90.f90',
  'random.f90', 'rand_legacy.f90')
  
AI = library('AI', 'ai.f90',
  link_with: shapes)

curses = dependency('ncurses')
if curses.found()
  cinterc = library('cinterc', 'cinter.c', dependencies : curses)
  
  cinter= library('cinter', 'cinter.f90',
    link_with: [cinterc],
    dependencies : curses)
    
  errs = library('errs', 'err.F90',
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
  test('ShortGame', tetran, args : ['-d', '100', '-s', '6', '6'], timeout : 30)
  
  testkeys_exe = executable('testkeys', 'tests/keytest.f90',
    link_with: [errs, cinter, keys])
    
  blockrand = executable('blockrand', 'tests/randtest.f90',
    link_with: shapes)
  test('RandomBlocks', blockrand)
  
  menu_test = executable('menu_test', 'tests/test_menu.f90',
    link_with: [cinter, menu])
  test('Menu', menu_test)
endif


  
shapetest = executable('shapetest', 'tests/test_shapes.f90',
  link_with: shapes)
test('BlockShapes', shapetest)

motiontest = executable('motiontest', 'tests/test_motion.f90',
  link_with: shapes)
test('BlockMotion', motiontest)
