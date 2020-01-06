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

cinter_src = os == 'windows' ? files('wincinter.f90') : files('unixcinter.f90')
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
