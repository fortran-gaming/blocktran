conf_data = configuration_data()
if fc.links('call random_init(.false., .false.); end', name: 'F2018 random_init')
  conf_data.set('_random_init', 'call random_init(repeatable, image_distinct)')
else
  conf_data.set('_random_init', 'call random_seed()')
endif
rand = configure_file(input : 'random.in.f90',
output : 'random.f90',
configuration : conf_data)

shapes = library('shapes', 'shapes.f90', 'fields.f90','rot90.f90', rand)

AI = library('AI', 'ai.f90',
  link_with: shapes)

cinterc = library('cinterc', 'cinter.c',
  dependencies : curses)

cinter_src = os == 'windows' ? files('wincinter.f90') : files('unixcinter.f90')
cinter = library('cinter', 'cinter.f90', cinter_src,
  link_with: [cinterc],
  dependencies : curses)

sleep = library('sleep', 'sleep_std.F90')

errs = library('errs', 'err.f90',
  link_with: cinter, dependencies: curses)

blocks = library('blocks', 'blocks.f90',
  link_with: [cinter, shapes])

menu = library('menu', 'menu.f90',
  link_with: [cinter, shapes, blocks, sleep])


keys = library('keys', 'keys.f90',
  link_with: [AI, cinter, blocks, shapes])
