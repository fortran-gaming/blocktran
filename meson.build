project('blocktran', 'c', 'fortran',
  default_options : ['default_library=static', 'buildtype=release', 'warning_level=3'],
  version: '1.5.2',
  meson_version : '>= 0.51')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

subdir('cmake')

subdir('src')

subdir('src/tests')

blocktran_exe = executable('blocktran',
  sources: ['src/main.f90', 'src/game.f90'],
  link_with: [cinter, menu, shapes, blocks, keys, AI, errs, sleep],  # need AI for fla$
  install: true)

if os != 'windows'
  test('ShortGame', blocktran_exe, args : ['-d', '100', '-s', '6', '6'],
    timeout : 30,
    is_parallel: false,
    suite: 'gui')
endif
