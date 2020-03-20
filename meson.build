project('tetran', 'c', 'fortran',
  default_options : ['default_library=static', 'buildtype=release', 'warning_level=3'],
  version: '1.3.2',
  meson_version : '>= 0.51')

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
os = host_machine.system()

subdir('cmake')

subdir('src')

subdir('src/tests')

tetran = executable('tetran', 'src/main.f90',
  link_with: [cinter, menu, shapes, blocks, keys, AI, errs],  # need AI for fla$
  install: true,
  link_language: 'fortran')  # link_language needed for e.g. Ubuntu 16.04
if os != 'windows'
  test('ShortGame', tetran, args : ['-d', '100', '-s', '6', '6'],
    timeout : 30,
    suite: 'gui')
endif
