add_library(AI ai.f90)
target_compile_options(AI PRIVATE ${FFLAGS})
target_link_libraries(AI PRIVATE shapes)

add_library(shapes shapes.f90 fields.f90 rot90.f90 random.f90)
target_compile_options(shapes PRIVATE ${FFLAGS})
if(f18random)
  target_sources(shapes PRIVATE rand.f90)
else()
  target_sources(shapes PRIVATE rand_legacy.f90)
endif()

if(CURSES_FOUND)
  add_library(cinterc cinter.c)
  target_compile_options(cinterc PRIVATE ${CFLAGS})
  target_include_directories(cinterc PRIVATE ${CURSES_INCLUDE_DIRS})
  target_link_libraries(cinterc PRIVATE ${CURSES_LIBRARIES})

  add_library(cinter cinter.f90)
  target_compile_options(cinter PRIVATE ${FFLAGS})
  target_link_libraries(cinter PRIVATE cinterc ${CURSES_LIBRARIES})
  if(WIN32)
    target_sources(cinter PRIVATE wincinter.f90)
  else()
    target_sources(cinter PRIVATE unixcinter.f90)
  endif()

  add_library(errs err.f90)
  target_compile_options(errs PRIVATE ${FFLAGS})
  target_link_libraries(errs PRIVATE cinter ${CURSES_LIBRARIES})

  add_library(menu menu.f90)
  target_compile_options(menu PRIVATE ${FFLAGS})
  target_link_libraries(menu PRIVATE cinter shapes blocks)


  add_library(blocks blocks.f90)
  target_compile_options(blocks PRIVATE ${FFLAGS})
  target_link_libraries(blocks PRIVATE cinter shapes)

  add_library(keys keys.f90)
  target_compile_options(keys PRIVATE ${FFLAGS})
  target_link_libraries(keys PRIVATE AI cinter blocks)
endif()
