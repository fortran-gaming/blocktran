cmake_minimum_required(VERSION 3.12)
project(tetran C Fortran)
enable_testing()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

if(WIN32 AND NOT DEFINED Curses_ROOT)
  set(Curses_ROOT c:/pdcurses)  # be sure you name PDcurses library compiled "curses.a", not "pdcurses.a"
endif()

add_library(rand src/random.f90)
if(f18random)
  target_sources(rand PRIVATE src/rand.f90)
else()
  target_sources(rand PRIVATE src/rand_legacy.f90)
endif()
target_compile_options(rand PRIVATE ${FFLAGS})


add_library(AI src/ai.f90)
target_compile_options(AI PRIVATE ${FFLAGS})
target_link_libraries(AI PRIVATE shapes)


find_package(Curses)
if(CURSES_FOUND)

  add_library(cinterc src/cinter.c)
  target_compile_options(cinterc PRIVATE ${CFLAGS})
  target_include_directories(cinterc PRIVATE ${CURSES_INCLUDE_DIRS})
  target_link_libraries(cinterc PRIVATE ${CURSES_LIBRARIES})

  add_library(cinter src/cinter.f90)
  target_compile_options(cinter PRIVATE ${FFLAGS})
  target_link_libraries(cinter PRIVATE cinterc ${CURSES_LIBRARIES})
  if(WIN32)
    target_sources(cinter PRIVATE src/wincinter.f90)
  else()
    target_sources(cinter PRIVATE src/unixcinter.f90)
  endif()

  add_library(errs src/err.f90)
  target_compile_options(errs PRIVATE ${FFLAGS})
  target_link_libraries(errs PRIVATE cinter ${CURSES_LIBRARIES})

  add_library(menu src/menu.f90)
  target_compile_options(menu PRIVATE ${FFLAGS})
  target_link_libraries(menu PRIVATE rand cinter shapes blocks)


  add_library(blocks src/blocks.f90)
  target_compile_options(blocks PRIVATE ${FFLAGS})
  target_link_libraries(blocks PRIVATE cinter shapes)

  add_library(keys src/keys.f90)
  target_compile_options(keys PRIVATE ${FFLAGS})
  target_link_libraries(keys PRIVATE AI cinter blocks)

#  executables

  add_executable(tetran src/main.f90)
  target_compile_options(tetran PRIVATE ${FFLAGS})
  target_link_libraries(tetran PRIVATE rand cinter menu blocks keys errs)
  add_test(NAME ShortGame COMMAND tetran -d 100 -s 6 6)
  set_tests_properties(ShortGame PROPERTIES TIMEOUT 20
    FIXTURES_REQUIRED CursesF)
  install(TARGETS tetran
          RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})

  add_executable(curses_test src/tests/test_curses.f90)
  target_compile_options(curses_test PRIVATE ${FFLAGS})
  target_link_libraries(curses_test PRIVATE cinter)
  add_test(NAME CursesLib COMMAND curses_test)
  set_tests_properties(CursesLib PROPERTIES
    FIXTURES_SETUP CursesF)

  add_executable(testkeys src/tests/keytest.f90)
  target_compile_options(testkeys PRIVATE ${FFLAGS})
  target_link_libraries(testkeys PRIVATE errs cinter keys)

  add_executable(menu_test src/tests/test_menu.f90)
  target_compile_options(menu_test PRIVATE ${FFLAGS})
  target_link_libraries(menu_test PRIVATE cinter menu)
  add_test(NAME TitleScreen COMMAND menu_test)
  set_tests_properties(TitleScreen PROPERTIES TIMEOUT 10
    FIXTURES_REQUIRED CursesF)
endif()

add_executable(blockrand src/tests/randtest.f90)
target_compile_options(blockrand PRIVATE ${FFLAGS})
target_link_libraries(blockrand PRIVATE rand shapes)
add_test(NAME RandomBlocks COMMAND blockrand)

add_library(shapes src/shapes.f90 src/fields.f90 src/rot90.f90)
target_compile_options(shapes PRIVATE ${FFLAGS})
target_link_libraries(shapes PRIVATE rand)

add_executable(shapetest src/tests/test_shapes.f90)
target_compile_options(shapetest PRIVATE ${FFLAGS})
target_link_libraries(shapetest PRIVATE rand shapes)
add_test(NAME BlockShapes COMMAND shapetest)

add_executable(motiontest src/tests/test_motion.f90)
target_compile_options(motiontest PRIVATE ${FFLAGS})
target_link_libraries(motiontest PRIVATE shapes)
add_test(NAME BlockMotion COMMAND motiontest)
