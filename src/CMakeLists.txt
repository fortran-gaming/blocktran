cmake_minimum_required(VERSION 3.12)
project(tetran Fortran C)
enable_testing()

include(../cmake/compilers.cmake)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()
#---- libraries
add_library(rand random.f90)
if(f2018)
  target_sources(rand PRIVATE rand.f90)
else()
  target_sources(rand PRIVATE rand_legacy.f90)
endif()
target_compile_options(rand PRIVATE ${FFLAGS})


add_library(AI ai.f90)
target_compile_options(AI PRIVATE ${FFLAGS})
target_link_libraries(AI PRIVATE shapes)


find_package(Curses)
if(CURSES_FOUND)

  if(f2008)
    add_library(errs err.f90)
  else()
    add_library(errs err_legacy.f90)
  endif()
  target_compile_options(errs PRIVATE ${FFLAGS})
  target_link_libraries(errs PRIVATE ${CURSES_LIBRARIES} ${FLIBS})

  add_library(cinterc cinter.c)
  target_compile_options(cinterc PRIVATE ${CFLAGS})
  target_include_directories(cinterc PRIVATE ${CURSES_INCLUDE_DIRS})
  target_link_libraries(cinterc PRIVATE errs ${CURSES_LIBRARIES})

  add_library(cinter cinter.f90)
  target_compile_options(cinter PRIVATE ${FFLAGS})
  target_link_libraries(cinter PRIVATE cinterc ${CURSES_LIBRARIES} ${FLIBS})
  
  add_library(menu menu.f90)
  target_compile_options(menu PRIVATE ${FFLAGS})
  target_link_libraries(menu PRIVATE rand cinter shapes blocks)

 
  add_library(blocks blocks.f90)
  target_compile_options(blocks PRIVATE ${FFLAGS})
  target_link_libraries(blocks PRIVATE errs cinter shapes)

  add_library(keys keys.f90)
  target_compile_options(keys PRIVATE ${FFLAGS})
  target_link_libraries(keys PRIVATE AI cinter blocks)

#  executables

  add_executable(tetran main.f90)
  target_compile_options(tetran PRIVATE ${FFLAGS})
  target_link_libraries(tetran PRIVATE rand cinter menu blocks keys errs)
  add_test(NAME ShortGame COMMAND tetran -d 100 -s 6 6)
  set_tests_properties(ShortGame PROPERTIES TIMEOUT 20)
  install(TARGETS tetran
          RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})

  add_executable(testkeys tests/keytest.f90)
  target_compile_options(testkeys PRIVATE ${FFLAGS})
  target_link_libraries(testkeys PRIVATE errs cinter keys)
  
  add_executable(blockrand tests/randtest.f90)
  target_compile_options(blockrand PRIVATE ${FFLAGS})
  target_link_libraries(blockrand PRIVATE rand errs blocks)
  add_test(NAME RandomBlocks COMMAND blockrand)
  
  add_executable(menu_test tests/test_menu.f90)
  target_compile_options(menu_test PRIVATE ${FFLAGS})
  target_link_libraries(menu_test PRIVATE errs cinter menu)
  add_test(NAME Menu COMMAND menu_test)
endif()

add_library(rotflip rot90.f90)
target_compile_options(rotflip PRIVATE ${FFLAGS})

add_library(shapes block_init.f90)
target_compile_options(shapes PRIVATE ${FFLAGS})
target_link_libraries(shapes PRIVATE rand rotflip)

add_executable(shapetest tests/test_shapes.f90)
target_compile_options(shapetest PRIVATE ${FFLAGS})
target_link_libraries(shapetest PRIVATE rand shapes ${FLIBS})
add_test(NAME BlockShapes COMMAND shapetest)

add_executable(motiontest tests/test_motion.f90)
target_compile_options(motiontest PRIVATE ${FFLAGS})
target_link_libraries(motiontest PRIVATE shapes ${FLIBS})
add_test(NAME BlockMotion COMMAND motiontest)


