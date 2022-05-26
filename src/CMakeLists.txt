add_library(game OBJECT game.f90)
target_link_libraries(game PRIVATE AI cinter menu shapes blocks keys errs sleep random)

add_library(AI OBJECT ai.f90)
target_link_libraries(AI PRIVATE shapes)

if(f18random)
  set(_random_init "call random_init(repeatable, image_distinct)")
else()
  set(_random_init "call random_seed()")
endif()

configure_file(random.in.f90 random.f90 @ONLY)
add_library(random OBJECT ${CMAKE_CURRENT_BINARY_DIR}/random.f90)

add_library(shapes OBJECT shapes.f90 fields.f90 rot90.f90)
target_link_libraries(shapes PRIVATE random)

if(HAVE_UNISTD_H)
  add_library(sleep sleep_unix.f90)
else()
  configure_file(sleep_win.f90.in sleep_win.f90 COPYONLY)
  add_library(sleep OBJECT ${CMAKE_CURRENT_BINARY_DIR}/sleep_win.f90)
endif()

add_library(cinter OBJECT cinter.f90 cinter.c)
target_link_libraries(cinter PRIVATE CURSES::CURSES)

add_library(errs OBJECT err.f90)
target_link_libraries(errs PRIVATE cinter)

add_library(menu OBJECT menu.f90)
target_link_libraries(menu PRIVATE cinter shapes blocks sleep)


add_library(blocks OBJECT blocks.f90)
target_link_libraries(blocks PRIVATE cinter shapes)

add_library(keys OBJECT keys.f90)
target_link_libraries(keys PRIVATE AI cinter blocks)

foreach(t AI shapes blocks cinter errs game keys menu random sleep)
  target_include_directories(${t} PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/include)
  set_target_properties(${t} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()
