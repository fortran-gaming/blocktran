add_library(AI OBJECT ai.f90)
target_link_libraries(AI PRIVATE shapes)

add_library(random OBJECT random.f90)

add_library(shapes OBJECT shapes.f90 fields.f90 rot90.f90)
target_link_libraries(shapes PRIVATE random)

add_library(sleep OBJECT sleep.cpp sleep.f90)
target_compile_features(sleep PRIVATE cxx_std_11)

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

set(tgt AI shapes blocks cinter errs keys menu random sleep)
set_property(TARGET ${tgt} PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET ${tgt} PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}/include)
