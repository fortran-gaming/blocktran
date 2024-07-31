cmake_minimum_required(VERSION 3.19...3.28)

project(blocktran
LANGUAGES C CXX Fortran
VERSION 1.7.0
)

enable_testing()

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

option(blocktran_BUILD_TESTING "enable testing" ${blocktran_IS_TOP_LEVEL})
option(find "find Ncurses or if not, build PDcurses" true)

include(cmake/compilers.cmake)

include(cmake/curses.cmake)

message(STATUS "${PROJECT_NAME} CMake ${CMAKE_VERSION} install prefix: ${CMAKE_INSTALL_PREFIX}")

add_executable(blocktran app/main.f90 src/game.f90)
target_link_libraries(blocktran PRIVATE AI cinter menu shapes blocks keys errs sleep random)
target_compile_options(blocktran PRIVATE ${compile_opts})
set_property(TARGET blocktran PROPERTY LINKER_LANGUAGE Fortran)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

install(TARGETS blocktran EXPORT ${PROJECT_NAME}-targets)

include(cmake/install.cmake)
