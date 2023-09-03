cmake_minimum_required(VERSION 3.19...3.27)

project(blocktran
LANGUAGES C CXX Fortran
VERSION 1.7.0
)

enable_testing()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

include(cmake/options.cmake)
include(cmake/compilers.cmake)

include(cmake/curses.cmake)

message(STATUS "${PROJECT_NAME} install prefix: ${CMAKE_INSTALL_PREFIX}")

add_executable(blocktran app/main.f90 src/game.f90)
target_link_libraries(blocktran PRIVATE AI cinter menu shapes blocks keys errs sleep random)
set_property(TARGET blocktran PROPERTY LINKER_LANGUAGE Fortran)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

install(TARGETS blocktran EXPORT ${PROJECT_NAME}-targets)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_test(NAME game COMMAND blocktran -d 100 -s 6 6)
  set_tests_properties(game PROPERTIES
  LABELS gui
  DISABLED $<BOOL:${CI}>
  TIMEOUT 30
  RESOURCE_LOCK display
  DEPENDS "shapes;motion;random"
  )
endif()

include(cmake/install.cmake)
