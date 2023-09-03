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

message(STATUS "${PROJECT_NAME} CMake ${CMAKE_VERSION} install prefix: ${CMAKE_INSTALL_PREFIX}")

if(CMAKE_GENERATOR MATCHES "Ninja")
  if(WIN32)
    set(_rec "MinGW Makefiles")
  else()
    set(_rec "Unix Makefiles")
  endif()
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.27.0 AND CMAKE_VERSION VERSION_LESS 3.27.5)
    message(WARNING "CMake ${CMAKE_VERSION} has a bug that prevents Ninja from working properly. Please use Makefile generator:
    cmake -G \"${_rec}\"")
  endif()
endif()

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
