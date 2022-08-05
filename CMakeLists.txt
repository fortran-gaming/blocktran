cmake_minimum_required(VERSION 3.14...3.25)

project(blocktran
LANGUAGES C CXX Fortran
VERSION 1.7.0
DESCRIPTION "Falling-block game in Fortran + Curses"
HOMEPAGE_URL https://www.github.com/fortran-gaming/blocktran
)

enable_testing()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

include(cmake/curses.cmake)

add_executable(blocktran app/main.f90)
target_link_libraries(blocktran PRIVATE game AI cinter menu shapes blocks keys errs sleep random)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

if(NOT BUILD_SHARED_LIBS)
  # static flags help avoid users needing libgfortran etc. on their Windows system
  # True static linking is an advanced topic.
  target_link_options(blocktran PRIVATE "$<$<AND:$<BOOL:${MINGW}>,$<Fortran_COMPILER_ID:GNU>>:-static;-static-libgfortran>")
endif()

install(TARGETS blocktran EXPORT ${PROJECT_NAME}-targets
LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

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
