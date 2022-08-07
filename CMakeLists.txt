cmake_minimum_required(VERSION 3.12...3.25)

project(blocktran
LANGUAGES C CXX Fortran
VERSION 1.7.0
)

enable_testing()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

include(cmake/curses.cmake)

add_executable(blocktran app/main.f90 src/game.f90)
target_link_libraries(blocktran PRIVATE AI cinter menu shapes blocks keys errs sleep random)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
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
