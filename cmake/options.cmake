message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION}  CMake ${CMAKE_VERSION}")

option(${PROJECT_NAME}_BUILD_TESTING "enable testing" true)
option(find "find Ncurses or if not, build PDcurses" true)

set(CI $ENV{CI})

include(GNUInstallDirs)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "default install path" FORCE)
endif()

file(GENERATE OUTPUT .gitignore CONTENT "*")
