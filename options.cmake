option(${PROJECT_NAME}_BUILD_TESTING "enable testing" true)
option(find "find Ncurses or if not, build PDcurses" true)

set(CI $ENV{CI})

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "default install path" FORCE)
endif()
