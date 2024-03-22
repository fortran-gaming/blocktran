option(${PROJECT_NAME}_BUILD_TESTING "enable testing" ${PROJECT_IS_TOP_LEVEL})

option(find "find Ncurses or if not, build PDcurses" true)

option(CMAKE_TLS_VERIFY "enable TLS verification" true)

set(CI $ENV{CI})
