# extra parts of X11 needed
find_package(X11 REQUIRED)

find_package(PkgConfig)


if(PKG_CONFIG_FOUND)
pkg_check_modules(_x11 x11)
pkg_check_modules(_xt xt)
pkg_check_modules(_xmu xmu)
pkg_check_modules(_xpm xpm)
pkg_check_modules(_xaw REQUIRED xaw7)
endif()

find_path(_xt_inc
NAMES Intrinsic.h
HINTS ${_xt_INCLUDE_DIRS}
PATHS /usr/include
PATH_SUFFIXES X11
REQUIRED
)

find_path(_xatom_inc
NAMES Xatom.h
HINTS ${_x11_INCLUDE_DIRS}
PATH_SUFFIXES X11
REQUIRED
)

find_path(_xmu_inc
NAMES StdSel.h
HINTS ${_xmu_INCLUDE_DIRS}
PATH_SUFFIXES X11/Xmu
REQUIRED
)


find_path(_xpm_inc
NAMES xpm.h
HINTS ${_xpm_INCLUDE_DIRS}
PATH_SUFFIXES X11
REQUIRED
)


find_path(_xaw_inc
NAMES Box.h
HINTS ${_xaw_INCLUDE_DIRS}
PATH_SUFFIXES X11/Xaw
REQUIRED
)

list(APPEND X11_INCLUDE_DIR
${_xt_inc} ${_xatom_inc}
${_xmu_inc} ${_xmu_inc}/..
${_xpm_inc}
${_xaw_inc} ${_xaw_inc}/..
)

# X11::Xaw doesn't exist in FindX11 even with latest CMake
find_library(Xaw NAMES Xaw7
HINTS ${_xaw_LIBRARY_DIRS}
REQUIRED
)

link_directories(${_xaw_LIBRARY_DIRS})
list(APPEND X11_LIBRARIES ${Xaw})

message(STATUS "X11_INCLUDE_DIR: ${X11_INCLUDE_DIR}
X11_LIBRARIES: ${X11_LIBRARIES}")
