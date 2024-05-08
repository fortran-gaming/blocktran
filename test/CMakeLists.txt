add_compile_options("${compile_opts}")

add_executable(blockrand randtest.f90)
target_link_libraries(blockrand PRIVATE shapes random)
add_test(NAME random COMMAND blockrand)

# blocks:{random,motion,shapes} fail if run simultaneously with gui
# and gui tests fail if run in parallel
add_executable(shapetest test_shapes.f90)
target_link_libraries(shapetest PRIVATE shapes random)
add_test(NAME shapes COMMAND shapetest)

add_executable(motiontest test_motion.f90)
target_link_libraries(motiontest PRIVATE shapes random)
add_test(NAME motion COMMAND motiontest)

set_property(TEST random shapes motion PROPERTY LABELS block)

add_executable(testkeys keytest.f90)
target_link_libraries(testkeys PRIVATE errs cinter keys shapes blocks AI sleep random)

add_executable(curses_test test_curses.f90)
target_link_libraries(curses_test PRIVATE cinter sleep)
add_test(NAME cursesLib COMMAND curses_test)

add_executable(menu_test test_menu.f90)
target_link_libraries(menu_test PRIVATE cinter menu shapes blocks random sleep)
add_test(NAME title_screen COMMAND menu_test)

# props
set_property(TARGET curses_test menu_test testkeys PROPERTY LINKER_LANGUAGE Fortran)

set_tests_properties(cursesLib title_screen PROPERTIES
LABELS gui
DISABLED $<BOOL:$ENV{CI}>
RESOURCE_LOCK display
DEPENDS "shapes;motion;random"
)
