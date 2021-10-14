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

set_tests_properties(random shapes motion PROPERTIES
  TIMEOUT 10
  LABELS block
)

add_executable(testkeys keytest.f90)
target_link_libraries(testkeys PRIVATE errs cinter cinterc keys shapes blocks AI sleep random)

add_executable(curses_test test_curses.f90)
target_link_libraries(curses_test PRIVATE cinter cinterc sleep)
add_test(NAME cursesLib COMMAND curses_test)
set_tests_properties(cursesLib PROPERTIES
  RESOURCE_LOCK display
  DEPENDS "shapes;motion;random"
  LABELS gui
  DISABLED $<BOOL:${CI}>
  TIMEOUT 10)

add_executable(menu_test test_menu.f90)
target_link_libraries(menu_test PRIVATE cinter cinterc menu shapes blocks random)
add_test(NAME title_screen COMMAND menu_test)
set_tests_properties(title_screen PROPERTIES
  TIMEOUT 10
  LABELS gui
  DISABLED $<BOOL:${CI}>
  RESOURCE_LOCK display
  DEPENDS "shapes;motion;random")
