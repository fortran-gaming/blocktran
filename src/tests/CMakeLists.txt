
add_executable(blockrand randtest.f90)
target_link_libraries(blockrand PRIVATE shapes)
add_test(NAME block:random COMMAND blockrand)

# blocks:{motion,shapes} fail if run simultaneously with gui
# and gui tests fail if run in parallel
add_executable(shapetest test_shapes.f90)
target_link_libraries(shapetest PRIVATE shapes)
add_test(NAME block:shapes COMMAND shapetest)

add_executable(motiontest test_motion.f90)
target_link_libraries(motiontest PRIVATE shapes)
add_test(NAME block:motion COMMAND motiontest)


add_executable(curses_test test_curses.f90)
target_link_libraries(curses_test PRIVATE cinter cinterc sleep)
if(NOT CI)
  add_test(NAME gui:cursesLib COMMAND curses_test)
  set_tests_properties(gui:cursesLib PROPERTIES RESOURCE_LOCK display)
endif()

add_executable(testkeys keytest.f90)
target_link_libraries(testkeys PRIVATE errs cinter cinterc keys shapes blocks AI sleep)

add_executable(menu_test test_menu.f90)
target_link_libraries(menu_test PRIVATE cinter cinterc menu shapes blocks)
if(NOT CI)
  add_test(NAME gui:title_screen COMMAND menu_test)
  set_tests_properties(gui:title_screen PROPERTIES TIMEOUT 10)
  set_tests_properties(gui:title_screen PROPERTIES RESOURCE_LOCK display)
endif()
