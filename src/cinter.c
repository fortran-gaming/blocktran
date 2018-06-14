#include <ncurses.h>
#include <stdio.h>

void macro_getmaxyx(WINDOW *win, int *y, int *x){
   getmaxyx(win,*y,*x);
}
