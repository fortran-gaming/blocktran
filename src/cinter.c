#include <curses.h>

void macro_getmaxyx(WINDOW *win, int *y, int *x){
   getmaxyx(win,*y,*x);
}
