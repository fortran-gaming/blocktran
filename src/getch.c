/* PDCurses uses a macro define for getch => wgetch, that is not visible to Fortran */
#include <curses.h>

#undef getch

int getch(void)
{
    return wgetch(stdscr);
}
