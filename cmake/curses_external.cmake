include(FetchContent)

FetchContent_Declare(pdcurses_proj
  GIT_REPOSITORY https://github.com/scivision/PDcurses.git
  GIT_TAG 4eb2e16e
)

FetchContent_MakeAvailable(pdcurses_proj)
