include(FetchContent)

FetchContent_Declare(pdcurses_proj
  GIT_REPOSITORY https://github.com/scivision/PDcurses.git
  GIT_TAG 4eb2e16e2d46a77fa7f27a9b4178175782606ff0
)

FetchContent_MakeAvailable(pdcurses_proj)
