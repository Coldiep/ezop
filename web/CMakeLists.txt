
set(NAME ezop.wt)

add_executable(${NAME}
    ezop.cpp
    ezop_widget.cpp
    ezop_menu.cpp
    projects_menu.cpp
    login_menu.cpp
    login.cpp
    registration.cpp
)

find_library(WT_LIBRARY wt)
find_library(WT_HTTP_LIBRARY wthttp)
find_library(WT_EXT_LIBRARY wtext)

find_path(SWI_HEADER_PATH SWI-Prolog.h
         PATHS /usr/lib/swipl-5.11.21/include
               /usr/lib/swipl-5.11.22/include
               /usr/lib/swipl-5.11.25/include
               /usr/lib/swi-prolog/include
)

find_file(SWI_LIBRARY libswipl.a
         PATHS /usr/lib/swipl-5.11.21/lib/
               /usr/lib/swipl-5.11.22/lib/
               /usr/lib/swipl-5.11.25/lib/
               /usr/lib/swi-prolog/lib
         PATH_SUFFIXES amd64 x86_64-linux
)

include_directories(${SWI_HEADER_PATH})

target_link_libraries (${NAME}
          ${WT_LIBRARY}
          ${WT_HTTP_LIBRARY}
          ${WT_EXT_LIBRARY}
          ${SWI_LIBRARY}
          pthread
          dl
          ncurses
          rt
          gmp
          sqlite3
)
