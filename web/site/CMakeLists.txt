
set(NAME ezop-site.wt)

add_executable(${NAME}
    site.cpp
    model/user.cpp
    model/project.cpp
    model/ontology.cpp
    view/morda.cpp
    site_session.cpp
)

find_library(WT_LIBRARY wt)
find_library(WT_HTTP_LIBRARY wthttp)
find_library(WT_EXT_LIBRARY wtext)
find_library(WT_DBO_LIBRARY wtdbo)
find_library(WT_DBO_SQLITE_LIBRARY wtdbosqlite3)

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
          ${WT_DBO_LIBRARY}
          ${WT_DBO_SQLITE_LIBRARY}
          ${SWI_LIBRARY}
          pthread
          dl
          ncurses
          rt
          gmp
          sqlite3
)
