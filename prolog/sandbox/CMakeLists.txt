
set(NAME swi-sandbox)

add_executable(${NAME}
    main.cpp
)

find_path(SWI_HEADER_PATH SWI-Prolog.h
         PATHS /usr/lib/swipl-5.11.21/include /usr/lib/swipl-5.11.22/include
)

find_file(SWI_LIBRARY libswipl.a
         PATHS /usr/lib/swipl-5.11.21/lib/ /usr/lib/swipl-5.11.22/lib/
         PATH_SUFFIXES amd64 x86_64-linux
)

include_directories(${SWI_HEADER_PATH})
message("path: ${SWI_LIBRARY}")

target_link_libraries (${NAME}
          ${SWI_LIBRARY}
          pthread
          dl
          ncurses
          rt
)
