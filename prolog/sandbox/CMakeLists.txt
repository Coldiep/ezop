
set(NAME swi-sandbox)

add_executable(${NAME}
    main.cpp
)

#find_library(SWI_LIBRARY swipl)
add_definitions(/usr/lib64/swipl-5.11.21/include)

target_link_libraries (${NAME}
          /usr/lib64/swipl-5.11.21/lib/amd64/libswipl.a
          pthread
          dl
          ncurses
          rt
)
