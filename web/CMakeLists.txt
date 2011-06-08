
set(NAME ezop.wt)

add_executable(${NAME}
    main.cpp
)

find_library(WT_LIBRARY wt)
find_library(WT_HTTP_LIBRARY wthttp)
find_library(WT_EXT_LIBRARY wtext)

target_link_libraries (${NAME}
          ${WT_LIBRARY}
          ${WT_HTTP_LIBRARY}
          ${WT_EXT_LIBRARY}
)
