
set(NAME ezop.wt)

add_executable(${NAME}
    main.cpp
)

find_library(WT_LIBRARY wt)
find_library(WT_HTTP_LIBRARY wthttp)

target_link_libraries (${NAME}
          ${WT_LIBRARY}
          ${WT_HTTP_LIBRARY}
)
