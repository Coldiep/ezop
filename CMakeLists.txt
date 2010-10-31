
cmake_minimum_required(VERSION 2.6)

project(EarleyParser CXX)

add_subdirectory(parser)
add_subdirectory(terms)

include_directories(${PROJECT_SOURCE_DIR}/parser ${PROJECT_SOURCE_DIR}/terms)

set(NAME ezop)

add_executable(${NAME}
    main.cpp
)

target_link_libraries (${NAME}
          parser
          terms
)
