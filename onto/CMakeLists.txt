
set(NAME onto)

add_library(${NAME}
    onto.cpp
)

target_link_libraries (${NAME}
          re-lexer
          parser
          terms
)
