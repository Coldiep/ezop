
set (NAME parser)

add_library(${NAME}
          grammar.cpp
          public_grammar.cpp
          earley_parser.cpp
)
