
set (NAME parser)

add_library(${NAME}
          grammar.cpp
          public_grammar.cpp
          earley_parser.cpp
          dfa.cpp
          exp_tree.cpp
          lexer.cpp
          scanner.cpp
          token.cpp
)
