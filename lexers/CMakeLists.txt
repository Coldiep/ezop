
set (NAME lexers)

add_library(${NAME}
          exp_tree.cpp
          scanner.cpp
          dfa.cpp
          open_token.cpp
          open_lexer.cpp
)
