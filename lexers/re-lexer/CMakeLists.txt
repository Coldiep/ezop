
set (NAME re-lexer)

add_library(${NAME}
          exp_tree.cpp
          scanner.cpp
          dfa.cpp
          re-token.cpp
          re-lexer.cpp
)
