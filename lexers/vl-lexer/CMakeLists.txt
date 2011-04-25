
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
)

set(NAME vl-lexer)

add_library(${NAME}
    lex.cpp
    lex_type.cpp
    rex/dfa.cpp
    rex/dfa_check.cpp
    rex/expressions_tree.cpp
    rex/minimize.cpp
    rex/nfa2dfa_transformer.cpp
    rex/nfa.cpp
    rex/parser.cpp
    rex/scanner.cpp
)

add_subdirectory(sandbox)
