
set(NAME lexer-sandbox)

add_executable(${NAME}
    main.cpp
)

target_link_libraries (${NAME}
          vl-lexer
)
