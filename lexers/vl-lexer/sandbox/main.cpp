
#include <lex.h>

int main() {
  try {
    lexer::LexType lex_type(1, "[1-9][0-9]*", "Integer", true);
  } catch(const std::exception& err) {
    std::cerr << err.what() << "\n";
  }

  return 0;
}

