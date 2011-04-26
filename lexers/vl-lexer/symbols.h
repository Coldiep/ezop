
#include <stdint.h>

namespace lexer {

inline char GetCp1251FromUtf16(uint16_t code) {
  if (code >= 0x0410 and code <= 0x042F) { // Прописные для кирилицы.
    return code - 0x0410 + 0xc0;
  } else if (code >= 0x0430 and code <= 0x044F) { // Строчные для кирилицы.
    return code - 0x0430 + 0xe0;
  } else if (code == 0x0401) { // Прописная Ё.
    return '\xa8';
  } else if (code == 0x0451) { // Строчная ё.
    return '\xb8';
  } else if (code < 0x80) { // ANSI символы.
    return code;
  }
  return '\0';
}

//! Преобразование UTF-16 кода в UTF-8 последовательность.
inline void GetUtf8Sequence(uint16_t code, std::string& seq) {
    const uint16_t kByteMask       = 0xbfu;
    const uint16_t kByteMark       = 0x80u;
    const uint16_t kTwoByteMark    = 0xc0u;
    const uint16_t kThreeByteMark  = 0xe0u;

    if (code < 0x80u) {
        seq += code;
    } else if (code < 0x800u) {
        char sbyte = (code | kByteMark) & kByteMask;
        code >>= 6;
        char fbyte = code | kTwoByteMark;

        seq += fbyte;
        seq += sbyte;
    } else {
        char tbyte = (code | kByteMark) & kByteMask;
        code >>= 6;
        char sbyte = (code | kByteMark) & kByteMask;
        code >>= 6;
        char fbyte = (code | kThreeByteMark);

        seq += fbyte;
        seq += sbyte;
        seq += tbyte;
    }
}

}  // namespace lexer

