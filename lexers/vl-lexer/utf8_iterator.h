
#pragma once

#include <symbols.h>

#include <boost/iterator/iterator_facade.hpp>
#include <stdint.h>
#include <cstring>
#include <algorithm>

namespace lexer {

/*!
 * \brief Описание UTF-8 символа из входного потока.
 */
struct Utf8Symbol {
  char      chain_[6];  //!< UTF-8 последовательность.
  unsigned  len_;       //!< Длина UTF-8 последовательности.
  char      cp1251_;    //!< CP1251 значение символа (только для английского и русского языков).
  uint16_t  utf16_;     //!< UTF-16 код символа.

  //! Инициалиизация UTF-16 значения нулем.
  Utf8Symbol()
    : len_(0)
    , cp1251_('\0')
    , utf16_(0) {
  }

  //! Конструктор копирования.
  Utf8Symbol(const Utf8Symbol& other)
    : len_(other.len_)
    , cp1251_(other.cp1251_)
    , utf16_(other.utf16_) {
    std::copy(other.chain_, other.chain_ + other.len_, chain_);
  }

  //! Оператор сравнения сравнивает UTF-8 последователности.
  bool operator==(const Utf8Symbol& other) const {
    return std::memcmp(chain_, other.chain_, len_) == 0;
  }

  //! Оператор присваивания.
  Utf8Symbol& operator=(const Utf8Symbol& other) {
    if (this != &other) {
      len_ = other.len_;
      cp1251_ = other.cp1251_;
      utf16_ = other.utf16_;
      std::copy(other.chain_, other.chain_ + other.len_, chain_);
    }
    return *this;
  }
};

/*!
 * \brief Класс, реализующий forward итератор по UTF-8 потоку.
 *
 * Класс реализован с помощью boost::iterator_facade, предоставляющим возможность легко
 * определить класс итератора, переопределяя всего три метода. Значение итератора -- это
 * объект класса Utf8Symbol, содержащего UTF-8 последовательность байтов и UTF-16 значение
 * этой последовательности. Итератор производит перекодировку UTF-8 последовательностей в
 * UTF-16 значения. Если обнаружена некорректная последовательность, то возвращается
 * нулевой значение и счетчик символов не увеличивается.
 *
 * В качестве параметра шаблона передается байтовый итератор.
 */
template <typename T>
class Utf8Iterator : public  boost::iterator_facade<Utf8Iterator<T>, Utf8Symbol, boost::forward_traversal_tag> {
  //! Тип байтового итератора.
  typedef T ByteIterator;

  // Объявление для iterator_facade.
  friend class boost::iterator_core_access;

  //!  Достигнут ли конец потока.
  inline bool CheckEnd() const {
    return iter_ == end_;
  }

  //! Получение следующего байта из потока.
  inline bool Next() {
    if (iter_ != end_ and ++iter_ != end_) {
      ++byte_pos_;
      return true;
    }

    return false;
  }

  /*
   * \brief Утилитная процедура определения того, является ли UTF-8 последовательность корректной. 
   */
  static inline bool IsLegalUtf8(const uint8_t* source, unsigned int len) {
    uint8_t a;
    const uint8_t* srcptr = source + len;
    switch (len) {
      default:
        return false;
      // Проверка значения последнего байта.
      case 4: if ((a = (*--srcptr)) < 0x80 or a > 0xBF) return false;
      case 3: if ((a = (*--srcptr)) < 0x80 or a > 0xBF) return false;
      case 2: if ((a = (*--srcptr)) > 0xBF) return false;

      switch (*source) {
        // Проверка соответствия значений первого и последнего байтов.
        case 0xE0: if (a < 0xA0) return false; break;
        case 0xED: if (a > 0x9F) return false; break;
        case 0xF0: if (a < 0x90) return false; break;
        case 0xF4: if (a > 0x8F) return false; break;
        default:   if (a < 0x80) return false;
      }

      case 1: if (*source >= 0x80 and *source < 0xC2) return false;
    }

    if (*source > 0xF4) {
      return false;
    }

    return true;
  }

  inline void RealIncrement() {
    /*
     * Таблица ниже реализует отображения вида первый байт  UTF-8 последо-
     * вательности байтов --> количество байтов в этой последовательности.
     * Реализация этого отображения в виде статической таблицы наиболее
     * быстрая.
     */
    static const unsigned TRAILING_BYTES[256] = {
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
    };
    unsigned extra_bytes_to_read = TRAILING_BYTES[static_cast<uint8_t>(*iter_)];

    symbol_.utf16_ = 0;
    symbol_.len_ = 0;

    // В целях оптимизации раскрываем цикл выделения значений из UTF-8 последовательности.
    uint32_t decoded_symbol = 0;
    switch (extra_bytes_to_read) {
      case 5:
        symbol_.chain_[0] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
        decoded_symbol <<= 6;
        if (not Next()) {
          return;
        }

      case 4:
        symbol_.chain_[extra_bytes_to_read-4] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
        decoded_symbol <<= 6;
        if (not Next()) {
          return;
        }

      case 3:
        symbol_.chain_[extra_bytes_to_read-3] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
        decoded_symbol <<= 6;
        if (not Next()) {
          return;
        }

      case 2:
        symbol_.chain_[extra_bytes_to_read-2] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
        decoded_symbol <<= 6;
        if (not Next()) {
          return;
        }

      case 1:
        symbol_.chain_[extra_bytes_to_read-1] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
        decoded_symbol <<= 6;
        if (not Next()) {
          return;
        }

      case 0:
        symbol_.chain_[extra_bytes_to_read] = *iter_;
        decoded_symbol += static_cast<uint8_t>(*iter_);
    }

    /*
     * Магические константы, использующиеся для перевода в UTF-8. Каждое
     * число в массиве соответвует байту в UTF-8 последовательности.
     */
    static const uint32_t OFFSETS_FROM_UTF8[6] = { 0x00000000UL, 0x00003080UL, 0x000E2080UL,
                             0x03C82080UL, 0xFA082080UL, 0x82082080UL };
    decoded_symbol -= OFFSETS_FROM_UTF8[extra_bytes_to_read];

    symbol_.len_ = extra_bytes_to_read + 1;

    // Проверка UTF-8 последовательности на корректность.
    if (not IsLegalUtf8((const uint8_t*)symbol_.chain_, symbol_.len_)) {
      return;
    }

    // Имеем 32 разрядное значени, которое представляет код данной UTF-8 последовательности.
    // Мы обрабатываем только значения, меньшие чем 0xffff.
    if (decoded_symbol > 0xffffu) {
      return;
    }

    // Увеличиваем счетчик символов только, если выделен корректный символ.
    symbol_.utf16_ = static_cast<uint16_t>(decoded_symbol);
    symbol_.cp1251_ = lexer::GetCp1251FromUtf16(symbol_.utf16_);
    ++sym_pos_;
  }

  //! Получение следующего символа из потока через оператор инкремента.
  inline void increment() {
    // Пытаемся взять очередной байт из потока.
    if (not Next()) {
      return;
    }

    // Вызываем реальные инкремент.
    RealIncrement();
  }

  //! Сравнение двух итераторов.
  inline bool equal(const Utf8Iterator& other) const {
    if (iter_ == end_ and other.iter_ == other.end_) {
      return true;
    }
    return iter_ == other.iter_;
  }

  //! Доступ к текущему символу в потоке.
  Utf8Symbol& dereference() const {
    return symbol_;
  }

public:
  //! Инициализация по умолчанию конструирует признак конца потока.
  Utf8Iterator()
    : iter_()
    , end_()
    , sym_pos_(0)
    , byte_pos_(0) {
  }

  /*!
   * \brief Конструктор с итераторами конца и начала.
   *
   * begin  Итератор начала байтового потока.
   * end    Итератор конца байтового потока.
   */
  Utf8Iterator(const ByteIterator& begin, const ByteIterator& end)
    : iter_(begin)
    , end_(end)
    , sym_pos_(0)
    , byte_pos_(1) {
    RealIncrement();
  }

  //! Возврат байтовой позиции.
  size_t GetBytePos() const {
    return byte_pos_;
  }

  //! Возврат символьной позиции.
  size_t GetSymbolPos() const {
    return sym_pos_;
  }

private:
  mutable Utf8Symbol  symbol_;   //!< Текущий UTF-8 символ в потоке.
  ByteIterator        iter_;     //!< Текущий итератор во входном потоке.
  ByteIterator        end_;      //!< Итератор конца потока.
  size_t              sym_pos_;  //!< Позиция в потоке, выраженная в символах.
  size_t              byte_pos_; //!< Позиция в потоке, выраженная в байтах.
};

}  // namespace lexer

