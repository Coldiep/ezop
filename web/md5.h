
#pragma once

#include <stdint.h>
#include <string.h>

#include <string>

namespace ezop {

namespace {
static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21
}

class Md5Impl {
public:
    Md5Impl() {
        Init();
    }

    /*!
     * \brief Реализация алгоритм MD5.
     *
     * \param[in]  text Указатель на буфер, где находится входной текст.
     * \param[in]  sz   Длина буфера в байтах.
     * \param[out] md5  Результат вычисления MD5.
     */
    void Generate(const char* text, size_t sz, std::string& md5) {
        Init();
        Update((uint8_t*)text, sz);
        Finalize();
        md5.assign((const char*)digest_, 16);
    }

private:
    void Init() {
        memset(count_, 0, 2 * sizeof(uint32_t));
        state_[0] = 0x67452301;
        state_[1] = 0xefcdab89;
        state_[2] = 0x98badcfe;
        state_[3] = 0x10325476;
    }

    void Update(uint8_t* input, uint32_t len) {
        // Вычисляем количество байтов в остатке от деления на 64.
        uint32_t index = (uint32_t)((count_[0] >> 3) & 0x3F);

        // Обновляем число битов.
        if ((count_[0] += (len << 3)) < (len << 3)) {
            ++count_[1];
        }
        count_[1] += (len >> 29);

        // Производим трансформацию по алгоритму MD5 всех 64 байтных блоков.
        uint32_t i = 0;
        uint32_t part_len = 64 - index;
        if (len >= part_len) {
            memcpy(&buffer_[index], input, part_len);
            Transform(buffer_);
            for (i = part_len; i + 63 < len; i += 64) {
                Transform(&input[i]);
            }
            index = 0;
        }

        // Запоминаем оставшийся текст.
        memcpy(&buffer_[index], &input[i], len - i);
    }

    void Finalize() {
        // Сохраняем количество битов.
        uint8_t bits[8];
        Encode(bits, count_, 8);

        // Заполняем дырки вплоть до 56 байтов.
        uint32_t index = (uint32_t)((count_[0] >> 3) & 0x3f);
        uint32_t pad_len = (index < 56) ? (56 - index) : (120 - index);
        Update(PADDING, pad_len);

        // Добавляем длину перед дыркой.
        Update(bits, 8);

        // Сохраняем состояние в дайджесте.
        Encode (digest_, state_, 16);

        // Очищаем буфер.
        memset(count_, 0, 2 * sizeof(uint32_t));
        memset(state_, 0, 4 * sizeof(uint32_t));
        memset(buffer_,0, 64 * sizeof(uint8_t));

    }

    inline uint32_t RotateLeft(uint32_t x, uint32_t n) {
        return ((x << n) | (x >> (32-n)));
    }

    inline uint32_t F(uint32_t x, uint32_t y, uint32_t z) {
        return ((x & y) | (~x & z));
    }

    inline uint32_t G(uint32_t x, uint32_t y, uint32_t z) {
        return ((x & z) | (y & ~z));
    }

    inline uint32_t H(uint32_t x, uint32_t y, uint32_t z) {
        return (x ^ y ^ z);
    }

    inline uint32_t I(uint32_t x, uint32_t y, uint32_t z) {
        return (y ^ (x | ~z));
    }

    inline void FF(uint32_t& a, uint32_t b, uint32_t c, uint32_t d, uint32_t x, uint32_t s, uint32_t ac) {
        a += F(b, c, d) + x + ac;
        a = RotateLeft(a, s);
        a += b;
    }

    inline void GG(uint32_t& a, uint32_t b, uint32_t c, uint32_t d, uint32_t x, uint32_t s, uint32_t ac) {
        a += G(b, c, d) + x + ac;
        a = RotateLeft(a, s);
        a += b;
    }

    inline void HH(uint32_t& a, uint32_t b, uint32_t c, uint32_t d, uint32_t x, uint32_t s, uint32_t ac) {
        a += H(b, c, d) + x + ac;
        a = RotateLeft(a, s);
        a += b;
    }

    inline void II(uint32_t& a, uint32_t b, uint32_t c, uint32_t d, uint32_t x, uint32_t s, uint32_t ac) {
        a += I(b, c, d) + x + ac;
        a = RotateLeft(a, s);
        a += b;
    }

    void Transform(uint8_t* block) {
       uint32_t a = state_[0];
       uint32_t b = state_[1];
       uint32_t c = state_[2];
       uint32_t d = state_[3];
       uint32_t x[16];

       Decode(x, block, 64);

       // Первый раунд.
       FF (a, b, c, d, x[ 0], S11, 0xd76aa478);
       FF (d, a, b, c, x[ 1], S12, 0xe8c7b756);
       FF (c, d, a, b, x[ 2], S13, 0x242070db);
       FF (b, c, d, a, x[ 3], S14, 0xc1bdceee);
       FF (a, b, c, d, x[ 4], S11, 0xf57c0faf);
       FF (d, a, b, c, x[ 5], S12, 0x4787c62a);
       FF (c, d, a, b, x[ 6], S13, 0xa8304613);
       FF (b, c, d, a, x[ 7], S14, 0xfd469501);
       FF (a, b, c, d, x[ 8], S11, 0x698098d8);
       FF (d, a, b, c, x[ 9], S12, 0x8b44f7af);
       FF (c, d, a, b, x[10], S13, 0xffff5bb1);
       FF (b, c, d, a, x[11], S14, 0x895cd7be);
       FF (a, b, c, d, x[12], S11, 0x6b901122);
       FF (d, a, b, c, x[13], S12, 0xfd987193);
       FF (c, d, a, b, x[14], S13, 0xa679438e);
       FF (b, c, d, a, x[15], S14, 0x49b40821);

       // Второй раунд.
       GG (a, b, c, d, x[ 1], S21, 0xf61e2562);
       GG (d, a, b, c, x[ 6], S22, 0xc040b340);
       GG (c, d, a, b, x[11], S23, 0x265e5a51);
       GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa);
       GG (a, b, c, d, x[ 5], S21, 0xd62f105d);
       GG (d, a, b, c, x[10], S22,  0x2441453);
       GG (c, d, a, b, x[15], S23, 0xd8a1e681);
       GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8);
       GG (a, b, c, d, x[ 9], S21, 0x21e1cde6);
       GG (d, a, b, c, x[14], S22, 0xc33707d6);
       GG (c, d, a, b, x[ 3], S23, 0xf4d50d87);
       GG (b, c, d, a, x[ 8], S24, 0x455a14ed);
       GG (a, b, c, d, x[13], S21, 0xa9e3e905);
       GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8);
       GG (c, d, a, b, x[ 7], S23, 0x676f02d9);
       GG (b, c, d, a, x[12], S24, 0x8d2a4c8a);

       // Третий раунд.
       HH (a, b, c, d, x[ 5], S31, 0xfffa3942);
       HH (d, a, b, c, x[ 8], S32, 0x8771f681);
       HH (c, d, a, b, x[11], S33, 0x6d9d6122);
       HH (b, c, d, a, x[14], S34, 0xfde5380c);
       HH (a, b, c, d, x[ 1], S31, 0xa4beea44);
       HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9);
       HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60);
       HH (b, c, d, a, x[10], S34, 0xbebfbc70);
       HH (a, b, c, d, x[13], S31, 0x289b7ec6);
       HH (d, a, b, c, x[ 0], S32, 0xeaa127fa);
       HH (c, d, a, b, x[ 3], S33, 0xd4ef3085);
       HH (b, c, d, a, x[ 6], S34,  0x4881d05);
       HH (a, b, c, d, x[ 9], S31, 0xd9d4d039);
       HH (d, a, b, c, x[12], S32, 0xe6db99e5);
       HH (c, d, a, b, x[15], S33, 0x1fa27cf8);
       HH (b, c, d, a, x[ 2], S34, 0xc4ac5665);

       // Четвертый раунд.
       II (a, b, c, d, x[ 0], S41, 0xf4292244);
       II (d, a, b, c, x[ 7], S42, 0x432aff97);
       II (c, d, a, b, x[14], S43, 0xab9423a7);
       II (b, c, d, a, x[ 5], S44, 0xfc93a039);
       II (a, b, c, d, x[12], S41, 0x655b59c3);
       II (d, a, b, c, x[ 3], S42, 0x8f0ccc92);
       II (c, d, a, b, x[10], S43, 0xffeff47d);
       II (b, c, d, a, x[ 1], S44, 0x85845dd1);
       II (a, b, c, d, x[ 8], S41, 0x6fa87e4f);
       II (d, a, b, c, x[15], S42, 0xfe2ce6e0);
       II (c, d, a, b, x[ 6], S43, 0xa3014314);
       II (b, c, d, a, x[13], S44, 0x4e0811a1);
       II (a, b, c, d, x[ 4], S41, 0xf7537e82);
       II (d, a, b, c, x[11], S42, 0xbd3af235);
       II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb);
       II (b, c, d, a, x[ 9], S44, 0xeb86d391);

       state_[0] += a;
       state_[1] += b;
       state_[2] += c;
       state_[3] += d;

       memset(x, 0, sizeof(x));
    }

    void Encode(uint8_t* dest, uint32_t* src, uint32_t len) {
        for (uint32_t i = 0, j = 0; j < len; i++, j += 4) {
            dest[j] = (uint8_t)(src[i] & 0xff);
            dest[j + 1] = (uint8_t)((src[i] >> 8) & 0xff);
            dest[j + 2] = (uint8_t)((src[i] >> 16) & 0xff);
            dest[j + 3] = (uint8_t)((src[i] >> 24) & 0xff);
        }
    }

    void Decode(uint32_t* dest, uint8_t* src, uint32_t len) {
        for (uint32_t i = 0, j = 0; j < len; i++, j += 4) {
            dest[i] = ((uint32_t)src[j]) | (((uint32_t)src[j+1])<<8) |
                      (((uint32_t)src[j+2])<<16) | (((uint32_t)src[j+3])<<24);
        }
    }

    uint32_t state_[4];
    uint32_t count_[2];
    uint8_t  buffer_[64];
    uint8_t  digest_[16];
    uint8_t  finalized_;
};

}  // namespace ezop

