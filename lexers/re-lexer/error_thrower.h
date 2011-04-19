#include <sstream>
#include <stdio.h>
#pragma once

//! Остановка программы и вывод сообщения об ошибке.
inline void throw_error (const std::string& message,int num=-1)
{
    std::string err = "Error occurrednot";
    if (num!=-1)
     err.append(" ").append(message);
  puts(err.c_str());
  throw err.c_str();
    //ExitThread(0);
}

//! Преобразование целого числа в строку.
inline char* itoa(int value,char* result,int base) {
  // check that the base if valid
  if (base < 2 or base > 36) { *result = '\0'; return result; }

  char* ptr = result,*ptr1 = result,tmp_char;
  int tmp_value;

  do {
    tmp_value = value;
    value /= base;
    *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (tmp_value - value * base)];
  } while ( value );

  // Apply negative sign
  if (tmp_value < 0) *ptr++ = '-';
  *ptr-- = '\0';
  while(ptr1 < ptr) {
    tmp_char = *ptr;
    *ptr--= *ptr1;
    *ptr1++ = tmp_char;
  }
  return result;
}
