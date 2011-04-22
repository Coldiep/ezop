

#ifndef STREAM_H__
#define STREAM_H__

#include "typedefs.h"

namespace rexp_{

    // представляет собой абстракцию потока, откуда берутся символы. Возвращает eos_en если в потоке больше нет символов.
    class stream{
    public:
        // iпоказывает конец потока
        enum { eos_en = 0 };

        enum buf_type_en{
            internal_string = 1, external_pointer };
    private:
        const char_type_t* pbuf_; // указатель на буфер в случае внешнего хранилища
        string_t buf_; // буфер, где хранятся символы для ввода
        buf_type_en buf_type_; // тип буфера

        unsigned int len_; // длина буфера
        unsigned int pos_; // текущая позиция в буфере

        // стек для запоминания позиций
        std::stack< unsigned int > state_stack_;

    public:
        // конструктор по умолчанию. Инициализирует поток пустой строкой
        stream()
            : buf_(""), len_(0), pos_(0), buf_type_(internal_string), pbuf_(0){}

        // конструктор берет строку в качестве параметра
        stream( const string_t& str )
            : buf_(str), len_(buf_.size()), pos_(0), buf_type_(internal_string), pbuf_(0){}

        // конструктор с внешним буфером
        stream( const char_type_t* pbuf ):len_((unsigned int)-1), pos_(0), buf_type_(external_pointer), pbuf_(pbuf){}

        // возвращает следующий символ из буфера или eos_en если в буфере символы отсутствуют
        char_type_t next()
        {
            if( buf_type_ == internal_string )
            {
                if( pos_ >= len_ ) return eos_en;
                return buf_[ pos_ ++ ];
            }
            else if( buf_type_ == external_pointer )
            {
                if( *(pbuf_ + pos_) ) return *(pbuf_ + pos_ ++);
                else return eos_en;
            }

            return eos_en;
        }


        // переходит к предыдущему символу если находится не в начале потока
        void back()
        {
            if( pos_ > 0 ) -- pos_;
            else pos_ = 0;
        }

        const string_t& get_buf() { return buf_; } // возвращает копию буфера

        // проверка на конец потока
        bool is_end()
        {
            if( buf_type_ == internal_string ) return pos_ >= len_;
            else if( buf_type_ == external_pointer ) return *(pbuf_ + pos_) == 0;

            return true;
        }

        unsigned int get_pos() { return pos_;    } // возвращает текущую позицию указателя в буфере
        unsigned int get_length() { return len_;    } // возвращает размер буфера

        // устанавливает текущую позицию в буфере
        void set_pos( unsigned int pos )
        {
            if( pos >= 0 && pos < len_ ) pos_ = pos;
        }

        // устанавливает позицию на начало
        void reset ()
        {
            pos_ = 0;
        }

        // методы для работы со стеком...

        // запоминает текущую позицию в стеке
        unsigned int remember_state()
        {
            state_stack_.push( pos_ );
            return pos_;
        }

        // восстанавливает позицию из стека
        unsigned int restore_state()
        {
            if( state_stack_.size() > 0 )
            {
                pos_ = state_stack_.top();
                state_stack_.pop();
            }

            return pos_;
        }

        // удаляет одну позицию из стека
        unsigned int release_state()
        {
            if( state_stack_.size() > 0 )
            {
                unsigned int tmp_ = state_stack_.top();
                state_stack_.pop();
                return tmp_;
            }

            return pos_;
        }
    };
}

#endif // STREAM_H__

