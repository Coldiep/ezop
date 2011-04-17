#include "re-lexer.h"
#include <string>
#pragma warning(disable: 4996)
#include "error_thrower.h"

using relexer::ReLexer;
using relexer::ReToken;

/*!
* \brief Конструктор по умолчанию.
*
*/
ReLexer::ReLexer() {
    cur_pos = 0;
    types_no = 0;
    tree = new TokenTree();
    stream = "";
    LexType* lt = new LexType();
    lt->id_ = -1;
    lt->name_ = "ERROR";
    lt->valid_ = false;
    lex_types.insert(lt);
}
/*!
* \brief Деструктор.
*
*/
ReLexer::~ReLexer() {
    for ( std::set<LexType*>::iterator it = this->lex_types.begin(); it != this->lex_types.end(); ++it ) {
        LexType* lt = (*it);

        if (lt->id_ != -1)
            delete lt->d_;

        delete lt;
    }
    delete this->tree;
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
std::string ReLexer::strip(std::string str) {
    /*
  
    std::string tmp = str.c_str();
    str.replace("\n"," ");
    std::string oldtmp;
    do
    {
      oldtmp = tmp;
      tmp.Replace("  "," ");
    }
    while(tmp.Length() != oldtmp.Length());
    return tmp.c_str();*/
    return str;
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
void ReLexer::set_stream(std::string s) {
    stream = strip(s);
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
void ReLexer::set_stream_file(std::string filename,std::string tail) {
    FILE* f = fopen(filename.c_str(),"r");

    if (f == NULL)
        throw_error("Failed to open input stream file");
    char s[9999];
    int lines = 0;

    while (fgets(s,9999,f) != NULL) { /*
                                         if (s[strlen(s)-1] == 10)
                                             s[strlen(s)-1] = ' ';*/
        stream.append(s);
    }
    stream.append(tail);
    stream = strip(stream);
}
/*!
* \brief Добавление лексического типа.
*
* \param[in] name    Название типа.
* \param[in] regexp    Регулярное выражение.
* \param[in] priority  Приоритет типа.
* \param[in] ret    Флаг для указания того, нужно ли включать данный тип в выходное дерево токенов.
* \param[in] id      Идентификатор типа.
*/
void ReLexer::add_type(std::string name,std::string regexp,int priority,int id_,bool ret) {
    LexType* lt = new LexType();

    if (id_ < 0)
        lt->id_ = ++types_no;
    else {
        types_no++;
        lt->id_ = id_;
    }
    lt->name_ = name;
    lt->regexp_ = regexp;
    lt->is_returned_ = ret;
    Scanner S;
    ExpTree* t = S.process(regexp);
    DFA* d = new DFA();
    d->build(t);
    lt->d_ = d;
    lt->valid_ = true;
    lt->priority_ = priority;
    lex_types.insert(lt);
    delete t;
}
/*!
* \brief Добавление лексического типа.
*
* \param[in] id      Идентификатор типа.
* \param[in] regexp    Регулярное выражение.
* \param[in] priority  Приоритет типа.
* \param[in] ret    Флаг для указания того, нужно ли включать данный тип в выходное дерево токенов.
*/
void ReLexer::add_type(int id_,std::string regexp,int priority,bool ret) {
    LexType* lt = new LexType();
    types_no++;
    lt->id_ = id_;
    lt->name_ = "";
    lt->regexp_ = regexp;
    lt->is_returned_ = ret;
    Scanner S;
    ExpTree* t = S.process(regexp);
    DFA* d = new DFA();
    d->build(t);
    lt->d_ = d;
    lt->valid_ = true;
    lt->priority_ = priority;
    lex_types.insert(lt);
    delete t;
}
/*!
* \brief Проведение лексического анализа.
*
*/
void ReLexer::analyze() {
    int id = 0;
    std::set<int> token_ends;
    token_ends.insert(-1);

    for ( std::set<int>::iterator it = token_ends.begin(); it != token_ends.end(); ++it ) {
        int start_pos = (*it);
        std::set<ReToken*> next_tokens = get_tokens(start_pos);

        for ( std::set<ReToken*>::iterator iter = next_tokens.begin(); iter != next_tokens.end(); ++iter ) {
            ReToken* tok = (*iter);
            char integer[10] = "";
            itoa(++id,integer,10);
            tok->id_ = integer;
            tree->add_node(tok);
            token_ends.insert(tok->finish_);
        }
    }
}
/*!
* \brief Сброс параметров всех лексических типов в начальные.
*
*/
void ReLexer::reset() {
    for ( std::set<LexType*>::iterator it = lex_types.begin(); it != lex_types.end(); ++it ) {
        LexType* lt = (*it);

        if (lt->id_ == -1)
            continue;

        lt->valid_ = true;
        lt->d_->curr_state_ = lt->d_->get_start_state();
    }
}
/*!
* \brief Получение списка токенов,начинающихся с заданной позиции.
*
* \param[in] pos Позиция во входном потоке.
*/
std::set<ReToken*> ReLexer::get_tokens(int pos) {
    std::map<unsigned int,bool> types_returned;
    std::map<unsigned int,int> types_priority;

    for ( std::set<LexType*>::iterator lex_types_it = lex_types.begin(); lex_types_it != lex_types.end();
        ++lex_types_it ) {
        LexType* lt = (*lex_types_it);
        types_returned[lt->id_] = lt->is_returned_;
        types_priority[lt->id_] = lt->priority_;
    }

    int i = ++pos;
    std::map<unsigned int,unsigned int> accepted_types;
    int invalid = 0;

    while (invalid < types_no) {
        if (i >= stream.length())
            break;

        for ( std::set<LexType*>::iterator it = lex_types.begin(); it != lex_types.end(); ++it ) {
            if (not((*it)->valid_))
                continue;

            if ((*it)->d_->move(stream[i])) {
                if ((*it)->d_->curr_state_->type_ == finish or (*it)->d_->curr_state_->type_ == start_finish)
                    accepted_types[(*it)->id_] = i;
            }
            else {
                (*it)->valid_ = false;
                invalid++;
            }
        }
        i++;
    }

    std::set<ReToken*> result;
    std::map<unsigned int,unsigned int>::iterator it = accepted_types.begin(),end = accepted_types.end();

    for (; it != end; ++it ) {
        bool add = true;

        for ( std::map<unsigned int,unsigned int>::iterator it1 = accepted_types.begin(); it1 != end; ++it1 ) {
            if ((*it).second == (*it1).second and (*it).first != (*it1).first
                and types_priority[(*it).first] < types_priority[(*it1).first]) {
                add = false;
                break;
            }
        }

        if (add)
            result.insert(new ReToken((*it).first,pos,(*it).second,stream.substr(pos,(*it).second - pos + 1),
                types_returned[(*it).first],(*it).first));
    }

    if (accepted_types.empty() and i < stream.length())
        result.insert(new ReToken(0,pos,(int)stream.length() - 1,stream.substr(pos,(int)stream.length() - pos),
            true));
    reset();
    return result;
}
/*!
* \brief Проверка,достигнут ли конец потока.
*
*/
bool ReLexer::IsEnd() {
    return (cur_pos++ >= stream.length());
}
/*!
* \brief Получение списка токенов,следующих за данным.
*
* \param[in] t Токен,список последователей которого необходимо получить.
*/
parser::Lexer::TokenList relexer::ReLexer::GetTokens(parser::Token::Ptr t) {
    int abs_pos_ = t->abs_pos_;
    int length_ = t->length_;
    std::set<ReToken*> next_tokens = get_tokens(abs_pos_ + length_ - 1);
    TokenList tokens_to_return;

    for ( std::set<ReToken*>::iterator it = next_tokens.begin(); it != next_tokens.end(); ++it ) {
        parser::Token::Ptr new_token = parser::Token::Ptr(new parser::Token((*it)->terminal_symbol_id_));
        new_token->type_ = (*it)->terminal_symbol_id_;
        new_token->abs_pos_ = (*it)->start_;
        new_token->length_ = (*it)->finish_ - (*it)->start_ + 1;
        tokens_to_return.push_back(new_token);
    }
    return tokens_to_return;
}