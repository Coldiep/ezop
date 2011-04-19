#include "error_thrower.h"
#include "re-token.h"


using relexer::ReToken;
using relexer::TokenTree;

/*!
* \brief Конструктор,заполняющий все поля экземпляра класса.
*
* \param[in] t        Тип токена.
* \param[in] s        Позиция начала токена.
* \param[in] f        Позиция конца токена.
* \param[in] st        Фрагмент входной цепочки,которому соотвествует данный токен.
* \param[in] ret      Флаг для указания того, нужно ли включать данный токен в выходное дерево токенов.
* \param[in] term_sym_id  Идентификатор терминального символа грамматики,которому соответствует данный токен.
*/
ReToken::ReToken(int t,int s,int f,std::string st,bool ret,int term_sym_id) {
    type_ = t;
    start_ = s;
    finish_ = f;
    str_ = st;
    is_returned_ = ret;
    terminal_symbol_id_ = term_sym_id;
}

/*!
* \brief Конструктор копирования.
*
*/
ReToken::ReToken(ReToken* t) {
    type_ = t->type_;
    start_ = t->start_;
    finish_ = t->finish_;
    str_ = t->str_;
    is_returned_ = t->is_returned_;
    this->children_ = t->children_;
    this->id_ = t->id_;
}

/*!
* \brief Конструктор по умолчанию.
*
*/
TokenTree::TokenTree() {
    this->root_ = new ReToken(0,-1,-1,"",true);
}

/*!
* \brief Добавление вершины к дереву.
*
* \param[in] ReToken  Токен,который следует добавить к дереву.
*/
void TokenTree::AddNode(ReToken* node) {
    std::vector<ReToken*> nodes2see;
    nodes2see.push_back(root_);
    int len = 1;
    int uses = 0;

    for ( int i = 0; i < len; i++ ) {
        if (nodes2see[i]->finish_ == node->start_ - 1) {
            for ( int j = 0; j < node->id_.length(); j++ )
                if (node->id_[j] == '.')
                    node->id_.erase(j,std::string::npos);
            char integer[10] = "";
            itoa(++uses,integer,10);
            node->id_.append(".").append(integer);

            nodes2see[i]->children_.insert(new ReToken(node));
        }

        for ( std::set<ReToken*>::iterator it = nodes2see[i]->children_.begin(); it != nodes2see[i]->children_.end(); ++it )
            {
            nodes2see.push_back(*it);
            len++;
        }
    }
}