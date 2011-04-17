#include "exp_tree.h"
#include <set>
#include <stdio.h>
#include <string.h>


using relexer::TreePoint;
using relexer::ExpTree;
using relexer::point_type;
using relexer::Position;


long n_id = 0;
std::string symbols;
std::set<TreePoint*> t_leaves;

/*!
* \brief Конструктор по умолчанию.
*
*/
TreePoint::TreePoint() : 
  left_(0),
  right_(0),
  parent_(0) { }

/*!
* \brief Конструктор,инициализирующийся одним символом.
*
* \param[in] c  Символ,который должен соответствовать вершине.
*
*/
TreePoint::TreePoint(char c) : 
  left_(0),
  right_(0),
  parent_(0),
  contents_(c),
  type_(symbol),
  nullable_(false) { }

/*!
* \brief Конструктор,инициализирующийся диапазоном символов.
*
* \param[in] c1  Первый символ из диапазона.
* \param[in] c2  Последний символ из диапазона.
*
*/
TreePoint::TreePoint(unsigned int c1,unsigned int c2) {
    type_ = symbol;
    TreePoint* prev = new TreePoint(c1);

    for ( unsigned i = c1 + 1; i <= c2; ++i ) {
        TreePoint* l = prev;
        TreePoint* r = new TreePoint(i);
        prev = new TreePoint();
        prev->left_ = l;
        prev->right_ = r;
        prev->type_ = opUnion;
        l->parent_ = r->parent_ = prev;
    }

    contents_ = prev->contents_;
    left_ = prev->left_;
    right_ = prev->right_;
    parent_ = prev->parent_;
    nullable_ = false;
    type_ = prev->type_;
}

/*!
* \brief Конструктор копирования.
*
* \param[in] tp  Вершина для копирования.
*
*/
TreePoint::TreePoint(TreePoint* tp) {
    left_ = tp->left_;
    right_ = tp->right_;
    parent_ = tp->parent_;
    type_ = tp->type_;
    contents_ = tp->contents_;
    nullable_ = tp->nullable_;
    firstpos_ = tp->firstpos_;
}

/*!
* \brief Деструктор.
*
*/
TreePoint::~TreePoint() {
    delete left_;

    if (0) {
        if (right_)
            delete right_;

        if (this->type_ != opIter and left_)
            delete left_;

        firstpos_.clear();
        followpos_.clear();
        lastpos_.clear();
        memset(this,NULL,sizeof(this));
    }
}

/*!
* \brief Вычисление множеств firstpos_,lastpos_ и followpos_ для вершины.
*
*/
void TreePoint::calc() {
    id_ = n_id++;

    if (right_ and left_) {
        right_->calc();

        if (type_ != opIter)
            left_->calc();

        switch (type_) {
            case opUnion: {
                firstpos_.insert(left_->firstpos_.begin(),left_->firstpos_.end());
                firstpos_.insert(right_->firstpos_.begin(),right_->firstpos_.end());
                lastpos_.insert(left_->lastpos_.begin(),left_->lastpos_.end());
                lastpos_.insert(right_->lastpos_.begin(),right_->lastpos_.end());
                break;
            }

            case opIter: {
                firstpos_ = left_->firstpos_;
                lastpos_ = left_->lastpos_;

                for ( std::set<TreePoint*>::iterator it = lastpos_.begin(); it != lastpos_.end(); ++it )
                    (*it)->followpos_.insert(firstpos_.begin(),firstpos_.end());
                break;
            }

            case opConcat: {
                if (left_->nullable_) {
                    firstpos_.insert(left_->firstpos_.begin(),left_->firstpos_.end());
                    firstpos_.insert(right_->firstpos_.begin(),right_->firstpos_.end());
                }
                else
                    firstpos_ = left_->firstpos_;

                if (right_->nullable_) {
                    lastpos_.insert(left_->lastpos_.begin(),left_->lastpos_.end());
                    lastpos_.insert(right_->lastpos_.begin(),right_->lastpos_.end());
                }
                else
                    lastpos_ = right_->lastpos_;

                for ( std::set<TreePoint*>::iterator it = left_->lastpos_.begin(); it != left_->lastpos_.end(); ++it )
                    (*it)->followpos_.insert(right_->firstpos_.begin(),right_->firstpos_.end());
                break;
            }
        }
    }
    else if (type_ == symbol) {
        firstpos_.insert(this);
        lastpos_.insert(this);
        t_leaves.insert(this);

        if (not(contents_ == '#' and end_ == 1) and symbols.find(contents_) == std::string::npos)
            symbols.push_back(contents_);
    }
}

/*!
* \brief Вывод параметров вершины на печать.
*
*/
void TreePoint::print(int n) {
    for ( int i = 0; i < n; i++ )
        printf("%s","   ");

    switch (type_) {
        case symbol:
            printf("%c",contents_);
            break;

        case opUnion:
            printf("%s","|");
            break;

        case opIter:
            printf("%s","*");
            break;

        case opConcat:
            printf("%s","&");
            break;

        case empty:
            printf("%s","<e>");
            break;
    }

    if (right_)
        right_->print(n + 1);

    if (left_)
        left_->print(n + 1);
}

//****************************************************************

/*!
* \brief Конструктор по умолчанию.
*
*/
ExpTree::ExpTree() {
    root_ = NULL;
}

/*!
* \brief Конструктор копирования.
*
* \param[in] t  Дерево для копирования.
*
*/
ExpTree::ExpTree(ExpTree* t) {
    root_ = t->root_;
    this->alphabet_ = t->alphabet_;
    this->leaves_ = t->leaves_;
}

/*!
* \brief Конструктор,инициализирующийся вершиной.
*
* \param[in] tp  Вершина,которая должна стать корнем дерева.
*
*/
ExpTree::ExpTree(TreePoint pnt) {
    root_ = new TreePoint();
    root_->contents_ = pnt.contents_;
    root_->id_ = pnt.id_;
    root_->left_ = pnt.left_;
    root_->parent_ = pnt.parent_;
    root_->right_ = pnt.right_;
    root_->type_ = pnt.type_;
    root_->nullable_ = pnt.nullable_;
    root_->firstpos_ = pnt.firstpos_;
    root_->followpos_ = pnt.followpos_;
    root_->lastpos_ = pnt.lastpos_;
}

/*!
* \brief Конструктор,инициализирующийся диапазоном символов.
*
* \param[in] c1  Первый символ из диапазона.
* \param[in] c2  Последний символ из диапазона.
*
*/
ExpTree::ExpTree(unsigned int c1,unsigned int c2) {
    root_ = new TreePoint(c1,c2);
}

/*!
* \brief Конструктор,инициализирующийся одним символом.
*
* \param[in] c  Символ,который должен соответствовать корню дерева.
*
*/
ExpTree::ExpTree(char c) {
    root_ = new TreePoint(c);
}

/*!
* \brief Конструктор,инициализирующийся типом вершины.
*
* \param[in] t  Тип,который должен иметь корень дерева.
*
*/
ExpTree::ExpTree(point_type t) {
    root_ = new TreePoint();
    root_->type_ = t;
    root_->nullable_ = (t == empty or t == opIter) ? true : false;
}

/*!
* \brief Проверка того, является ли дерево пустым.
*
*/
bool ExpTree::is_empty() {
    return root_ == NULL;
}


/*!
* \brief Построение нового дерева с корнем заданного типа.
*
* \param[in] t  Тип,который должен иметь корень дерева.
*
*/
ExpTree* ExpTree::make_new_root(point_type t) {
    TreePoint* pnt = new TreePoint();
    pnt->type_ = t;
    pnt->right_ = pnt->left_ = root_;
    root_->parent_ = pnt;
    pnt->nullable_ = t == opIter ? true : pnt->right_->nullable_;
    root_ = new TreePoint(pnt);
    ExpTree* out = new ExpTree();
    out->root_ = root_;
    return out;
}

/*!
* \brief Слияние двух деревьев в одно.
*
* \param[in] l  Левое поддерево.
* \param[in] r  Правое поддерево.
* \param[in] t  Тип,который должен иметь корень дерева.
*
*/
ExpTree* ExpTree::merge_trees(ExpTree* l,ExpTree* r,point_type t) {
    ExpTree* res = new ExpTree(t);
    l->root_->parent_ = r->root_->parent_ = res->root_;
    res->root_->left_ = l->root_;
    res->root_->right_ = r->root_;
    res->root_->nullable_ =
        t == opIter ? true : (t == opConcat ? (res->root_->right_->nullable_ and res->root_->left_->nullable_)
            : (res->root_->right_->nullable_ or res->root_->left_->nullable_));
    return res;
}

/*!
* \brief Вычисление множества followpos_.
*
*/
void ExpTree::calc_followpos() {
    symbols = "";
    t_leaves.clear();
    n_id = 0;
    root_->id_ = 0;
    root_->calc();
    alphabet_ = symbols;
    leaves_ = t_leaves;
}

/*!
* \brief Деструктор.
*
*/
ExpTree::~ExpTree() {
    delete root_;
}