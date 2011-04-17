#pragma warning(disable: 4715)
#include "exp_tree.h"
#include "dfa.h"
#include "error_thrower.h"

using namespace relexer;

int s_id = 0;

/*!
* \brief Конструктор по умолчанию.
*
*/
State::State() {
    marked_ = false;
}

/*!
* \brief Получение списка возможных переходов из данного состояния по определенному символу.
*
* \param[in] c  Символ, по которому должен осуществляться переход.
*
*/
unsigned int State::get_transition(char c) {
    std::map<char,unsigned int>::iterator it = transitions_.find(c);

    if (it != transitions_.end())
        return (*it).second;

    return 0;
}

/*!
* \brief Добавление перехода.
*
* \param[in] c    Символ, по которому осуществляется переход.
* \param[in] State  Идентификатор состояния,в которое осуществляется переход.
*
*/
void State::add_transition(char c,unsigned int State) {
    transitions_[c] = State;
}

//******************************************************************************************
/*!
* \brief Конструктор по умолчанию.
*
*/
DFA::DFA() { }


/*!
* \brief Добавление состояния.
*
* \param[in] State  Состояние,которое следует добавить.
*
*/
void DFA::add_state(State* s) {
    if (not(states_.empty()))
        for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it )
            if ((*it)->id_ == s->id_)
                return;
    states_.insert(s);
}

/*!
* \brief Добавление состояния.
*
* \param[in] id    Идентификатор состояния,которое следует добавить.
* \param[in] type  Тип состояния,которое следует добавить.
*
*/
void DFA::add_state(int id,state_type type) {
    if (not(states_.empty()))
        for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it )
            if ((*it)->id_ == id)
                throw_error("State with this id already exists: ",id);
    State* s = new State();
    s->id_ = id;
    s->type_ = type;
    states_.insert(s);
}


/*!
* \brief Создние нового состояния.
*
* \param[in] leaves_set    Множество вершин дерева регулярного выражения,которым будет соответствовать состояние.
* \param[in] type      Тип состояния.
* \return          Новое состояние.
*/
State* DFA::make_state(std::set<TreePoint*> leaves_set,state_type type) {
    State* s = new State();
    s->type_ = type;

    for ( std::set<TreePoint*>::iterator it = leaves_set.begin(); it != leaves_set.end(); ++it ) {
        s->ids_.insert((*it)->id_);

        if ((*it)->contents_ == '#')
            (*it)->contents_ = (*it)->contents_;

        if ((*it)->contents_ == '#' and (*it)->end_ == 1) {
            if (type == start)
                s->type_ = start_finish;
            else
                s->type_ = finish;
        }
    }
    s->id_ = ++s_id;

    for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it ) {
        if (s->ids_.size() != (*it)->ids_.size())
            continue;
        bool next = false;

        for ( std::set<int>::iterator itt = (*it)->ids_.begin(); itt != (*it)->ids_.end(); ++itt ) {
            if (s->ids_.find(*itt) == s->ids_.end()) {
                next = true;
                break;
            }
        }

        if (next)
            continue;
        else {
            s->id_ = (*it)->id_;
            return s;
        }
    }
    return s;
}

/*!
* \brief Добавление перехода.
*
* \param[in] beg_state  Состояние,из которого будет осуществляться переход.
* \param[in] c      Символ, по которому будет осуществляться переход.
* \param[in] beg_state  Состояние,в которое будет осуществляться переход
* 
*/
void DFA::add_transition(int beg_state,char c,int end_state) {
    State* bs = get_state(beg_state);
    State* es = get_state(end_state);
    bs->add_transition(c,es->id_);
}

/*!
* \brief Получение состояния по идентификатору.
*
* \param[in] ii    Идентификатор состояния.
* \return      Состояние ДКА с указанным идентификатором.
*/
State* DFA::get_state(int ii) {
    for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it ) {
        State* s = (*it);

        if (s->id_ == ii)
            return s;
    }

    throw_error("No State with id ",ii);
}

/*!
* \brief Получение начального состояния ДКА.
*
* \return  Начальное состояние ДКА.
*/
State* DFA::get_start_state() {
    for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it ) {
        State* s = (*it);

        if (s->type_ == start or s->type_ == start_finish)
            return s;
    }

    throw_error("No start State foundnot");
}

/*!
* \brief Запуск алгоритма проверки соответствия входной строки регулярному выражению,на основе которого построен ДКА.
*
* \param[in] str  Регулярное выражение.
* \return      1,если входная строка соответствует регулярному выражению,на основе которого построен ДКА,0 - если не соответствует.
*/
int DFA::process(std::string str) {
    //printf("Processing string '%s' of length %i: ",str.c_str(),str.length());
    State* s = get_start_state();

    for ( unsigned int i = 0; i < str.length(); i++ ) {
        unsigned int ind = s->get_transition(str[i]);

        if (ind == 0) {
            s->type_ = mid;
            break;
        }
        s = get_state(ind);
    }

    if (s->type_ == finish or s->type_ == start_finish)
        return 1;
    else
        return 0;
}

//! Структура для хранения информации о состоянии ДКА.
struct tri {
    tri(int idid,char aa,std::set<TreePoint*> ss) {
        s = ss;
        a = aa;
        id = idid;
    }
    std::set<TreePoint*> s;
    char a;
    int id;
};

/*!
* \brief Построение ДКА по дереву регулярного выражения.
*
* \param[in]  tr  Дерево регулярного выражения.
*
*/
void DFA::build(ExpTree* tr) {
    s_id = 0;
    bool flag = true;
    std::string input = tr->alphabet_;
    std::set<TreePoint*> leaves = tr->leaves_;

    State* firststate = make_state(tr->root_->firstpos_,start);
    add_state(firststate);

    while (flag) {
        flag = false;
        std::set<tri*> toadd;

        for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it ) {
            if ((*it)->marked_)
                continue;
            (*it)->marked_ = true;

            for ( unsigned int i = 0; i < input.length(); i++ ) {
                char a = input[i];
                std::set<TreePoint*> S;

                for ( std::set<TreePoint*>::iterator leaves_it = leaves.begin(); leaves_it != leaves.end();
                    ++leaves_it ) {
                    TreePoint* leaf = (*leaves_it);

                    for ( std::set<int>::iterator iter = (*it)->ids_.begin(); iter != (*it)->ids_.end(); ++iter )
                        if ((*iter) == leaf->id_ and leaf->contents_ == a) {
                            S.insert(leaf->followpos_.begin(),leaf->followpos_.end());
                        }
                }

                if (not(S.empty())) {
                    flag = true;
                    tri* tmp = new tri((*it)->id_,a,S);
                    toadd.insert(tmp);
                }
            }
        }

        for ( std::set<tri*>::iterator it = toadd.begin(); it != toadd.end(); ++it ) {
            State* newstate = make_state((*it)->s);
            add_state(newstate);
            add_transition((*it)->id,(*it)->a,newstate->id_);
        }
        toadd.clear();
    }
    curr_state_ = get_start_state();
}

/*!
* \brief Переход ДКА из текущего состояния по символу.
*
* \param[in] c  Символ, по которому должен осуществляться переход.
* \return    1,если переход возможен,0 - если невозможен.
*/
int DFA::move(char c) {
    unsigned int ind = curr_state_->get_transition(c);

    if (ind != 0) {
        curr_state_ = get_state(ind);
        return 1;
    }
    else
        return 0;
}

/*!
* \brief Деструктор.
*
*/
DFA::~DFA() {
    for ( std::set<State*>::iterator it = states_.begin(); it != states_.end(); ++it )
        delete (*it);
}