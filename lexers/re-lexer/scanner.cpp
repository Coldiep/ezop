#pragma warning(disable: 4172)
#include "error_thrower.h"
#include "scanner.h"

using relexer::Scanner;
using relexer::ExpTree;
using relexer::Position;

/*!
* \brief Обработка части регулярного выражения,заключенной в скобки.
*
* \param[in] p  Позиция во входной строке,с которой следует начать обработку.
* \return    Дерево разбора части регулярного выражения.
*/

ExpTree* Scanner::DoBrackets(Position& p) {
    ExpTree* t = new ExpTree();
    ++p;

    if (*p == 0)
        throw_error("'[' is at the end of the input");

    else if (*p == ']')
        throw_error("Invalid expression []");

    for (; *p != ']'; ++p ) {
        if (*p == 0)
            throw_error("Nonpair [ is at the end of the input");
        else if (*(p + 1) == '-') {
            unsigned int c1 = *p;
            ++p;
            ++p;

            if (*p == 0)
                throw_error("Nonpair [ is at the end of the input");

            if (*p == ']') {
                t = t->is_empty() ? new ExpTree(c1,0xff) : ExpTree::MergeTrees(t,new ExpTree(c1,0xff),opUnion);
                break;
            }
            else {
                if (t->is_empty())
                    t = new ExpTree(c1,*p);
                else
                    t = ExpTree::MergeTrees(t,new ExpTree(c1,*p),opUnion);
            }
        }
        else
            t = t->is_empty() ? new ExpTree(*p) : ExpTree::MergeTrees(t,new ExpTree(*p),opUnion);
    }
    return t;
}

/*!
* \brief Обработка регулярного выражения.
*
* \param[in] str  Строка,содержащая регулярное выражение.
* \return      Дерево разбора регулярного выражения.
*/
ExpTree* Scanner::Process(std::string str) {
    //printf("Processing regexp '%s' of length %i.\n",str.c_str(),str.length());
    //std::string input = str.append("#");//"(a|b)*abb#";//"N.*";//"a*b[c-de](a)f|n|(((b)*)c|d)";
    std::stack<point_type> mag;
    std::stack<ExpTree*> trees_mag;

    for ( Position p = str.c_str(); *p; ++p ) {
        char ch = *p;

        if (*p == '(') {
            if (*(p + 1) == ')')
                throw_error("Invalid expression ()");
            mag.push(OpLeftBracket);
        }
        else if (*p == ')') {
            if (mag.empty())
                throw_error("Nonpair bracket");
            else if (mag.top() != OpLeftBracket) {
                ExpTree* right = trees_mag.top();
                trees_mag.pop();
                ExpTree* left = trees_mag.top();
                trees_mag.pop();
                ExpTree* t = ExpTree::MergeTrees(left,right,mag.top());
                trees_mag.push(t);
                mag.pop();

                if (mag.top() != OpLeftBracket)
                    throw_error("Nonpair bracket");
            }
            mag.pop();
        }
        else if (*p == '|') {
            if (trees_mag.empty())
                throw_error("Missing left operand of |");
            mag.push(opUnion);
        }
        else if (*p == '*') {
            if (trees_mag.empty())
                throw_error("Missing left operand of *");
            ExpTree* t = trees_mag.top()->MakeNewRoot(opIter);
        }
        else if (*p == '+') {
            if (trees_mag.empty())
                throw_error("Missing left operand of +");
            ExpTree* t =
                ExpTree::MergeTrees(new ExpTree(trees_mag.top()),trees_mag.top()->MakeNewRoot(opIter),opConcat);
            trees_mag.pop();
            trees_mag.push(t);
        }
        else if (*p == '?') {
            if (trees_mag.empty())
                throw_error("Missing left operand of ?");
            ExpTree* t = ExpTree::MergeTrees(trees_mag.top(),new ExpTree(empty),opUnion);
            trees_mag.pop();
            trees_mag.push(t);
        }
        else if (*p == '.') {
            trees_mag.push(new ExpTree(0x20,0xff));
        }
        else if (*p == '\\') {
            ++p;
            trees_mag.push(new ExpTree(*p));
        }
        else if (*p == '[') {
            ExpTree* t = DoBrackets(p);
            trees_mag.push(t);
        }
        else {
            if (not(mag.empty()) or trees_mag.empty()) {
                trees_mag.push(new ExpTree(ch));
            }
            else {
                ExpTree* t = trees_mag.top()->MakeNewRoot(opConcat);
                t->root_->right_ = new TreePoint(ch);
                trees_mag.pop();
                trees_mag.push(t);
            }
        }

        if (*p != '(' and *p != '|' and *p != '\\' and *p != '*' and *p != '?' and *p != '+' and *p != ']' and *(p + 1) != '*'
            and *(p + 1) != '?' and *(p + 1) != '+' and not(mag.empty()) and (mag.top() == opUnion or mag.top() == opConcat)
            and trees_mag.size() >= 2) {
            ExpTree* r = trees_mag.top();
            trees_mag.pop();
            ExpTree* l = trees_mag.top();
            trees_mag.pop();
            trees_mag.push(ExpTree::MergeTrees(l,r,mag.top()));
            mag.pop();
        }

        if (*p != '(' and *p != '|' and *(p + 1) != '|' and *(p + 1) != '*' and *(p + 1) != '?' and *(p + 1) != '+'
            and *(p + 1) != ')' and *(p + 1) != 0)
            mag.push(opConcat);
    }
    ExpTree* e;

    if (mag.empty() and not(trees_mag.empty()))
        e = trees_mag.top();
    else
        while (not(mag.empty())) {
            ExpTree* r = trees_mag.top();
            trees_mag.pop();
            e = trees_mag.top()->MakeNewRoot(mag.top());
            e->root_->right_ = r->root_;
            mag.pop();
        }
    trees_mag.pop();
    e->MakeNewRoot(opConcat);
    TreePoint* end = new TreePoint('#');
    end->end_ = 1;
    e->root_->right_ = end;
    e->CalcFollowpos();

    return e;
}