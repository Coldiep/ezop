
#pragma once

#include <rex/expressions_tree.h>
#include <rex/Scanner.h>

namespace rexp {

// Грамматика регулярных выражений:

// RE  -->  SEQUENCE
// SEQUENCE  --> TERM_LIST
// TERM_LIST  -->  TERM | TERM TERM_LIST
// TERM  -->  LITERAL | ALTERNATION | REPETITION | PREDICTION | QUESTION_MARK | FINITE_REPETITION
// LITERAL  -->  SYMBOLS_SET | EPSILON | '(' RE ')'
// ALTERNATION  -->  TERM '|' TERM
// PLUS  -->  TERM '+'
// REPETITION  -->  TERM '*'
// PREDICTION  -->  TERM '/' TERM
// QUESTION_MARK  -->  TERM '?'
// FINITE_REPETITION  --> TERM BRACES_EXPRESSION | TERM BRACES1_EXPRESSION | TERM BRACES2_EXPRESSION
// BRACES_EXPRESSION  -->  '{' \d '}' | '{' \d ',' \d '}'

class Parser : public ExpressionTree {
public:
  // конструктор
  Parser( stream st );

  void Print() {
    expr_->Print();
  }

  // генерирует НКА
  void GetNfa(Nfa& nfa) {
    expr_->GenerateNfa(nfa);
  }

private:
  // корень дерева выражений
  AbstractExpr::Ptr expr_;

  // объект потока с РЕ
  Scanner scan_;

  // стек для скобок
  std::stack<unsigned> parentheses_stack_;

  // методы разбора...

  // правило RE --> SEQUENCE
  void Parse();

  // правило SEQUENCE  --> TERM_LIST
  AbstractExpr::Ptr Re();

  // правила TERM  -->  LITERAL | ALTERNATION | REPETITION | PREDICTION | QUESTION_MARK | FINITE_REPETITION
  AbstractExpr::Ptr Term();

  // правила LITERAL  -->  SYMBOLS_SET | '(' RE ')'
  AbstractExpr::Ptr Literal();
};

}

