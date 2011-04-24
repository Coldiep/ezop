
#include <rex/parser.h>
using rexp::Parser;

#include <stdexcept>
#include <sstream>

// разбор правила RE --> SEQUENCE
void Parser::Parse() {
  expr_ = Re();
}

// разбор правила SEQUENCE  --> TERM_LIST
Parser::AbstractExpr::Ptr Parser::Re() {
  AbstractExpr::Ptr exp = Term();

  // добавляем каждое RE к последовательности
  for(;;) {
    Scanner::Token::Ptr tok = scan_.GetToken();
    if(tok->Type() == Scanner::END) {
      scan_.Back();
      break;
    }

    if (tok->Type() == Scanner::RIGHT_PAR) {
      if (par_stack_.size() > 0) {
        scan_.Back();
        break;
      } else {
        throw std::invalid_argument("Лишняя завершающая круглая скобка в регулярном выражении");
      }
    }

    scan_.Back();
    exp = AbstractExpr::Ptr(new SequenceExpr(exp, Term()));
  }

  return exp;
}

// разбор правил TERM  -->  LITERAL | ALTERNATION | REPETITION | PREDICTION | QUESTION_MARK | FINITE_REPETITION
Parser::AbstractExpr::Ptr Parser::Term() {
  AbstractExpr::Ptr exp = Literal();
  Scanner::Token::Ptr tok = scan_.GetToken();

  switch (tok->Type()) {
  case Scanner::ALTER:
    return AbstractExpr::Ptr(new AlternationExpr(exp, Term()));

  case Scanner::FOR_SLASH:
    return AbstractExpr::Ptr(new PredictionExpr(exp, Term()));

  case Scanner::STAR:
    return AbstractExpr::Ptr(new RepetitionExpr(exp));

  case Scanner::PLUS:
    return AbstractExpr::Ptr(new SequenceExpr(exp, AbstractExpr::Ptr(new RepetitionExpr(exp))));

  case Scanner::QM:
    return AbstractExpr::Ptr(new AlternationExpr(AbstractExpr::Ptr(new SymbolSetExpr(std::string())), exp));

  case Scanner::BRACES_EXPR:
    return AbstractExpr::Ptr(new SequenceExpr(AbstractExpr::Ptr(new FiniteRepExpr(exp, tok->First())), AbstractExpr::Ptr(new RepetitionExpr(exp))));

  case Scanner::BRACES1_EXPR:
    return AbstractExpr::Ptr(new FiniteRepExpr(exp, tok->First()));

  case Scanner::BRACES2_EXPR:
    return AbstractExpr::Ptr(new FiniteRepExpr(exp, tok->First(), tok->Second()));

  default:
    break;
  }

  scan_.Back();
  return exp;
}

// разбор правил LITERAL  -->  SYMBOLS_SET | '(' RE ')'
Parser::AbstractExpr::Ptr Parser::Literal() {
  Scanner::Token::Ptr tok = scan_.GetToken();
  switch (tok->Type()) {
  case Scanner::LEFT_PAR: {
    // добавляем индикатор открывающиейся скобки в стек
    par_stack_.push(1);

    AbstractExpr::Ptr tmp_exp = Re();
    Scanner::Token::Ptr tok_tmp = scan_.GetToken();
    if (tok_tmp->Type() != Scanner::RIGHT_PAR) {
      std::stringstream st;
      st << "Ошибка при разборе правила LITERAL --> '(' RE ')'. Отсутствует завершающая круглая скобка в регулярном выражении";
      throw std::invalid_argument(st.str().c_str());
    }

    // удаляем индикатор открывающиейся скобки из стека
    par_stack_.pop();
    return tmp_exp;
  }

  case Scanner::INC_SYMBOLS:
    return AbstractExpr::Ptr(new SymbolSetExpr(tok->Data()));

  default:
    break;
  }

  throw std::invalid_argument("Отсутствует литерал в регулярном выражении");
  return AbstractExpr::Ptr();
}
