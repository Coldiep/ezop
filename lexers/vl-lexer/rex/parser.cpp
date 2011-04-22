
#include <rex/parser.h>
using rexp::Parser;

Parser::Parser(stream st)
  : scan_(st) {
  Parse();
}

// разбор правила RE --> SEQUENCE
void Parser::Parse() {
  // мы имеем непустую строку
  if (scan_.GetToken().Type() != Scanner::END) {
    scan_.Back();
    expr_ = Re();
  } else {
    expr_ = new SymbolSetExpr(std::string());
  }
}

// разбор правила SEQUENCE  --> TERM_LIST
Parser::AbstractExpr::Ptr Parser::Re() {
  AbstractExpr::Ptr exp = Term();

  // добавляем каждое RE к последовательности
  for(;;) {
    Scanner::Token tok = scan_.GetToken();
    if(tok.Type() == Scanner::END) {
      scan_.Back();
      break;
    }

    if (tok.Type() == Scanner::RIGHT_PAR) {
      if (parentheses_stack_.size() > 0) {
        scan_.Back();
        break;
      } else {
        throw std::logic_error("Лишняя завершающая круглая скобка в регулярном выражении");
      }
    }

    scan_.Back();
    exp = new SequenceExpr(exp, Term());
  }

  return exp;
}

// разбор правил TERM  -->  LITERAL | ALTERNATION | REPETITION | PREDICTION | QUESTION_MARK | FINITE_REPETITION
Parser::AbstractExpr::Ptr Parser::Term() {
  AbstractExpr::Ptr exp = Literal();
  Scanner::Token tok = scan_.GetToken();

  switch (tok.Type()) {
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
    return AbstractExpr::Ptr(new SequenceExpr(AbstractExpr::Ptr(new FiniteRepExpr(exp, tok.first())), AbstractExpr::Ptr(new RepetitionExpr(exp))));

  case Scanner::BRACES1_EXPR:
    return AbstractExpr::Ptr(new FiniteRepExpr(exp, tok.first()));

  case Scanner::BRACES2_EXPR:
    return AbstractExpr::Ptr(new FiniteRepExpr(exp, tok.first(), tok.second()));
  }

  scan_.Back();
  return exp;
}

// разбор правил LITERAL  -->  SYMBOLS_SET | '(' RE ')'
Parser::AbstractExpr::Ptr Parser::Literal() {
  Scanner::Token tok = scan_.GetToken();
  switch (tok.Type()) {
  case Scanner::LEFT_PAR: {
    // добавляем индикатор открывающиейся скобки в стек
    parentheses_stack_.push(1);

    AbstractExpr::Ptr tmp_exp = Re();
    Scanner::Token tok_tmp = scan_.GetToken();
    if (tok_tmp.Type() != Scanner::RIGHT_PAR) {
      std::stringstream st;
      st << "Ошибка при разборе правила LITERAL --> '(' RE ')'. Отсутствует завершающая круглая скобка в регулярном выражении";
      throw std::logic_error(st.str().c_str());
    }

    // удаляем индикатор открывающиейся скобки из стека
    parentheses_stack_.pop();
    return tmp_exp;
  }

  case Scanner::INC_SYMBOLS:
    return new SymbolSetExpr(tok.Data());
  }

  throw std::logic_error("Отсутствует литерал в регулярном выражении");
  return 0;
}
