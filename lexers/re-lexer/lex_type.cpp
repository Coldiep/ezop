
#include <rex/parser.h>
#include <rex/nfa2dfa_transformer.h>
#include <rex/minimize.h>

#include <lex_type.h>
using lexer::LexType;

LexType::LexType(unsigned id, const std::string& re, const std::string& name, bool ret)
  : id_(id)
  , re_(re)
  , name_(name)
  , cur_state_(0)
  , ret_(ret) {
  GenerateDfa();
}

LexType::LexType(unsigned id, const std::string& word)
  : id_(id)
  , name_(word)
  , cur_state_(0)
  , ret_(true) {
  dfa_.AddState(1);
  dfa_.SetStartState(1);
  unsigned i = 0;
  for (; i < word.length(); ++i) {
      dfa_.AddState(i + 2);
      dfa_.AddTransition(i + 1, word[i], i + 2);
  }
  dfa_.AddToAcceptSet(i + 1);
  Reset();
}

void LexType::GenerateDfa() {
  rexp::Parser parser(&re_[0], &re_[0] + re_.length());
  rexp::Nfa nfa;
  parser.GetNfa(nfa);
  rexp::Nfa2DfaTransformer::Transform(nfa, dfa_);
  rexp::Minimization min(dfa_);
  min.Minimize();
  Reset();
}

bool LexType::Move(char symbol) {
  cur_state_ = dfa_.Move(cur_state_, symbol);
  return cur_state_ != rexp::Dfa::ZERO_STATE;
}

