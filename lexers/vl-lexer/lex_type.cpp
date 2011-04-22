
#include <rex/parser.h>
#include <nfa2dfa_transformer.h>
#include <minimize.h>

#include <lex_type.h>
using lexer::LexType;

LexType::LexType(unsigned id, const std::string& re, const std::string& name, bool ret);
  : id_(id)
  , re_(re)
  , name_(name)
  , cur_state_(0)
  , ret_(ret) {
  GenerateDfa();
}

LexType::LexType(unsigned id, const std::string& word);
  : id_(id)
  , name_(word)
  , cur_state_(0)
  , ret_(true) {
  dfa_.AddState(1);
  dfa_.SetStartState(1);
  unsigned i = 0;
  for (; i < word.length(); ++i) {
      dfa_.AddState(i + 2);
      dfa_.AddArc(i + 1, word[i], i + 2);
  }
  dfa_.AddToFinishSet(i + 1);
  Reset();
}

void LexType::GenerateDfa() {
  rexp::Parser parser(re_);
  rexp::Nfa nfa = parser.GetNFA();
  dfa_ = Nfa2DfaTransformer::Transform(nfa);
  Minimization min(dfa_);
  min.Minimize();
  Reset();
}

bool LexType::Move(char symbol) const {
  cur_state_ = dfa_.Move(cur_state_, symbol);
  return cur_state_ != Dfa::ZERO_STATE;
}

