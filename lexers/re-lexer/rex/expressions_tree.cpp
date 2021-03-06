
#include <rex/expressions_tree.h>
using rexp::ExpressionTree;

#include <vector>
#include <iostream>

void ExpressionTree::AlternationExpr::GenerateNfa(Nfa& nfa) {
  // генерируем автоматы для левого и правого подвыражений
  Nfa left_nfa; left_expr_->GenerateNfa(left_nfa);
  Nfa right_nfa; right_expr_->GenerateNfa(right_nfa);

  // запоминаем допускающее состояние правого подвыражения
  Nfa::StateSet right_accept_states = right_nfa.GetAcceptStates();

  // запоминаем начальное состояние правого автомата
  unsigned right_start_state = right_nfa.GetStartState();

  // запоминаем начальное состояние левого автомата
  unsigned left_start_state = left_nfa.GetStartState();

  // запоминаем допускающие состояния левого автомат.
  Nfa::StateSet left_accept_states = left_nfa.GetAcceptStates();

  // клонируем левый автомат
  left_nfa.CloneFromOffset(nfa, 1);

  // запоминаем левое максимальное состояние
  unsigned offset_after_left = nfa.GetMaxStateNum();

  // клонируем правый автомат
  right_nfa.CloneFromOffset(nfa, offset_after_left);

  // запоминаем правое максимальное состояние
  unsigned max_state_number = nfa.GetMaxStateNum();

  // добавляем и устанавливаем новое начальное состояние
  nfa.AddState(1);
  nfa.SetStartState(1);

  // добавляем и устанавливаем новое допускающее состояние
  nfa.AddState(max_state_number + 1);
  nfa.CleanAcceptStates();
  nfa.AddToAcceptSet(max_state_number + 1);

  // добавляем переходы
  nfa.AddTransition(1, '\0', left_start_state + 1);
  nfa.AddTransition(1, '\0', right_start_state + offset_after_left);

  for (Nfa::StateSet::iterator state = left_accept_states.begin(); state != left_accept_states.end(); ++state) {
    nfa.AddTransition(*state + 1, '\0', max_state_number + 1);
  }

  for (Nfa::StateSet::iterator state = right_accept_states.begin(); state != right_accept_states.end(); ++state) {
    nfa.AddTransition(*state + offset_after_left, '\0', max_state_number + 1);
  }
}

void ExpressionTree::SequenceExpr::GenerateNfa(Nfa& nfa) {
  Nfa left_nfa; left_expr_->GenerateNfa(left_nfa);
  Nfa right_nfa; right_expr_->GenerateNfa(right_nfa);
  left_nfa.CloneFromOffset(nfa, 0);

  // запоминаем начальное состояние левого автомата
  unsigned left_start_state = nfa.GetStartState();

  // запоминаем допускающие состояния левого автомата.
  Nfa::StateSet left_accept_states = nfa.GetAcceptStates();
  nfa.CleanAcceptStates();

  unsigned right_start_state = nfa.GetMaxStateNum();
  right_nfa.CloneFromOffset(nfa, right_start_state - 1);

  // добавляем переходы из допускающих состояний левого автомата в начальное состояние правого.
  for (Nfa::StateSet::iterator state = left_accept_states.begin(); state != left_accept_states.end(); ++state) {
    // добавляем переходы из всех старых допускающих состояний к старому начальному
    nfa.AddTransition(*state, '\0', right_start_state);
  }

  // добавляем новое начальное состояние.
  nfa.SetStartState(left_start_state);
}

void ExpressionTree::RepetitionExpr::GenerateNfa(Nfa& nfa) {
  Nfa prev_nfa; expr_->GenerateNfa(prev_nfa);
  prev_nfa.CloneFromOffset(nfa, 1);

  // запоминает старое начальное состсояние
  unsigned old_start_state = nfa.GetStartState();

  // добавляет новое начальное состояние
  nfa.AddState(1);
  nfa.SetStartState(1);

  // добавляет переход в старое начальное состояние.
  nfa.AddTransition(1, '\0', old_start_state);

  // добавляем новое допускающее состояние.
  unsigned new_accept_state = nfa.GetMaxStateNum() + 1;
  nfa.AddState(new_accept_state);
  nfa.AddTransition(1, '\0', new_accept_state);

  // добавляем переходы из всех старых допускающих состояний к новому допускающему
  const Nfa::StateSet& old_accept_states = nfa.GetAcceptStates();
  for (Nfa::StateSet::const_iterator state = old_accept_states.begin(); state != old_accept_states.end(); ++state) {
    // добавляем переходы из всех старых допускающих состояний к старому начальному
    nfa.AddTransition(*state, '\0', old_start_state);

    // добавляем переходы из всех старых допускающих состояний к новому допускающему
    nfa.AddTransition(*state, '\0', new_accept_state);
  }

  // Очищаем допускающие состояние и добавляем новое единственное допускающее состояние.
  nfa.CleanAcceptStates();
  nfa.AddToAcceptSet(new_accept_state);
}

void ExpressionTree::FiniteRepExpr::CloneFromOffset(Nfa& nfa, Nfa nfa_tmp, size_t times) {
  nfa_tmp.CloneFromOffset(nfa, 0);

  // запоминаем старое начальное состояние
  unsigned old_start_state = nfa.GetStartState();
  for (unsigned cnt = 1; cnt < times; ++cnt) {
    unsigned max_state_number = nfa.GetMaxStateNum();
    nfa_tmp.CloneFromOffset(nfa, max_state_number - 1);
  }

  // добавляем новое начальное состояние
  nfa.SetStartState(old_start_state);
}

void ExpressionTree::FiniteRepExpr::GenerateNfa(Nfa& nfa) {
  Nfa gen_nfa; expr_->GenerateNfa(gen_nfa);

  if (expr_type_ == BRACES_TWO) {
    nfa.AddState(1);
    std::vector<unsigned> accept_states;
    for (unsigned cnt = 0; cnt <= (rep_cnt_hight_ - rep_cnt_low_); ++cnt) {
      // генерируем новый автомат и добавляем его к nfa начиная с nfa.GetMaxStateNum()
      Nfa nfa_tmp;
      CloneFromOffset(nfa_tmp, gen_nfa, rep_cnt_low_ + cnt);
      nfa_tmp.CloneFromOffset(nfa, nfa.GetMaxStateNum());

      // запоминаем допускающее состояние
      accept_states.push_back(*nfa.GetAcceptStates().begin());

      // добавляем новый эпсилон переход из нового начального состояния в старое начальное
      nfa.AddTransition(1, 0, nfa.GetStartState());
    }

    // добавляем новое начальное состояние
    unsigned max_state_number = nfa.GetMaxStateNum();
    nfa.AddState(max_state_number + 1);
    nfa.CleanAcceptStates();
    nfa.AddToAcceptSet(max_state_number + 1);
    nfa.SetStartState(1);

    for (std::vector<unsigned >::const_iterator it = accept_states.begin(); it != accept_states.end(); ++it) {
      // добавляем новый эпсилон переход из нового допускающего состояния в старое допускающее
      nfa.AddTransition(*it, 0, max_state_number + 1);
    }
  } else {
    CloneFromOffset(nfa, gen_nfa, rep_cnt_low_);
  }
}

void ExpressionTree::PredictionExpr::GenerateNfa(Nfa& nfa) {
  left_expr_->GenerateNfa(nfa);
  Nfa right_nfa; right_expr_->GenerateNfa(right_nfa);

  // запоминаем старое начальное состояние
  unsigned old_start_state = nfa.GetStartState();

  // запоминаем максимальный номер состояния
  unsigned max_state_number = nfa.GetMaxStateNum();

  right_nfa.CloneFromOffset(nfa, max_state_number);

  // запоминаем старое начальное состояние
  unsigned new_start_state = nfa.GetStartState();

  // добавляем новое начальное состояние
  nfa.SetStartState(old_start_state);

  // добавляем переход из нового начального состояния в старое начальное
  nfa.AddTransition(max_state_number, 0, new_start_state);
}

void ExpressionTree::AlternationExpr::Print() {
  left_expr_->Print();
  unsigned level = Level();
  for (unsigned tabs = 0; tabs < level; ++tabs) {
    std::cout << "  ";
  }
  std::cout << "|\n";
  right_expr_->Print();
}

void ExpressionTree::SequenceExpr::Print() {
  left_expr_->Print();
  unsigned level = Level();
  for (unsigned tabs = 0; tabs < level; ++tabs) {
    std::cout << "  ";
  }
  std::cout << '&' << std::endl;
  right_expr_->Print();
}

// вывод выражения на консоль
void ExpressionTree::RepetitionExpr::Print() {
  expr_->Print();
  unsigned level = Level();
  for (unsigned tabs = 0; tabs < level; ++tabs) {
    std::cout << "  ";
  }
  std::cout << '*' << std::endl;
}

// вывод выражения на консоль
void ExpressionTree::SymbolSetExpr::Print() {
  unsigned level = Level();
  for (unsigned tabs = 0; tabs < level; ++tabs ) {
    std::cout << "  ";
  }
  std::cout << "[" << buf_ << "]" << std::endl;
}

// вывод выражения на консоль
void ExpressionTree::FiniteRepExpr::Print() {
  expr_->Print();
  unsigned level = Level();
  for( unsigned tabs = 0; tabs < level; ++tabs) {
    std::cout << "  ";
  }
  std::cout << "{}" << std::endl;
}

// вывод выражения на консоль
void ExpressionTree::PredictionExpr::Print() {
  left_expr_->Print();
  unsigned level = Level();
  for (unsigned tabs = 0; tabs < level; ++tabs) {
    std::cout << "  ";
  }
  std::cout << "/" << std::endl;
  right_expr_->Print();
}
