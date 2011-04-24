
#include <rex/expressions_tree.h>
using rexp::ExpressionTree;

#include <vector>
#include <iostream>

void ExpressionTree::AlternationExpr::GenerateNfa(Nfa& nfa) {
  // генерируем автоматы для левого и правого подвыражений
  Nfa left_nfa; left_expr_->GenerateNfa(left_nfa);
  Nfa right_nfa; right_expr_->GenerateNfa(right_nfa);

  // запоминаем допускающие состояния левого подвыражения
  unsigned left_accept_state = *left_nfa.GetAcceptStates().begin();

  // запоминаем конечное состояние правого подвыражения
  unsigned right_accept_state = *right_nfa.GetAcceptStates().begin();

  // запоминаем стартовое состояние правого подвыражения
  unsigned left_start_state = left_nfa.GetStartState();

  // запоминаем стартовое состояние правого подвыражения
  unsigned right_start_state = right_nfa.GetStartState();

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

  // добавляем и устанавливаем новое конечное состояние
  nfa.AddState(max_state_number + 1);
  nfa.CleanAcceptStates();
  nfa.AddToAcceptSet(max_state_number + 1);

  // добавляем переходы
  nfa.AddTransition(1, 0, left_start_state + 1);
  nfa.AddTransition(1, 0, right_start_state + offset_after_left);
  nfa.AddTransition(left_accept_state + 1, 0, max_state_number + 1);
  nfa.AddTransition(right_accept_state + offset_after_left, 0, max_state_number + 1);
}

void ExpressionTree::SequenceExpr::GenerateNfa(Nfa& nfa) {
  Nfa left_nfa; left_expr_->GenerateNfa(left_nfa);
  Nfa right_nfa; right_expr_->GenerateNfa(right_nfa);
  left_nfa.CloneFromOffset(nfa, 0);

  // запоминаем старое состояние
  unsigned old_start_state = nfa.GetStartState();

  unsigned max_state_number = nfa.GetMaxStateNum();
  right_nfa.CloneFromOffset(nfa, max_state_number - 1);

  // добавляем новое
  nfa.SetStartState(old_start_state);
}

void ExpressionTree::RepetitionExpr::GenerateNfa(Nfa& nfa) {
  Nfa prev_nfa; expr_->GenerateNfa(prev_nfa);
  prev_nfa.CloneFromOffset(nfa, 1);

  // запоминает старое начальное состсояние
  unsigned old_start_state = prev_nfa.GetStartState();

  // добавляет новое начальное состояние
  nfa.AddState(1);
  nfa.SetStartState(1);

  // добавляет переход
  nfa.AddTransition(1, 0, old_start_state + 1);

  // запоминаем максимальное состояние
  unsigned max_state_number = nfa.GetMaxStateNum();

  // запоминаем старое конечное состояние
  unsigned old_accept_state = *nfa.GetAcceptStates().begin();

  // удаляем старое конечное состояние
  nfa.AddState(max_state_number + 1);
  nfa.CleanAcceptStates();

  // добавляем новое конечное состояние и переход
  nfa.AddToAcceptSet(max_state_number + 1);
  nfa.AddTransition(1, 0, max_state_number + 1);

  // добавляем переходы из всех старых конечных состояний к новому конечному
  nfa.AddTransition(old_accept_state, 0, max_state_number + 1);
  nfa.AddTransition(old_accept_state, 0, old_accept_state);
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
      // добавляем новый эпсилон переход из нового конечного состояния в старое конечное
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
  std::cout << "L" << std::endl;
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
