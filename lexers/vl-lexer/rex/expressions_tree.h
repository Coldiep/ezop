
#pragma once

#include <rex/nfa.h>

#include <string>

namespace rexp {

class ExpressionTree {
protected:
  // forward объявление вложенных классов
  class AlternationExpr;
  class SequenceExpr;
  class RepetitionExpr;
  class SymbolSetExpr;
  class FiniteRepExpr;
  class PredictionExpr;

  //! абстрактный базовый класс для всех классов выражений
  struct AbstractExpr {
    //! Тип умного указателя на класс.
    Typedef boost::shared_ptr<AbstractExpr> Ptr;

    //! типы выражений
    enum ExprType {
      ALTERNATION_EXPR = 1  //!< альтернатива E|E
      , SEQUENCE_EXPR       //!< последовательность EE
      , REPETITION_EXPR     //!< замыкание Клини E*
      , SYMBOL_SET_EXPR     //!< множество символов
      , FINITE_REP_EXPR     //!< конечное повторение {n}
      , PREDICTION_EXPR     //!< действителен если: E/E
    };

    //! возвращает копию данного РЕ
    virtual AbstractExpr::Ptr Clone() = 0;

    //! возвращает тип данного РЕ
    virtual ExprType Type() = 0;

    //! печатает на консоль дерево, соответствующее данному РЕ.
    virtual void Print() = 0;

    //! возвращает уровень данного узла в иерархии дерева
    virtual size_t Level() = 0;

    //! генерирует НКА, соответствующий данному РЕ
    virtual void GenerateNfa(Nfa& nfa) = 0;

    //! виртуальный деструктор
    virtual ~AbstractExpr() {
    }
  };

  //! соответствует правилу грамматики RE_ROOT --> RE1 | RE2
  class AlternationExpr : public AbstractExpr{
    AbstractExpr::Ptr left_expr_;  //!< Левое поддерево.
    AbstractExpr::Ptr right_expr_; //!< Правое поддерево.

  public:
    AlternationExpr(AbstractExpr::Ptr left_expr, AbstractExpr::Ptr right_expr)
      : left_expr_(left_expr)
      , right_expr_(right_expr) {
      assert(left_expr and right_expr_ and "Внутренняя ошибка при разборе РЕ вида A|B");
    }

    // клонирует данный объект
    AbstractExpr::Ptr Clone() {
      return AbstractExpr::Ptr(new AlternationExpr(left_expr_, right_expr_));
    }

    // тип выражения
    ExprType Type() {
      return ALTERNATION_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // уровень выражения в иерархии дерева
    size_t Level() {
      size_t left_level_ = left_expr_->Level() + 1;
      size_t right_level_ = right_expr_->Level() + 1;
      return left_level_ > right_level_ ? left_level_ : right_level_;
    }

    // генерация НКА
    void GenerateNfa(Nfa& nfa);
  };

  //! соответствует правилу RE_ROOT --> RE1 RE2
  class SequenceExpr : public AbstractExpr {
    // два указателя на объекты
    AbstractExpr::Ptr left_expr_;
    AbstractExpr::Ptr right_expr_;

  public:
    // конструктор с параметрами-указателями на подвыражения
    SequenceExpr(AbstractExpr::Ptr left_expr, AbstractExpr::Ptr right_expr)
      : left_expr_(left_expr)
      , right_expr_(right_expr) {
      assert(left_expr and "Внутренняя ошибка при разборе РЕ вида A B");
    }

    // клонирует объект
    AbstractExpr::Ptr Clone() {
      return AbstractExpr::Ptr(new SequenceExpr(left_expr_, right_expr_));
    }


    // тип выражения
    ExprType Type() {
      return SEQUENCE_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // уровень выражения в дереве
    size_t Level() {
      size_t left_level_ = left_expr_->Level() + 1;
      size_t right_level_ = 1;
      if (right_expr_) {
        right_level_ = right_expr_->Level() + 1;
      }
      return left_level_ > right_level_ ? left_level_ : right_level_;
    }

    // генерирует автомат, соответствующий этому РЕ
    void GenerateNfa(Nfa& nfa);
  };

  // соответствует правилу RE --> RE*
  class RepetitionExpr : public AbstractExpr {
    AbstractExpr::Ptr expr_;
  public:
    RepetitionExpr(AbstractExpr::Ptr expr)
      : expr_(expr) {
      assert(expr and "Внутренняя ошибка при разборе РЕ вида A*");
    }

    // клонирует объект
    AbstractExpr::Ptr Clone() {
      return AbstractExpr::Ptr(new RepetitionExpr(expr_));
    }

    // тип выражения
    ExprType Type() {
      return REPETITION_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // уровень выражения в дереве
    size_t Level() {
      return expr_->Level() + 1;
    }

    // генерирует автомат, соответствующий этому РЕ
    void GenerateNfa(Nfa& nfa);
  };

  // соответствует правилам RE -->  [...] | "..." | любой символ
  class SymbolSetExpr : public AbstractExpr {
    // множество символов
    std::string buf_;

  public:
    // конструктор берет множество символов
    SymbolSetExpr(std::string symbols)
      : buf_(symbols_) {
    }

    // клонирование
    AbstractExpr::Ptr Clone() {
      return AbstractExpr::Ptr(new SymbolSetExpr(buf_));
    }

    // тип выражения
    ExprType Type() {
      return SYMBOL_SET_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // возвращает 0 т.к. это лист дерева
    size_t Level(){
      return 0;
    }

    // генерирует НКА в соответствии с данным выражением
    void GenerateNfa(Nfa& nfa) {
      nfa.add_state(1);
      nfa.add_state(2);

      if (buf_.empty()) {
        nfa.AddTransition(1, 0, 2);
      } else {
        for (std::string::const_iterator it = buf_.begin(); it != buf_.end(); ++it) {
          nfa.AddTransition(1, *it, 2);
        }
      }

      nfa.SetStartState(1);
      nfa.AddToAcceptSet(2);
    }
  };

  // соответствует правилам RE_ROOT --> { n } | { n, m } | { n, }
  class FiniteRepExpr : public AbstractExpr {
    AbstractExpr::Ptr expr_;

    // n в правиле грамматики
    size_t rep_cnt_low_;

    // m в правиле грамматики
    size_t rep_cnt_hight_;

    // какое правило мы используем? 2 - { n, m }, 1 - { n }
    enum RepExprType {
      BRACES_ONE = 1
      , BRACES_TWO
    };
    RepExprType expr_type_;

  public:
    // конструктор для правила RE_ROOT --> { n }
    FiniteRepExpr(AbstractExpr::Ptr expr, size_t rep_cnt)
      : expr_(expr)
      , rep_cnt_low_(rep_cnt)
      , expr_type_(BRACES_ONE) {
      assert(expr_ and "Внутренняя ошибка при разборе РЕ вида {n}");
    }

    // конструктор для рпавила RE_ROOT --> { n, m }
    FiniteRepExpr(AbstractExpr::Ptr expr, size_t rep_cnt_low, size_t rep_cnt_hight)
      : expr_(expr)
      , rep_cnt_low_(rep_cnt_low)
      , rep_cnt_hight_(rep_cnt_hight)
      , expr_type_(BRACES_TWO) {
      assert(expr_ and "Внутренняя ошибка при разборе РЕ вида {n,m}");
    }

    // клонирование
    AbstractExpr::Ptr Clone() {
      if (expr_type_ == BRACES_TWO) {
        return AbstractExpr::Ptr(new FiniteRepExpr( expr_, rep_cnt_low_, rep_cnt_hight_));
      } else {
        return AbstractExpr::Ptr(new FiniteRepExpr( expr_, rep_cnt_low_ ));
      }
    }

    // тип выражения
    ExprType Type() {
      return FINITE_REP_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // ноль, т.к. это лист
    size_t Level() {
      return expr_->Level() + 1;
    }

    // вспомогательная функция для генерации автомата
    void CloneFromOffset(Nfa& nfa, Nfa nfa_tmp, size_t times);

    // генерация НКА, соответствующего этому выражению
    void GenerateNfa(Nfa& nfa);
  };

  // соответствует правилу RE --> RE / RE
  class PredictionExpr : public AbstractExpr {
    // левое подвыражение
    AbstractExpr::Ptr left_expr_;

    // правое подвыражение
    AbstractExpr::Ptr right_expr_;

  public:
    // конструктор
    PredictionExpr(AbstractExpr::Ptr left_expr, AbstractExpr::Ptr right_expr)
      : left_expr_(left_expr)
      , right_expr_(right_expr) {
    }

    // клонирование
    AbstractExpr::Ptr Clone() {
      return new PredictionExpr(left_expr_, right_expr_);
    }

    // тип выражения
    ExprType Type() {
      return PREDICTION_EXPR;
    }

    // вывод выражения на консоль
    void Print();

    // уровень выражения в дереве
    size_t Level() {
      return (left_expr_->Level() > right_expr_->Level() ? left_expr_->Level() : right_expr_->Level()) + 1;
    }

    // генерация НКА, соответствующего этому выражению
    void GenerateNfa(Nfa& nfa);
  };
};

}

