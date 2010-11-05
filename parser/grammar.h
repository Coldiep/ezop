

#ifndef GRAMMAR_H__
#define GRAMMAR_H__

#include <vector>

#include "public_grammar.h"
#include "list.h"

namespace parser {

namespace private_{


}

/*!
 * \brief Класс представляет собой оптимизированную для целей синтаксического анализа реализацию грамматики.
 *
 * Все правила грамматики хранятся в одном массиве подряд в порядке перечисления слева направо. Сначала идет
 * символ в левой части правила, затем символы в правой части, а затем символ разделитель с идентификатором
 * kBadSymbolId.
 *
 */
class Grammar {
 public:
  typedef PublicGrammar::MapId RuleId;            //!< Тип идентификаторов правила.
  typedef PublicGrammar::MapId SymbolId;          //!< Тип идентификаторов символа.
  typedef parser::list<RuleId> RuleIdList;        //!< Тип списка правил грамматики.
  typedef std::vector<RuleIdList> NontermVector;  //!< Тип вектора списка правил.

  /*!
   * \brief Класс представляет кэш помеченных правил, используемых в операции predictor.
   *
   * Для каждого нетерминального символа грамматики вектор nonterm_vector_ содержит список правил,
   * в которых данный символ стоит в левой части, а также правил, которые наследуются из таких
   * правил следующим образом:
   *   Если символ A принадлежит грамматике, то включаем в список все правила вида A --> X1 X2 .. Xn,
   *   а также правила вида B --> Y1 Y2 ... Yk, если имеется правило A --> B ... для каждого нетерминала
   *   B. Это делаем рекурсивно до тех пор, пока в спимок можно добавить новые правила.
   */
  struct PredictCache {
    //! Вектор списков правил грамматики для каждого нетерминала.
    NontermVector nonterm_vector_;

    /*!
     * \brief Инициализация количеством нетерминальных символов грамматики.
     *
     * \param num_of_nonterms количество нетерминальных символов грамматики.
     */
    void Init( RuleId num_of_nonterms ) {
      nonterm_vector_.resize(num_of_nonterms);
    }

    /*!
     * \brief Возвращает ссылку на список правил для переданного нетерминала.
     *
     * \param   id          Идентификатор нетерминала.
     * \return  RuleIdList  Список правил грамматики для переданного нетерминала.
     */
    RuleIdList& GetSymRules( RuleId id ) {
      return nonterm_vector_[id];
    }

    /*!
     * \brief Добавление правила для данного нетерминала.
     *
     * \param sym_id  Идентификатор символа, для которого добавляется правило.
     * \param rule_id Идентификатор добавляемого правила.
     */
    void AddSymRule( SymbolId sym_id, RuleId rule_id ) {
      nonterm_vector_[sym_id].push_back(rule_id);
    }
  };

  typedef std::vector< int > int_table_t;

  //! Идентификатор "плохого символа" грамматики.
  static const SymbolId kBadSymbolId = 0;

  int        start_symbol_index_;      // start symbol index of grammar
  int        max_symbol_id_;          // max symbol id (it can be more then number of symbols)
  int        max_rule_id_;          // max rule id (it can be more then number of rules)
  int        min_symbol_id_;          // min symbol id
  int        min_rule_id_;          // min rule id
  int        num_of_terminals_;        // the number of terminals in the grammar
  int        num_of_nonterminals_;      // the number of nonterminals in the grammar
  int        num_of_rules_;          // the number of rules in the grammar
  int        rules_space_;          // the number of bytes neeeded to keep grammar rules as it is
                          // described in rules_ variable's comments
  
  int_table_t    symbols_;            // the vector contains grammar symbols ids (firstly go terminals
                          // after nonterminals. The first_nonterminal_number_ contains
                          // the index of the first nonterminal in the vector)

  int_table_t    rules_;              // the vector contains grammar rules (each rule in the vector 
                          // consists of lh symbol and list of rh symbols and -1 at the
                          // end as delimiter). To read the rule it is needed to remember
                          // the the index of lhs and read till the element of the vector
                          // is not -1.
                          
  int_table_t    sym_id_to_symbols_map_;      // map symbol ids to grammar symbols
  int_table_t    rule_to_index_map_;        // map rules to index of vector of rules
  int_table_t    index_to_rule_map_;        // map index of vector of rules to rules
  int_table_t    id_to_internal_rule_map_;    // map internal rules id to rules
  int_table_t    internal_rule_to_id_map_;    // map rules to internal rule ids

  PublicGrammar*  public_grammar_;        // the pointer to public grammar
  
  private_::predict_cache  predict_cache_;      // the prediction cache

public:
  grammar();
  
  bool      is_nonterminal( int _symbol_id ) const { return _symbol_id >= num_of_terminals_; }
  int        get_start_symbol() const { return start_symbol_index_; }
  
  int        get_symbol_by_id( int _id ) const { return sym_id_to_symbols_map_[ _id-min_symbol_id_ ]; }
  int        get_id_by_symbol( int _sym_num ) const { return symbols_[ _sym_num ]; }
  int        get_index_by_rule( int _rule_num ) const { return rule_to_index_map_[ _rule_num ]; }
  int        get_rule_by_index( int _index ) const { return index_to_rule_map_[ _index ]; }
  int        get_rule_by_internal_id( int _id ) const { return id_to_internal_rule_map_[ _id-min_rule_id_ ]; }
  int        get_internal_id_by_rule( int _rule ) const { return internal_rule_to_id_map_[ _rule ]; }
  
  int        get_num_of_terminals() const { return num_of_terminals_; }
  int        get_num_of_nonterminals() const { return num_of_nonterminals_; }
  
  int        get_lhs_of_rule( int _rule_num ) const { return rules_[ get_index_by_rule( _rule_num ) ]; }
  int        get_rhs_of_rule( int _rule_num, int _sym_ind ) const { return rules_[ get_index_by_rule(_rule_num ) + _sym_ind + 1 ]; }


  private_::rule_list_t&  get_sym_rules( int _sym_index ) { return predict_cache_.get_sym_rules( _sym_index ); }

  const char* get_symbol_name( int _sym_num ) const {
    return (public_grammar_->GetSymbolTable().find(symbols_[_sym_num ]))->second.name_;
  }

  void      initialize( PublicGrammar* public_grammar );
  

};

} // namespace parser

#endif // GRAMMAR_H__
