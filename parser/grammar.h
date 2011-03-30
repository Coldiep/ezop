
#ifndef GRAMMAR_H__
#define GRAMMAR_H__

#include <vector>

#include "public_grammar.h"
#include "list.h"

namespace parser {

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

  typedef std::vector<SymbolId> SymbolIdTable;  //!< Тип таблицы символов.
  typedef std::vector<RuleId>   RuleIdTable;    //!< Тип таблицы правил.

  //! Идентификатор "плохого символа" грамматики.
  static const SymbolId kBadSymbolId = 0;

  SymbolId  start_symbol_index_;  //!< Индекс начального нетерминала грамматики.
  SymbolId  max_symbol_id_;       //!< Индекс символа грамматики с максимальным значением.
  RuleId    max_rule_id_;         //!< Индекс правила грамматики с максимальным значением.
  SymbolId  min_symbol_id_;       //!< Индекс символа грамматики с минимальным значением.
  RuleId    min_rule_id_;         //!< Индекс правила грамматики с минимальным значением.
  SymbolId  num_of_terminals_;    //!< Количество терминальных символов грамматики.
  SymbolId  num_of_nonterminals_; //!< Количество нетерминальных символов грамматики.
  RuleId    num_of_rules_;        //!< Количество правил грамматики.
  size_t    rules_space_;         //!< Размер буфера для того, чтобы хранить правила грамматики.

  SymbolIdTable symbols_; //!< Таблица символов грамматики, сначала следуют терминалы, после чего нетерминалы.
  RuleIdTable   rules_;   //!< Вектор правил грамматики, как описано в комментариях к классу.

  SymbolIdTable external_to_internal_symbols_map_; //!< Идентификаторы PublicGrammar --> внутренние идентификаторы символов.
  RuleIdTable   rule_to_offset_map_;               //!< Для идентификатора правила запоминаем его смещение в буфере правил.
  RuleIdTable   offset_to_rule_map_;               //!< Для смещения в буфере правил запоминаем его идентификатор.
  RuleIdTable   id_to_internal_rule_map_;          //!< Внутренние идентфикаторы правил --> идентфикаторы PublicGrammar.
  RuleIdTable   internal_rule_to_id_map_;          //!< Идентфикаторы правил PublicGrammar --> внутренние идентфикаторы.

  const PublicGrammar*  public_grammar_;  //!< Указатель на объект интерфейсной грамматики.
  PredictCache          predict_cache_;   //!< Кэш для операции Predictor.

public:
  /*!
   * \brief Конструктор инициализируются объектом PublicGrammar.
   *
   * \param[in] public_grammar Указатель на объект PublicGrammar.
   */
  explicit Grammar( const PublicGrammar* public_grammar );

  //! Проверка, является ли данный символ нетерминальным.
  bool IsNonterminal( SymbolId id ) const { return id > num_of_terminals_; }

  //! Получение начального нетерминала грамматики.
  SymbolId GetStartSymbol() const { return start_symbol_index_; }

  //! Возвращение идентификатора символа по его индексу.
  SymbolId GetInternalSymbolByExtrernalId( SymbolId id ) const { return external_to_internal_symbols_map_[id - min_symbol_id_]; }

  //! Возвращение внутреннего идентификатора по внешнему.
  SymbolId GetIdBySymbol( SymbolId sym_num ) const { return symbols_[sym_num]; }

  //! Получение смещения правила в буфере по его индексу.
  size_t GetOffsetByRule( RuleId rule_num ) const { return rule_to_offset_map_[rule_num]; }

  //! Получение идентификатора правила по смещению.
  RuleId GetRuleByOffset( size_t offset ) const { return offset_to_rule_map_[offset]; }

  //! Получение идентфикатора правила по внутреннему идентфикатору.
  RuleId GetRuleByInternalId( RuleId id ) const { return id_to_internal_rule_map_[id - min_rule_id_]; }

  //! Получение внутреннего идентификатора по внешнему.
  RuleId GetInternalIdByRule( RuleId id ) const { return internal_rule_to_id_map_[id]; }

  //! Получение количества терминальных символов грамматики.
  SymbolId GetNumOfTerminals() const { return num_of_terminals_; }

  //! Получение количества нетерминальных символов грамматики.
  SymbolId GetNumOfNonterminals() const { return num_of_nonterminals_; }
  
  //! Получение идентификатора символа левой части правила.
  SymbolId GetLhsOfRule( RuleId id ) const { return rules_[GetOffsetByRule(id)]; }

  //! Получение идентификатора символа в правой части правила.
  SymbolId GetRhsOfRule( RuleId rule_id, SymbolId rhs_num ) const { return rules_[GetOffsetByRule(rule_id) + rhs_num + 1]; }

  //! Получить список правил для данного символ из кэша Predictor.
  RuleIdList&  GetSymRules( SymbolId id ) { return predict_cache_.GetSymRules(id); }

  //! Получить имя символа.
  const char* GetSymbolName( SymbolId id ) const {
    return (public_grammar_->GetSymbolTable().find(symbols_[id]))->second.name_;
  }

  //! Инициалиизация грамматики -- преобразование из PublicGrammar.
  void Initialize();
};

} // namespace parser

#endif // GRAMMAR_H__
