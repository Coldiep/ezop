#ifndef PUBLIC_GRAMMAR_H__
#define PUBLIC_GRAMMAR_H__

#include <list>
#include <map>
#include <iostream>
#include <stdint.h>

namespace parser {

/*!
 * \brief Класс представляет интерфейсную грамматику парсера.
 *
 * Парсер использует оптимизированную для алгоритма синтаксического анализа
 * грамматику, определенную в классе Grammar. Эта грамматика генерируется
 * из ообъекта класса PublicGrammar. Таким образом, класс PublicGrammar
 * предоставляет интерфейс пользователю для добавления элементов в грамматику,
 * но добавленные элементы должны быть преобразованы во внутренний оптимальный
 * формат перед запуском алгоритма синтаксического анализа.
 */
class PublicGrammar {
public:

  //! Тип идентфикатора словаря символов и правил грамматики.
  typedef uint32_t MapId;

  //! Тип списка идентификаторов словаря.
  typedef std::list<MapId> MapIdList;

  //! Значение неккоректного идентифкатора, заразервированного для внутренних целей.
  static const MapId kUnknownMapId = 0;

  /*!
   * \brief Класс, представляющий символ грамматики.
   *
   * Символ характеризуется именем, а также признаком того, является ли он
   * нетерминальным или нет. Каждый символ имеет уникальный идентификатор,
   * который является в словаре символов, хранящемся в грамматике.
   */
  struct Symbol {
    const char* name_;        //!> Имя символа в человекочитаемом виде.
  const char* regex_;       //!> Регулярное выражение для терминального символа.
    bool        nonterminal_; //!< Признак того, что данный символ нетерминал.

    //! Конструктор по умолчанию.
    Symbol()
      : name_(NULL)
    {}

    //! Инициализация без регулярного выражения.
    Symbol( const char* name, bool nonterminal )
      : name_(name)
      , nonterminal_(nonterminal)
    {
    if (!nonterminal)
      regex_ = name;
  }
  //! Полная инициализация.
  Symbol( const char* name, bool nonterminal , const char* regex)
      : name_(name)
      , nonterminal_(nonterminal),
    regex_(regex)
    {}
  };
  /*!
   * \brief Класс, представляющий правило контекстно-свободной грамматики.
   *
   * Правило содержит имя в читабельном для человка виде, а также
   * идентификатор символа в левой части правила и список идентификаторов
   * символов в правой части правила. Более конкретно:
   *  A --> X1 X2 ... Xn
   *  представляется в виде:
   *    name_: A --> X1 X2 ... Xn
   *    lhs_symbol_: MapId(A)
   *    rhs_list_: MapId(X1), MapId(X2), ..., MapId(Xn).
   */
  struct Rule {
    const char* name_;        //!< Имя правила в читабельном для человека виде.
    int         lhs_symbol_;  //!< Идентификатор символа в левой части правила.
    MapIdList   rhs_list_;    //!< Список идентфикаторов символов в правой части правила.

    /*!
     * \brief Инициализация по умолчанию.
     *
     * Идентификатор в левой части правила инициализируется в нулем который представляет
     * значение, зарезервированное для того, чтобы держать инфомацию о некорректном
     * идентификаторе.
     */
    Rule()
      : name_(NULL)
      , lhs_symbol_(kUnknownMapId)
    {}

    /*!
     * \brief Обычная инициализация.
     *
     * \param name Название правила в читабельном для человека виде.
     */
    explicit Rule( const char* name )
      : name_(name)
      , lhs_symbol_(kUnknownMapId)
    {}
  };

  //! Тип словаря символов грамматики.
  typedef std::map<MapId, Symbol>    SymbolTable;

  //! Тип словаря правил грамматики.
  typedef std::map<MapId, Rule>      RuleTable;

private:
  SymbolTable    symbols_; //!< Словарь символов грамматики.
  RuleTable      rules_;   //!< Словарь правил грамматики.

  const char*    grammar_name_;     //!< Название грамматики в читабельном для человека виде.
  MapId          max_sym_id_;       //!< Максимальное значение идентификатора символа.
  MapId          max_rule_id_;      //!< Максимальное значение идентификатора правила.
  MapId          min_sym_id_;       //!< Минимальное значение идентификатора символа.
  MapId          min_rule_id_;      //!< Минимальное значение идентификатора правила.
  MapId          num_of_terms_;     //!< Число терминальных символов в грамматике.
  MapId          num_of_nonterms_;  //!< Число нетерминальных символов в грамматике.
  MapId          start_symbol_;     //!< Идентификатор начального нетерминала грамматики.

public:
  /*!
   * /brief Инициализация грамматики.
   *
   * \param desc Описание грамматики в удобном для человека виде.
   */
  explicit PublicGrammar( const char* desc );

  //------------------- Методы для добавления элементов в грамматику --------------------------

  /*!
   * \brief Добавление терминального символа.
   *
   * \param id    Идентификатор символа.
   * \param name  Имя символа для человека.
   */
  void AddTerminal( MapId id, const char* name );

  /*!
   * \brief Добавление терминального символа.
   *
   * \param id    Идентификатор символа.
   * \param name  Имя символа для человека.
   * \param regex Регулярное выражение для символа.
   */
  void AddTerminal( MapId id, const char* name, const char* regex );

  /*!
   * \brief Добавление нетерминального символа.
   *
   * \param id    Идентификатор символа.
   * \param name  Имя символа для человека.
   */
  void AddNonterminal( MapId id, const char* name );

  /*!
   * \brief Добавление правила.
   *
   * \param id    Идентификатор правила.
   * \param name  Имя символа для человека.
   */
  void AddRule( MapId id, const char* name );

  /*!
   * \brief Добавление символа в левой части правила.
   *
   * \param rule_id Идентификатор правила.
   * \param sym_id  Идентификатор символа.
   */
  void AddLhsSymbol( MapId rule_id, MapId sym_id );

  /*!
   * \brief Добавление символа в правой части правила.
   *
   * \param rule_id Идентификатор правила.
   * \param sym_id  Идентификатор символа.
   */
  void AddRhsSymbol( MapId rule_id, MapId sym_id );

  /*!
   * \brief Установка идентификатора начального нетерминала грамматики.
   *
   * \param id Идентификатор начального нетерминала.
   */
  void SetStartSymbolId( MapId id ) { start_symbol_ = id; }

  //------------------------- Методы для получения информации о содержимом грамматики ----------------------------

  //! Получение количества терминальных символов граммматики.
  MapId GetNumOfTerminals() const { return num_of_terms_; }

  //! Получение количества нетерминальных символов граммматики.
  MapId GetNumOfNonterminals() const { return num_of_nonterms_; }

  //! Получение максимального значения идентификатора символа.
  MapId GetMaxSymbolId() const { return max_sym_id_; }

  //! Получение максимального значения идентификатора правила.
  MapId GetMaxRuleId() const { return max_rule_id_; }

  //! Получение минимального значения идентификатора символа.
  MapId GetMinSymbolId() const { return min_sym_id_; }

  //! Получение минимального значения идентификатора правила.
  MapId GetMinRuleId() const { return min_rule_id_; }

  //! Получение разности между максимальным и минимальным значениями идентификатором символов.
  MapId GetSymbolIdInterval() const { return max_sym_id_ - min_sym_id_; }

  //! Получение разности между максимальным и минимальным значениями идентификатором правил символов.
  MapId GetRuleIdInterval() const { return max_rule_id_ - min_rule_id_; }
  
  //! Получение таблицы символов.
  const SymbolTable& GetSymbolTable() const { return symbols_; }

  //! Получение таблицы правил.
  const RuleTable& GetRuleTable() const { return rules_; }

  //! Получение идентификатора начального нетерминала грамматики.
  MapId GetStartSymbolId() const { return start_symbol_; }

  /*!
   * \brief Печать содержимого грамматики.
   *
   * \param out Поток, в который будет производиться печать.
   */
  void Print( std::ostream& out );
};

}  // namespace parser

#endif // PUBLIC_GRAMMAR_H__

