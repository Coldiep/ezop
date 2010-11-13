
/**************************************************************************************************************
  Earley's algorithm implementation declarations.
**************************************************************************************************************/


#ifndef EARLEY_PARSER_H__
#define EARLEY_PARSER_H__

#include "grammar.h"
#include "list.h"
#include "queue.h"
#include "stack.h"
#include "tree.h"
#include "lexer.h"
#include "allocator.h"
#include "ast.h"

#include <vector>
#include <deque>
#include <iostream>

//#define PRINT_STATS

namespace parser {

/*!
 * \brief Реализация алгоритма Эрли, модифицированного для обработки неоднозначностей.
 *
 * Класс реализует модификацию классического алгоритма Эрли, сделанную для корректного
 * построения всевозможных деревьев порождения для данной грамматики и данной входной
 * цепочки. Модификация заключается в расширении структуры ситуации Эрли и алгоритме
 * построения всевозможных деревьев порождения для данной цепочки.
 */
class EarleyParser {
  //! Тип списка ситуаций Эрли.
  typedef parser::list<struct Item*> ItemList;

  /*!
   * \brief Тип, реализующий расширенную ситуацию Эрли.
   *
   * В классическом определении ситуация содержит помеченное правило грамматики (правило с
   * точкой где-то в правой части) и номер состояния Эрли, в котором эта ситуация была
   * порождена. Модифицированный алгоритм оперирует расширенными состояниями Эрли, в которые
   * добавлены еще указатели на ситуации, приведшие к добавлению данной ситуации в состояние.
   */
  struct Item {
    // Элементы ситуации из классифеского определения.
    Grammar::RuleId rule_id_; //!< Идентификатор правила.
    unsigned        rhs_pos_; //!< Позиция метки в правой части правила.
    size_t          origin_;  //!< Номер состояния, в котором данная ситуация была порождена.

    // Элементы ситуации из расширенного определения.
    Item*     lptr_;  //!< Указатель на ситуацию, у которой метка стоит на символ левее.
    ItemList  rptrs_; //!< Список указателей на ситуации, которые послужили причиной сдвига метки.

    // Служебные поля.
    unsigned char error_;       //!< Номер ошибки, если анализатор работает в режиме восстановления после ошибок.
    bool          handled_;     //!< Флаг обработки ситуации при проходе построении дерева.
    size_t        order_number_;//!< Порядковый номер данной ситуации в состоянии.
    size_t        state_number_;//!< Номер состояния, котроому принадлежит ситуация.

    /*!
     * \brief Печать содержимого ситуации.
     *
     * \param[in] grammar Указатель на объект грамматики, у которой берутся символьные имена элементов.
     * \param[in] out Поток для вывода.
     */
    void Print( Grammar* grammar, std::ostream& out );

    //! Оператор сравнения.
    bool operator==( const item& rhs );
  };

  /*!
   * \brief Реализация состояния Эрли.
   *
   * Состояние Эрли представляет собой список ситуаций Эрли. В целях оптимизации этот список разделен
   * на несколько списков, по собственному списку для каждого символа грамматики. Это позволяет
   * оптимизировать поиск ситуаций для операций Scanner и Completer.
   */
  struct State {
    /*!
     * \brief Реализация списка ситуаций с меткой перед конкретным символом.
     *
     * Кроме, собственно, спсика ситуаций, структура содержит поле handled_by_predictor_, которое
     * позволяет не обрабатывать несколько раз одну и ту ситуацию операцией Predictor.
     */
    struct SymbolItemList {
      ItemList  elems_;                 //!< Список ситуаций.
      bool      handled_by_predictor_;  //!< Флаг обработки операцией Predictor.

      //! Конструктор по умолчанию необходим для стандартных контейнеров.
      SymbolItemList();

      //! Аналог деструктора, используется т.к. память реально не освобождается.
      void Uninit( EarleyParser* parser );
    };

    //! Индексированный список списков ситуаций для каждого символа.
    typedef std::vector<SymbolItemList>  ItemVector;

    ItemVector items_;                  //!< Список ситуаций для каждого символа грамматики.
    ItemVector items_with_empty_rules_; //!< Список ситуаций для правил с пустой правой частью.
    ItemList   state_items_;            //!< Список ситуаций в порядке их добавления в состояние.
    size_t     num_of_items_;           //!< Число элементов в состоянии.
    size_t     state_number_;           //!< Номер данного состояния.
    bool       is_completed_;           //!< Флаг того, что состояние содержит ситуацию вида [S--> alpha *, 0, ...].
    Token      token_;                  //!< Токен, послуживший инициатором создания этого состояния.
    Grammar*   grammar_;                //!< Указатель на объект грамматики.
    EarleyParser* parser_;              //!< Указатель на объект парсера.

    //! Конструктор по умолчанию.
    State();

    //! Деструктор.
    ~State();

    /*!
     * \brief Инициализация состояния.
     *
     * \param[in] parser  Указатель на объект парсера.
     * \param[in] grammar Указатель на объект грамматики.
     * \param[in] id      Номер состояния.
     * \param[in] token   Токен для данного состояния.
     */
    void Init( EarleyParser* parser, Grammar* grammar, size_t id, Token token );

    //! Деинициализация состояния.
    void Uninit();

    /*!
     * \brief Добавление новой систуации в состояние.
     *
     * \param[in] rule_id   Идентификатор правила для данной ситуации.
     * \param[in] dot       Позиция метки в правой части правила.
     * \param[in] origin    Номер состояния, в которое данная ситуация была первоначально добавлена.
     * \param[in] lptr      Указатель на ситуацию с меткой на символ левее.
     * \param[in] rptr      Указатель на ситуацию, послужившую причиной сдвига нетерминала слева от метки.
     * \param[in] error     Уровень ошибки, если анализатор работает в режиме восстановления после ошибок.
     */
    inline Item* AddItem( Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr, Item* rptr, unsigned char error = 0 );

    //! Печать содержимого состояния.
    void Print( std::ostream& out );
  };

  //! Стек для элементов, используемый при анализе.
  struct RhsStackElement {
    //! Элемент может быть ситуацией, списком указателей на ситуации или символом.
    enum RhsStackType {
      kItem = 1,//!< Ситуация.
      kRptrs,   //!< Список ссылок на ситуации.
      kSymbol   //!< Символ грамматики.
    };

    //! Тип элемента списка.
    RhsStackType type_;

    //! Данные элемента.
    union {
      Item*             item_;  //!< Указатель на элемент.
      ItemList*         rptrs_; //!< Указатель на список элементов.
      Grammar::SymbolId symbol_;//!< Символ.
    };
  };

  //! Тип элемента дерева порождения.
  struct ParseTreeElement {
    // typedefs
    typedef parser::tree_node<unsigned>       TreeNode;
    typedef parser::tree<unsigned, TreeNode>  Tree;

    Tree*     tree_; // the pointer to the
    TreeNode* node_;

    ParseTreeElement( Tree* tree, TreeNode* node )
      : tree_(tree)
      , node_(node)
    {}

    ParseTreeElement()
      : tree_(NULL)
      , node_(NULL)
    {}
  };

  typedef queue<Item*>                          ItemQueue;
  typedef parser::stack<RhsStackElement>        RhsStack;
  typedef std::deque<State*>                    StateVector;
  typedef tree_node<unsigned>                   ParseTreeNode;
  typedef parser::tree<ParseTreeNode::tree_node_element_t, ParseTreeNode> ParseTree; 
  typedef parser::list<ParseTree*>              ParseTreeList;
  typedef parser::list<ParseTreeElement>        ParseTreeNodeStack;

//////////////////////////////////////////////////////////////////////////
// friend declarations
//////////////////////////////////////////////////////////////////////////

  friend struct State;
  friend struct State::SymbolItemList;
  friend struct Item;

//////////////////////////////////////////////////////////////////////////
// variables
//////////////////////////////////////////////////////////////////////////

  ItemQueue        nonhandled_items_;      // the queue of unhandled items
  StateVector      states_;          // the states of the algorithm
  Grammar*          grammar_;          // the CF grammar
  Lexer*            lexer_;            // the lexical analyzer
  parser::allocator<Item>  items_pool_;        // the pool of items
  ParseTreeList     parse_tree_list_;      // the list of parse trees
  unsigned          max_error_value_;      // the maximum error value
  Token             cur_token_;          // the current token

//////////////////////////////////////////////////////////////////////////
// methods
//////////////////////////////////////////////////////////////////////////

  inline void Completer( Item* item );      // the Completer algorithm operation
  inline void Predictor( Item* item );      // the Predictor algorithm operation
  inline bool Scanner();          // the Scanner algorithm operation

  inline bool ErrorScanner();      // the function runs when scanner method is erred

  inline void Closure();          // the Closure algorithm operation
  inline bool InitFirstSet();      // the first set initialization

  inline void PutItemToNonhandledlist( Item* item, bool );  // put the item to the unhandled items list
                                      // if it is not there already
  inline bool IsItemInList( State::SymbolItemList&, item*, item* );

  inline void FillRhsStack( Item*, RhsStack& );  // fill rhs stack ot the passed item

  void BuildParseTrees( State& );
  inline void BuildParseTrees( Item*, ParseTreeNode*, ParseTree& );

  void BuildAstTrees( State& );
  inline void BuildAstTreesRecursive( Item*, ParseTreeNode*, ParseTree& );
  void BuildAstTreesIterative( Item*, ParseTreeNode*, ParseTree& );

  void PrintTree( ParseTreeNode*, unsigned, std::ostream& );
  void PrintTree( ParseTree*, std::ostream& );

public:
  Earleyparser( Grammar* grammar, Lexer* lexer, unsigned = 3 );
  ~EarleyParser();

  bool Parse();          // the parsing of the input string
  void Reset();          // reset parser object to init state 

  void Print( std::ostream& );      // print the items
  void PrintTrees( std::ostream& );  // print parse trees
};

} // namespace parser

#endif // EARLEY_PARSER_H__

