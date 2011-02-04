
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
public:
  //! Абстрактный интерфейс для взаимодействия с интерпретатором.
  struct Context {
    //! Виртуальный деструктор т.к. класс -- абстрактный интерфейс.
    virtual ~Context() {}
  };

private:
  /*!
   * \brief Тип, реализующий расширенную ситуацию Эрли.
   *
   * В классическом определении ситуация содержит помеченное правило грамматики (правило с
   * точкой где-то в правой части) и номер состояния Эрли, в котором эта ситуация была
   * порождена. Модифицированный алгоритм оперирует расширенными состояниями Эрли, в которые
   * добавлены еще указатели на ситуации, приведшие к добавлению данной ситуации в состояние.
   */
  struct Item {
    //! Структура для хранения пар (контекст, указатель на ситуацию "ниже").
    struct Rptr {
      Context*  context_; //!< Указатель на предоставляемый интерпретатором объект контекста.
      Item*     item_;    //!< Указатель на объект класса ситуации Эрли.

      //! Инициализация по умолчанию.
      Rptr()
        : context_(NULL)
        , item_(NULL)
      {}

      //! Инициализация всех полей.
      Rptr( Context* context, Item* item )
        : context_(contex)
        , item_(item)
      {}
    };

    //! Тип списка объектов Rptr.
    typedef parser::list<Rptr> Rptrs;

    // Элементы ситуации из классического определения.
    Grammar::RuleId rule_id_;     //!< Идентификатор правила.
    unsigned        rhs_pos_;     //!< Позиция метки в правой части правила.
    size_t          origin_;      //!< Номер состояния, в котором данная ситуация была порождена.

    // Элементы ситуации из расширенного определения.
    Item*           lptr_;        //!< Указатель на ситуацию, у которой метка стоит на символ левее.
    Rptrs           rptrs_;       //!< Список указателей на ситуации, которые послужили причиной сдвига метки.

    // Служебные поля.
    size_t          order_number_;//!< Порядковый номер данной ситуации в состоянии.
    size_t          state_number_;//!< Номер состояния, котроому принадлежит ситуация.

    /*!
     * \brief Печать содержимого ситуации.
     *
     * \param[in] grammar Указатель на объект грамматики, у которой берутся символьные имена элементов.
     * \param[in] out Поток для вывода.
     */
    void Print( Grammar* grammar, std::ostream& out );

    //! Оператор сравнения.
    bool operator==( const Item& rhs ) {
        return  rule_id_ == rhs.rule_id_
                and rhs_pos_ == rhs.rhs_pos_ 
                and origin_ == rhs.origin_ 
                and lptr_ == rhs.lptr_;
    }
  };

  //! Тип списка объектов ситуаций Эрли.
  typedef parser::list<Item*> ItemList;

  //! Диспетчер ситуаций Эрли, выделяет и освобождает память для ситуаций.
  struct ItemDispatcher {
    //! Тип блока ситуаций Эрли.
    typedef std::vector<Item> ItemBlock;

    //! Тип списка блоков ситуаций Эрли.
    typedef std::deque<ItemBlock> BlockList;

    BlockList block_list_;  //!< Список блоков ситуаций Эрли.
    size_t    block_size_;  //!< Размер блока.
    ItemList  free_list_;   //!< Список свободных ситуаций Эрли.
    size_t    block_pos_;   //!< Текущая позиция свободного элемента в блоке.

    /*!
     * \brief При инициализации передается размер репозитория.
     *
     * \param[in] sz размер репозитория в элементах.
     */
    ItemDispatcher( size_t sz )
      : block_size_(sz)
      , block_pos_(0)
    {}

    /*!
     * \brief Выделение памяти под ситуацию.
     *
     * \param[in] rule_id   Идентификатор правила для данной ситуации.
     * \param[in] dot       Позиция метки в правой части правила.
     * \param[in] origin    Номер состояния, в которое данная ситуация была первоначально добавлена.
     * \param[in] lptr      Указатель на ситуацию с меткой на символ левее.
     */
    inline Item* GetItem( Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr ) {
      Item* item = NULL;

      // Проверяем свободную память.
      if (not free_list_.empty()) {
        item = free_list_.back();
        free_list_.pop_back();
      } else {
        // Выделяем память для ситуации, если это необходимо.
        if (block_pos_ >= block_size_ or block_list_.empty()) {
          block_list_.push_back(ItemBlock());
          block_list_[block_list_.size()-1].resize(block_size_);
          block_pos_ = 0;
        }

        item =  block_list_[block_list_.size()-1][block_pos_++];
      }

      item->rule_id_  = rule_id;
      item->rhs_pos_  = dot;
      item->origin_   = origin;
      item->lptr_     = lptr;

      return item;
    }

    /*!
     * \brief Освободить память, используемую под ситуацию.
     *
     * \param[in] item Указатель на ситуацию.
     */
    void FreeItem( Item* item ) {
      free_list_.push_front(item);
      item->
    }
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
     * Кроме, собственно, списка ситуаций, структура содержит поле handled_by_predictor_, которое
     * позволяет не обрабатывать несколько раз одну и ту ситуацию операцией Predictor.
     */
    struct SymbolItemList {
      ItemList  elems_;                 //!< Список ситуаций.
      bool      handled_by_predictor_;  //!< Флаг обработки операцией Predictor.

      //! Конструктор по умолчанию необходим для стандартных контейнеров.
      SymbolItemList();

      //! Аналог деструктора, используется т.к. память реально не освобождается.
      void Uninit( ItemDispatcher* disp );
    };

    //! Индексированный список списков ситуаций для каждого символа.
    typedef std::vector<SymbolItemList>  ItemVector;

    ItemVector      items_;                  //!< Список ситуаций для каждого символа грамматики.
    ItemVector      items_with_empty_rules_; //!< Список ситуаций для правил с пустой правой частью.
    ItemList        state_items_;            //!< Список ситуаций в порядке их добавления в состояние.
    size_t          num_of_items_;           //!< Число ситуаций в состоянии.
    bool            is_completed_;           //!< Флаг того, что состояние содержит ситуацию вида [S--> alpha *, 0, ...].
    size_t          id_;                     //!< Уникальный идентификатор данного состояния.
    Token           token_;                  //!< Токен, послуживший инициатором создания этого состояния.
    Grammar*        grammar_;                //!< Указатель на объект грамматики.
    ItemDispatcher* disp_;                   //!< Указатель на объект диспетчера ситуаций.
    bool            valid_;                  //!< Установлен в true, если состояние рабочее.

    //! Конструктор по умолчанию.
    State();

    //! Деструктор.
    ~State();

    /*!
     * \brief Инициализация состояния.
     *
     * \param[in] disp    Указатель на объект диспетчера ситуаций.
     * \param[in] grammar Указатель на объект грамматики.
     * \param[in] id      Уникальный идентификатор состояния.
     * \param[in] token   Токен для данного состояния.
     */
    void Init( ItemDispatcher* disp, Grammar* grammar, size_t id, Token token );

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
     * \param[in] context   Указатель на контекст интерпретатора.
     * \param[in] error     Уровень ошибки, если анализатор работает в режиме восстановления после ошибок.
     */
    inline Item* AddItem( Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr, Item* rptr, Context* context, unsigned char error = 0 );

    //! Печать содержимого состояния.
    void Print( std::ostream& out );
  };

  //! Диспетчер состояний, управляет временем жизни состояний.
  struct StateDispatcher {
    //! Тип репозитория состояний.
    typedef std::deque<State> StateRepo;

    //! Тип пары (указатель на объект состояния, индекс состояния).
    typedef std::pair<State*, size_t> StatePnt;

    //! Тип списка состояний.
    typedef parser::list<StatePnt> StateList;

    StateRepo       repo_;        //!< Репозиторий состояний.
    StateList       free_states_; //!< Список свободных состояний.
    ItemDispatcher* disp_;        //!< Указатель на объект диспетчера ситуаций.
    Grammar*        grammar_;     //!< Указатель на объект грамматики.

    /*!
     * \brief Конструктор репозитория.
     *
     * \param disp    Указатель на объект диспетчера ситуаций.
     * \param grammar Указатель на объект грамматики.
     */
    StateRepo( ItemDispatcher* disp, Grammar* grammar )
      : disp_(disp)
      , grammar_(grammar)
    {}

    /*!
     * \brief Получение состояния по индексу.
     *
     * \param[in] index Индекс состояния.
     * \return          Указатель на объект состояния или нуль.
     */
    State* GetState( size_t index ) {
      if (index >= repo_.size()) {
        return NULL;
      }

      if (repo_[index].valid_) {
        return &repo_[index];
      }

      return NULL;
    }

    /*!
     * \brief Создание нового состояния.
     * 
     * \return Индекс нового состояния.
     */
    size_t AddState( Token token ) {
      if (not free_states_.empty()) {
        size_t index = free_states_.back().second;
        State* state = free_states_.back().first;
        state->Init(disp_, grammar_, token);
        free_states_.pop_back();
        return index;
      }

      repo_.push_back(State());
      repo.back().Init(disp_, grammar_, token);
      return repo_.size() - 1;
    }
  };

  typedef queue<Item*> ItemQueue;

  StateDispatcher  state_disp_;       //!< Диспетчер состояний.
  ItemDispatcher   item_disp_;        //!< Диспетчер ситуаций.
  Grammar*         grammar_;          //!< Указатель на объект грамматики.
  Lexer*           lexer_;            //!< Указатель на объект лексического анализатора.
  ItemQueue        nonhandled_items_; //!< Очередь необработанных ситуаций.

  /*!
   * \brief Реализация операции Completer.
   *
   * \param[in] item Ситуация, которую необходимо обработать.
   */
  inline void Completer( Item* item );

  /*!
   * \brief Реализацию операции Predictor.
   *
   * \param[in] item Ситуация, которую необходимо обработать.
   */
  inline void Predictor( Item* item );

  /*!
   * \brief реализация процедуры Scanner.
   *
   * \return true если в результате было добавлено хотя бы одно новое состояние.
   */
  inline bool Scanner();

  //! Итеративное выполнение операций Completer и Predictor.
  inline void Closure();

  //! Инициализация начального множества.
  inline bool InitFirstSet();

  /*!
   * \brief 
   *
   *
   *
   */
  inline void PutItemToNonhandledlist( Item* item, bool );  // put the item to the unhandled items list
                                      // if it is not there already
                                      //
  /*!
   * \brief Проверка на присутствие состояния в переданном списке.
   *
   *
   */
  inline bool IsItemInList( State::SymbolItemList&, Item*, Item* );

public:
  EarleyParser( Grammar* grammar, Lexer* lexer );
  ~EarleyParser();

  bool Parse();          // the parsing of the input string
  void Reset();          // reset parser object to init state

  void Print( std::ostream& );      // print the items
};

} // namespace parser

#endif // EARLEY_PARSER_H__

