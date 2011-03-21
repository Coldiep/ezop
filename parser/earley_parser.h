
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
        : context_(context)
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

#   ifdef DUMP_CONTENT
    /*!
     * \brief Печать содержимого ситуации.
     *
     * \param[in] grammar Указатель на объект грамматики, у которой берутся символьные имена элементов.
     * \param[in] out Поток для вывода.
     */
    void Dump( Grammar* grammar, std::ostream& out );
#   endif // DUMP_CONTENT

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

        item = &block_list_[block_list_.size()-1][block_pos_++];
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
      SymbolItemList()
        : handled_by_predictor_(false) {
      }

      //! Аналог деструктора, используется т.к. память реально не освобождается.
      void Uninit( ItemDispatcher* disp ) {
        while (not elems_.empty()) {
          disp->FreeItem(elems_.pop_back());
        }
        handled_by_predictor_ = false;
      }
    };

    //! Индексированный список списков ситуаций для каждого символа.
    typedef std::vector<SymbolItemList>  ItemVector;

    ItemVector      items_;                  //!< Список ситуаций для каждого символа грамматики.
    ItemVector      items_with_empty_rules_; //!< Список ситуаций для правил с пустой правой частью.
    ItemList        state_items_;            //!< Список ситуаций в порядке их добавления в состояние.
    size_t          num_of_items_;           //!< Число ситуаций в состоянии.
    bool            is_completed_;           //!< Флаг того, что состояние содержит ситуацию вида [S--> alpha *, 0, ...].
    size_t          id_;                     //!< Уникальный идентификатор данного состояния.
    Token::Ptr      token_;                  //!< Токен, послуживший инициатором создания этого состояния.
    ItemDispatcher* disp_;                   //!< Указатель на объект диспетчера ситуаций.
    Grammar*        grammar_;                //!< Указатель на объект грамматики.
    bool            valid_;                  //!< Установлен в true, если состояние рабочее.

    //! Конструктор по умолчанию.
    State()
      : num_of_items_(0)
      , is_completed_(false)
      , id_(0)
      , disp_(NULL)
      , grammar_(NULL)
      , valid_(false)
    {}

    /*!
     * \brief Инициализация состояния.
     *
     * \param[in] disp    Указатель на объект диспетчера ситуаций.
     * \param[in] grammar Указатель на объект грамматики.
     * \param[in] id      Уникальный идентификатор состояния.
     * \param[in] token   Токен для данного состояния.
     */
    void Init(ItemDispatcher* disp, Grammar* grammar, size_t id, Token::Ptr token) {
      num_of_items_ = 0;
      is_completed_ = false;
      id_           = id;
      token_        = token;
      disp_         = disp;
      grammar_      = grammar;
      valid_        = true;

      // Число списков для символов -- это число символов + 1 для метки в конце правила. Для этого
      // специального случая используется список с нулевым индексом.
      items_.resize(grammar_->GetNumOfTerminals() + grammar_->GetNumOfNonterminals() + 1);

      // Ситуаций для правил с пустой правой частью ровно столько, сколько нетерминальных символов в грамматике.
      items_with_empty_rules_.resize(grammar_->GetNumOfNonterminals());
    }

    //! Деинициализация состояния.
    void Uninit() {
      for (size_t i = 0; i < items_.size(); ++i) {
        items_[i].Uninit(disp_);
      }
      items_.clear();

      for (size_t i = 0; i < items_with_empty_rules_.size(); ++i) {
        items_with_empty_rules_[i].Uninit(disp_);
      }
      items_with_empty_rules_.clear();

      num_of_items_ = 0;
      is_completed_ = false;
      id_           = 0;
      token_        = Token::Ptr(new Token());
      disp_         = NULL;
      grammar_      = NULL;
      valid_        = false;
    }

    /*!
     * \brief Добавление новой систуации в состояние.
     *
     * \param[in] parser    Указатель на объект парсера.
     * \param[in] rule_id   Идентификатор правила для данной ситуации.
     * \param[in] dot       Позиция метки в правой части правила.
     * \param[in] origin    Номер состояния, в которое данная ситуация была первоначально добавлена.
     * \param[in] lptr      Указатель на ситуацию с меткой на символ левее.
     * \param[in] rptr      Указатель на ситуацию, послужившую причиной сдвига нетерминала слева от метки.
     * \param[in] context   Указатель на контекст интерпретатора.
     */
    inline Item* AddItem( EarleyParser* parser, Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr, Item* rptr, Context* context );

#   ifdef DUMP_CONTENT
    //! Печать содержимого состояния.
    void Dump( std::ostream& out ) {
      out << "\n****** State number = " << id_ << " *** Number of items = " << num_of_items_ << " ******\n";
      for (Item* item = state_items_.get_first(); item; item = state_items_.get_next()) {
        item->Dump(grammar_, out);
      }
    }
#   endif // DUMP_CONTENT
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
    StateDispatcher(ItemDispatcher* disp, Grammar* grammar)
      : disp_(disp)
      , grammar_(grammar)
    {}

    /*!
     * \brief Получение состояния по индексу.
     *
     * \param[in] index Индекс состояния.
     * \return          Указатель на объект состояния или нуль.
     */
    State* GetState(size_t index) {
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
    size_t AddState(Token::Ptr token) {
      if (not free_states_.empty()) {
        size_t id = free_states_.back().second;
        State* state = free_states_.back().first;
        state->Init(disp_, grammar_, id, token);
        free_states_.pop_back();
        return id;
      }

      repo_.push_back(State());
      size_t id = repo_.size() - 1;
      repo_[id].Init(disp_, grammar_, id, token);
      return id;
    }
  };

  typedef queue<Item*> ItemQueue;
  typedef parser::list<size_t> StateList;

  Grammar*          grammar_;          //!< Указатель на объект грамматики.
  Lexer*            lexer_;            //!< Указатель на объект лексического анализатора.
  StateDispatcher   state_disp_;       //!< Диспетчер состояний.
  ItemDispatcher    item_disp_;        //!< Диспетчер ситуаций.
  ItemQueue         nonhandled_items_; //!< Очередь необработанных ситуаций.

  /*!
   * \brief Реализация операции Completer.
   *
   * \param[in] state_id  Идентификатор состояния, которому принадлежит ситуация.
   * \param[in] item      Ситуация, которую необходимо обработать.
   */
  inline void Completer(size_t state_id, Item* item);

  /*!
   * \brief Реализацию операции Predictor.
   *
   * \param[in] state_id  Идентификатор состояния, которому принадлежит ситуация.
   * \param[in] item      Ситуация, которую необходимо обработать.
   */
  inline void Predictor(size_t state_id, Item* item);

  /*!
   * \brief реализация процедуры Scanner.
   *
   * \param[in] state_id      Идентификатор состояния, для которого вызывается процедура.
   * \param[in] token         Токен для обработки.
   * \param[in] new_state_id  Идентификатор состояния, которое было добавлено в результате выполнения процедуры.
   * \return                  true если в результате было добавлено новое состояние.
   */
  inline bool Scanner(size_t state_id, Token::Ptr token, size_t& new_state_id);

  //! Итеративное выполнение операций Completer и Predictor.
  inline void Closure(size_t state_id);

  //! Инициализация начального состояния.
  inline bool InitFirstState(size_t& state_id);

  /*!
   * \brief Положить ситуацию в список необработанных с необязательной проверкой на присутствие в списке.
   *
   * \param[in] item  Указатель на объект стиуации.
   * \param[in] check Проверять или нет присутствие ситуации в списке.
   */
  inline void PutItemToNonhandledList(Item* item, bool check) {
    if (not check or not nonhandled_items_.find(item)) {
      nonhandled_items_.push(item);
    }
  }

  /*!
   * \brief Проверка на присутствие состояния в переданном списке.
   *
   * \param[in] item_list Список ситуаций, в котором надо произвести поиск.
   * \param[in] item      Ситуация, которую надо искать.
   * \param[in] rptr      Ситуация для добавления, если искомая ситуация найдена.
   * \return              true если ситауация найдена.
   */
  inline bool IsItemInList(State::SymbolItemList& item_list, Item* item, Item* rptr) {
    Item tmp_item;
    tmp_item.rhs_pos_   = item->rhs_pos_ + 1;
    tmp_item.rule_id_   = item->rule_id_;
    tmp_item.origin_    = item->origin_;
    tmp_item.lptr_      = item;

    for (Item* cur = item_list.elems_.get_first(); cur; cur = item_list.elems_.get_next()) {
      if (tmp_item == *cur) {
        cur->rptrs_.push_back(Item::Rptr(NULL, rptr));
        return true;
      }
    }
    return false;
  }

public:
  /*!
   * \brief Конструктор класса.
   *
   * \param grammar Указатель на объект грамматики.
   * \param lexer   Указатель на объект лексического анализатора.
   */
  EarleyParser(Grammar* grammar, Lexer* lexer)
    : grammar_(grammar)
    , lexer_(lexer)
    , state_disp_(&item_disp_, grammar_)
    , item_disp_(1024*1024) {
  }

  /*!
   * \brief Синтаксический анализ потока терминальных символов, предоставляемого объектом Lexer.
   *
   * \return true если входная цепочка разобрана.
   */
  bool Parse();

  /*!
   * \brief Освобождение всех ресурсов, выделенных под предыдущий запуск Parse.
   */
  void Reset();
};

} // namespace parser

#endif // EARLEY_PARSER_H__

