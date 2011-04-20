#define PRINT_ADDING

#include "public_grammar.h"
#include "grammar.h"
#include "lexer.h"
#include "earley_parser.h"
#include "allocator.h"

#include "re-lexer.h"

using namespace parser;

#include <exception>
#include <iostream>

#include <time.h>

//! Интерфейс для взаимодействия с интерпретатором.
struct TestSemantic : public EarleyParser::Interpretator {
  EarleyParser* parser_;

  struct TreeContext : public EarleyParser::Context {
    typedef std::list<EarleyParser::Context::Ptr> ChildList;
    TreeContext(const std::string& name)
      : name_(name) {
    }
    ChildList   children_;
    std::string name_;
  };

  void PrintContext(EarleyParser::Context::Ptr context, const std::string& indent) {
    TreeContext* tree_context = static_cast<TreeContext*>(context.get());
    std::cout << indent << tree_context->name_ << "\n";
    for (TreeContext::ChildList::iterator it = tree_context->children_.begin(); it != tree_context->children_.end(); ++it) {
      PrintContext(*it, indent + "  ");
    }
  }

  /*!
   * \brief Начало работы алгоритма.
   *
   * \param[in] parser Указатель на объект парсера для получения информации о состояниях.
   */
  void Start(EarleyParser* parser) {
    parser_ = parser;
  }

  /*!
   * \brief Успешное завершение работы алгоритма.
   *
   * \param[in] item Ситуация с начальным символом в левой части.
   */
  void End(const EarleyParser::Item* item) {
    if (item and not item->rptrs_.empty()) {
      TreeContext* context = new TreeContext(parser_->grammar_->GetSymbolName(parser_->grammar_->GetLhsOfRule(item->rule_id_)));
      for (const EarleyParser::Item* cur = item; cur; cur = cur->lptr_) {
        if (not cur->rptrs_.empty()) {
          context->children_.push_back(cur->rptrs_.front().context_);
        }
      }
      PrintContext(EarleyParser::Context::Ptr(context), "");
    }
  }

  /*!
   * \brief Обработка добавления терминального символа.
   *
   * \param[in]   token   Токен терминала.
   * \param[in]   item    Ситуация, в которой производится сдвиг терминального символа.
   *
   * \return      Ненулевой указатель если данный символ необходимо обрабатывать.
   */
  EarleyParser::Context::Ptr HandleTerminal(Token::Ptr token, const EarleyParser::Item* item) {
    return EarleyParser::Context::Ptr(new TreeContext(parser_->grammar_->GetSymbolName(token->type_)));
  }

  /*!
   * \brief Обработка добавления нетерминального символа.
   *
   * \param[in]   rule_item   Ситуация, соответствующая правилу, в левой части которого стоит данный нетерминал.
   * \param[in]   left_item   Ситуация, в которой производится сдвиг нетерминального символа.
   *
   * \return      Ненулевой указатель если данный символ необходимо обрабатывать.
   */
  EarleyParser::Context::Ptr HandleNonTerminal(const EarleyParser::Item* rule_item, const EarleyParser::Item* left_item) {
    TreeContext* context = new TreeContext(parser_->grammar_->GetSymbolName(parser_->grammar_->GetLhsOfRule(rule_item->rule_id_)));
    for (const EarleyParser::Item* item = rule_item; item; item = item->lptr_) {
      if (not item->rptrs_.empty()) {
        context->children_.push_back(item->rptrs_.front().context_);
      }
    }
    return EarleyParser::Context::Ptr(context);
  }
};

int main() {

  try {
    PublicGrammar pg("test");
    pg.AddTerminal(1,"N");
    pg.AddTerminal(2,"+");
    pg.AddTerminal(3,"x");

    pg.AddNonterminal(4,"A");
    pg.AddNonterminal(5,"M");

    pg.AddRule(1,"A --> M + A");
    pg.AddLhsSymbol(1,4);
    pg.AddRhsSymbol(1,5);
    pg.AddRhsSymbol(1,2);
    pg.AddRhsSymbol(1,4);

    pg.AddRule(2,"A --> M");
    pg.AddLhsSymbol(2,4);
    pg.AddRhsSymbol(2,5);

    pg.AddRule(3,"M --> N x M");
    pg.AddLhsSymbol(3,5);
    pg.AddRhsSymbol(3,1);
    pg.AddRhsSymbol(3,3);
    pg.AddRhsSymbol(3,5);

    pg.AddRule(4,"M --> N");
    pg.AddLhsSymbol(4,5);
    pg.AddRhsSymbol(4,1);

    pg.SetStartSymbolId(4);

    pg.Print(std::cout);
    std::cout << "\n\n";

    Grammar gr(&pg);
  
    relexer::ReLexer ll;
    ll.AddType(1,"[1-9][0-9]*");
    ll.AddType(2,"\\+");
    ll.AddType(3,"x");
    ll.SetStream("12 + 34");
    ll.Analyze();

    TestSemantic interpretator;
    EarleyParser parser(&gr, &ll, &interpretator);

    if (parser.Parse()) {
        std::cout << "parse successful\n";
    } else {
      std::cout << "there is an error during parsing.\n";
    }
  } catch (std::exception& err) {
    std::cout << err.what() << std::endl;
  }

  return 0;
}
