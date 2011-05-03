#define PRINT_ADDING

#include <parser/grammar.h>
#include <parser/earley_parser.h>
#include <lexers/re-lexer/lex.h>

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
    return EarleyParser::Context::Ptr(new TreeContext(parser_->grammar_->GetSymbolName(token->type_) + std::string(" (") + token->text_ + ")"));
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
#if 0
    PublicGrammar pg("test");
    pg.AddTerminal(1,"integer");
    pg.AddTerminal(2,"real");
    pg.AddTerminal(3,"word");
    pg.AddTerminal(4,"boolean");
    pg.AddTerminal(6,"add");
    pg.AddTerminal(7,"multiplay");

    pg.AddNonterminal(8,"A");
    pg.AddNonterminal(9,"M");
    pg.AddNonterminal(10,"N");

    pg.AddRule(1,"A --> M + A");
    pg.AddLhsSymbol(1,8);
    pg.AddRhsSymbol(1,9);
    pg.AddRhsSymbol(1,6);
    pg.AddRhsSymbol(1,8);

    pg.AddRule(2,"A --> M");
    pg.AddLhsSymbol(2,8);
    pg.AddRhsSymbol(2,9);

    pg.AddRule(3,"M --> N x M");
    pg.AddLhsSymbol(3,9);
    pg.AddRhsSymbol(3,10);
    pg.AddRhsSymbol(3,7);
    pg.AddRhsSymbol(3,9);

    pg.AddRule(4,"M --> N");
    pg.AddLhsSymbol(4,9);
    pg.AddRhsSymbol(4,10);

    pg.AddRule(5,"N --> integer");
    pg.AddLhsSymbol(5,10);
    pg.AddRhsSymbol(5,1);

    pg.AddRule(6,"N --> real");
    pg.AddLhsSymbol(6,10);
    pg.AddRhsSymbol(6,2);

    pg.SetStartSymbolId(8);

    pg.Print(std::cout);

    Grammar gr(&pg);
  
    lexer::Lexer lexer;
    lexer.AddLexType(1,"[1-9][0-9]*", "integer", true);
    lexer.AddLexType(2,"-?([1-9][0-9]*)|(0?)\\.[0-9]*(-?(e|E)-?[1-9][0-9]*)?[0-9]*", "real", true);
    lexer.AddLexType(3,"[a-zA-Zа-яА-ЯёЁ_][a-zA-Zа-яА-ЯёЁ_0-9]*", "word", true);
    lexer.AddLexType(4,"(true)|(false)", "boolean", true);
    lexer.AddLexType(6,"\\+", "add", true);
    lexer.AddLexType(7, "\\*", "mul", true);
    lexer.AddLexType(8, "[:space:]+", "space", false);
    lexer.AddLexType(9,"\\/\\*.*\\*\\/", "comment", false);
    std::string st = "12345  /* hello */    + 34 * 55.4";
    lexer.SetInputStream(&st[0], &st[0] + st.length());
#endif


    PublicGrammar pg("test");
    pg.AddTerminal(1,"a");
    pg.AddTerminal(2,"aa");

    pg.AddNonterminal(3,"A");
    pg.SetStartSymbolId(3);

    pg.AddRule(1,"A --> A A");
    pg.AddLhsSymbol(1,3);
    pg.AddRhsSymbol(1,3);
    pg.AddRhsSymbol(1,3);

    pg.AddRule(2,"A --> a");
    pg.AddLhsSymbol(2,3);
    pg.AddRhsSymbol(2,1);

    pg.AddRule(3,"A --> aa");
    pg.AddLhsSymbol(3,3);
    pg.AddRhsSymbol(3,2);

    pg.Print(std::cout);

    lexer::Lexer lexer;
    lexer.AddLexType(1,"a", "one a", true);
    lexer.AddLexType(2,"aa", "two a", true);
    std::string st = "aaa";
    lexer.SetInputStream(&st[0], &st[0] + st.length());

    Grammar gr(&pg);

    TestSemantic interpretator;
    EarleyParser parser(&gr, &lexer, &interpretator);

     std::cout << "parse start\n";
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
