#include "re-lexer.h"
#include <string>
#pragma warning(disable: 4996)
#include "error_thrower.h"

using parser::lexer;

/*!
* \brief Конструктор по умолчанию.
*
*/
lexer::lexer()
{
  cur_pos = 0;
  types_no = 0; 
  tree = new token_tree();
  stream = "";
  lex_type* lt = new lex_type();
  lt->id = -1;
  lt->name = "ERROR";
  lt->valid = false;
  lex_types.insert(lt);
  
}
/*!
* \brief Деструктор.
*
*/
lexer::~lexer()
{
  for (std::set <lex_type*>::iterator it = this->lex_types.begin(); it!= this->lex_types.end(); ++it)
  {
  lex_type* lt = (*it);
    if (lt->id != -1)
      delete lt->d;
    delete lt;
  }
  delete this->tree;
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
std::string lexer::strip(std::string str)
{
  /*

  std::string tmp = str.c_str();
  str.replace("\n", " ");
  std::string oldtmp;
  do
  {
    oldtmp = tmp;
    tmp.Replace("  "," ");
  }
  while(tmp.Length() != oldtmp.Length());
  return tmp.c_str();*/
  return str;
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
void lexer::set_stream(std::string s)
{
  stream = strip(s);
}
/*!
* \brief Обработка входной строки для удаления лишних символов
*
* \param[in] str Строка для обработки.
*/
void lexer::set_stream_file(std::string filename, std::string tail)
{
  FILE*f = fopen(filename.c_str(),"r");
  if (f == NULL)
    throw_error("Failed to open input stream file");
  char s[9999];
  int lines = 0;
  while (fgets(s,9999,f) != NULL)
  {/*
    if (s[strlen(s)-1] == 10)
        s[strlen(s)-1] = ' ';*/
    stream.append (s);
  }
  stream.append(tail);
  stream = strip(stream);
}
/*!
* \brief Добавление лексического типа.
*
* \param[in] name		Название типа.
* \param[in] regexp		Регулярное выражение.
* \param[in] priority	Приоритет типа.
* \param[in] ret		Флаг для указания того, нужно ли включать данный тип в выходное дерево токенов.
* \param[in] id			Идентификатор типа.
*/
void lexer::add_type(std::string name, std::string regexp,int priority, int id_, bool ret)
{
  lex_type* lt = new lex_type();
  if (id_ < 0)
    lt->id = ++types_no;
  else
  {
    types_no++;
    lt->id = id_;
  }
  lt->name = name;
  lt->regexp = regexp;
  lt->is_returned = ret;
  scanner S;
  exp_tree* t = S.process(regexp);
  dfa* d = new dfa();
  d->build(t);
  lt->d = d;
  lt->valid = true;
  lt->priority = priority;
  lex_types.insert(lt);
  delete t;
}
/*!
* \brief Добавление лексического типа.
*
* \param[in] id			Идентификатор типа.
* \param[in] regexp		Регулярное выражение.
* \param[in] priority	Приоритет типа.
* \param[in] ret		Флаг для указания того, нужно ли включать данный тип в выходное дерево токенов.
*/
void lexer::add_type(int id_, std::string regexp, int priority, bool ret)
{
  lex_type* lt = new lex_type();
  types_no++;
  lt->id = id_;
  lt->name = "";
  lt->regexp = regexp;
  lt->is_returned = ret;
  scanner S;
  exp_tree* t = S.process(regexp);
  dfa* d = new dfa();
  d->build(t);
  lt->d = d;
  lt->valid = true;
  lt->priority = priority;
  lex_types.insert(lt);
  delete t;
}
/*!
* \brief Проведение лексического анализа.
*
*/
void lexer::analyze()
{
  int id = 0;
  std::set <int> token_ends;
  token_ends.insert(-1);
  for (std::set <int>::iterator it = token_ends.begin(); it != token_ends.end(); ++it)
  {
  int start_pos = (*it);
    std::set <token*> next_tokens = get_tokens(start_pos);
  for (std::set <token*>::iterator iter = next_tokens.begin(); iter != next_tokens.end(); ++iter)
    {
    token* tok = (*iter);
      char integer[10] = "";
      itoa(++id,integer,10);
      tok->id = integer;
      tree->add_node(tok);
      token_ends.insert(tok->finish);
    }
  }
}
/*!
* \brief Сброс параметров всех лексических типов в начальные.
*
*/
void lexer::reset()
{
  for (std::set <lex_type*>::iterator it = lex_types.begin(); it != lex_types.end(); ++it)
  {
  lex_type* lt = (*it);
    if (lt->id == -1)
      continue;
    lt->valid = true;
    lt->d->curr_state = lt->d->get_start_state();
  }
}
/*!
* \brief Получение списка токенов, начинающихся с заданной позиции.
*
* \param[in] pos Позиция во входном потоке.
*/
std::set <token*> lexer::get_tokens(int pos)
{
  std::map <unsigned int, bool> types_returned;
  std::map <unsigned int, int> types_priority;
  for (std::set <lex_type*>::iterator lex_types_it = lex_types.begin(); lex_types_it != lex_types.end(); ++lex_types_it)
  {
    lex_type* lt = (*lex_types_it);
    types_returned[lt->id] = lt->is_returned;
    types_priority[lt->id] = lt->priority;
  }

  int i = ++pos;
  std::map <unsigned int, unsigned int> accepted_types;
  int invalid = 0;
  while(invalid < types_no)
  {
    if(i >= stream.length())
      break;
    for(std::set <lex_type*>::iterator it = lex_types.begin(); it != lex_types.end(); ++it)
    {
      if (!((*it)->valid))
        continue;
      if((*it)->d->move(stream[i]))
      {
        if((*it)->d->curr_state->type == finish || (*it)->d->curr_state->type == start_finish)
          accepted_types[(*it)->id] = i;
      }
      else
      {
        (*it)->valid = false;
        invalid++;
      }
    }
    i++;
  }

  std::set <token*> result;
  std::map <unsigned int, unsigned int>::iterator it = accepted_types.begin (), end = accepted_types.end ();

  for (; it != end; ++ it)
  {
    bool add = true;
    for (std::map <unsigned int, unsigned int>::iterator it1 = accepted_types.begin (); it1 != end; ++it1 )
    {
      if ((*it).second == (*it1).second && (*it).first != (*it1).first && types_priority[(*it).first] < types_priority[(*it1).first])
      {
        add = false;
        break;
      }
    }
    if (add)
      result.insert(new token((*it).first,pos,(*it).second,stream.substr(pos,(*it).second-pos+1),types_returned[(*it).first],(*it).first));
  }
  if(accepted_types.empty() && i < stream.length())
    result.insert(new token(0,pos,(int)stream.length()-1,stream.substr(pos,(int)stream.length()-pos),true));
  reset();
  return result;
}
/*!
* \brief Проверка, достигнут ли конец потока.
*
*/
bool lexer::IsEnd()
{
  return (cur_pos++ >= stream.length());
}
/*!
* \brief Получение списка токенов, следующих за данным.
*
* \param[in] t Токен, список последователей которого необходимо получить.
*/
parser::Lexer::TokenList parser::lexer::GetTokens(Token::Ptr t)
{
	int abs_pos_ = t->abs_pos_;
	int length_ = t->length_;
	std::set <token*> next_tokens = get_tokens(abs_pos_ + length_ - 1);
	TokenList tokens_to_return;
	for (std::set <token*>::iterator it = next_tokens.begin(); it!=next_tokens.end(); ++it)
	{
		Token::Ptr new_token = Token::Ptr(new Token((*it)->terminal_symbol_id));
		new_token->type_ = (*it)->terminal_symbol_id;
		new_token->abs_pos_ = (*it)->start;
		new_token->length_ = (*it)->finish - (*it)->start + 1;
		tokens_to_return.push_back(new_token);
	}
	return tokens_to_return;
}
