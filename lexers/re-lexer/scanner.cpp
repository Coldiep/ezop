#pragma warning(disable: 4172)
#include "error_thrower.h"
#include "scanner.h"


exp_tree* scanner::do_brackets(Position& p)
  {
    exp_tree* t = new exp_tree();
    ++ p;
    if (*p == 0) throw_error("'[' is at the end of the input"); 
    else if (*p == ']') throw_error("Invalid expression []"); 
    for ( ; *p != ']'; ++ p){
      if (*p == 0) throw_error("Nonpair [ is at the end of the input"); 
      else 
        if (*(p+1) == '-')
        {
          unsigned int c1 = *p;
          ++ p;
          ++ p;
          if (*p == 0)  throw_error("Nonpair [ is at the end of the input"); 
          if (*p == ']')
          {
            t = t->is_empty() ? new exp_tree(c1,0xff) : exp_tree::merge_trees (t,new exp_tree(c1,0xff),opUnion);
            break;
          }
          else 
          {
            if(t->is_empty())
              t = new exp_tree(c1,*p);
            else
              t = exp_tree::merge_trees (t,new exp_tree(c1,*p),opUnion);
          }
        }
      else t = t->is_empty() ? new exp_tree(*p) : exp_tree::merge_trees (t,new exp_tree(*p),opUnion);

    }
    return t;
  }

exp_tree* scanner::process(std::string str)
  {
    //printf("Processing regexp '%s' of length %i.\n",str.c_str(),str.length());
    //std::string input = str.append("#");//"(a|b)*abb#";//"N.*";//"a*b[c-de](a)f|n|(((b)*)c|d)";
    std::stack <point_type> mag;
    std::stack <exp_tree*> trees_mag;
    
    for (Position p = str.c_str(); *p; ++ p)
    {
      char ch = *p;
      if (*p == '(')
      {
        if (*(p+1) == ')') throw_error("Invalid expression ()");
        mag.push (OpLeftBracket);
      }
      else if (*p == ')')
      {
        if (mag.empty () ) throw_error("Nonpair bracket");
        else if (mag.top () != OpLeftBracket)
        {
          exp_tree* right = trees_mag.top();
          trees_mag.pop();
          exp_tree* left = trees_mag.top();
          trees_mag.pop();
          exp_tree* t = exp_tree::merge_trees(left,right,mag.top());
          trees_mag.push(t);
          mag.pop();
          if (mag.top () != OpLeftBracket)
            throw_error("Nonpair bracket");
        }
        mag.pop ();
      }
      else if (*p == '|')
      {
        if(trees_mag.empty()) throw_error("Missing left operand of |");
        mag.push (opUnion);
      }
      else if (*p == '*')
      {
        if(trees_mag.empty()) throw_error("Missing left operand of *");
        exp_tree* t = trees_mag.top()->make_new_root(opIter);
      }
      else if (*p=='+')
      {
        if(trees_mag.empty()) throw_error("Missing left operand of +");
        exp_tree* t = exp_tree::merge_trees(new exp_tree(trees_mag.top()),trees_mag.top()->make_new_root(opIter),opConcat);
        trees_mag.pop();
        trees_mag.push(t);
      }
      else if (*p=='?')
      {
        if(trees_mag.empty()) throw_error("Missing left operand of ?");
        exp_tree* t = exp_tree::merge_trees(trees_mag.top(),new exp_tree(empty),opUnion);
        trees_mag.pop();
        trees_mag.push(t);

      }
      else if (*p=='.')
      {
        trees_mag.push(new exp_tree(0x20,0xff));
      }
      else if (*p=='\\')
      {
        ++p;
        trees_mag.push(new exp_tree(*p));
      }
      else if (*p=='[')
      {
        exp_tree* t = do_brackets(p);
        trees_mag.push(t);  
      }
      else
      {
        if(!(mag.empty()) || trees_mag.empty())
        {
          trees_mag.push(new exp_tree(ch));
        }
        else
        {
          exp_tree* t = trees_mag.top()->make_new_root(opConcat);
          t->root->right = new tree_point(ch);
          trees_mag.pop();
          trees_mag.push(t);
        }
      }
      
      if(*p!='(' && *p!= '|' && *p!='\\' && *p!='*' && *p!='?' && *p!='+' && *p!=']' && *(p+1)!='*' && *(p+1)!='?' && *(p+1)!='+' && !(mag.empty()) && (mag.top()==opUnion || mag.top()==opConcat) && trees_mag.size()>=2)
      {
        exp_tree* r = trees_mag.top();
        trees_mag.pop();
        exp_tree* l = trees_mag.top();
        trees_mag.pop();
        trees_mag.push(exp_tree::merge_trees(l,r,mag.top()));
        mag.pop();
      }
      if(*p!='(' && *p!= '|' && *(p+1)!='|' && *(p+1)!='*' && *(p+1)!='?' && *(p+1)!='+' && *(p+1)!=')' && *(p+1)!=0)
        mag.push(opConcat);
    }
    exp_tree* e;
    if (mag.empty() && !(trees_mag.empty()))
      e = trees_mag.top();
    else
      while(!(mag.empty()))
      {
        exp_tree* r = trees_mag.top();
        trees_mag.pop();
        e = trees_mag.top()->make_new_root(mag.top());
        e->root->right=r->root;
        mag.pop();
      }
    trees_mag.pop();
    e->make_new_root(opConcat);
    tree_point* end = new tree_point('#');
    end->end = 1;
    e->root->right = end;
    e->calc_followpos();

    return e;
  }


