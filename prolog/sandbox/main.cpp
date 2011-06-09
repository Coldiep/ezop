
#include <iostream>
#include <string>

#include <SWI-Prolog.h>

void Print(term_t t) {
  char *s;
  size_t len;
  switch (PL_term_type(t)) {
    case PL_VARIABLE:
    case PL_ATOM:
    case PL_INTEGER:
    case PL_FLOAT:
      PL_get_chars(t, &s, CVT_ALL);
      std::cout << s;
      break;
    case PL_STRING: {
      PL_get_string_chars(t, &s, &len);
      std::string res(s, len);
      std::cout << res;
    }
      break;
    case PL_TERM: {
      term_t a = PL_new_term_ref();
      atom_t name;
      int arity;
      PL_get_name_arity(t, &name, &arity);
      std::cout << "(" << PL_atom_chars(name);
      for (int n = 1; n <= arity; ++n) {
        PL_get_arg(n, t, a);
        if (n > 1) {
            std::cout << ", ";
        }
        Print(a);
      }
      std::cout << ")";
    }
      break;
    default:
      ;
  }
}

void PrintTermType(term_t term) {
  switch (PL_term_type(term)) {
    case PL_VARIABLE:
      std::cout << "PL_VARIABLE\n";
      break;
    case PL_ATOM:
      std::cout << "PL_ATOM\n";
      break;
    case PL_STRING:
      std::cout << "PL_STRING\n";
      break;
    case PL_INTEGER:
      std::cout << "PL_INTEGER\n";
      break;
    case PL_FLOAT:
      std::cout << "PL_FLOAT\n";
      break;
    case PL_TERM:
      std::cout << "PL_TERM\n";
      break;
    default:
      std::cout << "unknown type\n";
  }
}

foreign_t pl_write_atoms(term_t l) {
  term_t head = PL_new_term_ref();      /* variable for the elements */
  term_t list = PL_copy_term_ref(l);    /* copy as we need to write */
  while (PL_get_list(list, head, list)) {
    char *s;
    PrintTermType(head);
    if (PL_get_chars(head, &s, CVT_ALL)) {
      std::cout << s << "\n";
    } else {
      PL_fail;
    }
  }

  return PL_get_nil(list);              /* test end for [] */
}

int main(int argc, char* argv[]) {
  if (not PL_initialise(argc, argv)) {
    PL_halt(1);
  }

  term_t a0 = PL_new_term_refs(2);
  predicate_t p = PL_predicate("onto_list", 1, "ezop");
  if (qid_t qid = PL_open_query(NULL, PL_Q_NORMAL, p, a0)) {
    while (PL_next_solution(qid)) {
      Print(a0);
      std::cout << "\n";
    }
  }

  return 0;
}

