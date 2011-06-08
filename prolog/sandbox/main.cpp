
#include <iostream>

#include </usr/lib64/swipl-5.11.21/include/SWI-Prolog.h>

int main(int argc, char* argv[]) {
  if (not PL_initialise(argc, argv)) {
    PL_halt(1);
  }

  term_t a0 = PL_new_term_refs(2);
  predicate_t p = PL_predicate("hello", 1, "test");

  //PL_put_atom_chars(a0, );
  if (PL_call_predicate(NULL, PL_Q_NORMAL, p, a0)) {
    char* s = NULL;
    PL_get_chars(a0, &s, CVT_ALL);
    std::cout << s << "\n";
  }
  //qid_t qid = PL_open_query(NULL, PL_Q_NORMAL, p, a0);
  //PL_next_solution(qid);

#if 0
  PlEngine e(argv[0]);
  PlTermv av(1);
  PlTail l(av[0]);

  for (int i = 0; i < argc; ++i) {
    l.append(argv[i]);
  }
  l.close();

  PlQuery q("entry", av);
  std::cout << (q.next_solution() ? "true" : "false") << "\n";
#endif
  return 0;
}

