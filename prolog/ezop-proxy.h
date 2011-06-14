
#pragma once

#include <SWI-Prolog.h>

namespace ezop {

/// Класс для взаимодействия с сервером ЭЗОП.
struct EzopProxy : public boost::noncopyable {
  /**
   * \brief Инициализация "движка" SWI Prolog.
   *
   * \param path Путь к директории, где расположены данные и код ЭЗОП.
   */
  EzopProxy(const std::string& path)
    : path_(path) {
  }

  /// Корректно завершаем работу сервера Пролога.
  ~EzopProxy() {
  }

  // ***************  Объявление API системы ЭЗОП ********************

  /// Вспомогательная структура для сохранения списка онтологий.
  struct OntoInfo {
    std::string name_;       ///< Имя онтологии.
    std::string id_;         ///< Идентификатор онтологии.
    std::string parent_id_;  ///< Идентифкатор родительской онтологии.
    std::string content_;    ///< Текст онтологии.
  };

  typedef std::list<OntoInfo> OntoInfoList;

  /// Получить список онтологий.
  void GetOntoList(OntoInfoList& list) {
    char* argv[10];
    int argc = 0;

    std::string param = path_ + "/ezop.pl";
    argv[argc++] = "ezop-proxy";
    argv[argc++] = "-s";
    argv[argc++] = (char*)param.c_str();
    argv[argc]   = NULL;
    putenv("SWI_HOME_DIR=/usr/lib/swi-prolog");
    if (not PL_initialise(argc, (char**)argv)) {
      //PL_halt(1);
      throw std::invalid_argument("Cannot initialize Prolog engine");
    }
    term_t t = PL_new_term_refs(4);
    predicate_t p = PL_predicate("get_onto_list", 4, "ezop");
    if (qid_t qid = PL_open_query(NULL, PL_Q_NORMAL, p, t)) {
      while (PL_next_solution(qid)) {
        list.push_back(OntoInfo());
        OntoInfo& onto_info = list.back();

        char* s = NULL;
        PL_get_chars(t, &s, CVT_STRING | REP_UTF8);
        onto_info.name_ = s;

        PL_get_chars(t + 1, &s, CVT_STRING | REP_UTF8);
        onto_info.id_ = s;

        PL_get_chars(t + 2, &s, CVT_STRING | REP_UTF8);
        onto_info.parent_id_ = s;

        PL_get_chars(t + 3, &s, CVT_STRING | REP_UTF8);
        onto_info.content_ = s;
      }
    }
  }

private:
  std::string path_;
};

}  // namespace ezop.

