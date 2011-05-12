
#pragma once

#include <boost/shared_ptr.hpp>
#include <string>
#include <map>
#include <iostream>

namespace ezop { namespace terms {

/*!
 * \brief Класс типа абстрактной алгебры.
 *
 * Каждый тип имеет имя, которое уникальным образом идентифицирует
 * тип во множестве всех типов алгебры. У типа могут быть подтипы.
 */
struct Type {
  //! Тип умного указателя на Type.
  typedef boost::shared_ptr<Type> Ptr;

  //! Имя типа.
  std::string name_;

  //! Описание типа.
  std::string description_;

  // Инициализация именем и описанием.
  Type(const std::string& name, const std::string& desc)
    : name_(name)
    , description_(desc) {
  }
};

/*!
 * \brief Реализация множества типов.
 *
 * Пользователь может добавить или удалить имя, а также найти тип
 * по переданному имени.
 */
class TypeSet {
  //! Список типов, содержащихся в данном множестве.
  typedef std::map<std::string, Type::Ptr>  TypeList;

public:
  /*!
   * \brief Добавление нового типа в множество.
   *
   * \param name Имя типа.
   * \param desc Необязательное описание.
   */
  void AddType(const std::string& name, const std::string& desc = "");

  /**
   * \brief Удвление типа из множества.
   *
   * \param name Имя типа.
   */
  void RemoveType(const std::string& name);

  /**
   * \brief Найти тип во множестве.
   *
   * \param name Имя типа.
   * \return   Указатель на найденный тип или NULL.
   */
  const Type* Find(const std::string& name) const;

  /**
   * \brief Печать содержимого множества типов в поток.
   *
   * \param out Поток для печати.
   */
  void Print(std::ostream& out) const;

private:
  //! Список типов.
  TypeList type_list_;
};

}} // namespace ezop, terms.

