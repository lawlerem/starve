#include <TMB.hpp>

#include "include/family.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_INTEGER(link_code);
  PARAMETER(x);

  inv_link_function inv_link = {link_code};

  return inv_link(x);
}
