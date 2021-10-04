// Headers included in staRVe.cpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type family(objective_function<Type>* obj) {
  DATA_INTEGER(link_code);
  PARAMETER(x);

  inv_link_function inv_link = {link_code};

  return inv_link(x);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
