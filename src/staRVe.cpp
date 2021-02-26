#include <TMB.hpp>
using namespace density;

#include "include/data_in.hpp"
#include "include/time_segment.hpp"
#include "include/family.hpp"
#include "include/glm.hpp"

#include "include/covariance.hpp"
#include "include/kriging.hpp"
#include "include/nngp.hpp"
#include "include/observations.hpp"

#include "model/staRVe_model.hpp"
#include "model/family.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_STRING(model);
  if(model == "staRVe_model") {
     return staRVe_model(this);
  } else if(model == "family") {
     return family(this);
  } else {
     error("Unknown model.");
  }
  return 0;
}
