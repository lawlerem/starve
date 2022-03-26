#define TMB_LIB_INIT R_init_staRVe
#include <TMB.hpp>
using namespace density;

// #include "include/data_in.hpp"
// #include "include/time_segment.hpp"
#include "include/family.hpp"
#include "include/glm.hpp"

#include "include/covariance.hpp"
#include "include/kriging.hpp"
#include "include/nngp.hpp"
#include "include/observations.hpp"


#include "includeV2/utils.hpp"
#include "includeV2/dag.hpp"
#include "includeV2/covariance.hpp"
#include "includeV2/kriging.hpp"
#include "includeV2/persistent_graph.hpp"
#include "includeV2/transient_graph.hpp"

#include "model/staRVe_model.hpp"
#include "model/family.hpp"
#include "model/testing.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_STRING(model);
  if(model == "staRVe_model") {
    return staRVe_model(this);
  } else if(model == "family") {
    return family(this);
  } else if(model == "testing") {
    return testing(this);
  } else {
    error("Unknown model.");
  }
  return 0;
}
