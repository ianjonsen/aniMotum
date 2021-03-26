#define TMB_LIB_INIT R_init_foieGras
#include <TMB.hpp>
#include "TMB/sub/rw.hpp"
#include "TMB/sub/crw.hpp"
#include "TMB/sub/crwp.hpp"
#include "TMB/sub/mpm.hpp"
#include "TMB/sub/joint_mpm.hpp"

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRING(model_name);
  if (model_name == "rw") {
    return rw(this);
  } else if (model_name == "crw") {
    return crw(this);
  } else if (model_name == "crwp") {
    return crwp(this);
  } else if (model_name == "mpm") {
    return mpm(this);
  } else if (model_name == "jmpm") {
    return joint_mpm(this);
  } else {
    error ("Unknown model_name");
  }
  return 0;
}