#define TMB_LIB_INIT R_init_foieGras
#include <TMB.hpp>
#include "sub/ssm.hpp"
#include "sub/mpm.hpp"

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRING(model_name);
  if (model_name == "ssm") {
    return ssm(this);
  } else if (model_name == "mpm") {
    return mpm(this);
  } else {
    error ("Unknown model_name");
  }
  return 0;
}
