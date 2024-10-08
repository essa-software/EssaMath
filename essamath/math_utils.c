#include "math_utils.h"
#include "expression.h"
#include <complex.h>
#include <math.h>
#include <stdlib.h>

// NOLINTBEGIN(misc-no-recursion)

em_val em_numeric_nan(void){
   return em_createreal((__builtin_nan ("")));
}

em_val em_numeric_inf(void){
   return em_createreal((__builtin_inf ()));
}

em_val em_numeric_minf(void){
   return em_createreal(-(__builtin_inf ()));
}

em_val em_numeric_i(void){
   return em_createcomplex(I);
}

em_val em_numeric_e(void){
   return em_createreal(M_E);
}

em_val em_numeric_pi(void){
   return em_createreal(M_PI);
}

em_val em_numeric_phi(void){
   return em_createreal((1.0+sqrt(5))/2.0);
}

int em_numeric_equal(int* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            if((isnan(_a->emValue.emReal) && isnan(_b->emValue.emReal)) || (isinf(_a->emValue.emReal) && isinf(_b->emValue.emReal))) {
               *_result = 1;
            } else {
               *_result = (em_nearequal(_a->emValue.emReal, _b->emValue.emReal, EM_EPS));
            }
            return 1;
         }
         case EM_VALCOMPLEX:{
            if(cimag(_b->emValue.emComplex) == 0) {
               if((isnan(_a->emValue.emReal) && isnan(creal(_b->emValue.emComplex))) || (isinf(_a->emValue.emReal) && isinf(creal(_b->emValue.emReal)))) {
                  *_result = 1;
               } else {
                  *_result = (em_nearequal(_a->emValue.emReal, creal(_b->emValue.emComplex), EM_EPS));
               }
            } else {
               *_result = 0;
            }
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            if(cimag(_a->emValue.emComplex) == 0) {
               if((isnan(creal(_a->emValue.emComplex)) && isnan(_b->emValue.emReal)) || (isinf(creal(_a->emValue.emComplex)) && isinf(_b->emValue.emReal))) {
                  *_result = 1;
               } else {
                  *_result = (em_nearequal(creal(_a->emValue.emComplex), _b->emValue.emReal, EM_EPS));
               }
            } else {
               *_result = 0;
            }
            return 1;
         }
         case EM_VALCOMPLEX:{   
            if((isnan(creal(_a->emValue.emComplex)) && isnan(creal(_b->emValue.emComplex))) || (isinf(creal(_a->emValue.emComplex)) && isinf(creal(_b->emValue.emComplex)))) {
               *_result = 1;
            }else{
               *_result = (em_nearequal(creal(_a->emValue.emComplex), creal(_b->emValue.emComplex), EM_EPS)) && (em_nearequal(cimag(_a->emValue.emComplex), cimag(_b->emValue.emComplex), EM_EPS));
            }
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            *_result = 0;
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            *_result = 1;
            size = _b->emValue.emVector.emSize;
            for(size_t i = 0; i < size; i++){
               int res = 0;
               int valid = em_numeric_equal(&res, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  return 0;
               }
               *_result &= res;
            }
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_nequal(int* _result, const em_val* _a, const em_val* _b){
   int valid = em_numeric_equal(_result, _a, _b);

   if(valid){
      *_result = !_result;
   }

   return valid;
}

int em_numeric_gth(int* _result, const em_val* _a, const em_val* _b){
   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = (_a->emValue.emReal > _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            if(cimag(_b->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (_a->emValue.emReal > creal(_b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            if(cimag(_a->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (creal(_a->emValue.emComplex) > _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            if(cimag(_a->emValue.emComplex) != 0 || cimag(_b->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (creal(_a->emValue.emComplex) > creal(_b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
         return 0;
      }
      default:
      return 0;
   }
}

int em_numeric_geq(int* _result, const em_val* _a, const em_val* _b){
   int g, eq;
   int valid = 1;
   valid &= em_numeric_gth(&g, _a, _b);
   valid &= em_numeric_equal(&eq, _a, _b);

   *_result = (g || eq);
   return valid;
}

int em_numeric_lth(int* _result, const em_val* _a, const em_val* _b){
   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = (_a->emValue.emReal < _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            if(cimag(_b->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (_a->emValue.emReal < creal(_b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            if(cimag(_a->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (creal(_a->emValue.emComplex) < _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            if(cimag(_a->emValue.emComplex) != 0 || cimag(_b->emValue.emComplex) != 0){
               return 0;
            }
            *_result = (creal(_a->emValue.emComplex) < creal(_b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            return 0;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
         return 0;
      }
      default:
      return 0;
   }
}

int em_numeric_leq(int* _result, const em_val* _a, const em_val* _b){
   int l, eq;
   int valid = 1;
   valid &= em_numeric_lth(&l, _a, _b);
   valid &= em_numeric_equal(&eq, _a, _b);

   *_result = (l || eq);
   return valid;
}


int em_numeric_add(em_val* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;
   em_val* vector = NULL;

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createreal(_a->emValue.emReal + _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emReal + _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            return em_numeric_add(_result, _b, _a);
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createcomplex(_a->emValue.emComplex + _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emComplex + _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            return em_numeric_add(_result, _b, _a);
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            size = _a->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_add(vector + i, _a->emValue.emVector.emData + i, _b);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_add(vector + i, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_sub(em_val* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;
   em_val* vector = NULL;

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createreal(_a->emValue.emReal - _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emReal - _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_sub(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createcomplex(_a->emValue.emComplex - _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emComplex - _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_sub(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            size = _a->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_sub(vector + i, _a->emValue.emVector.emData + i, _b);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_sub(vector + i, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_mul(em_val* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;
   em_val* vector = NULL;

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createreal(_a->emValue.emReal * _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emReal * _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            return em_numeric_mul(_result, _b, _a);
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createcomplex(_a->emValue.emComplex * _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emComplex * _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            return em_numeric_mul(_result, _b, _a);
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            size = _a->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_mul(vector + i, _a->emValue.emVector.emData + i, _b);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_mul(vector + i, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_div(em_val* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;
   em_val* vector = NULL;

   // int invaliddenom = 1;
   // if(em_numeric_equal(&invaliddenom, _b, em_createreal(0))){
   //    if(invaliddenom){
   //       return 0;
   //    }
   // }

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createreal(_a->emValue.emReal / _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emReal / _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_div(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createcomplex(_a->emValue.emComplex / _b->emValue.emReal);
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(_a->emValue.emComplex / _b->emValue.emComplex);
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_div(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            size = _a->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_div(vector + i, _a->emValue.emVector.emData + i, _b);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_div(vector + i, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_pow(em_val* _result, const em_val* _a, const em_val* _b){
   size_t size = 0;
   em_val* vector = NULL;

   em_val isdivisiblebytwo;
   em_val _0 = em_createreal(0);
   em_val _2 = em_createreal(2);
   if(em_numeric_mod(&isdivisiblebytwo, _b, &_2)){
      int equal;
      if(em_numeric_equal(&equal, &isdivisiblebytwo, &_0)){
         if(equal){
            int islessthanzero = 1;
            if(em_numeric_lth(&islessthanzero, _a, &_0)){
               if(islessthanzero){
                  return 0;
               }
            }
         }
      }
   }

   switch(_a->emType){
      case EM_VALREAL:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createreal(pow(_a->emValue.emReal, _b->emValue.emReal));
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(cpow(_a->emValue.emReal, _b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_pow(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALCOMPLEX:{
      switch(_b->emType){
         case EM_VALREAL:{
            *_result = em_createcomplex(cpow(_a->emValue.emComplex, _b->emValue.emReal));
            return 1;
         }
         case EM_VALCOMPLEX:{
            *_result = em_createcomplex(cpow(_a->emValue.emComplex, _b->emValue.emComplex));
            return 1;
         }
         case EM_VALVECTOR:{
            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_pow(vector + i, _a, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      case EM_VALVECTOR:{
      switch(_b->emType){
         case EM_VALREAL:
         case EM_VALCOMPLEX:{
            size = _a->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_pow(vector + i, _a->emValue.emVector.emData + i, _b);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         case EM_VALVECTOR:{
            if(_a->emValue.emVector.emSize != _b->emValue.emVector.emSize){
               return 0;
            }

            size = _b->emValue.emVector.emSize;
            vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_pow(vector + i, _a->emValue.emVector.emData + i, _b->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
         }
         default:
         return 0;
         }
      }
      default:
      return 0;
   }
}

int em_numeric_neg(em_val* _result, const em_val* _a){
   em_val _0 = em_createreal(0);
   return em_numeric_sub(_result, &_0, _a);
}

int em_numeric_mod(em_val* _result, const em_val* _a, const em_val* _b){
   double a = 0, b = 0;

   if(!em_numeric_isinteger(_a)){
      return 0;
   }

   switch(_a->emType){
      case EM_VALREAL:
         a = _a->emValue.emReal;
      break;
      case EM_VALCOMPLEX:
         a = creal(_a->emValue.emComplex);
      break;
      default:
         return 0;
   }

   switch(_b->emType){
      case EM_VALREAL:
         b = _b->emValue.emReal;
      break;
      case EM_VALCOMPLEX:
         b = creal(_b->emValue.emComplex);
      break;
      default:
         return 0;
   }

   *_result = em_createreal((int)a % (int)b);
   return 1;
}

int em_numeric_factorial(em_val* _result, const em_val* _a){
   double res = 0;
   int result = 1;

   if(!em_numeric_isinteger(_a)){
      return 0;
   }

   switch(_a->emType){
      case EM_VALREAL:
         res = _a->emValue.emReal;
      break;
      case EM_VALCOMPLEX:
         res = creal(_a->emValue.emReal);
      break;
      default:
         return 0;
   }

   for(int i = 0; i < res; i++){
      result *= (int)res;
   }

   *_result = em_createreal(result);
   return 1;
}

static double cot(double _x){
   return 1/tan(_x);
}

static _Complex double ccot(_Complex double _x){
   return 1/ctan(_x);
}

static double sec(double _x){
   return 1/cos(_x);
}

static _Complex double csec(_Complex double _x){
   return 1/ccos(_x);
}

static double csc(double _x){
   return 1/sin(_x);
}

static _Complex double ccsc(_Complex double _x){
   return 1/csin(_x);
}

static double acot(double _x){
   return 1/atan(_x);
}

static _Complex double cacot(_Complex double _x){
   return 1/catan(_x);
}

static double asec(double _x){
   return 1/acos(_x);
}

static _Complex double casec(_Complex double _x){
   return 1/cacos(_x);
}

static double acsc(double _x){
   return 1/asin(_x);
}

static _Complex double cacsc(_Complex double _x){
   return 1/casin(_x);
}

static double coth(double _x){
   return 1/tanh(_x);
}

static _Complex double ccoth(_Complex double _x){
   return 1/ctanh(_x);
}

static double sech(double _x){
   return 1/cosh(_x);
}

static _Complex double csech(_Complex double _x){
   return 1/ccosh(_x);
}

static double csch(double _x){
   return 1/sinh(_x);
}

static _Complex double ccsch(_Complex double _x){
   return 1/csinh(_x);
}

static double acoth(double _x){
   return 1/atanh(_x);
}

static _Complex double cacoth(_Complex double _x){
   return 1/catanh(_x);
}

static double asech(double _x){
   return 1/acosh(_x);
}

static _Complex double casech(_Complex double _x){
   return 1/cacosh(_x);
}

static double acsch(double _x){
   return 1/asinh(_x);
}

static _Complex double cacsch(_Complex double _x){
   return 1/casinh(_x);
}

#define EM_NUMERIC_FUNC(name)                                                             \
int em_numeric_##name(em_val* _result, const em_val* _a){                                        \
   switch(_a->emType){                                                                     \
      case EM_VALREAL:                                                                    \
         *_result = em_createreal(name(_a->emValue.emReal));                               \
         return 1;                                                                        \
      case EM_VALCOMPLEX:                                                                 \
         *_result = em_createcomplex(c##name(_a->emValue.emComplex));                      \
         return 1;                                                                        \
      case EM_VALVECTOR:{                                                                 \
            size_t size = _a->emValue.emVector.emSize;                                     \
            em_val* vector = (em_val*)malloc(size*sizeof(em_val));                                       \
            for(size_t i = 0; i < size; i++){                                             \
               int valid = em_numeric_##name(vector + i, _a->emValue.emVector.emData + i);  \
               if(!valid){                                                                \
                  free(vector);                                                           \
                  return 0;                                                               \
               }                                                                          \
            }                                                                             \
            *_result = em_createvector(vector, size);                                     \
            return 1;                                                                     \
      }                                                                                   \
      default:                                                                            \
         return 0;                                                                        \
   }                                                                                      \
   return 0;                                                                              \
}

EM_NUMERIC_FUNC(exp)
EM_NUMERIC_FUNC(log)
EM_NUMERIC_FUNC(sin)
EM_NUMERIC_FUNC(cos)
EM_NUMERIC_FUNC(tan)
EM_NUMERIC_FUNC(cot)
EM_NUMERIC_FUNC(sec)
EM_NUMERIC_FUNC(csc)
EM_NUMERIC_FUNC(asin)
EM_NUMERIC_FUNC(acos)
EM_NUMERIC_FUNC(atan)
EM_NUMERIC_FUNC(acot)
EM_NUMERIC_FUNC(asec)
EM_NUMERIC_FUNC(acsc)
EM_NUMERIC_FUNC(sinh)
EM_NUMERIC_FUNC(cosh)
EM_NUMERIC_FUNC(tanh)
EM_NUMERIC_FUNC(coth)
EM_NUMERIC_FUNC(sech)
EM_NUMERIC_FUNC(csch)
EM_NUMERIC_FUNC(asinh)
EM_NUMERIC_FUNC(acosh)
EM_NUMERIC_FUNC(atanh)
EM_NUMERIC_FUNC(acoth)
EM_NUMERIC_FUNC(asech)
EM_NUMERIC_FUNC(acsch)

int em_numeric_atan2(em_val* _result, const em_val* _a, const em_val* _b){
   em_val x;
   if(em_numeric_div(&x, _b, _a)){
      return em_numeric_atan(_result, &x);
   }

   return 0;
}

int em_numeric_abs(em_val* _result, const em_val* _a){
   switch(_a->emType){
      case EM_VALREAL:
         *_result = em_createreal(fabs(_a->emValue.emReal));
         return 1;
      case EM_VALCOMPLEX:
         *_result = em_createcomplex(cabs(_a->emValue.emComplex));
         return 1;
      case EM_VALVECTOR:{
            size_t size = _a->emValue.emVector.emSize;
            em_val* vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_abs(vector + i, _a->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
      }
      default:
         return 0;
   }

}

int em_numeric_floor(em_val* _result, const em_val* _a){
   switch(_a->emType){
      case EM_VALREAL:
         *_result = em_createreal(floor(_a->emValue.emReal));
         return 1;
      case EM_VALCOMPLEX:
         if(cimag(_a->emValue.emComplex) != 0.0){
            return 0;
         }
         *_result = em_createcomplex(floor(creal(_a->emValue.emComplex)));
         return 1;
      case EM_VALVECTOR:{
            size_t size = _a->emValue.emVector.emSize;
            em_val* vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_floor(vector + i, _a->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
      }
      default:
         return 0;
   }

}

int em_numeric_ceil(em_val* _result, const em_val* _a){
   switch(_a->emType){
      case EM_VALREAL:
         *_result = em_createreal(ceil(_a->emValue.emReal));
         return 1;
      case EM_VALCOMPLEX:
         if(cimag(_a->emValue.emComplex) != 0.0){
            return 0;
         }
         *_result = em_createcomplex(ceil(creal(_a->emValue.emComplex)));
         return 1;
      case EM_VALVECTOR:{
            size_t size = _a->emValue.emVector.emSize;
            em_val* vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_ceil(vector + i, _a->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
      }
      default:
         return 0;
   }
}

int em_numeric_round(em_val* _result, const em_val* _a){
   switch(_a->emType){
      case EM_VALREAL:
         *_result = em_createreal(round(_a->emValue.emReal));
         return 1;
      case EM_VALCOMPLEX:
         if(cimag(_a->emValue.emComplex) != 0.0){
            return 0;
         }
         *_result = em_createcomplex(round(creal(_a->emValue.emComplex)));
         return 1;
      case EM_VALVECTOR:{
            size_t size = _a->emValue.emVector.emSize;
            em_val* vector = (em_val*)malloc(size*sizeof(em_val));
            for(size_t i = 0; i < size; i++){
               int valid = em_numeric_round(vector + i, _a->emValue.emVector.emData + i);
               if(!valid){
                  free(vector);
                  return 0;
               }
            }
            *_result = em_createvector(vector, size);
            return 1;
      }
      default:
         return 0;
   }
}

int em_numeric_isinteger(const em_val* _value){
   double a = 0.0, b = 0.0;
   
   switch(_value->emType){
      case EM_VALREAL:
         b = _value->emValue.emReal;
         break;
      case EM_VALCOMPLEX:{
         if(cimag(_value->emValue.emComplex) != 0.0){
            return 0;
         }
         b = creal(_value->emValue.emComplex);
         break;
      }
      default:
         return 0;
   }

   a = (int)b;

   return em_nearequal(a, b, 1e-9);
}


// zeta(n) for n = 2,...,33
static const double ZETAS_POS[32] = {
    1.6449340668482264, 1.2020569031595943, 1.0823232337111382,
    1.0369277551433699, 1.0173430619844491, 1.0083492773819228,
    1.0040773561979443, 1.0020083928260822, 1.0009945751278181,
    1.0004941886041195, 1.0002460865533080, 1.0001227133475785,
    1.0000612481350587, 1.0000305882363070, 1.0000152822594087,
    1.0000076371976379, 1.0000038172932650, 1.0000019082127166,
    1.0000009539620339, 1.0000004769329868, 1.0000002384505027,
    1.0000001192199260, 1.0000000596081891, 1.0000000298035035,
    1.0000000149015548, 1.0000000074507118, 1.0000000037253340,
    1.0000000018626597, 1.0000000009313274, 1.0000000004656629,
    1.0000000002328312, 1.0000000001164155
};

// zeta(1 - 2n) for n = 1,...,130, i.e. zeta(-1), zeta(-3), zeta(-5), ...
static const double ZETAS_NEG[130] = {
   -8.3333333333333333e-02,  8.3333333333333333e-03, -3.9682539682539683e-03,
    4.1666666666666667e-03, -7.5757575757575758e-03,  2.1092796092796093e-02,
   -8.3333333333333333e-02,  4.4325980392156863e-01, -3.0539543302701197e000,
    2.6456212121212121e001, -2.8146014492753623e002,  3.6075105463980464e003,
   -5.4827583333333333e004,  9.7493682385057471e005, -2.0052695796688079e007,
    4.7238486772162990e008, -1.2635724795916667e010,  3.8087931125245369e011,
   -1.2850850499305083e013,  4.8241448354850170e014, -2.0040310656516253e016,
    9.1677436031953308e017, -4.5979888343656503e019,  2.5180471921451096e021,
   -1.5001733492153929e023,  9.6899578874635941e024, -6.7645882379292821e026,
    5.0890659468662290e028, -4.1147288792557979e030,  3.5666582095375556e032,
   -3.3066089876577577e034,  3.2715634236478716e036, -3.4473782558278054e038,
    3.8614279832705259e040, -4.5892974432454332e042,  5.7775386342770432e044,
   -7.6919858759507135e046,  1.0813635449971655e049, -1.6029364522008965e051,
    2.5019479041560463e053, -4.1067052335810212e055,  7.0798774408494581e057,
   -1.2804546887939509e060,  2.4267340392333524e062, -4.8143218874045769e064,
    9.9875574175727531e066, -2.1645634868435186e069,  4.8962327039620553e071,
   -1.1549023923963520e074,  2.8382249570693707e076, -7.2612008803606716e078,
    1.9323514233419812e081, -5.3450160425288624e083,  1.5356028846422423e086,
   -4.5789872682265798e088,  1.4162025212194809e091, -4.5400652296092655e093,
    1.5076656758807860e096, -5.1830949148264564e098,  1.8435647427256529e101,
   -6.7805554753090959e103,  2.5773326702754605e106, -1.0119112875704598e109,
    4.1016346161542292e111, -1.7155244534032019e114,  7.4003425705269094e116,
   -3.2909225357054443e119,  1.5079831534164771e122, -7.1169879188254549e124,
    3.4580429141577772e127, -1.7290907606676748e130,  8.8936991695032969e132,
   -4.7038470619636015e135,  2.5571938231060206e138, -1.4284067500443528e141,
    8.1952152218313783e143, -4.8276485422727372e146,  2.9189612374770324e149,
   -1.8108932162568904e152,  1.1523577220021169e155, -7.5192311951981770e157,
    5.0294016576411050e160, -3.4473420444477677e163,  2.4207458645868515e166,
   -1.7409465920377677e169,  1.2819489863482243e172, -9.6624121108560918e174,
    7.4526910304300896e177, -5.8808393311674371e180,  4.7462718654907615e183,
   -3.9169132594772825e186,  3.3045071443226032e189, -2.8492890550994583e192,
    2.5103329345077587e195, -2.2593901995475253e198,  2.0769138004287608e201,
   -1.9494732174927259e204,  1.8680731471265914e207, -1.8270752662814577e210,
    1.8235386322595677e213, -1.8568690810125945e216,  1.9287189851195602e219,
   -2.0431170460286448e222,  2.2068411644527846e225, -2.4300821796490274e228,
    2.7274887879083470e231, -3.1197421573755085e234,  3.6358938724282600e237,
   -4.3168300030760883e240,  5.2204244879387200e243, -6.4292606949769305e246,
    8.0623033870130844e249, -1.0292714737903011e253,  1.3375329699780524e256,
   -1.7689480902797380e259,  2.3806479018092397e262, -3.2597127947194185e265,
    4.5404962371601213e268, -6.4328575193147851e271,  9.2687048675749311e274,
   -1.3579619500285181e278,  2.0227839736049322e281, -3.0629906992208336e284,
    4.7143085300742652e287, -7.3741045871355758e290,  1.1720962767050827e294,
   -1.8928866644685657e297,  3.1055517596048927e300, -5.1754977470366798e303,
    8.7601563446229215e306
};

int em_numeric_iseven(int64_t n) { return n % 2 == 0; }

double em_numeric_zeta(int64_t n){
   if (n < 0) {
      if (em_numeric_iseven(n)) {
         return 0.0;
      } if (-(1 + n)/2 < (int64_t)(sizeof(ZETAS_NEG)/sizeof(ZETAS_NEG[0]))) {
         return ZETAS_NEG[-(1 + n)/2];
      } if (em_numeric_iseven((1 - n)/2)) {
         return __builtin_inf();
      }           return -__builtin_inf();
     
   } if (n == 0) {
      return -0.5;
   } if (n == 1) {
      return __builtin_inf();
   } if ((n - 2) < (int64_t)(sizeof(ZETAS_POS)/sizeof(ZETAS_POS[0]))) {
      return ZETAS_POS[n - 2];
   }

   return 1.0/(1.0 - pow(0.5, (double)n));
}
static double digamma(int64_t n)
{
   // Table[BernoulliB[2n]/(2 n), {n,1,8}]
   const double c[] = {
      0.083333333333333333, -0.0083333333333333333,  0.0039682539682539683,
     -0.0041666666666666667, 0.0075757575757575758, -0.021092796092796093,
      0.083333333333333333, -0.44325980392156863
   };
   double res = 0;

   double t = 0.0;
   double t2 = 0.0;

   if (n <= 0) {
      return __builtin_nan("");
   }

   t = 1.0/(double)n;
   t2 = t*t;

   // map potentially small n to n >= 7
   if (n < 7) { // recurrence formula
      for (int64_t nu = 1; nu < 7 - n; ++nu) {
         res -= 1.0/((double)n + (double)nu);
      }
      res -= 1.0/(double)n;
      n = 7;
   }

   return res + log((double)n) - 0.5*t
      - t2*(c[0] + t2*(c[1] + t2*(c[2] + t2*(c[3] + t2*(c[4] + t2*(c[5] + t2*(c[6] + t2*c[7])))))));
}

/// harmonic number n
double em_numeric_harmonic(int64_t n)
{
   if (n <= 0) {
      return __builtin_nan("");
   } if (n < 20) {
      double sum = 1;
      for (int64_t k = 2; k <= n; ++k) {
         sum += 1.0/(double)k;
      }
      return sum;
   }        const double eulergamma = 0.57721566490153286;
      return eulergamma + digamma(n + 1);
  
}

static const double NEG_ETA[54] = {
    -0.69314718055994531, -0.82246703342411322, -0.90154267736969571,
    -0.94703282949724592, -0.97211977044690931, -0.98555109129743510,
    -0.99259381992283028, -0.99623300185264790, -0.99809429754160533,
    -0.99903950759827157, -0.99951714349806075, -0.99975768514385819,
    -0.99987854276326512, -0.99993917034597972, -0.99996955121309924,
    -0.99998476421490611, -0.99999237829204101, -0.99999618786961011,
    -0.99999809350817168, -0.99999904661158152, -0.99999952325821554,
    -0.99999976161323082, -0.99999988080131844, -0.99999994039889239,
    -0.99999997019885696, -0.99999998509923200, -0.99999999254955048,
    -0.99999999627475340, -0.99999999813736942, -0.99999999906868228,
    -0.9999999995343403 , -0.9999999997671699 , -0.9999999998835849 ,
    -0.9999999999417924 , -0.9999999999708962 , -0.9999999999854481 ,
    -0.9999999999927240 , -0.9999999999963620 , -0.9999999999981810 ,
    -0.9999999999990905 , -0.9999999999995453 , -0.9999999999997726 ,
    -0.9999999999998863 , -0.9999999999999432 , -0.9999999999999716 ,
    -0.9999999999999858 , -0.9999999999999929 , -0.9999999999999964 ,
    -0.9999999999999982 , -0.9999999999999991 , -0.9999999999999996 ,
    -0.9999999999999998 , -0.9999999999999999 , -0.9999999999999999
};

// Table[PolyLog[-2n+1,-1], {n,1,109}]
static const double NEG_ETA_NEG_N[] = {
   -0.25, 0.125           , -0.25                  ,  1.0625                ,
   -7.75                  ,  86.375                , -1365.25               ,
    29049.03125           , -800572.75             ,  2.7741322625e7        ,
   -1.18052913025e9       ,  6.05239800516875e10   , -3.67941677853775e12   ,
    2.6170760990658388e014, -2.1531418140800295e016,  2.0288775575173016e018,
   -2.1708009902623771e020,  2.6173826968455815e022, -3.5324148876863878e024,
    5.3042033406864907e026, -8.8138218364311577e028,  1.6128065107490779e031,
   -3.2355470001722734e033,  7.0876727476537493e035, -1.6890450341293966e038,
    4.3639690731216831e040, -1.2185998827061261e043,  3.6670584803153006e045,
   -1.1859898526302099e048,  4.1120769493584015e050, -1.5249042436787620e053,
    6.0349693196941307e055, -2.5437161764210696e058,  1.1396923802632288e061,
   -5.4180861064753979e063,  2.7283654799994374e066, -1.4529750514918543e069,
    8.1705519371067450e071, -4.8445781606678368e074,  3.0246694206649519e077,
   -1.9858807961690493e080,  1.3694474620720087e083, -9.9070382984295808e085,
    7.5103780796592646e088, -5.9598418264260881e091,  4.9455988887500020e094,
   -4.2873596927020241e097,  3.8791952037716163e100, -3.6600317773156342e103,
    3.5978775704117284e106, -3.6818662617467813e109,  3.9192743066421374e112,
   -4.3363921885063858e115,  4.9833162711780838e118, -5.9438653020209606e121,
    7.3533439019770134e124, -9.4293465716973561e127,  1.2525196404154548e131,
   -1.7223787163994400e134,  2.4505178680729537e137, -3.6051616659014189e140,
    5.4813803836499771e143, -8.6083892012122616e146,  1.3957139354298160e150,
   -2.3350508860591630e153,  4.0291297374794860e156, -7.1669946227411534e159,
    1.3136385964069363e163, -2.4799083462304252e166,  4.8198083696385558e169,
   -9.6400031196958281e172,  1.9833611905147644e176, -4.1959717912682865e179,
    9.1243724595750010e182, -2.0386902382464212e186,  4.6786408066350383e189,
   -1.1024400389046488e193,  2.6662916424238258e196, -6.6165585014771755e199,
    1.6841726974970032e203, -4.3957474813006951e206,  1.1760766011899571e210,
   -3.2245094671360478e213,  9.0570855543185808e216, -2.6054618058433054e220,
    7.6741449421726560e223, -2.3136880427961752e227,  7.1382598572408242e230,
   -2.2530900128907084e234,  7.2736404696018159e237, -2.4010608416429639e241,
    8.1026279414941787e244, -2.7945745738098571e248,  9.8485095122481192e251,
   -3.5456055356238575e255,  1.3036999220919921e259, -4.8948166866453784e262,
    1.8761736309852136e266, -7.3399918877807488e269,  2.9303136033539038e273,
   -1.1935494277949469e277,  4.9589310621971370e280, -2.1012240064879845e284,
    9.0784179834777353e287, -3.9987113012775244e291,  1.7952380922182709e295,
   -8.2136799002055846e298,  3.8290431596908477e302, -1.8184610414701105e306
};

/// negative Dirichlet eta function
double em_numeric_neg_eta(int64_t n)
{
   if (n < 0) {
      if (em_numeric_iseven(n)) {
         return 0.0;
      } if (-(1 + n)/2 < (int64_t)(sizeof(NEG_ETA_NEG_N)/sizeof(NEG_ETA_NEG_N[0]))) {
         return NEG_ETA_NEG_N[-(1 + n)/2];
      } if (em_numeric_iseven((1 - n)/2)) {
         return __builtin_inf();
      }           return -__builtin_inf();
     
   } if (n == 0) {
      return -0.5;
   } if (n <= (int64_t)(sizeof(NEG_ETA)/sizeof(NEG_ETA[0]))) {
      return NEG_ETA[n - 1];
   }        return -1.0;
  
}

// NOLINTEND(misc-no-recursion)
