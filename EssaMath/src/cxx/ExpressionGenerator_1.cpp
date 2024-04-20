#include "include/Defines.hpp"
#include "include/ExpressionGenerator.hpp"
#include "include/Parser.hpp"
#include "include/Optimize.inl"

namespace Essa::Math{

        template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::error_node(){
         return ::Essa::Math::parser<T>::error_node();
        }

        template<typename T> void expression_generator<T>::init_synthesize_map()
         {
            if(details::disable_enhanced_features)
               return;
            synthesize_map_["(v)o(v)"] = synthesize_vov_expression<T>::process;
            synthesize_map_["(c)o(v)"] = synthesize_cov_expression<T>::process;
            synthesize_map_["(v)o(c)"] = synthesize_voc_expression<T>::process;

            #define register_synthezier(S)                      \
            synthesize_map_[S ::node_type::id()] = S ::process; \

            register_synthezier(synthesize_vovov_expression0<T>)
            register_synthezier(synthesize_vovov_expression1<T>)
            register_synthezier(synthesize_vovoc_expression0<T>)
            register_synthezier(synthesize_vovoc_expression1<T>)
            register_synthezier(synthesize_vocov_expression0<T>)
            register_synthezier(synthesize_vocov_expression1<T>)
            register_synthezier(synthesize_covov_expression0<T>)
            register_synthezier(synthesize_covov_expression1<T>)
            register_synthezier(synthesize_covoc_expression0<T>)
            register_synthezier(synthesize_covoc_expression1<T>)
            register_synthezier(synthesize_cocov_expression1<T>)
            register_synthezier(synthesize_vococ_expression0<T>)

            register_synthezier(synthesize_vovovov_expression0<T>)
            register_synthezier(synthesize_vovovoc_expression0<T>)
            register_synthezier(synthesize_vovocov_expression0<T>)
            register_synthezier(synthesize_vocovov_expression0<T>)
            register_synthezier(synthesize_covovov_expression0<T>)
            register_synthezier(synthesize_covocov_expression0<T>)
            register_synthezier(synthesize_vocovoc_expression0<T>)
            register_synthezier(synthesize_covovoc_expression0<T>)
            register_synthezier(synthesize_vococov_expression0<T>)

            register_synthezier(synthesize_vovovov_expression1<T>)
            register_synthezier(synthesize_vovovoc_expression1<T>)
            register_synthezier(synthesize_vovocov_expression1<T>)
            register_synthezier(synthesize_vocovov_expression1<T>)
            register_synthezier(synthesize_covovov_expression1<T>)
            register_synthezier(synthesize_covocov_expression1<T>)
            register_synthezier(synthesize_vocovoc_expression1<T>)
            register_synthezier(synthesize_covovoc_expression1<T>)
            register_synthezier(synthesize_vococov_expression1<T>)

            register_synthezier(synthesize_vovovov_expression2<T>)
            register_synthezier(synthesize_vovovoc_expression2<T>)
            register_synthezier(synthesize_vovocov_expression2<T>)
            register_synthezier(synthesize_vocovov_expression2<T>)
            register_synthezier(synthesize_covovov_expression2<T>)
            register_synthezier(synthesize_covocov_expression2<T>)
            register_synthezier(synthesize_vocovoc_expression2<T>)
            register_synthezier(synthesize_covovoc_expression2<T>)

            register_synthezier(synthesize_vovovov_expression3<T>)
            register_synthezier(synthesize_vovovoc_expression3<T>)
            register_synthezier(synthesize_vovocov_expression3<T>)
            register_synthezier(synthesize_vocovov_expression3<T>)
            register_synthezier(synthesize_covovov_expression3<T>)
            register_synthezier(synthesize_covocov_expression3<T>)
            register_synthezier(synthesize_vocovoc_expression3<T>)
            register_synthezier(synthesize_covovoc_expression3<T>)
            register_synthezier(synthesize_vococov_expression3<T>)

            register_synthezier(synthesize_vovovov_expression4<T>)
            register_synthezier(synthesize_vovovoc_expression4<T>)
            register_synthezier(synthesize_vovocov_expression4<T>)
            register_synthezier(synthesize_vocovov_expression4<T>)
            register_synthezier(synthesize_covovov_expression4<T>)
            register_synthezier(synthesize_covocov_expression4<T>)
            register_synthezier(synthesize_vocovoc_expression4<T>)
            register_synthezier(synthesize_covovoc_expression4<T>)
         }

         template<typename T> void expression_generator<T>::set_parser(parser_t& p)
         {
            parser_ = &p;
         }

         template<typename T> void expression_generator<T>::set_uom(unary_op_map_t& unary_op_map)
         {
            unary_op_map_ = &unary_op_map;
         }

         template<typename T> void expression_generator<T>::set_bom(binary_op_map_t& binary_op_map)
         {
            binary_op_map_ = &binary_op_map;
         }

         template<typename T> void expression_generator<T>::set_ibom(inv_binary_op_map_t& inv_binary_op_map)
         {
            inv_binary_op_map_ = &inv_binary_op_map;
         }

         template<typename T> void expression_generator<T>::set_sf3m(sf3_map_t& sf3_map)
         {
            sf3_map_ = &sf3_map;
         }

         template<typename T> void expression_generator<T>::set_sf4m(sf4_map_t& sf4_map)
         {
            sf4_map_ = &sf4_map;
         }

         template<typename T> void expression_generator<T>::set_allocator(details::node_allocator& na)
         {
            node_allocator_ = &na;
         }

         template<typename T> void expression_generator<T>::set_strength_reduction_state(const bool enabled)
         {
            strength_reduction_enabled_ = enabled;
         }

         template<typename T> bool expression_generator<T>::strength_reduction_enabled() const
         {
            return strength_reduction_enabled_;
         }

         template<typename T> bool expression_generator<T>::valid_operator(const details::operator_type& operation, expression_generator<T>::binary_functor_t& bop)
         {
            typename binary_op_map_t::iterator bop_itr = binary_op_map_->find(operation);

            if ((*binary_op_map_).end() == bop_itr)
               return false;

            bop = bop_itr->second;

            return true;
         }

         template<typename T> bool expression_generator<T>::valid_operator(const details::operator_type& operation, unary_functor_t& uop)
         {
            typename unary_op_map_t::iterator uop_itr = unary_op_map_->find(operation);

            if ((*unary_op_map_).end() == uop_itr)
               return false;

            uop = uop_itr->second;

            return true;
         }

         template<typename T> details::operator_type expression_generator<T>::get_operator(const expression_generator<T>::binary_functor_t& bop) const
         {
            return (*inv_binary_op_map_).find(bop)->second;
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const T& v) const
         {
            return node_allocator_->template allocate<literal_node_t>(v);
         }

         template<typename T> bool expression_generator<T>::unary_optimisable(const details::operator_type& operation) const
         {
            return (details::e_abs   == operation) || (details::e_acos  == operation) ||
                   (details::e_acosh == operation) || (details::e_asin  == operation) ||
                   (details::e_asinh == operation) || (details::e_atan  == operation) ||
                   (details::e_atanh == operation) || (details::e_ceil  == operation) ||
                   (details::e_cos   == operation) || (details::e_cosh  == operation) ||
                   (details::e_exp   == operation) || (details::e_expm1 == operation) ||
                   (details::e_floor == operation) || (details::e_log   == operation) ||
                   (details::e_log10 == operation) || (details::e_log2  == operation) ||
                   (details::e_log1p == operation) || (details::e_neg   == operation) ||
                   (details::e_pos   == operation) || (details::e_round == operation) ||
                   (details::e_sin   == operation) || (details::e_sinc  == operation) ||
                   (details::e_sinh  == operation) || (details::e_sqrt  == operation) ||
                   (details::e_tan   == operation) || (details::e_tanh  == operation) ||
                   (details::e_cot   == operation) || (details::e_sec   == operation) ||
                   (details::e_csc   == operation) || (details::e_r2d   == operation) ||
                   (details::e_d2r   == operation) || (details::e_d2g   == operation) ||
                   (details::e_g2d   == operation) || (details::e_notl  == operation) ||
                   (details::e_sgn   == operation) || (details::e_erf   == operation) ||
                   (details::e_erfc  == operation) || (details::e_ncdf  == operation) ||
                   (details::e_frac  == operation) || (details::e_trunc == operation) ;
         }

         template<typename T> bool expression_generator<T>::sf3_optimisable(const std::string& sf3id, trinary_functor_t& tfunc) const
         {
            typename sf3_map_t::const_iterator itr = sf3_map_->find(sf3id);

            if (sf3_map_->end() == itr)
               return false;
            else
               tfunc = itr->second.first;

            return true;
         }

         template<typename T> bool expression_generator<T>::sf4_optimisable(const std::string& sf4id, quaternary_functor_t& qfunc) const
         {
            typename sf4_map_t::const_iterator itr = sf4_map_->find(sf4id);

            if (sf4_map_->end() == itr)
               return false;
            else
               qfunc = itr->second.first;

            return true;
         }

         template<typename T> bool expression_generator<T>::sf3_optimisable(const std::string& sf3id, details::operator_type& operation) const
         {
            typename sf3_map_t::const_iterator itr = sf3_map_->find(sf3id);

            if (sf3_map_->end() == itr)
               return false;
            else
               operation = itr->second.second;

            return true;
         }

         template<typename T> bool expression_generator<T>::sf4_optimisable(const std::string& sf4id, details::operator_type& operation) const
         {
            typename sf4_map_t::const_iterator itr = sf4_map_->find(sf4id);

            if (sf4_map_->end() == itr)
               return false;
            else
               operation = itr->second.second;

            return true;
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr (&branch)[1])
         {
            if (0 == branch[0])
            {
               return error_node();
            }
            else if (details::is_null_node(branch[0]))
            {
               return branch[0];
            }
            else if (details::is_break_node(branch[0]))
            {
               return error_node();
            }
            else if (details::is_continue_node(branch[0]))
            {
               return error_node();
            }
            else if (details::is_constant_node(branch[0]))
            {
               return synthesize_expression<unary_node_t,1>(operation,branch);
            }
            else if (unary_optimisable(operation) && details::is_variable_node(branch[0]) && !details::disable_enhanced_features)
            {
               return synthesize_uv_expression(operation,branch);
            }
            else if (unary_optimisable(operation) && details::is_ivector_node(branch[0]) && !details::disable_enhanced_features)
            {
               return synthesize_uvec_expression(operation,branch);
            }
            else
               return synthesize_unary_expression(operation,branch);
         }

         template<typename T> bool expression_generator<T>::is_assignment_operation(const details::operator_type& operation) const
         {
            return (
                     (details::e_addass == operation) ||
                     (details::e_subass == operation) ||
                     (details::e_mulass == operation) ||
                     (details::e_divass == operation) ||
                     (details::e_modass == operation)
                   ) &&
                   parser_->settings().assignment_enabled(operation);
         }

         template<typename T> std::string expression_generator<T>::to_str(const details::operator_type& operation) const
         {
            switch (operation)
            {
               case details::e_add  : return "+"      ;
               case details::e_sub  : return "-"      ;
               case details::e_mul  : return "*"      ;
               case details::e_div  : return "/"      ;
               case details::e_mod  : return "mod"      ;
               case details::e_pow  : return "^"      ;
               case details::e_lt   : return "<"      ;
               case details::e_lte  : return "<="     ;
               case details::e_gt   : return ">"      ;
               case details::e_gte  : return ">="     ;
               case details::e_eq   : return "=="     ;
               case details::e_ne   : return "!="     ;
               case details::e_and  : return "and"    ;
               case details::e_nand : return "nand"   ;
               case details::e_or   : return "or"     ;
               case details::e_nor  : return "nor"    ;
               case details::e_xor  : return "xor"    ;
               case details::e_xnor : return "xnor"   ;
               default              : return "UNKNOWN";
            }
         }

         template<typename T> bool expression_generator<T>::operation_optimisable(const details::operator_type& operation) const
         {
            return (details::e_add  == operation) ||
                   (details::e_sub  == operation) ||
                   (details::e_mul  == operation) ||
                   (details::e_div  == operation) ||
                   (details::e_mod  == operation) ||
                   (details::e_pow  == operation) ||
                   (details::e_lt   == operation) ||
                   (details::e_lte  == operation) ||
                   (details::e_gt   == operation) ||
                   (details::e_gte  == operation) ||
                   (details::e_eq   == operation) ||
                   (details::e_ne   == operation) ||
                   (details::e_and  == operation) ||
                   (details::e_nand == operation) ||
                   (details::e_or   == operation) ||
                   (details::e_nor  == operation) ||
                   (details::e_xor  == operation) ||
                   (details::e_xnor == operation) ;
         }

         template<typename T> std::string expression_generator<T>::branch_to_id(expression_node_ptr branch) const
         {
            static const std::string null_str   ("(null)" );
            static const std::string const_str  ("(c)"    );
            static const std::string var_str    ("(v)"    );
            static const std::string vov_str    ("(vov)"  );
            static const std::string cov_str    ("(cov)"  );
            static const std::string voc_str    ("(voc)"  );
            static const std::string str_str    ("(s)"    );
            static const std::string strrng_str ("(rngs)" );
            static const std::string cs_str     ("(cs)"   );
            static const std::string cstrrng_str("(crngs)");

            if (details::is_null_node(branch))
               return null_str;
            else if (details::is_constant_node(branch))
               return const_str;
            else if (details::is_variable_node(branch))
               return var_str;
            else if (details::is_vov_node(branch))
               return vov_str;
            else if (details::is_cov_node(branch))
               return cov_str;
            else if (details::is_voc_node(branch))
               return voc_str;
            else if (details::is_t0ot1ot2_node(branch))
               return "(" + dynamic_cast<details::T0oT1oT2_base_node<T>*>(branch)->type_id() + ")";
            else if (details::is_t0ot1ot2ot3_node(branch))
               return "(" + dynamic_cast<details::T0oT1oT2oT3_base_node<T>*>(branch)->type_id() + ")";
            else
               return "ERROR";
         }

         template<typename T> std::string expression_generator<T>::branch_to_id(expression_node_ptr (&branch)[2]) const
         {
            return branch_to_id(branch[0]) + std::string("o") + branch_to_id(branch[1]);
         }

         template<typename T> bool expression_generator<T>::cov_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_constant_node(branch[0]) &&
                      details::is_variable_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::voc_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_variable_node(branch[0]) &&
                      details::is_constant_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::vov_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_variable_node(branch[0]) &&
                      details::is_variable_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::cob_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_constant_node(branch[0]) &&
                     !details::is_constant_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::boc_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return !details::is_constant_node(branch[0]) &&
                       details::is_constant_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::cocob_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (
                 (details::e_add == operation) ||
                 (details::e_sub == operation) ||
                 (details::e_mul == operation) ||
                 (details::e_div == operation)
               )
            {
               return (details::is_constant_node(branch[0]) && details::is_cob_node(branch[1])) ||
                      (details::is_constant_node(branch[1]) && details::is_cob_node(branch[0])) ;
            }
            else
               return false;
         }

         template<typename T> bool expression_generator<T>::coboc_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (
                 (details::e_add == operation) ||
                 (details::e_sub == operation) ||
                 (details::e_mul == operation) ||
                 (details::e_div == operation)
               )
            {
               return (details::is_constant_node(branch[0]) && details::is_boc_node(branch[1])) ||
                      (details::is_constant_node(branch[1]) && details::is_boc_node(branch[0])) ;
            }
            else
               return false;
         }

         template<typename T> bool expression_generator<T>::uvouv_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_uv_node(branch[0]) &&
                      details::is_uv_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::vob_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return details::is_variable_node(branch[0]) &&
                     !details::is_variable_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::bov_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return !details::is_variable_node(branch[0]) &&
                       details::is_variable_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::binext_optimisable(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!operation_optimisable(operation))
               return false;
            else
               return !details::is_constant_node(branch[0]) ||
                      !details::is_constant_node(branch[1]) ;
         }

         template<typename T> bool expression_generator<T>::is_invalid_assignment_op(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (is_assignment_operation(operation))
            {

                  return (
                           !details::is_variable_node          (branch[0]) &&
                           !details::is_vector_elem_node       (branch[0]) &&
                           !details::is_rebasevector_elem_node (branch[0]) &&
                           !details::is_rebasevector_celem_node(branch[0]) &&
                           !details::is_vector_node            (branch[0])
                         );
            }
            else
               return false;
         }

         template<typename T> bool expression_generator<T>::is_constpow_operation(const details::operator_type& operation, expression_node_ptr(&branch)[2]) const
         {
            if (
                 !details::is_constant_node(branch[1]) ||
                  details::is_constant_node(branch[0]) ||
                  details::is_variable_node(branch[0]) ||
                  details::is_vector_node  (branch[0])
               )
               return false;

            const T c = static_cast<details::literal_node<T>*>(branch[1])->value();

            return cardinal_pow_optimisable(operation, c);
         }

         template<typename T> bool expression_generator<T>::is_invalid_break_continue_op(expression_node_ptr (&branch)[2]) const
         {
            return (
                     details::is_break_node   (branch[0]) ||
                     details::is_break_node   (branch[1]) ||
                     details::is_continue_node(branch[0]) ||
                     details::is_continue_node(branch[1])
                   );
         }

         template<typename T> bool expression_generator<T>::is_shortcircuit_expression(const details::operator_type& operation) const
         {
            if(details::disable_sc_andor)
               return false;
            return (
                     (details::e_scand == operation) ||
                     (details::e_scor  == operation)
                   );
         }

         template<typename T> bool expression_generator<T>::is_null_present(expression_node_ptr (&branch)[2]) const
         {
            return (
                     details::is_null_node(branch[0]) ||
                     details::is_null_node(branch[1])
                   );
         }

         template<typename T> bool expression_generator<T>::is_vector_eqineq_logic_operation(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!is_ivector_node(branch[0]) && !is_ivector_node(branch[1]))
               return false;
            else
               return (
                        (details::e_lt    == operation) ||
                        (details::e_lte   == operation) ||
                        (details::e_gt    == operation) ||
                        (details::e_gte   == operation) ||
                        (details::e_eq    == operation) ||
                        (details::e_ne    == operation) ||
                        (details::e_equal == operation) ||
                        (details::e_and   == operation) ||
                        (details::e_nand  == operation) ||
                        (details::e_or    == operation) ||
                        (details::e_nor   == operation) ||
                        (details::e_xor   == operation) ||
                        (details::e_xnor  == operation)
                      );
         }

         template<typename T> bool expression_generator<T>::is_vector_arithmetic_operation(const details::operator_type& operation, expression_node_ptr (&branch)[2]) const
         {
            if (!is_ivector_node(branch[0]) && !is_ivector_node(branch[1]))
               return false;
            else
               return (
                        (details::e_add == operation) ||
                        (details::e_sub == operation) ||
                        (details::e_mul == operation) ||
                        (details::e_div == operation) ||
                        (details::e_pow == operation)
                      );
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr (&branch)[2])
         {
            if ((0 == branch[0]) || (0 == branch[1]))
            {
               return error_node();
            }
            else if (is_invalid_assignment_op(operation,branch))
            {
               return error_node();
            }
            else if (is_invalid_break_continue_op(branch))
            {
               return error_node();
            }
            else if (is_vector_eqineq_logic_operation(operation, branch))
            {
               return synthesize_veceqineqlogic_operation_expression(operation, branch);
            }
            else if (is_vector_arithmetic_operation(operation, branch))
            {
               return synthesize_vecarithmetic_operation_expression(operation, branch);
            }
            else if (is_shortcircuit_expression(operation))
            {
               return synthesize_shortcircuit_expression(operation, branch);
            }
            else if (is_null_present(branch))
            {
               return synthesize_null_expression(operation, branch);
            }
            else if (is_constpow_operation(operation, branch) && !details::disable_cardinal_pow_optimisation)
            {
               return cardinal_pow_optimisation(branch);
            }

            expression_node_ptr result = error_node();

            if(!details::disable_enhanced_features){
               if (synthesize_expression(operation, branch, result))
               {
                  return result;
               }
               else
               {
                  /*
                     Possible reductions:
                     1. c o cob -> cob
                     2. cob o c -> cob
                     3. c o boc -> boc
                     4. boc o c -> boc
                  */
                  result = error_node();

                  if (cocob_optimisable(operation, branch))
                  {
                     result = synthesize_cocob_expression<T>::process((*this), operation, branch);
                  }
                  else if (coboc_optimisable(operation, branch) && (0 == result))
                  {
                     result = synthesize_coboc_expression<T>::process((*this), operation, branch);
                  }

                  if (result)
                     return result;
               }

               if (uvouv_optimisable(operation, branch))
               {
                  return synthesize_uvouv_expression(operation, branch);
               }
               else if (vob_optimisable(operation, branch))
               {
                  return synthesize_vob_expression<T>::process((*this), operation, branch);
               }
               else if (bov_optimisable(operation, branch))
               {
                  return synthesize_bov_expression<T>::process((*this), operation, branch);
               }
               else if (cob_optimisable(operation, branch))
               {
                  return synthesize_cob_expression<T>::process((*this), operation, branch);
               }
               else if (boc_optimisable(operation, branch))
               {
                  return synthesize_boc_expression<T>::process((*this), operation, branch);
               }
               else if (cov_optimisable(operation, branch))
               {
                  return synthesize_cov_expression<T>::process((*this), operation, branch);
               }
               else if (binext_optimisable(operation, branch))
               {
                  return synthesize_binary_ext_expression<T>::process((*this), operation, branch);
               }
               else
                  return synthesize_expression<binary_node_t,2>(operation, branch);
            }else{
               return synthesize_expression<binary_node_t,2>(operation, branch);
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr (&branch)[3])
         {
            if (
                 (0 == branch[0]) ||
                 (0 == branch[1]) ||
                 (0 == branch[2])
               )
            {
               details::free_all_nodes(*node_allocator(),branch);

               return error_node();
            }
            else
               return synthesize_expression<trinary_node_t,3>(operation, branch);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr (&branch)[4])
         {
            return synthesize_expression<quaternary_node_t,4>(operation,branch);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr b0)
         {
            expression_node_ptr branch[1] = { b0 };
            return (*this)(operation,branch);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::operator() (const details::operator_type& operation, expression_node_ptr& b0, expression_node_ptr& b1)
         {
            expression_node_ptr result = error_node();

            if ((0 != b0) && (0 != b1))
            {
               expression_node_ptr branch[2] = { b0, b1 };
               result = expression_generator<T>::operator()(operation, branch);
               b0 = branch[0];
               b1 = branch[1];
            }

            return result;
         }

         template class expression_generator<float>;
         template class expression_generator<double>;
         template class expression_generator<long double>;
         template class expression_generator<std::complex<float>>;
         template class expression_generator<std::complex<double>>;
         template class expression_generator<std::complex<long double>>;
}
