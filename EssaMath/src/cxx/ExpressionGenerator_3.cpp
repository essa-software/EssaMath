#include "include/ExpressionGenerator.hpp"
#include "include/Parser.hpp"
#include "include/Optimize.inl"

namespace Essa::Math{
         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_veceqineqlogic_operation_expression(const details::operator_type& operation,
                                                                                   expression_node_ptr (&branch)[2])
         {
            const bool is_b0_ivec = details::is_ivector_node(branch[0]);
            const bool is_b1_ivec = details::is_ivector_node(branch[1]);

            #define batch_eqineq_logic_case                 \
            case_stmt(details::e_lt    , details::lt_op   ) \
            case_stmt(details::e_lte   , details::lte_op  ) \
            case_stmt(details::e_gt    , details::gt_op   ) \
            case_stmt(details::e_gte   , details::gte_op  ) \
            case_stmt(details::e_eq    , details::eq_op   ) \
            case_stmt(details::e_ne    , details::ne_op   ) \
            case_stmt(details::e_equal , details::equal_op) \

            if (is_b0_ivec && is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_vecvec_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  batch_eqineq_logic_case
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else if (is_b0_ivec && !is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_vecval_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  batch_eqineq_logic_case
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else if (!is_b0_ivec && is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_valvec_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  batch_eqineq_logic_case
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else
               return error_node();

            #undef batch_eqineq_logic_case
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_vecarithmetic_operation_expression(const details::operator_type& operation,
                                                                                  expression_node_ptr (&branch)[2])
         {
            const bool is_b0_ivec = details::is_ivector_node(branch[0]);
            const bool is_b1_ivec = details::is_ivector_node(branch[1]);

            #define vector_ops                          \
            case_stmt(details::e_add , details::add_op) \
            case_stmt(details::e_sub , details::sub_op) \
            case_stmt(details::e_mul , details::mul_op) \
            case_stmt(details::e_div , details::div_op) \

            if (is_b0_ivec && is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_vecvec_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  vector_ops
                  case_stmt(details::e_pow,details:: pow_op)
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else if (is_b0_ivec && !is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_vecval_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  vector_ops
                  case_stmt(details::e_pow,details:: pow_op)
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else if (!is_b0_ivec && is_b1_ivec)
            {
               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                                    \
                  case op0 : return node_allocator_->                                                            \
                                template allocate_rrr<typename details::vec_binop_valvec_node<T,op1<T> > > \
                                   (operation, branch[0], branch[1]);                                            \

                  vector_ops
                  #undef case_stmt
                  default : return error_node();
               }
            }
            else
               return error_node();

            #undef vector_ops
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_shortcircuit_expression(const details::operator_type& operation, expression_node_ptr (&branch)[2])
         {
            if(details::disable_sc_andor)
               return error_node();
            expression_node_ptr result = error_node();

            if (details::is_constant_node(branch[0]))
            {
               if (
                    (details::e_scand == operation) &&
                    std::equal_to<T>()(T(0),branch[0]->value())
                  )
                  result = node_allocator_->template allocate_c<literal_node_t>(T(0));
               else if (
                         (details::e_scor == operation) &&
                         std::not_equal_to<T>()(T(0),branch[0]->value())
                       )
                  result = node_allocator_->template allocate_c<literal_node_t>(T(1));
            }

            if (details::is_constant_node(branch[1]) && (0 == result))
            {
               if (
                    (details::e_scand == operation) &&
                    std::equal_to<T>()(T(0),branch[1]->value())
                  )
                  result = node_allocator_->template allocate_c<literal_node_t>(T(0));
               else if (
                         (details::e_scor == operation) &&
                         std::not_equal_to<T>()(T(0),branch[1]->value())
                       )
                  result = node_allocator_->template allocate_c<literal_node_t>(T(1));
            }

            if (result)
            {
               details::free_node(*node_allocator_, branch[0]);
               details::free_node(*node_allocator_, branch[1]);

               return result;
            }
            else if (details::e_scand == operation)
            {
               return synthesize_expression<scand_node_t,2>(operation, branch);
            }
            else if (details::e_scor == operation)
            {
               return synthesize_expression<scor_node_t,2>(operation, branch);
            }
            else
               return error_node();
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::cardinal_pow_optimisation(const T& v, const T& c)
         {
            if(details::disable_cardinal_pow_optimisation)
               return error_node();
            const bool not_recipricol = details::is_true(details::numeric::geq<T>(c, T(0)));
            const unsigned int p = static_cast<unsigned int>(details::numeric::to_int32(details::numeric::abs(c)));

            if (0 == p)
               return node_allocator_->template allocate_c<literal_node_t>(T(1));
            else if (std::equal_to<T>()(T(2),c))
            {
               return node_allocator_->
                  template allocate_rr<typename details::vov_node<T,details::mul_op<T> > >(v,v);
            }
            else
            {
               if (not_recipricol)
                  return cardinal_pow_optimisation_impl<T,details::ipow_node>(v,p);
               else
                  return cardinal_pow_optimisation_impl<T,details::ipowinv_node>(v,p);
            }
         }

         template<typename T> bool expression_generator<T>::cardinal_pow_optimisable(const details::operator_type& operation, const T& c) const
         {
            if(details::disable_cardinal_pow_optimisation)
               return false;
            return (details::e_pow == operation) && (details::is_true(details::numeric::leq<T>(details::numeric::abs(c), T(60)))) && details::numeric::is_integer(c);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::cardinal_pow_optimisation(expression_node_ptr (&branch)[2])
         {
            if(details::disable_cardinal_pow_optimisation)
               return error_node();
            const T c = static_cast<details::literal_node<T>*>(branch[1])->value();
            const bool not_recipricol = details::is_true(details::numeric::geq<T>(c, T(0)));
            const unsigned int p = static_cast<unsigned int>(details::numeric::to_int32(details::numeric::abs(c)));

            node_allocator_->free(branch[1]);

            if (0 == p)
            {
               details::free_all_nodes(*node_allocator_, branch);

               return node_allocator_->template allocate_c<literal_node_t>(T(1));
            }
            else if (not_recipricol)
               return cardinal_pow_optimisation_impl<expression_node_ptr,details::bipow_node>(branch[0],p);
            else
               return cardinal_pow_optimisation_impl<expression_node_ptr,details::bipowninv_node>(branch[0],p);
         }

         template<typename T> bool expression_generator<T>::synthesize_expression(const details::operator_type& operation,
                                           expression_node_ptr (&branch)[2],
                                           expression_node_ptr& result)
         {
            result = error_node();

            if (!operation_optimisable(operation) ||  details::disable_enhanced_features)
               return false;

            const std::string node_id = branch_to_id(branch);

            const typename synthesize_map_t::iterator itr = synthesize_map_.find(node_id);

            if (synthesize_map_.end() != itr)
            {
               result = itr->second((*this), operation, branch);

               return true;
            }
            else
               return false;
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_uvouv_expression(const details::operator_type& operation, expression_node_ptr (&branch)[2])
         {
            // Definition: uv o uv
            details::operator_type o0 = static_cast<details::uv_base_node<T>*>(branch[0])->operation();
            details::operator_type o1 = static_cast<details::uv_base_node<T>*>(branch[1])->operation();
            const T& v0 = static_cast<details::uv_base_node<T>*>(branch[0])->v();
            const T& v1 = static_cast<details::uv_base_node<T>*>(branch[1])->v();
            unary_functor_t u0 = reinterpret_cast<unary_functor_t> (0);
            unary_functor_t u1 = reinterpret_cast<unary_functor_t> (0);
            typename expression_generator<T>::binary_functor_t f = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

            if (!valid_operator(o0,u0))
               return error_node();
            else if (!valid_operator(o1,u1))
               return error_node();
            else if (!valid_operator(operation,f))
               return error_node();

            expression_node_ptr result = error_node();

            if (
                 (details::e_neg == o0) &&
                 (details::e_neg == o1)
               )
            {
               switch (operation)
               {
                  // (-v0 + -v1) --> -(v0 + v1)
                  case details::e_add : result = (*this)(details::e_neg,
                                                    node_allocator_->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::add_op<T> > >(v0, v1));
                                        exprtk_debug(("(-v0 + -v1) --> -(v0 + v1)\n"));
                                        break;

                  // (-v0 - -v1) --> (v1 - v0)
                  case details::e_sub : result = node_allocator_->
                                                    template allocate_rr<typename details::
                                                       vov_node<T,details::sub_op<T> > >(v1, v0);
                                        exprtk_debug(("(-v0 - -v1) --> (v1 - v0)\n"));
                                        break;

                  // (-v0 * -v1) --> (v0 * v1)
                  case details::e_mul : result = node_allocator_->
                                                    template allocate_rr<typename details::
                                                       vov_node<T,details::mul_op<T> > >(v0, v1);
                                        exprtk_debug(("(-v0 * -v1) --> (v0 * v1)\n"));
                                        break;

                  // (-v0 / -v1) --> (v0 / v1)
                  case details::e_div : result = node_allocator_->
                                                    template allocate_rr<typename details::
                                                       vov_node<T,details::div_op<T> > >(v0, v1);
                                        exprtk_debug(("(-v0 / -v1) --> (v0 / v1)\n"));
                                        break;

                  default             : break;
               }
            }

            if (0 == result)
            {
               result = node_allocator_->template allocate_rrrrr<typename details::uvouv_node<T> >(v0, v1, u0, u1, f);
            }

            details::free_all_nodes(*node_allocator_,branch);
            return result;
         }

         #undef basic_opr_switch_statements
         #undef extended_opr_switch_statements
         #undef unary_opr_switch_statements

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_null_expression(const details::operator_type& operation, expression_node_ptr (&branch)[2])
         {
            /*
             Note: The following are the type promotion rules
             that relate to operations that include 'null':
             0. null ==/!=     null --> true false
             1. null operation null --> null
             2. x    ==/!=     null --> true/false
             3. null ==/!=     x    --> true/false
             4. x   operation  null --> x
             5. null operation x    --> x
            */

            typedef typename details::null_eq_node<T> nulleq_node_t;

            const bool b0_null = details::is_null_node(branch[0]);
            const bool b1_null = details::is_null_node(branch[1]);

            if (b0_null && b1_null)
            {
               expression_node_ptr result = error_node();

               if (details::e_eq == operation)
                  result = node_allocator_->allocate_c<literal_node_t>(T(1));
               else if (details::e_ne == operation)
                  result = node_allocator_->allocate_c<literal_node_t>(T(0));

               if (result)
               {
                  details::free_node(*node_allocator_,branch[0]);
                  details::free_node(*node_allocator_,branch[1]);

                  return result;
               }

               details::free_node(*node_allocator_,branch[1]);

               return branch[0];
            }
            else if (details::e_eq == operation)
            {
               expression_node_ptr result = node_allocator_->
                                                allocate_rc<nulleq_node_t>(branch[b0_null ? 0 : 1],true);

               details::free_node(*node_allocator_,branch[b0_null ? 1 : 0]);

               return result;
            }
            else if (details::e_ne == operation)
            {
               expression_node_ptr result = node_allocator_->
                                                allocate_rc<nulleq_node_t>(branch[b0_null ? 0 : 1],false);

               details::free_node(*node_allocator_,branch[b0_null ? 1 : 0]);

               return result;
            }
            else if (b0_null)
            {
               details::free_node(*node_allocator_,branch[0]);
               branch[0] = branch[1];
               branch[1] = error_node();
            }
            else if (b1_null)
            {
               details::free_node(*node_allocator_,branch[1]);
               branch[1] = error_node();
            }

            if (
                 (details::e_add == operation) || (details::e_sub == operation) ||
                 (details::e_mul == operation) || (details::e_div == operation) ||
                 (details::e_mod == operation) || (details::e_pow == operation)
               )
            {
               return branch[0];
            }

            details::free_node(*node_allocator_, branch[0]);

            if (
                 (details::e_lt    == operation) || (details::e_lte  == operation) ||
                 (details::e_gt    == operation) || (details::e_gte  == operation) ||
                 (details::e_and   == operation) || (details::e_nand == operation) ||
                 (details::e_or    == operation) || (details::e_nor  == operation) ||
                 (details::e_xor   == operation) || (details::e_xnor == operation) ||
                 (details::e_in    == operation) || (details::e_like == operation) ||
                 (details::e_ilike == operation)
               )
            {
               return node_allocator_->allocate_c<literal_node_t>(T(0));
            }

            return node_allocator_->template allocate<details::null_node<T> >();
         }

         template class expression_generator<float>;
         template class expression_generator<double>;
         template class expression_generator<long double>;
         template class expression_generator<std::complex<float>>;
         template class expression_generator<std::complex<double>>;
         template class expression_generator<std::complex<long double>>;
}
