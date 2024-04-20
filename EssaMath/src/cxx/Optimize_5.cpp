#include "include/Optimize.inl"
#include "include/Parser.hpp"

namespace Essa::Math{

            template<typename T>  details::expression_node<T>* synthesize_vococov_expression4<T>::process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2])
            {
               // ((v0 o0 (c0 o1 c1)) o2 v1) - Not possible
               exprtk_debug(("((v0 o0 (c0 o1 c1)) o2 v1) - Not possible\n"));
               return expression_generator<T>::error_node();
            }

            template<typename T>  std::string synthesize_vococov_expression4<T>::id(expression_generator<T>&,
                                         const details::operator_type,
                                         const details::operator_type,
                                         const details::operator_type)
            {
               return "INVALID";
            }
            compile_with_templates(synthesize_vococov_expression4)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_binary_ext_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               const bool left_neg  = is_neg_unary_node(branch[0]);
               const bool right_neg = is_neg_unary_node(branch[1]);

               if (left_neg && right_neg)
               {
                  if (
                       (details::e_add == operation) ||
                       (details::e_sub == operation) ||
                       (details::e_mul == operation) ||
                       (details::e_div == operation)
                     )
                  {
                     if (
                          !expr_gen.parser()->simplify_unary_negation_branch(branch[0]) ||
                          !expr_gen.parser()->simplify_unary_negation_branch(branch[1])
                        )
                     {
                        details::free_all_nodes(*expr_gen.node_allocator(),branch);

                        return expression_generator<T>::error_node();
                     }
                  }

                  switch (operation)
                  {
                                           // -f(x + 1) + -g(y + 1) --> -(f(x + 1) + g(y + 1))
                     case details::e_add : return expr_gen(details::e_neg,
                                              expr_gen.node_allocator()->
                                                 template allocate<typename details::binary_ext_node<T,details::add_op<T> > >
                                                    (branch[0],branch[1]));

                                           // -f(x + 1) - -g(y + 1) --> g(y + 1) - f(x + 1)
                     case details::e_sub : return expr_gen.node_allocator()->
                                              template allocate<typename details::binary_ext_node<T,details::sub_op<T> > >
                                                 (branch[1],branch[0]);

                     default             : break;
                  }
               }
               else if (left_neg && !right_neg)
               {
                  if (
                       (details::e_add == operation) ||
                       (details::e_sub == operation) ||
                       (details::e_mul == operation) ||
                       (details::e_div == operation)
                     )
                  {
                     if (!expr_gen.parser()->simplify_unary_negation_branch(branch[0]))
                     {
                        details::free_all_nodes(*expr_gen.node_allocator(),branch);

                        return expression_generator<T>::error_node();
                     }

                     switch (operation)
                     {
                                              // -f(x + 1) + g(y + 1) --> g(y + 1) - f(x + 1)
                        case details::e_add : return expr_gen.node_allocator()->
                                                 template allocate<typename details::binary_ext_node<T,details::sub_op<T> > >
                                                   (branch[1], branch[0]);

                                              // -f(x + 1) - g(y + 1) --> -(f(x + 1) + g(y + 1))
                        case details::e_sub : return expr_gen(details::e_neg,
                                                 expr_gen.node_allocator()->
                                                    template allocate<typename details::binary_ext_node<T,details::add_op<T> > >
                                                       (branch[0], branch[1]));

                                              // -f(x + 1) * g(y + 1) --> -(f(x + 1) * g(y + 1))
                        case details::e_mul : return expr_gen(details::e_neg,
                                                 expr_gen.node_allocator()->
                                                    template allocate<typename details::binary_ext_node<T,details::mul_op<T> > >
                                                       (branch[0], branch[1]));

                                              // -f(x + 1) / g(y + 1) --> -(f(x + 1) / g(y + 1))
                        case details::e_div : return expr_gen(details::e_neg,
                                                 expr_gen.node_allocator()->
                                                    template allocate<typename details::binary_ext_node<T,details::div_op<T> > >
                                                       (branch[0], branch[1]));

                        default             : return expression_generator<T>::error_node();
                     }
                  }
               }
               else if (!left_neg && right_neg)
               {
                  if (
                       (details::e_add == operation) ||
                       (details::e_sub == operation) ||
                       (details::e_mul == operation) ||
                       (details::e_div == operation)
                     )
                  {
                     if (!expr_gen.parser()->simplify_unary_negation_branch(branch[1]))
                     {
                        details::free_all_nodes(*expr_gen.node_allocator(),branch);

                        return expression_generator<T>::error_node();
                     }

                     switch (operation)
                     {
                                              // f(x + 1) + -g(y + 1) --> f(x + 1) - g(y + 1)
                        case details::e_add : return expr_gen.node_allocator()->
                                                 template allocate<typename details::binary_ext_node<T,details::sub_op<T> > >
                                                   (branch[0], branch[1]);

                                              // f(x + 1) - - g(y + 1) --> f(x + 1) + g(y + 1)
                        case details::e_sub : return expr_gen.node_allocator()->
                                                 template allocate<typename details::binary_ext_node<T,details::add_op<T> > >
                                                   (branch[0], branch[1]);

                                              // f(x + 1) * -g(y + 1) --> -(f(x + 1) * g(y + 1))
                        case details::e_mul : return expr_gen(details::e_neg,
                                                 expr_gen.node_allocator()->
                                                    template allocate<typename details::binary_ext_node<T,details::mul_op<T> > >
                                                       (branch[0], branch[1]));

                                              // f(x + 1) / -g(y + 1) --> -(f(x + 1) / g(y + 1))
                        case details::e_div : return expr_gen(details::e_neg,
                                                 expr_gen.node_allocator()->
                                                    template allocate<typename details::binary_ext_node<T,details::div_op<T> > >
                                                       (branch[0], branch[1]));

                        default             : return expression_generator<T>::error_node();
                     }
                  }
               }

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                          \
                  case op0 : return expr_gen.node_allocator()->                                         \
                                template allocate<typename details::binary_ext_node<T,op1<T> > > \
                                   (branch[0], branch[1]);                                             \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_binary_ext_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_vob_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               const T& v = static_cast<details::variable_node<T>*>(branch[0])->ref();

               if (details::is_sf3ext_node(branch[1]))
               {
                  typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

                  const bool synthesis_result =
                     synthesize_sf4ext_expression<T>::template compile_right<typename expression_generator<T>::vtype>
                        (expr_gen, v, operation, branch[1], result);

                  if (synthesis_result)
                  {
                     details::free_node(*expr_gen.node_allocator(),branch[1]);
                     return result;
                  }
               }

               if (
                    (details::e_mul == operation) ||
                    (details::e_div == operation)
                  )
               {
                  if (details::is_uv_node(branch[1]))
                  {
                     typedef details::uv_base_node<T>* uvbn_ptr_t;

                     details::operator_type o = static_cast<uvbn_ptr_t>(branch[1])->operation();

                     if (details::e_neg == o)
                     {
                        const T& v1 = static_cast<uvbn_ptr_t>(branch[1])->v();

                        details::free_node(*expr_gen.node_allocator(),branch[1]);

                        switch (operation)
                        {
                           case details::e_mul : return expr_gen(details::e_neg,
                                                    expr_gen.node_allocator()->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::mul_op<T> > >(v,v1));

                           case details::e_div : return expr_gen(details::e_neg,
                                                    expr_gen.node_allocator()->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::div_op<T> > >(v,v1));

                           default             : break;
                        }
                     }
                  }
               }

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_rc<typename details::vob_node<T,op1<T> > > \
                                   (v, branch[1]);                                                 \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_vob_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_bov_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               const T& v = static_cast<details::variable_node<T>*>(branch[1])->ref();

               if (details::is_sf3ext_node(branch[0]))
               {
                  typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

                  const bool synthesis_result =
                     synthesize_sf4ext_expression<T>::template compile_left<typename expression_generator<T>::vtype>
                        (expr_gen, v, operation, branch[0], result);

                  if (synthesis_result)
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);

                     return result;
                  }
               }

               if (
                    (details::e_add == operation) ||
                    (details::e_sub == operation) ||
                    (details::e_mul == operation) ||
                    (details::e_div == operation)
                  )
               {
                  if (details::is_uv_node(branch[0]))
                  {
                     typedef details::uv_base_node<T>* uvbn_ptr_t;

                     details::operator_type o = static_cast<uvbn_ptr_t>(branch[0])->operation();

                     if (details::e_neg == o)
                     {
                        const T& v0 = static_cast<uvbn_ptr_t>(branch[0])->v();

                        details::free_node(*expr_gen.node_allocator(),branch[0]);

                        switch (operation)
                        {
                           case details::e_add : return expr_gen.node_allocator()->
                                                    template allocate_rr<typename details::
                                                       vov_node<T,details::sub_op<T> > >(v,v0);

                           case details::e_sub : return expr_gen(details::e_neg,
                                                    expr_gen.node_allocator()->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::add_op<T> > >(v0,v));

                           case details::e_mul : return expr_gen(details::e_neg,
                                                    expr_gen.node_allocator()->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::mul_op<T> > >(v0,v));

                           case details::e_div : return expr_gen(details::e_neg,
                                                    expr_gen.node_allocator()->
                                                       template allocate_rr<typename details::
                                                          vov_node<T,details::div_op<T> > >(v0,v));
                           default : break;
                        }
                     }
                  }
               }

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_cr<typename details::bov_node<T,op1<T> > > \
                                   (branch[0], v);                                                 \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_bov_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_cob_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               const T c = static_cast<details::literal_node<T>*>(branch[0])->value();

               details::free_node(*expr_gen.node_allocator(),branch[0]);

               if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
               {
                  details::free_node(*expr_gen.node_allocator(),branch[1]);

                  return expr_gen(T(0));
               }
               else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
               {
                  details::free_node(*expr_gen.node_allocator(), branch[1]);

                  return expr_gen(T(0));
               }
               else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  return branch[1];
               else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  return branch[1];

               if (details::is_cob_node(branch[1]))
               {
                  // Simplify expressions of the form:
                  // 1. (1 * (2 * (3 * (4 * (5 * (6 * (7 * (8 * (9 + x))))))))) --> 40320 * (9 + x)
                  // 2. (1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + x))))))))) --> 45 + x
                  if (
                       (details::e_mul == operation) ||
                       (details::e_add == operation)
                     )
                  {
                     details::cob_base_node<T>* cobnode = static_cast<details::cob_base_node<T>*>(branch[1]);

                     if (operation == cobnode->operation())
                     {
                        switch (operation)
                        {
                           case details::e_add : cobnode->set_c(c + cobnode->c()); break;
                           case details::e_mul : cobnode->set_c(c * cobnode->c()); break;
                           default             : return expression_generator<T>::error_node();
                        }

                        return cobnode;
                     }
                  }

                  if (operation == details::e_mul)
                  {
                     details::cob_base_node<T>* cobnode = static_cast<details::cob_base_node<T>*>(branch[1]);
                     details::operator_type cob_opr = cobnode->operation();

                     if (
                          (details::e_div == cob_opr) ||
                          (details::e_mul == cob_opr)
                        )
                     {
                        switch (cob_opr)
                        {
                           case details::e_div : cobnode->set_c(c * cobnode->c()); break;
                           case details::e_mul : cobnode->set_c(cobnode->c() / c); break;
                           default             : return expression_generator<T>::error_node();
                        }

                        return cobnode;
                     }
                  }
                  else if (operation == details::e_div)
                  {
                     details::cob_base_node<T>* cobnode = static_cast<details::cob_base_node<T>*>(branch[1]);
                     details::operator_type cob_opr = cobnode->operation();

                     if (
                          (details::e_div == cob_opr) ||
                          (details::e_mul == cob_opr)
                        )
                     {
                        details::expression_node<T>* new_cobnode = expression_generator<T>::error_node();

                        switch (cob_opr)
                        {
                           case details::e_div : new_cobnode = expr_gen.node_allocator()->
                                                    template allocate_tt<typename details::cob_node<T,details::mul_op<T> > >
                                                       (c / cobnode->c(), cobnode->move_branch(0));
                                                 break;

                           case details::e_mul : new_cobnode = expr_gen.node_allocator()->
                                                    template allocate_tt<typename details::cob_node<T,details::div_op<T> > >
                                                       (c / cobnode->c(), cobnode->move_branch(0));
                                                 break;

                           default             : return expression_generator<T>::error_node();
                        }

                        details::free_node(*expr_gen.node_allocator(),branch[1]);

                        return new_cobnode;
                     }
                  }
               }
               else if (details::is_sf3ext_node(branch[1]))
               {
                  typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

                  const bool synthesis_result =
                     synthesize_sf4ext_expression<T>::template compile_right<typename expression_generator<T>::ctype>
                        (expr_gen, c, operation, branch[1], result);

                  if (synthesis_result)
                  {
                     details::free_node(*expr_gen.node_allocator(),branch[1]);

                     return result;
                  }
               }

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_tt<typename details::cob_node<T,op1<T> > > \
                                   (c,  branch[1]);                                                \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_cob_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_boc_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               const T c = static_cast<details::literal_node<T>*>(branch[1])->value();

               details::free_node(*(expr_gen.node_allocator()), branch[1]);

               if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
               {
                  details::free_node(*expr_gen.node_allocator(), branch[0]);

                  return expr_gen(T(0));
               }
               else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
               {
                  details::free_node(*expr_gen.node_allocator(), branch[0]);

                  return expr_gen(std::numeric_limits<T>::quiet_NaN());
               }
               else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  return branch[0];
               else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  return branch[0];

               if (details::is_boc_node(branch[0]))
               {
                  // Simplify expressions of the form:
                  // 1. (((((((((x + 9) * 8) * 7) * 6) * 5) * 4) * 3) * 2) * 1) --> (x + 9) * 40320
                  // 2. (((((((((x + 9) + 8) + 7) + 6) + 5) + 4) + 3) + 2) + 1) --> x + 45
                  if (
                       (details::e_mul == operation) ||
                       (details::e_add == operation)
                     )
                  {
                     details::boc_base_node<T>* bocnode = static_cast<details::boc_base_node<T>*>(branch[0]);

                     if (operation == bocnode->operation())
                     {
                        switch (operation)
                        {
                           case details::e_add : bocnode->set_c(c + bocnode->c()); break;
                           case details::e_mul : bocnode->set_c(c * bocnode->c()); break;
                           default             : return expression_generator<T>::error_node();
                        }

                        return bocnode;
                     }
                  }
                  else if (operation == details::e_div)
                  {
                     details::boc_base_node<T>* bocnode = static_cast<details::boc_base_node<T>*>(branch[0]);
                     details::operator_type        boc_opr = bocnode->operation();

                     if (
                          (details::e_div == boc_opr) ||
                          (details::e_mul == boc_opr)
                        )
                     {
                        switch (boc_opr)
                        {
                           case details::e_div : bocnode->set_c(c * bocnode->c()); break;
                           case details::e_mul : bocnode->set_c(bocnode->c() / c); break;
                           default             : return expression_generator<T>::error_node();
                        }

                        return bocnode;
                     }
                  }
                  else if (operation == details::e_pow)
                  {
                     // (v ^ c0) ^ c1 --> v ^(c0 * c1)
                     details::boc_base_node<T>* bocnode = static_cast<details::boc_base_node<T>*>(branch[0]);
                     details::operator_type        boc_opr = bocnode->operation();

                     if (details::e_pow == boc_opr)
                     {
                        bocnode->set_c(bocnode->c() * c);

                        return bocnode;
                     }
                  }
               }

               if (details::is_sf3ext_node(branch[0]))
               {
                  typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

                  const bool synthesis_result =
                     synthesize_sf4ext_expression<T>::template compile_left<typename expression_generator<T>::ctype>
                        (expr_gen, c, operation, branch[0], result);

                  if (synthesis_result)
                  {
                     free_node(*expr_gen.node_allocator(), branch[0]);

                     return result;
                  }
               }

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_cr<typename details::boc_node<T,op1<T> > > \
                                   (branch[0], c);                                                 \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_boc_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_cocob_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

               // (cob) o c --> cob
               if (details::is_cob_node(branch[0]))
               {
                  details::cob_base_node<T>* cobnode = static_cast<details::cob_base_node<T>*>(branch[0]);

                  const T c = static_cast<details::literal_node<T>*>(branch[1])->value();

                  if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return expr_gen(T(0));
                  }
                  else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return expr_gen(T(std::numeric_limits<T>::quiet_NaN()));
                  }
                  else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return branch[0];
                  }
                  else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return branch[0];
                  }
                  else if (std::equal_to<T>()(T(1),c) && (details::e_div == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return branch[0];
                  }

                  const bool op_addsub = (details::e_add == cobnode->operation()) ||
                                         (details::e_sub == cobnode->operation()) ;

                  if (op_addsub)
                  {
                     switch (operation)
                     {
                        case details::e_add : cobnode->set_c(cobnode->c() + c); break;
                        case details::e_sub : cobnode->set_c(cobnode->c() - c); break;
                        default             : return expression_generator<T>::error_node();
                     }

                     result = cobnode;
                  }
                  else if (details::e_mul == cobnode->operation())
                  {
                     switch (operation)
                     {
                        case details::e_mul : cobnode->set_c(cobnode->c() * c); break;
                        case details::e_div : cobnode->set_c(cobnode->c() / c); break;
                        default             : return expression_generator<T>::error_node();
                     }

                     result = cobnode;
                  }
                  else if (details::e_div == cobnode->operation())
                  {
                     if (details::e_mul == operation)
                     {
                        cobnode->set_c(cobnode->c() * c);
                        result = cobnode;
                     }
                     else if (details::e_div == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::div_op<T> > >
                                       (cobnode->c() / c, cobnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(), branch[0]);
                     }
                  }

                  if (result)
                  {
                     details::free_node(*expr_gen.node_allocator(),branch[1]);
                  }
               }

               // c o (cob) --> cob
               else if (details::is_cob_node(branch[1]))
               {
                  details::cob_base_node<T>* cobnode = static_cast<details::cob_base_node<T>*>(branch[1]);

                  const T c = static_cast<details::literal_node<T>*>(branch[0])->value();

                  if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return expr_gen(T(0));
                  }
                  else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);
                     details::free_node(*expr_gen.node_allocator(), branch[1]);

                     return expr_gen(T(0));
                  }
                  else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);

                     return branch[1];
                  }
                  else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[0]);

                     return branch[1];
                  }

                  if (details::e_add == cobnode->operation())
                  {
                     if (details::e_add == operation)
                     {
                        cobnode->set_c(c + cobnode->c());
                        result = cobnode;
                     }
                     else if (details::e_sub == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::sub_op<T> > >
                                       (c - cobnode->c(), cobnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_sub == cobnode->operation())
                  {
                     if (details::e_add == operation)
                     {
                        cobnode->set_c(c + cobnode->c());
                        result = cobnode;
                     }
                     else if (details::e_sub == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::add_op<T> > >
                                       (c - cobnode->c(), cobnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_mul == cobnode->operation())
                  {
                     if (details::e_mul == operation)
                     {
                        cobnode->set_c(c * cobnode->c());
                        result = cobnode;
                     }
                     else if (details::e_div == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::div_op<T> > >
                                       (c / cobnode->c(), cobnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_div == cobnode->operation())
                  {
                     if (details::e_mul == operation)
                     {
                        cobnode->set_c(c * cobnode->c());
                        result = cobnode;
                     }
                     else if (details::e_div == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::mul_op<T> > >
                                       (c / cobnode->c(), cobnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }

                  if (result)
                  {
                     details::free_node(*expr_gen.node_allocator(),branch[0]);
                  }
               }

               return result;
            }
            compile_with_templates(synthesize_cocob_expression)

            template<typename T>  expression_generator<T>::expression_node_ptr synthesize_coboc_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2])
            {
               typename expression_generator<T>::expression_node_ptr result = expression_generator<T>::error_node();

               // (boc) o c --> boc
               if (details::is_boc_node(branch[0]))
               {
                  details::boc_base_node<T>* bocnode = static_cast<details::boc_base_node<T>*>(branch[0]);

                  const T c = static_cast<details::literal_node<T>*>(branch[1])->value();

                  if (details::e_add == bocnode->operation())
                  {
                     switch (operation)
                     {
                        case details::e_add : bocnode->set_c(bocnode->c() + c); break;
                        case details::e_sub : bocnode->set_c(bocnode->c() - c); break;
                        default             : return expression_generator<T>::error_node();
                     }

                     result = bocnode;
                  }
                  else if (details::e_mul == bocnode->operation())
                  {
                     switch (operation)
                     {
                        case details::e_mul : bocnode->set_c(bocnode->c() * c); break;
                        case details::e_div : bocnode->set_c(bocnode->c() / c); break;
                        default             : return expression_generator<T>::error_node();
                     }

                     result = bocnode;
                  }
                  else if (details::e_sub == bocnode->operation())
                  {
                     if (details::e_add == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::boc_node<T,details::add_op<T> > >
                                       (bocnode->move_branch(0), c - bocnode->c());

                        details::free_node(*expr_gen.node_allocator(),branch[0]);
                     }
                     else if (details::e_sub == operation)
                     {
                        bocnode->set_c(bocnode->c() + c);
                        result = bocnode;
                     }
                  }
                  else if (details::e_div == bocnode->operation())
                  {
                     switch (operation)
                     {
                        case details::e_div : bocnode->set_c(bocnode->c() * c); break;
                        case details::e_mul : bocnode->set_c(bocnode->c() / c); break;
                        default             : return expression_generator<T>::error_node();
                     }

                     result = bocnode;
                  }

                  if (result)
                  {
                     details::free_node(*expr_gen.node_allocator(), branch[1]);
                  }
               }

               // c o (boc) --> boc
               else if (details::is_boc_node(branch[1]))
               {
                  details::boc_base_node<T>* bocnode = static_cast<details::boc_base_node<T>*>(branch[1]);

                  const T c = static_cast<details::literal_node<T>*>(branch[0])->value();

                  if (details::e_add == bocnode->operation())
                  {
                     if (details::e_add == operation)
                     {
                        bocnode->set_c(c + bocnode->c());
                        result = bocnode;
                     }
                     else if (details::e_sub == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::sub_op<T> > >
                                       (c - bocnode->c(), bocnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_sub == bocnode->operation())
                  {
                     if (details::e_add == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::boc_node<T,details::add_op<T> > >
                                       (bocnode->move_branch(0), c - bocnode->c());

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                     else if (details::e_sub == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::sub_op<T> > >
                                       (c + bocnode->c(), bocnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_mul == bocnode->operation())
                  {
                     if (details::e_mul == operation)
                     {
                        bocnode->set_c(c * bocnode->c());
                        result = bocnode;
                     }
                     else if (details::e_div == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::div_op<T> > >
                                       (c / bocnode->c(), bocnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }
                  else if (details::e_div == bocnode->operation())
                  {
                     if (details::e_mul == operation)
                     {
                        bocnode->set_c(bocnode->c() / c);
                        result = bocnode;
                     }
                     else if (details::e_div == operation)
                     {
                        result = expr_gen.node_allocator()->
                                    template allocate_tt<typename details::cob_node<T,details::div_op<T> > >
                                       (c * bocnode->c(), bocnode->move_branch(0));

                        details::free_node(*expr_gen.node_allocator(),branch[1]);
                     }
                  }

                  if (result)
                  {
                     details::free_node(*expr_gen.node_allocator(),branch[0]);
                  }
               }

               return result;
            }
            compile_with_templates(synthesize_coboc_expression)
}
