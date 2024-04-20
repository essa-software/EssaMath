#include "include/Optimize.inl"
#include "include/Parser.hpp"

namespace Essa::Math{

            template<typename T>details::expression_node<T>* synthesize_vov_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               const T& v1 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v2 = static_cast<details::variable_node<T>*>(branch[1])->ref();

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_rr<typename details::vov_node<T,op1<T> > > \
                                   (v1, v2);                                                       \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_vov_expression)

        template<typename T> details::expression_node<T>* synthesize_cov_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               const T  c = static_cast<details::literal_node<T>*> (branch[0])->value();
               const T& v = static_cast<details::variable_node<T>*>(branch[1])->ref  ();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);

               if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
                  return expr_gen(T(0));
               else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
                  return expr_gen(T(0));
               else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  return static_cast<details::variable_node<T>*>(branch[1]);
               else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  return static_cast<details::variable_node<T>*>(branch[1]);

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_cr<typename details::cov_node<T,op1<T> > > \
                                   (c, v);                                                         \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_cov_expression)

        template<typename T> details::expression_node<T>* synthesize_voc_expression<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               const T& v = static_cast<details::variable_node<T>*>(branch[0])->ref  ();
               const T  c = static_cast<details::literal_node<T>*> (branch[1])->value();

               details::free_node(*(expr_gen.node_allocator()), branch[1]);

               if (expr_gen.cardinal_pow_optimisable(operation,c))
               {
                  if (std::equal_to<T>()(T(1),c))
                     return branch[0];
                  else
                     return expr_gen.cardinal_pow_optimisation(v,c);
               }
               else if (std::equal_to<T>()(T(0),c) && (details::e_mul == operation))
                  return expr_gen(T(0));
               else if (std::equal_to<T>()(T(0),c) && (details::e_div == operation))
                  return expr_gen(std::numeric_limits<T>::quiet_NaN());
               else if (std::equal_to<T>()(T(0),c) && (details::e_add == operation))
                  return static_cast<details::variable_node<T>*>(branch[0]);
               else if (std::equal_to<T>()(T(1),c) && (details::e_mul == operation))
                  return static_cast<details::variable_node<T>*>(branch[0]);
               else if (std::equal_to<T>()(T(1),c) && (details::e_div == operation))
                  return static_cast<details::variable_node<T>*>(branch[0]);

               switch (operation)
               {
                  #define case_stmt(op0, op1)                                                      \
                  case op0 : return expr_gen.node_allocator()->                                     \
                                template allocate_rc<typename details::voc_node<T,op1<T> > > \
                                   (v, c);                                                         \

                  basic_opr_switch_statements
                  extended_opr_switch_statements
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }
            compile_with_templates(synthesize_voc_expression)

        template<typename T> details::expression_node<T>* synthesize_vovov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 v1) o1 (v2)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[0]);
               const T& v0 = vov->v0();
               const T& v1 = vov->v1();
               const T& v2 = static_cast<details::variable_node<T>*>(branch[1])->ref();
               const details::operator_type o0 = vov->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / v1) / v2 --> (vovov) v0 / (v1 * v2)
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", v0, v1, v2, result);

                     exprtk_debug(("(v0 / v1) / v2 --> (vovov) v0 / (v1 * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), v0, v1, v2, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, f0, f1);
            }

            template<typename T> std::string synthesize_vovov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_vovov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vovov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0) o0 (v1 o1 v2)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vov->v0();
               const T& v2 = vov->v1();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = vov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // v0 / (v1 / v2) --> (vovov) (v0 * v2) / v1
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", v0, v2, v1, result);

                     exprtk_debug(("v0 / (v1 / v2) --> (vovov) (v0 * v2) / v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), v0, v1, v2, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, f0, f1);
            }

            template<typename T>  std::string synthesize_vovov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_vovov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vovoc_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 v1) o1 (c)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[0]);
               const T& v0 = vov->v0();
               const T& v1 = vov->v1();
               const T   c = static_cast<details::literal_node<T>*>(branch[1])->value();
               const details::operator_type o0 = vov->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / v1) / c --> (vovoc) v0 / (v1 * c)
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "t/(t*t)", v0, v1, c, result);

                     exprtk_debug(("(v0 / v1) / c --> (vovoc) v0 / (v1 * c)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                     (expr_gen, id(expr_gen, o0, o1), v0, v1, c, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, c, f0, f1);
            }

            template<typename T>  std::string synthesize_vovoc_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_vovoc_expression0)
            
            template<typename T>  details::expression_node<T>* synthesize_vovoc_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0) o0 (v1 o1 c)
               const details::voc_base_node<T>* voc = static_cast<const details::voc_base_node<T>*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = voc->v();
               const T   c = voc->c();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = voc->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // v0 / (v1 / c) --> (vocov) (v0 * c) / v1
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", v0, c, v1, result);

                     exprtk_debug(("v0 / (v1 / c) --> (vocov) (v0 * c) / v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                     (expr_gen, id(expr_gen, o0, o1), v0, v1, c, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, c, f0, f1);
            }

            template<typename T>  std::string synthesize_vovoc_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_vovoc_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vocov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 c) o1 (v1)
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[0]);
               const T& v0 = voc->v();
               const T   c = voc->c();
               const T& v1 = static_cast<details::variable_node<T>*>(branch[1])->ref();
               const details::operator_type o0 = voc->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / c) / v1 --> (vovoc) v0 / (v1 * c)
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "t/(t*t)", v0, v1, c, result);

                     exprtk_debug(("(v0 / c) / v1 --> (vovoc) v0 / (v1 * c)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), v0, c, v1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, c, v1, f0, f1);
            }

            template<typename T>  std::string synthesize_vocov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_vocov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vocov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0) o0 (c o1 v1)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T   c = cov->c();
               const T& v1 = cov->v();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = cov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // v0 / (c / v1) --> (vovoc) (v0 * v1) / c
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)/t", v0, v1, c, result);

                     exprtk_debug(("v0 / (c / v1) --> (vovoc) (v0 * v1) / c\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), v0, c, v1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, c, v1, f0, f1);
            }

            template<typename T>  std::string synthesize_vocov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_vocov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_covov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c o0 v0) o1 (v1)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[0]);
               const T   c = cov->c();
               const T& v0 = cov->v();
               const T& v1 = static_cast<details::variable_node<T>*>(branch[1])->ref();
               const details::operator_type o0 = cov->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c / v0) / v1 --> (covov) c / (v0 * v1)
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", c, v0, v1, result);

                     exprtk_debug(("(c / v0) / v1 --> (covov) c / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), c, v0, v1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c, v0, v1, f0, f1);
            }

            template<typename T>  std::string synthesize_covov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_covov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_covov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c) o0 (v0 o1 v1)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[1]);
               const T   c = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vov->v0();
               const T& v1 = vov->v1();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = vov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // c / (v0 / v1) --> (covov) (c * v1) / v0
                  if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", c, v1, v0, result);

                     exprtk_debug(("c / (v0 / v1) --> (covov) (c * v1) / v0\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), c, v0, v1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c, v0, v1, f0, f1);
            }

            template<typename T>  std::string synthesize_covov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_covov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_covoc_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c0 o0 v) o1 (c1)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[0]);
               const T  c0 = cov->c();
               const T&  v = cov->v();
               const T  c1 = static_cast<details::literal_node<T>*>(branch[1])->value();
               const details::operator_type o0 = cov->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c0 + v) + c1 --> (cov) (c0 + c1) + v
                  if ((details::e_add == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0 + v) + c1 --> (cov) (c0 + c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 + c1, v);
                  }
                  // (c0 + v) - c1 --> (cov) (c0 - c1) + v
                  else if ((details::e_add == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0 + v) - c1 --> (cov) (c0 - c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 - c1, v);
                  }
                  // (c0 - v) + c1 --> (cov) (c0 + c1) - v
                  else if ((details::e_sub == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0 - v) + c1 --> (cov) (c0 + c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 + c1, v);
                  }
                  // (c0 - v) - c1 --> (cov) (c0 - c1) - v
                  else if ((details::e_sub == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0 - v) - c1 --> (cov) (c0 - c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 - c1, v);
                  }
                  // (c0 * v) * c1 --> (cov) (c0 * c1) * v
                  else if ((details::e_mul == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0 * v) * c1 --> (cov) (c0 * c1) * v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 * c1, v);
                  }
                  // (c0 * v) / c1 --> (cov) (c0 / c1) * v
                  else if ((details::e_mul == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0 * v) / c1 --> (cov) (c0 / c1) * v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 / c1, v);
                  }
                  // (c0 / v) * c1 --> (cov) (c0 * c1) / v
                  else if ((details::e_div == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0 / v) * c1 --> (cov) (c0 * c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 * c1, v);
                  }
                  // (c0 / v) / c1 --> (cov) (c0 / c1) / v
                  else if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0 / v) / c1 --> (cov) (c0 / c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 / c1, v);
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                     (expr_gen, id(expr_gen, o0, o1), c0, v, c1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c0, v, c1, f0, f1);
            }

            template<typename T>  std::string synthesize_covoc_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_covoc_expression0)

            template<typename T>  details::expression_node<T>* synthesize_covoc_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c0) o0 (v o1 c1)
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[1]);
               const T  c0 = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T&  v = voc->v();
               const T  c1 = voc->c();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = voc->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c0) + (v + c1) --> (cov) (c0 + c1) + v
                  if ((details::e_add == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0) + (v + c1) --> (cov) (c0 + c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 + c1, v);
                  }
                  // (c0) + (v - c1) --> (cov) (c0 - c1) + v
                  else if ((details::e_add == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0) + (v - c1) --> (cov) (c0 - c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 - c1, v);
                  }
                  // (c0) - (v + c1) --> (cov) (c0 - c1) - v
                  else if ((details::e_sub == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0) - (v + c1) --> (cov) (c0 - c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 - c1, v);
                  }
                  // (c0) - (v - c1) --> (cov) (c0 + c1) - v
                  else if ((details::e_sub == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0) - (v - c1) --> (cov) (c0 + c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 + c1, v);
                  }
                  // (c0) * (v * c1) --> (voc) v * (c0 * c1)
                  else if ((details::e_mul == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0) * (v * c1) --> (voc) v * (c0 * c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 * c1, v);
                  }
                  // (c0) * (v / c1) --> (cov) (c0 / c1) * v
                  else if ((details::e_mul == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0) * (v / c1) --> (cov) (c0 / c1) * v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 / c1, v);
                  }
                  // (c0) / (v * c1) --> (cov) (c0 / c1) / v
                  else if ((details::e_div == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0) / (v * c1) --> (cov) (c0 / c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 / c1, v);
                  }
                  // (c0) / (v / c1) --> (cov) (c0 * c1) / v
                  else if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0) / (v / c1) --> (cov) (c0 * c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 * c1, v);
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                     (expr_gen, id(expr_gen, o0, o1), c0, v, c1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c0, v, c1, f0, f1);
            }

            template<typename T>  std::string synthesize_covoc_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_covoc_expression1)

            template<typename T>  details::expression_node<T>* synthesize_cocov_expression0<T>::process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2])
            {
               // (c0 o0 c1) o1 (v) - Not possible.
               return expression_generator<T>::error_node();
            }
            compile_with_templates(synthesize_cocov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_cocov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c0) o0 (c1 o1 v)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[1]);
               const T  c0 = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T  c1 = cov->c();
               const T&  v = cov->v();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = cov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c0) + (c1 + v) --> (cov) (c0 + c1) + v
                  if ((details::e_add == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0) + (c1 + v) --> (cov) (c0 + c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 + c1, v);
                  }
                  // (c0) + (c1 - v) --> (cov) (c0 + c1) - v
                  else if ((details::e_add == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0) + (c1 - v) --> (cov) (c0 + c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 + c1, v);
                  }
                  // (c0) - (c1 + v) --> (cov) (c0 - c1) - v
                  else if ((details::e_sub == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(c0) - (c1 + v) --> (cov) (c0 - c1) - v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::sub_op<T> > >(c0 - c1, v);
                  }
                  // (c0) - (c1 - v) --> (cov) (c0 - c1) + v
                  else if ((details::e_sub == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(c0) - (c1 - v) --> (cov) (c0 - c1) + v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::add_op<T> > >(c0 - c1, v);
                  }
                  // (c0) * (c1 * v) --> (cov) (c0 * c1) * v
                  else if ((details::e_mul == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0) * (c1 * v) --> (cov) (c0 * c1) * v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 * c1, v);
                  }
                  // (c0) * (c1 / v) --> (cov) (c0 * c1) / v
                  else if ((details::e_mul == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0) * (c1 / v) --> (cov) (c0 * c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 * c1, v);
                  }
                  // (c0) / (c1 * v) --> (cov) (c0 / c1) / v
                  else if ((details::e_div == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(c0) / (c1 * v) --> (cov) (c0 / c1) / v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::div_op<T> > >(c0 / c1, v);
                  }
                  // (c0) / (c1 / v) --> (cov) (c0 / c1) * v
                  else if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(c0) / (c1 / v) --> (cov) (c0 / c1) * v\n"));

                     return expr_gen.node_allocator()->
                               template allocate_cr<typename details::cov_node<T,details::mul_op<T> > >(c0 / c1, v);
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>
                     (expr_gen, id(expr_gen, o0, o1), c0, c1, v, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c0, c1, v, f0, f1);
            }

            template<typename T>  std::string synthesize_cocov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "t)";
            }
            compile_with_templates(synthesize_cocov_expression1)
}
