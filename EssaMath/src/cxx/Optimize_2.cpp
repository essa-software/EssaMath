#include "include/Optimize.inl"
#include "include/Parser.hpp"

namespace Essa::Math{

            template<typename T>  details::expression_node<T>* synthesize_vococ_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v o0 c0) o1 (c1)
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[0]);
               const T&  v = voc->v();
               const T& c0 = voc->c();
               const T& c1 = static_cast<details::literal_node<T>*>(branch[1])->value();
               const details::operator_type o0 = voc->operation();
               const details::operator_type o1 = operation;

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v + c0) + c1 --> (voc) v + (c0 + c1)
                  if ((details::e_add == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(v + c0) + c1 --> (voc) v + (c0 + c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::add_op<T> > >(v, c0 + c1);
                  }
                  // (v + c0) - c1 --> (voc) v + (c0 - c1)
                  else if ((details::e_add == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(v + c0) - c1 --> (voc) v + (c0 - c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::add_op<T> > >(v, c0 - c1);
                  }
                  // (v - c0) + c1 --> (voc) v - (c0 + c1)
                  else if ((details::e_sub == o0) && (details::e_add == o1))
                  {
                     exprtk_debug(("(v - c0) + c1 --> (voc) v - (c0 + c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::add_op<T> > >(v, c1 - c0);
                  }
                  // (v - c0) - c1 --> (voc) v - (c0 + c1)
                  else if ((details::e_sub == o0) && (details::e_sub == o1))
                  {
                     exprtk_debug(("(v - c0) - c1 --> (voc) v - (c0 + c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::sub_op<T> > >(v, c0 + c1);
                  }
                  // (v * c0) * c1 --> (voc) v * (c0 * c1)
                  else if ((details::e_mul == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(v * c0) * c1 --> (voc) v * (c0 * c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::mul_op<T> > >(v, c0 * c1);
                  }
                  // (v * c0) / c1 --> (voc) v * (c0 / c1)
                  else if ((details::e_mul == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(v * c0) / c1 --> (voc) v * (c0 / c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::mul_op<T> > >(v, c0 / c1);
                  }
                  // (v / c0) * c1 --> (voc) v * (c1 / c0)
                  else if ((details::e_div == o0) && (details::e_mul == o1))
                  {
                     exprtk_debug(("(v / c0) * c1 --> (voc) v * (c1 / c0)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::mul_op<T> > >(v, c1 / c0);
                  }
                  // (v / c0) / c1 --> (voc) v / (c0 * c1)
                  else if ((details::e_div == o0) && (details::e_div == o1))
                  {
                     exprtk_debug(("(v / c0) / c1 --> (voc) v / (c0 * c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::div_op<T> > >(v, c0 * c1);
                  }
                  // (v ^ c0) ^ c1 --> (voc) v ^ (c0 * c1)
                  else if ((details::e_pow == o0) && (details::e_pow == o1))
                  {
                     exprtk_debug(("(v ^ c0) ^ c1 --> (voc) v ^ (c0 * c1)\n"));

                     return expr_gen.node_allocator()->
                               template allocate_rc<typename details::voc_node<T,details::pow_op<T> > >(v, c0 * c1);
                  }
               }

               const bool synthesis_result =
                  synthesize_sf3ext_expression<T>::template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::ctype>
                     (expr_gen, id(expr_gen, o0, o1), v, c0, c1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v, c0, c1, f0, f1);
            }

            template<typename T>  std::string synthesize_vococ_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "t";
            }
            compile_with_templates(synthesize_vococ_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vococ_expression1<T>::process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2])
            {
               // (v) o0 (c0 o1 c1) - Not possible.
               exprtk_debug(("(v) o0 (c0 o1 c1) - Not possible.\n"));
               return expression_generator<T>::error_node();
            }

            template<typename T>  details::expression_node<T>* synthesize_vovovov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 v1) o1 (v2 o2 v3)
               const details::vov_base_node<T>* vov0 = static_cast<details::vov_base_node<T>*>(branch[0]);
               const details::vov_base_node<T>* vov1 = static_cast<details::vov_base_node<T>*>(branch[1]);
               const T& v0 = vov0->v0();
               const T& v1 = vov0->v1();
               const T& v2 = vov1->v0();
               const T& v3 = vov1->v1();
               const details::operator_type o0 = vov0->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = vov1->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / v1) * (v2 / v3) --> (vovovov) (v0 * v2) / (v1 * v3)
                  if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, v2, v1, v3, result);

                     exprtk_debug(("(v0 / v1) * (v2 / v3) --> (vovovov) (v0 * v2) / (v1 * v3)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / v1) / (v2 / v3) --> (vovovov) (v0 * v3) / (v1 * v2)
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, v3, v1, v2, result);

                     exprtk_debug(("(v0 / v1) / (v2 / v3) --> (vovovov) (v0 * v3) / (v1 * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 + v1) / (v2 / v3) --> (vovovov) (v0 + v1) * (v3 / v2)
                  else if ((details::e_add == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)*(t/t)", v0, v1, v3, v2, result);

                     exprtk_debug(("(v0 + v1) / (v2 / v3) --> (vovovov) (v0 + v1) * (v3 / v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 - v1) / (v2 / v3) --> (vovovov) (v0 + v1) * (v3 / v2)
                  else if ((details::e_sub == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t-t)*(t/t)", v0, v1, v3, v2, result);

                     exprtk_debug(("(v0 - v1) / (v2 / v3) --> (vovovov) (v0 - v1) * (v3 / v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * v1) / (v2 / v3) --> (vovovov) ((v0 * v1) * v3) / v2
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "((t*t)*t)/t", v0, v1, v3, v2, result);

                     exprtk_debug(("(v0 * v1) / (v2 / v3) --> (vovovov) ((v0 * v1) * v3) / v2\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, v2, v3, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, v3, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovovov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vovovoc_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 v1) o1 (v2 o2 c)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[0]);
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[1]);
               const T& v0 = vov->v0();
               const T& v1 = vov->v1();
               const T& v2 = voc->v ();
               const T   c = voc->c ();
               const details::operator_type o0 = vov->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = voc->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / v1) * (v2 / c) --> (vovovoc) (v0 * v2) / (v1 * c)
                  if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)/(t*t)", v0, v2, v1, c, result);

                     exprtk_debug(("(v0 / v1) * (v2 / c) --> (vovovoc) (v0 * v2) / (v1 * c)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / v1) / (v2 / c) --> (vocovov) (v0 * c) / (v1 * v2)
                  if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, c, v1, v2, result);

                     exprtk_debug(("(v0 / v1) / (v2 / c) --> (vocovov) (v0 * c) / (v1 * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, v2, c, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, c, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovoc_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovovoc_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vovocov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 v1) o1 (c o2 v2)
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[0]);
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[1]);
               const T& v0 = vov->v0();
               const T& v1 = vov->v1();
               const T& v2 = cov->v ();
               const T   c = cov->c ();
               const details::operator_type o0 = vov->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = cov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / v1) * (c / v2) --> (vocovov) (v0 * c) / (v1 * v2)
                  if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, c, v1, v2, result);

                     exprtk_debug(("(v0 / v1) * (c / v2) --> (vocovov) (v0 * c) / (v1 * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / v1) / (c / v2) --> (vovovoc) (v0 * v2) / (v1 * c)
                  if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)/(t*t)", v0, v2, v1, c, result);

                     exprtk_debug(("(v0 / v1) / (c / v2) --> (vovovoc) (v0 * v2) / (v1 * c)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, c, v2, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, c, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovocov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovocov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vocovov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 c) o1 (v1 o2 v2)
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[0]);
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[1]);
               const T   c = voc->c ();
               const T& v0 = voc->v ();
               const T& v1 = vov->v0();
               const T& v2 = vov->v1();
               const details::operator_type o0 = voc->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = vov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 / c) * (v1 / v2) --> (vovocov) (v0 * v1) / (c * v2)
                  if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, v1, c, v2, result);

                     exprtk_debug(("(v0 / c) * (v1 / v2) --> (vovocov) (v0 * v1) / (c * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c) / (v1 / v2) --> (vovocov) (v0 * v2) / (c * v1)
                  if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", v0, v2, c, v1, result);

                     exprtk_debug(("(v0 / c) / (v1 / v2) --> (vovocov) (v0 * v2) / (c * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c, v1, v2, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, c, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vocovov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_covovov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c o0 v0) o1 (v1 o2 v2)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[0]);
               const details::vov_base_node<T>* vov = static_cast<details::vov_base_node<T>*>(branch[1]);
               const T   c = cov->c ();
               const T& v0 = cov->v ();
               const T& v1 = vov->v0();
               const T& v2 = vov->v1();
               const details::operator_type o0 = cov->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = vov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c / v0) * (v1 / v2) --> (covovov) (c * v1) / (v0 * v2)
                  if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", c, v1, v0, v2, result);

                     exprtk_debug(("(c / v0) * (v1 / v2) --> (covovov) (c * v1) / (v0 * v2)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c / v0) / (v1 / v2) --> (covovov) (c * v2) / (v0 * v1)
                  if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/(t*t)", c, v2, v0, v1, result);

                     exprtk_debug(("(c / v0) / (v1 / v2) --> (covovov) (c * v2) / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c, v0, v1, v2, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c, v0, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covovov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_covovov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_covocov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c0 o0 v0) o1 (c1 o2 v1)
               const details::cov_base_node<T>* cov0 = static_cast<details::cov_base_node<T>*>(branch[0]);
               const details::cov_base_node<T>* cov1 = static_cast<details::cov_base_node<T>*>(branch[1]);
               const T  c0 = cov0->c();
               const T& v0 = cov0->v();
               const T  c1 = cov1->c();
               const T& v1 = cov1->v();
               const details::operator_type o0 = cov0->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = cov1->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c0 + v0) + (c1 + v1) --> (covov) (c0 + c1) + v0 + v1
                  if ((details::e_add == o0) && (details::e_add == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)+t", (c0 + c1), v0, v1, result);

                     exprtk_debug(("(c0 + v0) + (c1 + v1) --> (covov) (c0 + c1) + v0 + v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 + v0) - (c1 + v1) --> (covov) (c0 - c1) + v0 - v1
                  else if ((details::e_add == o0) && (details::e_sub == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)-t", (c0 - c1), v0, v1, result);

                     exprtk_debug(("(c0 + v0) - (c1 + v1) --> (covov) (c0 - c1) + v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 - v0) - (c1 - v1) --> (covov) (c0 - c1) - v0 + v1
                  else if ((details::e_sub == o0) && (details::e_sub == o1) && (details::e_sub == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t-t)+t", (c0 - c1), v0, v1, result);

                     exprtk_debug(("(c0 - v0) - (c1 - v1) --> (covov) (c0 - c1) - v0 + v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) * (c1 * v1) --> (covov) (c0 * c1) * v0 * v1
                  else if ((details::e_mul == o0) && (details::e_mul == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) * (c1 * v1) --> (covov) (c0 * c1) * v0 * v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) / (c1 * v1) --> (covov) (c0 / c1) * (v0 / v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) / (c1 * v1) --> (covov) (c0 / c1) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) * (c1 / v1) --> (covov) (c0 * c1) / (v0 * v1)
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(c0 / v0) * (c1 / v1) --> (covov) (c0 * c1) / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) / (c1 / v1) --> (covov) ((c0 / c1) * v1) / v0
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 / c1), v1, v0, result);

                     exprtk_debug(("(c0 / v0) / (c1 / v1) --> (covov) ((c0 / c1) * v1) / v0\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) / (c1 / v1) --> (covov) (c0 / c1) * (v0 * v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t*(t*t)", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) / (c1 / v1) --> (covov) (c0 / c1) * (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) / (c1 * v1) --> (covov) (c0 / c1) / (v0 * v1)
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(c0 / v0) / (c1 * v1) --> (covov) (c0 / c1) / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c * v0) +/- (c * v1) --> (covov) c * (v0 +/- v1)
                  else if (
                            (std::equal_to<T>()(c0,c1)) &&
                            (details::e_mul == o0)      &&
                            (details::e_mul == o2)      &&
                            (
                              (details::e_add == o1) ||
                              (details::e_sub == o1)
                            )
                          )
                  {
                     std::string specfunc;

                     switch (o1)
                     {
                        case details::e_add : specfunc = "t*(t+t)"; break;
                        case details::e_sub : specfunc = "t*(t-t)"; break;
                        default             : return expression_generator<T>::error_node();
                     }

                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, specfunc, c0, v0, v1, result);

                     exprtk_debug(("(c * v0) +/- (c * v1) --> (covov) c * (v0 +/- v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c0, v0, c1, v1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c0, v0, c1, v1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covocov_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_covocov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vocovoc_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 c0) o1 (v1 o2 c1)
               const details::voc_base_node<T>* voc0 = static_cast<details::voc_base_node<T>*>(branch[0]);
               const details::voc_base_node<T>* voc1 = static_cast<details::voc_base_node<T>*>(branch[1]);
               const T  c0 = voc0->c();
               const T& v0 = voc0->v();
               const T  c1 = voc1->c();
               const T& v1 = voc1->v();
               const details::operator_type o0 = voc0->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = voc1->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 + c0) + (v1 + c1) --> (covov) (c0 + c1) + v0 + v1
                  if ((details::e_add == o0) && (details::e_add == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)+t", (c0 + c1), v0, v1, result);

                     exprtk_debug(("(v0 + c0) + (v1 + c1) --> (covov) (c0 + c1) + v0 + v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 + c0) - (v1 + c1) --> (covov) (c0 - c1) + v0 - v1
                  else if ((details::e_add == o0) && (details::e_sub == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)-t", (c0 - c1), v0, v1, result);

                     exprtk_debug(("(v0 + c0) - (v1 + c1) --> (covov) (c0 - c1) + v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 - c0) - (v1 - c1) --> (covov) (c1 - c0) + v0 - v1
                  else if ((details::e_sub == o0) && (details::e_sub == o1) && (details::e_sub == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)-t", (c1 - c0), v0, v1, result);

                     exprtk_debug(("(v0 - c0) - (v1 - c1) --> (covov) (c1 - c0) + v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) * (v1 * c1) --> (covov) (c0 * c1) * v0 * v1
                  else if ((details::e_mul == o0) && (details::e_mul == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) * (v1 * c1) --> (covov) (c0 * c1) * v0 * v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) / (v1 * c1) --> (covov) (c0 / c1) * (v0 / v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) / (v1 * c1) --> (covov) (c0 / c1) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) * (v1 / c1) --> (covov) (1 / (c0 * c1)) * v0 * v1
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", T(1) / (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 / c0) * (v1 / c1) --> (covov) (1 / (c0 * c1)) * v0 * v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) / (v1 / c1) --> (covov) ((c1 / c0) * v0) / v1
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c1 / c0), v0, v1, result);

                     exprtk_debug(("(v0 / c0) / (v1 / c1) --> (covov) ((c1 / c0) * v0) / v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) / (v1 / c1) --> (covov) (c0 * c1) * (v0 / v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t*(t/t)", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) / (v1 / c1) --> (covov) (c0 * c1) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) / (v1 * c1) --> (covov) (1 / (c0 * c1)) * v0 / v1
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t*(t/t)", T(1) / (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 / c0) / (v1 * c1) --> (covov) (1 / (c0 * c1)) * v0 / v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) * (v1 + c1) --> (vocovoc) (v0 * (1 / c0)) * (v1 + c1)
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)*(t+t)", v0, T(1) / c0, v1, c1, result);

                     exprtk_debug(("(v0 / c0) * (v1 + c1) --> (vocovoc) (v0 * (1 / c0)) * (v1 + c1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) * (v1 - c1) --> (vocovoc) (v0 * (1 / c0)) * (v1 - c1)
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_sub == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf4ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)*(t-t)", v0, T(1) / c0, v1, c1, result);

                     exprtk_debug(("(v0 / c0) * (v1 - c1) --> (vocovoc) (v0 * (1 / c0)) * (v1 - c1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c) +/- (v1 * c) --> (covov) c * (v0 +/- v1)
                  else if (
                            (std::equal_to<T>()(c0,c1)) &&
                            (details::e_mul == o0)      &&
                            (details::e_mul == o2)      &&
                            (
                              (details::e_add == o1) ||
                              (details::e_sub == o1)
                            )
                          )
                  {
                     std::string specfunc;

                     switch (o1)
                     {
                        case details::e_add : specfunc = "t*(t+t)"; break;
                        case details::e_sub : specfunc = "t*(t-t)"; break;
                        default             : return expression_generator<T>::error_node();
                     }

                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, specfunc, c0, v0, v1, result);

                     exprtk_debug(("(v0 * c) +/- (v1 * c) --> (covov) c * (v0 +/- v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c) +/- (v1 / c) --> (vovoc) (v0 +/- v1) / c
                  else if (
                            (std::equal_to<T>()(c0,c1)) &&
                            (details::e_div == o0)      &&
                            (details::e_div == o2)      &&
                            (
                              (details::e_add == o1) ||
                              (details::e_sub == o1)
                            )
                          )
                  {
                     std::string specfunc;

                     switch (o1)
                     {
                        case details::e_add : specfunc = "(t+t)/t"; break;
                        case details::e_sub : specfunc = "(t-t)/t"; break;
                        default             : return expression_generator<T>::error_node();
                     }

                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, specfunc, v0, v1, c0, result);

                     exprtk_debug(("(v0 / c) +/- (v1 / c) --> (vovoc) (v0 +/- v1) / c\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c0, v1, c1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, c0, v1, c1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovoc_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vocovoc_expression0)

            template<typename T>  details::expression_node<T>* synthesize_covovoc_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (c0 o0 v0) o1 (v1 o2 c1)
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[0]);
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[1]);
               const T  c0 = cov->c();
               const T& v0 = cov->v();
               const T  c1 = voc->c();
               const T& v1 = voc->v();
               const details::operator_type o0 = cov->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = voc->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (c0 + v0) + (v1 + c1) --> (covov) (c0 + c1) + v0 + v1
                  if ((details::e_add == o0) && (details::e_add == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)+t", (c0 + c1), v0, v1, result);

                     exprtk_debug(("(c0 + v0) + (v1 + c1) --> (covov) (c0 + c1) + v0 + v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 + v0) - (v1 + c1) --> (covov) (c0 - c1) + v0 - v1
                  else if ((details::e_add == o0) && (details::e_sub == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)-t", (c0 - c1), v0, v1, result);

                     exprtk_debug(("(c0 + v0) - (v1 + c1) --> (covov) (c0 - c1) + v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 - v0) - (v1 - c1) --> (covov) (c0 + c1) - v0 - v1
                  else if ((details::e_sub == o0) && (details::e_sub == o1) && (details::e_sub == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t-(t+t)", (c0 + c1), v0, v1, result);

                     exprtk_debug(("(c0 - v0) - (v1 - c1) --> (covov) (c0 + c1) - v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) * (v1 * c1) --> (covov) (c0 * c1) * v0 * v1
                  else if ((details::e_mul == o0) && (details::e_mul == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) * (v1 * c1) --> (covov) (c0 * c1) * v0 * v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) / (v1 * c1) --> (covov) (c0 / c1) * (v0 / v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) / (v1 * c1) --> (covov) (c0 / c1) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) * (v1 / c1) --> (covov) (c0 / c1) * (v1 / v0)
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t*(t/t)", (c0 / c1), v1, v0, result);

                     exprtk_debug(("(c0 / v0) * (v1 / c1) --> (covov) (c0 / c1) * (v1 / v0)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) / (v1 / c1) --> (covov) (c0 * c1) / (v0 * v1)
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(c0 / v0) / (v1 / c1) --> (covov) (c0 * c1) / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 * v0) / (v1 / c1) --> (covov) (c0 * c1) * (v0 / v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(c0 * v0) / (v1 / c1) --> (covov) (c0 * c1) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c0 / v0) / (v1 * c1) --> (covov) (c0 / c1) / (v0 * v1)
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "t/(t*t)", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(c0 / v0) / (v1 * c1) --> (covov) (c0 / c1) / (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (c * v0) +/- (v1 * c) --> (covov) c * (v0 +/- v1)
                  else if (
                            (std::equal_to<T>()(c0,c1)) &&
                            (details::e_mul == o0)      &&
                            (details::e_mul == o2)      &&
                            (
                              (details::e_add == o1) ||
                              (details::e_sub == o1)
                            )
                          )
                  {
                     std::string specfunc;

                     switch (o1)
                     {
                        case details::e_add : specfunc = "t*(t+t)"; break;
                        case details::e_sub : specfunc = "t*(t-t)"; break;
                        default             : return expression_generator<T>::error_node();
                     }

                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, specfunc, c0, v0, v1, result);

                     exprtk_debug(("(c * v0) +/- (v1 * c) --> (covov) c * (v0 +/- v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c0, v0, v1, c1, result);

               if (synthesis_result)
                  return result;

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f2 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);

               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o1,f1))
                  return expression_generator<T>::error_node();
               else if (!expr_gen.valid_operator(o2,f2))
                  return expression_generator<T>::error_node();
               else
                  return node_type::allocate(*(expr_gen.node_allocator()), c0, v0, v1, c1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covovoc_expression0<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "(t" << expr_gen.to_str(o0)
                         << "t)" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_covovoc_expression0)
}
