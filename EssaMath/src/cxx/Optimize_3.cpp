#include "include/Optimize.inl"
#include "include/Parser.hpp"

namespace Essa::Math{


            template<typename T>  details::expression_node<T>* synthesize_vococov_expression0<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // (v0 o0 c0) o1 (c1 o2 v1)
               const details::voc_base_node<T>* voc = static_cast<details::voc_base_node<T>*>(branch[0]);
               const details::cov_base_node<T>* cov = static_cast<details::cov_base_node<T>*>(branch[1]);
               const T  c0 = voc->c();
               const T& v0 = voc->v();
               const T  c1 = cov->c();
               const T& v1 = cov->v();
               const details::operator_type o0 = voc->operation();
               const details::operator_type o1 = operation;
               const details::operator_type o2 = cov->operation();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               if (expr_gen.parser()->settings().strength_reduction_enabled())
               {
                  // (v0 + c0) + (c1 + v1) --> (covov) (c0 + c1) + v0 + v1
                  if ((details::e_add == o0) && (details::e_add == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)+t", (c0 + c1), v0, v1, result);

                     exprtk_debug(("(v0 + c0) + (c1 + v1) --> (covov) (c0 + c1) + v0 + v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 + c0) - (c1 + v1) --> (covov) (c0 - c1) + v0 - v1
                  else if ((details::e_add == o0) && (details::e_sub == o1) && (details::e_add == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t+t)-t", (c0 - c1), v0, v1, result);

                     exprtk_debug(("(v0 + c0) - (c1 + v1) --> (covov) (c0 - c1) + v0 - v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 - c0) - (c1 - v1) --> (vovoc) v0 + v1 - (c1 + c0)
                  else if ((details::e_sub == o0) && (details::e_sub == o1) && (details::e_sub == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t+t)-t", v0, v1, (c1 + c0), result);

                     exprtk_debug(("(v0 - c0) - (c1 - v1) --> (vovoc) v0 + v1 - (c1 + c0)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) * (c1 * v1) --> (covov) (c0 * c1) * v0 * v1
                  else if ((details::e_mul == o0) && (details::e_mul == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) * (c1 * v1) --> (covov) (c0 * c1) * v0 * v1\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) / (c1 * v1) --> (covov) (c0 / c1) * (v0 * v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) / (c1 * v1) --> (covov) (c0 / c1) * (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) * (c1 / v1) --> (covov) (c1 / c0) * (v0 / v1)
                  else if ((details::e_div == o0) && (details::e_mul == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", (c1 / c0), v0, v1, result);

                     exprtk_debug(("(v0 / c0) * (c1 / v1) --> (covov) (c1 / c0) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c0) / (c1 / v1) --> (covov) (c0 / c1) * (v0 * v1)
                  else if ((details::e_mul == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)*t", (c0 / c1), v0, v1, result);

                     exprtk_debug(("(v0 * c0) / (c1 / v1) --> (covov) (c0 / c1) * (v0 * v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) / (c1 * v1) --> (covov) (1 / (c0 * c1)) * (v0 / v1)
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_mul == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>(expr_gen, "(t*t)/t", T(1) / (c0 * c1), v0, v1, result);

                     exprtk_debug(("(v0 / c0) / (c1 * v1) --> (covov) (1 / (c0 * c1)) * (v0 / v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 / c0) / (c1 / v1) --> (vovoc) (v0 * v1) * (1 / (c0 * c1))
                  else if ((details::e_div == o0) && (details::e_div == o1) && (details::e_div == o2))
                  {
                     const bool synthesis_result =
                        synthesize_sf3ext_expression<T>::
                           template compile<typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>(expr_gen, "(t*t)*t", v0, v1, T(1) / (c0 * c1), result);

                     exprtk_debug(("(v0 / c0) / (c1 / v1) --> (vovoc) (v0 * v1) * (1 / (c0 * c1))\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
                  // (v0 * c) +/- (c * v1) --> (covov) c * (v0 +/- v1)
                  else if (
                            (std::equal_to<T>()(c0,c1)) &&
                            (details::e_mul == o0)      &&
                            (details::e_mul == o2)      &&
                            (
                              (details::e_add == o1) || (details::e_sub == o1)
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

                     exprtk_debug(("(v0 * c) +/- (c * v1) --> (covov) c * (v0 +/- v1)\n"));

                     return (synthesis_result) ? result : expression_generator<T>::error_node();
                  }
               }

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c0, c1, v1, result);

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
                  return node_type::allocate(*(expr_gen.node_allocator()), v0, c0, c1, v1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vococov_expression0<T>::id(expression_generator<T>& expr_gen,
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
            compile_with_templates(synthesize_vococov_expression0)

            template<typename T>  details::expression_node<T>* synthesize_vovovov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (v1 o1 (v2 o2 v3))
               typedef typename synthesize_vovov_expression1<T>::node_type lcl_vovov_t;

               const lcl_vovov_t* vovov = static_cast<const lcl_vovov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vovov->t0();
               const T& v2 = vovov->t1();
               const T& v3 = vovov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen,id(expr_gen, o0, o1, o2), v0, v1, v2, v3, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (v1 o1 (v2 o2 v3))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, v3, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vovovov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vovovoc_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (v1 o1 (v2 o2 c))
               typedef typename synthesize_vovoc_expression1<T>::node_type lcl_vovoc_t;

               const lcl_vovoc_t* vovoc = static_cast<const lcl_vovoc_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vovoc->t0();
               const T& v2 = vovoc->t1();
               const T   c = vovoc->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovoc->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovoc->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovoc->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovoc->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, v2, c, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (v1 o1 (v2 o2 c))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, c, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovoc_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vovovoc_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vovocov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (v1 o1 (c o2 v2))
               typedef typename synthesize_vocov_expression1<T>::node_type lcl_vocov_t;

               const lcl_vocov_t* vocov = static_cast<const lcl_vocov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vocov->t0();
               const T   c = vocov->t1();
               const T& v2 = vocov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vocov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vocov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vocov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vocov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, c, v2, result);

               if (synthesis_result)
                  return result;
               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (v1 o1 (c o2 v2))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, c, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovocov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vovocov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vocovov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (c o1 (v1 o2 v2))
               typedef typename synthesize_covov_expression1<T>::node_type lcl_covov_t;

               const lcl_covov_t* covov = static_cast<const lcl_covov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T   c = covov->t0();
               const T& v1 = covov->t1();
               const T& v2 = covov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(covov->f0());
               const details::operator_type o2 = expr_gen.get_operator(covov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = covov->f0();
               typename expression_generator<T>::binary_functor_t f2 = covov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c, v1, v2, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (c o1 (v1 o2 v2))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, c, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vocovov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_covovov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // c o0 (v0 o1 (v1 o2 v2))
               typedef typename synthesize_vovov_expression1<T>::node_type lcl_vovov_t;

               const lcl_vovov_t* vovov = static_cast<const lcl_vovov_t*>(branch[1]);
               const T   c = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vovov->t0();
               const T& v1 = vovov->t1();
               const T& v2 = vovov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c, v0, v1, v2, result);

               if (synthesis_result)
                  return result;
               if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("c o0 (v0 o1 (v1 o2 v2))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), c, v0, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covovov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_covovov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_covocov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // c0 o0 (v0 o1 (c1 o2 v1))
               typedef typename synthesize_vocov_expression1<T>::node_type lcl_vocov_t;

               const lcl_vocov_t* vocov = static_cast<const lcl_vocov_t*>(branch[1]);
               const T  c0 = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vocov->t0();
               const T  c1 = vocov->t1();
               const T& v1 = vocov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vocov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vocov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vocov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vocov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c0, v0, c1, v1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("c0 o0 (v0 o1 (c1 o2 v1))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), c0, v0, c1, v1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covocov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_covocov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vocovoc_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (c0 o1 (v1 o2 c2))
               typedef typename synthesize_covoc_expression1<T>::node_type lcl_covoc_t;

               const lcl_covoc_t* covoc = static_cast<const lcl_covoc_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T  c0 = covoc->t0();
               const T& v1 = covoc->t1();
               const T  c1 = covoc->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(covoc->f0());
               const details::operator_type o2 = expr_gen.get_operator(covoc->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = covoc->f0();
               typename expression_generator<T>::binary_functor_t f2 = covoc->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c0, v1, c1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (c0 o1 (v1 o2 c2))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, c0, v1, c1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovoc_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vocovoc_expression1)

            template<typename T>  details::expression_node<T>* synthesize_covovoc_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // c0 o0 (v0 o1 (v1 o2 c1))
               typedef typename synthesize_vovoc_expression1<T>::node_type lcl_vovoc_t;

               const lcl_vovoc_t* vovoc = static_cast<const lcl_vovoc_t*>(branch[1]);
               const T  c0 = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vovoc->t0();
               const T& v1 = vovoc->t1();
               const T  c1 = vovoc->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovoc->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovoc->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovoc->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovoc->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c0, v0, v1, c1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("c0 o0 (v0 o1 (v1 o2 c1))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), c0, v0, v1, c1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covovoc_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_covovoc_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vococov_expression1<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 (c0 o1 (c1 o2 v1))
               typedef typename synthesize_cocov_expression1<T>::node_type lcl_cocov_t;

               const lcl_cocov_t* cocov = static_cast<const lcl_cocov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T  c0 = cocov->t0();
               const T  c1 = cocov->t1();
               const T& v1 = cocov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(cocov->f0());
               const details::operator_type o2 = expr_gen.get_operator(cocov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = cocov->f0();
               typename expression_generator<T>::binary_functor_t f2 = cocov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c0, c1, v1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 (c0 o1 (c1 o2 v1))\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, c0, c1, v1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vococov_expression1<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"  << expr_gen.to_str(o0)
                         << "(t" << expr_gen.to_str(o1)
                         << "(t" << expr_gen.to_str(o2)
                         << "t))";
            }
            compile_with_templates(synthesize_vococov_expression1)

            template<typename T>  details::expression_node<T>* synthesize_vovovov_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 ((v1 o1 v2) o2 v3)
               typedef typename synthesize_vovov_expression0<T>::node_type lcl_vovov_t;

               const lcl_vovov_t* vovov = static_cast<const lcl_vovov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vovov->t0();
               const T& v2 = vovov->t1();
               const T& v3 = vovov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, v2, v3, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 ((v1 o1 v2) o2 v3)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, v3, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovov_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovovov_expression2)

            template<typename T>  details::expression_node<T>* synthesize_vovovoc_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 ((v1 o1 v2) o2 c)
               typedef typename synthesize_vovoc_expression0<T>::node_type lcl_vovoc_t;

               const lcl_vovoc_t* vovoc = static_cast<const lcl_vovoc_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vovoc->t0();
               const T& v2 = vovoc->t1();
               const T   c = vovoc->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovoc->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovoc->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovoc->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovoc->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, v2, c, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 ((v1 o1 v2) o2 c)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, v2, c, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovovoc_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovovoc_expression2)

            template<typename T>  details::expression_node<T>* synthesize_vovocov_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 ((v1 o1 c) o2 v2)
               typedef typename synthesize_vocov_expression0<T>::node_type lcl_vocov_t;

               const lcl_vocov_t* vocov = static_cast<const lcl_vocov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T& v1 = vocov->t0();
               const T   c = vocov->t1();
               const T& v2 = vocov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vocov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vocov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vocov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vocov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, v1, c, v2, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 ((v1 o1 c) o2 v2)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, v1, c, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vovocov_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vovocov_expression2)

            template<typename T>  details::expression_node<T>* synthesize_vocovov_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 ((c o1 v1) o2 v2)
               typedef typename synthesize_covov_expression0<T>::node_type lcl_covov_t;

               const lcl_covov_t* covov = static_cast<const lcl_covov_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T   c = covov->t0();
               const T& v1 = covov->t1();
               const T& v2 = covov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(covov->f0());
               const details::operator_type o2 = expr_gen.get_operator(covov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = covov->f0();
               typename expression_generator<T>::binary_functor_t f2 = covov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c, v1, v2, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 ((c o1 v1) o2 v2)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, c, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovov_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vocovov_expression2)

            template<typename T>  details::expression_node<T>* synthesize_covovov_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // c o0 ((v1 o1 v2) o2 v3)
               typedef typename synthesize_vovov_expression0<T>::node_type lcl_vovov_t;

               const lcl_vovov_t* vovov = static_cast<const lcl_vovov_t*>(branch[1]);
               const T   c = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vovov->t0();
               const T& v1 = vovov->t1();
               const T& v2 = vovov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vovov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vovov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vovov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vovov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c, v0, v1, v2, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("c o0 ((v1 o1 v2) o2 v3)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), c, v0, v1, v2, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covovov_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_covovov_expression2)

            template<typename T>  details::expression_node<T>* synthesize_covocov_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // c0 o0 ((v0 o1 c1) o2 v1)
               typedef typename synthesize_vocov_expression0<T>::node_type lcl_vocov_t;

               const lcl_vocov_t* vocov = static_cast<const lcl_vocov_t*>(branch[1]);
               const T  c0 = static_cast<details::literal_node<T>*>(branch[0])->value();
               const T& v0 = vocov->t0();
               const T  c1 = vocov->t1();
               const T& v1 = vocov->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(vocov->f0());
               const details::operator_type o2 = expr_gen.get_operator(vocov->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = vocov->f0();
               typename expression_generator<T>::binary_functor_t f2 = vocov->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[0]);
               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), c0, v0, c1, v1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("c0 o0 ((v0 o1 c1) o2 v1)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), c0, v0, c1, v1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_covocov_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_covocov_expression2)

            template<typename T>  details::expression_node<T>* synthesize_vocovoc_expression2<T>::process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2])
            {
               // v0 o0 ((c0 o1 v1) o2 c1)
               typedef typename synthesize_covoc_expression0<T>::node_type lcl_covoc_t;

               const lcl_covoc_t* covoc = static_cast<const lcl_covoc_t*>(branch[1]);
               const T& v0 = static_cast<details::variable_node<T>*>(branch[0])->ref();
               const T  c0 = covoc->t0();
               const T& v1 = covoc->t1();
               const T  c1 = covoc->t2();
               const details::operator_type o0 = operation;
               const details::operator_type o1 = expr_gen.get_operator(covoc->f0());
               const details::operator_type o2 = expr_gen.get_operator(covoc->f1());

               typename expression_generator<T>::binary_functor_t f0 = reinterpret_cast<expression_generator<T>::binary_functor_t>(0);
               typename expression_generator<T>::binary_functor_t f1 = covoc->f0();
               typename expression_generator<T>::binary_functor_t f2 = covoc->f1();

               details::free_node(*(expr_gen.node_allocator()),branch[1]);

               details::expression_node<T>* result = expression_generator<T>::error_node();

               const bool synthesis_result =
                  synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, T3>
                     (expr_gen, id(expr_gen, o0, o1, o2), v0, c0, v1, c1, result);

               if (synthesis_result)
                  return result;
               else if (!expr_gen.valid_operator(o0,f0))
                  return expression_generator<T>::error_node();

               exprtk_debug(("v0 o0 ((c0 o1 v1) o2 c1)\n"));

               return node_type::allocate(*(expr_gen.node_allocator()), v0, c0, v1, c1, f0, f1, f2);
            }

            template<typename T>  std::string synthesize_vocovoc_expression2<T>::id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2)
            {
               return details::build_string()
                         << "t"   << expr_gen.to_str(o0)
                         << "((t" << expr_gen.to_str(o1)
                         << "t)"  << expr_gen.to_str(o2)
                         << "t)";
            }
            compile_with_templates(synthesize_vocovoc_expression2)
}
