#pragma once
#include "Defines.hpp"
#include "ExpressionGenerator.hpp"

namespace Essa::Math{

         #define compile_with_templates(ClassName)             \
         template class ClassName<float>;                      \
         template class ClassName<double>;                     \
         template class ClassName<long double>;                \
         template class ClassName<std::complex<float>>;        \
         template class ClassName<std::complex<double>>;       \
         template class ClassName<std::complex<long double>>;
    
         #define basic_opr_switch_statements         \
         case_stmt(details::e_add , details::add_op) \
         case_stmt(details::e_sub , details::sub_op) \
         case_stmt(details::e_mul , details::mul_op) \
         case_stmt(details::e_div , details::div_op) \
         case_stmt(details::e_pow , details::pow_op) \

         #define extended_opr_switch_statements        \
         case_stmt(details::e_lt   , details::lt_op  ) \
         case_stmt(details::e_lte  , details::lte_op ) \
         case_stmt(details::e_gt   , details::gt_op  ) \
         case_stmt(details::e_gte  , details::gte_op ) \
         case_stmt(details::e_eq   , details::eq_op  ) \
         case_stmt(details::e_ne   , details::ne_op  ) \

        template<typename T>
         struct synthesize_vov_expression
         {
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);
         };

        template<typename T>
         struct synthesize_cov_expression
         {
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);
         };

        template<typename T>
         struct synthesize_voc_expression
         {
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);
         };

        template<typename T>
         struct synthesize_sf3ext_expression
         {
            template <typename T0, typename T1, typename T2>
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& sf3opr,
                                                      T0 t0, T1 t1, T2 t2)
            {
               switch (sf3opr)
               {
                  #define case_stmt(op)                                                                              \
                  case details::e_sf##op : return details::T0oT1oT2_sf3ext<T,T0,T1,T2,details::sf##op##_op<T> >:: \
                                allocate(*(expr_gen.node_allocator()), t0, t1, t2);                                   \

                  case_stmt(00) case_stmt(01) case_stmt(02) case_stmt(03)
                  case_stmt(04) case_stmt(05) case_stmt(06) case_stmt(07)
                  case_stmt(08) case_stmt(09) case_stmt(10) case_stmt(11)
                  case_stmt(12) case_stmt(13) case_stmt(14) case_stmt(15)
                  case_stmt(16) case_stmt(17) case_stmt(18) case_stmt(19)
                  case_stmt(20) case_stmt(21) case_stmt(22) case_stmt(23)
                  case_stmt(24) case_stmt(25) case_stmt(26) case_stmt(27)
                  case_stmt(28) case_stmt(29) case_stmt(30)
                  #undef case_stmt
                  default : return expression_generator<T>::error_node();
               }
            }

            template <typename T0, typename T1, typename T2>
            static bool compile(expression_generator<T>& expr_gen, const std::string& id,
                                       T0 t0, T1 t1, T2 t2,
                                       details::expression_node<T>*& result)
            {
               details::operator_type sf3opr;

               if (!expr_gen.sf3_optimisable(id,sf3opr) && !details::disable_enhanced_features)
                  return false;
               else
                  result = synthesize_sf3ext_expression<T>::template process<T0, T1, T2>
                              (expr_gen, sf3opr, t0, t1, t2);

               return true;
            }
         };

        template<typename T>
         struct synthesize_sf4ext_expression
         {
            template <typename T0, typename T1, typename T2, typename T3>
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& sf4opr,
                                                      T0 t0, T1 t1, T2 t2, T3 t3)
            {
               switch (sf4opr)
               {
                  #define case_stmt0(op)                                                                                      \
                  case details::e_sf##op : return details::T0oT1oT2oT3_sf4ext<T,T0,T1,T2,T3,details::sf##op##_op<T> >:: \
                                allocate(*(expr_gen.node_allocator()), t0, t1, t2, t3);                                        \

                  #define case_stmt1(op)                                                                                             \
                  case details::e_sf4ext##op : return details::T0oT1oT2oT3_sf4ext<T,T0,T1,T2,T3,details::sfext##op##_op<T> >:: \
                                allocate(*(expr_gen.node_allocator()), t0, t1, t2, t3);                                               \

                  case_stmt0(48) case_stmt0(49) case_stmt0(50) case_stmt0(51)
                  case_stmt0(52) case_stmt0(53) case_stmt0(54) case_stmt0(55)
                  case_stmt0(56) case_stmt0(57) case_stmt0(58) case_stmt0(59)
                  case_stmt0(60) case_stmt0(61) case_stmt0(62) case_stmt0(63)
                  case_stmt0(64) case_stmt0(65) case_stmt0(66) case_stmt0(67)
                  case_stmt0(68) case_stmt0(69) case_stmt0(70) case_stmt0(71)
                  case_stmt0(72) case_stmt0(73) case_stmt0(74) case_stmt0(75)
                  case_stmt0(76) case_stmt0(77) case_stmt0(78) case_stmt0(79)
                  case_stmt0(80) case_stmt0(81) case_stmt0(82) case_stmt0(83)

                  case_stmt1(00) case_stmt1(01) case_stmt1(02) case_stmt1(03)
                  case_stmt1(04) case_stmt1(05) case_stmt1(06) case_stmt1(07)
                  case_stmt1(08) case_stmt1(09) case_stmt1(10) case_stmt1(11)
                  case_stmt1(12) case_stmt1(13) case_stmt1(14) case_stmt1(15)
                  case_stmt1(16) case_stmt1(17) case_stmt1(18) case_stmt1(19)
                  case_stmt1(20) case_stmt1(21) case_stmt1(22) case_stmt1(23)
                  case_stmt1(24) case_stmt1(25) case_stmt1(26) case_stmt1(27)
                  case_stmt1(28) case_stmt1(29) case_stmt1(30) case_stmt1(31)
                  case_stmt1(32) case_stmt1(33) case_stmt1(34) case_stmt1(35)
                  case_stmt1(36) case_stmt1(37) case_stmt1(38) case_stmt1(39)
                  case_stmt1(40) case_stmt1(41) case_stmt1(42) case_stmt1(43)
                  case_stmt1(44) case_stmt1(45) case_stmt1(46) case_stmt1(47)
                  case_stmt1(48) case_stmt1(49) case_stmt1(50) case_stmt1(51)
                  case_stmt1(52) case_stmt1(53) case_stmt1(54) case_stmt1(55)
                  case_stmt1(56) case_stmt1(57) case_stmt1(58) case_stmt1(59)
                  case_stmt1(60) case_stmt1(61)

                  #undef case_stmt0
                  #undef case_stmt1
                  default : return expression_generator<T>::error_node();
               }
            }

            template <typename T0, typename T1, typename T2, typename T3>
            static bool compile(expression_generator<T>& expr_gen, const std::string& id,
                                       T0 t0, T1 t1, T2 t2, T3 t3,
                                       details::expression_node<T>*& result)
            {
               details::operator_type sf4opr;

               if (!expr_gen.sf4_optimisable(id,sf4opr))
                  return false;
               else
                  result = synthesize_sf4ext_expression<T>::template process<T0, T1, T2, T3>
                              (expr_gen, sf4opr, t0, t1, t2, t3);

               return true;
            }

            // T o (sf3ext)
            template <typename ExternalType>
            static bool compile_right(expression_generator<T>& expr_gen,
                                             ExternalType t,
                                             const details::operator_type& operation,
                                             details::expression_node<T>*& sf3node,
                                             details::expression_node<T>*& result)
            {
               if (!details::is_sf3ext_node(sf3node))
                  return false;

               typedef details::T0oT1oT2_base_node<T>* sf3ext_base_ptr;

               sf3ext_base_ptr n = static_cast<sf3ext_base_ptr>(sf3node);
               const std::string id = "t" + expr_gen.to_str(operation) + "(" + n->type_id() + ")";

               switch (n->type())
               {
                  case details::expression_node<T>::e_covoc : return compile_right_impl
                                                                    <typename expression_generator<T>::covoc_t::sf3_type_node,ExternalType, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_covov : return compile_right_impl
                                                                    <typename expression_generator<T>::covov_t::sf3_type_node,ExternalType, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vocov : return compile_right_impl
                                                                    <typename expression_generator<T>::vocov_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vovoc : return compile_right_impl
                                                                    <typename expression_generator<T>::vovoc_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vovov : return compile_right_impl
                                                                    <typename expression_generator<T>::vovov_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  default                                      : return false;
               }
            }

            // (sf3ext) o T
            template <typename ExternalType>
            static bool compile_left(expression_generator<T>& expr_gen,
                                            ExternalType t,
                                            const details::operator_type& operation,
                                            details::expression_node<T>*& sf3node,
                                            details::expression_node<T>*& result)
            {
               if (!details::is_sf3ext_node(sf3node))
                  return false;

               typedef details::T0oT1oT2_base_node<T>* sf3ext_base_ptr;

               sf3ext_base_ptr n = static_cast<sf3ext_base_ptr>(sf3node);

               const std::string id = "(" + n->type_id() + ")" + expr_gen.to_str(operation) + "t";

               switch (n->type())
               {
                  case details::expression_node<T>::e_covoc : return compile_left_impl
                                                                    <typename expression_generator<T>::covoc_t::sf3_type_node,ExternalType, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_covov : return compile_left_impl
                                                                    <typename expression_generator<T>::covov_t::sf3_type_node,ExternalType, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vocov : return compile_left_impl
                                                                    <typename expression_generator<T>::vocov_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vovoc : return compile_left_impl
                                                                    <typename expression_generator<T>::vovoc_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::ctype>
                                                                       (expr_gen, id, t, sf3node, result);

                  case details::expression_node<T>::e_vovov : return compile_left_impl
                                                                    <typename expression_generator<T>::vovov_t::sf3_type_node,ExternalType, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype, typename expression_generator<T>::vtype>
                                                                       (expr_gen, id, t, sf3node, result);

                  default                                      : return false;
               }
            }

            template <typename SF3TypeNode, typename ExternalType, typename T0, typename T1, typename T2>
            static bool compile_right_impl(expression_generator<T>& expr_gen,
                                                  const std::string& id,
                                                  ExternalType t,
                                                  details::expression_node<T>*& node,
                                                  details::expression_node<T>*& result)
            {
               SF3TypeNode* n = dynamic_cast<SF3TypeNode*>(node);

               if (n)
               {
                  T0 t0 = n->t0();
                  T1 t1 = n->t1();
                  T2 t2 = n->t2();

                  return synthesize_sf4ext_expression<T>::template compile<ExternalType, T0, T1, T2>
                            (expr_gen, id, t, t0, t1, t2, result);
               }
               else
                  return false;
            }

            template <typename SF3TypeNode, typename ExternalType, typename T0, typename T1, typename T2>
            static bool compile_left_impl(expression_generator<T>& expr_gen,
                                                 const std::string& id,
                                                 ExternalType t,
                                                 details::expression_node<T>*& node,
                                                 details::expression_node<T>*& result)
            {
               SF3TypeNode* n = dynamic_cast<SF3TypeNode*>(node);

               if (n)
               {
                  T0 t0 = n->t0();
                  T1 t1 = n->t1();
                  T2 t2 = n->t2();

                  return synthesize_sf4ext_expression<T>::template compile<T0, T1, T2, ExternalType>
                            (expr_gen, id, t0, t1, t2, t, result);
               }
               else
                  return false;
            }
         };

        template<typename T>
         struct synthesize_vovov_expression0
         {
            typedef typename expression_generator<T>::vovov_t::type0 node_type;
            typedef typename expression_generator<T>::vovov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vovov_expression1
         {
            typedef typename expression_generator<T>::vovov_t::type1 node_type;
            typedef typename expression_generator<T>::vovov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vovoc_expression0
         {
            typedef typename expression_generator<T>::vovoc_t::type0 node_type;
            typedef typename expression_generator<T>::vovoc_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vovoc_expression1
         {
            typedef typename expression_generator<T>::vovoc_t::type1 node_type;
            typedef typename expression_generator<T>::vovoc_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vocov_expression0
         {
            typedef typename expression_generator<T>::vocov_t::type0 node_type;
            typedef typename expression_generator<T>::vocov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vocov_expression1
         {
            typedef typename expression_generator<T>::vocov_t::type1 node_type;
            typedef typename expression_generator<T>::vocov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_covov_expression0
         {
            typedef typename expression_generator<T>::covov_t::type0 node_type;
            typedef typename expression_generator<T>::covov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_covov_expression1
         {
            typedef typename expression_generator<T>::covov_t::type1 node_type;
            typedef typename expression_generator<T>::covov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_covoc_expression0
         {
            typedef typename expression_generator<T>::covoc_t::type0 node_type;
            typedef typename expression_generator<T>::covoc_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_covoc_expression1
         {
            typedef typename expression_generator<T>::covoc_t::type1 node_type;
            typedef typename expression_generator<T>::covoc_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_cocov_expression0
         {
            typedef typename expression_generator<T>::cocov_t::type0 node_type;
            static details::expression_node<T>* process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2]);
         };

        template<typename T>
         struct synthesize_cocov_expression1
         {
            typedef typename expression_generator<T>::cocov_t::type1 node_type;
            typedef typename expression_generator<T>::cocov_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vococ_expression0
         {
            typedef typename expression_generator<T>::vococ_t::type0 node_type;
            typedef typename expression_generator<T>::vococ_t::sf3_type sf3_type;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1);
         };

        template<typename T>
         struct synthesize_vococ_expression1
         {
            typedef typename expression_generator<T>::vococ_t::type0 node_type;

            static details::expression_node<T>* process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2]);
         };

        template<typename T>
         struct synthesize_vovovov_expression0
         {
            typedef typename expression_generator<T>::vovovov_t::type0 node_type;
            typedef typename expression_generator<T>::vovovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovoc_expression0
         {
            typedef typename expression_generator<T>::vovovoc_t::type0 node_type;
            typedef typename expression_generator<T>::vovovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovocov_expression0
         {
            typedef typename expression_generator<T>::vovocov_t::type0 node_type;
            typedef typename expression_generator<T>::vovocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovov_expression0
         {
            typedef typename expression_generator<T>::vocovov_t::type0 node_type;
            typedef typename expression_generator<T>::vocovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovov_expression0
         {
            typedef typename expression_generator<T>::covovov_t::type0 node_type;
            typedef typename expression_generator<T>::covovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covocov_expression0
         {
            typedef typename expression_generator<T>::covocov_t::type0 node_type;
            typedef typename expression_generator<T>::covocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovoc_expression0
         {
            typedef typename expression_generator<T>::vocovoc_t::type0 node_type;
            typedef typename expression_generator<T>::vocovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovoc_expression0
         {
            typedef typename expression_generator<T>::covovoc_t::type0 node_type;
            typedef typename expression_generator<T>::covovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vococov_expression0
         {
            typedef typename expression_generator<T>::vococov_t::type0 node_type;
            typedef typename expression_generator<T>::vococov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovov_expression1
         {
            typedef typename expression_generator<T>::vovovov_t::type1 node_type;
            typedef typename expression_generator<T>::vovovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovoc_expression1
         {
            typedef typename expression_generator<T>::vovovoc_t::type1 node_type;
            typedef typename expression_generator<T>::vovovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovocov_expression1
         {
            typedef typename expression_generator<T>::vovocov_t::type1 node_type;
            typedef typename expression_generator<T>::vovocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovov_expression1
         {
            typedef typename expression_generator<T>::vocovov_t::type1 node_type;
            typedef typename expression_generator<T>::vocovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovov_expression1
         {
            typedef typename expression_generator<T>::covovov_t::type1 node_type;
            typedef typename expression_generator<T>::covovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covocov_expression1
         {
            typedef typename expression_generator<T>::covocov_t::type1 node_type;
            typedef typename expression_generator<T>::covocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovoc_expression1
         {
            typedef typename expression_generator<T>::vocovoc_t::type1 node_type;
            typedef typename expression_generator<T>::vocovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovoc_expression1
         {
            typedef typename expression_generator<T>::covovoc_t::type1 node_type;
            typedef typename expression_generator<T>::covovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;
            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vococov_expression1
         {
            typedef typename expression_generator<T>::vococov_t::type1 node_type;
            typedef typename expression_generator<T>::vococov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovov_expression2
         {
            typedef typename expression_generator<T>::vovovov_t::type2 node_type;
            typedef typename expression_generator<T>::vovovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovoc_expression2
         {
            typedef typename expression_generator<T>::vovovoc_t::type2 node_type;
            typedef typename expression_generator<T>::vovovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovocov_expression2
         {
            typedef typename expression_generator<T>::vovocov_t::type2 node_type;
            typedef typename expression_generator<T>::vovocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovov_expression2
         {
            typedef typename expression_generator<T>::vocovov_t::type2 node_type;
            typedef typename expression_generator<T>::vocovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovov_expression2
         {
            typedef typename expression_generator<T>::covovov_t::type2 node_type;
            typedef typename expression_generator<T>::covovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
        };

        template<typename T>
         struct synthesize_covocov_expression2
         {
            typedef typename expression_generator<T>::covocov_t::type2 node_type;
            typedef typename expression_generator<T>::covocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovoc_expression2
         {
            typedef typename expression_generator<T>::vocovoc_t::type2 node_type;
            typedef typename expression_generator<T>::vocovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovoc_expression2
         {
            typedef typename expression_generator<T>::covovoc_t::type2 node_type;
            typedef typename expression_generator<T>::covovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vococov_expression2
         {
            typedef typename expression_generator<T>::vococov_t::type2 node_type;
            static details::expression_node<T>* process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2]);

            static std::string id(expression_generator<T>&,
                                         const details::operator_type,
                                         const details::operator_type,
                                         const details::operator_type);
         };

        template<typename T>
         struct synthesize_vovovov_expression3
         {
            typedef typename expression_generator<T>::vovovov_t::type3 node_type;
            typedef typename expression_generator<T>::vovovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovoc_expression3
         {
            typedef typename expression_generator<T>::vovovoc_t::type3 node_type;
            typedef typename expression_generator<T>::vovovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovocov_expression3
         {
            typedef typename expression_generator<T>::vovocov_t::type3 node_type;
            typedef typename expression_generator<T>::vovocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovov_expression3
         {
            typedef typename expression_generator<T>::vocovov_t::type3 node_type;
            typedef typename expression_generator<T>::vocovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovov_expression3
         {
            typedef typename expression_generator<T>::covovov_t::type3 node_type;
            typedef typename expression_generator<T>::covovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covocov_expression3
         {
            typedef typename expression_generator<T>::covocov_t::type3 node_type;
            typedef typename expression_generator<T>::covocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovoc_expression3
         {
            typedef typename expression_generator<T>::vocovoc_t::type3 node_type;
            typedef typename expression_generator<T>::vocovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovoc_expression3
         {
            typedef typename expression_generator<T>::covovoc_t::type3 node_type;
            typedef typename expression_generator<T>::covovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vococov_expression3
         {
            typedef typename expression_generator<T>::vococov_t::type3 node_type;
            typedef typename expression_generator<T>::vococov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovov_expression4
         {
            typedef typename expression_generator<T>::vovovov_t::type4 node_type;
            typedef typename expression_generator<T>::vovovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovovoc_expression4
         {
            typedef typename expression_generator<T>::vovovoc_t::type4 node_type;
            typedef typename expression_generator<T>::vovovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vovocov_expression4
         {
            typedef typename expression_generator<T>::vovocov_t::type4 node_type;
            typedef typename expression_generator<T>::vovocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovov_expression4
         {
            typedef typename expression_generator<T>::vocovov_t::type4 node_type;
            typedef typename expression_generator<T>::vocovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovov_expression4
         {
            typedef typename expression_generator<T>::covovov_t::type4 node_type;
            typedef typename expression_generator<T>::covovov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covocov_expression4
         {
            typedef typename expression_generator<T>::covocov_t::type4 node_type;
            typedef typename expression_generator<T>::covocov_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vocovoc_expression4
         {
            typedef typename expression_generator<T>::vocovoc_t::type4 node_type;
            typedef typename expression_generator<T>::vocovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_covovoc_expression4
         {
            typedef typename expression_generator<T>::covovoc_t::type4 node_type;
            typedef typename expression_generator<T>::covovoc_t::sf4_type sf4_type;
            typedef typename node_type::T0 T0;
            typedef typename node_type::T1 T1;
            typedef typename node_type::T2 T2;
            typedef typename node_type::T3 T3;

            static details::expression_node<T>* process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      details::expression_node<T>* (&branch)[2]);

            static std::string id(expression_generator<T>& expr_gen,
                                         const details::operator_type o0,
                                         const details::operator_type o1,
                                         const details::operator_type o2);
         };

        template<typename T>
         struct synthesize_vococov_expression4
         {
            typedef typename expression_generator<T>::vococov_t::type4 node_type;
            static details::expression_node<T>* process(expression_generator<T>&,
                                                      const details::operator_type&,
                                                      details::expression_node<T>* (&)[2]);

            static std::string id(expression_generator<T>&,
                                         const details::operator_type,
                                         const details::operator_type,
                                         const details::operator_type);
         };


        template<typename T>
         struct synthesize_binary_ext_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_vob_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_bov_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_cob_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_boc_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_cocob_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };

        template<typename T>
         struct synthesize_coboc_expression
         {
            static expression_generator<T>::expression_node_ptr process(expression_generator<T>& expr_gen,
                                                      const details::operator_type& operation,
                                                      expression_generator<T>::expression_node_ptr (&branch)[2]);
         };
}
