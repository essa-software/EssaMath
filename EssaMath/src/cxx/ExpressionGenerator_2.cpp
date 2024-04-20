#include "include/ExpressionGenerator.hpp"
#include "include/Parser.hpp"
#include "include/Optimize.inl"

namespace Essa::Math{
            #define case_stmt(N)                                                         \
            if (is_true(arg[(2 * N)].first)) { return arg[(2 * N) + 1].first->value(); } \

               template<typename T> T expression_generator<T>::switch_nodes::switch_impl_1::process(const arg_list_t& arg)
               {
                  case_stmt(0)

                  assert(arg.size() == ((2 * 1) + 1));

                  return arg.back().first->value();
               }

               template<typename T> T expression_generator<T>::switch_nodes::switch_impl_2::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)

                  assert(arg.size() == ((2 * 2) + 1));

                  return arg.back().first->value();
               }

            template<typename T> T expression_generator<T>::switch_nodes::switch_impl_3::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)
                  case_stmt(2)

                  assert(arg.size() == ((2 * 3) + 1));

                  return arg.back().first->value();
               }
            
            template<typename T> T expression_generator<T>::switch_nodes::switch_impl_4::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)
                  case_stmt(2) case_stmt(3)

                  assert(arg.size() == ((2 * 4) + 1));

                  return arg.back().first->value();
               }
               
            template<typename T> T expression_generator<T>::switch_nodes::switch_impl_5::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)
                  case_stmt(2) case_stmt(3)
                  case_stmt(4)

                  assert(arg.size() == ((2 * 5) + 1));

                  return arg.back().first->value();
               }
               
            template<typename T> T expression_generator<T>::switch_nodes::switch_impl_6::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)
                  case_stmt(2) case_stmt(3)
                  case_stmt(4) case_stmt(5)

                  assert(arg.size() == ((2 * 6) + 1));

                  return arg.back().first->value();
               }
               
            template<typename T> T expression_generator<T>::switch_nodes::switch_impl_7::process(const arg_list_t& arg)
               {
                  case_stmt(0) case_stmt(1)
                  case_stmt(2) case_stmt(3)
                  case_stmt(4) case_stmt(5)
                  case_stmt(6)

                  assert(arg.size() == ((2 * 7) + 1));

                  return arg.back().first->value();
               }

            #undef case_stmt

         #define unary_opr_switch_statements             \
         case_stmt(details::e_abs   , details::abs_op  ) \
         case_stmt(details::e_acos  , details::acos_op ) \
         case_stmt(details::e_acosh , details::acosh_op) \
         case_stmt(details::e_asin  , details::asin_op ) \
         case_stmt(details::e_asinh , details::asinh_op) \
         case_stmt(details::e_atan  , details::atan_op ) \
         case_stmt(details::e_atanh , details::atanh_op) \
         case_stmt(details::e_cos   , details::cos_op  ) \
         case_stmt(details::e_cosh  , details::cosh_op ) \
         case_stmt(details::e_exp   , details::exp_op  ) \
         case_stmt(details::e_expm1 , details::expm1_op) \
         case_stmt(details::e_log   , details::log_op  ) \
         case_stmt(details::e_log10 , details::log10_op) \
         case_stmt(details::e_log2  , details::log2_op ) \
         case_stmt(details::e_log1p , details::log1p_op) \
         case_stmt(details::e_neg   , details::neg_op  ) \
         case_stmt(details::e_pos   , details::pos_op  ) \
         case_stmt(details::e_sin   , details::sin_op  ) \
         case_stmt(details::e_sinc  , details::sinc_op ) \
         case_stmt(details::e_sinh  , details::sinh_op ) \
         case_stmt(details::e_sqrt  , details::sqrt_op ) \
         case_stmt(details::e_tan   , details::tan_op  ) \
         case_stmt(details::e_tanh  , details::tanh_op ) \
         case_stmt(details::e_cot   , details::cot_op  ) \
         case_stmt(details::e_sec   , details::sec_op  ) \
         case_stmt(details::e_csc   , details::csc_op  ) \
         case_stmt(details::e_erf   , details::erf_op  ) \
         case_stmt(details::e_erfc  , details::erfc_op ) \
         case_stmt(details::e_ncdf  , details::ncdf_op ) \

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_uv_expression(const details::operator_type& operation,
                                                             expression_node_ptr (&branch)[1])
         {
            T& v = static_cast<details::variable_node<T>*>(branch[0])->ref();

            switch (operation)
            {
               #define case_stmt(op0, op1)                                                         \
               case op0 : return node_allocator_->template allocate<typename details::unary_variable_node<T,op1<T> > >(v);

               unary_opr_switch_statements
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_uvec_expression(const details::operator_type& operation,
                                                               expression_node_ptr (&branch)[1])
         {
            switch (operation)
            {
               #define case_stmt(op0, op1)                                                   \
               case op0 : return node_allocator_->template allocate<typename details::unary_vector_node<T,op1<T> > > (operation, branch[0]);

               unary_opr_switch_statements
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::synthesize_unary_expression(const details::operator_type& operation,
                                                                expression_node_ptr (&branch)[1])
         {
            switch (operation)
            {
               #define case_stmt(op0, op1)                                                               \
               case op0 : return node_allocator_->template allocate<typename details::unary_branch_node<T,op1<T> > >(branch[0]);

               unary_opr_switch_statements
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::const_optimise_sf3(const details::operator_type& operation,
                                                       expression_node_ptr (&branch)[3])
         {
            expression_node_ptr temp_node = error_node();

            switch (operation)
            {
               #define case_stmt(op)                                                                                                      \
               case details::e_sf##op : temp_node = node_allocator_->template allocate<details::sf3_node<T,details::sf##op##_op<T> > >   \
                                (operation, branch);                                                                                      \
                             break;

               case_stmt(00) case_stmt(01) case_stmt(02) case_stmt(03)
               case_stmt(04) case_stmt(05) case_stmt(06) case_stmt(07)
               case_stmt(08) case_stmt(09) case_stmt(10) case_stmt(11)
               case_stmt(12) case_stmt(13) case_stmt(14) case_stmt(15)
               case_stmt(16) case_stmt(17) case_stmt(18) case_stmt(19)
               case_stmt(20) case_stmt(21) case_stmt(22) case_stmt(23)
               case_stmt(24) case_stmt(25) case_stmt(26) case_stmt(27)
               case_stmt(28) case_stmt(29) case_stmt(30) case_stmt(31)
               case_stmt(32) case_stmt(33) case_stmt(34) case_stmt(35)
               case_stmt(36) case_stmt(37) case_stmt(38) case_stmt(39)
               case_stmt(40) case_stmt(41) case_stmt(42) case_stmt(43)
               case_stmt(44) case_stmt(45) case_stmt(46) case_stmt(47)
               #undef case_stmt
               default : return error_node();
            }

            const T v = temp_node->value();

            details::free_node(*node_allocator_,temp_node);

            return node_allocator_->template allocate<literal_node_t>(v);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::varnode_optimise_sf3(const details::operator_type& operation, expression_node_ptr (&branch)[3])
         {
            typedef details::variable_node<T>* variable_ptr;

            const T& v0 = static_cast<variable_ptr>(branch[0])->ref();
            const T& v1 = static_cast<variable_ptr>(branch[1])->ref();
            const T& v2 = static_cast<variable_ptr>(branch[2])->ref();

            switch (operation)
            {
               #define case_stmt(op)                                                                \
               case details::e_sf##op : return node_allocator_->template allocate_rrr<details::sf3_var_node<T,details::sf##op##_op<T> > > \
                                (v0, v1, v2);                                                       \

               case_stmt(00) case_stmt(01) case_stmt(02) case_stmt(03)
               case_stmt(04) case_stmt(05) case_stmt(06) case_stmt(07)
               case_stmt(08) case_stmt(09) case_stmt(10) case_stmt(11)
               case_stmt(12) case_stmt(13) case_stmt(14) case_stmt(15)
               case_stmt(16) case_stmt(17) case_stmt(18) case_stmt(19)
               case_stmt(20) case_stmt(21) case_stmt(22) case_stmt(23)
               case_stmt(24) case_stmt(25) case_stmt(26) case_stmt(27)
               case_stmt(28) case_stmt(29) case_stmt(30) case_stmt(31)
               case_stmt(32) case_stmt(33) case_stmt(34) case_stmt(35)
               case_stmt(36) case_stmt(37) case_stmt(38) case_stmt(39)
               case_stmt(40) case_stmt(41) case_stmt(42) case_stmt(43)
               case_stmt(44) case_stmt(45) case_stmt(46) case_stmt(47)
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::special_function(const details::operator_type& operation, expression_node_ptr (&branch)[3])
         {
            if (!all_nodes_valid(branch))
               return error_node();
            else if (is_constant_foldable(branch))
               return const_optimise_sf3(operation,branch);
            else if (all_nodes_variables(branch))
               return varnode_optimise_sf3(operation,branch);
            else
            {
               switch (operation)
               {
                  #define case_stmt(op)                                                        \
                  case details::e_sf##op : return node_allocator_->template allocate<details::sf3_node<T,details::sf##op##_op<T> > >(operation, branch);

                  case_stmt(00) case_stmt(01) case_stmt(02) case_stmt(03)
                  case_stmt(04) case_stmt(05) case_stmt(06) case_stmt(07)
                  case_stmt(08) case_stmt(09) case_stmt(10) case_stmt(11)
                  case_stmt(12) case_stmt(13) case_stmt(14) case_stmt(15)
                  case_stmt(16) case_stmt(17) case_stmt(18) case_stmt(19)
                  case_stmt(20) case_stmt(21) case_stmt(22) case_stmt(23)
                  case_stmt(24) case_stmt(25) case_stmt(26) case_stmt(27)
                  case_stmt(28) case_stmt(29) case_stmt(30) case_stmt(31)
                  case_stmt(32) case_stmt(33) case_stmt(34) case_stmt(35)
                  case_stmt(36) case_stmt(37) case_stmt(38) case_stmt(39)
                  case_stmt(40) case_stmt(41) case_stmt(42) case_stmt(43)
                  case_stmt(44) case_stmt(45) case_stmt(46) case_stmt(47)
                  #undef case_stmt
                  default : return error_node();
               }
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::const_optimise_sf4(const details::operator_type& operation, expression_node_ptr (&branch)[4])
         {
            expression_node_ptr temp_node = error_node();

            switch (operation)
            {
               #define case_stmt(op)                                                                                                      \
               case details::e_sf##op : temp_node = node_allocator_-> template allocate<details::sf4_node<T,details::sf##op##_op<T> > >  \
                                            (operation, branch);                                                                          \
                                        break;

               case_stmt(48) case_stmt(49) case_stmt(50) case_stmt(51)
               case_stmt(52) case_stmt(53) case_stmt(54) case_stmt(55)
               case_stmt(56) case_stmt(57) case_stmt(58) case_stmt(59)
               case_stmt(60) case_stmt(61) case_stmt(62) case_stmt(63)
               case_stmt(64) case_stmt(65) case_stmt(66) case_stmt(67)
               case_stmt(68) case_stmt(69) case_stmt(70) case_stmt(71)
               case_stmt(72) case_stmt(73) case_stmt(74) case_stmt(75)
               case_stmt(76) case_stmt(77) case_stmt(78) case_stmt(79)
               case_stmt(80) case_stmt(81) case_stmt(82) case_stmt(83)
               case_stmt(84) case_stmt(85) case_stmt(86) case_stmt(87)
               case_stmt(88) case_stmt(89) case_stmt(90) case_stmt(91)
               case_stmt(92) case_stmt(93) case_stmt(94) case_stmt(95)
               case_stmt(96) case_stmt(97) case_stmt(98) case_stmt(99)
               #undef case_stmt
               default : return error_node();
            }

            const T v = temp_node->value();

            details::free_node(*node_allocator_,temp_node);

            return node_allocator_->template allocate<literal_node_t>(v);
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::varnode_optimise_sf4(const details::operator_type& operation, expression_node_ptr (&branch)[4])
         {
            typedef details::variable_node<T>* variable_ptr;

            const T& v0 = static_cast<variable_ptr>(branch[0])->ref();
            const T& v1 = static_cast<variable_ptr>(branch[1])->ref();
            const T& v2 = static_cast<variable_ptr>(branch[2])->ref();
            const T& v3 = static_cast<variable_ptr>(branch[3])->ref();

            switch (operation)
            {
               #define case_stmt(op)                                                                 \
               case details::e_sf##op : return node_allocator_->template allocate_rrrr<details::sf4_var_node<T,details::sf##op##_op<T> > > (v0, v1, v2, v3);

               case_stmt(48) case_stmt(49) case_stmt(50) case_stmt(51)
               case_stmt(52) case_stmt(53) case_stmt(54) case_stmt(55)
               case_stmt(56) case_stmt(57) case_stmt(58) case_stmt(59)
               case_stmt(60) case_stmt(61) case_stmt(62) case_stmt(63)
               case_stmt(64) case_stmt(65) case_stmt(66) case_stmt(67)
               case_stmt(68) case_stmt(69) case_stmt(70) case_stmt(71)
               case_stmt(72) case_stmt(73) case_stmt(74) case_stmt(75)
               case_stmt(76) case_stmt(77) case_stmt(78) case_stmt(79)
               case_stmt(80) case_stmt(81) case_stmt(82) case_stmt(83)
               case_stmt(84) case_stmt(85) case_stmt(86) case_stmt(87)
               case_stmt(88) case_stmt(89) case_stmt(90) case_stmt(91)
               case_stmt(92) case_stmt(93) case_stmt(94) case_stmt(95)
               case_stmt(96) case_stmt(97) case_stmt(98) case_stmt(99)
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::special_function(const details::operator_type& operation, expression_node_ptr (&branch)[4])
         {
            if (!all_nodes_valid(branch))
               return error_node();
            else if (is_constant_foldable(branch))
               return const_optimise_sf4(operation,branch);
            else if (all_nodes_variables(branch))
               return varnode_optimise_sf4(operation,branch);
            switch (operation)
            {
               #define case_stmt(op)                                                        \
               case details::e_sf##op : return node_allocator_->template allocate<details::sf4_node<T,details::sf##op##_op<T> > > (operation, branch);

               case_stmt(48) case_stmt(49) case_stmt(50) case_stmt(51)
               case_stmt(52) case_stmt(53) case_stmt(54) case_stmt(55)
               case_stmt(56) case_stmt(57) case_stmt(58) case_stmt(59)
               case_stmt(60) case_stmt(61) case_stmt(62) case_stmt(63)
               case_stmt(64) case_stmt(65) case_stmt(66) case_stmt(67)
               case_stmt(68) case_stmt(69) case_stmt(70) case_stmt(71)
               case_stmt(72) case_stmt(73) case_stmt(74) case_stmt(75)
               case_stmt(76) case_stmt(77) case_stmt(78) case_stmt(79)
               case_stmt(80) case_stmt(81) case_stmt(82) case_stmt(83)
               case_stmt(84) case_stmt(85) case_stmt(86) case_stmt(87)
               case_stmt(88) case_stmt(89) case_stmt(90) case_stmt(91)
               case_stmt(92) case_stmt(93) case_stmt(94) case_stmt(95)
               case_stmt(96) case_stmt(97) case_stmt(98) case_stmt(99)
               #undef case_stmt
               default : return error_node();
            }
         }

         template<typename T> bool expression_generator<T>::special_one_parameter_vararg(const details::operator_type& operation) const
         {
            return (
                     (details::e_sum  == operation) ||
                     (details::e_prod == operation) ||
                     (details::e_avg  == operation) ||
                     (details::e_min  == operation) ||
                     (details::e_max  == operation)
                   );
         }

         template<typename T> expression_generator<T>::expression_node_ptr expression_generator<T>::vector_element(const std::string& symbol,
                                                   vector_holder_ptr vector_base,
                                                   expression_node_ptr index)
         {
            expression_node_ptr result = error_node();
            if (details::is_constant_node(index))
            {
               std::size_t i = static_cast<std::size_t>(details::numeric::to_int64(index->value()));

               details::free_node(*node_allocator_,index);

               if (vector_base->rebaseable())
               {
                  return node_allocator_->template allocate<rebasevector_celem_node_t>(i,vector_base);
               }

               const typename ::Essa::Math::parser<T>::scope_element& se = parser_->sem_.get_element(symbol,i);

               if (se.index == i)
               {
                  result = se.var_node;
               }
               else
               {
                  typename ::Essa::Math::parser<T>::scope_element nse;
                  nse.name      = symbol;
                  nse.active    = true;
                  nse.ref_count = 1;
                  nse.type      = ::Essa::Math::parser<T>::scope_element::e_vecelem;
                  nse.index     = i;
                  nse.depth     = parser_->state_.scope_depth;
                  nse.data      = 0;
                  nse.var_node  = node_allocator_->template allocate<variable_node_t>((*(*vector_base)[i]), nse.name);

                  if (!parser_->sem_.add_element(nse))
                  {
                     parser_->set_synthesis_error("Failed to add new local vector element to SEM [1]");

                     parser_->sem_.free_element(nse);

                     result = error_node();
                  }

                  exprtk_debug(("vector_element() - INFO - Added new local vector element: %s\n",nse.name.c_str()));

                  parser_->state_.activate_side_effect("vector_element()");

                  result = nse.var_node;
               }
            }
            else if (vector_base->rebaseable())
               result = node_allocator_->template allocate<rebasevector_elem_node_t>(index,vector_base);
            else
               result = node_allocator_->template allocate<vector_elem_node_t>(index,vector_base);

            return result;
         }

         template<typename T> void expression_generator<T>::lodge_assignment(symbol_type cst, expression_node_ptr node)
         {
            parser_->state_.activate_side_effect("lodge_assignment()");

            if (!parser_->dec_.collect_assignments())
               return;

            std::string symbol_name;

            switch (cst)
            {
               case e_st_variable : symbol_name = parser_->symtab_store_
                                                     .get_variable_name(node);
                                    break;
               case e_st_vector   : {
                                       typedef details::vector_holder<T> vector_holder_t;

                                       vector_holder_t& vh = static_cast<vector_node_t*>(node)->vec_holder();

                                       symbol_name = parser_->symtab_store_.get_vector_name(&vh);
                                    }
                                    break;

               case e_st_vecelem  : {
                                       typedef details::vector_holder<T> vector_holder_t;

                                       vector_holder_t& vh = static_cast<vector_elem_node_t*>(node)->vec_holder();

                                       symbol_name = parser_->symtab_store_.get_vector_name(&vh);

                                       cst = e_st_vector;
                                    }
                                    break;

               default            : return;
            }

            if (!symbol_name.empty())
            {
               parser_->dec_.add_assignment(symbol_name,static_cast<::Essa::Math::parser<T>::symbol_type>(cst));
            }
         }

         template<typename T> const void* expression_generator<T>::base_ptr(expression_node_ptr node)
         {
            if (node)
            {
               switch(node->type())
               {
                  case details::expression_node<T>::e_variable:
                     return reinterpret_cast<const void*>(&static_cast<variable_node_t*>(node)->ref());

                  case details::expression_node<T>::e_vecelem:
                     return reinterpret_cast<const void*>(&static_cast<vector_elem_node_t*>(node)->ref());

                  case details::expression_node<T>::e_rbvecelem:
                     return reinterpret_cast<const void*>(&static_cast<rebasevector_elem_node_t*>(node)->ref());

                  case details::expression_node<T>::e_rbveccelem:
                     return reinterpret_cast<const void*>(&static_cast<rebasevector_celem_node_t*>(node)->ref());

                  case details::expression_node<T>::e_vector:
                     return reinterpret_cast<const void*>(static_cast<vector_node_t*>(node)->vec_holder().data());

               }
            }

            return reinterpret_cast<const void*>(0);
         }

         template<typename T> bool expression_generator<T>::assign_immutable_symbol(expression_node_ptr node)
         {
            typename ::Essa::Math::parser<T>::interval_t interval;
            const void* baseptr_addr = base_ptr(node);

            exprtk_debug(("assign_immutable_symbol - base ptr addr: %p\n", baseptr_addr));

            if (parser_->immutable_memory_map_.in_interval(baseptr_addr,interval))
            {
               typename ::Essa::Math::parser<T>::immutable_symtok_map_t::iterator itr = parser_->immutable_symtok_map_.find(interval);

               if (parser_->immutable_symtok_map_.end() != itr)
               {
                  token_t& token = itr->second;
                  parser_->set_error(
                     parser_error::make_error(parser_error::e_parser,
                        token,
                        "ERR211 - Symbol '" + token.value + "' cannot be assigned-to as it is immutable.",
                        exprtk_error_location));
               }
               else
                  parser_->set_synthesis_error("Unable to assign symbol is immutable.");

               return true;
            }

            return false;
         }
         template class expression_generator<float>;
         template class expression_generator<double>;
         template class expression_generator<long double>;
         template class expression_generator<std::complex<float>>;
         template class expression_generator<std::complex<double>>;
         template class expression_generator<std::complex<long double>>;
}
