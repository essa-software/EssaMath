#include "include/Parser.hpp"

namespace Essa::Math{
      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_multi_sequence(const std::string& source,
                                                      const bool enforce_crlbrackets)
      {
         token_t::token_type open_bracket  = token_t::e_lcrlbracket;
         token_t::token_type close_bracket = token_t::e_rcrlbracket;
         token_t::token_type seperator     = token_t::e_eof;

         if (!token_is(open_bracket))
         {
            if (!enforce_crlbrackets && token_is(token_t::e_lbracket))
            {
               open_bracket  = token_t::e_lbracket;
               close_bracket = token_t::e_rbracket;
               seperator     = token_t::e_comma;
            }
            else
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR102 - Expected '" + token_t::to_str(open_bracket) + "' for call to multi-sequence" +
                             ((!source.empty()) ? std::string(" section of " + source): ""),
                             exprtk_error_location));

               return error_node();
            }
         }
         else if (token_is(close_bracket))
         {
            return node_allocator_.allocate<details::null_node<T> >();
         }

         std::vector<expression_node_ptr> arg_list;
         std::vector<bool> side_effect_list;

         expression_node_ptr result = error_node();

         scoped_vec_delete<expression_node_t> sdd((*this),arg_list);

         scope_handler sh(*this);

         scoped_bool_or_restorer sbr(state_.side_effect_present);

         for ( ; ; )
         {
            state_.side_effect_present = false;

            expression_node_ptr arg = parse_expression();

            if (0 == arg)
               return error_node();
            else
            {
               arg_list.push_back(arg);
               side_effect_list.push_back(state_.side_effect_present);
            }

            if (token_is(close_bracket))
               break;

            const bool is_next_close = peek_token_is(close_bracket);

            if (!token_is(seperator) && is_next_close)
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR103 - Expected '" + details::to_str(seperator) + "' for call to multi-sequence section of " + source,
                             exprtk_error_location));

               return error_node();
            }

            if (token_is(close_bracket))
               break;
         }

         result = simplify(arg_list, side_effect_list, source.empty());

         sdd.delete_ptr = (0 == result);
         return result;
      }

      template<typename T> void parser<T>::lodge_symbol(const std::string& symbol,
                               const symbol_type st)
      {
         dec_.add_symbol(symbol,st);
      }

      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_vector()
      {
         const std::string symbol = current_token().value;

         vector_holder_ptr vec = vector_holder_ptr(0);

         const scope_element& se = sem_.get_active_element(symbol);

         if (
              !details::imatch(se.name, symbol) ||
              (se.depth > state_.scope_depth)   ||
              (scope_element::e_vector != se.type)
            )
         {
            typedef typename symtab_store::vector_context vec_ctxt_t;
            vec_ctxt_t vec_ctx = symtab_store_.get_vector_context(symbol);

            if (0 == vec_ctx.vector_holder)
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR114 - Symbol '" + symbol+ " not a vector",
                             exprtk_error_location));

               return error_node();
            }

            assert(0 != vec_ctx.vector_holder);
            assert(0 != vec_ctx.symbol_table );

            vec = vec_ctx.vector_holder;

            if (symbol_table_t::e_immutable == vec_ctx.symbol_table->mutability())
            {
               lodge_immutable_symbol(
                  current_token(),
                  make_memory_range(vec->data(), vec->size()));
            }
         }
         else
            vec = se.vec_node;

         assert(0 != vec);

         expression_node_ptr index_expr = error_node();

         next_token();

         if (!token_is(token_t::e_lsqrbracket))
         {
            return node_allocator_.allocate<vector_node_t>(vec);
         }
         else if (token_is(token_t::e_rsqrbracket))
         {
            return expression_generator_(T(vec->size()));
         }
         else if (0 == (index_expr = parse_expression()))
         {
            set_error(
               make_error(parser_error::e_syntax,
                          current_token(),
                          "ERR115 - Failed to parse index for vector: '" + symbol + "'",
                          exprtk_error_location));

            return error_node();
         }
         else if (!token_is(token_t::e_rsqrbracket))
         {
            set_error(
               make_error(parser_error::e_syntax,
                          current_token(),
                          "ERR116 - Expected ']' for index of vector: '" + symbol + "'",
                          exprtk_error_location));

            free_node(node_allocator_,index_expr);

            return error_node();
         }

         // Perform compile-time range check
         if (details::is_constant_node(index_expr))
         {
            const std::size_t index    = static_cast<std::size_t>(details::numeric::to_int32(index_expr->value()));
            const std::size_t vec_size = vec->size();

            if (index >= vec_size)
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR117 - Index of " + details::to_str(index) + " out of range for "
                             "vector '" + symbol + "' of size " + details::to_str(vec_size),
                             exprtk_error_location));

               free_node(node_allocator_,index_expr);

               return error_node();
            }
         }

         return expression_generator_.vector_element(symbol, vec, index_expr);
      }

      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_symtab_symbol()
      {
         const std::string symbol = current_token().value;

         // Are we dealing with a variable or a special constant?
         typedef typename symtab_store::variable_context var_ctxt_t;
         var_ctxt_t var_ctx = symtab_store_.get_variable_context(symbol);

         if (var_ctx.variable)
         {
            assert(var_ctx.symbol_table);

            expression_node_ptr result_variable = var_ctx.variable;

            if (symtab_store_.is_constant_node(symbol))
            {
               result_variable = expression_generator_(var_ctx.variable->value());
            }
            else if (symbol_table_t::e_immutable == var_ctx.symbol_table->mutability())
            {
               lodge_immutable_symbol(current_token(), make_memory_range(var_ctx.variable->ref()));
               result_variable = var_ctx.variable;
            }

            if (!post_variable_process(symbol))
               return error_node();

            lodge_symbol(symbol, e_st_variable);

            next_token();

            return result_variable;
         }

         // Are we dealing with a locally defined variable, vector or string?
         if (!sem_.empty())
         {
            scope_element& se = sem_.get_active_element(symbol);

            if (se.active && details::imatch(se.name, symbol))
            {
               if (scope_element::e_variable == se.type)
               {
                  se.active = true;
                  lodge_symbol(symbol, e_st_local_variable);

                  if (!post_variable_process(symbol))
                     return error_node();

                  next_token();

                  return se.var_node;
               }
               else if (scope_element::e_vector == se.type)
               {
                  return parse_vector();
               }
            }
         }

         // Are we dealing with a vector?
         if (symtab_store_.is_vector(symbol))
         {
            lodge_symbol(symbol, e_st_vector);
            return parse_vector();
         }

         if (details::is_reserved_symbol(symbol))
         {
               if (
                    settings_.function_enabled(symbol) ||
                    !details::is_base_function(symbol)
                  )
               {
                  set_error(
                     make_error(parser_error::e_syntax,
                                current_token(),
                                "ERR199 - Invalid use of reserved symbol '" + symbol + "'",
                                exprtk_error_location));

                  return error_node();
               }
         }

         // Should we handle unknown symbols?
         if (resolve_unknown_symbol_ && unknown_symbol_resolver_)
         {
            if (!(settings_.rsrvd_sym_usr_disabled() && details::is_reserved_symbol(symbol)))
            {
               symbol_table_t& symtab = symtab_store_.get_symbol_table();

               std::string error_message;

               if (unknown_symbol_resolver::e_usrmode_default == unknown_symbol_resolver_->mode)
               {
                  T default_value = T(0);

                  typename unknown_symbol_resolver::usr_symbol_type usr_symbol_type = unknown_symbol_resolver::e_usr_unknown_type;

                  if (unknown_symbol_resolver_->process(symbol, usr_symbol_type, default_value, error_message))
                  {
                     bool create_result = false;

                     switch (usr_symbol_type)
                     {
                        case unknown_symbol_resolver::e_usr_variable_type : create_result = symtab.create_variable(symbol, default_value);
                                                                            break;

                        case unknown_symbol_resolver::e_usr_constant_type : create_result = symtab.add_constant(symbol, default_value);
                                                                            break;

                        default                                           : create_result = false;
                     }

                     if (create_result)
                     {
                        expression_node_ptr var = symtab_store_.get_variable(symbol);

                        if (var)
                        {
                           if (symtab_store_.is_constant_node(symbol))
                           {
                              var = expression_generator_(var->value());
                           }

                           lodge_symbol(symbol, e_st_variable);

                           if (!post_variable_process(symbol))
                              return error_node();

                           next_token();

                           return var;
                        }
                     }
                  }

                  set_error(
                     make_error(parser_error::e_symtab,
                                current_token(),
                                "ERR200 - Failed to create variable: '" + symbol + "'" +
                                (error_message.empty() ? "" : " - " + error_message),
                                exprtk_error_location));

               }
               else if (unknown_symbol_resolver::e_usrmode_extended == unknown_symbol_resolver_->mode)
               {
                  if (unknown_symbol_resolver_->process(symbol, symtab, error_message))
                  {
                     expression_node_ptr result = parse_symtab_symbol();

                     if (result)
                     {
                        return result;
                     }
                  }

                  set_error(
                     make_error(parser_error::e_symtab,
                                current_token(),
                                "ERR201 - Failed to resolve symbol: '" + symbol + "'" +
                                (error_message.empty() ? "" : " - " + error_message),
                                exprtk_error_location));
               }

               return error_node();
            }
         }

         set_error(
            make_error(parser_error::e_syntax,
                       current_token(),
                       "ERR202 - Undefined symbol: '" + symbol + "'",
                       exprtk_error_location));

         return error_node();
      }

      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_symbol()
      {
         static const std::string symbol_if       = "if"      ;
         static const std::string symbol_while    = "while"   ;
         static const std::string symbol_repeat   = "repeat"  ;
         static const std::string symbol_for      = "for"     ;
         static const std::string symbol_switch   = "switch"  ;
         static const std::string symbol_null     = "null"    ;
         static const std::string symbol_break    = "break"   ;
         static const std::string symbol_continue = "continue";
         static const std::string symbol_var      = "var"     ;
         static const std::string symbol_swap     = "swap"    ;
         static const std::string symbol_return   = "return"  ;
         static const std::string symbol_not      = "not"     ;

         const std::string symbol = current_token().value;

         if (valid_base_operation(symbol))
         {
            return parse_base_operation();
         }
         else if (details::is_valid_sf_symbol(symbol))
         {
            // return parse_special_function();
            return error_node();
         }
         else if (details::imatch(symbol, symbol_null))
         {
            return parse_null_statement();
         }
         else if (symtab_store_.valid() || !sem_.empty())
         {
            return parse_symtab_symbol();
         }
         else
         {
            set_error(
               make_error(parser_error::e_symtab,
                          current_token(),
                          "ERR203 - Variable or function detected, yet symbol-table is invalid, Symbol: " + symbol,
                          exprtk_error_location));

            return error_node();
         }
      }

        template<typename T> parser<T>::expression_node_ptr parser<T>::parse_branch(precedence_level precedence)
      {
         stack_limit_handler slh(*this);

         if (!slh)
         {
            return error_node();
         }

         expression_node_ptr branch = error_node();

         if (token_t::e_number == current_token().type)
         {
            T numeric_value = T(0);

            if (details::string_to_real(current_token().value, numeric_value))
            {
               expression_node_ptr literal_exp = expression_generator_(numeric_value);

               if (0 == literal_exp)
               {
                  set_error(
                     make_error(parser_error::e_numeric,
                                current_token(),
                                "ERR204 - Failed generate node for scalar: '" + current_token().value + "'",
                                exprtk_error_location));

                  return error_node();
               }

               next_token();
               branch = literal_exp;
            }
            else
            {
               set_error(
                  make_error(parser_error::e_numeric,
                             current_token(),
                             "ERR205 - Failed to convert '" + current_token().value + "' to a number",
                             exprtk_error_location));

               return error_node();
            }
         }
         else if (token_t::e_symbol == current_token().type)
         {
            branch = parse_symbol();
         }
         else if (token_t::e_lbracket == current_token().type)
         {
            next_token();

            if (0 == (branch = parse_expression()))
               return error_node();
            else if (!token_is(token_t::e_rbracket))
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR206 - Expected ')' instead of: '" + current_token().value + "'",
                             exprtk_error_location));

               details::free_node(node_allocator_,branch);

               return error_node();
            }
            else if (!post_bracket_process(token_t::e_lbracket,branch))
            {
               details::free_node(node_allocator_,branch);

               return error_node();
            }
         }
         else if (token_t::e_lsqrbracket == current_token().type)
         {
            next_token();

            if (0 == (branch = parse_expression()))
               return error_node();
            else if (!token_is(token_t::e_rsqrbracket))
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR207 - Expected ']' instead of: '" + current_token().value + "'",
                             exprtk_error_location));

               details::free_node(node_allocator_,branch);

               return error_node();
            }
            else if (!post_bracket_process(token_t::e_lsqrbracket,branch))
            {
               details::free_node(node_allocator_,branch);

               return error_node();
            }
         }
         else if (token_t::e_lcrlbracket == current_token().type)
         {
            next_token();

            if (0 == (branch = parse_expression()))
               return error_node();
            else if (!token_is(token_t::e_rcrlbracket))
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR208 - Expected '}' instead of: '" + current_token().value + "'",
                             exprtk_error_location));

               details::free_node(node_allocator_,branch);

               return error_node();
            }
            else if (!post_bracket_process(token_t::e_lcrlbracket,branch))
            {
               details::free_node(node_allocator_,branch);

               return error_node();
            }
         }
         else if (token_t::e_sub == current_token().type)
         {
            next_token();
            branch = parse_expression(e_level11);

            if (
                 branch &&
                 !(
                    details::is_neg_unary_node    (branch) &&
                    simplify_unary_negation_branch(branch)
                  )
               )
            {
               expression_node_ptr result = expression_generator_(details::e_neg,branch);

               if (0 == result)
               {
                  details::free_node(node_allocator_,branch);

                  return error_node();
               }
               else
                  branch = result;
            }
         }
         else if (token_t::e_add == current_token().type)
         {
            next_token();
            branch = parse_expression(e_level13);
         }
         else if (token_t::e_eof == current_token().type)
         {
            set_error(
               make_error(parser_error::e_syntax,
                          current_token(),
                          "ERR209 - Premature end of expression[1]",
                          exprtk_error_location));

            return error_node();
         }
         else
         {
            set_error(
               make_error(parser_error::e_syntax,
                          current_token(),
                          "ERR210 - Premature end of expression[2]",
                          exprtk_error_location));

            return error_node();
         }

         return branch;
      }

      template<typename T> void parser<T>::set_error(const parser_error::type& error_type)
      {
         error_list_.push_back(error_type);
      }

      template<typename T> void parser<T>::remove_last_error()
      {
         if (!error_list_.empty())
         {
            error_list_.pop_back();
         }
      }

      template<typename T> void parser<T>::set_synthesis_error(const std::string& synthesis_error_message)
      {
         if (synthesis_error_.empty())
         {
            synthesis_error_ = synthesis_error_message;
         }
      }

      template<typename T> void parser<T>::register_local_vars(expression<T>& e)
      {
         for (std::size_t i = 0; i < sem_.size(); ++i)
         {
            scope_element& se = sem_.get_element(i);

            if (
                 (scope_element::e_variable == se.type) ||
                 (scope_element::e_vecelem  == se.type)
               )
            {
               if (se.var_node)
               {
                  e.register_local_var(se.var_node);
               }

               if (se.data)
               {
                  e.register_local_data(se.data, 1, 0);
               }
            }
            else if (scope_element::e_vector == se.type)
            {
               if (se.vec_node)
               {
                  e.register_local_var(se.vec_node);
               }

               if (se.data)
               {
                  e.register_local_data(se.data, se.size, 1);
               }
            }

            se.var_node  = 0;
            se.vec_node  = 0;
            se.data      = 0;
            se.ref_count = 0;
            se.active    = false;
         }
      }

      template<typename T> void parser<T>::register_return_results(expression<T>& e)
      {
         e.register_return_results(results_context_);
         results_context_ = 0;
      }

      template<typename T> void parser<T>::load_unary_operations_map(unary_op_map_t& m)
      {
         #define register_unary_op(Op, UnaryFunctor)            \
         m.insert(std::make_pair(Op,UnaryFunctor<T>::process)); \

         register_unary_op(details::e_abs   , details::abs_op  )
         register_unary_op(details::e_acos  , details::acos_op )
         register_unary_op(details::e_acosh , details::acosh_op)
         register_unary_op(details::e_asin  , details::asin_op )
         register_unary_op(details::e_asinh , details::asinh_op)
         register_unary_op(details::e_atanh , details::atanh_op)
         register_unary_op(details::e_cos   , details::cos_op  )
         register_unary_op(details::e_cosh  , details::cosh_op )
         register_unary_op(details::e_exp   , details::exp_op  )
         register_unary_op(details::e_expm1 , details::expm1_op)
         register_unary_op(details::e_log   , details::log_op  )
         register_unary_op(details::e_log10 , details::log10_op)
         register_unary_op(details::e_log2  , details::log2_op )
         register_unary_op(details::e_log1p , details::log1p_op)
         register_unary_op(details::e_neg   , details::neg_op  )
         register_unary_op(details::e_pos   , details::pos_op  )
         register_unary_op(details::e_sin   , details::sin_op  )
         register_unary_op(details::e_sinc  , details::sinc_op )
         register_unary_op(details::e_sinh  , details::sinh_op )
         register_unary_op(details::e_sqrt  , details::sqrt_op )
         register_unary_op(details::e_tan   , details::tan_op  )
         register_unary_op(details::e_tanh  , details::tanh_op )
         register_unary_op(details::e_cot   , details::cot_op  )
         register_unary_op(details::e_sec   , details::sec_op  )
         register_unary_op(details::e_csc   , details::csc_op  )
         register_unary_op(details::e_erf   , details::erf_op  )
         register_unary_op(details::e_erfc  , details::erfc_op )
         register_unary_op(details::e_ncdf  , details::ncdf_op )
         #undef register_unary_op
      }

      template<typename T> void parser<T>::load_binary_operations_map(binary_op_map_t& m)
      {
         typedef typename binary_op_map_t::value_type value_type;

         #define register_binary_op(Op, BinaryFunctor)       \
         m.insert(value_type(Op,BinaryFunctor<T>::process)); \

         register_binary_op(details::e_add  , details::add_op )
         register_binary_op(details::e_sub  , details::sub_op )
         register_binary_op(details::e_mul  , details::mul_op )
         register_binary_op(details::e_div  , details::div_op )
         register_binary_op(details::e_pow  , details::pow_op )
         register_binary_op(details::e_lt   , details::lt_op  )
         register_binary_op(details::e_lte  , details::lte_op )
         register_binary_op(details::e_gt   , details::gt_op  )
         register_binary_op(details::e_gte  , details::gte_op )
         register_binary_op(details::e_eq   , details::eq_op  )
         register_binary_op(details::e_ne   , details::ne_op  )
         #undef register_binary_op
      }

      template<typename T> void parser<T>::load_inv_binary_operations_map(inv_binary_op_map_t& m)
      {
         typedef typename inv_binary_op_map_t::value_type value_type;

         #define register_binary_op(Op, BinaryFunctor)       \
         m.insert(value_type(BinaryFunctor<T>::process,Op)); \

         register_binary_op(details::e_add  , details::add_op )
         register_binary_op(details::e_sub  , details::sub_op )
         register_binary_op(details::e_mul  , details::mul_op )
         register_binary_op(details::e_div  , details::div_op )
         register_binary_op(details::e_pow  , details::pow_op )
         register_binary_op(details::e_lt   , details::lt_op  )
         register_binary_op(details::e_lte  , details::lte_op )
         register_binary_op(details::e_gt   , details::gt_op  )
         register_binary_op(details::e_gte  , details::gte_op )
         register_binary_op(details::e_eq   , details::eq_op  )
         register_binary_op(details::e_ne   , details::ne_op  )
         #undef register_binary_op
      }

      template<typename T> void parser<T>::load_sf3_map(sf3_map_t& sf3_map)
      {
         typedef std::pair<trinary_functor_t,details::operator_type> pair_t;

         #define register_sf3(Op)                                                                             \
         sf3_map[details::sf##Op##_op<T>::id()] = pair_t(details::sf##Op##_op<T>::process,details::e_sf##Op); \

         register_sf3(00) register_sf3(01) register_sf3(02) register_sf3(03)
         register_sf3(04) register_sf3(05) register_sf3(06) register_sf3(07)
         register_sf3(08) register_sf3(09) register_sf3(10) register_sf3(11)
         register_sf3(12) register_sf3(13) register_sf3(14) register_sf3(15)
         register_sf3(16) register_sf3(17) register_sf3(18) register_sf3(19)
         register_sf3(20) register_sf3(21) register_sf3(22) register_sf3(23)
         register_sf3(24) register_sf3(25) register_sf3(26) register_sf3(27)
         register_sf3(28) register_sf3(29) register_sf3(30)
         #undef register_sf3

         #define register_sf3_extid(Id, Op)                                        \
         sf3_map[Id] = pair_t(details::sf##Op##_op<T>::process,details::e_sf##Op); \

         register_sf3_extid("(t-t)-t",23)  // (t-t)-t --> t-(t+t)
         #undef register_sf3_extid
      }

      template<typename T> void parser<T>::load_sf4_map(sf4_map_t& sf4_map)
      {
         typedef std::pair<quaternary_functor_t,details::operator_type> pair_t;

         #define register_sf4(Op)                                                                             \
         sf4_map[details::sf##Op##_op<T>::id()] = pair_t(details::sf##Op##_op<T>::process,details::e_sf##Op); \

         register_sf4(48) register_sf4(49) register_sf4(50) register_sf4(51)
         register_sf4(52) register_sf4(53) register_sf4(54) register_sf4(55)
         register_sf4(56) register_sf4(57) register_sf4(58) register_sf4(59)
         register_sf4(60) register_sf4(61) register_sf4(62) register_sf4(63)
         register_sf4(64) register_sf4(65) register_sf4(66) register_sf4(67)
         register_sf4(68) register_sf4(69) register_sf4(70) register_sf4(71)
         register_sf4(72) register_sf4(73) register_sf4(74) register_sf4(75)
         register_sf4(76) register_sf4(77) register_sf4(78) register_sf4(79)
         register_sf4(80) register_sf4(81) register_sf4(82) register_sf4(83)
         #undef register_sf4

         #define register_sf4ext(Op)                                                                                    \
         sf4_map[details::sfext##Op##_op<T>::id()] = pair_t(details::sfext##Op##_op<T>::process,details::e_sf4ext##Op); \

         register_sf4ext(00) register_sf4ext(01) register_sf4ext(02) register_sf4ext(03)
         register_sf4ext(04) register_sf4ext(05) register_sf4ext(06) register_sf4ext(07)
         register_sf4ext(08) register_sf4ext(09) register_sf4ext(10) register_sf4ext(11)
         register_sf4ext(12) register_sf4ext(13) register_sf4ext(14) register_sf4ext(15)
         register_sf4ext(16) register_sf4ext(17) register_sf4ext(18) register_sf4ext(19)
         register_sf4ext(20) register_sf4ext(21) register_sf4ext(22) register_sf4ext(23)
         register_sf4ext(24) register_sf4ext(25) register_sf4ext(26) register_sf4ext(27)
         register_sf4ext(28) register_sf4ext(29) register_sf4ext(30) register_sf4ext(31)
         register_sf4ext(32) register_sf4ext(33) register_sf4ext(34) register_sf4ext(35)
         register_sf4ext(36) register_sf4ext(36) register_sf4ext(38) register_sf4ext(39)
         register_sf4ext(40) register_sf4ext(41) register_sf4ext(42) register_sf4ext(43)
         register_sf4ext(44) register_sf4ext(45) register_sf4ext(46) register_sf4ext(47)
         register_sf4ext(48) register_sf4ext(49) register_sf4ext(50) register_sf4ext(51)
         register_sf4ext(52) register_sf4ext(53) register_sf4ext(54) register_sf4ext(55)
         register_sf4ext(56) register_sf4ext(57) register_sf4ext(58) register_sf4ext(59)
         register_sf4ext(60) register_sf4ext(61)
         #undef register_sf4ext
      }

      template<typename T> parser<T>::results_context_t& parser<T>::results_ctx()
      {
         if (0 == results_context_)
         {
            results_context_ = new results_context_t();
         }

         return (*results_context_);
      }

      template<typename T> void parser<T>::return_cleanup()
      {
         if(details::disable_return_statement) 
            return;
         if (results_context_)
         {
            delete results_context_;
            results_context_ = 0;
         }

         state_.return_stmt_present = false;
      }
      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_null_statement()
      {
         next_token();
         return node_allocator_.allocate<details::null_node<T> >();
      }

      template<typename T> bool parser<T>::post_variable_process(const std::string& symbol)
      {
         if (
              peek_token_is(token_t::e_lbracket   ) ||
              peek_token_is(token_t::e_lcrlbracket) ||
              peek_token_is(token_t::e_lsqrbracket)
            )
         {
            if (!settings_.commutative_check_enabled())
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR192 - Invalid sequence of variable '" + symbol + "' and bracket",
                             exprtk_error_location));

               return false;
            }

            lexer().insert_front(token_t::e_mul);
         }

         return true;
      }

      template<typename T> bool parser<T>::post_bracket_process(const typename token_t::token_type& token, expression_node_ptr& branch)
      {
         bool implied_mul = false;

         const lexer::parser_helper::token_advance_mode hold = prsrhlpr_t::e_hold;

         switch (token)
         {
            case token_t::e_lcrlbracket : implied_mul = token_is(token_t::e_lbracket   ,hold) ||
                                                        token_is(token_t::e_lcrlbracket,hold) ||
                                                        token_is(token_t::e_lsqrbracket,hold) ;
                                          break;

            case token_t::e_lbracket    : implied_mul = token_is(token_t::e_lbracket   ,hold) ||
                                                        token_is(token_t::e_lcrlbracket,hold) ||
                                                        token_is(token_t::e_lsqrbracket,hold) ;
                                          break;

            case token_t::e_lsqrbracket : implied_mul = token_is(token_t::e_lbracket   ,hold) ||
                                                        token_is(token_t::e_lcrlbracket,hold) ||
                                                        token_is(token_t::e_lsqrbracket,hold) ;
                                          break;

            default                     : return true;
         }

         if (implied_mul)
         {
            if (!settings_.commutative_check_enabled())
            {
               set_error(
                  make_error(parser_error::e_syntax,
                             current_token(),
                             "ERR193 - Invalid sequence of brackets",
                             exprtk_error_location));

               return false;
            }
            else if (token_t::e_eof != current_token().type)
            {
               lexer().insert_front(current_token().type);
               lexer().insert_front(token_t::e_mul);
               next_token();
            }
         }

         return true;
      }

      template<typename T> parser<T>::interval_t parser<T>::make_memory_range(const T& t)
      {
         const T* begin = reinterpret_cast<const T*>(&t);
         const T* end   = begin + 1;
         return interval_t(begin, end);
      }

      template<typename T> parser<T>::interval_t parser<T>::make_memory_range(const T* begin, const std::size_t size)
      {
         return interval_t(begin, begin + size);
      }

      template<typename T> parser<T>::interval_t parser<T>::make_memory_range(details::char_cptr begin, const std::size_t size)
      {
         return interval_t(begin, begin + size);
      }

      template<typename T> void parser<T>::lodge_immutable_symbol(const lexer::token& token, const interval_t interval)
      {
         immutable_memory_map_.add_interval(interval);
         immutable_symtok_map_[interval] = token;
      }

         template class parser<float>;
         template class parser<double>;
         template class parser<long double>;
         template class parser<std::complex<float>>;
         template class parser<std::complex<double>>;
         template class parser<std::complex<long double>>;
}
