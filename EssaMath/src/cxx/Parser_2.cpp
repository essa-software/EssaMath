#include "include/Parser.hpp"

namespace Essa::Math{


         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_assignment_operation(settings_assignment_opr assignment)
         {
            if (
                 (e_assign_unknown != assignment) &&
                 (static_cast<std::size_t>(assignment) < (details::assignment_ops_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_assignment_set_.find(details::assignment_ops_list[assignment - 1]);

               if (disabled_assignment_set_.end() != itr)
               {
                  disabled_assignment_set_.erase(itr);
               }
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_inequality_operation(settings_inequality_opr inequality)
         {
            if (
                 (e_ineq_unknown != inequality) &&
                 (static_cast<std::size_t>(inequality) < (details::inequality_ops_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_inequality_set_.find(details::inequality_ops_list[inequality - 1]);

               if (disabled_inequality_set_.end() != itr)
               {
                  disabled_inequality_set_.erase(itr);
               }
            }

            return (*this);
         }

         template<typename T> void parser<T>::settings_store::set_max_stack_depth(const std::size_t max_stack_depth)
         {
            max_stack_depth_ = max_stack_depth;
         }

         template<typename T> void parser<T>::settings_store::set_max_node_depth(const std::size_t max_node_depth)
         {
            max_node_depth_ = max_node_depth;
         }

         template<typename T> void parser<T>::settings_store::load_compile_options(const std::size_t compile_options)
         {
            enable_replacer_           = (compile_options & e_replacer            ) == e_replacer;
            enable_joiner_             = (compile_options & e_joiner              ) == e_joiner;
            enable_numeric_check_      = (compile_options & e_numeric_check       ) == e_numeric_check;
            enable_bracket_check_      = (compile_options & e_bracket_check       ) == e_bracket_check;
            enable_sequence_check_     = (compile_options & e_sequence_check      ) == e_sequence_check;
            enable_commutative_check_  = (compile_options & e_commutative_check   ) == e_commutative_check;
            enable_strength_reduction_ = (compile_options & e_strength_reduction  ) == e_strength_reduction;
            enable_collect_vars_       = (compile_options & e_collect_vars        ) == e_collect_vars;
            enable_collect_funcs_      = (compile_options & e_collect_funcs       ) == e_collect_funcs;
            enable_collect_assings_    = (compile_options & e_collect_assings     ) == e_collect_assings;
            disable_vardef_            = (compile_options & e_disable_vardef      ) == e_disable_vardef;
            disable_rsrvd_sym_usr_     = (compile_options & e_disable_usr_on_rsrvd) == e_disable_usr_on_rsrvd;
            disable_zero_return_       = (compile_options & e_disable_zero_return ) == e_disable_zero_return;
         }

         template<typename T> std::string parser<T>::settings_store::assign_opr_to_string(details::operator_type opr) const
         {
            switch (opr)
            {
               case details::e_assign : return ":=";
               case details::e_addass : return "+=";
               case details::e_subass : return "-=";
               case details::e_mulass : return "*=";
               case details::e_divass : return "/=";
               case details::e_modass : return "%=";
               default                : return ""  ;
            }
         }

         template<typename T> std::string parser<T>::settings_store::arith_opr_to_string(details::operator_type opr) const
         {
            switch (opr)
            {
               case details::e_add : return "+";
               case details::e_sub : return "-";
               case details::e_mul : return "*";
               case details::e_div : return "/";
               case details::e_mod : return "mod";
               default             : return "" ;
            }
         }

         template<typename T> std::string parser<T>::settings_store::inequality_opr_to_string(details::operator_type opr) const
         {
            switch (opr)
            {
               case details::e_lt    : return "<" ;
               case details::e_lte   : return "<=";
               case details::e_eq    : return "==";
               case details::e_equal : return "=" ;
               case details::e_ne    : return "!=";
               case details::e_nequal: return "<>";
               case details::e_gte   : return ">=";
               case details::e_gt    : return ">" ;
               default               : return ""  ;
            }
         }

         template<typename T> std::string parser<T>::settings_store::logic_opr_to_string(details::operator_type opr) const
         {
            switch (opr)
            {
               case details::e_and  : return "and" ;
               case details::e_or   : return "or"  ;
               case details::e_xor  : return "xor" ;
               case details::e_nand : return "nand";
               case details::e_nor  : return "nor" ;
               case details::e_xnor : return "xnor";
               case details::e_notl : return "not" ;
               default              : return ""    ;
            }
         }

      template<typename T> parser<T>::parser(const settings_t& settings)
      : settings_(settings)
      , resolve_unknown_symbol_(false)
      , results_context_(0)
      , unknown_symbol_resolver_(reinterpret_cast<unknown_symbol_resolver*>(0))
        #ifdef _MSC_VER
        #pragma warning(push)
        #pragma warning (disable:4355)
        #endif
      , sem_(*this)
        #ifdef _MSC_VER
        #pragma warning(pop)
        #endif
      , operator_joiner_2_(2)
      , operator_joiner_3_(3)
      , loop_runtime_check_(0)
      {
         init_precompilation();

         load_operations_map           (base_ops_map_     );
         load_unary_operations_map     (unary_op_map_     );
         load_binary_operations_map    (binary_op_map_    );
         load_inv_binary_operations_map(inv_binary_op_map_);
         load_sf3_map                  (sf3_map_          );
         load_sf4_map                  (sf4_map_          );

         expression_generator_.init_synthesize_map();
         expression_generator_.set_parser(*this);
         expression_generator_.set_uom(unary_op_map_);
         expression_generator_.set_bom(binary_op_map_);
         expression_generator_.set_ibom(inv_binary_op_map_);
         expression_generator_.set_sf3m(sf3_map_);
         expression_generator_.set_sf4m(sf4_map_);
         expression_generator_.set_strength_reduction_state(settings_.strength_reduction_enabled());

        settings_.disable_all_assignment_ops();
        settings_.disable_all_control_structures();
        settings_.disable_all_logic_ops();
      }

      template <typename T> void parser<T>::init_precompilation()
      {
         dec_.collect_variables() =
            settings_.collect_variables_enabled();

         dec_.collect_functions() =
            settings_.collect_functions_enabled();

         dec_.collect_assignments() =
            settings_.collect_assignments_enabled();

         if (settings_.replacer_enabled())
         {
            symbol_replacer_.clear();
            symbol_replacer_.add_replace("true" , "1", lexer::token::e_number);
            symbol_replacer_.add_replace("false", "0", lexer::token::e_number);
            helper_assembly_.token_modifier_list.clear();
            helper_assembly_.register_modifier(&symbol_replacer_);
         }

         if (settings_.commutative_check_enabled())
         {
            for (std::size_t i = 0; i < details::reserved_words_size; ++i)
            {
               commutative_inserter_.ignore_symbol(details::reserved_words[i]);
            }

            helper_assembly_.token_inserter_list.clear();
            helper_assembly_.register_inserter(&commutative_inserter_);
         }

         if (settings_.joiner_enabled())
         {
            helper_assembly_.token_joiner_list.clear();
            helper_assembly_.register_joiner(&operator_joiner_2_);
            helper_assembly_.register_joiner(&operator_joiner_3_);
         }

         if (
              settings_.numeric_check_enabled () ||
              settings_.bracket_check_enabled () ||
              settings_.sequence_check_enabled()
            )
         {
            helper_assembly_.token_scanner_list.clear();

            if (settings_.numeric_check_enabled())
            {
               helper_assembly_.register_scanner(&numeric_checker_);
            }

            if (settings_.bracket_check_enabled())
            {
               helper_assembly_.register_scanner(&bracket_checker_);
            }

            if (settings_.sequence_check_enabled())
            {
               helper_assembly_.register_scanner(&sequence_validator_      );
               helper_assembly_.register_scanner(&sequence_validator_3tkns_);
            }
         }
      }

      template<typename T> bool parser<T>::compile(const std::string& expression_string, expression<T>& expr)
      {
         for(size_t i = 0; i < 2; i++){
            if(i == 0){
               details::disable_enhanced_features = false;
               details::disable_cardinal_pow_optimisation = false;
            }else{
               details::disable_enhanced_features = true;
               details::disable_cardinal_pow_optimisation = true;
            }

            state_          .reset();
            error_list_     .clear();
            brkcnt_list_    .clear();
            synthesis_error_.clear();
            sem_            .cleanup();

            return_cleanup();

            expression_generator_.set_allocator(node_allocator_);

            if (expression_string.empty())
            {
               set_error(
                  make_error(parser_error::e_syntax,
                           "ERR001 - Empty expression!",
                           exprtk_error_location));

               return false;
            }

            if (!init(expression_string))
            {
               process_lexer_errors();
               return false;
            }

            if (lexer().empty())
            {
               set_error(
                  make_error(parser_error::e_syntax,
                           "ERR002 - Empty expression!",
                           exprtk_error_location));

               return false;
            }

            if (!run_assemblies())
            {
               return false;
            }

            symtab_store_.symtab_list_ = expr.get_symbol_table_list();
            dec_.clear();

            lexer().begin();

            next_token();

            expression_node_ptr e = parse_corpus();

            if ((0 != e) && (token_t::e_eof == current_token().type))
            {
               bool* retinvk_ptr = 0;

               if (state_.return_stmt_present)
               {
                  
               }

               if(i == 0){
                  expr.set_expression(e);
                  expr.set_retinvk(retinvk_ptr);
               }else{
                  expr.set_unoptimized_expr(e);
               }
            }
            else
            {
               if (error_list_.empty())
               {
                  set_error(
                     make_error(parser_error::e_syntax,
                              current_token(),
                              "ERR003 - Invalid expression encountered",
                              exprtk_error_location));
               }

               if ((0 != e) && branch_deletable(e))
               {
                  destroy_node(e);
               }

               dec_.clear    ();
               sem_.cleanup  ();
               return_cleanup();

               return false;
            }
         }
         
         register_local_vars(expr);
         register_return_results(expr);
         details::disable_enhanced_features = false;
         details::disable_cardinal_pow_optimisation = false;
         return !(!expr);
      }

      template<typename T> parser<T>::expression_t parser<T>::compile(const std::string& expression_string, parser<T>::symbol_table_t& symtab)
      {
         expression_t expression;
         expression.register_symbol_table(symtab);
         compile(expression_string,expression);
         return expression;
      }

      template<typename T> void parser<T>::process_lexer_errors()
      {
         for (std::size_t i = 0; i < lexer().size(); ++i)
         {
            if (lexer()[i].is_error())
            {
               std::string diagnostic = "ERR004 - ";

               switch (lexer()[i].type)
               {
                  case lexer::token::e_error      : diagnostic += "General token error";
                                                    break;

                  case lexer::token::e_err_symbol : diagnostic += "Symbol error";
                                                    break;

                  case lexer::token::e_err_number : diagnostic += "Invalid numeric token";
                                                    break;

                  case lexer::token::e_err_string : diagnostic += "Invalid string token";
                                                    break;

                  case lexer::token::e_err_sfunc  : diagnostic += "Invalid special function token";
                                                    break;

                  default                         : diagnostic += "Unknown compiler error";
               }

               set_error(
                  make_error(parser_error::e_lexer,
                             lexer()[i],
                             diagnostic + ": " + lexer()[i].value,
                             exprtk_error_location));
            }
         }
      }

      template<typename T> bool parser<T>::run_assemblies()
      {
         if (settings_.commutative_check_enabled())
         {
            helper_assembly_.run_inserters(lexer());
         }

         if (settings_.joiner_enabled())
         {
            helper_assembly_.run_joiners(lexer());
         }

         if (settings_.replacer_enabled())
         {
            helper_assembly_.run_modifiers(lexer());
         }

         if (
              settings_.numeric_check_enabled () ||
              settings_.bracket_check_enabled () ||
              settings_.sequence_check_enabled()
            )
         {
            if (!helper_assembly_.run_scanners(lexer()))
            {
               if (helper_assembly_.error_token_scanner)
               {
                  lexer::helper::bracket_checker*            bracket_checker_ptr     = 0;
                  lexer::helper::numeric_checker<T>*         numeric_checker_ptr     = 0;
                  lexer::helper::sequence_validator*         sequence_validator_ptr  = 0;
                  lexer::helper::sequence_validator_3tokens* sequence_validator3_ptr = 0;

                  if (0 != (bracket_checker_ptr = dynamic_cast<lexer::helper::bracket_checker*>(helper_assembly_.error_token_scanner)))
                  {
                     set_error(
                        make_error(parser_error::e_token,
                                   bracket_checker_ptr->error_token(),
                                   "ERR005 - Mismatched brackets: '" + bracket_checker_ptr->error_token().value + "'",
                                   exprtk_error_location));
                  }
                  else if (0 != (numeric_checker_ptr = dynamic_cast<lexer::helper::numeric_checker<T>*>(helper_assembly_.error_token_scanner)))
                  {
                     for (std::size_t i = 0; i < numeric_checker_ptr->error_count(); ++i)
                     {
                        lexer::token error_token = lexer()[numeric_checker_ptr->error_index(i)];

                        set_error(
                           make_error(parser_error::e_token,
                                      error_token,
                                      "ERR006 - Invalid numeric token: '" + error_token.value + "'",
                                      exprtk_error_location));
                     }

                     if (numeric_checker_ptr->error_count())
                     {
                        numeric_checker_ptr->clear_errors();
                     }
                  }
                  else if (0 != (sequence_validator_ptr = dynamic_cast<lexer::helper::sequence_validator*>(helper_assembly_.error_token_scanner)))
                  {
                     for (std::size_t i = 0; i < sequence_validator_ptr->error_count(); ++i)
                     {
                        std::pair<lexer::token,lexer::token> error_token = sequence_validator_ptr->error(i);

                        set_error(
                           make_error(parser_error::e_token,
                                      error_token.first,
                                      "ERR007 - Invalid token sequence: '" +
                                      error_token.first.value  + "' and '" +
                                      error_token.second.value + "'",
                                      exprtk_error_location));
                     }

                     if (sequence_validator_ptr->error_count())
                     {
                        sequence_validator_ptr->clear_errors();
                     }
                  }
                  else if (0 != (sequence_validator3_ptr = dynamic_cast<lexer::helper::sequence_validator_3tokens*>(helper_assembly_.error_token_scanner)))
                  {
                     for (std::size_t i = 0; i < sequence_validator3_ptr->error_count(); ++i)
                     {
                        std::pair<lexer::token,lexer::token> error_token = sequence_validator3_ptr->error(i);

                        set_error(
                           make_error(parser_error::e_token,
                                      error_token.first,
                                      "ERR008 - Invalid token sequence: '" +
                                      error_token.first.value  + "' and '" +
                                      error_token.second.value + "'",
                                      exprtk_error_location));
                     }

                     if (sequence_validator3_ptr->error_count())
                     {
                        sequence_validator3_ptr->clear_errors();
                     }
                  }
               }

               return false;
            }
         }

         return true;
      }

      // settings_store& settings()
      // {
      //    return settings_;
      // }

      template<typename T> parser_error::type parser<T>::get_error(const std::size_t& index) const
      {
         if (index < error_list_.size())
            return error_list_[index];
         else
            throw std::invalid_argument("parser::get_error() - Invalid error index specificed");
      }

      template <typename T> std::string parser<T>::error() const
      {
         if (!error_list_.empty())
         {
            return error_list_[0].diagnostic;
         }
         else
            return std::string("No Error");
      }

      template<typename T> std::size_t parser<T>::error_count() const
      {
         return error_list_.size();
      }

      template<typename T> parser<T>::dependent_entity_collector& parser<T>::dec()
      {
         return dec_;
      }

      template<typename T> bool parser<T>::replace_symbol(const std::string& old_symbol, const std::string& new_symbol)
      {
         if (!settings_.replacer_enabled())
            return false;
         else if (details::is_reserved_word(old_symbol))
            return false;
         else
            return symbol_replacer_.add_replace(old_symbol,new_symbol,lexer::token::e_symbol);
      }

      template<typename T> bool parser<T>::remove_replace_symbol(const std::string& symbol)
      {
         if (!settings_.replacer_enabled())
            return false;
         else if (details::is_reserved_word(symbol))
            return false;
         else
            return symbol_replacer_.remove(symbol);
      }

      template<typename T> void parser<T>::enable_unknown_symbol_resolver(unknown_symbol_resolver* usr)
      {
         resolve_unknown_symbol_ = true;

         if (usr)
            unknown_symbol_resolver_ = usr;
         else
            unknown_symbol_resolver_ = &default_usr_;
      }

      template<typename T> void parser<T>::enable_unknown_symbol_resolver(unknown_symbol_resolver& usr)
      {
         enable_unknown_symbol_resolver(&usr);
      }

      template<typename T> void parser<T>::disable_unknown_symbol_resolver()
      {
         resolve_unknown_symbol_  = false;
         unknown_symbol_resolver_ = &default_usr_;
      }

      template<typename T> void parser<T>::register_loop_runtime_check(loop_runtime_check& lrtchk)
      {
         loop_runtime_check_ = &lrtchk;
      }

      template<typename T> void parser<T>::clear_loop_runtime_check()
      {
         loop_runtime_check_ = loop_runtime_check_ptr(0);
      }

      template <typename T> bool parser<T>::valid_base_operation(const std::string& symbol) const
      {
         const std::size_t length = symbol.size();

         if (
              (length < 3) || // Shortest base op symbol length
              (length > 9)    // Longest base op symbol length
            )
            return false;
         else
            return settings_.function_enabled(symbol) &&
                   (base_ops_map_.end() != base_ops_map_.find(symbol));
      }

      template <typename T> bool parser<T>::valid_vararg_operation(const std::string& symbol) const
      {
         static const std::string s_sum     = "sum" ;
         static const std::string s_mul     = "mul" ;
         static const std::string s_avg     = "avg" ;
         static const std::string s_min     = "min" ;
         static const std::string s_max     = "max" ;
         static const std::string s_mand    = "mand";
         static const std::string s_mor     = "mor" ;
         static const std::string s_multi   = "~"   ;
         static const std::string s_mswitch = "[*]" ;

         return
               (
                  details::imatch(symbol,s_sum    ) ||
                  details::imatch(symbol,s_mul    ) ||
                  details::imatch(symbol,s_avg    ) ||
                  details::imatch(symbol,s_min    ) ||
                  details::imatch(symbol,s_max    ) ||
                  details::imatch(symbol,s_mand   ) ||
                  details::imatch(symbol,s_mor    ) ||
                  details::imatch(symbol,s_multi  ) ||
                  details::imatch(symbol,s_mswitch)
               ) &&
               settings_.function_enabled(symbol);
      }

      template <typename T> bool parser<T>::is_invalid_logic_operation(const details::operator_type operation) const
      {
         return settings_.logic_disabled(operation);
      }

      template <typename T> bool parser<T>::is_invalid_arithmetic_operation(const details::operator_type operation) const
      {
         return settings_.arithmetic_disabled(operation);
      }

      template <typename T> bool parser<T>::is_invalid_assignment_operation(const details::operator_type operation) const
      {
         return settings_.assignment_disabled(operation);
      }

      template <typename T> bool parser<T>::is_invalid_inequality_operation(const details::operator_type operation) const
      {
         return settings_.inequality_disabled(operation);
      }

      #ifdef exprtk_enable_debugging
      template <typename T> void parser<T>::next_token()
      {
         const std::string ct_str = current_token().value;
         const std::size_t ct_pos = current_token().position;
         parser_helper::next_token();
         const std::string depth(2 * state_.scope_depth,' ');
         exprtk_debug(("%s"
                       "prev[%s | %04d] --> curr[%s | %04d]  stack_level: %3d\n",
                       depth.c_str(),
                       ct_str.c_str(),
                       static_cast<unsigned int>(ct_pos),
                       current_token().value.c_str(),
                       static_cast<unsigned int>(current_token().position),
                       static_cast<unsigned int>(state_.stack_depth)));
      }
      #endif

      template <typename T> parser<T>::expression_node_ptr parser<T>::parse_corpus()
      {
         std::vector<expression_node_ptr> arg_list;
         std::vector<bool> side_effect_list;

         scoped_vec_delete<expression_node_t> sdd((*this),arg_list);

         lexer::token begin_token;
         lexer::token end_token;

         for ( ; ; )
         {
            state_.side_effect_present = false;

            begin_token = current_token();

            expression_node_ptr arg = parse_expression();

            if (0 == arg)
            {
               if (error_list_.empty())
               {
                  set_error(
                     make_error(parser_error::e_syntax,
                                current_token(),
                                "ERR009 - Invalid expression encountered",
                                exprtk_error_location));
               }

               return error_node();
            }
            else
            {
               arg_list.push_back(arg);

               side_effect_list.push_back(state_.side_effect_present);

               end_token = current_token();

               const std::string sub_expr = construct_subexpr(begin_token, end_token);

               exprtk_debug(("parse_corpus(%02d) Subexpr: %s\n",
                             static_cast<int>(arg_list.size() - 1),
                             sub_expr.c_str()));

               exprtk_debug(("parse_corpus(%02d) - Side effect present: %s\n",
                             static_cast<int>(arg_list.size() - 1),
                             state_.side_effect_present ? "true" : "false"));

               exprtk_debug(("-------------------------------------------------\n"));
            }

            if (lexer().finished())
               break;
            else if (token_is(token_t::e_eof,prsrhlpr_t::e_hold))
            {
               if (lexer().finished())
                  break;
               else
                  next_token();
            }
         }

         if (
              !arg_list.empty() &&
              is_return_node(arg_list.back())
            )
         {
            dec_.final_stmt_return_ = true;
         }

         const expression_node_ptr result = simplify(arg_list,side_effect_list);

         sdd.delete_ptr = (0 == result);

         return result;
      }

      template <typename T> std::string parser<T>::construct_subexpr(lexer::token& begin_token, lexer::token& end_token)
      {
         std::string result = lexer().substr(begin_token.position,end_token.position);

         for (std::size_t i = 0; i < result.size(); ++i)
         {
            if (details::is_whitespace(result[i])) result[i] = ' ';
         }

         return result;
      }

      template <typename T> const parser<T>::precedence_level parser<T>::default_precedence = e_level00;

         template <typename T> void parser<T>::state_t::set(const precedence_level& l,
                         const precedence_level& r,
                         const details::operator_type& o)
         {
            left  = l;
            right = r;
            operation = o;
         }

         template <typename T> void parser<T>::state_t::reset()
         {
            left      = e_level00;
            right     = e_level00;
            operation = details::e_default;
         }

      template <typename T> parser<T>::expression_node_ptr parser<T>::parse_expression(precedence_level precedence)
      {
         stack_limit_handler slh(*this);

         if (!slh)
         {
            return error_node();
         }

         expression_node_ptr expression = parse_branch(precedence);

         if (0 == expression)
         {
            return error_node();
         }

         bool break_loop = false;

         state_t current_state;

         for ( ; ; )
         {
            current_state.reset();

            switch (current_token().type)
            {
               case token_t::e_assign : current_state.set(e_level00, e_level00, details::e_assign); break;
               case token_t::e_addass : current_state.set(e_level00, e_level00, details::e_addass); break;
               case token_t::e_subass : current_state.set(e_level00, e_level00, details::e_subass); break;
               case token_t::e_mulass : current_state.set(e_level00, e_level00, details::e_mulass); break;
               case token_t::e_divass : current_state.set(e_level00, e_level00, details::e_divass); break;
               case token_t::e_modass : current_state.set(e_level00, e_level00, details::e_modass); break;
               case token_t::e_swap   : current_state.set(e_level00, e_level00, details::e_swap  ); break;
               case token_t::e_lt     : current_state.set(e_level05, e_level06, details::e_lt    ); break;
               case token_t::e_lte    : current_state.set(e_level05, e_level06, details::e_lte   ); break;
               case token_t::e_eq     : current_state.set(e_level05, e_level06, details::e_eq    ); break;
               case token_t::e_ne     : current_state.set(e_level05, e_level06, details::e_ne    ); break;
               case token_t::e_gte    : current_state.set(e_level05, e_level06, details::e_gte   ); break;
               case token_t::e_gt     : current_state.set(e_level05, e_level06, details::e_gt    ); break;
               case token_t::e_add    : current_state.set(e_level07, e_level08, details::e_add   ); break;
               case token_t::e_sub    : current_state.set(e_level07, e_level08, details::e_sub   ); break;
               case token_t::e_div    : current_state.set(e_level10, e_level11, details::e_div   ); break;
               case token_t::e_mul    : current_state.set(e_level10, e_level11, details::e_mul   ); break;
               case token_t::e_pow    : current_state.set(e_level12, e_level12, details::e_pow   ); break;
               default                : if (token_t::e_symbol == current_token().type)
                                        {
                                           static const std::string s_and   = "and"  ;
                                           static const std::string s_nand  = "nand" ;
                                           static const std::string s_or    = "or"   ;
                                           static const std::string s_nor   = "nor"  ;
                                           static const std::string s_xor   = "xor"  ;
                                           static const std::string s_xnor  = "xnor" ;
                                           static const std::string s_in    = "in"   ;
                                           static const std::string s_like  = "like" ;
                                           static const std::string s_ilike = "ilike";
                                           static const std::string s_and1  = "&"    ;
                                           static const std::string s_or1   = "|"    ;
                                           static const std::string s_not   = "not"  ;

                                           if (details::imatch(current_token().value,s_and))
                                           {
                                              current_state.set(e_level03, e_level04, details::e_and);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_and1))
                                           {
                                              if(!details::disable_sc_andor)
                                                current_state.set(e_level03, e_level04, details::e_scand);
                                              else
                                                current_state.set(e_level03, e_level04, details::e_and);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_nand))
                                           {
                                              current_state.set(e_level03, e_level04, details::e_nand);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_or))
                                           {
                                              current_state.set(e_level01, e_level02, details::e_or);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_or1))
                                           {
                                              if(!details::disable_sc_andor)
                                                current_state.set(e_level01, e_level02, details::e_scor);
                                              else
                                                current_state.set(e_level01, e_level02, details::e_or);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_nor))
                                           {
                                              current_state.set(e_level01, e_level02, details::e_nor);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_xor))
                                           {
                                              current_state.set(e_level01, e_level02, details::e_xor);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_xnor))
                                           {
                                              current_state.set(e_level01, e_level02, details::e_xnor);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_in))
                                           {
                                              current_state.set(e_level04, e_level04, details::e_in);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_like))
                                           {
                                              current_state.set(e_level04, e_level04, details::e_like);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_ilike))
                                           {
                                              current_state.set(e_level04, e_level04, details::e_ilike);
                                              break;
                                           }
                                           else if (details::imatch(current_token().value,s_not))
                                           {
                                              break;
                                           }
                                        }

                                        break_loop = true;
            }

            if (break_loop)
            {
               break;
            }
            else if (current_state.left < precedence)
               break;

            const lexer::token prev_token = current_token();

            next_token();

            expression_node_ptr right_branch   = error_node();
            expression_node_ptr new_expression = error_node();

            if (is_invalid_logic_operation(current_state.operation))
            {
               free_node(node_allocator_,expression);

               set_error(
                  make_error(parser_error::e_syntax,
                             prev_token,
                             "ERR010 - Invalid or disabled logic operation '" + details::to_str(current_state.operation) + "'",
                             exprtk_error_location));

               return error_node();
            }
            else if (is_invalid_arithmetic_operation(current_state.operation))
            {
               free_node(node_allocator_,expression);

               set_error(
                  make_error(parser_error::e_syntax,
                             prev_token,
                             "ERR011 - Invalid or disabled arithmetic operation '" + details::to_str(current_state.operation) + "'",
                             exprtk_error_location));

               return error_node();
            }
            else if (is_invalid_inequality_operation(current_state.operation))
            {
               free_node(node_allocator_,expression);

               set_error(
                  make_error(parser_error::e_syntax,
                             prev_token,
                             "ERR012 - Invalid inequality operation '" + details::to_str(current_state.operation) + "'",
                             exprtk_error_location));

               return error_node();
            }
            else if (is_invalid_assignment_operation(current_state.operation))
            {
               free_node(node_allocator_,expression);

               set_error(
                  make_error(parser_error::e_syntax,
                             prev_token,
                             "ERR013 - Invalid or disabled assignment operation '" + details::to_str(current_state.operation) + "'",
                             exprtk_error_location));

               return error_node();
            }

            if (0 != (right_branch = parse_expression(current_state.right)))
            {
               if (
                    details::is_return_node(expression  ) ||
                    details::is_return_node(right_branch)
                  )
               {
                  free_node(node_allocator_, expression  );
                  free_node(node_allocator_, right_branch);

                  set_error(
                     make_error(parser_error::e_syntax,
                                prev_token,
                                "ERR014 - Return statements cannot be part of sub-expressions",
                                exprtk_error_location));

                  return error_node();
               }

               new_expression = expression_generator_
                                  (
                                    current_state.operation,
                                    expression,
                                    right_branch
                                  );
            }

            if (0 == new_expression)
            {
               if (error_list_.empty())
               {
                  set_error(
                     make_error(parser_error::e_syntax,
                                prev_token,
                                !synthesis_error_.empty() ?
                                synthesis_error_ :
                                "ERR015 - General parsing error at token: '" + prev_token.value + "'",
                                exprtk_error_location));
               }

               free_node(node_allocator_, expression  );
               free_node(node_allocator_, right_branch);

               return error_node();
            }
            else
            {
               expression = new_expression;
            }
         }

         if ((0 != expression) && (expression->node_depth() > settings_.max_node_depth_))
         {
            set_error(
               make_error(parser_error::e_syntax,
                  current_token(),
                  "ERR016 - Expression depth of " + details::to_str(static_cast<int>(expression->node_depth())) +
                  " exceeds maximum allowed expression depth of " + details::to_str(static_cast<int>(settings_.max_node_depth_)),
                  exprtk_error_location));

            free_node(node_allocator_,expression);

            return error_node();
         }

         return expression;
      }

      template <typename T> bool parser<T>::simplify_unary_negation_branch(expression_node_ptr& node)
      {
         {
            typedef details::unary_branch_node<T,details::neg_op<T> > ubn_t;
            ubn_t* n = dynamic_cast<ubn_t*>(node);

            if (n)
            {
               expression_node_ptr un_r = n->branch(0);
               n->release();
               free_node(node_allocator_,node);
               node = un_r;

               return true;
            }
         }

         {
            typedef details::unary_variable_node<T,details::neg_op<T> > uvn_t;

            uvn_t* n = dynamic_cast<uvn_t*>(node);

            if (n)
            {
               const T& v = n->v();
               expression_node_ptr return_node = error_node();

               if (
                    (0 != (return_node = symtab_store_.get_variable(v))) ||
                    (0 != (return_node = sem_         .get_variable(v)))
                  )
               {
                  free_node(node_allocator_,node);
                  node = return_node;

                  return true;
               }
               else
               {
                  set_error(
                     make_error(parser_error::e_syntax,
                                current_token(),
                                "ERR017 - Failed to find variable node in symbol table",
                                exprtk_error_location));

                  free_node(node_allocator_,node);

                  return false;
               }
            }
         }

         return false;
      }

      template <typename T> parser<T>::expression_node_ptr parser<T>::error_node()
      {
         return reinterpret_cast<expression_node_ptr>(0);
      }

        template <typename T>  parser<T>::scoped_expression_delete::scoped_expression_delete(parser<T>& pr, expression_node_ptr& expression)
         : delete_ptr(true)
         , parser_(pr)
         , expression_(expression)
         {}

        template <typename T> parser<T>::scoped_expression_delete::~scoped_expression_delete()
         {
            if (delete_ptr)
            {
               free_node(parser_.node_allocator_, expression_);
            }
         }

        template <typename T> parser<T>::scoped_bool_negator::scoped_bool_negator(bool& bb)
         : b(bb)
         { b = !b; }

        template <typename T> parser<T>::scoped_bool_negator::~scoped_bool_negator()
         { b = !b; }

        
        template <typename T> parser<T>::scoped_bool_or_restorer::scoped_bool_or_restorer(bool& bb)
         : b(bb)
         , original_value_(bb)
         {}

        template <typename T> parser<T>::scoped_bool_or_restorer::~scoped_bool_or_restorer()
         {
            b = b || original_value_;
         }

        template <typename T> parser<T>::scoped_inc_dec::scoped_inc_dec(std::size_t& v)
         : v_(v)
         { ++v_; }

        template <typename T> parser<T>::scoped_inc_dec::~scoped_inc_dec()
         {
           assert(v_ > 0);
           --v_;
         }

      template<typename T> parser<T>::expression_node_ptr parser<T>::parse_base_operation()
      {
         typedef std::pair<base_ops_map_t::iterator,base_ops_map_t::iterator> map_range_t;

         const std::string operation_name   = current_token().value;
         const token_t     diagnostic_token = current_token();

         map_range_t itr_range = base_ops_map_.equal_range(operation_name);

         if (0 == std::distance(itr_range.first,itr_range.second))
         {
            set_error(
               make_error(parser_error::e_syntax,
                          diagnostic_token,
                          "ERR030 - No entry found for base operation: " + operation_name,
                          exprtk_error_location));

            return error_node();
         }

         static const std::size_t MaxNumberofParameters = 4;
         expression_node_ptr param_list[MaxNumberofParameters] = {0};

         const std::size_t parameter_count = parse_base_function_call(param_list, operation_name);

         if ((parameter_count > 0) && (parameter_count <= MaxNumberofParameters))
         {
            for (base_ops_map_t::iterator itr = itr_range.first; itr != itr_range.second; ++itr)
            {
               const details::base_operation_t& operation = itr->second;

               if (operation.num_params == parameter_count)
               {
                  switch (parameter_count)
                  {
                     #define base_opr_case(N)                                         \
                     case N : {                                                       \
                                 expression_node_ptr pl##N[N] = {0};                  \
                                 std::copy(param_list, param_list + N, pl##N);        \
                                 lodge_symbol(operation_name, e_st_function);         \
                                 return expression_generator_(operation.type, pl##N); \
                              }                                                       \

                     base_opr_case(1)
                     base_opr_case(2)
                     base_opr_case(3)
                     base_opr_case(4)
                     #undef base_opr_case
                  }
               }
            }
         }

         for (std::size_t i = 0; i < MaxNumberofParameters; ++i)
         {
            free_node(node_allocator_, param_list[i]);
         }

         set_error(
            make_error(parser_error::e_syntax,
                       diagnostic_token,
                       "ERR031 - Invalid number of input parameters for call to function: '" + operation_name + "'",
                       exprtk_error_location));

         return error_node();
      }

         template class parser<float>;
         template class parser<double>;
         template class parser<long double>;
         template class parser<std::complex<float>>;
         template class parser<std::complex<double>>;
         template class parser<std::complex<long double>>;

}
