#include "include/Parser.hpp"

namespace Essa::Math{


template<typename T> parser<T>::scope_element::scope_element()
         : name("???")
         , size (std::numeric_limits<std::size_t>::max())
         , index(std::numeric_limits<std::size_t>::max())
         , depth(std::numeric_limits<std::size_t>::max())
         , ref_count(0)
         , ip_index (0)
         , type (e_none)
         , active(false)
         , data     (0)
         , var_node (0)
         , vec_node (0)
         {}

         template<typename T> bool parser<T>::scope_element::operator < (const scope_element& se) const
         {
            if (ip_index < se.ip_index)
               return true;
            else if (ip_index > se.ip_index)
               return false;
            else if (depth < se.depth)
               return true;
            else if (depth > se.depth)
               return false;
            else if (index < se.index)
               return true;
            else if (index > se.index)
               return false;
            else
               return (name < se.name);
         }

         template<typename T> void parser<T>::scope_element::clear()
         {
            name   = "???";
            size   = std::numeric_limits<std::size_t>::max();
            index  = std::numeric_limits<std::size_t>::max();
            depth  = std::numeric_limits<std::size_t>::max();
            type   = e_none;
            active = false;
            ref_count = 0;
            ip_index  = 0;
            data      = 0;
            var_node  = 0;
            vec_node  = 0;
         }

        template<typename T> parser<T>::scope_element_manager::scope_element_manager(parser<T>& p)
         : parser_(p)
         , input_param_cnt_(0)
         {}

        template<typename T> std::size_t parser<T>::scope_element_manager::size() const
         {
            return element_.size();
         }

        template<typename T>  bool parser<T>::scope_element_manager::empty() const
         {
            return element_.empty();
         }

         template<typename T> parser<T>::scope_element& parser<T>::scope_element_manager::get_element(const std::size_t& index)
         {
            if (index < element_.size())
               return element_[index];
            else
               return null_element_;
         }

         template<typename T> parser<T>::scope_element& parser<T>::scope_element_manager::get_element(const std::string& var_name,
                                           const std::size_t index)
         {
            const std::size_t current_depth = parser_.state_.scope_depth;

            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               scope_element& se = element_[i];

               if (se.depth > current_depth)
                  continue;
               else if (
                         details::imatch(se.name, var_name) &&
                         (se.index == index)
                       )
                  return se;
            }

            return null_element_;
         }

         template<typename T> parser<T>::scope_element& parser<T>::scope_element_manager::get_active_element(const std::string& var_name,
                                                  const std::size_t index)
         {
            const std::size_t current_depth = parser_.state_.scope_depth;

            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               scope_element& se = element_[i];

               if (se.depth > current_depth)
                  continue;
               else if (
                         details::imatch(se.name, var_name) &&
                         (se.index == index)                &&
                         (se.active)
                       )
                  return se;
            }

            return null_element_;
         }

         template<typename T> bool parser<T>::scope_element_manager::add_element(const scope_element& se)
         {
            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               scope_element& cse = element_[i];

               if (
                    details::imatch(cse.name, se.name) &&
                    (cse.depth <= se.depth)            &&
                    (cse.index == se.index)            &&
                    (cse.size  == se.size )            &&
                    (cse.type  == se.type )            &&
                    (cse.active)
                  )
                  return false;
            }

            element_.push_back(se);
            std::sort(element_.begin(),element_.end());

            return true;
         }

         template<typename T> void parser<T>::scope_element_manager::deactivate(const std::size_t& scope_depth)
         {
            exprtk_debug(("deactivate() - Scope depth: %d\n",
                          static_cast<int>(parser_.state_.scope_depth)));

            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               scope_element& se = element_[i];

               if (se.active && (se.depth >= scope_depth))
               {
                  exprtk_debug(("deactivate() - element[%02d] '%s'\n",
                                static_cast<int>(i),
                                se.name.c_str()));

                  se.active = false;
               }
            }
         }

         template<typename T> void parser<T>::scope_element_manager::free_element(scope_element& se)
         {
            exprtk_debug(("free_element() - se[%s]\n", se.name.c_str()));

            switch (se.type)
            {
               case scope_element::e_variable   : delete reinterpret_cast<T*>(se.data);
                                                  delete se.var_node;
                                                  break;

               case scope_element::e_vector     : delete[] reinterpret_cast<T*>(se.data);
                                                  delete se.vec_node;
                                                  break;

               case scope_element::e_vecelem    : delete se.var_node;
                                                  break;

               default                          : return;
            }

            se.clear();
         }

         template<typename T> void parser<T>::scope_element_manager::cleanup()
         {
            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               free_element(element_[i]);
            }

            element_.clear();

            input_param_cnt_ = 0;
         }

         template<typename T> std::size_t parser<T>::scope_element_manager::next_ip_index()
         {
            return ++input_param_cnt_;
         }

         template<typename T> parser<T>::expression_node_ptr parser<T>::scope_element_manager::get_variable(const T& v)
         {
            for (std::size_t i = 0; i < element_.size(); ++i)
            {
               scope_element& se = element_[i];

               if (
                    se.active   &&
                    se.var_node &&
                    details::is_variable_node(se.var_node)
                  )
               {
                  variable_node_ptr vn = reinterpret_cast<variable_node_ptr>(se.var_node);

                  if (&(vn->ref()) == (&v))
                  {
                     return se.var_node;
                  }
               }
            }

            return expression_node_ptr(0);
         }
         
         template<typename T> parser<T>::scope_handler::scope_handler(parser<T>& p)
         : parser_(p)
         {
            parser_.state_.scope_depth++;
            #ifdef exprtk_enable_debugging
            const std::string depth(2 * parser_.state_.scope_depth,'-');
            exprtk_debug(("%s> Scope Depth: %02d\n",
                          depth.c_str(),
                          static_cast<int>(parser_.state_.scope_depth)));
            #endif
         }

        template<typename T> parser<T>::scope_handler::~scope_handler()
         {
            parser_.sem_.deactivate(parser_.state_.scope_depth);
            parser_.state_.scope_depth--;
            #ifdef exprtk_enable_debugging
            const std::string depth(2 * parser_.state_.scope_depth,'-');
            exprtk_debug(("<%s Scope Depth: %02d\n",
                          depth.c_str(),
                          static_cast<int>(parser_.state_.scope_depth)));
            #endif
         }

      template<typename T> parser<T>::stack_limit_handler::stack_limit_handler(parser<T>& p)
         : parser_(p)
         , limit_exceeded_(false)
         {
            if (++parser_.state_.stack_depth > parser_.settings_.max_stack_depth_)
            {
               limit_exceeded_ = true;
               parser_.set_error(
                  make_error(parser_error::e_parser,
                     "ERR000 - Current stack depth " + details::to_str(parser_.state_.stack_depth) +
                     " exceeds maximum allowed stack depth of " + details::to_str(parser_.settings_.max_stack_depth_),
                     exprtk_error_location));
            }
         }

        template<typename T> parser<T>::stack_limit_handler::~stack_limit_handler()
         {
            parser_.state_.stack_depth--;
         }

        template<typename T> bool parser<T>::stack_limit_handler::operator!()
         {
            return limit_exceeded_;
         }

         template<typename T> bool parser<T>::symtab_store::empty() const
         {
            return symtab_list_.empty();
         }

         template<typename T> void parser<T>::symtab_store::clear()
         {
            symtab_list_.clear();
         }

         template<typename T> bool parser<T>::symtab_store::valid() const
         {
            if (!empty())
            {
               for (std::size_t i = 0; i < symtab_list_.size(); ++i)
               {
                  if (symtab_list_[i].valid())
                     return true;
               }
            }

            return false;
         }

         template<typename T> bool parser<T>::symtab_store::valid_symbol(const std::string& symbol) const
         {
            if (!symtab_list_.empty())
               return symtab_list_[0].valid_symbol(symbol);
            else
               return false;
         }

         template<typename T> bool parser<T>::symtab_store::valid_function_name(const std::string& symbol) const
         {
            if (!symtab_list_.empty())
               return symtab_list_[0].valid_function(symbol);
            else
               return false;
         }

         template<typename T> parser<T>::symtab_store::variable_context parser<T>::symtab_store::get_variable_context(const std::string& variable_name) const
         {
            variable_context result;
            if (!valid_symbol(variable_name))
               return result;

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
               {
                  continue;
               }

               result.variable = local_data(i)
                                    .variable_store.get(variable_name);
               if (result.variable)
               {
                  result.symbol_table = &symtab_list_[i];
                  break;
               }
            }

            return result;
         }

         template<typename T> parser<T>::symtab_store::variable_ptr parser<T>::symtab_store::get_variable(const std::string& variable_name) const
         {
            if (!valid_symbol(variable_name))
               return reinterpret_cast<variable_ptr>(0);

            variable_ptr result = reinterpret_cast<variable_ptr>(0);

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else
                  result = local_data(i)
                              .variable_store.get(variable_name);

               if (result) break;
            }

            return result;
         }

         template<typename T> parser<T>::symtab_store::variable_ptr parser<T>::symtab_store::get_variable(const T& var_ref) const
         {
            variable_ptr result = reinterpret_cast<variable_ptr>(0);

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else
                  result = local_data(i).variable_store
                              .get_from_varptr(reinterpret_cast<const void*>(&var_ref));

               if (result) break;
            }

            return result;
         }

         template<typename T> parser<T>::symtab_store::vector_context parser<T>::symtab_store::get_vector_context(const std::string& vector_name) const
         {
            vector_context result;
            if (!valid_symbol(vector_name))
               return result;

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
               {
                  continue;
               }

               result.vector_holder = local_data(i).vector_store.get(vector_name);

               if (result.vector_holder)
               {
                  result.symbol_table = &symtab_list_[i];
                  break;
               }
            }

            return result;
         }

         template<typename T> parser<T>::symtab_store::vector_holder_ptr parser<T>::symtab_store::get_vector(const std::string& vector_name) const
         {
            if (!valid_symbol(vector_name))
               return reinterpret_cast<vector_holder_ptr>(0);

            vector_holder_ptr result = reinterpret_cast<vector_holder_ptr>(0);

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else
                  result =
                     local_data(i).vector_store.get(vector_name);

               if (result) break;
            }

            return result;
         }

         template<typename T> bool parser<T>::symtab_store::is_constant_node(const std::string& symbol_name) const
         {
            if (!valid_symbol(symbol_name))
               return false;

            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else if (local_data(i).variable_store.is_constant(symbol_name))
                  return true;
            }

            return false;
         }

         template<typename T> bool parser<T>::symtab_store::symbol_exists(const std::string& symbol) const
         {
            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else if (symtab_list_[i].symbol_exists(symbol))
                  return true;
            }

            return false;
         }

         template<typename T> bool parser<T>::symtab_store::is_variable(const std::string& variable_name) const
         {
            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else if (
                         symtab_list_[i].local_data().variable_store
                           .symbol_exists(variable_name)
                       )
                  return true;
            }

            return false;
         }

         template<typename T> bool parser<T>::symtab_store::is_vector(const std::string& vector_name) const
         {
            for (std::size_t i = 0; i < symtab_list_.size(); ++i)
            {
               if (!symtab_list_[i].valid())
                  continue;
               else if (
                         local_data(i).vector_store
                           .symbol_exists(vector_name)
                       )
                  return true;
            }

            return false;
         }

         template<typename T> std::string parser<T>::symtab_store::get_variable_name(const expression_node_ptr& ptr) const
         {
            return local_data().variable_store.entity_name(ptr);
         }

         template<typename T> std::string parser<T>::symtab_store::get_vector_name(const vector_holder_ptr& ptr) const
         {
            return local_data().vector_store.entity_name(ptr);
         }

         template<typename T> parser<T>::symtab_store::local_data_t& parser<T>::symtab_store::local_data(const std::size_t& index)
         {
            return symtab_list_[index].local_data();
         }

         template<typename T> const parser<T>::symtab_store::local_data_t& parser<T>::symtab_store::local_data(const std::size_t& index) const
         {
            return symtab_list_[index].local_data();
         }

         template<typename T> parser<T>::symbol_table_t& parser<T>::symtab_store::get_symbol_table(const std::size_t& index)
         {
            return symtab_list_[index];
         }

         template<typename T> parser<T>::parser_state::parser_state()
         : type_check_enabled(true)
         {
            reset();
         }

         template<typename T> void parser<T>::parser_state::reset()
         {
            parsing_return_stmt     = false;
            parsing_break_stmt      = false;
            return_stmt_present     = false;
            side_effect_present     = false;
            scope_depth             = 0;
            stack_depth             = 0;
            parsing_loop_stmt_count = 0;
         }

         #ifndef exprtk_enable_debugging
         template<typename T> void parser<T>::parser_state::activate_side_effect(const std::string&)
         #else
         template<typename T> void parser<T>::parser_state::activate_side_effect(const std::string& source)
         #endif
         {
            if (!side_effect_present)
            {
               side_effect_present = true;

               exprtk_debug(("activate_side_effect() - caller: %s\n",source.c_str()));
            }
         }

         template<typename T> bool parser<T>::unknown_symbol_resolver::process(const std::string& /*unknown_symbol*/,
                              usr_symbol_type&   st,
                              T&                 default_value,
                              std::string&       error_message)
         {
            if (e_usrmode_default != mode)
               return false;

            st = e_usr_variable_type;
            default_value = T(0);
            error_message.clear();

            return true;
         }

         template<typename T> bool parser<T>::unknown_symbol_resolver::process(const std::string& /* unknown_symbol */,
                              symbol_table_t&    /* symbol_table   */,
                              std::string&       /* error_message  */)
         {
            return false;
         }

         template<typename T> parser<T>::dependent_entity_collector::dependent_entity_collector(const std::size_t options)
         : options_(options)
         , collect_variables_  ((options_ & e_ct_variables  ) == e_ct_variables  )
         , collect_functions_  ((options_ & e_ct_functions  ) == e_ct_functions  )
         , collect_assignments_((options_ & e_ct_assignments) == e_ct_assignments)
         , return_present_   (false)
         , final_stmt_return_(false)
         {}

         template<typename T> void parser<T>::dependent_entity_collector::clear()
         {
            symbol_name_list_    .clear();
            assignment_name_list_.clear();
            retparam_list_       .clear();
            return_present_    = false;
            final_stmt_return_ = false;
         }

         template<typename T> bool& parser<T>::dependent_entity_collector::collect_variables()
         {
            return collect_variables_;
         }

         template<typename T> bool& parser<T>::dependent_entity_collector::collect_functions()
         {
            return collect_functions_;
         }

         template<typename T> bool& parser<T>::dependent_entity_collector::collect_assignments()
         {
            return collect_assignments_;
         }

         template<typename T> bool parser<T>::dependent_entity_collector::return_present() const
         {
            return return_present_;
         }

         template<typename T> bool parser<T>::dependent_entity_collector::final_stmt_return() const
         {
            return final_stmt_return_;
         }

         template<typename T> parser<T>::dependent_entity_collector::retparam_list_t parser<T>::dependent_entity_collector::return_param_type_list() const
         {
            return retparam_list_;
         }

         template<typename T> void parser<T>::dependent_entity_collector::add_symbol(const std::string& symbol, const symbol_type st)
         {
            switch (st)
            {
               case e_st_variable       :
               case e_st_vector         :
               case e_st_string         :
               case e_st_local_variable :
               case e_st_local_vector   :
               case e_st_local_string   : if (collect_variables_)
                                             symbol_name_list_
                                                .push_back(std::make_pair(symbol, st));
                                          break;

               case e_st_function       : if (collect_functions_)
                                             symbol_name_list_
                                                .push_back(std::make_pair(symbol, st));
                                          break;

               default                  : return;
            }
         }

         template<typename T> void parser<T>::dependent_entity_collector::add_assignment(const std::string& symbol, const symbol_type st)
         {
            switch (st)
            {
               case e_st_variable       :
               case e_st_vector         :
               case e_st_string         : if (collect_assignments_)
                                             assignment_name_list_
                                                .push_back(std::make_pair(symbol, st));
                                          break;

               default                  : return;
            }
         }

         template<typename T> const std::size_t parser<T>::settings_store::compile_all_opts =
                                     e_replacer          +
                                     e_joiner            +
                                     e_numeric_check     +
                                     e_bracket_check     +
                                     e_sequence_check    +
                                     e_commutative_check +
                                     e_strength_reduction;

         template<typename T> parser<T>::settings_store::settings_store(const std::size_t compile_options)
         : max_stack_depth_(400)
         , max_node_depth_(10000)
         {
           load_compile_options(compile_options);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_base_functions()
         {
            disabled_func_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_control_structures()
         {
            disabled_ctrl_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_logic_ops()
         {
            disabled_logic_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_arithmetic_ops()
         {
            disabled_arithmetic_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_assignment_ops()
         {
            disabled_assignment_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_all_inequality_ops()
         {
            disabled_inequality_set_.clear();
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_local_vardef()
         {
            disable_vardef_ = false;
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_base_functions()
         {
            std::copy(details::base_function_list,
                      details::base_function_list + details::base_function_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_func_set_, disabled_func_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_control_structures()
         {
            std::copy(details::cntrl_struct_list,
                      details::cntrl_struct_list + details::cntrl_struct_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_ctrl_set_, disabled_ctrl_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_logic_ops()
         {
            std::copy(details::logic_ops_list,
                      details::logic_ops_list + details::logic_ops_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_logic_set_, disabled_logic_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_arithmetic_ops()
         {
            std::copy(details::arithmetic_ops_list,
                      details::arithmetic_ops_list + details::arithmetic_ops_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_arithmetic_set_, disabled_arithmetic_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_assignment_ops()
         {
            std::copy(details::assignment_ops_list,
                      details::assignment_ops_list + details::assignment_ops_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_assignment_set_, disabled_assignment_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_all_inequality_ops()
         {
            std::copy(details::inequality_ops_list,
                      details::inequality_ops_list + details::inequality_ops_list_size,
                      std::insert_iterator<disabled_entity_set_t>
                        (disabled_inequality_set_, disabled_inequality_set_.begin()));
            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_local_vardef()
         {
            disable_vardef_ = true;
            return (*this);
         }

         template<typename T> bool parser<T>::settings_store::replacer_enabled           () const { return enable_replacer_;           }
         template<typename T> bool parser<T>::settings_store::commutative_check_enabled  () const { return enable_commutative_check_;  }
         template<typename T> bool parser<T>::settings_store::joiner_enabled             () const { return enable_joiner_;             }
         template<typename T> bool parser<T>::settings_store::numeric_check_enabled      () const { return enable_numeric_check_;      }
         template<typename T> bool parser<T>::settings_store::bracket_check_enabled      () const { return enable_bracket_check_;      }
         template<typename T> bool parser<T>::settings_store::sequence_check_enabled     () const { return enable_sequence_check_;     }
         template<typename T> bool parser<T>::settings_store::strength_reduction_enabled () const { return enable_strength_reduction_; }
         template<typename T> bool parser<T>::settings_store::collect_variables_enabled  () const { return enable_collect_vars_;       }
         template<typename T> bool parser<T>::settings_store::collect_functions_enabled  () const { return enable_collect_funcs_;      }
         template<typename T> bool parser<T>::settings_store::collect_assignments_enabled() const { return enable_collect_assings_;    }
         template<typename T> bool parser<T>::settings_store::vardef_disabled            () const { return disable_vardef_;            }
         template<typename T> bool parser<T>::settings_store::rsrvd_sym_usr_disabled     () const { return disable_rsrvd_sym_usr_;     }
         template<typename T> bool parser<T>::settings_store::zero_return_disabled       () const { return disable_zero_return_;       }

         template<typename T> bool parser<T>::settings_store::function_enabled(const std::string& function_name) const
         {
            if (disabled_func_set_.empty())
               return true;
            else
               return (disabled_func_set_.end() == disabled_func_set_.find(function_name));
         }

         template<typename T> bool parser<T>::settings_store::control_struct_enabled(const std::string& control_struct) const
         {
            if (disabled_ctrl_set_.empty())
               return true;
            else
               return (disabled_ctrl_set_.end() == disabled_ctrl_set_.find(control_struct));
         }

         template<typename T> bool parser<T>::settings_store::logic_enabled(const std::string& logic_operation) const
         {
            if (disabled_logic_set_.empty())
               return true;
            else
               return (disabled_logic_set_.end() == disabled_logic_set_.find(logic_operation));
         }

         template<typename T> bool parser<T>::settings_store::arithmetic_enabled(const details::operator_type& arithmetic_operation) const
         {
            if (disabled_logic_set_.empty())
               return true;
            else
               return disabled_arithmetic_set_.end() == disabled_arithmetic_set_
                                                            .find(arith_opr_to_string(arithmetic_operation));
         }

         template<typename T> bool parser<T>::settings_store::assignment_enabled(const details::operator_type& assignment) const
         {
            if (disabled_assignment_set_.empty())
               return true;
            else
               return disabled_assignment_set_.end() == disabled_assignment_set_
                                                           .find(assign_opr_to_string(assignment));
         }

         template<typename T> bool parser<T>::settings_store::inequality_enabled(const details::operator_type& inequality) const
         {
            if (disabled_inequality_set_.empty())
               return true;
            else
               return disabled_inequality_set_.end() == disabled_inequality_set_
                                                           .find(inequality_opr_to_string(inequality));
         }

         template<typename T> bool parser<T>::settings_store::function_disabled(const std::string& function_name) const
         {
            if (disabled_func_set_.empty())
               return false;
            else
               return (disabled_func_set_.end() != disabled_func_set_.find(function_name));
         }

         template<typename T> bool parser<T>::settings_store::control_struct_disabled(const std::string& control_struct) const
         {
            if (disabled_ctrl_set_.empty())
               return false;
            else
               return (disabled_ctrl_set_.end() != disabled_ctrl_set_.find(control_struct));
         }

         template<typename T> bool parser<T>::settings_store::logic_disabled(const std::string& logic_operation) const
         {
            if (disabled_logic_set_.empty())
               return false;
            else
               return (disabled_logic_set_.end() != disabled_logic_set_.find(logic_operation));
         }

         template<typename T> bool parser<T>::settings_store::assignment_disabled(const details::operator_type assignment_operation) const
         {
            if (disabled_assignment_set_.empty())
               return false;
            else
               return disabled_assignment_set_.end() != disabled_assignment_set_
                                                           .find(assign_opr_to_string(assignment_operation));
         }

         template<typename T> bool parser<T>::settings_store::logic_disabled(const details::operator_type logic_operation) const
         {
            if (disabled_logic_set_.empty())
               return false;
            else
               return disabled_logic_set_.end() != disabled_logic_set_
                                                           .find(logic_opr_to_string(logic_operation));
         }

         template<typename T> bool parser<T>::settings_store::arithmetic_disabled(const details::operator_type arithmetic_operation) const
         {
            if (disabled_arithmetic_set_.empty())
               return false;
            else
               return disabled_arithmetic_set_.end() != disabled_arithmetic_set_
                                                           .find(arith_opr_to_string(arithmetic_operation));
         }

         template<typename T> bool parser<T>::settings_store::inequality_disabled(const details::operator_type& inequality) const
         {
            if (disabled_inequality_set_.empty())
               return false;
            else
               return disabled_inequality_set_.end() != disabled_inequality_set_
                                                           .find(inequality_opr_to_string(inequality));
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_base_function(settings_base_funcs bf)
         {
            if (
                 (e_bf_unknown != bf) &&
                 (static_cast<std::size_t>(bf) < (details::base_function_list_size + 1))
               )
            {
               disabled_func_set_.insert(details::base_function_list[bf - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_control_structure(settings_control_structs ctrl_struct)
         {
            if (
                 (e_ctrl_unknown != ctrl_struct) &&
                 (static_cast<std::size_t>(ctrl_struct) < (details::cntrl_struct_list_size + 1))
               )
            {
               disabled_ctrl_set_.insert(details::cntrl_struct_list[ctrl_struct - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_logic_operation(settings_logic_opr logic)
         {
            if (
                 (e_logic_unknown != logic) &&
                 (static_cast<std::size_t>(logic) < (details::logic_ops_list_size + 1))
               )
            {
               disabled_logic_set_.insert(details::logic_ops_list[logic - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_arithmetic_operation(settings_arithmetic_opr arithmetic)
         {
            if (
                 (e_arith_unknown != arithmetic) &&
                 (static_cast<std::size_t>(arithmetic) < (details::arithmetic_ops_list_size + 1))
               )
            {
               disabled_arithmetic_set_.insert(details::arithmetic_ops_list[arithmetic - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_assignment_operation(settings_assignment_opr assignment)
         {
            if (
                 (e_assign_unknown != assignment) &&
                 (static_cast<std::size_t>(assignment) < (details::assignment_ops_list_size + 1))
               )
            {
               disabled_assignment_set_.insert(details::assignment_ops_list[assignment - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::disable_inequality_operation(settings_inequality_opr inequality)
         {
            if (
                 (e_ineq_unknown != inequality) &&
                 (static_cast<std::size_t>(inequality) < (details::inequality_ops_list_size + 1))
               )
            {
               disabled_inequality_set_.insert(details::inequality_ops_list[inequality - 1]);
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_base_function(settings_base_funcs bf)
         {
            if (
                 (e_bf_unknown != bf) &&
                 (static_cast<std::size_t>(bf) < (details::base_function_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_func_set_.find(details::base_function_list[bf - 1]);

               if (disabled_func_set_.end() != itr)
               {
                  disabled_func_set_.erase(itr);
               }
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_control_structure(settings_control_structs ctrl_struct)
         {
            if (
                 (e_ctrl_unknown != ctrl_struct) &&
                 (static_cast<std::size_t>(ctrl_struct) < (details::cntrl_struct_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_ctrl_set_.find(details::cntrl_struct_list[ctrl_struct - 1]);

               if (disabled_ctrl_set_.end() != itr)
               {
                  disabled_ctrl_set_.erase(itr);
               }
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_logic_operation(settings_logic_opr logic)
         {
            if (
                 (e_logic_unknown != logic) &&
                 (static_cast<std::size_t>(logic) < (details::logic_ops_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_logic_set_.find(details::logic_ops_list[logic - 1]);

               if (disabled_logic_set_.end() != itr)
               {
                  disabled_logic_set_.erase(itr);
               }
            }

            return (*this);
         }

         template<typename T> parser<T>::settings_store& parser<T>::settings_store::enable_arithmetic_operation(settings_arithmetic_opr arithmetic)
         {
            if (
                 (e_arith_unknown != arithmetic) &&
                 (static_cast<std::size_t>(arithmetic) < (details::arithmetic_ops_list_size + 1))
               )
            {
               const des_itr_t itr = disabled_arithmetic_set_.find(details::arithmetic_ops_list[arithmetic - 1]);

               if (disabled_arithmetic_set_.end() != itr)
               {
                  disabled_arithmetic_set_.erase(itr);
               }
            }

            return (*this);
         }

         template class parser<float>;
         template class parser<double>;
         template class parser<long double>;
         template class parser<std::complex<float>>;
         template class parser<std::complex<double>>;
         template class parser<std::complex<long double>>;

}
