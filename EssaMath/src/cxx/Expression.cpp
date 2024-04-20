#include "include/Expression.hpp"


namespace Essa::Math{

         template<typename T> expression<T>::control_block::control_block()
         : ref_count(0)
         , expr     (0)
         , results  (0)
         , retinv_null(false)
         , return_invoked(&retinv_null)
         {}

         template<typename T> expression<T>::control_block::control_block(expression_ptr e)
         : ref_count(1)
         , expr     (e)
         , results  (0)
         , retinv_null(false)
         , return_invoked(&retinv_null)
         {}

        template<typename T> expression<T>::control_block::~control_block()
         {
            if (expr && details::branch_deletable(expr))
            {
               destroy_node(expr);
            }

            if (!local_data_list.empty())
            {
               for (std::size_t i = 0; i < local_data_list.size(); ++i)
               {
                  switch (local_data_list[i].type)
                  {
                     case e_expr      : delete reinterpret_cast<expression_ptr>(local_data_list[i].pointer);
                                        break;

                     case e_vecholder : delete reinterpret_cast<vector_holder_ptr>(local_data_list[i].pointer);
                                        break;

                     case e_data      : delete reinterpret_cast<T*>(local_data_list[i].pointer);
                                        break;

                     case e_vecdata   : delete [] reinterpret_cast<T*>(local_data_list[i].pointer);
                                        break;
                     default          : break;
                  }
               }
            }

            if (results)
            {
               delete results;
            }
         }

         template<typename T> expression<T>::control_block::cntrl_blck_ptr_t expression<T>::control_block::create(expression_ptr e)
         {
            return new control_block(e);
         }

         template<typename T> void expression<T>::control_block::destroy(cntrl_blck_ptr_t& cntrl_blck)
         {
            if (cntrl_blck)
            {
               if (
                    (0 !=   cntrl_blck->ref_count) &&
                    (0 == --cntrl_blck->ref_count)
                  )
               {
                  delete cntrl_blck;
               }

               cntrl_blck = 0;
            }
         }

      template<typename T> expression<T>::expression()
      : control_block_(0)
      {
         set_expression(new details::null_node<T>());
      }

      template<typename T> expression<T>::expression(const expression<T>& e)
      : control_block_    (e.control_block_    )
      , symbol_table_list_(e.symbol_table_list_)
      {
         control_block_->ref_count++;
      }

      template<typename T> expression<T>::expression(const symbol_table<T>& symbol_table)
      : control_block_(0)
      {
         set_expression(new details::null_node<T>());
         symbol_table_list_.push_back(symbol_table);
      }

      template<typename T> expression<T>& expression<T>::operator=(const expression<T>& e)
      {
         if (this != &e)
         {
            if (control_block_)
            {
               if (
                    (0 !=   control_block_->ref_count) &&
                    (0 == --control_block_->ref_count)
                  )
               {
                  delete control_block_;
               }

               control_block_ = 0;
            }

            control_block_ = e.control_block_;
            control_block_->ref_count++;
            symbol_table_list_ = e.symbol_table_list_;
         }

         return *this;
      }

      template<typename T> std::string expression<T>::to_string() const{
         assert(control_block_      );
         assert(control_block_->unoptimized_expr);

         auto& typ = typeid(control_block_->expr);

         return control_block_->unoptimized_expr->to_string();
      }

      template<typename T> bool expression<T>::operator==(const expression<T>& e) const
      {
         return (this == &e);
      }

      template<typename T> bool expression<T>::operator!() const
      {
         return (
                  (0 == control_block_      ) ||
                  (0 == control_block_->expr)
                );
      }

      template<typename T> expression<T>& expression<T>::release()
      {
         Essa::Math::details::dump_ptr("expression::release", this);
         control_block::destroy(control_block_);

         return (*this);
      }

     template<typename T> expression<T>::~expression()
      {
         control_block::destroy(control_block_);
      }

      template<typename T> T expression<T>::value() const
      {
         assert(control_block_      );
         assert(control_block_->expr);

         return control_block_->expr->value();
      }

      template<typename T> T expression<T>::operator() () const
      {
         return value();
      }

      template<typename T> expression<T>::operator T() const
      {
         return value();
      }

      template<typename T> expression<T>::operator bool() const
      {
         return details::is_true(value());
      }

      template<typename T> void expression<T>::register_symbol_table(symbol_table<T>& st)
      {
         for (std::size_t i = 0; i < symbol_table_list_.size(); ++i)
         {
            if (&st == &symbol_table_list_[i])
            {
               return;
            }
         }

         symbol_table_list_.push_back(st);
      }

      template<typename T> const symbol_table<T>& expression<T>::get_symbol_table(const std::size_t& index) const
      {
         return symbol_table_list_[index];
      }

      template<typename T> symbol_table<T>& expression<T>::get_symbol_table(const std::size_t& index)
      {
         return symbol_table_list_[index];
      }

      template<typename T> const expression<T>::results_context_t& expression<T>::results() const
      {
         if (control_block_->results)
            return (*control_block_->results);
         else
         {
            return null_results;
         }
      }

      template<typename T> bool expression<T>::return_invoked() const
      {
         return (*control_block_->return_invoked);
      }
      
      template<typename T> expression<T>::symtab_list_t expression<T>::get_symbol_table_list() const
      {
         return symbol_table_list_;
      }

      template<typename T> void expression<T>::set_expression(const expression_ptr expr)
      {
         if (expr)
         {
            if (control_block_)
            {
               if (0 == --control_block_->ref_count)
               {
                  delete control_block_;
               }
            }

            control_block_ = control_block::create(expr);
         }
      }

      template<typename T> void expression<T>::set_unoptimized_expr(const expression_ptr expr)
      {
         if (expr)
         {
            if (!control_block_)
            {
               control_block_ = control_block::create(expr);
            }

            control_block_->unoptimized_expr = expr;
         }
      }

      template<typename T> void expression<T>::register_local_var(expression_ptr expr)
      {
         if (expr)
         {
            if (control_block_)
            {
               control_block_->
                  local_data_list.push_back(
                     typename expression<T>::control_block::
                        data_pack(reinterpret_cast<void*>(expr),
                                  control_block::e_expr));
            }
         }
      }

      template<typename T> void expression<T>::register_local_var(vector_holder_ptr vec_holder)
      {
         if (vec_holder)
         {
            if (control_block_)
            {
               control_block_->
                  local_data_list.push_back(
                     typename expression<T>::control_block::
                        data_pack(reinterpret_cast<void*>(vec_holder),
                                  control_block::e_vecholder));
            }
         }
      }

      template<typename T> void expression<T>::register_local_data(void* data, const std::size_t& size, const std::size_t data_mode)
      {
         if (data)
         {
            if (control_block_)
            {
               typename control_block::data_type dt = control_block::e_data;

               switch (data_mode)
               {
                  case 0 : dt = control_block::e_data;    break;
                  case 1 : dt = control_block::e_vecdata; break;
               }

               control_block_->
                  local_data_list.push_back(
                     typename expression<T>::control_block::
                        data_pack(reinterpret_cast<void*>(data), dt, size));
            }
         }
      }

      template<typename T> const typename expression<T>::control_block::local_data_list_t& expression<T>::local_data_list()
      {
         if (control_block_)
         {
            return control_block_->local_data_list;
         }
         else
         {
            return null_local_data_list;
         }
      }

      template<typename T> void expression<T>::register_return_results(results_context_t* rc)
      {
         if (control_block_ && rc)
         {
            control_block_->results = rc;
         }
      }

      template<typename T> void expression<T>::set_retinvk(bool* retinvk_ptr)
      {
         if (control_block_)
         {
            control_block_->return_invoked = retinvk_ptr;
         }
      }
         template class expression<float>;
         template class expression<double>;
         template class expression<long double>;
         template class expression<std::complex<float>>;
         template class expression<std::complex<double>>;
         template class expression<std::complex<long double>>;
}
