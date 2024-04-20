/*
 ******************************************************************
 *           C++ Mathematical Expression Toolkit Library          *
 *                                                                *
 * Author: Arash Partow (1999-2023)                               *
 * URL: https://www.partow.net/programming/exprtk/index.html      *
 *                                                                *
 * Copyright notice:                                              *
 * Free use of the C++ Mathematical Expression Toolkit Library is *
 * permitted under the guidelines and in accordance with the most *
 * current version of the MIT License.                            *
 * https://www.opensource.org/licenses/MIT                        *
 *                                                                *
 * Example expressions:                                           *
 * (00) (y + x / y) * (x - y / x)                                 *
 * (01) (x^2 / sin(2 * pi / y)) - x / 2                           *
 * (02) sqrt(1 - (x^2))                                           *
 * (03) 1 - sin(2 * x) + cos(pi / y)                              *
 * (04) a * exp(2 * t) + c                                        *
 * (05) if(((x + 2) == 3) and ((y + 5) <= 9),1 + w, 2 / z)        *
 * (06) (avg(x,y) <= x + y ? x - y : x * y) + 2 * pi / x          *
 * (07) z := x + sin(2 * pi / y)                                  *
 * (08) u := 2 * (pi * z) / (w := x + cos(y / pi))                *
 * (09) clamp(-1,sin(2 * pi * x) + cos(y / 2 * pi),+1)            *
 * (10) inrange(-2,m,+2) == if(({-2 <= m} and [m <= +2]),1,0)     *
 * (11) (2sin(x)cos(2y)7 + 1) == (2 * sin(x) * cos(2*y) * 7 + 1)  *
 * (12) (x ilike 's*ri?g') and [y < (3 z^7 + w)]                  *
 *                                                                *
 ******************************************************************
*/

#pragma once

#include "ExpressionNodes.hpp"
#include "OperatorHelpers.hpp"
#include "SymbolTable.hpp"

namespace Essa::Math{
   template <typename T>
   class expression
   {
   private:

      typedef details::expression_node<T>*  expression_ptr;
      typedef details::vector_holder<T>*    vector_holder_ptr;
      typedef std::vector<symbol_table<T> > symtab_list_t;

      struct control_block
      {
         enum data_type
         {
            e_unknown  ,
            e_expr     ,
            e_vecholder,
            e_data     ,
            e_vecdata
         };

         struct data_pack
         {
            data_pack()
            : pointer(0)
            , type(e_unknown)
            , size(0)
            {}

            data_pack(void* ptr, const data_type dt, const std::size_t sz = 0)
            : pointer(ptr)
            , type(dt)
            , size(sz)
            {}

            void*       pointer;
            data_type   type;
            std::size_t size;
         };

         typedef std::vector<data_pack> local_data_list_t;
         typedef results_context<T>     results_context_t;
         typedef control_block*         cntrl_blck_ptr_t;

         control_block();

         explicit control_block(expression_ptr e);

        ~control_block();

         static cntrl_blck_ptr_t create(expression_ptr e);

         static void destroy(cntrl_blck_ptr_t& cntrl_blck);

         std::size_t ref_count;
         expression_ptr expr;
         expression_ptr unoptimized_expr;
         local_data_list_t local_data_list;
         results_context_t* results;
         bool  retinv_null;
         bool* return_invoked;

      };

   public:

      expression();

      expression(const expression<T>& e);

      explicit expression(const symbol_table<T>& symbol_table);

      expression<T>& operator=(const expression<T>& e);

      std::string to_string() const;

      bool operator==(const expression<T>& e) const;

      bool operator!() const;

      expression<T>& release();

     ~expression();

      T value() const;

      T operator() () const;

      operator T() const;

      operator bool() const;

      void register_symbol_table(symbol_table<T>& st);

      const symbol_table<T>& get_symbol_table(const std::size_t& index = 0) const;

      symbol_table<T>& get_symbol_table(const std::size_t& index = 0);

      typedef results_context<T> results_context_t;

      const results_context_t& results() const;

      bool return_invoked() const;
      
   private:

      symtab_list_t get_symbol_table_list() const;

      void set_expression(const expression_ptr expr);

      void set_unoptimized_expr(const expression_ptr expr);

      void register_local_var(expression_ptr expr);

      void register_local_var(vector_holder_ptr vec_holder);

      void register_local_data(void* data, const std::size_t& size = 0, const std::size_t data_mode = 0);

      const typename control_block::local_data_list_t& local_data_list();

      void register_return_results(results_context_t* rc);

      void set_retinvk(bool* retinvk_ptr);

      control_block* control_block_;
      symtab_list_t  symbol_table_list_;
      typename control_block::local_data_list_t null_local_data_list;
      const results_context_t null_results;

      friend class parser<T>;
      friend class expression_helper<T>;
   }; // class expression
}
