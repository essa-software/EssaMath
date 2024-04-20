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

#include "Numeric.hpp"
#include "ParserHelpers.hpp"
#include "OperatorHelpers.hpp"
#include <cstdio>
#include <string>
#include <cassert>

namespace Essa::Math{
   namespace details
   {
      template <typename Type>
      class vector_holder
      {
      private:

         typedef Type value_type;
         typedef value_type* value_ptr;
         typedef const value_ptr const_value_ptr;

         class vector_holder_base
         {
         public:

            virtual ~vector_holder_base() {}

            inline value_ptr operator[](const std::size_t& index) const
            {
               return value_at(index);
            }

            inline std::size_t size() const
            {
               return vector_size();
            }

            inline value_ptr data() const
            {
               return value_at(0);
            }

            virtual inline bool rebaseable() const
            {
               return false;
            }

            virtual void set_ref(value_ptr*) {}

         protected:

            virtual value_ptr value_at(const std::size_t&) const = 0;
            virtual std::size_t vector_size()              const = 0;
         };

         class array_vector_impl : public vector_holder_base
         {
         public:

            array_vector_impl(const Type* vec, const std::size_t& vec_size)
            : vec_(vec)
            , size_(vec_size)
            {}

         protected:

            value_ptr value_at(const std::size_t& index) const
            {
               if (index < size_)
                  return const_cast<const_value_ptr>(vec_ + index);
               else
                  return const_value_ptr(0);
            }

            std::size_t vector_size() const
            {
               return size_;
            }

         private:

            array_vector_impl(const array_vector_impl&) exprtk_delete;
            array_vector_impl& operator=(const array_vector_impl&) exprtk_delete;

            const Type* vec_;
            const std::size_t size_;
         };

         template <typename Allocator,
                   template <typename, typename> class Sequence>
         class sequence_vector_impl : public vector_holder_base
         {
         public:

            typedef Sequence<Type,Allocator> sequence_t;

            sequence_vector_impl(sequence_t& seq)
            : sequence_(seq)
            {}

         protected:

            value_ptr value_at(const std::size_t& index) const
            {
               return (index < sequence_.size()) ? (&sequence_[index]) : const_value_ptr(0);
            }

            std::size_t vector_size() const
            {
               return sequence_.size();
            }

         private:

            sequence_vector_impl(const sequence_vector_impl&) exprtk_delete;
            sequence_vector_impl& operator=(const sequence_vector_impl&) exprtk_delete;

            sequence_t& sequence_;
         };

         class vector_view_impl : public vector_holder_base
         {
         public:

            typedef Essa::Math::vector_view<Type> vector_view_t;

            vector_view_impl(vector_view_t& vec_view)
            : vec_view_(vec_view)
            {}

            void set_ref(value_ptr* ref)
            {
               vec_view_.set_ref(ref);
            }

            virtual inline bool rebaseable() const
            {
               return true;
            }

         protected:

            value_ptr value_at(const std::size_t& index) const
            {
               return (index < vec_view_.size()) ? (&vec_view_[index]) : const_value_ptr(0);
            }

            std::size_t vector_size() const
            {
               return vec_view_.size();
            }

         private:

            vector_view_impl(const vector_view_impl&) exprtk_delete;
            vector_view_impl& operator=(const vector_view_impl&) exprtk_delete;

            vector_view_t& vec_view_;
         };

      public:

         typedef typename details::vec_data_store<Type> vds_t;

         vector_holder(Type* vec, const std::size_t& vec_size)
         : vector_holder_base_(new(buffer)array_vector_impl(vec,vec_size))
         {}

         explicit vector_holder(const vds_t& vds)
         : vector_holder_base_(new(buffer)array_vector_impl(vds.data(),vds.size()))
         {}

         template <typename Allocator>
         explicit vector_holder(std::vector<Type,Allocator>& vec)
         : vector_holder_base_(new(buffer)sequence_vector_impl<Allocator,std::vector>(vec))
         {}

         explicit vector_holder(Essa::Math::vector_view<Type>& vec)
         : vector_holder_base_(new(buffer)vector_view_impl(vec))
         {}

         inline value_ptr operator[](const std::size_t& index) const
         {
            return (*vector_holder_base_)[index];
         }

         inline std::size_t size() const
         {
            return vector_holder_base_->size();
         }

         inline value_ptr data() const
         {
            return vector_holder_base_->data();
         }

         void set_ref(value_ptr* ref)
         {
            vector_holder_base_->set_ref(ref);
         }

         bool rebaseable() const
         {
            return vector_holder_base_->rebaseable();
         }

      private:

         mutable vector_holder_base* vector_holder_base_;
         uchar_t buffer[64];
      };

      template <typename T>
      class expression_node : public node_collector_interface<expression_node<T> >,
                              public node_depth_base<expression_node<T> >
      {
      public:

         enum node_type
         {
            e_none          , e_null          , e_constant    , e_unary        ,
            e_binary        , e_binary_ext    , e_trinary     , e_quaternary   ,
            e_vararg        , e_conditional   , e_while       , e_repeat       ,
            e_for           , e_switch        , e_mswitch     , e_return       ,
            e_retenv        , e_variable      , e_stringvar   , e_stringconst  ,
            e_stringvarrng  , e_cstringvarrng , e_strgenrange , e_strconcat    ,
            e_stringvarsize , e_strswap       , e_stringsize  , e_stringvararg ,
            e_function      , e_vafunction    , e_genfunction , e_strfunction  ,
            e_strcondition  , e_strccondition , e_add         , e_sub          ,
            e_mul           , e_div           , e_mod         , e_pow          ,
            e_lt            , e_lte           , e_gt          , e_gte          ,
            e_eq            , e_ne            , e_and         , e_nand         ,
            e_or            , e_nor           , e_xor         , e_xnor         ,
            e_in            , e_like          , e_ilike       , e_inranges     ,
            e_ipow          , e_ipowinv       , e_abs         , e_acos         ,
            e_acosh         , e_asin          , e_asinh       , e_atan         ,
            e_atanh         , e_ceil          , e_cos         , e_cosh         ,
            e_exp           , e_expm1         , e_floor       , e_log          ,
            e_log10         , e_log2          , e_log1p       , e_neg          ,
            e_pos           , e_round         , e_sin         , e_sinc         ,
            e_sinh          , e_sqrt          , e_tan         , e_tanh         ,
            e_cot           , e_sec           , e_csc         , e_r2d          ,
            e_d2r           , e_d2g           , e_g2d         , e_notl         ,
            e_sgn           , e_erf           , e_erfc        , e_ncdf         ,
            e_frac          , e_trunc         , e_uvouv       , e_vov          ,
            e_cov           , e_voc           , e_vob         , e_bov          ,
            e_cob           , e_boc           , e_vovov       , e_vovoc        ,
            e_vocov         , e_covov         , e_covoc       , e_vovovov      ,
            e_vovovoc       , e_vovocov       , e_vocovov     , e_covovov      ,
            e_covocov       , e_vocovoc       , e_covovoc     , e_vococov      ,
            e_sf3ext        , e_sf4ext        , e_nulleq      , e_strass       ,
            e_vector        , e_vecelem       , e_rbvecelem   , e_rbveccelem   ,
            e_vecdefass     , e_vecvalass     , e_vecvecass   , e_vecopvalass  ,
            e_vecopvecass   , e_vecfunc       , e_vecvecswap  , e_vecvecineq   ,
            e_vecvalineq    , e_valvecineq    , e_vecvecarith , e_vecvalarith  ,
            e_valvecarith   , e_vecunaryop    , e_vecondition , e_break        ,
            e_continue      , e_swap
         };

         typedef T value_type;
         typedef expression_node<T>* expression_ptr;
         typedef node_collector_interface<expression_node<T> > nci_t;
         typedef typename nci_t::noderef_list_t noderef_list_t;
         typedef node_depth_base<expression_node<T> > ndb_t;

         virtual ~expression_node() {}

         inline virtual T value() const
         {
            return std::numeric_limits<T>::quiet_NaN();
         }

         inline virtual expression_node<T>* branch(const std::size_t& index = 0) const
         {
            return reinterpret_cast<expression_ptr>(index * 0);
         }

         inline virtual node_type type() const
         {
            return e_none;
         }

         inline virtual std::string to_string() const {
            return "";
         }
      }; // class expression_node

      template <typename T>
      class null_node exprtk_final : public expression_node<T>
      {
      public:

         inline T value() const exprtk_override
         {
            return std::numeric_limits<T>::quiet_NaN();
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_null;
         }

         inline std::string to_string() const exprtk_override{
            return "0";
         }
      };

      template <typename T, std::size_t N>
      inline void construct_branch_pair(std::pair<expression_node<T>*,bool> (&branch)[N],
                                        expression_node<T>* b,
                                        const std::size_t& index)
      {
         if (b && (index < N))
         {
            branch[index] = std::make_pair(b,branch_deletable(b));
         }
      }

      template <typename T>
      inline void construct_branch_pair(std::pair<expression_node<T>*,bool>& branch, expression_node<T>* b)
      {
         if (b)
         {
            branch = std::make_pair(b,branch_deletable(b));
         }
      }

      template <std::size_t N, typename T>
      inline void init_branches(std::pair<expression_node<T>*,bool> (&branch)[N],
                                expression_node<T>* b0,
                                expression_node<T>* b1 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b2 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b3 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b4 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b5 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b6 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b7 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b8 = reinterpret_cast<expression_node<T>*>(0),
                                expression_node<T>* b9 = reinterpret_cast<expression_node<T>*>(0))
      {
         construct_branch_pair(branch, b0, 0);
         construct_branch_pair(branch, b1, 1);
         construct_branch_pair(branch, b2, 2);
         construct_branch_pair(branch, b3, 3);
         construct_branch_pair(branch, b4, 4);
         construct_branch_pair(branch, b5, 5);
         construct_branch_pair(branch, b6, 6);
         construct_branch_pair(branch, b7, 7);
         construct_branch_pair(branch, b8, 8);
         construct_branch_pair(branch, b9, 9);
      }

      template <typename T>
      class null_eq_node exprtk_final : public expression_node<T>
      {
      public:
         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         explicit null_eq_node(expression_ptr branch, const bool equality = true)
         : equality_(equality)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);

            const T v = branch_.first->value();
            const bool result = details::numeric::is_nan(v);

            if (result)
               return equality_ ? T(1) : T(0);
            else
               return equality_ ? T(0) : T(1);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_nulleq;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return branch_.first->to_string() + (equality_ ? "==" : "!=") + "0";
         }
      private:

         bool equality_;
         branch_t branch_;
      };

      template <typename T>
      class literal_node exprtk_final : public expression_node<T>
      {
      public:

         explicit literal_node(const T& v)
         : value_(v)
         {}

         inline T value() const exprtk_override
         {
            return value_;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_constant;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return reinterpret_cast<expression_node<T>*>(0);
         }

         inline std::string to_string() const exprtk_override{
            const typename details::numeric::details::number_type<T>::type num_type;
            static const T local_pi = details::numeric::details::const_pi_impl<T>(num_type);
            static const T local_e = details::numeric::details::const_e_impl<T>(num_type);

            if(value_ == local_pi)
               return "%pi";
            else if(value_ == local_e)
               return "%e";
            else if(numeric::is_i<T>(value_))
               return "%i";
            else{
               return numeric::num_to_string<T>(value_);
            }
         }
      private:

         literal_node(const literal_node<T>&) exprtk_delete;
         literal_node<T>& operator=(const literal_node<T>&) exprtk_delete;

         const T value_;
      };

      template <typename T>
      class unary_node : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         unary_node(const operator_type& opr, expression_ptr branch)
         : operation_(opr)
         {
            construct_branch_pair(branch_,branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            const T arg = branch_.first->value();
            return numeric::process<T>(operation_,arg);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_unary;
         }

         inline operator_type operation() const
         {
            return operation_;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         inline void release()
         {
            branch_.second = false;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_final
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            char _buf[2048]{0};
            std::string _name = to_str(operation_);
            std::string _arg1 = branch_.first->to_string();
            sprintf(_buf, _name.c_str(), _arg1.c_str());
            return _buf;
         }
      private:

         operator_type operation_;
         branch_t branch_;
      };


      template <typename T>
      class binary_node : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         binary_node(const operator_type& opr,
                     expression_ptr branch0,
                     expression_ptr branch1)
         : operation_(opr)
         {
            init_branches<2>(branch_, branch0, branch1);
         }

         inline T value() const exprtk_override
         {
            assert(branch_[0].first);
            assert(branch_[1].first);

            const T arg0 = branch_[0].first->value();
            const T arg1 = branch_[1].first->value();

            return numeric::process<T>(operation_, arg0, arg1);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_binary;
         }

         inline operator_type operation() const
         {
            return operation_;
         }

         inline expression_node<T>* branch(const std::size_t& index = 0) const exprtk_override
         {
            if (0 == index)
               return branch_[0].first;
            else if (1 == index)
               return branch_[1].first;
            else
               return reinterpret_cast<expression_ptr>(0);
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_final
         {
            return expression_node<T>::ndb_t::template compute_node_depth<2>(branch_);
         }

         inline std::string to_string() const exprtk_override{
            char _buf[2048]{0};
            std::string _name = to_str(operation_);
            bool sig = false;
            
            if(binary_node* _node = dynamic_cast<binary_node*>(branch_[0].first))
               sig = check_significance(operation_, _node->operation());
            else
               sig = false;

            std::string _arg1 = (sig ? "(" : "") + branch_[0].first->to_string() + (sig ? ")" : "");
            
            if(binary_node* _node = dynamic_cast<binary_node*>(branch_[1].first))
               sig = check_significance(operation_, _node->operation());
            else
               sig = false;

            std::string _arg2 = (sig ? "(" : "") + branch_[1].first->to_string() + (sig ? ")" : "");
            sprintf(_buf, _name.c_str(), _arg1.c_str(), _arg2.c_str());
            return _buf;
         }
      private:

         operator_type operation_;
         branch_t branch_[2];
      };

      template <typename T, typename Operation>
      class binary_ext_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         binary_ext_node(expression_ptr branch0, expression_ptr branch1)
         {
            init_branches<2>(branch_, branch0, branch1);
         }

         inline T value() const exprtk_override
         {
            assert(branch_[0].first);
            assert(branch_[1].first);

            const T arg0 = branch_[0].first->value();
            const T arg1 = branch_[1].first->value();

            return Operation::process(arg0,arg1);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_binary_ext;
         }

         inline operator_type operation() const
         {
            return Operation::operation();
         }

         inline expression_node<T>* branch(const std::size_t& index = 0) const exprtk_override
         {
            if (0 == index)
               return branch_[0].first;
            else if (1 == index)
               return branch_[1].first;
            else
               return reinterpret_cast<expression_ptr>(0);
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::template compute_node_depth<2>(branch_);
         }

         inline std::string to_string() const exprtk_override{
            
            char _buf[2048]{0};
            std::string _name = to_str(operation());
            bool sig = false;
            
            if(binary_node<T>* _node = dynamic_cast<binary_node<T>*>(branch_[0].first))
               sig = check_significance(operation(), _node->operation());
            else
               sig = false;

            std::string _arg1 = (sig ? "(" : "") + branch_[0].first->to_string() + (sig ? ")" : "");
            
            if(binary_node<T>* _node = dynamic_cast<binary_node<T>*>(branch_[1].first))
               sig = check_significance(operation(), _node->operation());
            else
               sig = false;

            std::string _arg2 = (sig ? "(" : "") + branch_[1].first->to_string() + (sig ? ")" : "");
            sprintf(_buf, _name.c_str(), _arg1.c_str(), _arg2.c_str());
            return _buf;
         }
      protected:

         branch_t branch_[2];
      };

      template <typename T>
      class trinary_node : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         trinary_node(const operator_type& opr,
                      expression_ptr branch0,
                      expression_ptr branch1,
                      expression_ptr branch2)
         : operation_(opr)
         {
            init_branches<3>(branch_, branch0, branch1, branch2);
         }

         inline T value() const exprtk_override
         {
            assert(branch_[0].first);
            assert(branch_[1].first);
            assert(branch_[2].first);

            const T arg0 = branch_[0].first->value();
            const T arg1 = branch_[1].first->value();
            const T arg2 = branch_[2].first->value();

            switch (operation_)
            {
               default        : exprtk_debug(("trinary_node::value() - Error: Invalid operation\n"));
                                return std::numeric_limits<T>::quiet_NaN();
            }
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_trinary;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override exprtk_final
         {
            return expression_node<T>::ndb_t::template compute_node_depth<3>(branch_);
         }

         inline std::string to_string() const exprtk_override{
            char _buf[2048]{0};
            std::string _name = to_str(operation_);
            std::string _arg1 = branch_[0].first->to_string();
            std::string _arg2 = branch_[1].first->to_string();
            std::string _arg3 = branch_[2].first->to_string();
            sprintf(_buf, _name.c_str(), _arg1.c_str(), _arg2.c_str(), _arg3.c_str());
            return _buf;
         }
      protected:

         operator_type operation_;
         branch_t branch_[3];
      };

      template <typename T>
      class quaternary_node : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         quaternary_node(const operator_type& opr,
                         expression_ptr branch0,
                         expression_ptr branch1,
                         expression_ptr branch2,
                         expression_ptr branch3)
         : operation_(opr)
         {
            init_branches<4>(branch_, branch0, branch1, branch2, branch3);
         }

         inline T value() const exprtk_override
         {
            return std::numeric_limits<T>::quiet_NaN();
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_quaternary;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override exprtk_final
         {
            return expression_node<T>::ndb_t::template compute_node_depth<4>(branch_);
         }

         inline std::string to_string() const exprtk_override{
            char _buf[2048]{0};
            std::string _name = to_str(operation_);
            std::string _arg1 = branch_[0].first->to_string();
            std::string _arg2 = branch_[1].first->to_string();
            std::string _arg3 = branch_[2].first->to_string();
            std::string _arg4 = branch_[3].first->to_string();
            sprintf(_buf, _name.c_str(), _arg1.c_str(), _arg2.c_str(), _arg3.c_str(), _arg4.c_str());
            return _buf;
         }
      protected:

         operator_type operation_;
         branch_t branch_[4];
      };

      template <typename T>
      class ivariable
      {
      public:

         virtual ~ivariable() {}

         virtual T& ref() = 0;
         virtual const T& ref() const = 0;
      };

      template <typename T>
      class variable_node exprtk_final
                          : public expression_node<T>,
                            public ivariable      <T>
      {
      public:

         static T null_value;

         explicit variable_node()
         : value_(&null_value)
         {}

         explicit variable_node(T& v, std::string id)
         : value_(&v), id_(id)
         {}

         inline bool operator <(const variable_node<T>& v) const
         {
            return this < (&v);
         }

         inline T value() const exprtk_override
         {
            return (*value_);
         }

         inline T& ref() exprtk_override
         {
            return (*value_);
         }

         inline const T& ref() const exprtk_override
         {
            return (*value_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_variable;
         }

         inline std::string to_string() const exprtk_override{
            return id_.empty() ? numeric::num_to_string<T>(*value_) : id_;
         }
      private:

         T* value_;
         std::string id_;
      };

      template <typename T>
      T variable_node<T>::null_value = T(std::numeric_limits<T>::quiet_NaN());

      template <typename T> class vector_node;

      template <typename T>
      class vector_interface
      {
      public:

         typedef vector_node<T>*   vector_node_ptr;
         typedef vec_data_store<T> vds_t;

         virtual ~vector_interface() {}

         virtual std::size_t size   () const = 0;

         virtual vector_node_ptr vec() const = 0;

         virtual vector_node_ptr vec()       = 0;

         virtual       vds_t& vds   ()       = 0;

         virtual const vds_t& vds   () const = 0;

         virtual bool side_effect   () const { return false; }
      };

      template <typename T>
      class vector_node exprtk_final
                        : public expression_node <T>
                        , public vector_interface<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_holder<T>    vector_holder_t;
         typedef vector_node<T>*     vector_node_ptr;
         typedef vec_data_store<T>   vds_t;

         explicit vector_node(vector_holder_t* vh)
         : vector_holder_(vh)
         , vds_((*vector_holder_).size(),(*vector_holder_)[0])
         {
            vector_holder_->set_ref(&vds_.ref());
         }

         vector_node(const vds_t& vds, vector_holder_t* vh)
         : vector_holder_(vh)
         , vds_(vds)
         {}

         inline T value() const exprtk_override
         {
            return vds().data()[0];
         }

         vector_node_ptr vec() const exprtk_override
         {
            return const_cast<vector_node_ptr>(this);
         }

         vector_node_ptr vec() exprtk_override
         {
            return this;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vector;
         }

         std::size_t size() const exprtk_override
         {
            return vds().size();
         }

         vds_t& vds() exprtk_override
         {
            return vds_;
         }

         const vds_t& vds() const exprtk_override
         {
            return vds_;
         }

         inline vector_holder_t& vec_holder()
         {
            return (*vector_holder_);
         }

         inline std::string to_string() const exprtk_override{
            return "(vector_node)";
         }
      private:

         vector_holder_t* vector_holder_;
         vds_t                      vds_;
      };

      template <typename T>
      class vector_elem_node exprtk_final
                             : public expression_node<T>,
                               public ivariable      <T>
      {
      public:

         typedef expression_node<T>*            expression_ptr;
         typedef vector_holder<T>               vector_holder_t;
         typedef vector_holder_t*               vector_holder_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         vector_elem_node(expression_ptr index, vector_holder_ptr vec_holder)
         : vec_holder_(vec_holder)
         , vector_base_((*vec_holder)[0])
         {
            construct_branch_pair(index_, index);
         }

         inline T value() const exprtk_override
         {
            return *(vector_base_ + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline T& ref() exprtk_override
         {
            return *(vector_base_ + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline const T& ref() const exprtk_override
         {
            return *(vector_base_ + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecelem;
         }

         inline vector_holder_t& vec_holder()
         {
            return (*vec_holder_);
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(index_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(index_);
         }

         inline std::string to_string() const exprtk_override{
            return "(vector_elem_node)";
         }
      private:

         vector_holder_ptr vec_holder_;
         T* vector_base_;
         branch_t index_;
      };

      template <typename T>
      class rebasevector_elem_node exprtk_final
                                   : public expression_node<T>,
                                     public ivariable      <T>
      {
      public:

         typedef expression_node<T>*            expression_ptr;
         typedef vector_holder<T>               vector_holder_t;
         typedef vector_holder_t*               vector_holder_ptr;
         typedef vec_data_store<T>              vds_t;
         typedef std::pair<expression_ptr,bool> branch_t;

         rebasevector_elem_node(expression_ptr index, vector_holder_ptr vec_holder)
         : vector_holder_(vec_holder)
         , vds_((*vector_holder_).size(),(*vector_holder_)[0])
         {
            vector_holder_->set_ref(&vds_.ref());
            construct_branch_pair(index_, index);
         }

         inline T value() const exprtk_override
         {
            return *(vds_.data() + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline T& ref() exprtk_override
         {
            return *(vds_.data() + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline const T& ref() const exprtk_override
         {
            return *(vds_.data() + static_cast<std::size_t>(details::numeric::to_int64(index_.first->value())));
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_rbvecelem;
         }

         inline vector_holder_t& vec_holder()
         {
            return (*vector_holder_);
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(index_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(index_);
         }

      private:

         vector_holder_ptr vector_holder_;
         vds_t             vds_;
         branch_t          index_;
      };

      template <typename T>
      class rebasevector_celem_node exprtk_final
                                    : public expression_node<T>,
                                      public ivariable      <T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_holder<T>    vector_holder_t;
         typedef vector_holder_t*    vector_holder_ptr;
         typedef vec_data_store<T>   vds_t;

         rebasevector_celem_node(const std::size_t index, vector_holder_ptr vec_holder)
         : index_(index)
         , vector_holder_(vec_holder)
         , vds_((*vector_holder_).size(),(*vector_holder_)[0])
         {
            vector_holder_->set_ref(&vds_.ref());
         }

         inline T value() const exprtk_override
         {
            return *(vds_.data() + index_);
         }

         inline T& ref() exprtk_override
         {
            return *(vds_.data() + index_);
         }

         inline const T& ref() const exprtk_override
         {
            return *(vds_.data() + index_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_rbveccelem;
         }

         inline vector_holder_t& vec_holder()
         {
            return (*vector_holder_);
         }

         inline std::string to_string() const exprtk_override{
            return "(rebasevector_celem_node)";
         }
      private:

         const std::size_t index_;
         vector_holder_ptr vector_holder_;
         vds_t vds_;
      };

      template <typename T, std::size_t N>
      inline T axn(const T a, const T x)
      {
         // a*x^n
         return a * Essa::Math::details::numeric::fast_exp<T,N>::result(x);
      }

      template <typename T, std::size_t N>
      inline T axnb(const T a, const T x, const T b)
      {
         // a*x^n+b
         return a * Essa::Math::details::numeric::fast_exp<T,N>::result(x) + b;
      }

      template <typename T>
      struct sf_base
      {
         typedef typename details::functor_t<T>::Type Type;
         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::qfunc_t quaternary_functor_t;
         typedef typename functor_t::tfunc_t trinary_functor_t;
         typedef typename functor_t::bfunc_t binary_functor_t;
         typedef typename functor_t::ufunc_t unary_functor_t;
      };

      #define define_sfop3(NN, OP0, OP1)                 \
      template <typename T>                              \
      struct sf##NN##_op : public sf_base<T>             \
      {                                                  \
         typedef typename sf_base<T>::Type const Type;   \
         static inline T process(Type x, Type y, Type z) \
         {                                               \
            return (OP0);                                \
         }                                               \
         static inline std::string id()                  \
         {                                               \
            return (OP1);                                \
         }                                               \
      };                                                 \

      define_sfop3(00,(x + y) / z       ,"(t+t)/t")
      define_sfop3(01,(x + y) * z       ,"(t+t)*t")
      define_sfop3(02,(x + y) - z       ,"(t+t)-t")
      define_sfop3(03,(x + y) + z       ,"(t+t)+t")
      define_sfop3(04,(x - y) + z       ,"(t-t)+t")
      define_sfop3(05,(x - y) / z       ,"(t-t)/t")
      define_sfop3(06,(x - y) * z       ,"(t-t)*t")
      define_sfop3(07,(x * y) + z       ,"(t*t)+t")
      define_sfop3(08,(x * y) - z       ,"(t*t)-t")
      define_sfop3(09,(x * y) / z       ,"(t*t)/t")
      define_sfop3(10,(x * y) * z       ,"(t*t)*t")
      define_sfop3(11,(x / y) + z       ,"(t/t)+t")
      define_sfop3(12,(x / y) - z       ,"(t/t)-t")
      define_sfop3(13,(x / y) / z       ,"(t/t)/t")
      define_sfop3(14,(x / y) * z       ,"(t/t)*t")
      define_sfop3(15,x / (y + z)       ,"t/(t+t)")
      define_sfop3(16,x / (y - z)       ,"t/(t-t)")
      define_sfop3(17,x / (y * z)       ,"t/(t*t)")
      define_sfop3(18,x / (y / z)       ,"t/(t/t)")
      define_sfop3(19,x * (y + z)       ,"t*(t+t)")
      define_sfop3(20,x * (y - z)       ,"t*(t-t)")
      define_sfop3(21,x * (y * z)       ,"t*(t*t)")
      define_sfop3(22,x * (y / z)       ,"t*(t/t)")
      define_sfop3(23,x - (y + z)       ,"t-(t+t)")
      define_sfop3(24,x - (y - z)       ,"t-(t-t)")
      define_sfop3(25,x - (y / z)       ,"t-(t/t)")
      define_sfop3(26,x - (y * z)       ,"t-(t*t)")
      define_sfop3(27,x + (y * z)       ,"t+(t*t)")
      define_sfop3(28,x + (y / z)       ,"t+(t/t)")
      define_sfop3(29,x + (y + z)       ,"t+(t+t)")
      define_sfop3(30,x + (y - z)       ,"t+(t-t)")
      define_sfop3(31,(axnb<T,2>(x,y,z)),"       ")
      define_sfop3(32,(axnb<T,3>(x,y,z)),"       ")
      define_sfop3(33,(axnb<T,4>(x,y,z)),"       ")
      define_sfop3(34,(axnb<T,5>(x,y,z)),"       ")
      define_sfop3(35,(axnb<T,6>(x,y,z)),"       ")
      define_sfop3(36,(axnb<T,7>(x,y,z)),"       ")
      define_sfop3(37,(axnb<T,8>(x,y,z)),"       ")
      define_sfop3(38,(axnb<T,9>(x,y,z)),"       ")
      define_sfop3(39,x * numeric::log(y)   + z,"")
      define_sfop3(40,x * numeric::log(y)   - z,"")
      define_sfop3(41,x * numeric::log10(y) + z,"")
      define_sfop3(42,x * numeric::log10(y) - z,"")
      define_sfop3(43,x * numeric::sin(y) + z  ,"")
      define_sfop3(44,x * numeric::sin(y) - z  ,"")
      define_sfop3(45,x * numeric::cos(y) + z  ,"")
      define_sfop3(46,x * numeric::cos(y) - z  ,"")
      define_sfop3(47,details::is_true(x) ? y : z,"")

      #define define_sfop4(NN, OP0, OP1)                         \
      template <typename T>                                      \
      struct sf##NN##_op : public sf_base<T>                     \
      {                                                          \
         typedef typename sf_base<T>::Type const Type;           \
         static inline T process(Type x, Type y, Type z, Type w) \
         {                                                       \
            return (OP0);                                        \
         }                                                       \
         static inline std::string id()                          \
         {                                                       \
            return (OP1);                                        \
         }                                                       \
      };                                                         \

      define_sfop4(48,(x + ((y + z) / w)),"t+((t+t)/t)")
      define_sfop4(49,(x + ((y + z) * w)),"t+((t+t)*t)")
      define_sfop4(50,(x + ((y - z) / w)),"t+((t-t)/t)")
      define_sfop4(51,(x + ((y - z) * w)),"t+((t-t)*t)")
      define_sfop4(52,(x + ((y * z) / w)),"t+((t*t)/t)")
      define_sfop4(53,(x + ((y * z) * w)),"t+((t*t)*t)")
      define_sfop4(54,(x + ((y / z) + w)),"t+((t/t)+t)")
      define_sfop4(55,(x + ((y / z) / w)),"t+((t/t)/t)")
      define_sfop4(56,(x + ((y / z) * w)),"t+((t/t)*t)")
      define_sfop4(57,(x - ((y + z) / w)),"t-((t+t)/t)")
      define_sfop4(58,(x - ((y + z) * w)),"t-((t+t)*t)")
      define_sfop4(59,(x - ((y - z) / w)),"t-((t-t)/t)")
      define_sfop4(60,(x - ((y - z) * w)),"t-((t-t)*t)")
      define_sfop4(61,(x - ((y * z) / w)),"t-((t*t)/t)")
      define_sfop4(62,(x - ((y * z) * w)),"t-((t*t)*t)")
      define_sfop4(63,(x - ((y / z) / w)),"t-((t/t)/t)")
      define_sfop4(64,(x - ((y / z) * w)),"t-((t/t)*t)")
      define_sfop4(65,(((x + y) * z) - w),"((t+t)*t)-t")
      define_sfop4(66,(((x - y) * z) - w),"((t-t)*t)-t")
      define_sfop4(67,(((x * y) * z) - w),"((t*t)*t)-t")
      define_sfop4(68,(((x / y) * z) - w),"((t/t)*t)-t")
      define_sfop4(69,(((x + y) / z) - w),"((t+t)/t)-t")
      define_sfop4(70,(((x - y) / z) - w),"((t-t)/t)-t")
      define_sfop4(71,(((x * y) / z) - w),"((t*t)/t)-t")
      define_sfop4(72,(((x / y) / z) - w),"((t/t)/t)-t")
      define_sfop4(73,((x * y) + (z * w)),"(t*t)+(t*t)")
      define_sfop4(74,((x * y) - (z * w)),"(t*t)-(t*t)")
      define_sfop4(75,((x * y) + (z / w)),"(t*t)+(t/t)")
      define_sfop4(76,((x * y) - (z / w)),"(t*t)-(t/t)")
      define_sfop4(77,((x / y) + (z / w)),"(t/t)+(t/t)")
      define_sfop4(78,((x / y) - (z / w)),"(t/t)-(t/t)")
      define_sfop4(79,((x / y) - (z * w)),"(t/t)-(t*t)")
      define_sfop4(80,(x / (y + (z * w))),"t/(t+(t*t))")
      define_sfop4(81,(x / (y - (z * w))),"t/(t-(t*t))")
      define_sfop4(82,(x * (y + (z * w))),"t*(t+(t*t))")
      define_sfop4(83,(x * (y - (z * w))),"t*(t-(t*t))")

      define_sfop4(84,(axn<T,2>(x,y) + axn<T,2>(z,w)),"")
      define_sfop4(85,(axn<T,3>(x,y) + axn<T,3>(z,w)),"")
      define_sfop4(86,(axn<T,4>(x,y) + axn<T,4>(z,w)),"")
      define_sfop4(87,(axn<T,5>(x,y) + axn<T,5>(z,w)),"")
      define_sfop4(88,(axn<T,6>(x,y) + axn<T,6>(z,w)),"")
      define_sfop4(89,(axn<T,7>(x,y) + axn<T,7>(z,w)),"")
      define_sfop4(90,(axn<T,8>(x,y) + axn<T,8>(z,w)),"")
      define_sfop4(91,(axn<T,9>(x,y) + axn<T,9>(z,w)),"")
      define_sfop4(92,((details::is_true(x) && details::is_true(y)) ? z : w),"")
      define_sfop4(93,((details::is_true(x) || details::is_true(y)) ? z : w),"")
      define_sfop4(94,(details::is_true(numeric::lth<T>(x, y)) ? z : w),"")
      define_sfop4(95,(details::is_true(numeric::leq<T>(x, y)) ? z : w),"")
      define_sfop4(96,(details::is_true(numeric::gth<T>(x, y)) ? z : w),"")
      define_sfop4(97,(details::is_true(numeric::geq<T>(x, y)) ? z : w),"")
      define_sfop4(98,(details::is_true(numeric::equal(x,y)) ? z : w),"")
      define_sfop4(99,(x * numeric::sin(y) + z * numeric::cos(w)),"")

      define_sfop4(ext00,((x + y) - (z * w)),"(t+t)-(t*t)")
      define_sfop4(ext01,((x + y) - (z / w)),"(t+t)-(t/t)")
      define_sfop4(ext02,((x + y) + (z * w)),"(t+t)+(t*t)")
      define_sfop4(ext03,((x + y) + (z / w)),"(t+t)+(t/t)")
      define_sfop4(ext04,((x - y) + (z * w)),"(t-t)+(t*t)")
      define_sfop4(ext05,((x - y) + (z / w)),"(t-t)+(t/t)")
      define_sfop4(ext06,((x - y) - (z * w)),"(t-t)-(t*t)")
      define_sfop4(ext07,((x - y) - (z / w)),"(t-t)-(t/t)")
      define_sfop4(ext08,((x + y) - (z - w)),"(t+t)-(t-t)")
      define_sfop4(ext09,((x + y) + (z - w)),"(t+t)+(t-t)")
      define_sfop4(ext10,((x + y) + (z + w)),"(t+t)+(t+t)")
      define_sfop4(ext11,((x + y) * (z - w)),"(t+t)*(t-t)")
      define_sfop4(ext12,((x + y) / (z - w)),"(t+t)/(t-t)")
      define_sfop4(ext13,((x - y) - (z + w)),"(t-t)-(t+t)")
      define_sfop4(ext14,((x - y) + (z + w)),"(t-t)+(t+t)")
      define_sfop4(ext15,((x - y) * (z + w)),"(t-t)*(t+t)")
      define_sfop4(ext16,((x - y) / (z + w)),"(t-t)/(t+t)")
      define_sfop4(ext17,((x * y) - (z + w)),"(t*t)-(t+t)")
      define_sfop4(ext18,((x / y) - (z + w)),"(t/t)-(t+t)")
      define_sfop4(ext19,((x * y) + (z + w)),"(t*t)+(t+t)")
      define_sfop4(ext20,((x / y) + (z + w)),"(t/t)+(t+t)")
      define_sfop4(ext21,((x * y) + (z - w)),"(t*t)+(t-t)")
      define_sfop4(ext22,((x / y) + (z - w)),"(t/t)+(t-t)")
      define_sfop4(ext23,((x * y) - (z - w)),"(t*t)-(t-t)")
      define_sfop4(ext24,((x / y) - (z - w)),"(t/t)-(t-t)")
      define_sfop4(ext25,((x + y) * (z * w)),"(t+t)*(t*t)")
      define_sfop4(ext26,((x + y) * (z / w)),"(t+t)*(t/t)")
      define_sfop4(ext27,((x + y) / (z * w)),"(t+t)/(t*t)")
      define_sfop4(ext28,((x + y) / (z / w)),"(t+t)/(t/t)")
      define_sfop4(ext29,((x - y) / (z * w)),"(t-t)/(t*t)")
      define_sfop4(ext30,((x - y) / (z / w)),"(t-t)/(t/t)")
      define_sfop4(ext31,((x - y) * (z * w)),"(t-t)*(t*t)")
      define_sfop4(ext32,((x - y) * (z / w)),"(t-t)*(t/t)")
      define_sfop4(ext33,((x * y) * (z + w)),"(t*t)*(t+t)")
      define_sfop4(ext34,((x / y) * (z + w)),"(t/t)*(t+t)")
      define_sfop4(ext35,((x * y) / (z + w)),"(t*t)/(t+t)")
      define_sfop4(ext36,((x / y) / (z + w)),"(t/t)/(t+t)")
      define_sfop4(ext37,((x * y) / (z - w)),"(t*t)/(t-t)")
      define_sfop4(ext38,((x / y) / (z - w)),"(t/t)/(t-t)")
      define_sfop4(ext39,((x * y) * (z - w)),"(t*t)*(t-t)")
      define_sfop4(ext40,((x * y) / (z * w)),"(t*t)/(t*t)")
      define_sfop4(ext41,((x / y) * (z / w)),"(t/t)*(t/t)")
      define_sfop4(ext42,((x / y) * (z - w)),"(t/t)*(t-t)")
      define_sfop4(ext43,((x * y) * (z * w)),"(t*t)*(t*t)")
      define_sfop4(ext44,(x + (y * (z / w))),"t+(t*(t/t))")
      define_sfop4(ext45,(x - (y * (z / w))),"t-(t*(t/t))")
      define_sfop4(ext46,(x + (y / (z * w))),"t+(t/(t*t))")
      define_sfop4(ext47,(x - (y / (z * w))),"t-(t/(t*t))")
      define_sfop4(ext48,(((x - y) - z) * w),"((t-t)-t)*t")
      define_sfop4(ext49,(((x - y) - z) / w),"((t-t)-t)/t")
      define_sfop4(ext50,(((x - y) + z) * w),"((t-t)+t)*t")
      define_sfop4(ext51,(((x - y) + z) / w),"((t-t)+t)/t")
      define_sfop4(ext52,((x + (y - z)) * w),"(t+(t-t))*t")
      define_sfop4(ext53,((x + (y - z)) / w),"(t+(t-t))/t")
      define_sfop4(ext54,((x + y) / (z + w)),"(t+t)/(t+t)")
      define_sfop4(ext55,((x - y) / (z - w)),"(t-t)/(t-t)")
      define_sfop4(ext56,((x + y) * (z + w)),"(t+t)*(t+t)")
      define_sfop4(ext57,((x - y) * (z - w)),"(t-t)*(t-t)")
      define_sfop4(ext58,((x - y) + (z - w)),"(t-t)+(t-t)")
      define_sfop4(ext59,((x - y) - (z - w)),"(t-t)-(t-t)")
      define_sfop4(ext60,((x / y) + (z * w)),"(t/t)+(t*t)")
      define_sfop4(ext61,(((x * y) * z) / w),"((t*t)*t)/t")

      #undef define_sfop3
      #undef define_sfop4

      template <typename T, typename SpecialFunction>
      class sf3_node exprtk_final : public trinary_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;

         sf3_node(const operator_type& opr,
                  expression_ptr branch0,
                  expression_ptr branch1,
                  expression_ptr branch2)
         : trinary_node<T>(opr, branch0, branch1, branch2)
         {}

         inline T value() const exprtk_override
         {
            assert(trinary_node<T>::branch_[0].first);
            assert(trinary_node<T>::branch_[1].first);
            assert(trinary_node<T>::branch_[2].first);

            const T x = trinary_node<T>::branch_[0].first->value();
            const T y = trinary_node<T>::branch_[1].first->value();
            const T z = trinary_node<T>::branch_[2].first->value();

            return SpecialFunction::process(x, y, z);
         }

         inline std::string to_string() const exprtk_override{
            return "(sf3_node)";
         }
      };

      template <typename T, typename SpecialFunction>
      class sf4_node exprtk_final : public quaternary_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;

         sf4_node(const operator_type& opr,
                  expression_ptr branch0,
                  expression_ptr branch1,
                  expression_ptr branch2,
                  expression_ptr branch3)
         : quaternary_node<T>(opr, branch0, branch1, branch2, branch3)
         {}

         inline T value() const exprtk_override
         {
            assert(quaternary_node<T>::branch_[0].first);
            assert(quaternary_node<T>::branch_[1].first);
            assert(quaternary_node<T>::branch_[2].first);
            assert(quaternary_node<T>::branch_[3].first);

            const T x = quaternary_node<T>::branch_[0].first->value();
            const T y = quaternary_node<T>::branch_[1].first->value();
            const T z = quaternary_node<T>::branch_[2].first->value();
            const T w = quaternary_node<T>::branch_[3].first->value();

            return SpecialFunction::process(x, y, z, w);
         }

         inline std::string to_string() const exprtk_override{
            return "(sf4_node)";
         }
      };

      template <typename T, typename SpecialFunction>
      class sf3_var_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;

         sf3_var_node(const T& v0, const T& v1, const T& v2)
         : v0_(v0)
         , v1_(v1)
         , v2_(v2)
         {}

         inline T value() const exprtk_override
         {
            return SpecialFunction::process(v0_, v1_, v2_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_trinary;
         }

         inline std::string to_string() const exprtk_override{
            return "(sf3_var_node)";
         }
      private:

         sf3_var_node(const sf3_var_node<T,SpecialFunction>&) exprtk_delete;
         sf3_var_node<T,SpecialFunction>& operator=(const sf3_var_node<T,SpecialFunction>&) exprtk_delete;

         const T& v0_;
         const T& v1_;
         const T& v2_;
      };

      template <typename T, typename SpecialFunction>
      class sf4_var_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;

         sf4_var_node(const T& v0, const T& v1, const T& v2, const T& v3)
         : v0_(v0)
         , v1_(v1)
         , v2_(v2)
         , v3_(v3)
         {}

         inline T value() const exprtk_override
         {
            return SpecialFunction::process(v0_, v1_, v2_, v3_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_trinary;
         }

         inline std::string to_string() const exprtk_override{
            return "(sf4_var_node)";
         }
      private:

         sf4_var_node(const sf4_var_node<T,SpecialFunction>&) exprtk_delete;
         sf4_var_node<T,SpecialFunction>& operator=(const sf4_var_node<T,SpecialFunction>&) exprtk_delete;

         const T& v0_;
         const T& v1_;
         const T& v2_;
         const T& v3_;
      };

      template <typename T, typename VecFunction>
      class vectorize_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         explicit vectorize_node(const expression_ptr v)
         : ivec_ptr_(0)
         {
            construct_branch_pair(v_, v);

            if (is_ivector_node(v_.first))
            {
               ivec_ptr_ = dynamic_cast<vector_interface<T>*>(v_.first);
            }
            else
               ivec_ptr_ = 0;
         }

         inline T value() const exprtk_override
         {
            if (ivec_ptr_)
            {
               assert(v_.first);

               v_.first->value();

               return VecFunction::process(ivec_ptr_);
            }
            else
               return std::numeric_limits<T>::quiet_NaN();
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecfunc;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(v_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(v_);
         }

         inline std::string to_string() const exprtk_override{
            return "(vectorize_node)";
         }
      private:

         vector_interface<T>* ivec_ptr_;
         branch_t                    v_;
      };

      template <typename T, typename Operation>
      class vec_binop_vecvec_node exprtk_final
                                  : public binary_node     <T>
                                  , public vector_interface<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_node<T>*     vector_node_ptr;
         typedef vector_holder<T>*   vector_holder_ptr;
         typedef vec_data_store<T>   vds_t;

         using binary_node<T>::branch;

         vec_binop_vecvec_node(const operator_type& opr,
                               expression_ptr branch0,
                               expression_ptr branch1)
         : binary_node<T>(opr, branch0, branch1)
         , vec0_node_ptr_(0)
         , vec1_node_ptr_(0)
         , temp_         (0)
         , temp_vec_node_(0)
         , initialised_(false)
         {
            bool v0_is_ivec = false;
            bool v1_is_ivec = false;

            if (is_vector_node(branch(0)))
            {
               vec0_node_ptr_ = static_cast<vector_node_ptr>(branch(0));
            }
            else if (is_ivector_node(branch(0)))
            {
               vector_interface<T>* vi = reinterpret_cast<vector_interface<T>*>(0);

               if (0 != (vi = dynamic_cast<vector_interface<T>*>(branch(0))))
               {
                  vec0_node_ptr_ = vi->vec();
                  v0_is_ivec     = true;
               }
            }

            if (is_vector_node(branch(1)))
            {
               vec1_node_ptr_ = static_cast<vector_node_ptr>(branch(1));
            }
            else if (is_ivector_node(branch(1)))
            {
               vector_interface<T>* vi = reinterpret_cast<vector_interface<T>*>(0);

               if (0 != (vi = dynamic_cast<vector_interface<T>*>(branch(1))))
               {
                  vec1_node_ptr_ = vi->vec();
                  v1_is_ivec     = true;
               }
            }

            if (vec0_node_ptr_ && vec1_node_ptr_)
            {
               vector_holder<T>& vec0 = vec0_node_ptr_->vec_holder();
               vector_holder<T>& vec1 = vec1_node_ptr_->vec_holder();

               if (v0_is_ivec && (vec0.size() <= vec1.size()))
                  vds_ = vds_t(vec0_node_ptr_->vds());
               else if (v1_is_ivec && (vec1.size() <= vec0.size()))
                  vds_ = vds_t(vec1_node_ptr_->vds());
               else
                  vds_ = vds_t(std::min(vec0.size(),vec1.size()));

               temp_          = new vector_holder<T>(vds().data(),vds().size());
               temp_vec_node_ = new vector_node  <T>(vds(),temp_);

               initialised_ = true;
            }

            assert(initialised_);
         }

        ~vec_binop_vecvec_node()
         {
            delete temp_;
            delete temp_vec_node_;
         }

         inline T value() const exprtk_override
         {
            if (initialised_)
            {
               assert(branch(0));
               assert(branch(1));

               branch(0)->value();
               branch(1)->value();

               const T* vec0 = vec0_node_ptr_->vds().data();
               const T* vec1 = vec1_node_ptr_->vds().data();
                     T* vec2 = vds().data();

               loop_unroll::details lud(size());
               const T* upper_bound = vec2 + lud.upper_bound;

               while (vec2 < upper_bound)
               {
                  #define exprtk_loop(N)                          \
                  vec2[N] = Operation::process(vec0[N], vec1[N]); \

                  exprtk_loop( 0) exprtk_loop( 1)
                  exprtk_loop( 2) exprtk_loop( 3)
                  exprtk_loop( 4) exprtk_loop( 5)
                  exprtk_loop( 6) exprtk_loop( 7)
                  exprtk_loop( 8) exprtk_loop( 9)
                  exprtk_loop(10) exprtk_loop(11)
                  exprtk_loop(12) exprtk_loop(13)
                  exprtk_loop(14) exprtk_loop(15)

                  vec0 += lud.batch_size;
                  vec1 += lud.batch_size;
                  vec2 += lud.batch_size;
               }

               int i = 0;

               exprtk_disable_fallthrough_begin
               switch (lud.remainder)
               {
                  #define case_stmt(N)                                              \
                  case N : { vec2[i] = Operation::process(vec0[i], vec1[i]); ++i; } \

                  case_stmt(15) case_stmt(14)
                  case_stmt(13) case_stmt(12)
                  case_stmt(11) case_stmt(10)
                  case_stmt( 9) case_stmt( 8)
                  case_stmt( 7) case_stmt( 6)
                  case_stmt( 5) case_stmt( 4)
                  case_stmt( 3) case_stmt( 2)
                  case_stmt( 1)
               }
               exprtk_disable_fallthrough_end

               #undef exprtk_loop
               #undef case_stmt

               return (vds().data())[0];
            }
            else
               return std::numeric_limits<T>::quiet_NaN();
         }

         vector_node_ptr vec() const exprtk_override
         {
            return temp_vec_node_;
         }

         vector_node_ptr vec() exprtk_override
         {
            return temp_vec_node_;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecvecarith;
         }

         std::size_t size() const exprtk_override
         {
            return vds_.size();
         }

         vds_t& vds() exprtk_override
         {
            return vds_;
         }

         const vds_t& vds() const exprtk_override
         {
            return vds_;
         }

         inline std::string to_string() const exprtk_override{
            return "(vec_binop_vecvec_node)";
         }
      private:

         vector_node_ptr   vec0_node_ptr_;
         vector_node_ptr   vec1_node_ptr_;
         vector_holder_ptr temp_;
         vector_node_ptr   temp_vec_node_;
         bool              initialised_;
         vds_t             vds_;
      };

      template <typename T, typename Operation>
      class vec_binop_vecval_node exprtk_final
                                  : public binary_node     <T>
                                  , public vector_interface<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_node<T>*     vector_node_ptr;
         typedef vector_holder<T>*   vector_holder_ptr;
         typedef vec_data_store<T>   vds_t;

         using binary_node<T>::branch;

         vec_binop_vecval_node(const operator_type& opr,
                               expression_ptr branch0,
                               expression_ptr branch1)
         : binary_node<T>(opr, branch0, branch1)
         , vec0_node_ptr_(0)
         , temp_         (0)
         , temp_vec_node_(0)
         {
            bool v0_is_ivec = false;

            if (is_vector_node(branch(0)))
            {
               vec0_node_ptr_ = static_cast<vector_node_ptr>(branch(0));
            }
            else if (is_ivector_node(branch(0)))
            {
               vector_interface<T>* vi = reinterpret_cast<vector_interface<T>*>(0);

               if (0 != (vi = dynamic_cast<vector_interface<T>*>(branch(0))))
               {
                  vec0_node_ptr_ = vi->vec();
                  v0_is_ivec     = true;
               }
            }

            if (vec0_node_ptr_)
            {
               if (v0_is_ivec)
                  vds() = vec0_node_ptr_->vds();
               else
                  vds() = vds_t(vec0_node_ptr_->size());

               temp_          = new vector_holder<T>(vds());
               temp_vec_node_ = new vector_node  <T>(vds(),temp_);
            }
         }

        ~vec_binop_vecval_node()
         {
            delete temp_;
            delete temp_vec_node_;
         }

         inline T value() const exprtk_override
         {
            if (vec0_node_ptr_)
            {
               assert(branch(0));
               assert(branch(1));

                           branch(0)->value();
               const T v = branch(1)->value();

               const T* vec0 = vec0_node_ptr_->vds().data();
                     T* vec1 = vds().data();

               loop_unroll::details lud(size());
               const T* upper_bound = vec0 + lud.upper_bound;

               while (vec0 < upper_bound)
               {
                  #define exprtk_loop(N)                    \
                  vec1[N] = Operation::process(vec0[N], v); \

                  exprtk_loop( 0) exprtk_loop( 1)
                  exprtk_loop( 2) exprtk_loop( 3)
                  exprtk_loop( 4) exprtk_loop( 5)
                  exprtk_loop( 6) exprtk_loop( 7)
                  exprtk_loop( 8) exprtk_loop( 9)
                  exprtk_loop(10) exprtk_loop(11)
                  exprtk_loop(12) exprtk_loop(13)
                  exprtk_loop(14) exprtk_loop(15)

                  vec0 += lud.batch_size;
                  vec1 += lud.batch_size;
               }

               int i = 0;

               exprtk_disable_fallthrough_begin
               switch (lud.remainder)
               {
                  #define case_stmt(N)                                        \
                  case N : { vec1[i] = Operation::process(vec0[i], v); ++i; } \

                  case_stmt(15) case_stmt(14)
                  case_stmt(13) case_stmt(12)
                  case_stmt(11) case_stmt(10)
                  case_stmt( 9) case_stmt( 8)
                  case_stmt( 7) case_stmt( 6)
                  case_stmt( 5) case_stmt( 4)
                  case_stmt( 3) case_stmt( 2)
                  case_stmt( 1)
               }
               exprtk_disable_fallthrough_end

               #undef exprtk_loop
               #undef case_stmt

               return (vds().data())[0];
            }
            else
               return std::numeric_limits<T>::quiet_NaN();
         }

         vector_node_ptr vec() const exprtk_override
         {
            return temp_vec_node_;
         }

         vector_node_ptr vec() exprtk_override
         {
            return temp_vec_node_;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecvalarith;
         }

         std::size_t size() const exprtk_override
         {
            return vds().size();
         }

         vds_t& vds() exprtk_override
         {
            return vds_;
         }

         const vds_t& vds() const exprtk_override
         {
            return vds_;
         }

         inline std::string to_string() const exprtk_override{
            return "(vec_binop_vecval_node)";
         }
      private:

         vector_node_ptr   vec0_node_ptr_;
         vector_holder_ptr temp_;
         vector_node_ptr   temp_vec_node_;
         vds_t             vds_;
      };

      template <typename T, typename Operation>
      class vec_binop_valvec_node exprtk_final
                                  : public binary_node     <T>
                                  , public vector_interface<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_node<T>*     vector_node_ptr;
         typedef vector_holder<T>*   vector_holder_ptr;
         typedef vec_data_store<T>   vds_t;

         using binary_node<T>::branch;

         vec_binop_valvec_node(const operator_type& opr,
                               expression_ptr branch0,
                               expression_ptr branch1)
         : binary_node<T>(opr, branch0, branch1)
         , vec1_node_ptr_(0)
         , temp_         (0)
         , temp_vec_node_(0)
         {
            bool v1_is_ivec = false;

            if (is_vector_node(branch(1)))
            {
               vec1_node_ptr_ = static_cast<vector_node_ptr>(branch(1));
            }
            else if (is_ivector_node(branch(1)))
            {
               vector_interface<T>* vi = reinterpret_cast<vector_interface<T>*>(0);

               if (0 != (vi = dynamic_cast<vector_interface<T>*>(branch(1))))
               {
                  vec1_node_ptr_ = vi->vec();
                  v1_is_ivec     = true;
               }
            }

            if (vec1_node_ptr_)
            {
               if (v1_is_ivec)
                  vds() = vec1_node_ptr_->vds();
               else
                  vds() = vds_t(vec1_node_ptr_->size());

               temp_          = new vector_holder<T>(vds());
               temp_vec_node_ = new vector_node  <T>(vds(),temp_);
            }
         }

        ~vec_binop_valvec_node()
         {
            delete temp_;
            delete temp_vec_node_;
         }

         inline T value() const exprtk_override
         {
            if (vec1_node_ptr_)
            {
               assert(branch(0));
               assert(branch(1));

               const T v = branch(0)->value();
                           branch(1)->value();

                     T* vec0 = vds().data();
               const T* vec1 = vec1_node_ptr_->vds().data();

               loop_unroll::details lud(size());
               const T* upper_bound = vec0 + lud.upper_bound;

               while (vec0 < upper_bound)
               {
                  #define exprtk_loop(N)                    \
                  vec0[N] = Operation::process(v, vec1[N]); \

                  exprtk_loop( 0) exprtk_loop( 1)
                  exprtk_loop( 2) exprtk_loop( 3)
                  exprtk_loop( 4) exprtk_loop( 5)
                  exprtk_loop( 6) exprtk_loop( 7)
                  exprtk_loop( 8) exprtk_loop( 9)
                  exprtk_loop(10) exprtk_loop(11)
                  exprtk_loop(12) exprtk_loop(13)
                  exprtk_loop(14) exprtk_loop(15)

                  vec0 += lud.batch_size;
                  vec1 += lud.batch_size;
               }

               int i = 0;

               exprtk_disable_fallthrough_begin
               switch (lud.remainder)
               {
                  #define case_stmt(N)                                        \
                  case N : { vec0[i] = Operation::process(v, vec1[i]); ++i; } \

                  case_stmt(15) case_stmt(14)
                  case_stmt(13) case_stmt(12)
                  case_stmt(11) case_stmt(10)
                  case_stmt( 9) case_stmt( 8)
                  case_stmt( 7) case_stmt( 6)
                  case_stmt( 5) case_stmt( 4)
                  case_stmt( 3) case_stmt( 2)
                  case_stmt( 1)
               }
               exprtk_disable_fallthrough_end

               #undef exprtk_loop
               #undef case_stmt

               return (vds().data())[0];
            }
            else
               return std::numeric_limits<T>::quiet_NaN();
         }

         vector_node_ptr vec() const exprtk_override
         {
            return temp_vec_node_;
         }

         vector_node_ptr vec() exprtk_override
         {
            return temp_vec_node_;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecvalarith;
         }

         std::size_t size() const exprtk_override
         {
            return vds().size();
         }

         vds_t& vds() exprtk_override
         {
            return vds_;
         }

         const vds_t& vds() const exprtk_override
         {
            return vds_;
         }

         inline std::string to_string() const exprtk_override{
            return "(vec_binop_valvec_node)";
         }
      private:

         vector_node_ptr   vec1_node_ptr_;
         vector_holder_ptr temp_;
         vector_node_ptr   temp_vec_node_;
         vds_t             vds_;
      };

      template <typename T, typename Operation>
      class unary_vector_node exprtk_final
                              : public unary_node      <T>
                              , public vector_interface<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef vector_node<T>*     vector_node_ptr;
         typedef vector_holder<T>*   vector_holder_ptr;
         typedef vec_data_store<T>   vds_t;

         using expression_node<T>::branch;

         unary_vector_node(const operator_type& opr, expression_ptr branch0)
         : unary_node<T>(opr, branch0)
         , vec0_node_ptr_(0)
         , temp_         (0)
         , temp_vec_node_(0)
         {
            bool vec0_is_ivec = false;

            if (is_vector_node(branch()))
            {
               vec0_node_ptr_ = static_cast<vector_node_ptr>(branch());
            }
            else if (is_ivector_node(branch()))
            {
               vector_interface<T>* vi = reinterpret_cast<vector_interface<T>*>(0);

               if (0 != (vi = dynamic_cast<vector_interface<T>*>(branch())))
               {
                  vec0_node_ptr_ = vi->vec();
                  vec0_is_ivec   = true;
               }
            }

            if (vec0_node_ptr_)
            {
               if (vec0_is_ivec)
                  vds_ = vec0_node_ptr_->vds();
               else
                  vds_ = vds_t(vec0_node_ptr_->size());

               temp_          = new vector_holder<T>(vds());
               temp_vec_node_ = new vector_node  <T>(vds(),temp_);
            }
         }

        ~unary_vector_node()
         {
            delete temp_;
            delete temp_vec_node_;
         }

         inline T value() const exprtk_override
         {
            assert(branch());

            branch()->value();

            if (vec0_node_ptr_)
            {
               const T* vec0 = vec0_node_ptr_->vds().data();
                     T* vec1 = vds().data();

               loop_unroll::details lud(size());
               const T* upper_bound = vec0 + lud.upper_bound;

               while (vec0 < upper_bound)
               {
                  #define exprtk_loop(N)                 \
                  vec1[N] = Operation::process(vec0[N]); \

                  exprtk_loop( 0) exprtk_loop( 1)
                  exprtk_loop( 2) exprtk_loop( 3)
                  exprtk_loop( 4) exprtk_loop( 5)
                  exprtk_loop( 6) exprtk_loop( 7)
                  exprtk_loop( 8) exprtk_loop( 9)
                  exprtk_loop(10) exprtk_loop(11)
                  exprtk_loop(12) exprtk_loop(13)
                  exprtk_loop(14) exprtk_loop(15)

                  vec0 += lud.batch_size;
                  vec1 += lud.batch_size;
               }

               int i = 0;

               exprtk_disable_fallthrough_begin
               switch (lud.remainder)
               {
                  #define case_stmt(N)                                     \
                  case N : { vec1[i] = Operation::process(vec0[i]); ++i; } \

                  case_stmt(15) case_stmt(14)
                  case_stmt(13) case_stmt(12)
                  case_stmt(11) case_stmt(10)
                  case_stmt( 9) case_stmt( 8)
                  case_stmt( 7) case_stmt( 6)
                  case_stmt( 5) case_stmt( 4)
                  case_stmt( 3) case_stmt( 2)
                  case_stmt( 1)
               }
               exprtk_disable_fallthrough_end

               #undef exprtk_loop
               #undef case_stmt

               return (vds().data())[0];
            }
            else
               return std::numeric_limits<T>::quiet_NaN();
         }

         vector_node_ptr vec() const exprtk_override
         {
            return temp_vec_node_;
         }

         vector_node_ptr vec() exprtk_override
         {
            return temp_vec_node_;
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_vecunaryop;
         }

         std::size_t size() const exprtk_override
         {
            return vds().size();
         }

         vds_t& vds() exprtk_override
         {
            return vds_;
         }

         const vds_t& vds() const exprtk_override
         {
            return vds_;
         }

         inline std::string to_string() const exprtk_override{
            return "(vec_binop_valvec_node)";
         }
      private:

         vector_node_ptr   vec0_node_ptr_;
         vector_holder_ptr temp_;
         vector_node_ptr   temp_vec_node_;
         vds_t             vds_;
      };

      template <typename T>
      class scand_node exprtk_final : public binary_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         using binary_node<T>::branch;

         scand_node(const operator_type& opr,
                    expression_ptr branch0,
                    expression_ptr branch1)
         : binary_node<T>(opr, branch0, branch1)
         {}

         inline T value() const exprtk_override
         {
            assert(branch(0));
            assert(branch(1));

            return (
                     std::not_equal_to<T>()
                        (T(0),branch(0)->value()) &&
                     std::not_equal_to<T>()
                        (T(0),branch(1)->value())
                   ) ? T(1) : T(0);
         }

         inline std::string to_string() const exprtk_override{
            return "(scand_node)";
         }
      };

      template <typename T>
      class scor_node exprtk_final : public binary_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         using binary_node<T>::branch;

         scor_node(const operator_type& opr,
                   expression_ptr branch0,
                   expression_ptr branch1)
         : binary_node<T>(opr, branch0, branch1)
         {}

         inline T value() const exprtk_override
         {
            assert(branch(0));
            assert(branch(1));

            return (
                     std::not_equal_to<T>()
                        (T(0),branch(0)->value()) ||
                     std::not_equal_to<T>()
                        (T(0),branch(1)->value())
                   ) ? T(1) : T(0);
         }

         inline std::string to_string() const exprtk_override{
            return "(scor_node)";
         }
      };

      #define exprtk_define_unary_op(OpName)                    \
      template <typename T>                                     \
      struct OpName##_op                                        \
      {                                                         \
         typedef typename functor_t<T>::Type Type;              \
         typedef typename expression_node<T>::node_type node_t; \
                                                                \
         static inline T process(Type v)                        \
         {                                                      \
            return numeric:: OpName (v);                        \
         }                                                      \
                                                                \
         static inline node_t type()                            \
         {                                                      \
            return expression_node<T>::e_##OpName;              \
         }                                                      \
                                                                \
         static inline details::operator_type operation()       \
         {                                                      \
            return details::e_##OpName;                         \
         }                                                      \
      }; 
            template <typename T>
      class vov_base_node : public expression_node<T>
      {
      public:

         virtual ~vov_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T& v0() const = 0;

         virtual const T& v1() const = 0;
      };

      template <typename T>
      class cov_base_node : public expression_node<T>
      {
      public:

         virtual ~cov_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T c() const = 0;

         virtual const T& v() const = 0;
      };

      template <typename T>
      class voc_base_node : public expression_node<T>
      {
      public:

         virtual ~voc_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T c() const = 0;

         virtual const T& v() const = 0;
      };

      template <typename T>
      class vob_base_node : public expression_node<T>
      {
      public:

         virtual ~vob_base_node() {}

         virtual const T& v() const = 0;
      };

      template <typename T>
      class bov_base_node : public expression_node<T>
      {
      public:

         virtual ~bov_base_node() {}

         virtual const T& v() const = 0;
      };

      template <typename T>
      class cob_base_node : public expression_node<T>
      {
      public:

         virtual ~cob_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T c() const = 0;

         virtual void set_c(const T) = 0;

         virtual expression_node<T>* move_branch(const std::size_t& index) = 0;
      };

      template <typename T>
      class boc_base_node : public expression_node<T>
      {
      public:

         virtual ~boc_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T c() const = 0;

         virtual void set_c(const T) = 0;

         virtual expression_node<T>* move_branch(const std::size_t& index) = 0;
      };

      template <typename T>
      class uv_base_node : public expression_node<T>
      {
      public:

         virtual ~uv_base_node() {}

         inline virtual operator_type operation() const
         {
            return details::e_default;
         }

         virtual const T& v() const = 0;
      };

      template <typename T>
      class T0oT1oT2_base_node : public expression_node<T>
      {
      public:

         virtual ~T0oT1oT2_base_node() {}

         virtual std::string type_id() const = 0;
      };

      template <typename T>
      class T0oT1oT2oT3_base_node : public expression_node<T>
      {
      public:

         virtual ~T0oT1oT2oT3_base_node() {}

         virtual std::string type_id() const = 0;
      };

      template <typename T, typename Operation>
      class unary_variable_node exprtk_final : public uv_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef Operation operation_t;

         explicit unary_variable_node(const T& var)
         : v_(var)
         {}

         inline T value() const exprtk_override
         {
            return Operation::process(v_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return Operation::type();
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T& v() const exprtk_override
         {
            return v_;
         }

         inline std::string to_string() const exprtk_override{
            return numeric::num_to_string(v_);
         }
      private:

         unary_variable_node(const unary_variable_node<T,Operation>&) exprtk_delete;
         unary_variable_node<T,Operation>& operator=(const unary_variable_node<T,Operation>&) exprtk_delete;

         const T& v_;
      };

      template <typename T>
      class uvouv_node exprtk_final : public expression_node<T>
      {
      public:

         // UOpr1(v0) Op UOpr2(v1)
         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;
         typedef typename functor_t::ufunc_t    ufunc_t;
         typedef expression_node<T>*            expression_ptr;

         explicit uvouv_node(const T& var0,const T& var1,
                             ufunc_t uf0, ufunc_t uf1, bfunc_t bf)
         : v0_(var0)
         , v1_(var1)
         , u0_(uf0 )
         , u1_(uf1 )
         , f_ (bf  )
         {}

         inline T value() const exprtk_override
         {
            return f_(u0_(v0_),u1_(v1_));
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_uvouv;
         }

         inline const T& v0()
         {
            return v0_;
         }

         inline const T& v1()
         {
            return v1_;
         }

         inline ufunc_t u0()
         {
            return u0_;
         }

         inline ufunc_t u1()
         {
            return u1_;
         }

         inline ufunc_t f()
         {
            return f_;
         }

         inline std::string to_string() const exprtk_override{
            return "(uvouv_node)";
         }
      private:

         uvouv_node(const uvouv_node<T>&) exprtk_delete;
         uvouv_node<T>& operator=(const uvouv_node<T>&) exprtk_delete;

         const T& v0_;
         const T& v1_;
         const ufunc_t u0_;
         const ufunc_t u1_;
         const bfunc_t f_;
      };

      template <typename T, typename Operation>
      class unary_branch_node exprtk_final : public expression_node<T>
      {
      public:

         typedef Operation                      operation_t;
         typedef expression_node<T>*            expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;

         explicit unary_branch_node(expression_ptr branch)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            return Operation::process(branch_.first->value());
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return Operation::type();
         }

         inline operator_type operation() const
         {
            return Operation::operation();
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         inline void release()
         {
            branch_.second = false;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            char _buf[2048]{0};
            std::string _name = to_str(operation());
            std::string _arg1 = branch_.first->to_string();
            sprintf(_buf, _name.c_str(), _arg1.c_str());
            return _buf;
         }
      private:

         unary_branch_node(const unary_branch_node<T,Operation>&) exprtk_delete;
         unary_branch_node<T,Operation>& operator=(const unary_branch_node<T,Operation>&) exprtk_delete;

         branch_t branch_;
      };
            #define exprtk_crtype(Type)                          \
      param_to_str<is_const_ref< Type >::result>::result() \

      template <typename T>
      struct T0oT1oT2process
      {
         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;

         struct mode0
         {
            static inline T process(const T& t0, const T& t1, const T& t2, const bfunc_t bf0, const bfunc_t bf1)
            {
               // (T0 o0 T1) o1 T2
               return bf1(bf0(t0,t1),t2);
            }

            template <typename T0, typename T1, typename T2>
            static inline std::string id()
            {
               static const std::string result = "(" + exprtk_crtype(T0) + "o"   +
                                                       exprtk_crtype(T1) + ")o(" +
                                                       exprtk_crtype(T2) + ")"   ;
               return result;
            }
         };

         struct mode1
         {
            static inline T process(const T& t0, const T& t1, const T& t2, const bfunc_t bf0, const bfunc_t bf1)
            {
               // T0 o0 (T1 o1 T2)
               return bf0(t0,bf1(t1,t2));
            }

            template <typename T0, typename T1, typename T2>
            static inline std::string id()
            {
               static const std::string result = "(" + exprtk_crtype(T0) + ")o(" +
                                                       exprtk_crtype(T1) + "o"   +
                                                       exprtk_crtype(T2) + ")"   ;
               return result;
            }
         };
      };

      template <typename T>
      struct T0oT1oT20T3process
      {
         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;

         struct mode0
         {
            static inline T process(const T& t0, const T& t1,
                                    const T& t2, const T& t3,
                                    const bfunc_t bf0, const bfunc_t bf1, const bfunc_t bf2)
            {
               // (T0 o0 T1) o1 (T2 o2 T3)
               return bf1(bf0(t0,t1),bf2(t2,t3));
            }

            template <typename T0, typename T1, typename T2, typename T3>
            static inline std::string id()
            {
               static const std::string result = "(" + exprtk_crtype(T0) + "o"  +
                                                       exprtk_crtype(T1) + ")o" +
                                                 "(" + exprtk_crtype(T2) + "o"  +
                                                       exprtk_crtype(T3) + ")"  ;
               return result;
            }
         };

         struct mode1
         {
            static inline T process(const T& t0, const T& t1,
                                    const T& t2, const T& t3,
                                    const bfunc_t bf0, const bfunc_t bf1, const bfunc_t bf2)
            {
               // (T0 o0 (T1 o1 (T2 o2 T3))
               return bf0(t0,bf1(t1,bf2(t2,t3)));
            }
            template <typename T0, typename T1, typename T2, typename T3>
            static inline std::string id()
            {
               static const std::string result = "(" + exprtk_crtype(T0) +  ")o((" +
                                                       exprtk_crtype(T1) +  ")o("  +
                                                       exprtk_crtype(T2) +  "o"    +
                                                       exprtk_crtype(T3) +  "))"   ;
               return result;
            }
         };

         struct mode2
         {
            static inline T process(const T& t0, const T& t1,
                                    const T& t2, const T& t3,
                                    const bfunc_t bf0, const bfunc_t bf1, const bfunc_t bf2)
            {
               // (T0 o0 ((T1 o1 T2) o2 T3)
               return bf0(t0,bf2(bf1(t1,t2),t3));
            }

            template <typename T0, typename T1, typename T2, typename T3>
            static inline std::string id()
            {
               static const std::string result = "(" + exprtk_crtype(T0) + ")o((" +
                                                       exprtk_crtype(T1) + "o"    +
                                                       exprtk_crtype(T2) + ")o("  +
                                                       exprtk_crtype(T3) + "))"   ;
               return result;
            }
         };

         struct mode3
         {
            static inline T process(const T& t0, const T& t1,
                                    const T& t2, const T& t3,
                                    const bfunc_t bf0, const bfunc_t bf1, const bfunc_t bf2)
            {
               // (((T0 o0 T1) o1 T2) o2 T3)
               return bf2(bf1(bf0(t0,t1),t2),t3);
            }

            template <typename T0, typename T1, typename T2, typename T3>
            static inline std::string id()
            {
               static const std::string result = "((" + exprtk_crtype(T0) + "o"    +
                                                        exprtk_crtype(T1) + ")o("  +
                                                        exprtk_crtype(T2) + "))o(" +
                                                        exprtk_crtype(T3) + ")";
               return result;
            }
         };

         struct mode4
         {
            static inline T process(const T& t0, const T& t1,
                                    const T& t2, const T& t3,
                                    const bfunc_t bf0, const bfunc_t bf1, const bfunc_t bf2)
            {
               // ((T0 o0 (T1 o1 T2)) o2 T3
               return bf2(bf0(t0,bf1(t1,t2)),t3);
            }

            template <typename T0, typename T1, typename T2, typename T3>
            static inline std::string id()
            {
               static const std::string result = "((" + exprtk_crtype(T0) + ")o("  +
                                                        exprtk_crtype(T1) + "o"    +
                                                        exprtk_crtype(T2) + "))o(" +
                                                        exprtk_crtype(T3) + ")"    ;
               return result;
            }
         };
      };

      #undef exprtk_crtype

      template <typename T, typename T0, typename T1>
      struct nodetype_T0oT1 { static const typename expression_node<T>::node_type result; };
      template <typename T, typename T0, typename T1>
      const typename expression_node<T>::node_type nodetype_T0oT1<T,T0,T1>::result = expression_node<T>::e_none;

      #define synthesis_node_type_define(T0_, T1_, v_)                                                          \
      template <typename T, typename T0, typename T1>                                                           \
      struct nodetype_T0oT1<T,T0_,T1_> { static const typename expression_node<T>::node_type result; };         \
      template <typename T, typename T0, typename T1>                                                           \
      const typename expression_node<T>::node_type nodetype_T0oT1<T,T0_,T1_>::result = expression_node<T>:: v_; \

      synthesis_node_type_define(const T0&, const T1&,  e_vov)
      synthesis_node_type_define(const T0&, const T1 ,  e_voc)
      synthesis_node_type_define(const T0 , const T1&,  e_cov)
      synthesis_node_type_define(      T0&,       T1&, e_none)
      synthesis_node_type_define(const T0 , const T1 , e_none)
      synthesis_node_type_define(      T0&, const T1 , e_none)
      synthesis_node_type_define(const T0 ,       T1&, e_none)
      synthesis_node_type_define(const T0&,       T1&, e_none)
      synthesis_node_type_define(      T0&, const T1&, e_none)
      #undef synthesis_node_type_define

      template <typename T, typename T0, typename T1, typename T2>
      struct nodetype_T0oT1oT2 { static const typename expression_node<T>::node_type result; };
      template <typename T, typename T0, typename T1, typename T2>
      const typename expression_node<T>::node_type nodetype_T0oT1oT2<T,T0,T1,T2>::result = expression_node<T>::e_none;

      #define synthesis_node_type_define(T0_, T1_, T2_, v_)                                                            \
      template <typename T, typename T0, typename T1, typename T2>                                                     \
      struct nodetype_T0oT1oT2<T,T0_,T1_,T2_> { static const typename expression_node<T>::node_type result; };         \
      template <typename T, typename T0, typename T1, typename T2>                                                     \
      const typename expression_node<T>::node_type nodetype_T0oT1oT2<T,T0_,T1_,T2_>::result = expression_node<T>:: v_; \

      synthesis_node_type_define(const T0&, const T1&, const T2&, e_vovov)
      synthesis_node_type_define(const T0&, const T1&, const T2 , e_vovoc)
      synthesis_node_type_define(const T0&, const T1 , const T2&, e_vocov)
      synthesis_node_type_define(const T0 , const T1&, const T2&, e_covov)
      synthesis_node_type_define(const T0 , const T1&, const T2 , e_covoc)
      synthesis_node_type_define(const T0 , const T1 , const T2 , e_none )
      synthesis_node_type_define(const T0 , const T1 , const T2&, e_none )
      synthesis_node_type_define(const T0&, const T1 , const T2 , e_none )
      synthesis_node_type_define(      T0&,       T1&,       T2&, e_none )
      #undef synthesis_node_type_define

      template <typename T, typename T0, typename T1, typename T2, typename T3>
      struct nodetype_T0oT1oT2oT3 { static const typename expression_node<T>::node_type result; };
      template <typename T, typename T0, typename T1, typename T2, typename T3>
      const typename expression_node<T>::node_type nodetype_T0oT1oT2oT3<T,T0,T1,T2,T3>::result = expression_node<T>::e_none;

      #define synthesis_node_type_define(T0_, T1_, T2_, T3_, v_)                                                              \
      template <typename T, typename T0, typename T1, typename T2, typename T3>                                               \
      struct nodetype_T0oT1oT2oT3<T,T0_,T1_,T2_,T3_> { static const typename expression_node<T>::node_type result; };         \
      template <typename T, typename T0, typename T1, typename T2, typename T3>                                               \
      const typename expression_node<T>::node_type nodetype_T0oT1oT2oT3<T,T0_,T1_,T2_,T3_>::result = expression_node<T>:: v_; \

      synthesis_node_type_define(const T0&, const T1&, const T2&, const T3&, e_vovovov)
      synthesis_node_type_define(const T0&, const T1&, const T2&, const T3 , e_vovovoc)
      synthesis_node_type_define(const T0&, const T1&, const T2 , const T3&, e_vovocov)
      synthesis_node_type_define(const T0&, const T1 , const T2&, const T3&, e_vocovov)
      synthesis_node_type_define(const T0 , const T1&, const T2&, const T3&, e_covovov)
      synthesis_node_type_define(const T0 , const T1&, const T2 , const T3&, e_covocov)
      synthesis_node_type_define(const T0&, const T1 , const T2&, const T3 , e_vocovoc)
      synthesis_node_type_define(const T0 , const T1&, const T2&, const T3 , e_covovoc)
      synthesis_node_type_define(const T0&, const T1 , const T2 , const T3&, e_vococov)
      synthesis_node_type_define(const T0 , const T1 , const T2 , const T3 , e_none   )
      synthesis_node_type_define(const T0 , const T1 , const T2 , const T3&, e_none   )
      synthesis_node_type_define(const T0 , const T1 , const T2&, const T3 , e_none   )
      synthesis_node_type_define(const T0 , const T1&, const T2 , const T3 , e_none   )
      synthesis_node_type_define(const T0&, const T1 , const T2 , const T3 , e_none   )
      synthesis_node_type_define(const T0 , const T1 , const T2&, const T3&, e_none   )
      synthesis_node_type_define(const T0&, const T1&, const T2 , const T3 , e_none   )
      #undef synthesis_node_type_define

      template <typename T, typename T0, typename T1>
      class T0oT1 exprtk_final : public expression_node<T>
      {
      public:

         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;
         typedef T value_type;
         typedef T0oT1<T,T0,T1> node_type;

         T0oT1(T0 p0, T1 p1, const bfunc_t p2)
         : t0_(p0)
         , t1_(p1)
         , f_ (p2)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1<T,T0,T1>::result;
            return result;
         }

         inline operator_type operation() const exprtk_override
         {
            return e_default;
         }

         inline T value() const exprtk_override
         {
            return f_(t0_,t1_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline bfunc_t f() const
         {
            return f_;
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator,
                                                    T0 p0, T1 p1,
                                                    bfunc_t p2)
         {
            return allocator
                     .template allocate_type<node_type, T0, T1, bfunc_t&>
                        (p0, p1, p2);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1)";
         }
      private:

         T0oT1(const T0oT1<T,T0,T1>&) exprtk_delete;
         T0oT1<T,T0,T1>& operator=(const T0oT1<T,T0,T1>&) { return (*this); }

         T0 t0_;
         T1 t1_;
         const bfunc_t f_;
      };

      template <typename T, typename T0, typename T1, typename T2, typename ProcessMode>
      class T0oT1oT2 exprtk_final : public T0oT1oT2_base_node<T>
      {
      public:

         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;
         typedef T value_type;
         typedef T0oT1oT2<T,T0,T1,T2,ProcessMode> node_type;
         typedef ProcessMode process_mode_t;

         T0oT1oT2(T0 p0, T1 p1, T2 p2, const bfunc_t p3, const bfunc_t p4)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         , f0_(p3)
         , f1_(p4)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1oT2<T,T0,T1,T2>::result;
            return result;
         }

         inline operator_type operation()
         {
            return e_default;
         }

         inline T value() const exprtk_override
         {
            return ProcessMode::process(t0_, t1_, t2_, f0_, f1_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline T2 t2() const
         {
            return t2_;
         }

         bfunc_t f0() const
         {
            return f0_;
         }

         bfunc_t f1() const
         {
            return f1_;
         }

         std::string type_id() const exprtk_override
         {
            return id();
         }

         static inline std::string id()
         {
            return process_mode_t::template id<T0,T1,T2>();
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator, T0 p0, T1 p1, T2 p2, bfunc_t p3, bfunc_t p4)
         {
            return allocator
                      .template allocate_type<node_type, T0, T1, T2, bfunc_t, bfunc_t>
                         (p0, p1, p2, p3, p4);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2)";
         }
      private:

         T0oT1oT2(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
         const bfunc_t f0_;
         const bfunc_t f1_;
      };

      template <typename T, typename T0_, typename T1_, typename T2_, typename T3_, typename ProcessMode>
      class T0oT1oT2oT3 exprtk_final : public T0oT1oT2oT3_base_node<T>
      {
      public:

         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::bfunc_t    bfunc_t;
         typedef T value_type;
         typedef T0_ T0;
         typedef T1_ T1;
         typedef T2_ T2;
         typedef T3_ T3;
         typedef T0oT1oT2oT3<T,T0,T1,T2,T3,ProcessMode> node_type;
         typedef ProcessMode process_mode_t;

         T0oT1oT2oT3(T0 p0, T1 p1, T2 p2, T3 p3, bfunc_t p4, bfunc_t p5, bfunc_t p6)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         , t3_(p3)
         , f0_(p4)
         , f1_(p5)
         , f2_(p6)
         {}

         inline T value() const exprtk_override
         {
            return ProcessMode::process(t0_, t1_, t2_, t3_, f0_, f1_, f2_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline T2 t2() const
         {
            return t2_;
         }

         inline T3 t3() const
         {
            return t3_;
         }

         inline bfunc_t f0() const
         {
            return f0_;
         }

         inline bfunc_t f1() const
         {
            return f1_;
         }

         inline bfunc_t f2() const
         {
            return f2_;
         }

         inline std::string type_id() const exprtk_override
         {
            return id();
         }

         static inline std::string id()
         {
            return process_mode_t::template id<T0, T1, T2, T3>();
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator,
                                                    T0 p0, T1 p1, T2 p2, T3 p3,
                                                    bfunc_t p4, bfunc_t p5, bfunc_t p6)
         {
            return allocator
                      .template allocate_type<node_type, T0, T1, T2, T3, bfunc_t, bfunc_t>
                         (p0, p1, p2, p3, p4, p5, p6);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2oT3)";
         }
      private:

         T0oT1oT2oT3(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
         T3 t3_;
         const bfunc_t f0_;
         const bfunc_t f1_;
         const bfunc_t f2_;
      };

      template <typename T, typename T0, typename T1, typename T2>
      class T0oT1oT2_sf3 exprtk_final : public T0oT1oT2_base_node<T>
      {
      public:

         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::tfunc_t    tfunc_t;
         typedef T value_type;
         typedef T0oT1oT2_sf3<T,T0,T1,T2> node_type;

         T0oT1oT2_sf3(T0 p0, T1 p1, T2 p2, const tfunc_t p3)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         , f_ (p3)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1oT2<T,T0,T1,T2>::result;
            return result;
         }

         inline operator_type operation() const exprtk_override
         {
            return e_default;
         }

         inline T value() const exprtk_override
         {
            return f_(t0_, t1_, t2_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline T2 t2() const
         {
            return t2_;
         }

         tfunc_t f() const
         {
            return f_;
         }

         std::string type_id() const
         {
            return id();
         }

         static inline std::string id()
         {
            return "sf3";
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator, T0 p0, T1 p1, T2 p2, tfunc_t p3)
         {
            return allocator
                     .template allocate_type<node_type, T0, T1, T2, tfunc_t>
                        (p0, p1, p2, p3);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2_sf3)";
         }
      private:

         T0oT1oT2_sf3(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
         const tfunc_t f_;
      };

      template <typename T, typename T0, typename T1, typename T2>
      class sf3ext_type_node : public T0oT1oT2_base_node<T>
      {
      public:

         virtual ~sf3ext_type_node() {}

         virtual T0 t0() const = 0;

         virtual T1 t1() const = 0;

         virtual T2 t2() const = 0;
      };

      template <typename T, typename T0, typename T1, typename T2, typename SF3Operation>
      class T0oT1oT2_sf3ext exprtk_final : public sf3ext_type_node<T,T0,T1,T2>
      {
      public:

         typedef T value_type;
         typedef T0oT1oT2_sf3ext<T, T0, T1, T2, SF3Operation> node_type;

         T0oT1oT2_sf3ext(T0 p0, T1 p1, T2 p2)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1oT2<T,T0,T1,T2>::result;
            return result;
         }

         inline operator_type operation()
         {
            return e_default;
         }

         inline T value() const exprtk_override
         {
            return SF3Operation::process(t0_, t1_, t2_);
         }

         T0 t0() const exprtk_override
         {
            return t0_;
         }

         T1 t1() const exprtk_override
         {
            return t1_;
         }

         T2 t2() const exprtk_override
         {
            return t2_;
         }

         std::string type_id() const exprtk_override
         {
            return id();
         }

         static inline std::string id()
         {
            return SF3Operation::id();
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator, T0 p0, T1 p1, T2 p2)
         {
            return allocator
                     .template allocate_type<node_type, T0, T1, T2>
                        (p0, p1, p2);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2_sf3ext)";
         }
      private:

         T0oT1oT2_sf3ext(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
      };

      template <typename T>
      inline bool is_sf3ext_node(const expression_node<T>* n)
      {
         switch (n->type())
         {
            case expression_node<T>::e_vovov : return true;
            case expression_node<T>::e_vovoc : return true;
            case expression_node<T>::e_vocov : return true;
            case expression_node<T>::e_covov : return true;
            case expression_node<T>::e_covoc : return true;
            default                          : return false;
         }
      }

      template <typename T, typename T0, typename T1, typename T2, typename T3>
      class T0oT1oT2oT3_sf4 exprtk_final : public T0oT1oT2_base_node<T>
      {
      public:

         typedef typename details::functor_t<T> functor_t;
         typedef typename functor_t::qfunc_t    qfunc_t;
         typedef T value_type;
         typedef T0oT1oT2oT3_sf4<T, T0, T1, T2, T3> node_type;

         T0oT1oT2oT3_sf4(T0 p0, T1 p1, T2 p2, T3 p3, const qfunc_t p4)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         , t3_(p3)
         , f_ (p4)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1oT2oT3<T,T0,T1,T2,T3>::result;
            return result;
         }

         inline operator_type operation() const exprtk_override
         {
            return e_default;
         }

         inline T value() const exprtk_override
         {
            return f_(t0_, t1_, t2_, t3_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline T2 t2() const
         {
            return t2_;
         }

         inline T3 t3() const
         {
            return t3_;
         }

         qfunc_t f() const
         {
            return f_;
         }

         std::string type_id() const
         {
            return id();
         }

         static inline std::string id()
         {
            return "sf4";
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator, T0 p0, T1 p1, T2 p2, T3 p3, qfunc_t p4)
         {
            return allocator
                     .template allocate_type<node_type, T0, T1, T2, T3, qfunc_t>
                        (p0, p1, p2, p3, p4);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2oT3_sf4)";
         }
      private:

         T0oT1oT2oT3_sf4(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
         T3 t3_;
         const qfunc_t f_;
      };

      template <typename T, typename T0, typename T1, typename T2, typename T3, typename SF4Operation>
      class T0oT1oT2oT3_sf4ext exprtk_final : public T0oT1oT2oT3_base_node<T>
      {
      public:

         typedef T value_type;
         typedef T0oT1oT2oT3_sf4ext<T, T0, T1, T2, T3, SF4Operation> node_type;

         T0oT1oT2oT3_sf4ext(T0 p0, T1 p1, T2 p2, T3 p3)
         : t0_(p0)
         , t1_(p1)
         , t2_(p2)
         , t3_(p3)
         {}

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            static const typename expression_node<T>::node_type result = nodetype_T0oT1oT2oT3<T,T0,T1,T2,T3>::result;
            return result;
         }

         inline T value() const exprtk_override
         {
            return SF4Operation::process(t0_, t1_, t2_, t3_);
         }

         inline T0 t0() const
         {
            return t0_;
         }

         inline T1 t1() const
         {
            return t1_;
         }

         inline T2 t2() const
         {
            return t2_;
         }

         inline T3 t3() const
         {
            return t3_;
         }

         std::string type_id() const exprtk_override
         {
            return id();
         }

         static inline std::string id()
         {
            return SF4Operation::id();
         }

         template <typename Allocator>
         static inline expression_node<T>* allocate(Allocator& allocator, T0 p0, T1 p1, T2 p2, T3 p3)
         {
            return allocator
                     .template allocate_type<node_type, T0, T1, T2, T3>
                        (p0, p1, p2, p3);
         }

         inline std::string to_string() const exprtk_override{
            return "(T0oT1oT2oT3_sf4ext)";
         }
      private:

         T0oT1oT2oT3_sf4ext(const node_type&) exprtk_delete;
         node_type& operator=(const node_type&) exprtk_delete;

         T0 t0_;
         T1 t1_;
         T2 t2_;
         T3 t3_;
      };

      template <typename T>
      inline bool is_sf4ext_node(const expression_node<T>* n)
      {
         switch (n->type())
         {
            case expression_node<T>::e_vovovov : return true;
            case expression_node<T>::e_vovovoc : return true;
            case expression_node<T>::e_vovocov : return true;
            case expression_node<T>::e_vocovov : return true;
            case expression_node<T>::e_covovov : return true;
            case expression_node<T>::e_covocov : return true;
            case expression_node<T>::e_vocovoc : return true;
            case expression_node<T>::e_covovoc : return true;
            case expression_node<T>::e_vococov : return true;
            default                            : return false;
         }
      }

      template <typename T, typename T0, typename T1>
      struct T0oT1_define
      {
         typedef details::T0oT1<T, T0, T1> type0;
      };

      template <typename T, typename T0, typename T1, typename T2>
      struct T0oT1oT2_define
      {
         typedef details::T0oT1oT2<T, T0, T1, T2, typename T0oT1oT2process<T>::mode0> type0;
         typedef details::T0oT1oT2<T, T0, T1, T2, typename T0oT1oT2process<T>::mode1> type1;
         typedef details::T0oT1oT2_sf3<T, T0, T1, T2> sf3_type;
         typedef details::sf3ext_type_node<T, T0, T1, T2> sf3_type_node;
      };

      template <typename T, typename T0, typename T1, typename T2, typename T3>
      struct T0oT1oT2oT3_define
      {
         typedef details::T0oT1oT2oT3<T, T0, T1, T2, T3, typename T0oT1oT20T3process<T>::mode0> type0;
         typedef details::T0oT1oT2oT3<T, T0, T1, T2, T3, typename T0oT1oT20T3process<T>::mode1> type1;
         typedef details::T0oT1oT2oT3<T, T0, T1, T2, T3, typename T0oT1oT20T3process<T>::mode2> type2;
         typedef details::T0oT1oT2oT3<T, T0, T1, T2, T3, typename T0oT1oT20T3process<T>::mode3> type3;
         typedef details::T0oT1oT2oT3<T, T0, T1, T2, T3, typename T0oT1oT20T3process<T>::mode4> type4;
         typedef details::T0oT1oT2oT3_sf4<T, T0, T1, T2, T3> sf4_type;
      };

      template <typename T, typename Operation>
      class vov_node exprtk_final : public vov_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef Operation operation_t;

         // variable op variable node
         explicit vov_node(const T& var0, const T& var1)
         : v0_(var0)
         , v1_(var1)
         {}

         inline T value() const exprtk_override
         {
            return Operation::process(v0_,v1_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return Operation::type();
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T& v0() const exprtk_override
         {
            return v0_;
         }

         inline const T& v1() const exprtk_override
         {
            return v1_;
         }

         inline std::string to_string() const exprtk_override{
            return "(vov_node)";
         }
      protected:

         const T& v0_;
         const T& v1_;

      private:

         vov_node(const vov_node<T,Operation>&) exprtk_delete;
         vov_node<T,Operation>& operator=(const vov_node<T,Operation>&) exprtk_delete;
      };

      template <typename T, typename Operation>
      class cov_node exprtk_final : public cov_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef Operation operation_t;

         // constant op variable node
         explicit cov_node(const T& const_var, const T& var)
         : c_(const_var)
         , v_(var)
         {}

         inline T value() const exprtk_override
         {
            return Operation::process(c_,v_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return Operation::type();
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T c() const exprtk_override
         {
            return c_;
         }

         inline const T& v() const exprtk_override
         {
            return v_;
         }

         inline std::string to_string() const exprtk_override{
            return "(cov_node)";
         }
      protected:

         const T  c_;
         const T& v_;

      private:

         cov_node(const cov_node<T,Operation>&) exprtk_delete;
         cov_node<T,Operation>& operator=(const cov_node<T,Operation>&) exprtk_delete;
      };

      template <typename T, typename Operation>
      class voc_node exprtk_final : public voc_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef Operation operation_t;

         // variable op constant node
         explicit voc_node(const T& var, const T& const_var)
         : v_(var)
         , c_(const_var)
         {}

         inline T value() const exprtk_override
         {
            return Operation::process(v_,c_);
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T c() const exprtk_override
         {
            return c_;
         }

         inline const T& v() const exprtk_override
         {
            return v_;
         }

         inline std::string to_string() const exprtk_override{
            return "(voc_node)";
         }
      protected:

         const T& v_;
         const T  c_;

      private:

         voc_node(const voc_node<T,Operation>&) exprtk_delete;
         voc_node<T,Operation>& operator=(const voc_node<T,Operation>&) exprtk_delete;
      };

      template <typename T, typename Operation>
      class vob_node exprtk_final : public vob_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;
         typedef Operation operation_t;

         // variable op constant node
         explicit vob_node(const T& var, const expression_ptr branch)
         : v_(var)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return Operation::process(v_,branch_.first->value());
         }

         inline const T& v() const exprtk_override
         {
            return v_;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(vob_node)";
         }
      private:

         vob_node(const vob_node<T,Operation>&) exprtk_delete;
         vob_node<T,Operation>& operator=(const vob_node<T,Operation>&) exprtk_delete;

         const T& v_;
         branch_t branch_;
      };

      template <typename T, typename Operation>
      class bov_node exprtk_final : public bov_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;
         typedef Operation operation_t;

         // variable op constant node
         explicit bov_node(const expression_ptr branch, const T& var)
         : v_(var)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return Operation::process(branch_.first->value(),v_);
         }

         inline const T& v() const exprtk_override
         {
            return v_;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(bov_node)";
         }
      private:

         bov_node(const bov_node<T,Operation>&) exprtk_delete;
         bov_node<T,Operation>& operator=(const bov_node<T,Operation>&) exprtk_delete;

         const T& v_;
         branch_t branch_;
      };

      template <typename T, typename Operation>
      class cob_node exprtk_final : public cob_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;
         typedef Operation operation_t;

         // variable op constant node
         explicit cob_node(const T const_var, const expression_ptr branch)
         : c_(const_var)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return Operation::process(c_,branch_.first->value());
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T c() const exprtk_override
         {
            return c_;
         }

         inline void set_c(const T new_c) exprtk_override
         {
            (*const_cast<T*>(&c_)) = new_c;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         inline expression_node<T>* move_branch(const std::size_t&) exprtk_override
         {
            branch_.second = false;
            return branch_.first;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(cob_node)";
         }
      private:

         cob_node(const cob_node<T,Operation>&) exprtk_delete;
         cob_node<T,Operation>& operator=(const cob_node<T,Operation>&) exprtk_delete;

         const T  c_;
         branch_t branch_;
      };

      template <typename T, typename Operation>
      class boc_node exprtk_final : public boc_base_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr,bool> branch_t;
         typedef Operation operation_t;

         // variable op constant node
         explicit boc_node(const expression_ptr branch, const T const_var)
         : c_(const_var)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return Operation::process(branch_.first->value(),c_);
         }

         inline operator_type operation() const exprtk_override
         {
            return Operation::operation();
         }

         inline const T c() const exprtk_override
         {
            return c_;
         }

         inline void set_c(const T new_c) exprtk_override
         {
            (*const_cast<T*>(&c_)) = new_c;
         }

         inline expression_node<T>* branch(const std::size_t&) const exprtk_override
         {
            return branch_.first;
         }

         inline expression_node<T>* move_branch(const std::size_t&) exprtk_override
         {
            branch_.second = false;
            return branch_.first;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(boc_node)";
         }
      private:

         boc_node(const boc_node<T,Operation>&) exprtk_delete;
         boc_node<T,Operation>& operator=(const boc_node<T,Operation>&) exprtk_delete;

         const T  c_;
         branch_t branch_;
      };

      template <typename T, typename PowOp>
      class ipow_node exprtk_final: public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef PowOp operation_t;

         explicit ipow_node(const T& v)
         : v_(v)
         {}

         inline T value() const exprtk_override
         {
            return PowOp::result(v_);
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_ipow;
         }

         inline std::string to_string() const exprtk_override{
            return "(ipow_node)";
         }
      private:

         ipow_node(const ipow_node<T,PowOp>&) exprtk_delete;
         ipow_node<T,PowOp>& operator=(const ipow_node<T,PowOp>&) exprtk_delete;

         const T& v_;
      };

      template <typename T, typename PowOp>
      class bipow_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr, bool> branch_t;
         typedef PowOp operation_t;

         explicit bipow_node(expression_ptr branch)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return PowOp::result(branch_.first->value());
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_ipow;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(bipow_node)";
         }
      private:

         bipow_node(const bipow_node<T,PowOp>&) exprtk_delete;
         bipow_node<T,PowOp>& operator=(const bipow_node<T,PowOp>&) exprtk_delete;

         branch_t branch_;
      };

      template <typename T, typename PowOp>
      class ipowinv_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef PowOp operation_t;

         explicit ipowinv_node(const T& v)
         : v_(v)
         {}

         inline T value() const exprtk_override
         {
            return (T(1) / PowOp::result(v_));
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_ipowinv;
         }

         inline std::string to_string() const exprtk_override{
            return "(ipowinv_node)";
         }
      private:

         ipowinv_node(const ipowinv_node<T,PowOp>&) exprtk_delete;
         ipowinv_node<T,PowOp>& operator=(const ipowinv_node<T,PowOp>&) exprtk_delete;

         const T& v_;
      };

      template <typename T, typename PowOp>
      class bipowninv_node exprtk_final : public expression_node<T>
      {
      public:

         typedef expression_node<T>* expression_ptr;
         typedef std::pair<expression_ptr, bool> branch_t;
         typedef PowOp operation_t;

         explicit bipowninv_node(expression_ptr branch)
         {
            construct_branch_pair(branch_, branch);
         }

         inline T value() const exprtk_override
         {
            assert(branch_.first);
            return (T(1) / PowOp::result(branch_.first->value()));
         }

         inline typename expression_node<T>::node_type type() const exprtk_override
         {
            return expression_node<T>::e_ipowinv;
         }

         void collect_nodes(typename expression_node<T>::noderef_list_t& node_delete_list) exprtk_override
         {
            expression_node<T>::ndb_t::template collect(branch_, node_delete_list);
         }

         std::size_t node_depth() const exprtk_override
         {
            return expression_node<T>::ndb_t::compute_node_depth(branch_);
         }

         inline std::string to_string() const exprtk_override{
            return "(bipowninv_node)";
         }
      private:

         bipowninv_node(const bipowninv_node<T,PowOp>&) exprtk_delete;
         bipowninv_node<T,PowOp>& operator=(const bipowninv_node<T,PowOp>&) exprtk_delete;

         branch_t branch_;
      };

      template <typename T>
      inline bool is_vov_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const vov_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_cov_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const cov_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_voc_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const voc_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_cob_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const cob_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_boc_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const boc_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_t0ot1ot2_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const T0oT1oT2_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_t0ot1ot2ot3_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const T0oT1oT2oT3_base_node<T>*>(node));
      }

      template <typename T>
      inline bool is_uv_node(const expression_node<T>* node)
      {
         return (0 != dynamic_cast<const uv_base_node<T>*>(node));
      }
    }
}
