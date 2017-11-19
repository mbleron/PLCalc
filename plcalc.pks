create or replace package plcalc is
/* ======================================================================================

  MIT License

  Copyright (c) 2013-2017 Marc Bleron

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

=========================================================================================
    Change history :
    Marc Bleron       2013-02-19 - add serialize function
                                   fix bugs
    Marc Bleron       2013-02-21 - support for conditional and logical
                                   operators via "IF" function
    Marc Bleron       2013-03-16 - fix for "negative constant" bug :
                                   support for unary minus operator via internal
                                   "NEG" operator
    Marc Bleron       2013-06-14 - fix bug on unary minus operator
    Marc Bleron       2013-07-30 - add NULL handling
    Marc Bleron       2014-05-01 - new regex-free tokenizer
                                   new recursive descent parser based on EBNF grammar
                                   new "type-aware" evaluator
    Marc Bleron       2014-11-07 - new serializer with optional SQL output
    Marc Bleron       2015-03-31 - add T_STRING token type
                                   add extended expression "declare (...) return ..."
    Marc Bleron       2015-07-31 - add support for functions with variable number of
                                   arguments
    Marc Bleron       2016-07-01 - major refactoring and modularization
                                   add compilation to binary format
                                   add conversion to MathML (experimental)
    Marc Bleron       2017-11-19 - fix for issue #8 : serialization of operator and rhs
                                   operand having the same precedence gives wrong result
                                 - add SERIALIZE_NO_WS option to control spacing around
                                   operator
====================================================================================== */

  NULL_INF_OR_NAN        constant number := 0;
  KEEP_INF_OR_NAN        constant number := 1;
  
  NO_VALIDATE            constant number := 0;
  VALIDATE               constant number := 1;
  COMPILE_TLIST          constant number := 0;
  COMPILE_BINARY         constant number := 1;
  
  -- serialize options
  SERIALIZE_INFIX        constant binary_integer := 0;
  SERIALIZE_SQL          constant binary_integer := 1;
  SERIALIZE_NO_WS        constant binary_integer := 8;

  function tokenize (
    p_expr in varchar2
  ) 
  return plc_token_list deterministic ;


  function compile (
    p_expr    in varchar2
  , p_options in number default VALIDATE
  )
  return plc_token_list deterministic ;


  function compileBinary (
    p_expr    in varchar2
  , p_options in number default VALIDATE
  )
  return raw deterministic ;


  function eval (
    tlist   in plc_token_list
  , p_vars  in plc_bind_list default plc_bind_list()
  , p_flags in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic ;


  function eval (
    rawstream in raw
  , p_vars    in plc_bind_list default plc_bind_list()
  , p_flags   in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic ;

  
  function eval (
    p_expr    in varchar2
  , p_vars    in plc_bind_list default plc_bind_list()
  , p_options in number        default VALIDATE
  , p_flags   in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic ;

  
  function readBinaryStream (
    p_expr in raw
  )
  return plc_token_list pipelined;

  
  function serialize (
    tlist     in plc_token_list
  , p_options in binary_integer default SERIALIZE_INFIX
  )
  return varchar2 deterministic ;


  -- experimental
  -- convert a compiled RPN expression into Presentation MathML fragment
  function to_MathML (
    tlist        in plc_token_list
  , as_document  in binary_integer default 1
  )
  return varchar2 deterministic ;

  
  function to_infix (
    tlist in plc_token_list
  )
  return plc_token_list deterministic ;

end plcalc;
/
