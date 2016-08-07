create or replace package plcalc is
/* ======================================================================================

  MIT License

  Copyright (c) 2013-2016 Marc Bleron

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
====================================================================================== */

  NULL_INF_OR_NAN        constant number := 0;
  KEEP_INF_OR_NAN        constant number := 1;
  
  NO_VALIDATE            constant number := 0;
  VALIDATE               constant number := 1;
  COMPILE_TLIST          constant number := 0;
  COMPILE_BINARY         constant number := 1;
  
  SERIALIZE_INFIX        constant binary_integer := 0;
  SERIALIZE_SQL          constant binary_integer := 1;


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
create or replace package body plcalc is

  OP_MINUS      constant binary_integer := 1; -- -
  OP_PLUS       constant binary_integer := 2; -- +
  OP_MUL        constant binary_integer := 3; -- *
  OP_DIV        constant binary_integer := 4; -- /
  OP_EXP        constant binary_integer := 5; -- ^
  OP_MOD        constant binary_integer := 6; -- %
  OP_EQ         constant binary_integer := 7; -- =
  OP_LT         constant binary_integer := 8; -- <
  OP_GT         constant binary_integer := 9; -- >
  OP_LE         constant binary_integer := 10; -- <=
  OP_GE         constant binary_integer := 11; -- >=
  OP_NE         constant binary_integer := 12; -- !=
  OP_AND        constant binary_integer := 13; -- AND
  OP_OR         constant binary_integer := 14; -- OR
  OP_NOT        constant binary_integer := 15; -- NOT
  OP_UMINUS     constant binary_integer := 16; -- -
  OP_ASSIGN     constant binary_integer := 17; -- :=

  T_LEFT        constant binary_integer := 30; -- (
  T_RIGHT       constant binary_integer := 31; -- )
  T_COMMA       constant binary_integer := 32; -- ,
  T_AT          constant binary_integer := 33; -- @
  T_DOT         constant binary_integer := 34; -- .
  T_DECL        constant binary_integer := 35; -- declare
  T_RETURN      constant binary_integer := 36; -- return
  T_EOF         constant binary_integer := -1; -- end-of-file

  T_ARGC        constant binary_integer := 39;
  T_NUMBER      constant binary_integer := 40;
  T_IDENT       constant binary_integer := 41;
  T_FUNC        constant binary_integer := 42;
  T_PROP        constant binary_integer := 43;
  T_CONST       constant binary_integer := 44;
  T_STRING      constant binary_integer := 45;

  SEPARATOR_MISPLACED     constant varchar2(100) := 'Compilation error at position %d : separator misplaced or parentheses mismatched';
  PARENTHESES_MISMATCHED  constant varchar2(100) := 'Compilation error at position %d : parentheses mismatched';
  UNEXPECTED_SYMBOL       constant varchar2(100) := 'Syntax error at position %d : unexpected symbol ''%s''';
  UNEXPECTED_INSTEAD_OF   constant varchar2(100) := 'Syntax error at position %d : symbol ''%s'' found instead of ''%s''';
  FUNCTION_NO_ARGUMENT    constant varchar2(100) := 'Syntax error at position %d : function ''%s'' does not take any argument';
  FUNCTION_EXPECTS_ARG    constant varchar2(100) := 'Syntax error at position %d : function ''%s'' expects %d argument%s';
  FUNCTION_EXPECTS_LEAST  constant varchar2(100) := 'Syntax error at position %d : function ''%s'' expects at least %d argument%s';
  LEX_INVALID_CHAR        constant varchar2(100) := 'Lexical error at position %d : invalid character ''%s''';
  LEX_INVALID_NUMBER      constant varchar2(100) := 'Lexical error at position %d : invalid number ''%s''';
  
  C_PI                    constant binary_double := acos(-1d);

  type op_info  is record (argc binary_integer, prec binary_integer, assoc binary_integer);
  type op_map   is table of op_info        index by binary_integer;
  type fnmap    is table of simple_integer index by varchar2(30);
  type tmap     is table of varchar2(256)  index by binary_integer;
  type sqlmap   is table of varchar2(4000) index by varchar2(30);
  type keymap   is table of binary_double  index by varchar2(30);

  type string_tokenizer is record (expr varchar2(4000), pos binary_integer, token plc_token, options binary_integer);
  type token_reader is record (c_mode binary_integer, tlist plc_token_list, rawstream raw(2000), pos binary_integer, len binary_integer);
  type rpn_compiler is record (c_mode binary_integer, st plc_stack, output plc_stack, arg_cnt binary_integer, rawstream raw(2000));

  op   op_map;
  fnc  fnmap;
  tm   tmap;
  sqlm sqlmap;
  decimal_sep   varchar2(1);

  parse_exception  exception;
  pragma exception_init(parse_exception, -20000);

  function createOpInfo(p_argc simple_integer, p_prec simple_integer, p_assoc simple_integer) return op_info
  is
    r op_info;
  begin
    r.argc := p_argc;
    r.prec := p_prec;
    r.assoc := p_assoc;
    return r;
  end;

  procedure initSQLMap
  is
  begin

    sqlm('PI')     := 'acos(-1d)';
    sqlm('%')      := 'mod(:1, :2)';
    sqlm('^')      := 'power(:1, :2)';
    sqlm('IF')     := 'case when :1 then :2 else :3 end';
    sqlm('ISNULL') := ':1 is null';
    sqlm('NULLIF') := 'nullif(:1, :2)';
    sqlm('IFNULL') := 'nvl(:1, :2)';
    sqlm('ABS')    := 'abs(:1)';
    sqlm('MAX')    := 'greatest(:args)';
    sqlm('MIN')    := 'least(:args)';
    sqlm('COS')    := 'cos(:1)';
    sqlm('SIN')    := 'sin(:1)';
    sqlm('TAN')    := 'tan(:1)';
    sqlm('SQRT')   := 'sqrt(:1)';
    sqlm('EXP')    := 'exp(:1)';
    sqlm('LN')     := 'ln(:1)';
    sqlm('LOG')    := 'log(:1, :2)';
    sqlm('CEIL')   := 'ceil(:1)';
    sqlm('FLOOR')  := 'floor(:1)';
    sqlm('ROUND')  := 'round(:1, :2)';

  end;

  procedure initMaps is
  begin
    -- function identifier and argument count
    fnc('ABS')    := 1;
    fnc('COS')    := 1;
    fnc('SIN')    := 1;
    fnc('TAN')    := 1;
    fnc('SQRT')   := 1;
    fnc('EXP')    := 1;
    fnc('LN')     := 1;
    fnc('LOG')    := 2;
    fnc('CEIL')   := 1;
    fnc('FLOOR')  := 1;
    fnc('ROUND')  := 2;
    fnc('IF')     := 3;
    fnc('ISNULL') := 1;
    fnc('NULLIF') := 2;
    fnc('IFNULL') := 2;
    fnc('SUM')    := 4;
    -- variable-args functions
    fnc('MIN')    := -2;
    fnc('MAX')    := -2;
    
    -- operator argument count, precedence and associativity (0=left, 1=right)
    op(OP_ASSIGN) := createOpInfo(2,0,1);
    op(OP_OR)     := createOpInfo(2,1,0);
    op(OP_AND)    := createOpInfo(2,2,0);
    op(OP_NOT)    := createOpInfo(1,3,1);
    op(OP_EQ)     := createOpInfo(2,4,0);
    op(OP_LT)     := createOpInfo(2,4,0);
    op(OP_LE)     := createOpInfo(2,4,0);
    op(OP_GT)     := createOpInfo(2,4,0);
    op(OP_GE)     := createOpInfo(2,4,0);
    op(OP_NE)     := createOpInfo(2,4,0);
    op(OP_PLUS)   := createOpInfo(2,5,0);
    op(OP_MINUS)  := createOpInfo(2,5,0);
    op(OP_MUL)    := createOpInfo(2,6,0);
    op(OP_DIV)    := createOpInfo(2,6,0);
    op(OP_MOD)    := createOpInfo(2,6,0);
    op(OP_UMINUS) := createOpInfo(1,7,1);
    op(OP_EXP)    := createOpInfo(2,10,1);

    tm(OP_MINUS)  := '-';
    tm(OP_PLUS)   := '+';
    tm(OP_MUL)    := '*';
    tm(OP_DIV)    := '/';
    tm(OP_EXP)    := '^';
    tm(OP_MOD)    := '%';
    tm(OP_EQ)     := '=';
    tm(OP_LT)     := '<';
    tm(OP_GT)     := '>';
    tm(OP_LE)     := '<=';
    tm(OP_GE)     := '>=';
    tm(OP_NE)     := '!=';
    tm(OP_AND)    := 'AND';
    tm(OP_OR)     := 'OR';
    tm(OP_NOT)    := 'NOT';
    tm(OP_UMINUS) := '-';
    tm(OP_ASSIGN) := ':=';
    tm(T_LEFT)    := '(';
    tm(T_RIGHT)   := ')';
    tm(T_IDENT)   := '<identifier>';
    tm(T_AT)      := '@';
    tm(T_DOT)     := '.';
    tm(T_PROP)    := '<property>';
    tm(T_DECL)    := 'declare';
    tm(T_RETURN)  := 'return';
    tm(T_EOF)     := '<eof>';
    
    initSQLMap;
    
  end ;


  function getTokenType (p_typename in varchar2) return integer deterministic
  is
  begin
    return case p_typename
           when 'T_IDENT' then T_IDENT
           end;
  end;


  function getSQLExpr (op in varchar2)
  return varchar2 deterministic
  is
  begin
    return case when sqlm.exists(op) then sqlm(op) else op end;
  end;


  function getDecimalSeparator
  return varchar2
  is
    output  varchar2(1);
  begin
    select substr(value, 1, 1)
    into output
    from v$nls_parameters
    where parameter = 'NLS_NUMERIC_CHARACTERS';
  
    return output;
  end;


  function fn_min (r in out nocopy plc_token_list, j in binary_integer, opcnt in binary_integer, ignore_nulls in boolean default false) return binary_double is
    output  binary_double;
  begin

    for k in 1 .. opcnt loop
      if ignore_nulls then
        if r(j-k).numval < output or output is null then
          output := r(j-k).numval;
        end if;
      elsif r(j-k).numval is null then 
        output := null;
        exit;
      elsif r(j-k).numval < output or k = 1 then
        output := r(j-k).numval;
      end if;
    end loop;

    return output;
  end;


  function fn_max (r in out nocopy plc_token_list, j in binary_integer, opcnt in binary_integer, ignore_nulls in boolean default false) return binary_double is
    output  binary_double; 
  begin
    for k in 1 .. opcnt loop
      if ignore_nulls then
        if r(j-k).numval > output or output is null then
          output := r(j-k).numval;
        end if;
      elsif r(j-k).numval is null then 
        output := null;
        exit;
      elsif r(j-k).numval > output or k = 1 then
        output := r(j-k).numval;
      end if;
    end loop;
    return output;
  end;


  procedure error (
    p_message in varchar2
  , p_arg1 in varchar2 default null
  , p_arg2 in varchar2 default null
  , p_arg3 in varchar2 default null
  , p_arg4 in varchar2 default null
  ) 
  is
  begin
    raise_application_error(-20000, utl_lms.format_message(p_message, p_arg1, p_arg2, p_arg3, p_arg4));
  end;


  function createTokenizer (
    p_expr in varchar2
  , p_options in binary_integer default null
  )
  return string_tokenizer
  is
    tokenizer  string_tokenizer;
  begin
    decimal_sep := getDecimalSeparator();
    tokenizer.expr := p_expr;
    tokenizer.pos := 0;
    tokenizer.options := p_options;
    return tokenizer;
  end;


  function readToken (
    tokenizer in out nocopy string_tokenizer 
  )
  return plc_token
  is
    tmp    varchar2(30);
    pos    simple_integer := 0;
    c      varchar2(1 char);
    token  plc_token := plc_token();
    ttype  binary_integer;

    procedure set_token (
      p_type in binary_integer
    , p_strval in varchar2
    , p_pos in binary_integer default null
    ) 
    is
    begin
      token.type := p_type;
      case p_type
      when T_NUMBER then
        token.strval := to_char(to_number(replace(p_strval, '.', decimal_sep)), 'TM9', 'nls_numeric_characters=.,');
        token.numval := to_binary_double(replace(p_strval, '.', decimal_sep));
      when T_EOF then
        token.strval := tm(T_EOF);
      else
        token.strval := p_strval;
      end case;
      token.position := nvl(p_pos, tokenizer.pos);
      tokenizer.token := token;
    exception
      when value_error then  
        error(LEX_INVALID_NUMBER, p_pos, p_strval);
    end;
    
    function getc return varchar2 is
    begin
      tokenizer.pos := tokenizer.pos + 1;
      return substr(tokenizer.expr, tokenizer.pos, 1);
    end;
    
    function look_ahead return varchar2 is
    begin
      return substr(tokenizer.expr, tokenizer.pos + 1, 1); 
    end;

    procedure back is
    begin
      tokenizer.pos := tokenizer.pos - 1;
    end;
    
  begin

    c := getc;
    -- strip whitespaces
    while c in (chr(9), chr(10), chr(13), chr(32)) loop
      c := getc;
    end loop;
    
    if c is null then
      set_token(T_EOF, c);
    else
      tmp := null;
      case c
        when '(' then
          set_token(T_LEFT, c);
        when ')' then
          set_token(T_RIGHT, c);
        when '+' then
          set_token(OP_PLUS, c);
        when '*' then
          set_token(OP_MUL, c);
        when '-' then
          if tokenizer.token.type in (T_IDENT, T_NUMBER, T_RIGHT, T_FUNC) then
            ttype := OP_MINUS;
          else
            ttype := OP_UMINUS;
          end if;
          set_token(ttype, c);
        when '/' then
          set_token(OP_DIV, c);
        when '%' then
          set_token(OP_MOD, c);
        when '^' then
          set_token(OP_EXP, c);
        when '=' then
          set_token(OP_EQ, c);
        when ',' then
          set_token(T_COMMA, c);
          
        when '!' then
          tmp := c;
          c := getc;
          if c = '=' then
            tmp := tmp || c;
            set_token(OP_NE, tmp, tokenizer.pos-1);
          else 
            error(LEX_INVALID_CHAR, tokenizer.pos-1, tmp);
          end if;
          
        when '<' then
          tmp := c;
          ttype := OP_LT;
          pos := tokenizer.pos;
          c := getc;
          if c = '=' then
            tmp := tmp || c;
            ttype := OP_LE;
          else 
            back();
          end if;
          set_token(ttype, tmp, pos);
          
        when '>' then
          tmp := c;
          ttype := OP_GT;
          pos := tokenizer.pos;
          c := getc;
          if c = '=' then
            tmp := tmp || c;
            ttype := OP_GE;
          else
            back();
          end if;
          set_token(ttype, tmp, pos);
          
        when '"' then
         
          tmp := c;
          pos := tokenizer.pos;
          c := getc;
          while c is not null loop
            exit when c = '"';
            tmp := tmp || c;
            c := getc;
          end loop;
          if c is null then
            error(LEX_INVALID_CHAR, tokenizer.pos-1, substr(tmp,-1));
          else
            set_token(T_IDENT, substr(tmp,2), pos);
          end if;

        when '''' then
          tmp := null;
          pos := tokenizer.pos;
          c := getc;
          while c is not null loop
            if c = '''' then 
              if look_ahead() = c then
                c := getc;
              else
                exit;
              end if;
            end if;
            tmp := tmp || c;
            c := getc;
          end loop;
          if c is null then
            error(UNEXPECTED_SYMBOL, tokenizer.pos, tm(T_EOF));
          else
            set_token(T_STRING, tmp, pos);
          end if;

        when ':' then
          tmp := c;
          c := getc;
          if c = '=' then
            tmp := tmp || c;
            set_token(OP_ASSIGN, tmp, tokenizer.pos-1);
          else 
            error(LEX_INVALID_CHAR, tokenizer.pos-1, tmp);
          end if;
          
        when '#' then
        
          c := getc;
          while c is not null loop
            exit when c = chr(10);
            c := getc;
          end loop;         
        
        else
         
          case  
          when c between '0' and '9' then
            tmp := c;
            pos := tokenizer.pos;
            c := getc;
            while c between '0' and '9' loop
              tmp := tmp || c;
              c := getc;
            end loop;
            if c = '.' then
              tmp := tmp || c;
              c := getc;
              while c between '0' and '9' loop
                tmp := tmp || c;
                c := getc;
              end loop;
            end if;
            if c = 'e' or c = 'E' then
              tmp := tmp || c;
              c := getc;  
              if c = '-' or c = '+' then
                tmp := tmp || c;
                c := getc;
              end if;
              if c between '0' and '9' then
                tmp := tmp || c;
                c := getc;
                while c between '0' and '9' loop
                  tmp := tmp || c;
                  c := getc;
                end loop; 
              else
                error(LEX_INVALID_NUMBER, pos, tmp);
              end if;         
            end if;
            back();
            set_token(T_NUMBER, tmp, pos);

          when c between 'A' and 'Z' 
            or c between 'a' and 'z' then
           
            tmp := c;
            pos := tokenizer.pos;
            c := getc;
            while c between 'A' and 'Z'
               or c between 'a' and 'z'
               or c between '0' and '9'
               or c = '_' 
            loop
              tmp := tmp || c;
              c := getc;
            end loop;
            back();
            
            tmp := upper(tmp);
            
            if fnc.exists(tmp) then
              ttype := T_FUNC;
            elsif tmp in ('NULL','PI') then
              ttype := T_CONST;
            else
              ttype := 
                case tmp
                  when 'AND'     then OP_AND
                  when 'OR'      then OP_OR
                  when 'NOT'     then OP_NOT
                  when 'DECLARE' then T_DECL
                  when 'RETURN'  then T_RETURN
                  else T_IDENT
                end;
            end if;
            
            set_token(ttype, tmp, pos); 
          
          else
           
            error(LEX_INVALID_CHAR, tokenizer.pos, c);
            
          end case;
          
      end case;
      
    end if;
    
    return token;
  
  end;
  

  function tokenize (p_expr in varchar2) return plc_token_list
  is
    tokenizer  string_tokenizer := createTokenizer(p_expr);
    token      plc_token;
    tlist      plc_token_list := plc_token_list();
  begin
    token := readToken(tokenizer);
    while token.type != T_EOF loop
      tlist.extend;
      tlist(tlist.last) := token;
      token := readToken(tokenizer);
    end loop;
    return tlist;
  end;
  

  function encodeToken (
    token in plc_token
  )
  return raw
  is
    output raw(2000);
    
    procedure push (input in raw) is
    begin
      output := utl_raw.concat(output, input);
    end;

  begin

    push(utl_raw.substr(utl_raw.cast_from_binary_integer(token.type),-1));
    
    if token.type in (T_IDENT, T_FUNC, T_CONST, T_STRING) then
      push(utl_raw.substr(utl_raw.cast_from_binary_integer(lengthb(token.strval)),-1));
      push(utl_raw.cast_to_raw(token.strval));
    elsif token.type = T_NUMBER then
      push(utl_raw.cast_from_binary_double(token.numval));
    elsif token.type = T_ARGC then
      push(utl_raw.substr(utl_raw.cast_from_binary_integer(token.numval),-1));
    end if;
    
    return output;
    
  end;


  procedure enqueueRPNToken (
    cmp      in out nocopy rpn_compiler 
  , token    in plc_token
  )
  is
    top          plc_token;
    arg_cnt_tmp  binary_integer;
    
    procedure push (p_token in plc_token) is
    begin
      if cmp.c_mode = COMPILE_TLIST then
        cmp.output.push(p_token);
      else
        cmp.rawstream := utl_raw.concat(cmp.rawstream, encodeToken(p_token));
      end if;
    end;
    
  begin
    
    if token.type != T_RIGHT and cmp.arg_cnt = 0 then
      cmp.arg_cnt := 1;
    end if;

    case
    when token.type = T_NUMBER then
      
       push(token);

    when token.type = T_COMMA then
       
       cmp.arg_cnt := cmp.arg_cnt + 1;

       loop
         if cmp.st.isEmpty then
           error(SEPARATOR_MISPLACED, token.position);
         end if;
         top := cmp.st.peek;
         exit when top.type = T_LEFT;
         push(top);
         cmp.st.pop;
       end loop;

    when token.type between OP_MINUS and OP_ASSIGN then

       loop

         exit when cmp.st.isEmpty;
         top := cmp.st.peek;
         if op.exists(top.type)
            and (
                ( op(token.type).assoc = 0 and op(token.type).prec <= op(top.type).prec )
             or op(token.type).prec < op(top.type).prec
            )
         then
           push(top);
           cmp.st.pop;
         else
           exit;
         end if;
       end loop;

       cmp.st.push(token);

    when token.type = T_IDENT then
       
       push(token);
        
    when token.type = T_FUNC then
        
       if fnc(token.strval) = 0 then
         push(token);
       else
         cmp.st.push(token);
       end if;

    when token.type = T_LEFT then
      
       cmp.st.push(new plc_token(T_NUMBER, null, cmp.arg_cnt, null));
       cmp.arg_cnt := 0;

       cmp.st.push(token);

    when token.type = T_RIGHT then

      loop
        if cmp.st.isEmpty then
           error(PARENTHESES_MISMATCHED, token.position);
        end if;
        top := cmp.st.peek;
        exit when top.type = T_LEFT;
        push(top);
        cmp.st.pop;
      end loop;
      
      arg_cnt_tmp := cmp.arg_cnt;
      cmp.st.pop;
      cmp.arg_cnt := cmp.st.pop().numval;

      if not(cmp.st.isEmpty) then
        top := cmp.st.peek;

        if top.type = T_FUNC then
          
          if fnc(top.strval) < 0 then
            push(new plc_token(T_ARGC, null, arg_cnt_tmp, null));
          end if;
          
          push(top);
          cmp.st.pop;
        end if;
      end if;
          
    when token.type = T_CONST then
         
      push(token);
          
    when token.type = T_STRING then
         
      push(token);

    when token.type = T_EOF then

      loop
        exit when cmp.st.isEmpty;
        top := cmp.st.peek;
        if top.type = T_LEFT then
          error(PARENTHESES_MISMATCHED, top.position);
        end if;
        push(top);
        cmp.st.pop;
      end loop;
      
    else
      
      null;

    end case;

  end;


  procedure parse (
    tokenizer in out nocopy string_tokenizer
  , compiler  in out nocopy rpn_compiler
  )
  is
    ttype     binary_integer;
    pos       binary_integer;
    curr      varchar2(30);
    fn_name   varchar2(30);
    strval    varchar2(30);
    token     plc_token;

    procedure next_token is
    begin
      token := readToken(tokenizer);
      ttype := token.type;
      strval := token.strval;
      pos := token.position;
      if compiler.c_mode is not null then
        enqueueRPNToken(compiler, token);
      end if;
    end;

    function accept (t in binary_integer) return boolean is
    begin
      if ttype = t then
        curr := strval;
        next_token;
        return true;
      else
        return false;
      end if;
    end;
    
    procedure expect (t in binary_integer) is
    begin
      if not accept(t) then
        -- Error at position %d : unexpected symbol '%s' instead of '%s'
        error(UNEXPECTED_INSTEAD_OF, pos, strval, tm(t));
      end if;
    end;
    
    procedure expr;
    procedure extended_expr;
     
    -- boolean_factor ::= expr [ relational_op expr ]
    procedure boolean_factor is
    begin
      expr;
      if ttype in (OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE) then
        next_token;
        expr;
      end if;
    end;

    -- boolean_term ::= boolean_factor { "and" boolean_factor }
    procedure boolean_term is
    begin
      boolean_factor;
      while ttype = OP_AND loop
        next_token;
        boolean_factor;
      end loop;
    end;
    
    -- boolean_expr ::= [ "not" ] boolean_term { "or" boolean_term }
    procedure boolean_expr is
    begin
      if ttype = OP_NOT then
        next_token;
      end if;
      boolean_term;
      while ttype = OP_OR loop
        next_token;
        boolean_term;
      end loop;
    end;
    
    -- expr_list ::= boolean_expr { "," boolean_expr }
    function expr_list return pls_integer is
      cnt simple_integer := 0;
    begin     
      boolean_expr;
      cnt := cnt + 1;
      while ttype = T_COMMA loop
        next_token;
        boolean_expr;
        cnt := cnt + 1;
      end loop;
      return cnt;
    end;

    -- base ::= number | identifier | function [ "(" expr_list ")" ] | "(" extended_expr ")" | constant | property_expr | string
    procedure base is
      cnt       pls_integer;
      arg_count pls_integer;
    begin
      if accept(T_NUMBER) then
        null;
      elsif accept(T_IDENT) then
        null;
      elsif accept(T_FUNC) then
        fn_name := curr;
        arg_count := fnc(fn_name);
        
        if arg_count != 0 then
          expect(T_LEFT);
          cnt := expr_list;
          expect(T_RIGHT);          
          if arg_count < 0 then
            arg_count := abs(arg_count);
            if cnt < arg_count then
              -- Error at position %d : function '%s' expects at least %d argument(s)
              error(FUNCTION_EXPECTS_LEAST, pos-1, fn_name, arg_count, case when arg_count > 1 then 's' end);
            end if;
          elsif cnt != arg_count then
            -- Error at position %d : function '%s' expects %d argument(s)
            error(FUNCTION_EXPECTS_ARG, pos-1, fn_name, arg_count, case when arg_count > 1 then 's' end);
          end if;
          
        elsif accept(T_LEFT) then            
          -- Function '%s' does not take any argument
          error(FUNCTION_NO_ARGUMENT, pos, fn_name);
        end if;
          
      elsif accept(T_LEFT) then
        --boolean_expr;
        extended_expr;
        expect(T_RIGHT);
      elsif accept(T_CONST) then 
        null;
      -- string
      elsif accept(T_STRING) then 
        null;
      else
        -- Error at position %d : unexpected symbol '%s'
        error(UNEXPECTED_SYMBOL, pos, strval);
      end if;
    end; 

    -- factor ::= base { "^" base }
    procedure factor is
    begin
      base;
      while ttype = OP_EXP 
      loop
        next_token;
        base;
      end loop;
    end;

    -- term ::= factor { ( "*" | "/" | "%" ) factor }
    procedure term is
    begin
      factor;
      while ttype in (OP_MUL, OP_DIV, OP_MOD) 
      loop
        next_token;
        factor;
      end loop;
    end; 
    
    -- expr ::= [ "-" ] term { ( "+" | "-" ) term }
    procedure expr is
    begin
      if ttype = OP_UMINUS then
        next_token;
      end if;
      term;
      while ttype = OP_PLUS or ttype = OP_MINUS loop
        next_token;
        term;
      end loop;
    end;
    
    -- decl_expr      ::= identifier ":=" expr
    procedure decl_expr is
    begin
      expect(T_IDENT);
      expect(OP_ASSIGN);
      expr;
    end;
    
    -- decl_list      ::= decl_expr { "," decl_expr }
    procedure decl_list is
    begin
      decl_expr;
      while ttype = T_COMMA loop
        next_token;
        decl_expr;
      end loop;      
    end;
    
    -- extended_expr  ::= [ "declare" "(" decl_list ")" "return" ] boolean_expr
    procedure extended_expr is
    begin
      if accept(T_DECL) then
        expect(T_LEFT);
        decl_list;
        expect(T_RIGHT);
        expect(T_RETURN);
      end if;
      boolean_expr;
    end;

  begin

    next_token;
    --expr;
    extended_expr;
    expect(T_EOF);
    
  end;


  function createCompiler (
    p_mode in binary_integer 
  )
  return rpn_compiler
  is 
    compiler  rpn_compiler;
  begin
    compiler.c_mode := p_mode;
    compiler.st := plc_stack();
    compiler.arg_cnt := -1;
    if p_mode = COMPILE_TLIST then
      compiler.output := plc_stack();
    end if;
    return compiler;
  end;
    

  function compile_int (
    p_expr    in varchar2
  , p_options in number default VALIDATE
  , p_mode    in number default COMPILE_TLIST
  )
  return rpn_compiler
  is
    tokenizer  string_tokenizer := createTokenizer(p_expr);
    compiler   rpn_compiler     := createCompiler(p_mode);
    token      plc_token;
  begin

    if p_options = VALIDATE then
      parse(tokenizer, compiler);
    else
      loop
        token := readToken(tokenizer);
        enqueueRPNToken(compiler, token);
        exit when token.type = T_EOF;
      end loop;
    end if;

    return compiler;

  exception
    -- just to suppress the parser error stack
    when parse_exception then
      raise;
  end;

  
  function compile (
    p_expr    in varchar2
  , p_options in number default VALIDATE
  )
  return plc_token_list deterministic
  is
  begin
    return compile_int(p_expr, p_options, COMPILE_TLIST).output.tlist;
  end;  


  function compileBinary (
    p_expr    in varchar2
  , p_options in number default VALIDATE
  )
  return raw deterministic
  is
  begin
    return compile_int(p_expr, p_options, COMPILE_BINARY).rawstream;
  end;
    

  function createTokenReader (
    tlist     in plc_token_list default null
  , binstream in raw default null
  )
  return token_reader
  is
    reader  token_reader;
  begin
    if tlist is not null then
      reader.c_mode := COMPILE_TLIST;
      reader.tlist := tlist;
      reader.len := tlist.count;
    elsif binstream is not null then
      reader.c_mode := COMPILE_BINARY;
      reader.rawstream := binstream;
      reader.len := utl_raw.length(binstream);
    end if;
    reader.pos := 1;
    return reader;
  end;


  function readBinaryToken (
    reader in out nocopy token_reader
  )
  return plc_token
  is
    tokenType  binary_integer;
    strLen     binary_integer;
    str        varchar2(30);
    n1         binary_double;
    n2         binary_integer;
    token      plc_token;
    
    function readBytes (n in binary_integer) return raw is
      buf  raw(2000);
    begin
      buf := utl_raw.substr(reader.rawstream, reader.pos, n);
      reader.pos := reader.pos + n;
      return buf;
    end;
    
  begin
    
    tokenType := utl_raw.cast_to_binary_integer(readBytes(1));
    
    if tokenType in (T_IDENT, T_FUNC, T_CONST, T_STRING) then
      
      strLen := utl_raw.cast_to_binary_integer(readBytes(1));
      str := utl_raw.cast_to_varchar2(readBytes(strLen));
      token := plc_token(tokenType, str, null, null);
      
    elsif tokenType = T_NUMBER then
    
      n1 := utl_raw.cast_to_binary_double(readBytes(8));
      token := plc_token(tokenType, null, n1, null);
      
    elsif tokenType = T_ARGC then
      
      n2 := utl_raw.cast_to_binary_integer(readBytes(1));
      token := plc_token(tokenType, null, n2, null);      
    
    else
      token := plc_token(tokenType, null, null, null);
    end if;
    
    return token;

  end;
  
  
  function readBinaryStream (
    p_expr in raw
  )
  return plc_token_list pipelined
  is
    reader  token_reader := createTokenReader(binstream => p_expr);
  begin
    while reader.pos <= reader.len loop
      pipe row (readBinaryToken(reader));
    end loop; 
    return;
  end;


  function readRPNToken (
    reader in out nocopy token_reader
  )
  return plc_token
  is
    token  plc_token;
  begin
    if reader.c_mode = COMPILE_TLIST then
      
      token := reader.tlist(reader.pos);
      reader.pos := reader.pos + 1;
      
    elsif reader.c_mode = COMPILE_BINARY then
    
      token := readBinaryToken(reader);
    
    end if;
    
    return token;
  end;
  

  function eval_int (
    reader  in out nocopy token_reader
  , p_vars  in plc_bind_list
  , p_flags in number
  )
  return binary_double 
  is
    r      plc_token_list := plc_token_list();
    j      binary_integer := 0;
    v      keymap;
    token  plc_token;
    opcnt  binary_integer := 0;
    tmp    plc_token;

  begin

    for i in 1 .. p_vars.count loop
      if p_vars(i).name is not null then
        v(upper(p_vars(i).name)) := p_vars(i).val;
      end if;
    end loop;

    while reader.pos <= reader.len loop

      token := readRPNToken(reader);
      tmp := new plc_token();

      if token.type = T_IDENT then
       
        begin
          r.extend;
          j := j + 1;
          tmp.numval := v(token.strval);
        exception
          when no_data_found then
            tmp.strval := token.strval;
        end;
        
      elsif token.type = T_NUMBER or token.type = T_ARGC then
      
        r.extend;
        j := j + 1;
        tmp.numval := token.numval;
        
      elsif token.type = T_CONST then 

        r.extend;
        j := j + 1;

        case token.strval
          when 'NULL' then
            tmp.numval := null;
          when 'PI' then
            tmp.numval := c_pi;
        end case;       
        
      elsif token.type = T_FUNC then
      
        opcnt := fnc(token.strval);
      
        case token.strval
          when 'IF' then
            tmp.numval := case when r(j-2).numval = 1 then r(j-1).numval else r(j).numval end;
          when 'ISNULL' then
            tmp.numval := case when r(j).numval is null then 1 else 0 end;
          when 'NULLIF' then
            tmp.numval := nullif(r(j-1).numval, r(j).numval);
          when 'IFNULL' then
            tmp.numval := nvl(r(j-1).numval, r(j).numval);
          when 'ABS' then
            tmp.numval := abs(r(j).numval);
          when 'MAX' then
            opcnt := r(j).numval;
            tmp.numval := fn_max(r, j, opcnt);
            opcnt := opcnt + 1;          
          when 'MIN' then
            opcnt := r(j).numval;
            tmp.numval := fn_min(r, j, opcnt);
            opcnt := opcnt + 1;
          when 'COS' then
            tmp.numval := cos(r(j).numval);
          when 'SIN' then
            tmp.numval := sin(r(j).numval);
          when 'TAN' then
            tmp.numval := tan(r(j).numval);
          when 'SQRT' then
            tmp.numval := sqrt(r(j).numval);
          when 'EXP' then
            tmp.numval := exp(r(j).numval);
          when 'LN' then
            tmp.numval := ln(r(j).numval);
          when 'LOG' then
            tmp.numval := log(r(j-1).numval, r(j).numval);
          when 'CEIL' then
            tmp.numval := ceil(r(j).numval);
          when 'FLOOR' then
            tmp.numval := floor(r(j).numval);
          when 'ROUND' then
            tmp.numval := round(r(j-1).numval, r(j).numval);             
            
          -- add new operator/function implementation here   
          --when 'FOO' then
          --  tmp := foo(r(j-1), ..., r(j)) ;
          
        end case;
        
        opcnt := opcnt - 1;

        if opcnt = -1 then
          r.extend;
          j := j + 1;
        else
          r.trim(opcnt);
          j := j - opcnt;
        end if;
      
      elsif token.type = T_STRING then
       
        r.extend;
        j := j + 1;
        tmp.strval := token.strval;
            
      else
        -- Operators   
        case token.type
          when OP_PLUS then
            tmp.numval := r(j-1).numval + r(j).numval;
          when OP_MINUS then
            tmp.numval := r(j-1).numval - r(j).numval;
          when OP_MUL then
            tmp.numval := r(j-1).numval * r(j).numval;
          when OP_DIV then
            tmp.numval := r(j-1).numval / r(j).numval;
          when OP_EXP then
            tmp.numval := r(j-1).numval ** r(j).numval;
          when OP_MOD then
            tmp.numval := mod(r(j-1).numval, r(j).numval);
          when OP_UMINUS then
            tmp.numval := - r(j).numval;
          when OP_EQ then
            tmp.numval := case when r(j-1).numval = r(j).numval then 1 else 0 end;
          when OP_LT then
            tmp.numval := case when r(j-1).numval < r(j).numval then 1 else 0 end;
          when OP_LE then
            tmp.numval := case when r(j-1).numval <= r(j).numval then 1 else 0 end;
          when OP_GT then
            tmp.numval := case when r(j-1).numval > r(j).numval then 1 else 0 end;
          when OP_GE then
            tmp.numval := case when r(j-1).numval >= r(j).numval then 1 else 0 end;
          when OP_NE then
            tmp.numval := case when r(j-1).numval != r(j).numval then 1 else 0 end;
          when OP_AND then
            tmp.numval := case when r(j-1).numval = 1 and r(j).numval = 1 then 1 else 0 end;
          when OP_OR then
            tmp.numval := case when r(j-1).numval = 1 or r(j).numval = 1 then 1 else 0 end;
          when OP_NOT then
            tmp.numval := case when r(j).numval = 1 then 0 else 1 end;
          when OP_ASSIGN then
            v(r(j-1).strval) := r(j).numval;
            r.trim(2);
            j := j - 2;
            continue;

        end case;

        opcnt := op(token.type).argc - 1;

        if opcnt = -1 then
          r.extend;
          j := j + 1;
        else
          r.trim(opcnt);
          j := j - opcnt;
        end if;

      end if;

      r(j) := tmp;

    end loop;

    return case when p_flags = KEEP_INF_OR_NAN
                  or ( p_flags = NULL_INF_OR_NAN
                       and not(tmp.numval is nan or tmp.numval is infinite) )
           then tmp.numval end ;

  end ;


  function eval (
    tlist   in plc_token_list
  , p_vars  in plc_bind_list default plc_bind_list()
  , p_flags in number        default NULL_INF_OR_NAN
  )
  return binary_double
  deterministic
  is
    reader token_reader := createTokenReader(tlist => tlist);
  begin
    return eval_int(reader, p_vars, p_flags);
  end ;
  

  function eval (
    rawstream in raw
  , p_vars    in plc_bind_list default plc_bind_list()
  , p_flags   in number        default NULL_INF_OR_NAN
  )
  return binary_double
  deterministic
  is
    reader token_reader := createTokenReader(binstream => rawstream);
  begin
    return eval_int(reader, p_vars, p_flags);
  end ;


  function eval (
    p_expr    in varchar2
  , p_vars    in plc_bind_list default plc_bind_list()
  , p_options in number        default VALIDATE
  , p_flags   in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic 
  is
  begin
    return eval(compile(p_expr, p_options), p_vars, p_flags);
  end;

  
  function serialize (tlist in plc_token_list, p_options in binary_integer default SERIALIZE_INFIX)
  return varchar2 
  deterministic 
  is
  
    type my_rec is record (expr varchar2(32767), prec binary_integer);
    type my_stack is table of my_rec;
    st     my_stack := my_stack(); 

    token  plc_token;
    strval varchar2(30);
    i      binary_integer := 0;
    j      binary_integer := 0;
    output varchar2(32767);
    argc   binary_integer;
    asso   binary_integer;
    oprc   binary_integer;
    
    procedure push (str in out nocopy varchar2
                  , prec in binary_integer default null
                  , flush in boolean default true) is
    begin
      st.extend;
      st(st.last).expr := str;
      st(st.last).prec := prec;
      j := j + 1;
      if flush then
        str := null;
      end if;
    end;
    
    function pop return varchar2 is
      result varchar2(32767);
    begin
      result := st(j).expr;
      st.trim;
      j := j - 1;
      return result;
    end;
    
    procedure append (str in varchar2) is
    begin
      output := output || str;
    end;

  begin
   
    while i < tlist.count loop
     
      i := i + 1;
      token := tlist(i);
      
      case token.type
       
        when T_FUNC then
         
          argc := fnc(token.strval);
          
          if p_options = SERIALIZE_SQL then
           
            if argc >= 0 then
             
              output := getSQLExpr(token.strval);
              
              for p in 1 .. argc loop
                output := replace(output, ':'||to_char(p), st(j-argc+p).expr);
              end loop;
              st.trim(argc);
              j := j - argc;
              
            -- variable args
            elsif argc < 0 then
             
              argc := to_number(pop());
              for p in 1 .. argc loop
                if p > 1 then
                  append(',');
                end if;
                append(st(j-argc+p).expr);
              end loop;
              st.trim(argc);
              j := j - argc;
              output := replace(getSQLExpr(token.strval), ':args', output);
              
            end if;
          
          else
           
            -- variable args
            if argc < 0 then
              argc := to_number(pop());
            end if;
        
            append(token.strval);
            if argc > 0 then
              append('(');
              for p in reverse 1 .. argc loop
                append(st(j-p+1).expr);
                if p > 1 then
                  append(',');
                end if;           
              end loop;
              append(')');
              st.trim(argc);
              j := j - argc;
            end if;
          
          end if;
          
          push(output);
        
        when T_ARGC then
          push(token.numval, flush => false);
        
        when T_NUMBER then
          output := to_number(token.numval);
          push(output);
          
        when T_IDENT then

          output := token.strval;
          
          if p_options = SERIALIZE_SQL then
            output := '"' || output || '"';
          end if;
          
          push(output);
          
        when T_CONST then
        
          if p_options = SERIALIZE_SQL then
            output := getSQLExpr(token.strval);
            push(output);
          else
            push(token.strval, flush => false);
          end if;
          
        when T_STRING then
         
          output := '''' || token.strval || '''';
          push(output);
          
        --when T_PROP then
        --  push(token.strval);
          
        else
         
          if op.exists(token.type) then
            
            argc := op(token.type).argc;
            strval := tm(token.type);
          
            -- operator to function transformation
            if p_options = SERIALIZE_SQL and sqlm.exists(strval) then
             
              output := sqlm(strval);
             
              if argc > 0 then
                for p in 1 .. argc loop
                  output := replace(output, ':'||to_char(p), st(j-argc+p).expr);
                end loop;
                st.trim(argc);
                j := j - argc;
              end if;
              
              push(output);    
            
            else
          
              oprc := op(token.type).prec;
              asso := op(token.type).assoc;
              
              if argc = 1 then
                append(strval);
                append(' ');
                if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                  append('(');
                  append(st(j).expr);
                  append(')');
                else
                  append(st(j).expr);
                end if;
              else
                if st(j-1).prec < oprc or ( st(j-1).prec <= oprc and asso = 1 ) then
                  append('(');
                  append(st(j-1).expr);
                  append(')');
                else
                  append(st(j-1).expr);
                end if;
                append(' ');
                append(strval);
                append(' ');
                if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                  append('(');
                  append(st(j).expr);
                  append(')');
                else 
                  append(st(j).expr);
                end if;
              end if;
              
              st.trim(argc);
              j := j - argc;
              push(output, oprc);
            
            end if;
            
          end if;
      
      end case;
      
    end loop;
    
    return st(1).expr;
   
  end;


  function to_MathML (
    tlist        in plc_token_list
  , as_document  in binary_integer default 1
  )
  return varchar2 
  deterministic 
  is
  
    type my_rec is record (expr varchar2(32767), prec binary_integer);
    type my_stack is table of my_rec;
    st     my_stack := my_stack(); 

    token  plc_token;
    i      binary_integer := 0;
    j      binary_integer := 0;
    output varchar2(32767);
    argc   binary_integer;
    asso   binary_integer;
    oprc   binary_integer;
    
    operand1  varchar2(32767);
    operand2  varchar2(32767);
    
    procedure push (str in out nocopy varchar2
                  , prec in binary_integer default null
                  , flush in boolean default true) is
    begin
      st.extend;
      st(st.last).expr := str;
      st(st.last).prec := prec;
      j := j + 1;
      if flush then
        str := null;
      end if;
    end;
    
    function pop return varchar2 is
      result varchar2(32767);
    begin
      result := st(j).expr;
      st.trim;
      j := j - 1;
      return result;
    end;
    
    procedure append (str in varchar2) is
    begin
      output := output || str;
    end;

  begin
   
    while i < tlist.count loop
     
      i := i + 1;
      token := tlist(i);
      
      case token.type
       
        when T_FUNC then
         
          argc := fnc(token.strval);
           
          -- variable args
          if argc < 0 then
            argc := to_number(pop());
          end if;
          
          case token.strval
        
            when 'SQRT' then
              
              append('<msqrt>'||st(j).expr||'</msqrt>');
              st.trim(argc);
              j := j - argc;
              
            when 'EXP' then
              
              append('<msup><mi>e</mi><mrow>'||st(j).expr||'</mrow></msup>');
              st.trim(argc);
              j := j - argc;
              
            when 'LOG' then
              
              append('<msub><mi>log</mi><mrow>'||st(j-1).expr||'</mrow></msub>');
              append('<mo>&#x2061;</mo><mfenced>');
              append('<mrow>'||st(j).expr||'</mrow>'); 
              append('</mfenced>');
            
              st.trim(argc);
              j := j - argc;
            
            else
        
              append('<mi>'||lower(token.strval)||'</mi>');
              
              if argc > 0 then
                append('<mo>&#x2061;</mo><mfenced>');
                for p in reverse 1 .. argc loop
                  append('<mrow>'||st(j-p+1).expr||'</mrow>');           
                end loop;
                append('</mfenced>');
                st.trim(argc);
                j := j - argc;
              end if;
          
          end case;
          
          push(output);

        when T_ARGC then
          output := to_number(token.numval);
          push(output);
          
        when T_NUMBER then
          output := '<mn>'||to_number(token.numval)||'</mn>';
          push(output);
          
        when T_IDENT then

          output := '<mi>'||token.strval||'</mi>';
          push(output);
          
        when T_CONST then
        
          output := '<mi>'||token.strval||'</mi>';
          push(output);
          
        when T_STRING then
         
          output := '<mtext>''' || token.strval || '''</mtext>';
          push(output);
          
        else
         
          if op.exists(token.type) then
            
            argc := op(token.type).argc;
            oprc := op(token.type).prec;
            asso := op(token.type).assoc;
              
            if argc = 1 then
              
              append('<mo>'||token.strval||'</mo>');
              if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                append('<mfenced><mrow>');
                append(st(j).expr);
                append('</mrow></mfenced>');
              else
                append(st(j).expr);
              end if;
              
            else

              -- operand 1
              if st(j-1).prec < oprc or ( st(j-1).prec <= oprc and asso = 1 ) then
                operand1 := '<mfenced><mrow>' || st(j-1).expr || '</mrow></mfenced>';
              else
                operand1 := st(j-1).expr;
              end if;
              -- operand 2
              if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                operand2 := '<mfenced><mrow>' || st(j).expr || '</mrow></mfenced>';
              else 
                operand2 := st(j).expr;
              end if;
              
              case token.type
              
                when OP_DIV then
                  output := '<mfrac><mrow>' || operand1 || '</mrow><mrow>' || operand2 || '</mrow></mfrac>';
                when OP_EXP then
                  output := '<msup><mrow>' || operand1 || '</mrow><mrow>' || operand2 || '</mrow></msup>';
                  
                else
                  output := operand1 || '<mo>' || token.strval || '</mo>' || operand2;
              
              end case;
              
            end if;
              
            st.trim(argc);
            j := j - argc;
            push(output, oprc);
            
          end if;
      
      end case;
      
    end loop;
    
    output := st(1).expr;
    
    if as_document = 1 then
      output := '<math xmlns="http://www.w3.org/1998/Math/MathML">'
             || output
             || '</math>';
    end if;
    
    return output;
   
  end;


  function to_infix (tlist in plc_token_list)
  return plc_token_list 
  deterministic 
  is
  
    type my_rec is record (expr plc_token_list, prec binary_integer);
    type my_stack is table of my_rec;
    st     my_stack := my_stack(); 

    token  plc_token;
    i      binary_integer := 0;
    j      binary_integer := 0;
    output plc_token_list := plc_token_list();
    argc   binary_integer;
    asso   binary_integer;
    oprc   binary_integer;
    
    procedure push (arr in plc_token_list
                  , prec in binary_integer default null
                  ) is
    begin
      st.extend;
      st(st.last).expr := arr;
      st(st.last).prec := prec;
      j := j + 1;
    end;
    
    function pop return plc_token_list is
      result plc_token_list;
    begin
      result := st(j).expr;
      st.trim;
      j := j - 1;
      return result;
    end;
    
    procedure append (p_token in plc_token) is
    begin
      output.extend;
      output(output.last) := p_token;
    end;

    procedure append (p_arr in plc_token_list) is
      len  binary_integer := output.count;
    begin
      output.extend(p_arr.count);
      for i in 1 .. p_arr.count loop
        output(len+i) := p_arr(i);
      end loop;
    end;

  begin
   
    while i < tlist.count loop
     
      i := i + 1;
      token := tlist(i);
      
      case token.type
       
        when T_FUNC then
         
          argc := fnc(token.strval);
           
          -- variable args
          if argc < 0 then
            argc := to_number(pop()(1).numval);
          end if;
          output.delete;
          append(token);
          if argc > 0 then
            append( new plc_token(T_LEFT,'(',null,null) );
            for p in reverse 1 .. argc loop
              append(st(j-p+1).expr);
              if p > 1 then
                append( new plc_token(T_COMMA,',',null,null) );
              end if;           
            end loop;
            append( new plc_token(T_RIGHT,')',null,null) );
            st.trim(argc);
            j := j - argc;
          end if;
          
          push(output);

        when T_ARGC then
          push(plc_token_list(token));
          
        when T_NUMBER then
          
          push(plc_token_list(token));
          
        when T_IDENT then

          push(plc_token_list(token));
          
        when T_CONST then
        
          push(plc_token_list(token));
          
        when T_STRING then
         
          push(plc_token_list(token));
          
        else
         
          if op.exists(token.type) then
            
            argc := op(token.type).argc;    
            oprc := op(token.type).prec;
            asso := op(token.type).assoc;
         
            output.delete;
             
            if argc = 1 then
              append(token);
              if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                append( new plc_token(T_LEFT,'(',null,null) );
                append(st(j).expr);
                append( new plc_token(T_RIGHT,')',null,null) );
              else
                append(st(j).expr);
              end if;
            else
              if st(j-1).prec < oprc or ( st(j-1).prec <= oprc and asso = 1 ) then
                append( new plc_token(T_LEFT,'(',null,null) );
                append(st(j-1).expr);
                append( new plc_token(T_RIGHT,')',null,null) );
              else
                append(st(j-1).expr);
              end if;
              append(token);
              if st(j).prec < oprc or ( st(j).prec <= oprc and asso = 1 ) then
                append( new plc_token(T_LEFT,'(',null,null) );
                append(st(j).expr);
                append( new plc_token(T_RIGHT,')',null,null) );
              else 
                append(st(j).expr);
              end if;
            end if;
              
            st.trim(argc);
            j := j - argc;
            push(output, oprc);        
            
          end if;
      
      end case;
      
    end loop;
    
    return st(1).expr;
   
  end;


  function is_boolean (tlist in plc_token_list) 
  return boolean deterministic
  is
  begin
    return (tlist(tlist.last).type in (OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE, OP_NOT, OP_AND, OP_OR));
  end; 


begin
  initMaps;

end plcalc;
/
