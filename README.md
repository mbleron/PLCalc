# PLCalc - a PL/SQL RPN calculator

PLCalc is a PL/SQL API that provides the ability to evaluate arithmetic expressions, optionally with bind variables.  
Expressions may be compiled and stored in the database in RPN format, and the compiled form directly used for the computation (*"parse once, execute many"*).  
The API includes a tokenizer and a recursive-descent parser for validation (see LL grammar [here](#ebnf-grammar)).  
RPN compilation is achieved using the [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm). 

A serializer is also provided to convert a compiled expression back into :  
* a readable string in infix form
* a SQL-compatible expression
* a Presentation MathML document (experimental)

*(PLCalc is a revamped version of my previous work : RPN_UTIL)*

## Bug tracker

Found bugs? I'm sure there are...  
Please create an issue here on GitHub at <https://github.com/mbleron/oracle/issues>.

## Installation

```
@plc_bind_var.tps
@plc_bind_list.tps
@plc_token.typ
@plc_token_list.tps
@plc_stack.typ
@plcalc.pck
```

## Generalities

#### Arithmetic operators
The four arithmetic operators are supported :  
`+`  `-`  `*`  `/`

along with :  
`^` (exponentiation)  
`%` (modulo)

#### Relational and boolean operators

To be used in boolean expressions in conjunction with the `IF` function :  
`=`
`!=`
`>`
`>=`
`<`
`<=`  
`NOT`
`AND`
`OR`

#### Built-in functions 

| Name     | Syntax | Description
| :-------- | :------ | :----------- 
| `ABS`    | *abs(x)*      | absolute value
| `COS`    | *cos(x)*      | cosine
| `SIN`    | *sin(x)*      | sine
| `TAN`    | *tan(x)*      | tangent
| `SQRT`   | *sqrt(x)*     | square root
| `EXP`    | *exp(x)*      | exponential
| `LN`     | *ln(x)*       | natural logarithm
| `LOG`    | *log(n,x)*    | base n logarithm
| `CEIL`   | *ceil(x)*     | ceiling
| `FLOOR`  | *floor(x)*    | floor
| `ROUND`  | *round(x,n)*  | round
| `IF`     | *if (bool_expr, expr_true, expr_false)* | if expression
| `ISNULL` | *isnull(x)*   | test for NULL 
| `NULLIF` | *nullif(x,y)* | shorthand for `IF(x = y, NULL, x)`
| `IFNULL` | *ifnull(x,y)* | shorthand for `IF(ISNULL(x), y, x)`
| `MIN`    | *min(x1, x2, ..., xn)* | minimum of n values
| `MAX`    | *max(x1, x2, ..., xn)* | maximum of n values

#### Constants

| Name   | Description
| :---   | :---
| `PI`   | Number &pi;, internally computed as *arccos(-1)* in binary double precision  
| `NULL` |	Null value

#### Extended expression syntax

Expressions may be factorized and assigned to variables in a `DECLARE` prolog, and referenced multiple times in the main expression : 

```
declare ( x := y+1, q := ln(x+1) ) return (1 - q)/(1- q^n)
```

<br>
## Usage

#### Evaluating an expression string

The `eval` function, in its simplest : 

```
SQL> select plcalc.eval(p_expr => '(min(1+2,3*4)-1)^4') as result
  2  from dual;
 
    RESULT
----------
        16
```

By default, the input expression is parsed (validated).  
In order to bypass this validation, for example if we know for sure the expression is OK, we can set `p_options` argument to `NO_VALIDATE` (0) : 

```
select plcalc.eval(
         p_expr    => '(min(1+2,3*4)-1)^4'
       , p_options => 0
       ) 
       as result
from dual;
```

Since the calculation is made using double precision numbers, the result might evaluate to NaN ("Not a Number") or +/-Inf (Infinity).  
In that case, the default behaviour is to nullify the result.  
If, for some reasons, we do need to return the value as such, we can set `p_flags` to `KEEP_INF_OR_NAN` (1) : 
```
SQL> select to_char(
  2           plcalc.eval(
  3             p_expr  => '1/0'
  4           , p_flags => 1
  5           )
  6         ) as result
  7  from dual;
 
RESULT
----------------------------------------
Inf
```

<br>
#### Compiling an expression using default format

Compilation using the default format returns a collection (VARRAY) of type `PLC_TOKEN_LIST`.  
e.g.
```
declare
  tlist  plc_token_list;
begin
  tlist := plcalc.compile('(min(1+2,3*4)-1)^4');
end;
/
```
For debugging purpose, the output can also be conveniently inspected as relational data, like this : 
```
SQL> select *
  2  from table(plcalc.compile('(min(1+2,3*4)-1)^4'));
 
 TYPE STRVAL   NUMVAL  POSITION
----- ------- ------- ---------
   40 1             1         6
   40 2             2         8
    2 +                       7
   40 3             3        10
   40 4             4        12
    3 *                      11
   39               2 
   42 MIN                     2
   40 1             1        15
    1 -                      14
   40 4             4        18
    5 ^                      17
 
```

The optional `p_options` argument controls whether we want to `VALIDATE` or `NO_VALIDATE` the expression (default is `VALIDATE`).  

<br>
#### Compiling an expression using binary format

Binary compilation returns a scalar value of RAW data type, representing the same sequence of RPN tokens but in a more compact format.

```
SQL> declare
  2    rawstream  raw(2000);
  3  begin
  4    rawstream := plcalc.compileBinary('(min(1+2,3*4)-1)^4');
  5    dbms_output.put_line(rawstream);
  6  end;
  7  /
 
283FF0000000000000284000000000000000022840080000000000002840100000000000000327022A034D494E283FF00000000000000128401000000000000005
```

A helper table function is provided to read back a binary-compiled expression as relational data :  
```
SQL> select type, strval, numval
  2  from table(
  3         plcalc.readBinaryStream(
  4           hextoraw('283FF00000000000002840000000000000000'||
  5                    '2284008000000000000284010000000000000'||
  6                    '0327022A034D494E283FF0000000000000012'||
  7                    '8401000000000000005')
  8         )
  9       );
 
      TYPE STRVAL   NUMVAL
---------- ------- -------
        40               1
        40               2
         2         
        40               3
        40               4
         3         
        39               2
        42 MIN     
        40               1
         1         
        40               4
         5         
```

<br>
#### Evaluating a compiled expression

While a string expression may be directly evaluated using `eval` function, the real strength of PLCalc is in the evaluation of compiled expressions.  
Thus, the `eval` function is overloaded to accept both compiled formats (default and binary) : 

```
  function eval (
    tlist   in plc_token_list
  , p_vars  in plc_bind_list default plc_bind_list()
  , p_flags in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic ;
```

```
  function eval (
    rawstream in raw
  , p_vars    in plc_bind_list default plc_bind_list()
  , p_flags   in number        default NULL_INF_OR_NAN
  )
  return binary_double deterministic ;
```

We'll see how to bind variables at runtime in the next section.
 
<br>
#### Using bind variables

The `eval` function (all flavors of it) possesses an optional argument `p_vars` we can use to pass a collection of bind variables to the expression.  
The expected data type is `PLC_BIND_LIST` :  
```
create or replace type plc_bind_list is table of plc_bind_var;
```
which holds instances of `PLC_BIND_VAR` :  
```
create or replace type plc_bind_var is object (
  name  varchar2(30)
, val   binary_double
);
```

Variable name matching in the expression is case-insensitive.

A couple of examples : 

1. Binding different values of a variable to the same compiled expression
```
SQL> declare
  2    expr  plc_token_list := plcalc.compile('(1+x)^2');
  3    vars  plc_bind_list;
  4  begin
  5    for i in 1 .. 10 loop
  6      vars := plc_bind_list(plc_bind_var('X', i));
  7      dbms_output.put_line( to_number( plcalc.eval(expr, vars) ) );
  8    end loop;
  9  end;
 10  /
 
4
9
16
25
36
49
64
81
100
121
```  

2. Binding multiple variables
```
SQL> with vars (name, val) as (
  2    select 'a', 1 from dual union all
  3    select 'b', 2 from dual union all
  4    select 'c', 3 from dual
  5  )
  6  select plcalc.eval(
  7           p_expr => 'a + b + c'
  8         , p_vars => cast(collect(plc_bind_var(name, val)) as plc_bind_list)
  9         )
 10         as result
 11  from vars;
 
    RESULT
----------
         6
```

<br>
#### Serialization

1. Serialization of a compiled expression to its original infix form

Using `p_options` = 0 (SERIALIZE_INFIX, the default)
```
SQL> select plcalc.serialize(
  2           tlist     => plcalc.compile('min((1+x)^2, y)')
  3         , p_options => 0
  4         )
  5         as result
  6  from dual;
 
RESULT
--------------------------------------------------------------------------------
MIN((1 + X) ^ 2,Y)

```
<br>
2. Serialization to a SQL expression

Using `p_options` = 1 (SERIALIZE_SQL)

```
SQL> select plcalc.serialize(
  2           tlist     => plcalc.compile('min((1+x)^2, y)')
  3         , p_options => 1
  4         )
  5         as result
  6  from dual;
 
RESULT
--------------------------------------------------------------------------------
least(power(1 + "X", 2),"Y")
 
```
<br>
3. Serialization to Presentation MathML content

```
SQL> select plcalc.to_MathML(
  2           plcalc.compile('sin(exp(x+1))^sqrt((1+y)/5)/2 - 2*z^3*log(2,x) + min(x,y,z)')
  3         )
  4         as result
  5  from dual;

RESULT
--------------------------------------------------------------------------------
<math xmlns="http://www.w3.org/1998/Math/MathML"><mfrac><mrow><msup><mrow><mi>si
n</mi><mo>&#x2061;</mo><mfenced><mrow><msup><mi>e</mi><mrow><mi>X</mi><mo>+</mo>
<mn>1</mn></mrow></msup></mrow></mfenced></mrow><mrow><msqrt><mfrac><mrow><mfenc
ed><mrow><mn>1</mn><mo>+</mo><mi>Y</mi></mrow></mfenced></mrow><mrow><mn>5</mn><
/mrow></mfrac></msqrt></mrow></msup></mrow><mrow><mn>2</mn></mrow></mfrac><mo>-<
/mo><mn>2</mn><mo>*</mo><msup><mrow><mi>Z</mi></mrow><mrow><mn>3</mn></mrow></ms
up><mo>*</mo><msub><mi>log</mi><mrow><mn>2</mn></mrow></msub><mo>&#x2061;</mo><m
fenced><mrow><mi>X</mi></mrow></mfenced><mo>+</mo><mi>min</mi><mo>&#x2061;</mo><
mfenced><mrow><mi>X</mi></mrow><mrow><mi>Y</mi></mrow><mrow><mi>Z</mi></mrow></m
fenced></math>
```

Viewed in a compatible browser :  

<math xmlns="http://www.w3.org/1998/Math/MathML"><mfrac><mrow><msup><mrow><mi>sin</mi><mo>&#x2061;</mo><mfenced><mrow><msup><mi>e</mi><mrow><mi>X</mi><mo>+</mo><mn>1</mn></mrow></msup></mrow></mfenced></mrow><mrow><msqrt><mfrac><mrow><mfenced><mrow><mn>1</mn><mo>+</mo><mi>Y</mi></mrow></mfenced></mrow><mrow><mn>5</mn></mrow></mfrac></msqrt></mrow></msup></mrow><mrow><mn>2</mn></mrow></mfrac><mo>-</mo><mn>2</mn><mo>*</mo><msup><mrow><mi>Z</mi></mrow><mrow><mn>3</mn></mrow></msup><mo>*</mo><msub><mi>log</mi><mrow><mn>2</mn></mrow></msub><mo>&#x2061;</mo><mfenced><mrow><mi>X</mi></mrow></mfenced><mo>+</mo><mi>min</mi><mo>&#x2061;</mo><mfenced><mrow><mi>X</mi></mrow><mrow><mi>Y</mi></mrow><mrow><mi>Z</mi></mrow></mfenced></math>

<br>
#### Storing compiled expression in the database

```
create table expr_store (
  id      integer
, expr    varchar2(4000)
, tlist   plc_token_list
, constraint expr_store_pk primary key (id) using index
)
varray tlist store as securefile lob expr_store$tlist (cache)
;

insert into expr_store (id, expr, tlist)
values (
  1
, '(1+x)^3'
, plcalc.compile('(1+x)^3')
);
```

The compiled expression can now be accessed directly from the table and used in the `eval` function, with dynamic binding :  
```
with tmp (n) as (
  select level
  from dual
  connect by level <= 10000
)
select plcalc.eval(
         e.tlist
       , plc_bind_list(plc_bind_var('x', t.n))
       ) as result
from expr_store e
     cross join tmp t
where e.id = 1;
```


<br>
## Appendices

#### EBNF grammar
```
    extended_expr  ::= [ "declare" "(" decl_list ")" "return" ] boolean_expr
    decl_list      ::= decl_expr { "," decl_expr }
    decl_expr      ::= identifier ":=" expr
    
    boolean_expr   ::= [ "not" ] boolean_term { "or" boolean_term }
    boolean_term   ::= boolean_factor { "and" boolean_factor }
    boolean_factor ::= expr [ relational_op expr ]
                     
    relational_op  ::= "=" | "<" | ">" | "!=" | "<=" | ">="

    expr           ::= [ "-" ] term { ( "+" | "-" ) term }
    term           ::= factor { ( "*" | "/" | "%" ) factor }
    factor         ::= base { "^" base }
    base           ::= number
                     | identifier
                     | constant
                     | function [ "(" expr_list ")" ]
                     | "(" extended_expr ")"
                     | string
                 
    expr_list      ::= boolean_expr { "," boolean_expr }

    function       ::= "MIN" 
                     | "MAX"
                     | "IF"
                     # etc.
                     
    number         ::= \d+\.?\d*([eE][+-]?\d+)?
    identifier     ::= [a-zA-Z]\w*
    constant       ::= "NULL" 
                     | "PI"
    string         ::= "'" char* "'"
```


## CHANGELOG

### 1.0 (2016-08-07)

* First publication of the revised code



## Copyright and license

Copyright 2016 Marc Bleron. Released under MIT license.
