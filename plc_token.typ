create or replace type plc_token as object (
  type     integer
, strval   varchar2(30)
, numval   binary_double
, position integer
, constructor function plc_token (self in out nocopy plc_token) return self as result
, constructor function plc_token (self in out nocopy plc_token, n in binary_double) return self as result
, constructor function plc_token (self in out nocopy plc_token, str in varchar2) return self as result
)
/
create or replace type body plc_token is
  
  constructor function plc_token (self in out nocopy plc_token) return self as result
  is
  begin
    return;
  end;

  constructor function plc_token (self in out nocopy plc_token, n in binary_double) return self as result
  is
  begin
    self.numval := n;
    return;
  end;

  constructor function plc_token (self in out nocopy plc_token, str in varchar2) return self as result
  is
  begin
    self.strval := str;
    return;
  end;
  
end;
/
