create or replace type plc_stack is object (
  tlist  plc_token_list
, top    integer
, constructor function plc_stack (self in out nocopy plc_stack) return self as result deterministic
, constructor function plc_stack (self in out nocopy plc_stack, tlist in plc_token_list) return self as result deterministic
, member function serialize (sep in varchar2 default ' ') return varchar2 deterministic
, member procedure push (self in out nocopy plc_stack, token in plc_token)
, member procedure pop (self in out nocopy plc_stack, cnt in integer default 1)
, member function pop (self in out nocopy plc_stack) return plc_token
, member function peek (offset in integer default 0) return plc_token
, member function isEmpty return boolean
)
/
create or replace type body plc_stack is

  constructor function plc_stack (self in out nocopy plc_stack)
    return self as result deterministic
  is
  begin
    self.tlist := new plc_token_list();
    self.top := 0;
    return;
  end;

  constructor function plc_stack (self in out nocopy plc_stack, tlist in plc_token_list)
    return self as result deterministic
  is
  begin
    self.tlist := tlist;
    self.top := tlist.count;
    return;
  end;

  member function serialize (sep in varchar2 default ' ')
  return varchar2 deterministic
  is
    buf varchar2(32767);
  begin
    for i in 1..top loop
      buf := buf || sep || self.tlist(i).strval;
    end loop;
    return substr(buf, length(sep));
  end;

  member procedure push (self in out nocopy plc_stack, token in plc_token) is
  begin
    self.tlist.extend;
    top := top + 1;
    tlist(top) := token;
  end;

  member procedure pop (self in out nocopy plc_stack, cnt in integer default 1) is
  begin
    self.tlist.trim(cnt);
    self.top := self.top - cnt;
  end;

  member function pop (self in out nocopy plc_stack) return plc_token is
    token plc_token := self.peek();
  begin
    self.pop();
    return token;
  end;

  member function peek (offset in integer default 0) return plc_token is
  begin
    return self.tlist(top-offset);
  end;

  member function isEmpty return boolean is
  begin
    return (top = 0);
  end;

end;
/
