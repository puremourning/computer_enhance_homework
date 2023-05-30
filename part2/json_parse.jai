#import "Hash_Table";
#import "Basic";

Object :: Table(string, Value);
Array :: [..]Value;

Value :: struct {
  kind : enum {
    NULL;
    STRING;
    NUMBER;
    OBJECT;
    BOOLEAN;
    ARRAY;
  } = .NULL;

  str\_val: string;
  num\_val: float64; #place str_val;
  obj\_val: *Object; #place str_val;
  arr\_val: Array; #place str_val;
  bool_val: bool; #place str_val;
}

Parser :: struct {
  data: string;
  pos: s64;
  current: string;
}

Parse_Result :: struct {
  failed: bool; // negative so 0-init means "OK"
  pos : s64;
  message : string;
}

pchar :: ( ch: *u8 ) -> string {
  s : string;
  s.data = ch;
  s.count = 1;
  return s;
}

is_eof :: inline ( using parser: *Parser ) -> bool {
  return pos >= data.count;
}

is_whitespace :: inline ( char: u8 ) -> bool {
  return char == #char " " ||
         char == #char "\t" ||
         char == #char "\r" ||
         char == #char "\n";
}

eat_whitespace :: inline ( using parser: *Parser ) {
  while ( !is_eof( parser ) && is_whitespace( data[ pos ] ) ) {
    advance( parser );
  }
}

// The idea is that the cursor always points to the position _after_ the last
// eaten token.
// So the pattern is:
//   eat_whitespace( ) ; pos is first char of next token
//   parse_something() ; pos is first char after the something
advance :: inline ( using parser: *Parser, how_many: s64 = 1 ) {
  advance( *current, how_many );
  pos += how_many;
}

peek_next_token_pos :: inline ( using parser: *Parser ) -> s64 {
  loc := parser.pos;
  // Skip whitespace; we only want to peek the next actual token
  while loc < data.count && is_whitespace( data[ loc ] ) {
    loc += 1;
  }
  return loc;
}

eat_if_token :: inline ( using parser: *Parser, token: u8 ) -> bool {
  loc := peek_next_token_pos( parser );
  if loc < data.count && data[ loc ] == token {
    // got it; eat it
    advance( parser, loc - pos ); // pos now points at the token we just found
    advance( parser ); // eat the token
    return true;
  }
  return false;
}

failed :: inline ( using parser: *Parser, message: string ) -> Parse_Result {
  return Parse_Result.{
    failed = true,
    pos = pos,
    message = message,
  };
}

ok :: inline ( using parser: *Parser ) -> Parse_Result {
  return Parse_Result.{};
}

parse_value :: ( using parser : *Parser ) -> Value, Parse_Result {
  eat_whitespace( parser );
  if is_eof( parser ) {
    return Value.{}, Parse_Result.{ false, parser.pos, "Expected a value" };
  }

  value: Value;
  parse_result: Parse_Result;
  if data[ pos ] == {
    case #char "{"; {
      value, parse_result = parse_object( parser );
    }
    case #char "\""; {
      value, parse_result = parse_string( parser );
    }
    case #char "["; {
      value, parse_result = parse_array( parser );
    }
    case; {
      // check if it looks like a number, else error
      if is_digit( data[ pos ] ) ||
         data[ pos ] == #char "-" ||
         data[ pos ] == #char "+" {
        value, parse_result = parse_number( parser );
      } else if data[ pos ] == #char "t" || data[ pos ] == #char "f" {
        value, parse_result = parse_boolean( parser );
      } else {
        parse_result = failed( parser, "Expected a number" );
      }
    }
  }
  return value, parse_result;
}

parse_separator :: inline ( using parser: *Parser, sep: u8 ) -> Parse_Result {
  eat_whitespace( parser );
  if !is_eof( parser ) && data[ pos ] == sep {
    advance( parser ); // eat it
    return ok( parser );
  }

  return failed( parser, tprint( "Expected '%'", pchar( *sep ) ) );
}

parse_object :: ( using parser : *Parser ) -> Value, Parse_Result {
  value := Value.{ kind = .OBJECT, obj_val = New( Object ) };
  parse_result: Parse_Result;
  parse_result = parse_separator( parser, #char "{" );
  if parse_result.failed return .{}, parse_result;

  ate_comma := true;
  while !is_eof( parser ) && !eat_if_token( parser, #char "}" ) {
    if !ate_comma {
      return .{}, failed( parser, "expected a comma between object members" );
    }

    key: Value;
    this_value: Value;

    key, parse_result = parse_string( parser );
    if parse_result.failed return .{}, parse_result;

    parse_result = parse_separator( parser, #char ":" );
    if parse_result.failed return .{}, parse_result;

    this_value, parse_result = parse_value( parser );
    if parse_result.failed return .{}, parse_result;

    table_add( value.obj_val, key.str_val, *this_value );
    ate_comma = eat_if_token( parser, #char "," );
  }

  if ate_comma && value.obj_val.count > 0 {
      return .{}, failed( parser, "extra comma after last object member" );
  }

  return value, ok( parser );
}

parse_string :: ( using parser : *Parser ) -> Value, Parse_Result {
  value := Value.{ kind = .STRING };
  parse_result: Parse_Result;

  parse_result = parse_separator( parser, #char "\"");
  if parse_result.failed return .{}, parse_result;

  value.str_val.data = data.data + pos;
  while !is_eof( parser ) && !eat_if_token( parser, #char "\"" ) {

    // TODO: Handle escaped double-quote!
    // NOTE: Requires copying the string because we have to remove the slashes
    // if data[ pos ] == #char "\\" &&
    //    (pos+1) < data.count &&
    //    data[ pos + 1 ] == #char "\"" {

    //   // skip over this escaping token "\"
    //   advance( parser );
    // }
    value.str_val.count += 1;
    advance( parser );
  }

  if is_eof( parser ) {
    return .{}, failed( parser, "String was not closed" );
  }

  return value, ok( parser );
}

parse_array :: ( using parser : *Parser ) -> Value, Parse_Result {
  value := Value.{ kind = .ARRAY };
  parse_result: Parse_Result;

  parse_result = parse_separator( parser, #char "[" );
  if parse_result.failed return .{}, parse_result;

  ate_comma := true;
  while !is_eof( parser ) && !eat_if_token( parser, #char "]" ) {
    if !ate_comma {
      return .{}, failed( parser, "Expected a comma between list elements" );
    }

    this_value: Value;
    this_value, parse_result = parse_value( parser );
    if parse_result.failed return .{}, parse_result;

    array_add( *value.arr_val, this_value );
    ate_comma = eat_if_token( parser, #char "," );
  }

  if ate_comma && value.arr_val {
    return .{}, failed( parser, "Extra comma after last list element" );
  }
  return value, ok( parser );
}

parse_number :: ( using parser : *Parser ) -> Value, Parse_Result {
  value := Value.{ kind = .NUMBER };
  parse_result: Parse_Result;

  eat_whitespace( parser );

  old_pos := pos;
  number: string;
  number.data = data.data + pos;

  if eat_if_token( parser, #char "-" ) || eat_if_token( parser, #char "+" ) {
    number.count += 1;
  }

  parse_digits :: () -> bool #expand {
    old_count := number.count;
    while !is_eof( parser ) && is_digit( data[ pos ] ) {
      number.count += 1;
      advance( parser );
    }
    return number.count > old_count;
  }

  if !parse_digits() {
    // TODO: Is it legal to have nothing before the decimal point?
    return .{}, failed( parser, "Expected digits in number" );
  }

  if eat_if_token( parser, #char "." ) {
    number.count += 1;
    if !parse_digits() {
      return .{}, failed( parser, "Expected digits after number sep" );
    }
  }

  // exponent
  if eat_if_token( parser, #char "e" ) ||
     eat_if_token( parser, #char "E" ) {
    number.count += 1;
    if eat_if_token( parser, #char "-" ) || eat_if_token( parser, #char "+" ) {
      number.count += 1;
    }
    if !parse_digits() {
      return .{}, failed( parser, "Expected digits after number sep" );
    }
  }

  value.num_val = string_to_float64( number );
  return value, ok( parser );
}

parse_boolean :: ( using parser : *Parser ) -> Value, Parse_Result {

  value := Value.{ kind = .BOOLEAN };
  parse_result: Parse_Result;

  eat_whitespace( parser );

  TRUE :: "true";
  FALSE :: "false";

  if pos + TRUE.count < data.count {
    maybe_true: string;
    maybe_true.data = data.data + pos;
    maybe_true.count = TRUE.count;
    if maybe_true == TRUE {
      advance( parser, TRUE.count );
      return Value.{ kind = .BOOLEAN, bool_val = true }, ok( parser );
    }
  }
  if pos + FALSE.count < data.count {
    maybe_false: string;
    maybe_false.data = data.data + pos;
    maybe_false.count = FALSE.count;
    if maybe_false == FALSE {
      advance( parser, FALSE.count );
      return Value.{ kind = .BOOLEAN, bool_val = false }, ok( parser );
    }
  }

  return .{}, failed( parser, "Expected true or false" );
}
