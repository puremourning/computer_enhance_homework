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

  union {
    str_val: string;
    num_val: float64;
    obj_val: *Object;
    arr_val: Array;
    bool_val: bool;
  }
}

Parser :: struct {
  data: string;
  pos: s64;
  current: string;
}

Parse_Result :: struct {
  // negative so 0-init means "OK"
  failed: bool;
}

Parse_Error :: struct {
  #as result: Parse_Result;
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
  return Parse_Error.{
    result.failed = true,
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
    return .{}, failed( parser, "Expected a value" );
  }

  value: Value;
  parse_result: Parse_Result;

  // peek at the token to see what to parse
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
      if is_digit( data[ pos ] ) || data[ pos ] == #char "-" {
        value, parse_result = parse_number( parser );
      } else if data[ pos ] == #char "t" || data[ pos ] == #char "f" {
        value, parse_result = parse_boolean( parser );
      } else if data[ pos ] == #char "n" {
        value, parse_result = parse_null( parser );
      } else {
        parse_result = failed( parser, "Unrecognised value" );
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

  builder: String_Builder; // only used if we see any gnarly escapes
  defer reset( *builder );

  value.str_val.data = data.data + pos;
  ate_close := false;
  is_escaped := false;
  while !is_eof( parser ) && !ate_close {
    // TODO: Unicode!

    if data[ pos ] == #char "\\" {
      // we have an escape, so we need to switch to copying
      if pos + 1 >= data.count {
        // illegal escape
        return .{}, failed( parser, "Illegal escape at end of file" );
      }
      is_escaped = true;
      if !builder.initialized {
        init_string_builder( *builder );
        // copy in what we have so far
        append( *builder, value.str_val );
      }
      // eat the escape and process the next char (we know there is at least 1)
      advance( parser );
    }

    if data[ pos ] == #char "\"" && !is_escaped {
      // end of string; eat it and move on
      advance( parser );
      ate_close = true;
      break;
    }

    BACKSPACE : u8 : 8;
    FORMFEED  : u8 : 12;

    if builder.initialized {
      // we're copying
      if is_escaped {
        is_escaped = false;
        if data[ pos ] == {
          case #char "\""; #through;
          case #char "\\"; #through;
          case #char "/"; append( *builder, data[ pos ] ); advance( parser );

          case #char "b"; append( *builder, BACKSPACE ); advance( parser );
          case #char "f"; append( *builder, FORMFEED ); advance( parser );
          case #char "n"; append( *builder, #char "\n" ); advance( parser );
          case #char "r"; append( *builder, #char "\r" ); advance( parser );
          case #char "t"; append( *builder, #char "\t" ); advance( parser );
          case #char "u"; {
            // eat this "u"
            advance( parser );

            // eat the 1-4 chars
            digits : string;
            digits.data = data.data + pos;
            digits.count = 0;
            if parse_digits( parser, *digits, 4, .HEX ) != 4 {
              return .{}, failed( parser,
                                  "Expected exactly 4 hex digits afer \\u" );
            }

            // TODO: do something with these digits
            append( *builder, "UNICODE:'" );
            append( *builder, digits );
            append( *builder, "'" );
          }
          case; {
            return .{}, failed( parser,
                                tprint( "Invalid escape char '%'",
                                        pchar( *data[ pos ] ) ) );
          }
        }
      } else {
        // just eat the char
        append( *builder, data[ pos ] );
        advance( parser );
      }
    } else {
      assert( !is_escaped );
      // we're referencing
      value.str_val.count += 1;
      advance( parser );
    }
  }

  if !ate_close {
    return .{}, failed( parser, "String was not closed" );
  }

  if builder.initialized {
    // TODO tricky - in a real program, we'd worry about the memory managment,
    // but for this program we just leak _everything_
    value.str_val = builder_to_string( *builder );
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

DigitBase :: enum {
  DEC;
  HEX;
}

parse_digits :: inline ( using parser: *Parser,
                         number : *string, // must point into parser.data
                         max_digits: s64 = -1,
                         $base: DigitBase = .DEC ) -> s64 {
  count : s64 = 0;
  if max_digits == -1 {
    max_digits = data.count - pos;
  }

  #if base == .DEC {
    qualifies :: is_digit;
  } else {
    qualifies :: ( ch: u8 ) -> bool {
      return is_digit( ch )
        || ( ch >= #char "a" && ch <= #char "f" )
        || ( ch >= #char "A" && ch <= #char "F" );
    };
  }

  while !is_eof( parser ) && count < max_digits && qualifies( data[ pos ] ) {
    count += 1;
    number.count += 1;
    advance( parser );
  }
  return count;
}

parse_number :: ( using parser : *Parser ) -> Value, Parse_Result {
  value := Value.{ kind = .NUMBER };
  parse_result: Parse_Result;

  eat_whitespace( parser );

  old_pos := pos;
  number : string;
  number.data = data.data + pos;

  if eat_if_token( parser, #char "-" ) {
    number.count += 1;
  }

  if !parse_digits( parser, *number ) {
    return .{}, failed( parser, "Expected digits in number" );
  }

  if eat_if_token( parser, #char "." ) {
    number.count += 1;
    if !parse_digits( parser, *number ) {
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
    if !parse_digits( parser, *number ) {
      return .{}, failed( parser, "Expected digits after number sep" );
    }
  }

  // maybe replace this and just calculate it yourself. you already have all the
  // code required, though the actual algorithm implemented below is its own
  // project. Not sure that i care too much about that..
  value.num_val = string_to_float64( number );
  return value, ok( parser );
}

parse_boolean :: ( using parser : *Parser ) -> Value, Parse_Result {

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

parse_null :: ( using parser : *Parser ) -> Value, Parse_Result {

  eat_whitespace( parser );

  NULL :: "null";

  if pos + NULL.count < data.count {
    maybe_null: string;
    maybe_null.data = data.data + pos;
    maybe_null.count = NULL.count;
    if maybe_null == NULL {
      advance( parser, NULL.count );
      return Value.{ kind = .NULL }, ok( parser );
    }
  }

  return .{}, failed( parser, "Expected null" );
}

// DEBUGGING ONLY

print_value :: ( val: Value ) {
  builder : String_Builder;
  init_string_builder( *builder );
  defer reset( *builder );

  print_value_to_builder( *builder, val );
  append( *builder, "\n" );
  print( builder_to_string( *builder ) );
}

print_value_to_builder :: ( builder: *String_Builder,
                            val: Value,
                            indent: s16 = 0 ) {
  pindent :: ( builder: *String_Builder, indent: s16 ) {
    for 0..indent-1 {
      append( builder, " " );
    }
  }
  if #complete val.kind == {
    case .STRING; {
      print_to_builder( builder, "%", val.str_val );
    }
    case .NUMBER; {
      print_to_builder( builder, "%", val.num_val );
    }
    case .OBJECT; {
      append( builder, "{\n" );
      for val.obj_val {
        pindent( builder, indent + 2 );
        print_to_builder( builder, "% := ", it_index );
        print_value_to_builder( builder, it, indent + 4 );
        append( builder, ",\n" );
      }
      pindent( builder, indent );
      append( builder, "}" );
    }
    case .ARRAY; {
      append( builder, "[\n" );
      for val.arr_val {
        pindent( builder, indent + 2 );
        print_to_builder( builder, "[%] := ", it_index );
        print_value_to_builder( builder, it, indent + 4 );
        append( builder, ",\n" );
      }
      pindent( builder, indent );
      append( builder, "]" );
    }
    case .NULL; {
      append( builder, "NULL" );
    }
    case .BOOLEAN; {
      append( builder, ifx val.bool_val "true" else "false" );
    }
  }
}
