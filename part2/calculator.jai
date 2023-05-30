Args :: struct {
  segments_file_name := "segments.json";
  distances_file_name := "distances.sp";
}


main :: () {
  cmdline_ok, args := parse_arguments(
    Args,
    Default_Argument_Flags
      & ~Argument_Flags.SORT_HELP
      & ~Argument_Flags.FREE_ARGUMENTS_ALLOWED
      | Argument_Flags.SHOW_HELP_ON_ERROR );

  if !cmdline_ok {
    print( "\nTry harder with the command line argument guessing.\n" );
    return;
  }

  data, file_ok := read_entire_file( args.segments_file_name );
  if !file_ok {
    print( "Unable to read %. Did you generate it?", args.segments_file_name );
  }

  parser : Parser;
  parser.data = data;
  val, parse_result := parse_value( *parser );

  if parse_result.failed {
    print( "Failed to parse the JSON: % around %\n",
           parse_result.message,
           parse_result.pos );
    return;
  }

  print( "Result is a %\n", val.kind );
  print_value( val );
}

print_value :: ( val: Value ) {
  builder : String_Builder;
  defer free_buffers( *builder );

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

#load "json_parse.jai";

#import "Basic";
#import "File";
#import "Command_Line";
