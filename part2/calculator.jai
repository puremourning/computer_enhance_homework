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
  parser.current = data;
  val, parse_result := parse_value( *parser );

  if parse_result.failed {
    print( "Failed to parse the JSON: % around %\n",
           parse_result.message,
           parse_result.pos );
    return;
  }

  print( "Result is a %\n", val.kind );
  
  // now read the parts we want
  if val.kind != .OBJECT {
    print( "Expecting an object\n" );
    return;
  }

  points := table_find_pointer( val.obj_val, "points" );
  if !points || points.kind != .ARRAY {
    print( "Expected a points array" );
    return;
  }

  distances: []float64;
  expected_sum: float64;
  {
    file, success := read_entire_file( args.distances_file_name );
    if !success {
      print( "Unable to open distances file %\n", args.distances_file_name );
      return;
    }

    distances.count = <<( cast( *s64 ) file.data );
    advance( *file, size_of( type_of( s64 ) ) );

    expected_sum = <<( cast( *float64 ) file.data );
    advance( *file, size_of( type_of( float64 ) ) );

    distances.data = xx file.data;

    print( "Read % distances from %\n",
           distances.count,
           args.distances_file_name );
  }

  print_context := context;
  print_context.print_style.default_format_float.trailing_width = 17;
  print_context.print_style.default_format_float.mode = .SHORTEST;
  push_context print_context {
  sum : float64 = 0;
    for points.arr_val {
      if it.kind != .OBJECT {
        print( "Expected each element to be an object\n" );
        return;
      }
      x0, y0, x1, y1 := table_find_pointer( it.obj_val, "x0" ),
                        table_find_pointer( it.obj_val, "y0" ),
                        table_find_pointer( it.obj_val, "x1" ),
                        table_find_pointer( it.obj_val, "y1" );

      if !x0 || !y0 || !x1 || !y1 {
        print( "Expected x0,y0 and x1,y1 elements in object\n" );
        return;
      }

      distance := basic_haversine_distance( Segment. {
        x0.num_val,
        y0.num_val,
        x1.num_val,
        y1.num_val,
      } );
      if it_index >= distances.count {
        print( "Too many entries (%) for the distances file; do they match ?\n",
               it_index + 1 );
        return;
      }
      if distance != distances[ it_index ] {
        print( "Calcualted values don't match dumped data at index %:\n",
               it_index );
        print( "  x0,y0: %,%\n", x0.num_val, y0.num_val );
        print( "  x1,y1: %,%\n", x1.num_val, y1.num_val );
        print( "  Calcualted: %\n", distance );
        print( "  Generated: %\n\n", distances[ it_index ] );
      }
      sum += distance;
    }

    print( "The sum was %\n", sum );
    if sum != expected_sum {
      print( "The sum did not match expected %\n", expected_sum );
    }
  }
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
#load "basic_haversine.jai";

#import "Basic";
#import "File";
#import "Command_Line";
