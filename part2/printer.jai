Args :: struct {
  file_name := "segments.json";
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

  data, file_ok := read_entire_file( args.file_name );
  if !file_ok {
    print( "Unable to read %. Did you generate it?", args.file_name );
  }

  parser : Parser;
  parser.data = data;
  parser.current = data;
  val, parse_result := parse_value( *parser );

  if parse_result.failed {
    parse_error := cast( *Parse_Error ) *parse_result;
    print( "Failed to parse the JSON: % around %\n",
           parse_error.message,
           parse_error.pos );
    return;
  }

  print( "Result is a %\n", val.kind );
  print_value( val );
}

#load "json_parse.jai";

#import "Basic";
#import "File";
#import "Command_Line";
