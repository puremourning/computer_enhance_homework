#load "basic_haversine.jai";

Args :: struct {
  num_segments := 100;                          @"?Number of line segments"
  num_partitions := 20;                         @"?Number of zones to sample"
  segments_file_name := "segments.json";        @"?File name for JSON output"
  distances_file_name := "distances.sp";        @"?File name for binary output"
  random_seed: u64 = 102948394247;              @"?Random seed"
}

generate :: ( args: Args )
  -> segments: [..] Segment, distances: [..] float64, sum: float64 {
  segments: [..] Segment;
  distances: [..] float64;
  array_reserve( *segments, args.num_segments );
  array_reserve( *distances, args.num_segments );

  sum : float64 = 0.0;

  Region :: struct {
    x_min : float64;
    y_min : float64;
    x_max : float64;
    y_max : float64;
  };

  bounds : [..]Region;
  if args.num_partitions > 0 {
    array_reserve( *bounds, args.num_partitions );

    for 0..args.num_partitions-1 {
      // TODO: make these partitions smaller, based on the index ?
      x1 := Random.random_get_within_range( -180, 180 );
      x2 := Random.random_get_within_range( -180, 180 );
      y1 := Random.random_get_within_range( -90, 90 );
      y2 := Random.random_get_within_range( -90, 90 );

      array_add( *bounds, Region.{
        min( x1, x2 ),
        min( y1, y2 ),
        max( x1, x2 ),
        max( x1, x2 ),
      } );
    }
  } else {
    array_add( *bounds, Region. { -180, -90, 180, 90 } );
  }

  for 0..args.num_segments-1 {
    region := bounds[ it % bounds.count ];

    segment := Segment.{
      xx Random.random_get_within_range( xx region.x_min, xx region.x_max ),
      xx Random.random_get_within_range( xx region.y_min, xx region.y_max ),
      xx Random.random_get_within_range( xx region.x_min, xx region.x_max ),
      xx Random.random_get_within_range( xx region.y_min, xx region.y_max ),
    };

    distance := basic_haversine_distance( segment );
    sum += distance;

    array_add( *distances, distance );
    array_add( *segments, segment );
  }

  return segments, distances, sum;
}

main :: () {
  success, args, is_set := parse_arguments(
    Args,
    Default_Argument_Flags
      & ~Argument_Flags.SORT_HELP
      & ~Argument_Flags.FREE_ARGUMENTS_ALLOWED
      | Argument_Flags.SHOW_HELP_ON_ERROR );

  if !success {
    print( "\nTry harder with the command line argument guessing.\n" );
    return;
  }

  Random.random_seed( args.random_seed );

  segments, distances, sum := generate( args );

  package: SP.Create_Package;
  defer SP.deinit( *package );
  SP.init( *package );
  SP.add( *package, "distances", xx distances );
  SP.write( *package, "distances.sp" );

  print_context := context;
  print_context.print_style.default_format_float.trailing_width = 17;
  push_context print_context {
    json_write_file( args.segments_file_name, Document.{ points = segments } );
    print( "The total sum was: %\n", sum );
  }

}

#import "jason";
#import "Basic";
SP :: #import "Simple_Package";
#import "Command_Line";

