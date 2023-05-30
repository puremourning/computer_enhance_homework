Math :: #import "Math";
Random :: #import "Random";

Segment :: struct {
  x0: float64;
  y0: float64;
  x1: float64;
  y1: float64;
}

Document :: struct {
  points: []Segment;
}

RADIUS : float64 : 6372.8;

radians :: inline ( deg: float64 ) -> float64 {
  return deg * ( Math.TAU64 / 360.0 );
}

square :: inline ( x: float64 ) -> float64 {
  return x*x;
}

basic_haversine_distance :: ( s: Segment ) -> float64 {

  lat1 := s.y0;
  lat2 := s.y1;
  lon1 := s.x0;
  lon2 := s.x1;

  dLat := radians(lat2 - lat1);
  dLon := radians(lon2 - lon1);
  lat1 = radians(lat1);
  lat2 = radians(lat2);

  a := square( Math.sin( dLat / 2.0 ) )
       + Math.cos( lat1 )
       * Math.cos( lat2 )
       * square( Math.sin( dLon / 2.0 ) );

  c := 2.0 * Math.asin( Math.sqrt( a ) );

  return RADIUS * c;
}
