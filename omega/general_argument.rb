template = <<EOF
non_neg := { [a,b,c,d,e] :
a >= 0 &&
b >= 0 &&
c >= 0 &&
d >= 0 &&
e >= 0};

double0 := { [a,b,c,d,e] :
   (!((ratio * (1 + e) <= 20 * (1 + (c + d + 1)) &&
   (delta * (a + 1) >= 10* (b + 1) &&
    delta * (b + 1) >= 10* (a + 1) &&
    !
    ((delta * (a + 1) >= 10* (b + 1 + 1) &&
     delta * (b + 1 + 1) >= 10* (a + 1))) &&
    c + d + 1 + e + 1 = b + 1 &&
    delta * (c + 1) >= 10* (d + 1) &&
    delta * (d + 1) >= 10* (c + 1) &&
    delta * (c + d + 1 + 1) >= 10* (e + 1) &&
    delta * (e + 1) >= 10* (c + d + 1 + 1))))) ||
    ((delta * (a + 1) >= 10* (c + 1) &&
     delta * (c + 1) >= 10* (a + 1)) &&
    (delta * (d + 1) >= 10* (e + 1) &&
     delta * (e + 1) >= 10* (d + 1)) &&
    delta * (a + c + 1 + 1) >= 10* (d + e + 1 + 1) &&
    delta * (d + e + 1 + 1) >= 10* (a + c + 1 + 1))} ;

double1 := { [a, b, c, d, e] :
   (!((ratio * (1 + e) <= 20 * (1 + (c + d + 1))) &&
    delta * (1 + a + 1) >= 10* (b + 1) &&
    delta * (b + 1) >= 10* (1 + a + 1) &&
    !
    ((delta * (a + 1) >= 10* (b + 1) &&
     delta * (b + 1) >= 10* (a + 1))) &&
    c + d + 1 + e + 1 = b &&
    delta * (c + 1) >= 10* (d + 1) &&
    delta * (d + 1) >= 10* (c + 1) &&
    delta * (c + d + 1 + 1) >= 10* (e + 1) &&
    delta * (e + 1) >= 10* (c + d + 1 + 1))) || 
    ((delta * (a + 1) >= 10* (c + 1) &&
     delta * (c + 1) >= 10* (a + 1)) &&
    (delta * (d + 1) >= 10* (e + 1) &&
     delta * (e + 1) >= 10* (d + 1)) &&
    delta * (a + c + 1 + 1) >= 10* (d + e + 1 + 1) &&
    delta * (d + e + 1 + 1) >= 10* (a + c + 1 + 1))
    } ;


single0 := { [a, b, c, d, e] :
   (! (20 * (1 + (c + d + 1)) < ratio * (1 + e))) ||
   ((!((delta * (a + 1) >= 10* (b + 1) &&
         delta * (b + 1) >= 10* (a + 1) &&
    (!
    ((delta * (a + 1) >= 10* (b + 1 + 1) &&
     delta * (b + 1 + 1) >= 10* (a + 1)))) &&
    c + d + 1 + e + 1 = b + 1 &&
    delta * (c + 1) >= 10* (d + 1) &&
    delta * (d + 1) >= 10* (c + 1) &&
    delta * (c + d + 1 + 1) >= 10* (e + 1) &&
    delta * (e + 1) >= 10* (c + d + 1 + 1)))) ||
    (delta * (a + 1) >= 10* (c + d + 1 + 1) &&
    delta * (c + d + 1 + 1) >= 10* (a + 1)) &&
    (delta * (c + 1) >= 10* (d + 1) &&
    delta * (d + 1) >= 10* (c + 1)) &&
    delta * (a + c + d + 1 + 1 + 1) >= 10* (e + 1) &&
    delta * (e + 1) >= 10* (a + c + d + 1 + 1 + 1))}; 

single1 := { [a,b,c,d,e] :
   (! (20 * (1 + (c + d + 1)) < ratio * (1 + e))) ||
   ((!((delta * (1 + a + 1) >= 10* (b + 1) &&
   delta * (b + 1) >= 10* (1 + a + 1) &&
   (!
   ((delta * (a + 1) >= 10* (b + 1) &&
   delta * (b + 1) >= 10* (a + 1)))) &&
   c + d + 1 + e + 1 = b &&
   delta * (c + 1) >= 10* (d + 1) &&
   delta * (d + 1) >= 10* (c + 1) &&
   delta * (c + d + 1 + 1) >= 10* (e + 1) &&
   delta * (e + 1) >= 10* (c + d + 1 + 1)))) ||
   ((delta * (a + 1) >= 10* (c + d + 1 + 1) &&
   delta * (c + d + 1 + 1) >= 10* (a + 1)) &&
   (delta * (c + 1) >= 10* (d + 1) &&
   delta * (d + 1) >= 10* (c + 1)) &&
   delta * (a + c + d + 1 + 1 + 1) >= 10* (e + 1) &&
   delta * (e + 1) >= 10* (a + c + d + 1 + 1 + 1))) };

example (non_neg - single0);
example (non_neg - single1);
example (non_neg - double1);
example (non_neg - double0);
EOF

  print template.gsub("delta", ARGV[0]).gsub("ratio", ARGV[1])


  
