#! /opt/local/bin/ruby
fcount = 0

print ARGV[0].to_i / 10.0
print "\t"
print ARGV[1].to_i / 20.0
print "\t"

while line = STDIN.gets
  next if line.length < 5
  if line.match "FALSE" then
    fcount += 1
    if fcount == 4 then
      print "o"
      exit
    end
    next
  end
  print "x"
  exit
end

print "?"

