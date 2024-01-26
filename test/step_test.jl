
using Formatting

name = "apply-load"
nlgeom = "YES"
inc = 1000000

fmt = "*Step, name={:s}, nlgeom={:s}, inc={:d16}"

lines = format(fmt, name, nlgeom, inc)