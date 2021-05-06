# These functions use the PCR equation to calculate
# the number of PCR products after N cycles starting
# from x molecules of the template.

PCRamp = function(x, n){
  # calculates number of amplicons starting from x initial DNA molecules in n cycles.
  x*((2^n)-2*n)
}


PCRamp.steps = function(x, n){
  # calculates progression of generation of amplicons starting from x initial DNA molecules in each of the n cycles
  x*(2^(1:n)-2*(1:n))
}