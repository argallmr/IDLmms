function cak_fchiprob, prob, degfree

; fast verions of the chisquare increment to for
; error determination given the desired probablility
; and number of degreess of freedom.

i=0l
p=0.0
chisq = 0

while p lt prob and i lt 1e3 do begin

	chisq = chisq + 1.
	p = igamma(degfree/2., chisq/2.)
	i = i + 1l

endwhile

i=0
chisq = chisq - 1.
p =0.0

while p lt prob and i lt 1e3 do begin

	chisq = chisq + 1e-1
	p = igamma(degfree/2., chisq/2.)
	i = i + 1l

endwhile

i=0
chisq = chisq - 1e-1
p=0.0

while p lt prob and i lt 1e3 do begin

	chisq = chisq + 1.e-2
	p = igamma(degfree/2., chisq/2.)
	i = i + 1l

endwhile

i=0
chisq = chisq - 1e-2
p=0.0

while p lt prob and i lt 1e2 do begin

	chisq = chisq + 1.e-3
	p = igamma(degfree/2., chisq/2.)
	i = i + 1

endwhile

i=0
chisq = chisq - 1e-3
p=0.0

while p lt prob and i lt 1e2 do begin

	chisq = chisq + 1.e-4
	p = igamma(degfree/2., chisq/2.)
	i = i + 1

endwhile

return, chisq

end
