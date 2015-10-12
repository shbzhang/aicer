;Calculate ZScale value of an image, use the algorithm in IRAF.
;based on:
;http://stsdas.stsci.edu/stsci_python_epydoc_2.7/docs/numdisplay/numdisplay.zscale-module.html

; MAX_REJECT = 0.5
; MIN_NPIXELS = 5
; GOOD_PIXEL = 0
; BAD_PIXEL = 1
; KREJ = 2.5
; MAX_ITERATIONS = 5

FUNCTION zsc_sample, image, maxpix

; Figure out which pixels to use for the zscale algorithm
; Returns the 1-d array samples
; Don't worry about the bad pixel mask or zmask for the moment
; Sample in a square grid, and return the first maxpix in the sample

	sz = size(image)
	nc = sz[1]
	nl = sz[2]
	stride = max ([1.0, sqrt((nc - 1) * (nl - 1) / float(maxpix))])
	stride = fix(stride)
	samples = (image[0:nc-1:stride,0:nl-1:stride])[*]
	samples = samples(where(~ finite(samples, /NAN)))
	return, samples
END

FUNCTION zsc_compute_sigma, flat, badpix

; Compute the rms deviation from the mean of a flattened array.
; Ignore rejected pixels

; Accumulate sum and sum of squares
	goodpixels = where(~ badpix)
	if goodpixels[0] eq -1 then return, !values.nan
	ngoodpixels = n_elements(goodpixels)
	if ngoodpixels eq 1 then return, 0.0 $
	else return, sqrt(variance(flat[goodpixels]))
END

FUNCTION zsc_fit_line, samples, KREJ = krej, MAX_ITERATIONS = max_interations

if ~ keyword_set(krej) then krej = 2.5
if ~ keyword_set(max_interations) then max_interations = 5
if ~ keyword_set(ngrow) then ngrow = 1

; First re-map indices from -1.0 to 1.0
npix = n_elements(samples)
xscale = 2.0 / (npix - 1)
xnorm = findgen(npix)
xnorm = xnorm * xscale - 1.0

ngoodpix = npix
minpix = max([5, fix(npix*0.5)])
last_ngoodpix = npix + 1

; This is the mask used in k-sigma clipping. 0 is good, 1 is bad
badpix = intarr(npix)
badpix[*] = 0

;
; Iterate

for niter = 1, max_interations do begin

	; Accumulate sums to calculate straight line fit
	goodpixels = where(~ badpix)
	ngoodpixels = n_elements(goodpixels)

	if (ngoodpixels ge last_ngoodpix) or (ngoodpixels lt minpix) then break

	; Slope and intercept
	result = linfit(xnorm[goodpixels], samples[goodpixels])
	intercept = result[0]
	slope = result[1]
	fitted = xnorm*slope + intercept
	flat = samples - fitted

	; Compute the k-sigma rejection threshold
	sigma = zsc_compute_sigma(flat, badpix)

	threshold = sigma * krej

	; Detect and reject pixels further than k*sigma from the fitted line
	newbad = where(abs(flat) gt threshold)
	IF newbad[0] ne -1 then badpix[newbad] = 1

	; Convolve with a kernel of length ngrow
;	kernel = numpy.ones(ngrow,dtype="int32")
;	badpix = numpy.convolve(badpix, kernel, mode='same')

endfor

; Transform the line coefficients back to the X range [0:npix-1]
zstart = intercept - slope
zslope = slope * xscale

return, {ngoodpixels:ngoodpixels, zstart:zstart, zslope:zslope}
END

FUNCTION zscale, image, NSAMPLES=nsamples, CONTRAST=contrast

;     Implement IRAF zscale algorithm
;     nsamples=1000 and contrast=0.25 are the IRAF display task defaults
;     bpmask and zmask not implemented yet
;     image is a 2-d numpy array
;     returns (z1, z2)

if ~ keyword_set(nsamples) then nsamples = 1000
if ~ keyword_set(contrast) then contrast = 0.25

	; Sample the image
	samples = zsc_sample(image, nsamples)
	npix = n_elements(samples)
	samples = samples[sort(samples)]
	zmin = samples[0]
	zmax = samples[npix-1]
	; For a zero-indexed array
	center_pixel = (npix - 1) / 2
	med = median(samples, /EVEN)

	; Fit a line to the sorted array of samples
	minpix = max([5, fix(npix * 0.5)])
	ngrow = max([1, fix(npix * 0.01)])
	result = zsc_fit_line(samples)

	if result.ngoodpixels lt minpix then begin
		z1 = zmin
		z2 = zmax
	endif else begin
		if contrast gt 0 then zslope = result.zslope / contrast else zslope = result.zslope
		z1 = max([zmin, med - (center_pixel - 1) * zslope])
		z2 = min([zmax, med + (npix - center_pixel) * zslope])
	endelse
	return, [z1, z2]
END
