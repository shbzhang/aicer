;+
; NAME:
;       SAO2TBL
; PURPOSE:
;       Read color table from a .sao file of DS9 and return to an array
;
; CALLING SEQUENCE:
;       Result = sao2tbl(file)
;
; INPUT:
;		file - SAO CT file name
;
; OUTPUT:
;		Result - array of 256*3 as an IDL CT
;
; EXAMPLES:
;		result = sao2tbl('cool.sao')
;-
function farr, arr, x

;return the function value defined by array arr

	knot = arr[*,sort(arr[0,*])]
	knot = knot[*,uniq(knot[0,*])]
	knot = [[min(x)-1, knot[1,0]], [knot], [max(x)+1, knot[1,n_elements(knot[0,*])-1]]]
	f = x
	for i = 0, n_elements(x)-1 do begin
		kn = (where(x[i] lt knot[0,*]))[0]-1
		kx = [knot[0,kn],knot[0,kn+1]]
		ky = [knot[1,kn],knot[1,kn+1]]
		f[i] = ky[0]+(x[i]-kx[0])*(ky[1]-ky[0])/(kx[1]-kx[0])
	endfor
	return,f
end

function sao2tbl, file

;load color from a .sao (DS9) file

	openr, lun, file, /GET_LUN
	word = 0B
	text = ''
	while ~ eof(lun) do begin
		readu, lun, word
		if word ne 15B then text = text+string(word)
	endwhile
	close, lun
	pr = strpos(text, 'RED:')
	pg = strpos(text, 'GREEN:')
	pb = strpos(text, 'BLUE:')
	tr = strmid(text,pr+6,pg-pr-8)
	tg = strmid(text,pg+8,pb-pg-10)
	tb = strmid(text,pb+7,strlen(text)-pb-9)
	void = execute('r = [['+strjoin(strsplit(tr, ')(', /EXTRACT), '],[')+']]')
	if ~ void then return, -1
	void = execute('g = [['+strjoin(strsplit(tg, ')(', /EXTRACT), '],[')+']]')
	if ~ void then return, -1
	void = execute('b = [['+strjoin(strsplit(tb, ')(', /EXTRACT), '],[')+']]')
	if ~ void then return, -1
	lr = farr(r,findgen(256)/255)*255
	lg = farr(g,findgen(256)/255)*255
	lb = farr(b,findgen(256)/255)*255
	return,[[lr],[lg],[lb]]
end