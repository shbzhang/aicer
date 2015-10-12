;+
; NAME:
;       LUT2TBL
; PURPOSE:
;       Read color table from a .lut file of DS9 or GILDAS and return to an array
;
; CALLING SEQUENCE:
;       Result = lut2tbl(file)
;
; INPUT:
;		file - LUT CT file name
;
; OUTPUT:
;		Result - array of 256*3 as an IDL CT
;
; EXAMPLES:
;		result = lut2tbl('cool.lut')
;-
function lut2tbl, file

;load color from a .sao (DS9) file

	lut = (read_ascii(file)).(0)
	line = n_elements(lut[0,*])
	idx = findgen(!d.table_size)/(!d.table_size-1)*(line-1)
	r=interpolate((lut[0,*])[*],idx)
	r=r/max(r)*255
	g=interpolate((lut[1,*])[*],idx)
	g=g/max(g)*255
	b=interpolate((lut[2,*])[*],idx)
	b=b/max(b)*255

	return,[[r],[g],[b]]
end
