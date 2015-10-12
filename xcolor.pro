;+
; NAME:
;       XCOLOR
;
; PURPOSE:
;       A palette as Paint in windows
;
; AUTHOR:
;       Shaobo Zhang
;       2 West Beijing Road, Rm 612
;       Phone: 086-025-83332106
;       E-mail: shbzhang@pmo.ac.cn
;
; CALLING SEQUENCE:
;       XCOLOR
;		XCOLOR, GROUP_LEADER = leader, COLOR = color, /MODAL
;
; OPTIONAL INPUT KEYWORD:
;       GROUP_LEADER, MODAL - see help of "widget_base()"
;		COLOR - color index of current color table. Default is 0
;
; EXAMPLES:
;		XCOLOR, color = 10
;		XCOLOR, GROUP_LEADER = event.top, COLOR = 10, /MODAL
;-
PRO XCOLOR_EVENT, event

COMMON xid, xtable, xlight, xresult, xhls, xrgb, status, prewindow
COMMON xco, hls, rgb, xcol
COMMON xim, ximage, xbar, xarrow, xplane

IF event.id EQ xtable THEN BEGIN
	IF event.press NE 0 THEN status[0] = 1
	IF event.release NE 0 THEN status[0] = 0
	IF status[0] EQ 0 THEN RETURN
	hls[0] = event.x & hls[2] = event.y/255.
	IF event.x LT 0  THEN hls[0] = 0
	IF event.x GT 359 THEN hls[0] = 359
	IF event.y LT 0 THEN hls[2] = 0.
	IF event.y GT 255 THEN hls[2] = 1.
	COLOR_CONVERT, hls, rgb, INTERLEAVE = 1, /HLS_RGB

	WIDGET_CONTROL, event.id, GET_VALUE = temp
	WSET, temp
	TV, ximage, TRUE = 3
	TV, intarr(8,3,3), hls[0]-9, hls[2]*255-1, TRUE = 3
	TV, intarr(8,3,3), hls[0]+2, hls[2]*255-1, TRUE = 3
	TV, intarr(3,8,3), hls[0]-1, hls[2]*255-9, TRUE = 3
	TV, intarr(3,8,3), hls[0]-1, hls[2]*255+2, TRUE = 3

	WIDGET_CONTROL, xlight, GET_VALUE = temp
	WSET, temp
	xbar[*,*,0] = hls[0]
	FOR i=0,34 DO xbar[i,*,1] = findgen(256)/255.
	xbar[*,*,2] = hls[2]
	COLOR_CONVERT, xbar, xbaro, INTERLEAVE = 2, /HLS_RGB
	TV, xbaro, TRUE = 3

	WIDGET_CONTROL, xresult, GET_VALUE = temp
	WSET, temp
	xplane[*,*,0] = rgb[0]
	xplane[*,*,1] = rgb[1]
	xplane[*,*,2] = rgb[2]
	TV, xplane, TRUE = 3

	WIDGET_CONTROL, xhls[0], SET_VALUE = string(hls[0], FORMAT = '(I0)')
	WIDGET_CONTROL, xhls[2], SET_VALUE = string(hls[2]*255, FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[0], SET_VALUE = string(rgb[0], FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[1], SET_VALUE = string(rgb[1], FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[2], SET_VALUE = string(rgb[2], FORMAT = '(I0)')

	RETURN
ENDIF

IF event.id EQ xlight THEN BEGIN
	IF event.press NE 0 THEN status[1] = 1
	IF event.release NE 0 THEN status[1] = 0
	IF status[1] EQ 0 THEN RETURN
	hls[1] = event.y/255.
	IF event.y LT 0 THEN hls[1] = 0.
	IF event.y GT 255 THEN hls[1] = 1.
	COLOR_CONVERT, hls, rgb, INTERLEAVE = 1, /HLS_RGB

	WIDGET_CONTROL, xlight, GET_VALUE = temp
	WSET, temp
	xarrow[*] = 0
	FOR i=0,7 DO xarrow[i,max([0,hls[1]*255-i]):min([hls[1]*255+i,255]),*] = 255
	TV, xarrow, 35, 0, TRUE = 3

	WIDGET_CONTROL, xresult, GET_VALUE = temp
	WSET, temp
	xplane[*,*,0] = rgb[0]
	xplane[*,*,1] = rgb[1]
	xplane[*,*,2] = rgb[2]
	TV, xplane, TRUE = 3

	WIDGET_CONTROL, xhls[1], SET_VALUE = string(hls[1]*255, FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[0], SET_VALUE = string(rgb[0], FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[1], SET_VALUE = string(rgb[1], FORMAT = '(I0)')
	WIDGET_CONTROL, xrgb[2], SET_VALUE = string(rgb[2], FORMAT = '(I0)')

	RETURN
ENDIF

which = event.id EQ [xhls,xrgb]
IF max(which) THEN BEGIN
	IF (size(event, /STRUCTURE)).structure_name EQ 'WIDGET_KBRD_FOCUS' THEN BEGIN
		IF event.enter EQ 1 THEN RETURN
	ENDIF
	WIDGET_CONTROL, event.id, GET_VALUE = text
	text = max([0,fix('0'+text)])
	IF ~ where(which) THEN text = min([text,359]) ELSE text = min([text,255])
	WIDGET_CONTROL, event.id, SET_VALUE = string(text, FORMAT = '(I0)')
	IF where(which) LE 2 THEN BEGIN
		IF where(which) EQ 0 THEN hls[0] = text ELSE hls[where(which)] = text/255.
		COLOR_CONVERT, hls, rgb, INTERLEAVE = 1, /HLS_RGB
		WIDGET_CONTROL, xrgb[0], SET_VALUE = string(rgb[0], FORMAT = '(I0)')
		WIDGET_CONTROL, xrgb[1], SET_VALUE = string(rgb[1], FORMAT = '(I0)')
		WIDGET_CONTROL, xrgb[2], SET_VALUE = string(rgb[2], FORMAT = '(I0)')
	ENDIF ELSE BEGIN
		rgb[where(which) MOD 3] = text
		COLOR_CONVERT, rgb, hls, INTERLEAVE = 1, /RGB_HLS
		WIDGET_CONTROL, xhls[0], SET_VALUE = string(hls[0], FORMAT = '(I0)')
		WIDGET_CONTROL, xhls[1], SET_VALUE = string(hls[1]*255, FORMAT = '(I0)')
		WIDGET_CONTROL, xhls[2], SET_VALUE = string(hls[2]*255, FORMAT = '(I0)')
	ENDELSE

	WIDGET_CONTROL, xtable, GET_VALUE = temp
	WSET, temp
	TV, ximage, TRUE = 3
	TV, intarr(8,3,3), hls[0]-9, hls[2]*255-1, TRUE = 3
	TV, intarr(8,3,3), hls[0]+2, hls[2]*255-1, TRUE = 3
	TV, intarr(3,8,3), hls[0]-1, hls[2]*255-9, TRUE = 3
	TV, intarr(3,8,3), hls[0]-1, hls[2]*255+2, TRUE = 3

	WIDGET_CONTROL, xlight, GET_VALUE = temp
	WSET, temp
	xbar[*,*,0] = hls[0]
	FOR i=0,34 DO xbar[i,*,1] = findgen(256)/255.
	xbar[*,*,2] = hls[2]
	COLOR_CONVERT, xbar, xbaro, INTERLEAVE = 2, /HLS_RGB
	TV, xbaro, TRUE = 3
	xarrow[*] = 0
	FOR i=0,7 DO xarrow[i,max([0,hls[1]*255-i]):min([hls[1]*255+i,255]),*] = 255
	TV, xarrow, 35, 0, TRUE = 3

	WIDGET_CONTROL, xresult, GET_VALUE = temp
	WSET, temp
	xplane[*,*,0] = rgb[0]
	xplane[*,*,1] = rgb[1]
	xplane[*,*,2] = rgb[2]
	TV, xplane, TRUE = 3
ENDIF

WIDGET_CONTROL, event.id, GET_UVALUE = action
CASE action OF
	'DONE':BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		ct = 0
		TVLCT, ct, /GET
		ct[xcol, *] = rgb
		TVLCT, ct
		WSET, prewindow
	END
	'HELP':BEGIN
		PRINT,'XColor by shbzhang'
	END
	ELSE:
ENDCASE
END



PRO XCOLOR, GROUP_LEADER = leader, COLOR = color, MODAL = modal

	;replace xpcolor

COMMON xid, xtable, xlight, xresult, xhls, xrgb, status, prewindow
COMMON xco, hls, rgb, xcol
COMMON xim, ximage, xbar, xarrow, xplane

prewindow = !D.window

ct = 0
TVLCT, ct, /GET
IF ~ keyword_set(color) THEN xcol = 0 ELSE xcol = color
IF xcol GE 256 THEN xcol = 255
rgb = ct[xcol, *]
COLOR_CONVERT, rgb, hls, INTERLEAVE = 1, /RGB_HLS

status = [0,0]

if (n_elements(leader) GT 0L) then $
	xbase = widget_base(TITLE = "XColor", /COLUMN, GROUP_LEADER = leader, $
		MODAL = keyword_set(modal)) $
else $
	xbase = widget_base(TITLE="XColor", /COLUMN)

xbase1 = widget_base(xbase, ROW = 1)
xtable = widget_draw(xbase1, /BUTTON_EVENT, /MOTION_EVENT, $
	XSIZE = 360, YSIZE = 256)
xlight = widget_draw(xbase1, /BUTTON_EVENT, /MOTION_EVENT, $
	XSIZE = 45, YSIZE = 256)

xbase1 = widget_base(xbase, ROW = 1)
xresult = widget_draw(xbase1, XSIZE = 110, YSIZE = 70)

xbase2 = widget_base(xbase1, ROW = 3)
xhue = widget_label(xbase2, VALUE = ' Hue       :')
xhue = widget_text(xbase2, VALUE = string(hls[0], FORMAT = '(I0)'), UVALUE = 'HUE', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)
xred = widget_label(xbase2, VALUE = ' Red  :')
xred = widget_text(xbase2, VALUE = string(rgb[0], FORMAT = '(I0)'), UVALUE = 'RED', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)
xlightness = widget_label(xbase2, VALUE = ' Lightness :')
xlightness = widget_text(xbase2, VALUE = string(hls[1]*255, FORMAT = '(I0)'), UVALUE = 'LIGHTNESS', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)
xgreen = widget_label(xbase2, VALUE = ' Green:')
xgreen = widget_text(xbase2, VALUE = string(rgb[1], FORMAT = '(I0)'), UVALUE = 'GREEN', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)
xsaturation = widget_label(xbase2, VALUE = ' Saturation:')
xsaturation = widget_text(xbase2, VALUE = string(hls[2]*255, FORMAT = '(I0)'), UVALUE = 'SATURATION', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)
xblue = widget_label(xbase2, VALUE = ' Blue :')
xblue = widget_text(xbase2, VALUE = string(rgb[2], FORMAT = '(I0)'), UVALUE = 'BLUE', $
	/EDITABLE, /NO_NEWLINE, /WRAP, /KBRD_FOCUS_EVENTS, XSIZE = 3)

xhls = [xhue, xlightness, xsaturation]
xrgb = [xred, xgreen, xblue]

xbase1 = widget_base(xbase, ROW = 1, /GRID_LAYOUT, /ALIGN_CENTER)
xdone = widget_button(xbase1, VALUE = 'Done', UVALUE = 'DONE')
xhelp = widget_button(xbase1, VALUE = 'Help', UVALUE = 'HELP')

WIDGET_CONTROL, xbase, /REALIZE

ximage = fltarr(360,256,3)
FOR i=0,255 DO ximage[*,i,0] = findgen(360)
ximage[*,*,1] = 0.5
FOR i=0,359 DO ximage[i,*,2] = findgen(256)/255.
COLOR_CONVERT, ximage, ximage, INTERLEAVE = 2, /HLS_RGB

xbar = fltarr(35,256,3)
xbar[*,*,0] = hls[0]
FOR i=0,34 DO xbar[i,*,1] = findgen(256)/255.
xbar[*,*,2] = hls[2]
COLOR_CONVERT, xbar, xbaro, INTERLEAVE = 2, /HLS_RGB

xarrow = bytarr(8,256,3)
xarrow[*] = 0
FOR i=0,7 DO xarrow[i,max([0,hls[1]*255-i]):min([hls[1]*255+i,255]),*] = 255

xplane = bytarr(110,70,3)
xplane[*,*,0] = rgb[0]
xplane[*,*,1] = rgb[1]
xplane[*,*,2] = rgb[2]

DEVICE, DECOMPOSED = 0
WIDGET_CONTROL, xtable, GET_VALUE = temp
WSET, temp
TV, ximage, TRUE = 3
TV, intarr(8,3,3), hls[0]-9, hls[2]*255-1, TRUE = 3
TV, intarr(8,3,3), hls[0]+2, hls[2]*255-1, TRUE = 3
TV, intarr(3,8,3), hls[0]-1, hls[2]*255-9, TRUE = 3
TV, intarr(3,8,3), hls[0]-1, hls[2]*255+2, TRUE = 3

WIDGET_CONTROL, xlight, GET_VALUE = temp
WSET, temp
TV, xbaro, TRUE=3
TV, xarrow, 35, 0, TRUE = 3

WIDGET_CONTROL, xresult, GET_VALUE = temp
WSET, temp
TV, xplane, TRUE=3

XMANAGER, 'xcolor', xbase
END
