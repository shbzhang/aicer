;HISTORY
;Written by Shaobo, Zhang, 26 March 2008.
;26.Mar.08, GUI of main window
;29.Mar.08, define structure of image plot
;04.Apr.08, list control button and save/load button
;09.Apr.08, finish save and load button
;24.Jun.08, add lock button to the main base and coordinate parameter to the layer structure
;25.Jun.08, define system variable !layercache to send&receive data between procedures
;26.Jun.08, add some widget to image procdure and make widget module arrange well
;27.Jun.08, finish the GUI part of image procedure and some function of the widget
;02.Jul.08, the widgets position are now dicided by proceduces themselves; finish the image proceduce.
;31.Jul.08, level part for contour widget
;04.Aug.08, color function part for contour lines
;25.Aug.08, image part of main procedure
;26.Aug.08, convert procedure and RGB image plot part were done
;01.Sep.08, finish the text part, change the form of contour widget
;23.Oct.08, change the way of drawing boundary lines to  regular form and irregular form
;08,Dec.08, procedure XColor is added to change color
;
;UPDATA
;beta0.1
;the former program FCP
;beta0.2
;1.the open/save and help button were move into the menu of the interface
;2.image layer can now draw the fits file in a contour mode
;3.widgets of the same function have been write into a module
;  the appearance of the whole program can be changed more easily
;4.add the missing value choice for CONTOUR, which can fill the regular mapping gaps with missing value
;beta0.2.1
;1.fix a widget display error in point part of CONTOUR widget
;2.add scale choice to Pixal Distribution module for IMAGE widget, like DS9
;  a new function to calculate the ZScale value of a image were added, based on the algorithm in IRAF
;3.display what's the problem when error encountered
;beta0.2.2
;1.NEW LAYER:shape was added for things like beamsize, ruler, arrow
;2.colorbar of image layer can work and the axis of the bar was added
;3.fix the mistake that point can not be drawn when the symbol type was not change for the first time
;beta0.2.3
;1.Image layer: nan pixals can be draw as the maximum/minmum color of the fits file
;2.Text layer: can use WCS as the position to draw text
;beta0.2.4
;1.Shape layer: ellipse and box can be plotted with correct size and angle at different DEC.
;NOTE: in this version, the size of ellipse has been changed from radius to diameter.
;beta0.2.5
;1.help
;2.NaN value color for IMAGE widget
;beta0.2.6
;1.Image layer: fix a critical error in drawing RGB images.
;Note: this error may cause unexpected NaN values appearing in some part of the images.
;2.Setting: paper size is editable now, format as A4,B5,5*5,10*30,etc are accepted.
;beta0.2.7
;1.Contour layer: minus line thick represent dashed linestyle.
;2.Image layer: use compound widgets for coordinates conversion (no visual change)
;3.Optimize the color widget to display faster
;4.Contour layer: modify the irregular mapping contour, more fast.
;5.Point layer: use compound widgets for draw type.
;  new type: draw the vector of polarization.
;beta0.2.8
;1.Point layer: polarization angle are in degree unit and orient from North to East
;  "draw source name" now do not draw names out side the plot range.
;2.Flexible input for tick format
;beta0.2.9
;1.Point layer: fix a error when drawing with symbol "line"
;2.Point layer: add the new type "Vector" to draw bars or arrows
;beta0.2.10
;1.changes for dealing with image created by GILDAS
;2.rename the main file name with lowercase
;beta0.2.11
;1.correct the error while reading a coordinate starting with '-00'
;2.adjust the position of some widgets
;3.the color table now accept LUT table given by ds9 or GILDAS
;beta0.2.12
;1.correct image scale
;2.adjust the plot position according to the tickname length, and color bar.
;3.accept fits file with epoch B1950
;beta0.2.13
;1.flag option for POINT layer
;2.correct the POINT plot for coordinate below 0 or over 180 degree
;beta0.2.14
;1.a coordinate system type should be set before plot.
;2.Image layer: the convert module has been removed, all the image will be converted automatically.
;3.AICer now can plot ordinary maps (eg. PV map) under the coordinate system type "Ordinary plot"
;beta0.2.15
;1.Shape layer: aicer can draw filled shapes (require new astron lib)
;2.Image layer: numbers for color bar have the same font as the axis numbers
;2.Contour layer: numbers for contour label have the same font as the axis numbers
;3.Point layer: correct the saving problem of flag column
;beta0.2.16
;1.Image layer: fix a bug when using new astron lib (verson after Jul,2013)
;2.optimize the plot range for ordinary plot
;beta0.2.17
;1.multi-panel plot
;2.new format for save files, use "save" and "restore" command
;3.Point layer: add "Histogram" in the symbol type. This option could be used to plot spectra.
;4.Shape layer: the shapes are drawn with noclip option. Shapes could be plotted out of the plot range.
;5.correct a coordiante bug while using "Ordernary plot".
;TODO
;04,Jan.09, *the help for AICer has not been updated to the newest version
;           resize for module_frame can not work on X windows
;           find a better way to transfer layer information without using system variable
;26,Feb.09, * draw shapes on the plot(beamsize, arrow, etc)
;           * colorbar function of image layer
;29,Sep,09, * NAN data color problem in IMAGE layer, max/min?
;08,Oct,09, * correct the error in SHAPE layer, rular/ellipse/rectangle can be displayed in correct angle and size.
;16,Apr,10, draw arch or spline

FUNCTION layer_define
    layer={aicerlist, $
        valid:0, $  ;whether the created layer is vaild
        uname:'', $ ;user name
        name:'', $  ;internal name
        type:'A4', $
        ;IMAGE:SINGLE/RGB/CONTOUR map
        ;CONTOUR:SAME/DIFFERENT color for each level
        ;POINT:POINT/VECTOR
        ;CUSTOM:paper size
        ;SHAPE:shape to draw
	;PANEL:AIC/SCRIPT
        file:strarr(3), $   ;file name, only use the first element except RGB map
        ra:'', $	;-	PANEL:page offset X
        dec:'', $	;^	PANEL:page offset Y
        raof:'', $	;|
        raot:'', $	;|coordinate for current layer
        decof:'', $	;|
        decot:'', $	;|
        rau:'', $	;v
        decu:'', $	;-
        lenu:'', $	;CONTOUR:unit of boundary length
        cra:0, $
        cdec:0, $
        ;CONTOUR:column of ra,dec
        ;POINT:column of ra,dec
        ;TEXT:x,y
        ;CUSTOM:format of x,y coordinate number
        cvalue:0, $
        ;CONTOUR:column of intensity
        ;POINT:column of flux
        cinter:0, $
        ;CONTOUR:-2 -> 1%;-1 -> 1.0;ge 0 -> column of sigma
        ;POINT:column of source name
        cerror:0, $
        ;POINT:column of error
        times:0.0, $
        ;CONTOUR:level interval = times * cinter
	;PANEL: page scale
        ctrust:0, $
        ;*CONTOUR:column of trust value
        ;POINT:colume of flag to filter POINT data
        texti:'Right Ascension (J2000)', $
        ;CONTOUR:levels input
        ;TEXT:output text
        ;COSTOM:X label
        texto:'Declination (J2000)', $
        ;CONTOUR:levels output
        ;POINT:flag for the points to draw
        ;COSTOM:Y label
        length:0.0, $
        ;CONTOUR:map spacing or max boundary segment length
        Bval:[0.0d,0.0d,-1.0d,-1.0d,0.0d,0.0d], $
        ;IMAGE:scaling value for images
        ;CONTOUR:blanking value
        ;CUSTOM:
        ;[0:1]x/y tick gap
        ;[2:3]x/y minor
        ;[4:5]x/y label position
        missing:0.0, $
        ;CONTOUR:missing value for regular mapping gaps
        dsmooth:12, $
        ;IMAGE:degree of leefilt for IMAGE
        ;CONTOUR:width of smooth for contour lines
        ;CUSTOM:quality of image layer
        afont:'!3 !3 !3 !3 !3', $
        ;CONTOUR:symbol type
        ;TEXT:char font
        ;POINT:symbol type
        ;CUSTOM:line(0),grid(0),number(1),label(1),title(1)
        asize:'0 0 1 1 1', $
        ;CONTOUR:point symbol size(1), label charsize(1)
        ;TEXT:char size
        ;POINT:symbol size and arrow length scale
        ;CUSTOM:line(0),grid(0),number(1),label(1),title(1)
        athick:'1 1 1 1 1', $
        ;CONTOUR:line thick
        ;TEXT:'1.0' for char thick
        ;POINT:'1.0' for symbol thick
        ;CUSTOM:line(1),grid(1),number(1),label(1),title(1)
        aorient:'[I0]!Uh!N[I0]!Um!N[F0.1]!Us!N%'+"[I02]!Uo!N[I02]'"+'[F04.1]"', $
        ;TEXT:char orientation
        ;POINT:length scale of vector point
        ;CUSTOM:tick format
        dthick:1.0, $
        ;CONTOUR:default thick for CONTOUR
        parameter:[0,0,0,0,0,0,0,0,0], $
        ;MAIN:
        ;[1]WCS type
        ;IMAGE:
        ;[0]type of convert coordinate (0=nocovert,1=GA->CE,2=CE->GA)
        ;[3:5]data scaling type for 3 channels
        ;[6]whether the SINGLE image is a CONTOUR image 
	;  (2=contour & diff, 1=contour & same, -1=single & same, -2=single & diff)
        ;[7]whether to draw a colorbar (0=no,1=draw)
        ;CONTOUR:
        ;[0]draw point and fill (2=draw & fill, 1=draw & unfill, -1=not draw & unfill, -2=not draw & fill)
        ;[1]draw label (0=no,1=draw)
        ;[2]draw downhill line (0=no,1=draw)
        ;[3]fill contour (0=no,1=fill)
        ;[4]draw boundary line (0=no,1=draw)
        ;[5]set blanking value (0=no,1=set)
        ;[6]regular or not (0=irregular,1=regular)
        ;[8]set missing value (0=no,1=set)
        ;POINT:
        ;[0](0=Normal point,1=Resize according to flux,2=Draw source name)
        ;[1]draw filled point (0=not fill, 1=fill)
        ;[2]use flag (0=not use,1=use)
        ;TEXT:
        ;SHAPE
        ;[0]filled shape(0=unfill, 1=fill)
        ;CUSTOM:
        ;[0]whether to extend axis
        ;[1]coordinate system(0=celestial,1=galactic)
        ct:[[indgen(256)],[indgen(256)],[indgen(256)]], $
        ;IMAGE:color table image, first element for NaN values
        ;CONTOUR:color for contour lines
        ;POINT:ct[0,*] for points color
        ;TEXT:ct[0,*] for char color
        ;CUSTOM:ct[0:4,*] for line, grid, number, label, title
        pct:''}
        ;IMAGE:file name of personal color table
        ;CONTOUR:file name of personal color table
        ;CUSTOM:title text
    
    return,layer
end

PRO AICer_fillblank, event, sinfo
    
    ;fill the blank of coordinate and range according to file data
    
    ;WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), GET_VALUE = sinfo
    ;sinfo = widget_info(widget_info(event.top, FIND_BY_UNAME = 'wsou'), /COMBOBOX_GETTEXT)
    
    sinfo = strsplit(sinfo, /EXTRACT)
    scorr = where(strpos(sinfo,':') NE -1)
    IF n_elements(scorr) EQ 2 THEN BEGIN
        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), SET_VALUE = sinfo[scorr[0]]
        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), SET_VALUE = sinfo[scorr[1]]
    ENDIF
    
END
;--------------------------------------------------------------------------------------------------



FUNCTION layer_default, layer
    
    ;default a layer
    layer.valid = 0
    layer.uname = ''
    layer.name = ''
    layer.type = ''
    layer.file = ['','','']
    layer.ra = ''
    layer.dec = ''
    layer.raof = ''
    layer.raot = ''
    layer.decof = ''
    layer.decot = ''
    layer.rau = ''
    layer.decu = ''
    layer.cra = 0
    layer.cdec = 0
    layer.cvalue = 0
    layer.cinter = 0
    layer.cerror = 0
    layer.times = 1.0
    layer.ctrust = 0
    layer.length = 0.0
    layer.Bval[*] = 0
    layer.missing = 0.0
    layer.texti = 'Right Ascension (J2000)'
    layer.texto = 'Declination (J2000)'
    layer.dsmooth=8
    layer.afont=''
    layer.asize=''
    layer.athick=''
    layer.aorient='[I0]!Uh!N[I0]!Um!N[F0.1]!Us!N%'+"[I02]!Uo!N[I02]'"+'[F04.1]"'
    layer.dthick=1.0
    layer.parameter=[1,0,0,0,0,0,0,0,0]
    layer.ct=[[indgen(256)],[indgen(256)],[indgen(256)]]
    layer.pct=''
    RETURN, layer
END
;--------------------------------------------------------------------------------------------------



FUNCTION AICer_fillheader, event, header
    
    ;save coordinate and other infomation to the header layer
    
    header.uname = widget_info(widget_info(event.top, FIND_BY_UNAME = 'wsou'), /COMBOBOX_GETTEXT)
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
    header.ra = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
    header.dec = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), GET_VALUE = temp
    header.raof = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), GET_VALUE = temp
    header.raot = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), GET_VALUE = temp
    header.decof = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), GET_VALUE = temp
    header.decot = temp
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), GET_VALUE = temp
    header.rau = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wrau'), /DROPLIST_SELECT)]
    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), GET_VALUE = temp
    header.decu = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wdecu'), /DROPLIST_SELECT)]
    RETURN, header
END
;--------------------------------------------------------------------------------------------------



FUNCTION AICer_openaic, filename
;read a aic file
;return 0 for file not exist
;return 1 for file not readable
;return 2 for not empty or invalid aic file
    if n_params() lt 1 then begin
        print,'Syntax - AICer_openaic, filename'
        return,-1
    endif
    IF ~file_test(filename) THEN RETURN, 0
    IF ~file_test(filename, /READ) THEN RETURN, 1
    loaddata = ''
    ;newlist = 
    OPENR, lun, filename, /get_lun
    READF, lun, loaddata
    IF loaddata EQ '>BEGIN<' THEN BEGIN
        loadlayer = layer_define()
        WHILE (~ eof(lun)) AND (loaddata NE '>END<') DO BEGIN
            READF, lun, loaddata
            CASE loaddata OF
                '+LAYER+':BEGIN
                    loadlayer = layer_default(loadlayer)
                END
                '[VALID]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.valid = fix(loaddata)
                END
                '[USER NAME]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.uname = loaddata
                END
                '[SYSTEM NAME]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.name = loaddata
                END
                '[TYPE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.type = loaddata
                END
                '[FILE NAME1]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.file[0] = loaddata
                END
                '[FILE NAME2]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.file[1] = loaddata
                END
                '[FILE NAME3]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.file[2] = loaddata
                END
                '[R.A.]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.ra = loaddata
                END
                '[DEC.]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.dec = loaddata
                END
                '[R.A. OFFSET FROM]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.raof = loaddata
                END
                '[R.A. OFFSET TO]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.raot = loaddata
                END
                '[DEC. OFFSET FROM]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.decof = loaddata
                END
                '[DEC. OFFSET TO]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.decot = loaddata
                END
                '[R.A. UNIT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.rau = loaddata
                END
                '[DEC. UNIT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.decu = loaddata
                END
                '[TEXT INPUT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.texti = loaddata
                END
                '[TEXT OUTPUT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.texto = loaddata
                END
                '[COLUMN OF R.A.]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.cra = fix(loaddata)
                END
                '[COLUMN OF DEC.]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.cdec = fix(loaddata)
                END
                '[COLUMN OF VALUE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.cvalue = fix(loaddata)
                END
                '[COLUMN OF INTERVAL]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.cinter = fix(loaddata)
                END
                '[COLUMN OF ERROR]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.cerror = fix(loaddata)
                END
                '[TIMES]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.times = float(loaddata)
                END
                '[COLUMN OF TRUST]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.ctrust = fix(loaddata)
                END
                '[LENGTH]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.length = float(loaddata)
                END
                '[LENGTH UNIT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.lenu = loaddata
                END
                '[BLANKING VALUE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.Bval = float(strsplit(loaddata, /EXTRACT))
                END
                '[MISSING VALUE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.missing = float(loaddata)
                END
                '[SMOOTH DEGREE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.dsmooth = fix(loaddata)
                END
                '[FONT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.afont = loaddata
                END
                '[SIZE]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.asize = loaddata
                END
                '[THICK]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.athick = loaddata
                END
                '[ORIENTATION]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.aorient = loaddata
                END
                '[DEFAULT THICK]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.dthick = float(loaddata)
                END
                '[PARAMETER]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.parameter = fix(strsplit(loaddata, /EXTRACT))
                END
                '[COLOR TABLE]=':BEGIN
                    FOR j = 0, 47 DO BEGIN
                        READF, lun, loaddata
                        loadlayer.ct[j*16:j*16+15] = fix(strsplit(loaddata, /EXTRACT))
                    ENDFOR
                END
                '[PERSONAL CT]=':BEGIN
                    READF, lun, loaddata
                    loadlayer.pct = loaddata
                END
                '-LAYER-':BEGIN
                    IF n_elements(newlist) GE 1 THEN newlist = [loadlayer, newlist] ELSE newlist = loadlayer
                END
                '[COMMENT]=':BEGIN
                    READF, lun, loaddata
                END
                ELSE:
            ENDCASE
        ENDWHILE
    ENDIF ELSE BEGIN
        CATCH, error
	IF error NE 0 THEN BEGIN
	    CATCH, /CANCEL
	    RETURN,2
        ENDIF
        restore,filename
        newlist = temporary(layerlist)
    ENDELSE
    CLOSE, lun
    FREE_LUN, lun

    IF SIZE(newlist,/TYPE) ne 8 THEN RETURN,2
    IF tag_names(newlist, /structure_name) ne 'AICERLIST' THEN RETURN,2
    RETURN, newlist
END
;--------------------------------------------------------------------------------------------------



FUNCTION segmentintersection, a1, a2, b1, b2
    ;return the intersection of segment a1a2 and b1b2
    
    IF total((a1+a2-b1-b2)^2) GT total((a1-a2)^2) + total((b1-b2)^2) THEN RETURN, 0b
    
    v1 = a1-b1
    v2 = a2-b1
    vc = b2-b1
    test1 = (v1[0]*vc[1]-v1[1]*vc[0])*(v2[0]*vc[1]-v2[1]*vc[0]) LE 0
    v1 = b1-a1
    v2 = b2-a1
    vc = a2-a1
    test2 = (v1[0]*vc[1]-v1[1]*vc[0])*(v2[0]*vc[1]-v2[1]*vc[0]) LE 0
    RETURN, test1 AND test2
    
END
;--------------------------------------------------------------------------------------------------



FUNCTION levelconvert, leveltext
    
    ;convert level from script form to full form.
    ;'' is return if no level input, 'error' is return if the level input has mistakes.
    ;ignore string after ';' as comments
    
    leveltext = strmid(leveltext, 0, strpos(leveltext+';', ';'))
    IF leveltext NE '' THEN BEGIN
        leveltext = strsplit(leveltext, /EXTRACT)
        IF ~ valid_num(leveltext[0]) THEN RETURN, 'error'
        levelarray = string(float(leveltext[0]))
        FOR i = 1l, n_elements(leveltext)-1 DO BEGIN
            IF strcmp(leveltext[i], 'to', /FOLD_CASE) THEN BEGIN
                IF i+1 GT n_elements(leveltext)-1 THEN RETURN, 'error'
                IF ~ valid_num(leveltext[i+1]) THEN RETURN, 'error'
                IF i+2 GT n_elements(leveltext)-1 THEN BEGIN
                    inter = 1
                    fin = i+1
                ENDIF ELSE BEGIN
                    IF strcmp(leveltext[i+2], 'by', /FOLD_CASE) THEN BEGIN
                        IF i+3 GT n_elements(leveltext)-1 THEN RETURN, 'error'
                        IF ~ valid_num(leveltext[i+3]) THEN RETURN, 'error'
                        inter = float(leveltext[i+3])
                        IF abs(inter) LT 1e-6*float(leveltext[0]) THEN RETURN, 'error';inf levels
                        fin = i+3
                    ENDIF ELSE BEGIN
                        inter = 1
                        fin = i+1
                    ENDELSE
                ENDELSE
                FOR j = float(leveltext[i-1])+inter, float(leveltext[i+1]), inter DO $
		    levelarray = levelarray+' '+string(j)
                i=fin
            ENDIF ELSE BEGIN
                IF ~ valid_num(leveltext[i]) THEN RETURN, 'error'
                levelarray = levelarray+' '+string(float(leveltext[i]))
            ENDELSE
        ENDFOR
    ENDIF ELSE RETURN, 'error'
    levelarray = float(strsplit(levelarray, /EXTRACT))
    levelarray = levelarray[uniq(levelarray, sort(levelarray))]
    levelarray = strjoin(string(levelarray), ' ')
    RETURN, levelarray
END
;--------------------------------------------------------------------------------------------------



FUNCTION textconvert, inputtext, SOURCE = source, RA = ra, DEC = dec
    
    ;convert text script form to full form.
    ;replace the /xxx with corresponding string
    
    outputtext = ' '+inputtext+' '
    outputtext = strsplit(outputtext, '/source', /EXTRACT, /REGEX)
    IF n_elements(outputtext) GT 1 THEN outputtext = strjoin(outputtext, source)
    outputtext = strsplit(outputtext, '/ra', /EXTRACT, /REGEX)
    IF n_elements(outputtext) GT 1 THEN outputtext = strjoin(outputtext, ra)
    outputtext = strsplit(outputtext, '/dec', /EXTRACT, /REGEX)
    IF n_elements(outputtext) GT 1 THEN outputtext = strjoin(outputtext, dec)
    outputtext = strmid(outputtext, 1, strlen(outputtext)-2)
    RETURN, outputtext
END
;--------------------------------------------------------------------------------------------------



FUNCTION arrayextend, array, d
    array_d = make_array(n_elements(array), d, TYPE=size(array, /TYPE))
    FOR i=0l, d-1 DO BEGIN
        array_d[*,i] = array
    ENDFOR
    RETURN, array_d
END
;--------------------------------------------------------------------------------------------------



FUNCTION str2ang, str, RA=RA, DEC=DEC
    
    ;convert angle string in different format to angle
    
    str = strtrim(str,2)
    minus = strmid(str,0,1) EQ '-'
    hr = (strpos(str, 'h') GE 0) OR (strpos(str, 'H') GE 0)
    temp = strjoin(strsplit(str, 'h', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'H', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'd', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'D', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'm', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'M', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 's', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, 'S', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, ':', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, '_', /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, "'", /EXTRACT), ' ')
    temp = strjoin(strsplit(temp, '"', /EXTRACT), ' ')
    
    temp = abs(double(strsplit(temp, ' ', /EXTRACT)))
    
    IF ~keyword_set(RA) THEN RA = 0 ELSE RA = 1
    hr = hr OR (RA AND n_elements(temp) GT 1)
    
    angle = ten(temp)*(minus?-1:1)*(hr?15:1)
    IF RA THEN angle = angle mod 360.
    
    RETURN, angle
END
;--------------------------------------------------------------------------------------------------



FUNCTION valid_format, format
    
    ;Check if a string is a valid format representation.
    
    format = strlowcase(strtrim(format, 2))
    h = strmid(format, 0, 1)	;header of format
    v = (h EQ 'a' OR h EQ 'd' OR h EQ 'e' OR h EQ 'f' OR h EQ 'i')	;begin with 'adefi'
    IF ~v THEN RETURN, 0
    sp = strpos(format, '+')	;plus sign in format
    sm = strpos(format, '-')	;minus sign in format
    v = v AND (sp*sm EQ 2 OR abs(sp*sm) EQ 1)
    IF ~v THEN RETURN, 0
    r = strmid(format, max([sp+1,sm+1,1]))	;rest
    sp = strpos(r, '+')
    sm = strpos(r, '-')
    sd = strpos(r, '.')
    v = v AND (valid_num(r) OR r EQ '')	;follow with a vaild number
    v = v AND (sp EQ -1 AND sm EQ -1)	;without sign char
    v = v AND (sd EQ -1 OR (sd NE 0 AND sd NE strlen(r)-1))	;dot is not the first or last char,'.1' or '1.' are not accepted
    RETURN, v
END
;--------------------------------------------------------------------------------------------------



FUNCTION deform, str
    
    ;
    
    str = strtrim(str, 2)
    IF str EQ '' THEN RETURN,'error'
    
    l = strpos(str, '[')
    r = strpos(str, ']')
    IF ~(l EQ 0 AND r GT 0) THEN RETURN,'error'
    numf = strmid(str, l+1, r-l-1)
    str = strmid(str, r+1)
    IF valid_format(numf) THEN result = ['('+numf+')'] ELSE RETURN, 'error'
    IF str EQ '' THEN RETURN, result	;end of str
    
    l = strpos(str, '[')
    r = strpos(str, ']')
    IF l LT 0 AND r LT 0 THEN RETURN, [result, str]
    IF ~(l GE 0 AND r GT l) THEN RETURN,'error'
    result = [result, strmid(str, 0, l)]
    numf = strmid(str, l+1, r-l-1)
    str = strmid(str, r+1)
    IF valid_format(numf) THEN result = [result, '('+numf+')'] ELSE RETURN, 'error'
    
    l = strpos(str, '[')
    r = strpos(str, ']')
    IF l LT 0 AND r LT 0 THEN RETURN, [result, str]
    IF ~(l GE 0 AND r GT l) THEN RETURN,'error'
    result = [result, strmid(str, 0, l)]
    numf = strmid(str, l+1, r-l-1)
    str = strmid(str, r+1)
    IF valid_format(numf) THEN result = [result, '('+numf+')', str] ELSE RETURN, 'error'
    
    RETURN, result
END
;--------------------------------------------------------------------------------------------------



FUNCTION gettickv, range, part
    
    ;
    
    
    RETURN, result
END
;--------------------------------------------------------------------------------------------------



PRO module_frame_event, event
    
    ;resize the frame according to the event
    
    WIDGET_CONTROL, event.id, GET_VALUE = title
    sign = strmid(title,0,1)
    IF sign EQ '-' THEN BEGIN
        WIDGET_CONTROL, widget_info(event.id, /SIBLING), ysize = (widget_info(event.id, /GEOMETRY)).YSIZE/2
        STRPUT, title, '+'
    ENDIF ELSE BEGIN
        ysize = (widget_info(widget_info(event.id, /SIBLING), /GEOMETRY)).YSIZE
        WIDGET_CONTROL, widget_info(event.id, /SIBLING), YSIZE = ysize
        STRPUT, title, '-'
    ENDELSE
    WIDGET_CONTROL, event.id, SET_VALUE = title
    
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_frame, parent, TITLE = title, FNAME = fname, XSIZE=xsize
    
    ;creat a frame module for other widget
    
    IF ~keyword_set(fname) THEN fname = ''
    
    module = widget_base(parent, SPACE = 0, UNAME = fname)
    
    sys = strcmp(!d.name, 'win', /FOLD_CASE)
    
    IF ~ keyword_set(title) THEN title = ''
    IF sys THEN $
        mlabel = widget_button(module, VALUE = '- '+title, UVALUE = 'FRAME', $
	    /ALIGN_LEFT, XOFFSET = !x_width*0.05, YSIZE = 18) $
    ELSE $
        mlabel = widget_label(module, VALUE = title, $
            XOFFSET = !x_width*0.05)
    
    mframe = widget_base(module, SPACE = 0, /COLUMN, /FRAME, $
        XSIZE = keyword_set(xsize)?(xsize):(!x_width), $
        YOFFSET = (widget_info(mlabel, /GEOMETRY)).YSIZE/(sys+1), $
        YPAD = (widget_info(mlabel, /GEOMETRY)).YSIZE/2)
    RETURN, mframe
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_text, parent, VALUE = value, UVALUE = uvalue, TITLE = title, TAIL = tail, $
        XSIZE = xsize
    
    ;creat a text module
    ;can generate EVENT but not change by EVENT of other widget
    mbase = widget_base(parent, ROW = 1)
    
    IF keyword_set(title) THEN title = widget_label(mbase, VALUE = title)
    
    IF ~ keyword_set(value) THEN value = ''
    IF ~ keyword_set(uvalue) THEN uvalue = ''
    IF ~ keyword_set(xsize) THEN $
        mtext = widget_text(mbase, VALUE = value, UVALUE = uvalue, /EDITABLE, /KBRD_FOCUS_EVENTS) $
        ELSE $
        mtext = widget_text(mbase, VALUE = value, UVALUE = uvalue, /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = xsize)
    
    IF keyword_set(tail) THEN tail = widget_label(mbase, VALUE = tail)
    
    RETURN, mbase
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_font, parent, UVALUE = uvalue, UNAME = uname, SELECTFONT = selectfont
    
    ;widget of font setting
    
    IF ~ keyword_set(uvalue) THEN uvalue = 'FONT'
    IF ~ keyword_set(uname) THEN uname = 'font'
    
    fontlist = ['3  Simplex Roman', $
        '4  Simplex Greek', $
        '5  Duplex Roman', $
        '6  Complex Roman', $
        '7  Complex Greek', $
        '8  Complex Italic', $
        '9  Math and Special', $
        '11 Gothic English', $
        '12 Simplex Script', $
        '13 Complex Script', $
        '14 Gothic Italian', $
        '15 Gothic German', $
        '16 B Cyrilic', $
        '17 Triplex Roman', $
        '18 Triplex Italic', $
        '20 Miscellaneous']
    mfont = widget_droplist(parent, UVALUE = uvalue, UNAME = uname, TITLE = 'Font  ', $
        VALUE = fontlist)
    IF keyword_set(selectfont) THEN BEGIN
        select = where(strmid(fontlist, 0, 2) EQ strmid(selectfont+'   ',1,2))
        WIDGET_CONTROL, mfont, SET_DROPLIST_SELECT = select
    ENDIF
    RETURN, mfont
END
;--------------------------------------------------------------------------------------------------



FUNCTION font_set, leader, uname
    
    ;get font from module_font
    
    WIDGET_CONTROL, widget_info(leader, FIND_BY_UNAME = uname), GET_VALUE = fontlist
    select = widget_info(widget_info(leader, FIND_BY_UNAME = uname), /DROPLIST_SELECT)
    selectfont = '!'+(strsplit(fontlist[select], /EXTRACT))[0]
    RETURN, selectfont
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_symbol, parent, UVALUE = uvalue, UNAME = uname, SELECTSYM = selectsym
    
    ;widget of symbol setting
    
    IF ~ keyword_set(uvalue) THEN uvalue = 'SYMBOL'
    IF ~ keyword_set(uname) THEN uname = 'symbol'
    
    symbollist=['Plus sign(+)', $
        'X', $
        'Asterisk(*)', $
        'Period(.)', $
        'Triangle', $
        'Diamond', $
        'Square', $
        'Pentagram', $
        'Hexagon', $
        'Circle', $
        'Line', $
	'Histogram']
    msymbol = widget_droplist(parent, UVALUE = uvalue, UNAME = uname, TITLE = 'Type  ', VALUE = symbollist)
    IF keyword_set(selectsym) THEN BEGIN
        select = (where(symbollist EQ selectsym))[0]
        WIDGET_CONTROL, msymbol, SET_DROPLIST_SELECT = select
    ENDIF
    RETURN, msymbol
END
;--------------------------------------------------------------------------------------------------



FUNCTION symbol_set, symbol, FILL = fill, THICK = thick
    
    ;set symbol decided by module_symbol
    
    IF ~ keyword_set(fill) THEN fill = 0
    IF ~ keyword_set(thick) THEN thick = 1.0
    CASE symbol OF
        'Plus sign(+)':psym=1
        'X':psym=7
        'Asterisk(*)':psym=2
        'Period(.)':psym=3
        'Triangle':BEGIN
            psym=8
            USERSYM, !Triangle, FILL=fill, THICK = thick
        END
        'Diamond':BEGIN
            psym=8
            USERSYM, !Diamond, FILL=fill, THICK = thick
        END
        'Square':BEGIN
            psym=8
            USERSYM, !Square, FILL=fill, THICK = thick
        END
        'Pentagram':BEGIN
            psym=8
            USERSYM, !Pentagram, FILL=fill, THICK = thick
        END
        'Hexagon':BEGIN
            psym=8
            USERSYM, !Hexagon, FILL=fill, THICK = thick
        END
        'Circle':BEGIN
            psym=8
            USERSYM, !Circle, FILL=fill, THICK = thick
        END
        'Line':BEGIN
            psym=0
        END
	'Histogram':BEGIN
	    psym=10
	END
        ELSE:psym=0
    ENDCASE
    RETURN, psym
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_limit, parent, UVALUE = uvalue, UNAME = uname, SELECTLIM = selectlim
    
    ;widget of image limits
    
    IF ~ keyword_set(uvalue) THEN uvalue = 'LIMIT'
    IF ~ keyword_set(uname) THEN uname = 'limit'
    
    limitlist=['User', $	0
    'MinMax', $	1
    '99.5%', $	2
    '99%', $	3
    '98%', $	4
    '95%', $	5
    '90%', $	6
    '80%', $	7
    'ZScale'];	8
    mlimit = widget_droplist(parent, UVALUE = uvalue, UNAME = uname, TITLE = 'Limits', VALUE = limitlist)
    IF keyword_set(selectlim) THEN BEGIN
        select = (where(limitlist EQ selectlim))[0]
        WIDGET_CONTROL, mlimit, SET_DROPLIST_SELECT = select
    ENDIF
    RETURN, mlimit
END
;--------------------------------------------------------------------------------------------------



FUNCTION module_exit, parent
    
    ;creat an exit module
    
    mbase = widget_base(parent, ROW = 1, /ALIGN_CENTER, /GRID_LAYOUT)
    mhelp = widget_button(mbase, VALUE = 'Help', UVALUE = 'HELP', $
        SCR_XSIZE = (!x_width-20)/3)
    mcancel = widget_button(mbase, VALUE = 'Cancel', UVALUE = 'CANCEL')
    mdone = widget_button(mbase, VALUE = 'Done', UVALUE = 'DONE')
    
    RETURN, mbase
END
;--------------------------------------------------------------------------------------------------



PRO DRAW_DISTRIBUTION, index, wID, bID
    
    ;draw the pixel distribution of fits file to set the bval
    
    error = 0
    IF file_test(!layercache.file[index]) THEN BEGIN	;file does not exsit?
        FITS_OPEN, !layercache.file[index], fcb, MESSAGE = mess
        IF mess EQ '' THEN BEGIN	;wrong fits file?
            FITS_READ, fcb, odata, header
            FITS_CLOSE, fcb
            ;resample
            fits_sz = size(odata,/DIMENSIONS)
            data = congrid(odata, min([fits_sz[0],1000]), min([fits_sz[1],1000]))
            
            fin = where(finite(data)) & IF fin[0] NE -1 THEN data = data[fin] ELSE data = [0]
            
            IF max(data) NE min(data) THEN BEGIN	;data single value?
                IF !layercache.Bval[index*2] EQ 0 AND !layercache.Bval[index*2+1] EQ 0 THEN BEGIN
                    !layercache.Bval[index*2] = min(data, MAX = temp)
                    !layercache.Bval[index*2+1] = temp
                ENDIF
                WIDGET_CONTROL, bID[0], SENSITIVE = 1
                labeltext = strtrim(string(!layercache.Bval[index*2], FORMAT = '(G12.4)'), 2)
                WIDGET_CONTROL, bID[0], SET_VALUE = labeltext
                WIDGET_CONTROL, bID[1], SENSITIVE = 1
                labeltext = strtrim(string(!layercache.Bval[index*2+1], FORMAT = '(G12.4)'), 2)
                WIDGET_CONTROL, bID[1], SET_VALUE = labeltext
                
                h = alog(histogram(data, NBINS = 200, LOCATIONS = l))
                inf = where(~ finite(h)) & IF inf[0] NE -1 THEN h[inf] = 0
                l = [l, l[n_elements(l)-1]+l[1]-l[0]]
                !histpoly = [[reform(transpose([[l],[l]]),2*n_elements(l))], $
                    [0,reform(transpose([[h],[h]]),2*n_elements(h)),0]]
                WIDGET_CONTROL, wID, SENSITIVE = 1
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, wID, GET_VALUE = temp
                WSET, temp
                TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
                PLOT, [0], XRANGE = [min(l), max(l)], YRANGE = [0, max(h)], $
                    XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
                POLYFILL, !histpoly[*,0],!histpoly[*,1], COLOR = 1
                OPLOT, [!layercache.bval[index*2],!layercache.bval[index*2]],[0,max(h)],COLOR = 2
                OPLOT, [!layercache.bval[index*2+1],!layercache.bval[index*2+1]],[0,max(h)],COLOR = 3
                
                data = data[sort(data)]
                n_data = n_elements(data)
                !limitvalue[*,0] = [data[0],data[n_data-1]]
                !limitvalue[*,1] = [(data[n_data*0.0025]+data[n_data*0.0025-1])/2, $
                    (data[n_data*0.9975]+data[n_data*0.9975-1])/2]
                !limitvalue[*,2] = [(data[n_data*0.005]+data[n_data*0.005-1])/2, $
                    (data[n_data*0.995]+data[n_data*0.995-1])/2]
                !limitvalue[*,3] = [(data[n_data*0.01]+data[n_data*0.01-1])/2, $
                    (data[n_data*0.99]+data[n_data*0.99-1])/2]
                !limitvalue[*,4] = [(data[n_data*0.025]+data[n_data*0.025-1])/2, $
                    (data[n_data*0.975]+data[n_data*0.975-1])/2]
                !limitvalue[*,5] = [(data[n_data*0.05]+data[n_data*0.05-1])/2, $
                    (data[n_data*0.95]+data[n_data*0.95-1])/2]
                !limitvalue[*,6] = [(data[n_data*0.1]+data[n_data*0.1-1])/2, $
                    (data[n_data*0.9]+data[n_data*0.9-1])/2]
                !limitvalue[*,7] = zscale(odata)
            ENDIF ELSE error = 3
        ENDIF ELSE error = 2
    ENDIF ELSE IF !layercache.file[index] NE '' THEN error = 1
    
    IF error GT 0 THEN BEGIN
        WIDGET_CONTROL, wID, SENSITIVE = 1
        ;	SET_PLOT, 'win'
        ;	DEVICE, DECOMPOSED = 0
        WIDGET_CONTROL, wID, GET_VALUE = temp
        WSET, temp
        TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
        PLOT, [0], XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
        WIDGET_CONTROL, wID, SENSITIVE = 0
        WIDGET_CONTROL, bID[0], SENSITIVE = 0
        WIDGET_CONTROL, bID[1], SENSITIVE = 0
        CASE error OF
            1:PRINT,'Fits file does not exist.'
            2:PRINT,'Wrong file format.'
            3:PRINT,'Data range too small.'
            ELSE:
        ENDCASE
    ENDIF
END
;--------------------------------------------------------------------------------------------------



PRO DRAW_PREVIEW, ID
    
    ;draw a preview of image layer
    
END
;--------------------------------------------------------------------------------------------------



FUNCTION log_scale, data
    ;RETURN, alog(1000*data+1)/alog(1000)
    RETURN, alog(abs(data)+1)*((data GE 0)*2-1)
END
FUNCTION power_scale, data
    ;RETURN, (1000^data-1)/1000
    RETURN, (1.01d^(abs(data))-1)*((data GE 0)*2-1)
END
FUNCTION sqrt_scale, data
    ;RETURN,sqrt(data)
    RETURN, sqrt(abs(data))*((data GE 0)*2-1)
END
FUNCTION square_scale, data
    ;RETURN,data^2
    RETURN, data^2*((data GE 0)*2-1)
END
FUNCTION image_scale, image, lower, upper, scaletype
    
    ;scaling image to value between 0d and 1d
    
    def = 1000
    if lower eq upper then return, image > 0 < 0
    image = ((image-lower)/(upper-lower)) > 0 < 1
    CASE scaletype OF
        1:image = alog(def*image+1)/alog(def)
	;(log_scale(image)-log_scale(lower))/(log_scale(upper)-log_scale(lower))		;log
        2:image = (def^image-1)/def
	;(power_scale(image)-power_scale(lower))/(power_scale(upper)-power_scale(lower))	;exp
        3:image = sqrt(image)
	;(sqrt_scale(image)-sqrt_scale(lower))/(sqrt_scale(upper)-sqrt_scale(lower))		;square root
        4:image = image^2
	;(square_scale(image)-square_scale(lower))/(square_scale(upper)-square_scale(lower))	;squared
        5:image = hist_equal(image)/255.
	;histogram-equalized
        ELSE:image = image
	;linear
    ENDCASE
    return, image
END
;--------------------------------------------------------------------------------------------------



PRO AICer_image_event, event
    
    ;Creat or edit a image layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RA':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.ra = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RAOF':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.raof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RAOT':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.raot = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RAU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.rau = temp[event.index]
        END
        
        'DEC':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.dec = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DECOF':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.decof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DECOT':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.decot = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DECU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.decu = temp[event.index]
        END
        
        'RGB':BEGIN
            ;		SET_PLOT, 'win'
            ;		DEVICE, DECOMPOSED = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'colortable'), GET_VALUE = temp
            WSET, temp
            TVLCT, 0,0,0
            ERASE
            bar = ((!d.table_size-2)*indgen(!x_width-20)/(!x_width-21)+1)#replicate(1,45)
	    ;index 0 is used for NaN values
            TVLCT, !layercache.ct
            IF event.select THEN BEGIN
                !layercache.type = 'RGB'
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icbase'), SENSITIVE = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile1'), SET_VALUE = 'R'
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile2'), SENSITIVE = 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile3'), SENSITIVE = 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'R'
		labeltext = (!layercache.file[0] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[0])
 		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                TV, bar[*,0:14], 0, 30, CHANNEL = 1
                TV, bar[*,15:29], 0, 15, CHANNEL = 2
                TV, bar[*,30:44], 0, 0, CHANNEL = 3
            ENDIF ELSE BEGIN
                ;WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                !layercache.type = 'SINGLE'
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icbase'), SENSITIVE = 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile1'), SET_VALUE = 'File'
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile2'), SENSITIVE = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ifile3'), SENSITIVE = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'File'
		labeltext = (!layercache.file[0] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[0])
 		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                TV, bar, 0, 0
                ;IF channel NE 'R'
            ENDELSE
        END
        
        'FILE1':BEGIN
            fpath = file_dirname(!layercache.file[0])
            ffile = dialog_pickfile(TITLE = (!layercache.type EQ 'RGB')?'Select Image File for RED Channel':'Select Image File', $
                FILTER = [['*.fits', '*.*'], ['Fits Image (*.fits)', 'All Files (*.*)']], $
                PATH = fpath)
            IF ffile NE '' THEN BEGIN
                !layercache.file[0] = ffile
                IF !layercache.type EQ 'RGB' THEN $
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'R'
 		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = file_basename(ffile)
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                ibval = [widget_info(event.top, FIND_BY_UNAME = 'ibval0'), $
                    widget_info(event.top, FIND_BY_UNAME = 'ibval1')]
                !layercache.bval[0:1] = 0
                DRAW_DISTRIBUTION, 0, idisp, ibval
                !layercache.parameter[3] = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = 0
            ENDIF
        END
        
        'FILE2':BEGIN
            fpath = file_dirname(!layercache.file[1])
            ffile = dialog_pickfile(TITLE = 'Select Image File for GREEN Channel', $
                FILTER = [['*.fits', '*.*'], ['Fits Image (*.fits)', 'All Files (*.*)']], $
                PATH = fpath)
            IF ffile NE '' THEN BEGIN
                !layercache.file[1] = ffile
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'G'
 		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = file_basename(ffile)
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                ibval = [widget_info(event.top, FIND_BY_UNAME = 'ibval0'), $
                    widget_info(event.top, FIND_BY_UNAME = 'ibval1')]
                !layercache.bval[2:3] = 0
                DRAW_DISTRIBUTION, 1, idisp, ibval
                !layercache.parameter[4] = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = 0
            ENDIF
        END
        
        'FILE3':BEGIN
            fpath = file_dirname(!layercache.file[2])
            ffile = dialog_pickfile(TITLE = 'Select Image File for BLUE Channel', $
                FILTER = [['*.fits', '*.*'], ['Fits Image (*.fits)', 'All Files (*.*)']], $
                PATH = fpath)
            IF ffile NE '' THEN BEGIN
                !layercache.file[2] = ffile
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'B'
 		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = file_basename(ffile)
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                ibval = [widget_info(event.top, FIND_BY_UNAME = 'ibval0'), $
                    widget_info(event.top, FIND_BY_UNAME = 'ibval1')]
                !layercache.bval[4:5] = 0
                DRAW_DISTRIBUTION, 2, idisp, ibval
                !layercache.parameter[5] = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = 0
            ENDIF
        END
        
        'PREV':BEGIN
            IF !layercache.type EQ 'RGB' THEN BEGIN
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                ibval = [widget_info(event.top, FIND_BY_UNAME = 'ibval0'), $
                    widget_info(event.top, FIND_BY_UNAME = 'ibval1')]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'limit'), SET_DROPLIST_SELECT = 0
                CASE channel OF
                    'R':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'B'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[5]
                        DRAW_DISTRIBUTION, 2, idisp, ibval
		        labeltext = (!layercache.file[2] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[2])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                    'G':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'R'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[3]
                        DRAW_DISTRIBUTION, 0, idisp, ibval
		        labeltext = (!layercache.file[0] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[0])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                    'B':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'G'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[4]
                        DRAW_DISTRIBUTION, 1, idisp, ibval
		        labeltext = (!layercache.file[1] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[1])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                ENDCASE
            ENDIF
        END
        
        'NEXT':BEGIN
            IF !layercache.type EQ 'RGB' THEN BEGIN
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                ibval = [widget_info(event.top, FIND_BY_UNAME = 'ibval0'), $
                    widget_info(event.top, FIND_BY_UNAME = 'ibval1')]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'limit'), SET_DROPLIST_SELECT = 0
                CASE channel OF
                    'R':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'G'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[4]
                        DRAW_DISTRIBUTION, 1, idisp, ibval
		        labeltext = (!layercache.file[1] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[1])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                    'G':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'B'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[5]
                        DRAW_DISTRIBUTION, 2, idisp, ibval
		        labeltext = (!layercache.file[2] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[2])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                    'B':BEGIN
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), SET_VALUE = 'R'
                        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'iscale'), SET_DROPLIST_SELECT = !layercache.parameter[3]
                        DRAW_DISTRIBUTION, 0, idisp, ibval
		        labeltext = (!layercache.file[0] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[0])
 		        WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'itip'), SET_VALUE = labeltext
                    END
                ENDCASE
            ENDIF
        END
        
        'HEADER':BEGIN
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
            CASE channel OF
                'G':currfile = !layercache.file[1]
                'B':currfile = !layercache.file[2]
                ELSE:currfile = !layercache.file[0]
            ENDCASE
            FITS_READ,currfile,dat,hdr,/HEADER_ONLY
            IF file_test(currfile) THEN XDISPLAYFILE, /MODAL, GROUP = event.top, DONE_BUTTON = 'Close', TEXT = hdr, TITLE = currfile
        END
        
        ;image contour option
        'IC':BEGIN
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icp'), SENSITIVE = event.select
            !layercache.parameter[6] = (event.select*2-1)*abs(!layercache.parameter[6])
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_CT'), SENSITIVE = ~ event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_MI'), SENSITIVE = ~ event.select
        END
        
        'CP':BEGIN
            type = !layercache.type
            !layercache.type = (!layercache.parameter[6] EQ 2)?'DIFFERENT':'SAME'
            AICer_contour, event.top, TYPE = 2
            !layercache.parameter[6] = (!layercache.type EQ 'SAME')?1:2
            !layercache.type = type
        END
        
        ;	'CONV':BEGIN
        ;		!layercache.parameter[0] = event.value
        ;	END
        
        'MODIFY':BEGIN
            nan = !layercache.ct[0,*]
            TVLCT, !layercache.ct
            XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct=currct
            !layercache.ct[0,*] = nan
            ;		SET_PLOT, 'win'
            ;		DEVICE, DECOMPOSED = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'colortable'), GET_VALUE = temp
            WSET, temp
            TVLCT, 0,0,0
            ERASE
            bar = ((!d.table_size-2)*indgen(!x_width-20)/(!x_width-21)+1)#replicate(1,45);index 0 is used for NaN values
            TVLCT, !layercache.ct
            IF !layercache.type EQ 'RGB' THEN BEGIN
                TV, bar[*,0:14], 0, 30, CHANNEL=1
                TV, bar[*,15:29], 0, 15, CHANNEL=2
                TV, bar[*,30:44], 0, 0, CHANNEL=3
            ENDIF ELSE TV, bar, 0, 0
        END
        
        'USERCT':BEGIN
            nan = !layercache.ct[0,*]
            cpath = file_dirname(!layercache.pct)
            cfile = dialog_pickfile(TITLE = 'Select Color Table File', $
                FILTER = [['*.tbl', '*.lut', '*.sao', '*.*'], ['Color Table (*.tbl)', 'DS9/GILDAS (*.lut)', 'SAO/DS9 (*.sao)', 'All Files (*.*)']], $
                PATH = cpath)
            IF cfile NE '' THEN BEGIN
                !layercache.pct = cfile
                dot = strpos(cfile, '.', /REVERSE_SEARCH)
                extension = (dot EQ -1)?'':strmid(cfile, dot+1, strlen(cfile)-dot-1)
                CASE extension OF
                    'sao':BEGIN
                        !layercache.ct = sao2tbl(cfile)
                        TVLCT, !layercache.ct
                        XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                    END
                    'lut':BEGIN
                        !layercache.ct = lut2tbl(cfile)
                        TVLCT, !layercache.ct
                        XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                    END
                    ELSE:XLOADCT, GROUP = event.top, /MODAL, FILE = !layercache.pct
                ENDCASE
                currct = intarr(!d.table_size,3)
                TVLCT, currct, /GET
                !layercache.ct=currct
                !layercache.ct[0,*] = nan
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'colortable'), GET_VALUE = temp
                WSET, temp
                TVLCT, 0,0,0
                ERASE
                bar = ((!d.table_size-2)*indgen(!x_width-20)/(!x_width-21)+1)#replicate(1,45);index 0 is used for NaN values
                IF !layercache.type EQ 'RGB' THEN BEGIN
                    TV, bar[*,0:14], 0, 30, CHANNEL=1
                    TV, bar[*,15:29], 0, 15, CHANNEL=2
                    TV, bar[*,30:44], 0, 0, CHANNEL=3
                ENDIF ELSE TV, bar, 0, 0
            ENDIF
        END
        
        'LIMIT':BEGIN
            IF !layercache.type EQ 'RGB' THEN BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                CASE channel OF
                    'R':index = 0
                    'G':index = 1
                    'B':index = 2
                ENDCASE
            ENDIF ELSE index = 0
            IF event.index GE 1 AND event.index LE 8 THEN BEGIN
                !layercache.bval[index*2:index*2+1] = !limitvalue[*,event.index-1]
                
                xmin = (!histpoly[*,0])[0]
                xmax = (!histpoly[*,0])[n_elements(!histpoly[*,0])-1]
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                WIDGET_CONTROL, idisp, GET_VALUE = temp
                WSET, temp
                TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
                PLOT, [0], XRANGE = [xmin, xmax], YRANGE = [0, max(!histpoly[*,1])], $
                    XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
                POLYFILL, !histpoly[*,0],!histpoly[*,1], COLOR = 1
                OPLOT, [!layercache.bval[index*2],!layercache.bval[index*2]],[0,max(!histpoly[*,1])],COLOR = 2
                OPLOT, [!layercache.bval[index*2+1],!layercache.bval[index*2+1]],[0,max(!histpoly[*,1])],COLOR = 3
                
                labeltext = strtrim(string(!layercache.Bval[index*2], FORMAT = '(G12.4)'), 2)
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ibval0'), SET_VALUE = labeltext
                labeltext = strtrim(string(!layercache.Bval[index*2+1], FORMAT = '(G12.4)'), 2)
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ibval1'), SET_VALUE = labeltext
            ENDIF
        END
        
        'SCALE':BEGIN
            IF !layercache.type EQ 'SINGLE' THEN BEGIN
                !layercache.parameter[3] = widget_info(event.id, /DROPLIST_SELECT)
            ENDIF ELSE BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                CASE channel OF
                    'R':BEGIN
                        !layercache.parameter[3] = widget_info(event.id, /DROPLIST_SELECT)
                    END
                    'G':BEGIN
                        !layercache.parameter[4] = widget_info(event.id, /DROPLIST_SELECT)
                    END
                    'B':BEGIN
                        !layercache.parameter[5] = widget_info(event.id, /DROPLIST_SELECT)
                    END
                ENDCASE
            ENDELSE
        END
        
        'DISP':BEGIN
            IF EVENT.press NE 0 THEN !status = EVENT.press
            IF EVENT.release NE 0 THEN !status = 0
            IF !status EQ 0 THEN BREAK
            px = EVENT.x
            IF EVENT.x LT 0 then px = 0
            IF EVENT.x GT !x_width-21 THEN px = !x_width-21
            xmin = (!histpoly[*,0])[0]
            xmax = (!histpoly[*,0])[n_elements(!histpoly[*,0])-3]
            bval = xmin+(xmax-xmin)*px/(!x_width-21)
            IF !layercache.type EQ 'RGB' THEN BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                CASE channel OF
                    'R':index = 0
                    'G':index = 1
                    'B':index = 2
                ENDCASE
            ENDIF ELSE index = 0
            IF !status EQ 1 THEN BEGIN
                IF bval LT !layercache.bval[index*2] THEN BREAK
                !layercache.bval[index*2+1] = bval
            ENDIF ELSE BEGIN
                IF bval GT !layercache.bval[index*2+1] THEN BREAK
                !layercache.bval[index*2] = bval
            ENDELSE
            xmax = (!histpoly[*,0])[n_elements(!histpoly[*,0])-1]
            ;		SET_PLOT, 'win'
            ;		DEVICE, DECOMPOSED = 0
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            WSET, temp
            TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
            PLOT, [0], XRANGE = [xmin, xmax], YRANGE = [0, max(!histpoly[*,1])], $
                XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
            POLYFILL, !histpoly[*,0],!histpoly[*,1], COLOR = 1
            
            OPLOT, [!layercache.bval[index*2],!layercache.bval[index*2]],[0,max(!histpoly[*,1])],COLOR = 2
            OPLOT, [!layercache.bval[index*2+1],!layercache.bval[index*2+1]],[0,max(!histpoly[*,1])],COLOR = 3
            labeltext = strtrim(string(!layercache.Bval[index*2], FORMAT = '(G12.4)'), 2)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ibval0'), SET_VALUE = labeltext
            labeltext = strtrim(string(!layercache.Bval[index*2+1], FORMAT = '(G12.4)'), 2)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ibval1'), SET_VALUE = labeltext
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'limit'), SET_DROPLIST_SELECT = 0
        END
        
        'BVAL0':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = bval0
                IF !layercache.type EQ 'RGB' THEN BEGIN
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                    CASE channel OF
                        'R':index = 0
                        'G':index = 1
                        'B':index = 2
                    ENDCASE
                ENDIF ELSE index = 0
                !layercache.bval[index*2] = min([double(bval0),!layercache.bval[index*2+1]])
                
                xmin = (!histpoly[*,0])[0]
                xmax = (!histpoly[*,0])[n_elements(!histpoly[*,0])-1]
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                WIDGET_CONTROL, idisp, GET_VALUE = temp
                WSET, temp
                TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
                PLOT, [0], XRANGE = [xmin, xmax], YRANGE = [0, max(!histpoly[*,1])], $
                    XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
                POLYFILL, !histpoly[*,0],!histpoly[*,1], COLOR = 1
                OPLOT, [!layercache.bval[index*2],!layercache.bval[index*2]],[0,max(!histpoly[*,1])],COLOR = 2
                OPLOT, [!layercache.bval[index*2+1],!layercache.bval[index*2+1]],[0,max(!histpoly[*,1])],COLOR = 3
                
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'limit'), SET_DROPLIST_SELECT = 0
            ENDIF
        END
        
        'BVAL1':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = bval1
                IF !layercache.type EQ 'RGB' THEN BEGIN
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'icurr'), GET_VALUE = channel
                    CASE channel OF
                        'R':index = 0
                        'G':index = 1
                        'B':index = 2
                    ENDCASE
                ENDIF ELSE index = 0
                !layercache.bval[index*2+1] = max([double(bval1),!layercache.bval[index*2]])
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                
                xmin = (!histpoly[*,0])[0]
                xmax = (!histpoly[*,0])[n_elements(!histpoly[*,0])-1]
                idisp = widget_info(event.top, FIND_BY_UNAME = 'idisp')
                WIDGET_CONTROL, idisp, GET_VALUE = temp
                WSET, temp
                TVLCT, [0,0,255,255], [0,230,0,255], [0,0,0,0]
                PLOT, [0], XRANGE = [xmin, xmax], YRANGE = [0, max(!histpoly[*,1])], $
                    XSTYLE = 5, YSTYLE = 5, POSITION = [0,0,1,1], COLOR=1, /NODATA
                POLYFILL, !histpoly[*,0],!histpoly[*,1], COLOR = 1
                OPLOT, [!layercache.bval[index*2],!layercache.bval[index*2]],[0,max(!histpoly[*,1])],COLOR = 2
                OPLOT, [!layercache.bval[index*2+1],!layercache.bval[index*2+1]],[0,max(!histpoly[*,1])],COLOR = 3
                
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'limit'), SET_DROPLIST_SELECT = 0
            ENDIF
        END
        
        'NSMOOTH':BEGIN
            !layercache.dsmooth = event.index
        END
        
        'BAR':BEGIN
            !layercache.parameter[7] = event.select
        END
        
        'NANCOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                TVLCT, !layercache.ct
                ;XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                XCOLOR, GROUP = event.top, /MODAL
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 0
                currct = intarr(!d.table_size,3)
                TVLCT, currct, /GET
                !layercache.ct = currct
            ENDIF
        END
        
        'DONE':BEGIN
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Image Layer Help', /MODAL, GROUP = event.top, TITLE = 'Image Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Change coordinate and range to fix the image range.', $
                '3.A single map or a RGB map? that''s up to you;', $
                '  pick your fits file and decide whether to convert the coordinate.', $
                '  if you need a contour line for you image, "Convert" it to contour', $
                '4.Select a value scale and choose a color table of your own.', $
                '5.Select the degree of smooth...']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_image, leader
    
    IF !layercache.parameter[6] EQ 0 THEN !layercache.parameter[6] = -1;for old version
    
    ;Creat or edit a image layer(GUI).
    
    ibase = widget_base(TITLE = 'Image layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 3)
    
    ;********
    ;colunm 1
    icol = widget_base(ibase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;name widget
    imbase0 = module_text(icol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
    
    ;coordinate widget
    imbase0 = module_frame(icol, TITLE = 'Plot Region')
    
    imbase1 = module_text(imbase0, VALUE = !layercache.ra, UVALUE = 'RA', TITLE = 'X center')
    imbase1 = widget_base(imbase0, ROW = 1)
    irat2 = widget_label(imbase1, VALUE = ' range')
    iraof = widget_text(imbase1, VALUE = !layercache.raof, UVALUE = 'RAOF', UNAME = 'iraof', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = 45)
    iraot = widget_text(imbase1, VALUE = !layercache.raot, UVALUE = 'RAOT', UNAME = 'iraot', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = 45)
    irau = widget_droplist(imbase1, VALUE = ['arcsec','arcmin','degree'], UVALUE = 'RAU', UNAME = 'irau')
    CASE !layercache.rau OF
        'arcmin':WIDGET_CONTROL, irau, SET_DROPLIST_SELECT = 1
        'degree':WIDGET_CONTROL, irau, SET_DROPLIST_SELECT = 2
        ELSE:
    ENDCASE
    
    imbase1 = module_text(imbase0, VALUE = !layercache.dec, UVALUE = 'DEC', TITLE = 'Y center')
    imbase1 = widget_base(imbase0, ROW = 1)
    idect2 = widget_label(imbase1, VALUE = ' range')
    idecof = widget_text(imbase1, VALUE = !layercache.decof, UVALUE = 'DECOF', UNAME = 'idecof', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = 45)
    idecot = widget_text(imbase1, VALUE = !layercache.decot, UVALUE = 'DECOT', UNAME = 'idecot', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = 45)
    idecu = widget_droplist(imbase1, VALUE = ['arcsec','arcmin','degree'], UVALUE = 'DECU', UNAME = 'idecu')
    CASE !layercache.decu OF
        'arcmin':WIDGET_CONTROL, idecu, SET_DROPLIST_SELECT = 1
        'degree':WIDGET_CONTROL, idecu, SET_DROPLIST_SELECT = 2
        ELSE:
    ENDCASE
    
    ;file select widget
    imbase0 = module_frame(icol, TITLE = 'File Information')
    
    imbase1 = widget_base(imbase0, ROW = 1, /EXCLUSIVE)
    isingle = widget_button(imbase1, VALUE = 'Single image', UVALUE = 'SINGLE', UNAME = 'isingle')
    irgb = widget_button(imbase1, VALUE = 'RGB image', UVALUE = 'RGB', UNAME = 'irgb')
    imbase1 = widget_base(imbase0, ROW = 1, /ALIGN_CENTER, /GRID_LAYOUT)
    ifile1 = widget_button(imbase1, VALUE = 'File', UVALUE = 'FILE1', UNAME = 'ifile1', TOOLTIP = 'Open browse')
    ifile2 = widget_button(imbase1, VALUE = 'G', UVALUE = 'FILE2', UNAME = 'ifile2', TOOLTIP = 'Open browse', SENSITIVE = 0)
    ifile3 = widget_button(imbase1, VALUE = 'B', UVALUE = 'FILE3', UNAME = 'ifile3', TOOLTIP = 'Open browse', SENSITIVE = 0)
    
    imbase1 = widget_base(imbase0, ROW = 1, /ALIGN_CENTER)
    iprev = widget_button(imbase1, VALUE = '<', UVALUE = 'PREV', UNAME = 'iprev')
    icurr = widget_label(imbase1, VALUE = 'File', UVALUE = 'CURR', UNAME = 'icurr', XSIZE = 40, /ALIGN_CENTER)
    inext = widget_button(imbase1, VALUE = '>', UVALUE = 'NEXT', UNAME = 'inext')
    
    ;imbase1 = widget_base(imbase0, /ALIGN_CENTER)
    labeltext = (!layercache.file[0] EQ '')?'Please select a FITS file!':file_basename(!layercache.file[0])
    itip = widget_label(imbase0, VALUE = labeltext, UVALUE = 'TIP', UNAME = 'itip', $
	SCR_XSIZE = !x_width)
  
    imbase1 = widget_base(imbase0, /ALIGN_CENTER)
    iheader = widget_button(imbase1, VALUE = 'View FITS header', UVALUE = 'HEADER', UNAME = 'iheader')
    
    imbase1 = widget_base(imbase0, COLUMN = 1, UNAME = 'icbase', SENSITIVE = (!layercache.type NE 'RGB'))
    imbase2 = widget_base(imbase1, /NONEXCLUSIVE)
    iic = widget_button(imbase2, VALUE = 'Convert image to contour', UVALUE = 'IC', UNAME = 'iic')
    imbase2 = widget_base(imbase1, /ALIGN_CENTER)
    icp = widget_button(imbase2, VALUE = 'Contour Properties', UVALUE = 'CP', UNAME = 'icp', SENSITIVE = (!layercache.parameter[6] GT 0))
    
    ;PART: Coordinate Conversion
    ;imbase0 = module_frame(ibase, TITLE = 'Coordinate Conversion')
    ;imbase1 = widget_base(imbase0, COLUMN = 1, /ALIGN_LEFT)
    ;iconv = cw_bgroup(imbase0,['No conversion', 'Galactic  ->  Celestial', 'Celestial ->  Galactic'], UVALUE = 'CONV', UNAME = 'iconv', $
    ;	COLUMN = 1, /EXCLUSIVE, /NO_RELEASE)
    
    ;********
    ;colunm 2
    icol = widget_base(ibase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;PART: Pixel Distribution
    imbase0 = module_frame(icol, TITLE = 'Pixel Distribution')
    imbase1 = widget_base(imbase0, ROW = 1)
    ilimit = module_limit(imbase1)
    scalelist=['Linear', $		0
    'Log', $		1
    'Power', $		2
    'Square Root', $3
    'Squared', $	4
    'Histo-Equal'];	5
    iscale = widget_droplist(imbase1, UVALUE = 'SCALE', UNAME = 'iscale', TITLE = ' Scale', VALUE = scalelist)
    WIDGET_CONTROL, iscale, SET_DROPLIST_SELECT = !layercache.parameter[3]
    
    imbase1 = widget_base(imbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    idisp = widget_draw(imbase1, UVALUE = 'DISP', UNAME = 'idisp', /BUTTON_EVENT, /MOTION_EVENT, $
        SCR_XSIZE = !x_width-20, SCR_YSIZE = 100)
    imbase2 = widget_base(imbase1, ROW = 1)
    labeltext = strtrim(string(!layercache.Bval[0], FORMAT = '(G12.4)'), 2)
    ibval0 = widget_text(imbase2, VALUE = labeltext, UVALUE = 'BVAL0', UNAME = 'ibval0', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    ibvalt = widget_label(imbase2, VALUE = ' - ')
    labeltext = strtrim(string(!layercache.Bval[1], FORMAT = '(G12.4)'), 2)
    ibval1 = widget_text(imbase2, VALUE = labeltext, UVALUE = 'BVAL1', UNAME = 'ibval1', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    
    ;PART: Color Table
    imbase0 = module_frame(icol, TITLE = 'Color Table', FNAME = 'module_CT')
    
    imbase1 = widget_base(imbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    colortable = widget_draw(imbase1, UNAME = 'colortable', $
        SCR_XSIZE = !x_width-20, SCR_YSIZE = 45)
    imbase2 = widget_base(imbase1, ROW = 1, /GRID_LAYOUT)
    modify = widget_button(imbase2, VALUE = 'Modify', UVALUE = 'MODIFY', UNAME = 'modify', TOOLTIP = 'Modify current CT')
    userct = widget_button(imbase2, VALUE = 'User', UVALUE = 'USERCT', UNAME = 'userct', TOOLTIP = 'Input CT from a file')
    WIDGET_CONTROL, widget_info(imbase0, /PARENT), SENSITIVE = (!layercache.parameter[6] LE 0)
    
    imbase2 = widget_base(imbase1, ROW = 1, /ALIGN_LEFT)
    inancolort = widget_label(imbase2, VALUE = 'NaN Color ')
    inancolor = widget_draw(imbase2, UVALUE = 'NANCOLOR', UNAME = 'inancolor', BUTTON_EVENT = 'AICer_image_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    
    ;PART: Miscellaneous Items
    imbase0 = module_frame(icol, TITLE = 'Miscellaneous Items', FNAME = 'module_MI')
    imbase1 = widget_base(imbase0, COLUMN = 1, /ALIGN_LEFT)
    insmooth = widget_droplist(imbase1, UVALUE = 'NSMOOTH', UNAME = 'insmooth', TITLE = 'Smooth degree', $
        VALUE = ['0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15'])
    imbase2 = widget_base(imbase1, /NONEXCLUSIVE)
    ibar = widget_button(imbase2, VALUE = 'Draw a color bar', UVALUE = 'BAR', UNAME = 'ibar')
    WIDGET_CONTROL, widget_info(imbase0, /PARENT), SENSITIVE = (!layercache.parameter[6] LE 0)
    
    ;exit widget
    imbase0 = module_exit(icol)
    
    ;;********
    ;;colunm 3
    ;icol = widget_base(ibase, COLUMN = 1, XPAD = 0, YPAD = 0)
    ;
    ;;PART: Preview
    ;imbase0 = module_frame(icol, TITLE = 'Preview', XSIZE=420)
    ;imbase1 = widget_base(imbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    ;iview = widget_draw(imbase1, UVALUE = 'VIEW', UNAME = 'iview', $
    ;	SCR_XSIZE = 400, SCR_YSIZE = 400)
    
    ;**************
    ;widget realize
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, ibase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, ibase, /REALIZE
    
    ;*****************
    ;widget initiation
    IF !layercache.type EQ 'RGB' THEN BEGIN
        WIDGET_CONTROL, irgb, /SET_BUTTON
        WIDGET_CONTROL, ifile1, SET_VALUE = 'R'
        WIDGET_CONTROL, ifile2, SET_VALUE = 'G', /SENSITIVE
        WIDGET_CONTROL, ifile3, SET_VALUE = 'B', /SENSITIVE
        WIDGET_CONTROL, icurr, SET_VALUE = 'R'
    ENDIF ELSE WIDGET_CONTROL, isingle, /SET_BUTTON
    
    WIDGET_CONTROL, iic, SET_BUTTON = (!layercache.parameter[6] GT 0)
    ;WIDGET_CONTROL, iconv, SET_VALUE = !layercache.parameter[0]
    
    IF !layercache.parameter[6] LT 0 THEN BEGIN
        WIDGET_CONTROL, insmooth, SET_DROPLIST_SELECT = !layercache.dsmooth
        WIDGET_CONTROL, ibar, SET_BUTTON = !layercache.parameter[7]
    ENDIF
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    WIDGET_CONTROL, colortable, GET_VALUE = temp
    WSET, temp
    bar = ((!d.table_size-2)*indgen(!x_width-20)/(!x_width-21)+1)#replicate(1,45);index 0 is used for NaN values
    TVLCT, !layercache.ct
    IF !layercache.type EQ 'RGB' THEN BEGIN
        TV, bar[*,0:14], 0, 30, CHANNEL = 1
        TV, bar[*,15:29], 0, 15, CHANNEL = 2
        TV, bar[*,30:44], 0, 0, CHANNEL = 3
    ENDIF ELSE TV, bar, 0, 0
    
    WIDGET_CONTROL, inancolor, GET_VALUE = temp
    WSET, temp
    ERASE,0
    
    DRAW_DISTRIBUTION, 0, idisp, [ibval0, ibval1]
    DRAW_PREVIEW, iview
    
    XMANAGER, 'AICer_image', ibase
    
END
;--------------------------------------------------------------------------------------------------




PRO AICer_contour_event, event
    
    ;Creat or edit a contour layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RA':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.ra = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'RAU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.rau = temp[event.index]
        END
        
        'DEC':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.dec = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DECU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.decu = temp[event.index]
        END
        
        'FILE':BEGIN
            fpath = file_dirname(!layercache.file[0])
            ffile = dialog_pickfile(TITLE = 'Select Data File', $
                FILTER = [['*.dat', '*.txt', '*.*'], ['Data Files (*.dat)', 'Text Files (*.txt)', 'All Files (*.*)']], $
                PATH = fpath)
            IF ffile NE '' THEN BEGIN
                !layercache.file[0] = ffile
                tabletext = rd_tfile(ffile, 30)
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ctable'), SET_VALUE = tabletext
            ENDIF
        END
        
        'CRA':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ctable'), /TABLE_SELECT))[0]
            !layercache.cra = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'CDEC':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ctable'), /TABLE_SELECT))[0]
            !layercache.cdec = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'CVALUE':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ctable'), /TABLE_SELECT))[0]
            !layercache.cvalue = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'CINTERCOL':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ctable'), /TABLE_SELECT))[0]
            !layercache.cinter = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccinter'), SET_VALUE = col
        END
        
        'CINTERPER':BEGIN
            !layercache.cinter = -2
            col = ' 1%'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccinter'), SET_VALUE = col
        END
        
        'CINTERONE':BEGIN
            !layercache.cinter = -1
            col = '1.0'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccinter'), SET_VALUE = col
        END
        
        'REGULAR':BEGIN
            !layercache.parameter[6] = event.select
            IF event.select THEN WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'clent'), SET_VALUE = 'Map spacing           ' $
                ELSE WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'clent'), SET_VALUE = 'Maximum segment length'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cfbase2'), SENSITIVE = event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cfbase3'), SENSITIVE = (event.select AND !layercache.parameter[8])
        END
        
        'LEN':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.length = float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'LENU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.lenu = temp[event.index]
        END
        
        'BLANK':BEGIN
            !layercache.parameter[5] = event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cfbase1'), SENSITIVE = event.select
        END
        
        'BVAL0':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.Bval[0] = float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'BVAL1':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.Bval[1] = float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'MISSING':BEGIN
            !layercache.parameter[8] = event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cfbase3'), SENSITIVE = event.select
        END
        
        'MISSINGVAL':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.missing = float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'TIMES':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.times = (float(temp) EQ 0)?1.0:float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'LEVEL':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = input
                !layercache.texti = input
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
                result = levelconvert(input)
                IF result EQ 'error' THEN BEGIN
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccount'), SET_VALUE = '0'
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cvalue'), SET_VALUE = ['error']
                ENDIF ELSE BEGIN
                    !layercache.texto = result
                    output = strsplit(result, /EXTRACT)
                    count = n_elements(output)
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccount'), SET_VALUE = string(count, FORMAT = '(i3)')
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cvalue'), SET_VALUE = output
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cslider'),  SET_SLIDER_MAX = count, SET_VALUE = 1
                    ;ct
                    type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
                    ;			SET_PLOT, 'win'
                    ;			DEVICE, DECOMPOSED = 0
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cshow'), GET_VALUE = temp
                    WSET, temp
                    bar = intarr(!x_width-40,30)
                    IF ~ type THEN BEGIN
                        bar[*,*] = 0
                    ENDIF ELSE BEGIN
                        FOR i=0l, count-1 DO BEGIN
                            a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                            b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                            bar[a:b,*] = 255*i/(count-1)
                        ENDFOR
                    ENDELSE
                    TV, bar, 0, 0
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
                    WSET, temp
                    ERASE, 0
                    ;thick
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cdthick'), GET_VALUE = dthick
                    !layercache.athick = strjoin(replicate(string(float(dthick)), count), ' ')
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cthick'), SET_VALUE = strtrim(string(float(dthick), FORMAT = '(F6.2)'),2)
                ENDELSE
            ENDIF
        END
        
        'VALUE':BEGIN
            type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
            level = widget_info(event.id, /DROPLIST_SELECT)
            count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cslider'), SET_VALUE = level+1
            ;ct
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
            WSET, temp
            bar = intarr(50,24)
            bar[*,*] = 0
            IF type THEN TVLCT, [[replicate(currct[255*level/(count-1),0], !d.table_size)], $
	        [replicate(currct[255*level/(count-1),1], !d.table_size)], $
		[replicate(currct[255*level/(count-1),2], !d.table_size)]]
            TV, bar, 0, 0
            TVLCT, currct
            ;thick
            lthick = strtrim(string(float((strsplit(!layercache.athick, /EXTRACT))[level]), FORMAT = '(F6.2)'),2)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cthick'), SET_VALUE = lthick
        END
        
        'SLIDER':BEGIN
            type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
            count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
            WIDGET_CONTROL, event.id, GET_VALUE = level
            level = level-1
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cvalue'), SET_DROPLIST_SELECT = level
            ;ct
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
            WSET, temp
            ERASE,type*255*level/(count-1)
            ;thick
            lthick = strtrim(string(float((strsplit(!layercache.athick, /EXTRACT))[level]), FORMAT = '(F6.2)'),2)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cthick'), SET_VALUE = lthick
        END
        
        'DIFF':BEGIN
            type = widget_info(event.id, /BUTTON_SET)
            level = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_SELECT)
            count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
            ;		SET_PLOT, 'win'
            ;		DEVICE, DECOMPOSED = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cshow'), GET_VALUE = temp
            WSET, temp
            bar = intarr(!x_width-40,30)
            IF ~ type THEN BEGIN
                bar[*,*] = 0
            ENDIF ELSE BEGIN
                FOR i=0l,count-1 DO BEGIN
                    a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                    b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                    bar[a:b,*] = 255*i/(count-1)
                ENDFOR
            ENDELSE
            TV, bar, 0, 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
            WSET, temp
            ERASE,type*255*level/(count-1)
        END
        
        'MOD':BEGIN
            type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
            level = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_SELECT)
            count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
            TVLCT, !layercache.ct
            XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
            ;		SET_PLOT, 'win'
            ;		DEVICE, DECOMPOSED = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cshow'), GET_VALUE = temp
            WSET, temp
            bar = intarr(!x_width-40,30)
            IF ~ type THEN BEGIN
                bar[*,*] = 0
            ENDIF ELSE BEGIN
                FOR i=0l,count-1 DO BEGIN
                    a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                    b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                    bar[a:b,*] = 255*i/(count-1)
                ENDFOR
            ENDELSE
            TV, bar, 0, 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
            WSET, temp
            ERASE,type*255*level/(count-1)
        END
        
        'USER':BEGIN
            cpath = file_dirname(!layercache.pct)
            cfile = dialog_pickfile(TITLE = 'Select Color Table File', $
                FILTER = [['*.tbl', '*.lut', '*.sao', '*.*'], ['Color Table (*.tbl)', 'DS9/GILDAS (*.lut)', 'SAO/DS9 (*.sao)', 'All Files (*.*)']], $
                PATH = cpath)
            IF cfile NE '' THEN BEGIN
                !layercache.pct = cfile
                type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
                level = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_SELECT)
                count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
                
                dot = strpos(cfile, '.', /REVERSE_SEARCH)
                extension = (dot EQ -1)?'':strmid(cfile, dot+1, strlen(cfile)-dot-1)
                CASE extension OF
                    'sao':BEGIN
                        !layercache.ct = sao2tbl(cfile)
                        TVLCT, !layercache.ct
                        XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                    END
                    'lut':BEGIN
                        !layercache.ct = lut2tbl(cfile)
                        TVLCT, !layercache.ct
                        XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                    END
                    ELSE:XLOADCT, GROUP = event.top, /MODAL, FILE = !layercache.pct
                ENDCASE
                
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cshow'), GET_VALUE = temp
                WSET, temp
                bar = intarr(!x_width-40,30)
                IF ~ type THEN BEGIN
                    bar[*,*] = 0
                ENDIF ELSE BEGIN
                    FOR i=0l,count-1 DO BEGIN
                        a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                        b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                        bar[a:b,*] = 255*i/(count-1)
                    ENDFOR
                ENDELSE
                TV, bar, 0, 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
                WSET, temp
                ERASE,type*255*level/(count-1)
            ENDIF
        END
        
        'DTHICK':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = dthick
            count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
            !layercache.athick = strjoin(replicate(string(float(dthick)), count), ' ')
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cthick'), SET_VALUE = strtrim(string(float(dthick), FORMAT = '(F6.2)'),2)
        END
        
        'COLOR':BEGIN
            IF EVENT.release THEN BEGIN
                type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
                level = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_SELECT)
                count = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_NUMBER)
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 255*level/(count-1)
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cshow'), GET_VALUE = temp
                WSET, temp
                bar = intarr(!x_width-40,30)
                IF ~ type THEN BEGIN
                    bar[*,*] = 0
                ENDIF ELSE BEGIN
                    FOR i=0l,count-1 DO BEGIN
                        a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                        b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                        bar[a:b,*] = 255*i/(count-1)
                    ENDFOR
                ENDELSE
                TV, bar, 0, 0
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ccolor'), GET_VALUE = temp
                WSET, temp
                ERASE,type*255*level/(count-1)
            ENDIF
        END
        
        'THICK':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = thick
            level = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cvalue'), /DROPLIST_SELECT)
            output = strsplit(!layercache.athick, /EXTRACT)
            output[level] = strtrim(string(float(thick)),2)
            !layercache.athick = strjoin(output, ' ')
        END
        
        'POINT':BEGIN
            !layercache.parameter[0] = (event.select*2-1)*abs(!layercache.parameter[0])
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cpbase1'), SENSITIVE = event.select
        END
        
        'LABEL':BEGIN
            type = widget_info(event.id, /BUTTON_SET)
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cpbase2'), SENSITIVE = type
        END
        
        'CHARSIZE':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.asize = (strsplit(!layercache.asize, /EXTRACT))[0]+' '+string(abs(float(temp)))
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SYMTYPE':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = symbollist
            !layercache.afont = symbollist[event.index]
        END
        
        'SYMSIZE':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.asize = string(abs(float(temp)))+' '+(strsplit(!layercache.asize, /EXTRACT))[1]
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SYMFILL':BEGIN
            !layercache.parameter[0] = (event.select+1)*((!layercache.parameter[0] GT 0)*2-1)
        END
        
        'DONE':BEGIN
            ;ct
            type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cdiff'), /BUTTON_SET)
            !layercache.type = type?'DIFFERENT':'SAME'
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct = currct
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'cdthick'), GET_VALUE = temp
            !layercache.dthick = (float(temp) EQ 0)?1.0:float(temp)
            ;parameter
            temp = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cnlee'), /DROPLIST_SELECT)
            !layercache.dsmooth = temp
            ;    	sign = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cpoint'), /BUTTON_SET)?1:-1
            ;    	value = widget_info(widget_info(event.top, FIND_BY_UNAME = 'csymfill'), /BUTTON_SET)+1
            ;		!layercache.parameter[0] = sign*value
            !layercache.parameter[4] = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cbound'), /BUTTON_SET)
            !layercache.parameter[1] = widget_info(widget_info(event.top, FIND_BY_UNAME = 'clabel'), /BUTTON_SET)
            !layercache.parameter[2] = widget_info(widget_info(event.top, FIND_BY_UNAME = 'chill'), /BUTTON_SET)
            !layercache.parameter[3] = widget_info(widget_info(event.top, FIND_BY_UNAME = 'cfill'), /BUTTON_SET)
            
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Contour Layer Help', /MODAL, GROUP = event.top, TITLE = 'Contour Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Change coordinate to fix the center point.', $
                '3.Pick your data file and press each button to', $
                '  select column that is currently highlighted.', $
                '4.Decide regular or irregular mapping', $
                '  a.regular:input the mapping spacing', $
                '  b.irregular:input the max segment length of boundary', $
                '  Set the blanking values if you need', $
                '  Set the missing values if you want to fill the missing positions', $
                '5.Select the unit and scale value', $
                '  Input the levels you need and press ENTER', $
                '  eg. "1 to 10", "1.5 to 10 by .3", "0 to -100 by -10"', $
                '  if the input is wrong, there will be an error tip', $
                '  *DO REMEMBER TO PRESS ENTER AFTER YOU INPUT', $
                '6.Decide to use different color for levels or not.', $
                '  Make a color table of your own.', $
                '7.Customize the plot yourself in Parameter part.', $
                '  That''s easy, I''m lazy.']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_contour, leader, TYPE = type
    
    ;Creat or edit a contour layer(GUI).
    
    IF ~ keyword_set(type) THEN type = 1
    
    cbase = widget_base(TITLE = 'Contour layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 4-type)
    
    IF type EQ 1 THEN BEGIN;CALLED by AICer_image to modify image-contour
        
        ;********
        ;colunm 1
        ccol = widget_base(cbase, COLUMN = 1, XPAD = 0, YPAD = 0)
        
        ;name widget
        cmbase0 = module_text(ccol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
        
        ;*****************
        ;coordinate widget
        cmbase0 = module_frame(ccol, TITLE = 'Reference Point')
        
        cmbase1 = module_text(cmbase0, VALUE = !layercache.ra, UVALUE = 'RA', TITLE = 'X center')
        cmbase1 = module_text(cmbase0, VALUE = !layercache.dec, UVALUE = 'DEC', TITLE = 'Y center')
        
        ;**************************
        ;contour file select widget
        cmbase0 = module_frame(ccol, TITLE = 'File Information')
        
        ;file & table
        cmbase1 = widget_base(cmbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
        cfile = widget_button(cmbase1, VALUE = 'Data file', UVALUE = 'FILE', UNAME = 'cfile', TOOLTIP = 'Open browse')
        IF file_test(!layercache.file[0]) THEN tabletext = rd_tfile(!layercache.file[0], 30) ELSE tabletext = ['']
        ctable = widget_table(cmbase1, VALUE = tabletext, UVALUE = 'TABLE', UNAME = 'ctable', $
            /NO_ROW_HEADERS, /RESIZEABLE_COLUMNS, COLUMN_WIDTH = 50, BACKGROUND_COLOR=[[240,240,240],[225,225,225]], $
            SCR_XSIZE = !x_width-20, SCR_YSIZE = 150, XSIZE = 40, YSIZE = 30)
        
        ;column of ra, dec, value
        cmbase1 = widget_base(cmbase0, ROW = 1)
        ccrat = widget_label(cmbase1, VALUE = 'R.A. col ')
        labeltext = string(!layercache.cra, FORMAT = '(I3)')
        ccra = widget_button(cmbase1, VALUE = labeltext, UVALUE = 'CRA', UNAME = 'ccra', TOOLTIP = 'Column of R.A.')
        ccrat = widget_label(cmbase1, VALUE = ' unit ')
        crau = widget_droplist(cmbase1, VALUE = ['arcsec','arcmin','degree','absolute'], UVALUE = 'RAU', UNAME = 'crau')
        CASE !layercache.rau OF
            'arcmin':WIDGET_CONTROL, crau, SET_DROPLIST_SELECT = 1
            'degree':WIDGET_CONTROL, crau, SET_DROPLIST_SELECT = 2
            'absolute':WIDGET_CONTROL, crau, SET_DROPLIST_SELECT = 3
            ELSE:
        ENDCASE
        
        cmbase1 = widget_base(cmbase0, ROW = 1)
        ccdect = widget_label(cmbase1, VALUE = 'DEC. col ')
        labeltext = string(!layercache.cdec, FORMAT = '(I3)')
        ccdec = widget_button(cmbase1, VALUE = labeltext, UVALUE = 'CDEC', UNAME = 'ccdec', TOOLTIP = 'Column of DEC.')
        ccdect = widget_label(cmbase1, VALUE = ' unit ')
        cdecu = widget_droplist(cmbase1, VALUE = ['arcsec','arcmin','degree','absolute'], UVALUE = 'DECU', UNAME = 'cdecu')
        CASE !layercache.decu OF
            'arcmin':WIDGET_CONTROL, cdecu, SET_DROPLIST_SELECT = 1
            'degree':WIDGET_CONTROL, cdecu, SET_DROPLIST_SELECT = 2
            'absolute':WIDGET_CONTROL, cdecu, SET_DROPLIST_SELECT = 3
            ELSE:
        ENDCASE
        
        cmbase1 = widget_base(cmbase0, ROW = 1)
        ccvaluet = widget_label(cmbase1, VALUE = 'Value col ')
        labeltext = string(!layercache.cvalue, FORMAT = '(I3)')
        ccvalue = widget_button(cmbase1, VALUE = labeltext, UVALUE = 'CVALUE', UNAME = 'ccvalue', TOOLTIP = 'Column of value')
        
        ;regular mapping & length
        cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
        cregular = widget_button(cmbase1, VALUE = 'Regular mapping points', UVALUE = 'REGULAR', UNAME = 'cregular')
        WIDGET_CONTROL, cregular, SET_BUTTON = !layercache.parameter[6]
        labeltext = !layercache.parameter[6]?'Map spacing           ':'Maximum segment length'
        clent = widget_label(cmbase0, VALUE = labeltext, UVALUE = 'LENT', UNAME = 'clent')
        cmbase1 = widget_base(cmbase0, ROW = 1, /ALIGN_CENTER)
        labeltext = strtrim(string(!layercache.length, FORMAT = '(G12.4)'), 2)
        clen = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'LEN', UNAME = 'clen', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
        clenu = widget_droplist(cmbase1, VALUE = ['arcsec','arcmin','degree'], UVALUE = 'LENU', UNAME = 'clenu')
        CASE !layercache.lenu OF
            'arcsec':WIDGET_CONTROL, clenu, SET_DROPLIST_SELECT = 0
            'arcmin':WIDGET_CONTROL, clenu, SET_DROPLIST_SELECT = 1
            'degree':WIDGET_CONTROL, clenu, SET_DROPLIST_SELECT = 2
            ELSE:
        ENDCASE
        
        ;set blanking value
        cmbase1 = widget_base(cmbase0, /NONEXCLUSIVE)
        cblank = widget_button(cmbase1, VALUE = 'Set blanking value', UVALUE = 'BLANK')
        WIDGET_CONTROL, cblank, SET_BUTTON = !layercache.parameter[5]
        cmbase1 = widget_base(cmbase0, ROW = 1, UNAME = 'cfbase1', /ALIGN_CENTER, SENSITIVE = !layercache.parameter[5])
        labeltext = strtrim(string(!layercache.Bval[0], FORMAT = '(G12.4)'), 2)
        cbval0 = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'BVAL0', UNAME = 'cbval0', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
        cbvalt = widget_label(cmbase1, VALUE = ' - ')
        labeltext = strtrim(string(!layercache.Bval[1], FORMAT = '(G12.4)'), 2)
        cbval1 = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'BVAL1', UNAME = 'cbval1', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
        
        ;set missing value
        SENSITIVE = !layercache.parameter[6]
        cmbase1 = widget_base(cmbase0, UNAME = 'cfbase2', /NONEXCLUSIVE, SENSITIVE = !layercache.parameter[6])
        cmissing = widget_button(cmbase1, VALUE = 'Set missing value', UVALUE = 'MISSING')
        WIDGET_CONTROL, cmissing, SET_BUTTON = !layercache.parameter[8]
        labeltext = strtrim(string(!layercache.missing, FORMAT = '(G12.4)'), 2)
        cmbase1 = widget_base(cmbase0, ROW = 1, UNAME = 'cfbase3', /ALIGN_CENTER, SENSITIVE = (!layercache.parameter[6] AND !layercache.parameter[8]))
        cmissing = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'MISSINGVAL', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
        
    ENDIF
    
    ;********
    ;colunm 2
    ccol = widget_base(cbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;levels and color table widget
    cmbase0 = module_frame(ccol, TITLE = 'Levels && ColorTable')
    
    ;inter*time input
    cmbase1 = widget_base(cmbase0, ROW = 1)
    ccintert = widget_label(cmbase1, VALUE = 'Interval ')
    CASE !layercache.cinter OF
        -1:labeltext = '1.0'
        -2:labeltext = ' 1%'
        ELSE:labeltext = string(!layercache.cinter, FORMAT = '(I3)')
    ENDCASE
    ccinter = widget_button(cmbase1, VALUE = labeltext, UVALUE = 'CINTER', UNAME = 'ccinter', TOOLTIP = 'Column of interval', /MENU)
    IF type EQ 1 THEN ccintercol = widget_button(ccinter, VALUE = 'selected column as sigma', UVALUE = 'CINTERCOL', UNAME = 'ccintercol')
    ccinterper = widget_button(ccinter, VALUE = '1%', UVALUE = 'CINTERPER', UNAME = 'ccinterper')
    ccinterone = widget_button(ccinter, VALUE = '1 unit', UVALUE = 'CINTERONE', UNAME = 'ccinterone')
    ccintert = widget_label(cmbase1, VALUE = '*')
    labeltext = strtrim(string(!layercache.times, FORMAT = '(G12.5)'), 2)
    ctimes = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'TIMES', UNAME = 'ctimes', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
    
    ;level
    cmbase1 = widget_base(cmbase0, ROW = 1)
    clevelt = widget_label(cmbase1, VALUE = 'Input  ')
    clevel = widget_text(cmbase1, VALUE = !layercache.texti, UVALUE = 'LEVEL', UNAME = 'clevel', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = !x_width-60)
    cmbase1 = widget_base(cmbase0, ROW = 1)
    ccountt1 = widget_label(cmbase1, VALUE = 'Output ')
    ccount = widget_label(cmbase1, VALUE = '   ', UVALUE = 'COUNT', UNAME = 'ccount')
    ccountt2 = widget_label(cmbase1, VALUE = ' levels: ')
    cvalue = widget_droplist(cmbase1, VALUE = ['        '], UVALUE = 'VALUE', UNAME = 'cvalue')
    cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
    cdiff = widget_button(cmbase1, VALUE = 'Use different color for levels', UVALUE = 'DIFF', UNAME = 'cdiff')
    
    cmbase1 = widget_base(cmbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    cslider = widget_slider(cmbase1, MINIMUM = 1, UVALUE = 'SLIDER', UNAME = 'cslider', /DRAG, $
        SCR_XSIZE = !x_width-20)
    cshow = widget_draw(cmbase1, UNAME = 'cshow', /FRAME, $
        SCR_XSIZE = !x_width-40, SCR_YSIZE = 30)
    cmbase2 = widget_base(cmbase1, ROW = 1, /GRID_LAYOUT)
    cmod = widget_button(cmbase2, VALUE = 'Modify', UVALUE = 'MOD', UNAME = 'cmod', TOOLTIP = 'Modify level color')
    cuser = widget_button(cmbase2, VALUE = 'User', UVALUE = 'USER', UNAME = 'cuser', TOOLTIP = 'Input level color from a file')
    
    cmbase1 = widget_base(cmbase0, ROW = 1, /ALIGN_LEFT)
    cdthickt = widget_label(cmbase1, VALUE = 'Change line thick ')
    labeltext = strtrim(string(!layercache.dthick, FORMAT = '(F6.2)'), 2)
    cdthick = widget_text(cmbase1, VALUE = labeltext, UVALUE = 'DTHICK', UNAME = 'cdthick', /EDITABLE, $
        XSIZE = 6)
    
    cmbase1 = widget_base(cmbase0, ROW = 2, /ALIGN_CENTER, /FRAME)
    ccolort = widget_label(cmbase1, VALUE = 'Current level color ')
    ccolor = widget_draw(cmbase1, UVALUE = 'COLOR', UNAME = 'ccolor', /BUTTON_EVENT, $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    cthickt = widget_label(cmbase1, VALUE = 'Current line thick  ')
    cthick = widget_text(cmbase1, VALUE = '', UVALUE = 'THICK', UNAME = 'cthick', /EDITABLE, $
        SCR_XSIZE = 50)
    
    ;********
    ;colunm 3
    ccol = widget_base(cbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;*************************
    ;Contour Properties widget
    cmbase0 = module_frame(ccol, TITLE = 'Contour Properties')
    ;labels
    cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
    clabel = widget_button(cmbase1, VALUE = 'Draw labels', UVALUE = 'LABEL', UNAME = 'clabel')
    
    cmbase1 = widget_base(cmbase0, ROW = 1, UNAME = 'cpbase2', /ALIGN_CENTER, SENSITIVE = !layercache.parameter[1])
    labeltext = strtrim(string(float((strsplit(!layercache.asize+' 0 0', /EXTRACT))[1]), FORMAT = '(F6.2)'),2)
    ccharsize = module_text(cmbase1, VALUE = labeltext, UVALUE = 'CHARSIZE', TITLE = 'Label charsize', XSIZE = 8)
    
    ;hill lines
    cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
    chill = widget_button(cmbase1, VALUE = 'Draw downhill marks', UVALUE = 'HILL', UNAME = 'chill')
    
    ;fill contour
    cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
    cfill = widget_button(cmbase1, VALUE = 'Fill levels', UVALUE = 'FILL', UNAME = 'cfill')
    
    
    ;**************************
    ;Miscellaneous Items widget
    cmbase0 = module_frame(ccol, TITLE = 'Miscellaneous Items')
    
    ;smooth
    cnlee = widget_droplist(cmbase0, UVALUE = 'NLEE', UNAME = 'cnlee', TITLE = 'Smoothing window width ', VALUE = ['0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15'])
    
    ;draw points
    IF type EQ 1 THEN BEGIN
        cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
        cpoint = widget_button(cmbase1, VALUE = 'Draw data points', UVALUE = 'POINT', UNAME = 'cpoint')
        WIDGET_CONTROL, cpoint, SET_BUTTON = (!layercache.parameter[0] GT 0)
        cmbase1 = widget_base(cmbase0, COLUMN = 1, UNAME = 'cpbase1', /ALIGN_CENTER, SENSITIVE = (!layercache.parameter[0] GT 0))
        csymbol = module_symbol(cmbase1, UVALUE = 'SYMTYPE', SELECTSYM = !layercache.afont)
        labeltext = strtrim(string(float((strsplit(!layercache.asize+' 0 0', /EXTRACT))[0]), FORMAT = '(F6.2)'),2)
        csymsize = module_text(cmbase1, VALUE = labeltext, UVALUE = 'SYMSIZE', TITLE = 'Size  ', XSIZE = 8)
        cmbase2 = widget_base(cmbase1, ROW = 1, /NONEXCLUSIVE)
        csymfill = widget_button(cmbase2, VALUE = 'Filled symbol', UVALUE = 'SYMFILL', UNAME = 'csymfill')
        WIDGET_CONTROL, csymfill, SET_BUTTON = (abs(!layercache.parameter[0]) EQ 2)
    ENDIF
    
    ;boundary line
    cmbase1 = widget_base(cmbase0, ROW = 1, /NONEXCLUSIVE)
    cbound = widget_button(cmbase1, VALUE = 'Draw boundary line', UVALUE = 'BOUND', UNAME = 'cbound')
    
    ;******************
    ;exit button widget
    cmbase0 = module_exit(ccol)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, cbase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, cbase, /REALIZE
    
    ;widget initiation
    input = !layercache.texti
    result = levelconvert(input)
    IF result EQ 'error' THEN BEGIN
        WIDGET_CONTROL, ccount, SET_VALUE = '0'
        WIDGET_CONTROL, cvalue, SET_VALUE = ['error']
    ENDIF ELSE BEGIN
        !layercache.texto = result
        output = strsplit(result, /EXTRACT)
        count = n_elements(output)
        WIDGET_CONTROL, ccount, SET_VALUE = string(count, FORMAT = '(i3)')
        WIDGET_CONTROL, cvalue, SET_VALUE = output
        WIDGET_CONTROL, cslider,  SET_SLIDER_MAX = count, SET_VALUE = 1
        type = !layercache.type EQ 'DIFFERENT'
        ;	SET_PLOT, 'win'
        ;	DEVICE, DECOMPOSED = 0
        TVLCT, !layercache.ct
        WIDGET_CONTROL, cshow, GET_VALUE = temp
        WSET, temp
        bar = intarr(!x_width-40,30)
        IF ~ type THEN BEGIN
            WIDGET_CONTROL, cdiff, SET_BUTTON = 0
            bar[*,*] = 0
        ENDIF ELSE BEGIN
            WIDGET_CONTROL, cdiff, SET_BUTTON = 1
            FOR i=0l, count-1 DO BEGIN
                a = (i EQ 0)?0:(fix(11+float(!x_width-62)/(2*count-2))+(i-1)*float(!x_width-62)/(count-1)+1)
                b = (i EQ count-1)?(!x_width-41):(fix(11+float(!x_width-62)/(2*count-2))+i*float(!x_width-62)/(count-1))
                bar[a:b,*] = 255*i/(count-1)
            ENDFOR
        ENDELSE
        TV, bar, 0, 0
        ;	currct = intarr(!d.table_size,3)
        ;	TVLCT, currct, /GET
        WIDGET_CONTROL, ccolor, GET_VALUE = temp
        WSET, temp
        ERASE, 0
        ;	bar = intarr(50,24)
        ;	bar[*,*] = 0
        ;	IF type THEN TVLCT, [[replicate(currct[0,0], !d.table_size)], [replicate(currct[0,1], !d.table_size)], [replicate(currct[0,2], !d.table_size)]]
        ;	TV, bar, 0, 0
        ;	TVLCT, currct
        ;thick
        lthick = strtrim(string(float(!layercache.athick), FORMAT = '(F6.2)'), 2)
        WIDGET_CONTROL, cthick, SET_VALUE = lthick
    ENDELSE
    WIDGET_CONTROL, cnlee, SET_DROPLIST_SELECT = !layercache.dsmooth
    WIDGET_CONTROL, cbound, SET_BUTTON = !layercache.parameter[4]
    WIDGET_CONTROL, clabel, SET_BUTTON = !layercache.parameter[1]
    WIDGET_CONTROL, chill, SET_BUTTON = !layercache.parameter[2]
    WIDGET_CONTROL, cfill, SET_BUTTON = !layercache.parameter[3]
    
    XMANAGER, 'AICer_contour', cbase
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_point_event, event
    
    ;Creat or edit a contour layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'FILE':BEGIN
            ppath = file_dirname(!layercache.file[0])
            pfile = dialog_pickfile(TITLE = 'Select Catalog File', $
                FILTER = [['*.cat', '*.sou', '*.dat', '*.txt', '*.*'], ['Catalog Files (*.cat)', 'Source Files (*.sou)', 'Data Files (*.dat)', 'Text Files (*.txt)', 'All Files (*.*)']], $
                PATH = ppath)
            IF pfile NE '' THEN BEGIN
                !layercache.file[0] = pfile
                tabletext = rd_tfile(pfile, 40)
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ptable'), SET_VALUE = tabletext
            ENDIF
        END
        
        'CRA':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.cra = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'CDEC':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.cdec = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'RAU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.rau = temp[event.index]
        END
        
        'DECU':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.decu = temp[event.index]
        END
        
        'C_A':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.cvalue = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'C_B':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.cinter = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'C_C':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.cerror = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'WFLAG':BEGIN
            !layercache.parameter[2] = event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pflagbase'), SENSITIVE = event.select
        END
        
        'CFLAG':BEGIN
            col = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'ptable'), /TABLE_SELECT))[0]
            !layercache.ctrust = col
            col = string(col, FORMAT = '(I3)')
            WIDGET_CONTROL, event.id, SET_VALUE = col
        END
        
        'FLAG':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.texto = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'COLOR':BEGIN
            IF EVENT.release THEN BEGIN
                ;XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                XCOLOR, GROUP = event.top, /MODAL
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 0
            ENDIF
        END
        
        'TYPE':BEGIN
            !layercache.type = event.value?'VECTOR':'POINT'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_a'), SENSITIVE = event.value
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_b'), SENSITIVE = event.value
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_c'), SENSITIVE = 0
            IF event.value THEN BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_at'), SET_VALUE = 'Magnitude  '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_bt'), SET_VALUE = 'P.Angle    '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_btt'), SET_VALUE = '  deg E of N'
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_ct'), SET_VALUE = 'P.A. error '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psubtype'), SET_VALUE = ['Polarization', 'Arrow', 'Arrow with PA error']
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psymfill'), SET_VALUE = 'Align center'
            ENDIF ELSE BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_at'), SET_VALUE = 'Flux       '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_bt'), SET_VALUE = 'Name       '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_btt'), SET_VALUE = ''
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_ct'), SET_VALUE = ''
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psubtype'), SET_VALUE = ['Uniform size', 'Resize according to flux', 'Draw source name']
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psymfill'), SET_VALUE = 'Filled symbol'
            ENDELSE
            !layercache.parameter[0] = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psubtype'), SET_DROPLIST_SELECT = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'psymbol'), SENSITIVE = ~ event.value
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'plength'), SENSITIVE = event.value
        END
        
        'SUBTYPE':BEGIN
            !layercache.parameter[0] = event.index
            IF !layercache.type EQ 'VECTOR' THEN BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_a'), SENSITIVE = 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_b'), SENSITIVE = 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_c'), SENSITIVE = event.index EQ 2
            ENDIF ELSE BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_a'), SENSITIVE = event.index EQ 1
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_b'), SENSITIVE = event.index EQ 2
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'pc_c'), SENSITIVE = 0
            ENDELSE
        END
        
        'SYMTYPE':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.afont = temp[event.index]
        END
        
        'SYMSIZE':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.asize = strtrim(string(float(temp), FORMAT = '(F6.2)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SYMTHICK':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.athick = strtrim(string(float(temp), FORMAT = '(F6.2)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SYMFILL':BEGIN
            !layercache.parameter[1] = event.select
        END
        
        'LENGTH':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.aorient = strtrim(string(double(temp), FORMAT = '(G0)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DONE':BEGIN
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct = currct
            
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Point Layer Help', /MODAL, GROUP = event.top, TITLE = 'Point Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Pick your catalog file and press each button to', $
                '  select column that is currently highlighted.', $
                '3.Choose a type to draw point or vector.', $
                '4.Customize the plot yourself in Parameter part', $
                '  That''s easy, feel free to try this by yourself.']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
END
;--------------------------------------------------------------------------------------------------



PRO AICer_point, leader
    
    ;Creat or edit a point layer(GUI).
    
    pbase = widget_base(TITLE = 'Point layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 2)
    
    ;********
    ;colunm 1
    pcol = widget_base(pbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;name widget
    pmbase0 = module_text(pcol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
    
    ;file select widget
    pmbase0 = module_frame(pcol, TITLE = 'File Information')
    
    pmbase1 = widget_base(pmbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    pfile = widget_button(pmbase1, VALUE = 'Catalog file', UVALUE = 'FILE', UNAME = 'pfile', TOOLTIP = 'Open browse')
    ptable = widget_table(pmbase1, UVALUE = 'TABLE', UNAME = 'ptable', $
        /NO_ROW_HEADERS, /RESIZEABLE_COLUMNS, COLUMN_WIDTH = 50, BACKGROUND_COLOR=[[240,240,240],[225,225,225]], $
        SCR_XSIZE = !x_width-20, SCR_YSIZE = 150, XSIZE = 40, YSIZE = 30)
    
    ;column and unit selection
    ;title
    pmbase1 = widget_base(pmbase0, /ALIGN_LEFT)
    pcdect1 = widget_label(pmbase1, VALUE = '            Column    Unit')
    ;ra
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT)
    pcrat1 = widget_label(pmbase1, VALUE = 'R.A.       ')
    labeltext = string(!layercache.cra, FORMAT = '(I3)')
    pcra = widget_button(pmbase1, VALUE = labeltext, UVALUE = 'CRA', UNAME = 'pcra', TOOLTIP = 'Column of R.A.')
    pcrat = widget_label(pmbase1, VALUE = '  ')
    prau = widget_droplist(pmbase1, VALUE = ['absolute','arcsec','arcmin','degree'], UVALUE = 'RAU', UNAME = 'prau')
    CASE !layercache.rau OF
        'arcsec':WIDGET_CONTROL, prau, SET_DROPLIST_SELECT = 1
        'arcmin':WIDGET_CONTROL, prau, SET_DROPLIST_SELECT = 2
        'degree':WIDGET_CONTROL, prau, SET_DROPLIST_SELECT = 3
        ELSE:
    ENDCASE
    ;dec
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT)
    pcdect1 = widget_label(pmbase1, VALUE = 'DEC.       ')
    labeltext = string(!layercache.cdec, FORMAT = '(I3)')
    pcdec = widget_button(pmbase1, VALUE = labeltext, UVALUE = 'CDEC', UNAME = 'pcdec', TOOLTIP = 'Column of DEC.')
    pcdect = widget_label(pmbase1, VALUE = '  ')
    pdecu = widget_droplist(pmbase1, VALUE = ['absolute','arcsec','arcmin','degree'], UVALUE = 'DECU', UNAME = 'pdecu')
    CASE !layercache.decu OF
        'arcsec':WIDGET_CONTROL, pdecu, SET_DROPLIST_SELECT = 1
        'arcmin':WIDGET_CONTROL, pdecu, SET_DROPLIST_SELECT = 2
        'degree':WIDGET_CONTROL, pdecu, SET_DROPLIST_SELECT = 3
        ELSE:
    ENDCASE
    ;fluw/magnitude
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT, UNAME = 'pc_a', $
        SENSITIVE = (!layercache.type EQ 'VECTOR') OR (!layercache.parameter[0] EQ 1))
    pc_at = widget_label(pmbase1, VALUE = 'Flux       ', UNAME = 'pc_at')
    labeltext = string(!layercache.cvalue, FORMAT = '(I3)')
    pc_a = widget_button(pmbase1, VALUE = labeltext, UVALUE = 'C_A')
    ;name/angle
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT, UNAME = 'pc_b', $
        SENSITIVE = (!layercache.type EQ 'VECTOR') OR (!layercache.parameter[0] EQ 2))
    pc_bt = widget_label(pmbase1, VALUE = 'Name       ', UNAME = 'pc_bt')
    labeltext = string(!layercache.cinter, FORMAT = '(I3)')
    pc_b = widget_button(pmbase1, VALUE = labeltext, UVALUE = 'C_B')
    pc_btt = widget_label(pmbase1, VALUE = '            ', UNAME = 'pc_btt')
    ;-/error
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT, UNAME = 'pc_c', $
        SENSITIVE = (!layercache.type EQ 'VECTOR') AND (!layercache.parameter[0] EQ 2))
    pc_ct = widget_label(pmbase1, VALUE = '           ', UNAME = 'pc_ct')
    labeltext = string(!layercache.cerror, FORMAT = '(I3)')
    pc_c = widget_button(pmbase1, VALUE = labeltext, UVALUE = 'C_C')
    
    ;flag
    pmbase1 = widget_base(pmbase0, ROW = 1, /ALIGN_LEFT)
    pmbase2 = widget_base(pmbase1, /NONEXCLUSIVE)
    pwflag = widget_button(pmbase2, VALUE = 'Flag', UVALUE = 'WFLAG')
    pmbase2 = widget_base(pmbase1, ROW = 1, /ALIGN_LEFT, UNAME = 'pflagbase', SENSITIVE = !layercache.parameter[2])
    labeltext = string(!layercache.ctrust, FORMAT = '(I3)')
    pcflag = widget_button(pmbase2, VALUE = labeltext, UVALUE = 'CFLAG', UNAME = 'pcflag', TOOLTIP = 'Column of flag')
    pcflagt = widget_label(pmbase2, VALUE = ' = ')
    pcflag = widget_text(pmbase2, VALUE = !layercache.texto, UVALUE = 'FLAG', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 12)
    
    ;********
    ;colunm 2
    pcol = widget_base(pbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;parameter widget
    pmbase0 = module_frame(pcol, TITLE = 'Display Type')
    ;ptype = cw_bgroup(pmbase0,['Normal point', 'Resize according to flux', 'Draw source name', 'Vector'], UVALUE = 'TYPE', UNAME = 'ptype', $
    ;	COLUMN = 1, /EXCLUSIVE, /NO_RELEASE)
    pmbase1 = widget_base(pmbase0, COLUMN = 1, /ALIGN_LEFT)
    ptype = cw_bgroup(pmbase1,['Point', 'Vector'], UVALUE = 'TYPE', UNAME = 'ptype', $
        ROW = 1, /EXCLUSIVE, /NO_RELEASE)
    psubtype = widget_droplist(pmbase1, VALUE = ['                          '], UVALUE = 'SUBTYPE', UNAME = 'psubtype', TITLE = '  ')
    
    ;*************
    ;symbol widget
    pmbase0 = module_frame(pcol, TITLE = 'Symbol')
    
    pmbase1 = widget_base(pmbase0, COLUMN = 1)
    psymbol = module_symbol(pmbase1, UVALUE = 'SYMTYPE', UNAME = 'psymbol', SELECTSYM = !layercache.afont)
    
    labeltext = strtrim(string(double(!layercache.asize), FORMAT = '(F6.2)'), 2)
    psymsize = module_text(pmbase1, VALUE = labeltext, UVALUE = 'SYMSIZE', TITLE = 'Size  ', XSIZE = 8)
    labeltext = strtrim(string(float(!layercache.athick), FORMAT = '(F6.2)'), 2)
    psymthick = module_text(pmbase1, VALUE = labeltext, UVALUE = 'SYMTHICK', TITLE = 'Thick ', XSIZE = 8)
    
    pmbase2 = widget_base(pmbase1, ROW = 1)
    pcolort = widget_label(pmbase2, VALUE = 'Color ')
    pcolor = widget_draw(pmbase2, UVALUE = 'COLOR', UNAME = 'pcolor', BUTTON_EVENT = 'AICer_point_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    pmbase2 = widget_base(pmbase1, ROW = 1, /NONEXCLUSIVE)
    psymfill = widget_button(pmbase2, VALUE = 'Filled symbol', UVALUE = 'SYMFILL', UNAME = 'psymfill')
    
    pmbase2 = widget_base(pmbase1, ROW = 1, UNAME = 'plength', SENSITIVE = !layercache.type EQ 'VECTOR')
    plengtht = widget_label(pmbase2, VALUE = 'Length ')
    labeltext = string(double(!layercache.aorient), FORMAT = '(G0)')
    plength = widget_text(pmbase2, VALUE = labeltext, UVALUE = 'LENGTH', /EDITABLE, /KBRD_FOCUS_EVENTS, XSIZE = 14)
    plengtht = widget_label(pmbase2, VALUE = ' deg = 1 unit')
    
    ;exit button widget
    pmbase0 = module_exit(pcol)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, pbase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, pbase, /REALIZE
    
    ;widget initiation
    file = !layercache.file[0]
    IF file_test(file) THEN BEGIN
        tabletext = rd_tfile(file, 40)
        WIDGET_CONTROL, ptable, SET_VALUE = tabletext
    ENDIF
    
    WIDGET_CONTROL, ptype, SET_VALUE = !layercache.type EQ 'VECTOR'
    IF !layercache.type EQ 'VECTOR' THEN BEGIN
        WIDGET_CONTROL, pc_at, SET_VALUE = 'Magnitude  '
        WIDGET_CONTROL, pc_bt, SET_VALUE = 'P.Angle    '
        WIDGET_CONTROL, pc_ct, SET_VALUE = 'P.A. error '
        WIDGET_CONTROL, psubtype, SET_VALUE = ['Polarization', 'Arrow', 'Arrow with PA error']
        WIDGET_CONTROL, pc_btt, SET_VALUE = '  deg E of N'
        WIDGET_CONTROL, psymfill, SET_VALUE = 'Align center'
    ENDIF ELSE BEGIN
        WIDGET_CONTROL, psubtype, SET_VALUE = ['Uniform size', 'Resize according to flux', 'Draw source name']
        WIDGET_CONTROL, psymfill, SET_VALUE = 'Filled symbol'
    ENDELSE
    WIDGET_CONTROL, psubtype, SET_DROPLIST_SELECT = !layercache.parameter[0]
    
    WIDGET_CONTROL, pwflag, SET_BUTTON = !layercache.parameter[2]
    WIDGET_CONTROL, psymbol, SENSITIVE = !layercache.type NE 'VECTOR'
    WIDGET_CONTROL, psymfill, SET_BUTTON = !layercache.parameter[1]
    
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    TVLCT, !layercache.ct
    WIDGET_CONTROL, pcolor, GET_VALUE = temp
    WSET, temp
    ERASE, 0
    ;bar = intarr(50,24)
    ;bar[*,*] = 0
    ;TVLCT, [[replicate(!layercache.ct[0,0], !d.table_size)], [replicate(!layercache.ct[0,1], !d.table_size)], [replicate(!layercache.ct[0,2], !d.table_size)]]
    ;TVSCL, bar, 0, 0
    
    XMANAGER, 'AICer_point', pbase
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_text_event, event
    
    ;Creat or edit a text layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'COOR':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.rau = temp[event.index]
        END
        
        'RA':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.raof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'DEC':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.decof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'TEXT':BEGIN
            IF (size(event, /STRUCTURE)).STRUCTURE_NAME EQ 'WIDGET_KBRD_FOCUS' AND ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.texti = strjoin(temp, ' ')
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'CHARFONT':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = fontlist
            !layercache.afont = '!'+(strsplit(fontlist[event.index], /EXTRACT))[0]
        END
        
        'CHARSIZE':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.asize = strtrim(string(abs(float(temp)), FORMAT = '(F6.2)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'CHARTHICK':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.athick = strtrim(string(abs(float(temp)), FORMAT = '(F6.2)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'COLOR':BEGIN
            IF EVENT.release THEN BEGIN
                ;XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                XCOLOR, GROUP = event.top, /MODAL
                ;SET_PLOT, 'win'
                ;DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE
            ENDIF
        END
        
        'CHARORIENT':BEGIN
            !layercache.aorient = strtrim(string(event.value, FORMAT = '(I3)'), 2)
        END
        
        'DONE':BEGIN
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct = currct
            
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Text Layer Help', /MODAL, GROUP = event.top, TITLE = 'Text Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Give a position to draw the text', $
                '3.Write something to praise the author? or any other things you need', $
                '  *use /source /ra /dec to represent the parameter', $
                '   of the source', $
                '4.Font? Size? Thick? Color? Orientation? All are yours.']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
END
;--------------------------------------------------------------------------------------------------



PRO AICer_text, leader
    
    ;Creat or edit a text layer(GUI).
    
    tbase = widget_base(TITLE = 'Text layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 1)
    
    ;********
    ;colunm 1
    tcol = widget_base(tbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;name widget
    tmbase0 = module_text(tcol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
    
    ;position widget
    tmbase0 = module_frame(tcol, TITLE = 'Position')
    
    tcoortype = widget_droplist(tmbase0, UVALUE = 'COOR', UNAME = 'tcoor', TITLE = 'Coordinate',$
        VALUE = ['Normalized(%)', 'Absolute(WCS)'])
    IF !layercache.rau EQ 'Absolute(WCS)' THEN WIDGET_CONTROL, tcoortype, SET_DROPLIST_SELECT = 1
    ;labeltext = strtrim(string(float(!layercache.raof), FORMAT = '(F6.2)'), 2)
    tmbase1 = module_text(tmbase0, VALUE = !layercache.raof, UVALUE = 'RA', TITLE = 'X ')
    ;labeltext = strtrim(string(float(!layercache.decof), FORMAT = '(F6.2)'), 2)
    tmbase1 = module_text(tmbase0, VALUE = !layercache.decof, UVALUE = 'DEC', TITLE = 'Y ')
    
    ;text widget
    tmbase0 = module_frame(tcol, TITLE = 'Text')
    
    tmbase1 = widget_base(tmbase0, COLUMN = 1, /ALIGN_CENTER)
    ttext = widget_text(tmbase0, VALUE = strtrim(!layercache.texti,2), UVALUE = 'TEXT', UNAME = 'ttext', $
        /EDITABLE, /WRAP, /KBRD_FOCUS_EVENTS, $
        SCR_XSIZE = !x_width-20, YSIZE = 4)
    
    ;parameter widget
    tmbase0 = module_frame(tcol, TITLE = 'Format')
    
    tfont = module_font(tmbase0, UVALUE = 'CHARFONT', UNAME = 'tcharfont', SELECTFONT = !layercache.afont)
    labeltext = strtrim(string(float(!layercache.asize), FORMAT = '(F6.2)'), 2)
    tmbase1 = module_text(tmbase0, VALUE = labeltext, UVALUE = 'CHARSIZE', TITLE = 'Size  ', XSIZE = 8)
    labeltext = strtrim(string(float(!layercache.athick), FORMAT = '(F6.2)'), 2)
    tmbase1 = module_text(tmbase0, VALUE = labeltext, UVALUE = 'CHARTHICK', TITLE = 'Thick ', XSIZE = 8)
    tmbase1 = widget_base(tmbase0, ROW = 1)
    tcolort = widget_label(tmbase1, VALUE = 'Color ')
    tcolor = widget_draw(tmbase1, UVALUE = 'COLOR', UNAME = 'tcolor', BUTTON_EVENT = 'AICer_text_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    tmbase1 = widget_base(tmbase0, ROW = 2)
    tcharorientt = widget_label(tmbase1, VALUE = 'Orientation')
    tcharorient = widget_slider(tmbase1, VALUE = fix(!layercache.aorient), UVALUE = 'CHARORIENT', UNAME = 'tcharorient', $
        MINIMUM = -90, MAXIMUM = 90, SCR_XSIZE = !x_width-20)
    
    ;exit button widget
    tmbase0 = module_exit(tcol)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, tbase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, tbase, /REALIZE
    
    ;widget initiation
    
    ;IF !layercache.type EQ 'TIP' THEN BEGIN
    ;	WIDGET_CONTROL, ttip, SET_BUTTON = 1
    ;ENDIF ELSE BEGIN
    ;	WIDGET_CONTROL, ttitle, SET_BUTTON = 1
    ;	WIDGET_CONTROL, tsbase3, SENSITIVE = 0
    ;	WIDGET_CONTROL, tpbase4, SENSITIVE = 0
    ;	WIDGET_CONTROL, tcharorient, SENSITIVE = 0
    ;ENDELSE
    
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    TVLCT, !layercache.ct
    WIDGET_CONTROL, tcolor, GET_VALUE = temp
    WSET, temp
    ERASE, 0
    ;bar = intarr(50,24)
    ;bar[*,*] = 0
    ;TVLCT, [[replicate(!layercache.ct[0,0], !d.table_size)], [replicate(!layercache.ct[0,1], !d.table_size)], [replicate(!layercache.ct[0,2], !d.table_size)]]
    ;TVSCL, bar, 0, 0
    
    XMANAGER, 'AICer_text', tbase
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_shape_event, event
    
    ;Creat or edit a text layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'LINE':BEGIN
            !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp1'), SET_VALUE = 'P1'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp2'), SET_VALUE = 'P2'
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'spu'), SET_VALUE = ' '
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sangle'), SENSITIVE = 0
        END
        
        'ARROW':BEGIN
            !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp1'), SET_VALUE = 'Tail'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp2'), SET_VALUE = 'Head'
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'spu'), SET_VALUE = ' '
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sangle'), SENSITIVE = 0
        END
        
        'RULER':BEGIN
            !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp1'), SET_VALUE = 'Center'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp2'), SET_VALUE = 'Length'
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'spu'), SET_VALUE = ' deg'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sangle'), SENSITIVE = 1
        END
        
        'ELLIPSE':BEGIN
            !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp1'), SET_VALUE = 'Center'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp2'), SET_VALUE = 'Diameter'
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'spu'), SET_VALUE = ' deg'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sangle'), SENSITIVE = 1
        END
        
        'RECTANGLE':BEGIN
            !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp1'), SET_VALUE = 'Center'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sp2'), SET_VALUE = 'Size'
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'spu'), SET_VALUE = ' deg'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'sangle'), SENSITIVE = 1
        END
        
        'SP1X':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.raof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SP1Y':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.decof = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SP1T':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.rau = temp[event.index]
        END
        
        'SP2X':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.raot = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SP2Y':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.decot = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'SP2T':BEGIN
            WIDGET_CONTROL, event.id, GET_VALUE = temp
            !layercache.decu = temp[event.index]
        END
        
        'ANGLE':BEGIN
            !layercache.aorient = strtrim(string(event.value, FORMAT = '(I3)'), 2)
        END
        
        'LINETHICK':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.athick = strtrim(string(float(temp), FORMAT = '(F6.2)'), 2)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
        
        'COLOR':BEGIN
            IF EVENT.release THEN BEGIN
                ;XLOADCT, GROUP = event.top, /MODAL, /USE_CURRENT
                XCOLOR, GROUP = event.top, /MODAL
                ;SET_PLOT, 'win'
                ;DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 0
            ENDIF
        END
        
        'SHAPEFILL':BEGIN
            !layercache.parameter[0] = event.select
        END
        
        'DONE':BEGIN
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct = currct
            
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Shape Layer Help', /MODAL, GROUP = event.top, TITLE = 'Shape Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Which shape you want to draw?', $
                '3.Set the parameters of the shape.', $
                '4.Thick? Color? hum~~']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
END
;--------------------------------------------------------------------------------------------------



PRO AICer_shape, leader
    
    ;Creat or edit a text layer(GUI).
    
    sbase = widget_base(TITLE = 'Shape layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 1)
    
    ;********
    ;colunm 1
    scol = widget_base(sbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;name widget
    smbase0 = module_text(scol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
    
    ;shape type widget
    smbase0 = module_frame(scol, TITLE = 'Type')
    
    smbase1 = widget_base(smbase0, COLUMN = 2, /EXCLUSIVE)
    sline = widget_button(smbase1, VALUE = 'Line', UVALUE = 'LINE', UNAME = 'sline')
    sarrow = widget_button(smbase1, VALUE = 'Arrow', UVALUE = 'ARROW', UNAME = 'sarrow')
    sruler = widget_button(smbase1, VALUE = 'Ruler', UVALUE = 'RULER', UNAME = 'sruler')
    sellipse = widget_button(smbase1, VALUE = 'Ellipse', UVALUE = 'ELLIPSE', UNAME = 'sellipse')
    srectangle = widget_button(smbase1, VALUE = 'Rectangle', UVALUE = 'RECTANGLE', UNAME = 'srectangle')
    
    ;position widget
    smbase0 = module_frame(scol, TITLE = 'Position && Size')
    
    smbase1 = widget_base(smbase0, ROW = 2)
    
    sp1 = widget_label(smbase1, VALUE = 'label_TBD', UNAME = 'sp1')
    sp1t = widget_droplist(smbase1, UVALUE = 'SP1T', UNAME = 'sp1t', $
        VALUE = ['Normalized(%)', 'Absolute(WCS)'])
    IF !layercache.rau EQ 'Absolute(WCS)' THEN WIDGET_CONTROL, sp1t, SET_DROPLIST_SELECT = 1
    sp1x = widget_text(smbase1, VALUE = !layercache.raof, UVALUE = 'SP1X', UNAME = 'sp1x', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    sp1y = widget_text(smbase1, VALUE = !layercache.decof, UVALUE = 'SP1Y', UNAME = 'sp1y', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    
    smbase1 = widget_base(smbase0, ROW = 1)
    
    sp2 = widget_label(smbase1, VALUE = 'label_TBD', UNAME = 'sp2')
    sp2t = widget_droplist(smbase1, UVALUE = 'SP2T', UNAME = 'sp2t', $
        VALUE = ['Normalized(%)', 'Absolute(WCS)'])
    IF !layercache.decu EQ 'Absolute(WCS)' THEN WIDGET_CONTROL, sp2t, SET_DROPLIST_SELECT = 1
    smbase1 = widget_base(smbase0, ROW = 1)
    sp2x = widget_text(smbase1, VALUE = !layercache.raot, UVALUE = 'SP2X', UNAME = 'sp2x', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    sp2y = widget_text(smbase1, VALUE = !layercache.decot, UVALUE = 'SP2Y', UNAME = 'sp2y', /EDITABLE, /KBRD_FOCUS_EVENTS, $
        XSIZE = 12)
    ;spu = widget_label(smbase1, VALUE = 'label', UNAME = 'spu')
    
    sangleb = widget_base(smbase0, ROW = 2, UNAME = 'sangle', SENSITIVE = ~(!layercache.type EQ 'LINE' OR !layercache.type EQ 'ARROW'))
    
    sanglet = widget_label(sangleb, VALUE = 'Angle (deg)')
    sangle = widget_slider(sangleb, VALUE = fix(!layercache.aorient), UVALUE = 'ANGLE', $
        MINIMUM = -90, MAXIMUM = 90, SCR_XSIZE = !x_width-20)
    ;sangle = cw_fslider(sangleb, VALUE = float(!layercache.aorient), UVALUE = 'ANGLE', /DRAG, /EDIT, $
    ;	MINIMUM = -90, MAXIMUM = 90, XSIZE = !x_width-20)
    
    ;parameter widget
    smbase0 = module_frame(scol, TITLE = 'Properties')
    
    labeltext = strtrim(string(float(!layercache.athick), FORMAT = '(F6.2)'), 2)
    smbase1 = module_text(smbase0, VALUE = labeltext, UVALUE = 'LINETHICK', TITLE = 'Thick ', XSIZE = 8)
    smbase1 = widget_base(smbase0, ROW = 1)
    scolort = widget_label(smbase1, VALUE = 'Color ')
    scolor = widget_draw(smbase1, UVALUE = 'COLOR', UNAME = 'tcolor', BUTTON_EVENT = 'AICer_shape_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    smbase1 = widget_base(smbase0, ROW = 1, /NONEXCLUSIVE)
    sfill = widget_button(smbase1, VALUE = 'Filled shape', UVALUE = 'SHAPEFILL', UNAME = 'shapefill')
    
    ;exit button widget
    smbase0 = module_exit(scol)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, sbase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, sbase, /REALIZE
    
    ;widget initiation
    
    CASE !layercache.type OF
        'LINE':BEGIN
            WIDGET_CONTROL, sline, /SET_BUTTON
            WIDGET_CONTROL, sp1, SET_VALUE = 'P1'
            WIDGET_CONTROL, sp2, SET_VALUE = 'P2'
            ;		WIDGET_CONTROL, spu, SET_VALUE = ' '
        END
        'ARROW':BEGIN
            WIDGET_CONTROL, sarrow, /SET_BUTTON
            WIDGET_CONTROL, sp1, SET_VALUE = 'Tail'
            WIDGET_CONTROL, sp2, SET_VALUE = 'Head'
            ;		WIDGET_CONTROL, spu, SET_VALUE = ' '
        END
        'RULER':BEGIN
            WIDGET_CONTROL, sruler, /SET_BUTTON
            WIDGET_CONTROL, sp1, SET_VALUE = 'Center'
            WIDGET_CONTROL, sp2, SET_VALUE = 'Length'
            ;		WIDGET_CONTROL, spu, SET_VALUE = ' deg'
        END
        'ELLIPSE':BEGIN
            WIDGET_CONTROL, sellipse, /SET_BUTTON
            WIDGET_CONTROL, sp1, SET_VALUE = 'Center'
            WIDGET_CONTROL, sp2, SET_VALUE = 'Diameter'
            ;		WIDGET_CONTROL, spu, SET_VALUE = ' deg'
        END
        'RECTANGLE':BEGIN
            WIDGET_CONTROL, srectangle, /SET_BUTTON
            WIDGET_CONTROL, sp1, SET_VALUE = 'Center'
            WIDGET_CONTROL, sp2, SET_VALUE = 'Size'
            ;		WIDGET_CONTROL, spu, SET_VALUE = ' deg'
        END
        ELSE:
    ENDCASE
    WIDGET_CONTROL, sfill, SET_BUTTON = !layercache.parameter[0]
    
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    TVLCT, !layercache.ct
    WIDGET_CONTROL, scolor, GET_VALUE = temp
    WSET, temp
    ERASE, 0
    ;bar = intarr(50,24)
    ;bar[*,*] = 0
    ;TVLCT, [[replicate(!layercache.ct[0,0], !d.table_size)], [replicate(!layercache.ct[0,1], !d.table_size)], [replicate(!layercache.ct[0,2], !d.table_size)]]
    ;TVSCL, bar, 0, 0
    
    ;WIDGET_CONTROL, sfill, SET_BUTTON = !layercache.parameter[0]
    
    XMANAGER, 'AICer_shape', sbase
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_panel_event, event
    
    ;Creat or edit a text layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'NAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.uname = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END

	'AIC':BEGIN
	    !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_AF'), SENSITIVE = event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_PI'), SENSITIVE = ~ event.select
	END

	'PRO':BEGIN
	    !layercache.type = layeraction
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_AF'), SENSITIVE = ~ event.select
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'module_PI'), SENSITIVE = event.select
	END
	
        'FILE':BEGIN
            lpath = file_dirname(!layercache.file[0])
	    IF !layercache.type EQ 'AIC' THEN BEGIN
                lfile = dialog_pickfile(TITLE = 'Select Saved AICer File', PATH = lpath, $
                    FILTER = [['*.aic', '*.*'], ['AICer Files (*.aic)', 'All Files (*.*)']])
	    ENDIF ELSE BEGIN
                lfile = dialog_pickfile(TITLE = 'Select IDL Procedure File', PATH = lpath, $
                    FILTER = [['*.pro', '*.sav', '*.*'], ['Procedure Script (*.pro)', 'Saved Routine (*.sav)', 'All Files (*.*)']])
	    ENDELSE
            IF lfile NE '' THEN BEGIN
                !layercache.file[0] = lfile
		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ltip'), SET_VALUE = 'File selected: '+file_basename(!layercache.file[0])
            ENDIF
        END
 
	'XOFF':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.ra = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END

	'YOFF':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.dec = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END

	'SCALE':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.times = (float(temp) EQ 0)?1.0:float(temp)
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END
  
	'PRONAME':BEGIN
            IF ~ event.(3) THEN BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                !layercache.file[1] = temp
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 0
            ENDIF
        END

        'DONE':BEGIN
            IF !layercache.valid EQ 0 THEN !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Panel Layer Help', /MODAL, GROUP = event.top, TITLE = 'Shape Layer Help', DONE_BUTTON = 'Close', $
                TEXT = ['How to use this?', $
                '1.Input a layer name you like or use the default one.', $
                '2.Choose to load a panel from a saved .aic file or an idl procedure script.', $
                '3.Set the page offset and scale of the imported aicer page.']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
 
	ELSE:
    ENDCASE
END
;--------------------------------------------------------------------------------------------------



PRO AICer_panel, leader
    
    ;Creat or edit a panel layer(GUI).
    
    lbase = widget_base(TITLE = 'Panel layer', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 1)
    
    ;********
    ;colunm 1
    lcol = widget_base(lbase, COLUMN = 1, XPAD = 0, YPAD = 0)
    
    ;name widget
    lmbase0 = module_text(lcol, VALUE = !layercache.uname, UVALUE = 'NAME', TITLE = 'Name ')
    
    ;panel source widget
    lmbase0 = module_frame(lcol, TITLE = 'Type')
    ltype = widget_label(lmbase0, VALUE = 'Import the panel from:', /ALIGN_LEFT)
    lmbase1 = widget_base(lmbase0, COLUMN = 1, /EXCLUSIVE)
    laic = widget_button(lmbase1, VALUE = 'A saved AICer file (.aic file)', UVALUE = 'AIC', UNAME = 'laic')
    lpro = widget_button(lmbase1, VALUE = 'An IDL procedure/function', UVALUE = 'PRO', UNAME = 'lpro')
    
    ;file selection widget
    lmbase0 = module_frame(lcol, TITLE = 'AICer File Info', FNAME = 'module_AF')

    lmbase1 = widget_base(lmbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    lfile = widget_button(lmbase1, VALUE = 'File', UVALUE = 'FILE', UNAME = 'lfile', TOOLTIP = 'Open browse')
    IF !layercache.file[0] EQ '' THEN labeltext = 'Please select a file!' ELSE labeltext = 'File selected: '+file_basename(!layercache.file[0])
    ltip = widget_label(lmbase0, VALUE = labeltext, UVALUE = 'TIP', UNAME = 'ltip', $
	SCR_XSIZE = !x_width)
    lxoff = module_text(lmbase0, VALUE = !layercache.ra, UVALUE = 'XOFF', TITLE = 'X offset', TAIL = ' cm', XSIZE=6)
    lyoff = module_text(lmbase0, VALUE = !layercache.dec, UVALUE = 'YOFF', TITLE = 'Y offset', TAIL = ' cm', XSIZE=6)
    labeltext = strtrim(string(!layercache.times, FORMAT = '(G12.5)'), 2)
    lscale = module_text(lmbase0, VALUE = labeltext, UVALUE = 'SCALE', TITLE = 'Scale   ', XSIZE = 12)
    WIDGET_CONTROL, widget_info(lmbase0, /PARENT), SENSITIVE = (!layercache.type EQ 'AIC')

    ;procedure info
    lmbase0 = module_frame(lcol, TITLE = 'Procedure/Function Info', FNAME = 'module_PI')

    lproname = module_text(lmbase0, VALUE = !layercache.file[1], UVALUE = 'PRONAME', TITLE = 'Name', XSIZE=20)
    WIDGET_CONTROL, widget_info(lmbase0, /PARENT), SENSITIVE = ~(!layercache.type EQ 'AIC')
 
    ;exit button widget
    lmbase0 = module_exit(lcol)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, lbase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, lbase, /REALIZE
    
    ;widget initiation
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    IF !layercache.type eq 'AIC' THEN WIDGET_CONTROL, laic, /SET_BUTTON ELSE WIDGET_CONTROL, lpro, /SET_BUTTON
    
    XMANAGER, 'AICer_panel', lbase
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer_custom_event, event
    
    ;Creat or edit a text layer(event).
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    
    CASE layeraction OF
        
        'LINECOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 0
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 0
            ENDIF
        END
        
        'GRIDCOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 1
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 1
            ENDIF
        END
        
        'NUMBCOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 2
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 2
            ENDIF
        END
        
        'LABECOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 3
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 3
            ENDIF
        END
        
        'TITLCOLOR':BEGIN
            IF EVENT.release THEN BEGIN
                XCOLOR, GROUP = event.top, /MODAL, COLOR = 4
                ;			SET_PLOT, 'win'
                ;			DEVICE, DECOMPOSED = 0
                WIDGET_CONTROL, event.id, GET_VALUE = temp
                WSET, temp
                ERASE, 4
            ENDIF
        END
        
        ;	'NUMBXF':BEGIN
        ;		!layercache.cra = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbmn'), SENSITIVE = event.index NE 5
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbsc'), SENSITIVE = event.index NE 5
        ;	END
        ;
        ;	'NUMBHR':BEGIN
        ;		!layercache.parameter[1] = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbmn'), SENSITIVE = event.index EQ 0 AND !layercache.cra NE 5
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbsc'), SENSITIVE = event.index EQ 0 AND !layercache.cra NE 5
        ;	END
        ;
        ;	'NUMBMN':BEGIN
        ;		!layercache.parameter[2] = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbsc'), SENSITIVE = event.index EQ 0
        ;	END
        ;
        ;	'NUMBSC':BEGIN
        ;		!layercache.parameter[3] = event.index
        ;	END
        ;
        ;	'NUMBYF':BEGIN
        ;		!layercache.cdec = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbam'), SENSITIVE = event.index NE 4
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbas'), SENSITIVE = event.index NE 4
        ;	END
        ;
        ;	'NUMBDG':BEGIN
        ;		!layercache.parameter[4] = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbam'), SENSITIVE = event.index EQ 0 AND !layercache.cra NE 5
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbas'), SENSITIVE = event.index EQ 0 AND !layercache.cra NE 5
        ;	END
        ;
        ;	'NUMBAM':BEGIN
        ;		!layercache.parameter[5] = event.index
        ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbas'), SENSITIVE = event.index EQ 0
        ;	END
        ;
        ;	'NUMBAS':BEGIN
        ;		!layercache.parameter[6] = event.index
        ;	END
        
        'PICKFILE':BEGIN
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'afileshow'), GET_VALUE = afile
            apath = file_dirname(afile)
            afile = dialog_pickfile(TITLE = 'Output To File', DEFAULT_EXTENSION = 'ps', $
                FILTER = [['*.ps', '*.*'], ['PostScript (*.ps)', 'All Files (*.*)']], $
                PATH = apath)
            IF afile NE '' THEN WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'afileshow'), SET_VALUE = afile
        END
        
        'DONE':BEGIN
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'apaper'), GET_VALUE = temp
            ;		!layercache.type = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'apaper'), /DROPLIST_SELECT)]
            
            !layercache.type = widget_info(widget_info(event.top, FIND_BY_UNAME = 'apaper'), /COMBOBOX_GETTEXT)
            
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'aquality'), GET_VALUE = temp
            temp = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'aquality'), /DROPLIST_SELECT)]
            !layercache.dsmooth = fix(temp)
            
            !layercache.afont = 'X X '+font_set(event.top, 'anumbfont') $
                +' '+font_set(event.top, 'alabefont') $
                +' '+font_set(event.top, 'atitlfont')
            
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbsize'), GET_VALUE = temp
            !layercache.asize = '0 0 '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'alabesize'), GET_VALUE = temp
            !layercache.asize += ' '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'atitlsize'), GET_VALUE = temp
            !layercache.asize += ' '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'alabex'), GET_VALUE = temp
            !layercache.texti = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'alabey'), GET_VALUE = temp
            !layercache.texto = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'atitle'), GET_VALUE = temp
            !layercache.pct = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'axtickgap'), GET_VALUE = temp
            !layercache.Bval[0] = abs(float(temp))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'aytickgap'), GET_VALUE = temp
            !layercache.Bval[1] = abs(float(temp))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'axminor'), GET_VALUE = temp
            !layercache.Bval[2] = (temp EQ '')?-1:abs(fix(temp))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'ayminor'), GET_VALUE = temp
            !layercache.Bval[3] = (temp EQ '')?-1:abs(fix(temp))
            !layercache.parameter[0] = widget_info(widget_info(event.top, FIND_BY_UNAME = 'aextend'), /BUTTON_SET)
            
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'alinethick'), GET_VALUE = temp
            !layercache.athick = string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'agridthick'), GET_VALUE = temp
            !layercache.athick += ' '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbthick'), GET_VALUE = temp
            !layercache.athick += ' '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'alabethick'), GET_VALUE = temp
            !layercache.athick += ' '+string(abs(float(temp)))
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'atitlthick'), GET_VALUE = temp
            !layercache.athick += ' '+string(abs(float(temp)))
            
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbxf'), GET_VALUE = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'anumbyf'), GET_VALUE = temp1
            !layercache.aorient = temp+'%'+temp1
            
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'afileshow'), GET_VALUE = temp
            !layercache.file[1] = temp
            ;		WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'apaper'), GET_VALUE = droplistvalue
            ;		!layercache.type = droplistvalue[widget_info(widget_info(event.top, FIND_BY_UNAME = 'apaper'), /DROPLIST_SELECT)]
            
            currct = intarr(!d.table_size,3)
            TVLCT, currct, /GET
            !layercache.ct = currct
            
            !layercache.valid = 1
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'CANCEL':BEGIN
            !layercache.valid = 0
            WIDGET_CONTROL, event.top, /DESTROY
        END
        
        'HELP':BEGIN
            XDISPLAYFILE, 'Custom Help', /MODAL, GROUP = event.top, TITLE='Custom Help', DONE_BUTTON = 'Close', $
                TEXT = ['What is this?', $
                'Modify the performance of result,', $
                'Adjust the axis parameter and output file name.', $
                'If you do not need a element, set its size or thick to 0.']
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
    ENDCASE
END
;--------------------------------------------------------------------------------------------------



PRO AICer_custom, leader
    
    ;Creat or edit a image layer(GUI).
    
    abase = widget_base(TITLE = 'Custom', /MODAL, GROUP_LEADER = leader, TLB_FRAME_ATTR = 0, COLUMN = 1)
    
    ;custom paper widget
    ambase0 = module_frame(abase, TITLE = 'Output')
    
    ambase1 = widget_base(ambase0, ROW = 1)
    apickfile = widget_button(ambase1, VALUE = 'File', UVALUE = 'PICKFILE', UNAME = 'apickfile')
    labeltext = strtrim(!layercache.file[1], 2) EQ ''? 'AICer.ps':!layercache.file[1]
    afileshow = widget_text(ambase1, VALUE = labeltext, UVALUE = 'FILESHOW', UNAME = 'afileshow', /EDITABLE, XSIZE = 20)
    ambase1 = widget_base(ambase0, ROW = 1)
    apapert = widget_label(ambase1, VALUE = 'Paper size ')
    apaper = widget_combobox(ambase1, UVALUE = 'PAPER', UNAME = 'apaper', /editable, $
        VALUE = ['A4','B5','20*20','10*10'])
    apapert = widget_label(ambase1, VALUE = 'cm')
    apapert = widget_label(ambase0, VALUE = '(e.g., A3, B4, 10*20, 15*22.)')
    aquality = widget_droplist(ambase0, UVALUE = 'QUALITY', UNAME = 'aquality', TITLE = 'Image quality ', $
        VALUE = ['5','12','24','40','75','120','240'])
    
    ;custom axis widget
    ambase0 = module_frame(abase, TITLE = 'Axis')
    
    aatab = widget_tab(ambase0, UVALUE = '', SCR_XSIZE = !x_width-10, /ALIGN_CENTER, MULTILINE = 2)
    afont = strsplit(!layercache.afont+' !3 !3 !3 !3 !3', /EXTRACT)
    asize = strtrim(string(float((strsplit(!layercache.asize+' 0 0 0 0 0', /EXTRACT))), FORMAT = '(F6.2)'),2)
    athick = strtrim(string(float((strsplit(!layercache.athick+' 0 0 0 0 0', /EXTRACT))), FORMAT = '(F6.2)'),2)
    
    aabase1 = widget_base(aatab, TITLE = 'Line', COLUMN = 1, /BASE_ALIGN_LEFT)
    alinethick = cw_field(aabase1, VALUE = athick[0], TITLE = 'Thick ', UVALUE = 'LINETHICK', UNAME = 'alinethick', XSIZE = 8)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;alinethickt = widget_label(aabase2, VALUE = 'Thick ')
    ;alinethick = widget_text(aabase2, VALUE = athick[0], UVALUE = 'LINETHICK', UNAME = 'alinethick', /EDITABLE, XSIZE = 8)
    aabase2 = widget_base(aabase1, ROW = 1)
    alinecolort = widget_label(aabase2, VALUE = 'Color ')
    alinecolor = widget_draw(aabase2, UVALUE = 'LINECOLOR', UNAME = 'alinecolor', BUTTON_EVENT = 'AICer_custom_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    aabase2 = widget_base(aabase1, ROW = 1)
    aabase3 = widget_base(aabase2, COLUMN = 1)
    atickgapt = widget_label(aabase3, VALUE = ' ')
    atickgapt = widget_text(aabase3, VALUE = 'Axis X', XSIZE = 6)
    atickgapt = widget_text(aabase3, VALUE = 'Axis Y', XSIZE = 6)
    aabase3 = widget_base(aabase2, COLUMN = 1)
    atickgapt = widget_label(aabase3, VALUE = 'Tick Gap(deg)')
    IF !layercache.Bval[0] EQ 0 THEN labeltext = '' ELSE labeltext = string(!layercache.Bval[0], FORMAT = '(G0)')
    axtickgap = widget_text(aabase3, VALUE = labeltext, UVALUE = 'XTICKGAP', UNAME = 'axtickgap', /EDITABLE, XSIZE = 12)
    IF !layercache.Bval[1] EQ 0 THEN labeltext = '' ELSE labeltext = string(!layercache.Bval[1], FORMAT = '(G0)')
    aytickgap = widget_text(aabase3, VALUE = labeltext, UVALUE = 'XTICKGAP', UNAME = 'aytickgap', /EDITABLE, XSIZE = 12)
    aabase3 = widget_base(aabase2, COLUMN = 1)
    atickgapt = widget_label(aabase3, VALUE = 'Minor')
    IF !layercache.Bval[2] EQ -1 THEN labeltext = '' ELSE labeltext = string(!layercache.Bval[2], FORMAT = '(I0)')
    axminor = widget_text(aabase3, VALUE = labeltext, UVALUE = 'XMINOR', UNAME = 'axminor', /EDITABLE, XSIZE = 4)
    IF !layercache.Bval[3] EQ -1 THEN labeltext = '' ELSE labeltext = string(!layercache.Bval[3], FORMAT = '(I0)')
    ayminor = widget_text(aabase3, VALUE = labeltext, UVALUE = 'YMINOR', UNAME = 'ayminor', /EDITABLE, XSIZE = 4)
    aabase2 = widget_base(aabase1, COLUMN = 1, /NONEXCLUSIVE)
    aextend = widget_button(aabase2, VALUE = 'Extend axis range', UVALUE = 'EXTEND', UNAME = 'aextend')
    
    aabase1 = widget_base(aatab, TITLE = 'Grid', COLUMN = 1, /BASE_ALIGN_LEFT)
    agridthick = cw_field(aabase1, VALUE = athick[1], TITLE = 'Thick ', UVALUE = 'GRIDTHICK', UNAME = 'agridthick', XSIZE = 8)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;agridthickt = widget_label(aabase2, VALUE = 'Thick ')
    ;agridthick = widget_text(aabase2, VALUE = athick[1], UVALUE = 'GRIDTHICK', UNAME = 'agridthick', /EDITABLE, XSIZE = 8)
    aabase2 = widget_base(aabase1, ROW = 1)
    agridcolort = widget_label(aabase2, VALUE = 'Color ')
    agridcolor = widget_draw(aabase2, UVALUE = 'GRIDCOLOR', UNAME = 'agridcolor', BUTTON_EVENT = 'AICer_custom_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    
    aabase1 = widget_base(aatab, TITLE = 'Number', COLUMN = 1, /BASE_ALIGN_LEFT)
    anumbfont = module_font(aabase1, UVALUE = 'NUMBFONT', UNAME = 'anumbfont', SELECTFONT = afont[2])
    anumbsize = cw_field(aabase1, VALUE = asize[2], TITLE = 'Size  ', UVALUE = 'NUMBSIZE', UNAME = 'anumbsize', XSIZE = 8)
    anumbthick = cw_field(aabase1, VALUE = athick[2], TITLE = 'Thick ', UVALUE = 'NUMBTHICK', UNAME = 'anumbthick', XSIZE = 8)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;anumbsizet = widget_label(aabase2, VALUE = 'Size  ')
    ;anumbsize = widget_text(aabase2, VALUE = asize[2], UVALUE = 'NUMBSIZE', UNAME = 'anumbsize', /EDITABLE, XSIZE = 8)
    ;anumbthickt = widget_label(aabase2, VALUE = '   Thick ')
    ;anumbthick = widget_text(aabase2, VALUE = athick[2], UVALUE = 'NUMBTHICK', UNAME = 'anumbthick', /EDITABLE, XSIZE = 8)
    aabase2 = widget_base(aabase1, ROW = 1)
    anumbcolort = widget_label(aabase2, VALUE = 'Color ')
    anumbcolor = widget_draw(aabase2, UVALUE = 'NUMBCOLOR', UNAME = 'anumbcolor', BUTTON_EVENT = 'AICer_custom_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    
    labeltext = strsplit(' '+!layercache.aorient+' ', '%', /EXTRACT)
    labeltext = strtrim(([labeltext,''])[0:1], 2)
    IF labeltext[0] eq '' THEN labeltext[0] = '[I0]!Uh!N[I0]!Um!N[F0.1]!Us!N'
    IF labeltext[1] eq '' THEN labeltext[1] = "[I02]!Uo!N[I02]'"+'[F04.1]"'
    
    aabase2 = widget_base(aabase1, ROW = 1)
    anumbft = widget_label(aabase1, VALUE = 'Format')
    anumbxf = cw_field(aabase1, VALUE = labeltext[0], TITLE = 'X ', UVALUE = 'NUMBXF', UNAME = 'anumbxf', XSIZE = 32)
    anumbyf = cw_field(aabase1, VALUE = labeltext[1], TITLE = 'Y ', UVALUE = 'NUMBYF', UNAME = 'anumbyf', XSIZE = 32)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;anumbxft = widget_label(aabase2, VALUE = 'X ')
    ;anumbxf = widget_text(aabase2, VALUE = labeltext[0], UVALUE = 'NUMBXF', UNAME = 'anumbxf', /EDITABLE, XSIZE = 32)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;anumbyft = widget_label(aabase2, VALUE = 'Y ')
    ;anumbyf = widget_text(aabase2, VALUE = labeltext[1], UVALUE = 'NUMBYF', UNAME = 'anumbyf', /EDITABLE, XSIZE = 32)
    ;aabase2 = widget_base(aabase1, /ALIGN_CENTER)
    ;anumbeg = widget_draw(aabase1, UVALUE = 'NUMBEG', UNAME = 'anumbeg', SCR_XSIZE = !x_width-80, SCR_YSIZE = 24)
    
    aabase1 = widget_base(aatab, TITLE = 'Label', COLUMN = 1, /BASE_ALIGN_LEFT)
    alabex = cw_field(aabase1, VALUE = !layercache.texti, TITLE = 'X label ', UVALUE = 'LABEX', UNAME = 'alabex')
    alabey = cw_field(aabase1, VALUE = !layercache.texto, TITLE = 'Y label ', UVALUE = 'LABEY', UNAME = 'alabey')
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;alabet = widget_label(aabase2, VALUE = 'Xlabel ')
    ;alabex = widget_text(aabase2, VALUE = !layercache.texti, UVALUE = 'LABEX', UNAME = 'alabex', /EDITABLE)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;alabet = widget_label(aabase2, VALUE = 'Ylabel ')
    ;alabey = widget_text(aabase2, VALUE = !layercache.texto, UVALUE = 'LABEY', UNAME = 'alabey', /EDITABLE)
    alabefont = module_font(aabase1, UVALUE = 'LABEFONT', UNAME = 'alabefont', SELECTFONT = afont[3])
    
    alabesize = cw_field(aabase1, VALUE = asize[3], TITLE = 'Size  ', UVALUE = 'LABESIZE', UNAME = 'alabesize', XSIZE = 8)
    alabethick = cw_field(aabase1, VALUE = athick[3], TITLE = 'Thick ', UVALUE = 'LABETHICK', UNAME = 'alabethick', XSIZE = 8)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;alabesizet = widget_label(aabase2, VALUE = 'Size  ')
    ;alabesize = widget_text(aabase2, VALUE = asize[3], UVALUE = 'LABESIZE', UNAME = 'alabesize', /EDITABLE, XSIZE = 8)
    ;alabethickt = widget_label(aabase2, VALUE = '   Thick ')
    ;alabethick = widget_text(aabase2, VALUE = athick[3], UVALUE = 'LABETHICK', UNAME = 'alabethick', /EDITABLE, XSIZE = 8)
    aabase2 = widget_base(aabase1, ROW = 1)
    alabecolort = widget_label(aabase2, VALUE = 'Color ')
    alabecolor = widget_draw(aabase2, UVALUE = 'LABECOLOR', UNAME = 'alabecolor', BUTTON_EVENT = 'AICer_custom_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    
    aabase1 = widget_base(aatab, TITLE = 'Title', COLUMN = 1, /BASE_ALIGN_LEFT)
    
    atitle = cw_field(aabase1, VALUE = !layercache.pct, TITLE = 'Title ', UVALUE = 'TITLE', UNAME = 'atitle')
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;atitlt = widget_label(aabase2, VALUE = 'Title ')
    ;atitle = widget_text(aabase2, VALUE = !layercache.pct, UVALUE = 'TITLE', UNAME = 'atitle', /EDITABLE)
    atitlfont = module_font(aabase1, UVALUE = 'TITLFONT', UNAME = 'atitlfont', SELECTFONT = afont[4])
    
    
    atitlsize = cw_field(aabase1, VALUE = asize[4], TITLE = 'Size  ', UVALUE = 'TITLSIZE', UNAME = 'atitlsize', XSIZE = 8)
    atitlthick = cw_field(aabase1, VALUE = athick[4], TITLE = 'Thick ', UVALUE = 'TITLTHICK', UNAME = 'atitlthick', XSIZE = 8)
    ;aabase2 = widget_base(aabase1, ROW = 1)
    ;atitlsizet = widget_label(aabase2, VALUE = 'Size  ')
    ;atitlsize = widget_text(aabase2, VALUE = asize[4], UVALUE = 'TITLSIZE', UNAME = 'atitlsize', /EDITABLE, XSIZE = 8)
    ;atitlthickt = widget_label(aabase2, VALUE = '   Thick ')
    ;atitlthick = widget_text(aabase2, VALUE = athick[4], UVALUE = 'TITLTHICK', UNAME = 'atitlthick', /EDITABLE, XSIZE = 8)
    aabase2 = widget_base(aabase1, ROW = 1)
    atitlcolort = widget_label(aabase2, VALUE = 'Color ')
    atitlcolor = widget_draw(aabase2, UVALUE = 'TITLCOLOR', UNAME = 'atitlcolor', BUTTON_EVENT = 'AICer_custom_event', $
        SCR_XSIZE = 50, SCR_YSIZE = 24)
    
    ;exit button widget
    ambase0 = module_exit(abase)
    
    wgeom = widget_info(leader, /GEOMETRY)
    WIDGET_CONTROL, abase, XOFFSET = wgeom.xoffset+160, YOFFSET = wgeom.yoffset+15
    WIDGET_CONTROL, abase, /REALIZE
    
    ;widget initiation
    WIDGET_CONTROL, apaper, GET_VALUE = temp
    select = (where(strcmp(!layercache.type, temp, /FOLD_CASE)))[0]
    IF select EQ -1 AND !layercache.type NE '' THEN BEGIN
        WIDGET_CONTROL, apaper, COMBOBOX_ADDITEM = !layercache.type
        WIDGET_CONTROL, apaper, SET_COMBOBOX_SELECT = n_elements(temp)
    ENDIF ELSE WIDGET_CONTROL, apaper, SET_COMBOBOX_SELECT = select
    
    WIDGET_CONTROL, aquality, GET_VALUE = temp
    select = (where(strcmp(string(!layercache.dsmooth, FORMAT = '(I0)'), temp, /FOLD_CASE)))[0]
    IF select EQ -1 THEN select = 0
    WIDGET_CONTROL, aquality, SET_DROPLIST_SELECT = select
    
    ;SET_PLOT, 'win'
    ;DEVICE, DECOMPOSED = 0
    TVLCT, !layercache.ct
    
    WIDGET_CONTROL, alinecolor, GET_VALUE = temp
    WSET, temp
    ERASE, 0
    
    WIDGET_CONTROL, agridcolor, GET_VALUE = temp
    WSET, temp
    ERASE, 1
    
    WIDGET_CONTROL, anumbcolor, GET_VALUE = temp
    WSET, temp
    ERASE, 2
    
    WIDGET_CONTROL, alabecolor, GET_VALUE = temp
    WSET, temp
    ERASE, 3
    
    WIDGET_CONTROL, atitlcolor, GET_VALUE = temp
    WSET, temp
    ERASE, 4
    
    WIDGET_CONTROL, aextend, SET_BUTTON = !layercache.parameter[0]
    
    ;WIDGET_CONTROL, anumbxf, SET_DROPLIST_SELECT = !layercache.cra
    ;WIDGET_CONTROL, anumbhr, SET_COMBOBOX_SELECT = !layercache.parameter[1]
    ;WIDGET_CONTROL, anumbmn, SET_COMBOBOX_SELECT = !layercache.parameter[2], SENSITIVE = !layercache.cra NE 5 AND !layercache.parameter[1] EQ 0
    ;WIDGET_CONTROL, anumbsc, SET_COMBOBOX_SELECT = !layercache.parameter[3], SENSITIVE = !layercache.cra NE 5 AND !layercache.parameter[1] EQ 0 AND !layercache.parameter[2] EQ 0
    ;WIDGET_CONTROL, anumbyf, SET_DROPLIST_SELECT = !layercache.cdec
    ;WIDGET_CONTROL, anumbdg, SET_COMBOBOX_SELECT = !layercache.parameter[4]
    ;WIDGET_CONTROL, anumbam, SET_COMBOBOX_SELECT = !layercache.parameter[5], SENSITIVE = !layercache.cdec NE 4 AND !layercache.parameter[4] EQ 0
    ;WIDGET_CONTROL, anumbas, SET_COMBOBOX_SELECT = !layercache.parameter[6], SENSITIVE = !layercache.cdec NE 4 AND !layercache.parameter[4] EQ 0 AND !layercache.parameter[5] EQ 0
    
    XMANAGER, 'AICer_custom', abase
    
END
;--------------------------------------------------------------------------------------------------




PRO FITS_CONVERT, fitsfilename, image, range, wcs, $
        NRA = nra, NDEC = ndec, XGRID = xgrid, YGRID = ygrid, ERROR = error, INTERP = interp;, NAN = nan
    
    ;convert corrdinate of fits data
    ;error: error in read fits file
    ; = 0:successful
    ; = 1:file does not exist
    ; = 2:invalid fits file
    ;	;nan: way to deal with NaN values
    ;	; = 0 not change
    ;	; = 1 max
    ;	; = 2 min
    ;	; = 3 (max+min)/2
    
    IF ~ keyword_set(nra) THEN nra = 300
    IF ~ keyword_set(ndec) THEN ndec = 300
    IF ~ keyword_set(interp) THEN interp = 0
    
    IF ~ file_test(fitsfilename) THEN BEGIN
        error = 1 & RETURN
    END
    ;IF ~ keyword_set(nan) THEN nan = 0
    
    FITS_OPEN, fitsfilename, fcb, message = mess
    IF mess NE '' THEN BEGIN
        error = 2 & RETURN
    END
    FITS_READ, fcb, data, header
    FITS_CLOSE, fcb
    IF wcs EQ 2 THEN SXADDPAR,header,'CTYPE1',strmid(sxpar(header,'CTYPE1'),0,3)
    IF wcs EQ 2 THEN SXADDPAR,header,'CTYPE2',strmid(sxpar(header,'CTYPE2'),0,3)
    EXTAST, header, astr, noparams, ALT = alt
    
    ;for fits generated by GILDAS, no need for astron after Jul,2013
    IF strmid(astr.ctype[0],5,3) EQ 'GLS' THEN astr.ctype[0] = strmid(astr.ctype[0],0,5)+'SFL'
    IF strmid(astr.ctype[1],5,3) EQ 'GLS' THEN astr.ctype[1] = strmid(astr.ctype[1],0,5)+'SFL'
    
    fits_sz = size(data, /DIMENSIONS)
    fits_wcs = strcmp(astr.ctype[0],'GLON',4) and strcmp(astr.ctype[1],'GLAT',4)
    
    axis_x = dindgen(nra+1) * (range[1] - range[0]) / nra + range[0]
    axis_y = dindgen(ndec+1) * (range[3] - range[2]) / ndec + range[2]
    wcs_x = axis_x # replicate(1d,ndec+1)
    wcs_y = replicate(1d,nra+1) # axis_y
    xgrid = wcs_x
    ygrid = wcs_y
    IF wcs EQ 0 AND fits_wcs EQ 1 THEN BEGIN
        GLACTC, wcs_x, wcs_y, 2000, twcs_x, twcs_y, 1, /DEGREE
        wcs_x = twcs_x
        wcs_y = twcs_y
    ENDIF
    IF wcs EQ 1 AND fits_wcs EQ 0 THEN BEGIN
        GLACTC, twcs_x, twcs_y, 2000, wcs_x, wcs_y, 2, /DEGREE
        wcs_x = twcs_x
        wcs_y = twcs_y
    ENDIF
    IF get_equinox(header) EQ 1950 THEN BPRECESS, wcs_x, wcs_y, wcs_x, wcs_y
    
    AD2XY, wcs_x, wcs_y, astr, grid_x, grid_y
    ;grid_x = grid_x mod abs(360d/astr.cdelt[0]/astr.cd[0])
    ;grid_y = grid_y mod abs(360d/astr.cdelt[1]/astr.cd[3])
    image = dblarr(nra+1, ndec+1)
    image[*] = 0
    
    ;data_max = max(data[where(finite(data))])
    IF interp THEN BEGIN
        ;for contour
        data_ex = where((grid_x LT 0) OR (grid_x GT fits_sz[0]-1) OR (grid_y LT 0) OR (grid_y GT fits_sz[1]-1), COMPLEMENT = data_in)
        image = interpolate(data, grid_x, grid_y)
        IF data_ex[0] NE -1 THEN image[data_ex] = !values.d_nan;value_ex
    ENDIF ELSE BEGIN
        ;for image
        data_ex = where((grid_x LE -0.5) OR (grid_x GE fits_sz[0]-0.5) OR (grid_y LE -0.5) OR (grid_y GE fits_sz[1]-0.5), COMPLEMENT = data_in)
        IF data_in[0] NE -1 THEN BEGIN
            image[data_in] = data[round(grid_x[data_in]), round(grid_y[data_in])]
        ENDIF
        IF data_ex[0] NE -1 THEN image[data_ex] = !values.d_nan;value_ex
    ENDELSE
    
    error=0
    RETURN
END
;--------------------------------------------------------------------------------------------------



PRO ROTATEELLIPSE, dmaj, dmin, angle, dec
    
    ;get parameter of ellipse for shape layer
    
    ar = angle *!DTOR
    dr = dec *!DTOR
    AAA = (cos(ar)*cos(dr)/dmaj)^2 + (sin(ar)*cos(dr)/dmin)^2
    BBB = (1/dmaj^2-1/dmin^2)*sin(2*ar)*cos(dr)
    CCC = (sin(ar)/dmaj)^2 + (cos(ar)/dmin)^2
    ;ellipse is r^2*(AAA*cos(theta)^2+BBB*cos(theta)*sin(theta)+CCC*sin(theta)^2)=1
    IF BBB NE 0 THEN phi = atan((CCC-AAA)/BBB)/2+!dpi/4 ELSE phi = !DPI/2
    angle = phi /!DTOR
    dmaj = 1/sqrt(AAA*cos(phi)^2+BBB*cos(phi)*sin(phi)+CCC*sin(phi)^2)
    dmin = 1/sqrt(AAA*cos(phi+!dpi/2)^2+BBB*cos(phi+!dpi/2)*sin(phi+!dpi/2)+CCC*sin(phi+!dpi/2)^2)
END
;--------------------------------------------------------------------------------------------------



PRO AICer_main, layerlist, singlepanel=singlepanel, pageoffset=pageoffset, pagescale=pagescale
    
    ;main procedure of AICer, draw layers in the list onto a PS file
    ;Inputs
    ;layerlist: list of layer structure, could be restored from a .aic file.
    ;Keywords
    ;singlepanel: a scalar, set 1 to draw a plot with single panel.
    ;pageoffset: 2 elements array, offset of the lower left corner of the page. 
    ;pagescale: a scalar to scale the size of page.
    
    if n_params() lt 1 then begin
        print,'Syntax - AICer_main, layerlist, [singlepanel=, pageoffset=, pagescale=]'
        print,'The layerlist must be an AICer strucutre'
        return
    endif
    if ~n_elements(singlepanel) then singlepanel=1
    if ~keyword_set(pageoffset) or n_elements(pageoffset) ne 2 then pageoffset=[0,0]
    if n_elements(pagescale) ne 1 then pagescale=1d
    if pagescale eq 0 then pagescale=1d

    ;extract the template layer
    layer = layerlist[n_elements(layerlist)-1]
    
    ;get sourcename
    source = (strsplit(layer.uname, /EXTRACT))[0]
    
    ;get coordinate system
    wcs = layer.parameter[1]
    
    ;calculate plot range
    RAcen = str2ang(layer.ra, /RA)
    DECcen = str2ang(layer.dec, /DEC)
    
    RAoff = double([layer.raof, layer.raot])
    CASE layer.rau OF
        'arcsec':RAoff = RAoff/3600.
        'arcmin':RAoff = RAoff/60.
        ELSE:
    ENDCASE
    IF RAoff[0] EQ RAoff[1] THEN RAoff = [0d,360d]
    IF wcs EQ 2 THEN RArange = RAcen + RAoff ELSE BEGIN
	RAoff = RAoff[reverse(sort(RAoff))]
	RArange = RAcen+RAoff/cos(DECcen*!DPI/180.)
    ENDELSE
    
    DECoff = double([layer.decof, layer.decot])
    CASE layer.decu OF
        'arcsec':DECoff = DECoff/3600.
        'arcmin':DECoff = DECoff/60.
        ELSE:
    ENDCASE
    IF DECoff[0] EQ DECoff[1] THEN DECoff = [-90d,90d]
    IF wcs NE 2 THEN DECoff = DECoff[sort(DECoff)]
    DECrange = DECcen+DECoff
    plotrange = [RArange, DECrange]
    
    ;size of paper
    paper = layer.type
    CASE paper OF
        'A0':pagesize = [84.1, 118.9]
        'A1':pagesize = [59.4, 84.1]
        'A2':pagesize = [42.0, 59.4]
        'A3':pagesize = [29.7, 42.0]
        'A4':pagesize = [21.0, 29.7]
        'A5':pagesize = [14.8, 21.0]
        'A6':pagesize = [10.5, 14.8]
        'A7':pagesize = [7.4, 10.5]
        'A8':pagesize = [5.2, 7.4]
        'B0':pagesize = [103.0, 145.6]
        'B1':pagesize = [72.8, 103.0]
        'B2':pagesize = [51.5, 72.8]
        'B3':pagesize = [36.4, 51.5]
        'B4':pagesize = [25.7, 36.4]
        'B5':pagesize = [18.2, 25.7]
        'B6':pagesize = [12.8, 18.2]
        'B7':pagesize = [9.1, 12.8]
        'B8':pagesize = [6.4, 9.1]
        '10*10':pagesize = [10, 10]
        '20*20':pagesize = [20, 20]
        ELSE:BEGIN
            pagesize = [float(strsplit(paper,'*',/EXTRACT)), 0]
            IF pagesize[0] GT 0 AND pagesize[1] GT 0 THEN BEGIN
                pagesize = pagesize[[0,1]]
            ENDIF ELSE BEGIN
                PRINT, 'Warning - Unrecognizable paper size, using default: A4'
                pagesize = [21.0, 29.7]
            ENDELSE
        END
    ENDCASE
    pagesize *= pagescale

    ;orientation of paper
    orientation = 'portrait'
    IF orientation EQ 'landscape' THEN BEGIN
        orientation = 1
        pagesize = reverse(pagesize)
    ENDIF ELSE orientation = 0

    savescreen = !d.name
    ;CATCH, error
    ;IF error NE 0 THEN BEGIN
    ;	print,error
    ;	SET_PLOT, savescreen
    ;	CATCH, /CANCEL
    ;	RETURN
    ;ENDIF
    ;if not multi-panel plot, define the page.
    IF singlepanel THEN BEGIN
        SET_PLOT, 'ps'
        DEVICE, /CLOSE
        outputfile = strtrim(layer.file[1], 2) EQ ''? 'AICer.ps':layer.file[1]
        DEVICE, FILENAME = outputfile, BITS_PER_PIXEL = 8, /COLOR, /ENCAPSULATED, $
            XSIZE = pagesize[0], YSIZE = pagesize[1], LANDSCAPE = orientation
    ENDIF
    
    ;character set
    axisfont = strsplit(layer.afont+' !3 !3 !3 !3 !3', /EXTRACT)	;line(X), grid(X), number, label, title
    axissize = float(strsplit(layer.asize+' 0 0 0 0 0', /EXTRACT)) *pagescale	;line, grid, number, label, title
    axisthick = float(strsplit(layer.athick+' 0 0 0 0 0', /EXTRACT)) *pagescale	;line, grid, number, label, title
    ;if all layers are panel, do not plot axis, only plot panels
    npanel = total(layerlist.name EQ 'PANEL')	;total number of panel layers
    nlayer = n_elements(layerlist)-1		;total number of plot layers
    IF npanel GT 0 AND npanel EQ nlayer THEN axisthick[*] = 0
    ;axisRAformat = strsplit(layer.ra+' * * * * * *', /EXTRACT)
    ;axisRAformat[0] = validformat(axisRAformat[0])?axisRAformat[0]:'I0'
    ;axisRAformat[1] = (axisRAformat[1] EQ ':')?':':'!Uh!N'
    ;axisRAformat[2] = validformat(axisRAformat[2])?axisRAformat[2]:'I0'
    ;axisRAformat[3] = (axisRAformat[1] EQ ':')?':':'!Um!N'
    ;axisRAformat[4] = validformat(axisRAformat[4])?axisRAformat[4]:'F0.1'
    ;axisRAformat[5] = (axisRAformat[1] EQ ':')?'':'!Us!N'
    ;axisDECformat = strsplit(layer.dec+' * * * * * *', /EXTRACT)
    ;axisDECformat[0] = validformat(axisDECformat[0])?axisDECformat[0]:'I02'
    ;axisDECformat[1] = (axisDECformat[1] EQ ':')?':':'!Uo!N'
    ;axisDECformat[2] = validformat(axisDECformat[2])?axisDECformat[2]:'I02'
    ;axisDECformat[3] = (axisDECformat[1] EQ ':')?':':''''
    ;axisDECformat[4] = validformat(axisDECformat[4])?axisDECformat[4]:'F04.1'
    ;axisDECformat[5] = (axisDECformat[1] EQ ':')?'':'"'
    xlabel = axisfont[3]+textconvert(layer.texti, SOURCE = source, RA = layer.ra, DEC = layer.dec)+'!X'
    ylabel = axisfont[3]+textconvert(layer.texto, SOURCE = source, RA = layer.ra, DEC = layer.dec)+'!X'
    axistitle = axisfont[4]+textconvert(layer.pct, SOURCE = source, RA = layer.ra, DEC = layer.dec)+'!X'
    
    ;tickvalue (number) format
    tickformat = ([strsplit(' '+layer.aorient+' ', '%', /EXTRACT),''])[0:1]
    RAformat = deform(tickformat[0])
    IF RAformat[0] EQ 'error' THEN BEGIN
        PRINT, 'Warning - Wrong format for RA tick number, use default'
        RAformat = deform('[I0]!Uh!N[I0]!Um!N[F0.1]!Us!N')
    ENDIF
    RAticktype = n_elements(RAformat)/2
    DECformat = deform(tickformat[1])
    IF DECformat[0] EQ 'error' THEN BEGIN
        PRINT, 'Warning - Wrong format for DEC tick number, use default'
        DECformat = deform("[I02]!Uo!N[I02]'"+'[F04.1]"')
    ENDIF
    IF n_elements(DECformat) EQ 1 THEN DECformat = [DECformat, '']
    DECticktype = n_elements(DECformat)/2
    
    ;decide the margin around plot
    incharsize = [0.1736, 0.2546]	;single character size in centimeter
    wordspace = 0.25
    linespace = 1.0
    ;digital number of dec nicknames
    DEClen = (DECrange[0] GE 0?0:1)	;sign
    for i=0,DECticktype-1 do DEClen += (DECformat[i*2] ne '')*strlen(string(0,format = DECformat[i*2]))+1
    ;DEClen = 11+(DECrange[0] GE 0?0:1)	;strlen(string(abs(DECrange[0]), FORMAT = '(I02)'))+(DECrange[0] GE 0?0:1)+9
    ;whether draw a color bar
    colorbar = total((layerlist.valid eq 1) and (layerlist.name eq 'IMAGE') and layerlist.parameter[7]) ge 1
    margin_extend = 0.02*[pagesize[0], pagesize[1]]*layer.parameter[0]
    margin_left = pagesize[1]*0.01+margin_extend[0]+incharsize[0]*(DEClen+(DEClen+1)*wordspace)*axissize[2]+incharsize[1]*(1+linespace)*axissize[3]
    margin_right = pagesize[0]*0.03+margin_extend[0]+incharsize[0]*(8+8*wordspace)*axissize[2]*colorbar
    margin_top = pagesize[1]*0.01+margin_extend[1]+incharsize[1]*(1+2*linespace)*axissize[4]*1.25
    margin_bottom = pagesize[1]*0.01+margin_extend[1]+incharsize[1]*(1+2*linespace)*axissize[2]+incharsize[1]*(1+linespace)*axissize[3]
    ;add offset to the page, draw multi-panel plot
    if ~singlepanel then begin
        margin_left += pageoffset[0]
        margin_right += !d.x_size/1000.-pagesize[0]-pageoffset[0]
        margin_top += !d.y_size/1000.-pagesize[1]-pageoffset[1]
        margin_bottom += pageoffset[1]
    endif

    ;calculate position for plot
    IF wcs eq 2 THEN BEGIN
        xmin = margin_left/(!d.x_size/1000.)
        xmax = 1-margin_right/(!d.x_size/1000.)
        ymin = margin_bottom/(!d.y_size/1000.)
        ymax = 1-margin_top/(!d.y_size/1000.)
    ENDIF ELSE BEGIN
        plotratio = abs((RAoff[0]-RAoff[1])/(DECoff[1]-DECoff[0]))
        pageratio = (!d.x_size/1000.-margin_left-margin_right)/(!d.y_size/1000.-margin_top-margin_bottom)
        IF plotratio GT pageratio THEN BEGIN
            xmin = margin_left/(!d.x_size/1000.)
            xmax = 1-margin_right/(!d.x_size/1000.)
            ymin = margin_bottom/(!d.y_size/1000.)
            ymax = ymin+(!d.x_size/1000.-margin_left-margin_right)/plotratio/(!d.y_size/1000.)
        ENDIF ELSE BEGIN
            xmax = 1-margin_right/(!d.x_size/1000.)
            xmin = xmax-(!d.y_size/1000.-margin_top-margin_bottom)*plotratio/(!d.x_size/1000.)
            ymin = margin_bottom/(!d.y_size/1000.)
            ymax = 1-margin_top/(!d.y_size/1000.)
        ENDELSE
    ENDELSE
    plotposition = [xmin, ymin, xmax, ymax]

    ;calculate position for axis, if axis is not extend, axis has same position as plot.
    axisextend = layer.parameter[0]	;whether to extend axis
    extendlen = 0.02 * min([!d.x_size/1000.*(xmax-xmin), !d.y_size/1000.*(ymax-ymin)]) / [!d.x_size/1000., !d.y_size/1000.]
    axisRArange = RArange+[-1.,+1.]*axisextend*extendlen[0]*(RArange[1]-RArange[0])/(xmax-xmin)
    axisDECrange = DECrange+[-1.,+1.]*axisextend*extendlen[1]*(DECrange[1]-DECrange[0])/(ymax-ymin)
    axisposition = [xmin-axisextend*extendlen[0], ymin-axisextend*extendlen[1], xmax+axisextend*extendlen[0], ymax+axisextend*extendlen[1]]

    ;gap and minor
    gapnum = 3.5d
    tickgap1 = [360d, 180d, 90d, 60d]
    tickmin1 = [   0,    4,   3,   3]
    tickgap2 = [30d, 20d, 15d, 10d, 5d, 4d, 3d, 2d, 1d]
    tickmin2 = [  6,   4,   5,   5,  5,  4,  6,  4,  4]
    tickgap3 = [5d, 3d, 2d, 1d]
    tickmin3 = [ 5,  3,  4,  4]
    
    ;calculate the best gap and minor between ticks
    axisRAgap = layer.bval[0]
    IF abs(axisRArange[0]-axisRArange[1]) GT layer.bval[0]*50 THEN BEGIN
        ;PRINT,'Warning - Too small X tick gap, use default'
        axisRAup = abs(axisRArange[0]-axisRArange[1]) / gapnum
        idx = (where(axisRAup GE tickgap1))[0]
        IF idx GE 0 THEN BEGIN
            axisRAgap = tickgap1[idx]
            axisRAminor = tickmin1[idx]
        ENDIF ELSE BEGIN
            multiply = [1d, 1/15d, 1/15d*60d, 1/15d*60d*60d]
            FOR i = (RAticktype GT 0), RAticktype DO BEGIN
                up = axisRAup*multiply[i]
                idx = (where(up GE tickgap2))[0]
                IF idx GE 0 THEN BEGIN
                    axisRAgap = tickgap2[idx]/multiply[i]
                    axisRAminor = tickmin2[idx]
                    BREAK
                ENDIF
            ENDFOR
            IF idx LT 0 THEN BEGIN
                up = axisRAup*multiply[RAticktype]
                mag = floor(alog10(up))
                idx = (where(up/(10d^mag) GE tickgap3))[0]
                axisRAgap = tickgap3[idx]*(10d^mag)/multiply[RAticktype]
                axisRAminor = tickmin3[idx]
            ENDIF
        ENDELSE
    ENDIF
    IF layer.bval[2] GT 0 THEN axisRAminor = layer.bval[2]
    
    axisDECgap = layer.bval[1]
    IF abs(axisDECrange[1]-axisDECrange[0]) GT layer.bval[1]*50 THEN BEGIN
        ;PRINT,'Warning - Too small Y tick gap, use default'
        axisDECup = abs(axisDECrange[0]-axisDECrange[1]) / gapnum
        idx = (where(axisDECup GE tickgap1))[0]
        IF idx GE 0 THEN BEGIN
            axisDECgap = tickgap1[idx]
            axisDECminor = tickmin1[idx]
        ENDIF ELSE BEGIN
            multiply = [0d, 1d, 60d, 60d*60d]
            FOR i = 1l, DECticktype DO BEGIN
                up = axisDECup*multiply[i]
                idx = (where(up GE tickgap2))[0]
                IF idx GE 0 THEN BEGIN
                    axisDECgap = tickgap2[idx]/multiply[i]
                    axisDECminor = tickmin2[idx]
                    BREAK
                ENDIF
            ENDFOR
            IF idx LT 0 THEN BEGIN
                up = axisDECup*multiply[DECticktype]
                mag = floor(alog10(up))
                idx = (where(up/(10d^mag) GE tickgap3))[0]
                axisDECgap = tickgap3[idx]*(10d^mag)/multiply[DECticktype]
                axisDECminor = tickmin3[idx]
            ENDIF
        ENDELSE
    ENDIF
    IF layer.bval[3] GT 0 THEN axisDECminor = layer.bval[3]
    
    ;tickv & ticks
    ;tickgap = [360d, 90d, 60d, 30d, 20d, 15d, 10d, 6d ,5d, 3d, 2d, 1d, 30/60d, 20/60d, 15/60d, 10/60d, 5/60d, 4/60d, 3/60d, 2/60d, 1/60d, $
    ;	30/3600d, 20/3600d, 15/3600d, 10/3600d, 5/3600d, 3/3600d, 2/3600d, 1/3600d, 5/36000d, 2/36000d, 1/36000d, 1/360000d, 1/3600000]
    ;tickminor = [ 0,   9,   6,   6,   4,   3,   5,  6,  5,  6,  6,  6,      6,      4,      3,      5,     5,     4,     6,     6,     6, $
    ;	       6,        4,        3,        5,       5,       6,       5,       5,        5,        5,        5,         5,         5]
    ;gapnum = 3.5
    ;
    ;axisRAgap = layer.bval[0]
    ;axisRAminor = layer.bval[2]
    ;narrowgap = layer.bval[0]*50 LT abs(axisRArange[0]-axisRArange[1])
    ;IF narrowgap THEN BEGIN
    ;	IF axisRAgap NE 0 THEN PRINT,'Warning - Too small X tick gap, use default'
    ;	axisRAgap = tickgap[(where((axisRArange[0]-axisRArange[1])/15 GT gapnum*tickgap))[0]]*15
    ;	IF axisRAminor LT 0 THEN axisRAminor = tickminor[(where((axisRArange[0]-axisRArange[1])/15 GT gapnum*tickgap))[0]]
    ;ENDIF ELSE axisRAminor = axisRAminor*(axisRAminor GE 0)
    ;axisDECgap = layer.bval[1]
    ;axisDECminor = layer.bval[3]
    ;narrowgap = layer.bval[1]*50 LT abs(axisDECrange[1]-axisDECrange[0])
    ;IF narrowgap THEN BEGIN
    ;	IF axisDECgap NE 0 THEN PRINT,'Warning - Too small Y tick gap, use default'
    ;	axisDECgap = tickgap[(where(axisDECrange[1]-axisDECrange[0] GT gapnum*tickgap))[0]]
    ;	IF axisDECminor LT 0 THEN axisDECminor = tickminor[(where(axisDECrange[1]-axisDECrange[0] GT gapnum*tickgap))[0]]
    ;ENDIF ELSE axisDECminor = axisDECminor*(axisDECminor GE 0)
    
    ;get tickvalue
    ATbond = axisRArange/axisRAgap	;boundary of axis ticks
    ATsort = ATbond[1] lt ATbond[0]
    ATbond = [ceil(min(ATbond)), floor(max(ATbond))]
    axisRAtickv = axisRAgap*(ATbond[0]+dindgen(ATbond[1]-ATbond[0]+1))
    IF ATsort THEN axisRAtickv = reverse(axisRAtickv)
    IF n_elements(axisRAtickv) LE 1 THEN BEGIN
        axisRAtickv = axisRArange
        axisRAminor = 0
    ENDIF
    axisRAticks = n_elements(axisRAtickv)-1
    
    ATbond = axisDECrange/axisDECgap	;boundary of axis ticks
    ATsort = ATbond[1] lt ATbond[0]
    ATbond = [ceil(min(ATbond)), floor(max(ATbond))]
    axisDECtickv = axisDECgap*(ATbond[0]+dindgen(ATbond[1]-ATbond[0]+1))
    IF ATsort THEN axisDECtickv = reverse(axisDECtickv)
    IF n_elements(axisDECtickv) LE 1 THEN BEGIN
        axisDECtickv = axisDECrange
        axisDECminor = 0
    ENDIF
    axisDECticks = n_elements(axisDECtickv)-1
    
    ;get tickname
    IF wcs eq 2 THEN tn = axisRAtickv ELSE BEGIN	;tickname
        tn = axisRAtickv mod 360.	;tickname
        tn = tn + 360*(tn LT 0)
    ENDELSE
    CASE RAticktype OF
        0:BEGIN
            IF strpos(strlowcase(RAformat[0]), 'i') GE 0 THEN tn = round(tn)
            axisRAtickname = string(tn, FORMAT = RAformat[0])
        END
        1:BEGIN
            hour = tn/15.
            IF strpos(strlowcase(RAformat[0]), 'i') GE 0 THEN hour = round(hour)
            axisRAtickname = string(hour, FORMAT = RAformat[0])+RAformat[1]
        END
        2:BEGIN
            tn = tn/15.
            hour = fix(tn)
            minute = abs(tn-hour)*60.
            carry = string(minute, FORMAT = '(F0.2)') EQ '60.00'
            minute = minute*(~carry)
            hour = (hour+carry) mod 24
            ;combine
            hour = string(hour, FORMAT = RAformat[0])+RAformat[1]
            IF strpos(strlowcase(RAformat[2]), 'i') GE 0 THEN minute = round(minute)
            minute = string(minute, FORMAT = RAformat[2])+RAformat[3]
            axisRAtickname = replicate('', n_elements(hour))
            skip_hr = n_elements(uniq(hour)) EQ 1
            IF skip_hr THEN axisRAtickname[0] += hour[0] ELSE axisRAtickname += hour
            axisRAtickname += minute
        END
        ELSE:BEGIN
            tn = tn/15.
            hour = fix(tn)
            tn = abs(tn-hour)*60.
            minute = fix(tn)
            second = abs(tn-minute)*60
            carry = string(second, FORMAT = '(F0.2)') EQ '60.00'
            second = second*(~carry)
            minute = minute+carry
            carry = minute EQ 60
            minute = minute*(~carry)
            hour = (hour+carry) mod 24
            ;combine
            hour = string(hour, FORMAT = RAformat[0])+RAformat[1]
            minute = string(minute, FORMAT = RAformat[2])+RAformat[3]
            IF strpos(strlowcase(RAformat[4]), 'i') GE 0 THEN second = round(second)
            second = string(second, FORMAT = RAformat[4])+RAformat[5]
            axisRAtickname = replicate('', n_elements(hour))
            skip_hr = n_elements(uniq(hour)) EQ 1
            skip_mn = skip_hr AND (n_elements(uniq(minute)) EQ 1)
            IF skip_hr THEN axisRAtickname[0] += hour[0] ELSE axisRAtickname += hour
            IF skip_mn THEN axisRAtickname[0] += minute[0] ELSE axisRAtickname += minute
            axisRAtickname += second
        END
    ENDCASE
    tn = axisDECtickv	;tickname
    CASE DECticktype OF
        1:BEGIN
            IF strpos(strlowcase(DECformat[0]), 'i') GE 0 THEN tn = round(tn)
            axisDECtickname = string(tn, FORMAT = DECformat[0])+DECformat[1]
        END
        2:BEGIN
            sign = tn GE 0
            tn = abs(tn)
            degree = fix(tn)
            arcmin = (tn-degree)*60.
            carry = string(arcmin, FORMAT = '(F0.2)') EQ '60.00'
            arcmin = arcmin*(~carry)
            degree = degree+carry
            ;combine
            IF strpos(DECformat[0], '+') GE 0 THEN sign = strmid(string(sign*2-1, FORMAT = '(I+2)'), 0, 1) $
                ELSE sign = strtrim(strmid(string(sign*2-1, FORMAT = '(I2)'), 0, 1))
            DECformat[0] = strjoin(strsplit(DECformat[0], '+', /EXTRACT))
            degree = sign+string(degree, FORMAT = DECformat[0])+DECformat[1]
            IF strpos(strlowcase(DECformat[2]), 'i') GE 0 THEN arcmin = round(arcmin)
            arcmin = string(arcmin, FORMAT = DECformat[2])+DECformat[3]
            axisDECtickname = replicate('', n_elements(degree))
            skip_dg = n_elements(uniq(degree)) EQ 1
            IF skip_dg THEN axisDECtickname[0] += degree[0] ELSE axisDECtickname += degree
            axisDECtickname += arcmin
        END
        ELSE:BEGIN
            sign = tn GE 0
            tn = abs(tn)
            degree = fix(tn)
            tn = (tn-degree)*60.
            arcmin = fix(tn)
            arcsec = (tn-arcmin)*60
            carry = string(arcsec, FORMAT = '(F0.2)') EQ '60.00'
            arcsec = arcsec*(~carry)
            arcmin = arcmin+carry
            carry = arcmin EQ 60
            arcmin = arcmin*(~carry)
            degree = degree+carry
            ;combine
            IF strpos(DECformat[0], '+') GE 0 THEN sign = strmid(string(sign*2-1, FORMAT = '(I+2)'), 0, 1) $
                ELSE sign = strtrim(strmid(string(sign*2-1, FORMAT = '(I2)'), 0, 1))
            DECformat[0] = strjoin(strsplit(DECformat[0], '+', /EXTRACT))
            degree = sign+string(degree, FORMAT = DECformat[0])+DECformat[1]
            arcmin = string(arcmin, FORMAT = DECformat[2])+DECformat[3]
            IF strpos(strlowcase(DECformat[2]), 'i') GE 0 THEN arcsec = round(arcsec)
            arcsec = string(arcsec, FORMAT = DECformat[4])+DECformat[5]
            axisDECtickname = replicate('', n_elements(degree))
            skip_dg = n_elements(uniq(degree)) EQ 1
            skip_am = skip_dg AND (n_elements(uniq(arcmin)) EQ 1)
            IF skip_dg THEN axisDECtickname[0] += degree[0] ELSE axisDECtickname += degree
            IF skip_am THEN axisDECtickname[0] += arcmin[0] ELSE axisDECtickname += arcmin
            axisDECtickname += arcsec
        END
    ENDCASE
    
    
    ;;combine
    ;accuracy = 2
    ;hms = axisRAtickv
    ;dms = axisDECtickv
    ;RADEC, hms, dms, hour, minute, second, degree, arcmin, arcsec
    ;
    ;carry = string(second, FORMAT = '(F0.1)') EQ '60.0'
    ;second = second*(~carry)
    ;minute = minute+carry
    ;carry = (minute EQ 60)
    ;minute = minute*(~carry)
    ;hour = (hour+carry) mod 24
    ;second = string(second, FORMAT = '(F0.1)')+raf[2]
    ;minute = string(minute, FORMAT = '(I0)')+raf[1]
    ;hour = string(hour, FORMAT = '(I0)')+raf[0]
    ;axisRAtickname = replicate('', n_elements(second))
    ;skip_hr = n_elements(uniq(hour)) EQ 1
    ;skip_sc = n_elements(uniq(second)) EQ 1 AND float(second[0]) EQ 0.
    ;skip_mn = (n_elements(uniq(minute)) EQ 1) AND ((skip_hr AND ~ skip_sc) OR (~ skip_hr AND skip_sc AND (float(minute[0]) EQ 0.)))
    ;IF skip_hr THEN axisRAtickname[0] += hour[0] ELSE axisRAtickname += hour
    ;IF skip_mn THEN axisRAtickname[0] += minute[0] ELSE axisRAtickname += minute
    ;IF skip_sc THEN axisRAtickname[0] += second[0] ELSE axisRAtickname += second
    ;
    ;sign = (degree GE 0) AND (arcmin GE 0) AND (arcsec GE 0)
    ;degree = abs(degree) & arcmin = abs(arcmin) & arcsec = abs(arcsec)
    ;carry = string(arcsec, FORMAT = '(F04.1)') EQ '60.0'
    ;arcsec = arcsec*(~carry)
    ;arcmin = arcmin+carry
    ;carry = (arcmin EQ 60)
    ;arcmin = arcmin*(~carry)
    ;degree = degree+carry
    ;arcsec = string(arcsec, FORMAT = '(F04.1)')+decf[2]
    ;arcmin = string(arcmin, FORMAT = '(I02)')+decf[1]
    ;degree = string(degree, FORMAT = '(I02)')+decf[0]
    ;FOR i=0,n_elements(sign)-1 DO IF ~ sign[i] THEN degree[i] = '-'+degree[i]
    ;;sign = strtrim(strmid(string(sign*2-1, FORMAT = '(I2)'), 0, 1))
    ;axisDECtickname = replicate('', n_elements(arcsec))
    ;IF n_elements(uniq(degree)) EQ 1 THEN $
    ;	axisDECtickname[0] += degree[0] ELSE axisDECtickname += degree
    ;IF n_elements(uniq(arcmin)) EQ 1 AND float(arcmin[0]) EQ 0. AND (n_elements(uniq(degree)) EQ 1 OR n_elements(uniq(arcsec)) EQ 1) THEN $
    ;	axisDECtickname[0] += arcmin[0] ELSE axisDECtickname += arcmin
    ;IF n_elements(uniq(arcsec)) EQ 1 AND float(arcsec[0]) EQ 0. THEN $
    ;	axisDECtickname[0] += arcsec[0] ELSE axisDECtickname += arcsec
    
    axisRAtickname = axisfont[2]+axisRAtickname+'!X'
    axisDECtickname = axisfont[2]+axisDECtickname+'!X'
    
    ;axistick
    axisxticklen = extendlen[1]/(axisposition[3]-axisposition[1])
    axisyticklen = extendlen[0]/(axisposition[2]-axisposition[0])
    
    ;axiscolor
    axiscolor = layer.ct
    
    ;image quality
    imagequality = max([layer.dsmooth,1])
    
    ;for ordinary plot
    ;IF wcs eq 2 THEN axisRArange = reverse(axisRArange)
    
    ;define coordinate
    ;TVLCT, axiscolor
    PLOT, [0], /YNOZERO, /NODATA, /NOERASE, $
        XRANGE = axisRArange, YRANGE = axisDECrange, $
    ;XMARGIN = pagemargin[0:1], YMARGIN = pagemargin[2:3], $
        XSTYLE = 5, YSTYLE = 5, POSITION = axisposition;, $
    ;	XTICKS = axisRAticks, YTICKS = axisDECticks, $
    ;	XTICKV = axisRAtickv, YTICKV = axisDECtickv, $
    ;	XMINOR = axisRAminor, YMINOR = axisDECminor, $
    ;	XTICKNAME = axisRAtickname, YTICKNAME = axisDECtickname, $
    ;	XTICKLEN = axisxticklen, YTICKLEN = axisyticklen, $
    ;	XTITLE='Right Ascension', YTITLE='Declination', $
    ;	COLOR = 0, POSITION = axisposition, $
    ;	CHARSIZE = character[0], CHARTHICK = charthick[0],$
    ;	XTHICK = linethick[0], YTHICK = linethick[0],background = 255
    ;set font for all numbers, ***may cause problems.
    XYOUTS,axisposition[0],axisposition[1],axisfont[2],/NORMAL
    
    ;print,axisrarange
    firstnotpanel = (where(layerlist.name NE 'PANEL'))[0]

    FOR layernum = n_elements(layerlist)-2, firstnotpanel, -1 DO BEGIN
        layer = layerlist[layernum]
        IF layer.valid EQ 2 THEN CONTINUE	;skip HIDE layers
        ;	RAt = strsplit(layer.ra, ':', /EXTRACT)
        ;	DECt = strsplit(layer.dec, ':', /EXTRACT)
        ;	IF n_elements(RAt) EQ 3 THEN RAcen = ten(RAt[0], RAt[1], RAt[2])*15 ELSE RAcen = double(layer.ra)
        ;	IF n_elements(DECt) EQ 3 THEN DECcen = ten(DECt[0], DECt[1], DECt[2]) ELSE DECcen = double(layer.dec)
        RAcen = str2ang(layer.ra, /RA)
        DECcen = str2ang(layer.dec, /DEC)
        
        
        CASE layer.name OF
            'IMAGE':BEGIN
                pixpercm = imagequality
                RAoff = double([layer.raof, layer.raot])
                IF layer.raof EQ '' THEN RAoff[0] = -!values.d_infinity
                IF layer.raot EQ '' THEN RAoff[1] = !values.d_infinity
                CASE layer.rau OF
                    'arcsec':RAoff = RAoff/3600.
                    'arcmin':RAoff = RAoff/60.
                    ELSE:
                ENDCASE
                RAoff = RAoff[reverse(sort(RAoff))]
                RArange = RAcen+RAoff/cos(DECcen*!DPI/180.)
                DECoff = double([layer.decof, layer.decot])
                IF layer.decof EQ '' THEN DECoff[0] = -!values.d_infinity
                IF layer.decot EQ '' THEN DECoff[1] = !values.d_infinity
                CASE layer.decu OF
                    'arcsec':DECoff = DECoff/3600.
                    'arcmin':DECoff = DECoff/60.
                    ELSE:
                ENDCASE
                DECoff = DECoff[sort(DECoff)]
                DECrange = DECcen+DECoff
                layerrange = [min([RArange[0], plotrange[0]]), max([RArange[1], plotrange[1]]), max([DECrange[0], plotrange[2]]), min([DECrange[1], plotrange[3]])]
                pixRA = fix((layerrange[0]-layerrange[1]) $
                    /(plotrange[0]-plotrange[1]) $
                    *(plotposition[2]-plotposition[0])*!d.x_size/1000.*pixpercm)
                pixDEC = fix((layerrange[3]-layerrange[2]) $
                    /(plotrange[3]-plotrange[2]) $
                    *(plotposition[3]-plotposition[1])*!d.y_size/1000.*pixpercm)
                ;			IF ~ file_test(layer.file[0]) THEN BEGIN
                ;				PRINT, 'Error - Image file does not exist in layer: '+layer.uname
                ;				BREAK
                ;			ENDIF
                
                error = 0b
                IF pixRA LT 1 THEN BEGIN
                    PRINT, 'Warning - Too small range for R.A. in layer: '+layer.uname
                    error = 1b
                ENDIF
                IF pixDEC LT 1 THEN BEGIN
                    PRINT, 'Warning - Too small range for DEC. in layer: '+layer.uname
                    error = 1b
                ENDIF
                IF error THEN BREAK
                
                ;begin plot
                CASE layer.type of
                    'SINGLE':BEGIN	;SINGLE image
                        IF layer.parameter[6] LE 0 THEN BEGIN	;image-image
                            FITS_CONVERT, layer.file[0], image, layerrange, wcs, $
                                NRA = pixRA-1, NDEC = pixDEC-1, ERROR = error
                            CASE error OF
                                1:PRINT, 'Error - Image file does not exist in layer: '+layer.uname
                                2:PRINT, 'Error - Invalid fits file format in layer: '+layer.uname
                                ELSE:BEGIN
                                    ;find NaN and save to a variable
                                    image_nan = where(finite(image,/NAN))
                                    ;smooth
                                    IF layer.dsmooth NE 0 THEN image = smooth(image, layer.dsmooth, /NAN)
                                    ;just in case NO error prompt
                                    IF image_nan[0] NE -1 THEN image[image_nan] = 0
                                    ;scale image
                                    image = image_scale(image, layer.bval[0], layer.bval[1], layer.parameter[3])
                                    image = round(image*(!d.table_size-2)+1)
                                    ;first index for nan color
                                    IF image_nan[0] NE -1 THEN image[image_nan] = 0
                                    ;plot
                                    TVLCT, layer.ct
                                    TV, image, layerrange[0], layerrange[2], XSIZE = layerrange[1]-layerrange[0], YSIZE = layerrange[3]-layerrange[2], /DATA
                                END
                            ENDCASE
                            
                        ENDIF ELSE BEGIN	;image-contour
                            FITS_CONVERT, layer.file[0], valuegrid, layerrange, wcs, $
                                NRA = pixRA-1, NDEC = pixDEC-1, XGRID = RAgrid, YGRID = DECgrid, ERROR = error, /interp
                            CASE error OF
                                1:PRINT, 'Error - Image file does not exist in layer: '+layer.uname
                                2:PRINT, 'Error - Invalid fits file format in layer: '+layer.uname
                                ELSE:BEGIN
                                    ;convvaluegrid
                                    layer.type = (layer.parameter[6] EQ 2)?'DIFFERENT':'SAME'
                                    
                                    nan = where(finite(valuegrid, /NAN))
                                    ;smooth
                                    IF file_test(layer.file[0]) AND layer.dsmooth NE 0 THEN BEGIN
                                        ;IF nan[0] NE -1 THEN valuegrid[nan] = min(valuegrid[where(finite(valuegrid))])
                                        valuegrid = smooth(valuegrid, layer.dsmooth, /NAN)
                                    ENDIF
                                    ;boundary value
                                    valuegrid = valuegrid > layer.bval[0] < layer.bval[1]
                                    IF nan[0] NE -1 THEN valuegrid[nan] = !values.d_nan
                                    
                                    levels = float(strsplit(layer.texto, /EXTRACT))
                                    CASE layer.cinter OF
                                        -2:BEGIN
                                            interval = (layer.bval[1]-layer.bval[0])/100d
                                            levels = levels*interval*layer.times+layer.bval[0]
                                        END
                                        ELSE:levels = levels*layer.times	;interval = 1.
                                    ENDCASE
                                    levels = levels(sort(levels))
                                    levels = levels(uniq(levels))
                                    thick = float(strsplit(layer.athick, /EXTRACT)) *pagescale
                                    dthick = layer.dthick *pagescale
                                    linestyle = (float(strsplit(layer.athick, /EXTRACT)) LT 0)*2
                                    IF layer.type EQ 'SAME' THEN color = replicate(0, n_elements(levels)) ELSE color = 255*findgen(n_elements(levels))/(n_elements(levels)-1)
                                    label = replicate(layer.parameter[1],n_elements(levels))
                                    lsize = float((strsplit(layer.asize+' 0 0', /EXTRACT))[1]) *pagescale
                                    
                                    ;draw the boundary contour and data contours
                                    TVLCT, layer.ct
                                    IF layer.parameter[4] THEN BEGIN;boundary
                                        checkgrid = finite(valuegrid)
                                        CONTOUR, checkgrid, RAgrid, DECgrid, /OVERPLOT, /CLOSED, C_COLOR = 0, C_THICK = thick, C_LINESTYLE = 1, LEVELS = [0.001]
                                    ENDIF
                                    ;OPLOT, RAabs[boundary],DECabs[boundary], COLOR = 0, LINESTYLE = 1
                                    CONTOUR, valuegrid, RAgrid, DECgrid, /OVERPLOT, LEVELS = levels, CLOSED = 1, $
                                        C_COLOR = color, C_THICK = thick, C_LINESTYLE=linestyle, C_LABELS = label, $
					C_CHARSIZE = lsize, C_CHARTHICK = dthick, $
                                        DOWNHILL = layer.parameter[2], FILL = layer.parameter[3]
                                END
                            ENDCASE
                        ENDELSE
                    END
                    'RGB':BEGIN	;RGB image
                        image = fltarr(3, pixRA, pixDEC)
                        image[*] = 0
                        FOR i = 0, 2 DO BEGIN
                            FITS_CONVERT, layer.file[i], channel, layerrange, wcs, $
                                NRA = pixRA-1, NDEC = pixDEC-1, ERROR = error
                            CASE error OF
                                1:PRINT, 'Warning - Image file does not exist in layer: '+layer.uname+' channel'+string(i+1, FORMAT = '(I1)')
                                2:PRINT, 'Warning - Invalid fits file format in layer: '+layer.uname+' channel'+string(i+1, FORMAT = '(I1)')
                                ELSE:BEGIN
                                    image_nan = where(finite(channel,/NAN))
                                    IF layer.dsmooth NE 0 THEN channel = smooth(channel, layer.dsmooth, /NAN)
                                    IF image_nan[0] NE -1 THEN channel[image_nan] = 0
                                    channel = image_scale(channel, layer.bval[i*2], layer.bval[i*2+1], layer.parameter[i+3])
                                    channel = round(channel*(!d.table_size-2)+1)
                                    IF image_nan[0] NE -1 THEN channel[image_nan] = 0
                                    image[i,*,*] = channel
                                END
                            ENDCASE
                        ENDFOR
                        TVLCT, layer.ct
                        TV, image, layerrange[0], layerrange[2], XSIZE = layerrange[1]-layerrange[0], YSIZE = layerrange[3]-layerrange[2], TRUE = 1, /DATA
                    END
                    'PICTURE':BEGIN ;JPEG or BMP image
                    END
                    ELSE:
                ENDCASE
                
                ;draw color bar
                IF layer.parameter[7] THEN BEGIN
                    colorbar = (findgen(!d.table_size-2)/(!d.table_size-3)*(layer.bval[1]-layer.bval[0])+layer.bval[0])##replicate(1,20)
                    colorbar = round(image_scale(colorbar, layer.bval[0], layer.bval[1], layer.parameter[3])*(!d.table_size-3)+1)
                    
                    ;TV, colorbar, axisRArange[1], axisDECrange[0], XSIZE = 2*extendlen[0]*(axisRArange[1]-axisRArange[0]), YSIZE = axisDECrange[1]-axisDECrange[0], /DATA
                    TV, colorbar, axisposition[2], axisposition[1], XSIZE = 2*extendlen[0], YSIZE = axisposition[3]-axisposition[1], /NORMAL
                    
                    TVLCT, axiscolor
                    ;draw boundary
                    PLOTS, [axisposition[2], axisposition[2]+2*extendlen[0], axisposition[2]+2*extendlen[0], axisposition[2]], $
                        [axisposition[1], axisposition[1], axisposition[3], axisposition[3]], /NORMAL, $
                        THICK = axisthick[0], COLOR = 0
                    ;draw line
                    AXIS, axisposition[2]+2*extendlen[0], axisposition[1], YAXIS = 1, /NORMAL, $
                        YRANGE = [layer.bval[0], layer.bval[1]], YSTYLE = 1, XTICKLEN = axisxticklen, $
                        YTHICK = axisthick[0], YTICKLEN = axisyticklen, YTICKNAME = replicate(' ', 20), COLOR = 0
                    ;draw number
                    ;XYOUTS, axisposition[2]+2*extendlen[0], axisposition[1], axisfont[2], /NORMAL
                    AXIS, axisposition[2]+2*extendlen[0], axisposition[1], YAXIS = 1, /NORMAL, $
                        YRANGE = [layer.bval[0], layer.bval[1]], YSTYLE = 1, YTICKLAYOUT = 1, $
                        CHARSIZE = axissize[2], CHARTHICK = axisthick[2], COLOR = 2
                ENDIF
            END;image
            
            'CONTOUR':BEGIN
                IF ~ file_test(layer.file[0]) THEN BEGIN
                    PRINT,'Error - Data file does not exist in layer: '+layer.uname
                    BREAK
                ENDIF
                
                ;derive absolute coordinate of RA&DEC
                data = (read_ascii(layer.file[0], COMMENT_SYMBOL = '!')).(0)
                n_col = (size(data))[1]
                error = 0
                IF layer.cra GE n_col THEN BEGIN
                    PRINT,'Error - Invalid R.A. column selected in layer: '+layer.uname
                    error = 1
                ENDIF
                IF layer.cdec GE n_col THEN BEGIN
                    PRINT,'Error - Invalid DEC. column selected in layer: '+layer.uname
                    error = 1
                ENDIF
                IF layer.cvalue GE n_col THEN BEGIN
                    PRINT,'Error - Invalid value column selected in layer: '+layer.uname
                    error = 1
                ENDIF
                IF layer.cinter GE n_col THEN BEGIN
                    PRINT,'Error - Invalid interval selected in layer: '+layer.uname
                    error = 1
                ENDIF
                IF error THEN BREAK
                
                RAoff = (data[layer.cra, *])[*]
                DECoff = (data[layer.cdec, *])[*]
                value = (data[layer.cvalue, *])[*]
                
                good = finite(RAoff) AND finite(DECoff)
                IF total(~good) GT 0 THEN BEGIN
                    PRINT,'Warning - Lines where R.A. or DEC. is NAN are removed in layer: '+layer.uname
                    IF total(good) GT 0 THEN BEGIN
                        RAoff = RAoff[where(good)]
                        DECoff = DECoff[where(good)]
                        value = value[where(good)]
                    ENDIF
                ENDIF
                
                CASE layer.rau OF
                    'arcsec':RAoff = double(RAoff)/3600.
                    'arcmin':RAoff = double(RAoff)/60.
                    ELSE:
                ENDCASE
                CASE layer.decu OF
                    'arcsec':DECoff = double(DECoff)/3600.
                    'arcmin':DECoff = double(DECoff)/60.
                    ELSE:
                ENDCASE
                IF layer.rau NE 'absolute' THEN RAabs = RAcen+RAoff/cos(DECcen*!DPI/180.) ELSE BEGIN
                    RAabs = dblarr(n_elements(RAoff))
                    FOR i=0l, n_elements(RAoff)-1 DO RAabs[i] = str2ang(RAoff[i], /RA)
                ENDELSE
                IF layer.decu NE 'absolute' THEN DECabs = DECcen+DECoff ELSE BEGIN
                    DECabs = dblarr(n_elements(DECoff))
                    FOR i=0l, n_elements(DECoff)-1 DO DECabs[i] = str2ang(DECoff[i], /DEC)
                ENDELSE
                
                ;    	        data = rd_tfile(layer.file[0], NOCOMMENT = '|', /AUTOCOL)
                ;    	        IF data[0] NE '' THEN BEGIN
                ;	            	RAt = transpose(data[layer.cra, *])
                ;	            	DECt = transpose(data[layer.cdec, *])
                ;	            	value = double(transpose(data[layer.cvalue, *]))
                ;	            	name = transpose(data[layer.cinter, *])
                ;	            	RAabs = dblarr(n_elements(value))
                ;	            	DECabs = dblarr(n_elements(value))
                ;	            	FOR i=0, n_elements(value)-1 DO BEGIN
                ;	            		RAc = strsplit(RAt[i], ':', /EXTRACT)
                ;						DECc = strsplit(DECt[i], ':', /EXTRACT)
                ;						IF n_elements(RAc) EQ 3 THEN RAabs[i] = ten(RAc[0], RAc[1], RAc[2])*15 ELSE RAabs[i] = double(RAt[i])
                ;						IF n_elements(DECc) EQ 3 THEN DECabs[i] = ten(DECc[0], DECc[1], DECc[2]) ELSE DECabs[i] = double(DECt[i])
                ;					ENDFOR
                
                IF n_elements(value) LT 3 THEN BEGIN
                    PRINT,'Warning - No enough points for contouring in layer: '+layer.uname
                    BREAK
                ENDIF
                ;set blanking value
                IF layer.parameter[5] THEN BEGIN
                    IF layer.Bval[1] GE layer.Bval[0] THEN blank = where(value GE layer.Bval[0] AND value LE layer.Bval[1]) $
                        ELSE blank = where(value LE layer.Bval[1] OR value GE layer.Bval[0])
                    IF n_elements(blank) GT n_elements(value)-3 THEN BEGIN
                        PRINT,'Warning - No enough points left after blanking in layer: '+layer.uname
                        BREAK
                    ENDIF
                    IF blank[0] NE -1 THEN value[blank] = !values.d_nan
                    
                    ;				IF layer.Bval[1] GE layer.Bval[0] THEN nonblank = where(value GE layer.Bval[1] OR value LE layer.Bval[0]) $
                    ;				ELSE nonblank = where(value LE layer.Bval[0] AND value GE layer.Bval[1])
                    ;				IF n_elements(nonblank) LT 3 THEN BREAK
                    ;				value = value[nonblank]
                    ;				RAoff = RAoff[nonblank]
                    ;				DECoff = DECoff[nonblank]
                    ;				RAabs = RAabs[nonblank]
                    ;				DECabs = DECabs[nonblank]
                ENDIF
                
                length = layer.length
                CASE layer.lenu OF
                    'arcsec':length = length/3600.
                    'arcmin':length = length/60.
                    ELSE:
                ENDCASE
                
                IF layer.parameter[6] THEN BEGIN
                    ;regular mapping
                    steplen = length
                    bearerr = 0.1
                    ninter = 15
                    
                    ;derive the coordinate grid
                    RAglen = steplen/cos(DECcen*!DPI/180.)
                    RAgnum = round((max(RAoff)-min(RAoff))/steplen)+1
                    RAgval = RAcen+(round(min(RAoff)/steplen)+indgen(RAgnum))*RAglen
                    DECglen = steplen
                    DECgnum = round((max(DECoff)-min(DECoff))/steplen)+1
                    DECgval = DECcen+(round(min(DECoff)/steplen)+indgen(DECgnum))*DECglen
                    
                    IF layer.parameter[8] THEN BEGIN	;missing value
                        FOR i=0l,RAgnum-1 DO BEGIN
                            FOR j=0l,DECgnum-1 DO BEGIN
                                within = where(RAabs GE RAgval[i]-RAglen*bearerr AND RAabs LE RAgval[i]+RAglen*bearerr $
                                    AND DECabs GE DECgval[j]-DECglen*bearerr AND DECabs LE DECgval[j]+DECglen*bearerr)
                                IF within[0] EQ -1 THEN BEGIN
                                    RAabs = [RAabs,RAgval[i]]
                                    DECabs = [DECabs,DECgval[j]]
                                    value = [value,layer.missing]
                                ENDIF ELSE BEGIN
                                    nan = where(finite(value[within], /NAN))
                                    IF nan[0] NE -1 THEN value[within[nan]] = layer.missing
                                ENDELSE
                            ENDFOR
                        ENDFOR
                    ENDIF
                    
                    ;triangulate the value
                    TRIANGULATE, RAabs, DECabs, triangles, TOLERANCE = 1e-12*max(abs([raabs,decabs]))
                    valuegrid = trigrid(RAabs, DECabs, value, triangles, $EXTRAPOLATE = boundary, $
                        MISSING = !values.d_nan, $
                        NX = (RAgnum-1)*ninter+1, NY = (DECgnum-1)*ninter+1, XGRID = RAgrid, YGRID = DECgrid)
                    RAgrid = arrayextend(RAgrid, n_elements(DECgrid))
                    DECgrid = transpose(arrayextend(DECgrid, n_elements(RAgrid[*,0])))
                    
                    ;erase the contour out of the plot region
                    checkgrid = bytarr(n_elements(valuegrid[*,0]), n_elements(valuegrid[0,*]))
                    checkgrid[*] = 1b
                    
                    IF ~ layer.parameter[8] THEN BEGIN	;missing value
                        FOR i=0l,RAgnum-1 DO BEGIN
                            FOR j=0l,DECgnum-1 DO BEGIN
                                within = where(RAabs GE RAgval[i]-RAglen*bearerr AND RAabs LE RAgval[i]+RAglen*bearerr $
                                    AND DECabs GE DECgval[j]-DECglen*bearerr AND DECabs LE DECgval[j]+DECglen*bearerr $
                                    AND finite(value, /NAN) EQ 0)
                                IF within[0] EQ -1 THEN BEGIN
                                    within = where(RAgrid GE RAgval[i]-RAglen*(1-0.5/ninter) AND RAgrid LE RAgval[i]+RAglen*(1-0.5/ninter) $
                                        AND DECgrid GE DECgval[j]-DECglen*(1-0.5/ninter) AND DECgrid LE DECgval[j]+DECglen*(1-0.5/ninter))
                                    IF within[0] NE -1 THEN checkgrid[within] = 0b
                                ENDIF
                            ENDFOR
                        ENDFOR
                    ENDIF
                    
                    ;				checkgrid[0:57, (size(checkgrid))[2]-1] = 0
                    
                ENDIF ELSE BEGIN
                    ;irregular mapping
                    ;decide boundary line
                    segment = length
                    
                    ;throw away far node
                    ;inside[where(finite(value, /NAN))] = -2
                    inside = replicate(1b, n_elements(value));1b:inside,0b:too far
                    REPEAT BEGIN
                        inside AND= (inside NE 2b);0b->0b,1b->1b,2b->0b
                        IF (where(inside))[0] EQ -1 THEN BREAK;no point inside
                        group = where(inside)
                        FOR i=0l, n_elements(group)-1 DO BEGIN
                            neighbors = total((RAoff[group]-(RAoff[group])[i])^2+(DECoff[group]-(DECoff[group])[i])^2 LE segment^2, /INTEGER)-1	;neighbor=distance<segment exclude self
                            IF neighbors LE 1 THEN inside[group[i]] = 2b;no or only one neighbor
                        ENDFOR
                    ENDREP UNTIL (where(inside EQ 2b))[0] EQ -1;no new point to throw
                    ;no enough node for triangulate
                    IF total(inside, /INTEGER) LT 3 THEN BEGIN
                        PRINT,'Warning - Too small length for the boundary segment in layer: '+layer.uname
                        BREAK
                    ENDIF
                    RAoff = RAoff[where(inside)]
                    DECoff = DECoff[where(inside)]
                    RAgp = RAabs[where(inside)]
                    DECgp = DECabs[where(inside)]
                    valuegp = value[where(inside)]
                    
                    ;get initial boundary
                    TRIANGULATE, RAoff, DECoff, triangles, boundary, TOLERANCE = 1e-12*max(abs([raoff,decoff]))
                    boundary = [boundary[n_elements(boundary)-1], boundary, boundary[0]]
                    FOR i=1l, n_elements(RAoff) DO BEGIN
                        IF i EQ n_elements(boundary)-1 THEN BREAK	;end of the cycle
                        ;if right boundary node go to next
                        length = (RAoff[boundary[i]]-RAoff[boundary[i+1]])^2+(DECoff[boundary[i]]-DECoff[boundary[i+1]])^2
                        IF length LE segment^2 THEN CONTINUE
                        ;find candidate for boundary
                        distance = (RAoff - RAoff[boundary[i]])^2+(DECoff - DECoff[boundary[i]])^2
                        candidate = where(distance LE segment^2 AND distance GT 0)
                        ;not exist, go to next node directly
                        IF candidate[0] EQ -1 THEN CONTINUE
                        ;make sure no intersection
                        nocross = replicate(1b, n_elements(candidate))
                        FOR k=0l,n_elements(candidate)-1 DO BEGIN
                            cross = 0b
                            FOR j=1l,i-2 DO BEGIN
                                cross = segmentintersection([RAoff[boundary[j]], DECoff[boundary[j]]], $
                                    [RAoff[boundary[j+1]], DECoff[boundary[j+1]]], $
                                    [RAoff[boundary[i]], DECoff[boundary[i]]], $
                                    [RAoff[candidate[k]], DECoff[candidate[k]]])
                                IF cross THEN BREAK
                            ENDFOR
                            IF cross THEN nocross[k] = 0b
                        ENDFOR
                        IF (where(nocross))[0] NE -1 THEN candidate = candidate[where(nocross)] ELSE CONTINUE
                        ;find the first counterclockwise one
                        IF n_elements(candidate) GT 1 THEN BEGIN
                            angleini = atan(DECoff[boundary[i-1]]-DECoff[boundary[i]], RAoff[boundary[i-1]]-RAoff[boundary[i]])
                            angle = atan(DECoff[candidate]-DECoff[boundary[i]], RAoff[candidate]-RAoff[boundary[i]]) - angleini
                            angle += (angle LE 0)*2*!PI
                            candidate = candidate[where(angle EQ min(angle))]
                        ENDIF
                        ;find the nearest one
                        IF n_elements(candidate) GT 1 THEN BEGIN
                            length = (DECoff[candidate]-DECoff[boundary[i]])^2+(RAoff[candidate]-RAoff[boundary[i]])^2
                            candidate = candidate[where(length EQ min(length))]
                        ENDIF
                        theone = candidate[0]
                        boundary = [boundary[0:i], theone, boundary[i+1:n_elements(boundary)-1]]
                    ENDFOR
                    boundary = boundary[1:n_elements(boundary)-1]
                    
                    ;derive the average step length
                    bearerr = 0.1
                    
                    RAsort = RAoff[sort(RAoff)]
                    RAinter = RAsort-shift(RAsort,1)
                    RAglen = mean(RAinter[where(RAinter GT max(RAinter)*bearerr)])
                    RAinter = (-RAinter[0])/RAglen+1
                    DECsort = DECoff[sort(DECoff)]
                    DECinter = DECsort-shift(DECsort,1)
                    DECglen = mean(DECinter[where(DECinter GT max(DECinter)*bearerr)])
                    DECinter = (-DECinter[0])/DECglen+1
                    
                    RAinter = min([RAinter,60])
                    DECinter = min([DECinter,60])
                    
                    ;triangulate the value
                    ;TRIANGULATE, RAabs[where(inside GE 0)], DECabs[where(inside GE 0)], triangles
                    valuegrid = trigrid(RAgp, DECgp, valuegp, triangles, $EXTRAPOLATE = boundary, $
                        MISSING = !values.d_nan, $/QUINTIC, $
                        NX = RAinter*15+1, NY = DECinter*15+1, XGRID = RAgrid, YGRID = DECgrid)
                    RAgrid = arrayextend(RAgrid, n_elements(DECgrid))
                    DECgrid = transpose(arrayextend(DECgrid, n_elements(RAgrid[*,0])))
                    
                    ;erase the contour out of the plot region
                    checkgrid = bytarr(size(valuegrid, /DIMENSIONS)) AND 0b
                    
                    ;METHOD 1: PRECISE but too COMPLEX
                    ;FOR i=0, n_elements(checkgrid)-1 DO BEGIN
                    ;	angle = atan(DECabs-DECgrid[i], RAabs-RAgrid[i])
                    ;	IF (where(angle GT 0))[0] EQ -1 THEN CONTINUE
                    ;	angle = min(angle[where(angle GT 0)])/4
                    ;	outnode = [max(RAabs)+1, (max(RAabs)+1-RAgrid[i])*tan(angle)+DECgrid[i]]
                    ;	intersections = 0
                    ;	FOR k=0,n_elements(boundary)-2 DO BEGIN
                    ;		IF RAabs[boundary[k]] LT RAgrid[i] AND RAabs[boundary[k+1]] LT RAgrid[i] THEN CONTINUE
                    ;		IF DECabs[boundary[k]] LT DECgrid[i] AND DECabs[boundary[k+1]] LT DECgrid[i] THEN CONTINUE
                    ;		cross = segmentintersection([RAabs[boundary[k]], DECabs[boundary[k]]], $
                    ;			[RAabs[boundary[k+1]], DECabs[boundary[k+1]]], $
                    ;			[RAgrid[i], DECgrid[i]], $
                    ;			[RAgrid[i], DECgrid[i]])
                    ;		IF cross EQ 1 THEN BEGIN
                    ;			checkgrid[i] = 1
                    ;			BREAK
                    ;		ENDIF
                    ;		cross= segmentintersection([RAabs[boundary[k]], DECabs[boundary[k]]], $
                    ;			[RAabs[boundary[k+1]], DECgp[boundary[k+1]]], $
                    ;			[RAgrid[i], DECgrid[i]], $
                    ;			outnode)
                    ;		IF cross EQ 1 THEN intersections++
                    ;	ENDFOR
                    ;	IF intersections MOD 2 THEN checkgrid[i] = 1
                    ;ENDFOR
                    
                    ;METHOD 2: SIMPLE
                    FOR i=0l, n_elements(boundary)-2 DO BEGIN
                        IF RAgp[boundary[i]] NE RAgp[boundary[i+1]] THEN BEGIN
                            RAmin = min([RAgp[boundary[i]],RAgp[boundary[i+1]]], MAX = RAmax)
                            nx = where(RAgrid[*,0] LT RAmax AND RAgrid[*,0] GE RAmin)
                            IF nx[0] EQ -1 THEN CONTINUE
                            k = (DECgp[boundary[i+1]]-DECgp[boundary[i]])/(RAgp[boundary[i+1]]-RAgp[boundary[i]])
                            b = (RAgp[boundary[i+1]]*DECgp[boundary[i]]-RAgp[boundary[i]]*DECgp[boundary[i+1]])/(RAgp[boundary[i+1]]-RAgp[boundary[i]])
                            y = k*RAgrid[nx,0]+b
                            FOR j=0l, n_elements(nx)-1 DO BEGIN
                                DECdelta = min(abs(DECgrid[0,*]-y[j]), ny)
                                checkgrid[nx[j], ny] = ~checkgrid[nx[j],ny]
                            ENDFOR
                        ENDIF
                    ENDFOR
                    FOR i=0l, n_elements(checkgrid[*,0])-1 DO BEGIN
                        terminal = where(checkgrid[i,*])
                        FOR j=0l, n_elements(terminal)/2-1 DO BEGIN
                            checkgrid[i,terminal[2*j]:terminal[2*j+1]] = 1b
                        ENDFOR
                    ENDFOR
                ENDELSE
                
                outland = RAgrid GT plotrange[0] OR RAgrid LT plotrange[1] OR DECgrid LT plotrange[2] OR DECgrid GT plotrange[3]
                checkgrid = checkgrid AND ~outland
                ;			invalid = where(finite(valuegrid, /NAN))
                ;			IF invalid[0] NE -1 THEN checkgrid[invalid] = 0
                inside = where(checkgrid, COMPLEMENT = outside)
                IF inside[0] EQ -1 THEN BEGIN
                    PRINT,'Warning - Outside the plot range for contour in layer: '+layer.uname
                    BREAK
                ENDIF
                ;smooth
                IF layer.dsmooth GT 0 THEN BEGIN
                    valuegrid = smooth(valuegrid, layer.dsmooth, /NAN, /EDGE_TRUNCATE);leefilt(valuegrid, layer.dsmooth);
                ENDIF
                IF outside[0] NE -1 THEN valuegrid[outside] = !values.d_nan
                ;fill
                IF layer.parameter[3] THEN BEGIN
                    invalid = where(finite(valuegrid, /NAN))
                    IF invalid[0] NE -1 THEN valuegrid[invalid] = -10000000d
                ENDIF
                
                ;derive levels thick
                CASE layer.cinter OF
                    -2:interval = max(abs(valuegrid[inside]), /NAN)/100d
                    -1:interval = 1.
                    ELSE:interval = data[layer.cinter, *]
                ENDCASE
                DRAWCONTOUR:
                levels = float(strsplit(layer.texto, /EXTRACT))*interval*layer.times
                levels = levels(sort(levels))
                levels = levels(uniq(levels))
                thick = abs(float(strsplit(layer.athick, /EXTRACT)))  *pagescale
                dthick = layer.dthick  *pagescale
                linestyle = (float(strsplit(layer.athick, /EXTRACT)) LT 0)*2
                IF layer.type EQ 'SAME' THEN color = replicate(0, n_elements(levels)) ELSE color = 255*findgen(n_elements(levels))/(n_elements(levels)-1)
                label = replicate(layer.parameter[1],n_elements(levels))
                lsize = float((strsplit(layer.asize+' 0 0', /EXTRACT))[1]) *pagescale
                
                ;Export contour surface to fits
                ;sz = (size(valuegrid))[1:2]/2
                ;MAKE_ASTR, astr, DELT = [(RAgrid[1,0]-RAgrid[0,0])*cos(DECgrid[sz[0],sz[1]]*!DPI/180.), DECgrid[0,1]-DECgrid[0,0]],$
                ;	CRPIX = sz+1, CRVAL = [RAgrid[sz[0],sz[1]], DECgrid[sz[0],sz[1]]]
                ;hdr = strarr(1)
                ;PUTAST, hdr, astr
                ;FITS_WRITE, 'wtf.fits', valuegrid, hdr
                ;FITS_WRITE, 'wtfmask.fits', checkgrid, hdr
                
                ;draw the boundary contour and data contours
                TVLCT, layer.ct
                IF layer.parameter[4] THEN BEGIN
                    checkgrid[0,0] = 0
                    CONTOUR, checkgrid, RAgrid, DECgrid, /OVERPLOT, /CLOSED, C_COLOR = 0, C_THICK = dthick, C_LINESTYLE = 1, LEVELS = [0.001]
                ENDIF
                ;OPLOT, RAabs[boundary],DECabs[boundary], COLOR = 0, LINESTYLE = 1
                CONTOUR, valuegrid, RAgrid, DECgrid, /OVERPLOT, LEVELS = levels, CLOSED = 0, $
                    C_COLOR = color, C_THICK = thick, C_LINESTYLE=linestyle, C_LABELS = label, C_CHARSIZE = lsize, $
                    DOWNHILL = layer.parameter[2], FILL = layer.parameter[3]
                
                ;Export contour lines to DS9 mode
                ;			CONTOUR, valuegrid, RAgrid, DECgrid, /OVERPLOT, LEVELS = levels, $
                ;            	CLOSED = 0, /PATH_DOUBLE, /PATH_DATA_COORDS, PATH_INFO = info, PATH_XY = xy
                ;            CONTOUR, checkgrid, RAgrid, DECgrid, /OVERPLOT, LEVELS = [1], $
                ;            	/CLOSED, /PATH_DOUBLE, /PATH_DATA_COORDS, PATH_INFO = info_b, PATH_XY = xy_b
                ;IF n_elements(xy) gt 1 THEN BEGIN
                ;	        CLOSE, 1
                ;    	    OPENW, 1, 'AICer.con'
                ;    	    cc = string([32B])+strjoin(strtrim(string(xy),2), string([32B]))+string([32B])+string([10B])
                ;    	    ending = info.offset+info.n-1
                ;			closed = where(info.type)
                ;    	    IF closed[0] NE -1 THEN cc[ending[closed]] += cc[info[closed].offset]
                ;    	    cc[ending] += string([10B])
                ;            WRITEU, 1, cc
                ;			CLOSE, 1
                ;			OPENW, 1, 'AICerBoundary.con'
                ;			cc = string([32B])+strjoin(strtrim(string(xy_b),2), string([32B]))+string([32B])+string([10B])
                ;			ending = info_b.offset+info_b.n-1
                ;			closed = where(info_b.type)
                ;			IF closed[0] NE -1 THEN cc[ending[closed]] += cc[info_b[closed].offset]
                ;			cc[ending] += string([10B])
                ;			WRITEU, 1, cc
                ;			CLOSE, 1
                ;END
                
                ;draw data points
                IF layer.parameter[0] GT 0 THEN BEGIN
                    symbol = symbol_set(layer.afont, FILL = (layer.parameter[0] EQ 2))
                    psize = float((strsplit(layer.asize+' 0 0', /EXTRACT))[0]) *pagescale
                    OPLOT, RAabs[where(finite(value, /NAN) EQ 0)], DECabs[where(finite(value, /NAN) EQ 0)], PSYM = symbol, SYMSIZE = psize, COLOR = 0
                ENDIF
            END;contour
            
            'POINT':BEGIN
                IF ~ file_test(layer.file[0]) THEN BEGIN
                    PRINT, 'Error - Catalog file does not exist in layer: '+layer.uname
                    BREAK
                ENDIF
                data = rd_tfile(layer.file[0], NOCOMMENT = '|', /AUTOCOL)
                IF data[0] EQ '' THEN BEGIN
                    PRINT,'Error - Empty catalog file in layer: '+layer.uname
                    BREAK
                ENDIF
                n_col = (size(data))[1]
                
                IF layer.parameter[2] THEN BEGIN
                    IF layer.ctrust GE n_col THEN BEGIN
                        PRINT,'Error - Invalid flag column selected in layer: '+layer.uname
                        BREAK
                    ENDIF
                    flag = (data[layer.ctrust, *])[*]
                    select = where(strcmp(flag, layer.texto, /FOLD_CASE))
                    IF select[0] EQ -1 THEN BEGIN
                        PRINT,'Error - No line with flag "'+layer.texto+'" in layer: '+layer.uname
                        BREAK
                    ENDIF
                    data = data[*, select]
                ENDIF
                
                
                IF layer.cra GE n_col THEN BEGIN
                    PRINT,'Error - Invalid R.A. column selected in layer: '+layer.uname
                    BREAK
                ENDIF
                
                IF layer.cdec GE n_col THEN BEGIN
                    PRINT,'Error - Invalid DEC. column selected in layer: '+layer.uname
                    BREAK
                ENDIF
                
                IF layer.type EQ 'VECTOR' OR layer.parameter[0] EQ 1 THEN BEGIN
                    IF layer.cvalue GE n_col THEN BEGIN
                        PRINT,'Error - Invalid column selected in layer: '+layer.uname
                        BREAK
                    ENDIF ELSE value1 = (data[layer.cvalue, *])[*]
                ENDIF
                
                IF layer.type EQ 'VECTOR' OR layer.parameter[0] EQ 2 THEN BEGIN
                    IF layer.cinter GE n_col THEN BEGIN
                        PRINT,'Error - Invalid column selected in layer: '+layer.uname
                        BREAK
                    ENDIF ELSE value2 = (data[layer.cinter, *])[*]
                ENDIF
                
                IF layer.type EQ 'VECTOR' AND layer.parameter[0] EQ 2 THEN BEGIN
                    IF layer.cerror GE n_col THEN BEGIN
                        PRINT,'Error - Invalid column selected in layer: '+layer.uname
                        BREAK
                    ENDIF ELSE BEGIN
                        value3 = (data[layer.cerror, *])[*]
                        ;IFA = double(data[layer.cvalue:layer.cvalue+3, *]) ;intensities of four angles
                        ;IQ = IFA[0,*]-IFA[2,*]
                        ;IU = IFA[1,*]-IFA[3,*]
                        ;II = total(IFA, 1)/2
                        ;value = sqrt(IQ^2+IU^2)/II
                        ;angle = atan(IU/IQ)/2.
                        ;angle = (double((data[layer.cinter, *])[*])+90.)*!dtor
                    ENDELSE
                ENDIF
                
                RAt = (data[layer.cra, *])[*]
                DECt = (data[layer.cdec, *])[*]
                
                CASE layer.rau OF
                    'arcsec':RAt = double(RAt)/3600.
                    'arcmin':RAt = double(RAt)/60.
                    ELSE:
                ENDCASE
                CASE layer.decu OF
                    'arcsec':DECt = double(DECt)/3600.
                    'arcmin':DECt = double(DECt)/60.
                    ELSE:
                ENDCASE
                IF layer.rau NE 'absolute' THEN RAabs = RAcen+RAt/cos(DECcen*!DPI/180.) ELSE BEGIN
                    RAabs = dblarr(n_elements(RAt))
                    FOR i=0l, n_elements(RAt)-1 DO RAabs[i] = str2ang(RAt[i], /RA)
                ENDELSE
                IF layer.decu NE 'absolute' THEN DECabs = DECcen+DECt ELSE BEGIN
                    DECabs = dblarr(n_elements(DECt))
                    FOR i=0l, n_elements(DECt)-1 DO DECabs[i] = str2ang(DECt[i], /DEC)
                ENDELSE
                IF wcs NE 2 THEN RAabs -= (RAabs gt plotrange[0])*360d
                
                TVLCT, layer.ct
                symsize = double(layer.asize) *pagescale
                symthick = float(layer.athick) *pagescale
                symstyle = (float(layer.athick) GE 0)?0:2
                arrowlength = double(layer.aorient) *pagescale
                IF layer.parameter[0] LE 1 THEN symbol = symbol_set(layer.afont, FILL = layer.parameter[1], THICK = symthick)
                IF layer.type NE 'VECTOR' THEN BEGIN
                    CASE layer.parameter[0] OF
                        0:BEGIN;uniform point
                            IF symbol NE 0 THEN BEGIN	;draw point
                                OPLOT, RAabs, DECabs, PSYM = symbol, SYMSIZE = symsize, THICK = symthick, COLOR = 0, LINESTYLE = symstyle
                            ENDIF ELSE BEGIN	;connect points with line
                                seg = rd_tfile(layer.file[0], NOCOMMENT = '|') NE ''
                                seg = [0b,seg]
                                seg = uniq(seg);+seg(uniq(seg))
                                idx = indgen(n_elements(seg)/2)*2
                                len = seg[idx+1]-seg[idx]
                                ;seg = where(rd_tfile(layer.file[0], NOCOMMENT = '|') EQ '')
                                ;seg = [0, seg-indgen(n_elements(seg))]
                                pos = 0
                                
                                FOR i=0l, n_elements(len)-1 DO BEGIN
                                    OPLOT, RAabs[pos:pos+len[i]-1], DECabs[pos:pos+len[i]-1], THICK = symthick, COLOR = 0, LINESTYLE = symstyle
                                    pos = pos+len[i]
                                    
                                ENDFOR
                                ;oplot,raabs[1:50],decabs[1:50], THICK = symthick, COLOR = 0, LINESTYLE = symstyle
                            ENDELSE
                        END
                        1:BEGIN;resize according to flux
                            ;value = alog(value)
                            value = double(value1)
                            FOR i=0l,n_elements(RAabs)-1 DO BEGIN
                                OPLOT, [RAabs[i]], [DECabs[i]], PSYM = symbol, SYMSIZE = symsize*value1[i]/max(value1), THICK = symthick, COLOR = 0
                            ENDFOR
                        END
                        2:BEGIN; draw name
                            name = value2
                            inside = where(RAabs LE max(plotrange[0:1]) AND RAabs GE min(plotrange[0:1]) AND DECabs GE min(plotrange[2:3]) AND DECabs LE max(plotrange[2:3]))
                            IF inside[0] EQ -1 THEN BREAK
                            XYOUTS, RAabs[inside], DECabs[inside], name[inside], /DATA, CHARSIZE = symsize, CHARTHICK = symthick, COLOR = 0, ALIGNMENT=0.5
                        END
                        ELSE:
                    ENDCASE
                ENDIF ELSE BEGIN
                    sharp = !pi/10	;half angle of arrow head
                    mag = double(value1)*arrowlength
                    ang = (double(value2)+90.)*!dtor
                    dx = -mag*cos(ang)/cos(DECabs*!dtor)
                    dy = mag*sin(ang)
                    RAf = RAabs-dx/2*layer.parameter[1]
                    DECf = DECabs-dy/2*layer.parameter[1]
                    RAt = RAf+dx
                    DECt = DECf+dy
                    CASE layer.parameter[0] OF
                        0:BEGIN;Polarization
                            FOR i=0l,n_elements(RAabs)-1 DO BEGIN
                                OPLOT, [RAf[i], RAt[i]], [DECf[i], DECt[i]], THICK = symthick, COLOR = 0
                            ENDFOR
                        END
                        1:BEGIN;arrow
                            FOR i=0l,n_elements(RAabs)-1 DO BEGIN
                                OPLOT, [RAf[i], RAt[i]], [DECf[i], DECt[i]], THICK = symthick, COLOR = 0
                                USERSYM, [0,-cos(ang[i]+sharp),-cos(ang[i]-sharp),0],$
                                    [0,-sin(ang[i]+sharp),-sin(ang[i]-sharp),0], /FILL, THICK = symthick
                                OPLOT, [RAt[i]], [DECt[i]], PSYM = 8, SYMSIZE = symsize, THICK = symthick, COLOR = 0
                            ENDFOR
                        END
                        2:BEGIN;arrow with error
                            errlen = double(value1)*arrowlength/4
                            angp = (double(value2)+double(value3)+90.)*!dtor
                            angm = (double(value2)-double(value3)+90.)*!dtor
                            dxp = -errlen*cos(angp)/cos(DECabs*!dtor)
                            dyp = errlen*sin(angp)
                            dxm = -errlen*cos(angm)/cos(DECabs*!dtor)
                            dym = errlen*sin(angm)
                            RApf = RAabs-dxp/2*layer.parameter[1]
                            DECpf = DECabs-dyp/2*layer.parameter[1]
                            RApt = RApf+dxp
                            DECpt = DECpf+dyp
                            RAmf = RAabs-dxm/2*layer.parameter[1]
                            DECmf = DECabs-dym/2*layer.parameter[1]
                            RAmt = RAmf+dxm
                            DECmt = DECmf+dym
                            FOR i=0l,n_elements(RAabs)-1 DO BEGIN
                                OPLOT, [RAf[i], RAt[i]], [DECf[i], DECt[i]], THICK = symthick, COLOR = 0
                                USERSYM, [0,-cos(ang[i]+sharp),-cos(ang[i]-sharp),0],$
                                    [0,-sin(ang[i]+sharp),-sin(ang[i]-sharp),0], /FILL, THICK = symthick
                                OPLOT, [RAt[i]], [DECt[i]], PSYM = 8, SYMSIZE = symsize, THICK = symthick, COLOR = 0
                                OPLOT, [RApf[i], RApt[i]], [DECpf[i], DECpt[i]], THICK = symthick, COLOR = 0
                                OPLOT, [RAmf[i], RAmt[i]], [DECmf[i], DECmt[i]], THICK = symthick, COLOR = 0
                            ENDFOR
                        END
                        ELSE:
                    ENDCASE
                ENDELSE
            END;point
            
            'TEXT':BEGIN
                text = layer.afont+textconvert(layer.texti, SOURCE = source, RA = layer.ra, DEC = layer.dec)+'!X'
                TVLCT, layer.ct
                CASE layer.rau OF
                    'Absolute(WCS)':BEGIN
                        TX = str2ang(layer.raof, /RA)
                        TY = str2ang(layer.decof, /DEC)
                    END
                    ELSE:BEGIN
                        TX = plotrange[0]+(plotrange[1]-plotrange[0])*float(layer.raof)/100.
                        TY = plotrange[2]+(plotrange[3]-plotrange[2])*float(layer.decof)/100.
                    END
                ENDCASE
                charsize = float(layer.asize) *pagescale
                charthick = float(layer.athick) *pagescale
                charorient = float(layer.aorient)
                XYOUTS, TX, TY, text, /DATA, CHARSIZE = charsize, CHARTHICK = charthick, COLOR = 0, ORIENTATION = charorient, /NOCLIP
            END;text
            
            'SHAPE':BEGIN
                CASE layer.rau OF
                    'Normalized(%)':BEGIN
                        x1 = plotrange[0]+(plotrange[1]-plotrange[0])*float(layer.raof)/100.
                        y1 = plotrange[2]+(plotrange[3]-plotrange[2])*float(layer.decof)/100.
                    END
                    'Absolute(WCS)':BEGIN
                        x1 = str2ang(layer.raof, /RA)
                        y1 = str2ang(layer.decof, /DEC)
                    END
                    ELSE:
                ENDCASE
                linethick = double(layer.athick) *pagescale
                linestyle = (float(layer.athick) GE 0)?0:2
                TVLCT, layer.ct
                
                CASE layer.type OF
                    'LINE':BEGIN
                        CASE layer.decu OF
                            'Normalized(%)':BEGIN
                                x2 = plotrange[0]+(plotrange[1]-plotrange[0])*double(layer.raot)/100.
                                y2 = plotrange[2]+(plotrange[3]-plotrange[2])*double(layer.decot)/100.
                            END
                            'Absolute(WCS)':BEGIN
                                x2 = str2ang(layer.raot, /RA)
                                y2 = str2ang(layer.decot, /DEC)
                            END
                            ELSE:
                        ENDCASE
                        OPLOT, [x1, x2], [y1, y2], COLOR = 0, THICK = linethick, LINESTYLE = linestyle, /NOCLIP
                    END
                    
                    'ARROW':BEGIN
                        CASE layer.decu OF
                            'Normalized(%)':BEGIN
                                x2 = plotrange[0]+(plotrange[1]-plotrange[0])*double(layer.raot)/100.
                                y2 = plotrange[2]+(plotrange[3]-plotrange[2])*double(layer.decot)/100.
                            END
                            'Absolute(WCS)':BEGIN
                                x2 = str2ang(layer.raot, /RA)
                                y2 = str2ang(layer.decot, /DEC)
                            END
                            ELSE:
                        ENDCASE
                        ARROW, x1, y1, x2, y2, /DATA, COLOR = 0, THICK = linethick
                    END
                    
                    'RULER':BEGIN
                        xc = x1
                        yc = y1
                        CASE layer.decu OF
                            'Normalized(%)':BEGIN
                                length = (plotrange[1]-plotrange[0])*double(layer.raot)/100.*cos(yc*!DTOR)
                            END
                            'Absolute(WCS)':BEGIN
                                if execute('temp = '+layer.raot,1,1) then length = temp else length = 0d
                            END
                            ELSE:
                        ENDCASE
                        lobs = abs(length)
                        aobs = double(layer.aorient)
                        lwcs = lobs*sqrt(sin(aobs*!DTOR)^2+cos(aobs*!DTOR)^2/cos(yc*!DTOR)^2)
                        awcs = atan(tan(aobs*!DTOR)*cos(yc*!DTOR))/!DTOR
                        dx = cos(awcs*!DTOR)*lwcs/2
                        dy = sin(awcs*!DTOR)*lwcs/2
                        OPLOT, [xc-dx-dy/10/cos(yc*!DTOR), xc-dx, xc+dx, xc+dx-dy/10/cos(yc*!DTOR)], $
                            [yc-dy+dx/10*cos(yc*!DTOR), yc-dy, yc+dy, yc+dy+dx/10*cos(yc*!DTOR)], $
                            COLOR = 0, THICK = linethick
                    END
                    
                    'ELLIPSE':BEGIN
                        xc = x1
                        yc = y1
                        CASE layer.decu OF
                            'Normalized(%)':BEGIN
                                dmaj = (plotrange[1]-plotrange[0])*double(layer.raot)/100.*cos(yc*!DTOR)
                                dmin = (plotrange[3]-plotrange[2])*double(layer.decot)/100.
                            END
                            'Absolute(WCS)':BEGIN
                                if execute('temp = '+layer.raot,1,1) then dmaj = temp else dmaj = 0d
                                if execute('temp = '+layer.decot,1,1) then dmin = temp else dmin = 0d
                            END
                            ELSE:
                        ENDCASE
                        angle = double(layer.aorient)
                        ROTATEELLIPSE, dmaj, dmin, angle, yc
                        TVELLIPSE, dmaj/2, dmin/2, xc, yc, angle, /DATA, $
                            COLOR = 0, THICK = linethick, LINESTYLE = linestyle, FILL=layer.parameter[0]
                    END
                    
                    'RECTANGLE':BEGIN
                        xc = x1
                        yc = y1
                        CASE layer.decu OF
                            'Normalized(%)':BEGIN
                                w1 = (plotrange[1]-plotrange[0])*double(layer.raot)/100.*cos(yc*!DTOR)
                                w2 = (plotrange[3]-plotrange[2])*double(layer.decot)/100.
                            END
                            'Absolute(WCS)':BEGIN
                                if execute('temp = '+layer.raot,1,1) then w1 = temp else w1 = 0d
                                if execute('temp = '+layer.decot,1,1) then w2 = temp else w2 = 0d
                            END
                            ELSE:
                        ENDCASE
                        w1obs = abs(w1)
                        w2obs = abs(w2)
                        aobs = double(layer.aorient)
                        w1wcs = w1obs*sqrt(sin(aobs*!DTOR)^2+cos(aobs*!DTOR)^2/cos(yc*!DTOR)^2)
                        w2wcs = w2obs*sqrt(sin(aobs*!DTOR)^2+cos(aobs*!DTOR)^2/cos(yc*!DTOR)^2)
                        awcs = atan(tan(aobs*!DTOR)*cos(yc*!DTOR))/!DTOR
                        w1x = cos(awcs*!DTOR)*w1wcs/2
                        w1y = sin(awcs*!DTOR)*w1wcs/2
                        w2x = cos(awcs*!DTOR)*w2wcs/2
                        w2y = sin(awcs*!DTOR)*w2wcs/2
                        IF ~layer.parameter[0] THEN $
                            OPLOT, [xc+w1x+w2y/cos(yc*!DTOR), xc+w1x-w2y/cos(yc*!DTOR), xc-w1x-w2y/cos(yc*!DTOR), $
			    xc-w1x+w2y/cos(yc*!DTOR), xc+w1x+w2y/cos(yc*!DTOR)], $
                            [yc+w1y-w2x*cos(yc*!DTOR), yc+w1y+w2x*cos(yc*!DTOR), yc-w1y+w2x*cos(yc*!DTOR), $
			    yc-w1y-w2x*cos(yc*!DTOR), yc+w1y-w2x*cos(yc*!DTOR)], $
                            COLOR = 0, THICK = linethick, LINESTYLE = linestyle, /NOCLIP $
                            ELSE $
                            POLYFILL, [xc+w1x+w2y/cos(yc*!DTOR), xc+w1x-w2y/cos(yc*!DTOR), xc-w1x-w2y/cos(yc*!DTOR), $
			    xc-w1x+w2y/cos(yc*!DTOR), xc+w1x+w2y/cos(yc*!DTOR)], $
                            [yc+w1y-w2x*cos(yc*!DTOR), yc+w1y+w2x*cos(yc*!DTOR), yc-w1y+w2x*cos(yc*!DTOR), $
			    yc-w1y-w2x*cos(yc*!DTOR), yc+w1y-w2x*cos(yc*!DTOR)], $
                            COLOR = 0, /NOCLIP
                        ;TVBOX, [w1, w2], xc, yc, /DATA, COLOR = 0, ANGLE = awcs, THICK = linethick
                    END
                    
                    ELSE:
                ENDCASE
            END;shape
            
            'PANEL':BEGIN
		savep = !p
		savex = !x
		savey = !y
		savez = !z
		CASE layer.type OF
		    'AIC':BEGIN
			panel = AICer_openaic(layer.file[0])	;read a .aic file
	                IF size(panel, /TYPE) ne 8 THEN BEGIN
		            CASE panel OF
                                0:PRINT,'Error - AIC file does not exist in layer: '+layer.uname
                                1:PRINT,'Error - AIC file is not readable in layer: '+layer.uname
                                2:PRINT,'Error - AIC file does not contain an AICer layer in layer: '+layer.uname
			        ELSE:
		            ENDCASE
		            BREAK
		        ENDIF
		        AICer_main, panel, singlepanel = 0, $
			    pageoffset = [float(layer.ra),float(layer.dec)] *pagescale+pageoffset, $
			    pagescale = layer.times *pagescale
		    END
		    'PRO':BEGIN
		        IF layer.file[1] EQ '' THEN BEGIN	;no pro name
		            PRINT,'Error - Procedure name is not given in layer: '+layer.uname
			    BREAK
		        ENDIF
		        IF total(strcmp(layer.file[1], routine_info(), /FOLD_CASE)) GE 1 THEN BEGIN	;name is a procedure
		    	    CATCH, error
			    IF error NE 0 THEN BEGIN
			        PRINT,'Error - Procedure running error in layer: '+layer.uname
			        PRINT,'      - '+!ERROR_STATE.MSG
			        CATCH, /CANCEL
			        BREAK
			    ENDIF
			    CALL_PROCEDURE,layer.file[1]
			    BREAK
		        ENDIF
		        IF total(strcmp(layer.file[1], routine_info(/function), /FOLD_CASE)) THEN BEGIN	;name is a function
		            CATCH, error
			    IF error NE 0 THEN BEGIN
			        PRINT,'Error - Function running error in layer: '+layer.uname
			        PRINT,'      - '+!ERROR_STATE.MSG
			        CATCH, /CANCEL
			        BREAK
			    ENDIF
			    panelresult = CALL_FUNCTION(layer.file[1])
			    BREAK
		        ENDIF
		        print,'Error - Procedure/Function is undefined in layer: '+layer.uname	;name is undefined
		    END
		    ELSE:
		ENDCASE
		!p = savep
		!x = savex
		!y = savey
		!z = savez
	    END;panel
            
	    ELSE:
        ENDCASE
    ENDFOR
    
    ;colorplot
    
    ;draw the axis formally
    TVLCT, axiscolor
    
    IF axisthick[0] GT 0 THEN BEGIN	;line
        PLOT, [0], /YNOZERO, /NODATA, /NOERASE, $
            XRANGE = axisRArange, YRANGE = axisDECrange, $
            ;XMARGIN = pagemargin[0:1], YMARGIN = pagemargin[2:3], $
        XSTYLE = 1, YSTYLE = 1, $
            XTICKS = axisRAticks, YTICKS = axisDECticks, $
            XTICKV = axisRAtickv, YTICKV = axisDECtickv, $
            XMINOR = axisRAminor, YMINOR = axisDECminor, $
            XTICKNAME = replicate(' ', n_elements(axisRAtickname)), $
            YTICKNAME = replicate(' ', n_elements(axisDECtickname)), $
            XTICKLEN = axisxticklen, YTICKLEN = axisyticklen, $
            POSITION = axisposition, $
            XTHICK = axisthick[0], YTHICK = axisthick[0], COLOR = 0
    ENDIF
    
    IF axisthick[1] GT 0 THEN BEGIN	;grid
        FOR i = 0, n_elements(axisRAtickv)-1 do $
            OPLOT, [axisRAtickv[i],axisRAtickv[i]], axisDECrange, $
            THICK = axisthick[1], COLOR = 1, linestyle = 1
        FOR i = 0, n_elements(axisDECtickv)-1 do $
            OPLOT, axisRArange, [axisDECtickv[i],axisDECtickv[i]], $
            THICK = axisthick[1], COLOR = 1, linestyle = 1
    ENDIF
    
    IF axisthick[2] GT 0 AND axissize[2] GT 0 THEN BEGIN	;number
        PLOT, [0], /YNOZERO, /NODATA, /NOERASE, $
            XRANGE = axisRArange, YRANGE = axisDECrange, $
            ;XMARGIN = pagemargin[0:1], YMARGIN = pagemargin[2:3], $
        XSTYLE = 1, YSTYLE = 1, $
            XTICKS = axisRAticks, YTICKS = axisDECticks, $
            XTICKV = axisRAtickv, YTICKV = axisDECtickv, $
            XMINOR = axisRAminor, YMINOR = axisDECminor, $
            XTICKLAYOUT = 1, YTICKLAYOUT = 1, $
            XTICKNAME = axisRAtickname, YTICKNAME = axisDECtickname, $
            XTICKLEN = axisxticklen, YTICKLEN = axisyticklen, $
            POSITION = axisposition, $
            CHARSIZE = axissize[2], CHARTHICK = axisthick[2], COLOR = 2
    ENDIF
    
    IF axisthick[3] GT 0 AND axissize[3] GT 0 THEN BEGIN	;label
        xcen = axisposition[0]+(axisposition[2]-axisposition[0])/2
        ylbl = axisposition[1]-incharsize[1]*((1+2*linespace)*axissize[2]+axissize[3])/(!d.y_size/1000.)
        XYOUTS, xcen, ylbl, xlabel, /NORMAL, ALIGN = 0.5, $
            CHARSIZE = axissize[3], CHARTHICK = axisthick[3], COLOR = 3
        xlbl = axisposition[0]-(incharsize[0]*(DEClen+(DEClen+1)*wordspace)*axissize[2]+incharsize[1]*linespace*axissize[3])/(!d.x_size/1000.)
        ycen = axisposition[1]+(axisposition[3]-axisposition[1])/2
        XYOUTS, xlbl, ycen, ylabel, /NORMAL, ALIGN = 0.5, ORIENTATION = 90, $
            CHARSIZE = axissize[3], CHARTHICK = axisthick[3], COLOR = 3
    ENDIF
    ;PLOT, [0], /YNOZERO, /NODATA, /NOERASE, $
    ;	XRANGE = axisRArange, YRANGE = axisDECrange, $
    ;	XMARGIN = pagemargin[0:1], YMARGIN = pagemargin[2:3], $
    ;	XSTYLE = 1, YSTYLE = 1, $
    ;	XTICKS = axisRAticks, YTICKS = axisDECticks, $
    ;	XTICKV = axisRAtickv, YTICKV = axisDECtickv, $
    ;	XMINOR = 1, YMINOR = 1, $
    ;	XTICKNAME = replicate(' ', n_elements(axisRAtickname)), $
    ;	YTICKNAME = replicate('    ', n_elements(axisDECtickname)), $
    ;	XTICKLAYOUT = 1, YTICKLAYOUT = 1, $
    ;	XTITLE = xlabel, YTITLE = ylabel, $
    ;	POSITION = axisposition, $
    ;	CHARSIZE = labesize, CHARTHICK = axisthick[3], COLOR = 3

    IF axisthick[4] GT 0 AND axissize[4] GT 0 THEN BEGIN	;title
        PLOT, [0], /YNOZERO, /NODATA, /NOERASE, $
            XRANGE = axisRArange, YRANGE = axisDECrange, $
            ;XMARGIN = pagemargin[0:1], YMARGIN = pagemargin[2:3], $
	    XSTYLE = 5, YSTYLE = 5, $
            POSITION = axisposition, $
            TITLE = axistitle, $
            CHARSIZE = axissize[4], CHARTHICK = axisthick[4], COLOR = 4
    ENDIF

    ;Draw panels on the top of the plot list
    IF firstnotpanel ge 1 THEN BEGIN
	FOR layernum = firstnotpanel-1, 0, -1 DO BEGIN
            layer = layerlist[layernum]	
	    IF layer.valid EQ 2 THEN CONTINUE
	    savep = !p
	    savex = !x
	    savey = !y
	    savez = !z
	    CASE layer.type OF
		'AIC':BEGIN
		    panel = AICer_openaic(layer.file[0])	;read a .aic file
	            IF size(panel, /TYPE) ne 8 THEN BEGIN
		        CASE panel OF
                            0:PRINT,'Error - AIC file does not exist in layer: '+layer.uname
                            1:PRINT,'Error - AIC file is not readable in layer: '+layer.uname
                            2:PRINT,'Error - AIC file does not contain an AICer layer in layer: '+layer.uname
			    ELSE:
		        ENDCASE
		        BREAK
		    ENDIF
		    AICer_main, panel, singlepanel = 0, $
			pageoffset = [float(layer.ra),float(layer.dec)] *pagescale+pageoffset, $
			pagescale = layer.times *pagescale
		END
		'PRO':BEGIN
		    IF layer.file[1] EQ '' THEN BEGIN	;no pro name
		        PRINT,'Error - Procedure name is not given in layer: '+layer.uname
			BREAK
		    ENDIF
		    IF total(strcmp(layer.file[1], routine_info(), /FOLD_CASE)) GE 1 THEN BEGIN	;name is a procedure
		    	CATCH, error
			IF error NE 0 THEN BEGIN
			    PRINT,'Error - Procedure running error in layer: '+layer.uname
			    PRINT,'      - '+!ERROR_STATE.MSG
			    CATCH, /CANCEL
			    BREAK
			ENDIF
			CALL_PROCEDURE,layer.file[1]
			BREAK
		    ENDIF
		    IF total(strcmp(layer.file[1], routine_info(/function), /FOLD_CASE)) THEN BEGIN	;name is a function
		        CATCH, error
			IF error NE 0 THEN BEGIN
			    PRINT,'Error - Function running error in layer: '+layer.uname
			    PRINT,'      - '+!ERROR_STATE.MSG
			    CATCH, /CANCEL
			    BREAK
			ENDIF
			panelresult = CALL_FUNCTION(layer.file[1])
			BREAK
		    ENDIF
		    print,'Error - Procedure/Function is undefined in layer: '+layer.uname	;name is undefined
		END
		ELSE:
	    ENDCASE
	    !p = savep
	    !x = savex
	    !y = savey
	    !z = savez
	ENDFOR
    ENDIF
;    FOR layernum = n_elements(layerlist)-2, 0, -1 DO BEGIN
;        layer = layerlist[layernum]
;        IF layer.valid EQ 2 THEN CONTINUE
        ;	RAt = strsplit(layer.ra, ':', /EXTRACT)
        ;	DECt = strsplit(layer.dec, ':', /EXTRACT)
        ;	IF n_elements(RAt) EQ 3 THEN RAcen = ten(RAt[0], RAt[1], RAt[2])*15 ELSE RAcen = double(layer.ra)
        ;	IF n_elements(DECt) EQ 3 THEN DECcen = ten(DECt[0], DECt[1], DECt[2]) ELSE DECcen = double(layer.dec)
;        RAcen = str2ang(layer.ra, /RA)
;        DECcen = str2ang(layer.dec, /DEC)
        
        
;        CASE layer.name OF
 
    
    IF singlepanel THEN BEGIN
        DEVICE, /CLOSE
        SET_PLOT, savescreen
    ENDIF
END
;--------------------------------------------------------------------------------------------------



PRO AICer_event, event
    
    WIDGET_CONTROL, event.top, GET_UVALUE = layerlist, /NO_COPY
    
    WIDGET_CONTROL, event.id, GET_UVALUE = layeraction
    ;print, event, layeraction
    
    CASE layeraction OF
        
        'WCS':BEGIN
            !layercache = layerlist[n_elements(layerlist)-1]
            !layercache.parameter[1] = event.index
            CASE event.index OF
                0:BEGIN
                    !layercache.texti = 'Right Ascension (J2000)'
                    !layercache.texto = 'Declination (J2000)'
                    !layercache.aorient = '[I0]!Uh!N[I0]!Um!N[F0.1]!Us!N%'+"[I02]!Uo!N[I02]'"+'[F04.1]"'
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 2
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 2
                END
                1:BEGIN
                    !layercache.texti = 'Galactic Longitude'
                    !layercache.texto = 'Galactic Latitude'
                    !layercache.aorient = '[F0.1]%[F0.1]'
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 2
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 2
                END
                2:BEGIN
                    !layercache.texti = ''
                    !layercache.texto = ''
                    !layercache.aorient = '[F0.2]%[F0.2]'
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 3
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 3
                END
                ELSE:
            ENDCASE
            layerlist[n_elements(layerlist)-1] = !layercache
        END
        
        ;open catalog file and put source info into source droplist
        'CATALOG':BEGIN
            cpath = file_dirname(layerlist[n_elements(layerlist)-1].file[0])
            cfile = dialog_pickfile(TITLE = 'Select Catalog File', $
                FILTER = [['*.sou', '*.cat', '*.*'], ['Source Files (*.sou)', 'Catalog Files (*.cat)', 'All Files (*.*)']], $
                PATH = cpath)
            IF cfile NE '' THEN BEGIN
                layerlist[n_elements(layerlist)-1].file[0] = cfile
                cdata = rd_tfile(cfile);if none , NOCOMMENT = '!'
                AICer_fillblank, event, cdata[0]
                cdata = gettok(cdata,' ')
                ;FOR i=0,n_elements(cdata)-1 DO cdata[i] = (strsplit(cdata[i], /EXTRACT))[0]+' '
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SET_VALUE = cdata
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SENSITIVE = 1
            ENDIF
        END
        
        'SOURCE':BEGIN
            IF widget_info(event.id, /COMBOBOX_NUMBER) GT 1 THEN BEGIN
                IF layerlist[n_elements(layerlist)-1].file[0] NE '' THEN BEGIN
                    cdata = rd_tfile(layerlist[n_elements(layerlist)-1].file[0])
                    ind = where(strpos(cdata, event.str) GE 0)
                    IF ind[0] NE -1 THEN AICer_fillblank, event, cdata[ind[0]]
                ENDIF
            ENDIF
        END
        
        ;	'LOCK':BEGIN
        ;		WIDGET_CONTROL, event.id, GET_VALUE = state
        ;		IF state EQ 'L' THEN BEGIN
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wcata'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SENSITIVE = 1
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SENSITIVE = 1
        ;			WIDGET_CONTROL, event.id, SET_VALUE = ' '
        ;		ENDIF ELSE BEGIN
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wcata'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SENSITIVE = 0
        ;			WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SENSITIVE = 0
        ;			WIDGET_CONTROL, event.id, SET_VALUE = 'L'
        ;		ENDELSE
        ;	END
        
        'LIST':BEGIN
            IF event.clicks EQ 2 THEN BEGIN
                select = event.index
                IF select GE 0 AND select LE n_elements(layerlist)-2 THEN BEGIN
                    ;WIDGET_CONTROL, event.id, SET_LIST_SELECT = select
                    !layercache = layerlist[select]
                    CASE !layercache.name OF
                        'IMAGE': AICer_image, event.top
                        'CONTOUR': AICer_contour, event.top
                        'POINT': AICer_point, event.top
                        'TEXT': AICer_text, event.top
                        'SHAPE': AICer_shape, event.top
			'PANEL': AICer_panel, event.top
                        ELSE:
                    ENDCASE
                    IF !layercache.valid GE 1 THEN layerlist[select] = !layercache
                    listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                    hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                    IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                    WIDGET_CONTROL, event.id, SET_VALUE = listvalue
                    WIDGET_CONTROL, event.id, SET_LIST_SELECT = select
                ENDIF
            ENDIF
        END
        
        'UP':BEGIN
            listid = widget_info(Event.top, FIND_BY_UNAME = 'wlist')
            select = (widget_info(listid, /LIST_SELECT))[0]
            IF min(select) GT 0 AND max(select) LE n_elements(layerlist)-2 THEN BEGIN
                templayer = layerlist[select]
                layerlist[select] = layerlist[select-1]
                layerlist[select-1] = templayer
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, listid, SET_VALUE = listvalue
                WIDGET_CONTROL, listid, SET_LIST_SELECT = select-1
            ENDIF
        END
        
        'DOWN':BEGIN
            listid = widget_info(Event.top, FIND_BY_UNAME = 'wlist')
            select = (widget_info(listid, /LIST_SELECT))[0]
            IF select GE 0 AND select LE n_elements(layerlist)-3 THEN BEGIN
                templayer = layerlist[select]
                layerlist[select] = layerlist[select+1]
                layerlist[select+1] = templayer
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, listid, SET_VALUE = listvalue
                WIDGET_CONTROL, listid, SET_LIST_SELECT = select+1
            ENDIF
        END
        
        'IMAGE':BEGIN
            ;newlayer = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'IMAGE', count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'IMAGE'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'IMAGE'
            !layercache.type = 'SINGLE'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
            !layercache.ra = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
            !layercache.dec = temp
            ;    	WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), GET_VALUE = temp
            ;    	!layercache.raof = temp
            ;    	WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), GET_VALUE = temp
            ;    	!layercache.raot = temp
            ;    	WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), GET_VALUE = temp
            ;    	!layercache.decof = temp
            ;    	WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), GET_VALUE = temp
            ;    	!layercache.decot = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), GET_VALUE = temp
            !layercache.rau = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wrau'), /DROPLIST_SELECT)]
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), GET_VALUE = temp
            !layercache.decu = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wdecu'), /DROPLIST_SELECT)]
            !layercache.cra = -1
            !layercache.cdec = -1
            !layercache.cvalue = -1
            !layercache.cinter = -2
            !layercache.times = 10.0
            !layercache.ctrust = -1
            !layercache.texti = '1 to 10'
            !layercache.texto = '      1.00000       2.00000       3.00000       4.00000       5.00000       6.00000       7.00000       8.00000       9.00000       10.0000'
            !layercache.afont = ''
            !layercache.asize = '1.0 1.0'	;point, label
            !layercache.athick = '2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0'
            !layercache.dthick = 2.00
            !layercache.dsmooth = 0
            !layercache.bval = [0,0,0,0,0,0]
            !layercache.ct[0,*] = 255
            !layercache.parameter = [0,0,0,0,0,0,-1,0]
            AICer_image, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
                WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'New image layer added'
            ENDIF
        END
        
        'CONTOUR':BEGIN
            ;!layercache = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'CONTOUR', count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'CONTOUR'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'CONTOUR'
            !layercache.type = 'OFF'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
            !layercache.ra = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
            !layercache.dec = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), GET_VALUE = temp
            !layercache.raof = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), GET_VALUE = temp
            !layercache.raot = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), GET_VALUE = temp
            !layercache.decof = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), GET_VALUE = temp
            !layercache.decot = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), GET_VALUE = temp
            !layercache.rau = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wrau'), /DROPLIST_SELECT)]
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), GET_VALUE = temp
            !layercache.decu = temp[widget_info(widget_info(event.top, FIND_BY_UNAME = 'wdecu'), /DROPLIST_SELECT)]
            !layercache.cra = 1
            !layercache.cdec = 2
            !layercache.cvalue = 3
            !layercache.cinter = -2
            !layercache.times = 10.0
            !layercache.ctrust = -1
            !layercache.texti = '1 to 10'
            !layercache.texto = '      1.00000       2.00000       3.00000       4.00000       5.00000       6.00000       7.00000       8.00000       9.00000       10.0000'
            !layercache.length = 30.0
            !layercache.Bval = [0.0,0.0]
            !layercache.dsmooth = 0
            !layercache.lenu = 'arcsec'
            !layercache.afont = 'Plus sign(+)'
            !layercache.asize = '1.0 1.0'	;point, label
            !layercache.athick = '2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0'
            !layercache.dthick = 2.00
            !layercache.parameter = [-1,0,0,0,0,0,1,0,0]
            AICer_contour, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
                WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'New contour layer added'
            ENDIF
        END
        
        'POINT':BEGIN
            ;newlayer = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'POINT', count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'POINT'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'POINT'
            !layercache.type = 'SINGLE'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
            !layercache.ra = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
            !layercache.dec = temp
            !layercache.raof = 'OFF'
            !layercache.raot = 'OFF'
            !layercache.decof = 'OFF'
            !layercache.decot = 'OFF'
            !layercache.rau = 'absolute'
            !layercache.decu = 'absolute'
            !layercache.cra = 1
            !layercache.cdec = 2
            !layercache.cvalue = 3
            !layercache.cinter = 0
            !layercache.cerror = 0
            !layercache.ctrust = 0
            !layercache.texto = ''
            !layercache.afont = 'Plus sign(+)'
            !layercache.asize = '1.0'
            !layercache.athick = '1.0'
            !layercache.aorient = '1.0'
            !layercache.ct = 0
            !layercache.dsmooth = -1
            !layercache.parameter = [0,0,0]
            AICer_point, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
                WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'New point layer added'
            ENDIF
        END
        
        'TEXT':BEGIN
            ;newlayer = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'TEXT',count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'TEXT'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'TEXT'
            !layercache.type = 'TITLE'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
            !layercache.ra = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
            !layercache.dec = temp
            !layercache.raof = '50'
            !layercache.raot = '-1'
            !layercache.decof = '50'
            !layercache.decot = '-1'
            !layercache.rau = 'OFF'
            !layercache.decu = 'OFF'
            !layercache.cra = -1
            !layercache.cdec = -1
            !layercache.cvalue = -1
            !layercache.cinter = -1
            !layercache.ctrust = -1
            !layercache.texti = ''
            !layercache.texto = ''
            !layercache.afont = '!3'
            !layercache.asize = '1.0'
            !layercache.athick = '1.0'
            !layercache.aorient = '0'
            !layercache.ct = 0
            !layercache.dsmooth = -1
            AICer_text, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
                WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'New text layer added'
            ENDIF
        END
        
        'SHAPE':BEGIN
            ;newlayer = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'SHAPE',count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'SHAPE'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'SHAPE'
            !layercache.type = 'LINE'
            !layercache.file = ['','','']
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), GET_VALUE = temp
            !layercache.ra = temp
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), GET_VALUE = temp
            !layercache.dec = temp
            !layercache.raof = '0'
            !layercache.raot = '100'
            !layercache.decof = '0'
            !layercache.decot = '100'
            !layercache.rau = 'Normalized(%)'
            !layercache.decu = 'Normalized(%)'
            !layercache.cra = -1
            !layercache.cdec = -1
            !layercache.cvalue = -1
            !layercache.cinter = -1
            !layercache.ctrust = -1
            !layercache.afont = ''
            !layercache.asize = '1.0'
            !layercache.athick = '1.0'
            !layercache.aorient = '0.0'
            !layercache.ct = 0
            !layercache.dsmooth = -1
            !layercache.parameter = [0,0]
            AICer_shape, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = (layerlist.uname)[0:n_elements(layerlist)-2]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = 0
            ENDIF
        END
       
	'PANEL':BEGIN
            ;newlayer = layerlist[n_elements(layerlist)-1]
            temp = where(layerlist.name EQ 'PANEL',count)
            !layercache = layer_default(!layercache)
            !layercache.valid = 0
            !layercache.uname = 'PANEL'+string(count+1, FORMAT = '(i3)')
            !layercache.name = 'PANEL'
            !layercache.type = 'AIC'
            !layercache.file = ['','','']
            !layercache.ra = '0.0'
            !layercache.dec = '0.0'
	    !layercache.times = 1.0
            !layercache.rau = 'Normalized(%)'
            !layercache.decu = 'Normalized(%)'
	    AICer_panel, event.top
            IF !layercache.valid THEN BEGIN
                layerlist = [!layercache,layerlist]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = (layerlist.uname)[0:n_elements(layerlist)-2]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = 0
            ENDIF
        END
 
        'EDIT':BEGIN
            select = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'wlist'), /LIST_SELECT))[0]
            IF select GE 0 AND select LE n_elements(layerlist)-2 THEN BEGIN
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
                !layercache = layerlist[select]
                CASE !layercache.name OF
                    'IMAGE': AICer_image, event.top
                    'CONTOUR': AICer_contour, event.top
                    'POINT': AICer_point, event.top
                    'TEXT': AICer_text, event.top
                    'SHAPE': AICer_shape, event.top
		    'PANEL': AICer_panel, event.top
                    ELSE:
                ENDCASE
                IF !layercache.valid GE 1 THEN layerlist[select] = !layercache
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
            ENDIF
        END
        
        'DELETE':BEGIN
            IF n_elements(layerlist) GT 1 THEN BEGIN
                IF dialog_message('Confirm layer delete?', /QUESTION) EQ 'Yes' THEN BEGIN
                    select = widget_info(widget_info(Event.top, FIND_BY_UNAME = 'wlist'), /LIST_SELECT)
                    IF select[0] EQ -1 THEN BREAK
                    list = indgen(n_elements(layerlist))
                    FOR i = n_elements(select)-1,0,-1 DO BEGIN
                        complement = where(list NE select[i])
                        IF complement[0] EQ -1 THEN BREAK
                        list = list[complement]
                    ENDFOR
                    layerlist = layerlist[list]
                    IF n_elements(layerlist) GT 1 THEN BEGIN
                        listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                        hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                        IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                    ENDIF ELSE BEGIN
                        listvalue = ['']
                    ENDELSE
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                    WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select[0]
                ENDIF
            ENDIF
        END
        
        'HIDE':BEGIN
            IF n_elements(layerlist) GT 1 THEN BEGIN
                select = widget_info(widget_info(event.top, FIND_BY_UNAME = 'wlist'), /LIST_SELECT)
                FOR i=0,n_elements(select)-1 DO $
                    IF select[i] GE 0 AND select[i] LE n_elements(layerlist)-2 THEN $
                    layerlist[select[i]].valid = (layerlist[select[i]].valid mod 2) +1
                
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
            ENDIF
        END
        
        'COPY':BEGIN
            select = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'wlist'), /LIST_SELECT))[0]
            IF min(select) GE 0 AND max(select) LE n_elements(layerlist)-2 THEN BEGIN
                !layercopy = layerlist[select]
            ENDIF
        END
        
        'PASTE':BEGIN
            IF !layercopy.valid GT 0 THEN BEGIN
                select = (widget_info(widget_info(event.top, FIND_BY_UNAME = 'wlist'), /LIST_SELECT))[0]
                ;print,select
                IF select GT 0 THEN BEGIN
                    layerlist = [layerlist[0:select-1],!layercopy, layerlist[select:*]]
                ENDIF ELSE BEGIN
                    layerlist = [!layercopy, layerlist]
                ENDELSE
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
            ENDIF
        END
        
        'CUSTOM':BEGIN
            !layercache = layerlist[n_elements(layerlist)-1]
            !layercache.valid = 0
            AICer_custom, event.top
            IF !layercache.valid THEN layerlist[n_elements(layerlist)-1] = !layercache
        END
        
        'NEW':BEGIN
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wwcs'), SET_DROPLIST_SELECT = 0
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), SET_VALUE = ''
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SET_VALUE = ['SourceName'], SENSITIVE=0
            ;AICer_fillblank, event
            !layercache = layer_default(!layercache)
            layerlist = !layercache
	    layerlist.type='A4'
            layerlist.Bval = [0.0d,0.0d,-1.0d,-1.0d,0.0d,0.0d]
            layerlist.dsmooth = 24
            layerlist.asize = '0 0 1 1 1'
            layerlist.athick = '1 1 1 1 1'
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = ['']
            WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'Welcome to AICer!'
        END
        
        'SAVE':BEGIN
            layerlist[n_elements(layerlist)-1] = AICer_fillheader(event, layerlist[n_elements(layerlist)-1])
            sfile = dialog_pickfile(TITLE = 'Save AICer list file', FILTER = [['*.aic','*.*'],['AICer List File (*.aic)','All Files (*.*)']], DEFAULT_EXTENSION = 'aic', /WRITE)
            IF file_test(sfile) THEN overwrite = dialog_message([sfile+' already exists,', 'Overwrite?'], /QUESTION) ELSE overwrite = 'Yes'
            IF sfile EQ '' THEN overwrite = 'No'
            IF overwrite EQ 'Yes' THEN BEGIN
                ;            CLOSE, 1
                ;            OPENW, 1, sfile
                ;            PRINTF, 1, '>BEGIN<'
                ;            PRINTF, 1, ''
                ;            FOR i = n_elements(layerlist)-1,0,-1 DO BEGIN
                ;            	PRINTF, 1, '+LAYER+'
                ;            	PRINTF, 1, '[VALID]='
                ;            	PRINTF, 1, string(layerlist[i].valid, FORMAT = '(I1)')
                ;            	PRINTF, 1, '[USER NAME]='
                ;            	PRINTF, 1, layerlist[i].uname
                ;            	PRINTF, 1, '[SYSTEM NAME]='
                ;            	PRINTF, 1, layerlist[i].name
                ;            	PRINTF, 1, '[TYPE]='
                ;            	PRINTF, 1, layerlist[i].type
                ;            	PRINTF, 1, '[FILE NAME1]='
                ;            	PRINTF, 1, layerlist[i].file[0]
                ;            	PRINTF, 1, '[FILE NAME2]='
                ;            	PRINTF, 1, layerlist[i].file[1]
                ;            	PRINTF, 1, '[FILE NAME3]='
                ;            	PRINTF, 1, layerlist[i].file[2]
                ;            	PRINTF, 1, '[R.A.]='
                ;            	PRINTF, 1, layerlist[i].ra
                ;            	PRINTF, 1, '[DEC.]='
                ;            	PRINTF, 1, layerlist[i].dec
                ;            	PRINTF, 1, '[R.A. OFFSET FROM]='
                ;            	PRINTF, 1, layerlist[i].raof
                ;            	PRINTF, 1, '[R.A. OFFSET TO]='
                ;            	PRINTF, 1, layerlist[i].raot
                ;            	PRINTF, 1, '[DEC. OFFSET FROM]='
                ;            	PRINTF, 1, layerlist[i].decof
                ;            	PRINTF, 1, '[DEC. OFFSET TO]='
                ;            	PRINTF, 1, layerlist[i].decot
                ;            	PRINTF, 1, '[R.A. UNIT]='
                ;            	PRINTF, 1, layerlist[i].rau
                ;            	PRINTF, 1, '[DEC. UNIT]='
                ;            	PRINTF, 1, layerlist[i].decu
                ;            	PRINTF, 1, '[TEXT INPUT]='
                ;            	PRINTF, 1, layerlist[i].texti
                ;            	PRINTF, 1, '[TEXT OUTPUT]='
                ;            	PRINTF, 1, layerlist[i].texto
                ;            	PRINTF, 1, '[COLUMN OF R.A.]='
                ;            	PRINTF, 1, string(layerlist[i].cra, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[COLUMN OF DEC.]='
                ;            	PRINTF, 1, string(layerlist[i].cdec, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[COLUMN OF VALUE]='
                ;            	PRINTF, 1, string(layerlist[i].cvalue, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[COLUMN OF INTERVAL]='
                ;            	PRINTF, 1, string(layerlist[i].cinter, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[COLUMN OF ERROR]='
                ;            	PRINTF, 1, string(layerlist[i].cerror, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[TIMES]='
                ;            	PRINTF, 1, strtrim(string(layerlist[i].times),2)
                ;            	PRINTF, 1, '[COLUMN OF TRUST]='
                ;            	PRINTF, 1, string(layerlist[i].ctrust, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[LENGTH]='
                ;            	PRINTF, 1, strtrim(string(layerlist[i].length),2)
                ;            	PRINTF, 1, '[LENGTH UNIT]='
                ;            	PRINTF, 1, layerlist[i].lenu
                ;            	PRINTF, 1, '[BLANKING VALUE]='
                ;            	PRINTF, 1, strtrim(string(layerlist[i].Bval),2)
                ;            	PRINTF, 1, '[MISSING VALUE]='
                ;            	PRINTF, 1, strtrim(string(layerlist[i].missing),2)
                ;            	PRINTF, 1, '[SMOOTH DEGREE]='
                ;            	PRINTF, 1, string(layerlist[i].dsmooth, FORMAT = '(I3)')
                ;            	PRINTF, 1, '[FONT]='
                ;            	PRINTF, 1, layerlist[i].afont
                ;            	PRINTF, 1, '[SIZE]='
                ;            	PRINTF, 1, layerlist[i].asize
                ;            	PRINTF, 1, '[THICK]='
                ;            	PRINTF, 1, layerlist[i].athick
                ;            	PRINTF, 1, '[ORIENTATION]='
                ;		PRINTF, 1, layerlist[i].aorient
                ;            	PRINTF, 1, '[DEFAULT THICK]='
                ;            	PRINTF, 1, strtrim(string(layerlist[i].dthick),2)
                ;    			PRINTF, 1, '[PARAMETER]='
                ;    			PRINTF, 1, string(layerlist[i].parameter, FORMAT = '(I3)')
                ;				PRINTF, 1, '[COLOR TABLE]='
                ;            	FOR j = 0, 15 DO PRINTF, 1, string(layerlist[i].ct[j*16:j*16+15,0], FORMAT = '(I03)')
                ;            	FOR j = 0, 15 DO PRINTF, 1, string(layerlist[i].ct[j*16:j*16+15,1], FORMAT = '(I03)')
                ;            	FOR j = 0, 15 DO PRINTF, 1, string(layerlist[i].ct[j*16:j*16+15,2], FORMAT = '(I03)')
                ;				PRINTF, 1, '[PERSONAL CT]='
                ;            	PRINTF, 1, layerlist[i].pct
                ;            	PRINTF, 1, '-LAYER-'
                ;            	PRINTF, 1, ''
                ;            ENDFOR
                ;           	PRINTF, 1, '>END<'
                ;            CLOSE, 1
                save,layerlist,filename=sfile
                WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'Saved: '+file_basename(sfile)
            ENDIF
        END
        
        'OPEN':BEGIN
            lfile = dialog_pickfile(TITLE = 'Open AICer list file', FILTER = [['*.aic','*.*'],['AICer List File (*.aic)','All Files (*.*)']], /MUST_EXIST)
            IF lfile EQ '' THEN BREAK	;close dialog without select any file, then lfile = ''
	    newlist = AICer_openaic(lfile)	;read a .aic file
	    IF size(newlist, /TYPE) ne 8 THEN BEGIN
	        CASE newlist OF
                    0:PRINT,'Error - AIC file does not exist.'
                    1:PRINT,'Error - AIC file is not readable.'
                    2:PRINT,'Error - AIC file does not contain an AICer layer.'
		    ELSE:
		ENDCASE
		BREAK
	    ENDIF
            IF n_elements(newlist) LE 0 THEN newlist = !layercache	;empty aic file
            layerlist = newlist
            !layercache = layerlist[n_elements(layerlist)-1]
                
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wwcs'), SET_DROPLIST_SELECT = !layercache.parameter[1]
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wra'), SET_VALUE = !layercache.ra
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdec'), SET_VALUE = !layercache.dec
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraof'), SET_VALUE = !layercache.raof
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wraot'), SET_VALUE = !layercache.raot
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecof'), SET_VALUE = !layercache.decof
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecot'), SET_VALUE = !layercache.decot
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wsou'), SET_VALUE = (!layercache.uname EQ '')?['SourceName']:[(strsplit(!layercache.uname, /EXTRACT))[0]]
            CASE !layercache.rau OF
                'arcsec':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 0
                'arcmin':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 1
                'degree':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 2
                ELSE:WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wrau'), SET_DROPLIST_SELECT = 3
            ENDCASE
            CASE !layercache.decu OF
                'arcsec':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 0
                'arcmin':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 1
                'degree':WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 2
                ELSE:WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wdecu'), SET_DROPLIST_SELECT = 3
            ENDCASE
            IF n_elements(newlist) GE 2 THEN BEGIN
                listvalue = (layerlist.uname)[0:n_elements(layerlist)-2]
                hidelayer = where((layerlist.valid)[0:n_elements(layerlist)-2] EQ 2)
                IF hidelayer[0] NE -1 THEN listvalue[hidelayer] = '[Hide] '+listvalue[hidelayer]
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = listvalue
                WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_LIST_SELECT = select
            ENDIF ELSE WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wlist'), SET_VALUE = ['']
            WIDGET_CONTROL, event.top, BASE_SET_TITLE = 'AICer - '+lfile
            WIDGET_CONTROL, widget_info(event.top, FIND_BY_UNAME = 'wtime'), SET_VALUE = 'Loaded: '+file_basename(lfile)
        END
        
        'CLOSE':BEGIN
            WIDGET_CONTROL, event.top, /DESTROY
            RETURN
        END
        
        'CONTEXT':BEGIN
            XDISPLAYFILE, 'AICer Help', /MODAL, GROUP = event.top, TITLE='AICer Help', DONE_BUTTON = 'Close', $
                TEXT = ['What is this?',$
                '  AICer = Astronomical Image Compounder', $
                '  It can help you to overlay images in one plot.', $
                '', $
                'How to use?', $
                '  1.Select a coordinate system type you want to plot in the ', $
                '      "Plot Region" part.', $
                '    you can fill the blanks to define a plot range', $
                '    OR', $
                '    If you have a source catalog, press the "Catalog" button', $
                '    to load a source list file. The following blanks will be', $
                '    filled automatically', $
                '   *Tip: the little square button can be used to lock the blanks', $
                '  2.Layer is a layer of image to draw',$
                '    Use "Add" to create a new layer', $
                '    Use "Edit" to modify the selected layer', $
                '    Use "Hide" to hide a layer from plotting', $
                '    Use the arrow button to move the selected layer up and down', $
                '    Use "Copy" to copy the select layer into memory', $
                '    Use "Paste" to paste the layer in memory to the top of ', $
                '      the selected layer', $
                '    Use "Delete" to delete the selected layer', $
                '  3.Click "Draw" to output current layers to a PS file', $
                '    In "File" menu, you can "Save" current layers and settings', $
                '      or "Open" a previous saved one', $
                '    In "Setting" menu, you can set the output style', $
                '', $
                'Example:', $
                '  *Getting start', $
                '     Just press "Draw" to draw your first plot.', $
                '     Open the "AICer.ps" file to see the result.', $
                '  *Plot a FITS image', $
                '     Fill "Coordinate" blanks to define a plot region', $
                '     "Add" an "Image" layer', $
                '     Open a FITS file', $
                '     Press "Done" and then "Draw"', $
                '  *Overlay a contour of a FITS image', $
                '     "Add" an "Image" layer', $
                '     Open a FITS file', $
                '     Tick the box of "Convert image to contour" in image panel', $
                '     Press "Done" and then "Draw"', $
                '  *Overlay a contour of "X-Y-Value" table', $
                '     "Add" a "Contour" layer', $
                '     Open a ascii file', $
                '     Select column for x,y,value',$
                '     Press "Done" and then "Draw"', $
                '  *Mark sources on the plot', $
                '     "Add" a "Point" layer', $
                '     Open a catalog file', $
                '     Select column for coordinate',$
                '     Press "Done" and then "Draw"', $
                '', $
                'What is more?', $
                '  Thanks for using this procedure!', $
                '  If you want to report bugs or have any suggestion', $
                '  and comments, please feel free to contact me:', $
                '      shbzhang@pmo.ac.cn', $
                '      Room 625, Purple Mountain Observatory, CAS', $
                '      2 West Beijing Road, Nanjing 210008, China']
        END
        
        'ABOUT':BEGIN
            temp = dialog_message(['AICer','Version: beta0.2.17','','By Shaobo Zhang','shbzhang@pmo.ac.cn'], $
                TITLE = 'About AICer', /INFORMATION, /CENTER)
        END
        
        'DRAW':BEGIN
            WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'Drawing...'
            etime = systime(/SECOND)
            layerlist[n_elements(layerlist)-1] = AICer_fillheader(event, layerlist[n_elements(layerlist)-1])
            AICer_main, layerlist
            etime = strtrim(string(systime(/SECOND)-etime, FORMAT = '(f6.2)'),2)
            WIDGET_CONTROL, widget_info(event.top,FIND_BY_UNAME = 'wtime'), SET_VALUE = 'DONE in '+etime+'s!'
        END
        
        'FRAME':BEGIN
            MODULE_FRAME_EVENT, event
        END
        
        ELSE:
        
    ENDCASE
    
    WIDGET_CONTROL, event.top, SET_UVALUE = layerlist, /NO_COPY
    
END
;--------------------------------------------------------------------------------------------------



PRO AICer, aicfilename
    
    ;size of widget based on a single button widget and space between them
    DEFSYSV, '!x_width', 250	;width of widget
    DEFSYSV, '!status', 0	;for draw pixal distribution
    
    angle = findgen(4) * (!PI*2/3.)
    symbol = transpose([[sin(angle)], [cos(angle)]])
    DEFSYSV, '!Triangle', symbol
    
    angle = findgen(5) * (!PI*2/4.)
    symbol = transpose([[cos(angle)], [sin(angle)]])
    DEFSYSV, '!Diamond', symbol
    
    angle = findgen(5) * (!PI*2/4.)+!PI/4.
    symbol = transpose([[cos(angle)], [sin(angle)]])
    DEFSYSV, '!Square', symbol
    
    angle = findgen(11) * (!PI*2/10.)
    less = cos(2*!PI/5.)/cos(!PI/5.)
    symbol = transpose([[[1, less, 1, less, 1, less, 1, less, 1, less, 1]*sin(angle)], [[1, less, 1, less, 1, less, 1, less, 1, less, 1]*cos(angle)]])
    DEFSYSV, '!Pentagram', symbol
    
    angle = findgen(7) * (!PI*2/6.)
    symbol = transpose([[cos(angle)], [sin(angle)]])
    DEFSYSV, '!Hexagon', symbol
    
    angle = findgen(37) * (!PI*2/36.)
    symbol = transpose([[sin(angle)], [cos(angle)]])
    DEFSYSV, '!Circle', symbol
    
    ;execute without GUI
    IF n_params() EQ 1 THEN BEGIN
        aiclist = AICer_openaic(aicfilename)
	IF size(aiclist, /TYPE) ne 8 THEN BEGIN
	    CASE aiclist OF
                0:PRINT,'Error - AIC file does not exist.'
                1:PRINT,'Error - AIC file is not readable.'
                2:PRINT,'Error - AIC file does not contain an AICer layer.'
	    ELSE:
	    ENDCASE
	    RETURN
	ENDIF
	AICer_main, aiclist
        RETURN
    ENDIF

    ;GUI
    wbase = widget_base(TITLE = 'AICer', COLUMN = 1, MBAR = wmbar)
    
    ;mbar widget
    wfile = widget_button(wmbar, VALUE = 'File', /MENU)
    wnew = widget_button(wfile, VALUE = 'New', UVALUE = 'NEW', UNAME = 'wnew', ACCELERATOR = 'Ctrl+N')
    wopen = widget_button(wfile, VALUE = 'Open...', UVALUE = 'OPEN', UNAME = 'wopen', ACCELERATOR = 'Ctrl+O')
    wsave = widget_button(wfile, VALUE = 'Save', UVALUE = 'SAVE', UNAME = 'wsave', ACCELERATOR = 'Ctrl+S', /SEPARATOR)
    ;wsaveas = widget_button(wfile, VALUE = 'Save As...', UVALUE = 'SAVEAS', UNAME = 'wsaveas')
    wclose = widget_button(wfile, VALUE = 'Exit', UVALUE = 'CLOSE', UNAME = 'wclose', ACCELERATOR = 'Ctrl+X', /SEPARATOR)
    
    ;wlayer = widget_button(wmbar, VALUE = 'Layer', /MENU)
    ;wadd = widget_button(wlayer, VALUE = 'Add', UVALUE = 'ADD', UNAME = 'wadd', /MENU)
    ;wedit = widget_button(wlayer, VALUE = 'Edit', UVALUE = 'EDIT', UNAME = 'wedit')
    ;whide = widget_button(wlayer, VALUE = 'Hide', UVALUE = 'HIDE', UNAME = 'whide')
    ;wcopy = widget_button(wlayer, VALUE = 'Copy', UVALUE = 'COPY', UNAME = 'wcopy')
    ;wpaste = widget_button(wlayer, VALUE = 'Paste', UVALUE = 'PASTE', UNAME = 'wpaste')
    ;wdel = widget_button(wlayer, VALUE = 'Delete', UVALUE = 'DELETE', UNAME = 'wdel')
    ;wimage = widget_button(wadd, VALUE = 'Image', UVALUE = 'IMAGE', UNAME = 'wimage')
    ;wcontour = widget_button(wadd, VALUE = 'Contour', UVALUE = 'CONTOUR', UNAME = 'wcontour')
    ;wpoint = widget_button(wadd, VALUE = 'Point', UVALUE = 'POINT', UNAME = 'wpoint')
    ;wtext = widget_button(wadd, VALUE = 'Text', UVALUE = 'TEXT', UNAME = 'wtext')
    
    wsetting = widget_button(wmbar, VALUE = 'Setting', /MENU)
    wcustom = widget_button(wsetting, VALUE = 'Properties', UVALUE = 'CUSTOM', UNAME = 'wcustom')
    
    whelp = widget_button(wmbar, VALUE = 'Help', /MENU)
    wcontext = widget_button(whelp, VALUE = 'Context...', UVALUE = 'CONTEXT', UNAME = 'wcontext', ACCELERATOR = 'Ctrl+H')
    wabout = widget_button(whelp, VALUE = 'About...', UVALUE = 'ABOUT', UNAME = 'wabout')
    
    
    ;coordinate widget
    wmbase0 = module_frame(wbase, TITLE = 'Plot Region')
    
    wwcs = widget_droplist(wmbase0,  UVALUE = 'WCS', UNAME= 'wwcs', $
        VALUE = ['Celestial(ra,dec)', 'Galactic(gl,gb)', 'Ordinary plot(x,y)'], TITLE = 'Coordinate system')
    ;wra = CW_FIELD(wmbase0, TITLE = "R.A.", UVALUE = 'RA', UNAME = 'wra')
    wmbase1 = widget_base(wmbase0, ROW = 1)
    wrat1 = widget_label(wmbase1, VALUE = 'X center', UNAME = 'wrat')
    wra = widget_text(wmbase1, VALUE = '', UVALUE = 'RA', UNAME = 'wra', /EDITABLE)
    wmbase1 = widget_base(wmbase0, ROW = 1)
    wrat2 = widget_label(wmbase1, VALUE = ' range')
    wraof = widget_text(wmbase1, VALUE = '', UVALUE = 'RAOF', UNAME = 'wraof', /EDITABLE, $
        SCR_XSIZE = 45)
    wraot = widget_text(wmbase1, VALUE = '', UVALUE = 'RAOT', UNAME = 'wraot', /EDITABLE, $
        SCR_XSIZE = 45)
    wrau = widget_droplist(wmbase1, VALUE = ['arcsec','arcmin','degree','other..'], UVALUE = 'RAU', UNAME = 'wrau')
    WIDGET_CONTROL, wrau, SET_DROPLIST_SELECT = 2
    ;wdec = CW_FIELD(wmbase0, TITLE = 'DEC. ', UVALUE = 'DEC', UNAME = 'wdec')
    wmbase1 = widget_base(wmbase0, ROW = 1)
    wdect1 = widget_label(wmbase1, VALUE = 'Y center', UNAME = 'wdect')
    wdec = widget_text(wmbase1, VALUE = '', UVALUE = 'DEC', UNAME = 'wdec', /EDITABLE)
    wmbase1 = widget_base(wmbase0, ROW = 1)
    wdect2 = widget_label(wmbase1, VALUE = ' range')
    wdecof = widget_text(wmbase1, VALUE = '', UVALUE = 'DECOF', UNAME = 'wdecof', /EDITABLE, $
        SCR_XSIZE = 45)
    wdecot = widget_text(wmbase1, VALUE = '', UVALUE = 'DECOT', UNAME = 'wdecot', /EDITABLE, $
        SCR_XSIZE = 45)
    wdecu = widget_droplist(wmbase1, VALUE = ['arcsec','arcmin','degree','other..'], UVALUE = 'DECU', UNAME = 'wdecu')
    WIDGET_CONTROL, wdecu, SET_DROPLIST_SELECT = 2
    
    ;source catalog widget
    wmbase1 = widget_base(wmbase0, COLUMN = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    wcata = widget_button(wmbase1, VALUE = 'Get coordinate from a catalog', UVALUE = 'CATALOG', UNAME = 'wcata', TOOLTIP = 'Source catalog')
    wsou = widget_combobox(wmbase1, VALUE = ['Source Name'], UVALUE = 'SOURCE', UNAME = 'wsou', $/EDITABLE, $
        SCR_XSIZE = !x_width-100, SENSITIVE = 0)
    ;wlock = widget_button(wmbase1, VALUE = ' ', UVALUE = 'LOCK', UNAME = 'wlock', TOOLTIP = 'Lock')
    
    ;widget of layerlist compound
    wmbase0 = module_frame(wbase, TITLE = 'Layer')
    
    wmbase1 = widget_base(wmbase0, ROW = 1, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
    wlist = widget_list(wmbase1, VALUE = [''], UVALUE = 'LIST', UNAME = 'wlist', /MULTIPLE, $
        SCR_XSIZE = !x_width-30, SCR_YSIZE = 120)
    wmbase2 = widget_base(wmbase1, COLUMN = 1, SPACE = 4);,/GRID_LAYOUT)
    icon = [[1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [0,0,0,0,0,0,0,0,0],$
        [1,0,0,0,0,0,0,0,1],$
        [1,1,0,0,0,0,0,1,1],$
        [1,1,1,0,0,0,1,1,1],$
        [1,1,1,1,0,1,1,1,1]]
    icon = byte([[[icon*236]],[[icon*233]],[[icon*216]]])
    wup = widget_button(wmbase2, VALUE = icon, UVALUE = 'UP', UNAME = 'wup', TOOLTIP = 'Move this layer upward', $
        SCR_YSIZE = 36, ACCELERATOR = 'Ctrl+Up')
    icon = reverse(icon,2)
    wdown = widget_button(wmbase2, VALUE = icon, UVALUE = 'DOWN', UNAME = 'wdown', TOOLTIP = 'Move this layer downward', $
        SCR_YSIZE = 36, ACCELERATOR = 'Ctrl+Down')
    wmbase1 = widget_base(wmbase0, ROW = 2, /ALIGN_CENTER, /GRID_LAYOUT)
    wadd = widget_button(wmbase1, VALUE = 'Add->', UVALUE = 'ADD', UNAME = 'wadd', TOOLTIP = 'Add a layer', $
        /MENU)
    wedit = widget_button(wmbase1, VALUE = 'Edit', UVALUE = 'EDIT', UNAME = 'wedit', TOOLTIP = 'Edit this layer')
    whide = widget_button(wmbase1, VALUE = 'Hide', UVALUE = 'HIDE', UNAME = 'whide', TOOLTIP = 'Hide this layer')
    wcopy = widget_button(wmbase1, VALUE = 'Copy', UVALUE = 'COPY', UNAME = 'wcopy', TOOLTIP = 'Copy this layer')
    wpaste = widget_button(wmbase1, VALUE = 'Paste', UVALUE = 'PASTE', UNAME = 'wpaste', TOOLTIP = 'Paste layer')
    wdel = widget_button(wmbase1, VALUE = 'Delete', UVALUE = 'DELETE', UNAME = 'wdel', TOOLTIP = 'Delete this layer')
    
    ;children of wadd, 3 kinds of way
    wimage = widget_button(wadd, VALUE = 'Image', UVALUE = 'IMAGE', UNAME = 'wimage', ACCELERATOR = 'Shift+I')
    wcontour = widget_button(wadd, VALUE = 'Contour', UVALUE = 'CONTOUR', UNAME = 'wcontour', ACCELERATOR = 'Shift+C')
    wpoint = widget_button(wadd, VALUE = 'Point', UVALUE = 'POINT', UNAME = 'wpoint', ACCELERATOR = 'Shift+P')
    wtext = widget_button(wadd, VALUE = 'Text', UVALUE = 'TEXT', UNAME = 'wtext', ACCELERATOR = 'Shift+T')
    wshape = widget_button(wadd, VALUE = 'Shape', UVALUE = 'SHAPE', UNAME = 'wshape', ACCELERATOR = 'Shift+S')
    wpanel = widget_button(wadd, VALUE = 'Panel', UVALUE = 'PANEL', UNAME = 'wpanel', ACCELERATOR = 'Shift+L')
    ;wbg = cw_bgroup(wmbase0,['Add',' Copy ',' Edit ','Paste',' Hide ','Delete'], $
    ;	BUTTON_UVALUE = ['ADD','COPY','EDIT','PASTE','HIDE','DELETE'], COLUMN = 3, UVALUE = 'OPR')
    
    ;widget for output
    wdraw = widget_button(wbase, VALUE = 'Draw', UVALUE = 'DRAW', UNAME = 'wdraw', TOOLTIP = 'Draw compounded image', $
        SCR_XSIZE = !x_width-20, SCR_YSIZE = 40)
    
    wtime = widget_label(wbase, VALUE = 'Welcome to AICer!', UVALUE = 'TIME', UNAME = 'wtime', $
        SCR_XSIZE = !x_width)
    
    ;adjust the base widget offset, then realize it
    ;SET_PLOT, 'win'
    DEVICE, DECOMPOSED = 0
    DEVICE, GET_SCREEN_SIZE = screensize
    ;IF screensize[0] GT 2000 THEN screensize[0] = screensize[0]/2
    ;xcenter = screenSize[0]*0.4
    ;ycenter = screenSize[1]*0.4
    ;wgeom = widget_info(wbase, /GEOMETRY)
    ;xsize = wgeom.scr_xsize+2*wgeom.margin
    ;ysize = wgeom.scr_ysize+2*wgeom.margin
    WIDGET_CONTROL, wbase, XOFFSET = (screensize[0]*0.1 < 80), YOFFSET = (screensize[1]*0.1 < 20)
    
    WIDGET_CONTROL, /REALIZE, wbase
    
    layerlist = layer_define()
    
    DEFSYSV, '!layercache', layerlist
    DEFSYSV, '!layercopy', layerlist
    DEFSYSV, '!histpoly', fltarr(402,2)
    DEFSYSV, '!limitvalue', dblarr(2, 10)
    ;DEFSYSV, '!imagepreview', fltarr(400,400,3)
    
    WIDGET_CONTROL, wbase, SET_UVALUE = layerlist, /NO_COPY
    
    XMANAGER, 'AICer', wbase, /NO_BLOCK
    
END

