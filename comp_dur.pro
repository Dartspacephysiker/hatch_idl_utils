FUNCTION comp_dur,imask,WhereChange=wherechange

nSeries = N_Elements(imask)
buffered = [0, imask, 0]
wChange = Where(buffered[1:*] NE buffered, nChange)
wChange = Reform(wChange, 2, nChange/2, /Overwrite)
duration = Reform(wChange[1,*] - wChange[0,*])

;return where change happens, if user desires
IF Arg_Present(wherechange) THEN wherechange = wChange;

print, "nelements:", n_elements(wherechange), " and if arg_present:", Arg_Present(wherechange)

print, duration
RETURN,duration
END