;2016/09/17
;Int types are
; 1  		Byte                 (An 8-bit unsigned integer ranging in value from 0 to 255. Pixels in images are commonly represented as byte data.)
; 2  		Integer              (A 16-bit signed integer ranging from -32,768 to +32,767.)
; 12 		Unsigned Integer     (A 16-bit unsigned integer ranging from 0 to 65535.)
; 3  		Long                 (32-bit signed integer ranging in value from -2 147 483 648 to +2 147 483 647.)
; 13 		Unsigned Long        (A 32-bit unsigned integer ranging in value from 0 to 4 294 967 296.)
; 14 		64-bit Long          (A 64-bit signed integer ranging in value from -9 223 372 036 854 775 808 to +9 223 372 036 854 775 807.)
; 15 		64-bit Unsigned Long (A 64-bit unsigned integer ranging in value from 0 to 18 446 744 073 709 551 615.)
FUNCTION IS_INT_TYPE,data

  COMPILE_OPT IDL2,STRICTARRSUBS

  RETURN,( (WHERE(SIZE(data,/TYPE) EQ [1,2,3,12,13,14,15]))[0] NE -1)

END
