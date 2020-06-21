#

LW_PATH := "/Applications/LispWorks 7.0 (32-bit)/LispWorks (32-bit).app/Contents/MacOS/lispworks-7-0-0-x86-darwin"

all:
	$(LW_PATH) -build src/delivery-script.lisp
	zip -r mediaimport.zip "Media Import.app"
