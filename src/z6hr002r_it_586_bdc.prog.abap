*&---------------------------------------------------------------------*
*& Report  Z6HR002R_IT_586_BDC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR002R_IT_586_BDC.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: INFOTYPE 586 UPDATION
* OBJECT TYPE       : BDC PROGRAM    FUNC. CONSULTANT  : RAM MANOHAR
*          DEVELOPER: RAMAKRISHNA
*      CREATION DATE: 15.06.2010
*        DEV REQUEST: IRDK900144
*              TCODE: ZHR002
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*


data: begin of i_upload occurs 0,

pernr like pa0586-pernr,

bedda(10) type c,

ebdda(10) type c,

pin01 like pa0586-pin01,

pin02 like pa0586-pin01,

pin03 like pa0586-pin01,

pin04 like pa0586-pin01,

pin05 like pa0586-pin01,

pin06 like pa0586-pin01,

pin07 like pa0586-pin01,

pin08 like pa0586-pin01,

pin09 like pa0586-pin01,

pin11 like pa0586-pin01,

pin12 like pa0586-pin01,

pin13 like pa0586-pin01,

pin14 like pa0586-pin01,

pin16 like pa0586-pin01,

pin17 like pa0586-pin01,

pin18 like pa0586-pin01,

pin19 like pa0586-pin01,

pin20 like pa0586-pin01,

pin21 like pa0586-pin01,

pin22 like pa0586-pin01,

pin23 like pa0586-pin01,

pin24 like pa0586-pin01,

pin25 like pa0586-pin01,

pin26 like pa0586-pin01,

pin27 like pa0586-pin01,

pin28 like pa0586-pin01,

ain01 like pa0586-ain01,

ain02 like pa0586-ain01,

ain03 like pa0586-ain01,

ain04 like pa0586-ain01,

ain05 like pa0586-ain01,

ain06 like pa0586-ain01,

ain07 like pa0586-ain01,

ain08 like pa0586-ain01,

ain09 like pa0586-ain01,

ain11 like pa0586-ain01,

ain12 like pa0586-ain01,

ain13 like pa0586-ain01,

ain14 like pa0586-ain01,

ain16 like pa0586-ain01,

ain17 like pa0586-ain01,

ain18 like pa0586-ain01,

ain19 like pa0586-ain01,

ain20 like pa0586-ain01,

ain21 like pa0586-ain01,

ain22 like pa0586-ain01,

ain23 like pa0586-ain01,

ain24 like pa0586-ain01,

ain25 like pa0586-ain01,

ain26 like pa0586-ain01,

ain27 like pa0586-ain01,

ain28 like pa0586-ain01,

end of i_upload.

data: begin of p586 occurs 0,

icode like pa0586-itc01,

pinvt like pa0586-pin01,

ainvt like pa0586-ain01,

end of p586.

data: p0586 like p0586 ,

ia586 like pa0586 occurs 0 with header line,

return like bapireturn1,

pin_ainvt type pin_ainvt.

************************************************************************

* S E L E C T I O N - S C R E E N D E F I N I T I O N *

************************************************************************

selection-screen begin of block b1 with frame title text-001.

parameters: p_file like rlgrap-filename obligatory,

p_begda like pa0586-begda obligatory,

p_endda like pa0586-endda obligatory.

selection-screen end of block b1.

************************************************************************

* E V E N T H A N D L I N G - B E G I N *

************************************************************************

at selection-screen on value-request for p_file.

perform get_file using p_file.

************************************************************************

* START OF SELECTION - B E G I N *

************************************************************************

start-of-selection.

data: lines like sy-index.

*Getting the file data.

perform upload using p_file.

loop at i_upload.

ia586-pernr = i_upload-pernr.

ia586-endda = p_endda.

ia586-begda = p_begda.

perform col_row.

perform do.

append ia586.

clear : ia586,p586.

refresh : p586.

endloop.

loop at ia586.

move-corresponding ia586 to p0586.

CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'

EXPORTING

NUMBER = p0586-pernr.

CALL FUNCTION 'HR_INFOTYPE_OPERATION'

EXPORTING

INFTY = '0586'

NUMBER = p0586-pernr

LOCKINDICATOR = ''

VALIDITYEND = p0586-endda

VALIDITYBEGIN = p0586-begda

RECORD = p0586

OPERATION = 'INS'

NOCOMMIT = ''

tclas = 'A'

IMPORTING

RETURN = RETURN

EXCEPTIONS

OTHERS = 0.

CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'

EXPORTING

NUMBER = p0586-pernr.

clear : p0586.

endloop.

*&---------------------------------------------------------------------*

*& Form GET_FILE

*&---------------------------------------------------------------------*

* Show "Open File" dialog box on F4

*----------------------------------------------------------------------*

* -->P_W_FILE Name of the file selected by the user

*----------------------------------------------------------------------*

form get_file using p_w_file.

call function 'KD_GET_FILENAME_ON_F4'

CHANGING

file_name = p_w_file

EXCEPTIONS

mask_too_long = 1

others = 2.

if sy-subrc ne 0.

message id sy-msgid type sy-msgty number sy-msgno

with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

endif.

endform. "GET_FILE

*&---------------------------------------------------------------------*

*& Form UPLOAD

*&---------------------------------------------------------------------*

* Upload the file into the internal table for processing

*----------------------------------------------------------------------*

* -->P_LOADFILE Name of file to upload

*----------------------------------------------------------------------*

form upload using p_loadfile.

* Read the file into the Internal Table

CALL FUNCTION 'WS_UPLOAD'

EXPORTING

filename = p_loadfile

filetype = 'DAT'

TABLES

data_tab = i_upload.

if sy-subrc ne 0.

message id sy-msgid type sy-msgty number sy-msgno

with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

endif.

endform. "UPLOAD

FORM col_row .

if i_upload-ain01 is not initial or i_upload-pin01 is not initial.

p586-icode = '01'.

p586-pinvt = i_upload-pin01.

p586-ainvt = i_upload-ain01.

append p586.

clear p586.

endif.

if i_upload-ain02 is not initial or i_upload-pin02 is not initial.

p586-icode = '02'.

p586-pinvt = i_upload-pin02.

p586-ainvt = i_upload-ain02.

append p586.

clear p586.

endif.

if i_upload-ain03 is not initial or i_upload-pin03 is not initial.

p586-icode = '03'.

p586-pinvt = i_upload-pin03.

p586-ainvt = i_upload-ain03.

append p586.

clear p586.

endif.

if i_upload-ain04 is not initial or i_upload-pin04 is not initial.

p586-icode = '04'.

p586-pinvt = i_upload-pin04.

p586-ainvt = i_upload-ain04.

append p586.

clear p586.

endif.

if i_upload-ain05 is not initial or i_upload-pin05 is not initial.

p586-icode = '05'.

p586-pinvt = i_upload-pin05.

p586-ainvt = i_upload-ain05.

append p586.

clear p586.

endif.

if i_upload-ain06 is not initial or i_upload-pin06 is not initial.

p586-icode = '06'.

p586-pinvt = i_upload-pin06.

p586-ainvt = i_upload-ain06.

append p586.

clear p586.

endif.

if i_upload-ain07 is not initial or i_upload-pin07 is not initial.

p586-icode = '07'.

p586-pinvt = i_upload-pin07.

p586-ainvt = i_upload-ain07.

append p586.

clear p586.

endif.

if i_upload-ain08 is not initial or i_upload-pin08 is not initial.

p586-icode = '08'.

p586-pinvt = i_upload-pin08.

p586-ainvt = i_upload-ain08.

append p586.

clear p586.

endif.

if i_upload-ain09 is not initial or i_upload-pin09 is not initial.

p586-icode = '09'.

p586-pinvt = i_upload-pin09.

p586-ainvt = i_upload-ain03.

append p586.

clear p586.

endif.

if i_upload-ain11 is not initial or i_upload-pin11 is not initial.

p586-icode = '11'.

p586-pinvt = i_upload-pin11.

p586-ainvt = i_upload-ain11.

append p586.

clear p586.

endif.

if i_upload-ain12 is not initial or i_upload-pin12 is not initial.

p586-icode = '12'.

p586-pinvt = i_upload-pin12.

p586-ainvt = i_upload-ain12.

append p586.

clear p586.

endif.

if i_upload-ain13 is not initial or i_upload-pin13 is not initial.

p586-icode = '13'.

p586-pinvt = i_upload-pin13.

p586-ainvt = i_upload-ain13.

append p586.

clear p586.

endif.

if i_upload-ain14 is not initial or i_upload-pin14 is not initial.

p586-icode = '14'.

p586-pinvt = i_upload-pin14.

p586-ainvt = i_upload-ain14.

append p586.

clear p586.

endif.

if i_upload-ain16 is not initial or i_upload-pin16 is not initial.

p586-icode = '16'.

p586-pinvt = i_upload-pin16.

p586-ainvt = i_upload-ain16.

append p586.

clear p586.

endif.

if i_upload-ain17 is not initial or i_upload-pin17 is not initial.

p586-icode = '17'.

p586-pinvt = i_upload-pin17.

p586-ainvt = i_upload-ain17.

append p586.

clear p586.

endif.

if i_upload-ain18 is not initial or i_upload-pin18 is not initial.

p586-icode = '18'.

p586-pinvt = i_upload-pin18.

p586-ainvt = i_upload-ain18.

append p586.

clear p586.

endif.

if i_upload-ain19 is not initial or i_upload-pin19 is not initial.

p586-icode = '19'.

p586-pinvt = i_upload-pin19.

p586-ainvt = i_upload-ain19.

append p586.

clear p586.

endif.

if i_upload-ain20 is not initial or i_upload-pin20 is not initial.

p586-icode = '20'.

p586-pinvt = i_upload-pin20.

p586-ainvt = i_upload-ain20.

append p586.

clear p586.

endif.

if i_upload-ain21 is not initial or i_upload-pin21 is not initial.

p586-icode = '21'.

p586-pinvt = i_upload-pin21.

p586-ainvt = i_upload-ain21.

append p586.

clear p586.

endif.

if i_upload-ain22 is not initial or i_upload-pin22 is not initial.

p586-icode = '22'.

p586-pinvt = i_upload-pin22.

p586-ainvt = i_upload-ain22.

append p586.

clear p586.

endif.

if i_upload-ain23 is not initial or i_upload-pin23 is not initial.

p586-icode = '23'.

p586-pinvt = i_upload-pin23.

p586-ainvt = i_upload-ain23.

append p586.

clear p586.

endif.

if i_upload-ain24 is not initial or i_upload-pin24 is not initial.

p586-icode = '24'.

p586-pinvt = i_upload-pin24.

p586-ainvt = i_upload-ain24.

append p586.

clear p586.

endif.

if i_upload-ain25 is not initial or i_upload-pin25 is not initial.

p586-icode = '25'.

p586-pinvt = i_upload-pin25.

p586-ainvt = i_upload-ain25.

append p586.

clear p586.

endif.

if i_upload-ain26 is not initial or i_upload-pin26 is not initial.

p586-icode = '26'.

p586-pinvt = i_upload-pin26.

p586-ainvt = i_upload-ain26.

append p586.

clear p586.

endif.

if i_upload-ain27 is not initial or i_upload-pin27 is not initial.

p586-icode = '27'.

p586-pinvt = i_upload-pin27.

p586-ainvt = i_upload-ain27.

append p586.

clear p586.

endif.
if i_upload-ain28 is not initial or i_upload-pin28 is not initial.

p586-icode = '28'.

p586-pinvt = i_upload-pin28.

p586-ainvt = i_upload-ain28.

append p586.

clear p586.

endif.

ENDFORM. " col_row

FORM do .

data : loop_itc(11) value 'IA586-ITC01',

loop_pin(11) value 'IA586-PIN01',

loop_ain(11) value 'IA586-AIN01',

index(2) type p value 1,

unpacked_index(2).

field-symbols: <itc>, <pin> , <ain>.

loop at p586.

unpack index to unpacked_index.

loop_itc+09(2) = unpacked_index.

loop_pin+09(2) = unpacked_index.

loop_ain+09(2) = unpacked_index.

assign (loop_itc) to <itc>.

assign (loop_pin) to <pin>.

assign (loop_ain) to <ain>.

<itc> = p586-icode.

<pin> = p586-pinvt.

<ain> = p586-ainvt.

add 1 to index.

endloop.

ENDFORM. " do
