*&---------------------------------------------------------------------*
*& Report  Z6HR002R_IT_585_BDC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR002R_IT_585_BDC.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: INFOTYPE 585 UPDATION
* OBJECT TYPE       : BDC PROGRAM    FUNC. CONSULTANT  : RAM MANOHAR
*          DEVELOPER: RAMAKRISHNA
*      CREATION DATE: 16.06.2010
*        DEV REQUEST: IRDK900144
*              TCODE: ZHR003
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*


data: begin of i_upload occurs 0,

pernr like PA0585-pernr,

bedda(10) type c,

ebdda(10) type c,

pcn01 like PA0585-pcn01,

pcn02 like PA0585-pcn01,

pcn03 like PA0585-pcn01,

pcn04 like PA0585-pcn01,

pcn05 like PA0585-pcn01,

pcn06 like PA0585-pcn01,

pcn07 like PA0585-pcn01,

pcn08 like PA0585-pcn01,

pcn09 like PA0585-pcn01,

pcn11 like PA0585-pcn01,

pcn12 like PA0585-pcn01,

pcn13 like PA0585-pcn01,

pcn14 like PA0585-pcn01,

pcn16 like PA0585-pcn01,

pcn17 like PA0585-pcn01,

pcn18 like PA0585-pcn01,

pcn19 like PA0585-pcn01,

pcn20 like PA0585-pcn01,

pcn21 like PA0585-pcn01,

pcn22 like PA0585-pcn01,

pcn23 like PA0585-pcn01,

pcn24 like PA0585-pcn01,

pcn25 like PA0585-pcn01,

acn01 like PA0585-acn01,

acn02 like PA0585-acn01,

acn03 like PA0585-acn01,

acn04 like PA0585-acn01,

acn05 like PA0585-acn01,

acn06 like PA0585-acn01,

acn07 like PA0585-acn01,

acn08 like PA0585-acn01,

acn09 like PA0585-acn01,

acn11 like PA0585-acn01,

acn12 like PA0585-acn01,

acn13 like PA0585-acn01,

acn14 like PA0585-acn01,

acn16 like PA0585-acn01,

acn17 like PA0585-acn01,

acn18 like PA0585-acn01,

acn19 like PA0585-acn01,

acn20 like PA0585-acn01,

acn21 like PA0585-acn01,

acn22 like PA0585-acn01,

acn23 like PA0585-acn01,

acn24 like PA0585-acn01,

acn25 like PA0585-acn01,

end of i_upload.

data: begin of p585 occurs 0,

SBSEC like PA0585-sbs01,

pcnvt like PA0585-pcn01,

acnvt like PA0585-acn01,

end of p585.

data: p0585 like p0585 ,

ia585 like PA0585 occurs 0 with header line,

return like bapireturn1,

PIN_PCNTR type PIN_PCNTR.

************************************************************************

* S E L E C T I O N - S C R E E N D E F I N I T I O N *

************************************************************************

selection-screen begin of block b1 with frame title text-001.

parameters: p_file like rlgrap-filename obligatory,

p_begda like PA0585-begda obligatory,

p_endda like PA0585-endda obligatory.

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

    ia585-pernr = i_upload-pernr.

    ia585-endda = p_endda.

    ia585-begda = p_begda.

    perform col_row.

    perform do.

    append ia585.

    clear : ia585,p585.

    refresh : p585.

  endloop.

  loop at ia585.

    move-corresponding ia585 to p0585.

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        NUMBER = p0585-pernr.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        INFTY         = '0585'
        NUMBER        = p0585-pernr
        LOCKINDICATOR = ''
        VALIDITYEND   = p0585-endda
        VALIDITYBEGIN = p0585-begda
        RECORD        = p0585
        OPERATION     = 'INS'
        NOCOMMIT      = ''
        tclas         = 'A'
      IMPORTING
        RETURN        = RETURN
      EXCEPTIONS
        OTHERS        = 0.

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        NUMBER = p0585-pernr.

    clear : p0585.

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
      file_name     = p_w_file
    EXCEPTIONS
      mask_too_long = 1
      others        = 2.

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

*&---------------------------------------------------------------------*
*&      Form  col_row
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM col_row .

  if i_upload-acn01 is not initial or i_upload-pcn01 is not initial.

    p585-sbsec = '01'.

    p585-pcnvt = i_upload-pcn01.

    p585-acnvt = i_upload-acn01.

    append p585.

    clear p585.

  endif.

  if i_upload-acn02 is not initial or i_upload-pcn02 is not initial.

    p585-sbsec = '02'.

    p585-pcnvt = i_upload-pcn02.

    p585-acnvt = i_upload-acn02.

    append p585.

    clear p585.

  endif.

  if i_upload-acn03 is not initial or i_upload-pcn03 is not initial.

    p585-sbsec = '03'.

    p585-pcnvt = i_upload-pcn03.

    p585-acnvt = i_upload-acn03.

    append p585.

    clear p585.

  endif.

  if i_upload-acn04 is not initial or i_upload-pcn04 is not initial.

    p585-sbsec = '04'.

    p585-pcnvt = i_upload-pcn04.

    p585-acnvt = i_upload-acn04.

    append p585.

    clear p585.

  endif.

  if i_upload-acn05 is not initial or i_upload-pcn05 is not initial.

    p585-sbsec = '05'.

    p585-pcnvt = i_upload-pcn05.

    p585-acnvt = i_upload-acn05.

    append p585.

    clear p585.

  endif.

  if i_upload-acn06 is not initial or i_upload-pcn06 is not initial.

    p585-sbsec = '06'.

    p585-pcnvt = i_upload-pcn06.

    p585-acnvt = i_upload-acn06.

    append p585.

    clear p585.

  endif.

  if i_upload-acn07 is not initial or i_upload-pcn07 is not initial.

    p585-sbsec = '07'.

    p585-pcnvt = i_upload-pcn07.

    p585-acnvt = i_upload-acn07.

    append p585.

    clear p585.

  endif.

  if i_upload-acn08 is not initial or i_upload-pcn08 is not initial.

    p585-sbsec = '08'.

    p585-pcnvt = i_upload-pcn08.

    p585-acnvt = i_upload-acn08.

    append p585.

    clear p585.

  endif.

  if i_upload-acn09 is not initial or i_upload-pcn09 is not initial.

    p585-sbsec = '09'.

    p585-pcnvt = i_upload-pcn09.

    p585-acnvt = i_upload-acn03.

    append p585.

    clear p585.

  endif.

  if i_upload-acn11 is not initial or i_upload-pcn11 is not initial.

    p585-sbsec = '11'.

    p585-pcnvt = i_upload-pcn11.

    p585-acnvt = i_upload-acn11.

    append p585.

    clear p585.

  endif.

  if i_upload-acn12 is not initial or i_upload-pcn12 is not initial.

    p585-sbsec = '12'.

    p585-pcnvt = i_upload-pcn12.

    p585-acnvt = i_upload-acn12.

    append p585.

    clear p585.

  endif.

  if i_upload-acn13 is not initial or i_upload-pcn13 is not initial.

    p585-sbsec = '13'.

    p585-pcnvt = i_upload-pcn13.

    p585-acnvt = i_upload-acn13.

    append p585.

    clear p585.

  endif.

  if i_upload-acn14 is not initial or i_upload-pcn14 is not initial.

    p585-sbsec = '14'.

    p585-pcnvt = i_upload-pcn14.

    p585-acnvt = i_upload-acn14.

    append p585.

    clear p585.

  endif.

  if i_upload-acn16 is not initial or i_upload-pcn16 is not initial.

    p585-sbsec = '16'.

    p585-pcnvt = i_upload-pcn16.

    p585-acnvt = i_upload-acn16.

    append p585.

    clear p585.

  endif.

  if i_upload-acn17 is not initial or i_upload-pcn17 is not initial.

    p585-sbsec = '17'.

    p585-pcnvt = i_upload-pcn17.

    p585-acnvt = i_upload-acn17.

    append p585.

    clear p585.

  endif.

  if i_upload-acn18 is not initial or i_upload-pcn18 is not initial.

    p585-sbsec = '18'.

    p585-pcnvt = i_upload-pcn18.

    p585-acnvt = i_upload-acn18.

    append p585.

    clear p585.

  endif.

  if i_upload-acn19 is not initial or i_upload-pcn19 is not initial.

    p585-sbsec = '19'.

    p585-pcnvt = i_upload-pcn19.

    p585-acnvt = i_upload-acn19.

    append p585.

    clear p585.

  endif.

  if i_upload-acn20 is not initial or i_upload-pcn20 is not initial.

    p585-sbsec = '20'.

    p585-pcnvt = i_upload-pcn20.

    p585-acnvt = i_upload-acn20.

    append p585.

    clear p585.

  endif.

  if i_upload-acn21 is not initial or i_upload-pcn21 is not initial.

    p585-sbsec = '21'.

    p585-pcnvt = i_upload-pcn21.

    p585-acnvt = i_upload-acn21.

    append p585.

    clear p585.

  endif.

  if i_upload-acn22 is not initial or i_upload-pcn22 is not initial.

    p585-sbsec = '22'.

    p585-pcnvt = i_upload-pcn22.

    p585-acnvt = i_upload-acn22.

    append p585.

    clear p585.

  endif.

  if i_upload-acn23 is not initial or i_upload-pcn23 is not initial.

    p585-sbsec = '23'.

    p585-pcnvt = i_upload-pcn23.

    p585-acnvt = i_upload-acn23.

    append p585.

    clear p585.

  endif.

  if i_upload-acn24 is not initial or i_upload-pcn24 is not initial.

    p585-sbsec = '24'.

    p585-pcnvt = i_upload-pcn24.

    p585-acnvt = i_upload-acn24.

    append p585.

    clear p585.

  endif.

  if i_upload-acn25 is not initial or i_upload-pcn25 is not initial.

    p585-sbsec = '25'.

    p585-pcnvt = i_upload-pcn25.

    p585-acnvt = i_upload-acn25.

    append p585.

    clear p585.

  endif.

ENDFORM. " col_row

*&---------------------------------------------------------------------*
*&      Form  do
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM do .

  data : loop_itc(11) value 'IA585-SBS01',

  loop_pcn(11) value 'IA585-pcn01',

  loop_acn(11) value 'IA585-acn01',

  index(2) type p value 1,

  unpacked_index(2).

  field-symbols: <itc>, <pcn> , <acn>.

  loop at p585.

    unpack index to unpacked_index.

    loop_itc+09(2) = unpacked_index.

    loop_pcn+09(2) = unpacked_index.

    loop_acn+09(2) = unpacked_index.

    assign (loop_itc) to <itc>.

    assign (loop_pcn) to <pcn>.

    assign (loop_acn) to <acn>.

    <itc> = p585-sbsec.

    <pcn> = p585-pcnvt.

    <acn> = p585-acnvt.

    add 1 to index.

  endloop.

ENDFORM. " do
