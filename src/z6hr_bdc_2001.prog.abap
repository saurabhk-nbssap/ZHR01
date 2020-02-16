*&---------------------------------------------------------------------*
*& Report  Z6HR_BDC_2001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR_BDC_2001.

TYPE-POOLS : SLIS,TRUXS.

INCLUDE ZBDCRECX1.

TYPES : BEGIN OF S_UPLOAD,
         PERNR TYPE PA2001-PERNR,
         BEGDA(10) ,
         ENDDA(10),
         BEGUZ(05),
         ENDUZ(05),
END   OF S_UPLOAD .
DATA : I_UPLOAD TYPE STANDARD TABLE OF  S_UPLOAD,
      WA_UPLOAD TYPE S_UPLOAD.

DATA : V_FILENAME TYPE IBIPPARMS-PATH.
DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.
DATA:TRAN_MODE .

*Data decleration for Error Message
DATA:
     T_MSG TYPE TABLE OF BDCMSGCOLL,   " Collecting Error messages
     W_MSG TYPE BDCMSGCOLL,
     W_MSG1(51).

SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME .

PARAMETERS: P_FILE  TYPE IBIPPARMS-PATH OBLIGATORY.
*            E_FILE   TYPE RLGRAP-FILENAME .       " Error File Path.

SELECTION-SCREEN END OF BLOCK S2 .

SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-T01.
PARAMETERS:  P_FORE     RADIOBUTTON   GROUP RAD DEFAULT 'X' ,
             P_BACK     RADIOBUTTON   GROUP RAD ,
             P_NOERR    RADIOBUTTON   GROUP RAD .

SELECTION-SCREEN END OF BLOCK S01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE '
    IMPORTING
      FILE_NAME     = P_FILE.

  IF NOT P_FILE IS INITIAL.
    V_FILENAME = P_FILE.
  ENDIF.


START-OF-SELECTION.

  PERFORM  F_UPLOAD.
  IF I_UPLOAD IS NOT INITIAL.
    IF P_FORE EQ 'X' .
      TRAN_MODE = 'A' .
    ELSEIF P_BACK EQ 'X' .
      TRAN_MODE = 'E' .
    ELSEIF P_NOERR EQ 'X' .
      TRAN_MODE = 'N' .
    ENDIF .

    PERFORM  RUN_BDC .
    CLEAR: BDCDATA , BDCDATA[] .
    REFRESH BDCDATA.

  ENDIF.







*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPLOAD .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*  *I_FIELD_SEPERATOR =
      I_LINE_HEADER = 'X'
      I_TAB_RAW_DATA = IT_RAW
      I_FILENAME = V_FILENAME
    TABLES
       I_TAB_CONVERTED_DATA = I_UPLOAD
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS = 2.

  IF SY-SUBRC = 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  RUN_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RUN_BDC .

LOOP AT I_UPLOAD INTO WA_UPLOAD.

perform bdc_dynpro      using 'SAPMP50A' '1000'.
perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
perform bdc_field       using 'RP50G-PERNR'                              WA_UPLOAD-PERNR."'00000143'.
perform bdc_field       using 'RP50G-TIMR6'                              'X'.
perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
perform bdc_field       using 'RP50G-CHOIC'                              '2001'.
perform bdc_field       using 'RP50G-SUBTY'                              'LWP'.
perform bdc_dynpro      using 'MP200000' '2000'.
perform bdc_field       using 'BDC_CURSOR'                              'P2001-ENDUZ'.
perform bdc_field       using 'BDC_OKCODE'                              '/00'.
perform bdc_field       using 'P2001-BEGDA'                             WA_UPLOAD-BEGDA." '11.11.2014'.
perform bdc_field       using 'P2001-ENDDA'                             WA_UPLOAD-ENDDA." '11.11.2014'.
perform bdc_field       using 'P2001-BEGUZ'                             WA_UPLOAD-BEGUZ." '09:00'.
perform bdc_field       using 'P2001-ENDUZ'                             WA_UPLOAD-ENDUZ." '13:00'.
perform bdc_dynpro      using 'MP200000' '2000'.
perform bdc_field       using 'BDC_CURSOR'                              'P2001-BEGDA'.
perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
perform bdc_field       using 'P2001-BEGDA'                             WA_UPLOAD-BEGDA." '11.11.2014'.
perform bdc_field       using 'P2001-ENDDA'                             WA_UPLOAD-ENDDA." '11.11.2014'.
perform bdc_field       using 'P2001-BEGUZ'                             WA_UPLOAD-BEGUZ." '09:00'.
perform bdc_field       using 'P2001-ENDUZ'                             WA_UPLOAD-ENDUZ." '13:00'.
*perform bdc_field       using 'P2001-STDAZ'                              '    4.00'.

CLEAR: T_MSG.
  IF BDCDATA IS NOT INITIAL.
    CALL TRANSACTION 'PA30' USING BDCDATA MODE TRAN_MODE UPDATE 'S'
      MESSAGES INTO T_MSG .

  LOOP AT T_MSG INTO W_MSG WHERE MSGTYP = 'E'.
      FORMAT COLOR 6 INTENSIFIED OFF.
      WRITE : / 'ERROR in Absence Data Updation ' ,'For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR

  ENDLOOP.

  IF T_MSG IS NOT INITIAL.
      READ TABLE T_MSG INTO W_MSG WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC <> 0.
        FORMAT COLOR 5 INTENSIFIED OFF.
        WRITE : / 'Absence data updated Successfully for User ID:' , WA_UPLOAD-PERNR."USRID1 ."'Absence data updated Successfully for User ID', WA_UPLOAD-USRID1.
      ENDIF.
      CLEAR: T_MSG , W_MSG.
    ENDIF.
  ENDIF.
CLEAR: BDCDATA , BDCDATA[] .
        REFRESH BDCDATA.
ENDLOOP.

ENDFORM.                    " RUN_BDC
