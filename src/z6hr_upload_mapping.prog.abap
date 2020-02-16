*&---------------------------------------------------------------------*
*& Report  Z6HR_UPLOAD_MAPPING
*&
*&---------------------------------------------------------------------*
*&
*& this program is alternate way to Upload data into mapping table used for Peoplesoft and SAP
*&---------------------------------------------------------------------*

REPORT  Z6HR_UPLOAD_MAPPING.
TYPE-POOLS : SLIS,TRUXS.

DATA : V_FILENAME TYPE IBIPPARMS-PATH.
DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

TYPES : BEGIN OF S_UPLOAD,
TABNAME TYPE Z6HR_PPL_2_SAP-TABNAME,
FIELD TYPE Z6HR_PPL_2_SAP-FIELD,
PPL_SFT TYPE Z6HR_PPL_2_SAP-PPL_SFT ,
SAP TYPE Z6HR_PPL_2_SAP-SAP,
DESCR TYPE Z6HR_PPL_2_SAP-DESCR,
END OF S_UPLOAD .

DATA : I_UPLOAD TYPE STANDARD TABLE OF  S_UPLOAD.
DATA : WA_UPLOAD TYPE S_UPLOAD.

DATA: WA_Z6HR_PPL_2_SAP TYPE Z6HR_PPL_2_SAP.

SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME .

PARAMETERS: P_FILE  TYPE IBIPPARMS-PATH OBLIGATORY.
*            E_FILE   TYPE RLGRAP-FILENAME .       " Error File Path.

SELECTION-SCREEN END OF BLOCK S2 .


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
  PERFORM UPLOAD_TABLE.

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
*&      Form  UPLOAD_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_TABLE .

  LOOP AT I_UPLOAD INTO WA_UPLOAD.
    move-corresponding wa_upload to WA_Z6HR_PPL_2_SAP.

    INSERT Z6HR_PPL_2_SAP FROM WA_Z6HR_PPL_2_SAP.
    IF SY-SUBRC <> 0.
      FORMAT COLOR 6 INTENSIFIED OFF.

      condense wa_upload-TABNAME .
      condense wa_upload-FIELD.
      condense WA_UPLOAD-PPL_SFT.
      condense WA_UPLOAD-SAP.
      condense WA_UPLOAD-DESCR.

      WRITE : / 'DATA Already Uploaded: ' ,wa_upload-TABNAME ,',',wa_upload-FIELD, ',',
                                           WA_UPLOAD-PPL_SFT ,',',
                                           WA_UPLOAD-SAP ,',',
                                           WA_UPLOAD-DESCR.
    elseif sy-subrc = 0.

      FORMAT COLOR 5 INTENSIFIED OFF.
      condense wa_upload-TABNAME .
      condense wa_upload-FIELD.
      condense WA_UPLOAD-PPL_SFT.
      condense WA_UPLOAD-SAP.
      condense WA_UPLOAD-DESCR.
      WRITE : / 'DATA Uploaded: ' ,wa_upload-TABNAME ,',',wa_upload-FIELD, ',',
                                           WA_UPLOAD-PPL_SFT ,',',
                                           WA_UPLOAD-SAP ,',',
                                           WA_UPLOAD-DESCR.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " UPLOAD_TABLE
