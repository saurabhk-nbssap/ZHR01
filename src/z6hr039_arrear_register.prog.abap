*&---------------------------------------------------------------------*
*& Report  Z6HR039_ARREAR_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6hr039_arrear_register.


TYPES: BEGIN OF ty_datatab,
        lgart       TYPE lgart,
        lgart_text  TYPE char64,
        perpa       TYPE char4,
        monat       TYPE monat,
        gjahr       TYPE gjahr,
        paytyp      TYPE char8,
        payid       TYPE char8,
        bon_dt      TYPE char8,
        amount      TYPE char16,
       END OF ty_datatab.

***************************************Internal table and workarea******************************

DATA: it_datatab TYPE STANDARD TABLE OF ty_datatab,
      it_z6hr039_arr_reg TYPE STANDARD TABLE OF z6hr039_arr_reg.


DATA: wa_datatab TYPE ty_datatab,
      wa_z6hr039_arr_reg TYPE z6hr039_arr_reg.

DATA: file_path TYPE string.

DATA : gd_scol   TYPE i VALUE '1',
       gd_srow   TYPE i VALUE '1',
       gd_ecol   TYPE i VALUE '10',
       gd_erow   TYPE i VALUE '1000'.
**************************************Selection parameters*************************************


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_pernr  TYPE p_pernr OBLIGATORY MATCHCODE OBJECT prem,
            p_file   TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.


***************************************At selection for file browsing***************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

  file_path = p_file.


*************************************************************************************************

START-OF-SELECTION.

  CALL FUNCTION 'ENQUEUE_EPROG'
    EXPORTING
*     MODE_TRDIR           = 'E'
      programm             = sy-repid
*     X_PROGRAMM           = ' '
*     _SCOPE               = '2'
*     _WAIT                = ' '
*     _COLLECT             = ' '
   EXCEPTIONS
     foreign_lock         = 1
     system_failure       = 2
     OTHERS               = 3
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM upload_excel_file TABLES   it_datatab
                             USING   p_file
                                     gd_scol
                                     gd_srow
                                     gd_ecol
                                     gd_erow.

  PERFORM data_collect.
  PERFORM update_db.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_excel_file TABLES   p_table
                       USING    p_file
                                p_scol
                                p_srow
                                p_ecol
                                p_erow.


  DATA : lt_intern TYPE  kcde_cells OCCURS 0 WITH HEADER LINE.

  DATA : ld_index TYPE i.
  FIELD-SYMBOLS : <fs>.
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = p_file
      i_begin_col             = p_scol
      i_begin_row             = p_srow
      i_end_col               = p_ecol
      i_end_row               = p_erow
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED.
    WRITE:/ 'Error Uploading file'.
    EXIT.
  ENDIF.

  IF lt_intern[] IS INITIAL.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED.
    WRITE:/ 'No Data Uploaded'.
    EXIT.
  ELSE.
    SORT lt_intern BY row col.
    LOOP AT lt_intern.
      MOVE lt_intern-col TO ld_index.
      ASSIGN COMPONENT ld_index OF STRUCTURE p_table TO <fs>.
      MOVE lt_intern-value TO <fs>.
      AT END OF row.
        APPEND p_table.
        CLEAR p_table.
      ENDAT.
    ENDLOOP.
  ENDIF.


ENDFORM.                    "UPLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  DATA_COLLECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_collect .

  LOOP AT it_datatab INTO wa_datatab.
    CLEAR: wa_z6hr039_arr_reg.

    wa_z6hr039_arr_reg-pernr  = p_pernr.
    wa_z6hr039_arr_reg-lgart  = wa_datatab-lgart.
    wa_z6hr039_arr_reg-gjahr  = wa_datatab-gjahr.
    wa_z6hr039_arr_reg-monat  = wa_datatab-monat.

    REPLACE ALL OCCURRENCES OF ',' IN wa_datatab-amount WITH ''.
    CONDENSE wa_datatab-amount.
    wa_z6hr039_arr_reg-amount = wa_datatab-amount.

    APPEND wa_z6hr039_arr_reg TO it_z6hr039_arr_reg.
    CLEAR: wa_datatab.
  ENDLOOP.

ENDFORM.                    " DATA_COLLECT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_db .
  DATA: lv_pernr TYPE p_pernr.

  CLEAR: lv_pernr.
  SELECT SINGLE pernr FROM z6hr039_arr_reg
    INTO lv_pernr WHERE pernr = p_pernr.
  IF sy-subrc <> 0.
    SORT it_z6hr039_arr_reg BY pernr lgart gjahr monat.
    INSERT z6hr039_arr_reg FROM TABLE it_z6hr039_arr_reg ACCEPTING DUPLICATE KEYS.
    COMMIT WORK.
  ELSE.
    MESSAGE 'No record Updated - Data Already Exists' TYPE 'E'.
  ENDIF.

  CLEAR: lv_pernr.
  SELECT SINGLE pernr FROM z6hr039_arr_reg
    INTO lv_pernr WHERE pernr = p_pernr.
  IF sy-subrc = 0.
    MESSAGE 'Successfully Uploaded' TYPE 'S'.
  ELSE.
    MESSAGE 'No record Updated' TYPE 'E'.
  ENDIF.

ENDFORM.                    " UPDATE_DB
