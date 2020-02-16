*----------------------------------------------------------------------*
*   Developer:    SaurabhK
*   Date:         Thursday, September 14, 2017 11:32:53
*   Description:  BDC: PA30: Create new bank inforecord(infotype 9) for bank change for payroll
*   Request ID:   IRDK929254
*----------------------------------------------------------------------*
REPORT sy-repid
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*----------------------------------------------------------------------*
*   types                                                *
*----------------------------------------------------------------------*
TYPE-POOLS : slis, truxs, abap.

TABLES: sscrfields.

TYPES: BEGIN OF ty_file,
         pernr(8)  TYPE  c,        "rp50g-pernr,        "C  8   Personnel number
         begda(10) TYPE  c,        "p0009-begda,        "C  10  Begin date for inforecord
         trtyp(1)  TYPE  c,        "p0009-zztran_type,  "C  1   Transaction Type
       END OF ty_file,

       BEGIN OF ty_log,
         pernr     TYPE p0009-pernr,
         begda(10) TYPE c,                "p0009-begda,
         trtyp     TYPE p0009-zztran_type,
         log(256)  TYPE c,
       END OF ty_log.

*----------------------------------------------------------------------*
*   data declaration                                            *
*----------------------------------------------------------------------*
DATA: it_log TYPE TABLE OF ty_log,
      wa_log TYPE ty_log.

DATA: lv_msg   TYPE string.

* cl_salv* related *
DATA: lo_table     TYPE REF TO cl_salv_table,
      lo_container TYPE REF TO cl_gui_container,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_columns   TYPE REF TO cl_salv_columns_table,
      lo_column    TYPE REF TO cl_salv_column,
      lo_layout    TYPE REF TO cl_salv_layout,
      lo_layo      TYPE REF TO cl_salv_layout_service,
      lo_key       TYPE salv_s_layout_key,
      lo_info      TYPE salv_s_layout_info.

* file_format_download *
DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
      it_comp        TYPE abap_component_tab,
      wa_comp        TYPE abap_componentdescr.

* cl_gui_frontend_services=>file_save_dialog *
* cl_gui_frontend_services=>gui_download *
DATA: it_fileformat TYPE TABLE OF fieldnames,
      wa_fileformat TYPE fieldnames.

DATA: file      TYPE string,
      path      TYPE string,
      file_path TYPE string,
      title     TYPE string,
      defname   TYPE string,
      defext    TYPE string.
*----------------------------------------------------------------------*
*   include                                              *
*----------------------------------------------------------------------*
INCLUDE zzbdcrecx1.

*----------------------------------------------------------------------*
*   selection screen - additional elements
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-bt1,
                  BEGIN OF LINE,
                  PUSHBUTTON 1(20) text-btn USER-COMMAND dwn,
                  COMMENT 24(50) text-cmt,
                  END OF LINE,
                  END OF BLOCK b3.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

AT SELECTION-SCREEN.
  IF foreg EQ abap_true.
    MOVE 'A' TO ctumode.
  ELSEIF backg EQ abap_true.
    MOVE 'N' TO ctumode.
  ELSEIF error EQ abap_true.
    MOVE 'E' TO ctumode.
  ENDIF.

  IF sscrfields-ucomm EQ 'DWN'.
    PERFORM file_format_download.
  ENDIF.

*----------------------------------------------------------------------*
*   initialisation                                                *
*---------------------------------------------------------------------
  PERFORM refresh_data.

*----------------------------------------------------------------------*
*   start-of-selection                                               *
*---------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM file_to_tab.
  PERFORM bdc.
  PERFORM display_log.
  PERFORM cleanup.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_data .
  REFRESH: it, it_log, messtab, bdcdata, it_comp.
  CLEAR: wa, wa_log, lv_msg, wa_comp, file, path, file_path, title, defname, defext.
  FREE: lo_table,
        lo_container,
        lo_functions,
        lo_columns,
        lo_column,
        lo_layout,
        lo_layo,
        lo_key,
        lo_info,
        lo_structdescr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILE_FORMAT_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_format_download .
* get structure for header
  TRY .
      lo_structdescr ?= cl_abap_structdescr=>describe_by_data( p_data = wa ).
      it_comp = lo_structdescr->get_components( ).
    CATCH cx_root.
      MESSAGE 'Error while reading structure' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
  IF it_comp IS NOT INITIAL.
    LOOP AT it_comp INTO wa_comp.
      PERFORM fill_header USING wa_comp-name.
      CLEAR wa_comp.
    ENDLOOP.
    IF it_fileformat IS NOT INITIAL.
      PERFORM file_download.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->COL_NAME  text
*----------------------------------------------------------------------*
FORM fill_header  USING  VALUE(col_name).
  CLEAR wa_fileformat.
  wa_fileformat-fieldname = col_name.
  APPEND wa_fileformat TO it_fileformat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_download .
  CLEAR: file, path, file_path, title, defname, defext.
  title = 'File Format for PA30-Bank Change BDC'.
  defname = 'Bank_Change_PA30_BDC_File.xls'.
  defext = 'xls'.
  TRY.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = title
          default_file_name = defname
          default_extension = defext
        CHANGING
          filename          = file
          path              = path
          fullpath          = file_path.
    CATCH cx_root.
      MESSAGE 'File path could not be determined. File format not downloaded' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
  IF file_path IS INITIAL.
    MESSAGE 'No file specified. File not downloaded.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  TRY .
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename              = file_path
          write_field_separator = 'X'
          fieldnames            = it_fileformat
        CHANGING
          data_tab              = it.

      REFRESH it.
    CATCH cx_root.
      MESSAGE 'File could not be downloaded' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILE_TO_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_to_tab .
  IF NOT p_file IS INITIAL.
    v_file = p_file.
  ELSE.
    SET CURSOR FIELD 'P_FILE'.
    MESSAGE 'Fill in all the required fields.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF v_file IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_field_seperator    = abap_true
        i_line_header        = abap_true
        i_tab_raw_data       = it_raw
        i_filename           = v_file
      TABLES
        i_tab_converted_data = it
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      IF sy-msgid EQ 'UX' AND sy-msgno EQ '893'.  " Replace file cannot be processed with a meaningful error
        CONCATENATE 'Is file' v_file 'still open? Please close the file.' INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSE.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc .
* Note: Certain additional fields are also included but filled with no-data just in case for future use
  IF it IS NOT INITIAL.
    TRY .
        LOOP AT it INTO wa.
          MOVE-CORRESPONDING wa TO wa_log.  " Prepare for log output

          PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.  " Initial Screen
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=COP'.
          PERFORM bdc_field       USING 'RP50G-PERNR'       " Enter person number from file
                                        wa-pernr.           "'10106'.
          PERFORM bdc_field       USING 'RP50G-TIMR6'
                                        'X'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'RP50G-CHOIC'.
          PERFORM bdc_field       USING 'RP50G-CHOIC'
                                        '9'.

          PERFORM bdc_dynpro      USING 'MP000900' '2000'.  " 2nd screen after copy
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=UPD'.
          PERFORM bdc_field       USING 'P0009-BEGDA'       " Enter begin date for new inforec from file(format DD.MM.YYYY)
                                        wa-begda.           "'01.09.2017'.
          " Keep all other details default

          PERFORM bdc_field       USING 'P0009-ZZTRAN_TYPE'       " Update new transaction type from file
                                        wa-trtyp.                 "'D'.

          PERFORM bdc_transaction USING 'PA30'.

          READ TABLE messtab WITH KEY msgtyp = 'E'.
          IF sy-subrc <> 0.
            READ TABLE messtab WITH KEY msgtyp = 'S' msgid = 'PG' msgnr = '102'.  " Record Created
            IF sy-subrc = 0.
              " Do something
            ENDIF.
          ENDIF.
          " Fill log
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = messtab-msgid
              lang      = '-D'
              no        = messtab-msgnr
              v1        = messtab-msgv1
              v2        = messtab-msgv2
              v3        = messtab-msgv3
              v4        = messtab-msgv4
            IMPORTING
              msg       = wa_log-log
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            MOVE 'No messages could be read' TO wa_log-log.
          ENDIF.

          APPEND wa_log TO it_log.
          CLEAR: wa, wa_log.
        ENDLOOP.
      CATCH cx_sy_conversion_no_number .
        MESSAGE 'Input Data Error: Please check your file' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      CATCH cx_sy_conversion_overflow.
        MESSAGE 'Input Data Error: Please check your file' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
    ENDTRY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log .  " oo alv
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = lo_table
        CHANGING
          t_table      = it_log.
    CATCH cx_salv_msg .
  ENDTRY.

  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.

* change column headers
  FREE lo_column.
  TRY.
      lo_column ?= lo_columns->get_column( columnname = 'BEGDA' ).
    CATCH cx_salv_not_found.
  ENDTRY.
  lo_column->set_long_text( value = 'Start date' ).
  lo_column->set_medium_text( value = 'Start date' ).
  lo_column->set_short_text( value = 'Start date' ).

  FREE lo_column.
  TRY.
      lo_column ?= lo_columns->get_column( columnname = 'TRTYP' ).
    CATCH cx_salv_not_found.
  ENDTRY.
  lo_column->set_long_text( value = 'Transaction Type' ).
  lo_column->set_medium_text( value = 'Trans. Type' ).
  lo_column->set_short_text( value = 'Tr. Type' ).

  FREE lo_column.
  TRY.
      lo_column ?= lo_columns->get_column( columnname = 'LOG' ).
    CATCH cx_salv_not_found.
  ENDTRY.
  lo_column->set_long_text( value = 'Processing Log' ).
  lo_column->set_medium_text( value = 'Process. Log' ).
  lo_column->set_short_text( value = 'Log' ).

  CALL METHOD lo_columns->set_optimize. " Default input bool true

  CALL METHOD lo_table->get_functions
    RECEIVING
      value = lo_functions.

  CALL METHOD lo_functions->set_all. " Default input bool true

  CALL METHOD lo_table->get_layout
    RECEIVING
      value = lo_layout.

  lo_key-report = sy-repid.

  CALL METHOD lo_layout->set_key
    EXPORTING
      value = lo_key.

  CALL METHOD lo_layout->set_save_restriction.

  CALL METHOD lo_table->display.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEANUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cleanup .
  FREE: ctumode, nodata, v_file, lv_msg, file, path, file_path, title, defname, defext,
        bdcdata, messtab, wa, wa_log, wa_comp,
        bdcdata[], messtab[], it, it_raw, it_log, it_comp,
        lo_table, lo_container, lo_functions, lo_columns, lo_column, lo_layout, lo_layo, lo_key, lo_info, lo_structdescr.
ENDFORM.
