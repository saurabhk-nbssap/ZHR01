
*&---------------------------------------------------------------------*
*& REPORT  Z6HR004_BANK_FORMAT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Leave Balances Report
* OBJECT TYPE       : Report                FUNC. CONSULTANT  :RAM MANOHAR
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 02.12.2010
*        DEV REQUEST: IRDK903585
*  TCODE            : ZPY_LEAVE
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

REPORT  z6hr035r_leave_balances.

TYPE-POOLS : truxs,slis.

TABLES : hrpy_rgdir,
         pa0000,
         pa0001,
         pa0002,
         pa0008.

TYPES : BEGIN OF i_final,
        pernr TYPE pernr_d,
        ename TYPE emnam,
        gbdat TYPE gbdat,
        begda TYPE datum,
        endda TYPE datum,
        amont TYPE pad_amt7s,
        amont_mon TYPE pad_amt7s,   "Anees
        kostl TYPE kostl, "Anees
        ktext TYPE ktext, "Anees
        anzhl TYPE ptm_quonum,
        persk TYPE persk,
        limit TYPE ptm_quonum,
        END OF i_final.
TYPES : BEGIN OF i_begda,
        pernr TYPE pernr_d,
        begda TYPE datum,
        persg TYPE persg,
        END OF i_begda.
FIELD-SYMBOLS : <f1>,<g1>.
DATA : it_final TYPE TABLE OF i_final.
DATA : wa_final TYPE i_final.
DATA : v_cnt(2) TYPE n.
DATA : v_text(20) TYPE c.
DATA : v_tabix TYPE sy-tabix.

DATA : it_text_data TYPE truxs_t_text_data,
       wa_text_data(4096) TYPE c.
DATA : it_pa0001 TYPE pa0001_itab,
       wa_pa0001 TYPE pa0001,
       wa_pa0006 TYPE pa2006.
DATA : wa_pa0008 TYPE pa0008.
DATA : wa_pa0002 TYPE pa0002.

DATA : payresult     TYPE pay99_result.
DATA : p_fpper       LIKE hrpy_rgdir-fpper,
       wa_pernr      LIKE pa0009-pernr,
       wa_first_date TYPE sy-datum,
       wa_last_date  TYPE sy-datum.

DATA: ld_filename TYPE string,
      ld_path TYPE string,
      ld_fullpath TYPE string,
      ld_result TYPE i.

DATA : f_period TYPE t009b-poper,
       f_year   TYPE t009b-bdatj.

*Declarations for ALV Grid.

DATA : wa_header TYPE slis_listheader.
DATA : t_header TYPE slis_t_listheader WITH HEADER LINE.

DATA:   it_st_list_top_of_page TYPE slis_t_listheader,
        it_st_fieldcat         TYPE slis_t_fieldcat_alv,
        i_events           TYPE slis_t_event,
        st_layout              TYPE slis_layout_alv,
        s_repid   LIKE sy-repid,
        s_variant LIKE disvariant,
        code LIKE disvariant-handle.
DATA : w_gt_sort TYPE slis_t_sortinfo_alv .
DATA : it_begda TYPE STANDARD TABLE OF i_begda,
      it_pa0302 TYPE STANDARD TABLE OF pa0302,
      wa_pa0302 TYPE pa0302,
      wa_begda TYPE i_begda.
*&---------------------------------------------------------------------*
*&                      SELECTION SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE text-001 .
SELECT-OPTIONS : s_pernr FOR pa0000-pernr
                   MATCHCODE OBJECT prem,
                 s_persg FOR pa0001-persg,
                 s_abkrs FOR pa0001-abkrs,
                 s_stat2 FOR pa0000-stat2,
                 s_werks FOR pa0001-werks,
                 s_date FOR pa0001-begda,
                 s_bukrs FOR pa0001-bukrs.

SELECTION-SCREEN END OF BLOCK a01.



*  **--------------------------------------------------------------------*
*       Initialisation
**---------------------------------------------------------------------*
INITIALIZATION.
*-------------*
  s_repid = sy-repid.
  PERFORM variant_init.


*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM get_employee_data.

  PERFORM f_display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  GET_SBI_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM f_display_data .


  PERFORM fieldcat1 USING it_st_fieldcat[]. " PUT DATA IN ALV
  PERFORM layout_build USING st_layout.     " BUILD LAYOUT
  PERFORM f4000_events_init CHANGING i_events.
  PERFORM reuse_alv_list_display.


  REFRESH it_st_list_top_of_page.
  CLEAR   it_st_list_top_of_page.
  REFRESH it_st_fieldcat.
  CLEAR   it_st_fieldcat.
*      PERFORM DOWNLOAD.

ENDFORM.                    " GET_SBI_DATA
" GET_HDFC_DATA
*&---------------------------------------------------------------------*
*&      FORM  GET_PAYROLL_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM get_employee_data.
  DATA: date_temp TYPE dats,
        i TYPE i.

  SELECT * FROM pa0000
         INTO CORRESPONDING FIELDS OF TABLE it_final
                                 WHERE pernr IN s_pernr
*                                 and   begda le sy-datum
                                 AND   endda GE sy-datum
                                 AND   stat2 IN s_stat2.
  IF sy-subrc = 0.
    SORT it_final BY pernr.
    LOOP AT it_final INTO wa_final.
      i = 1.
      AT NEW pernr.
        i = 0.
      ENDAT.
      IF i = 1.
        DELETE it_final WHERE pernr = wa_final-pernr AND endda = wa_final-endda.
      ENDIF.
    ENDLOOP.

    "Anees "For BEGDA = date of joining
    SELECT * FROM pa0001
             INTO CORRESPONDING FIELDS OF TABLE it_begda
                                     WHERE pernr IN s_pernr.
*                                 AND   STAT2 IN s_STAT2.
    DELETE it_begda WHERE persg = 'T'.
    LOOP AT it_final INTO wa_final.
      i = 0.
      CLEAR date_temp.
      LOOP AT it_begda INTO wa_begda WHERE pernr = wa_final-pernr.
        IF i = 0.
          date_temp = wa_begda-begda.
        ENDIF.
        IF i = 1.
          IF date_temp > wa_begda-begda.
            DELETE it_begda WHERE begda = date_temp
                              AND pernr = wa_final-pernr.
            date_temp = wa_begda-begda.
          ELSE.
            DELETE it_begda WHERE begda = wa_begda-begda
                              AND pernr = wa_final-pernr.
          ENDIF.
        ENDIF.
        i = 1.
        CLEAR wa_begda.
      ENDLOOP.
      CLEAR wa_final.
    ENDLOOP.
    LOOP AT it_final INTO wa_final.
      READ TABLE it_begda INTO wa_begda WITH KEY pernr = wa_final-pernr.
      IF sy-subrc = 0.
        wa_final-begda = wa_begda-begda.
        MODIFY it_final FROM wa_final.
      ENDIF.
      CLEAR wa_final.
    ENDLOOP.
  ENDIF.
  "Deleting records of those who resigned
  IF NOT it_final IS INITIAL.
    SELECT * FROM pa0302 INTO TABLE it_pa0302
      FOR ALL ENTRIES IN it_final
      WHERE pernr = it_final-pernr
        AND massn = 'I7'.
*      AND massg = '01'.
    IF sy-subrc = 0.
      LOOP AT it_pa0302 INTO wa_pa0302.
        READ TABLE it_final INTO wa_final WITH KEY pernr = wa_pa0302-pernr.
        IF sy-subrc = 0.
          DELETE it_final WHERE pernr = wa_pa0302-pernr.
        ENDIF.
        CLEAR wa_pa0302.
      ENDLOOP.
    ENDIF.
  ENDIF.
  "End
  IF NOT it_final IS INITIAL.
    SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE it_pa0001
                                 FOR ALL ENTRIES IN it_final
                                 WHERE pernr EQ it_final-pernr
                                   AND begda LE sy-datum
                                   AND endda GE sy-datum
                                   AND werks IN s_werks
                                   AND abkrs IN s_abkrs
                                   AND persg IN s_persg
                                   AND bukrs IN s_bukrs.            "Anees
    DELETE it_pa0001 WHERE persg = 'T'. "Anees
  ENDIF.
  LOOP AT it_final INTO wa_final.
    v_tabix = sy-tabix.
    READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_final-pernr.
    IF sy-subrc EQ 0.
      wa_final-persk = wa_pa0001-persk.
      wa_final-ename = wa_pa0001-ename.
      wa_final-kostl = wa_pa0001-kostl.
      IF wa_final-kostl IS NOT INITIAL.
        SELECT SINGLE ktext FROM cskt INTO wa_final-ktext
          WHERE kostl = wa_final-kostl.
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM  pa0002 INTO wa_pa0002 WHERE pernr EQ wa_final-pernr
                                                  AND begda LE sy-datum
                                                  AND endda GE sy-datum.
    IF sy-subrc EQ 0.
      wa_final-gbdat = wa_pa0002-gbdat.


    ENDIF.

    IF wa_pa0001-persg EQ 'M'  OR wa_pa0001-persg EQ 'D' OR wa_pa0001-persg EQ 'N'.
      wa_final-limit = 240.
    ELSEIF wa_pa0001-persg EQ 'W'  OR wa_pa0001-persg EQ 'S'.
      wa_final-limit = 180.
    ENDIF.
    SELECT SINGLE * FROM  pa2006 INTO wa_pa0006 WHERE pernr EQ wa_final-pernr
                                                  AND subty EQ '01'
                                                  AND begda LE sy-datum
                                                  AND endda GE sy-datum.
    IF sy-subrc EQ 0.
      wa_final-anzhl = wa_pa0006-anzhl - wa_pa0006-kverb.
    ENDIF.
    SELECT SINGLE * FROM  pa0008 INTO wa_pa0008 WHERE pernr EQ wa_final-pernr
                                                  AND begda LE sy-datum
                                                  AND endda GE sy-datum.
    IF sy-subrc EQ 0.
      wa_final-anzhl = wa_pa0006-anzhl - wa_pa0006-kverb.
      DO.
        v_cnt = sy-index.
        CONCATENATE 'LGA' v_cnt INTO v_text.
        ASSIGN COMPONENT v_text OF STRUCTURE wa_pa0008 TO <f1>.
        CONCATENATE 'BET' v_cnt INTO v_text.
        ASSIGN COMPONENT v_text OF STRUCTURE wa_pa0008 TO <g1>.

        IF <f1> EQ '1001' OR <f1> EQ '1002' OR <f1> EQ '1003'.
          wa_final-amont = wa_final-amont + <g1>.
        ENDIF.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
      wa_final-amont_mon = wa_final-amont.
      wa_final-amont = wa_final-amont * 12.
    ENDIF.

    MODIFY it_final FROM wa_final INDEX v_tabix.
    CLEAR  wa_final.
  ENDLOOP.
  DELETE it_final WHERE amont_mon IS  INITIAL.  "Anees
  DELETE it_final WHERE ename IS  INITIAL.      "Anees
  DELETE it_final WHERE begda NOT IN s_date.
  SORT it_final BY pernr.
ENDFORM.                    " GET_PAYROLL_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download .





ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .



ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ST_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat1 USING p_it_st_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.


  ls_fieldcat-fieldname = 'PERNR'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Employee'.
  ls_fieldcat-seltext_l = 'Employee'.
  ls_fieldcat-seltext_s = 'Employee'.
  ls_fieldcat-outputlen = 8.
  ls_fieldcat-ref_fieldname = 'PERNR'.
  ls_fieldcat-ref_tabname = 'PA0001'.
  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'ENAME'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Emp.Name'.
  ls_fieldcat-ref_fieldname = 'ENAME'.
  ls_fieldcat-ref_tabname = 'PA0001'.
  ls_fieldcat-outputlen = 40.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'GBDAT'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Birth.Date'.
  ls_fieldcat-ref_fieldname = 'GBDAT'.
  ls_fieldcat-ref_tabname = 'PA0002'.
  ls_fieldcat-outputlen = 10.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-fieldname = 'BEGDA'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Date.Of Joining'.
  ls_fieldcat-outputlen = 10.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  "Anees
  ls_fieldcat-fieldname = 'AMONT_MON'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Basic + Da Per Month'.
*    LS_FIELDCAT-ref_fieldname = 'BET01'.
*    LS_FIELDCAT-ref_TABname = 'PA0008'.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.
  "End

*    LS_FIELDCAT-FIELDNAME = 'AMONT'.
*    LS_FIELDCAT-tabname = 'IT_FINAL'.
*    LS_FIELDCAT-SELTEXT_M = 'Basic + Da'.
**    LS_FIELDCAT-ref_fieldname = 'BET01'.
**    LS_FIELDCAT-ref_TABname = 'PA0008'.
*
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.


  ls_fieldcat-fieldname = 'ANZHL'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Leave Balances'.
  ls_fieldcat-seltext_l = 'Leave Balances'.
  ls_fieldcat-seltext_s = 'Leave Balances'.
  ls_fieldcat-ddictxt = 'M'.
  ls_fieldcat-ref_fieldname = 'ANZHL'.
  ls_fieldcat-ref_tabname = 'PA2006'.


  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-fieldname = 'PERSK'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Grade '.
  ls_fieldcat-ref_fieldname = 'PERSK'.
  ls_fieldcat-ref_tabname = 'PA0001'.


  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'LIMIT'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Limit '.
  ls_fieldcat-ddictxt = 'M'.
  ls_fieldcat-ref_fieldname = 'ANZHL'.
  ls_fieldcat-ref_tabname = 'PA2006'.


  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  "Anees
  ls_fieldcat-fieldname = 'KOSTL'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Cost.Center'.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'KTEXT'.
  ls_fieldcat-tabname = 'IT_FINAL'.
  ls_fieldcat-seltext_m = 'Cost.Center'.

  APPEND ls_fieldcat TO p_it_st_fieldcat.
  CLEAR ls_fieldcat.
  "End

ENDFORM.                                                    " FIELDCAT1
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout_build USING wa_layout TYPE slis_layout_alv.
  wa_layout-zebra             = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  wa_layout-no_totalline      = ' '.
  wa_layout-info_fieldname      = 'COLOR'.
  wa_layout-confirmation_prompt = 'X'.
*  wa_LAYOUT-edit = 'X'.
ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  F4000_EVENTS_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_EVENTS  text
*----------------------------------------------------------------------*
FORM f4000_events_init  CHANGING p_i_events.

  DATA: line_event TYPE slis_alv_event.

  CLEAR line_event.
  line_event-name = 'PF_STATUS_SET'.
  line_event-form = 'F4200_PF_STATUS_SET'.
  APPEND line_event TO i_events.

ENDFORM.                    " F4000_EVENTS_INIT
*---------------------------------------------------------------------*
* FORM F4200_PF_STATUS_SET *
*---------------------------------------------------------------------*

FORM f4200_pf_status_set USING i_extab TYPE slis_t_extab.

  REFRESH i_extab.
*  SET PF-STATUS 'STANDARD' OF PROGRAM 'SAPLSALV' EXCLUDING i_extab.
  SET PF-STATUS 'ZBANK1'.
ENDFORM.                    "f4200_pf_status_set
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LIST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reuse_alv_list_display .



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING

   i_callback_program                = s_repid
*   I_CALLBACK_PF_STATUS_SET          = ' '
   i_callback_user_command           = 'USER_COMMAND'
   i_callback_top_of_page            = 'TOP-OF-PAGE'

   i_background_id                   = 'ALV_BACKGROUND'
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   is_layout                         = st_layout
   it_fieldcat                       = it_st_fieldcat[]

   i_default                         = 'X'
   i_save                            = 'A'
*   IS_VARIANT                        =
   it_events                         = i_events

*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    t_outtab                          = it_final
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " REUSE_ALV_LIST_DISPLAY




*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top-of-page.
*----------------*
  wa_header-typ = 'A'.
  wa_header-info = 'Employee Leave Balances'.
  APPEND wa_header TO t_header.
****
*  WA_HEADER-TYP = 'A'.
*  concatenate 'Company Code:' p_bukrs
*  INTO WA_HEADER-INFO SEPARATED BY SPACE.
*  APPEND WA_HEADER TO T_HEADER.
****
*  WA_HEADER-TYP = 'A'.
*
*  CONCATENATE 'Posting Date From : ' s_BUDAT-LOW+6(2) '-'  s_BUDAT-LOW+4(2) '-'
*   s_BUDAT-LOW(4) 'TO :' s_BUDAT-HIGH+6(2) '-'  s_BUDAT-HIGH+4(2) '-'
*   s_BUDAT-HIGH(4) INTO WA_HEADER-INFO SEPARATED BY SPACE.
*  APPEND WA_HEADER TO T_HEADER.

*  ***
*  WA_HEADER-TYP = 'A'.
*  concatenate 'Fiscal Year:' p_gjahr
*  INTO WA_HEADER-INFO SEPARATED BY SPACE.
*  APPEND WA_HEADER TO T_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary       = t_header[]
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
            .
  CLEAR t_header.
  CLEAR t_header[].
ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      FORM USER_COMMAND
*&---------------------------------------------------------------------*

FORM user_command USING r_ucomm LIKE sy-ucomm
rs_user_command TYPE slis_selfield.
  CASE r_ucomm .
    WHEN 'DOWNLOAD'.
      PERFORM download.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_init .

  CLEAR s_variant.
  s_variant-report = s_repid.
  s_variant-handle = code.

ENDFORM.                    " VARIANT_INIT
