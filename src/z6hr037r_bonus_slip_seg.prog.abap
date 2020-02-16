*&---------------------------------------------------------------------*
*& Report  Z6HR037R_BONUS_SLIP_SEG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6hr037r_bonus_slip_seg.
TABLES : hrpy_rgdir, pa0001, t512t.

TYPES : BEGIN OF st_itab,
          pernr TYPE pernr_d,
          ename TYPE char40,
          tearn TYPE pad_amt7s,
          tdeduc TYPE pad_amt7s,
          kostl TYPE char10,
          ktext TYPE ktext, "char40,
          begda TYPE dats,
          apr TYPE pad_amt7s,
          may TYPE pad_amt7s,
          jun TYPE pad_amt7s,
          jul TYPE pad_amt7s,
          aug TYPE pad_amt7s,
          sep TYPE pad_amt7s,
          oct TYPE pad_amt7s,
          nov TYPE pad_amt7s,
          dmb TYPE pad_amt7s,
          jan TYPE pad_amt7s,
          feb TYPE pad_amt7s,
          mar TYPE pad_amt7s,
 END OF st_itab.

DATA: itab TYPE STANDARD TABLE OF st_itab,
      wa_itab TYPE st_itab,
      it_final TYPE z6hr037t_bonus_slip,
      wa_final TYPE z6hr037_bonus_slip.

DATA: itab_basic TYPE STANDARD TABLE OF st_itab,
      wa_itab_basic TYPE st_itab,
      it_final_basic TYPE z6hr037t_bonus_slip,
      wa_final_basic TYPE z6hr037_bonus_slip.

DATA: itab_da TYPE STANDARD TABLE OF st_itab,
      wa_itab_da TYPE st_itab,
      it_final_da TYPE z6hr037t_bonus_slip,
      wa_final_da TYPE z6hr037_bonus_slip.

DATA: p_bonus(3) TYPE n,
      lf_fm_name TYPE rs38l_fnam.
DATA: it_pa0001 TYPE STANDARD TABLE OF pa0001,
      wa_pa0001 TYPE pa0001,
      it_hrp1000_dep TYPE STANDARD TABLE OF hrp1000,
      it_hrp1000_des TYPE STANDARD TABLE OF hrp1000,
      wa_hrp1000 TYPE hrp1000,
      it_pa0009 TYPE STANDARD TABLE OF pa0009,
      wa_pa0009 TYPE pa0009.

FIELD-SYMBOLS : <table>    TYPE  table .     " Main Internal Table
FIELD-SYMBOLS : <table_basic>    TYPE  table .     " Main Internal Table
FIELD-SYMBOLS : <table_da>    TYPE  table .     " Main Internal Table
*&---------------------------------------------------------------------*
*&      Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-015.

SELECT-OPTIONS : s_pernr FOR pa0001-pernr MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK a .

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-016.

SELECT-OPTIONS : s_abkrs FOR hrpy_rgdir-abkrs MATCHCODE OBJECT h_t549t OBLIGATORY.
PARAMETERS     : p_gjahr LIKE t549q-pabrj OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

*&---------------------------------------------------------------------*
*&      Main Processing
*&---------------------------------------------------------------------*
p_bonus = '20'.

SUBMIT z6hr037r_bonus_slip_c
     WITH s_pernr IN s_pernr
     WITH s_abkrs IN s_abkrs
     WITH p_gjahr = p_gjahr
     WITH p_bonus = p_bonus
 EXPORTING LIST TO MEMORY
 AND RETURN.

assign itab to <table>.
ASSIGN itab_basic to <table_basic>.
ASSIGN itab_da to <table_da>.

IMPORT itab = <table> FROM MEMORY ID 'BONUS'.
IMPORT itab_basic = <table_basic> FROM MEMORY ID 'BONUS_BASIC'.
IMPORT itab_da = <table_da> FROM MEMORY ID 'BONUS_DA'.


SORT itab BY pernr.

SELECT * FROM pa0009 INTO TABLE it_pa0009
      FOR ALL ENTRIES IN itab
      WHERE pernr = itab-pernr
        AND begda <= sy-datum
        AND endda >= sy-datum.
IF sy-subrc = 0.
  SORT it_pa0009 BY pernr.
ENDIF.

SELECT * FROM pa0001 INTO TABLE it_pa0001
      FOR ALL ENTRIES IN itab
      WHERE pernr = itab-pernr
        AND begda <= sy-datum
        AND endda >= sy-datum.

IF sy-subrc = 0.
  SORT it_pa0001 BY pernr.
  SELECT * FROM hrp1000 INTO TABLE it_hrp1000_dep
      FOR ALL ENTRIES IN it_pa0001
      WHERE objid = it_pa0001-orgeh
        AND otype = 'O'.

  SELECT * FROM hrp1000 INTO TABLE it_hrp1000_des
      FOR ALL ENTRIES IN it_pa0001
      WHERE objid = it_pa0001-plans
        AND otype = 'S'.

ENDIF.

LOOP AT itab INTO wa_itab.
  MOVE-CORRESPONDING wa_itab TO wa_final.

  READ TABLE it_pa0001 INTO wa_pa0001
                  WITH KEY pernr = wa_itab-pernr.
  IF sy-subrc = 0.
    wa_final-grad = wa_pa0001-persk.

    READ TABLE it_hrp1000_dep INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-orgeh.
    IF sy-subrc = 0.
      wa_final-dept = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.

    READ TABLE it_hrp1000_des INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-plans.
    IF sy-subrc = 0.
      wa_final-desg = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.
  ENDIF.

  READ TABLE it_pa0009 INTO wa_pa0009
                 WITH KEY pernr = wa_itab-pernr.
  IF sy-subrc = 0.
    wa_final-bankn = wa_pa0009-bankn.
  ENDIF.

  APPEND wa_final TO it_final.
  CLEAR: wa_itab, wa_final, wa_pa0001, wa_hrp1000.
ENDLOOP.

* BASIC
LOOP AT itab_basic INTO wa_itab_basic.
  MOVE-CORRESPONDING wa_itab_basic TO wa_final_basic.

  READ TABLE it_pa0001 INTO wa_pa0001
                  WITH KEY pernr = wa_itab_basic-pernr.
  IF sy-subrc = 0.
    wa_final_basic-grad = wa_pa0001-persk.

    READ TABLE it_hrp1000_dep INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-orgeh.
    IF sy-subrc = 0.
      wa_final_basic-dept = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.

    READ TABLE it_hrp1000_des INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-plans.
    IF sy-subrc = 0.
      wa_final_basic-desg = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.
  ENDIF.

  READ TABLE it_pa0009 INTO wa_pa0009
                 WITH KEY pernr = wa_itab_basic-pernr.
  IF sy-subrc = 0.
    wa_final_basic-bankn = wa_pa0009-bankn.
  ENDIF.

  APPEND wa_final_basic TO it_final_basic.
  CLEAR: wa_itab_basic, wa_final_basic, wa_pa0001, wa_hrp1000.
ENDLOOP.

*DA

LOOP AT itab_da INTO wa_itab_da .
  MOVE-CORRESPONDING wa_itab_da  TO wa_final_da .

  READ TABLE it_pa0001 INTO wa_pa0001
                  WITH KEY pernr = wa_itab_da-pernr.
  IF sy-subrc = 0.
    wa_final_da-grad = wa_pa0001-persk.

    READ TABLE it_hrp1000_dep INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-orgeh.
    IF sy-subrc = 0.
      wa_final_da-dept = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.

    READ TABLE it_hrp1000_des INTO wa_hrp1000
                  WITH KEY objid = wa_pa0001-plans.
    IF sy-subrc = 0.
      wa_final_da-desg = wa_hrp1000-stext.
      CLEAR wa_hrp1000.
    ENDIF.
  ENDIF.

  READ TABLE it_pa0009 INTO wa_pa0009
                 WITH KEY pernr = wa_itab_da-pernr.
  IF sy-subrc = 0.
    wa_final_da-bankn = wa_pa0009-bankn.
  ENDIF.

  APPEND wa_final_da  TO it_final_da .
  CLEAR: wa_itab_da , wa_final_da , wa_pa0001, wa_hrp1000.
ENDLOOP.


*&---------------------------------------------------------------------*
*&      Smart Forms
*&---------------------------------------------------------------------*

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname                 = 'Z6HR037S_BONUS_SLIP_SEG'
*   VARIANT                  = ' '
*   DIRECT_CALL              = ' '
  IMPORTING
    fm_name                  = lf_fm_name
* EXCEPTIONS
*   NO_FORM                  = 1
*   NO_FUNCTION_MODULE       = 2
*   OTHERS                   = 3
          .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


CALL FUNCTION lf_fm_name
EXPORTING
  p_gjahr = p_gjahr
TABLES
  it_final = it_final
  it_final_basic = it_final_basic
  it_final_da = it_final_da
EXCEPTIONS
  formatting error    = 1
  internal_error      = 2
  send_error          = 3
  user_cancelled      = 4
  OTHERS              = 5 .

IF sy-subrc <> 0.
* Error handling
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
