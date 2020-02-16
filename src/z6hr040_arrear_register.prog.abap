*&---------------------------------------------------------------------*
*& Report  Z6HR040_ARREAR_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6hr040_arrear_register.
TYPE-POOLS: kkblo, slis.
TABLES: z6hr039_arr_reg.


TYPES: BEGIN OF st_final,
         pernr      TYPE p_pernr,
         gjahr      TYPE gjahr,
         monat      TYPE monat,
         ename      TYPE emnam,
         month      TYPE char8,
         basic      TYPE pad_amt7s,
         da         TYPE pad_amt7s,
         hra        TYPE pad_amt7s,
         trans      TYPE pad_amt7s,
         educt      TYPE pad_amt7s,
         lv_enc     TYPE pad_amt7s,
         medic      TYPE pad_amt7s,
         overtime   TYPE pad_amt7s,
         shift      TYPE pad_amt7s,
         lta        TYPE pad_amt7s,
         incent     TYPE pad_amt7s,
         bonus      TYPE pad_amt7s,
         others     TYPE pad_amt7s,
         total      TYPE pad_amt7s,
         pf_arr     TYPE pad_amt7s,
         vpf_arr    TYPE pad_amt7s,
         tax        TYPE pad_amt7s,
         union      TYPE pad_amt7s,
         insur      TYPE pad_amt7s,
         oth_dedct  TYPE pad_amt7s,
         dedct      TYPE pad_amt7s,
         net_tot    TYPE pad_amt7s,
       END OF st_final,

       BEGIN OF st_pa0001,
         pernr      TYPE persno,
         ename      TYPE emnam,
       END OF st_pa0001.

***************************************Internal table and workarea******************************

DATA: it_arrears    TYPE STANDARD TABLE OF z6hr039_arr_reg,
      it_final      TYPE STANDARD TABLE OF st_final,
      it_pa0001     TYPE STANDARD TABLE OF st_pa0001,
      it_fieldcat   TYPE slis_t_fieldcat_alv.

DATA: wa_arrears    TYPE z6hr039_arr_reg,
      wa_fieldcat   TYPE slis_fieldcat_alv,
      wa_pa0001     TYPE st_pa0001,
      wa_final      TYPE st_final.

**************************************Selection parameters*************************************


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: s_pernr TYPE z6hr039_arr_reg-pernr  MATCHCODE OBJECT prem.
*SELECT-OPTIONS: s_pernr  FOR z6hr039_arr_reg-pernr  MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b1.

*************************************************************************************************


START-OF-SELECTION.

  PERFORM get_data.
  PERFORM collect_data.
  PERFORM output_list.
  PERFORM layout.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  REFRESH: it_arrears.
  SELECT * FROM z6hr039_arr_reg
    INTO TABLE it_arrears
    WHERE pernr = s_pernr.
  IF sy-subrc = 0.
    SORT it_arrears BY pernr gjahr monat.

    SELECT pernr
           ename
      FROM pa0001
      INTO TABLE it_pa0001
      FOR ALL ENTRIES IN it_arrears
      WHERE pernr = it_arrears-pernr.
    IF sy-subrc = 0.
      SORT it_pa0001 BY pernr.
    ENDIF.
  ELSE.
    MESSAGE 'No Data Selected' TYPE 'E'.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  COLLECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM collect_data .
  LOOP AT it_arrears INTO wa_arrears.

    CLEAR: wa_final.
    wa_final-pernr = wa_arrears-pernr.
    wa_final-gjahr = wa_arrears-gjahr.
    wa_final-monat = wa_arrears-monat.

    CLEAR: wa_pa0001.
    READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_arrears-pernr.
    IF sy-subrc = 0.
      wa_final-ename = wa_pa0001-ename.
    ENDIF.

    IF wa_arrears-monat IS NOT INITIAL.
      PERFORM get_month.
    ELSE.
      wa_final-gjahr = '2011'.
      wa_final-monat = '10'.
      wa_final-month = 'Jan-12'.
    ENDIF.

    PERFORM get_arrs.

    COLLECT wa_final INTO it_final.
  ENDLOOP.
  SORT it_final BY pernr gjahr monat.

ENDFORM.                    " COLLECT_DATA
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_list .

  PERFORM build_fieldcat USING:
*        'PERNR'     'IT_FINAL' 'Emp.No'              10 'R',
*        'ENAME'     'IT_FINAL' 'Emp.Name'            20 'L',
        'MONTH'     'IT_FINAL' 'Month'               10 'L',

        'BASIC'     'IT_FINAL' 'Basic'               15 'R',
        'DA'        'IT_FINAL' 'DA'                  15 'R',
        'HRA'       'IT_FINAL' 'HRA'                 15 'R',
        'TRANS'     'IT_FINAL' 'Transp.'             15 'R',
        'EDUCT'     'IT_FINAL' 'Educt.'              15 'R',
*        'LV_ENC'    'IT_FINAL' 'Leave.Enc.'          15 'R',
*        'MEDIC'     'IT_FINAL' 'Medical'             15 'R',
        'OVERTIME'  'IT_FINAL' 'Over.Time'           15 'R',
        'SHIFT'     'IT_FINAL' 'Shift.Allw.'         15 'R',
*        'LTA'       'IT_FINAL' 'LTA'                 15 'R',
        'INCENT'    'IT_FINAL' 'Prod.Incentive'      15 'R',
*        'BONUS'     'IT_FINAL' 'Bonus'               15 'R',
*        'OTHERS'    'IT_FINAL' 'Others'              15 'R',
        'TOTAL'     'IT_FINAL' 'Gross.Total.'        15 'R',

        'PF_ARR'    'IT_FINAL' 'PF.VPF.Arrears'      15 'R'
*        'VPF_ARR'   'IT_FINAL' 'VPF.arrears'         15 'R',
*        'TAX'       'IT_FINAL' 'Income.Tax'          15 'R',
*        'INSUR'     'IT_FINAL' 'Other.Insurance'     15 'R',
*        'OTH_DEDCT' 'IT_FINAL' 'Other.Deduction'     15 'R',
*        'DEDCT'     'IT_FINAL' 'Total.Deduction'     15 'R',

*        'NET_TOT'   'IT_FINAL' 'Net.Total'           15 'R'
        .

ENDFORM.                    " OUTPUT_LIST
*----------------------------------------------------------------------*
* build field catalog entry                                            *
*----------------------------------------------------------------------*
FORM build_fieldcat USING a_fieldname
                          a_tabname
                          a_heading
                          a_outputlen
                          a_justification.

  wa_fieldcat-fieldname = a_fieldname.
  wa_fieldcat-tabname = a_tabname.
  wa_fieldcat-seltext_l = a_heading.
  wa_fieldcat-outputlen = a_outputlen.
  wa_fieldcat-just = a_justification.
  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM.                    "BUILD_FIELDCAT
**&---------------------------------------------------------------------*
**&      Form  layout
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
      i_callback_program                = sy-repid
      i_callback_top_of_page            = 'TOP_OF_PAGE'
*     is_layout                         = g_layout
      it_fieldcat                       = it_fieldcat
      i_default                         = 'X'
      i_save                            = 'A'
*      IS_VARIANT                        = 'X'
    TABLES
      t_outtab                          = it_final
   EXCEPTIONS
      program_error                     = 1
*     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "layout
*&---------------------------------------------------------------------*
*&      Form  GET_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_month .

  DATA: month TYPE char8,
        year TYPE char2.

  CLEAR: month, year.
  IF wa_arrears-gjahr = '2011'.
    year = '11'.
  ELSEIF wa_arrears-gjahr = '2012'.
    year = '12'.
  ENDIF.

  CASE wa_arrears-monat.
    WHEN 01.
      month = 'Apr'.
    WHEN 02.
      month = 'May'.
    WHEN 03.
      month = 'Jun'.
    WHEN 04.
      month = 'Jul'.
    WHEN 05.
      month = 'Aug'.
    WHEN 06.
      month = 'Sep'.
    WHEN 07.
      month = 'Oct'.
    WHEN 08.
      month = 'Nov'.
    WHEN 09.
      month = 'Dec'.
    WHEN 10.
      month = 'Jan'.
      year = year + 1.
    WHEN 11.
      month = 'Feb'.
      year  = year + 1.
    WHEN 12.
      month = 'Mar'.
      year  = year + 1.
    WHEN OTHERS.

  ENDCASE.

  CONCATENATE month year INTO wa_final-month SEPARATED BY '-'.

ENDFORM.                    " GET_MONTH
*&---------------------------------------------------------------------*
*&      Form  GET_ARRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_arrs .

  CASE wa_arrears-lgart.
    WHEN '1101'.
      wa_final-lta = wa_final-lta + wa_arrears-amount.
    WHEN '1102'.
      wa_final-medic = wa_final-medic + wa_arrears-amount.
    WHEN '1109'.
      wa_final-others = wa_final-others + wa_arrears-amount.
    WHEN '1110'.
      wa_final-bonus = wa_final-bonus + wa_arrears-amount.
    WHEN '2009'.
      wa_final-oth_dedct = wa_final-oth_dedct + wa_arrears-amount.
    WHEN '4500'.
      wa_final-basic = wa_final-basic + wa_arrears-amount.
    WHEN '4501'.
      wa_final-da = wa_final-da + wa_arrears-amount.
    WHEN '4502'.
      wa_final-da = wa_final-da + wa_arrears-amount.
    WHEN '4503'.
      wa_final-hra = wa_final-hra + wa_arrears-amount.
    WHEN '4504'.
      wa_final-educt = wa_final-educt + wa_arrears-amount.
    WHEN '4505'.
      wa_final-trans = wa_final-trans + wa_arrears-amount.
    WHEN '4510'.
      wa_final-incent = wa_final-incent + wa_arrears-amount.
    WHEN '4601'.
      wa_final-overtime = wa_final-overtime + wa_arrears-amount.
    WHEN '4602'.
      wa_final-overtime = wa_final-overtime + wa_arrears-amount.
    WHEN '4603'.
      wa_final-overtime = wa_final-overtime + wa_arrears-amount.
    WHEN '4604'.
      wa_final-shift = wa_final-shift + wa_arrears-amount.
    WHEN '4605'.
      wa_final-shift = wa_final-shift + wa_arrears-amount.
    WHEN '4606'.
      wa_final-shift = wa_final-shift + wa_arrears-amount.
    WHEN '4607'.
      wa_final-shift = wa_final-shift + wa_arrears-amount.
    WHEN '4610'.
      wa_final-lv_enc = wa_final-lv_enc + wa_arrears-amount.
    WHEN '4611'.
      wa_final-lv_enc = wa_final-lv_enc + wa_arrears-amount.
    WHEN '4701'.
      wa_final-pf_arr = wa_final-pf_arr + wa_arrears-amount.
    WHEN '4702'.
      wa_final-vpf_arr = wa_final-vpf_arr + wa_arrears-amount.
    WHEN '/460'.
      wa_final-tax = wa_final-tax + wa_arrears-amount.
    WHEN '2004'.
      wa_final-insur = wa_final-insur + wa_arrears-amount.
    WHEN OTHERS.
  ENDCASE.

  wa_final-total = wa_final-basic
                 + wa_final-da
                 + wa_final-hra
                 + wa_final-trans
                 + wa_final-educt
*                 + wa_final-lv_enc
*                 + wa_final-medic
                 + wa_final-overtime
                 + wa_final-shift
*                 + wa_final-lta
                 + wa_final-incent
*                 + wa_final-bonus
*                 + wa_final-others
                 .

  wa_final-pf_arr = wa_final-pf_arr
                  + wa_final-vpf_arr.

  wa_final-dedct = wa_final-pf_arr
*                 + wa_final-vpf_arr
*                 + wa_final-tax
*                 + wa_final-insur
*                 + wa_final-oth_dedct
                 .

  wa_final-net_tot = wa_final-total
                   - wa_final-dedct.

ENDFORM.                    " GET_ARRS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA : t_header TYPE slis_t_listheader,
        wa_header TYPE slis_listheader,
        t_line LIKE wa_header-info,
        wa_text TYPE string.

* Title
  wa_header-typ  = 'H'.
  wa_header-info = 'Indofil Industries Limited'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.

* Heading
  wa_header-typ  = 'H'.
  wa_header-info = 'Arrear statement for the period Apr-11 to Nov-12'.
  APPEND wa_header TO t_header.
  CLEAR: wa_header, t_line.

  READ TABLE it_final INTO wa_final INDEX 1.
* Employee number
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_final-pernr
    IMPORTING
      output = wa_final-pernr.

  CLEAR: wa_text.
  CONCATENATE 'Emp.Code:' wa_final-pernr INTO wa_text SEPARATED BY ' '.
  wa_header-typ  = 'H'.
  wa_header-info = wa_text.
  APPEND wa_header TO t_header.
  CLEAR: wa_header, t_line.

* Employee name
  CLEAR: wa_text.
  CONCATENATE 'Emp.Name:' wa_final-ename INTO wa_text SEPARATED BY ' '.
  wa_header-typ  = 'H'.
  wa_header-info = wa_text.
  APPEND wa_header TO t_header.
  CLEAR: wa_header, t_line.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header
   "   i_logo             = ''.
  "i_end_of_list_grid       = ''
  "i_alv_form               = ''.

.
ENDFORM.                    "TOP_OF_PAGE
