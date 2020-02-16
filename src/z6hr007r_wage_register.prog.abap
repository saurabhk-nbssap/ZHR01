*&---------------------------------------------------------------------*
*& Report  Z6HR007R_WAGE_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR007R_WAGE_REGISTER.


*&---------------------------------------------------------------------*
*&                      TABLES
*&---------------------------------------------------------------------*

TABLES : hrpy_rgdir, PA0001, t512t.


*&---------------------------------------------------------------------*
*&                      TYPE POOLS
*&---------------------------------------------------------------------*

type-pools : slis.

*&---------------------------------------------------------------------*
*&                      Selection Screen
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
*{   REPLACE        SBXK900030                                        1
*\PARAMETER : p_var  LIKE  disvariant-variant .
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Friday, October 12, 2018 13:15:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Use PARAMETERS, not PARAMETER
* Solution   - Replace PARAMETERS with PARAMETER
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
  PARAMETERS : p_var LIKE disvariant-variant.
*}   REPLACE
  SELECTION-SCREEN END   OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-015.
SELECT-OPTIONS : s_pernr FOR PA0001-pernr
                   MATCHCODE OBJECT PREM.

*                 s_paydt FOR hrpy_rgdir-paydt OBLIGATORY.
*PARAMETERS: S_PAYDT LIKE hrpy_rgdir-paydt OBLIGATORY.
*PARAMETERS: p_FPPER LIKE hrpy_rgdir-FPPER OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a .

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-016.

SELECT-OPTIONS : s_ABKRS for hrpy_rgdir-ABKRS
                   MATCHCODE OBJECT H_T549T OBLIGATORY.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 2(30) text-007.
PARAMETERS:p_PABRP like QPPNP-pabrp OBLIGATORY,
           p_PABRJ like QPPNP-PABRJ OBLIGATORY.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK b01.

data : p_FPPER LIKE hrpy_rgdir-FPPER.

*&---------------------------------------------------------------------*
*&                      FIELD SYMBOLS
*&---------------------------------------------------------------------*

FIELD-SYMBOLS : <table>    TYPE  table ,     " Main Internal Table
                <struc> ,
                <table1>   TYPE  table,
                <struc1>,
                <table2>   TYPE  table,
                <struc2>,

                <table3>   TYPE  table,
                <struc3>,
                <wstruc>,                    " Header Struct for <table>
                <field> ,
                <component> .

DATA      :   alv_fieldcat TYPE              slis_t_fieldcat_alv ,
              lt_alv_cat   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat1   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat2   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat3   TYPE  TABLE OF    lvc_s_fcat ,
              P_VARI LIKE DISVARIANT-VARIANT,
               i_repid   LIKE sy-repid,
              it_fieldcat  LIKE  LINE  OF    lt_alv_cat .

DATA      :   i_table      TYPE  REF   TO    data ,
              i_struct     TYPE  REF   TO    data ,
              tabix        TYPE sy-tabix.
*
data : v_recnt type i.
DATA : BEGIN OF it_field_dynamic OCCURS 0,
      fieldname LIKE dd03l-fieldname,
      reftab  LIKE dd03l-tabname,
      reffield  LIKE dd03l-reffield,
      desc(35),
      END OF it_field_dynamic.

*&---------------------------------------------------------------------*
*&                      ALV Declarations
*&---------------------------------------------------------------------*


**-- FOR TREE ALV
DATA : g_save(1)            TYPE  c                      ,
       g_exit(1)            TYPE  c                      ,
       g_variant            LIKE  disvariant             ,
       gx_variant           LIKE  disvariant             ,
       f2code               LIKE  sy-ucomm VALUE  '&ETA' ,
       layout               TYPE  slis_layout_alv        ,
       prg                  LIKE  sy-repid               ,
       keyinfo  TYPE slis_keyinfo_alv,
       gt_list_top_of_page  TYPE  slis_t_listheader      ,
       gt_events            TYPE  slis_t_event           ,
       fieldcat             TYPE  slis_t_fieldcat_alv    .

*&---------------------------------------------------------------------*
*&                      DATA DECLARATIONS
*&---------------------------------------------------------------------*

DATA : BEGIN OF itab1 OCCURS 0 ,
           pernr LIKE hrpy_rgdir-pernr,

           paydt LIKE hrpy_rgdir-paydt,
           seqnr LIKE hrpy_rgdir-seqnr,

       END OF itab1.

DATA : BEGIN OF rt1 occurs 0.
        INCLUDE STRUCTURE pc207 .
data : END OF rt1.

DATA : begin of rt2 occurs 0,
         ZWAGE_GRP like Z6HRA_WAGE_GRP-ZWAGE_GRP,
        ABART  like pc207-abart,
        LGART  like pc207-lgart,
*        APZNR  like pc207-apznr,
*        CNTR1  like pc207-cntr1,
*        CNTR2  like pc207-cntr2,
*        CNTR3  like pc207-cntr3,
        ALZNR  like pc207-alznr,
        C1ZNR  like pc207-c1znr,
        BTZNR  like pc207-btznr,
        ABZNR  like pc207-abznr,
        V0TYP  like pc207-v0typ,
        V0ZNR  like pc207-v0znr,
        ZEINH  like pc207-zeinh,
        BETPE  like pc207-betpe,
        ANZHL  like pc207-anzhl,
        BETRG  like pc207-betrg,
        RTE_CURR like pc207-RTE_CURR,
        AMT_CURR like pc207-AMT_CURR,
        ZZINDICATOR type Z6HRA_WAGE_GRP-ZZINDICATOR,
       end of rt2.

DATA: payresult TYPE pay99_result.
DATA : employee LIKE pa0021-betrg,
       employer LIKE pa0021-betrg,
       total    LIKE pa0021-betrg,
       tot_wage LIKE pa0021-betrg,
       amt(21),
       amt_wrd LIKE spell-word,
       v_tabix TYPE i,
       count    TYPE i.
DATA: DATE LIKE SY-DATUM.

DATA : BEGIN OF IT_LGART OCCURS 0,
       LGART LIKE P0015-LGART,
       ZWAGE_GRP like Z6HRA_WAGE_GRP-ZWAGE_GRP,
       SEQ       TYPE Z6HRA_WAGE_GRP-SEQ,
       END OF IT_LGART.

data : begin of wa_Z6HRA_WAGE_GRP,
       LGART       type Z6HRA_WAGE_GRP-LGART,
       ZWAGE_GRP   type Z6HRA_WAGE_GRP-ZWAGE_GRP,
       ZWAGE_TXT   type Z6HRA_WAGE_GRP-ZWAGE_TXT,
       SEQ         type Z6HRA_WAGE_GRP-SEQ,
       ZZINDICATOR type Z6HRA_WAGE_GRP-ZZINDICATOR,
       end of wa_Z6HRA_WAGE_GRP.

data : it_Z6HRA_WAGE_GRP like standard table of wa_Z6HRA_WAGE_GRP.
*&---------------------------------------------------------------------*
*&     AT SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var .
  p_vari = p_var.
  PERFORM f4_for_variant.
  p_var = p_vari.
AT SELECTION-SCREEN.
    i_repid = sy-repid.
  p_vari = p_var.
  PERFORM pai_of_selection_screen.

*&---------------------------------------------------------------------*
*&     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
  prg = sy-repid .

*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM display_list .
*  PERFORM F_GET_WAGE_TYPES.
  perform wage_grp_get.
  PERFORM f_fill_fieldcat_struct_dyn.
  PERFORM F_field_cat_for_dyn_table.
  PERFORM F_CREATE_DYNAMIC_TABLE.
  PERFORM get_data.
  PERFORM DISPLAY_DATA.

*&---------------------------------------------------------------------*
*&                      END-OF-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  concatenate p_PABRJ p_PABRP  into p_fpper.

  FIELD-SYMBOLS : <f1>,<g1>,<h1>,<g2>,<h2>,<g3>,
                 <GS1>,<HS1>,<GS2>,<HS2>,<GS3>,<HS3>.
  data : v_text(30),fl_flag,V_TEXT1(30).
  data : begin of stru,
         pernr like pa0015-pernr,
         ename like pa0001-ename,
         paydt like sy-datum,
         bet01 like pa0008-bet01,
         bet02 like pa0008-bet01,
         end of stru.
  data : v_total like pa0008-bet01,
         esic    type p decimals 2,
         esic1   like pa0008-bet01,
         v_dedc  like pa0008-bet01,
         V_ETOT like pa0008-bet01,
         V_DTOT like pa0008-bet01,
         V_NTOT like pa0008-bet01..

  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab1
                     WHERE pernr IN s_pernr
*{   REPLACE        PREK900435                                        1
*\                     AND   paydt EQ s_paydt.
                     and ABKRS in s_ABKRS
*                     AND   paydt EQ s_paydt
                     and FPPER eq p_FPPER
                     and INPER eq p_fpper
                     and   occat eq space.
*}   REPLACE



  SORT ITAB1 BY PERNR PAYDT SEQNR.
  LOOP AT itab1.
    v_tabix  = sy-tabix.
    esic = '1.75'.
    clear : v_total,v_dedc,  esic1,V_ETOT,V_DTOT,V_NTOT.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
*             clusterid               = t500l_header-relid
                 employeenumber          =  itab1-pernr
                 sequencenumber          =  itab1-seqnr
                 read_only_international =  'X'
            CHANGING
                 payroll_result          = payresult
            EXCEPTIONS
                 error_generating_import = 2
                 import_mismatch_error   = 3
                 subpool_dir_full        = 4
                 no_read_authority       = 5
                 no_record_found         = 6
                 versions_do_not_match   = 7
                 OTHERS                  = 8.
    IF sy-subrc <> 0.
      DELETE itab1 .
      CONTINUE.
    ELSE.
*   count = count + 1.
      clear : rt2, rt2[].
      SORT RT1 BY LGART.
      LOOP AT payresult-inter-rt INTO rt1.
*        IF RT1-LGART+0(1) EQ '/'.
*          MOVE RT1-LGART+1(3) TO RT1-LGART.
*        ENDIF.

        move-corresponding rt1 to rt2.
        collect rt2.
        clear : rt2.
      endloop.

*      ----add wage grp
      loop at rt2 .
        clear wa_Z6HRA_WAGE_GRP.
        read table it_Z6HRA_WAGE_GRP into wa_Z6HRA_WAGE_GRP with key lgart = rt2-lgart
                                                               binary search.
        if sy-subrc = 0.
          rt2-ZWAGE_GRP = wa_Z6HRA_WAGE_GRP-ZWAGE_GRP.
          rt2-ZZINDICATOR = wa_Z6HRA_WAGE_GRP-ZZINDICATOR.
        endif.

        modify rt2 transporting ZWAGE_GRP ZZINDICATOR.
      endloop.

*      -----

      sort rt2 by  ZWAGE_GRP   .
      loop at  rt2.
        at new ZWAGE_GRP.
          clear v_total.
        endat.

        v_total = v_total + rt2-betrg.
        at end of ZWAGE_GRP.
          fl_flag = 1.

*          clear: v_etot,v_dtot,v_ntot.
        endAT.
        if fl_flag = 1.
          clear fl_flag.

          LOOP AT it_Z6HRA_WAGE_GRP into wa_Z6HRA_WAGE_GRP  .
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE wa_Z6HRA_WAGE_GRP
                                                   TO <f1>.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.

              CASE SY-INDEX.
                WHEN '2'.
                  IF RT2-ZWAGE_GRP+0(1) NE '/'.
*                 IF RT2-LGART  EQ <F1> and rt2-lgart ne '6710'.
*                  IF RT2-ZWAGE_GRP  EQ <F1> and rt2-lgart ne '6710'.
                    if rt2-ZWAGE_GRP eq <F1>.
                      ASSIGN COMPONENT rt2-ZWAGE_GRP OF STRUCTURE <STRUC>
                                              TO <G1>.
*                    ASSIGN COMPONENT RT2-ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
                      CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
*                    CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
                      assign (v_text) to <h1>.

                      <h1> = v_total.
*                      condense <h1> no-gaps.
*---for total earnings and deductions
                      if RT2-lgart = wa_Z6HRA_WAGE_GRP-lgart.
                        IF RT2-zzindicator = 'E'.
                          assign component 'TEARN' OF STRUCTURE <STRUC> TO <GS1>.
                          CONCATENATE '<struc>' '-' 'TEARN' INTO V_TEXT1.
                          assign (v_text1) to <hS1>.
                          V_ETOT = V_ETOT + v_total.        "rt2-betrg.
                          <HS1> = V_ETOT.
                          CLEAR V_TEXT1.
                        ELSEIF RT2-zzindicator = 'D'.
                          assign component 'TDEDUC' OF STRUCTURE <STRUC> TO <GS2>.
                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO V_TEXT1.
                          assign (v_text1) to <hS2>.
                          V_DTOT = V_DTOT + v_total.        "rt2-betrg.
                          <HS2> = V_DTOT.
                          CLEAR V_TEXT1.
                        ELSEIF RT2-zzindicator = 'N'.
                          assign component 'TNET' OF STRUCTURE <STRUC> TO <GS3>.
                          CONCATENATE '<struc>' '-' 'TNET' INTO V_TEXT1.
                          assign (v_text1) to <hS3>.
                          V_NTOT = V_NTOT + rt2-betrg.
                          <HS3> = V_NTOT.
                          CLEAR V_TEXT1.
                        ENDIF.
                      endif.

*                        IF RT2-zzindicator = 'D'.
*                          assign component 'TDEDUC' OF STRUCTURE <STRUC> TO <GS2>.
*                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO V_TEXT1.
*                          assign (v_text1) to <hS2>.
*                          V_DTOT = V_DTOT + rt2-betrg.
*                          <HS2> = V_DTOT.
*                          CLEAR V_TEXT1.
*                        endif.
*----for total earnings and deductions


*                  Endif.
                    ELSE.
                      IF RT2-ZWAGE_GRP+1(3)  EQ <F1>.
*                    ASSIGN COMPONENT RT2-LGART+1(3) OF STRUCTURE <STRUC>
*                                            TO <G1>.
                        ASSIGN COMPONENT rt2-ZWAGE_GRP OF STRUCTURE <STRUC>
                                              TO <G1>.
                        CONCATENATE '<struc>' '-' RT2-ZWAGE_GRP+1(3) INTO V_TEXT.
                        assign (v_text) to <h1>.
*                    <h1> = RT2-BETRG.
*                    if v_text ne '<struc>-560'.
                        v_DEDC = v_DEDC + rt2-betrg.
                      ENDIF.
*                    condense <h1> no-gaps.
*                  Endif.

                    ENDIF.
                  endif.
              ENDCASE.
            ENDDO.

          ENDLOOP.
        ENDIF.

      ENDLOOP.
*      at end of paydt.
      at end of pernr.
        FL_flag = 1.
      endat.
      if fl_flag = 1.
        clear fl_flag.
        move-corresponding itab1 to stru.
        stru-bet01 = v_total - v_dedc.
        if v_total lt '7500'.
          esic1 =   v_total  * esic / 100 .
        endif.
        stru-bet02 = esic1.
        select single ename from pa0001 client specified
                                 into stru-ename
                                 where mandt = sy-mandt
                                   and pernr = itab1-pernr
                                   and begda le sy-datum
                                   and endda ge sy-datum.


        move-corresponding stru to <struc>.
        append <struc> to <table>.
        clear <struc>.
      endif.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list .
  PERFORM  eventtab_build USING gt_events .
  PERFORM  build_layout USING layout .
  PERFORM build_comment USING gt_list_top_of_page[].

ENDFORM.                    " display_list

*&---------------------------------------------------------------------*
*&      Form  f4_for_variant
*&---------------------------------------------------------------------*
FORM f4_for_variant.
  g_save = 'A'.
  g_variant-report = prg.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    " f4_for_variant

*&---------------------------------------------------------------------*
*&      Form  pai_of_selection_screen
*&---------------------------------------------------------------------*
FORM pai_of_selection_screen.
   IF NOT P_VARI IS INITIAL.
    MOVE g_variant TO gx_variant.
    MOVE P_VARI TO GX_VARIANT-VARIANT.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE.
    PERFORM initialize_variant.
  ENDIF.
ENDFORM.                    " pai_of_selection_screen


*&---------------------------------------------------------------------*
*&      Form  initialize_variant
*&---------------------------------------------------------------------*
FORM initialize_variant.
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = prg.
  gx_variant = g_variant .
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

ENDFORM.                    " initialize_variant

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout USING p_layout TYPE slis_layout_alv.
  p_layout-f2code       = '&IC1'.
  p_layout-zebra        = 'X'.
*  p_layout-detail_popup = ' '.
ENDFORM.                    " build_layout

*&---------------------------------------------------------------------*
*&      Form  disp_alv
*&---------------------------------------------------------------------*
*FORM disp_alv.
**  layout-coltab_fieldname = 'COLOR'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program = prg
*      it_fieldcat        = fieldcat
*      i_save             = g_save
*      is_variant         = g_variant
*      is_layout          = layout
*      it_events          = gt_events[]
*    TABLES
*      t_outtab           = disptab.
**ENDFORM.                    " disp_alv

FORM display_output .
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'.


  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table1> IS INITIAL.
    LOOP AT lt_alv_cat1 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE1>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.



    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE1>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table1>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat2[] IS INITIAL.
    LOOP AT lt_alv_cat2 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE2>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.


    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE2>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table2>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat3[] IS INITIAL.
    LOOP AT lt_alv_cat3 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE3>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE3>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table3>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
    EXPORTING
      i_interface_check             = ' '
*     IS_PRINT                      =
*     I_SCREEN_START_COLUMN         = 0
*     I_SCREEN_START_LINE           = 0
*     I_SCREEN_END_COLUMN           = 0
*     I_SCREEN_END_LINE             = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER       =
*     ES_EXIT_CAUSED_BY_USER        =
    EXCEPTIONS
      program_error                 = 0
      OTHERS                        = 0.


*      fieldcat-fieldname = 'MATNR'.
*      fieldcat-col_pos   = '37'.
*      fieldcat-ref_fieldname = 'MATNR'.
*      fieldcat-ref_tabname = 'MARA'.
*      fieldcat-tabname = 'I_HEADER'.
*      fieldcat-seltext_m   = 'Material'.
**      it_fieldcat-outputlen = 22 .
*      APPEND fieldcat TO alv_fieldcat .
*      CLEAR fieldcat .


*  CALL FUNCTION 'ZCFM_HIDE_INITIALFIELD_ALV'
*    EXPORTING
*      struc      = <struc>
*      fieldcat   = alv_fieldcat
*      hide       = 'X'
*    IMPORTING
*      fieldcat_e = alv_fieldcat
*    TABLES
*      it_tab     = <table>.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*     EXPORTING
*          i_callback_program = sy-repid
*          i_bypassing_buffer = 'X'
**            i_callback_pf_status_set = 'SET_PF_STAT_L'
*          i_callback_user_command  = 'USER_COMMAND'
*          i_callback_top_of_page  = 'TOP_OF_PAGE'
*          it_fieldcat        = alv_fieldcat
*          it_events          = gt_events
**          is_Layout          = layout
**            I_STRUCTURE_NAME   = <struc>
*          TABLES
*          t_outtab           = <table>.

*  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
*            EXPORTING
**         I_INTERFACE_CHECK        = ' '
*                 i_callback_program       = SY-REPID
**         I_CALLBACK_PF_STATUS_SET =
*                i_callback_user_command  = 'USER_COMMAND'
**                 is_layout                =  layout
*                 it_fieldcat             =  alv_fieldcat
**                 is_print                =  alv_print
**         IT_EXCLUDING             =
**         IT_SPECIAL_GROUPS        =
**         IT_SORT                  =
**         IT_FILTER                =
**         IS_SEL_HIDE              =
**         I_SCREEN_START_COLUMN    = 0
**        i_screen_start_line      = 70
**         I_SCREEN_END_COLUMN      = 0
**         I_SCREEN_END_LINE        = 0
**                i_default                = 'A'
**                i_save                   = g_save
**                is_variant               = g_variant
*                it_events                = GT_events
*
**         IT_EVENT_EXIT            =
*
*                 i_tabname_header         = 'I_HEADER'
*
*                 i_tabname_item           = '<TABLE>'
**         I_STRUCTURE_NAME_HEADER  =
**         I_STRUCTURE_NAME_ITEM    =
*                 is_keyinfo               = keyinfo
**         IS_PRINT                 =
**    IMPORTING
**         E_EXIT_CAUSED_BY_CALLER  =
**         ES_EXIT_CAUSED_BY_USER   =
*            TABLES
*                 t_outtab_header          = i_header
*                 t_outtab_item            = <table>
*            EXCEPTIONS
*                 program_error            = 1
*                 OTHERS                   = 2.



ENDFORM.                    " DIsplay_output

*
*---------------------------------------------------------------------*
*       FORM eventtab_build                                           *
*---------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  CONSTANTS:
  gc_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*  gc_formname_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_events.

  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.

*  READ TABLE lt_events WITH KEY name =  slis_ev_user_command
*                           INTO ls_event.
*  IF sy-subrc = 0.
*    MOVE gc_formname_user_command TO ls_event-form.
*    APPEND ls_event TO lt_events.
*  ENDIF.
ENDFORM.                    "eventtab_build

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM top_of_page .
  DATA : v_tcount TYPE i.
*  V_TCOUNT = V_TCOUNT + 1.
*  IF V_TCOUNT EQ 1.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
*  ENDIF.
ENDFORM.                    "top_of_page

*&--------------------------------------------------------------------*
*&      Form  BUILD_comment
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_HEADING  text
*---------------------------------------------------------------------*
FORM build_comment USING p_heading TYPE slis_t_listheader.
  DATA: line        TYPE  slis_listheader ,
        rs_variant  TYPE  disvariant      ,
        c_date(10)                        .

  CLEAR : gt_list_top_of_page[], gt_list_top_of_page .

  line-info = 'INDOFIL INDUSTRIES LTD' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  line-info = 'REPORT : Wage Register' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.
  IF NOT rs_variant IS INITIAL .
    CONCATENATE 'Layout : ' rs_variant-text INTO line-info
                                             SEPARATED BY space .
    line-typ  = 'S' .
    APPEND line TO gt_list_top_of_page .
    CLEAR line .
  ENDIF .

  line-typ = 'S' .
  WRITE sy-datum TO c_date .
  CONCATENATE 'Date : ' c_date INTO line-info SEPARATED BY space .
  APPEND line TO gt_list_top_of_page .
  CLEAR line .

ENDFORM .                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_GET_WAGE_TYPES .
*  PERFORM F_ADD_WAGE_TYPE USING '5000'.
*  PERFORM F_ADD_WAGE_TYPE USING '5001'.
*  PERFORM F_ADD_WAGE_TYPE USING '5020'.
*  PERFORM F_ADD_WAGE_TYPE USING '5021'.
*  PERFORM F_ADD_WAGE_TYPE USING '5022'.
*  PERFORM F_ADD_WAGE_TYPE USING '5030'.
*  PERFORM F_ADD_WAGE_TYPE USING '5040'.
*  PERFORM F_ADD_WAGE_TYPE USING '5050'.
*  PERFORM F_ADD_WAGE_TYPE USING '5060'.
*  PERFORM F_ADD_WAGE_TYPE USING '5070'.
*  PERFORM F_ADD_WAGE_TYPE USING '5080'.
*  PERFORM F_ADD_WAGE_TYPE USING '5090'.
*  PERFORM F_ADD_WAGE_TYPE USING '5210'.
*  PERFORM F_ADD_WAGE_TYPE USING '5220'.
*  PERFORM F_ADD_WAGE_TYPE USING '5230'.
*  PERFORM F_ADD_WAGE_TYPE USING '5240'.
*  PERFORM F_ADD_WAGE_TYPE USING '5510'.
*  PERFORM F_ADD_WAGE_TYPE USING '5520'.
*  PERFORM F_ADD_WAGE_TYPE USING '5530'.
*  PERFORM F_ADD_WAGE_TYPE USING '5540'.
*  PERFORM F_ADD_WAGE_TYPE USING '5920'.
*
*  PERFORM F_ADD_WAGE_TYPE USING '6610'.
*  PERFORM F_ADD_WAGE_TYPE USING '6710'.
*  PERFORM F_ADD_WAGE_TYPE USING '6720'.
*  PERFORM F_ADD_WAGE_TYPE USING '6730'.
*  PERFORM F_ADD_WAGE_TYPE USING '6740'.
*  PERFORM F_ADD_WAGE_TYPE USING '6750'.
*  PERFORM F_ADD_WAGE_TYPE USING '7470'.
*  PERFORM F_ADD_WAGE_TYPE USING '7480'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3F1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3P1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3E1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3W1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/460'.
*  PERFORM F_ADD_WAGE_TYPE USING '/560'.

  PERFORM F_ADD_WAGE_TYPE USING '1001'.
  PERFORM F_ADD_WAGE_TYPE USING '1002'.
  PERFORM F_ADD_WAGE_TYPE USING '1003'.
  PERFORM F_ADD_WAGE_TYPE USING '1004'.
  PERFORM F_ADD_WAGE_TYPE USING '1005'.
  PERFORM F_ADD_WAGE_TYPE USING '1006'.
  PERFORM F_ADD_WAGE_TYPE USING '1007'.
  PERFORM F_ADD_WAGE_TYPE USING '1008'.
  PERFORM F_ADD_WAGE_TYPE USING '1010'.
  PERFORM F_ADD_WAGE_TYPE USING '1011'.
  PERFORM F_ADD_WAGE_TYPE USING '1012'.
  PERFORM F_ADD_WAGE_TYPE USING '1013'.
  PERFORM F_ADD_WAGE_TYPE USING '1014'.

  PERFORM F_ADD_WAGE_TYPE USING '1101'.
  PERFORM F_ADD_WAGE_TYPE USING '1102'.
  PERFORM F_ADD_WAGE_TYPE USING '1103'.
  PERFORM F_ADD_WAGE_TYPE USING '1104'.
  PERFORM F_ADD_WAGE_TYPE USING '1105'.
  PERFORM F_ADD_WAGE_TYPE USING '1106'.
  PERFORM F_ADD_WAGE_TYPE USING '1107'.
  PERFORM F_ADD_WAGE_TYPE USING '1108'.
  PERFORM F_ADD_WAGE_TYPE USING '1109'.
  PERFORM F_ADD_WAGE_TYPE USING '1110'.

  PERFORM F_ADD_WAGE_TYPE USING '1301'.
  PERFORM F_ADD_WAGE_TYPE USING '1302'.
  PERFORM F_ADD_WAGE_TYPE USING '1303'.
  PERFORM F_ADD_WAGE_TYPE USING '1304'.
  PERFORM F_ADD_WAGE_TYPE USING '1305'.
  PERFORM F_ADD_WAGE_TYPE USING '1306'.
  PERFORM F_ADD_WAGE_TYPE USING '1307'.

  PERFORM F_ADD_WAGE_TYPE USING '1312'.
  PERFORM F_ADD_WAGE_TYPE USING '1313'.
  PERFORM F_ADD_WAGE_TYPE USING '1314'.
  PERFORM F_ADD_WAGE_TYPE USING '1315'.
  PERFORM F_ADD_WAGE_TYPE USING '1316'.
  PERFORM F_ADD_WAGE_TYPE USING '1317'.

  PERFORM F_ADD_WAGE_TYPE USING '5000'.
  PERFORM F_ADD_WAGE_TYPE USING '5001'.
  PERFORM F_ADD_WAGE_TYPE USING '5002'.
  PERFORM F_ADD_WAGE_TYPE USING '5003'.
  PERFORM F_ADD_WAGE_TYPE USING '5004'.
  PERFORM F_ADD_WAGE_TYPE USING '5005'.
  PERFORM F_ADD_WAGE_TYPE USING '5006'.
  PERFORM F_ADD_WAGE_TYPE USING '5007'.
  PERFORM F_ADD_WAGE_TYPE USING '5008'.
  PERFORM F_ADD_WAGE_TYPE USING '5009'.
  PERFORM F_ADD_WAGE_TYPE USING '5010'.
  PERFORM F_ADD_WAGE_TYPE USING '5011'.
  PERFORM F_ADD_WAGE_TYPE USING '5012'.
  PERFORM F_ADD_WAGE_TYPE USING '5013'.

  PERFORM F_ADD_WAGE_TYPE USING '5100'.
  PERFORM F_ADD_WAGE_TYPE USING '5101'.
  PERFORM F_ADD_WAGE_TYPE USING '5102'.
  PERFORM F_ADD_WAGE_TYPE USING '5103'.
  PERFORM F_ADD_WAGE_TYPE USING '5104'.
  PERFORM F_ADD_WAGE_TYPE USING '5105'.
  PERFORM F_ADD_WAGE_TYPE USING '5106'.
  PERFORM F_ADD_WAGE_TYPE USING '5107'.
  PERFORM F_ADD_WAGE_TYPE USING '5108'.
  PERFORM F_ADD_WAGE_TYPE USING '5109'.
  PERFORM F_ADD_WAGE_TYPE USING '5110'.
  PERFORM F_ADD_WAGE_TYPE USING '5111'.

  PERFORM F_ADD_WAGE_TYPE USING '5201'.
  PERFORM F_ADD_WAGE_TYPE USING '5202'.

  PERFORM F_ADD_WAGE_TYPE USING '/ZP1'.

  PERFORM F_ADD_WAGE_TYPE USING '2001'.
  PERFORM F_ADD_WAGE_TYPE USING '2002'.
  PERFORM F_ADD_WAGE_TYPE USING '2003'.
  PERFORM F_ADD_WAGE_TYPE USING '2004'.
  PERFORM F_ADD_WAGE_TYPE USING '2005'.
  PERFORM F_ADD_WAGE_TYPE USING '2006'.
  PERFORM F_ADD_WAGE_TYPE USING '2007'.
  PERFORM F_ADD_WAGE_TYPE USING '2008'.
  PERFORM F_ADD_WAGE_TYPE USING '2009'.
  PERFORM F_ADD_WAGE_TYPE USING '2010'.
  PERFORM F_ADD_WAGE_TYPE USING '2011'.

  PERFORM F_ADD_WAGE_TYPE USING '3001'.
  PERFORM F_ADD_WAGE_TYPE USING '3002'.
  PERFORM F_ADD_WAGE_TYPE USING '3021'.
  PERFORM F_ADD_WAGE_TYPE USING '3004'.
  PERFORM F_ADD_WAGE_TYPE USING '3005'.
  PERFORM F_ADD_WAGE_TYPE USING '3006'.
  PERFORM F_ADD_WAGE_TYPE USING '3010'.
  PERFORM F_ADD_WAGE_TYPE USING '3011'.
  PERFORM F_ADD_WAGE_TYPE USING '3015'.
  PERFORM F_ADD_WAGE_TYPE USING '3016'.
  PERFORM F_ADD_WAGE_TYPE USING '3024'.

  PERFORM F_ADD_WAGE_TYPE USING '/3E1'.
  PERFORM F_ADD_WAGE_TYPE USING '/3F1'.
  PERFORM F_ADD_WAGE_TYPE USING '/3F2'.
  PERFORM F_ADD_WAGE_TYPE USING '/3P1'.
  PERFORM F_ADD_WAGE_TYPE USING '/3W1'.
  PERFORM F_ADD_WAGE_TYPE USING '/460'.
  PERFORM F_ADD_WAGE_TYPE USING '/462'.

  PERFORM F_ADD_WAGE_TYPE USING '/ROR'.



endform.                    " F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*&      Form  F_ADD_WAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0923   text
*----------------------------------------------------------------------*
form F_ADD_WAGE_TYPE  using    value(p_0923).
  MOVE : P_0923 TO IT_LGART-LGART.
  APPEND IT_LGART.
  CLEAR  IT_LGART.
endform.                    " F_ADD_WAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_fill_fieldcat_struct_dyn .

  it_field_dynamic-fieldname = 'PERNR'.
  it_field_dynamic-desc      = 'Employee'.
  it_field_dynamic-reftab   = 'PA0015'.

  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'ENAME'.
  it_field_dynamic-desc      = 'Emp Name'.
  it_field_dynamic-reftab   = 'PA0001'.
  COLLECT it_field_dynamic.

*  it_field_dynamic-fieldname = 'PAYDT'.
*  it_field_dynamic-desc      = 'Pay Date'.
*  it_field_dynamic-reftab   = 'HRPY_RGDIR'.
*  COLLECT it_field_dynamic.
  clear   it_field_dynamic.
  clear it_lgart.
  LOOP AT it_lgart.
*    SELECT SINGLE * FROM t512t CLIENT SPECIFIED
*                        WHERE mandt = sy-mandt
*                          and sprsl = sy-langu
*                          and molga = '40'
*                          AND lgart = it_lgart-lgart.


*    if it_lgart-LGART+0(1) eq '/'.
*      IT_LGART-LGART = IT_LGART-LGART+1(3).
*      MODIFY IT_LGART TRANSPORTING LGART.
*
*    ENDIF.

    clear wa_Z6HRA_WAGE_GRP .
    read table it_Z6HRA_WAGE_GRP into wa_Z6HRA_WAGE_GRP
     with key lgart = it_lgart-lgart
     binary search.
    if sy-subrc = 0.
      it_field_dynamic-fieldname = wa_Z6HRA_WAGE_GRP-ZWAGE_GRP.
      it_field_dynamic-desc      = wa_Z6HRA_WAGE_GRP-ZWAGE_TXT.
      it_field_dynamic-reftab   = 'PA0008'.
      it_field_dynamic-reffield = 'BET01'.
      COLLECT it_field_dynamic.
      clear   it_field_dynamic.
    endif.

*    it_field_dynamic-fieldname = it_lgart-LGART.
*    it_field_dynamic-desc      = t512t-lgtxt.
*    it_field_dynamic-reftab   = 'T512T'.
*    COLLECT it_field_dynamic.

  ENDLOOP.

  it_field_dynamic-fieldname = 'TEARN'.
  it_field_dynamic-desc      = 'Total Earnings'.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'BET01'.
  COLLECT it_field_dynamic.
*
  it_field_dynamic-fieldname = 'TDEDUC'.
  it_field_dynamic-desc      = 'Total Deductions'.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'BET01'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'TNET'.
  it_field_dynamic-desc      = 'NET'.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'BET01'.
  COLLECT it_field_dynamic.


endform.                    " f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*&      Form  F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_field_cat_for_dyn_table .
  LOOP AT it_field_dynamic.
    IF IT_FIELD_DYNAMIC-FIELDNAME EQ 'PERNR'
           OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ENAME'
           OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'PAYDT'
           OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'BET01'
           OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'BET02'.

      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = it_field_dynamic-fieldname.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    else.
      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = 'BET01'.
*      it_fieldcat-ref_field = 'ZWAGE_TXT'.
      it_fieldcat-DO_SUM = 'X'.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    ENDIF.
  ENDLOOP.

endform.                    " F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_CREATE_DYNAMIC_TABLE .

  IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic structure -> i_struct
*    CALL METHOD zcl_alv_struct_create=>create_dynammic_stru
*      EXPORTING
*        it_fieldcatalog = lt_alv_cat
*      IMPORTING
*        ep_table        = i_struct.
*    ASSIGN i_struct->* TO <struc> .       " Header Structure for <table>


* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table> .
* Create dynamic work area and assign to FS

    create data i_struct like line of <table>.
    assign i_struct->* to <struc>.


  endif.

endform.                    " F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DISPLAY_DATA .

  DATA : it_sort type slis_sortinfo_alv occurs 1 with header line.

  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table> IS INITIAL.
    LOOP AT lt_alv_cat INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.

*    ----hide column
    DATA:
  ld_column      TYPE lvc_fname,
  ld_hide          TYPE C ,
  w_lines type i,
  w_no type i,
  wA_no(4) type c.
    FIELD-SYMBOLS:
      <ls_entry>     TYPE any,
      <ld_fld>         TYPE any.

    describe table alv_fieldcat lines w_lines.

*    w_no = w_lines - 2.
*    wA_no = w_no + 1.
*    do w_no times.
*      wA_no = wa_no - 1.
*      condense wA_no.
*      ld_column = wA_no ."ld_column + '01'.
*      if ld_column ge 1 and ld_column lt 10.
*        concatenate '0' ld_column into ld_column.
*      endif.
    loop at it_field_dynamic where REFFIELD = 'BET01'.
      CLEAR ld_column.
      ld_column = it_field_dynamic-FIELDNAME.
      condense ld_column.
      LOOP AT <table> ASSIGNING <ls_entry>.
        clear ld_hide.
        ASSIGN COMPONENT ld_column OF STRUCTURE <ls_entry> TO <ld_fld>.

*        IF ( <ld_fld>   > '0.00' ) .
        IF ( <ld_fld>   <> '0.00' ) .
          ld_hide = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      READ TABLE alv_fieldcat INTO fieldcat
                           WITH KEY fieldname = ld_column.
      IF ( syst-subrc = 0 ).
        if ld_hide = 'X'.
          fieldcat-no_out = ''.
        else.
          fieldcat-no_out = 'X'.
        endif.

        MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
      ENDIF.
    ENDLOOP.

       READ TABLE alv_fieldcat INTO fieldcat
                           WITH KEY fieldname = 'TNET'.
        IF SY-SUBRC = 0.
        fieldcat-no_out = 'X'.
        ENDIF.
        MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
*    ----

    it_sort-spos = 1.
    it_sort-fieldname = 'PERNR'.
    it_sort-up = 'X'.
    it_sort-subtot = ''.
    APPEND it_sorT.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = i_repid
            i_bypassing_buffer = 'X'
*            i_callback_pf_status_set = 'SET_PF_STAT_L'
*            i_callback_user_command  = 'USER_COMMAND'
            i_callback_top_of_page  = 'TOP_OF_PAGE'
            it_fieldcat        = alv_fieldcat
            IT_SORT                = it_sort[]
            I_DEFAULT                         = 'X'
             I_SAVE                            = 'A'
             IS_VARIANT = G_VARIANT
            it_events          = gt_events
*          is_Layout          = layout
*            I_STRUCTURE_NAME   = <struc>
            TABLES
            t_outtab           = <table>.
  ENDIF.
endform.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  WAGE_GRP_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WAGE_GRP_GET .
  data : lv_tabix type sy-tabix.
  select LGART
         ZWAGE_GRP
         ZWAGE_TXT
         SEQ
         ZZINDICATOR from Z6HRA_WAGE_GRP into table it_Z6HRA_WAGE_GRP
         for all entries in it_lgart
         where lgart eq it_lgart-lgart.

  if sy-subrc = 0.
    sort it_Z6HRA_WAGE_GRP by lgart ZWAGE_GRP.
  endif.

*  loop at it_lgart.
*    lv_tabix = sy-tabix.
*    clear wa_Z6HRA_WAGE_GRP.
*    read table it_Z6HRA_WAGE_GRP into wa_Z6HRA_WAGE_GRP
*    with key lgart = it_lgart-lgart
*    binary search.
*    if sy-subrc = 0.
*      it_lgart-ZWAGE_GRP = wa_Z6HRA_WAGE_GRP-ZWAGE_GRP.
*      IT_lgart-SEQ       = wa_Z6HRA_WAGE_GRP-SEQ.
*    endif.
*
*    modify it_lgart  index lv_tabix transporting ZWAGE_GRP SEQ.
*  endloop.

  LOOP AT it_Z6HRA_WAGE_GRP  INTO WA_Z6HRA_WAGE_GRP .
    it_lgart-LGART = wa_Z6HRA_WAGE_GRP-LGART.
    it_lgart-ZWAGE_GRP = wa_Z6HRA_WAGE_GRP-ZWAGE_GRP.
    IT_lgart-SEQ       = wa_Z6HRA_WAGE_GRP-SEQ.
    APPEND IT_LGART.
  ENDLOOP.
**---sort with seq
  CLEAR IT_LGART.
  sort it_lgart by seq ascending.
ENDFORM.                    " WAGE_GRP_GET
