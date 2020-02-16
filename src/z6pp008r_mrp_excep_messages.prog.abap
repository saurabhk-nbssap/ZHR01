*&---------------------------------------------------------------------*
*& Report  Z6PP008R_MRP_EXCEP_MESSAGES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6PP008R_MRP_EXCEP_MESSAGES.
*&--------------------------------------------------------------------*
*& Report  ZW_PP_13_MRP.
*&
*&--------------------------------------------------------------------*

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: MRP Exception Messages
* OBJECT TYPE       : Report                FUNC. CONSULTANT  : Burzis
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 09.08.2010
*        DEV REQUEST: IRDK900896
*             TCODE : ZPP008
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
*                     Table Declaration                                *
*----------------------------------------------------------------------*
TABLES: marc,      "Plant Data for Material
        mdez,
        t458a.  "Exception Messages in Material Requirements Planning

*----------------------------------------------------------------------*
*                     Internal table                                   *
*----------------------------------------------------------------------*
DATA: BEGIN OF t_list OCCURS 0,
      matnr LIKE marc-matnr,  " Material
      werks LIKE marc-werks,  " Plant
      dispo LIKE marc-dispo,  " MRP Controller
      disgr LIKE marc-disgr,  " MRP Grp
      fevor LIKE marc-fevor,  " Production Scheduler
      ekgrp LIKE marc-ekgrp,  " Purchaseing Grp
END OF t_list.

DATA BEGIN OF t_mdps OCCURS 0.
        INCLUDE STRUCTURE mdps. "Item in MRP document
DATA END OF t_mdps.
DATA: BEGIN OF t_mdez OCCURS 0.
        INCLUDE STRUCTURE mdez.
DATA: END   OF t_mdez.

DATA: BEGIN OF t_mdsu OCCURS 0.
        INCLUDE STRUCTURE mdsu.
DATA: END   OF t_mdsu.

DATA: BEGIN OF t_data OCCURS 0,
        matnr LIKE marc-matnr,
        maktx LIKE makt-maktx,
        delb0 LIKE mdez-delb0,
        werks LIKE marc-werks,
*        EXTRA LIKE MDEZ-EXTRA,
*{   REPLACE        SBXK900030                                        1
*\        order_no LIKE /sapht/drmsr3xn-order_no,
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 03, 2018 18:01:00
* Changed By - ABAP01 - BhushanM
* Purpose    - Simplification list - 2220005 - S/4 HANA: Data Model Changes in Pricing and Condition Technic
* Solution   - Used alternate Data Declaration
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
        order_no(12) TYPE c,
*}   REPLACE
*        ebeln LIKE ekpo-ebeln,
*        ebeln type zw_d_ord,
        ebelp LIKE ekpo-ebelp,
        mng01 LIKE mdez-mng01,
        dat00 LIKE mdez-dat00,
        dat01 LIKE mdez-dat01,
        umdat LIKE mdez-umdat,
        dat02 LIKE mdez-dat02,
        auskt LIKE mdez-auskt,
        auslt LIKE t458b-auslt,
        name1 LIKE lfa1-name1,
      END   OF t_data.

TYPE-POOLS : slis.

DATA: xfieldcat    TYPE slis_t_fieldcat_alv.

DATA : wa_fieldcat TYPE slis_fieldcat_alv,
       layout      TYPE slis_layout_alv,
       g_repid     LIKE sy-repid.


******Selection-Screen to get the inputs from user****
*--------------------------------------------------------------------*
*                     Selection Screen                               *
*--------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS:
s_matnr FOR marc-matnr,
s_dispo FOR marc-dispo  OBLIGATORY, " MRP C
s_werks FOR marc-werks  OBLIGATORY NO-EXTENSION NO INTERVALS,
s_auskt for mdez-auskt MATCHCODE OBJECT ZSHAUSKT. " Exception Code
SELECTION-SCREEN: END OF BLOCK b1.
******           end of Selection-Screen                      ****
*---------------------------------------------------------------------*
*                     Processing Logic                                *
*---------------------------------------------------------------------*
TOP-OF-PAGE.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  PERFORM sub_get_data.

END-OF-SELECTION.

  PERFORM sub_write_data.


*&---------------------------------------------------------------------*
*&      Form  SUB_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_get_data.

  DATA: l_daynr  LIKE hrvsched-daynr,
        l_daytxt LIKE hrvsched-daytxt.

  REFRESH: t_list, t_mdps, t_mdez, t_data.

  SELECT matnr werks
    INTO TABLE t_list
    FROM marc
    WHERE matnr IN s_matnr
      AND werks IN s_werks
      AND dispo IN s_dispo.
  SORT t_list BY werks matnr.

*--------------------------------------------------------------------*
*                     Function Module for list the MRP Exception     *
*--------------------------------------------------------------------*
  LOOP AT t_list.


    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
        matnr                    = t_list-matnr
        werks                    = t_list-werks
      TABLES
        mdpsx                    = t_mdps
        mdezx                    = t_mdez
        mdsux                    = t_mdsu
      EXCEPTIONS
        material_plant_not_found = 1
        plant_not_found          = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    LOOP AT t_mdps.
*      IF ( t_mdps-delkz = 'POitem' OR
*           t_mdps-delkz = 'PurRqs' ).
*      ELSE.
*        DELETE t_mdps INDEX sy-tabix.
*      ENDIF.
*    ENDLOOP.

    LOOP AT t_mdez.
      IF t_mdez-auskt IS INITIAL.
        DELETE t_mdez INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
 if not s_auskt is initial.
   loop at t_mdez.
     if not t_mdez-auskt in s_auskt.
          DELETE t_mdez INDEX sy-tabix.
     endif.
   endloop.
 endif.

    LOOP AT t_mdez.
      t_data-matnr = t_list-matnr.
      t_data-werks = t_list-werks.
      t_data-delb0 = t_mdez-delb0.
*      T_DATA-EXTRA = T_MDEZ-EXTRA.
      t_data-mng01 = t_mdez-mng01.
      t_data-dat00 = t_mdez-dat00.
      t_data-dat02 = t_mdez-dat02.
      t_data-umdat = t_mdez-umdat.
      t_data-auskt = t_mdez-auskt.
      IF NOT t_data-umdat IS INITIAL.
        CLEAR: l_daynr, l_daytxt.
*        T_DATA-DAT01 = T_DATA-UMDAT - 5.
        CALL FUNCTION 'RH_GET_DATE_DAYNAME'
          EXPORTING
            langu  = sy-langu
            date   = t_data-umdat
          IMPORTING
            daynr  = l_daynr
            daytxt = l_daytxt.
        CASE l_daynr.
          WHEN 7.
*            T_DATA-DAT01 = T_DATA-UMDAT - 6.
          WHEN 6.
*            T_DATA-DAT01 = T_DATA-UMDAT - 5.
          WHEN OTHERS.
*            T_DATA-DAT01 = T_DATA-UMDAT - 7.
        ENDCASE.
      ENDIF.
      SELECT SINGLE maktx
      INTO   t_data-maktx
      FROM   makt
      WHERE  matnr = t_data-matnr.
      SELECT SINGLE name1
      INTO   t_data-name1
      FROM   lfa1
      WHERE  lifnr = t_mdez-lifnr.
      SPLIT t_mdez-extra AT '/'
       INTO t_data-order_no t_mdez-extra.
      t_data-ebelp = t_mdez-extra+0(5).
      SELECT SINGLE auslt
      INTO   t_data-auslt
      FROM   t458b
      WHERE  spras = sy-langu
      AND    aussl = t_mdez-aussl.
      APPEND t_data.
      CLEAR: t_data.
    ENDLOOP.


  ENDLOOP.
  SORT t_data BY matnr delb0.

ENDFORM.                    " SUB_GET_DATA


*---------------------------------------------------------------------*
*                     Output Processing                               *
*---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  SUB_WRITE_DATA
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*

FORM sub_write_data.
  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
      i_program_name               = g_repid
      i_internal_tabname           = 'T_DATA'
*   I_STRUCTURE_NAME             =
*   I_CLIENT_NEVER_DISPLAY       = 'X'
 i_inclname                   = g_repid
*      i_bypassing_buffer           = 'X'
      i_buffer_active              = ' '
    CHANGING
      ct_fieldcat                  = xfieldcat
   EXCEPTIONS
     inconsistent_interface       = 1
     program_error                = 2
     OTHERS                       = 3
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT  xfieldcat INTO wa_fieldcat .
    CASE wa_fieldcat-fieldname.
      WHEN 'NAME1'.
        wa_fieldcat-seltext_l = 'Vendor'.
        wa_fieldcat-seltext_m = 'Vendor'.
        wa_fieldcat-seltext_s = 'Vendor'.
        wa_fieldcat-ddictxt   = 'S'.
      WHEN 'MATNR'.
        wa_fieldcat-col_pos   = 1.
      WHEN 'DELB0'.
        wa_fieldcat-col_pos   = 2.
      WHEN 'MNG01'.
        wa_fieldcat-do_sum    = 'X'.
*      WHEN 'DAT01'.
*        WA_FIELDCAT-SELTEXT_L = 'New GRN Dt'.
*        WA_FIELDCAT-SELTEXT_M = 'New GRN Dt'.
*        WA_FIELDCAT-SELTEXT_S = 'New GRN Dt'.
*        WA_FIELDCAT-DDICTXT   = 'S'.
    ENDCASE.
    MODIFY xfieldcat FROM wa_fieldcat.
  ENDLOOP.
  layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      is_layout          = layout
      it_fieldcat        = xfieldcat[]
      i_save             = 'A'
    TABLES
      t_outtab           = t_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " SUB_WRITE_DATA


*Selection texts
*----------------------------------------------------------
* S_DISPO         MRP Controller
* S_MATNR         Material
* S_WERKS         Plant
