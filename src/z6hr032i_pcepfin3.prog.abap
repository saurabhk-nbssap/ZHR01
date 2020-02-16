*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN3                                                   *
*----------------------------------------------------------------------*

*  GET PERNR
*  READ RGDIR WITH SEQNO.

 START-OF-SELECTION.
*   PERFORM check_auth_obj .
   CLEAR afy_switch.
   CALL METHOD cl_hrpayin_switch_check_5=>hrlocin_sfws_sc_05
     RECEIVING
       rv_active = htext_switch.
   IF htext_switch = 'X'.
*   Check if Alternate Financial Year solution is implemented
     PERFORM is_afy_implemented IN PROGRAM pcutsin0 IF FOUND USING pn-begda
                                                                   pn-endda
                                                          CHANGING afy_switch.

*    PERFORM is_afy_implemented CHANGING afy_switch.
     IF afy_switch = 'X'.
       PERFORM find_paydate USING pnpxabkr
                                  pn
                         CHANGING paydate.
     ENDIF.                             " IF afy_switch = 1.
   ENDIF.                               " IF htext_switch = 'X'.

*  INITIALIZATION
   main_tab-member_last_month = 0.
   main_tab-new_this_month = 0.
   main_tab-left_service = 0.

* THE PREVIOUS MONTH BEGDA AND ENDDA ARE NEEDED TO PASS TO THE MACRO TO
* COMPUTE THE NUMBER OF EMPLOYEES LAST MONTH.
   last_month_endda = pn-endda.
   last_month_endda = pn-begda - 1.
   last_month_begda = last_month_endda.
   last_month_begda+6(2) = '01'.
   next_month_begda = pn-endda + 1.

*  Get the address no. from T7INF1 for the selected Trust Id
   PERFORM get_addrno USING trust_id.

*  Read accuracy factor from T511K
   PERFORM get_acc_factor USING 'GENAU' sy-datum
                          CHANGING acc_factor.

* AB13042017 INSERT START...
  CLEAR pnpabkrs.
  pnpabkrs-sign = 'I'.
  pnpabkrs-option = 'EQ'.
  pnpabkrs-low = pnpxabkr.
  APPEND pnpabkrs.
* AB13042017 INSERT END...

*  For Basic Pay
*   PERFORM WAGE_GRP_GET USING PN-ENDDA
*                      CHANGING v_fpper
*                               v_flper.

 GET pernr.

*  Clearing the internal tables.
   CLEAR form5_tab.
   CLEAR form10_tab.
   CLEAR main_tab.
   CLEAR form5_tab_entry.
   CLEAR form10_tab_entry.


*  Import the cluster results
   PERFORM import_results.

* Check if there is a record in EPF table for this month
   PERFORM check_this_month.

*  Checking whether the employee is new subscriber or not
   IF reponame = 'FORM5' OR reponame = 'FORM12A'.
     PERFORM check_new_subs.
   ENDIF.

* Checking if the employee has left the trust in this month.
   IF reponame = 'FORM10' OR reponame = 'FORM12A'.
     PERFORM check_leavers.
   ENDIF.

* Read family data for Form5 and Form10
   IF reponame = 'FORM5' OR reponame = 'FORM10'.
     PERFORM get_family_info.
   ENDIF.

* FORMAT THE NAME OF THE EMPLOYEE.
   PERFORM fill_emplye_det  USING nbegda nendda .

* Convert employee name to upper case.
   PERFORM convert_to_upper USING e_name
                            CHANGING emp_name.

* DECIDES WHICH FORM TO RUN.
   CASE reponame.

     WHEN 'FORM5'.
       IF NOT form5_tab_entry IS INITIAL.
         MOVE pernr-pernr TO form5_tab-pernr.
         MOVE emp_name TO form5_tab-ename.
         MOVE t7inf1-tstad TO form5_tab-tstad.
         APPEND form5_tab.
       ENDIF.

     WHEN 'FORM10'.
       IF NOT form10_tab_entry IS INITIAL.
         PERFORM reason_to_leave.
         MOVE pernr-pernr TO form10_tab-pernr.
         MOVE emp_name TO form10_tab-ename.
         MOVE t7inf1-tstad TO form10_tab-tstad.
         APPEND form10_tab.
       ENDIF.

     WHEN 'FORM12A'.
       MOVE pernr-pernr TO main_tab-pernr.

*  NO. OF MEMBERS LAST MONTH
       PERFORM check_last_month.

*  Fill the internal table
       PERFORM fill_main_tab.
       APPEND main_tab.

     WHEN OTHERS.
       MESSAGE i117(hrpadin01).
       EXIT.
   ENDCASE.

 END-OF-SELECTION.

*  INFORMATION GIVEN IF THE DATA ENTERED IN THE SELECTION SCREEN
*  DOES NOT YIELD ANY RECORDS.
   CASE reponame.

     WHEN 'FORM5'.
       PERFORM check_int_table TABLES form5_tab.
       sname = '40EPF005'.
     WHEN 'FORM10'.
       PERFORM check_int_table TABLES form10_tab.
       sname = '40EPF010'.
     WHEN 'FORM12A'.
       PERFORM check_int_table TABLES main_tab.
       sname = '40EPF12A'.
       SORT main_tab ASCENDING.
       PERFORM fill_final_tab.

   ENDCASE.

   IF afy_switch = 'X'.
     date = paydate.
   ELSE.
     date = pn-begda.
   ENDIF.                              " IF afy_switch = 1.

   CALL FUNCTION 'HR_IN_GET_DATE_COMPONENTS'
        EXPORTING
             idate                         = date
       IMPORTING
             day                           = pf_day
             month                         = pf_mon
             year                          = pf_year
             stext                         = pf_month
*         LTEXT                         =
*         USERDATE                      =
       EXCEPTIONS
            input_date_is_initial         = 1
            text_for_month_not_maintained = 2
            OTHERS                        = 3
             .

   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.


   CONCATENATE pf_month pf_year INTO pf_month_year SEPARATED BY sep.

   CALL FUNCTION 'HR_IN_GET_FISCAL_PERIOD'
     EXPORTING
       parea1                = pnpxabkr              "PNPABKRS-LOW
       pyear1                = pn-pabrj
     IMPORTING
       pbegda                = fybegda
       pendda                = fyendda
     EXCEPTIONS
       no_entry_found_t549a  = 1
       no_entry_found_t549q  = 2
       no_begin_period_found = 3
       no_end_period_found   = 4
       no_cntrl_record_found = 5
       OTHERS                = 6.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text   = 'Generating List, please wait.....'(039)
     EXCEPTIONS
       OTHERS = 1.

   MESSAGE s192(hrpadin01).
   PERFORM display_results.

* Begin of AFY change
*&---------------------------------------------------------------------*
*&      Form  IS_AFY_IMPLEMENTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_AFY_SWITCH  text
*----------------------------------------------------------------------*
 FORM is_afy_implemented CHANGING pv_afy_switch TYPE abrwt.
   DATA:
     lv_inafy TYPE abrwt.
   SELECT
   SINGLE kwert
     FROM t511k
     INTO lv_inafy
    WHERE molga = '40'
      AND konst = 'INAFY'
      AND begda <= pn-endda
      AND endda >= pn-begda.
   IF sy-subrc = 0.
     pv_afy_switch = lv_inafy.
   ENDIF.                               " IF sy-subrc = 0.
 ENDFORM.                               " FORM IS_AFY_IMPLEMENTED
* End of AFY change
*&---------------------------------------------------------------------*
*&      Form  FIND_PAYDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PNPXABKR  text
*      -->P_PN  text
*      <--P_PAYDATE  text
*----------------------------------------------------------------------*
 FORM find_paydate USING p_pnpxabkr LIKE pnpxabkr
                         p_pn       LIKE pn
                CHANGING p_paydate TYPE sy-datum.
   DATA:
     l_t549a TYPE t549a,
     l_t549s TYPE t549s.
   SELECT SINGLE * FROM t549a INTO l_t549a WHERE abkrs = pnpxabkr.
   IF sy-subrc = 0.
     SELECT SINGLE * FROM t549s INTO l_t549s
             WHERE molga = '40'
               AND datmo = l_t549a-datmo
               AND permo = p_pn-permo
               AND pabrj = p_pn-pabrj
               AND pabrp = p_pn-pabrp
               AND datid = '01'.
     IF sy-subrc = 0.
       p_paydate = l_t549s-pdate.
     ENDIF.                             " IF sy-subrc = 0.
   ENDIF.                               " IF sy-subrc = 0.
 ENDFORM.                               " FIND_PAYDATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 09.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* FORM check_auth_obj .
*   TYPES: BEGIN OF ty_t500p,
*           persa TYPE t500p-persa,    " Personal Area
*         END OF ty_t500p,
*         BEGIN OF ty_t503t,
*           persk TYPE t503t-persk,    " Employee SubGroup
*         END OF ty_t503t,
*         BEGIN OF ty_t549t,
*          abkrs TYPE t549t-abkrs,     " Payroll Area
*        END OF ty_t549t
*           .
*   DATA: t_t500p TYPE TABLE OF ty_t500p,  " Personal Area
*         w_t500p TYPE ty_t500p,
*         t_t503t TYPE TABLE OF ty_t503t,  " Employee SubGroup
*         w_t503t TYPE ty_t503t,
*         t_t549t TYPE TABLE OF ty_t549t, " Payroll Area
*         w_t549t TYPE ty_t549t.
*
*   FREE : t_t500p[], t_t503t[], t_t549t[].
*   CLEAR: w_t500p, w_t503t, w_t549t.
*
*   break test1.
**PNPPERSK, PERSK
** PNPXABKR, PNPPERSK, PNPWERKS
*   AUTHORITY-CHECK OBJECT 'P_PCR' " Payroll Area
*                  ID 'ACTVT' FIELD '03'
*                  ID 'ABRKS' FIELD pnpxabkr.
*   IF sy-subrc NE 0.
*     SET CURSOR FIELD pnpxabkr.
*     MESSAGE 'You have no authorization for this transaction in Payroll Area' TYPE 'I' DISPLAY LIKE 'E'.
*     LEAVE LIST-PROCESSING.
*   ENDIF.
*
****** Start Code: Added by CS on 14.10.2015 for Personal Area Authorization. *****
*   SELECT persa  " Fetch values of Personal Area
*     FROM t500p
*     INTO TABLE t_t500p
*     WHERE persa IN pnpwerks
*      .
*
*   CLEAR: pnpwerks, lv_persa_auth_flg.
*   REFRESH: pnpwerks[].
*   IF t_t500p[] IS NOT INITIAL.
*     LOOP AT t_t500p INTO w_t500p.
*       AUTHORITY-CHECK OBJECT 'P_ORGIN' " Personal Area
*                      ID 'ACTVT' FIELD '03'
*                      ID 'PERSA' FIELD w_t500p-persa.
*       IF sy-subrc EQ 0.
*         pnpwerks-sign = 'I'.
*         pnpwerks-option = 'EQ'.
*         pnpwerks-low = w_t500p-persa.
*         APPEND pnpwerks.
*         CLEAR: pnpwerks.
*       ELSE.
*         IF lv_persa_auth_flg IS INITIAL.  " Authorization Flag
*           lv_persa_auth_flg = 'X'.
*         ENDIF.
*       ENDIF.
*       CLEAR: w_t500p.
*     ENDLOOP.
*   ENDIF.
*   IF pnpwerks[] IS INITIAL.
*     pnpwerks-sign = 'I'.
*     pnpwerks-option = 'EQ'.
*     pnpwerks-low = ''.
*     APPEND pnpwerks.
*     CLEAR: pnpwerks.
*   ENDIF.
****** End Code: Added by CS on 14.10.2015 for Personal Group Authorization. *****
*
****** Start Code: Added by CS on 14.10.2015 for Employee Subgroup Authorization. *****
*   SELECT persk  " Fetch values of Employee Subgroup
*     FROM t503t
*     INTO TABLE t_t503t
*     WHERE persk IN pnppersk
*      AND sprsl EQ sy-langu.
*   .
*
*   CLEAR: pnppersk, lv_persk_auth_flg.
*   REFRESH: pnppersk[].
*   IF t_t503t[] IS NOT INITIAL.
*     LOOP AT t_t503t INTO w_t503t.
*       AUTHORITY-CHECK OBJECT 'P_ORGIN' " Employee Subgroup
*                      ID 'ACTVT' FIELD '03'
*                      ID 'PERSK' FIELD w_t503t-persk.
*       IF sy-subrc EQ 0.
*         pnppersk-sign = 'I'.
*         pnppersk-option = 'EQ'.
*         pnppersk-low = w_t503t-persk.
*         APPEND pnppersk.
*         CLEAR: pnppersk.
*       ELSE.
*         IF lv_persk_auth_flg IS INITIAL.  " Authorization Flag
*           lv_persk_auth_flg = 'X'.
*         ENDIF.
*       ENDIF.
*       CLEAR: w_t503t.
*     ENDLOOP.
*   ENDIF.
*   IF pnppersk[] IS INITIAL.
*     pnppersk-sign = 'I'.
*     pnppersk-option = 'EQ'.
*     pnppersk-low = ''.
*     APPEND pnppersk.
*     CLEAR: pnppersk.
*   ENDIF.
****** End Code: Added by CS on 14.10.2015 for Employee SubGroup Authorization. *****
*
* ENDFORM.                    " CHECK_AUTH_OBJ
