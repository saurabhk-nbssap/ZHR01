*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN8                                                   *
*XXXNTnote number  <date>   Note<note number>:<short description>
*RSKNT666437       30092003 Note666437: Form 5 dispalying employees
*                                       already qualified for PF.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Form  BUILD_ERROR
*&---------------------------------------------------------------------*
 FORM build_error TABLES   hr_error STRUCTURE hr_error
                  USING    $pernr
                           $msgid
                           $msgno
                           $msgv1
                           $msgv2
                           $msgv3
                           $msgv4.

   hr_error-pernr = $pernr.
   hr_error-arbgb = $msgid.
   hr_error-msgty = 'E'.
   hr_error-msgno = $msgno.
   hr_error-msgv1 = $msgv1.
   hr_error-msgv2 = $msgv2.
   hr_error-msgv3 = $msgv3.
   hr_error-msgv4 = $msgv4.
   APPEND hr_error.

 ENDFORM.                              " BUILD_ERROR

*&---------------------------------------------------------------------*
*&      Form  FILL_MAIN_TAB
*&---------------------------------------------------------------------*
 FORM fill_main_tab.

   LOOP AT pepf WHERE tstid = trust_id.
     pepf-erpfa = '3603'.
     pepf-eredw = '3601'.
     pepf-ereda = '3602'.

     LOOP AT prt WHERE cntr1 = pepf-cntr1.

       CASE prt-lgart.

*  READ THE PF EE CONTRIBUTION AMOUNT FROM RT
         WHEN pepf-eepfw.
           main_tab-pf_ee_contr = main_tab-pf_ee_contr + prt-betrg.

*  READ THE VPF EE CONTRIBUTION AMOUNT FROM RT
*         WHEN PEPF-EVPFW.
*           MAIN_TAB-PF_EE_CONTR = MAIN_TAB-PF_EE_CONTR + PRT-BETRG.

*  READ THE PF ER CONTRIBUTION AMOUNT FROM RT
         WHEN pepf-erpfw.
           main_tab-pf_er_contr = main_tab-pf_er_contr + prt-betrg.
*  READ THE PF ER CONTRIBUTION AMOUNT FROM RT
         WHEN pepf-evpfw.
           main_tab-pf_er_vpf = main_tab-pf_er_vpf + prt-betrg.

*  READ THE PF ADMIN CHARGES FROM RT
         WHEN pepf-erpfa.
           main_tab-pf_admin_chgs = main_tab-pf_admin_chgs + prt-betrg.

*  READ THE PF BASIS FROM RT
         WHEN pf_basis.
           main_tab-pf_basis = main_tab-pf_basis + prt-betrg.

*  READ THE EDLI BASIS FROM RT
           IF pepf-payed <> 0.
             SELECT SINGLE * FROM t511k WHERE
             molga = '40' AND konst = 'EDLIB' AND
             begda <= pepf-begda AND endda >= pepf-endda.
             IF sy-subrc = 0.
               MOVE t511k-kwert TO edliper.
             ENDIF.

             IF pn-paper LT edliper.
               main_tab-edli_basis = main_tab-edli_basis + prt-betrg.
             ELSE.
               READ TABLE prt WITH KEY lgart =  edli_basis
                                       cntr1 =  pepf-cntr1.
               IF sy-subrc = 0.
                 main_tab-edli_basis = main_tab-edli_basis + prt-betrg.
               ENDIF.
             ENDIF.
           ENDIF.
           " EDLI BASIS IS SAME AS PF BASIS BEFORE CONSTANT 'EDLIB'
           " MAINTAINED IN V_T511K

*  READ THE PENSION AND EDLI BASIS FROM RT
         WHEN pen_basis.
           main_tab-pen_basis  = main_tab-pen_basis  + prt-betrg.

*  READ THE ER PENSION CONTRIBUTION FROM RT
         WHEN pepf-erpnw.
           main_tab-pen_er_contr = main_tab-pen_er_contr + prt-betrg.

*  READ THE ER EDLI CONTRIBUTION FROM RT
         WHEN pepf-eredw.
           main_tab-edli_er_contr = main_tab-edli_er_contr + prt-betrg.

*  READ THE ER EDLI ADMIN CONTRIBUTION FROM RT
         WHEN pepf-ereda.
           main_tab-edli_admin_chgs = main_tab-edli_admin_chgs +
                                      prt-betrg.

       ENDCASE.
     ENDLOOP.

     main_tab-pf_rate = pepf-eectr.
     IF pepf-pfrfn IS NOT INITIAL.                          "PKT1152442
       main_tab-pfrfn = pepf-pfrfn.
     ELSE.
       main_tab-pfref = pepf-pfref.
     ENDIF.

*     main_tab-eepfn = pepf-eepfn.
*     main_tab-eepnn = pepf-eepnn.

     IF pepf-eepfm IS NOT INITIAL.
       main_tab-eepfn = pepf-eepfm.
     ELSE.
       main_tab-eepfn = pepf-eepfn.
     ENDIF.

     IF pepf-eepnm IS NOT INITIAL.
       main_tab-eepnn = pepf-eepnm.
     ELSE.
       main_tab-eepnn = pepf-eepnn.
     ENDIF.

     main_tab-pnflg = pepf-pnflg.
     main_tab-payed = pepf-payed.
*
*     PERFORM get_basic USING V_FPPER V_FLPER    "Yearly Basic Calcultaion
*                       CHANGING basic.

   ENDLOOP.

   PERFORM wagetypes_correction.
   CLEAR: main_tab-pf_ee_contr,
          main_tab-pf_er_vpf,
          main_tab-pf_er_contr,
          main_tab-pen_er_contr ,
          main_tab-basic ,
          main_tab-edli_admin_chgs ,
          main_tab-pf_admin_chgs,
**  Changes start Naga 16-11-2019
          main_tab-basic_arrear,
          main_tab-pf_ee_arrear,
          main_tab-pf_vpf_arrear,
          main_tab-pf_er_arrear,
          main_tab-pen_er_arrear.
**  Changes end Naga 16-11-2019

   main_tab-pf_ee_contr       = tmp_3f1.
   main_tab-pf_er_vpf         = tmp_3f2.
   main_tab-pf_er_contr       = tmp_3f3.
   main_tab-pen_er_contr      = tmp_3f4.
*   main_tab-basic             = tmp_1001. " Naga 16-11-2019
   main_tab-edli_admin_chgs   = tmp_3602.
   main_tab-pf_admin_chgs     = tmp_3603.
**  Changes start Naga 16-11-2019

   IF ( pn-pabrp >= '06' AND pn-pabrj = '2019' )
     OR ( pn-pabrj > '2019' ).

     IF tmp_146 > 15000.
       main_tab-basic             = tmp_146.
     ELSE.
       IF tmp_111 >= 15000.
         main_tab-basic           = 15000.
       ELSE.
         main_tab-basic           = tmp_111.
       ENDIF.
     ENDIF.

   ELSE.
     main_tab-basic             = tmp_1001.
   ENDIF.

   main_tab-basic_arrear     = tmp_zfa.
   main_tab-pf_ee_arrear     = tmp_5201.
   main_tab-pf_vpf_arrear    = tmp_5202.
   main_tab-pf_er_arrear     = tmp_zf3.
   main_tab-pen_er_arrear    = tmp_zf4.
**  Changes end Naga 16-11-2019

***********start ~ NK on 20.12.2016**************
   SELECT SINGLE icnum FROM pa0185
     INTO main_tab-icnum
     WHERE pernr = main_tab-pernr
     AND   subty = '08'. " for UAN
***********  end ~ NK on 20.12.2016**************
*   MAIN_TAB-PF_ADMIN_CHGS = MAIN_TAB-PF_ADMIN_CHGS / ACC_FACTOR.
*   MAIN_TAB-EDLI_ER_CONTR = MAIN_TAB-EDLI_ER_CONTR / ACC_FACTOR.
*   MAIN_TAB-EDLI_ADMIN_CHGS = MAIN_TAB-EDLI_ADMIN_CHGS / ACC_FACTOR.
 ENDFORM.                              " FILL_MAIN_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_FINAL_TAB
*&---------------------------------------------------------------------*
 FORM fill_final_tab.
   CLEAR final_tab.
   MOVE t7inf1-tstad TO final_tab-tstad.
   LOOP AT main_tab.

     final_tab-tot_pf_basis = final_tab-tot_pf_basis + main_tab-pf_basis
                                          .

     final_tab-tot_pf_ee_contr = final_tab-tot_pf_ee_contr +
                                 main_tab-pf_ee_contr.

     final_tab-tot_pf_er_contr = final_tab-tot_pf_er_contr +
                                 main_tab-pf_er_contr.
     final_tab-tot_pf_vpf = final_tab-tot_pf_vpf +
                                 main_tab-pf_er_vpf.

     final_tab-tot_pf_admin_chgs = final_tab-tot_pf_admin_chgs +
                                   main_tab-pf_admin_chgs.

     final_tab-tot_pen_basis = final_tab-tot_pen_basis
                               + main_tab-pen_basis.

     final_tab-tot_pen_er_contr = final_tab-tot_pen_er_contr +
                                  main_tab-pen_er_contr.

     final_tab-tot_edli_basis =  final_tab-tot_edli_basis
                               + main_tab-edli_basis.

     final_tab-tot_edli_er_contr = final_tab-tot_edli_er_contr +
                                   main_tab-edli_er_contr.

     final_tab-tot_edli_admin_chgs = final_tab-tot_edli_admin_chgs +
                                     main_tab-edli_admin_chgs.

     final_tab-tot_no_emp = final_tab-tot_no_emp + 1.

     final_tab-pf_rate = main_tab-pf_rate.
     IF main_tab-pfrfn IS NOT INITIAL.                      "PKT1152442
       final_tab-pfrfn = main_tab-pfrfn.
     ELSE.
       final_tab-pfref = main_tab-pfref.
     ENDIF.
     final_tab-no_new_this_month = final_tab-no_new_this_month +
                                   main_tab-new_this_month.


     final_tab-no_left_service = final_tab-no_left_service +
                                 main_tab-left_service.

     final_tab-no_last_month = final_tab-no_last_month +
                               main_tab-member_last_month.

*  Calculating no. of subscribers for pension fund
     IF main_tab-pnflg = 'X'.
       final_tab-no_new_this_month_pen = final_tab-no_new_this_month_pen
                                           + main_tab-new_this_month.

       final_tab-no_left_service_pen = final_tab-no_left_service_pen +
                                       main_tab-left_service.
     ENDIF.

*  Calculating no. of subscribers for EDLI
     IF main_tab-payed = '1'.
       final_tab-no_new_this_month_edli =
             final_tab-no_new_this_month_edli + main_tab-new_this_month.

       final_tab-no_left_service_edli = final_tab-no_left_service_edli +
                                            main_tab-left_service.
     ENDIF.

* Calculating no. of subscribers last month for pension fund and EDLI
     IF main_tab-last_mon_pnflg = 'X'.
       final_tab-no_last_month_pen = final_tab-no_last_month_pen +
                                     main_tab-member_last_month.
     ENDIF.

     IF main_tab-last_mon_payed = '1'.
       final_tab-no_last_month_edli = final_tab-no_last_month_edli +
                                      main_tab-member_last_month.
     ENDIF.

   ENDLOOP.

   PERFORM round_amt USING '100'
                     CHANGING final_tab-tot_edli_er_contr.

   PERFORM round_amt USING '5'
                     CHANGING final_tab-tot_pf_admin_chgs.

   PERFORM round_amt USING '5'
                     CHANGING final_tab-tot_edli_admin_chgs.

   final_tab-total_members = final_tab-no_last_month +
                             final_tab-no_new_this_month -
                             final_tab-no_left_service.

   final_tab-total_members_pen = final_tab-no_last_month_pen +
                                 final_tab-no_new_this_month_pen -
                                 final_tab-no_left_service_pen.

   final_tab-total_members_edli = final_tab-no_last_month_edli +
                                  final_tab-no_new_this_month_edli -
                                  final_tab-no_left_service_edli.

   APPEND final_tab.
 ENDFORM.                              " FILL_FINAL_TAB

*&---------------------------------------------------------------------*
*&      Form  IMPORT_RESULTS
*&---------------------------------------------------------------------*
 FORM import_results.
*  IMPORT ALL CLUSTER RESULTS.
   CLEAR prt.
   CLEAR pepf.
   CLEAR lepf.
   CLEAR temp_rgdir.

   REFRESH prt.
   REFRESH pepf.
   REFRESH lepf.
   REFRESH temp_rgdir.

   CLEAR rgdir.

   cd-key-pernr = pernr-pernr.
   rp-imp-c2-cu.

*  RETURN CODE FROM IMPORT.
   IF rp-imp-cd-subrc <> 0.
     MESSAGE s124(hrpadin01) WITH pernr-pernr.
*  Payroll results not found for the employee no. &
     PERFORM build_error TABLES hr_error
                         USING space sy-msgid sy-msgno
                         sy-msgv1 space space space.
     REJECT.
   ENDIF.

   LOOP AT rgdir WHERE payty = '' AND fpbeg GE pn-begda AND fpend LE
pn-endda.                                                  "RSKNT666437
     MOVE-CORRESPONDING rgdir TO temp_rgdir.
     APPEND temp_rgdir.
   ENDLOOP.

   SORT temp_rgdir BY seqnr DESCENDING.
   READ TABLE temp_rgdir INDEX 1.
   rx-key-seqno = temp_rgdir-seqnr.
   rx-key-pernr = pernr-pernr.
   rp-imp-c2-in.

*  RETURN CODE FROM IMPORT.
   IF rp-imp-in-subrc <> 0.
     REJECT.
   ENDIF.

   prt[] = rt[].
   pcrt[] = crt[].
   pepf[] = epf[].

   CLEAR rgdir.
   CLEAR temp_rgdir.
   REFRESH temp_rgdir.

   LOOP AT rgdir WHERE payty = '' AND
                       fpbeg GE last_month_begda AND
                       fpend LE last_month_endda.          "RSKNT666437
     MOVE-CORRESPONDING rgdir TO temp_rgdir.
     APPEND temp_rgdir.
   ENDLOOP.

   SORT temp_rgdir BY seqnr DESCENDING.
   READ TABLE temp_rgdir INDEX 1.
   rx-key-seqno = temp_rgdir-seqnr.
   rx-key-pernr = pernr-pernr.
   rp-imp-c2-in.

*  RETURN CODE FROM IMPORT.
   IF rp-imp-in-subrc = 0.
     lepf[] = epf[].
   ELSE.
     MESSAGE s139(hrpadin01) WITH pernr-pernr.
*  Last month payroll results not available for the employee no. &
     PERFORM build_error TABLES hr_error
                         USING space sy-msgid sy-msgno
                         sy-msgv1 space space space.

   ENDIF.

 ENDFORM.                              " IMPORT_RESULTS

*&---------------------------------------------------------------------*
*&      Form  CHECK_LAST_MONTH
*&---------------------------------------------------------------------*
 FORM check_last_month.
   READ TABLE lepf WITH KEY tstid = trust_id
                            endda = last_month_endda.
   IF sy-subrc = 0.
     main_tab-member_last_month = 1.
     main_tab-last_mon_pnflg = lepf-pnflg.
     main_tab-last_mon_payed = lepf-payed.
   ENDIF.
 ENDFORM.                              " CHECK_LAST_MONTH

*&---------------------------------------------------------------------*
*&      Form  FILL_FORM5_TAB
*&---------------------------------------------------------------------*
 FORM fill_form5_tab.
   MOVE-CORRESPONDING pepf TO form5_tab.
   form5_tab-dojpf = pepf-begda.
*To find out the total period of previous service.
   rp-provide-from-frst p0023 space begda1800 pn-endda.
   IF pnp-sw-found EQ '1'.
     service_days = p0587-begda - p0023-begda.
     service_months = service_days / 30.
     form5_tab-prev_service = service_months.
   ELSE.
     form5_tab-prev_service = 0.
   ENDIF.
   form5_tab_entry = 1.

 ENDFORM.                              " FILL_FORM5_TAB

*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_UPPER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->STRING_LOWER  text
*      <--STRING_UPPER  text
*----------------------------------------------------------------------*
 FORM convert_to_upper USING    string_lower
                       CHANGING string_upper.

*   CALL FUNCTION '2054_TRANSLATE_2_UPPERCASE'
*       EXPORTING
*            I_STRING      = STRING_LOWER
*       IMPORTING
*            E_STRING      = STRING_UPPER
**     EXCEPTIONS
**          ERROR_OCCURED = 1
**          OTHERS        = 2
*             .
*   IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*   ENDIF.

   MOVE string_lower TO string_upper.
   TRANSLATE string_upper TO UPPER CASE.


 ENDFORM.                              " CONVERT_TO_UPPER

*&---------------------------------------------------------------------*
*&      Form  CHECK_INT_TABLE
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_TAB_NAME  text
*----------------------------------------------------------------------*
 FORM check_int_table TABLES p_tab_name.

   DESCRIBE TABLE p_tab_name LINES num.
   IF num EQ 0.
     MESSAGE i116(hrpadin01).
     EXIT.
   ENDIF.
* AB08022017 INSERT START...
   LOOP AT pnpabkrs.
     IF pnptimr9 EQ 'X'.
       CALL FUNCTION 'HR_MX_GET_PAYROLL_PERIOD'
         EXPORTING
*          payroll_area   = pnpxabkr
           payroll_area   = pnpabkrs-low
           date           = sy-datum
         IMPORTING
           payroll_year   = p_pabrj
           payroll_period = p_pabrp
*          PERIOD_BEGIN   =
*          PERIOD_END     =
         EXCEPTIONS
           t549a_error    = 1
           t549q_error    = 2
           OTHERS         = 3.
       IF sy-subrc <> 0.
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
       ELSE.
         CONCATENATE p_pabrj p_pabrp INTO p_fpper.
       ENDIF.
     ELSE.
       CONCATENATE pnppabrj pnppabrp INTO p_fpper.
     ENDIF.

     SELECT pernr
            seqnr
            fpper FROM hrpy_rgdir
             APPENDING TABLE itab1
                   FOR ALL ENTRIES IN main_tab
                 WHERE pernr EQ main_tab-pernr
                   AND abkrs EQ pnpabkrs-low
                   AND fpper EQ p_fpper
                   AND inper EQ p_fpper.

   ENDLOOP. "LOOP AT pnpabkrs.
* AB08022017 INSERT END...
 ENDFORM.                              " CHECK_INT_TABLE

*&---------------------------------------------------------------------*
*&      Form  ROUND_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RND_FACTOR   text
*      <--P_AMT  text
*----------------------------------------------------------------------*
 FORM round_amt USING    VALUE(p_rnd_factor)
                CHANGING p_amt.

   CALL FUNCTION 'HR_IN_ROUND_AMT'
     EXPORTING
       amount = p_amt
       rndoff = p_rnd_factor
     IMPORTING
       retamt = p_amt.


 ENDFORM.                    " ROUND_AMT

*&---------------------------------------------------------------------*
*&      Form  FILL_EMPLYE_DET
*&---------------------------------------------------------------------*
 FORM fill_emplye_det  USING nbegda nendda.

   DATA:  bck_subrc LIKE sy-subrc.
   DATA:  BEGIN OF p0002_tmp OCCURS 0.
       INCLUDE STRUCTURE p0002.
   DATA:  END OF p0002_tmp.

   DATA format_name LIKE pernr-ename.

   nbegda = pn-begda.
   nendda = pn-endda.


   CALL FUNCTION 'HR_READ_INFOTYPE'
     EXPORTING
*      TCLAS           = 'A'
       pernr           = pernr-pernr
       infty           = '0002'
       begda           = nbegda
       endda           = nendda
     IMPORTING
       subrc           = bck_subrc
     TABLES
       infty_tab       = p0002_tmp
     EXCEPTIONS
       infty_not_found = 1
       OTHERS          = 2.

   IF bck_subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

   READ TABLE p0002_tmp INDEX 1.

*    concatenate p0002_tmp-vorna p0002_tmp-nachn into empye_det-ename.
   CALL FUNCTION 'RP_EDIT_NAME'
     EXPORTING
       format    = '01'
       langu     = sy-langu
       molga     = '40'
       pp0002    = p0002_tmp
     IMPORTING
       edit_name = format_name.

   MOVE format_name TO e_name.


 ENDFORM.                    " FILL_EMPLYE_DET
*&---------------------------------------------------------------------*
*&      Form  WAGE_GRP_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM wage_grp_get USING pn-endda
                   CHANGING v_fpper
                            v_flper .

   DATA: p_gjahr  TYPE gjahr,
         v_fsdate TYPE sy-datum,
         v_fedate TYPE sy-datum.
   DATA : lv_tabix TYPE sy-tabix.
   DATA : t_t549q TYPE t549q OCCURS 0 WITH HEADER LINE.


   p_gjahr = pn-endda+0(4).

   CALL FUNCTION 'HR_IN_GET_FISCAL_PERIOD'
     EXPORTING
       parea1 = 'IN'
       pyear1 = p_gjahr
     IMPORTING
       pbegda = v_fsdate
       pendda = v_fedate
*      FIYEAR =
* EXCEPTIONS
*      NO_ENTRY_FOUND_T549A        = 1
*      NO_ENTRY_FOUND_T549Q        = 2
*      NO_BEGIN_PERIOD_FOUND       = 3
*      NO_END_PERIOD_FOUND         = 4
*      NO_CNTRL_RECORD_FOUND       = 5
*      OTHERS = 6
     .
   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ELSE.

     CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
       EXPORTING
         get_begda   = v_fsdate
         get_endda   = v_fedate
*        GET_PERMO   = RPTIME_PERIOD
*   IMPORTING
*        GET_PABRJ   =
*        GET_PABRP   =
       TABLES
         get_periods = t_t549q
*   EXCEPTIONS
*        NO_PERIOD_FOUND       = 1
*        NO_VALID_PERMO        = 2
*        OTHERS      = 3
       .
     IF sy-subrc EQ 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
       LOOP AT t_t549q.
         MOVE-CORRESPONDING t_t549q TO wa_t549q.
         CONCATENATE  t_t549q-pabrj t_t549q-pabrp  INTO wa_t549q-fpper.
         APPEND wa_t549q TO it_t549q.
         CLEAR  wa_t549q.
       ENDLOOP.
     ENDIF.

   ENDIF.

   READ TABLE it_t549q INTO wa_t549q INDEX 1.
   IF sy-subrc EQ 0.
     v_fpper = wa_t549q-fpper.
   ENDIF.
   DESCRIBE TABLE it_t549q LINES v_lines.
   READ TABLE it_t549q INTO wa_t549q INDEX v_lines.
   IF sy-subrc EQ 0.
     v_flper = wa_t549q-fpper.
   ENDIF.

 ENDFORM.                    " WAGE_GRP_GET
*&---------------------------------------------------------------------*
*&      Form  GET_BASIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_FPPER  text
*      -->P_V_FPPER  text
*----------------------------------------------------------------------*
 FORM get_basic  USING    fp_per TYPE hrpy_rgdir-fpper
                          fl_per TYPE hrpy_rgdir-fpper
                 CHANGING v_total TYPE pc207-betrg.


   DATA : BEGIN OF itab1 OCCURS 0 ,
            pernr LIKE hrpy_rgdir-pernr,
            paydt LIKE hrpy_rgdir-paydt,
            seqnr LIKE hrpy_rgdir-seqnr,
            fpper LIKE hrpy_rgdir-fpper,
            inper LIKE hrpy_rgdir-inper,
            persg LIKE pa0001-persg,
          END OF itab1.
   DATA: wa_pa0001 TYPE pa0001,
         payresult TYPE pay99_result,
         fl_flag.
*      v_total like pc207-betrg.
   FIELD-SYMBOLS : <f1>,<g1>,<gs1>,<struc>,<h1>,<hs1>.

   DATA : v_text(30),v_text1(30),
          v_etot LIKE pa0008-bet01.
   DATA : BEGIN OF rt1 OCCURS 0.
       INCLUDE STRUCTURE pc207 .
   DATA : END OF rt1.
   DATA : BEGIN OF rt2 OCCURS 0,
            fpper       LIKE hrpy_rgdir-inper,
            inper       LIKE hrpy_rgdir-inper,
            betrg       LIKE pc207-betrg,
            zwage_grp   LIKE z6hra_wage_grp-zwage_grp, " added by nk on 12.01.2017
            lgart       LIKE pc207-lgart,                 " added by nk on 12.01.2017
            zzindicator TYPE z6hra_wage_grp-zzindicator, " added by nk on 12.01.2017
          END OF rt2.


   SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab1
                      WHERE pernr = pernr-pernr
*{   REPLACE        PREK900435                                        1
*\                     AND   paydt EQ s_paydt.
                      AND abkrs = pnpxabkr
*                     AND   paydt EQ s_paydt
                      AND fpper BETWEEN fp_per AND fl_per
                      AND inper BETWEEN fp_per AND fl_per
                      AND occat EQ space.

   LOOP AT itab1 .
     SELECT SINGLE * FROM pa0001 INTO wa_pa0001
       WHERE pernr EQ itab1-pernr
         AND begda LE sy-datum
         AND endda GE sy-datum
         AND persg EQ 'D'.
     IF sy-subrc EQ 0.
       DELETE itab1 INDEX sy-tabix.
       CLEAR itab1.
     ENDIF.
   ENDLOOP.

   SORT itab1 BY pernr paydt seqnr.
   LOOP AT itab1.
     CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
       EXPORTING
*        clusterid               = t500l_header-relid
         employeenumber          = itab1-pernr
         sequencenumber          = itab1-seqnr
         read_only_international = 'X'
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

       CLEAR : rt2, rt2[].
       SORT rt1 BY lgart.
       LOOP AT payresult-inter-rt INTO rt1.

         IF rt1-lgart EQ '1001' OR rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
                 OR rt1-lgart EQ '5000' OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.

           MOVE-CORRESPONDING rt1 TO rt2.
           MOVE : itab1-fpper TO rt2-fpper.
           MOVE : itab1-inper TO rt2-inper.

           COLLECT rt2.
           CLEAR : rt2.
         ENDIF.
       ENDLOOP.

**      ----add wage grp       " added by nk on 12.01.2017 - ZPY_SALREG
*      loop at rt2 .
*        clear wa_z6hra_wage_grp.
*        read table it_z6hra_wage_grp into wa_z6hra_wage_grp with key lgart = rt2-lgart
*                                                               binary search.
*        if sy-subrc = 0.
*          rt2-zwage_grp = wa_z6hra_wage_grp-zwage_grp.
*          rt2-zzindicator = wa_z6hra_wage_grp-zzindicator.
*        endif.
*
*        modify rt2 transporting zwage_grp zzindicator.
*      endloop.
**      -----

       SORT rt2 BY  fpper inper .
       LOOP AT  rt2.
         AT NEW  fpper.
           fl_flag = 1.
         ENDAT.
         IF fl_flag = 1.
           CLEAR fl_flag.
           LOOP AT it_t549q INTO wa_t549q  .
             DO.
               ASSIGN COMPONENT sy-index OF STRUCTURE wa_t549q
                                                    TO <f1>.
               IF sy-subrc NE 0.
                 EXIT.
               ENDIF.

               CASE sy-index.
                 WHEN '9'.

                   IF rt2-fpper EQ <f1> AND rt2-inper EQ <f1>.


*                    ASSIGN COMPONENT RT2-ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
*                    MOVE RT2-FPPER TO V_TXT.
*                    CONCATENATE '<struc>' '-' V_TXT INTO V_TEXT.
**                    CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
*                    assign (v_text) to <h1>.
*                    <h1> = RT2-BETRG.
                     v_total = v_total + rt2-betrg.


*                      condense <h1> no-gaps.
*---for total earnings and deductions

                   ENDIF.

*                when '2'.
*                  if rt2-zwage_grp eq <f1>.
*                      assign component rt2-zwage_grp of structure <struc>
*                                              to <g1>.
**                    ASSIGN COMPONENT RT2-ZWAGE_GRP OF STRUCTURE <STRUC>
**                                            TO <G1>.
*                      concatenate '<struc>' '-' rt2-zwage_grp into v_text.
**                    CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
*                      assign (v_text) to <h1>.
*
*                      <h1> = v_total.
**                      condense <h1> no-gaps.
**---for total earnings and deductions
*                      if rt2-lgart = wa_z6hra_wage_grp-lgart.
*                        if rt2-zzindicator = 'E'.
*                          assign component 'TEARN' of structure <struc> to <gs1>.
*                          concatenate '<struc>' '-' 'TEARN' into v_text1.
*                          assign (v_text1) to <hs1>.
*                          v_etot = v_etot + v_total.        "rt2-betrg.
*                          <hs1> = v_etot.
*                          clear v_text1.
**                        elseif rt2-zzindicator = 'D'.
**                          assign component 'TDEDUC' of structure <struc> to <gs2>.
**                          concatenate '<struc>' '-' 'TDEDUC' into v_text1.
**                          assign (v_text1) to <hs2>.
**                          v_dtot = v_dtot + v_total.        "rt2-betrg.
**                          <hs2> = v_dtot.
**                          clear v_text1.
**                        elseif rt2-zzindicator = 'N'.
**                          assign component 'TNET' of structure <struc> to <gs3>.
**                          concatenate '<struc>' '-' 'TNET' into v_text1.
**                          assign (v_text1) to <hs3>.
**                          v_ntot = v_ntot + rt2-betrg.
**                          <hs3> = v_ntot.
**                          clear v_text1.
*                        endif.
*                      endif.
*
**                        IF RT2-zzindicator = 'D'.
**                          assign component 'TDEDUC' OF STRUCTURE <STRUC> TO <GS2>.
**                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO V_TEXT1.
**                          assign (v_text1) to <hS2>.
**                          V_DTOT = V_DTOT + rt2-betrg.
**                          <HS2> = V_DTOT.
**                          CLEAR V_TEXT1.
**                        endif.
**----for total earnings and deductions
*
*                    endif.

               ENDCASE.
             ENDDO.
           ENDLOOP.
         ENDIF.

       ENDLOOP.
     ENDIF.
   ENDLOOP.
 ENDFORM.                    " GET_BASIC
*&---------------------------------------------------------------------*
*&      Form  WAGETYPES_CORRECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM wagetypes_correction .
   DATA: BEGIN OF vlist OCCURS 0,
           line(1024) TYPE c,
         END OF vlist.
   DATA: list_tab LIKE STANDARD TABLE OF abaplist.
*   DATA: seltab TYPE TABLE OF rsparams,
*         seltab_wa LIKE LINE OF seltab.
   DATA: blank TYPE char8,
         lines TYPE i.
   DATA: BEGIN OF wa_wages,
           cocd       TYPE char4,
           comp_name  TYPE char32,
           pa         TYPE char4,
           pa_txt     TYPE char32,
           pyarea     TYPE char16,
           pyarea_txt TYPE char32,
           perpa      TYPE char8,
           name       TYPE char32,
           for_period TYPE char16,
           pmt_dt     TYPE char16,
           py_type    TYPE char8,
           py_id      TYPE char8,
           cgrp       TYPE  char8,
           wt         TYPE char8,
           wt_txt     TYPE char32,
           no_of      TYPE char16,
           amount     TYPE char38, "char32,
           crcy       TYPE char8,
         END OF wa_wages,
         it_wages LIKE STANDARD TABLE OF wa_wages.


   CLEAR: seltab_wa,
          tmp_3f1,
          tmp_3f2,
          tmp_3f3,
          tmp_3f4,
          tmp_1001,
          tmp_3602,
          tmp_3603,
** 28-11-2019 Naga start
          tmp_146,
          tmp_111,
          tmp_zfa,
          tmp_5201,
          tmp_5202,
          tmp_zf3,
          tmp_zf4.
** 28-11-2019 Naga end
   REFRESH: seltab.

   PERFORM wagetype_fill.

   SUBMIT h99cwtr0
     WITH SELECTION-TABLE seltab
     EXPORTING LIST TO MEMORY
     AND RETURN.

* From memory transfer the program output into internal table through below FM :

   CALL FUNCTION 'LIST_FROM_MEMORY'
     TABLES
       listobject = list_tab.

   IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ENDIF.

* Convert a (Saved) List Object to ASCI by using below FM.
   IF list_tab[] IS NOT INITIAL.

     CALL FUNCTION 'LIST_TO_ASCI'
       EXPORTING
         list_index = -1
       TABLES
         listasci   = vlist
         listobject = list_tab.

     IF sy-subrc <> '0'.
       WRITE:/ 'LIST_TO_ASCI error !! ', sy-subrc.
     ELSE.

     ENDIF.
   ENDIF.

   LOOP AT vlist WHERE NOT line CS '-----' .
     SPLIT vlist-line AT '|' INTO  blank
                                   wa_wages-cocd
                                   wa_wages-comp_name
                                   wa_wages-pa
                                   wa_wages-pa_txt
                                   wa_wages-pyarea
                                   wa_wages-pyarea_txt
                                   wa_wages-perpa
                                   wa_wages-name
                                   wa_wages-for_period
                                   wa_wages-pmt_dt
                                   wa_wages-py_type
                                   wa_wages-py_id
                                   wa_wages-cgrp
                                   wa_wages-wt
                                   wa_wages-wt_txt
                                   wa_wages-no_of
                                   wa_wages-amount
                                   wa_wages-crcy.
     REPLACE ALL OCCURRENCES OF ',' IN wa_wages-amount WITH ''.
     CONDENSE : wa_wages-cocd,
               wa_wages-comp_name,
               wa_wages-pa,
               wa_wages-pa_txt,
               wa_wages-pyarea,
               wa_wages-pyarea_txt,
               wa_wages-perpa,
               wa_wages-name,
               wa_wages-for_period,
               wa_wages-pmt_dt,
               wa_wages-py_type,
               wa_wages-py_id,
               wa_wages-cgrp,
               wa_wages-wt,
               wa_wages-wt_txt,
               wa_wages-no_of,
               wa_wages-amount,
               wa_wages-crcy.
* Naga
     IF wa_wages-for_period NE pn-paper.
       CONTINUE.
     ENDIF.
* Naga
     APPEND wa_wages TO it_wages.
     CLEAR wa_wages.
   ENDLOOP.
* DELETE it_wages INDEX 1.                  "IHDK903813
   DESCRIBE TABLE it_wages LINES lines.
   LOOP AT it_wages INTO wa_wages.
     CASE wa_wages-wt.
       	WHEN '/3F1'.
         tmp_3f1 = tmp_3f1 + wa_wages-amount.
       	WHEN '/3F2'.
         tmp_3f2 = tmp_3f2 + wa_wages-amount.
       	WHEN '/3F3'.
         tmp_3f3 = tmp_3f3 + wa_wages-amount.
       	WHEN '/3F4'.
         tmp_3f4 = tmp_3f4 + wa_wages-amount.
        WHEN '1001' OR '1002' OR '1003'.          " --IHDK903766 ++IHDK903893
*        WHEN '/146' OR '1001' OR '1002' OR '1003'.  " ++IHDK903766 --IHDK903797
*       WHEN '/146' OR '/ZFA' OR '1001' OR '1002' OR '1003'.  " ++IHDK903797 --IHDK903893
         tmp_1001 = tmp_1001 + wa_wages-amount.
       	WHEN '3602'.
         tmp_3602 = tmp_3602 + wa_wages-amount.
       	WHEN '3603'.
         tmp_3603 = tmp_3603 + wa_wages-amount.
**  Changes start by Naga 16-11-2019
        WHEN '/146'.
          tmp_146 = tmp_146 + wa_wages-amount.
        WHEN '/111'.
          tmp_111 = tmp_111 + wa_wages-amount.
        WHEN '/ZFA'.
          tmp_zfa = tmp_zfa  + wa_wages-amount.
        WHEN '5201'.
         tmp_5201 = tmp_5201 + wa_wages-amount.
        WHEN '5202'.
         tmp_5202 = tmp_5202 + wa_wages-amount.
        WHEN '/ZF3'.
         tmp_zf3  = tmp_zf3  + wa_wages-amount.
        WHEN '/ZF4'.
         tmp_zf4  = tmp_zf4  + wa_wages-amount.
**  Changes end by Naga 16-11-2019
     ENDCASE.
   ENDLOOP.
 ENDFORM.                    " WAGETYPES_CORRECTION
*&---------------------------------------------------------------------*
*&      Form  WAGETYPE_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM wagetype_fill .

   "Persnal Number
   seltab_wa-selname = 'PNPPERNR'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = pernr-pernr.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "Begin Date
   seltab_wa-selname = 'BEGD_CAL'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = pn-begda.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "End Date
   seltab_wa-selname = 'ENDD_CAL'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = pn-endda.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "PF_EE_CONTR
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '/3F1'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "PF_ER_VPF
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '/3F2'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "PF_ER_CONTR
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '/3F3'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "PEN_ER_CONTR
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '/3F4'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "Basic Pay
   " Changes begin IHDK903766
   IF ( pn-pabrp >= '06' AND pn-pabrj = '2019' )
     OR ( pn-pabrj > '2019' ).

     seltab_wa-selname = 'S_LGART'.
     seltab_wa-kind = 'S'.
     seltab_wa-sign    = 'I'.
     seltab_wa-option  = 'EQ'.
     seltab_wa-low    = '/146'.
     seltab_wa-high   = '0'.
     APPEND seltab_wa TO seltab.
     CLEAR seltab_wa.

     seltab_wa-selname = 'S_LGART'.
     seltab_wa-kind    = 'S'.
     seltab_wa-sign    = 'I'.
     seltab_wa-option  = 'EQ'.
     seltab_wa-low     = '/111'.
     seltab_wa-high    = '0'.
     APPEND seltab_wa TO seltab.
     CLEAR seltab_wa.

     " Changes begin IHDK903797
*     seltab_wa-selname = 'S_LGART'.
*     seltab_wa-kind = 'S'.
*     seltab_wa-sign    = 'I'.
*     seltab_wa-option  = 'EQ'.
*     seltab_wa-low    = '/ZFA'.
*     seltab_wa-high   = '0'.
*     APPEND seltab_wa TO seltab.
*     CLEAR seltab_wa.
     " Changes End IHDK903797
   ELSE.

     " Changes End IHDK903766

     seltab_wa-selname = 'S_LGART'.
     seltab_wa-kind = 'S'.
     seltab_wa-sign    = 'I'.
     seltab_wa-option  = 'EQ'.
     seltab_wa-low    = '1001'.
     seltab_wa-high   = '0'.
     APPEND seltab_wa TO seltab.
     CLEAR seltab_wa.

     seltab_wa-selname = 'S_LGART'.
     seltab_wa-kind = 'S'.
     seltab_wa-sign    = 'I'.
     seltab_wa-option  = 'EQ'.
     seltab_wa-low    = '1002'.
     seltab_wa-high   = '0'.
     APPEND seltab_wa TO seltab.
     CLEAR seltab_wa.

     seltab_wa-selname = 'S_LGART'.
     seltab_wa-kind = 'S'.
     seltab_wa-sign    = 'I'.
     seltab_wa-option  = 'EQ'.
     seltab_wa-low    = '1003'.
     seltab_wa-high   = '0'.
     APPEND seltab_wa TO seltab.
     CLEAR seltab_wa.

   ENDIF.                              " IHDK903766
   "Edliadm
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '3602'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   "PF_ADMIN_CHGS
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '3603'.
   seltab_wa-high   = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

***   Changes start Naga 16-11-2019
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind    = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low     = '/ZFA'.
   seltab_wa-high    = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind    = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low     = '5201'.
   seltab_wa-high    = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind    = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low     = '5202'.
   seltab_wa-high    = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind    = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low     = '/ZF3'.
   seltab_wa-high    = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.

   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind    = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low     = '/ZF4'.
   seltab_wa-high    = '0'.
   APPEND seltab_wa TO seltab.
   CLEAR seltab_wa.
***   Changes End Naga 16-11-2019
 ENDFORM.                    " WAGETYPE_FILL
