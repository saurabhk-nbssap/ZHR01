*----------------------------------------------------------------------*
*  INCLUDE PCF16IN4                                                    *
*XXXNTnote number  <date>   Note<note number>:<short description>
*MKINT811821      25012005  Note811821       : Form 16 legal changes as
*                                              per Circular 06/2004
*----------------------------------------------------------------------*
* Customization made only for fiscal year 2010 by Anees
*----------------------------------------------------------------------*

FORM PRINT_MODULE.

  DATA: LANG LIKE SY-LANGU,
        PAGE_NO TYPE I,
        TAX_PERK(10),
        EERECVR(9),
        VAL_PERK(10),
        VAL_F12BA LIKE PC207-BETRG,
        PROF_F12BA LIKE PC207-BETRG,
        SAL_SEC17 LIKE PC207-BETRG,
        TOT_VAL_PERK LIKE PC207-BETRG,
        TOT_EERECVR LIKE PC207-BETRG,
        TOT_TAX_PERK LIKE PC207-BETRG,
        SALARIES_WO_PERK LIKE PC207-BETRG,
        F12BA_BALANCE LIKE PC207-BETRG,
        F12BA_STD_DED LIKE PC207-BETRG.

  DATA: BEGIN OF TEMP_PAYMENTS OCCURS 10.
          INCLUDE STRUCTURE PINBK.
  DATA: END OF TEMP_PAYMENTS.
  DATA: OUT TYPE I.
  DATA: TEMP_PSCRIPT TYPE C.
  DATA : CNTR TYPE I.                             " MDSNT927906
  DATA : J TYPE I.
  DATA : TOTAL_TAX LIKE PC207-BETRG.
  DATA : BEGIN OF TEMP_S88 OCCURS 0.
          INCLUDE STRUCTURE INT_S88.
  DATA : END OF TEMP_S88.
  DATA : CONT_AMT(20),
         DED_AMT(20),
         SEQ TYPE I.
  DATA:  FORM_DATE LIKE SY-DATUM.
  DATA : conmt_epf(20),                     "PKT1155907
         dedmt_epf(20).
  MOVE 'EN' TO LANG.
  ENDDATE = SY-DATUM.
  FORM_DATE = '99991231'.
  FORM_DATE+0(4) = YEAR.

********START OF PDF CODING ********
  SNAME = '40TAX016'.
  PERFORM GET_LAYOUT_SET USING SNAME ENDDATE.
  PERFORM GET_OUTPUT_TYPE USING    ENDDATE
                          CHANGING FORMNAME
                                   TEMP_PSCRIPT.
  IF DISP_FLG_LOT GE 1.
    P_SCRIPT = TEMP_PSCRIPT.
    IF P_SCRIPT EQ 'X'.
      CLEAR P_PDF.
    ELSE.
      P_PDF = 'X'.
    ENDIF.
  ENDIF.

  CLEAR SNAME.
  CLEAR FORMNAME.

  IF P_SCRIPT = GC_X.
********END OF PDF CODING**********

    IF SY-BATCH = 'X' AND ( NOT LAYOUT IS INITIAL OR
                            NOT LAYOUT1 IS INITIAL OR
                            NOT LAYOUT2 IS INITIAL OR
                            NOT LAYOUT3 IS INITIAL ).
      DISP_FLG_LOT = -1.
    ENDIF.

  DATA:  FORM_16(1) TYPE C.
    LOOP AT FINAL_TAB.
     CALL METHOD CL_HRPAYIN_SWITCH_CHECK_4=>HRLOCIN_SFWS_SC_01
      RECEIVING
       RV_ACTIVE = HTEXT_SWITCH
        .
     IF HTEXT_SWITCH = 'X'.
       PERFORM get_custom_text CHANGING FLEXTXT final_tab-pernr.
     ELSE.
           FLEXTXT = final_tab-pernr.
     ENDIF.
************************************************************************
* CLEARING VARIABLES OF FORM12BA
      CLEAR VAL_F12BA.CLEAR PROF_F12BA.CLEAR SAL_SEC17.CLEAR TOT_VAL_PERK.
      CLEAR TOT_EERECVR.CLEAR TOT_TAX_PERK.
************************************************************************
      SNAME = '40TAX16A'.
    CLEAR FORM_16.                        "Print Form 16AA
    IF infos IS INITIAL.
        IF FINAL_TAB-BALANCE < 150000 AND FINAL_TAB-BUS_PROF <= 0 AND
           FINAL_TAB-TDS_IFOS <= 0.
          IF DISP_FLG_LOT < 1 AND LAYOUT3 NE ' '.
            FORMNAME = LAYOUT3.
          ELSE.
            PERFORM GET_LAYOUT_SET USING SNAME ENDDATE.
          ENDIF.
        ELSE.
         FORM_16 = 'X'.                  "Print Form 16
         IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
            FORMNAME = LAYOUT.
          ELSE.
            SNAME = '40TAX016'.
          PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE.
          ENDIF.
        ENDIF.
      ELSE.
      FORM_16 = 'X'.                     "Print Form 16
      IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
          FORMNAME = LAYOUT.
        ELSE.
          SNAME = '40TAX016'.
        PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE.
        ENDIF.
      ENDIF.

      PERFORM OPEN_FORM USING LANG.

*  PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' PBEGDA.
*  PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' PENDDA.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'assm_start' PENDDA(4).
      PERFORM CONVERT_TO_SCRIPTVAR USING 'assm_end' ASSM_END.

* Get employer address
    CLEAR SADR.
      CLEAR STATE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
*    PERFORM ADDRESS USING COMP_CD ADDR1_VAL.    "MKINT910704
      PERFORM ER_ADDRESS USING FINAL_TAB-TANNO.                "MKINT910704
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename1'(010) ADDR1_VAL-NAME1 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename2'(016) SADR-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Estreet'(024) SADR-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epobox'(025) SADR-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epocode'(026) SADR-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ecity'(027) SADR-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Edistrict'(028) SADR-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Estate'(035) STATE.
      ENDIF.

* Start of First Page
      PERFORM START_FORM USING FORMNAME LANG 'MAIN'.

* Get TDS Circle address
      CLEAR SADR.
      CLEAR STATE.
      PERFORM GET_ADDRESS USING FINAL_TAB-ADDRS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cname1'(036) SADR-NAME1 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cname2'(063) SADR-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cstreet'(064) SADR-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cpobox'(065) SADR-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cpocode'(066) SADR-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ccity'(067) SADR-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cdistrict'(068) SADR-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                         CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Cstate'(069) STATE.
      ENDIF.

      LOOP AT HD_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
        IF HD_TAB-F16_BEGDA > PBEGDA.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' HD_TAB-F16_BEGDA.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' PBEGDA.
        ENDIF.
        IF HD_TAB-F16_ENDDA < PENDDA.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' HD_TAB-F16_ENDDA.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' PENDDA.
        ENDIF.
      ENDLOOP.

* Print acknowledgement numbers of the Form 24Q eFiling
    DATA: TEMP(10) TYPE C,
          qtr_no type N value 0.
    CLEAR TEMP.

    CLEAR ACKNO_TAB.

    LOOP AT ACKNO_TAB WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  1.
      EXIT.
    ENDLOOP.

    perform convert_to_scriptvar using 'acknoq1_1' ackno_tab-ackno1.
    perform convert_to_scriptvar using 'acknoq1_2' ackno_tab-ackno2.
    perform convert_to_scriptvar using 'acknoq1_3' ackno_tab-ackno3.
    perform convert_to_scriptvar using 'acknoq1_4' ackno_tab-ackno4.

    CLEAR ACKNO_TAB.

    LOOP AT ACKNO_TAB WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  2.

      EXIT.
    ENDLOOP.

    perform convert_to_scriptvar using 'acknoq2_1' ackno_tab-ackno1.
    perform convert_to_scriptvar using 'acknoq2_2' ackno_tab-ackno2.
    perform convert_to_scriptvar using 'acknoq2_3' ackno_tab-ackno3.
    perform convert_to_scriptvar using 'acknoq2_4' ackno_tab-ackno4.


    CLEAR ACKNO_TAB.

    LOOP AT ACKNO_TAB WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  3.
      EXIT.
    ENDLOOP.

    perform convert_to_scriptvar using 'acknoq3_1' ackno_tab-ackno1.
    perform convert_to_scriptvar using 'acknoq3_2' ackno_tab-ackno2.
    perform convert_to_scriptvar using 'acknoq3_3' ackno_tab-ackno3.
    perform convert_to_scriptvar using 'acknoq3_4' ackno_tab-ackno4.

    CLEAR ACKNO_TAB.

    LOOP AT ACKNO_TAB WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  4.
      EXIT.
    ENDLOOP.

    perform convert_to_scriptvar using 'acknoq4_1' ackno_tab-ackno1.
    perform convert_to_scriptvar using 'acknoq4_2' ackno_tab-ackno2.
    perform convert_to_scriptvar using 'acknoq4_3' ackno_tab-ackno3.
    perform convert_to_scriptvar using 'acknoq4_4' ackno_tab-ackno4.


* Employer info
      PERFORM CONVERT_TO_SCRIPTVAR USING 'panno' FINAL_TAB-PANNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tanno' FINAL_TAB-TANNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'girno' FINAL_TAB-GIRNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tdscr' FINAL_TAB-TDSCR.

* Employee info
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ename' FINAL_TAB-ENAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'design' FINAL_TAB-POSITION.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'empno' FINAL_TAB-PERNR.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'icnum' FINAL_TAB-ICNUM.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'gender' FINAL_TAB-GENDER.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dob' FINAL_TAB-DOB.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ffname' FINAL_TAB-FFNAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'flname' FINAL_TAB-FLNAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'hnumb' FINAL_TAB-HNUMB.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'locality' FINAL_TAB-LOCALITY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'pin' FINAL_TAB-PIN.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'city' FINAL_TAB-CITY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'country' FINAL_TAB-COUNTRY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'teln' FINAL_TAB-TELN.

    PERFORM WRITE_FORM USING 'EMP' 'APPEND' 'BODY' 'EMPNAME'.

* Compute for sl. no. 1(b) of form 16
      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                             AND CNTR2 <= FINAL_TAB-CNTR2
                             AND EVCLS_SPEC NE 17 .
        VAL_F12BA = VAL_F12BA + FORM12BA_TAB-TAX_PERK .
      ENDLOOP.

*   Value of perq u/s 17(2) from Prev Emp added.
      VAL_F12BA = VAL_F12BA + FINAL_TAB-PETD_S172.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'VAL_F12BA' VAL_F12BA.

* Compute for sl. no. 1(c) of form 16
      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                             AND CNTR2 <= FINAL_TAB-CNTR2
                             AND EVCLS_SPEC = 17 .
        PROF_F12BA = PROF_F12BA + FORM12BA_TAB-TAX_PERK.
      ENDLOOP.

*   Value of Profits u/s 17(3) from Prev Emp added.
      PROF_F12BA = PROF_F12BA + FINAL_TAB-PETD_S173.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'PROF_F12BA' PROF_F12BA.

* Compute for sl. no. 1(a) of form 16
      SAL_SEC17 = FINAL_TAB-GROSS_SAL - ( PROF_F12BA + VAL_F12BA ).
      PERFORM CONVERT_TO_SCRIPTVAR USING 'SAL_SEC17' SAL_SEC17.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'gross_sal' FINAL_TAB-GROSS_SAL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec10_all' FINAL_TAB-SEC10_ALL.
      PERFORM WRITE_FORM USING 'DETAILS1' 'APPEND' 'BODY' 'MAIN'.
      PERFORM WRITE_FORM USING 'DETAILS2' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2.
        IF SEC10_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear SEC10_TAB_AMOUNT.
       PERFORM sign_algmnt USING SEC10_TAB-AMOUNT
                            CHANGING SEC10_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'SEC10_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'balance' FINAL_TAB-BALANCE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'std_ded' FINAL_TAB-STD_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'etall' ENT_ALL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ptax' FINAL_TAB-PTAX.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'aggr_ded' FINAL_TAB-AGGR_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'salaries' FINAL_TAB-SALARIES.

    clear oth_income_s.                                    "PKT1148133
    PERFORM sign_algmnt USING final_tab-oth_income
                            CHANGING oth_income_s.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'oth_income'
                                        OTH_INCOME_S.
    PERFORM CONVERT_TO_SCRIPTVAR USING  'DEDN_S24' FINAL_TAB-DEDN_S24.
    PERFORM CONVERT_TO_SCRIPTVAR USING  'BUS_PROF' FINAL_TAB-BUS_PROF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'bus_prof' FINAL_TAB-BUS_PROF.
     PERFORM WRITE_FORM USING 'DETAILS13' 'APPEND' 'BODY' 'MAIN'.

      LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        IF IFOS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear IFOS_TAB_AMOUNT.
       PERFORM sign_algmnt USING IFOS_TAB-AMOUNT
                            CHANGING IFOS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' IFOS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'IFOS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'gross_tot_income'
                                FINAL_TAB-GROSS_TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec80_ded' FINAL_TAB-SEC80_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_income'
                                FINAL_TAB-TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_tot_income'
                           FINAL_TAB-TAX_TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'epf' FINAL_TAB-EPF_TOT.
      READ TABLE INT_S88 WITH KEY PERNR = FINAL_TAB-PERNR
                           CNTR2 = FINAL_TAB-CNTR2.
       IF SY-SUBRC = 0.
         clear : conmt_epf, dedmt_epf.
       ELSE.
         conmt_epf = FINAL_TAB-EPF_TOT.
         read table int_s80 with key pernr = final_tab-pernr
                                     sbsec = '15' sbdiv = '01'.
         if sy-subrc eq 0.
            dedmt_epf = int_s80-dedmt.
         endif.
       ENDIF.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt_epf' conmt_epf.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt_epf' dedmt_epf.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88_ded' FINAL_TAB-SEC88_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88b_ded' FINAL_TAB-SEC88B_DED
            .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88c_ded' FINAL_TAB-SEC88C_DED
            .
* MKINT811821
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88d_ded' FINAL_TAB-SEC88D_DED
            .
* RK1054554
    IF FORM_16 IS INITIAL.
       FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF = FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF
                                              - FINAL_TAB-EDU_CESS.
    ENDIF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_bef_relief' FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'chapVI_ded'
                               FINAL_TAB-CHAPVI_DED.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'surch_amt'
                            FINAL_TAB-SURCHG.            "MDSNT927906

      PERFORM CONVERT_TO_SCRIPTVAR USING 'educ_cess'
                              FINAL_TAB-EDU_CESS.


      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec89_relief'
                                FINAL_TAB-SEC89_RELIEF.

      PERFORM WRITE_FORM USING 'DETAILS14' 'APPEND' 'BODY' 'MAIN'.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_payable'
                              FINAL_TAB-TAX_PAYABLE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_deducted'
                             FINAL_TAB-TAX_DEDUCTED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'employer_tax'
                             FINAL_TAB-TAX_PAID_EMPLOYER.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_tax_deducted'
                             FINAL_TAB-TOT_TAX_DEDUCTED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_employer'
                             FINAL_TAB-TAX_PAID_EMPLOYER.

   clear net_tax_payable_s.                          "PKT1148133
   PERFORM sign_algmnt USING FINAL_TAB-NET_TAX_PAYABLE
                           CHANGING net_tax_payable_s.

   PERFORM CONVERT_TO_SCRIPTVAR USING 'net_tax_payable'
                                          net_tax_payable_s.


      TOTAL_TAX = FINAL_TAB-TAX_DEDUCTED + FINAL_TAB-TAX_PAID_EMPLOYER."MDSNT927906

      PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_TAX' TOTAL_TAX.

      CLEAR I.
      CLEAR S80_TOTAL.
      CLEAR CONT_AMT.
      CLEAR DED_AMT.
      CLEAR CNTR.
      CLEAR SEQ.
      CNTR = 0.
      LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
        SEQ = SEQ + 1.
      ENDLOOP.
      I = 1.
      CNTR = 1.
      LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                            CNTR2 = FINAL_TAB-CNTR2.
        I = I + 1.
        IF INT_S88-INAMT IS INITIAL.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'invmt' INT_S88-INVMT.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'invmt' INT_S88-INAMT.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'itlmt' INT_S88-ITLMT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S88-QLAMT.
        IF CNTR NE SEQ.
          CONT_AMT = ''.
          DED_AMT  = ''.
        ELSE.
          READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                        SBSEC = '15' SBDIV = '01' CNTR2 = INT_S88-CNTR2.
          IF SY-SUBRC EQ 0.
            CONT_AMT  = INT_S80-COAMT.
            CONDENSE CONT_AMT NO-GAPS.
            DED_AMT = INT_S80-DEDMT.
            CONDENSE DED_AMT NO-GAPS.
          ENDIF.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' CONT_AMT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' DED_AMT.
        PERFORM WRITE_FORM USING 'SEC88_DED' 'APPEND' 'BODY' 'MAIN'.
        CNTR = CNTR + 1.
      ENDLOOP.

      CLEAR INT_S80.
      READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                  CNTR2 = FINAL_TAB-CNTR2
                                  SBSEC = '01' SBDIV = '01'.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
      S80_TOTAL = S80_TOTAL + INT_S80-COAMT.

      PERFORM WRITE_FORM USING 'DETAILS10' 'APPEND' 'BODY' 'MAIN'.
*   s80ccd_amt = s88_total + s80_total.
      PERFORM CONVERT_TO_SCRIPTVAR USING 's80ccd' S80CCD_AMT.
      PERFORM WRITE_FORM USING 'DETAILS11' 'APPEND' 'BODY' 'MAIN'.
      J = 0.
*
      IF SNAME = '40TAX016'.                             "RBSNT927906
        LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                      CNTR2 = FINAL_TAB-CNTR2 AND
                                      SBSEC <> '01' AND SBSEC <> '15'.
          CLEAR TEMP.
          CLEAR TEMP1.
          TEMP = ALPHA+J(1).
          J = J + 1.
          TEMP1 = '('.
          MOVE TEMP TO TEMP1+1(1).
          MOVE ')' TO TEMP1+2(1).
          CONDENSE TEMP1 NO-GAPS.
          MOVE TEMP1 TO INT_S80-SBTDS+0(3).
          PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S80-QLAMT.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
          PERFORM WRITE_FORM USING 'SEC80_DED' 'APPEND' 'BODY' 'MAIN'.
        ENDLOOP.
      ELSE.
        IF SNAME = '40TAX16A'.
          LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                        CNTR2 = FINAL_TAB-CNTR2.
            CLEAR TEMP.
            CLEAR TEMP1.                                                  "MDSNT927906
            TEMP = ALPHA+J(1).
            J = J + 1.
            TEMP1 = '('.
            MOVE TEMP TO TEMP1+1(1).
            MOVE ')' TO TEMP1+2(1).
            CONDENSE TEMP1 NO-GAPS.
            MOVE TEMP1 TO INT_S80-SBTDS+0(3).
            PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
            PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S80-QLAMT.
            PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
            PERFORM WRITE_FORM USING 'SEC80_DED' 'APPEND' 'BODY' 'MAIN'.
          ENDLOOP.
        ENDIF.
      ENDIF.

      PERFORM WRITE_FORM USING 'DETAILS3' 'APPEND' 'BODY' 'MAIN'.

*      CLEAR i.
*      i = 1.
*      LOOP AT int_s88 WHERE pernr = final_tab-pernr AND
*                            cntr2 = final_tab-cntr2.
*        i = i + 1.
*        PERFORM convert_to_scriptvar USING 'invmt' int_s88-invmt.
*        PERFORM convert_to_scriptvar USING 'qlamt' int_s88-qlamt.
*        PERFORM write_form USING 'SEC88_DED' 'APPEND' 'BODY' 'MAIN'.
*      ENDLOOP.

      CLEAR TEMP.
      CLEAR TEMP1.
      I = 0.
      TEMP = ALPHA+I(1).
      TEMP1 = '('.
      MOVE TEMP TO TEMP1+1(1).
      MOVE ')' TO TEMP1+2(1).
      CONDENSE TEMP1 NO-GAPS.
      MOVE 'TOTAL' TO TEMP1+4.
      TEMP = ALPHA+0(1).
      MOVE '[(' TO TEMP1+10.
      MOVE TEMP TO TEMP1+12.
      MOVE ') to (' TO TEMP1+13.
      IF I = 0.
        I = 1.
      ENDIF.
      I = I - 1.
      TEMP = ALPHA+I(1).
      MOVE TEMP TO TEMP1+20.
      MOVE ')]' TO TEMP1+21.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_sec88' TEMP1.

      I = I + 1.
      MOVE '[I(' TO TEMP1.
      TEMP = ALPHA+I(1).
      MOVE TEMP TO TEMP1+3.
      CONDENSE TEMP1 NO-GAPS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'p14_text' TEMP1.

      PERFORM WRITE_FORM USING 'DETAILS4' 'APPEND' 'BODY' 'MAIN'.
      PERFORM WRITE_FORM USING 'DETAILS5' 'APPEND' 'BODY' 'MAIN'.

      TEMP_PAYMENTS[] = PAYMENTS[].

***          LOOP AT payments WHERE pernr = final_tab-pernr AND
***                                 cntr2 = final_tab-cntr2.
***            CASE  payments-fpper+4.
***              WHEN '01'.
***                IF NOT date1 IS INITIAL AND NOT bank1 IS INITIAL.
***                  payments-tdate = date1.
***                  payments-bname = bank1.
***                ENDIF.
***              WHEN '02'.
***                IF NOT date2 IS INITIAL AND NOT bank2 IS INITIAL.
***                  payments-tdate = date2.
***                  payments-bname = bank2.
***                ENDIF.
***              WHEN '03'.
***                IF NOT date3 IS INITIAL AND NOT bank3 IS INITIAL.
***                  payments-tdate = date3.
***                  payments-bname = bank3.
***                ENDIF.
***              WHEN '04'.
***                IF NOT date4 IS INITIAL AND NOT bank4 IS INITIAL.
***                  payments-tdate = date4.
***                  payments-bname = bank4.
***                ENDIF.
***              WHEN '05'.
***                IF NOT date5 IS INITIAL AND NOT bank5 IS INITIAL.
***                  payments-tdate = date5.
***                  payments-bname = bank5.
***                ENDIF.
***              WHEN '06'.
***                IF NOT date6 IS INITIAL AND NOT bank6 IS INITIAL.
***                  payments-tdate = date6.
***                  payments-bname = bank6.
***                ENDIF.
***              WHEN '07'.
***                IF NOT date7 IS INITIAL AND NOT bank7 IS INITIAL.
***                  payments-tdate = date7.
***                  payments-bname = bank7.
***                ENDIF.
***              WHEN '08'.
***                IF NOT date8 IS INITIAL AND NOT bank8 IS INITIAL.
***                  payments-tdate = date8.
***                  payments-bname = bank8.
***                ENDIF.
***              WHEN '09'.
***                IF NOT date9 IS INITIAL AND NOT bank9 IS INITIAL.
***                  payments-tdate = date9.
***                  payments-bname = bank9.
***                ENDIF.
***              WHEN '10'.
***                IF NOT date10 IS INITIAL AND NOT bank10 IS INITIAL.
***                  payments-tdate = date10.
***                  payments-bname = bank10.
***                ENDIF.
***              WHEN '11'.
***                IF NOT date11 IS INITIAL AND NOT bank11 IS INITIAL.
***                  payments-tdate = date11.
***                  payments-bname = bank11.
***                ENDIF.
***              WHEN '12'.
***                IF NOT date12 IS INITIAL AND NOT bank12 IS INITIAL.
***                  payments-tdate = date12.
***                  payments-bname = bank12.
***                ENDIF.
***              WHEN '00'.
**** Populating, for period for the offcycle runs.
***                LOOP AT temp_payments WHERE pernr = final_tab-pernr AND
***                cntr2 = final_tab-cntr2 AND seqnr > payments-seqnr.
***                  IF temp_payments-odate IS INITIAL.
***                    MOVE temp_payments-fpper TO payments-fpper.
***                    EXIT.
***                  ENDIF.
***                ENDLOOP.
***            ENDCASE.
***            MODIFY payments.
***          ENDLOOP.

***          SORT payments BY pernr seqnr.

*BADI to return the bank details and the tax paid date
*for more than 12 tax remittances.

      DATA  : CUST_EXIT TYPE REF TO IF_EX_HR_IN_BANK_TRANSFER.

      CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
        CHANGING
          INSTANCE = CUST_EXIT.


      CALL METHOD CUST_EXIT->GET_TAX_BANK_DETAILS
        EXPORTING
          FLT_VAL      = '40'
        CHANGING
          TAX_PAYMENTS = PAYMENTS[].

      SORT PAYMENTS BY SEQNR.
      TEMP_PAYMENTS[] = PAYMENTS[].

      CLEAR PR_EMP_TOTAL.
      CLEAR FPMON.
      CLEAR OUT.
      LOOP AT PAYMENTS WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        PR_EMP_TOTAL = PR_EMP_TOTAL + PAYMENTS-TAXPD.
        IF PAYMENTS-TAXPD = 0.
          CONTINUE.
        ENDIF.
* If for the offcycle run the tax date and bank is blank then
* the details are picked up from the regular payroll result, i.e from
* the period in which the offcycle was run.
        IF NOT PAYMENTS-ODATE IS INITIAL.
          IF PAYMENTS-TDATE IS INITIAL AND PAYMENTS-BNAME IS INITIAL.
*          IF FPMON <> PAYMENTS-FPPER+4(2).
*            CLEAR OF_TAX_AMT.
*            CLEAR FPMON.
*          ENDIF.
            LOOP AT TEMP_PAYMENTS WHERE PERNR = FINAL_TAB-PERNR AND
              CNTR2 = FINAL_TAB-CNTR2 AND
              SEQNR > PAYMENTS-SEQNR AND TAXPD > 0.
              OUT = 1.
              OF_TAX_AMT = OF_TAX_AMT + PAYMENTS-TAXPD.
              FPMON = PAYMENTS-FPPER+4(2).
              EXIT.
            ENDLOOP.
            IF OUT = 1.
              CLEAR OUT.
              CONTINUE.
            ENDIF.
          ELSE.
*          OF_TAX_AMT = 0.
*          CLEAR FPMON.
          ENDIF.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'pay_date' PAYMENTS-TDATE.
        BANKNAME = PAYMENTS-BNAME.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'bank' BANKNAME.
*      IF FPMON = PAYMENTS-FPPER+4(2) AND OF_TAX_AMT > 0.
        IF OF_TAX_AMT > 0.
          PAYMENTS-TAXPD = PAYMENTS-TAXPD + OF_TAX_AMT.
          CLEAR OF_TAX_AMT.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_paid' PAYMENTS-TAXPD.
        PERFORM WRITE_FORM USING 'DETAILS6' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'total' PR_EMP_TOTAL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tds_petd' FINAL_TAB-TDS_PETD.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tds_ifos' FINAL_TAB-TDS_IFOS.

      PERFORM CONVERT_INR_TO_WORDS USING PR_EMP_TOTAL
                                   CHANGING TAX_DEDUCTED_WRDS.
      REPLACE 'Rupees' WITH SPACE INTO TAX_DEDUCTED_WRDS.
      CONDENSE TAX_DEDUCTED_WRDS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'amt_in_words' TAX_DEDUCTED_WRDS.
      PERFORM WRITE_FORM USING 'DETAILS7' 'APPEND' 'BODY' 'MAIN'.

      SLNO = 1.

    IF FORM_16 = 'X'.
      COUNTER = 5.                                            "RBSNT927906
    ELSE.
      COUNTER = 4.
    ENDIF.

IF wa_t7insw-begda > pbegda OR wa_t7insw-reval IS INITIAL.
*      SORT TEMSEFIN_TAB BY DT_CHALLAN .                     "RK1054554
       SORT TEMSEFIN_TAB BY PAYDATE. "1274331" "GG"
      LOOP AT TEMSEFIN_TAB WHERE PERNR = FINAL_TAB-PERNR AND PAYDATE <= final_tab-f16_begda
                              AND PAYDATE >= final_tab-f16_endda.    "MDSNT927906

*        if temsefin_tab-incometax is initial."1274331gg"  "RK1054554
*          continue.
*        endif.

        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-208
                               SLNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TDS'
                               TEMSEFIN_TAB-INCOMETAX  .
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-207
                               TEMSEFIN_TAB-SURCHARGE.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-211
                               TEMSEFIN_TAB-EDUCESS.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TotalTax'
                               TEMSEFIN_TAB-BETRG.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-210
                               TEMSEFIN_TAB-CHKNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'BSR'
                               TEMSEFIN_TAB-BRCOD.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'c_date'
                               TEMSEFIN_TAB-CHDAT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'voucher'
                               TEMSEFIN_TAB-CHLNO.
        PERFORM WRITE_FORM USING 'DETAILS8' 'APPEND' 'BODY' 'MAIN'.
        SLNO = SLNO + 1.
      ENDLOOP.                                                "MDSNT927906
      PERFORM WRITE_FORM USING 'DETAILS9' 'APPEND' 'BODY' 'MAIN'.
      PERFORM END_FORM.
* End of First Page
ELSE.
*      SORT TEMSEFIN_TAB BY DT_CHALLAN .                     "RK1054554
       SORT TEMSEFIN_TAB BY PAYDATE. "1274331" "GG"
      LOOP AT TEMSEFIN_TAB WHERE PERNR = FINAL_TAB-PERNR
                            AND FPBEG <= FINAL_TAB-F16_BEGDA
                            AND FPEND >= FINAL_TAB-F16_ENDDA.    "MDSNT927906

*        if temsefin_tab-incometax is initial."1274331gg"  "RK1054554
*          continue.
*        endif.

        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-208
                               SLNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TDS'
                               TEMSEFIN_TAB-INCOMETAX  .
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-207
                               TEMSEFIN_TAB-SURCHARGE.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-211
                               TEMSEFIN_TAB-EDUCESS.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TotalTax'
                               TEMSEFIN_TAB-BETRG.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-210
                               TEMSEFIN_TAB-CHKNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'BSR'
                               TEMSEFIN_TAB-BRCOD.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'c_date'
                               TEMSEFIN_TAB-CHDAT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'voucher'
                               TEMSEFIN_TAB-CHLNO.
        PERFORM WRITE_FORM USING 'DETAILS8' 'APPEND' 'BODY' 'MAIN'.
        SLNO = SLNO + 1.
      ENDLOOP.                                                "MDSNT927906
      PERFORM WRITE_FORM USING 'DETAILS9' 'APPEND' 'BODY' 'MAIN'.
      PERFORM END_FORM.

ENDIF.
* Annexure
      IF DISP_FLG_LOT LT 1 AND LAYOUT1 NE ' '.
        FORMNAME1 = LAYOUT1.
      ELSE.
        FORMNAME1 = 'HR_IN_TAXF16NX_Y'.
      ENDIF.
      LANG = 'EN'.

      PERFORM START_FORM USING FORMNAME1 LANG 'PAGE1'.

      PERFORM WRITE_FORM USING 'HEADER' 'APPEND' 'BODY' 'MAIN'.

* Gross components
      PERFORM WRITE_FORM USING 'GROSS_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT GROSS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2    .
        IF GROSS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear GROSS_TAB_AMOUNT.
       PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
       PERFORM sign_algmnt USING GROSS_TAB-AMOUNT
                            CHANGING GROSS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' GROSS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'GROSS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.

* Perk components
      PERFORM WRITE_FORM USING 'PERK_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT PERK_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        IF PERK_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear PERK_TAB_AMOUNT.
       PERFORM sign_algmnt USING PERK_TAB-AMOUNT
                            CHANGING PERK_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' PERK_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'PERK_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.

      PERFORM WRITE_FORM USING 'GROSS_TOTAL' 'APPEND' 'BODY' 'MAIN'.

*  IFOS components
      PERFORM WRITE_FORM USING 'IFOS_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        IF IFOS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear IFOS_TAB_AMOUNT.
       PERFORM sign_algmnt USING IFOS_TAB-AMOUNT
                            CHANGING IFOS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' IFOS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'IFOS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.
      PERFORM WRITE_FORM USING 'IFOS_TOTAL' 'APPEND' 'BODY' 'MAIN'.


* Section 10 components
      PERFORM WRITE_FORM USING 'SEC10_HEADER' 'APPEND' 'BODY' 'MAIN'.

      LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2.
        IF SEC10_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
       clear SEC10_TAB_AMOUNT.
       PERFORM sign_algmnt USING SEC10_TAB-AMOUNT
                            CHANGING SEC10_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'SEC10_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.
      PERFORM WRITE_FORM USING 'SEC10_TOTAL' 'APPEND' 'BODY' 'MAIN'.

      PERFORM WRITE_FORM USING 'FOOTER' 'APPEND' 'BOTTOM' 'MAIN'.

      PERFORM END_FORM.

* End of Annexure

* End of Second Page

* Second Annexure For Form 12BA

* FORM 12BA IS TO BE PRINTED ONLY IF SALARY PAID OR SALARY PAYABLE IS
* GREATER THAN THE AMOUNT IN THE CONSTANT F12BA IN T511P

*    SELECT SINGLE  * FROM  T511P
*                WHERE MOLGA  = '40'
*                AND   KONST  = 'F12BA'
*                AND BEGDA LE PBEGDA
*                AND ENDDA GE PENDDA.
*    IF FINAL_TAB-GROSS_SAL GT T511P-BETRG.
*    CLEAR TOT_VAL_PERK.
*    CLEAR TOT_EERECVR.
*    CLEAR TOT_TAX_PERK.

      IF DISP_FLG_LOT LT 1 AND LAYOUT2 NE ' '.
        FORMNAME2 = LAYOUT2.
      ELSE.
        FORMNAME2 = 'HR_IN_TAXF16NX_P'.
      ENDIF.
      LANG = 'EN'.

      PERFORM START_FORM USING FORMNAME2 LANG 'PAGE1'.

      PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'HDR'.
      PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'FLEFT'.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fin_start' FIN_START.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fin_end' FIN_END.

*--- Compute for sl. no.6 of form 12BA------------------*
      F12BA_BALANCE = SAL_SEC17 + PROF_F12BA - FINAL_TAB-SEC10_ALL.
      SNAME = '40INDED'.
      ENDDATE = PENDDA.
      PERFORM RE596F USING SNAME ENDDATE.
      PERFORM (T596F-MODNA) IN PROGRAM (T596F-PGMNA)
                           USING F12BA_BALANCE
                                 ENDDATE
                           CHANGING F12BA_STD_DED.
      SALARIES_WO_PERK = F12BA_BALANCE - ( F12BA_STD_DED + ENT_ALL +
                                           FINAL_TAB-PTAX ).

      PERFORM CONVERT_TO_SCRIPTVAR USING 'salaries_wo_perk'
   SALARIES_WO_PERK.

*---------------------------------------------------------*

*  FILLING OF THE MAIN WINDOW.*****************

      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                                 CNTR2 = FINAL_TAB-CNTR2.

***************************************************
* FORM THE VARIABLE.
        VAL_PERK = 'VAL_PERK'.
        CONCATENATE VAL_PERK FORM12BA_TAB-EVCLS_SPEC INTO VAL_PERK.

  clear : VAL_PERK_S, EERECVR_S, TAX_PERK_S.            "PKT1148133

     PERFORM sign_algmnt USING FORM12BA_TAB-VAL_PERK
                            CHANGING VAL_PERK_S.
     PERFORM CONVERT_TO_SCRIPTVAR USING VAL_PERK
                               VAL_PERK_S.

***************************************************
* FORM THE VARIABLE.

        EERECVR = 'EERECVR'.
        CONCATENATE EERECVR FORM12BA_TAB-EVCLS_SPEC INTO EERECVR.

     PERFORM sign_algmnt USING FORM12BA_TAB-EERECVR
                            CHANGING EERECVR_S.
     PERFORM CONVERT_TO_SCRIPTVAR USING EERECVR
                               EERECVR_S.

***************************************************
* FORM THE VARIABLE.

        TAX_PERK = 'TAX_PERK'.
        CONCATENATE TAX_PERK FORM12BA_TAB-EVCLS_SPEC INTO TAX_PERK.

     PERFORM sign_algmnt USING FORM12BA_TAB-TAX_PERK
                            CHANGING TAX_PERK_S.
     PERFORM CONVERT_TO_SCRIPTVAR USING TAX_PERK
                                   TAX_PERK_S.

***************************************************

        IF FORM12BA_TAB-EVCLS_SPEC NE 17.

          TOT_VAL_PERK = TOT_VAL_PERK + FORM12BA_TAB-VAL_PERK.
          TOT_EERECVR = TOT_EERECVR + FORM12BA_TAB-EERECVR.
          TOT_TAX_PERK = TOT_TAX_PERK + FORM12BA_TAB-TAX_PERK.

   clear : TOT_VAL_PERK_S, TOT_EERECVR_S, TOT_TAX_PERK_S.  "PKT1148133

          PERFORM sign_algmnt USING TOT_VAL_PERK
                              CHANGING TOT_VAL_PERK_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_VAL_PERK'
                                         TOT_VAL_PERK_S.

          PERFORM sign_algmnt USING TOT_EERECVR
                              CHANGING TOT_EERECVR_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_EERECVR'
                                         TOT_EERECVR_S.

          PERFORM sign_algmnt USING TOT_TAX_PERK
                              CHANGING TOT_TAX_PERK_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_TAX_PERK'
                                         TOT_TAX_PERK_S.
        ENDIF.

        PERFORM WRITE_FORM USING 'FORM12BA_COMPONENTS' 'APPEND' 'BODY'
                                                   'DETAILS'.

      ENDLOOP.
*  FILLING OF THE MAIN WINDOW.*****************

*      IF FORMNAME2 = LAYOUT2.
*        PERFORM END_FORM.
*
*        PERFORM OPEN_FORM USING LANG.
*
*        PERFORM START_FORM USING FORMNAME2 LANG 'PAGE2'.
*
*        PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'FLEFT1'.
*        PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'DETAILS1'.
*        PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'FOOTER'.
*      ENDIF.

      PERFORM END_FORM.

*    ENDIF.        "FINAL_TAB-GROSS_SAL GT T511P-BETRG

    ENDLOOP.        "FINAL_TAB

    PERFORM CLOSE_FORM.
********START OF PDF CODING*******
  ELSE.
    PERFORM PRINT_MODULE_PDF.
  ENDIF.
*******END OF PDF CODING**********

ENDFORM.                    "PRINT_MODULE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
*       text
FORM DISPLAY_LIST.

  DATA: BEGIN OF FLD_NAM OCCURS 8,
                FIELD1(20),
                FIELD2(6),
                FIELD3(9),
          END OF FLD_NAM.

  DATA: REPID LIKE SY-REPID.

  LOOP AT HD_TAB.
    MOVE-CORRESPONDING HD_TAB TO HD_TAB1.
    APPEND HD_TAB1.
  ENDLOOP.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = 'HINCF160'
      I_INTERNAL_TABNAME     = 'HD_TAB1'
      I_INCLNAME             = 'PCF16IN2'
    CHANGING
      CT_FIELDCAT            = FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.


  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'F16_BEGDA'.
  FIELDCAT-FIELDNAME = 'F16_BEGDA'.
  FIELDCAT-SELTEXT_M = 'From Date'(037).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.


  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'F16_ENDDA'.
  FIELDCAT-FIELDNAME = 'F16_ENDDA'.
  FIELDCAT-SELTEXT_M = 'End Date'(038).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'PERNR'.
  FIELDCAT-FIELDNAME = 'PERNR'.
  FIELDCAT-SELTEXT_M = 'Employee No.'(041).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 13.
  MODIFY FIELDCAT INDEX SY-TABIX.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'ENAME'.
  FIELDCAT-FIELDNAME = 'ENAME'.
  FIELDCAT-SELTEXT_M = 'Employee Name'(044).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 14.
  MODIFY FIELDCAT INDEX SY-TABIX.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'POSITION'.
  FIELDCAT-FIELDNAME = 'POSITION'.
  FIELDCAT-SELTEXT_M = 'Employee Designation'(061).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 20.
  MODIFY FIELDCAT INDEX SY-TABIX.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'ICNUM'.
  FIELDCAT-FIELDNAME = 'ICNUM'.
  FIELDCAT-SELTEXT_M = 'PAN Number'(045).
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.

*  read table fieldcat with key fieldname = 'GENDER'.
*  fieldcat-seltext_m = 'Gender'(202).
*  fieldcat-ddictxt = 'M'.
*  fieldcat-outputlen = 10.
*  modify fieldcat index sy-tabix.

  REPID = SY-REPID.
  REFRESH G_ITAB_FCODE1.
  MOVE 'AMBC' TO G_ITAB_FCODE1-FCODE.
  APPEND G_ITAB_FCODE1.
  MOVE 'CORC' TO G_ITAB_FCODE1-FCODE.
  APPEND G_ITAB_FCODE1.

* BEGIN OF ESS CHANGES
  IF PNPESSCF = SPACE.
* END OF ESS CHANGES
    MESSAGE S192(HRPADIN01).
    PERFORM DISPLAY_ALV_GRID IN PROGRAM HINCALV0
                         TABLES HD_TAB1 FIELDCAT G_ITAB_FCODE1
                         USING REPID TEXT-015 TEXT-062.
    IF SY-BATCH = 'X'.
*   For output in SAP Script
      PERFORM ERROR_CASES.
      PERFORM PRINT_MODULE.
    ENDIF.

    CHECK SY-SUBRC = 0.

* BEGIN OF ESS CHANGES
  ELSE.
    PERFORM PRINT_MODULE.
  ENDIF.
* END OF ESS CHANGES

ENDFORM.                               " DISPLAY_LIST


*&---------------------------------------------------------------------*
*&      Form  READ_NAME
*&---------------------------------------------------------------------*
*       Gets the Employee Name
*----------------------------------------------------------------------*
*      -->$LANGU    Language                                           *
*      <--$EMP_NAM  Employee Name                                      *
*----------------------------------------------------------------------*
FORM READ_NAME USING $LANGU CHANGING $EMP_NAM.

  CALL FUNCTION 'RP_EDIT_NAME'
       EXPORTING
           FORMAT    = '01'
            LANGU     = $LANGU
            MOLGA     = '40'
          PP0002     =  P0002
        IMPORTING
            EDIT_NAME = $EMP_NAM
*         RETCODE   =
       EXCEPTIONS
            OTHERS    = 1.

ENDFORM.                               " READ_NAME
*&---------------------------------------------------------------------*
*&      Form  PRINT_USING_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_USING_WRITE.
  CONSTANTS : ZX TYPE I VALUE 80,
              A  TYPE I VALUE 1.

  DATA: AX TYPE I VALUE 10,
        BX TYPE I VALUE 10,
        CX TYPE I,
        DX TYPE I,
        LX TYPE I,
        MX TYPE I,
        NX TYPE I,
        LENGTH TYPE I.

  DATA: IT_T7INI2 TYPE HASHED TABLE OF T7INI2 WITH UNIQUE KEY ICODE,
        WA_T7INI2 TYPE T7INI2 .

  DATA: LANG LIKE SY-LANGU,
        PAGE_NO TYPE I,
        TAX_PERK(10),
        EERECVR(9),
        VAL_PERK(10),
        VAL_F12BA LIKE PC207-BETRG,
        PROF_F12BA LIKE PC207-BETRG,
        SAL_SEC17 LIKE PC207-BETRG,
        TOT_VAL_PERK LIKE PC207-BETRG,
        TOT_EERECVR LIKE PC207-BETRG,
        OUT TYPE I,
        TOT_TAX_PERK LIKE PC207-BETRG.

  DATA: BEGIN OF TEMP_PAYMENTS OCCURS 10.
          INCLUDE STRUCTURE PINBK.
  DATA: END OF TEMP_PAYMENTS.

  TABLES: T52DB.
*  DATA: sadr1 like sadr occurs 10 with header line,
*  STATE1 LIKE T005U-BEZEI.
  DATA: NAME1 LIKE ADDR1_VAL-NAME1, SIGN TYPE C.  "MKINT910704

  SELECT * FROM T7INI2 INTO TABLE IT_T7INI2.


  LOOP AT FINAL_TAB.
************************************************************************
* CLEARING VARIABLES OF FORM12BA
    CLEAR VAL_F12BA.CLEAR PROF_F12BA.CLEAR SAL_SEC17.CLEAR TOT_VAL_PERK.
    CLEAR TOT_EERECVR.CLEAR TOT_TAX_PERK.
************************************************************************
    MX = ZX / 2.
    LX = MX - 13.
    NX = MX + 13.
    BX = MX + 18.
    AX = 6.

    FORMAT INTENSIFIED ON.
    WRITE / .
    WRITE : AT 35 'FORM NO. 16'  CENTERED COLOR COL_HEADING,/.
    WRITE : AT LX '   [See rule 31(1)(a) ]'(078) CENTERED.
    IF ORG = 'X'.
      WRITE 70 '"ORIGINAL"'.
    ELSE.
      WRITE 66 '"NON-ORIGINAL"'.
    ENDIF.
    SKIP 1.
    WRITE: /
    'Certificate U/S 203 of the Income-tax Act, 1961 for tax'(079),
          'deducted at source from',
          / 'income chargeable under the head "Salaries"'(080).
    SKIP.
*    FORMAT RESET.
    CLEAR STATE.
*    PERFORM ADDRESS USING COMP_CD.    "MKINT910704
    PERFORM ER_ADDRESS USING FINAL_TAB-TANNO.                "MKINT910704
*    SADR1 = SADR.
    IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
      PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
    ENDIF.

    WRITE AT A(ZX) SY-ULINE.
    WRITE: / SY-VLINE, AT AX 'Name & address of the employer'(081),
             AT MX SY-VLINE,
             ' Name & designation of the Employee'(082),
             AT ZX SY-VLINE.

    AX = AX + 2.

    NAME1 = ADDR1_VAL-NAME1.
    WRITE: / SY-VLINE,
             AT AX ADDR1_VAL-NAME1,          "MKINT910704
             AT MX SY-VLINE ,
             ' Mr/Ms:'(083) , (30) FINAL_TAB-ENAME,AT ZX
             SY-VLINE.

    WRITE: / SY-VLINE,
             AT AX SADR-STRAS,
             AT MX SY-VLINE ,' Desig.:'(084) ,
             FINAL_TAB-POSITION, AT ZX SY-VLINE.

    LENGTH = STRLEN( SADR-ORT01 ).
    WRITE: / SY-VLINE,
             AT AX(LENGTH) SADR-ORT01, SADR-PSTLZ+0(6),
             AT MX SY-VLINE , ' Emp  #:'(085),
             FINAL_TAB-PERNR, AT ZX SY-VLINE.

    LENGTH = STRLEN( STATE ).
    WRITE: / SY-VLINE,AT AX(LENGTH) STATE,
             AT MX SY-VLINE ,
             AT ZX SY-VLINE.

    WRITE: / '' , AT A SY-VLINE,
             AT MX SY-VLINE,
             AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT A(ZX) SY-ULINE,AT MX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
            AT 2(LX) 'PAN/GIR NO.' CENTERED COLOR COL_HEADING ,
            AT 28(12) 'TAN' CENTERED COLOR COL_HEADING ,
            AT MX(ZX) 'PAN/GIR NO.' CENTERED COLOR COL_HEADING ,
            AT LX SY-VLINE, AT MX SY-VLINE , AT ZX  SY-VLINE.

*   WRITE: / sy-vline, AT a(zx) sy-uline,AT mx sy-vline, AT zx sy-vline.

    CLEAR SADR.
    CLEAR STATE.
    PERFORM GET_ADDRESS USING FINAL_TAB-ADDRS.
    IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL
         ).
      PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                   CHANGING STATE.
    ENDIF.

    LENGTH = STRLEN( FINAL_TAB-PANNO ).
    WRITE:/ SY-VLINE, AT AX(LENGTH) FINAL_TAB-PANNO,'/',FINAL_TAB-GIRNO,
            AT LX SY-VLINE,
            FINAL_TAB-TANNO, AT MX SY-VLINE , AT BX FINAL_TAB-ICNUM ,
            AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT A(ZX) SY-ULINE,AT MX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,(25) 'TDS Circle where Annual'(086) COLOR COL_HEADING,
               AT 28(25) 'Period'(087) CENTERED COLOR COL_HEADING,
               AT NX(LX) 'Assessment Year'(088) CENTERED COLOR COL_HEADING,
               AT LX SY-VLINE, AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, (25) 'Ret/Stat under Sec 206'(089) COLOR
COL_HEADING,
           AT 28(25) '' CENTERED COLOR COL_HEADING,
           AT NX(LX) '' CENTERED COLOR COL_HEADING,
           AT LX SY-VLINE,  AT NX SY-VLINE,
           AT ZX SY-VLINE.

    WRITE: / SY-VLINE, (25) 'is to be filed' COLOR COL_HEADING,
           AT 28(25) '' CENTERED COLOR COL_HEADING,
           AT NX(LX) '' CENTERED COLOR COL_HEADING,
           AT LX SY-VLINE, AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT A(ZX) SY-ULINE,AT LX SY-VLINE, AT NX SY-VLINE,
           AT ZX SY-VLINE.

    WRITE: / SY-VLINE,AT AX ADDR1_VAL-NAME1,        "MKINT910704
            AT LX SY-VLINE, '   From'(091),
            AT MX SY-VLINE, '   To'(090),AT NX SY-VLINE,
            AT BX PENDDA(4), '-', ASSM_END,
            AT ZX SY-VLINE.

    WRITE: / SY-VLINE,AT AX SADR-STRAS,
             AT LX SY-VLINE,
             PBEGDA, AT MX SY-VLINE, PENDDA,AT NX SY-VLINE,
             AT ZX SY-VLINE.

    LENGTH = STRLEN( SADR-ORT01 ).
    WRITE: / SY-VLINE, AT AX(LENGTH) SADR-ORT01, SADR-PSTLZ+0(6),
            AT LX SY-VLINE,
            AT MX SY-VLINE,AT NX SY-VLINE, AT ZX SY-VLINE.

    LENGTH = STRLEN( STATE ).
    WRITE: / SY-VLINE,AT AX(LENGTH) STATE,
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT A(ZX) SY-ULINE,AT LX SY-VLINE, AT NX SY-VLINE,
           AT ZX SY-VLINE.

    WRITE : /, AT A(ZX)
    'DETAILS OF SALARY PAID AND ANY OTHER INCOME AND TAX DEDUCTED'
     CENTERED COLOR COL_HEADING, / .

    NX = ZX - 16.
    MX = NX - 16.
    LX = MX - 16.

    WRITE  AT A(ZX) SY-ULINE.

    WRITE : / SY-VLINE,
            AT 2(LX) 'Particulars'(092) CENTERED COLOR COL_HEADING ,
            AT LX(16)'Rs.' CENTERED COLOR COL_HEADING ,
            AT MX(16)'Rs.' CENTERED COLOR COL_HEADING ,
            AT NX(16)'Rs.' CENTERED COLOR COL_HEADING ,
            AT LX SY-VLINE,AT MX SY-VLINE,AT NX SY-VLINE,
            AT ZX SY-VLINE .

    WRITE : / SY-VLINE, AT LX SY-VLINE,
              AT MX SY-VLINE, AT NX SY-VLINE,
              AT ZX SY-VLINE.

    WRITE  AT A(ZX) SY-ULINE.


*   Positions to print amounts

    BX = LX + 11.
    AX = BX - 16.
    CX = BX + 15.
    DX = LX - 16.


    WRITE: / SY-VLINE, '1.Gross salary'(093),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.

    LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                           AND CNTR2 = FINAL_TAB-CNTR2
                           AND EVCLS_SPEC NE 17 .
      VAL_F12BA = VAL_F12BA + FORM12BA_TAB-TAX_PERK .
    ENDLOOP.


    READ TABLE FORM12BA_TAB WITH KEY PERNR = FINAL_TAB-PERNR
                                     EVCLS_SPEC = 17
                                     CNTR2 = FINAL_TAB-CNTR2.
    IF SY-SUBRC = 0.
      PROF_F12BA = FORM12BA_TAB-TAX_PERK.
    ENDIF.

    SAL_SEC17 = FINAL_TAB-GROSS_SAL - ( PROF_F12BA + VAL_F12BA ).

    WRITE: / SY-VLINE, '  a) Salary as per provisions'(094),
                 AT BX SAL_SEC17 CURRENCY ' ' ,
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  contained in sec.17(1)'(095),
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  b) Value of perq u/s 17(2)'(096),
                 AT BX VAL_F12BA CURRENCY ' ' ,
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  (as per Form 12BA wherever'(097),
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '   wherever applicable)'(098),
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  c) Profits in lieu of sal'(099),
                 AT BX PROF_F12BA CURRENCY ' ',
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  u/s 17(3)',
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  (as per Form 12BA wherever'(097),
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  applicable)'(100),
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '  d) Total'(101),
                 AT BX FINAL_TAB-GROSS_SAL CURRENCY ' ',
                 AT LX SY-VLINE, AT MX SY-VLINE,
                 AT NX SY-VLINE,
                 AT ZX SY-VLINE.


    WRITE: / SY-VLINE,
             '2.Less: Allowance to the'(102),
             AT BX FINAL_TAB-SEC10_ALL CURRENCY ' ',
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '  extent exempt under sec 10'(103),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.


    WRITE: / SY-VLINE, '3.Balance(1-2)'(104),
             AT BX FINAL_TAB-BALANCE CURRENCY ' ',
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.

    WRITE: / SY-VLINE, '4.Deductions'(105),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, '  (a)Standard Deduction'(106),
             AT AX FINAL_TAB-STD_DED CURRENCY ' ',
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.

    WRITE: / SY-VLINE, '  (b)Tax on Employment'(107),
             AT AX FINAL_TAB-PTAX CURRENCY ' ',
             AT LX SY-VLINE,AT MX SY-VLINE,
             AT NX SY-VLINE,
             AT ZX SY-VLINE.

    WRITE: / SY-VLINE, '5.Aggregate of 4(a+b)'(108),
             AT BX FINAL_TAB-AGGR_DED CURRENCY ' ',
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '6.Income chargeable under the'(109),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT CX FINAL_TAB-SALARIES CURRENCY ' ',
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '  head "SALARIES"(3-5)'(110),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '7.Add: Any other Income'(111),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT CX FINAL_TAB-OTH_INCOME CURRENCY ' ',
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '  reported by the employee'(112),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '8.Gross Total Income(6+7)'(113),
             AT LX SY-VLINE, AT MX SY-VLINE,
             AT CX FINAL_TAB-GROSS_TOT_INCOME CURRENCY ' ',
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             '9.Deductions under chap VI-A'(114),
             AT LX SY-VLINE,
             AT MX SY-VLINE,
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,
             AT DX '     Gross Amt'(115),
             AT LX SY-VLINE, 'Qualifying Amt'(116),
             AT MX SY-VLINE, 'Deductible Amt'(117),
             AT NX SY-VLINE, AT ZX SY-VLINE.

    WRITE : / SY-VLINE, AT LX SY-VLINE, AT 26 'Rs.',
            AT LX SY-VLINE, '     Rs.'(118), AT MX SY-VLINE,
            '     Rs.'(118),
            AT NX SY-VLINE, AT ZX SY-VLINE .

    AX = AX + 9.
    BX = BX + 9.
    DX = DX + 4.

    LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR
                     AND CNTR2 = FINAL_TAB-CNTR2.
      IF SY-LINNO = 64.
        PERFORM PAGE-END.
      ENDIF.
      LENGTH = STRLEN( INT_S80-SBTDS ) .
      WRITE: / SY-VLINE, AT 7(LENGTH) INT_S80-SBTDS NO-GAP,
               '(' NO-GAP,INT_S80-SBDIV NO-GAP, ')',
               AT DX INT_S80-COAMT CURRENCY ' ',
               AT AX INT_S80-QLAMT CURRENCY ' ',
               AT BX INT_S80-DEDMT CURRENCY ' ',
               AT LX SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE,
               AT ZX SY-VLINE.
    ENDLOOP.

    BX = LX + 13.
    AX = BX - 17.
    DX = LX - 16.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '10.Aggregate of deductible'(119),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-SEC80_DED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '   amounts under Chap VI-A'(120),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '11.Total Income(8-10)'(121),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-TOT_INCOME CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '12.Tax on total income'(122),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-TAX_TOT_INCOME CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '13.Rebate and relief under'(123),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '   Chapter VIII'(124),
            AT 22 'Gross Amt'(125),
            AT LX SY-VLINE, 'Qualifying Amt'(116),
            AT MX SY-VLINE, 'Deductible Amt'(117),
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, AT LX SY-VLINE, AT 26 'Rs.',
            AT LX SY-VLINE, '     Rs.'(118), AT MX SY-VLINE,
            '     Rs.'(118),
            AT NX SY-VLINE, AT ZX SY-VLINE .

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, '   Under Section 88'(126),AT LX SY-VLINE,
            AT MX SY-VLINE, AT NX SY-VLINE, AT ZX SY-VLINE .

    AX = AX + 9.
    BX = BX + 9.
    DX = DX + 4.

    LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR
                      AND CNTR2 = FINAL_TAB-CNTR2.
      IF SY-LINNO = 64.
        PERFORM PAGE-END.
      ENDIF.
      READ TABLE IT_T7INI2 INTO WA_T7INI2
      WITH TABLE KEY ICODE = INT_S88-ICODE.

      WRITE :/ SY-VLINE, AT 7 WA_T7INI2-ITDES, "AT 7 INT_S88-ICODE,
               AT DX INT_S88-INVMT CURRENCY ' ',
               AT DX INT_S88-INAMT CURRENCY ' ',
               AT AX INT_S88-QLAMT CURRENCY ' ',
               AT LX SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE,
               AT ZX SY-VLINE.
    ENDLOOP.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, '   Total Sec88 deductions'(127),
            AT LX SY-VLINE, AT MX SY-VLINE,
            AT CX FINAL_TAB-SEC88_DED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE .

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, '   Under Section 88B'(128),AT LX SY-VLINE,
            AT MX SY-VLINE, AT CX FINAL_TAB-SEC88B_DED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE .

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, '   Under Section 88C'(129),AT LX SY-VLINE,
            AT MX SY-VLINE, AT CX FINAL_TAB-SEC88C_DED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE .

    BX = LX + 13.
    AX = BX - 17.
    DX = LX - 16.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '14.Aggregate of tax rebates'(130),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-CHAPVI_DED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '   and relief at 13'(131),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '15.Tax payable(12-14) and '(132),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-TAX_PAYABLE CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '   surcharge thereon'(133),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT NX SY-VLINE, AT ZX SY-VLINE.


    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '16.Less Tax deduc at source'(134),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-TOT_TAX_DEDUCTED CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.

    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '17.Tax payable/refundable'(135),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT CX FINAL_TAB-NET_TAX_PAYABLE CURRENCY ' ',
            AT NX SY-VLINE, AT ZX SY-VLINE.


    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE: / SY-VLINE,
            '   (15-16)'(136),
            AT LX SY-VLINE,
            AT MX SY-VLINE,
            AT NX SY-VLINE, AT ZX SY-VLINE.


    IF SY-LINNO = 64.
      PERFORM PAGE-END.
    ENDIF.

    WRITE : / SY-VLINE, AT LX SY-VLINE,
              AT MX SY-VLINE, AT NX SY-VLINE,
              AT ZX SY-VLINE .


    WRITE AT A(ZX) SY-ULINE.

    WHILE SY-LINNO < 64.
      WRITE: / SY-VLINE, AT ZX SY-VLINE.
    ENDWHILE.
    WRITE AT A(ZX) SY-ULINE.
    NEW-PAGE.

    WRITE AT A(ZX) SY-ULINE.
    WRITE : / SY-VLINE, AT 2(79)
  'Details of tax deducted and deposited into Central Govt Account'(137)
    CENTERED COLOR COL_HEADING, AT ZX SY-VLINE .

    AX = 18.
    DX = 65.
    WRITE: / SY-VLINE, AT A(ZX) SY-ULINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE,'Date of Payment'(138) COLOR COL_HEADING,
             ' Name of the bank & branch where tax deposited'(139)
             COLOR COL_HEADING,
             AT AX SY-VLINE, AT DX SY-VLINE,
             AT 66(14) 'Amount'(140) CENTERED COLOR COL_HEADING,
             AT ZX SY-VLINE .

    WRITE : '|', AT ZX '|'.
    WRITE AT A(ZX) SY-ULINE.


    CLEAR PR_EMP_TOTAL.
*    LOOP AT PAYMENTS WHERE PERNR = FINAL_TAB-PERNR.
**                      AND CNTR2 = FINAL_TAB-CNTR2.
*      PR_EMP_TOTAL = PR_EMP_TOTAL + PAYMENTS-TAX_PAID.
*      CASE  PAYMENTS-FPPER+4.
*        WHEN '01'.  TAX_PAY_DATE = DATE1.  BANKNAME = BANK1.
*        WHEN '02'.  TAX_PAY_DATE = DATE2.  BANKNAME = BANK2.
*        WHEN '03'.  TAX_PAY_DATE = DATE3.  BANKNAME = BANK3.
*        WHEN '04'.  TAX_PAY_DATE = DATE4.  BANKNAME = BANK4.
*        WHEN '05'.  TAX_PAY_DATE = DATE5.  BANKNAME = BANK5.
*        WHEN '06'.  TAX_PAY_DATE = DATE6.  BANKNAME = BANK6.
*        WHEN '07'.  TAX_PAY_DATE = DATE7.  BANKNAME = BANK7.
*        WHEN '08'.  TAX_PAY_DATE = DATE8.  BANKNAME = BANK8.
*        WHEN '09'.  TAX_PAY_DATE = DATE9.  BANKNAME = BANK9.
*        WHEN '10'.  TAX_PAY_DATE = DATE10. BANKNAME = BANK10.
*        WHEN '11'.  TAX_PAY_DATE = DATE11. BANKNAME = BANK11.
*        WHEN '12'.  TAX_PAY_DATE = DATE12. BANKNAME = BANK12.
*      ENDCASE.
*      WRITE: / '|', TAX_PAY_DATE, AT ax sy-vline,  BANKNAME,
*                AT cx PAYMENTS-TAX_PAID ,
*                At dx sy-vline,
*                AT zx '|'.
*    ENDLOOP.

    TEMP_PAYMENTS[] = PAYMENTS[].

***    LOOP AT payments WHERE pernr = final_tab-pernr.
***      CASE  payments-fpper+4.
***        WHEN '01'.
***          IF NOT date1 IS INITIAL AND NOT bank1 IS INITIAL.
***            payments-tdate = date1.
***            payments-bname = bank1.
***          ENDIF.
***        WHEN '02'.
***          IF NOT date2 IS INITIAL AND NOT bank2 IS INITIAL.
***            payments-tdate = date2.
***            payments-bname = bank2.
***          ENDIF.
***        WHEN '03'.
***          IF NOT date3 IS INITIAL AND NOT bank3 IS INITIAL.
***            payments-tdate = date3.
***            payments-bname = bank3.
***          ENDIF.
***        WHEN '04'.
***          IF NOT date4 IS INITIAL AND NOT bank4 IS INITIAL.
***            payments-tdate = date4.
***            payments-bname = bank4.
***          ENDIF.
***        WHEN '05'.
***          IF NOT date5 IS INITIAL AND NOT bank5 IS INITIAL.
***            payments-tdate = date5.
***            payments-bname = bank5.
***          ENDIF.
***        WHEN '06'.
***          IF NOT date6 IS INITIAL AND NOT bank6 IS INITIAL.
***            payments-tdate = date6.
***            payments-bname = bank6.
***          ENDIF.
***        WHEN '07'.
***          IF NOT date7 IS INITIAL AND NOT bank7 IS INITIAL.
***            payments-tdate = date7.
***            payments-bname = bank7.
***          ENDIF.
***        WHEN '08'.
***          IF NOT date8 IS INITIAL AND NOT bank8 IS INITIAL.
***            payments-tdate = date8.
***            payments-bname = bank8.
***          ENDIF.
***        WHEN '09'.
***          IF NOT date9 IS INITIAL AND NOT bank9 IS INITIAL.
***            payments-tdate = date9.
***            payments-bname = bank9.
***          ENDIF.
***        WHEN '10'.
***          IF NOT date10 IS INITIAL AND NOT bank10 IS INITIAL.
***            payments-tdate = date10.
***            payments-bname = bank10.
***          ENDIF.
***        WHEN '11'.
***          IF NOT date11 IS INITIAL AND NOT bank11 IS INITIAL.
***            payments-tdate = date11.
***            payments-bname = bank11.
***          ENDIF.
***        WHEN '12'.
***          IF NOT date12 IS INITIAL AND NOT bank12 IS INITIAL.
***            payments-tdate = date12.
***            payments-bname = bank12.
***          ENDIF.
***        WHEN '00'.
**** Populating, for period for the offcycle runs.
***          LOOP AT temp_payments WHERE pernr = final_tab-pernr AND
***          seqnr > payments-seqnr.
***            IF temp_payments-odate IS INITIAL.
***              MOVE temp_payments-fpper TO payments-fpper.
***              EXIT.
***            ENDIF.
***          ENDLOOP.
***      ENDCASE.
***      MODIFY payments.
***    ENDLOOP.

    SORT PAYMENTS BY PERNR SEQNR.

*BADI to return the bank details and the tax paid date
*for more than 12 tax remittances.

    DATA  : CUST_EXIT TYPE REF TO IF_EX_HR_IN_BANK_TRANSFER.

    CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
      CHANGING
        INSTANCE = CUST_EXIT.


    CALL METHOD CUST_EXIT->GET_TAX_BANK_DETAILS
      EXPORTING
        FLT_VAL      = '40'
      CHANGING
        TAX_PAYMENTS = PAYMENTS[].

    SORT PAYMENTS BY SEQNR.
    TEMP_PAYMENTS[] = PAYMENTS[].

    CLEAR PR_EMP_TOTAL.
    CLEAR FPMON.
    CLEAR OUT.
    LOOP AT PAYMENTS WHERE PERNR = FINAL_TAB-PERNR.
      PR_EMP_TOTAL = PR_EMP_TOTAL + PAYMENTS-TAXPD.
      IF PAYMENTS-TAXPD = 0.
        CONTINUE.
      ENDIF.
* If for the offcycle run the tax date and bank is blank then
* the details are picked up from the regular payroll result, i.e from
* the period in which the offcycle was run.
      IF NOT PAYMENTS-ODATE IS INITIAL.
        IF PAYMENTS-TDATE IS INITIAL AND PAYMENTS-BNAME IS INITIAL.
*          IF FPMON <> PAYMENTS-FPPER+4(2).
*            CLEAR OF_TAX_AMT.
*            CLEAR FPMON.
*          ENDIF.
          CLEAR TEMP_PAYMENTS.
          LOOP AT TEMP_PAYMENTS WHERE PERNR = FINAL_TAB-PERNR AND
            SEQNR > PAYMENTS-SEQNR AND TAXPD > 0.
            OUT = 1.
            OF_TAX_AMT = OF_TAX_AMT + PAYMENTS-TAXPD.
            FPMON = PAYMENTS-FPPER+4(2).
            EXIT.
          ENDLOOP.
          IF OUT = 1.
            CLEAR OUT.
            CONTINUE.
          ENDIF.
        ELSE.
*          OF_TAX_AMT = 0.
*          CLEAR FPMON.
        ENDIF.
      ENDIF.
      TAX_PAY_DATE = PAYMENTS-TDATE.
      BANKNAME = PAYMENTS-BNAME.

      WRITE: / '|', TAX_PAY_DATE, AT AX SY-VLINE,  BANKNAME,
                AT CX PAYMENTS-TAXPD CURRENCY ' ',
                AT DX SY-VLINE,
                AT ZX '|'.

    ENDLOOP.



    WRITE: / '|', AT AX SY-VLINE, AT DX SY-VLINE,
                AT ZX '|'.
    WRITE AT A(ZX) SY-ULINE.
    AX = 18.
    DX = 15.
    WRITE : / SY-VLINE, AT AX 'Total'(141), AT CX PR_EMP_TOTAL COLOR
    COL_TOTAL CURRENCY ' ', AT ZX SY-VLINE.
    WRITE : / SY-VLINE,
            AT AX 'ADD:Amount Deducted by Previous Employer'(142), AT CX
FINAL_TAB-TDS_PETD COLOR COL_TOTAL CURRENCY ' ', AT ZX SY-VLINE.
    WRITE : / SY-VLINE, AT AX 'ADD:Tax deducted on other incomes'(143),
  AT CX FINAL_TAB-TDS_IFOS COLOR COL_TOTAL CURRENCY ' ', AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE AT A(ZX) SY-ULINE.
    WRITE : / SY-VLINE, AT AX 'Total Amount Deducted'(144),
                 AT CX FINAL_TAB-TOT_TAX_DEDUCTED COLOR COL_TOTAL
CURRENCY ' ',
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE,  AT ZX SY-VLINE.
    WRITE AT A(ZX) SY-ULINE.

    PERFORM CONVERT_INR_TO_WORDS USING PR_EMP_TOTAL
                                 CHANGING TAX_DEDUCTED_WRDS.

    LENGTH = STRLEN( TAX_DEDUCTED_WRDS ).
    WRITE: / SY-VLINE,'Certified that a sum of '(145), AT (LENGTH)
TAX_DEDUCTED_WRDS
           COLOR COL_HEADING, AT ZX SY-VLINE,
 / SY-VLINE,
'has been deducted at source and paid to the credit of the Central'(146)
NO-GAP,'Government.'(147), AT ZX SY-VLINE,/ SY-VLINE,TEXT-148 NO-GAP ,
'per', AT ZX SY-VLINE, / SY-VLINE, 'records', AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT 40 'Signature of the person responsible'(149),
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT 40 'for deduction of tax'(150), AT ZX SY-VLINE
 .
    WRITE: / SY-VLINE, AT 40 'for', NAME1, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT ZX SY-VLINE.

    LENGTH = STRLEN( RP_DSG ).
    WRITE: / SY-VLINE, 'Date:'(151), SY-DATUM, AT 40 'Full Name:'(152) ,
             RP_NAME, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, 'Place:'(153),RPLACE, AT 40 'Designation:'(154),
             AT (LENGTH) RP_DSG, AT ZX SY-VLINE.


    WHILE SY-LINNO < 64.
      WRITE: / SY-VLINE, AT ZX SY-VLINE.
    ENDWHILE.
    WRITE AT A(ZX) SY-ULINE.

    NEW-PAGE NO-TITLE.
    LX = ZX / 2 - 13.

*Print Annexure.
    WRITE:  AT LX 'Annexure to Form No. 16'(155) CENTERED COLOR
COL_HEADING,
/
.

    WRITE: AT A(ZX) SY-ULINE.
    WRITE: / SY-VLINE,'  Name:'(177),FINAL_TAB-ENAME,
    AT 40 'Emp No.:'(176),
           FINAL_TAB-PERNR, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

    AX = ZX / 3.
    MX = AX * 2.
    DX = MX + 10.
    LX = AX - 10.
    MX = 56.
    CX = MX - LX.

    WRITE: / SY-VLINE,
            AT LX(CX) 'Particulars'(092) CENTERED COLOR COL_HEADING,
            AT MX(AX) 'Amount(Rs.)'(156) CENTERED COLOR COL_HEADING,
            AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

    WRITE: / SY-VLINE,AT 2(78) 'Emoluments paid'(157) COLOR COL_HEADING,
*             AT lx sy-vline, AT mx sy-vline,
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

* Gross components
    LOOP AT GROSS_TAB WHERE PERNR = FINAL_TAB-PERNR
                        AND CNTR2 = FINAL_TAB-CNTR2 .
      LENGTH = STRLEN( GROSS_TAB-SPEC_TXT ) .

      IF GROSS_TAB-SIGN <> 0 .
        SIGN = '-'.
      ELSE.
        SIGN = SPACE.
      ENDIF.
      WRITE: / SY-VLINE, AT LX SY-VLINE,
               AT (LENGTH) GROSS_TAB-SPEC_TXT,
               AT MX GROSS_TAB-AMOUNT  CURRENCY ' ' NO-GAP, SIGN,
               AT MX SY-VLINE,AT ZX   SY-VLINE.
    ENDLOOP.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
*    WRITE: / sy-vline, AT lx sy-vline, AT mx sy-vline, AT zx sy-vline.
    WRITE: AT A(ZX) SY-ULINE.

    WRITE: / SY-VLINE,AT 2(78) 'Perks'(158) COLOR COL_HEADING,
*             AT lx sy-vline, AT mx sy-vline,
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

* Perk components
    LOOP AT PERK_TAB WHERE PERNR = FINAL_TAB-PERNR
                      AND CNTR2 = FINAL_TAB-CNTR2.
      LENGTH = STRLEN( PERK_TAB-SPEC_TXT ) .

      IF SEC10_TAB-SIGN = -1.
        SIGN = '-'.
      ELSE.
        SIGN = SPACE.
      ENDIF.

      WRITE: / SY-VLINE, AT LX SY-VLINE,
               AT (LENGTH) PERK_TAB-SPEC_TXT,
               AT MX PERK_TAB-AMOUNT CURRENCY ' ' NO-GAP, SIGN,
               AT MX SY-VLINE,AT ZX SY-VLINE.
    ENDLOOP.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT LX(ZX) SY-ULINE.

    WRITE: / SY-VLINE,
            AT LX(CX) ' Gross emoluments'(159)  COLOR COL_HEADING,
            FINAL_TAB-GROSS_SAL COLOR COL_TOTAL CURRENCY ' ',AT MX
SY-VLINE,
            AT LX SY-VLINE,  AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

    WRITE: / SY-VLINE,
             AT 2(78) 'Income from other sources'(160) COLOR COL_HEADING
,
*             AT lx sy-vline, AT mx sy-vline,
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

*  IFOS components
    LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR
                       AND CNTR2 = FINAL_TAB-CNTR2.
      LENGTH = STRLEN( IFOS_TAB-SPEC_TXT ) .
      IF IFOS_TAB-SIGN = -1.
        SIGN = '-'.
      ELSE.
        SIGN = SPACE.
      ENDIF.
      WRITE: / SY-VLINE, AT LX SY-VLINE,
               AT (34) IFOS_TAB-SPEC_TXT,
               IFOS_TAB-AMOUNT CURRENCY ' ' NO-GAP, SIGN ,
               AT MX SY-VLINE, AT ZX   SY-VLINE.
    ENDLOOP.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT LX(ZX) SY-ULINE.

    WRITE: / SY-VLINE,
           AT LX(CX) ' Total Income from other sources'(161)
           COLOR COL_HEADING,
           FINAL_TAB-OTH_INCOME COLOR COL_TOTAL CURRENCY ' ',
           AT LX SY-VLINE, AT MX SY-VLINE,
           AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

    WRITE: / SY-VLINE,AT 2(78) 'Exemptions u/s 10'(162) COLOR
COL_HEADING,
*             AT lx sy-vline, AT mx sy-vline,
             AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

* Section 10 components
    LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR
                       AND CNTR2 = FINAL_TAB-CNTR2.
      LENGTH = STRLEN( SEC10_TAB-SPEC_TXT ) .
      IF SEC10_TAB-SIGN = -1.
        SIGN = '-'.
      ELSE.
        SIGN = SPACE.
      ENDIF.

      WRITE: / SY-VLINE, AT LX SY-VLINE,
               AT (LENGTH) SEC10_TAB-SPEC_TXT,
               AT MX SEC10_TAB-AMOUNT CURRENCY ' ' NO-GAP,SIGN,
               AT MX SY-VLINE, AT ZX   SY-VLINE.
    ENDLOOP.

    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT LX(ZX) SY-ULINE.

    WRITE: / SY-VLINE,
            AT LX(CX) ' Total Exemption'(163) COLOR COL_HEADING,
            FINAL_TAB-SEC10_ALL COLOR COL_TOTAL CURRENCY ' ',
            AT LX SY-VLINE, AT MX SY-VLINE,
            AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX SY-VLINE.

    WHILE SY-LINNO < 58.
      WRITE: / SY-VLINE, AT LX SY-VLINE, AT MX SY-VLINE, AT ZX
               SY-VLINE.
    ENDWHILE.
*    WRITE: / sy-vline, AT lx sy-vline, AT mx sy-vline, AT zx sy-vline.
    WRITE: AT A(ZX) SY-ULINE.

    SKIP 5.
    LENGTH = STRLEN( RP_DSG ).
    WRITE: / 'Date:'(151), SY-DATUM, AT 40 'Full Name:'(152) , RP_NAME.
    WRITE: / 'Place:'(153),RPLACE, AT 40 'Designation:'(154),
    AT (LENGTH) RP_DSG.

*    SKIP 3.
* Second Annexure For Form 12BA
    NEW-PAGE NO-TITLE.

* FORM 12BA IS TO BE PRINTED ONLY IF SALARY PAID OR SALARY PAYABLE IS
* GREATER THAN THE AMOUNT IN THE CONSTANT F12BA IN T511P
*  CLEAR TOT_VAL_PERK.
*    CLEAR TOT_EERECVR.
*    CLEAR TOT_TAX_PERK.

    LX = ZX - 2.

    WRITE : AT (LX) 'FORM No. 12BA'(164)  CENTERED COLOR COL_HEADING.
    WRITE : AT (LX) 'See rule 26A(2)'(165) CENTERED.
    WRITE : AT (LX) 'as inserted by'(166) CENTERED.
    WRITE : AT (LX)
 'the Income-tax (22nd Amendment) Rules, 2001, 25th September,2001'(167)
CENTERED.
    SKIP 2.
    WRITE : AT (LX)
    'Statement  showing particulars of perquisites, other fringe'(168)
    CENTERED.
    WRITE : AT (LX) TEXT-169  CENTERED.
    SKIP 2.
    LENGTH = STRLEN( ADDR1_VAL-NAME1 ).            "MKINT910704
    WRITE : '1) Name and address of employer :'(170).
    WRITE : '  ', AT (LENGTH) ADDR1_VAL-NAME1 NO-GAP,',', SADR-STRAS,
            SADR-ORT01 NO-GAP, '-' NO-GAP,STATE.
    LENGTH = STRLEN( FINAL_TAB-ENAME ).
    WRITE : '2) Name and designation of employee :'(171).
    WRITE : / '   Mr/Ms:'(172) NO-GAP, AT (LENGTH) FINAL_TAB-ENAME ,
            'Desig:'(173) NO-GAP,
            FINAL_TAB-POSITION, 'Emp#.'(174) NO-GAP, FINAL_TAB-PERNR.
    WRITE : '3) Assessment Year:'(175) NO-GAP, PENDDA(4) NO-GAP, '-'
NO-GAP,
            ASSM_END.

*  FILLING OF THE MAIN WINDOW.*****************
    MX = 7.
    NX = 35.
    AX = 50.
    BX = 65.
    WRITE: AT A(ZX) SY-ULINE.

    WRITE: / SY-VLINE, 'S.No',
           AT MX SY-VLINE, 'Nature of perquisite'(178),
           AT NX SY-VLINE, 'Value of'(179) ,
           AT AX SY-VLINE, 'Amount,if any'(180),
           AT BX SY-VLINE, 'Amount of'(181),
           AT ZX SY-VLINE.
    WRITE: / SY-VLINE,
           AT MX SY-VLINE, '(see rule 3)'(182),
           AT NX SY-VLINE, 'perq as per'(183) ,
           AT AX SY-VLINE, 'paid by'(184),
           AT BX SY-VLINE, 'taxable'(185),
           AT ZX SY-VLINE.
    WRITE: / SY-VLINE, '',
           AT MX SY-VLINE,
           AT NX SY-VLINE, 'rules'(186) ,
           AT AX SY-VLINE, 'employee'(187),
           AT BX SY-VLINE, 'perquisite'(188),
           AT ZX SY-VLINE.
    WRITE: / SY-VLINE,
           AT MX SY-VLINE,
           AT NX SY-VLINE, '(Rs.)'(189) ,
           AT AX SY-VLINE, '(Rs.)'(189),
           AT BX SY-VLINE, '(Rs.)'(189),
           AT ZX SY-VLINE .
    WRITE: / SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE, AT AX SY-VLINE,
           AT BX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.

    SELECT * FROM T52DB WHERE SPRSL = 'E'
                        AND   MOLGA =  '40'
                        AND   EVCLS =  '09'
                        AND   EVCLV <= '17'.
      CLEAR: FORM12BA_TAB.
*    LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR AND
*                               CNTR2 = FINAL_TAB-CNTR2.
      READ TABLE FORM12BA_TAB WITH KEY PERNR = FINAL_TAB-PERNR
                                          CNTR2 = FINAL_TAB-CNTR2
                                          EVCLS_SPEC = T52DB-EVCLV.
      WRITE: / SY-VLINE, (3) T52DB-EVCLV CENTERED,
           AT MX SY-VLINE NO-GAP, (26) T52DB-EVCVT,
         AT NX SY-VLINE NO-GAP, (12) FORM12BA_TAB-VAL_PERK CURRENCY ' ',
          AT AX SY-VLINE NO-GAP, (12) FORM12BA_TAB-EERECVR CURRENCY ' ',
         AT BX SY-VLINE NO-GAP, (12) FORM12BA_TAB-TAX_PERK CURRENCY ' ',
           AT ZX SY-VLINE.

      IF FORM12BA_TAB-EVCLS_SPEC NE 17.

        TOT_VAL_PERK = TOT_VAL_PERK + FORM12BA_TAB-VAL_PERK.
        TOT_EERECVR = TOT_EERECVR + FORM12BA_TAB-EERECVR.
        TOT_TAX_PERK = TOT_TAX_PERK + FORM12BA_TAB-TAX_PERK.

      ENDIF.

*    ENDLOOP.
    ENDSELECT.
    WRITE: / SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE, AT AX SY-VLINE,
             AT BX SY-VLINE, AT ZX SY-VLINE.

    WRITE: AT A(ZX) SY-ULINE.
    WRITE: / SY-VLINE,
           AT MX SY-VLINE, 'Total value of Perq'(190),
           AT NX SY-VLINE NO-GAP, (12) TOT_VAL_PERK CURRENCY ' ',
           AT AX SY-VLINE NO-GAP, (12) TOT_EERECVR CURRENCY ' ',
           AT BX SY-VLINE NO-GAP, (12) TOT_TAX_PERK CURRENCY ' ',
           AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE, AT AX SY-VLINE,
           AT BX SY-VLINE, AT ZX SY-VLINE.

    WRITE: AT MX(ZX) SY-ULINE.
    WRITE: / SY-VLINE,
         AT MX SY-VLINE, 'Total value of Profits'(191),
         AT NX SY-VLINE NO-GAP, (12) FORM12BA_TAB-VAL_PERK CURRENCY ' ',
         AT AX SY-VLINE NO-GAP, (12) FORM12BA_TAB-EERECVR CURRENCY ' ',
         AT BX SY-VLINE NO-GAP, (12) FORM12BA_TAB-TAX_PERK CURRENCY ' ',
         AT ZX SY-VLINE.
    WRITE: / SY-VLINE, AT MX SY-VLINE, AT NX SY-VLINE, AT AX SY-VLINE,
           AT BX SY-VLINE, AT ZX SY-VLINE.
    WRITE: AT A(ZX) SY-ULINE.
    SKIP 2.
    WRITE: AT 2 'DECLARATION BY EMPLOYER'(192).
    SKIP.

    LENGTH = STRLEN( RP_DSG ).

    WRITE: 'I', RP_NAME ,'S/0 working as'(193),AT (LENGTH) RP_DSG,
'(designation) do'(194), 'hereby declare on behalf of'(195), NAME1 ,
'(name of the'(196),TEXT-074,TEXT-075,TEXT-076,TEXT-077.

    SKIP 4.
    WRITE: / , AT 40 'Signature of the person responsible'(149),/,
               AT 40 'for deduction of tax'(150).
    SKIP 2.
    WRITE: / 'Place:'(153),RPLACE.
    WRITE: / 'Date:'(151), SY-DATUM.

    WRITE: /.
    WRITE: AT 40 'Full Name:'(152) , RP_NAME,/,
           AT 40 'Designation:'(154), AT (LENGTH) RP_DSG.


*  FILLING OF THE MAIN WINDOW.*****************



  ENDLOOP.        "FINAL_TAB

ENDFORM.                    " PRINT_USING_WRITE
*&---------------------------------------------------------------------*
*&      Form  PAGE-END
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAGE-END .
  IF SY-PAGNO = 1.
    WRITE : / SY-VLINE, AT 32 SY-VLINE,
              AT 48 SY-VLINE, AT 64 SY-VLINE,
              AT 80 SY-VLINE .
    WRITE AT 1(80) SY-ULINE.
    SKIP.
    WRITE AT 1(80) SY-ULINE.
  ELSE.
  ENDIF.
ENDFORM.                    " PAGE-END



********START OF PDF CODING**************

*&--------------------------------------------------------------------*
*&      Form  print_module_pdf
*&--------------------------------------------------------------------*
*       Subroutine For displaying the PDF Output
*       No parameters need to be passed
*---------------------------------------------------------------------*
FORM PRINT_MODULE_PDF.

  DATA:   LANG TYPE SY-LANGU,
          PAGE_NO TYPE I,
          TAX_PERK(10) TYPE C,
          EERECVR(9) TYPE C,
          VAL_PERK(10) TYPE C,
          VAL_F12BA TYPE MAXBT,
          PROF_F12BA TYPE MAXBT,
          SAL_SEC17 TYPE MAXBT,
          TOT_VAL_PERK TYPE MAXBT,
          TOT_EERECVR TYPE MAXBT,
          TOT_TAX_PERK TYPE MAXBT,
          SALARIES_WO_PERK TYPE MAXBT,
          F12BA_BALANCE TYPE MAXBT,
          F12BA_STD_DED TYPE MAXBT,
          TOTAL_TAXPDF LIKE PC207-BETRG,
          CONMT_PF_PDF TYPE MAXBT,
          DEDMT_PF_PDF TYPE MAXBT.

  DATA: BEGIN OF TEMP_PAYMENTS OCCURS 10.
          INCLUDE STRUCTURE PINBK.
  DATA: END OF TEMP_PAYMENTS.
  DATA: OUT TYPE I.
  DATA : J TYPE I.

  data: l_fp             type ref to if_fp,
        l_pdfobj         type ref to if_fp_pdf_object,
        l_fpex           type ref to cx_fp_runtime,
        l_filename_out   type string.

  data: p_dest       type rfcdest Value 'ADS_HTTPS'.
  data: s_field(100)  type c value 'data[0].FORM16_BODY_PAGE2.FORM16_2.TOT_AMT_DEDUC.SignatureField1[0]'.
  data: s_field1(100)  type c value 'data[0].F12BA_BODY_PAGE.F12BA.FOOTER.DETAILS.SignatureField1[0]'.
  data: l_out           type xstring,
        l_in            type xstring,
        l_f16           type xstring,
        l_12ba          type xstring,
        l_annex         type xstring,
        formoutput type fpformoutput,
        form12ba   type fpformoutput,
        formannex  type fpformoutput.


* COntrol log changes 09/11/2009.
    DATA: control_log TYPE HRIN_CONTLOG,
          wa_cont_log LIKE LINE OF control_log.
    DATA: contlogform type fpformoutput.
    data: s_field10(100)  type c value 'data[0].SignatureField1[0]'.

  DATA : CNTR TYPE I,                             " MDSNT927906
          SEQ TYPE I.
  DATA: CNTR_F16 TYPE I,
        CNTR_REC TYPE N.
  MOVE 'EN' TO LANG.
  ENDDATE = SY-DATUM.
  IF NOT adscon IS INITIAL.
    p_dest = adscon.
  ENDIF.

********START OF PDF CODING********
  DATA : LV_FORMNAME TYPE FPNAME.
  DATA : LV_TEXT(21) TYPE C,
         LV_TEXT1(18) TYPE C,
         LV_TEXT2(3) TYPE C,
         LV_MESG TYPE STRING.
  DATA LV_W_CX_ROOT TYPE REF TO CX_ROOT.
*******END OF PDF CODING***********
  IF SY-BATCH = 'X' AND ( NOT LAYOUT IS INITIAL OR
                          NOT LAYOUT1 IS INITIAL OR
                          NOT LAYOUT2 IS INITIAL OR
                          NOT LAYOUT3 IS INITIAL ).
    DISP_FLG_LOT = -1.
  ENDIF.

********START OF CODING************
  PERFORM JOB_OPEN_PDF USING LANG.
********END OF CODING************
  DESCRIBE TABLE FINAL_TAB LINES CNTR_F16.
*  PERFORM get_custom_text CHANGING FLEXTXT.
  LOOP AT FINAL_TAB.
********START OF PDF CODING************
    CLEAR :     GT_GROSSTAB,
                GT_PERKSTAB,
                GT_IFOSTAB,
                GT_EXEMPTAB,
                GT_GROSSTAB,
                GT_PERKSTAB,
                GT_IFOSTAB,
                GS_F16DATA,
                GS_F12BADETAILS.
********END OF PDF CODING************
    CLEAR : formoutput,
            FORMANNEX,
            FORM12BA.
************************************************************************
* CLEARING VARIABLES OF FORM12BA
    CLEAR VAL_F12BA.CLEAR PROF_F12BA.CLEAR SAL_SEC17.CLEAR TOT_VAL_PERK.
    CLEAR TOT_EERECVR.CLEAR TOT_TAX_PERK.
************************************************************************
    SNAME = '40TAX16A'.
    IF INFOS IS INITIAL.
      IF FINAL_TAB-BALANCE < 150000 AND FINAL_TAB-BUS_PROF <= 0 AND
         FINAL_TAB-TDS_IFOS <= 0.
        IF DISP_FLG_LOT < 1 AND LAYOUT3 NE ' '.
          FORMNAME = LAYOUT3.
        ELSE.
          PERFORM GET_LAYOUT_SET USING SNAME ENDDATE.
        ENDIF.
      ELSE.
        IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
          FORMNAME = LAYOUT.
        ELSE.
          SNAME = '40TAX016'.
          PERFORM GET_LAYOUT_SET USING SNAME ENDDATE.
        ENDIF.
      ENDIF.
    ELSE.
      IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
        FORMNAME = LAYOUT.
      ELSE.
        SNAME = '40TAX016'.
        PERFORM GET_LAYOUT_SET USING SNAME ENDDATE.
      ENDIF.
    ENDIF.
********START OF PDF CODING***********
    MOVE FORMNAME TO LV_FORMNAME.
********END OF PDF CODING*************
* Get employer address
    CLEAR STATE.
    CLEAR SADR.
    PERFORM ER_ADDRESS USING FINAL_TAB-TANNO.
********START OF PDF CODING*********
    CLEAR : GS_F16DATA.

    MOVE : ADDR1_VAL-NAME1 TO GS_F16DATA-ENAME1,
           DOC_TYPE TO GS_F16DATA-DOC_TYPE,
           SADR-STRAS TO GS_F16DATA-ESTREET,
    SADR-PSTLZ+0(6) TO GS_F16DATA-EPOCODE,
    SADR-ORT01 TO GS_F16DATA-ECITY,
    PENDDA+0(4) TO GS_F16DATA-ASSM_START,
    ASSM_END TO GS_F16DATA-ASSM_END.
    LOOP AT HD_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                         CNTR2 = FINAL_TAB-CNTR2.
      IF HD_TAB-F16_BEGDA > PBEGDA.
        MOVE HD_TAB-F16_BEGDA TO GS_F16DATA-FYBEGDA.
      ELSE.
        IF afy_switch IS INITIAL.
        MOVE PBEGDA TO GS_F16DATA-FYBEGDA.
        ELSE.
        IF afy_switch = 1.
          PBEGDA+4(4) = '0401'.
          MOVE PBEGDA TO GS_F16DATA-FYBEGDA.
        ENDIF.
        ENDIF.
      ENDIF.
      IF HD_TAB-F16_ENDDA < PENDDA.
        MOVE HD_TAB-F16_ENDDA TO GS_F16DATA-FYENDDA.
      ELSE.
        MOVE PENDDA TO GS_F16DATA-FYENDDA.
      ENDIF.
    ENDLOOP.
********END OF PDF CODING*********
    IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
      PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                     CHANGING STATE.
    ENDIF.

********START OF PDF CODING*********
    MOVE : STATE TO GS_F16DATA-ESTATE.
********END OF PDF CODING*********

* Start of First Page

* Get TDS Circle address
    CLEAR SADR.
    CLEAR STATE.
    PERFORM GET_ADDRESS USING FINAL_TAB-ADDRS.
    IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
      PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
    ENDIF.
********START OF PDF CODING*********
    MOVE : SADR-NAME1 TO GS_F16DATA-CNAME1,
           SADR-STRAS TO GS_F16DATA-CSTREET,
    SADR-PSTLZ+0(6) TO GS_F16DATA-CPOCODE,
    SADR-ORT01 TO GS_F16DATA-CCITY,
    STATE TO GS_F16DATA-CSTATE.
********END OF PDF CODING*********


    LOOP AT HD_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                         CNTR2 = FINAL_TAB-CNTR2.
      IF HD_TAB-F16_BEGDA > PBEGDA.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' HD_TAB-F16_BEGDA.
      ELSE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' PBEGDA.
      ENDIF.
      IF HD_TAB-F16_ENDDA < PENDDA.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' HD_TAB-F16_ENDDA.
      ELSE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' PENDDA.
      ENDIF.
    ENDLOOP.

***********Print acknowledgement numbers of the Form 24Q eFiling
    CLEAR TEMP.                                                      "PKT1086329

    CLEAR ACKNO_TAB.

    LOOP AT ACKNO_tab WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  1.
      EXIT.
    ENDLOOP.

  MOVE : ackno_tab-ackno1 TO ackno_new-ackno1_1,
         ackno_tab-ackno2 TO ackno_new-ackno1_2,
         ackno_tab-ackno3 TO ackno_new-ackno1_3,
         ackno_tab-ackno4 TO ackno_new-ackno1_4.

    CLEAR ACKNO_tab.

    LOOP AT ACKNO_tab WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  2.

      EXIT.
    ENDLOOP.
 MOVE : ackno_tab-ackno1 TO ackno_new-ackno2_1,
        ackno_tab-ackno2 TO ackno_new-ackno2_2,
        ackno_tab-ackno3 TO ackno_new-ackno2_3,
        ackno_tab-ackno4 TO ackno_new-ackno2_4.

    CLEAR ACKNO_tab.

    LOOP AT ACKNO_tab WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  3.
      EXIT.
    ENDLOOP.
    MOVE : ackno_tab-ackno1 TO ackno_new-ackno3_1,
         ackno_tab-ackno2 TO ackno_new-ackno3_2,
         ackno_tab-ackno3 TO ackno_new-ackno3_3,
         ackno_tab-ackno4 TO ackno_new-ackno3_4.

    CLEAR ACKNO_tab.

    LOOP AT ACKNO_tab WHERE PERNR     =  FINAL_TAB-PERNR     AND
                            F16_BEGDA >= FINAL_TAB-F16_BEGDA AND
                            F16_ENDDA <= FINAL_TAB-F16_ENDDA AND
                            QUARTER   =  4.
      EXIT.
    ENDLOOP.
    MOVE : ackno_tab-ackno1 TO ackno_new-ackno4_1,
           ackno_tab-ackno2 TO ackno_new-ackno4_2,
           ackno_tab-ackno3 TO ackno_new-ackno4_3,
           ackno_tab-ackno4 TO ackno_new-ackno4_4.
 append ackno_tab to ackno_new.

  MOVE : ackno_new-ackno1_1 TO GS_F16DATA-acknoq1_1,
         ackno_new-ackno1_2 TO GS_F16DATA-acknoq1_2,
         ackno_new-ackno1_3 TO GS_F16DATA-acknoq1_3,
         ackno_new-ackno1_4 TO GS_F16DATA-acknoq1_4,


         ackno_new-ackno2_1 TO GS_F16DATA-acknoq2_1,
         ackno_new-ackno2_2 TO GS_F16DATA-acknoq2_2,
         ackno_new-ackno2_3 TO GS_F16DATA-acknoq2_3,
         ackno_new-ackno2_4 TO GS_F16DATA-acknoq2_4,



         ackno_new-ackno3_1 TO GS_F16DATA-acknoq3_1,
         ackno_new-ackno3_2 TO GS_F16DATA-acknoq3_2,
         ackno_new-ackno3_3 TO GS_F16DATA-acknoq3_3,
         ackno_new-ackno3_4 TO GS_F16DATA-acknoq3_4,

         ackno_new-ackno4_1 TO GS_F16DATA-acknoq4_1,
         ackno_new-ackno4_2 TO GS_F16DATA-acknoq4_2,
         ackno_new-ackno4_3 TO GS_F16DATA-acknoq4_3,
         ackno_new-ackno4_4 TO GS_F16DATA-acknoq4_4.


* Employer info
********START OF PDF CODING*********
    MOVE : FINAL_TAB-PANNO TO GS_F16DATA-PANNO,
           FINAL_TAB-TANNO TO GS_F16DATA-TANNO,
           FINAL_TAB-GIRNO TO GS_F16DATA-GIRNO.
********END OF PDF CODING*********
     CALL METHOD CL_HRPAYIN_SWITCH_CHECK_4=>HRLOCIN_SFWS_SC_01
      RECEIVING
       RV_ACTIVE = HTEXT_SWITCH
        .
     IF HTEXT_SWITCH = 'X'.
       PERFORM get_custom_text CHANGING FLEXTXT final_tab-pernr.
     ELSE.
           FLEXTXT = final_tab-pernr.
     ENDIF.
* Employee info
********START OF PDF CODING*********
    MOVE : FINAL_TAB-ENAME TO GS_F16DATA-ENAME,
           FINAL_TAB-POSITION TO GS_F16DATA-DESIGN,
           FINAL_TAB-PERNR TO GS_F16DATA-EMPNO,
           FINAL_TAB-ICNUM TO GS_F16DATA-ICNUM,
           FINAL_TAB-GENDER TO GS_F16DATA-GENDER,
           FINAL_TAB-DOB TO GS_F16DATA-DOB,
           FINAL_TAB-FFNAME TO GS_F16DATA-FFNAME,
           FINAL_TAB-FLNAME TO GS_F16DATA-FLNAME,
           FINAL_TAB-HNUMB TO GS_F16DATA-HNUMB,
           FINAL_TAB-LOCALITY TO GS_F16DATA-LOCALITY,
           FINAL_TAB-PIN TO GS_F16DATA-PIN,
           FINAL_TAB-CITY TO GS_F16DATA-CITY,
           FINAL_TAB-TELN TO GS_F16DATA-TELN,
           FLEXTXT TO GS_F16DATA-FLEXTXT.
********END OF PDF CODING*********

* Compute for sl. no. 1(b) of form 16
    LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                           AND CNTR2 <= FINAL_TAB-CNTR2
                           AND EVCLS_SPEC NE 17 .
      VAL_F12BA = VAL_F12BA + FORM12BA_TAB-TAX_PERK .
    ENDLOOP.

*   Value of perq u/s 17(2) from Prev Emp added.
    VAL_F12BA = VAL_F12BA + FINAL_TAB-PETD_S172.
********START OF PDF CODING*********
    MOVE VAL_F12BA TO GS_F16DATA-VAL_F12BA.
********END OF PDF CODING*********
* Compute for sl. no. 1(c) of form 16
    LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                           AND CNTR2 <= FINAL_TAB-CNTR2
                           AND EVCLS_SPEC = 17 .
      PROF_F12BA = PROF_F12BA + FORM12BA_TAB-TAX_PERK.
    ENDLOOP.

*   Value of Profits u/s 17(3) from Prev Emp added.
    PROF_F12BA = PROF_F12BA + FINAL_TAB-PETD_S173.
********START OF PDF CODING*************
    MOVE PROF_F12BA TO GS_F16DATA-PROF_F12BA.
********END OF PDF CODING**************

* Compute for sl. no. 1(a) of form 16
    SAL_SEC17 = FINAL_TAB-GROSS_SAL - ( PROF_F12BA + VAL_F12BA ).
********START OF PDF CODING*********
    MOVE : SAL_SEC17 TO GS_F16DATA-SAL_SEC17,
           FINAL_TAB-GROSS_SAL TO GS_F16DATA-GROSS_SAL,
           FINAL_TAB-SEC10_ALL TO GS_F16DATA-SEC10_ALL,
           FINAL_TAB-BALANCE TO GS_F16DATA-BALANCE,
           FINAL_TAB-STD_DED TO GS_F16DATA-STD_DED,
                  ENT_ALL TO GS_F16DATA-ETALL,
           FINAL_TAB-PTAX TO GS_F16DATA-PTAX,
           FINAL_TAB-AGGR_DED TO GS_F16DATA-AGGR_DED,
           FINAL_TAB-SALARIES TO GS_F16DATA-SALARIES,
           FINAL_TAB-OTH_INCOME TO GS_F16DATA-OTH_INCOME,
           FINAL_TAB-GROSS_TOT_INCOME TO GS_F16DATA-GROSS_TOT_INCOME,
           FINAL_TAB-SEC80_DED TO GS_F16DATA-SEC80_DED,
           FINAL_TAB-SEC88_DED TO GS_F16DATA-SEC88_DED,
           FINAL_TAB-SEC88B_DED TO GS_F16DATA-SECB88_DED,
           FINAL_TAB-SEC88C_DED TO GS_F16DATA-SECC88_DED,
           FINAL_TAB-TOT_INCOME TO GS_F16DATA-TOT_INCOME,
           FINAL_TAB-TAX_TOT_INCOME TO GS_F16DATA-TAX_TOT_INCOME,
           FINAL_TAB-SURCHG TO GS_F16DATA-SURCHARGE,
           FINAL_TAB-EDU_CESS TO GS_F16DATA-EDU_CESS,
           FINAL_TAB-EPF_TOT TO GS_F16DATA-TOT_EPF,
           FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF TO GS_F16DATA-TAX_BEF_RELIEF,
           FINAL_TAB-CHAPVI_DED  TO GS_F16DATA-CHAPVI_DED,
           FINAL_TAB-SEC89_RELIEF TO GS_F16DATA-SEC89_RELIEF,
           FINAL_TAB-TAX_PAYABLE  TO GS_F16DATA-TAX_PAYABLE,
           FINAL_TAB-TAX_DEDUCTED TO GS_F16DATA-TAX_DEDUCTED,
           FINAL_TAB-TAX_PAID_EMPLOYER TO GS_F16DATA-EMPLOYER_TAX,
           FINAL_TAB-TOT_TAX_DEDUCTED TO GS_F16DATA-TOT_TAX_DEDUCTED,
           FINAL_TAB-NET_TAX_PAYABLE TO GS_F16DATA-NET_TAX_PAYABLE,
           FINAL_TAB-DEDN_S24 TO GS_F16DATA-DED_S24,
           FINAL_TAB-BUS_PROF TO GS_F16DATA-BUS_PROF.
           READ TABLE INT_S88 WITH KEY PERNR = FINAL_TAB-PERNR
                                 CNTR2 = FINAL_TAB-CNTR2.
             IF SY-SUBRC = 0.
               clear : conmt_pf_pdf, dedmt_pf_pdf.
             ELSE.
               conmt_pf_pdf = FINAL_TAB-EPF_TOT.
               read table int_s80 with key pernr = final_tab-pernr
                                           sbsec = '15' sbdiv = '01'.
               if sy-subrc eq 0.
                  dedmt_pf_pdf = int_s80-dedmt.
               endif.
             ENDIF.
    MOVE : conmt_pf_pdf TO GS_F16DATA-conmt_pf,
           dedmt_pf_pdf TO GS_F16DATA-dedmt_pf.
********END OF PDF CODING*********
TOTAL_TAXPDF = FINAL_TAB-TAX_DEDUCTED + FINAL_TAB-TAX_PAID_EMPLOYER.
MOVE TOTAL_TAXPDF TO GS_F16DATA-TOTAL_TAX.

    READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                CNTR2 = FINAL_TAB-CNTR2
                                SBSEC = '01' SBDIV = '01'.
    IF SY-SUBRC EQ 0.
      IF INT_S80-COAMT IS INITIAL AND INT_S80-QLAMT IS INITIAL.

      ELSE.
        IF INT_S80-QLAMT IS INITIAL.
          GS_F16DATA-SEC80_CCC_CONMT = INT_S80-COAMT.
        ELSE.
          GS_F16DATA-SEC80_CCC_CONMT = INT_S80-QLAMT.
        ENDIF.
      ENDIF.
      GS_F16DATA-SEC80_CCC_DEDMT = INT_S80-DEDMT.
    ENDIF.

***
    IF SNAME = '40TAX016'.                             "RBSNT927906
      CLEAR J.
      LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                    CNTR2 = FINAL_TAB-CNTR2 AND
                                    SBSEC <> '01' AND SBSEC <> '15'.
        CLEAR TEMP.
        CLEAR TEMP1.
        TEMP = ALPHA+J(1).
        J = J + 1.
        TEMP1 = '('.
        MOVE TEMP TO TEMP1+1(1).
        MOVE ')' TO TEMP1+2(1).
        CONDENSE TEMP1 NO-GAPS.
        MOVE TEMP1 TO INT_S80-SBTDS+0(3).
        CLEAR GS_DEDUCTIONS.
        MOVE : INT_S80-SBTDS TO GS_DEDUCTIONS-SBTDS,
               INT_S80-SBDIV TO GS_DEDUCTIONS-SBDIV,
               INT_S80-COAMT TO GS_DEDUCTIONS-CONMT,
               INT_S80-QLAMT TO GS_DEDUCTIONS-QLAMT,
               INT_S80-DEDMT TO GS_DEDUCTIONS-DEDMT.
        CONCATENATE GS_DEDUCTIONS-SBTDS '(' GS_DEDUCTIONS-SBDIV ')' INTO GS_DEDUCTIONS-SBTDS.
        APPEND GS_DEDUCTIONS TO GT_DEDUCTIONS.

      ENDLOOP.
    ELSE.
      IF SNAME = '40TAX16A'.
        LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                      CNTR2 = FINAL_TAB-CNTR2.
          CLEAR TEMP.
          CLEAR TEMP1.                                                  "MDSNT927906
          TEMP = ALPHA+J(1).
          J = J + 1.
          TEMP1 = '('.
          MOVE TEMP TO TEMP1+1(1).
          MOVE ')' TO TEMP1+2(1).
          CONDENSE TEMP1 NO-GAPS.
          MOVE TEMP1 TO INT_S80-SBTDS+0(3).
          CLEAR GS_DEDUCTIONS.
          MOVE : INT_S80-SBTDS TO GS_DEDUCTIONS-SBTDS,
                 INT_S80-SBDIV TO GS_DEDUCTIONS-SBDIV,
                 INT_S80-COAMT TO GS_DEDUCTIONS-CONMT,
                 INT_S80-QLAMT TO GS_DEDUCTIONS-QLAMT,
                 INT_S80-DEDMT TO GS_DEDUCTIONS-DEDMT.
          CONCATENATE GS_DEDUCTIONS-SBTDS '(' GS_DEDUCTIONS-SBDIV ')' INTO GS_DEDUCTIONS-SBTDS.
          APPEND GS_DEDUCTIONS TO GT_DEDUCTIONS.

        ENDLOOP.
      ENDIF.
    ENDIF.

    CLEAR I.
      CLEAR SEQ.
      CNTR = 0.
      LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
        SEQ = SEQ + 1.
      ENDLOOP.
      I = 1.
      CNTR = 1.
    LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                          CNTR2 = FINAL_TAB-CNTR2.
      I = I + 1.
********START OF PDF CODING************
      CLEAR GS_REBATE.
      MOVE : INT_S88-ITEXT TO GS_REBATE-ITEXT,
             INT_S88-QLAMT TO GS_REBATE-QLAMT.
      IF INT_S88-INAMT IS NOT INITIAL.
         MOVE INT_S88-INVMT TO GS_REBATE-INVMT.
      ELSE.
         MOVE INT_S88-INAMT TO GS_REBATE-INVMT.
      ENDIF.
      IF CNTR NE SEQ.
          GS_REBATE-CONMT = ''.
          GS_REBATE-DEDMT  = ''.
      ELSE.
          READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                        SBSEC = '15' SBDIV = '01' CNTR2 = INT_S88-CNTR2.
          IF SY-SUBRC EQ 0.
            GS_REBATE-CONMT  = INT_S80-COAMT.
*            CONDENSE GS_REBATE-CONMT NO-GAPS.
            GS_REBATE-DEDMT = INT_S80-DEDMT.
*            CONDENSE GS_REBATE-DEDMT NO-GAPS.
           ENDIF.
      ENDIF.
      CNTR = CNTR + 1.
      APPEND GS_REBATE TO GT_REBATE.
********END OF PDF CODING************
    ENDLOOP.

    CLEAR TEMP.
    CLEAR TEMP1.
    I = 0.
    TEMP = ALPHA+I(1).
    TEMP1 = '('.
    MOVE TEMP TO TEMP1+1(1).
    MOVE ')' TO TEMP1+2(1).
    CONDENSE TEMP1 NO-GAPS.
    MOVE 'TOTAL' TO TEMP1+4.
    TEMP = ALPHA+0(1).
    MOVE '[(' TO TEMP1+10.
    MOVE TEMP TO TEMP1+12.
    MOVE ') to (' TO TEMP1+13.
    IF I = 0.
      I = 1.
    ENDIF.
    I = I - 1.
    TEMP = ALPHA+I(1).
    MOVE TEMP TO TEMP1+20.
    MOVE ')]' TO TEMP1+21.
********START OF PDF CODING************
    MOVE TEMP1 TO GS_F16DATA-TOT_SEC88.
********END OF PDF CODING************

    I = I + 1.
    MOVE '[I(' TO TEMP1.
    TEMP = ALPHA+I(1).
    MOVE TEMP TO TEMP1+3.
    CONDENSE TEMP1 NO-GAPS.
********START OF PDF CODING************
    MOVE TEMP1 TO GS_F16DATA-P14_TEXT.
********END OF PDF CODING************

    TEMP_PAYMENTS[] = PAYMENTS[].

    SORT PAYMENTS BY PERNR SEQNR.

*BADI to return the bank details and the tax paid date
*for more than 12 tax remittances.

    DATA  : CUST_EXIT TYPE REF TO IF_EX_HR_IN_BANK_TRANSFER.

    CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
      CHANGING
        INSTANCE = CUST_EXIT.


    CALL METHOD CUST_EXIT->GET_TAX_BANK_DETAILS
      EXPORTING
        FLT_VAL      = '40'
      CHANGING
        TAX_PAYMENTS = PAYMENTS[].

    SORT PAYMENTS BY SEQNR.
    TEMP_PAYMENTS[] = PAYMENTS[].

    CLEAR PR_EMP_TOTAL.
    CLEAR FPMON.
    CLEAR OUT.
    LOOP AT PAYMENTS WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
      PR_EMP_TOTAL = PR_EMP_TOTAL + PAYMENTS-TAXPD.
      IF PAYMENTS-TAXPD = 0.
        CONTINUE.
      ENDIF.
* If for the offcycle run the tax date and bank is blank then
* the details are picked up from the regular payroll result, i.e from
* the period in which the offcycle was run.
      IF NOT PAYMENTS-ODATE IS INITIAL.
        IF PAYMENTS-TDATE IS INITIAL AND PAYMENTS-BNAME IS INITIAL.
*          IF FPMON <> PAYMENTS-FPPER+4(2).
*            CLEAR OF_TAX_AMT.
*            CLEAR FPMON.
*          ENDIF.
          LOOP AT TEMP_PAYMENTS WHERE PERNR = FINAL_TAB-PERNR AND
            CNTR2 = FINAL_TAB-CNTR2 AND
            SEQNR > PAYMENTS-SEQNR AND TAXPD > 0.
            OUT = 1.
            OF_TAX_AMT = OF_TAX_AMT + PAYMENTS-TAXPD.
            FPMON = PAYMENTS-FPPER+4(2).
            EXIT.
          ENDLOOP.
          IF OUT = 1.
            CLEAR OUT.
            CONTINUE.
          ENDIF.
        ELSE.
*          OF_TAX_AMT = 0.
*          CLEAR FPMON.
        ENDIF.
      ENDIF.
      BANKNAME = PAYMENTS-BNAME.
*      IF FPMON = PAYMENTS-FPPER+4(2) AND OF_TAX_AMT > 0.
      IF OF_TAX_AMT > 0.
        PAYMENTS-TAXPD = PAYMENTS-TAXPD + OF_TAX_AMT.
        CLEAR OF_TAX_AMT.
      ENDIF.
********START OF PDF CODING************
      CLEAR GS_BANKDETAILS.
      MOVE : PAYMENTS-TDATE TO GS_BANKDETAILS-PAY_DATE,
             BANKNAME TO  GS_BANKDETAILS-BANK_NAME,
             PAYMENTS-TAXPD TO GS_BANKDETAILS-TAX_PAID.
      APPEND GS_BANKDETAILS TO GT_BANKDETAILS.

********END OF PDF CODING************

    ENDLOOP.
********START OF PDF CODING************
    PERFORM CONVERT_INR_TO_WORDS USING PR_EMP_TOTAL
                                 CHANGING TAX_DEDUCTED_WRDS.
    REPLACE 'Rupees' WITH SPACE INTO TAX_DEDUCTED_WRDS.
    CONDENSE TAX_DEDUCTED_WRDS.
    MOVE : PR_EMP_TOTAL TO GS_F16DATA-TOTAL,
           FINAL_TAB-TDS_PETD TO GS_F16DATA-TDS_PETD,
           FINAL_TAB-TDS_IFOS TO GS_F16DATA-TDS_IFOS,
           TAX_DEDUCTED_WRDS TO GS_F16DATA-AMT_IN_WORDS,
           FTEXT1 TO GS_F16DATA-FTEXT1,
           FTEXT2 TO GS_F16DATA-FTEXT2,
           RP_NAME TO GS_F16DATA-RP_NAME,
           RP_SOF TO GS_F16DATA-RP_SOF,
           RP_DSG TO GS_F16DATA-RP_DSG,
           REPODATE TO GS_F16DATA-REPODATE,
           RPLACE TO GS_F16DATA-RPLACE.

    CLEAR SLNO.
    SLNO = 1.
IF wa_t7insw-begda > pbegda OR wa_t7insw-reval IS INITIAL.

    SORT TEMSEFIN_TAB BY PAYDATE."1274331""GG"
    LOOP AT TEMSEFIN_TAB WHERE PERNR = FINAL_TAB-PERNR
                            AND FPBEG >= FINAL_TAB-F16_BEGDA
                            AND FPEND <= FINAL_TAB-F16_ENDDA      . "MDSNT
*      IF TEMSEFIN_TAB-INCOMETAX  IS INITIAL."gg1274331"
*        CONTINUE.
*      ENDIF.

      GS_TEMSEDETAILS-SL_NO = SLNO.
       MOVE :
         TEMSEFIN_TAB-INCOMETAX  TO GS_TEMSEDETAILS-INCOMETAX,
         TEMSEFIN_TAB-EDUCESS    TO GS_TEMSEDETAILS-EDUCESS,
         TEMSEFIN_TAB-SURCHARGE  TO GS_TEMSEDETAILS-SURCHARGE,
         TEMSEFIN_TAB-BETRG      TO GS_TEMSEDETAILS-TOTTAX,
         TEMSEFIN_TAB-CHKNO      TO GS_TEMSEDETAILS-CHKNO,
         TEMSEFIN_TAB-BRCOD      TO GS_TEMSEDETAILS-BR_CODE,
         TEMSEFIN_TAB-CHDAT      TO GS_TEMSEDETAILS-DT_CHALLAN,
         TEMSEFIN_TAB-CHLNO      TO GS_TEMSEDETAILS-BANK_CHALLAN .
         APPEND GS_TEMSEDETAILS TO GT_TEMSEDETAILS.
      SLNO = SLNO + 1.
       CLEAR : TEMSEFIN_TAB,GS_TEMSEDETAILS.
    ENDLOOP.
    "Anees
  DATA: leaving TYPE dats.
  select single begda from pa0302 into leaving where pernr = FINAL_TAB-PERNR    "Only for left employees
                                                            and massn = 'I7'.
  IF sy-subrc = 0.
    LOOP AT TEMSEFIN_TAB WHERE PERNR = FINAL_TAB-PERNR
                            AND FPBEG >= FINAL_TAB-F16_BEGDA
                            AND FPEND >= FINAL_TAB-F16_ENDDA.   "For appending left month tax to output
      READ TABLE  GT_TEMSEDETAILS INTO GS_TEMSEDETAILS WITH KEY INCOMETAX = TEMSEFIN_TAB-INCOMETAX
                                                                  EDUCESS = TEMSEFIN_TAB-EDUCESS
                                                                  SURCHARGE = TEMSEFIN_TAB-SURCHARGE
                                                                  TOTTAX = TEMSEFIN_TAB-BETRG
                                                                  CHKNO = TEMSEFIN_TAB-CHKNO
                                                                  BR_CODE = TEMSEFIN_TAB-BRCOD
                                                                  DT_CHALLAN = TEMSEFIN_TAB-CHDAT
                                                                  BANK_CHALLAN = TEMSEFIN_TAB-CHLNO.
      IF sy-subrc <> 0.
      GS_TEMSEDETAILS-SL_NO = SLNO.
       MOVE :
         TEMSEFIN_TAB-INCOMETAX  TO GS_TEMSEDETAILS-INCOMETAX,
         TEMSEFIN_TAB-EDUCESS    TO GS_TEMSEDETAILS-EDUCESS,
         TEMSEFIN_TAB-SURCHARGE  TO GS_TEMSEDETAILS-SURCHARGE,
         TEMSEFIN_TAB-BETRG      TO GS_TEMSEDETAILS-TOTTAX,
         TEMSEFIN_TAB-CHKNO      TO GS_TEMSEDETAILS-CHKNO,
         TEMSEFIN_TAB-BRCOD      TO GS_TEMSEDETAILS-BR_CODE,
         TEMSEFIN_TAB-CHDAT      TO GS_TEMSEDETAILS-DT_CHALLAN,
         TEMSEFIN_TAB-CHLNO      TO GS_TEMSEDETAILS-BANK_CHALLAN .
         APPEND GS_TEMSEDETAILS TO GT_TEMSEDETAILS.
      SLNO = SLNO + 1.
       CLEAR : TEMSEFIN_TAB,GS_TEMSEDETAILS.
      ENDIF.
     ENDLOOP.
  ENDIF.
  DELETE GT_TEMSEDETAILS WHERE INCOMETAX is INITIAL AND EDUCESS IS INITIAL AND SURCHARGE is INITIAL AND TOTTAX is INITIAL.
    "End
   ELSE.

    SORT TEMSEFIN_TAB BY PAYDATE."1274331""GG"
      LOOP AT TEMSEFIN_TAB WHERE PERNR = FINAL_TAB-PERNR
                            AND FPBEG >= FINAL_TAB-F16_BEGDA
                            AND FPEND <= FINAL_TAB-F16_ENDDA.    "MDSNT927906
*      IF TEMSEFIN_TAB-INCOMETAX  IS INITIAL."gg1274331"
*        CONTINUE.
*      ENDIF.

      GS_TEMSEDETAILS-SL_NO = SLNO.
       MOVE :
         TEMSEFIN_TAB-INCOMETAX  TO GS_TEMSEDETAILS-INCOMETAX,
         TEMSEFIN_TAB-EDUCESS    TO GS_TEMSEDETAILS-EDUCESS,
         TEMSEFIN_TAB-SURCHARGE  TO GS_TEMSEDETAILS-SURCHARGE,
         TEMSEFIN_TAB-BETRG      TO GS_TEMSEDETAILS-TOTTAX,
         TEMSEFIN_TAB-CHKNO      TO GS_TEMSEDETAILS-CHKNO,
         TEMSEFIN_TAB-BRCOD      TO GS_TEMSEDETAILS-BR_CODE,
         TEMSEFIN_TAB-CHDAT      TO GS_TEMSEDETAILS-DT_CHALLAN,
         TEMSEFIN_TAB-CHLNO      TO GS_TEMSEDETAILS-BANK_CHALLAN .
         APPEND GS_TEMSEDETAILS TO GT_TEMSEDETAILS.
      SLNO = SLNO + 1.
       CLEAR : TEMSEFIN_TAB,GS_TEMSEDETAILS.
    ENDLOOP.

ENDIF.
* Section 10 components

    LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                            CNTR2 = FINAL_TAB-CNTR2.
      IF SEC10_TAB-SIGN = -1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
      ELSE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
      ENDIF.
      clear SEC10_TAB_AMOUNT.
      PERFORM sign_algmnt USING SEC10_TAB-AMOUNT
                            CHANGING SEC10_TAB_AMOUNT.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
********START OF COMMENTING************
      CLEAR GS_ANNEXURE.
      MOVE : SEC10_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
        SEC10_TAB_AMOUNT_S = SEC10_TAB-AMOUNT.
        IF SEC10_TAB-SIGN = -1.
          CONCATENATE '-' SEC10_TAB_AMOUNT_S into SEC10_TAB_AMOUNT_S.
        ELSE.
          CONCATENATE '' SEC10_TAB_AMOUNT_S into  SEC10_TAB_AMOUNT_S.
        ENDIF.
        CONDENSE SEC10_TAB_AMOUNT_S.
       clear SEC10_TAB_AMOUNT.
       PERFORM sign_algmnt USING SEC10_TAB_AMOUNT_S
                            CHANGING SEC10_TAB_AMOUNT.
        MOVE : SEC10_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.

      APPEND GS_ANNEXURE TO GT_EXEMPTAB.
********END OF COMMENTING***************

    ENDLOOP.

*      IFOS components
    LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
********START OF COMMENTING************
      CLEAR GS_ANNEXURE.
      MOVE : IFOS_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
        IFOS_TAB_AMOUNT_S = IFOS_TAB-AMOUNT.
        IF IFOS_TAB-SIGN = -1.
          CONCATENATE '-' IFOS_TAB_AMOUNT_S into IFOS_TAB_AMOUNT_S.
        ELSE.
          CONCATENATE '' IFOS_TAB_AMOUNT_S into  IFOS_TAB_AMOUNT_S.
        ENDIF.
        CONDENSE IFOS_TAB_AMOUNT_S.
       clear IFOS_TAB_AMOUNT.
       PERFORM sign_algmnt USING IFOS_TAB_AMOUNT_S
                            CHANGING IFOS_TAB_AMOUNT.
        MOVE : IFOS_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.

      APPEND GS_ANNEXURE TO GT_IFOSTAB.
********END OF COMMENTING************

    ENDLOOP.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            I_NAME     = LV_FORMNAME
          IMPORTING
            E_FUNCNAME = GV_FMNAME.
      CATCH CX_ROOT INTO LV_W_CX_ROOT.
        LV_MESG = LV_W_CX_ROOT->GET_TEXT( ).
        MESSAGE E201(HRPADIN01) WITH LV_FORMNAME LV_MESG.
    ENDTRY.

  IF DSIGN <> 'X'.
    if pnpesscf = 'X'.
      GS_FPDOCPARAMS-FILLABLE = 'X'.
    endif.
    CALL FUNCTION GV_FMNAME
      EXPORTING
        /1BCDWB/DOCPARAMS = GS_FPDOCPARAMS
        F16_DISPLAY       = GS_F16DATA
        F16_DEDUCTIONS    = GT_DEDUCTIONS
        F16_REBATE        = GT_REBATE
        F16_BANKDETAILS   = GT_BANKDETAILS
        CHALLAN_DETAILS   = GT_TEMSEDETAILS
        ANNEXURE_IFOS     = GT_IFOSTAB
        ANNEXURE_EXEMPS   = GT_EXEMPTAB
      importing
        /1BCDWB/FORMOUTPUT = formoutput
      EXCEPTIONS
        USAGE_ERROR       = 1
        SYSTEM_ERROR      = 2
        INTERNAL_ERROR    = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    GS_FPDOCPARAMS-FILLABLE = 'X'.
    CALL FUNCTION GV_FMNAME
      EXPORTING
        /1BCDWB/DOCPARAMS = GS_FPDOCPARAMS
        F16_DISPLAY       = GS_F16DATA
        F16_DEDUCTIONS    = GT_DEDUCTIONS
        F16_REBATE        = GT_REBATE
        F16_BANKDETAILS   = GT_BANKDETAILS
        CHALLAN_DETAILS   = GT_TEMSEDETAILS
        ANNEXURE_IFOS     = GT_IFOSTAB
        ANNEXURE_EXEMPS   = GT_EXEMPTAB
      importing
        /1BCDWB/FORMOUTPUT = formoutput
      EXCEPTIONS
        USAGE_ERROR       = 1
        SYSTEM_ERROR      = 2
        INTERNAL_ERROR    = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

    CLEAR : GT_DEDUCTIONS,GT_REBATE,GT_BANKDETAILS,GT_TEMSEDETAILS.
********END OF CODING**************

* End of First Page

* Annexure
    IF DISP_FLG_LOT LT 1 AND LAYOUT1 NE ' '.
      FORMNAME1 = LAYOUT1.
    ELSE.
      FORMNAME1 = 'HR_IN_TAXF16NX_Y'.
    ENDIF.
    LANG = 'EN'.

* Gross components
    LOOP AT GROSS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                            CNTR2 = FINAL_TAB-CNTR2    .
********START OF COMMENTING************
      CLEAR GS_ANNEXURE.
      MOVE : GROSS_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
        GROSS_TAB_AMOUNT_S = GROSS_TAB-AMOUNT.
        IF GROSS_TAB-SIGN = -1.
          CONCATENATE '-' GROSS_TAB_AMOUNT_S into GROSS_TAB_AMOUNT_S.
        ELSE.
          CONCATENATE '' GROSS_TAB_AMOUNT_S into  GROSS_TAB_AMOUNT_S.
        ENDIF.
        CONDENSE GROSS_TAB_AMOUNT_S.
       clear GROSS_TAB_AMOUNT.
       PERFORM sign_algmnt USING GROSS_TAB_AMOUNT_S
                            CHANGING GROSS_TAB_AMOUNT.
        MOVE : GROSS_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.

      APPEND GS_ANNEXURE TO GT_GROSSTAB.
********END OF COMMENTING**************

    ENDLOOP.

* Perk components

    LOOP AT PERK_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
**********START OF PDF CODING************
      CLEAR GS_ANNEXURE.
      MOVE : PERK_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
        PERK_TAB_AMOUNT_S = PERK_TAB-AMOUNT.
        IF PERK_TAB-SIGN = -1.
          CONCATENATE '-' PERK_TAB_AMOUNT_S into PERK_TAB_AMOUNT_S.
        ELSE.
          CONCATENATE '' PERK_TAB_AMOUNT_S into  PERK_TAB_AMOUNT_S.
        ENDIF.
        CONDENSE PERK_TAB_AMOUNT_S.
       clear PERK_TAB_AMOUNT.
       PERFORM sign_algmnt USING PERK_TAB_AMOUNT_S
                            CHANGING PERK_TAB_AMOUNT.
        MOVE : PERK_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.

      APPEND GS_ANNEXURE TO GT_PERKSTAB.
********END OF COMMENTING************

    ENDLOOP.

**  IFOS components
*    LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
*                           CNTR2 = FINAL_TAB-CNTR2.
*********START OF COMMENTING************
*      CLEAR GS_ANNEXURE.
*      MOVE : IFOS_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
*        IFOS_TAB_AMOUNT_S = IFOS_TAB-AMOUNT.
*        IF IFOS_TAB-SIGN = -1.
*          CONCATENATE '-' IFOS_TAB_AMOUNT_S into IFOS_TAB_AMOUNT_S.
*        ELSE.
*          CONCATENATE '' IFOS_TAB_AMOUNT_S into  IFOS_TAB_AMOUNT_S.
*        ENDIF.
*        CONDENSE IFOS_TAB_AMOUNT_S.
*       clear IFOS_TAB_AMOUNT.
*       PERFORM sign_algmnt USING IFOS_TAB_AMOUNT_S
*                            CHANGING IFOS_TAB_AMOUNT.
*        MOVE : IFOS_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.
*
*      APPEND GS_ANNEXURE TO GT_IFOSTAB.
*********END OF COMMENTING************
*
*    ENDLOOP.
** Section 10 components
*
*    LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
*                            CNTR2 = FINAL_TAB-CNTR2.
*      IF SEC10_TAB-SIGN = -1.
*        PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
*      ELSE.
*        PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
*      ENDIF.
*      clear SEC10_TAB_AMOUNT.
*      PERFORM sign_algmnt USING SEC10_TAB-AMOUNT
*                            CHANGING SEC10_TAB_AMOUNT.
*      PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
*********START OF COMMENTING************
*      CLEAR GS_ANNEXURE.
*      MOVE : SEC10_TAB-SPEC_TXT TO GS_ANNEXURE-SPEC_TEXT.
*        SEC10_TAB_AMOUNT_S = SEC10_TAB-AMOUNT.
*        IF SEC10_TAB-SIGN = -1.
*          CONCATENATE '-' SEC10_TAB_AMOUNT_S into SEC10_TAB_AMOUNT_S.
*        ELSE.
*          CONCATENATE '' SEC10_TAB_AMOUNT_S into  SEC10_TAB_AMOUNT_S.
*        ENDIF.
*        CONDENSE SEC10_TAB_AMOUNT_S.
*       clear SEC10_TAB_AMOUNT.
*       PERFORM sign_algmnt USING SEC10_TAB_AMOUNT_S
*                            CHANGING SEC10_TAB_AMOUNT.
*        MOVE : SEC10_TAB_AMOUNT TO GS_ANNEXURE-AMOUNT.
*
*      APPEND GS_ANNEXURE TO GT_EXEMPTAB.
*********END OF COMMENTING***************
*
*    ENDLOOP.
********START OF COMMENTING************
    MOVE FORMNAME1 TO LV_FORMNAME.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            I_NAME     = LV_FORMNAME
          IMPORTING
            E_FUNCNAME = GV_FMNAME.
      CATCH CX_ROOT INTO LV_W_CX_ROOT.
        LV_MESG = LV_W_CX_ROOT->GET_TEXT( ).
        MESSAGE E201(HRPADIN01) WITH LV_FORMNAME LV_MESG.
    ENDTRY.

    CALL FUNCTION GV_FMNAME
      EXPORTING
        /1BCDWB/DOCPARAMS = GS_FPDOCPARAMS
        F16_DISPLAY       = GS_F16DATA
        ANNEXURE_GROSS    = GT_GROSSTAB
        ANNEXURE_PERKS    = GT_PERKSTAB
        ANNEXURE_IFOS     = GT_IFOSTAB
        ANNEXURE_EXEMPS   = GT_EXEMPTAB
      IMPORTING
        /1BCDWB/FORMOUTPUT = FORMANNEX
      EXCEPTIONS
        USAGE_ERROR       = 1
        SYSTEM_ERROR      = 2
        INTERNAL_ERROR    = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

********END OF CODING***************
* End of Annexure

* End of Second Page

* Second Annexure For Form 12BA

* FORM 12BA IS TO BE PRINTED ONLY IF SALARY PAID OR SALARY PAYABLE IS
* GREATER THAN THE AMOUNT IN THE CONSTANT F12BA IN T511P

    IF DISP_FLG_LOT LT 1 AND LAYOUT2 NE ' '.
      FORMNAME2 = LAYOUT2.
    ELSE.
      FORMNAME2 = 'HR_IN_TAXF16NX_P'.
    ENDIF.
    LANG = 'EN'.
*--- Compute for sl. no.6 of form 12BA------------------*
    F12BA_BALANCE = SAL_SEC17 + PROF_F12BA - FINAL_TAB-SEC10_ALL.
    SNAME = '40INDED'.
    ENDDATE = PENDDA.
    PERFORM RE596F USING SNAME ENDDATE.
    PERFORM (T596F-MODNA) IN PROGRAM (T596F-PGMNA)
                         USING F12BA_BALANCE
                               ENDDATE
                         CHANGING F12BA_STD_DED.
    SALARIES_WO_PERK = F12BA_BALANCE - ( F12BA_STD_DED + ENT_ALL +
                                         FINAL_TAB-PTAX ).

*---------------------------------------------------------*

*  FILLING OF THE MAIN WINDOW.*****************
*********START OF PDF CHANGE *****************
    MOVE : SALARIES_WO_PERK TO GS_F16DATA-SAL_WO_WORK,
           FIN_START TO  GS_F16DATA-FIN_START,
           FIN_END TO  GS_F16DATA-FIN_END.
*********END OF PDF CHANGE *****************
    LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                               CNTR2 = FINAL_TAB-CNTR2.
      IF FORM12BA_TAB-EVCLS_SPEC NE 17.

        TOT_VAL_PERK = TOT_VAL_PERK + FORM12BA_TAB-VAL_PERK.
        TOT_EERECVR = TOT_EERECVR + FORM12BA_TAB-EERECVR.
        TOT_TAX_PERK = TOT_TAX_PERK + FORM12BA_TAB-TAX_PERK.
      ENDIF.
*********START OF PDF CHANGE *****************
      CASE FORM12BA_TAB-EVCLS_SPEC.
        WHEN 1.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE1,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE1,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE1.
        WHEN 2.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE2,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE2,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE2.

        WHEN 3.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE3,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE3,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE3.
        WHEN 4.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE4,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE4,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE4.

        WHEN 5.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE5,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE5,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE5.

        WHEN 6.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE6,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE6,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE6.

        WHEN 7.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE7,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE7,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE7.

        WHEN 8.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE8,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE8,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE8.

        WHEN 9.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE9,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE9,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE9.

        WHEN 10.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE10,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE10,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE10.

        WHEN 11.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE11,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE11,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE11.

        WHEN 12.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE12,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE12,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE12.

        WHEN 13.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE13,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE13,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE13.

        WHEN 14.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE14,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE14,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE14.

        WHEN 15.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE15,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE15,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE15.

        WHEN 16.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE16,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE16,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE16.

        WHEN 17.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE17,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE17,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE17.
        WHEN 18.
          MOVE : FORM12BA_TAB-VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE18,
                 FORM12BA_TAB-EERECVR TO GS_F12BADETAILS-EERECVR-VALUE18,
                 FORM12BA_TAB-TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE18.

      ENDCASE.
      MOVE : TOT_VAL_PERK TO GS_F12BADETAILS-VAL_PERK-VALUE19,
             TOT_EERECVR TO GS_F12BADETAILS-EERECVR-VALUE19,
             TOT_TAX_PERK TO GS_F12BADETAILS-TAX_PERK-VALUE19.

*********END OF PDF CHANGE *****************

    ENDLOOP.
*  FILLING OF THE MAIN WINDOW.*****************
**********START OF PDF CODING*************
    MOVE FORMNAME2 TO LV_FORMNAME.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            I_NAME     = LV_FORMNAME
          IMPORTING
            E_FUNCNAME = GV_FMNAME.
      CATCH CX_ROOT INTO LV_W_CX_ROOT.
        LV_MESG = LV_W_CX_ROOT->GET_TEXT( ).
        MESSAGE E201(HRPADIN01) WITH LV_FORMNAME LV_MESG.
    ENDTRY.

    CALL FUNCTION GV_FMNAME
      EXPORTING
        /1BCDWB/DOCPARAMS = GS_FPDOCPARAMS
        F16_DISPLAY       = GS_F16DATA
        F12BA_PERQUISITES = GS_F12BADETAILS
      IMPORTING
        /1BCDWB/FORMOUTPUT = FORM12BA
      EXCEPTIONS
        USAGE_ERROR       = 1
        SYSTEM_ERROR      = 2
        INTERNAL_ERROR    = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* COntrol log changes 09/11/2009.

  wa_cont_log-pernr = FINAL_TAB-PERNR.
  wa_cont_log-BEGDA = f16_begda.
  wa_cont_log-endda = f16_endda.
  wa_cont_log-date = sy-datum.
  wa_cont_log-time = SY-UZEIT.
  wa_cont_log-year = year.
  APPEND wa_cont_log to control_log.
**********END OF PDF CODING*************

    IF FORMNAME2 = LAYOUT2.
********START OF COMMENTING************

*      PERFORM end_form.
*
*      PERFORM open_form USING lang.
*
*      PERFORM start_form USING formname2 lang 'PAGE2'.
*
*      PERFORM write_form USING '' 'APPEND' 'BODY' 'FLEFT1'.
*      PERFORM write_form USING '' 'APPEND' 'BODY' 'DETAILS1'.
*      PERFORM write_form USING '' 'APPEND' 'BODY' 'FOOTER'.
********END OF COMMENTING************

    ENDIF.

********  ENDLOOP.        "FINAL_TAB
****************START OF COMMENTING************
********  CALL FUNCTION 'FP_JOB_CLOSE'
********* IMPORTING
*********   E_RESULT             =
********   EXCEPTIONS
********     USAGE_ERROR          = 1
********     SYSTEM_ERROR         = 2
********     INTERNAL_ERROR       = 3
********     OTHERS               = 4
********            .
********  IF SY-SUBRC <> 0.
********* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*********         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
********  ENDIF.

********END OF COMMENTING************

  IF NOT dsign IS INITIAL.
*   Digitally sign the document and download
    CNTR_REC = final_tab-cntr1.
*   get FP reference
  l_fp = cl_fp=>get_reference( ).

  try.
*     create PDF Object
      l_pdfobj = l_fp->create_pdf_object( connection = p_dest ).

*     set document
    call method l_pdfobj->set_document
      exporting
        pdfdata = formoutput-PDF.

*     set signature
      call method l_pdfobj->set_signature
        exporting
*          keyname     = s_key
          fieldname   = s_field.
*          reason      = s_reason
*          location    = s_loc
*          contactinfo = s_cinfo.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_f16 = l_out.

  catch cx_fp_runtime_internal into l_fpex.
    perform error using l_fpex 'INTERNAL ERROR'.
  catch cx_fp_runtime_system into l_fpex.
    perform error using l_fpex 'SYSTEM ERROR'.
  catch cx_fp_runtime_usage into l_fpex.
    perform error using l_fpex 'USAGE ERROR'.
  endtry.

*   download PDF
    data: l_len      type i,
          l_tab      type tsfixml.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer                = l_f16
      importing
        output_length         = l_len
      tables
        binary_tab            = l_tab.
    IF cntr_f16 <= 1.
      concatenate folpath FINAL_TAB-PERNR '_Form 16.PDF'
      into l_filename_out.
    else.
      concatenate folpath FINAL_TAB-PERNR '_' cntr_rec '_Form 16.PDF'
      into l_filename_out.
    endif.
  IF NOT FOLPATH IS INITIAL.
    call method cl_gui_frontend_services=>gui_download
      exporting
         bin_filesize            = l_len
         filename                = l_filename_out
         filetype                = 'BIN'
      changing
         data_tab                = l_tab
      exceptions
         others                  = 1.
    if sy-subrc = 0.
*      write:/ 'Datei erfolgreich geschrieben'(001).
    endif.
  ENDIF.
  clear l_out.
* Download Form 12BA
   try.
*     set document
      call method l_pdfobj->set_document
        exporting
          pdfdata = form12ba-PDF.

*     set signature
      call method l_pdfobj->set_signature
        exporting
*          keyname     = s_key
          fieldname   = s_field1.
*          reason      = s_reason
*          location    = s_loc
*          contactinfo = s_cinfo.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_12ba = l_out.

    catch cx_fp_runtime_internal into l_fpex.
      perform error using l_fpex 'INTERNAL ERROR'.
    catch cx_fp_runtime_system into l_fpex.
      perform error using l_fpex 'SYSTEM ERROR'.
    catch cx_fp_runtime_usage into l_fpex.
      perform error using l_fpex 'USAGE ERROR'.
    endtry.

*   download PDF
  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer                = l_12ba
    importing
      output_length         = l_len
    tables
      binary_tab            = l_tab.
    IF CNTR_F16 <= 1.
      concatenate folpath FINAL_TAB-PERNR '_Form 12BA.PDF'
      into l_filename_out.
    ELSE.
      concatenate folpath FINAL_TAB-PERNR '_' cntr_rec '_Form 12BA.PDF'
      into l_filename_out.
    ENDIF.
  IF NOT FOLPATH IS INITIAL.
    call method cl_gui_frontend_services=>gui_download
      exporting
         bin_filesize            = l_len
         filename                = l_filename_out
         filetype                = 'BIN'
      changing
         data_tab                = l_tab
      exceptions
         others                  = 1.
    if sy-subrc = 0.
*      write:/ 'Datei erfolgreich geschrieben'(001).
    endif.
  ENDIF.
  clear l_out.
* download Annexure to Form 16
   try.
*     set document
      call method l_pdfobj->set_document
        exporting
          pdfdata = formannex-PDF.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_annex = l_out.

    catch cx_fp_runtime_internal into l_fpex.
      perform error using l_fpex 'INTERNAL ERROR'.
    catch cx_fp_runtime_system into l_fpex.
      perform error using l_fpex 'SYSTEM ERROR'.
    catch cx_fp_runtime_usage into l_fpex.
      perform error using l_fpex 'USAGE ERROR'.
    endtry.

*   download PDF
    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer                = l_annex
      importing
        output_length         = l_len
      tables
        binary_tab            = l_tab.

   IF cntr_f16 <= 1.
    concatenate folpath FINAL_TAB-PERNR '_Annex.PDF'
    into l_filename_out.
   ELSE.
    concatenate folpath FINAL_TAB-PERNR '_' cntr_rec '_Annex.PDF'
    into l_filename_out.
   ENDIF.
  IF NOT FOLPATH IS INITIAL.
    call method cl_gui_frontend_services=>gui_download
    exporting
       bin_filesize            = l_len
       filename                = l_filename_out
       filetype                = 'BIN'
    changing
       data_tab                = l_tab
    exceptions
       others                  = 1.
  if sy-subrc = 0.
*      write:/ 'Datei erfolgreich geschrieben'(001).
    endif.
  ENDIF.

 ENDIF.

*** Send e-Mail

  IF DMAIL = 'X'.
    PERFORM email_pdf_output USING l_f16
                                   final_tab-ename 'Form 16'.
    PERFORM email_pdf_output USING l_12ba
                                   final_tab-ename 'Form 12BA'.
    PERFORM email_pdf_output USING l_annex
                                   final_tab-ename 'Annexure to Form 16'.

  ENDIF.


  ENDLOOP.        "FINAL_TAB
  IF pnpesscf = 'X'.
*   get FP reference
  l_fp = cl_fp=>get_reference( ).

  try.
*     create PDF Object
      l_pdfobj = l_fp->create_pdf_object( connection = p_dest ).

*     set document
    call method l_pdfobj->set_document
      exporting
        pdfdata = formoutput-PDF.

**     set signature
*      call method l_pdfobj->set_signature
*        exporting
**          keyname     = s_key
*          fieldname   = s_field.
**          reason      = s_reason
**          location    = s_loc
**          contactinfo = s_cinfo.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_f16 = l_out.

  catch cx_fp_runtime_internal into l_fpex.
    perform error using l_fpex 'INTERNAL ERROR'.
  catch cx_fp_runtime_system into l_fpex.
    perform error using l_fpex 'SYSTEM ERROR'.
  catch cx_fp_runtime_usage into l_fpex.
    perform error using l_fpex 'USAGE ERROR'.
  endtry.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer                = l_f16
      importing
        output_length         = l_len
      tables
        binary_tab            = l_tab.

  try.
*     set document
      call method l_pdfobj->set_document
        exporting
          pdfdata = form12ba-PDF.

*     set signature
      call method l_pdfobj->set_signature
        exporting
*          keyname     = s_key
          fieldname   = s_field1.
*          reason      = s_reason
*          location    = s_loc
*          contactinfo = s_cinfo.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_12ba = l_out.

    catch cx_fp_runtime_internal into l_fpex.
      perform error using l_fpex 'INTERNAL ERROR'.
    catch cx_fp_runtime_system into l_fpex.
      perform error using l_fpex 'SYSTEM ERROR'.
    catch cx_fp_runtime_usage into l_fpex.
      perform error using l_fpex 'USAGE ERROR'.
    endtry.

*   download PDF
  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer                = l_12ba
    importing
      output_length         = l_len
    tables
      binary_tab            = l_tab.

* download Annexure to Form 16
   try.
*     set document
      call method l_pdfobj->set_document
        exporting
          pdfdata = formannex-PDF.

*     execute, call ADS
      call method l_pdfobj->execute( ).

      call method l_pdfobj->get_document
        importing
          pdfdata = l_out.

      l_annex = l_out.

    catch cx_fp_runtime_internal into l_fpex.
      perform error using l_fpex 'INTERNAL ERROR'.
    catch cx_fp_runtime_system into l_fpex.
      perform error using l_fpex 'SYSTEM ERROR'.
    catch cx_fp_runtime_usage into l_fpex.
      perform error using l_fpex 'USAGE ERROR'.
    endtry.

*   download PDF
    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer                = l_annex
      importing
        output_length         = l_len
      tables
        binary_tab            = l_tab.
*    CONCATENATE l_f16 l_12ba l_annex INTO pdf_string_x IN BYTE MODE.
      pdf_string_x = l_f16.
      EXPORT pdf_string_x TO MEMORY ID 'PDFF'.
  ENDIF.

  IF NOT dsign IS INITIAL.

          TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
               I_NAME     =  'HR_IN_CONTRLOG'
              IMPORTING
               E_FUNCNAME = GV_FMNAME.
             CATCH CX_ROOT INTO LV_W_CX_ROOT.
              LV_MESG = LV_W_CX_ROOT->GET_TEXT( ).
              MESSAGE E201(HRPADIN01) WITH LV_FORMNAME LV_MESG.
          ENDTRY.

        CALL FUNCTION GV_FMNAME
         EXPORTING
           /1BCDWB/DOCPARAMS        = GS_FPDOCPARAMS
           CONTLOG                  = control_log
         IMPORTING
           /1BCDWB/FORMOUTPUT       = contlogform
         EXCEPTIONS
           USAGE_ERROR              = 1
           SYSTEM_ERROR             = 2
           INTERNAL_ERROR           = 3
           OTHERS                   = 4
                  .
        IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      try.
*     create PDF Object
          l_pdfobj = l_fp->create_pdf_object( connection = p_dest ).

*     set document
          call method l_pdfobj->set_document
            EXPORTING
              pdfdata = contlogform-PDF.

**     set signature
          call method l_pdfobj->set_signature
            exporting
*          keyname     = s_key
              fieldname   = s_field10.
*          reason      = s_reason
*          location    = s_loc
*          contactinfo = s_cinfo.

*     execute, call ADS
          call method l_pdfobj->execute( ).

          call method l_pdfobj->get_document
            IMPORTING
              pdfdata = l_out.

          l_f16 = l_out.

        catch cx_fp_runtime_internal into l_fpex.
          perform error using l_fpex 'INTERNAL ERROR'.
        catch cx_fp_runtime_system into l_fpex.
          perform error using l_fpex 'SYSTEM ERROR'.
        catch cx_fp_runtime_usage into l_fpex.
          perform error using l_fpex 'USAGE ERROR'.
      endtry.

*   download PDF
*      data: l_len      type i,
*            l_tab      type tsfixml.

      call function 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = l_out
        IMPORTING
          output_length = l_len
        TABLES
          binary_tab    = l_tab.

      concatenate folpath sy-datum '_' sy-uzeit '_controllog.PDF'
      into l_filename_out.

      IF NOT FOLPATH IS INITIAL.
        call method cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize = l_len
            filename     = l_filename_out
            filetype     = 'BIN'
          CHANGING
            data_tab     = l_tab
          EXCEPTIONS
            others       = 1.
        if sy-subrc = 0.
*      write:/ 'Datei erfolgreich geschrieben'(001).
        endif.
      ENDIF.
    ENDIF.

********START OF COMMENTING************
  CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
   EXCEPTIONS
     USAGE_ERROR          = 1
     SYSTEM_ERROR         = 2
     INTERNAL_ERROR       = 3
     OTHERS               = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "PRINT_MODULE_PDF



*&--------------------------------------------------------------------*
*&      Form  job_open_pdf
*&--------------------------------------------------------------------*
*       Opening the Spool
*---------------------------------------------------------------------*
*      -->IV_LANG    Language
*---------------------------------------------------------------------*
FORM JOB_OPEN_PDF USING IV_LANG TYPE SYLANGU.

  DATA: LS_OPTIONS TYPE ITCPO.
  DATA: LV_DIALOG TYPE C.
  DATA: LS_PRI_PARAMS TYPE PRI_PARAMS.

*  IF PNPESSCF = GC_X.
**    LS_OPTIONS-TDNOPRINT = GC_X.
**    LS_OPTIONS-TDPREVIEW = GC_X.
**    LS_OPTIONS-TDTITLE = SPACE.
**    LS_OPTIONS-TDGETOTF = GC_X.
**    LS_OPTIONS-TDPRINTER = 'POSTSCPT'.
*
*    LV_DIALOG = GC_X.
*  ELSE.
    LV_DIALOG = GC_X.
*  ENDIF.
* END OF ESS CHANGES

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      NO_DIALOG      = GC_X
      MODE           = 'CURRENT'
    IMPORTING
      OUT_PARAMETERS = LS_PRI_PARAMS.

  LS_OPTIONS-TDCOPIES   = LS_PRI_PARAMS-PRCOP.
  LS_OPTIONS-TDDEST     = LS_PRI_PARAMS-PDEST.
  LS_OPTIONS-TDNEWID    = LS_PRI_PARAMS-PRNEW.
  LS_OPTIONS-TDIMMED    = LS_PRI_PARAMS-PRIMM.
  LS_OPTIONS-TDDELETE   = LS_PRI_PARAMS-PRREL.
  LS_OPTIONS-TDLIFETIME = LS_PRI_PARAMS-PEXPI.
  LS_OPTIONS-TDTITLE    = LS_PRI_PARAMS-PRTXT.
  LS_OPTIONS-TDCOVER    = LS_PRI_PARAMS-PRSAP.
  LS_OPTIONS-TDCOVTITLE = LS_PRI_PARAMS-PRTXT.
  LS_OPTIONS-TDRECEIVER = LS_PRI_PARAMS-PRREC.
  LS_OPTIONS-TDDIVISION = LS_PRI_PARAMS-PRABT.
  LS_OPTIONS-TDAUTORITY = LS_PRI_PARAMS-PRBER.

  PERFORM FILL_OUTPUTPARAMS USING LS_OPTIONS
                            CHANGING GS_FPOUTPARAMS.
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      IE_OUTPUTPARAMS = GS_FPOUTPARAMS
    EXCEPTIONS
      CANCEL          = 1
      USAGE_ERROR     = 2
      SYSTEM_ERROR    = 3
      INTERNAL_ERROR  = 4
      OTHERS          = 5.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    "job_open_pdf

*&---------------------------------------------------------------------*
*&      Form  fill_outputparams
*&---------------------------------------------------------------------*
*This form will fill all the print parameters for the form
*---------------------------------------------------------------------*
*  -->  IS_ITCPO Print Options of script
*  <->  XS_OUTPUTPARAMS Out Parameters for PDF .
*---------------------------------------------------------------------*
FORM FILL_OUTPUTPARAMS USING    IS_ITCPO                 TYPE ITCPO
                                 CHANGING XS_OUTPUTPARAMS TYPE SFPOUTPUTPARAMS.
IF PNPESSCF = ' '.
  XS_OUTPUTPARAMS-DEVICE     = IS_ITCPO-TDPRINTER.
*  XS_OUTPUTPARAMS-NODIALOG   = GC_X.
  XS_OUTPUTPARAMS-PREVIEW    = IS_ITCPO-TDPREVIEW.
  XS_OUTPUTPARAMS-DEST       = IS_ITCPO-TDDEST.
  XS_OUTPUTPARAMS-REQNEW     = IS_ITCPO-TDNEWID.
  XS_OUTPUTPARAMS-REQIMM     = IS_ITCPO-TDIMMED.
  XS_OUTPUTPARAMS-REQDEL     = IS_ITCPO-TDDELETE.
  XS_OUTPUTPARAMS-REQFINAL   = IS_ITCPO-TDFINAL.
  XS_OUTPUTPARAMS-SENDDATE   = IS_ITCPO-TDSENDDATE.
  XS_OUTPUTPARAMS-SENDTIME   = IS_ITCPO-TDSENDTIME.
  XS_OUTPUTPARAMS-SCHEDULE   = IS_ITCPO-TDSCHEDULE.
  XS_OUTPUTPARAMS-COPIES     = IS_ITCPO-TDCOPIES.
  XS_OUTPUTPARAMS-DATASET    = IS_ITCPO-TDDATASET.
  XS_OUTPUTPARAMS-SUFFIX1    = IS_ITCPO-TDSUFFIX1.
  XS_OUTPUTPARAMS-SUFFIX2    = IS_ITCPO-TDSUFFIX2.
  XS_OUTPUTPARAMS-COVTITLE   = IS_ITCPO-TDCOVTITLE.
  XS_OUTPUTPARAMS-COVER      = IS_ITCPO-TDCOVER.
  XS_OUTPUTPARAMS-RECEIVER   = IS_ITCPO-TDRECEIVER.
  XS_OUTPUTPARAMS-DIVISION   = IS_ITCPO-TDDIVISION.
  XS_OUTPUTPARAMS-LIFETIME   = IS_ITCPO-TDLIFETIME.
  XS_OUTPUTPARAMS-AUTHORITY  = IS_ITCPO-TDAUTORITY.
  XS_OUTPUTPARAMS-RQPOSNAME  = IS_ITCPO-RQPOSNAME.
  XS_OUTPUTPARAMS-ARCMODE    = IS_ITCPO-TDARMOD.
  XS_OUTPUTPARAMS-NOARMCH    = IS_ITCPO-TDNOARMCH.
  XS_OUTPUTPARAMS-TITLE      = IS_ITCPO-TDTITLE.
  XS_OUTPUTPARAMS-NOPREVIEW  = IS_ITCPO-TDNOPREV.
  XS_OUTPUTPARAMS-NOPRINT    = IS_ITCPO-TDNOPRINT.
  IF DSIGN = 'X'.
    XS_OUTPUTPARAMS-JOB_PROFILE = 'Custom/Print/Remove_Annote'.
  ENDIF.

  IF DSIGN = 'X'.
    GS_FPOUTPARAMS-nodialog = 'X'. " suppress printer dialog popup
    GS_FPOUTPARAMS-GETPDF = 'X'. " launch print preview
  ENDIF.
ELSE.
    GS_FPOUTPARAMS-nodialog = 'X'. " suppress printer dialog popup
    GS_FPOUTPARAMS-GETPDF = 'X'. " launch print preview
    GS_FPOUTPARAMS-preview   = 'X'.
ENDIF.
ENDFORM.                    " fill_outputparams


***********END OF PDF CODING*****************
*&---------------------------------------------------------------------*
*&      Form  DRAW_TDSBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM DRAW_TDSBOX TABLES IN_TAB STRUCTURE ITCSY
                   OUT_TAB STRUCTURE ITCSY.
  DATA: CH(2) TYPE C.
  COUNTER = COUNTER + 1.
  MOVE COUNTER TO CH.
  IN_TAB-NAME = 'YP'.
  IN_TAB-VALUE = CH.
  APPEND IN_TAB.
  OUT_TAB-NAME = 'YP'.
  OUT_TAB-VALUE = CH.
  APPEND OUT_TAB.

ENDFORM.                    "DRAW_TDSBOX

*&---------------------------------------------------------------------*
*&      Form  WRITEFORM1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*

form writeform1 TABLES in_par STRUCTURE itcsy
                       out_par STRUCTURE itcsy.

  loop at in_par where name = 'PAGE'.
    pagenum = in_par-value.
  endloop.
endform.                                                    "writeform1

*&---------------------------------------------------------------------*
*&      Form  WRITEFORM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*

form writeform2 TABLES in_par STRUCTURE itcsy
                       out_par STRUCTURE itcsy.

  loop at out_par where name = 'PAGE'.
    out_par-value = pagenum + 1.
    modify out_par.
  endloop.
endform.                                                    "writeform2

*&---------------------------------------------------------------------*
*&      Form  reset_counter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*

form reset_counter TABLES in_par STRUCTURE itcsy
                       out_par STRUCTURE itcsy.
    PAGENUM = '4'.
endform.                    "reset_counter
*&---------------------------------------------------------------------*
*&      Form  SIGN_ALGMNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FINAL_TAB_OTH_INCOME  text
*      <--P_OTH_INCOME_S  text
*----------------------------------------------------------------------*
FORM SIGN_ALGMNT  USING    P_FINAL_TAB_OTH_INCOME
                  CHANGING P_OTH_INCOME_S.
  DATA: TEXT1(1) TYPE C.
  DATA: value(20) type c.
  value = P_FINAL_TAB_OTH_INCOME.
  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
  P_OTH_INCOME_S = value.
ENDFORM.                    " SIGN_ALGMNT

*&---------------------------------------------------------------------*
*&      Form  error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FPEX     text
*      -->P_STR      text
*----------------------------------------------------------------------*
form error using p_fpex type ref to cx_fp_runtime
                 p_str  type string.
data: l_errcode  type i,
      l_errmsg   type string,
      l_string   type string.

  write:/ '***************************************************'.
  write:/ '***', p_str.
  write:/ '***************************************************'.
  skip 2.

  call method p_fpex->get_errall
    importing
      errcode  = l_errcode
      errmsg   = l_errmsg.

  write:/ 'ERROR CODE       : ', l_errcode.
  write:/ 'ERROR MESSAGE    : ', l_errmsg.

  l_string = p_fpex->get_text( ).
  write:/ l_string.

endform.                    "error





*&---------------------------------------------------------------------*
*&      Form  EMAIL_PDF_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FINAL_TAB_OTH_INCOME  text
*      <--P_OTH_INCOME_S  text
*----------------------------------------------------------------------*

FORM email_pdf_output USING ls_formoutput TYPE xstring
                            p_ename desc.

* email setting's data def
  DATA: ls_doc_data TYPE sodocchgi1.
  DATA:
  lt_receivers       TYPE TABLE OF somlreci1 WITH HEADER LINE,
  lt_objpack         TYPE TABLE OF sopcklsti1 WITH HEADER LINE,
  lt_contents        TYPE TABLE OF solix,
  ls_objpack         TYPE sopcklsti1,
  l_mail_des(255)    type c.

  DATA  lp_offset TYPE i.
  DATA  lt_solix TYPE solix_tab.
  DATA  ls_solix_line TYPE solix.
  DATA  ls_archive_params TYPE toa_dara.
  DATA  lp_pdf_string_len TYPE i.
  DATA  lp_solix_rows TYPE i.
  DATA  lp_last_row_length TYPE i.
  DATA  lp_row_length TYPE i.
  DATA  lp_doc_length TYPE so_obj_len.
  DATA: lv_tablines_mail TYPE i.
  DATA: l_mailid TYPE comm_id_long.
  DATA: length1 type i.
  DATA: length2 type i.

******* get mailing data

* get document data
  DATA: lv_description(255) TYPE c,
        l_text(40) type c.

  CONCATENATE desc 'for ' p_ename
              INTO lv_description SEPARATED BY space.

  ls_doc_data-obj_name   = l_text.
  ls_doc_data-obj_descr  = lv_description.
  ls_doc_data-obj_langu  = sy-langu.
  ls_doc_data-obj_sort   = sy-datum.
  ls_doc_data-obj_expdat = sy-datum + 30.
  ls_doc_data-sensitivty = 'O'.
  ls_doc_data-obj_prio   = '1'.
  ls_doc_data-no_change  = 'X'.
  ls_doc_data-proc_type  = 'T'.
  ls_doc_data-proc_name  = 'SP01'.
  ls_doc_data-proc_syst  = sy-sysid.
  ls_doc_data-proc_clint = sy-mandt.
  ls_doc_data-skip_scren = ' '.
  ls_doc_data-to_do_out  = ' '.
  ls_doc_data-free_del   = ' '.
  ls_doc_data-doc_size   = XSTRLEN( ls_formoutput ).

* * transform xstring to SOLIX table format for the PDF form data
  DESCRIBE TABLE lt_solix.
  lp_row_length = sy-tleng.
  lp_offset = 0.

  lp_pdf_string_len = XSTRLEN( ls_formoutput ).

  lp_solix_rows = lp_pdf_string_len DIV lp_row_length.
  lp_last_row_length = lp_pdf_string_len MOD lp_row_length.
  DO lp_solix_rows TIMES.
    ls_solix_line-line =
           ls_formoutput+lp_offset(lp_row_length).
    APPEND ls_solix_line TO lt_contents.
    ADD lp_row_length TO lp_offset.
  ENDDO.
  IF lp_last_row_length > 0.
    CLEAR ls_solix_line-line.
    ls_solix_line-line = ls_formoutput+lp_offset(lp_last_row_length).
    APPEND ls_solix_line TO lt_contents.
  ENDIF.

  DESCRIBE TABLE lt_contents LINES lv_tablines_mail.
  length2 = lv_tablines_mail.
  length1 = 1.
* get packaging list
  CLEAR ls_objpack.
  ls_objpack-transf_bin = 'X'.
  ls_objpack-obj_descr  = desc.
  ls_objpack-obj_langu  = sy-ucomm.
  ls_objpack-body_start = length1.
  ls_objpack-body_num   = length2.
  ls_objpack-doc_type   = 'PDF'.
  ls_objpack-doc_size   = XSTRLEN( ls_formoutput ).
  APPEND ls_objpack TO lt_objpack.

* get reciver details
   rp-read-infotype final_tab-pernr 0105 P0105 PBEGDA PENDDA.
   RP-PROVIDE-FROM-LAST P0105 '0010' PBEGDA PENDDA.
   SORT P0105 DESCENDING BY ENDDA.
   READ TABLE P0105 WITH KEY '0010'.

   LT_RECEIVERS-RECEIVER = P0105-USRID_LONG.
   LT_RECEIVERS-REC_TYPE = 'U'.
   APPEND LT_RECEIVERS.

  IF NOT LT_RECEIVERS-RECEIVER IS INITIAL.
*   send mail
    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data                    = ls_doc_data
        put_in_outbox                    = 'X'
        commit_work                      = 'X'
      TABLES
        packing_list                     = lt_objpack
*     OBJECT_HEADER                      =
*     CONTENTS_BIN                       =
*     CONTENTS_TXT                       =
      contents_hex                       = lt_contents
        receivers                        = lt_receivers
   EXCEPTIONS
     too_many_receivers               = 1
     document_not_sent                = 2
     document_type_not_exist          = 3
     operation_no_authorization       = 4
     parameter_error                  = 5
     x_error                          = 6
     enqueue_error                    = 7
     OTHERS                           = 8 .
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " email_pdf_output
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOM_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CUSTOM_TEXT CHANGING P_FLEXTXT p_pernr.

    DATA: cust_text TYPE REF TO HR_IN_F16_HTEXT1,
          check_tmp(1) type c.
    clear : check_tmp.

    TRY.
        GET BADI cust_text
          FILTERS
            FLT_VAL = '40'.

        CALL BADI cust_text->CUSTOM_TEXT_F16
          EXPORTING
            PERNR     = pernr-pernr
            P0001     = p0001
            F16BEG    = f16_begda
            F16END    = f16_endda
            FLT_VAL   = '40'
          IMPORTING
            CUST_TEXT = p_flextxt
            .

        check_tmp = 'T'.

      CATCH CX_BADI_NOT_IMPLEMENTED.
    ENDTRY.
   If sy-subrc NE 0.
     p_flextxt = p_pernr.
   ENDIF.
ENDFORM.                    " GET_CUSTOM_TEXT
