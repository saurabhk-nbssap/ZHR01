*----------------------------------------------------------------------*
*  INCLUDE PCF16IN7                                                    *
*XXXNTnote number  <date>   Note<note number>:<short description>
*RSKNT601191       04032003 Note601191: Arrears from previous year
*                                       missing in Annexure to F16
*RSKNT609605       31032003 Note609605: Form 16/12BA changes as per
*                                       circular 13/2002
*MKRNT609797       04042003 Note609797: Form 16: Printing of prev.
*                                       employment perks in annexure
*RSKNT615316       17042003 Note615316: Tax Payable/refundable in Form
*                                       16 displays fractional amount
*RSKNT617260       25042003 Note617260: Missing values in Form 12 BA
*                                       under the column (4)
*MKRNT616229       02022004 Note616229: Printing WT '/6I2' from
*                                       previous year in Form 16
*MKINT811821       25012005 Note811821: Form 16 legal changes as
*                                       per Circular 06/2004
*----------------------------------------------------------------------*
* Customization made only for fiscal year 2010 by Anees
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_INFOTYPES
*&---------------------------------------------------------------------*
*      Read the Employee Data from the Infotypes
*----------------------------------------------------------------------*
 FORM READ_INFOTYPES.

   RP-PROVIDE-FROM-LAST P0001 SPACE PBEGDA PENDDA.

   RP-PROVIDE-FROM-LAST P0002 SPACE PBEGDA PENDDA.
   IF PNP-SW-FOUND EQ 0.
     MESSAGE S089(HRPADIN01) WITH '0002' PERNR-PERNR PBEGDA PENDDA.
*   There is no infotype & for personnel no & from period & to &
     PERFORM BUILD_ERROR TABLES HR_ERROR
                        USING SPACE SY-MSGID SY-MSGNO
                        SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
     REJECT.
   ENDIF.

   RP-PROVIDE-FROM-LAST P0021 '11' PBEGDA PENDDA.

   RP-PROVIDE-FROM-LAST P0185 '02' PBEGDA PENDDA.

   SELECT SINGLE KWERT FROM T511K INTO KWERT
    WHERE MOLGA = CALCMOLGA
      AND KONST = 'ADDSS'
      AND BEGDA <= PBEGDA
      AND ENDDA >= PBEGDA.

   RP-PROVIDE-FROM-LAST P0006 KWERT PBEGDA PENDDA.

*  'AND PNPESSCF = SPACE' has been added below for ESS changes
   IF PNP-SW-FOUND EQ 0 AND PNPESSCF = SPACE.
     MESSAGE S089(HRPADIN01) WITH '0185' PERNR-PERNR PBEGDA PENDDA.
*   There is no infotype & for personnel no & from period & to &
     PERFORM BUILD_ERROR TABLES HR_ERROR
                        USING SPACE SY-MSGID SY-MSGNO
                        SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
   ENDIF.

 ENDFORM.                              " READ_INFOTYPES

*&---------------------------------------------------------------------*
*&      Form  FILL_MAIN_TAB
*&---------------------------------------------------------------------*
 FORM FILL_MAIN_TAB.
* Loop at RT and fill MAIN_TAB with required wage types
   CLEAR MAIN_TAB.
   CLEAR EMPLOYER_TAX.
   LOOP AT RT.
     CASE RT-LGART.
       WHEN GROSS_SAL.
         MAIN_TAB-GROSS_SAL = MAIN_TAB-GROSS_SAL + RT-BETRG.
       WHEN SEC10_ALL.
         MAIN_TAB-SEC10_ALL = MAIN_TAB-SEC10_ALL + RT-BETRG.
       WHEN BALANCE.
         MAIN_TAB-BALANCE = MAIN_TAB-BALANCE + RT-BETRG.
       WHEN STD_DED.
         MAIN_TAB-STD_DED = MAIN_TAB-STD_DED + RT-BETRG.
       WHEN PTAX.
         MAIN_TAB-PTAX = MAIN_TAB-PTAX + RT-BETRG.
       WHEN AGGR_DED.
         MAIN_TAB-AGGR_DED = MAIN_TAB-AGGR_DED + RT-BETRG.
       WHEN SALARIES.
         MAIN_TAB-SALARIES = MAIN_TAB-SALARIES + RT-BETRG.
       WHEN OTH_INCOME.
         MAIN_TAB-OTH_INCOME = MAIN_TAB-OTH_INCOME + RT-BETRG.
       WHEN BUS_PROF OR GAIN_LONGN OR GAIN_LONGS OR GAIN_SHORT OR DVDND OR INTST OR OTH_IN.
         MAIN_TAB-BUS_PROF = MAIN_TAB-BUS_PROF + RT-BETRG.
       WHEN DEDN_INTEREST_S24 OR DEDN_REPAIR_S24 OR DEDN_OTHERS_S24 OR TOT_LETTABLE_VAL.
         MAIN_TAB-DEDN_S24 =  MAIN_TAB-DEDN_S24 + RT-BETRG.
       WHEN GROSS_TOT_INCOME.
         MAIN_TAB-GROSS_TOT_INCOME = MAIN_TAB-GROSS_TOT_INCOME
                                            + RT-BETRG.
       WHEN SEC80_DED.
         MAIN_TAB-SEC80_DED = MAIN_TAB-SEC80_DED + RT-BETRG.
       WHEN TOT_INCOME.
         MAIN_TAB-TOT_INCOME = MAIN_TAB-TOT_INCOME + RT-BETRG.
       WHEN TAX_TOT_INCOME.
         MAIN_TAB-TAX_TOT_INCOME = MAIN_TAB-TAX_TOT_INCOME +
                                            RT-BETRG.
       WHEN SEC88_DED.
         MAIN_TAB-SEC88_DED = MAIN_TAB-SEC88_DED + RT-BETRG.
       WHEN SEC88B_DED.
         MAIN_TAB-SEC88B_DED = MAIN_TAB-SEC88B_DED + RT-BETRG.
       WHEN SEC88C_DED.
         MAIN_TAB-SEC88C_DED = MAIN_TAB-SEC88C_DED + RT-BETRG.
       WHEN SEC88D_DED.                                "MKINT811821
         MAIN_TAB-SEC88D_DED = MAIN_TAB-SEC88D_DED + RT-BETRG.
       WHEN EPF_TOT.
         MAIN_TAB-EPF_TOT = MAIN_TAB-EPF_TOT + RT-BETRG.
       WHEN PYR_EPF.                                   "MKRNT616229
         IF RT-BETRG > 0.
           MAIN_TAB-EPF_TOT = MAIN_TAB-EPF_TOT + RT-BETRG.
         ENDIF.
       WHEN CHAPVI_DED.
         MAIN_TAB-CHAPVI_DED = MAIN_TAB-CHAPVI_DED + RT-BETRG.
*       WHEN TAX_PAYABLE.
*         MAIN_TAB-TAX_PAYABLE = MAIN_TAB-TAX_PAYABLE +
*                                         RT-BETRG.
       WHEN TAX_PAYABLE.
         MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                      = MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                        + RT-BETRG.
       WHEN SURCHG_AMT.
         MAIN_TAB-SURCHG = MAIN_TAB-SURCHG + RT-BETRG.

       WHEN SEC_CESS.
         MAIN_TAB-EDU_CESS =  MAIN_TAB-EDU_CESS + RT-BETRG.

       WHEN EDU_CESS.
         MAIN_TAB-EDU_CESS =  MAIN_TAB-EDU_CESS + RT-BETRG.

       WHEN SEC89_RELIEF.
         MAIN_TAB-SEC89_RELIEF = MAIN_TAB-SEC89_RELIEF +
                                 RT-BETRG.
       WHEN TAX_DED_SO_FAR.
         MAIN_TAB-TAX_DED_SO_FAR = MAIN_TAB-TAX_DED_SO_FAR +
                                            RT-BETRG.
       WHEN TAX_THIS_MONTH.
         MAIN_TAB-TAX_THIS_MONTH = MAIN_TAB-TAX_THIS_MONTH +
                                            RT-BETRG.
       WHEN TDS_PETD.
         MAIN_TAB-TDS_PETD = MAIN_TAB-TDS_PETD + RT-BETRG.

       WHEN TDS_PETD_SAP.
         MAIN_TAB-TDS_PETD = MAIN_TAB-TDS_PETD + RT-BETRG.

       WHEN TDS_IFOS.
         MAIN_TAB-TDS_IFOS = MAIN_TAB-TDS_IFOS + RT-BETRG.
       WHEN PET_S172.
         MAIN_TAB-PETD_S172 = MAIN_TAB-PETD_S172 + RT-BETRG.
       WHEN PET_S173.
         MAIN_TAB-PETD_S173 = MAIN_TAB-PETD_S173 + RT-BETRG.
     ENDCASE.
   ENDLOOP.

*   PERFORM READ_CRT USING VOL_TAX 'Y'
*                    CHANGING MAIN_TAB-VOL_TAX.
   READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
                LGART = VOL_TAX
                CUMTY = 'Y'.
   IF SY-SUBRC = 0.
     MAIN_TAB-VOL_TAX = F16-BETRG.
   ENDIF.


* Calculate total tax deducted = tax deducted so far(/456) +
*                                tax this month(/460) +
*                                voluntary tax(/462)
*   MAIN_TAB-CHAPVI_DED = MAIN_TAB-CHAPVI_DED + MAIN_TAB-SEC89_RELIEF.

   MAIN_TAB-TAX_PAYABLE = MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                          - MAIN_TAB-SEC89_RELIEF.

*RSKNT615316
   CALL FUNCTION 'HR_IN_ROUND_AMT'
     EXPORTING
       AMOUNT = MAIN_TAB-TAX_PAYABLE
       RNDOFF = 100
       RNDLMT = 'N'
     IMPORTING
       RETAMT = MAIN_TAB-TAX_PAYABLE.
*RSKNT615316

* BADI to return the tax paid by the employer on behalf of
* the employee.                                "RSKNT609605

   DATA: CUST_EXIT TYPE REF TO IF_EX_HR_IN_TAX_EMPLOYER.

   CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
     CHANGING
       INSTANCE = CUST_EXIT.

   CALL METHOD CUST_EXIT->GET_EMPLOYER_TAX
     EXPORTING
       EMPNO         = PERNR-PERNR
       RESULTS_TABLE = RT[]
       F16_TABLE     = F16[]
       F16_CNTR2     = F16_CNTR2
       FLT_VAL       = '40'
     IMPORTING
       EMPLOYER_TAX  = EMPLOYER_TAX.


   MAIN_TAB-TOT_TAX_DEDUCTED = MAIN_TAB-TAX_DED_SO_FAR +
                               MAIN_TAB-TAX_THIS_MONTH +
                               MAIN_TAB-VOL_TAX.

   MAIN_TAB-TAX_DEDUCTED   =   MAIN_TAB-TAX_DED_SO_FAR +
                               MAIN_TAB-TAX_THIS_MONTH +
                               MAIN_TAB-VOL_TAX - EMPLOYER_TAX.

   MAIN_TAB-TAX_PAID_EMPLOYER = EMPLOYER_TAX.   "RSKNT609605

   MAIN_TAB-NET_TAX_PAYABLE = MAIN_TAB-TAX_PAYABLE -
                              MAIN_TAB-TOT_TAX_DEDUCTED.

   MOVE F16_CNTR2 TO MAIN_TAB-CNTR1.
   APPEND MAIN_TAB.

 ENDFORM.                              " FILL_MAIN_TAB


*&---------------------------------------------------------------------*
*&      Form  FILL_HD_TAB
*&---------------------------------------------------------------------*
 FORM FILL_HD_TAB.
   CLEAR HD_TAB.
*  Read table T7INT5 to obtain employer info.
   PERFORM RE_T7INT5 USING T7IN0P-TXGRP.
   PERFORM READ_NAME USING SY-LANGU CHANGING HD_TAB-ENAME.
   MOVE PERNR-PERNR TO HD_TAB-PERNR.
   RP-PROVIDE-FROM-LAST P0185 '02' PBEGDA PENDDA.
   IF sy-subrc = 0.
     MOVE P0185-ICNUM TO HD_TAB-ICNUM.
   ENDIF.
   MOVE P0002-GESCH TO HD_TAB-GENDER.
   MOVE P0021-FAVOR TO HD_TAB-FFNAME.
   MOVE P0021-FANAM TO HD_TAB-FLNAME.
   MOVE P0006-STRAS TO HD_TAB-HNUMB.
   MOVE P0006-LOCAT TO HD_TAB-LOCALITY.
   MOVE P0006-PSTLZ TO HD_TAB-PIN.
   MOVE P0006-ORT01 TO HD_TAB-CITY.
   MOVE P0006-LAND1 TO HD_TAB-COUNTRY.
   MOVE P0006-TELNR TO HD_TAB-TELN.
   IF HD_TAB-GENDER = 1.
     HD_TAB-GENDER = 'M'.
   ELSE.
     HD_TAB-GENDER = 'F'.
   ENDIF.
   MOVE P0002-GBDAT TO HD_TAB-DOB.
   MOVE F16_CNTR2 TO HD_TAB-CNTR2.
   CLEAR COCD.
   READ TABLE COCD WITH KEY CNTR2 = F16_CNTR2 STAT2 = '3'.
   MOVE F16_BEGDA TO HD_TAB-F16_BEGDA.
*   MOVE COCD-ENDDA TO HD_TAB-F16_ENDDA.
   READ TABLE COCD WITH KEY CNTR2 = F16_CNTR2.  "PRANT981544
   IF SY-SUBRC = 0.
     MOVE COCD-ENDDA TO HD_TAB-F16_ENDDA.
   ENDIF.

   RP-PROVIDE-FROM-LAST P0001 SPACE F16_BEGDA COCD-ENDDA.
   CLEAR T528T.
   PERFORM GET_POSIT USING P0001-OTYPE
                           P0001-PLANS
                           P0001-ENDDA
                     CHANGING HD_TAB-POSITION.

* Get the employer details from feature 40ECC
   MOVE-CORRESPONDING P0001 TO PME01.
   MOVE COMP_CD TO PME01-BUKRS.
*   PERFORM RE549D USING '40ECC' '3' ECCBACK BACK_SUBRC.
*   IF BACK_SUBRC = 0.
*     HD_TAB-PANNO = ECCBACK+3(15).
*     HD_TAB-TANNO = ECCBACK+19(15).
*     HD_TAB-GIRNO = ECCBACK+30(10).
*   ENDIF.
   CLEAR ECC_TAB.
   REFRESH ECC_TAB.
   CALL FUNCTION 'HR_FEATURE_BACKTABLE'
       EXPORTING
            FEATURE                     = '40ECC'
            STRUC_CONTENT               = PME01
            KIND_OF_ERROR               = '3'
       TABLES
            BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
      EXCEPTIONS
*           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
           OTHERS                      = 0
            .
   IF SY-SUBRC = 0.                                         "#EC *
     CLEAR ECC_TAB.
     LOOP AT ECC_TAB.

       CASE SY-TABIX.
         WHEN 1.
         WHEN 2.
           HD_TAB-PANNO = ECC_TAB-BACK+0(15).
         WHEN 3.
           HD_TAB-TANNO = ECC_TAB-BACK+0(15).
         WHEN 4.
           LEN = STRLEN( ECC_TAB-BACK ).
           IF  LEN > 0.
             HD_TAB-GIRNO = ECC_TAB-BACK+0(20).
             LEN = STRLEN( HD_TAB-PANNO ).
             IF LEN > 0.
               MOVE '/' TO HD_TAB-PANNO+LEN(1).
             ENDIF.
           ENDIF.
       ENDCASE.

     ENDLOOP.
   ENDIF.

   APPEND HD_TAB.
 ENDFORM.                              " FILL_HD_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_FINAL_TAB
*&---------------------------------------------------------------------*
 FORM FILL_FINAL_TAB.
   CLEAR FINAL_TAB.
   MOVE-CORRESPONDING HD_TAB TO FINAL_TAB.
   MOVE-CORRESPONDING MAIN_TAB TO FINAL_TAB.
   APPEND FINAL_TAB.
 ENDFORM.                              " FILL_FINAL_TAB

*&---------------------------------------------------------------------*
*&      Form  READ_RT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_lgart   text
*      <--P_SAL  text
*----------------------------------------------------------------------*
 FORM READ_RT USING    VALUE(P_LGART)
              CHANGING P_SAL.

   LOOP AT RT WHERE LGART = P_LGART.
     P_SAL = P_SAL + RT-BETRG.
   ENDLOOP.


 ENDFORM.                              " READ_RT

*&---------------------------------------------------------------------*
*&      Form  READ_CRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGART   text
*      -->P_CUMTY   text
*      <--P_PAYMENT  text
*----------------------------------------------------------------------*
 FORM READ_CRT USING    VALUE(P_LGART)
                        VALUE(P_CUMTY)
               CHANGING P_PAYMENT.

   LOOP AT CRT WHERE LGART = P_LGART AND CUMTY = P_CUMTY.
     P_PAYMENT = P_PAYMENT + CRT-BETRG.
   ENDLOOP.

 ENDFORM.                              " READ_CRT

*&---------------------------------------------------------------------*
*&      Form  GET_FISCAL_YEAR
*&---------------------------------------------------------------------*
 FORM GET_FISCAL_YEAR.

   DATA: BEGIN OF rgdir_tmp OCCURS 100.
          INCLUDE STRUCTURE PC261.
   DATA: END OF rgdir_tmp.
   DATA: BEGIN OF rgdir_wa.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_WA.
   data : tmp_bgda like sy-datum,
          tmp_edda like sy-datum,
          tmp_yr(4) type n.
   clear : tmp_bgda, tmp_yr.
   tmp_yr = year + 1.
   tmp_bgda+0(4) = tmp_yr.
   tmp_bgda+4(4) = '0401'.
   PBEGDA = YEAR.
   PBEGDA+4(4) = '0401'.
   IF afy_switch = 'X'.
     pbegda+4(2) = g_t54c0-txmth.
     pbegda+6(2) = g_t54c0-txday.
   ENDIF.                              " IF afy_switch = 1.

***** FGM changes to consider the correction run after 12th month and
***** before the next FY 1st period normal run.
   IF crun_switch = 'X' AND afy_switch = ''.
      CLEAR : rgdir_wa, rgdir_tmp.
      REFRESH rgdir_tmp.
      LOOP AT rgdir INTO  rgdir_wa
                   WHERE fpbeg GE tmp_bgda.
*                     AND payty =  'B'.
          MOVE-CORRESPONDING rgdir_wa TO rgdir_tmp.
          APPEND rgdir_tmp.
      ENDLOOP.
      SORT rgdir_tmp ASCENDING BY fpbeg.
      LOOP AT rgdir_tmp.
        if rgdir_tmp-payty = ''.
           exit.
        endif.
      ENDLOOP.
      tmp_edda    = rgdir_tmp.
      PENDDA(4)   = tmp_edda+0(4).
      PENDDA+4(4) = tmp_edda+4(4).
   else.
      PENDDA(4) = YEAR + 1.
      PENDDA+4(4) = '0331'.
   endif.
   ASSM_END = PENDDA(4).
   ASSM_END = ASSM_END + 1.
   FIN_START = PBEGDA(4).
   FIN_END = PENDDA(4).

 ENDFORM.                              " GET_FISCAL_YEAR


*&---------------------------------------------------------------------*
*&      Form  FILL_INT_S80
*&---------------------------------------------------------------------*
 FORM FILL_INT_S80.
   DATA : GRS LIKE PC207-BETRG,
          LMT LIKE PC207-BETRG.

   CLEAR: INT_S80, WA_S80.
   I = 0.
   SORT INT_S80 BY SBSEC SBDIV.
   SELECT SINGLE * FROM T7INI7 WHERE SBSEC = '15' AND
                                         BEGDA LE PENDDA  AND
                                         ENDDA GE PBEGDA.
   LMT = T7INI7-SSCLT.
   LOOP AT S80.
     IF S80-SBSEC = 01.
       IF S80-SDVLT > LMT.
         S80-SDVLT = LMT.
       ENDIF.
* CONMT and COAMT refer to the same value (contribution/investment
* amount. COAMT is added to increase the field length so that it
* can capture amount > 9999999.99. This field will be filled up
* after customer has applied relevant Note/HR SP. For payroll
* results created earlier, CONMT should be used for reporting.
       IF S80-COAMT IS INITIAL.                   "PKT1253106
        IF S80-CONMT GT S80-SDVLT.
         GRS = S80-SDVLT.
         LMT = LMT - GRS.
       ELSE.
         GRS = S80-CONMT.
         LMT = S80-SDVLT - GRS.
       ENDIF.
       ELSE.
        IF S80-COAMT GT S80-SDVLT.
         GRS = S80-SDVLT.
         LMT = LMT - GRS.
        ELSE.
         GRS = S80-COAMT.
         LMT = S80-SDVLT - GRS.
        ENDIF.
       ENDIF.
     ENDIF.

     IF S80-SBSEC = 15.
       READ TABLE RT WITH KEY LGART = '/6I2'.
       IF SY-SUBRC EQ 0 AND RT-BETRG > 0.
         S80-DEDMT = S80-DEDMT + RT-BETRG.
         S80-QLAMT = S80-QLAMT + RT-BETRG.
         S80-CONMT = S80-CONMT + RT-BETRG.
         S80-COAMT = S80-COAMT + RT-BETRG.
       ENDIF.
     READ TABLE S80 WITH KEY SBSEC = '15' SBDIV = '01' INTO WA_S80.
     IF WA_S80-COAMT IS INITIAL.
       IF S80-CONMT GT LMT.
         S80-QLAMT = LMT.
         S80-DEDMT = LMT.
         GRS = LMT.
       ELSE.
         GRS = S80-CONMT.
         ENDIF.
       ELSE.
         IF S80-COAMT GT LMT.
           S80-QLAMT = LMT.
           S80-DEDMT = LMT.
           GRS = LMT.
         ELSE.
           GRS = S80-COAMT.
         ENDIF.
       ENDIF.
     ENDIF.

     MOVE F16_CNTR2 TO INT_S80-CNTR2.
     TEMP = ALPHA+I(1).
     I = I + 1.
     TEMP1 = '('.
     MOVE TEMP TO TEMP1+1(1).
     MOVE ')' TO TEMP1+2(1).
     CONDENSE TEMP1 NO-GAPS.
     MOVE-CORRESPONDING S80 TO INT_S80.
     MOVE PERNR-PERNR TO INT_S80-PERNR.
     SELECT SINGLE * FROM T7INI5 WHERE SBSEC = S80-SBSEC.
     MOVE-CORRESPONDING T7INI5 TO INT_S80.
     IF S80-SBSEC = '15' OR S80-SBSEC = '1'.
       READ TABLE S80 WITH KEY SBSEC = '15' SBDIV = '01' INTO WA_S80.
       IF WA_S80-COAMT IS INITIAL.
         MOVE S80-CONMT TO INT_S80-COAMT.
       ELSE.
         MOVE S80-COAMT TO INT_S80-COAMT.
       ENDIF.
       MOVE GRS TO INT_S80-QLAMT.
       MOVE GRS TO INT_S80-DEDMT.
     ELSE.
       IF S80-COAMT IS INITIAL.
         MOVE S80-CONMT TO INT_S80-COAMT.
       ELSE.
         MOVE S80-COAMT TO INT_S80-COAMT.
       ENDIF.
     ENDIF.
     MOVE T7INI5-SBTDS TO TEMP1+4.
     MOVE TEMP1 TO INT_S80-SBTDS.
     APPEND INT_S80.
   ENDLOOP.
   SORT INT_S80 BY SBSEC SBDIV.
*   DELETE INT_S80 WHERE DEDMT IS INITIAL.                   "RBSNT920906
 ENDFORM.                              " FILL_INT_S80

*&---------------------------------------------------------------------*
*&      Form  FILL_INT_S88
*&---------------------------------------------------------------------*
 FORM FILL_INT_S88.
   DATA : TEMP2(10) TYPE C.
   CLEAR RESULT.
   CLEAR INT_S88.
   I = 1.
   SORT S88 BY ICODE.
   LOOP AT S88.
     PERFORM GET_ROMAN_NUMBER USING I.
     TEMP = RESULT.
     I = I + 1.
     TEMP1 = '('.
     MOVE TEMP TO TEMP1.
     TEMP2 = STRLEN( TEMP1 ).
     MOVE ')' TO TEMP1+TEMP2(1).
     CONDENSE TEMP1 NO-GAPS.
     MOVE F16_CNTR2 TO INT_S88-CNTR2.
     MOVE-CORRESPONDING S88 TO INT_S88.
     MOVE PERNR-PERNR TO INT_S88-PERNR.
     SELECT SINGLE * FROM T7INI3 WHERE ICODE = S88-ICODE.
     MOVE T7INI3-ITEXT(50) TO TEMP1+5.
     MOVE-CORRESPONDING T7INI3 TO INT_S88.
     MOVE TEMP1 TO INT_S88-ITEXT.
     APPEND INT_S88.
     IF S88-INAMT IS INITIAL.
        s88_total = s88_total + s88-invmt.
     ELSE.
        s88_total = s88_total + s88-inamt.
     ENDIF.
   ENDLOOP.
*   TEMP = ALPHA+I(1).
*   TEMP1 = '('.
*   MOVE TEMP TO TEMP1+1(1).
*   MOVE ')' TO TEMP1+2(1).
*   CONDENSE TEMP1 NO-GAPS.
   SORT INT_S88 BY ICODE.
 ENDFORM.                              " FILL_INT_S88


*&---------------------------------------------------------------------*
*&      Form  FILL_GROSS_TAB
*&---------------------------------------------------------------------*
 FORM FILL_GROSS_TAB.
TYPES: begin of st_betrg,
      betrg TYPE betrg,
      END OF st_betrg.
DATA: it_betrg TYPE STANDARD TABLE OF st_betrg,
      wa_betrg TYPE st_betrg.
DATA: payresult TYPE pay99_result.
DATA: TEMP_RT TYPE STANDARD TABLE OF PAY99_RESULT-INTER-RT,
      WA_TEMP_RT TYPE PAY99_RESULT-INTER-RT.
DATA : BEGIN OF rt1 occurs 0.
        INCLUDE STRUCTURE pc207 .
DATA : END OF rt1.
DATA: c_betrg TYPE char16,
      c_betrg1 TYPE char16,
      c_betrg2 TYPE char16,
      c_bukrs TYPE char4.
DATA: c_lv_lines TYPE i,
      line TYPE i.
DATA: i_txtlines TYPE STANDARD TABLE OF char256,
      wa_i_txtlines TYPE char256.
DATA: betrg TYPE P LENGTH 9 DECIMALS 2.
DATA: count TYPE i.
DATA:  LIST_tab like STANDARD TABLE OF ABAPLIST.
DATA: it_wages TYPE STANDARD TABLE OF ZF16_WAGETYPES,
      wa_wages TYPE ZF16_WAGETYPES.
DATA: temp_cntr2 LIKE PINCC-CNTR2,
      wa_pa0302 TYPE pa0302,
      flag.

   SORT RT BY LGART.
   LOOP AT I512W WHERE NOT AKLAS+10(2) IS INITIAL.
     CLEAR GROSS_TAB.
     GROSS_TAB-PERNR = PERNR-PERNR.
     MOVE F16_CNTR2 TO GROSS_TAB-CNTR2.
     READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
                 LGART = I512W-LGART
                 CUMTY = 'Y'.

     IF SY-SUBRC = 0.
       "Anees
       CLEAR wa_wages.
       SELECT single lgart from ZF16_WAGETYPES INTO wa_wages-lgart WHERE lgart = I512W-LGART.
       IF sy-subrc <> 0.
         GROSS_TAB-AMOUNT = F16-BETRG.
         GROSS_TAB-SIGN = F16-ANZHL.
       ENDIF.
       "Anees
*       GROSS_TAB-AMOUNT = F16-BETRG.
*       GROSS_TAB-SIGN = F16-ANZHL.
     ELSE.
       CLEAR wa_wages.
       SELECT single lgart from ZF16_WAGETYPES INTO wa_wages-lgart WHERE lgart = I512W-LGART.
       IF sy-subrc <> 0.
       PERFORM READ_RT USING I512W-LGART
                       CHANGING GROSS_TAB-AMOUNT.
       GROSS_TAB-SIGN = RT-ANZHL.
       ENDIF.
     ENDIF.
     IF GROSS_TAB-AMOUNT <> 0.
       GROSS_TAB-EVCLS_SPEC = I512W-AKLAS+10(2).
       PERFORM READ_SPEC_TXT USING '06' I512W-AKLAS+10(2)
                             CHANGING GROSS_TAB-SPEC_TXT.
       COLLECT GROSS_TAB.
     ENDIF.
   ENDLOOP.
*RSKNT601191
*   LOOP AT S89 WHERE LGART = '/616'..
*     GROSS_TAB-AMOUNT = GROSS_TAB-AMOUNT + S89-BETRG.
*   ENDLOOP.
*   IF SY-SUBRC EQ 0.
*     IF GROSS_TAB-AMOUNT > 0.
*       GROSS_TAB-SIGN = 1.
*     ELSE.
*       GROSS_TAB-AMOUNT =  ABS( GROSS_TAB-AMOUNT ).
*       GROSS_TAB-SIGN = -1.
*     ENDIF.
*     GROSS_TAB-SPEC_TXT = 'Arrears from previous years'(198).
*     GROSS_TAB-EVCLS_SPEC = 24.
*     COLLECT GROSS_TAB.
*   ENDIF.
*RSKNT601191

"Anees
CLEAR flag.
SELECT SINGLE * FROM pa0302 INTO wa_pa0302
  WHERE pernr = pernr-pernr
  AND massn = 'I4'
  AND begda >= '20100401'
  AND endda <= '20110331'.
IF sy-subrc = 0.
  flag = 'X'.
ENDIF.

CLEAR: betrg, wa_wages.
SELECT * from ZF16_WAGETYPES INTO CORRESPONDING FIELDS OF TABLE it_wages.
LOOP AT it_wages into wa_wages.
REFRESH: list_tab, i_txtlines.
CLEAR: c_lv_lines.
  SUBMIT H99CWTR0 with PNPPERNR-LOW = pernr-pernr
                  WITH BEGD_CAL = '20100401'
                  WITH ENDD_CAL = '20110331'
                  WITH S_LGART = wa_wages-lgart
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


      CALL FUNCTION 'LIST_TO_ASCI'
        EXPORTING
          list_index = -1
        TABLES
          listasci   = i_txtlines
          listobject = list_tab.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Reading value from ASCI text into betrg.


DESCRIBE TABLE i_txtlines LINES c_lv_lines.

  IF c_lv_lines = 5.
    CLEAR: betrg, c_betrg, c_betrg1, c_betrg2.
    READ TABLE i_txtlines INTO wa_i_txtlines  INDEX 4.
    IF sy-subrc = 0.
    c_betrg = wa_i_txtlines+235(15).
    split c_betrg at ',' into c_betrg1 c_betrg2.
    clear c_betrg.
    concatenate c_betrg1 c_betrg2 into c_betrg.
    "incase two commas are present
    split c_betrg at ',' into c_betrg1 c_betrg2.
    clear c_betrg.
    concatenate c_betrg1 c_betrg2 into c_betrg.
    "
    CONDENSE c_betrg.
    betrg = c_betrg.
    c_bukrs = wa_i_txtlines+1(4).
    ENDIF.
  ELSEIF c_lv_lines > 5.
    REFRESH it_betrg.
    i = 1.
    DO c_lv_lines TIMES.
      IF i > 3 and i < c_lv_lines.
        CLEAR: wa_betrg, c_betrg, c_betrg1, c_betrg2.
        READ TABLE i_txtlines INTO wa_i_txtlines  INDEX i.
        IF sy-subrc = 0.
        c_betrg = wa_i_txtlines+235(15).
        split c_betrg at ',' into c_betrg1 c_betrg2.
        clear c_betrg.
        concatenate c_betrg1 c_betrg2 into c_betrg.
        "incase two commas are present
        split c_betrg at ',' into c_betrg1 c_betrg2.
        clear c_betrg.
        concatenate c_betrg1 c_betrg2 into c_betrg.
        "
        CONDENSE c_betrg.
        wa_betrg-betrg = c_betrg.
        APPEND wa_betrg to it_betrg.
        CLEAR wa_betrg.
        ENDIF.
        c_bukrs = wa_i_txtlines+1(4).
      ENDIF.
      i = i + 1.
    ENDDO.
    CLEAR betrg.
    LOOP AT it_betrg into wa_betrg.
      betrg = betrg + wa_betrg-betrg.
      CLEAR wa_betrg.
    ENDLOOP.
  ENDIF.


  IF betrg IS NOT INITIAL.
   CLEAR I512W.
   IF flag = 'X'.
     IF c_bukrs = '2000'.
     temp_cntr2 = '02'.
     ELSE.
     temp_cntr2 = '01'.
     ENDIF.
   ELSE.
     temp_cntr2 = '01'.
   ENDIF.

   READ TABLE I512W WITH KEY lgart = wa_wages-lgart.
   IF sy-subrc = 0.
     READ TABLE F16 WITH KEY CNTR2 = temp_cntr2"'01'
                             LGART = I512W-LGART.
     IF sy-subrc = 0.
*       IF f16-betrg = 0.                             "Wage type present in F16 but not in Gross_tab because bwert = 0.
         CLEAR GROSS_TAB.
         GROSS_TAB-PERNR = PERNR-PERNR.
         GROSS_TAB-AMOUNT = BETRG.
         GROSS_TAB-SIGN = rt1-anzhl.
         IF GROSS_TAB-AMOUNT <> 0.
           GROSS_TAB-CNTR2 = temp_cntr2."'01'.
           GROSS_TAB-EVCLS_SPEC = I512W-AKLAS+10(2).
           PERFORM READ_SPEC_TXT USING '06' I512W-AKLAS+10(2)
                        CHANGING GROSS_TAB-SPEC_TXT.
         ENDIF.
         COLLECT GROSS_TAB.
         CLEAR GROSS_TAB.
*       ENDIF.
     Else.                                             "Wage type not present in F16, collect in gross_tab
       CLEAR GROSS_TAB.
       GROSS_TAB-PERNR = PERNR-PERNR.
       GROSS_TAB-AMOUNT = BETRG.
       GROSS_TAB-SIGN = rt1-anzhl.
       IF GROSS_TAB-AMOUNT <> 0.
         GROSS_TAB-CNTR2 = temp_cntr2."'01'.
         GROSS_TAB-EVCLS_SPEC = I512W-AKLAS+10(2).
         PERFORM READ_SPEC_TXT USING '06' I512W-AKLAS+10(2)
                        CHANGING GROSS_TAB-SPEC_TXT.
       ENDIF.
       COLLECT GROSS_TAB.
       CLEAR GROSS_TAB.
    ENDIF.
   ENDIF.
  ENDIF.
CLEAR: wa_wages, betrg.
ENDLOOP.
"End Anees

   SORT GROSS_TAB BY PERNR EVCLS_SPEC.

 ENDFORM.                              " FILL_GROSS_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_IFOS_TAB
*&---------------------------------------------------------------------*
 FORM FILL_IFOS_TAB.

   SORT RT BY LGART.
   LOOP AT I512W WHERE NOT AKLAS+14(2) IS INITIAL.
     CLEAR IFOS_TAB.
     IFOS_TAB-PERNR = PERNR-PERNR.
     MOVE F16_CNTR2 TO IFOS_TAB-CNTR2.
*     READ TABLE CRT WITH KEY LGART = I512W-LGART CUMTY = 'Y'.
     READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
                 LGART = I512W-LGART
                 CUMTY = 'Y'.
     IF SY-SUBRC = 0.
       IFOS_TAB-AMOUNT = F16-BETRG.
       IFOS_TAB-SIGN = F16-ANZHL.
     ELSE.
       PERFORM READ_RT USING I512W-LGART
                       CHANGING IFOS_TAB-AMOUNT.
       IFOS_TAB-SIGN = RT-ANZHL.
     ENDIF.
     IF IFOS_TAB-AMOUNT <> 0.
       IFOS_TAB-EVCLS_SPEC = I512W-AKLAS+14(2).
       PERFORM READ_SPEC_TXT USING '08' I512W-AKLAS+14(2)
                             CHANGING IFOS_TAB-SPEC_TXT.
       COLLECT IFOS_TAB.
     ENDIF.

   ENDLOOP.
   SORT IFOS_TAB BY PERNR EVCLS_SPEC.

 ENDFORM.                              " FILL_IFOS_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_PERK_TAB
*&---------------------------------------------------------------------*
 FORM FILL_PERK_TAB.

   SORT RT BY LGART.
   LOOP AT I512W WHERE NOT AKLAS+16(2) IS INITIAL.
     CLEAR PERK_TAB.
*    If WT specification > 30 , it should not be printed in Perks
*    section
*     IF I512W-AKLAS+16(2) > 30.
     IF I512W-AKLAS+16(2) BETWEEN 31 AND 60.        "MKRNT609797
       CONTINUE.
     ENDIF.
     PERK_TAB-PERNR = PERNR-PERNR.
     MOVE F16_CNTR2 TO PERK_TAB-CNTR2.
*     READ TABLE CRT WITH KEY LGART = I512W-LGART CUMTY = 'Y'.
     READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
                 LGART = I512W-LGART
                 CUMTY = 'Y'.
     IF SY-SUBRC = 0.
       PERK_TAB-AMOUNT = F16-BETRG.
       PERK_TAB-SIGN = F16-ANZHL.
     ELSE.
       PERFORM READ_RT USING I512W-LGART
                       CHANGING PERK_TAB-AMOUNT.
       PERK_TAB-SIGN = RT-ANZHL.
     ENDIF.
     IF PERK_TAB-AMOUNT <> 0.
       PERK_TAB-EVCLS_SPEC = I512W-AKLAS+16(2).
       PERFORM READ_SPEC_TXT USING '09' I512W-AKLAS+16(2)
                             CHANGING PERK_TAB-SPEC_TXT.
       COLLECT PERK_TAB.
     ENDIF.

   ENDLOOP.
   SORT PERK_TAB BY PERNR EVCLS_SPEC.

 ENDFORM.                              " FILL_PERK_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_SEC10_TAB
*&---------------------------------------------------------------------*
 FORM FILL_SEC10_TAB.

   SORT RT BY LGART.
   LOOP AT I512W WHERE NOT AKLAS+12(2) IS INITIAL.
     CLEAR SEC10_TAB.
     SEC10_TAB-PERNR = PERNR-PERNR.
     MOVE F16_CNTR2 TO SEC10_TAB-CNTR2.
*     READ TABLE CRT WITH KEY LGART = I512W-LGART CUMTY = 'Y'.
     READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
                 LGART = I512W-LGART
                 CUMTY = 'Y'.
     IF SY-SUBRC = 0.
       SEC10_TAB-AMOUNT = F16-BETRG.
       SEC10_TAB-SIGN = F16-ANZHL.
     ELSE.
       PERFORM READ_RT USING I512W-LGART
                       CHANGING SEC10_TAB-AMOUNT.
       SEC10_TAB-SIGN = RT-ANZHL.
     ENDIF.
     IF SEC10_TAB-AMOUNT <> 0.
       SEC10_TAB-EVCLS_SPEC = I512W-AKLAS+12(2).
       PERFORM READ_SPEC_TXT USING '07' I512W-AKLAS+12(2)
                             CHANGING SEC10_TAB-SPEC_TXT.
       COLLECT SEC10_TAB.
     ENDIF.

   ENDLOOP.
   SORT SEC10_TAB BY PERNR EVCLS_SPEC.

 ENDFORM.                              " FILL_SEC10_TAB

*&---------------------------------------------------------------------*
*&      Form  IMPORT_RESULTS
*&---------------------------------------------------------------------*
 FORM IMPORT_RESULTS.
   DATA : TMP_COCD_INDX LIKE SY-TABIX.

   DATA : TMP_DATE LIKE COCD-BEGDA.
   DATA : TMP_PER LIKE RGDIR-FPPER.
   DATA : N TYPE I.
   DATA : LV_CH_DATE(10) TYPE C. "char date
   DATA : LV_YEAR TYPE PIN_TAXYR.
   DATA: PAY-YEAR(4),
         PAY-PERIOD(2),
         period-endate TYPE DATUM.
   DATA:  PAY-PERIOD1 like T009B-POPER."gg"1274331"
   DATA: BEGIN OF RGDIR_TMP OCCURS 100.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_TMP.
   DATA: BEGIN OF RGDIR_WA.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_WA.
   DATA: TMP_PERMO like RGDIR-PERMO,
         TMP_ABKRS like RGDIR-ABKRS.

   SORT RGDIR BY FPPER.
   CLEAR FOUND.
   CLEAR CONTI.
   REFRESH TMP_RGDIR.
   TMP_RGDIR[] = RGDIR[].

   IF MULTIPLE_F16 = 'X' OR REHIRING = 'X'.
*    If company using multiple form 16.
*    Eliminate the results which are in the next financial year.
     TMP_COCD_INDX = SY-TABIX.

     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> '' .
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> '' .
       IF TMP_RGDIR-PAYTY <> ''.
        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY
         IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
            TMP_RGDIR-FPBEG+4(2) = '02' OR
            TMP_RGDIR-FPBEG+4(2) = '03'.
           IF TMP_RGDIR-INPER NE '000000'.
             IF TMP_RGDIR-INPER+0(4) EQ YEAR.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
             ELSE.
              CONTINUE.
             ENDIF.
           ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
           ENDIF.
         ELSE.           "If inperiod is not JAN,FEB,MAR
           CONTINUE.
         ENDIF.

        ELSE.    "If In-period is same as input of selection screen
         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
        MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
        APPEND RGDIR.
       ENDIF.
     ENDLOOP.
     CLEAR TMP_ABKRS.
     TMP_ABKRS = TMP_RGDIR-ABKRS.
     CLEAR RGDIR.
     SORT RGDIR BY SEQNR.
     READ TABLE REHIRE_DATES INDEX TMP_COCD_INDX.
     IF SY-SUBRC = 0.
       TMP_DATE = REHIRE_DATES-P_DATE.
       IF REHIRING = 'X'.
         clear: period-endate, tmp_permo.
         select single permo into tmp_permo
                  from t549a
                  where abkrs = TMP_ABKRS.
            select single ENDDA into period-endate
                     from t549q
                  where permo = tmp_permo
                    and BEGDA <= F16_ENDDA
                    AND ENDDA >= F16_ENDDA.
          loop at rgdir where fpbeg >= f16_begda AND fpend <= period-endate.
           clear: RGDIR_WA, RGDIR_TMP.
           Refresh RGDIR_TMP.
           if rgdir-fpper = rgdir-inper.
             tmp_per = rgdir-fpper.

             clear TEMSEFIN_TAB.
             IF RGDIR-PAYTY  EQ ' ' .                       "gg1274331"
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                              PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                              PAYPER+0(2)  =  RGDIR-FPPER+4(2).
             ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
               CLEAR : LV_CH_DATE.
               PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                              PAYDATE  =  LV_CH_DATE.

             ENDIF.
             IF SY-SUBRC NE 0.
               MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.      "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
               MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
               MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
               MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.    "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid.
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by inper.
               read table rgdir_tmp index 1.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
               loop at rt.
                 CASE RT-LGART.
                   WHEN '/4MT'.
                     TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                     TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                   WHEN '/4MS'.
                     TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                     TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                   WHEN '/4ME' or '/4MH'.
                     TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                     TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                   WHEN '/460'.
                     TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                     TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                 ENDCASE.
               endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-paydt changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

               DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
                     result(1),
                     flag.
               clear flag.
*                     CHECK_RECORD TYPE C.

               TRY.
                   GET BADI BD_TAX
                     FILTERS
                       FLT_VAL = '40'.

                   CALL BADI BD_TAX->CHK_F24Q
                     EXPORTING
                       empno         = pernr-pernr
                       begda         = rgdir-fpbeg
                       endda         = rgdir-fpend
                       rgdir_table   = rgdir[]
                       results_table = rt[]
                       f16_table     = f16[]
                       f16_cntr2     = f16_cntr2
                       flt_val       = '40'
                     IMPORTING
                       result        = result.
                   flag = 'X'.

                 CATCH CX_BADI_NOT_IMPLEMENTED.
               ENDTRY.

               IF  RESULT <> ''AND FLAG = 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
                 CLEAR  TEMSEFIN_TAB.
                 CLEAR  TEMSEFIN_TABRT.
               elseIF FLAG NE 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
               CLEAR  TEMSEFIN_TAB.
               CLEAR  TEMSEFIN_TABRT.
             ENDIF.
               clear flag.
             endif.
             endif.

          endloop.
      ELSE.
       LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= TMP_DATE.
         clear: RGDIR_WA, RGDIR_TMP.
         Refresh RGDIR_TMP.
         IF RGDIR-FPPER = RGDIR-INPER.
           TMP_PER = RGDIR-FPPER.
             clear TEMSEFIN_TAB.
             IF RGDIR-PAYTY  EQ ' ' .                       "gg1274331"
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                              PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                              PAYPER+0(2)  =  RGDIR-FPPER+4(2).
             ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
               CLEAR : LV_CH_DATE.
               PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                              PAYDATE  =  LV_CH_DATE.

             ENDIF.
             IF SY-SUBRC NE 0.
               MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.      "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
               MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
               MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
               MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.    "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*********pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid.
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by inper.
               read table rgdir_tmp index 1.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*               PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
               loop at rt.
                 CASE RT-LGART.
                   WHEN '/4MT'.
                     TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                     TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                   WHEN '/4MS'.
                     TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                     TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                   WHEN '/4ME' or '/4MH'.
                     TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                     TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                   WHEN '/460'.
                     TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                     TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                 ENDCASE.
               endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-PAYDT changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*    DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
*           RESULT(1).
                CLEAR FLAG.
                 TRY.
                     GET BADI BD_TAX
                       FILTERS
                         FLT_VAL = '40'.

                     CALL BADI BD_TAX->CHK_F24Q
                       EXPORTING
                         empno         = pernr-pernr
                         begda         = rgdir-fpbeg
                         endda         = rgdir-fpend
                         rgdir_table   = rgdir[]
                         results_table = rt[]
                         f16_table     = f16[]
                         f16_cntr2     = f16_cntr2
                         flt_val       = '40'
                       IMPORTING
                         result        = result.
                         FLAG = 'X'.

                   CATCH CX_BADI_NOT_IMPLEMENTED.
                 ENDTRY.

                 IF  RESULT <> ''AND FLAG = 'X'.
                   APPEND TEMSEFIN_TAB.
                   APPEND TEMSEFIN_TABRT.

                   CLEAR  TEMSEFIN_TAB.
                   CLEAR  TEMSEFIN_TABRT.

               ELSEIF FLAG NE 'X'..
                 APPEND TEMSEFIN_TAB.
               APPEND TEMSEFIN_TABRT.

               CLEAR  TEMSEFIN_TAB.
               CLEAR  TEMSEFIN_TABRT.
               endif.
               CLEAR FLAG.

             ENDIF.
             ENDIF.
       ENDLOOP.
      ENDIF.
       IF SY-SUBRC = 0.
         LOOP AT RGDIR WHERE FPPER <= TMP_PER AND FPEND <= TMP_DATE.
         ENDLOOP.

         IF SY-SUBRC = 0.
           RX-KEY-SEQNO = RGDIR-SEQNR.
           RX-KEY-PERNR = PERNR-PERNR.
           RP-IMP-C2-IN.
           FOUND = 'X'.
         ENDIF.
       ELSE.
         IF TERM_PREV_FY = 'X'.
           NEXT = 'X'.
           FOUND ='X'.
         ENDIF.
       ENDIF.
     ENDIF.
   ELSE.
*    IF THE COMPANY USES SINGLE FORM 16.
     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> '' .
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
* to stop picking the cross financial years record
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> '' .
       IF TMP_RGDIR-PAYTY <> ''.
        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY
         IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
            TMP_RGDIR-FPBEG+4(2) = '02' OR
            TMP_RGDIR-FPBEG+4(2) = '03' OR
            TMP_RGDIR-PAYTY = 'B'.
           IF TMP_RGDIR-INPER NE '000000'.
             IF TMP_RGDIR-INPER+0(4) EQ YEAR.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
             ELSE.
              CONTINUE.
             ENDIF.
           ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
           ENDIF.
         ELSE.           "If inperiod is not JAN,FEB,MAR
           CONTINUE.
         ENDIF.

        ELSE.    "If In-period is same as input of selection screen
         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
         MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
         APPEND RGDIR.
       ENDIF.
     ENDLOOP.
     IF F16_CENDDA = '00000000'.
       F16_CENDDA = F16_ENDDA.
     ENDIF.

     LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_CENDDA.
       IF rgdir-payty <> 'B' AND rgdir-fpbeg >= F16_ENDDA.
         CONTINUE.
       ENDIF.
       clear: RGDIR_WA, RGDIR_TMP.
       Refresh RGDIR_TMP.
       IF RGDIR-FPPER = RGDIR-INPER.      "PRANT981544
         RX-KEY-SEQNO = RGDIR-SEQNR.
         FOUND = 'X'.
       clear TEMSEFIN_TAB.
       IF RGDIR-PAYTY  EQ ' ' .  "GEETHA"
         READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                        PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                        PAYPER+0(2)  =  RGDIR-FPPER+4(2).
       ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
         CLEAR : LV_CH_DATE.
         PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
         READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                        PAYDATE  =  LV_CH_DATE.
       ENDIF.
       IF SY-SUBRC NE 0.
         MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.            "gg1274331"
         MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
         MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
         MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
         MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*         MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
         MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.
         MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
         MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
         loop at rgdir into  rgdir_wa
                      where fpbeg = rgdir-fpbeg
                        and fpend = rgdir-fpend
                        and payid = rgdir-payid.
             move-corresponding rgdir_wa to rgdir_tmp.
             append rgdir_tmp.
         endloop.
         sort rgdir_tmp descending by inper.
         read table rgdir_tmp index 1.
         PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*         PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
         loop at rt.
           CASE RT-LGART.
             WHEN '/4MT'.
               TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
               TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
             WHEN '/4MS'.
               TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
               TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

             WHEN '/4ME' or '/4MH'.
               TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
               TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
             WHEN '/460'.
               TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
               TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
           ENDCASE.
         endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-paydt changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*        DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK.
*                     CHECK_RECORD TYPE C.
           CLEAR FLAG.

           TRY.
               GET BADI BD_TAX
                 FILTERS
                   FLT_VAL = '40'.

               CALL BADI BD_TAX->CHK_F24Q
                 EXPORTING
                   empno         = pernr-pernr
                   begda         = rgdir-fpbeg
                   endda         = rgdir-fpend
                   rgdir_table   = rgdir[]
                   results_table = rt[]
                   f16_table     = f16[]
                   f16_cntr2     = f16_cntr2
                   flt_val       = '40'
                 IMPORTING
                   result        = result.
                   FLAG = 'X'.

             CATCH CX_BADI_NOT_IMPLEMENTED.
           ENDTRY.

           IF  RESULT <> ''AND FLAG = 'X'.
             APPEND TEMSEFIN_TAB.
         APPEND TEMSEFIN_TABRT.

         CLEAR  TEMSEFIN_TAB.
         CLEAR  TEMSEFIN_TABRT.
           ELSEIF FLAG NE 'X'.
*           endif.

           APPEND TEMSEFIN_TAB.
           APPEND TEMSEFIN_TABRT.

           CLEAR  TEMSEFIN_TAB.
           CLEAR  TEMSEFIN_TABRT.
           ENDIF.
           CLEAR FLAG.

         ENDIF.
        ENDIF.
     ENDLOOP.
     IF TERM_PREV_FY = 'X' AND FOUND = ''.           "RK1046194
        NEXT = 'X'.
        FOUND ='X'.
     ENDIF.
     IF FOUND = 'X'.
       RX-KEY-PERNR = PERNR-PERNR.
       RP-IMP-C2-IN.
     ENDIF.
   ENDIF.
*   IF FOUND IS INITIAL.
*     REJECT.
*   ENDIF.
 ENDFORM.                              " IMPORT_RESULTS

 FORM IMPORT_RESULTS_CRUN.
   DATA : TMP_COCD_INDX LIKE SY-TABIX.

   DATA : TMP_DATE LIKE COCD-BEGDA.
   DATA : TMP_PER LIKE RGDIR-FPPER.
   DATA : N TYPE I.
   DATA : LV_CH_DATE(10) TYPE C. "char date
   DATA : LV_YEAR TYPE PIN_TAXYR.
   DATA: PAY-YEAR(4),
         PAY-PERIOD(2),
         period-endate TYPE DATUM.
   DATA:  PAY-PERIOD1 like T009B-POPER."gg"1274331"
   DATA: BEGIN OF RGDIR_TMP OCCURS 100.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_TMP.
   DATA: BEGIN OF RGDIR_WA.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_WA.
   DATA: TMP_PERMO like RGDIR-PERMO,
         TMP_ABKRS like RGDIR-ABKRS.

   SORT RGDIR BY FPPER.
   CLEAR FOUND.
   CLEAR CONTI.
   REFRESH TMP_RGDIR.
   TMP_RGDIR[] = RGDIR[].

   IF MULTIPLE_F16 = 'X' OR REHIRING = 'X'.
*    If company using multiple form 16.
*    Eliminate the results which are in the next financial year.
     TMP_COCD_INDX = SY-TABIX.

     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> ''
                  OR ( FPPER+(4) = YEAR AND inpty = 'B' ).
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> ''
                          OR ( FPPER+(4) = YEAR AND inpty = 'B' ).
       IF TMP_RGDIR-PAYTY <> ''.

        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY

          IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
             TMP_RGDIR-FPBEG+4(2) = '02' OR
             TMP_RGDIR-FPBEG+4(2) = '03'.
            IF TMP_RGDIR-INPER NE '000000'.
              IF TMP_RGDIR-INPER+0(4) EQ YEAR.
                MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
                APPEND RGDIR.
              ELSE.
                CONTINUE.
              ENDIF.
            ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
            ENDIF.
          ELSE.           "If inperiod is not JAN,FEB,MAR
            CONTINUE.
          ENDIF.

        ELSE.    "If In-period is same as input of selection screen

         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
        MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
        APPEND RGDIR.
       ENDIF.
     ENDLOOP.
     CLEAR TMP_ABKRS.
     TMP_ABKRS = TMP_RGDIR-ABKRS.
     CLEAR RGDIR.
     SORT RGDIR BY SEQNR.
     READ TABLE REHIRE_DATES INDEX TMP_COCD_INDX.
     IF SY-SUBRC = 0.
       TMP_DATE = REHIRE_DATES-P_DATE.
       IF REHIRING = 'X'.
         clear: period-endate, tmp_permo.
         select single permo into tmp_permo
                  from t549a
                  where abkrs = TMP_ABKRS.
            select single ENDDA into period-endate
                     from t549q
                  where permo = tmp_permo
                    and BEGDA <= F16_ENDDA
                    AND ENDDA >= F16_ENDDA.
          loop at rgdir where fpbeg >= f16_begda AND fpend <= period-endate.
           clear: RGDIR_WA, RGDIR_TMP.
           Refresh RGDIR_TMP.
           if rgdir-fpper = rgdir-inper.
             tmp_per = rgdir-fpper.

             clear TEMSEFIN_TAB.
             IF RGDIR-PAYTY  EQ ' ' .                       "gg1274331"
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                              PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                              PAYPER+0(2)  =  RGDIR-FPPER+4(2).
             ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
               CLEAR : LV_CH_DATE.
               PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                              PAYDATE  =  LV_CH_DATE.

             ENDIF.
             IF SY-SUBRC NE 0.
               MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.      "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
               MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
               MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
               MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.    "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid.
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by inper.
               read table rgdir_tmp index 1.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
               loop at rt.
                 CASE RT-LGART.
                   WHEN '/4MT'.
                     TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                     TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                   WHEN '/4MS'.
                     TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                     TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                   WHEN '/4ME' or '/4MH'.
                     TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                     TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                   WHEN '/460'.
                     TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                     TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                 ENDCASE.
               endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse using PME01 changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

               DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
                     result(1),
                     flag.
               clear flag.
*                     CHECK_RECORD TYPE C.

               TRY.
                   GET BADI BD_TAX
                     FILTERS
                       FLT_VAL = '40'.

                   CALL BADI BD_TAX->CHK_F24Q
                     EXPORTING
                       empno         = pernr-pernr
                       begda         = rgdir-fpbeg
                       endda         = rgdir-fpend
                       rgdir_table   = rgdir[]
                       results_table = rt[]
                       f16_table     = f16[]
                       f16_cntr2     = f16_cntr2
                       flt_val       = '40'
                     IMPORTING
                       result        = result.
                   flag = 'X'.

                 CATCH CX_BADI_NOT_IMPLEMENTED.
               ENDTRY.

               IF  RESULT <> ''AND FLAG = 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
                 CLEAR  TEMSEFIN_TAB.
                 CLEAR  TEMSEFIN_TABRT.
               elseIF FLAG NE 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
               CLEAR  TEMSEFIN_TAB.
               CLEAR  TEMSEFIN_TABRT.
             ENDIF.
               clear flag.
             endif.
             endif.

          endloop.
      ELSE. " No rehiring
       LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= TMP_DATE.
         clear: RGDIR_WA, RGDIR_TMP.
         Refresh RGDIR_TMP.
         IF RGDIR-FPPER = RGDIR-INPER.
           TMP_PER = RGDIR-FPPER.
             clear TEMSEFIN_TAB.
             IF RGDIR-PAYTY  EQ ' ' .                       "gg1274331"
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                              PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                              PAYPER+0(2)  =  RGDIR-FPPER+4(2).
             ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
               CLEAR : LV_CH_DATE.
               PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                              PAYDATE  =  LV_CH_DATE.

             ENDIF.
             IF SY-SUBRC NE 0.
               MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.      "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
               MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
               MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
               MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.    "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*********pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid. " same date type 'A'
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by seqnr.
               read table rgdir_tmp index 1.
               READ TABLE temsefin_tab TRANSPORTING NO FIELDS
                 WITH KEY fpbeg = rgdir_tmp-fpbeg
                          fpend = rgdir_tmp-fpend.
               IF sy-subrc = 0.
                 CONTINUE.
               ENDIF.                  " IF sy-subrc = 0.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*               PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
               loop at rt.
                 CASE RT-LGART.
                   WHEN '/4MT'.
                     TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                     TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                   WHEN '/4MS'.
                     TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                     TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                   WHEN '/4ME' or '/4MH'.
                     TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                     TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                   WHEN '/460'.
                     TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                     TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                 ENDCASE.
               endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse using PME01 changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*    DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
*           RESULT(1).
                CLEAR FLAG.
                 TRY.
                     GET BADI BD_TAX
                       FILTERS
                         FLT_VAL = '40'.

                     CALL BADI BD_TAX->CHK_F24Q
                       EXPORTING
                         empno         = pernr-pernr
                         begda         = rgdir-fpbeg
                         endda         = rgdir-fpend
                         rgdir_table   = rgdir[]
                         results_table = rt[]
                         f16_table     = f16[]
                         f16_cntr2     = f16_cntr2
                         flt_val       = '40'
                       IMPORTING
                         result        = result.
                         FLAG = 'X'.

                   CATCH CX_BADI_NOT_IMPLEMENTED.
                 ENDTRY.

                 IF  RESULT <> ''AND FLAG = 'X'.
                   APPEND TEMSEFIN_TAB.
                   APPEND TEMSEFIN_TABRT.

                   CLEAR  TEMSEFIN_TAB.
                   CLEAR  TEMSEFIN_TABRT.

               ELSEIF FLAG NE 'X'..
                 APPEND TEMSEFIN_TAB.
               APPEND TEMSEFIN_TABRT.

               CLEAR  TEMSEFIN_TAB.
               CLEAR  TEMSEFIN_TABRT.
               endif.
               CLEAR FLAG.

             ENDIF.
             ENDIF.
       ENDLOOP.
      ENDIF.
       IF SY-SUBRC = 0.
*         LOOP AT RGDIR WHERE FPPER <= TMP_PER AND FPEND <= TMP_DATE.
*         ENDLOOP.
         LOOP AT RGDIR WHERE FPEND <= TMP_DATE AND payty <> 'A'.
         ENDLOOP.

         IF SY-SUBRC = 0.
           RX-KEY-SEQNO = RGDIR-SEQNR.
           RX-KEY-PERNR = PERNR-PERNR.
           RP-IMP-C2-IN.
           FOUND = 'X'.
         ENDIF.
       ELSE.
         IF TERM_PREV_FY = 'X'.
           NEXT = 'X'.
           FOUND ='X'.
         ENDIF.
       ENDIF.
     ENDIF.
   ELSE.
*    IF THE COMPANY USES SINGLE FORM 16.
     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> ''
                          OR ( FPPER+(4) = YEAR AND inpty = 'B' ).
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
* to stop picking the cross financial years record
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> ''
                          OR ( FPPER+(4) = YEAR AND inpty = 'B' ).
       IF TMP_RGDIR-PAYTY <> ''.
        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY
         IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
            TMP_RGDIR-FPBEG+4(2) = '02' OR
            TMP_RGDIR-FPBEG+4(2) = '03'.
           IF TMP_RGDIR-INPER NE '000000'.
             IF TMP_RGDIR-INPER+0(4) EQ YEAR.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
             ELSE.
              CONTINUE.
             ENDIF.
           ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
           ENDIF.
         ELSE.           "If inperiod is not JAN,FEB,MAR
           CONTINUE.
         ENDIF.

        ELSE.    "If In-period is same as input of selection screen
         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
         MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
         APPEND RGDIR.
       ENDIF.
     ENDLOOP.
     SORT rgdir BY seqnr.
     LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_ENDDA.
       clear: RGDIR_WA, RGDIR_TMP.
       Refresh RGDIR_TMP.
       IF RGDIR-FPPER = RGDIR-INPER.      "PRANT981544
         RX-KEY-SEQNO = RGDIR-SEQNR.
         FOUND = 'X'.
       clear TEMSEFIN_TAB.
       IF RGDIR-PAYTY  EQ ' ' .  "GEETHA"
         READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                        PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                        PAYPER+0(2)  =  RGDIR-FPPER+4(2).
       ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
         CLEAR : LV_CH_DATE.
         PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
         READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                        PAYDATE  =  LV_CH_DATE.
       ENDIF.
       IF SY-SUBRC NE 0.
         MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.            "gg1274331"
         MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
         MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
         MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
         MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*         MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
         MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.
         MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
         MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
         loop at rgdir into  rgdir_wa
                      where fpbeg = rgdir-fpbeg
                        and fpend = rgdir-fpend
                        and payid = rgdir-payid.
             move-corresponding rgdir_wa to rgdir_tmp.
             append rgdir_tmp.
         endloop.
         sort rgdir_tmp descending by seqnr.
         read table rgdir_tmp index 1.
         READ TABLE temsefin_tab TRANSPORTING NO FIELDS
           WITH KEY fpbeg = rgdir_tmp-fpbeg
                    fpend = rgdir_tmp-fpend.
         IF sy-subrc = 0.
           CONTINUE.
         ENDIF.                  " IF sy-subrc = 0.
         PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*         PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
         loop at rt.
           CASE RT-LGART.
             WHEN '/4MT'.
               TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
               TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
             WHEN '/4MS'.
               TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
               TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

             WHEN '/4ME' or '/4MH'.
               TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
               TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
             WHEN '/460'.
               TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
               TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
           ENDCASE.
         endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse using PME01 changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*        DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK.
*                     CHECK_RECORD TYPE C.
           CLEAR FLAG.

           TRY.
               GET BADI BD_TAX
                 FILTERS
                   FLT_VAL = '40'.

               CALL BADI BD_TAX->CHK_F24Q
                 EXPORTING
                   empno         = pernr-pernr
                   begda         = rgdir-fpbeg
                   endda         = rgdir-fpend
                   rgdir_table   = rgdir[]
                   results_table = rt[]
                   f16_table     = f16[]
                   f16_cntr2     = f16_cntr2
                   flt_val       = '40'
                 IMPORTING
                   result        = result.
                   FLAG = 'X'.

             CATCH CX_BADI_NOT_IMPLEMENTED.
           ENDTRY.

           IF  RESULT <> ''AND FLAG = 'X'.
             APPEND TEMSEFIN_TAB.
         APPEND TEMSEFIN_TABRT.

         CLEAR  TEMSEFIN_TAB.
         CLEAR  TEMSEFIN_TABRT.
           ELSEIF FLAG NE 'X'.
*           endif.

           APPEND TEMSEFIN_TAB.
           APPEND TEMSEFIN_TABRT.

           CLEAR  TEMSEFIN_TAB.
           CLEAR  TEMSEFIN_TABRT.
           ENDIF.
           CLEAR FLAG.

         ENDIF.
        ENDIF.
     ENDLOOP.
     IF TERM_PREV_FY = 'X' AND FOUND = ''.           "RK1046194
        NEXT = 'X'.
        FOUND ='X'.
     ENDIF.
     IF FOUND = 'X'.
       RX-KEY-PERNR = PERNR-PERNR.
       RP-IMP-C2-IN.
     ENDIF.
   ENDIF.
 ENDFORM.                              " IMPORT_RESULTS


*&---------------------------------------------------------------------*
*&      Form  FILL_FORM12BA_TAB
*&---------------------------------------------------------------------*
 FORM FILL_FORM12BA_TAB.

   DATA: DED_EVAL_SPEC TYPE I,
         EVL_SPEC(2) TYPE N.

   CLEAR EVAL_TAB.
   REFRESH EVAL_TAB.
   CLEAR FORM12BA_TAB.
   LOOP AT I512W WHERE NOT AKLAS+16(2) IS INITIAL.
     MOVE-CORRESPONDING I512W TO EVAL_TAB.
     EVAL_TAB-EVCLS_SPEC = I512W-AKLAS+16(2).
     APPEND EVAL_TAB.
   ENDLOOP.

   SORT EVAL_TAB BY EVCLS_SPEC LGART.
   SORT RT BY LGART.
   LOOP AT TMP_COCD.
     LOOP AT EVAL_TAB.

       IF EVAL_TAB-EVCLS_SPEC GE 31.
         EXIT.
       ELSE.
         CLEAR FORM12BA_TAB.

         FORM12BA_TAB-PERNR = PERNR-PERNR.
         FORM12BA_TAB-EVCLS_SPEC = EVAL_TAB-EVCLS_SPEC.
         MOVE TMP_COCD-CNTR2 TO FORM12BA_TAB-CNTR2.
*    READ TABLE CRT WITH KEY LGART = EVAL_TAB-LGART CUMTY = 'Y'.
         READ TABLE F16 WITH KEY CNTR2 = TMP_COCD-CNTR2
                 LGART = EVAL_TAB-LGART
                 CUMTY = 'Y'.

         IF SY-SUBRC = 0.
           FORM12BA_TAB-TAX_PERK = F16-BETRG.
         ELSE.
*         PERFORM READ_RT USING EVAL_TAB-LGART
*                         CHANGING FORM12BA_TAB-TAX_PERK.
           PERFORM READ_F16 USING EVAL_TAB-LGART
                                  ''
                                  TMP_COCD-CNTR2
                           CHANGING FORM12BA_TAB-TAX_PERK.
         ENDIF.

         COLLECT FORM12BA_TAB.
       ENDIF.
     ENDLOOP.
* To display the fields with blank as zero on Form12BA.
   CLEAR: FORM12BA_TAB.
     evl_spec = '01'.
     DO 18 TIMES.
      READ TABLE FORM12BA_TAB WITH KEY EVCLS_SPEC = EVL_SPEC PERNR = PERNR-PERNR .
       IF SY-SUBRC NE 0.
        form12ba_tab-cntr2 = '01'.
        form12ba_tab-pernr = pernr-pernr.
        form12ba_tab-evcls_spec = evl_spec.
        form12ba_tab-tax_perk = '0.00'.
        form12ba_tab-eerecvr = '0.00'.
        form12ba_tab-val_perk = '0.00'.
        APPEND FORM12BA_TAB.
      ENDIF.
       evl_spec = evl_spec + '01'.
      ENDDO.
* End of changes to display the fields with blank as zero on Form12BA.

     LOOP AT FORM12BA_TAB WHERE PERNR = PERNR-PERNR AND
                                CNTR2 = TMP_COCD-CNTR2.

       CLEAR DED_EVAL_SPEC.

       DED_EVAL_SPEC = FORM12BA_TAB-EVCLS_SPEC + 30.
*RSKNT617260
       LOOP AT EVAL_TAB WHERE EVCLS_SPEC = DED_EVAL_SPEC.
         READ TABLE F16 WITH KEY CNTR2 = TMP_COCD-CNTR2
                 LGART = EVAL_TAB-LGART
                 CUMTY = 'Y'.
         IF SY-SUBRC = 0.
           FORM12BA_TAB-EERECVR = FORM12BA_TAB-EERECVR + ABS( F16-BETRG
).
         ENDIF.
       ENDLOOP.
*RSKNT617260
       FORM12BA_TAB-VAL_PERK = FORM12BA_TAB-EERECVR +
                               FORM12BA_TAB-TAX_PERK .
       MODIFY FORM12BA_TAB.
     ENDLOOP.

   ENDLOOP.
 ENDFORM.                    " FILL_FORM12BA_TAB
*&---------------------------------------------------------------------*
*&      Form  READ_F16
*&---------------------------------------------------------------------*

 FORM READ_F16  USING    P_WTYPE LIKE T512W-LGART
                         P_CUMTY LIKE T54C3-CUMTY
                         P_CNTR2 LIKE PC207-CNTR2
                CHANGING AMUNT   LIKE PC207-BETRG.
   CLEAR F16.
   LOOP AT F16 WHERE CNTR2 = P_CNTR2 AND
                     LGART = P_WTYPE AND
                     CUMTY = P_CUMTY.
     AMUNT = AMUNT + F16-BETRG.
   ENDLOOP.

 ENDFORM.                                                   " READ_F16

*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  GET_BANK_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BT_BANKS  text
*      -->P_BT_BANKL  text
*      <--P_BNAME  text
*----------------------------------------------------------------------*
 FORM GET_BANK_DETAILS  USING    P_BT_BANKS
                                 P_BT_BANKL
                        CHANGING P_BNAME.
   SELECT SINGLE BANKA BRNCH ORT01 FROM BNKA
     INTO (BNAME, BRNCH, BCITY)
    WHERE BANKS = P_BT_BANKS AND
          BANKL = P_BT_BANKL.
   IF SY-SUBRC = 0.
     CLEAR LEN.
     MOVE BNAME(22) TO PAYMENTS-BNAME.
     LEN = STRLEN( PAYMENTS-BNAME ).
     MOVE ',' TO PAYMENTS-BNAME+LEN(1).
     LEN = STRLEN( PAYMENTS-BNAME ).
     MOVE BRNCH(21) TO PAYMENTS-BNAME+LEN(21).
     LEN = STRLEN( PAYMENTS-BNAME ).
     MOVE ',' TO PAYMENTS-BNAME+LEN(1).
     LEN = STRLEN( PAYMENTS-BNAME ).
     MOVE BCITY(14) TO PAYMENTS-BNAME+LEN(14).
*             CONDENSE PAYMENTS-BNAME NO-GAPS.
   ENDIF.
   P_BNAME = BNAME.
 ENDFORM.                    " GET_BANK_DETAILS
*
*&---------------------------------------------------------------------*
*&      Form  ER_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM ER_ADDRESS USING PTANNO .                              "MKINT910704
   DATA: ADD_NO TYPE T500P-ADRNR.
   DATA:  AA(1), BB(1), CC(1), DD(1), EE(1),
          FF(1), GG(1), HH(1), II(1).

   DATA: ADRNR LIKE HRCA_COMPANY-ADDRESS.
   DATA: SADR1 LIKE SADR OCCURS 10 WITH HEADER LINE.

   DATA:  SELECTION LIKE ADDR1_SEL.
*          ADDRESS_VALUE LIKE ADDR1_VAL.

*   RP-PROVIDE-FROM-LAST P0001 SPACE F16_BEGDA F16_ENDDA.
*   MOVE P0001-WERKS TO PERS_AREA.

*  READ TABLE HD_TAB INDEX 1.
   SELECT SINGLE ADRNR INTO ADD_NO FROM T500P
                                   WHERE PERSA = PERS_AREA.

   SELECTION-ADDRNUMBER = ADD_NO.

   CALL FUNCTION 'ADDR_GET'
     EXPORTING
       ADDRESS_SELECTION = SELECTION
       ADDRESS_GROUP     = 'CA01'  "to read SADR for unchanged
     IMPORTING  "data
       ADDRESS_VALUE     = ADDR1_VAL  "both structures filled
       SADR              = SADR  "choose one of them
     EXCEPTIONS
       ADDRESS_NOT_EXIST = 1
       OTHERS            = 2.                               "SADR40A

   IF SADR-LAND1 IS INITIAL.                         "Note 1067021
*       READ TABLE HD_TAB INDEX 1.
     PERFORM ADDRESS USING COMP_CD ADDR1_VAL.
   ENDIF.

* BADI to return the address of the Employer

   DATA  : CUST_EXIT TYPE REF TO IF_EX_HR_IN_ER_ADDRESS,
           CHECK_IMPL(1)    TYPE C.
   DATA: AD_TAX TYPE REF TO HR_IN_ER_ADDRESS,
         result(1),
         flag.
   clear flag.

   TRY.
      GET BADI AD_TAX
         FILTERS
            FLT_VAL = '40'.

      CALL BADI AD_TAX->GET_ER_ADDRESS
         EXPORTING
            P0001     = IT0001[]
         TANNO     = PTANNO
            FLT_VAL   = '40'
         CHANGING
            SADR      = SADR
            ADDR1_VAL = ADDR1_VAL.
            flag = 'X'.

      CATCH CX_BADI_NOT_IMPLEMENTED.
   ENDTRY.

   IF ADDR1_VAL-NAME1 IS INITIAL. AA = 'N'. ENDIF.
   IF SADR-NAME2 IS INITIAL. BB = 'N'. ENDIF.
   IF SADR-NAME3 IS INITIAL. CC = 'N'. ENDIF.
   IF SADR-NAME4 IS INITIAL. DD = 'N'. ENDIF.
   IF SADR-STRAS IS INITIAL. EE = 'N'. ENDIF.
   IF SADR-PFACH IS INITIAL. FF = 'N'. ENDIF.
   IF SADR-PSTLZ IS INITIAL. GG = 'N'. ENDIF.
   IF SADR-ORT01 IS INITIAL. HH = 'N'. ENDIF.
   IF SADR-ORT02 IS INITIAL. II = 'N'. ENDIF.

   PERFORM CONVERT_TO_SCRIPTVAR USING 'A' AA.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'B' BB.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'C' CC.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'D' DD.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'E' EE.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'F' FF.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'G' GG.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'H' HH.
   PERFORM CONVERT_TO_SCRIPTVAR USING 'I' II.
 ENDFORM.                    " ER_ADDRESS

 "PRANT981544
*---------------------------------------------------------------------*
*       FORM DET_TERM_PREV_FY                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
 FORM DET_TERM_PREV_FY.

   T_COCD[] = COCD[].
   CLEAR TERM_PREV_FY.

   READ TABLE COCD INDEX 1.
   IF SY-SUBRC = 0.
     IF COCD-STAT2 = 0.
       LOOP AT T_COCD WHERE CNTR2 GT COCD-CNTR2 AND
                              STAT2 = '3'.
       ENDLOOP.
       IF SY-SUBRC EQ 0.
         TERM_PREV_FY = 'X'.
         EXIT.
       ENDIF.
     ENDIF.
   ENDIF.


 ENDFORM.                    "DET_TERM_PREV_FY
*&---------------------------------------------------------------------*
*&      Form  POP_REHIRE_DATES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM POP_REHIRE_DATES.

   DATA : TMP_TABIX LIKE SY-TABIX.
   DATA : TMP1_COCD LIKE TMP_COCD OCCURS 10 WITH HEADER LINE.
   CLEAR   rehire_dates.                       "RK1062648
   REFRESH rehire_dates.                       "RK1062648
   TMP1_COCD[] = TMP_COCD[].

   LOOP AT TMP_COCD .
     IF SY-TABIX = 1." and TERM_PREV_FY ne 'X'.
       CONTINUE.
     ENDIF.

*     if SY-TABIX = 2 and TERM_PREV_FY = 'X'.
*       continue.
*     endif.

     REHIRE_DATES-R_DATE = TMP_COCD-BEGDA.

     IF REHIRE_DATES-R_DATE+6(2) NE '01'.
       TMP_TABIX = SY-TABIX - 1.
       READ TABLE TMP1_COCD INDEX TMP_TABIX.
       IF SY-SUBRC = 0.
         REHIRE_DATES-P_DATE = TMP1_COCD-ENDDA.
       ENDIF.
     ELSE.
       REHIRE_DATES-P_DATE = REHIRE_DATES-R_DATE.
     ENDIF.
     APPEND REHIRE_DATES.
   ENDLOOP.
   REHIRE_DATES-R_DATE = TMP_COCD-ENDDA.
   REHIRE_DATES-P_DATE = TMP_COCD-ENDDA.
   APPEND REHIRE_DATES.
 ENDFORM.                    " POP_REHIRE_DATES
*&---------------------------------------------------------------------*
*&      Form  IMPORT_RT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM IMPORT_RT USING P_PREV_RGDIR-SEQNR.

   CLEAR PAYMENTS.
   RX-KEY-SEQNO = P_PREV_RGDIR-SEQNR.
   RX-KEY-PERNR = PERNR-PERNR.
   RP-IMP-C2-IN.
   MOVE PERNR-PERNR TO PAYMENTS-PERNR.
   MOVE F16_CNTR2 TO PAYMENTS-CNTR2.
   MOVE P_PREV_RGDIR-SEQNR TO PAYMENTS-SEQNR.
   READ TABLE RGDIR WITH KEY SEQNR = P_PREV_RGDIR-SEQNR.
     IF sy-subrc EQ 0.
      MOVE RGDIR-PAYDT TO PAYMENTS-TDATE.
     ENDIF.
   PERFORM READ_RT USING TAX_THIS_MONTH CHANGING PAYMENTS-TAXPD.
   PERFORM READ_RT USING VOL_TAX CHANGING PAYMENTS-TAXPD.
   APPEND PAYMENTS.

 ENDFORM.                    " IMPORT_RT

*&---------------------------------------------------------------------*
*&      Form  PAYMENTS_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM PAYMENTS_TAB .

   DATA: BEGIN OF PREV_RGDIR OCCURS 100.
           INCLUDE STRUCTURE PC261.
   DATA: END OF PREV_RGDIR.

   DATA: BEGIN OF CURNT_RGDIR OCCURS 100.
           INCLUDE STRUCTURE PC261.
   DATA: END OF CURNT_RGDIR.

   DATA: c_date(4) TYPE c,
         c_abrj TYPE t569v-pabrj.

   PREV_RGDIR[]  =  RGDIR[].                           "RK1063025
   CURNT_RGDIR[] =  RGDIR[].                           "RK1063025
   SORT PREV_RGDIR BY SEQNR.
   SORT CURNT_RGDIR BY SEQNR.

   IF MULTIPLE_F16 = 'X'.
     CLEAR : PREV_RGDIR, CURNT_RGDIR.
     SORT PREV_RGDIR BY FPPER INPER.
     LOOP AT PREV_RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_ENDDA.
       CLEAR CURNT_RGDIR.
       IF PREV_RGDIR-PAYTY = ''.
         LOOP AT CURNT_RGDIR WHERE FPPER = PREV_RGDIR-FPPER AND
                                 SEQNR > PREV_RGDIR-SEQNR.
           IF CURNT_RGDIR-INPER+(4) = PREV_RGDIR-INPER+(4).
             CONTI = 1.
             EXIT.
           ENDIF.
         ENDLOOP.
       ELSEIF PREV_RGDIR-PAYTY <> ''.
         LOOP AT CURNT_RGDIR WHERE FPBEG = PREV_RGDIR-FPBEG AND
                                   FPEND = PREV_RGDIR-FPEND AND
                                 SEQNR > PREV_RGDIR-SEQNR.
           IF CURNT_RGDIR-INPER+0(4) EQ YEAR.
             CONTI = 1.
             EXIT.
           ENDIF.
         ENDLOOP.
       ENDIF.

       IF CONTI = 1.
         CLEAR CONTI.
         CONTINUE.
       ELSEIF PREV_RGDIR-PAYTY <> '' AND PREV_RGDIR-INPER+0(4) <> YEAR
              AND PREV_RGDIR-INPER <> '000000'.
         CONTINUE.
       ENDIF.
       PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
     ENDLOOP.
   ELSE.                " Multiple Form 16 Disable
     IF crun_switch IS INITIAL.
       LOOP AT PREV_RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_ENDDA.
         RX-KEY-SEQNO = PREV_RGDIR-SEQNR.
         RX-KEY-PERNR = PERNR-PERNR.
         IF PREV_RGDIR-PAYTY <> '' AND PREV_RGDIR-FPBEG = PREV_RGDIR-IPEND " IF PAYTYPE IS NOT INITIAL
                 AND PREV_RGDIR-INPER = '000000'.
           PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
       ELSEIF PREV_RGDIR-PAYTY EQ ''.
           IF PREV_RGDIR-INPER = PREV_RGDIR-FPPER.
             PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
           ENDIF.
         ENDIF.
       ENDLOOP.
     ELSEIF afy_switch = 'X' AND crun_switch = 'X'.
       LOOP AT PREV_RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_ENDDA.
         RX-KEY-SEQNO = PREV_RGDIR-SEQNR.
         RX-KEY-PERNR = PERNR-PERNR.
         IF PREV_RGDIR-PAYTY <> '' AND PREV_RGDIR-FPBEG = PREV_RGDIR-IPEND " IF PAYTYPE IS NOT INITIAL
                 AND PREV_RGDIR-INPER = '000000'.
           PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
         ELSEIF PREV_RGDIR-PAYTY EQ ''.
           IF PREV_RGDIR-INPER = PREV_RGDIR-FPPER.
             PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
           ENDIF.
         ENDIF.
       ENDLOOP.
     ELSEIF afy_switch = '' AND crun_switch = 'X'.
       c_date = '0331'.
       c_abrj = ( F16_ENDDA+0(4) ) + 1.
       concatenate c_abrj c_date into F16_CENDDA.
       LOOP AT PREV_RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_CENDDA.
         IF prev_rgdir-payty <> 'B' AND prev_rgdir-fpbeg >= F16_ENDDA.
          EXIT.
         ENDIF.
         RX-KEY-SEQNO = PREV_RGDIR-SEQNR.
         RX-KEY-PERNR = PERNR-PERNR.
         IF PREV_RGDIR-PAYTY <> '' AND PREV_RGDIR-FPBEG = PREV_RGDIR-IPEND " IF PAYTYPE IS NOT INITIAL
                 AND PREV_RGDIR-INPER = '000000'.
           PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
         ELSEIF PREV_RGDIR-PAYTY EQ ''.
           IF PREV_RGDIR-INPER = PREV_RGDIR-FPPER.
             PERFORM IMPORT_RT USING PREV_RGDIR-SEQNR.
           ENDIF.
         ENDIF.
       ENDLOOP.
     ENDIF.
   ENDIF.
 ENDFORM.                    " PAYMENTS_TAB
*&---------------------------------------------------------------------*
*&      Form  FILL_ACKNO_TAB
*&---------------------------------------------------------------------*
 FORM FILL_ACKNO_TAB.

* Fetch acknowledgement number for Form 24Q eFiling
* (1) Get all TAN numbers applicable for the employee
* (2) Get Acknowledgement Number

DATA: ZINAC_STRUCT LIKE PINAC,
      COUNT(4) TYPE C,
      QUARTER(4) TYPE C,
      OFFSET TYPE I,
      WA_ACKNO LIKE LINE OF ACKNO_TAB,
      ACK_NUMBER(28) TYPE C.
*      ZINAC_STRUCT-TANNO = HD_TAB-TANNO.
      ZINAC_STRUCT-FYEAR = YEAR.

     COUNT = '0000'.
   provide * from p0001 between f16_begda AND f16_endda.
     PERFORM GET_QUARTER USING    P0001-BEGDA P0001-ENDDA
                         CHANGING QUARTER.

*          PME01-BUKRS = P0001-BUKRS.
*          PME01-WERKS = p0001-WERKS.
*          PME01-BTRTL = p0001-BTRTL.

    MOVE-CORRESPONDING p0001 to pme01.

*   Get the employer details from feature 40ECC
     CLEAR ECC_TAB.
     REFRESH ECC_TAB.
     CALL FUNCTION 'HR_FEATURE_BACKTABLE'
         EXPORTING
              FEATURE                     = '40ECC'
              STRUC_CONTENT               = PME01
              KIND_OF_ERROR               = '3'
         TABLES
              BACK                        = ECC_TAB
*        CHANGING
*             STATUS                      =
        EXCEPTIONS
*             DUMMY                       = 1
*             ERROR_OPERATION             = 2
*             NO_BACKVALUE                = 3
*             FEATURE_NOT_GENERATED       = 4
*             INVALID_SIGN_IN_FUNID       = 5
*             TAB_IN_REPORT_FIELD_IN_PE03 = 6
             OTHERS                      = 0
              .
     IF SY-SUBRC = 0.                                         "#EC *
       CLEAR ECC_TAB.
       READ TABLE ECC_TAB INDEX 3.
       ZINAC_STRUCT-TANNO = ECC_TAB-BACK+0(15).
     ENDIF.


     CLEAR ECC_TAB.
     REFRESH ECC_TAB.

     CLEAR ECC_TAB.
     REFRESH ECC_TAB.
     CALL FUNCTION 'HR_FEATURE_BACKTABLE'
         EXPORTING
              FEATURE                     = '40ACK'
              STRUC_CONTENT               = ZINAC_STRUCT
              KIND_OF_ERROR               = '3'
         TABLES
              BACK                        = ECC_TAB
*        CHANGING
*             STATUS                      =
        EXCEPTIONS
*             DUMMY                       = 1
*             ERROR_OPERATION             = 2
*             NO_BACKVALUE                = 3
*             FEATURE_NOT_GENERATED       = 4
*             INVALID_SIGN_IN_FUNID       = 5
*             TAB_IN_REPORT_FIELD_IN_PE03 = 6
             OTHERS                      = 0.

     IF SY-SUBRC = 0.                                         "#EC *
       CLEAR ECC_TAB.
       LOOP AT ECC_TAB.
         CLEAR ACKNO_TAB.
         ACKNO_TAB-QUARTER = ECC_TAB-BACK+0(1).
         ACKNO_TAB-F16_BEGDA = HD_TAB-F16_BEGDA.
         ACKNO_TAB-F16_ENDDA = HD_TAB-F16_ENDDA.
         ACKNO_TAB-PERNR     = HD_TAB-PERNR.
         ACK_NUMBER          = ECC_TAB+2(28).

         OFFSET = ACKNO_TAB-QUARTER - 1.

         CHECK QUARTER+OFFSET(1) = 'X'.
         COUNT+OFFSET(1) = COUNT+OFFSET(1) + 1.
         CASE COUNT+OFFSET(1).
           WHEN 1.
               ACKNO_TAB-ACKNO1 = ACK_NUMBER.
               APPEND ACKNO_TAB.
           WHEN 2.
               READ TABLE ACKNO_TAB INTO WA_ACKNO
                          WITH KEY QUARTER = ACKNO_TAB-QUARTER
                                   PERNR   = ACKNO_TAB-PERNR.
               if WA_ACKNO-ACKNO1 = ACK_NUMBER.
                  COUNT+OFFSET(1) = COUNT+OFFSET(1) - 1.
                  continue.
               endif.
               CONCATENATE ',' ACK_NUMBER INTO ACKNO_TAB-ACKNO2
                           SEPARATED BY SPACE.
               CONDENSE ACKNO_TAB-ACKNO2.
*               ACKNO_TAB-ACKNO2 = ACK_NUMBER.
               MODIFY ACKNO_TAB FROM ACKNO_TAB TRANSPORTING ACKNO2
                                WHERE QUARTER = ACKNO_TAB-QUARTER
                                  AND PERNR   = ACKNO_TAB-PERNR.
           WHEN 3.
               READ TABLE ACKNO_TAB INTO WA_ACKNO
                          WITH KEY QUARTER = ACKNO_TAB-QUARTER
                                   PERNR   = ACKNO_TAB-PERNR.
               if WA_ACKNO-ACKNO1 = ACK_NUMBER OR WA_ACKNO-ACKNO2+2(28) = ACK_NUMBER.
                  COUNT+OFFSET(1) = COUNT+OFFSET(1) - 1.
                  continue.
               endif.
               ACKNO_TAB-ACKNO3 = ACK_NUMBER.
               MODIFY ACKNO_TAB FROM ACKNO_TAB TRANSPORTING ACKNO3
                                WHERE QUARTER = ACKNO_TAB-QUARTER
                                  AND PERNR   = ACKNO_TAB-PERNR.

           WHEN 4.
               READ TABLE ACKNO_TAB INTO WA_ACKNO
                          WITH KEY QUARTER = ACKNO_TAB-QUARTER
                                   PERNR   = ACKNO_TAB-PERNR.
               if WA_ACKNO-ACKNO1 = ACK_NUMBER OR WA_ACKNO-ACKNO2+2(28) = ACK_NUMBER
                  OR WA_ACKNO-ACKNO3 = ACK_NUMBER .

                  COUNT+OFFSET(1) = COUNT+OFFSET(1) - 1.
                  continue.
               endif.
               CONCATENATE ',' ACK_NUMBER INTO ACKNO_TAB-ACKNO4
                           SEPARATED BY SPACE.
               CONDENSE ACKNO_TAB-ACKNO4.
*               ACKNO_TAB-ACKNO4 = ACK_NUMBER.
               MODIFY ACKNO_TAB FROM ACKNO_TAB TRANSPORTING ACKNO4
                                WHERE QUARTER = ACKNO_TAB-QUARTER
                                  AND PERNR   = ACKNO_TAB-PERNR.
          ENDCASE.

       ENDLOOP.
     ENDIF.
  ENDPROVIDE.
 ENDFORM.                              " FILL_ACKNO_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_QUARTER
*&---------------------------------------------------------------------*


FORM GET_QUARTER USING BEGDA ENDDA CHANGING QUARTER.        "RK1067021
  CLEAR QUARTER.
  IF BEGDA+4(2) >= 4 AND BEGDA+4(2) <= 6.  "First quarter
    QUARTER+0(1) = 'X'.
    IF ENDDA+4(2) >= 7 OR ( ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3 ).
"Second quarter
      QUARTER+1(1) = 'X'.
      IF ENDDA+4(2) >= 10  OR ( ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3 ).
"Third quarter
        QUARTER+2(1) = 'X'.
        IF ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3.  "Fourth quarter
          QUARTER+3(1) = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSEIF BEGDA+4(2) >= 7 AND BEGDA+4(2) <= 9."Second quarter
      QUARTER+1(1) = 'X'.
      IF ENDDA+4(2) >= 9 OR ( ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3 ).
"Third quarter
        QUARTER+2(1) = 'X'.
        IF ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3.  "Fourth quarter
          QUARTER+3(1) = 'X'.
        ENDIF.
      ENDIF.
  ELSEIF BEGDA+4(2) >= 10 AND BEGDA+4(2) <= 12. "Third quarter
      QUARTER+2(1) = 'X'.
      IF ENDDA+4(2) >= 1 AND ENDDA+4(2) <= 3.   "Fourth quarter
        QUARTER+3(1) = 'X'.
      ENDIF.
  ELSEIF BEGDA+4(2) >= 1 AND BEGDA+4(2) <= 3.   "Fourth quarter
      QUARTER+3(1) = 'X'.
  ENDIF.
ENDFORM.                              " GET_QUARTER
*&      Form  DET_REHIRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DET_REHIRING .

DATA: prev_cocd LIKE cocd,
      tmp_cocd LIKE cocd occurs 10 with header line.

  tmp_cocd[] = cocd[].

*Global flags  rehiring are used to exclusively to
*execute Rehiring related code if Rehiring is done.
*rehiring flag is set if, for an employee, there is an employment
*status change from 0 (withdrawn) to 3 (active) at least once
*in COCD until the current payroll period.

  clear: rehiring.
  LOOP AT tmp_cocd .

    IF prev_cocd-stat2 = '0'  and "Rehiring
       tmp_cocd-stat2 = '3'.
      rehiring = 'X'.
    ENDIF.
    prev_cocd = tmp_cocd.
  ENDLOOP.

ENDFORM.                    " DET_REHIRING

*&---------------------------------------------------------------------*
*&      Form  IMPORT_RT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*   "geetha 29.07.2007
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM IMPORT_CURRRT USING P_PREV_RGDIR-SEQNR.

   RX-KEY-SEQNO = P_PREV_RGDIR-SEQNR.
   RX-KEY-PERNR = PERNR-PERNR.
   RP-IMP-C2-IN.
*   PERFORM READ_RT USING TAX_THIS_MONTH CHANGING PAYMENTS-TAXPD.
*   PERFORM READ_RT USING VOL_TAX CHANGING PAYMENTS-TAXPD.
 ENDFORM.                    " IMPORT_RT
*&---------------------------------------------------------------------*
*&      Form  GET_TAN_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TAN_NUMBER  text
*----------------------------------------------------------------------*
 FORM GET_TANNO using T_P0001 changing TT_TANNO.

IF wa_t7insw-begda > pbegda OR ( wa_t7insw-reval IS INITIAL )." IS INITIAL.
   MOVE-CORRESPONDING T_P0001 TO PME01.
   MOVE COMP_CD TO PME01-BUKRS.
* Get the employer details from feature 40ECC
   CLEAR ECC_TAB.
   REFRESH ECC_TAB.
   CLEAR tt_tanno.
   CALL FUNCTION 'HR_FEATURE_BACKTABLE'
       EXPORTING
            FEATURE                     = '40ECC'
            STRUC_CONTENT               = PME01
            KIND_OF_ERROR               = '3'
       TABLES
            BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
      EXCEPTIONS
           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
           OTHERS                      = 0
            .
   IF SY-SUBRC = 0.                                         "#EC *
     CLEAR ECC_TAB.
     READ TABLE ECC_TAB INDEX 3.
     TAN-NUMBER = ECC_TAB-BACK+0(15).
   ENDIF.

   tt_tanno = TAN-NUMBER.
 ELSE.
   RP-PROVIDE-FROM-LAST P0185 '05' PBEGDA PENDDA.
   tt_tanno = p0185-icnum.
   emp_panno = p0185-ICOLD.
   emp_girno = p0185-AUTH1.
 ENDIF.
 ENDFORM.                    " GET_TAN_NUMBER*&---------------------------------------------------------------------*
*&      Form  get_challan "1274331"
*&---------------------------------------------------------------------*
 FORM GET_CHALLAN.
   DATA: IT_TAN TYPE TABLE OF PINCHL WITH HEADER LINE,
           T_RECNO(9) TYPE N VALUE 0.


   DATA : count(5) Type n value 0.
   DATA : W_PERNR TYPE PERNR-PERNR value 0.
   data : incometax1 type PC207-BETRG,
          surcharge1 type PC207-BETRG,
          educess1 type PC207-BETRG,
          incometax2 type PC207-BETRG,
          surcharge2 type PC207-BETRG,
          educess2 type PC207-BETRG,
          TTAX2    TYPE PC207-BETRG,
          TOTTAX1  TYPE PC207-BETRG,
          TTAX1    TYPE PC207-BETRG,
          TTAX     TYPE PC207-BETRG,
          BETRG    TYPE PC207-BETRG,
          TEDUCESS TYPE PC207-BETRG,
          TSURCHARGE TYPE PC207-BETRG,
          TINCOMETAX  TYPE PC207-BETRG,
          TEDUCESS1 TYPE PC207-BETRG,
          TSURCHARGE1 TYPE PC207-BETRG,
          TINCOMETAX1  TYPE PC207-BETRG,
          TFPPER TYPE SY-DATUM,
          TEMSEFPBEGDA TYPE SY-DATUM,
          TEMSEFPENDDA TYPE SY-DATUM.



   DATA :w_payper type PC261-FPPER,
         W_BEG TYPE  SY-DATUM,
         W_END TYPE SY-DATUM,
         w_paydate type sy-datum,
         DELTA TYPE PC207-BETRG,
         SUM TYPE PC207-BETRG,
         SUM1 TYPE PC207-BETRG,
         SUM4MT TYPE PC207-BETRG,
         SUM4MS TYPE PC207-BETRG,
         SUM4ME TYPE PC207-BETRG,
         SUM4MT1 TYPE PC207-BETRG,
         SUM4MS1 TYPE PC207-BETRG,
         SUM4ME1 TYPE PC207-BETRG,
         DELTA4MT TYPE PC207-BETRG,
         DELTA4MS TYPE PC207-BETRG,
         DELTA4ME TYPE PC207-BETRG,
         SUM460   TYPE PC207-BETRG,
         L_PERNR  TYPE PERNR-PERNR,
         L_FPPER  TYPE PC261-FPPER.

   DATA:DELTA1 TYPE PC207-BETRG,

* sbegda TYPE begda,
          q_bukrs type bukrs.
*        sendda TYPE endda.
   DATA:
         IDX TYPE SY-TABIX,

        RATIO TYPE F,
        t_enddate type sy-datum,
        t_permo like RGDIR-PERMO,
        t_endda type sy-datum.
*   SORT TEMSEFIN_TAB BY FPBEG FPEND.
  SORT TEMSEFIN_TAB.
  DELETE ADJACENT DUPLICATES FROM TEMSEFIN_TAB.
   LOOP AT TEMSEFIN_TAB .                                   "gg1274331"
     I2_KEY-TANNO = TEMSEFIN_TAB-TANNO.
     I2_KEY-PERNR = TEMSEFIN_TAB-PERNR.
     I2_KEY-FPBEG = TEMSEFIN_TAB-FPBEG. "DATES_TAB-FPBEG.
     I2_KEY-FPEND = TEMSEFIN_TAB-FPEND.
     I2_KEY-IDTY  = ' '.
     RP-INIT-BUFFER.
     RP-REF-C4-I2.
     RP-REF-C4-I2-O.
     RP-IMP-C4-I2.
     IF RP_IMP_I2_SUBRC = 0.
       READ TABLE P4DEDT  WITH KEY TANNO = TEMSEFIN_TAB-TANNO
                                  PERNR = TEMSEFIN_TAB-PERNR
                                  FPBEG = TEMSEFIN_TAB-FPBEG
                                  FPEND = TEMSEFIN_TAB-FPEND.
       MOVE-CORRESPONDING P4DEDT TO TEMSEFIN_TAB.
       IF TEMSEFIN_TAB-BETRG IS INITIAL.
             MOVE P4DEDT-AMONT TO TEMSEFIN_TAB-BETRG.
       ENDIF.
       I1_KEY-TANNO =  TEMSEFIN_TAB-TANNO.
       IF TEMSEFIN_TAB-FPPER EQ '000000'.
         TFPPER = TEMSEFIN_TAB-FPEND.
         IF TFPPER+4(2) EQ 12.
           TFPPER+4(2) =  '01'.
           TFPPER+0(4) =    ( TFPPER+0(4) ) + 1.
         ELSE.
           TFPPER+4(2) =  ( TEMSEFIN_TAB-FPEND+4(2) ) + 1 .
         ENDIF.
         TFPPER+6(2) =   '01'.
         TFPPER      = TFPPER - 1  .

         I1_KEY-FPEND =  TFPPER.
       ELSE.
         I1_KEY-FPEND =  TEMSEFIN_TAB-FPEND.
       IF rehiring = 'X'.
         t_enddate = temsefin_tab-fpend.
         LOOP AT P0001 where begda le t_enddate and endda ge t_enddate.
           IF sy-subrc EQ 0.
             SELECT single * from T549A where abkrs = P0001-abkrs.
                t_permo = T549A-permo.
             SELECT single * from T549Q where permo = t_permo and pabrp = temsefin_tab-fpper+4(2) and pabrj = temsefin_tab-fpper+0(4).
                 t_endda = T549Q-endda.
           ENDIF.
         ENDLOOP.
          I1_KEY-FPEND = t_endda.
       ENDIF.
      ENDIF.

       clear: t_enddate, t_permo, t_endda.

       RP-INIT-BUFFER.
       RP-REF-C4-I1.
       RP-REF-C4-I1-O.
       RP-IMP-C4-I1.
       IF RP_IMP_I1_SUBRC = 0.
         READ TABLE P4CHLN WITH KEY CHLNO = TEMSEFIN_TAB-CHLNO
                                    TANNO = TEMSEFIN_TAB-TANNO
                                     CHDAT = TEMSEFIN_TAB-CHDAT.
         MOVE-CORRESPONDING P4CHLN TO TEMSEFIN_TAB.
         MODIFY  TEMSEFIN_TAB.
*         CLEAR TEMSEFIN_TAB. "GEETHA "

       ENDIF .
     ELSE.
       MOVE :  TEMSEFIN_TAB-FPBEG TO TEMSEFPBEGDA,
               TEMSEFIN_TAB-FPEND TO TEMSEFPENDDA.
       do.
         i2_key-idty = i2_key-idty + 1.
         RP-INIT-BUFFER.
         RP-REF-C4-I2.
         RP-REF-C4-I2-O.
         RP-IMP-C4-I2.
         IF RP_IMP_I2_SUBRC = 0.
           READ TABLE P4DEDT  WITH KEY TANNO = TEMSEFIN_TAB-TANNO
                                       PERNR = TEMSEFIN_TAB-PERNR  FPBEG = TEMSEFPBEGDA
                                       FPEND = TEMSEFPENDDA.


           MOVE-CORRESPONDING P4DEDT TO TEMSEFIN_TAB.
           IF TEMSEFIN_TAB-BETRG IS INITIAL.
             MOVE P4DEDT-AMONT TO TEMSEFIN_TAB-BETRG.
          ENDIF.

           RATIO = TEMSEFIN_TAB-BETRG / TEMSEFIN_TAB-TTAX.
           I1_KEY-TANNO =  TEMSEFIN_TAB-TANNO.
           IF TEMSEFIN_TAB-FPPER EQ '000000'.
             TFPPER = TEMSEFIN_TAB-FPEND.
             IF TFPPER+4(2) EQ 12.
               TFPPER+4(2) =  '01'.
               TFPPER+0(4) =    ( TFPPER+0(4) ) + 1.
             ELSE.
               TFPPER+4(2) =  ( TEMSEFIN_TAB-FPEND+4(2) ) + 1 .
*           TFPPER+6(2) =   '01'.
*           TFPPER      = TFPPER - 1  .
             ENDIF.
             TFPPER+6(2) =   '01'.
             TFPPER      = TFPPER - 1  .


             I1_KEY-FPEND =  TFPPER.
           ELSE.
             I1_KEY-FPEND =  TEMSEFIN_TAB-FPEND.
       IF rehiring = 'X'.
         t_enddate = temsefin_tab-fpend.
         LOOP AT P0001 where begda le t_enddate and endda ge t_enddate.
           IF sy-subrc EQ 0.
             SELECT single * from T549A where abkrs = P0001-abkrs.
                t_permo = T549A-permo.
             SELECT single * from T549Q where permo = t_permo and pabrp = temsefin_tab-fpper+4(2) and pabrj = temsefin_tab-fpper+0(4).
                 t_endda = T549Q-endda.
           ENDIF.
         ENDLOOP.
          I1_KEY-FPEND = t_endda.
       ENDIF.
      ENDIF.

       clear: t_enddate, t_permo, t_endda.

           RP-INIT-BUFFER.
           RP-REF-C4-I1.
           RP-REF-C4-I1-O.
           RP-IMP-C4-I1.
           IF RP_IMP_I1_SUBRC = 0.
             READ TABLE P4CHLN WITH KEY CHLNO = TEMSEFIN_TAB-CHLNO
                                        TANNO = TEMSEFIN_TAB-TANNO
                                        CHDAT = TEMSEFIN_TAB-CHDAT.
             MOVE-CORRESPONDING P4CHLN TO TEMSEFIN_TAB.
             if temsefin_tab-pernr ne w_pernr or temsefin_tab-fpper
                        ne w_payper or temsefin_tab-paydate ne w_paydate.

               incometax1 = TEMSEFIN_TAB-INCOMETAX.
               surcharge1 = TEMSEFIN_TAB-SURCHARGE.
               educess1   = TEMSEFIN_TAB-EDUCESS.
               TEMSEFIN_TAB-SURCHARGE = surcharge1 * RATIO.
               TEMSEFIN_TAB-EDUCESS   = educess1 *   RATIO.
               TEMSEFIN_TAB-INCOMETAX = temsefin_tab-betrg - ( temsefin_tab-surcharge + temsefin_tab-educess ).
               MODIFY TEMSEFIN_TAB.
                SUM460                 = SUM460 + TEMSEFIN_TAB-BETRG.
             ELSE.
               TEMSEFIN_TAB-SURCHARGE = surcharge1 * RATIO.
               TEMSEFIN_TAB-EDUCESS   = educess1 * RATIO.
               TEMSEFIN_TAB-INCOMETAX = temsefin_tab-betrg - ( temsefin_tab-surcharge + temsefin_tab-educess ).
               MOVE-CORRESPONDING TEMSEFIN_TAB TO TEMSEFIN_TAB1.
               APPEND  TEMSEFIN_TAB1.
               SUM460                 = SUM460 + TEMSEFIN_TAB1-BETRG.

             ENDIf.

             MOVE : TEMSEFIN_TAB-PERNR TO W_PERNR,
                    TEMSEFIN_TAB-fpper TO W_payper,
                    TEMSEFIN_TAB-PAYDATE TO W_PAYDATE.
           ENDIF.
           CLEAR TEMSEFIN_TAB1.
         ELSE .
           EXIT.
         ENDIF.
       enddo.
       READ TABLE TEMSEFIN_TABRT WITH KEY PERNR = TEMSEFIN_TAB-PERNR
                                          PAYDATE = TEMSEFIN_TAB-PAYDATE.
              CLEAR :L_PERNR,L_FPPER.
             IF TEMSEFIN_TABRT-TTAX NE SUM460.
               DELETE TEMSEFIN_TAB WHERE PERNR = TEMSEFIN_TABRT-PERNR AND PAYDATE = TEMSEFIN_TABRT-PAYDATE.
               DELETE TEMSEFIN_TAB1 WHERE PERNR = TEMSEFIN_TABRT-PERNR AND PAYDATE = TEMSEFIN_TABRT-PAYDATE.
               MOVE TEMSEFIN_TAB-PERNR TO L_PERNR.
               MOVE TEMSEFIN_TAB-FPPER TO L_FPPER.
               MESSAGE S306(HRPADIN01) WITH L_PERNR  L_FPPER.
               PERFORM BUILD_ERROR TABLES HR_ERROR
                         USING SPACE SY-MSGID SY-MSGNO
                         L_PERNR  L_FPPER  SPACE  SPACE.

             ENDIF.
       CLEAR TEMSEFIN_TAB.
       clear sum460.
*       CLEAR TEMSEFIN_TAB.
     ENDIF.
   ENDLOOP.
   APPEND LINES OF TEMSEFIN_TAB1 TO TEMSEFIN_TAB.
   CLEAR TEMSEFIN_TAB.
   SORT TEMSEFIN_TAB BY PERNR PAYDATE .        "gg1274331"
   LOOP AT TEMSEFIN_TABRT .
     LOOP AT  TEMSEFIN_TAB  WHERE   PERNR = TEMSEFIN_TABRT-PERNR AND
                                    PAYDATE = TEMSEFIN_TABRT-PAYDATE AND
                                    FPPER = TEMSEFIN_TABRT-FPPER.

       SUM4MT = SUM4MT + TEMSEFIN_TAB-INCOMETAX.
       SUM4MS = SUM4MS + TEMSEFIN_TAB-SURCHARGE.
       SUM4ME = SUM4ME + TEMSEFIN_TAB-EDUCESS.

     ENDLOOP.
     MOVE :   SUM4MT TO SUM4MT1,
              SUM4MS TO SUM4MS1,
              SUM4ME TO SUM4ME1.
     DELTA4MT  =  ( TEMSEFIN_TABRT-INCOMETAX - SUM4MT ).
     DELTA4MS  =  ( TEMSEFIN_TABRT-SURCHARGE - SUM4MS ).
     DELTA4ME  =  ( TEMSEFIN_TABRT-EDUCESS -  SUM4ME  ).
     IF DELTA4MT NE 0 OR DELTA4MS NE 0 OR DELTA4ME NE 0.
       IF DELTA4MT NE 0 .
         SUM4MT  = SUM4MT + DELTA4MT.
       ENDIF.
       IF DELTA4MS NE 0.
         SUM4MS = SUM4MS + DELTA4MS.
       ENDIF.
       IF DELTA4ME NE 0.
         SUM4ME = SUM4ME + DELTA4ME.
       ENDIF.

       LOOP AT  TEMSEFIN_TAB  WHERE   PERNR = TEMSEFIN_TABRT-PERNR AND
                                      PAYDATE = TEMSEFIN_TABRT-PAYDATE.
         IF NOT SUM4ME1 IS INITIAL.
         TEDUCESS   = ( ( TEMSEFIN_TAB-EDUCESS * DELTA4ME ) / SUM4ME1 ) .
         ENDIF.
         TEMSEFIN_TAB-EDUCESS = TEMSEFIN_TAB-EDUCESS +  TEDUCESS .
         IF NOT SUM4MS1 IS INITIAL.
         TSURCHARGE   = ( ( TEMSEFIN_TAB-SURCHARGE * DELTA4MS ) / SUM4MS1 ).
         ENDIF.
         TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE +  TSURCHARGE.
         TEMSEFIN_TAB-INCOMETAX = TEMSEFIN_TAB-BETRG - ( TEMSEFIN_TAB-EDUCESS + TEMSEFIN_TAB-SURCHARGE ).
         MODIFY TEMSEFIN_TAB.
         CLEAR TEDUCESS.
         CLEAR TSURCHARGE.

       ENDLOOP.
     ENDIF.
     CLEAR  SUM4MT .
     CLEAR  SUM4MS .
     CLEAR  SUM4ME.
     CLEAR  DELTA4MT.
     CLEAR  DELTA4MS.
     CLEAR  DELTA4ME.


   ENDLOOP.
 ENDFORM.                    "GET_CHALLAN
*&---------------------------------------------------------------------*
*&      Form  GET_TAN_TEMSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PME01  text
*      <--P_TEMSE_TAN  text
*----------------------------------------------------------------------*
FORM GET_TAN_TEMSE  USING    PME01 type PME01
                    CHANGING P_TEMSE_TAN.
   CLEAR ECC_TAB.
   REFRESH ECC_TAB.
IF wa_t7insw-begda > pbegda OR wa_t7insw-reval IS INITIAL.
   CALL FUNCTION 'HR_FEATURE_BACKTABLE'
       EXPORTING
            FEATURE                     = '40ECC'
            STRUC_CONTENT               = PME01
            KIND_OF_ERROR               = '3'
       TABLES
            BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
      EXCEPTIONS
           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
           OTHERS                      = 0
            .
     IF SY-SUBRC = 0.                                         "#EC *
       CLEAR ECC_TAB.
       READ TABLE ECC_TAB INDEX 3.
       P_TEMSE_TAN = ECC_TAB-BACK+0(15).
     ENDIF.
ELSE.
    RP-PROVIDE-FROM-LAST P0185 05 PBEGDA PENDDA.
    P_TEMSE_TAN = t_tanno.
ENDIF.
ENDFORM.                    " GET_TAN_TEMSE

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
     AND konst = 'INAFY'.
*     AND begda <= pendda
*     AND endda >= pbegda.
  IF sy-subrc = 0.
    pv_afy_switch = lv_inafy.
  ENDIF.                               " IF sy-subrc = 0.
ENDFORM.                               " FORM IS_AFY_IMPLEMENTED
* End of AFY change
*&---------------------------------------------------------------------*
*&      Form  GET_TAN_TEMSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PME01  text
*      <--P_TEMSE_TAN  text
*----------------------------------------------------------------------*
FORM GET_TAN_TEMSE_1  USING    PME01 type PME01 PBEGDA
                      CHANGING P_TEMSE_TAN.
   CLEAR ECC_TAB.
   REFRESH ECC_TAB.
IF wa_t7insw-begda > pbegda OR wa_t7insw-reval IS INITIAL.
   CALL FUNCTION 'HR_FEATURE_BACKTABLE'
       EXPORTING
            FEATURE                     = '40ECC'
            STRUC_CONTENT               = PME01
            KIND_OF_ERROR               = '3'
       TABLES
            BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
      EXCEPTIONS
           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
           OTHERS                      = 0
            .
     IF SY-SUBRC = 0.                                         "#EC *
       CLEAR ECC_TAB.
       READ TABLE ECC_TAB INDEX 3.
       P_TEMSE_TAN = ECC_TAB-BACK+0(15).
     ENDIF.
ELSE.
*    RP-PROVIDE-FROM-LAST P0185 05 PBEGDA PENDDA.
    LOOP AT P0185 WHERE subty = '05' AND begda <= pbegda AND endda >= endda.
      P_TEMSE_TAN = p0185-icnum.
    ENDLOOP.
ENDIF.
ENDFORM.                    " GET_TAN_TEMSE
*&---------------------------------------------------------------------*
*&      Form  READ_COCD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KEYFLD  text
*----------------------------------------------------------------------*
FORM READ_COCD  USING    P_KEYFLD.

LOOP AT TMP_COCD WHERE BUKRS = P_KEYFLD.

     CLEAR FLAG_TAX.
     f16_cocd   = tmp_cocd-bukrs.
     f16_cntr2  = tmp_cocd-cntr2.
     f16_begda  = tmp_cocd-begda.
     f16_endda  = tmp_cocd-endda.
     f16_stat2  = tmp_cocd-stat2.

* Import country cluster
     PERFORM IMPORT_RESULTS.

     IF NEXT = 'X'.      "PRANT981544
       CLEAR NEXT.
       CONTINUE.
     ENDIF.
*     READ TABLE TEMSEFIN_TAB WITH KEY PERNR = PERNR-PERNR FPBEG = TMP_COCD-BEGDA FPEND = TMP_COCD-ENDDA.
    LOOP AT TEMSEFIN_TAB INTO wa_temsefin_tab WHERE FPBEG >= TMP_COCD-BEGDA AND FPEND <= TMP_COCD-ENDDA.
      IF WA_TEMSEFIN_TAB-TTAX NE 0.
          FLAG_TAX = 'X'.
          EXIT.
      ENDIF.
    ENDLOOP.
*    LOOP AT TEMSEFIN_TAB INTO wa_temsefin_tab.
*      total_tax = total_tax + wa_temsefin_tab-ttax.
*    ENDLOOP.
    IF REHIRING = 'X' AND FLAG_TAX = ' ' AND FOUND NE 'X'.
*         IF WA_TEMSEFIN_TAB-TTAX EQ 0.
          CONTINUE.
*         ENDIF.
    ENDIF.
   IF FOUND IS INITIAL AND REHIRING NE 'X'.
      REJECT.
   ENDIF.
* Fill the header table for the employee.
     PERFORM FILL_HD_TAB.


     PERFORM FILL_MAIN_TAB.

     PERFORM FILL_FINAL_TAB.

*     PERFORM read_temse.         " MDSNT927906

     PERFORM FILL_ACKNO_TAB.     " RK1060098

     PERFORM FILL_INT_S80.

     PERFORM FILL_INT_S88.

     PERFORM FILL_GROSS_TAB.

     PERFORM FILL_IFOS_TAB.

     PERFORM FILL_PERK_TAB.

     PERFORM FILL_SEC10_TAB.
     CONTI = 1.

   ENDLOOP.

ENDFORM.                    " READ_COCD
*&---------------------------------------------------------------------*
*&      Form  READ_COCD_TAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COCD_TAN .
DATA: n type I.
DESCRIBE TABLE TMP_COCD LINES n.
LOOP AT TMP_COCD.

     CLEAR FLAG_TAX.
     f16_cocd   = tmp_cocd-bukrs.
     f16_cntr2  = tmp_cocd-cntr2.
     f16_begda  = tmp_cocd-begda.
     IF sy-tabix = n AND multiple_f16 = 'X' .
       f16_begda = pbegda.
     ENDIF.
     f16_endda  = tmp_cocd-endda.
     f16_stat2  = tmp_cocd-stat2.
     RP-PROVIDE-FROM-LAST P0185 '05' F16_BEGDA F16_ENDDA.
     IF sy-subrc = 0 AND TAN_NO IS INITIAL.
       t_tanno = p0185-icnum.
       emp_panno = p0185-ICOLD.
       emp_girno = p0185-AUTH1.
     ELSEIF sy-subrc = 0 AND ( P0185-ICNUM IN TAN_NO ).
       t_tanno = p0185-icnum.
       emp_panno = p0185-ICOLD.
       emp_girno = p0185-AUTH1.
     ELSEIF sy-subrc = 0 AND NOT ( P0185-ICNUM IN TAN_NO ).
       continue.
     ENDIF.
* Import country cluster
     PERFORM IMPORT_RESULTS_TAN.

     IF NEXT = 'X'.      "PRANT981544
       CLEAR NEXT.
       CONTINUE.
     ENDIF.

*     READ TABLE TEMSEFIN_TAB WITH KEY PERNR = PERNR-PERNR FPBEG = TMP_COCD-BEGDA FPEND = TMP_COCD-ENDDA.
    LOOP AT TEMSEFIN_TAB INTO wa_temsefin_tab WHERE FPBEG >= TMP_COCD-BEGDA AND FPEND <= TMP_COCD-ENDDA.
      IF WA_TEMSEFIN_TAB-TTAX NE 0.
          FLAG_TAX = 'X'.
          EXIT.
      ENDIF.
    ENDLOOP.
    CLEAR wa_temsefin_tab.
    LOOP AT TEMSEFIN_TAB INTO wa_temsefin_tab.
      total_tax = total_tax + wa_temsefin_tab-ttax.
    ENDLOOP.
    IF REHIRING = 'X' AND FLAG_TAX = ' ' AND FOUND NE 'X'.
*         IF WA_TEMSEFIN_TAB-TTAX EQ 0.
          CONTINUE.
*         ENDIF.
    ENDIF.
   IF FOUND IS INITIAL AND REHIRING NE 'X'.
      REJECT.
   ENDIF.
* Fill the header table for the employee.
     PERFORM FILL_HD_TAB_TAN.


     PERFORM FILL_MAIN_TAB.

     PERFORM FILL_FINAL_TAB.

*     PERFORM read_temse.         " MDSNT927906

     PERFORM FILL_ACKNO_TAB.     " RK1060098

     PERFORM FILL_INT_S80.

     PERFORM FILL_INT_S88.

     PERFORM FILL_GROSS_TAB.

     PERFORM FILL_IFOS_TAB.

     PERFORM FILL_PERK_TAB.

     PERFORM FILL_SEC10_TAB.
     CONTI = 1.

   ENDLOOP.


ENDFORM.                    " READ_COCD_TAN
*&---------------------------------------------------------------------*
*&      Form  FILL_HD_TAB_TAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_HD_TAB_TAN .

  CLEAR HD_TAB.
*  Read table T7INT5 to obtain employer info.
   PERFORM RE_T7INT5 USING T7IN0P-TXGRP.
   PERFORM READ_NAME USING SY-LANGU CHANGING HD_TAB-ENAME.
   MOVE PERNR-PERNR TO HD_TAB-PERNR.
   RP-PROVIDE-FROM-LAST P0185 '02' PBEGDA PENDDA.
   IF sy-subrc = 0.
     MOVE P0185-ICNUM TO HD_TAB-ICNUM.
   ENDIF.
   MOVE P0002-GESCH TO HD_TAB-GENDER.
   MOVE P0021-FAVOR TO HD_TAB-FFNAME.
   MOVE P0021-FANAM TO HD_TAB-FLNAME.
   MOVE P0006-STRAS TO HD_TAB-HNUMB.
   MOVE P0006-LOCAT TO HD_TAB-LOCALITY.
   MOVE P0006-PSTLZ TO HD_TAB-PIN.
   MOVE P0006-ORT01 TO HD_TAB-CITY.
   MOVE P0006-LAND1 TO HD_TAB-COUNTRY.
   MOVE P0006-TELNR TO HD_TAB-TELN.
   IF HD_TAB-GENDER = 1.
     HD_TAB-GENDER = 'M'.
   ELSE.
     HD_TAB-GENDER = 'F'.
   ENDIF.
   MOVE P0002-GBDAT TO HD_TAB-DOB.
   MOVE F16_CNTR2 TO HD_TAB-CNTR2.
   CLEAR COCD.
   READ TABLE COCD WITH KEY CNTR2 = F16_CNTR2 STAT2 = '3'.
   MOVE F16_BEGDA TO HD_TAB-F16_BEGDA.
*   MOVE COCD-ENDDA TO HD_TAB-F16_ENDDA.
   READ TABLE COCD WITH KEY CNTR2 = F16_CNTR2.  "PRANT981544
   IF SY-SUBRC = 0.
     MOVE COCD-ENDDA TO HD_TAB-F16_ENDDA.
   ENDIF.

   RP-PROVIDE-FROM-LAST P0001 SPACE F16_BEGDA COCD-ENDDA.
   CLEAR T528T.
   PERFORM GET_POSIT USING P0001-OTYPE
                           P0001-PLANS
                           P0001-ENDDA
                     CHANGING HD_TAB-POSITION.

  HD_TAB-TANNO = t_tanno.
  HD_TAB-PANNO = emp_panno.
  HD_TAB-GIRNO = emp_girno.
*  ENDIF.

   APPEND HD_TAB.


ENDFORM.                    " FILL_HD_TAB_TAN
*&---------------------------------------------------------------------*
*&      Form  IMPORT_RESULTS_TAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPORT_RESULTS_TAN .
DATA : TMP_COCD_INDX LIKE SY-TABIX.

   DATA : TMP_DATE LIKE COCD-BEGDA.
   DATA : TMP_PER LIKE RGDIR-FPPER.
   DATA : N TYPE I.
   DATA : LV_CH_DATE(10) TYPE C. "char date
   DATA : LV_YEAR TYPE PIN_TAXYR.
   DATA: PAY-YEAR(4),
         PAY-PERIOD(2),
         period-endate TYPE DATUM.
   DATA:  PAY-PERIOD1 like T009B-POPER."gg"1274331"
   DATA: BEGIN OF RGDIR_TMP OCCURS 100.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_TMP.
   DATA: BEGIN OF RGDIR_WA.
         INCLUDE STRUCTURE PC261.
   DATA: END OF RGDIR_WA.
   DATA: TMP_PERMO like RGDIR-PERMO,
         TMP_ABKRS like RGDIR-ABKRS.

   SORT RGDIR BY FPPER.
   CLEAR FOUND.
   CLEAR CONTI.
   REFRESH TMP_RGDIR.
   TMP_RGDIR[] = RGDIR[].

   IF MULTIPLE_F16 = 'X' OR REHIRING = 'X'.
*    If company using multiple form 16.
*    Eliminate the results which are in the next financial year.
     TMP_COCD_INDX = SY-TABIX.

     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> '' .
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> '' .
       IF TMP_RGDIR-PAYTY <> ''.
        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY
         IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
            TMP_RGDIR-FPBEG+4(2) = '02' OR
            TMP_RGDIR-FPBEG+4(2) = '03'.
           IF TMP_RGDIR-INPER NE '000000'.
             IF TMP_RGDIR-INPER+0(4) EQ YEAR.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
             ELSE.
              CONTINUE.
             ENDIF.
           ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
           ENDIF.
         ELSE.           "If inperiod is not JAN,FEB,MAR
           CONTINUE.
         ENDIF.

        ELSE.    "If In-period is same as input of selection screen
         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
        MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
        APPEND RGDIR.
       ENDIF.
     ENDLOOP.
     CLEAR TMP_ABKRS.
     TMP_ABKRS = TMP_RGDIR-ABKRS.
     CLEAR RGDIR.
     SORT RGDIR BY SEQNR.
     READ TABLE REHIRE_DATES INDEX TMP_COCD_INDX.
     IF SY-SUBRC = 0.
       TMP_DATE = REHIRE_DATES-P_DATE.
       IF REHIRING = 'X'.
         clear: period-endate, tmp_permo.
         select single permo into tmp_permo
                  from t549a
                  where abkrs = TMP_ABKRS.
         select  SINGLE ENDDA into PERIOD-ENDATE
                  from t549q
                  where permo = tmp_permo
                    and BEGDA <= F16_ENDDA
                    AND ENDDA >= F16_ENDDA.
         loop at rgdir where fpbeg >= f16_begda AND fpend <= PERIOD-ENDATE.
           clear: RGDIR_WA, RGDIR_TMP.
           Refresh RGDIR_TMP.
           if rgdir-fpper = rgdir-inper.
             tmp_per = rgdir-fpper.

             clear TEMSEFIN_TAB.
             IF RGDIR-PAYTY  EQ ' ' .                       "gg1274331"
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                              PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                              PAYPER+0(2)  =  RGDIR-FPPER+4(2).
             ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
               CLEAR : LV_CH_DATE.
               PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
               READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                              PAYDATE  =  LV_CH_DATE.

             ENDIF.
             IF SY-SUBRC NE 0.
               MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.      "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
               MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
               MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
               MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.    "gg1274331"
               MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
               MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid.
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by inper.
               read table rgdir_tmp index 1.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
               loop at rt.
                 CASE RT-LGART.
                   WHEN '/4MT'.
                     TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                     TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                   WHEN '/4MS'.
                     TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                     TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                   WHEN '/4ME' or '/4MH'.
                     TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                     TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                   WHEN '/460'.
                     TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                     TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                 ENDCASE.
               endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-paydt changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

               DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
                     result(1),
                     flag.
               clear flag.
*                     CHECK_RECORD TYPE C.

               TRY.
                   GET BADI BD_TAX
                     FILTERS
                       FLT_VAL = '40'.

                   CALL BADI BD_TAX->CHK_F24Q
                     EXPORTING
                       empno         = pernr-pernr
                       begda         = rgdir-fpbeg
                       endda         = rgdir-fpend
                       rgdir_table   = rgdir[]
                       results_table = rt[]
                       f16_table     = f16[]
                       f16_cntr2     = f16_cntr2
                       flt_val       = '40'
                     IMPORTING
                       result        = result.
                   flag = 'X'.

                 CATCH CX_BADI_NOT_IMPLEMENTED.
               ENDTRY.

               IF  RESULT <> ''AND FLAG = 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
                 CLEAR  TEMSEFIN_TAB.
                 CLEAR  TEMSEFIN_TABRT.
               elseIF FLAG NE 'X'.
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.
                 CLEAR  TEMSEFIN_TAB.
                 CLEAR  TEMSEFIN_TABRT.
               ENDIF.
               clear flag.
             endif.
             endif.

           endloop.
         ELSE.
           LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= TMP_DATE.
         clear: RGDIR_WA, RGDIR_TMP.
         Refresh RGDIR_TMP.
             IF RGDIR-FPPER = RGDIR-INPER.
               TMP_PER = RGDIR-FPPER.
               clear TEMSEFIN_TAB.
               IF RGDIR-PAYTY  EQ ' ' .                     "gg1274331"
                 READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                                PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                                PAYPER+0(2)  =  RGDIR-FPPER+4(2).
               ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
                 CLEAR : LV_CH_DATE.
                 PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
                 READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                                PAYDATE  =  LV_CH_DATE.

               ENDIF.
               IF SY-SUBRC NE 0.
                 MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.    "gg1274331"
                 MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
                 MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
                 MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
                 MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*               MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
                 MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.  "gg1274331"
                 MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
                 MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*********pickup the latest values of monthly tax wage types
               loop at rgdir into  rgdir_wa
                            where fpbeg = rgdir-fpbeg
                              and fpend = rgdir-fpend
                              and payid = rgdir-payid.
                   move-corresponding rgdir_wa to rgdir_tmp.
                   append rgdir_tmp.
               endloop.
               sort rgdir_tmp descending by inper.
               read table rgdir_tmp index 1.
               PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*               PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
                 loop at rt.
                   CASE RT-LGART.
                     WHEN '/4MT'.
                       TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                       TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
                     WHEN '/4MS'.
                       TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                       TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

                     WHEN '/4ME' or '/4MH'.
                       TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                       TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
                     WHEN '/460'.
                       TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                       TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
                   ENDCASE.
                 endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-PAYDT changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*    DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
*           RESULT(1).
                CLEAR FLAG.
                 TRY.
                     GET BADI BD_TAX
                       FILTERS
                         FLT_VAL = '40'.

                     CALL BADI BD_TAX->CHK_F24Q
                       EXPORTING
                         empno         = pernr-pernr
                         begda         = rgdir-fpbeg
                         endda         = rgdir-fpend
                         rgdir_table   = rgdir[]
                         results_table = rt[]
                         f16_table     = f16[]
                         f16_cntr2     = f16_cntr2
                         flt_val       = '40'
                       IMPORTING
                         result        = result.
                         FLAG = 'X'.

                   CATCH CX_BADI_NOT_IMPLEMENTED.
                 ENDTRY.

               IF  RESULT <> '' AND FLAG = 'X'.
                   APPEND TEMSEFIN_TAB.
                   APPEND TEMSEFIN_TABRT.

                   CLEAR  TEMSEFIN_TAB.
                   CLEAR  TEMSEFIN_TABRT.

               ELSEIF FLAG NE 'X'..
                 APPEND TEMSEFIN_TAB.
                 APPEND TEMSEFIN_TABRT.

                 CLEAR  TEMSEFIN_TAB.
                 CLEAR  TEMSEFIN_TABRT.
               endif.
               CLEAR FLAG.

             ENDIF.
             ENDIF.
           ENDLOOP.
         ENDIF.
         IF SY-SUBRC = 0.
           LOOP AT RGDIR WHERE FPPER <= TMP_PER AND FPEND <= TMP_DATE.
           ENDLOOP.

           IF SY-SUBRC = 0.
             RX-KEY-SEQNO = RGDIR-SEQNR.
             RX-KEY-PERNR = PERNR-PERNR.
             RP-IMP-C2-IN.
             FOUND = 'X'.
           ENDIF.
         ELSE.
           IF TERM_PREV_FY = 'X'.
             NEXT = 'X'.
             FOUND ='X'.
           ENDIF.
         ENDIF.
       ENDIF.
     ELSE.
*    IF THE COMPANY USES SINGLE FORM 16.
     REFRESH RGDIR.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR OR   PAYTY <> '' .
       MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
       APPEND RGDIR.
     ENDLOOP.
     PERFORM PAYMENTS_TAB.
* to stop picking the cross financial years record
     REFRESH RGDIR.
* Finding next year
     CLEAR LV_YEAR.
     LV_YEAR = YEAR + 1.
     LOOP AT TMP_RGDIR WHERE INPER+(4) = YEAR
                          OR PAYTY <> '' .
       IF TMP_RGDIR-PAYTY <> ''.
        IF TMP_RGDIR-FPBEG+0(4) EQ LV_YEAR. "If year is not same as FY
         IF TMP_RGDIR-FPBEG+4(2) = '01' OR  "If inperiod is JAN,FEB,MAR
            TMP_RGDIR-FPBEG+4(2) = '02' OR
            TMP_RGDIR-FPBEG+4(2) = '03'.
           IF TMP_RGDIR-INPER NE '000000'.
             IF TMP_RGDIR-INPER+0(4) EQ YEAR.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
             ELSE.
              CONTINUE.
             ENDIF.
           ELSE.
              MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
              APPEND RGDIR.
           ENDIF.
         ELSE.           "If inperiod is not JAN,FEB,MAR
           CONTINUE.
         ENDIF.

        ELSE.    "If In-period is same as input of selection screen
         IF TMP_RGDIR-INPER NE '000000'.
           IF TMP_RGDIR-INPER+0(4) EQ YEAR.
             MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
             APPEND RGDIR.
           ELSE.
               CONTINUE.
           ENDIF.
         ELSE.
           MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
           APPEND RGDIR.
         ENDIF.
        ENDIF.

       ELSE. "result appended in case of regular payroll
         MOVE-CORRESPONDING TMP_RGDIR TO RGDIR.
         APPEND RGDIR.
       ENDIF.
     ENDLOOP.

       LOOP AT RGDIR WHERE FPBEG >= F16_BEGDA AND FPEND <= F16_ENDDA.
       clear: RGDIR_WA, RGDIR_TMP.
       Refresh RGDIR_TMP.
         IF RGDIR-FPPER = RGDIR-INPER.      "PRANT981544
           RX-KEY-SEQNO = RGDIR-SEQNR.
           FOUND = 'X'.
         clear TEMSEFIN_TAB.
         IF RGDIR-PAYTY  EQ ' ' .  "GEETHA"
           READ  TABLE EXCEL_TAB WITH KEY EMP_NO  =  PERNR-PERNR
                                          PAYPER+2(4)  =  RGDIR-FPPER+0(4)
                                          PAYPER+0(2)  =  RGDIR-FPPER+4(2).
         ELSE.
* To convert the RGDIR-PAYDT to char10 format to match the
* format of paydate in excel_tab
           CLEAR : LV_CH_DATE.
           PERFORM CONV_DAT_TO_CHAR USING RGDIR-PAYDT CHANGING LV_CH_DATE.
           READ  TABLE EXCEL_TAB WITH KEY EMP_NO   =  PERNR-PERNR
                                          PAYDATE  =  LV_CH_DATE.
         ENDIF.
         IF SY-SUBRC NE 0.
           MOVE PERNR-PERNR TO TEMSEFIN_TAB-PERNR.          "gg1274331"
           MOVE RGDIR-PAYDT TO TEMSEFIN_TAB-PAYDATE.
           MOVE RGDIR-FPPER TO TEMSEFIN_TAB-FPPER.
           MOVE RGDIR-FPBEG to TEMSEFIN_TAB-FPBEG.
           MOVE RGDIR-FPEND TO TEMSEFIN_TAB-FPEND.
*         MOVE T_TANNO to TEMSEFIN_TAB-TANNO.
           MOVE PERNR-PERNR TO TEMSEFIN_TABRT-PERNR.
           MOVE RGDIR-PAYDT TO TEMSEFIN_TABRT-PAYDATE.
           MOVE RGDIR-FPPER TO TEMSEFIN_TABRT-FPPER.
*******to pickup the latest values of monthly tax wage types
         loop at rgdir into  rgdir_wa
                      where fpbeg = rgdir-fpbeg
                        and fpend = rgdir-fpend
                        and payid = rgdir-payid.
             move-corresponding rgdir_wa to rgdir_tmp.
             append rgdir_tmp.
         endloop.
         sort rgdir_tmp descending by inper.
         read table rgdir_tmp index 1.
         PERFORM IMPORT_CURRRT USING RGDIR_TMP-SEQNR.
*         PERFORM IMPORT_CURRRT USING RGDIR-SEQNR.
           loop at rt.
             CASE RT-LGART.
               WHEN '/4MT'.
                 TEMSEFIN_TAB-INCOMETAX   = TEMSEFIN_TAB-INCOMETAX + RT-BETRG.
                 TEMSEFIN_TABRT-INCOMETAX = TEMSEFIN_TABRT-INCOMETAX + RT-BETRG.
               WHEN '/4MS'.
                 TEMSEFIN_TAB-SURCHARGE = TEMSEFIN_TAB-SURCHARGE + RT-BETRG.
                 TEMSEFIN_TABRT-SURCHARGE = TEMSEFIN_TABRT-SURCHARGE + RT-BETRG.

               WHEN '/4ME' or '/4MH'.
                 TEMSEFIN_TAB-EDUCESS  =  TEMSEFIN_TAB-EDUCESS + RT-BETRG.
                 TEMSEFIN_TABRT-EDUCESS = TEMSEFIN_TABRT-EDUCESS + RT-BETRG.
               WHEN '/460'.
                 TEMSEFIN_TAB-TTAX    = TEMSEFIN_TAB-TTAX + RT-BETRG.
                 TEMSEFIN_TABRT-TTAX  = TEMSEFIN_TABRT-TTAX + RT-BETRG.
             ENDCASE.
           endloop.
* To pick the TAN No according to the original organisational
* status of the Employee that is based on the wpbp state of
* the employee
               clear : temse_tan, PME01.
               sort wpbp descending by endda.
               read table wpbp index 1.
               move-corresponding wpbp to PME01.
*               CALL FEATURE
               perform get_tan_temse_1 using PME01 rgdir-paydt changing temse_tan.
               Move temse_tan to TEMSEFIN_TAB-TANNO.

*        DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK.
*                     CHECK_RECORD TYPE C.
           CLEAR FLAG.

           TRY.
               GET BADI BD_TAX
                 FILTERS
                   FLT_VAL = '40'.

               CALL BADI BD_TAX->CHK_F24Q
                 EXPORTING
                   empno         = pernr-pernr
                   begda         = rgdir-fpbeg
                   endda         = rgdir-fpend
                   rgdir_table   = rgdir[]
                   results_table = rt[]
                   f16_table     = f16[]
                   f16_cntr2     = f16_cntr2
                   flt_val       = '40'
                 IMPORTING
                   result        = result.
                   FLAG = 'X'.

             CATCH CX_BADI_NOT_IMPLEMENTED.
           ENDTRY.

           IF  RESULT <> ''AND FLAG = 'X'.
             APPEND TEMSEFIN_TAB.
             APPEND TEMSEFIN_TABRT.

             CLEAR  TEMSEFIN_TAB.
             CLEAR  TEMSEFIN_TABRT.
           ELSEIF FLAG NE 'X'.
*           endif.

           APPEND TEMSEFIN_TAB.
           APPEND TEMSEFIN_TABRT.

           CLEAR  TEMSEFIN_TAB.
           CLEAR  TEMSEFIN_TABRT.
           ENDIF.
           CLEAR FLAG.

         ENDIF.
        ENDIF.
       ENDLOOP.
       IF TERM_PREV_FY = 'X' AND FOUND = ''.                "RK1046194
         NEXT = 'X'.
         FOUND ='X'.
       ENDIF.
       IF FOUND = 'X'.
         RX-KEY-PERNR = PERNR-PERNR.
         RP-IMP-C2-IN.
       ENDIF.
     ENDIF.
*     IF FOUND IS INITIAL.
*       REJECT.
*     ENDIF.

ENDFORM.                    " IMPORT_RESULTS_TAN
