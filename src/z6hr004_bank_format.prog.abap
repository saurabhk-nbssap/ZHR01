
*&---------------------------------------------------------------------*
*& REPORT  Z6HR004_BANK_FORMAT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:
* OBJECT TYPE       :                 FUNC. CONSULTANT  :RAM MANOHAR
*          DEVELOPER:SUPRIYA
*      CREATION DATE:   29.09.2010
*        DEV REQUEST:
*  TCODE            :
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 30/12/2016
*   REASON FOR CHANGE: PERNR - full length not fetch properly in o/p
*   KEY FIELDS:     WA_SBI-REF_DOC,WA_HDFC-REF_DOC,WA_NEFT-REF_DOC
*   REQUEST #: IRDK926673
* --------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*   CHANGED BY: Saptarshi Chatterjee
*   CHANGE ON: 27/06/2017
*   REASON FOR CHANGE: Addition of DBS Bank Salary report
*   KEY FIELDS:
*   REQUEST #: IRDK927889
* --------------------------------------------------------------------*

REPORT  Z6HR004_BANK_FORMAT.

TYPE-POOLS : TRUXS,SLIS.

TABLES : HRPY_RGDIR,
         PA0009.

DATA : IT_TEXT_DATA TYPE TRUXS_T_TEXT_DATA,
       wa_text_data(4096) type c.

DATA : BEGIN OF WA_TAB,
           PERNR LIKE HRPY_RGDIR-PERNR,
           PAYDT LIKE HRPY_RGDIR-PAYDT,
           SEQNR LIKE HRPY_RGDIR-SEQNR,
       END OF WA_TAB.

DATA : BEGIN OF RT1 .
        INCLUDE STRUCTURE PC207 .
DATA : END OF RT1.

DATA : BEGIN OF WA_RT2,
       PERNR    LIKE P0009-PERNR,
       ABART    LIKE PC207-ABART,
       LGART    LIKE PC207-LGART,
       BETRG    LIKE PC207-BETRG,
       RTE_CURR LIKE PC207-RTE_CURR,
       AMT_CURR LIKE PC207-AMT_CURR,
       END OF WA_RT2.

DATA: BEGIN OF WA_NEFT,
      TRANTYPE(1)        TYPE C, " TRANSACTION TYPE
      BEN_CODE(13)       TYPE C,   "BENEFICIARY CODE
      BEN_ACCT(25)       TYPE C,  "BENEFICIARY ACCOUNT NO (SERVICE LEVEL)
      INST_AMT(20)       TYPE C, "INSTRUMENT AMOUNT
      BEN_NAME(200)      TYPE C,  "BENEFICIARY NAME
      DRA_LOCATION(20)   TYPE C, "DRAWEE LOCATION
      PRNT_LOCATION(20)  TYPE C, "PRINT LOCATION
      BEN_ADDR1(70)      TYPE C, "BENE ADDRESS1
      BEN_ADDR2(70)      TYPE C, "BENE ADDRESS2
      BEN_ADDR3(70)      TYPE C, "BENE ADDRESS3
      BEN_ADDR4(70)      TYPE C, "BENE ADDRESS4
      BEN_ADDR5(20)      TYPE C, "BENE ADDRESS5
      INST_REF(20)       TYPE C, "INST REFERENCE NUMBER(DATE IN NUMBER FROMAT)
      REF_DOC(20)        TYPE C, "CUSTOMER REFERENCE(NARRATION)
      PAY_DET1(30)       TYPE C, " PAYMENT DETAILS 1
      PAY_DET2(30)       TYPE C, " PAYMENT DETAILS 2
      PAY_DET3(30)       TYPE C, " PAYMENT DETAILS 3
      PAY_DET4(30)       TYPE C, " PAYMENT DETAILS 4
      PAY_DET5(30)       TYPE C, " PAYMENT DETAILS 5
      PAY_DET6(30)       TYPE C, " PAYMENT DETAILS 6
      PAY_DET7(30)       TYPE C, " PAYMENT DETAILS 7
      CHQ_NUM(12)        TYPE C, " CHEQUE NUMBER(EMPLOYEE NO)
      CHQ_DATE(10)       TYPE C, " CHEQUE DATE (PAY DATE)
      MICR_NUM(15)       TYPE C, " MICRO NUMBER
      IFC_CODE(15)       TYPE C, " IFC CODE
      BEN_BNAME(100)     TYPE C, " BEN BANK NAME
      BEN_BRNCH(40)      TYPE C, " BEN BRANCH NAME
      add(20)            TYPE C,
      END OF WA_NEFT.

*DATA : BEGIN OF WA_DBS_HEADER,
*        record_typ_h(6)      TYPE C, "Record type Header
*        file_date_h(8)       TYPE C, "File creation date
*        comp_id_h(12)        TYPE C, "Company ID
*        sender_name_h(35)    TYPE C, "Sender name
*       END OF WA_DBS_HEADER.

DATA :  record_typ_h(7)      TYPE C, "Record type Header
        file_date_h(8)       TYPE C, "File creation date
        comp_id_h(20)        TYPE C, "Company ID
        sender_name_h(50)    TYPE C, "Sender name   " IHDK901485: increased length for IR

        record_typ_t(7)      TYPE C, "Record type Trailer
        total_tran_t(8)      TYPE C, "Total number of transaction
        total_amount_t(20)   TYPE C. "Total transaction amount


DATA : BEGIN OF WA_DBS,
*        record_typ_h(7)      TYPE C, "Record type Header
*        file_date_h(8)       TYPE C, "File creation date
*        comp_id_h(12)        TYPE C, "Company ID
*        sender_name_h(35)    TYPE C, "Sender name

        record_typ(7)        TYPE C, "Record type
        payment_typ(8)       TYPE C, "Payment type
        sender_acc(20)       TYPE C, "Sender Account number
        from_acc_cur(50)      TYPE C, "From Account Currency  " IHDK901485: increased length for IR
        cust_ref1(16)        TYPE C, "Customer reference
        pay_curr(3)          TYPE C, "Payment Currency
        batch_id(5)          TYPE C, "Batch ID
        payment_date(10)     TYPE C, "Payment Date
        bank_charges(3)      TYPE C, "Bank Charges
        da_bank_charges(50)  TYPE C, "Debit Account for Bank Charges
        receive_party(35)    TYPE C, "Receiving Party Name
        payble_to(70)        TYPE C, "Payable To
        b_add1(35)           TYPE C, "Beneficiary Address line 1
        b_add2(35)           TYPE C, "Beneficiary Address line 2
        b_add3(35)           TYPE C, "Beneficiary Address line 3
        b_acc(35)            TYPE C, "Beneficiary Account number
        b_acc_type(2)        TYPE C, "Beneficiary Account type
        blank_field1(1)      TYPE C, "Blank field
        blank_field2(1)      TYPE C, "Blank field
        ifsc_code(11)        TYPE C, "IFSC Code
        b_swift(11)          TYPE C, "Beneficiary bank SWIFT BIC U
        b_bank_name(35)      TYPE C, "Beneficiary Bank Name
        b_bank_address(105)  TYPE C, "Beneficiary Bank Address W
        b_bank_country(5)    TYPE C, "Beneficiary Bank Country X
        b_bank_routing(35)   TYPE C, "Beneficiary Bank Routing Code Y
        i_swift(11)          TYPE C, "Intermediary Bank SWIFT BIC Z
        amount_curr(3)       TYPE C, "Amount Currency
        amount(15)           TYPE C, "Amount
        fx_conref1(50)       TYPE C, "FX Contract Reference 1
        amount_u1(15)        TYPE C, "Amount to be Utilized 1
        fx_conref2(50)       TYPE C, "FX Contract Reference 2
        amount_u2(50)        TYPE C, "Amount to be Utilized 2
        tcode(2)             TYPE C, "Transaction Code
        blank_field3(1)      TYPE C, "Blank field
        cust_ref2(16)        TYPE C, "Cust Reference
        payment_detail(90)   TYPE C, "Payment Details
        ins_ord_bank(128)    TYPE C, "Instruction to ordering bank
        blank_field4(1)      TYPE C, "Blank field
        blank_field5(1)      TYPE C, "Blank field
        blank_field6(1)      TYPE C, "Blank field
        blank_field7(1)      TYPE C, "Blank field
        blank_field8(1)      TYPE C, "Blank field
        purpose_pay(6)       TYPE C, "Purpose of payment
        purpose_pay_o(35)    TYPE C, "Purpose of payment Others
        delivery_mode(1)     TYPE C, "Delivery Mode
        print_at_loc(16)     TYPE C, "Print at location
        payable_at_loc(16)   TYPE C, "Payable at location
        advice_name(35)      TYPE C, "Advice Name
        address1(35)         TYPE C, "Address line 1
        address2(35)         TYPE C, "Address line 2
        address3(35)         TYPE C, "Address line 3
        country(35)          TYPE C, "Country
        post_code(8)         TYPE C, "Postal code BB
        email1(35)           TYPE C, "Email1
        email2(35)           TYPE C, "Email2
        email3(35)           TYPE C, "Email3
        email4(35)           TYPE C, "Email4
        email5(35)           TYPE C, "Email5
        phone1(23)           TYPE C, "Phone number 1
        phone2(23)           TYPE C, "Phone number 2
        phone3(23)           TYPE C, "Phone number 3
        phone4(23)           TYPE C, "Phone number 4
        phone5(23)           TYPE C, "Phone number 5
        invoice_detail(7000) TYPE C, "Invoice Details
        clint_ref1(40)       TYPE C, "Client reference 1
        clint_ref2(40)       TYPE C, "Client reference 2
        clint_ref3(40)       TYPE C, "Client reference 3
        clint_ref4(40)       TYPE C, "Client reference 4

*        record_typ_t(7)      TYPE C, "Record type Trailer
*        total_tran_t(8)      TYPE C, "Total number of transaction
*        total_amount_t(12)   TYPE C, "Total transaction amount

       END OF WA_DBS.

DATA : BEGIN OF WA_DBS_FOOTER,
        record_typ_t(7)      TYPE C, "Record type Trailer
        total_tran_t(8)      TYPE C, "Total number of transaction
        total_amount_t(20)   TYPE C, "Total transaction amount
       END OF WA_DBS_FOOTER.

data :BEGIN OF WA_HDFC,
      BEN_ACCT(25)       TYPE C,"ACCOUNT NO
      DRCR(4),                     "DR/CR INDICATOR
      INST_AMT(20)       TYPE C,"AMOUNT
      REF_DOC(20)        TYPE C,                            "NARRATI0N
      add(20)            TYPE C,
      END OF WA_HDFC.

data :BEGIN OF WA_SBI,
      BEN_ACCT(25)       TYPE C,
      INST_AMT(20)       TYPE C,"AMOUNT
      REF_DOC(20)        TYPE C,                            "NARRATI0N
      add(20)            TYPE C,
      END OF WA_SBI.

DATA : BEGIN OF WA_PA0009,
       PERNR            TYPE PA0009-PERNR,
       ENDDA            TYPE PA0009-ENDDA,
       BEGDA            TYPE PA0009-BEGDA,
       EMFTX            TYPE PA0009-EMFTX,
       BANKS            TYPE PA0009-BANKS,
       BANKL            TYPE PA0009-BANKL,
       BANKN            TYPE PA0009-BANKN,
       ZZPAYEE_NAME     TYPE PA0009-ZZPAYEE_NAME,
       ZZBANK_NAME      TYPE PA0009-ZZBANK_NAME,
       ZZBANK_BRANCH    TYPE PA0009-ZZBANK_BRANCH,
       ZZRTGS_NEFT_IFSC TYPE PA0009-ZZRTGS_NEFT_IFSC,
       bukrs            type pa0001-bukrs,
       END OF WA_PA0009.

DATA: ITAB      LIKE  STANDARD TABLE OF WA_TAB,
      RT2       LIKE  STANDARD TABLE OF WA_RT2,
      IT_NEFT   LIKE  STANDARD TABLE OF WA_NEFT,
      IT_HDFC   LIKE  STANDARD TABLE OF WA_HDFC,
      IT_SBI    LIKE  STANDARD TABLE OF WA_SBI,
      IT_PA0009 LIKE  STANDARD TABLE OF WA_PA0009.

DATA : IT_DBS   LIKE  STANDARD TABLE OF WA_DBS.
       "WA_DBS   TYPE LT_DBS.
*DATA : IT_DBS_HEADER LIKE STANDARD TABLE OF WA_DBS_HEADER,
*       IT_DBS_FOOTER LIKE STANDARD TABLE OF WA_DBS_FOOTER.

DATA : PAYRESULT     TYPE PAY99_RESULT.
DATA : P_FPPER       LIKE HRPY_RGDIR-FPPER,
       wa_pernr      like pa0009-pernr,
       WA_FIRST_DATE TYPE SY-DATUM,
       WA_LAST_DATE  TYPE SY-DATUM.

DATA: LD_FILENAME TYPE STRING,
      LD_PATH TYPE STRING,
      LD_FULLPATH TYPE STRING,
      LD_RESULT TYPE I.

DATA : F_PERIOD TYPE T009B-POPER,
       F_YEAR   TYPE T009B-BDATJ.

DATA : it_pa0009_lines TYPE i,
       it_rt2_lines    TYPE i,
       it_dbs_lines    type i.
DATA : wa_heading(500) TYPE c,
       wa_footer(500) TYPE c.

DATA : it_t012k type table of t012k,  " Added by Saptarshi Chatterjee, 20.07.2017
       wa_t012k type t012k.
DATA : gv_bankn type t012k-bankn.

*Declarations for ALV Grid.

DATA : WA_HEADER TYPE SLIS_LISTHEADER.
DATA : T_HEADER TYPE SLIS_T_LISTHEADER WITH HEADER LINE.

DATA:   it_st_list_top_of_page TYPE slis_t_listheader,
        it_st_fieldcat         TYPE slis_t_fieldcat_alv,
        i_events           TYPE slis_t_event,
        st_layout              TYPE slis_layout_alv,
        s_repid   LIKE sy-repid,
        s_variant LIKE disvariant,
        code LIKE disvariant-handle.
DATA : w_gt_sort TYPE slis_t_sortinfo_alv .

*&---------------------------------------------------------------------*
*&                      SELECTION SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK A01 WITH FRAME TITLE TEXT-001 .
SELECT-OPTIONS : S_PERNR FOR PA0009-PERNR
                   MATCHCODE OBJECT PREM.
PARAMETERS     : P_ABKRS LIKE HRPY_RGDIR-ABKRS
                   MATCHCODE OBJECT H_T549T OBLIGATORY,
                 p_bukrs type pa0001-bukrs OBLIGATORY.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(31) TEXT-002.
PARAMETERS :P_PABRP LIKE QPPNP-PABRP OBLIGATORY,
            P_PABRJ LIKE QPPNP-PABRJ OBLIGATORY.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END   OF BLOCK A01 .

SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-003 .
PARAMETERS:P_PAYDT LIKE SY-DATUM,
           N1(15)  TYPE C.
SELECTION-SCREEN END   OF BLOCK B01 .

SELECTION-SCREEN BEGIN OF BLOCK C01 WITH FRAME TITLE TEXT-004 .
PARAMETERS: P_SBI RADIOBUTTON  GROUP RAD,
            P_NEFT RADIOBUTTON  GROUP RAD,
            P_HDFC RADIOBUTTON  GROUP RAD,
            P_DBS RADIOBUTTON GROUP RAD.  "Added by Saptarshi Chatterjee,27.06.2017
SELECTION-SCREEN END   OF BLOCK C01 .

SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE TEXT-005 .
PARAMETERS: P_PATH TYPE STRING. .
SELECTION-SCREEN END OF BLOCK S2 .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE      = 'SAVE AS '
      DEFAULT_EXTENSION = ''"'CSV'
      DEFAULT_FILE_NAME = 'BANKFORMAT'
      "INITIAL_DIRECTORY = 'C:\TEMP\'
      INITIAL_DIRECTORY = 'C:\'
    CHANGING
      FILENAME          = LD_FILENAME
      PATH              = LD_PATH
      FULLPATH          = LD_FULLPATH
      USER_ACTION       = LD_RESULT.

  P_PATH = LD_FULLPATH.

*  **--------------------------------------------------------------------*
*       Initialisation
**---------------------------------------------------------------------*
INITIALIZATION.
*-------------*
  S_REPID = SY-REPID.
  PERFORM variant_init.


*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM GET_PAYROLL_DATA.
  IF P_SBI = 'X'.
    PERFORM GET_SBI_DATA.
  ELSEIF P_NEFT = 'X'.
    PERFORM GET_NEFT_DATA.
  ELSEIF P_HDFC = 'X'.
    PERFORM GET_HDFC_DATA.
  ELSEIF P_DBS = 'X'.
    PERFORM GET_DBS_DATA.
  ENDIF.
*&---------------------------------------------------------------------*
*&      FORM  GET_SBI_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_SBI_DATA .

  clear IT_PA0009.
  SELECT PERNR
         ENDDA
         BEGDA
         EMFTX
         BANKS
         BANKL
         BANKN
         ZZPAYEE_NAME
         ZZBANK_NAME
         ZZBANK_BRANCH
         ZZRTGS_NEFT_IFSC FROM PA0009 INTO TABLE IT_PA0009
         FOR ALL ENTRIES IN ITAB
         WHERE PERNR = ITAB-PERNR
         and SUBTY = '0'
         AND   BEGDA Le wa_last_date
         AND   ENDDA GE WA_LAST_DATE
         AND   ZZTRAN_TYPE = 'S'.
  IF SY-SUBRC = 0.
    SORT IT_PA0009 BY PERNR begda descending.
  ENDIF.

  delete adjacent duplicates from IT_PA0009 comparing pernr.
  sort IT_PA0009 by pernr.

  loop at it_pa0009 into wa_pa0009.
    select single bukrs from pa0001 into wa_pa0009-bukrs
                         where pernr eq wa_pa0009-pernr
                           and begda le sy-datum
                           and endda ge sy-datum.
    modify it_pa0009 from wa_pa0009 TRANSPORTING bukrs.
    clear wa_pa0009.
  endloop.
  if not p_bukrs is INITIAL.
  delete it_pa0009 where bukrs ne p_bukrs.
  endif.
  CLEAR WA_RT2.
  LOOP AT RT2 INTO WA_RT2.
    CLEAR WA_PERNR.
    wa_pernr = wa_rt2-pernr.
    shift wa_pernr left deleting leading '0'.

    READ TABLE IT_PA0009 INTO WA_PA0009 WITH KEY PERNR = WA_RT2-PERNR.
    IF SY-SUBRC = 0.
      WA_SBI-BEN_ACCT     = WA_PA0009-BANKN.
*    ENDIF.

*    WA_HDFC-DRCR = 'C'.
      WA_SBI-INST_AMT = WA_RT2-BETRG.
*      concatenate wa_pernr+0(5) N1 into WA_SBI-REF_DOC .
      concatenate wa_pernr+0(8) N1 into WA_SBI-REF_DOC .     " modified by NK on 30.12.2016
      WA_SBI-add = ''.
      APPEND WA_SBI TO IT_SBI.
      CLEAR WA_SBI.
    endif.
  ENDLOOP.

  CLEAR :IT_TEXT_DATA,IT_TEXT_DATA[].
  IF IT_SBI IS NOT INITIAL.
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
  EXPORTING
    I_FIELD_SEPERATOR          = ','
*     I_LINE_HEADER              =
*     I_FILENAME                 =
*     I_APPL_KEEP                = ' '
  TABLES
    I_TAB_SAP_DATA             = IT_SBI
  CHANGING
    I_TAB_CONVERTED_DATA       = IT_TEXT_DATA
 EXCEPTIONS
   CONVERSION_FAILED          = 1
   OTHERS                     = 2
          .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      perform display_data.
*      PERFORM DOWNLOAD.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_SBI_DATA
*&---------------------------------------------------------------------*
*&      FORM  GET_NEFT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_NEFT_DATA .

  clear IT_PA0009.
  SELECT PERNR
         ENDDA
         BEGDA
         EMFTX
         BANKS
         BANKL
         BANKN
         ZZPAYEE_NAME
         ZZBANK_NAME
         ZZBANK_BRANCH
         ZZRTGS_NEFT_IFSC FROM PA0009 INTO TABLE IT_PA0009
         FOR ALL ENTRIES IN ITAB
         WHERE PERNR = ITAB-PERNR
         and SUBTY = '0'
         AND   BEGDA le wa_last_date
          AND   ENDDA GE WA_LAST_DATE
         AND   ZZTRAN_TYPE = 'N'.
  IF SY-SUBRC = 0.
    SORT IT_PA0009 BY PERNR begda descending.
  ENDIF.

  delete adjacent duplicates from IT_PA0009 comparing pernr.
  sort IT_PA0009 by pernr.
  loop at it_pa0009 into wa_pa0009.
    select single bukrs from pa0001 into wa_pa0009-bukrs
                         where pernr eq wa_pa0009-pernr
                           and begda le sy-datum
                           and endda ge sy-datum.
    modify it_pa0009 from wa_pa0009 TRANSPORTING bukrs.
    clear wa_pa0009.
  endloop.
  if not p_bukrs is INITIAL.
  delete it_pa0009 where bukrs ne p_bukrs.
  endif.
  CLEAR WA_RT2.
  LOOP AT RT2 INTO WA_RT2.
    CLEAR WA_PERNR.
    wa_pernr = wa_rt2-pernr.
    shift wa_pernr left deleting leading '0'.

    WA_NEFT-TRANTYPE = 'N'.
    WA_NEFT-BEN_CODE = ''.
    WA_NEFT-INST_AMT = WA_RT2-BETRG.

    READ TABLE IT_PA0009 INTO WA_PA0009 WITH KEY PERNR = WA_RT2-PERNR.
    IF SY-SUBRC = 0.
      WA_NEFT-BEN_ACCT     = WA_PA0009-BANKN.
      WA_NEFT-BEN_NAME     = WA_PA0009-ZZPAYEE_NAME.
      WA_NEFT-IFC_CODE     = WA_PA0009-ZZRTGS_NEFT_IFSC.
      WA_NEFT-BEN_BNAME    = WA_PA0009-ZZBANK_NAME.
      WA_NEFT-BEN_BRNCH    = WA_PA0009-ZZBANK_BRANCH.
*    ENDIF.

      WA_NEFT-DRA_LOCATION   = ''.
      WA_NEFT-PRNT_LOCATION  = ''.
      WA_NEFT-BEN_ADDR1      = ''.
      WA_NEFT-BEN_ADDR2      = ''.
      WA_NEFT-BEN_ADDR3      = ''.
      WA_NEFT-BEN_ADDR4      = ''.
      WA_NEFT-BEN_ADDR5      = ''.

*      WA_NEFT-INST_REF       = .
      CONCATENATE P_PAYDT+6(2)  P_PAYDT+4(2)  P_PAYDT+0(4) INTO WA_NEFT-INST_REF .
*    WA_NEFT-REF_DOC        = N1.
*      concatenate wa_pernr+0(5) N1 into WA_NEFT-REF_DOC .
      concatenate wa_pernr+0(8) N1 into WA_NEFT-REF_DOC .    " modified by NK on 30.12.2106
      WA_NEFT-PAY_DET1       = ''.
      WA_NEFT-PAY_DET2       = ''.
      WA_NEFT-PAY_DET3       = ''.
      WA_NEFT-PAY_DET4       = ''.
      WA_NEFT-PAY_DET5       = ''.
      WA_NEFT-PAY_DET6       = ''.
      WA_NEFT-PAY_DET7       = ''.

      shift WA_RT2-PERNR left deleting leading '0'.
      WA_NEFT-CHQ_NUM        = WA_RT2-PERNR.
      CONCATENATE P_PAYDT+6(2) '/' P_PAYDT+4(2) '/' P_PAYDT+0(4) INTO WA_NEFT-CHQ_DATE .
*      WA_NEFT-CHQ_DATE       =.
      WA_NEFT-MICR_NUM       = ''.

      WA_NEFT-add = ''.

      APPEND WA_NEFT TO IT_NEFT.
      CLEAR WA_NEFT.
    endif.
  ENDLOOP.

  CLEAR :IT_TEXT_DATA,IT_TEXT_DATA[].
  IF IT_NEFT IS NOT INITIAL.
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
  EXPORTING
    I_FIELD_SEPERATOR          = ','
*     I_LINE_HEADER              =
*     I_FILENAME                 =
*     I_APPL_KEEP                = ' '
  TABLES
    I_TAB_SAP_DATA             = IT_NEFT
  CHANGING
    I_TAB_CONVERTED_DATA       = IT_TEXT_DATA
 EXCEPTIONS
   CONVERSION_FAILED          = 1
   OTHERS                     = 2
          .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      perform display_data.
*      PERFORM DOWNLOAD.
*      CHECK LD_RESULT EQ '0'.

    ENDIF.

  ENDIF.
ENDFORM.                    " GET_NEFT_DATA
*&---------------------------------------------------------------------*
*&      FORM  GET_HDFC_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_HDFC_DATA .

*  --header
  WA_HDFC-BEN_ACCT = 'ACCOUNT'.
  WA_HDFC-DRCR     = 'DRCR'.
  WA_HDFC-INST_AMT = 'AMOUNT'.
  WA_HDFC-REF_DOC  = 'NARRATION'.
  WA_HDFC-add       = ''.
  append WA_HDFC to it_HDFC.
  clear WA_HDFC.
*  ---

  clear IT_PA0009.
  SELECT PERNR
         ENDDA
         BEGDA
         EMFTX
         BANKS
         BANKL
         BANKN
         ZZPAYEE_NAME
         ZZBANK_NAME
         ZZBANK_BRANCH
         ZZRTGS_NEFT_IFSC FROM PA0009 INTO TABLE IT_PA0009
         FOR ALL ENTRIES IN ITAB
         WHERE PERNR = ITAB-PERNR
         and SUBTY = '0'
         AND   BEGDA le wa_last_date
          AND   ENDDA GE WA_LAST_DATE
         AND   ZZTRAN_TYPE = 'I'.
  IF SY-SUBRC = 0.
    SORT IT_PA0009 BY PERNR begda descending.
  ENDIF.

  delete adjacent duplicates from IT_PA0009 comparing pernr.

  sort IT_PA0009 by pernr.
  loop at it_pa0009 into wa_pa0009.
    select single bukrs from pa0001 into wa_pa0009-bukrs
                         where pernr eq wa_pa0009-pernr
                           and begda le sy-datum
                           and endda ge sy-datum.
    modify it_pa0009 from wa_pa0009 TRANSPORTING bukrs.
    clear wa_pa0009.
  endloop.
  if not p_bukrs is INITIAL.
  delete it_pa0009 where bukrs ne p_bukrs.
  endif.
  CLEAR WA_RT2.
  LOOP AT RT2 INTO WA_RT2.
    CLEAR WA_PERNR.
    wa_pernr = wa_rt2-pernr.
    shift wa_pernr left deleting leading '0'.

    READ TABLE IT_PA0009 INTO WA_PA0009 WITH KEY PERNR = WA_RT2-PERNR.
    IF SY-SUBRC = 0.
      WA_HDFC-BEN_ACCT     = WA_PA0009-BANKN.
*    ENDIF.

      WA_HDFC-DRCR = 'C'.
      WA_HDFC-INST_AMT = WA_RT2-BETRG.
*      concatenate wa_pernr+0(5) N1 into WA_HDFC-REF_DOC .
      concatenate wa_pernr+0(8) N1 into WA_HDFC-REF_DOC .            " modified by NK on 30.12.2016
      WA_HDFC-add       = ''.
      APPEND WA_HDFC TO IT_HDFC.
      CLEAR WA_HDFC.
    endif.
  ENDLOOP.

  CLEAR :IT_TEXT_DATA,IT_TEXT_DATA[].
  IF IT_HDFC IS NOT INITIAL.
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
  EXPORTING
    I_FIELD_SEPERATOR          = ','
*     I_LINE_HEADER              =
*     I_FILENAME                 =
*     I_APPL_KEEP                = ' '
  TABLES
    I_TAB_SAP_DATA             = IT_HDFC
  CHANGING
    I_TAB_CONVERTED_DATA       = IT_TEXT_DATA
 EXCEPTIONS
   CONVERSION_FAILED          = 1
   OTHERS                     = 2
          .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      perform display_data.
*      PERFORM DOWNLOAD.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_HDFC_DATA

*&---------------------------------------------------------------------*
*&      FORM  GET_DBS_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM GET_DBS_DATA.



   clear IT_PA0009.
     SELECT PERNR
         ENDDA
         BEGDA
         EMFTX
         BANKS
         BANKL
         BANKN
         ZZPAYEE_NAME
         ZZBANK_NAME
         ZZBANK_BRANCH
         ZZRTGS_NEFT_IFSC FROM PA0009 INTO TABLE IT_PA0009
         FOR ALL ENTRIES IN ITAB
         WHERE PERNR = ITAB-PERNR
         and SUBTY = '0'
         AND   BEGDA le wa_last_date
          AND   ENDDA GE WA_LAST_DATE
         AND   ZZTRAN_TYPE = 'D'.

       SELECT *
         from t012k
         into table it_t012k
         where bukrs = p_bukrs  "'1000'; IHDK901479: HR: S_K: ZPY_BANKTRF: DBS sel as per p_bukrs(IR): 26.4.19
          and  hbkid = 'DBS01'
          and  hktid = 'CA001'.

         IF SY-SUBRC = 0.
             SORT IT_PA0009 BY PERNR begda descending.
         ENDIF.

          delete adjacent duplicates from IT_PA0009 comparing pernr.

          sort IT_PA0009 by pernr.

            loop at it_pa0009 into wa_pa0009.
              select single bukrs from pa0001 into wa_pa0009-bukrs
                                    where pernr eq wa_pa0009-pernr
                                      and begda le sy-datum
                                      and endda ge sy-datum.
              modify it_pa0009 from wa_pa0009 TRANSPORTING bukrs.
              clear wa_pa0009.
            endloop.
            LOOP AT it_t012k into wa_t012k.
               gv_bankn = wa_t012k-bankn.
            ENDLOOP.


            if not p_bukrs is INITIAL.
            delete it_pa0009 where bukrs ne p_bukrs.
            endif.

            DESCRIBE TABLE IT_PA0009 LINES IT_PA0009_LINES.
            DESCRIBE TABLE rt2 LINES it_rt2_lines.

            CLEAR WA_RT2.
            LOOP AT RT2 INTO WA_RT2.
              CLEAR WA_PERNR.
              wa_pernr = wa_rt2-pernr.
              shift wa_pernr left deleting leading '0'.

              READ TABLE IT_PA0009 INTO WA_PA0009 WITH KEY PERNR = WA_RT2-PERNR.
              IF SY-SUBRC = 0.
                wa_dbs-receive_party  = wa_pa0009-ZZPAYEE_NAME."emftx.
                wa_dbs-b_acc          = wa_pa0009-bankn.
                wa_dbs-ifsc_code      = wa_pa0009-zzrtgs_neft_ifsc.
                wa_dbs-cust_ref2      = wa_pa0009-pernr.
                wa_dbs-payment_detail = wa_pa0009-pernr.

*                CONCATENATE p_paydt+6(2) p_paydt+4(2) p_paydt+0(4) into wa_dbs-file_date_h.
*                 wa_dbs-record_typ_h = ' HEADER'.
*                 wa_dbs-comp_id_h    = '    ININDO01'.
*                 wa_dbs-sender_name_h = 'INDOFIL INDUSTRIES LTD'.
*               CONCATENATE wa_dbs-record_typ_h  wa_dbs-file_date_h  wa_dbs-comp_id_h  wa_dbs-sender_name_h into wa_heading.

                CONCATENATE p_paydt+6(2) p_paydt+4(2) p_paydt+0(4) into file_date_h.
                 record_typ_h = ' HEADER'.
                 " IHDK901481: HR: S_K: ZPY_BANKTRF: DBS sel as per p_bukrs(IR): 26.4.19
                 comp_id_h    = cond #( when p_bukrs = '1000' then '            ININDO01'
                                        when p_bukrs = '2800' then '             ININDR1' ).
                 " IHDK901483: HR: S_K: ZPY_BANKTRF: DBS sel as per p_bukrs(IR): 26.4.19
                 sender_name_h = cond #( when p_bukrs = '1000' then 'INDOFIL INDUSTRIES LTD'
                                         when p_bukrs = '2800' then 'INDOREAGENS POLYMER ADDITIVES' ). " mail from K patil on 29.05.2019 to reduce length
*                                                                    INDOREAGENS POLYMER ADDITIVES PRIVATE LIMITED
               CONCATENATE record_typ_h file_date_h comp_id_h sender_name_h into wa_heading.

*                CONCATENATE p_paydt+6(2) p_paydt+4(2) p_paydt+0(4) into wa_dbs_header-file_date_h.
*                wa_dbs_header-record_typ_h = 'HEADER'.
*                wa_dbs_header-comp_id_h    = 'ININDO01'.
*                wa_dbs_header-sender_name_h = 'INDOFIL INDUSTRIES LTD'.



                 wa_dbs-record_typ = 'PAYMENT'.
                 wa_dbs-payment_typ = 'SAL'.
                 "wa_dbs-sender_acc = '811210011111'.
                 wa_dbs-sender_acc = gv_bankn.
                 wa_dbs-from_acc_cur = 'INR'.
                 wa_dbs-cust_ref1 = ' '.
                 wa_dbs-pay_curr = 'INR'.
                 wa_dbs-batch_id = ' '.
                 wa_dbs-payment_date = file_date_h.
                 wa_dbs-bank_charges = ' '.
                 wa_dbs-da_bank_charges = ' '.
                 wa_dbs-payble_to = ' '.
                 wa_dbs-b_add1 = ' '.
                 wa_dbs-b_add2 = ' '.
                 wa_dbs-b_add3 = ' '.
                 wa_dbs-b_acc_type = ' '.
                 wa_dbs-b_swift = ' '.
                 wa_dbs-b_bank_name = ' '.
                 wa_dbs-b_bank_address = ' '.
                 wa_dbs-b_bank_country = ' '.
                 wa_dbs-b_bank_routing = ' '.
                 wa_dbs-i_swift = ' '.
                 wa_dbs-amount_curr = ' '.
                 wa_dbs-amount = wa_rt2-betrg.
                 wa_dbs-fx_conref1 = ' '.
                 wa_dbs-amount_u1 = ' '.
                 wa_dbs-fx_conref2 = ' '.
                 wa_dbs-amount_u2 = ' '.
                 wa_dbs-tcode = '22 - Salary Payment'.
                 "wa_dbs-cust_ref2 = ' '.
                 "wa_dbs-payment_detail = ' '.
                 wa_dbs-ins_ord_bank = ' '.
                 wa_dbs-purpose_pay = ' '.
                 wa_dbs-purpose_pay_o = ' '.
                 wa_dbs-delivery_mode = ' '.
                 wa_dbs-print_at_loc = ' '.
                 wa_dbs-payable_at_loc = ' '.
                 wa_dbs-advice_name = ' '.
                 wa_dbs-address1 = ' '.
                 wa_dbs-address2 = ' '.
                 wa_dbs-address3 = ' '.
                 wa_dbs-country = ' '.
                 wa_dbs-post_code = ' '.
                 wa_dbs-phone1 = ' '.
                 wa_dbs-phone2 = ' '.
                 wa_dbs-phone3 = ' '.
                 wa_dbs-phone4 = ' '.
                 wa_dbs-phone5 = ' '.
                 wa_dbs-invoice_detail = ' '.
                 wa_dbs-clint_ref1 = ' '.
                 wa_dbs-clint_ref2 = ' '.
                 wa_dbs-clint_ref3 = ' '.
                 wa_dbs-clint_ref4 = ' '.
                 wa_dbs-blank_field1 = ' '. "Blank field
                 wa_dbs-blank_field2 = ' '. "Blank field
                 wa_dbs-blank_field3 = ' '. "Blank field
                 wa_dbs-blank_field4 = ' '. "Blank field
                 wa_dbs-blank_field5 = ' '. "Blank field
                 wa_dbs-blank_field6 = ' '. "Blank field
                 wa_dbs-blank_field7 = ' '. "Blank field
                 wa_dbs-blank_field8 = ' '. "Blank field

                "CONCATENATE p_paydt+6(2) p_paydt+4(2) p_paydt+0(4) into wa_dbs-payment_date.

                "wa_dbs-payment_date = p_paydt.
*                CONCATENATE P_PAYDT+6(2)  P_PAYDT+4(2)  P_PAYDT+0(4) INTO WA_NEFT-INST_REF .
**              WA_NEFT-REF_DOC        = N1.
**                concatenate wa_pernr+0(5) N1 into WA_NEFT-REF_DOC .
*                concatenate wa_pernr+0(8) N1 into WA_NEFT-REF_DOC .    " modified by NK on 30.12.2106
*                shift WA_RT2-PERNR left deleting leading '0'.
*                WA_NEFT-CHQ_NUM        = WA_RT2-PERNR.
*                CONCATENATE P_PAYDT+6(2) '/' P_PAYDT+4(2) '/' P_PAYDT+0(4) INTO WA_NEFT-CHQ_DATE .
**                WA_NEFT-CHQ_DATE       =.
*                WA_NEFT-MICR_NUM       = ''.
*                WA_NEFT-add = ''.

*                wa_dbs-record_typ_t = 'TRAILER'.
*                wa_dbs-total_tran_t = IT_PA0009_LINES.
*                wa_dbs-total_amount_t = wa_dbs-total_amount_t + wa_rt2-betrg.
*                 CONCATENATE wa_dbs-record_typ_t wa_dbs-total_tran_t wa_dbs-total_amount_t into wa_footer.

                "record_typ_t   = 'TRAILER'.
                "total_tran_t   = it_pa0009_lines.
                "total_tran_t   = it_rt2_lines.
                total_amount_t = total_amount_t + wa_rt2-betrg.
                 "CONCATENATE record_typ_t total_tran_t total_amount_t into wa_footer.

*                wa_dbs_footer-record_typ_t = 'TRAILER'.
*                wa_dbs_footer-total_tran_t = IT_PA0009_LINES.
*                wa_dbs_footer-total_amount_t = wa_dbs_footer-total_amount_t + wa_rt2-betrg.

*                if sy-tabix eq 1.
*                  APPEND wa_dbs_header to it_dbs .
*                endif.
*                if sy-tabix eq it_pa0009_lines + 2.
*                  APPEND wa_dbs_footer to it_dbs.
*
*                endif.

                APPEND WA_DBS TO IT_DBS.
                CLEAR WA_DBS.

                "APPEND wa_dbs_header to it_dbs ."index 1.
              endif.
            ENDLOOP.

              DESCRIBE TABLE it_dbs LINES it_dbs_lines.
              INSERT  wa_heading into it_dbs index 1.

                record_typ_t   = 'TRAILER'.
                "total_tran_t   = it_pa0009_lines.
                total_tran_t   = it_dbs_lines.
                "total_amount_t = total_amount_t + wa_rt2-betrg.
                 CONCATENATE record_typ_t total_tran_t total_amount_t into wa_footer.


             " INSERT  wa_footer into it_dbs index it_pa0009_lines + 2 .
              ""wa_footer-total_tran_t = it_dbs_lines.
              INSERT  wa_footer into it_dbs index it_dbs_lines + 2.

              CLEAR :IT_TEXT_DATA,IT_TEXT_DATA[].
                  IF it_dbs IS NOT INITIAL.
                    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
                  EXPORTING
                    I_FIELD_SEPERATOR          = ','
*                     I_LINE_HEADER              =
*                     I_FILENAME                 =
*                     I_APPL_KEEP                = ' '
                  TABLES
                    I_TAB_SAP_DATA             = it_dbs
                  CHANGING
                    I_TAB_CONVERTED_DATA       = IT_TEXT_DATA
                 EXCEPTIONS
                   CONVERSION_FAILED          = 1
                   OTHERS                     = 2
                          .
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ELSE.
                      perform display_data.
*                      PERFORM DOWNLOAD.
*                      CHECK LD_RESULT EQ '0'.

                    ENDIF.

                  ENDIF.



ENDFORM.



*&---------------------------------------------------------------------*
*&      FORM  GET_PAYROLL_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_PAYROLL_DATA .

  CONCATENATE P_PABRJ P_PABRP  INTO P_FPPER.

  SELECT * FROM HRPY_RGDIR INTO CORRESPONDING FIELDS OF TABLE ITAB
  WHERE PERNR IN S_PERNR
  AND   ABKRS EQ P_ABKRS
  AND   FPPER EQ P_FPPER
  AND   INPER EQ P_FPPER
  AND   OCCAT EQ SPACE.

  SORT ITAB BY PERNR PAYDT SEQNR.
  LOOP AT ITAB INTO WA_TAB.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
*                CLUSTERID               = T500L_HEADER-RELID
                 EMPLOYEENUMBER          =  WA_TAB-PERNR
                 SEQUENCENUMBER          =  WA_TAB-SEQNR
                 READ_ONLY_INTERNATIONAL =  'X'
            CHANGING
                 PAYROLL_RESULT          = PAYRESULT
            EXCEPTIONS
                 ERROR_GENERATING_IMPORT = 2
                 IMPORT_MISMATCH_ERROR   = 3
                 SUBPOOL_DIR_FULL        = 4
                 NO_READ_AUTHORITY       = 5
                 NO_RECORD_FOUND         = 6
                 VERSIONS_DO_NOT_MATCH   = 7
                 OTHERS                  = 8.
    IF SY-SUBRC <> 0.
      DELETE ITAB FROM WA_TAB .
      CONTINUE.
    ELSE.


      LOOP AT PAYRESULT-INTER-RT INTO RT1 WHERE LGART = '/559'.
        WA_RT2-PERNR     = WA_TAB-PERNR.
        WA_RT2-ABART     = RT1-ABART.
        WA_RT2-LGART     = RT1-LGART.
        WA_RT2-BETRG     = RT1-BETRG .
        WA_RT2-RTE_CURR  = RT1-RTE_CURR.
        WA_RT2-AMT_CURR  = RT1-AMT_CURR.

        COLLECT WA_RT2 INTO RT2.
        CLEAR : WA_RT2.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  f_year   = P_PABRJ .
  f_period = P_PABRP .

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR              = f_year
*     I_MONMIT             = 00
      I_PERIV              = 'V3'
      I_POPER              = F_PERIOD
   IMPORTING
     E_DATE               = WA_FIRST_DATE
   EXCEPTIONS
     INPUT_FALSE          = 1
     T009_NOTFOUND        = 2
     T009B_NOTFOUND       = 3
     OTHERS               = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR              = f_year
*     I_MONMIT             = 00
      I_PERIV              = 'V3'
      I_POPER              = F_PERIOD
   IMPORTING
     E_DATE               = WA_last_DATE
   EXCEPTIONS
     INPUT_FALSE          = 1
     T009_NOTFOUND        = 2
     T009B_NOTFOUND       = 3
     OTHERS               = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GET_PAYROLL_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD .


  IF  NOT IT_TEXT_DATA IS INITIAL.
*    read table it_text_data into wa_text_data index 1.
*    if sy-subrc eq 0.
*      TRANSLATE wa_text_data USING ';,'.
*    endif.

    CLEAR wa_text_data .
    LOOP AT IT_TEXT_DATA INTO wa_text_data  .
      TRANSLATE wa_text_data USING ';,'.
      MODIFY IT_TEXT_DATA FROM wa_text_data.
    ENDLOOP.
  ENDIF.


  CHECK LD_RESULT EQ '0'.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*   BIN_FILESIZE                    =
      FILENAME                        = P_PATH
      FILETYPE                        = 'ASC'

    TABLES
      DATA_TAB                        = IT_TEXT_DATA
*   FIELDNAMES                      =
   EXCEPTIONS
     FILE_WRITE_ERROR                = 1
     NO_BATCH                        = 2
     GUI_REFUSE_FILETRANSFER         = 3
     INVALID_TYPE                    = 4
     NO_AUTHORITY                    = 5
     UNKNOWN_ERROR                   = 6
     HEADER_NOT_ALLOWED              = 7
     SEPARATOR_NOT_ALLOWED           = 8
     FILESIZE_NOT_ALLOWED            = 9
     HEADER_TOO_LONG                 = 10
     DP_ERROR_CREATE                 = 11
     DP_ERROR_SEND                   = 12
     DP_ERROR_WRITE                  = 13
     UNKNOWN_DP_ERROR                = 14
     ACCESS_DENIED                   = 15
     DP_OUT_OF_MEMORY                = 16
     DISK_FULL                       = 17
     DP_TIMEOUT                      = 18
     FILE_NOT_FOUND                  = 19
     DATAPROVIDER_EXCEPTION          = 20
     CONTROL_FLUSH_ERROR             = 21
     OTHERS                          = 22
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .

  PERFORM FIELDCAT1 USING IT_ST_FIELDCAT[]. " PUT DATA IN ALV
  PERFORM LAYOUT_BUILD USING ST_LAYOUT.     " BUILD LAYOUT
  PERFORM f4000_events_init CHANGING i_events.
  PERFORM REUSE_ALV_LIST_DISPLAY.


  REFRESH it_st_list_top_of_page.
  CLEAR   it_st_list_top_of_page.
  REFRESH it_st_fieldcat.
  CLEAR   it_st_fieldcat.

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ST_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT1 USING P_IT_ST_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  if p_sbi = 'X'.

    LS_FIELDCAT-FIELDNAME = 'BEN_ACCT'.
    LS_FIELDCAT-tabname = 'WA_SBI'.
    LS_FIELDCAT-SELTEXT_M = 'Bank Account No'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'INST_AMT'.
    LS_FIELDCAT-tabname = 'WA_SBI'.
    LS_FIELDCAT-SELTEXT_M = 'Insrument No'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'REF_DOC'.
    LS_FIELDCAT-tabname = 'WA_SBI'.
    LS_FIELDCAT-SELTEXT_M = 'Ref Docu No'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

  ELSEIF P_NEFT = 'X'.

    LS_FIELDCAT-FIELDNAME = 'TRANTYPE'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'TRAN TYP'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_CODE'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Blank'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ACCT'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Account No.'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'INST_AMT'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Amount'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_NAME'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Payee Name'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'DRA_LOCATION'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'DRA LOCATION'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PRNT_LOCATION'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PRNT LOCATION'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'PRNT_LOCATION'.
*    LS_FIELDCAT-tabname = 'WA_NEFT'.
*    LS_FIELDCAT-SELTEXT_M = 'PRNT LOCATION'.
*    LS_FIELDCAT-OUTPUTLEN = 10.
*    LS_FIELDCAT-KEY       = 'X'.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ADDR1'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'BEN ADDR1'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ADDR2'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'BEN ADDR2'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ADDR3'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'BEN ADDR3'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ADDR4'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'BEN ADDR4'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_ADDR5'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'BEN ADDR5'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'INST_REF'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Date'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'REF_DOC'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Narration'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET1'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET1'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET2'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET2'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET3'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET3'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET4'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET4'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET5'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET5'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET6'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET6'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_DET7'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'PAY DET7'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'PAY_DET7'.
*    LS_FIELDCAT-tabname = 'WA_NEFT'.
*    LS_FIELDCAT-SELTEXT_M = 'PAY DET7'.
*    LS_FIELDCAT-OUTPUTLEN = 10.
**  LS_FIELDCAT-KEY       = 'X'.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CHQ_NUM'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Employee'.
    LS_FIELDCAT-OUTPUTLEN = 10.
*  LS_FIELDCAT-KEY       = 'X'.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CHQ_DATE'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'DATE'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'MICR_NUM'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'MICR NUM'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'IFC_CODE'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'RTGS No.'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_BNAME'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Bank Name'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BEN_BRNCH'.
    LS_FIELDCAT-tabname = 'WA_NEFT'.
    LS_FIELDCAT-SELTEXT_M = 'Bank Address'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

  elseif p_hdfc = 'X'.

    LS_FIELDCAT-FIELDNAME = 'BEN_ACCT'.
    LS_FIELDCAT-tabname = 'WA_HDFC'.
    LS_FIELDCAT-SELTEXT_M = 'ACCOUNT'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'DRCR'.
    LS_FIELDCAT-tabname = 'WA_HDFC'.
    LS_FIELDCAT-SELTEXT_M = 'DR/CR'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'INST_AMT'.
    LS_FIELDCAT-tabname = 'WA_HDFC'.
    LS_FIELDCAT-SELTEXT_M = 'AMOUNT'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'REF_DOC'.
    LS_FIELDCAT-tabname = 'WA_HDFC'.
    LS_FIELDCAT-SELTEXT_M = 'NARRATION'.
    LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

   elseif p_dbs = 'X'.

*    LS_FIELDCAT-FIELDNAME = 'RECORD_TYP_H'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'RECORD TYPE'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'FILE_DATE_H'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'FILE CREATION DATE'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'COMP_ID_H'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'COMPANY ID'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'SENDER_NAME_H'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'SENDER NAME'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.



    LS_FIELDCAT-FIELDNAME = 'RECORD_TYP'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'RECORD TYPE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAYMENT_TYP'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYMENT TYPE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'SENDER_ACC'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'SENDER ACC NUMBER'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'FROM_ACC_CUR'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'FROM ACCOUNT CURRENCY'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CUST_REF1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CUSTOMER REFERENCE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAY_CURR'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYMENT CURRENCY'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BATCH_ID'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BATCH ID'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAYMENT_DATE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYMENT DATE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BANK_CHARGES'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BANK CHARGES'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'DA_BANK_CHARGES'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'DEBIT ACCOUNT FOR BANK CHARGES'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'RECEIVE_PARTY'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'RECEIVING PARTY NAME'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAYBLE_TO'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYBLE TO'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_ADD1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY ADDRESS LINE1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_ADD2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY ADDRESS LINE2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_ADD3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY ADDRESS LINE3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_ACC'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY ACCOUNT NUMBER'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_ACC_TYPE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY ACCOUNT TYPE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.


    LS_FIELDCAT-FIELDNAME = 'IFSC_CODE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'IFSC CODE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_SWIFT'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY BANK SWIFT BIC U'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_BANK_NAME'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY BANK NAME'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_BANK_ADDRESS'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY BANK ADDRESS W'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_BANK_COUNTRY'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY BANK COUNTRY X'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'B_BANK_ROUTING'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BENEFICIERY BANK ROUTING CODE Y'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'I_SWIFT'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'INTERMEDIARY BANK SWIFT BIC Z'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'AMOUNT_CURR'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'AMOUNT CURRENCY'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'AMOUNT'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'AMOUNT'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'FX_CONREF1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'FX CONTRACT REFERENCE 1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'AMOUNT_U1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'AMOUNT TO BE UTILIZED1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'FX_CONREF2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'FX CONTRACT REFERENCE 2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'AMOUNT_U2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'AMOUNT TO BE UTILIZED2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'TCODE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'TRANSACTION CODE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.


    LS_FIELDCAT-FIELDNAME = 'CUST_REF2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CUSTOMER REFERENCE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAYMENT_DETAIL'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYMENT DETAILS'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.


    LS_FIELDCAT-FIELDNAME = 'INS_ORD_BANK'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'INSTRUCTION TO ORDERING BANK'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD4'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD4'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD5'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD5'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD6'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD6'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD7'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD7'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'BLANK_FIELD8'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'BLANK FIELD8'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PURPOSE_PAY'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PURPOSE OF PAYMENT'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PURPOSE_PAY_O'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PURPOSE OF PAYMENT OTHERS'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'DELIVERY_MODE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'DELIVERY MODE'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PRINT_AT_LOC'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PRINT AT LOCATION'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PAYABLE_AT_LOC'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PAYABLE AT LOCATION'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'ADVICE_NAME'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'ADVICE NAME'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'ADDRESS1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'ADDRESS LINE 1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'ADDRESS2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'ADDRESS LINE 2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'ADDRESS3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'ADDRESS LINE 3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'COUNTRY'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'COUNTRY'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'POST_CODE'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'POSTAL CODE BB'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'EMAIL1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'EMAIL1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'EMAIL2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'EMAIL2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'EMAIL3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'EMAIL3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'EMAIL4'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'EMAIL4'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'EMAIL5'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'EMAIL5'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PHONE1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PHONE1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PHONE2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PHONE2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PHONE3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PHONE3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PHONE4'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PHONE4'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'PHONE5'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'PHONE5'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'INVOICE_DETAIL'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'INVOICE_DETAILS'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CLIENT_REF1'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CLIENT REFERENCE1'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CLIENT_REF2'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CLIENT REFERENCE2'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CLIENT_REF3'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CLIENT REFERENCE3'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

    LS_FIELDCAT-FIELDNAME = 'CLIENT_REF4'.
    LS_FIELDCAT-tabname = 'WA_DBS'.
    LS_FIELDCAT-SELTEXT_M = 'CLIENT REFERENCE4'.
    "LS_FIELDCAT-OUTPUTLEN = 10.
    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
    CLEAR LS_FIELDCAT.

****************     TRAILER     **************************
*    LS_FIELDCAT-FIELDNAME = 'RECORD_TYP_T'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'RECORD TYPE TRAILER'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'TOTAL_TRAN_T'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'TOTAL TRANSACTION'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.
*
*    LS_FIELDCAT-FIELDNAME = 'TOTAL_AMOUNT_T'.
*    LS_FIELDCAT-tabname = 'WA_DBS'.
*    LS_FIELDCAT-SELTEXT_M = 'TOTAL TRANSCTION AMOUNT'.
*    "LS_FIELDCAT-OUTPUTLEN = 10.
*    APPEND LS_FIELDCAT TO P_IT_ST_FIELDCAT.
*    CLEAR LS_FIELDCAT.

  endif.
ENDFORM.                                                    " FIELDCAT1
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_LAYOUT  text
*----------------------------------------------------------------------*
FORM LAYOUT_BUILD USING WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
  WA_LAYOUT-ZEBRA             = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WA_LAYOUT-NO_TOTALLINE      = ' '.
  WA_LAYOUT-info_fieldname      = 'COLOR'.
  wa_LAYOUT-confirmation_prompt = 'X'.
*  wa_LAYOUT-edit = 'X'.
ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  F4000_EVENTS_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_EVENTS  text
*----------------------------------------------------------------------*
FORM F4000_EVENTS_INIT  CHANGING P_I_EVENTS.

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
FORM REUSE_ALV_LIST_DISPLAY .

  if p_sbi =  'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING

     I_CALLBACK_PROGRAM                = S_REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'TOP-OF-PAGE'

     I_BACKGROUND_ID                   = 'ALV_BACKGROUND'
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = ST_LAYOUT
     IT_FIELDCAT                       = it_st_fieldcat[]

     I_DEFAULT                         = 'X'
     I_SAVE                            = 'A'
*   IS_VARIANT                        =
     IT_EVENTS                         = i_events

*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_SBI
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEif p_HDFC =  'X'.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING

     I_CALLBACK_PROGRAM                = S_REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'TOP-OF-PAGE'

     I_BACKGROUND_ID                   = 'ALV_BACKGROUND'
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = ST_LAYOUT
     IT_FIELDCAT                       = it_st_fieldcat[]

     I_DEFAULT                         = 'X'
     I_SAVE                            = 'A'
*   IS_VARIANT                        =
     IT_EVENTS                         = i_events

*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_HDFC
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEif p_NEFT =  'X'.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING

     I_CALLBACK_PROGRAM                = S_REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'TOP-OF-PAGE'

     I_BACKGROUND_ID                   = 'ALV_BACKGROUND'
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = ST_LAYOUT
     IT_FIELDCAT                       = it_st_fieldcat[]

     I_DEFAULT                         = 'X'
     I_SAVE                            = 'A'
*   IS_VARIANT                        =
     IT_EVENTS                         = i_events

*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_NEFT
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    ELSEif p_dbs =  'X'.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING

     I_CALLBACK_PROGRAM                = S_REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'TOP-OF-PAGE'

     I_BACKGROUND_ID                   = 'ALV_BACKGROUND'
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = ST_LAYOUT
     IT_FIELDCAT                       = it_st_fieldcat[]

     I_DEFAULT                         = 'X'
     I_SAVE                            = 'A'
*   IS_VARIANT                        =
     IT_EVENTS                         = i_events

*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_dbs
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " REUSE_ALV_LIST_DISPLAY




*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP-OF-PAGE.
*----------------*
  WA_HEADER-TYP = 'A'.
  WA_HEADER-INFO = 'BANK FORMAT'.
  APPEND WA_HEADER TO T_HEADER.
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
      IT_LIST_COMMENTARY       = T_HEADER[]
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
            .
  CLEAR T_HEADER.
  CLEAR T_header[].
ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      FORM USER_COMMAND
*&---------------------------------------------------------------------*

FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
RS_USER_COMMAND TYPE SLIS_SELFIELD.
  case r_ucomm .
    when 'DOWNLOAD'.
      PERFORM DOWNLOAD.
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
FORM VARIANT_INIT .

  CLEAR s_variant.
  s_variant-report = s_repid.
  s_variant-handle = code.

ENDFORM.                    " VARIANT_INIT
