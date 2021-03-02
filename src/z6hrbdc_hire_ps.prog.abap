
report z6hrbdc_hire_ps
       no standard page heading line-size 255.

include zbdcrecx1_hr.


type-pools : slis,truxs.
data  v_mode.
data : v_filename type ibipparms-path.
data: it_raw type truxs_t_text_data.
tables: somlreci1.
*Data decleration for Error Message
data:
  t_msg      type table of bdcmsgcoll,   " Collecting Error messages
  w_msg      type bdcmsgcoll,
  w_msg1(51).

*Data decleration for Error Message
data:
  t_msg2      type table of bdcmsgcoll,   " Collecting Error messages
  w_msg2      type bdcmsgcoll,
  w_msg12(51).

*Structure for error message
types : begin of ty_s_error,
          msg_err(60) type c,
        end of ty_s_error.
data:
  wa_path    type string,
  wa_error   type string,
  wa_cnt     type i,
  w_mode     type c,
  wa_cnt1(2) type n,
  it_output  type table of ty_s_error,
  wa_output  like line of it_output.

data : p0017 like p0017.

types : begin of s_upload,
          pernr(08)," TYPE PA0000-PERNR,"	Personnel Number
          begda(10)," TYPE PA0000-BEGDA,"(10)(DATS)	Start Date
          massn(03)," TYPE PA0000-MASSN,"(2)(CHAR)  Action Type                XXXX
          massg(03)," TYPE PA0000-MASSG,"(2)(Char)  Reason for Action          XXXX
          bukrs              type pa0001-bukrs, "(4)(CHAR)  Company Code                     XXXX CHECK NO NEED IN RECORDING
          werks              type pa0001-werks, "(4)(CHAR)  Personnel Area
          btrtl              type pa0001-btrtl, "(4)(CHAR)  Personnel Subarea
          persg              type pa0001-persg, "(1)(CHAR)  Employee Group
          persk              type pa0001-persk, "(2)(CHAR)  Employee Subgroup
          abkrs              type pa0001-abkrs, "(2)(char)  Payroll Area
          kostl              type pa0001-kostl, "(10)(CHAR)  Cost Center
          orgeh              type pa0001-orgeh, "(8)(NUMC)  Organizational Unit
          description1(20),
*ZZSTATE TYPE PA0001-ZZSTATE,"  " REGION IN INFOTYPE 01
          plans(08)," TYPE PA0001-PLANS,"(8)(NUMC)  Job(Position)                    XXXX
          description2(40),
          mstbr              type pa0001-mstbr, "(8)(CHAR)  Supervisor Area
          zzlocation_key(11)," TYPE PA0001-ZZLOCATION_KEY,"(3)(CHAR)  Location  key  XXXX
          anred(04)," TYPE PA0002-ANRED,")(CHAR)  Initials                           XXXX
          nachn              type pa0002-nachn, "(40)(CHAR)  Last Name
          midnm              type pa0002-midnm, "(40)(CHAR)  Second Name
          vorna              type pa0002-vorna, "(40)(CHAR)  First Name
          gesch              type pa0002-gesch, "(1)(CHAR)  Gender Key                            XXXX
          gbdat(10)," TYPE PA0002-GBDAT,"( 8)(DATS)  Date of Birth
          gblnd              type pa0002-gblnd, "(3) (CHAR)  Country of Birth                     XXXX
          gbort              type pa0002-gbort, "(40)(CHAR)  Birthplace
          natio              type pa0002-natio, "(3)(CHAR)  Nationality                         XXXX
          famst              type pa0002-famst, "(1)(CHAR)  Marital Status Key                    XXXX
          anssa              type pa0006-anssa, "(4)(CHAR)  Address Record Type                   XXXX
          locat              type pa0006-locat, "(40)(CHAR) 2nd Address Line
          stras              type pa0006-stras, "(60)(CHAR)  Street and House Number
          ort01              type pa0006-ort01, "(60)(CHAR)  City
          pstlz              type pa0006-pstlz, "(10)(CHAR)  Postal Code
          land1              type pa0006-land1, "(3)(CHAR)  Country Key
          state              type pa0006-state, "(3)(CHAR)  Region (State, Province, County)     XXXX
          schkz(15)," TYPE PA0007-SCHKZ,"(8)(CHAR)  Work Schedule Rule               XXXX

          trfar              type pa0008-trfar, "CHAR(02)    "Pay scale type                      XXXX                INFOTYPE 008
          trfgb              type pa0008-trfgb, "CHAR(02)    "Pay Scale Area                      XXXX                INFOTYPE 008
          trfgr              type pa0008-trfgr, "CHAR(08)   " Pay Scale Group                     XXXX                INFOTYPE 008
          trfst(03)," TYPE PA0008-TRFST,"CHAR(02)   " Pay Scale Level                     XXXX                INFOTYPE 008
          lga01(10),"TYPE PA0008-LGA01,"CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008
          bet01              type pa0008-bet01, "   " Wage Type Amount for Payments
          waers              type pa0008-waers,   " Currency Key

          bnksa              type pa0009-bnksa, "(4)(CHAR)  Type of Bank Details Record           XXXX
          zlsch              type pa0009-zlsch, "(1)(CHAR)  Payment Method                        XXXX
          banks              type pa0009-banks, "(3)(CHAR)  Bank Country key                      XXXX
          bankl              type pa0009-bankl, "(15)(CHAR) Bank Keys
          bankn              type pa0009-bankn, "(18)(CHAR) Bank account number
          zzrtgs_neft_ifsc   type pa0009-zzrtgs_neft_ifsc, "(15)(CHAR)  RTGS/NEFT/IFSC
          erkla              type pa0017-erkla, "(CHAR)(1)  Reimbursement Group for Meals/Accommodations: Statutory
          ergru              type  pa0017-ergru, "(CHAR)(2) Reimbursement Group for Meals/Accomm. - Enterprise-Specific
          spebe              type  pa0017-spebe, "(CHAR)(1) Employee Grouping for Travel Expense Type
          stype              type pa0416-stype, "Quota type (attendance/absence quota)
          numbr(10), "TYPE  PA0416-NUMBR,"(DEC)(10) Quota Number Compensated
          begda2001(10)," TYPE PA2001-BEGDA,    "Start Date
          endda2001(10)," TYPE PA2001-ENDDA,    "End Date
          awart(16)," TYPE PA2001-AWART,        "    Attendance or Absence Type     XXXX               NOT IN THIS PROGRAM
          famsa              type  pa0021-famsa, "(CHAR)(4) Type of Family Record                 XXXX
          favor              type  pa0021-favor, "(CHAR)(40)  First Name
          fanam              type  pa0021-fanam, "(CHAR)(40)  Last Name
          fasex              type  pa0021-fasex, "(CHAR)(1) Gender Key
          fgbdt(10)," TYPE  PA0021-FGBDT,"(DATS)(8) Date of Birth
          fgbot              type  pa0021-fgbot, "(CHAR)(40)  Birthplace
          fanat(30)," TYPE  PA0021-FANAT,"(CHAR)(3) Nationality  NOW USE AS CELL NUMBER
          kdbsl              type  pa0021-kdbsl, "(CHAR)(2) Allowance Authorization
          kdzul              type  pa0021-kdzul, "(CHAR(2)  Child Allowances
          kdgbr              type  pa0021-kdgbr, "(CHAR)(2) Child Allowance Entitlement
          usrty1             type  pa0105-usrty, "(CHAR)(4) Communication Type                 user id
          usrid1             type  pa0105-usrid, "(CHAR)(30)  Communication ID/Number
*USRTY2 TYPE  PA0105-USRTY,"(CHAR)(4) Communication Type                 cell
*USRID2 TYPE  PA0105-USRID,"(CHAR)(30)  Communication ID/Number
          usrty3             type  pa0105-usrty, "(CHAR)(4) Communication Type                 email-id
          usrid3             type  pa0105-usrid, "(CHAR)(30)  Communication ID/Number
          induct_snd         type c length 1,    " Y = send induction email, N = Do not send induction email; IHDK900899
          induct_bcc         type c length 256,  " comma separated list of bcc recipients of induction email; IHDK900899
          flg                type i,
        end   of s_upload .

data : i_upload type standard table of  s_upload.
data : i_upload_tmp type standard table of  s_upload.
data : i_upload_fin type standard table of  s_upload.
data  : st_upload   type s_upload,
        wa_upload   type s_upload,
        wa_upload21 type s_upload,
        wa_upload06 type s_upload,
        wa_upload08 type s_upload,
        wa_upload15 type s_upload,
        wa_upload14 type s_upload.

data: wa_z6hr_ps_sap_pos type z6hr_ps_sap_pos.

data: it_2001 type standard table of s_upload,
      wa_2001 type s_upload.

data: it_0416 type standard table of s_upload,
      wa_0416 type s_upload.

data: test_pernr1 type pa0001-pernr,
      enddate(10),
      ex_pernr    type pa0001-pernr,
      ex_mstbr    type pa0001-mstbr.


types: begin of s_upload008,
         pernr      type pa0000-pernr, "  Personnel Number
         begda(10),
         endda(10),
         preas      type pa0008-preas,
         trfar      type pa0008-trfar, "CHAR(02)    "Pay scale type                      XXXX                INFOTYPE 008
         trfgb      type pa0008-trfgb, "CHAR(02)    "Pay Scale Area                      XXXX                INFOTYPE 008
         trfgr      type pa0008-trfgr, "CHAR(08)   " Pay Scale Group                     XXXX                INFOTYPE 008
         trfst(03)," TYPE PA0008-TRFST,"CHAR(02)   " Pay Scale Level                     XXXX                INFOTYPE 008
         lga01_code type pa0008-lga01, "CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008
         lga01(10),"TYPE PA0008-LGA01,"CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008
         bet01      type pa0008-bet01, "   " Wage Type Amount for Payments
         waers      type pa0008-waers,   " Currency Key
       end of s_upload008.

data: i_upload008  type standard table of s_upload008,
      wa_upload008 type s_upload008.


data : ret    type hrpbsinbapiret,
       wa_ret type line of hrpbsinbapiret.

data : begin of wa_pa0008,
         pernr     type pa0008-pernr,
         begda(10),"    TYPE PA0008-BEGDA,
         endda(10),"    TYPE PA0008-ENDDA,
         preas     type pa0008-preas, "Reason for Changing Master Data
         trfar     type pa0008-trfar, "Pay scale type
         trfgb     type pa0008-trfgb, "Pay Scale Area
         trfgr     type pa0008-trfgr, "Pay Scale Group
         trfst     type pa0008-trfst, "Pay Scale Level
         lga01(08),"   TYPE PA0008-LGA01,
         bet01     type pa0008-bet01,
         lga02(08),"   TYPE PA0008-LGA02,
         bet02     type pa0008-bet02,
         lga03(08),"    TYPE PA0008-LGA03,
         bet03     type pa0008-bet03,
         lga04(08),"   TYPE PA0008-LGA04,
         bet04     type pa0008-bet04,
         lga05(08),"   TYPE PA0008-LGA05,
         bet05     type pa0008-bet05,
         lga06(08),"   TYPE PA0008-LGA06,
         bet06     type pa0008-bet06,
         lga07(08),"   TYPE PA0008-LGA07,
         bet07     type pa0008-bet07,
         lga08(08),"   TYPE PA0008-LGA08,
         bet08     type pa0008-bet08,
         lga09(08),"   TYPE PA0008-LGA09,
         bet09     type pa0008-bet09,
         lga10(08),"   TYPE PA0008-LGA10,
         bet10     type pa0008-bet10,
         lga11(08),"   TYPE PA0008-LGA11,
         bet11     type pa0008-bet11,
         lga12(08),"    TYPE PA0008-LGA12,
         bet12     type pa0008-bet12,
         lga13(08),"    TYPE PA0008-LGA13,
         bet13     type pa0008-bet13,
         lga14(08),"   TYPE PA0008-LGA14,
         bet14     type pa0008-bet14,
         lga15(08),"   TYPE PA0008-LGA15,
         bet15     type pa0008-bet15,
         lga16(08),"   TYPE PA0008-LGA16,
         bet16     type pa0008-bet16,
         lga17(08),"   TYPE PA0008-LGA17,
         bet17     type pa0008-bet17,
         lga18(08),"   TYPE PA0008-LGA18,
         bet18     type pa0008-bet18,
         lga19     type pa0008-lga19,
         bet19     type pa0008-bet19,
         lga20     type pa0008-lga20,
         bet20     type pa0008-bet20,
       end of wa_pa0008.

data: it_pa0008 like table of wa_pa0008.

data: it_tsad3t like table of tsad3t with header line.  "AB25072017 INSERT...

data: tran_mode, test_pernr type pa0000-pernr .

data: first(02), second(05).

data: massn          type pa0000-massn, "*MASSN(03)," TYPE PA0000-MASSN,"(2)(CHAR)  Action Type                XXXX Mapped in table
      massg          type pa0000-massg, "*MASSG(03)," TYPE PA0000-MASSG,"(2)(Char)  Reason for Action          XXXX  Mapped in table
      zzlocation_key type pa0001-zzlocation_key , "*ZZLOCATION_KEY(11)," TYPE PA0001-ZZLOCATION_KEY,"(3)(CHAR)  Location  key  XXXX Mapped in table
      anred          type pa0002-anred, "*ANRED(03)," TYPE PA0002-ANRED,")(CHAR)  Initials                           XXXX  T522G mapped
      gesch          type pa0002-gesch, "*GESCH TYPE PA0002-GESCH,"(1)(CHAR)  Gender Key                            XXXX IF  M = 1 IF F = 2 " by logic hard code
      gblnd          type pa0002-gblnd, "*GBLND TYPE PA0002-GBLND,"(3) (CHAR)  Country of Birth                     XXXX T005 IF LANDK = IND SELECT LAND1 SPRAS = EN " by logic hard code
*      NATIO TYPE PA0002-NATIO,"*NATIO TYPE PA0002-NATIO,"(3)(CHAR)  Nationality                           XXXX T005" by logic hard code
      famst          type pa0002-famst, "*FAMST TYPE PA0002-FAMST,"(1)(CHAR)  Marital Status Key                    XXXX " by logic hard code
      anssa          type pa0006-anssa, "*ANSSA TYPE PA0006-ANSSA,"(4)(CHAR)  Address Record Type                   XXXX NNED TO MAP IN TABLE T591A DONE FOR 2 ENTRIES IN ird
*ZZSTATE TYPE PA0001-ZZSTATE,"*ZZSTATE TYPE PA0001-STATE,"(3)(CHAR)  Region (State, Province, County)     XXXX NEED TO MAP IN TABLE DONE
      state          type pa0006-state, "*STATE TYPE PA0006-STATE,"(3)(CHAR)  Region (State, Province, County)     XXXX NEED TO MAP IN TABLE DONE IN IRD NEED TO DO IN QA
      state_tmp      type pa0006-state, "*STATE TYPE PA0006-STATE,"(3)(CHAR)  Region (State, Province, County)     XXXX NEED TO MAP IN TABLE DONE IN IRD NEED TO DO IN QA
      schkz          type pa0007-schkz, "*SCHKZ(11)," TYPE PA0007-SCHKZ,"(8)(CHAR)  Work Schedule Rule               XXXX NEED TO MAP IN TABLE DONE
      bnksa          type pa0009-bnksa, "*BNKSA TYPE PA0009-BNKSA,"(4)(CHAR)  Type of Bank Details Record           XXXX t591a table need to map
*ZLSCH TYPE PA0009-ZLSCH,"*ZLSCH TYPE PA0009-ZLSCH,"(1)(CHAR)  Payment Method                        XXXX T042Z table need to map
*BANKS TYPE PA0009-BANKS,"*BANKS TYPE PA0009-BANKS,"(3)(CHAR)  Bank Country key                      XXXX
*PLANS_CHAR(10)," TYPE PA0001-PLANS," pOSITION.                                                                 XXXX MAPPED
      plans_num      type pa0001-plans, " pOSITION.                                                                 XXXX MAPPED
*PERSK TYPE PA0001-PERSK, " eMPLOYEE SUBGROUP
      awart          type pa2001-awart, " Attendance or Absence Type
      famsa          type pa0021-famsa, "Type of Family Record
      fasex          type pa0021-fasex, " GENDER KEY OF FAMILY MEMBER

      trfar          type pa0008-trfar, "CHAR(02)    "Pay scale type                      XXXX                INFOTYPE 008
      trfgb          type pa0008-trfgb, "CHAR(02)    "Pay Scale Area                      XXXX                INFOTYPE 008
      trfgr          type pa0008-trfgr, "CHAR(08)   " Pay Scale Group                     XXXX                INFOTYPE 008
      trfst          type pa0008-trfst, "CHAR(02)   " Pay Scale Level                     XXXX                INFOTYPE 008
      lga01          type pa0008-lga01. "CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008

* AB25072017 INSERT START...
data: gv_name(50)  type c,
      gv_date(10)  type c,
      gv_title(10) type c,
      gv_remark    type j_3r_comm,
      gv_user      like  bapibname-bapibname.

data: it_groups like table of bapiagr,
      wa_groups like bapiagr,
      it_return like table of bapiret2,
      wa_return like bapiret2.
* AB25072017 INSERT END...

data: action(03), action_type(03).
data: zpernr1 type pa0000-pernr.
data: zzbegda(10),
      gv_modi      type string,
      gv_mail      type string,
      gv_pernr     type persno,
      gv_pernr1(8).

selection-screen begin of block s2 with frame .

parameters: p_file  type ibipparms-path obligatory.
*            E_FILE   TYPE RLGRAP-FILENAME .       " Error File Path.

selection-screen end of block s2 .

selection-screen begin of block s01 with frame title text-t01.
parameters: p_fore  radiobutton group rad default 'X',
            p_back  radiobutton   group rad,
            p_noerr radiobutton   group rad.

select-options :  p_rec for somlreci1-receiver no-display.

selection-screen end of block s01.

include zbdcrecx1_bdc_note.


at selection-screen on value-request for p_file.

  call function 'F4_FILENAME'
    exporting
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE '
    importing
      file_name     = p_file.

  if not p_file is initial.
    v_filename = p_file.
  endif.

start-of-selection.

  perform  f_upload.
  perform  check_data.

  if i_upload is not initial.
    if p_fore eq 'X' .
      tran_mode = 'A' .
    elseif p_back eq 'X' .
      tran_mode = 'E' .
    elseif p_noerr eq 'X' .
      tran_mode = 'N' .
    endif .

    loop at i_upload into wa_upload.

      perform check_existing_emp.
*      IF WA_UPLOAD-MASSN = 'HIR' OR WA_UPLOAD-MASSN = 'CNF' OR WA_UPLOAD-MASSN = 'PRO'.
*      IF WA_UPLOAD-PLANS IS NOT INITIAL.
*        PERFORM CHK_POSITION.
*      ENDIF.
*      ENDIF.

      on change of wa_upload-usrid1.
        wa_upload-flg = wa_upload-flg + 1.
      endon.

      if wa_upload-massn is initial and wa_upload-awart is not initial and wa_upload-stype is initial.
        wa_upload-flg = 1.
      elseif  wa_upload-massn is initial and wa_upload-awart is initial and wa_upload-stype is not initial.
        wa_upload-flg = 1.
      elseif wa_upload-massn is initial and ( wa_upload-btrtl is not initial or
                                              wa_upload-abkrs is not initial or
                                              wa_upload-kostl is not initial or
                                              wa_upload-orgeh is not initial or
                                              wa_upload-plans is not initial or
                                              wa_upload-mstbr is not initial or
                                              wa_upload-state is not initial or
                                              wa_upload-zzlocation_key is not initial ).
        wa_upload-flg = 1.
      elseif wa_upload-massn is initial and ( wa_upload-famsa is not initial or
                                              wa_upload-favor is not initial ).
        wa_upload-flg = 1.

      endif.

      translate wa_upload-famsa to upper case.

      if wa_upload-massn is not initial.
        wa_upload-flg = wa_upload-flg + 1.
        action = wa_upload-massn.
      else.
        if wa_upload-flg = 0.
          wa_upload-massn = action.
        else.
          clear action.
        endif.
      endif.

      if wa_upload-abkrs = 'TH'.
        wa_upload-abkrs = 'TM'.
      endif.
      if wa_upload-massn is initial and wa_upload-awart is not initial and wa_upload-stype is initial.
        wa_upload-massn = 'AL'."Absence
        wa_upload-flg = 1.
      elseif  wa_upload-massn is initial and wa_upload-awart is initial and wa_upload-stype is not initial.
        wa_upload-massn = 'LE'." leave Encashment
        wa_upload-flg = 1.
      endif.

      if wa_upload-orgeh = '50002995'. " mail receive from santosh swami on 31.08.2017 PS
        wa_upload-orgeh = '50000095'.
      endif.


      modify i_upload from  wa_upload transporting famsa massn flg abkrs pernr orgeh.
      if wa_upload-massn = 'AL' and wa_upload-awart is not initial and wa_upload-stype is initial.
        wa_2001 = wa_upload.
        append wa_2001 to it_2001.
        clear: wa_2001.
      endif.

      if wa_upload-massn = 'LE' and wa_upload-stype is not initial and wa_upload-awart is initial.
        wa_0416 = wa_upload.
        append wa_0416 to it_0416.
        clear: wa_0416.
      endif.

    endloop.

    clear: wa_upload.

    loop at i_upload into wa_upload .
*      SELECT SINGLE PERNR FROM PA0000 INTO TEST_PERNR WHERE PERNR = WA_UPLOAD-USRID1.
*      IF SY-SUBRC <> 0.

*      ON CHANGE OF WA_UPLOAD-FLG.""MASSN .

      if wa_upload-flg > 0.
        case wa_upload-massn.
          when 'HIR' or 'ADD' or 'REH'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr." AND MASSN = 'I1'.
            if sy-subrc <> 0.
              if wa_upload-plans is not initial.
                perform chk_position.
              endif.

*                PERFORM CHK_POSITION.
              perform map_data.
              format color 3 intensified off.
              write : / 'Action:', action_type.
              perform run_bdc_pa40.
              clear: bdcdata , bdcdata[].
              refresh bdcdata.
              perform assignto_pos_hir.
              clear: zpernr1.
              select single pernr from pa0000 into zpernr1 where pernr = wa_upload-pernr.
              if sy-subrc = 0.
                perform send_mail.
***             below line for user creating in Su01 is commented as per requirement from venu on date 15.02.2018
***             as user getting created in Su01 and hence licence issue
***              below line uncomment as instucted by venu on 13.02.2018 PS
                perform create_user. "AB25072017 INSERT...
***             below perform is to send mail before confirmation of employee ---
**              in case of management employees - send mail on 2 nd and 5 th month after date of join
**              in case of trainee - send mail on 3 rd 7 th 10 th month after date of join
                if wa_upload-massn = 'HIR' or wa_upload-massn = 'ADD' or wa_upload-massn = 'REH'.
                  break 10106.
*{   REPLACE        SBXK900101                                        1
*\                if sy-sysid = 'IRD' or sy-sysid = 'IRQ'. "below testing pending in QA , need to move this pgm in PRD for user licence changes done above code , hence checking only for IRD IRQ
*                  if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ). "below testing pending in QA , need to move this pgm in PRD for user licence changes done above code , hence checking only for IRD IRQ
*}   REPLACE
                    perform send_preconf_mail.
                    if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ).
                    if condense( to_upper( wa_upload-induct_snd ) ) eq 'Y'. " IHDK900899
                      perform send_induction_email using wa_upload. " IHDK900606: HR: S_K: ZHR_PS: Add induction/welcome email: 13.2.19
                    endif.
                    endif.
*                  endif.
                endif.
              endif.
            else.
              format color 6 intensified off.
              write : / text-002 ,wa_upload-pernr." /'Person already hired. User ID', WA_UPLOAD-USRID1.
            endif.
          when 'RDG'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr." AND MASSN = 'I1'.
            if sy-subrc = 0.
              if wa_upload-plans is not initial.
                clear: bdcdata, bdcdata[].

                format color 3 intensified off.
                write : / 'Action: RDG'.", ACTION_TYPE.
                perform change_position.
                perform map_data.
                perform redesignation.
                clear: bdcdata , bdcdata[] .
                refresh bdcdata.

*              PERFORM ASSIGNTO_POS.
                write: /'-------------------------------------------------------------------------------------------------------------'.
*

*                PERFORM CHK_POSITION.
*                PERFORM MAP_DATA.
*                PERFORM ASSIGNTO_POS_HIR.
*                FORMAT COLOR 3 INTENSIFIED OFF.
*                WRITE : / 'Action: Redesignation'.
*                PERFORM REDESIGNATION .
*                CLEAR: BDCDATA , BDCDATA[] .
*                REFRESH BDCDATA.
*                WRITE: /'-------------------------------------------------------------------------------------------------------------'.
*
              endif.
            endif.
          when 'TER' or 'RET' or 'SUS'.

            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
              select single pernr
                from pa0000 into test_pernr
                where pernr = wa_upload-pernr               "USRID1
                and massn = 'I7'.
              if sy-subrc <> 0.

                perform map_data.
                format color 3 intensified off.
                write : / 'Action:', action_type.
                perform termination.
                clear: bdcdata , bdcdata[] .
                refresh bdcdata.
                clear: zpernr1.
                select single pernr from pa0000 into zpernr1 where pernr = wa_upload-pernr.
                if sy-subrc = 0.
                  perform send_mail.
                  break 10106.
                  " commenting this because we have shifted this in saperate program Z_ON_SEPERATION_DUE_DEACTIVE
*                 because we run saperation action for future date we cant deactivate any user if immidiatly
*                 we will schedule program Z_ON_SEPERATION_DUE_DEACTIVE daily which will check due date of 'I7' action and then deactivate in ATR table
*                 date : 9.7.2019 PS
                  perform update_indonet_master.


                  " IRDK930518 ----> HR: S_K: ZHR_PS: Delimit user logon/roles on user separation
                  perform delimit_user_logon.
                  " ----> End IRDK930518
                  " IHDK900728: HR: S_K: ZHR_PS: Delete conf email on separation: 25.2.19
                  if new zcl_email( )->delete_conf_email_on_sep( exporting iv_pernr = zpernr1 ).
                    format color 5 intensified off.
                    write: / 'Scheduled confirmation emails deleted successfully'.
                    format reset.
                  endif.
                  " End IHDK900728
                endif.
                write: /'-------------------------------------------------------------------------------------------------------------'.
              else.
                format color 6 intensified off.
                write : / text-006 ,wa_upload-pernr."USRID1." /'Saperation already takes place for User ID', WA_UPLOAD-USRID1.
                write: /'-------------------------------------------------------------------------------------------------------------'.
              endif.
            else.
              format color 6 intensified off.
              write : / text-007 ,wa_upload-pernr."USRID1." /'Saperation already takes place for User ID', WA_UPLOAD-USRID1.
            endif.
          when 'CNF' or 'PRB' or 'PRC'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
              select single pernr from pa0000 into test_pernr
                where pernr = wa_upload-pernr               "USRID1
                and massn = 'I3'.
              if sy-subrc <> 0.
                if wa_upload-plans is not initial.
                  perform chk_position.
                endif.

                perform map_data.
                format color 3 intensified off.
                write : / 'Action:', action_type.

                perform confirmation.
                clear: bdcdata , bdcdata[] .
                refresh bdcdata.
*                PERFORM ASSIGNTO_POS.
              else.
                format color 6 intensified off.
                write : / text-008 ,wa_upload-pernr."USRID1." /'Employee already Confirmed--', WA_UPLOAD-USRID1.
              endif.
            endif.
          when 'PRO' ."OR 'RDG'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
*              SELECT SINGLE PERNR FROM PA0000 INTO TEST_PERNR WHERE PERNR = WA_UPLOAD-USRID1 AND MASSN = 'I6'.
*              IF SY-SUBRC <> 0.

              if wa_upload-plans is not initial.
                clear: bdcdata, bdcdata[].
                perform change_position.
              endif.
              perform map_data.
*              PERFORM ASSIGNTO_POS_HIR.
              format color 3 intensified off.
              write : / 'Action:', action_type.

              perform promotion.
              clear: bdcdata , bdcdata[] .
              refresh bdcdata.

*              PERFORM ASSIGNTO_POS.
              write: /'-------------------------------------------------------------------------------------------------------------'.
*              ELSE.
*                FORMAT COLOR 6 INTENSIFIED OFF.
*                WRITE : / TEXT-008 ,WA_UPLOAD-USRID1." /'Employee already Confirmed--', WA_UPLOAD-USRID1.
*              ENDIF.
            endif.
          when 'TAB'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
              select single pernr from pa0000 into test_pernr
                where pernr = wa_upload-pernr               "USRID1
                and massn = 'I8'.
              if sy-subrc <> 0.

                if wa_upload-plans is not initial.
                  perform chk_position.
                endif.

                perform map_data.
                format color 3 intensified off.
                write : / 'Action:', action_type.

                perform trainee_absorption.
                clear: bdcdata , bdcdata[] .
                refresh bdcdata.
*                PERFORM ASSIGNTO_POS.
              endif.
            else.
              format color 6 intensified off.
              write : / text-016 ,wa_upload-pernr.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
          when 'XEC'. "Transfer -Inter Company
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
              if wa_upload-plans is not initial.
*                PERFORM CREATE_POSITION.
                perform chk_position.
              endif.

              perform map_data.
              perform assignto_pos_hir.
              format color 3 intensified off.
              write : / 'Action:', action_type.

              perform intercom.
              clear: bdcdata , bdcdata[] .
              refresh bdcdata.
*              PERFORM ASSIGNTO_POS.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
          when 'XRC' or 'TFR' or 'UTF'. "Transfer -Intra Company
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
              if wa_upload-plans is not initial.
                perform chk_position.
              endif.
              perform map_data.
              format color 3 intensified off.
              write : / 'Action:', action_type.

              perform intracom.
              clear: bdcdata , bdcdata[] .
              refresh bdcdata.
*              PERFORM ASSIGNTO_POS.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
          when 'AL'."Absence data updation
            clear: bdcdata , bdcdata[].
            refresh bdcdata.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
*            LOOP AT IT_2001 INTO WA_2001 WHERE PERNR = WA_UPLOAD-PERNR.
              if wa_upload-awart is not initial. " infotype 2001 Attendance or Absence Type
*                PERFORM MAP_DATA.
                clear : awart.
                select single sap  from z6hr_ppl_2_sap into awart
                  where tabname = 'PA2001'
                  and ppl_sft = wa_upload-awart.
                if sy-subrc <> 0 . clear:awart. endif.
                format color 3 intensified off.
                write : / 'Action: Absence Data'.
                if wa_upload-awart <> 'KM_AB_ODL_TK'.
                  perform update_2001.
                else.
                  format color 6 intensified off.
                  write: /'Can not Upload OD leave in SAP for Employee: ', wa_upload-pernr.
                endif.
                write: /'-------------------------------------------------------------------------------------------------------------'.
              endif.
            else.
              format color 6 intensified off.
              write : / text-017 ,wa_upload-pernr.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
*            ENDLOOP.
          when 'LE'."leave Encashment
            clear: bdcdata , bdcdata[].
            refresh bdcdata.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.
*             LOOP AT IT_0416 INTO WA_0416 WHERE PERNR = WA_UPLOAD-PERNR.
              if wa_upload-stype is not initial." infotype 0416 Quota Number Compensated
                format color 3 intensified off.
                write : / 'Action: Leave Encashment'.
                perform update_0416.
                write: /'-------------------------------------------------------------------------------------------------------------'.
              endif.
*              ENDLOOP.
*            Since now no need of approval from SAP , approved data will come from PS in File
*            CALL TRANSACTION 'HRPBSIN_LVENC_INFTY' AND SKIP FIRST SCREEN .
            else.
              format color 6 intensified off.
              write : / text-017 ,wa_upload-pernr.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
*          WHEN 'DTA'.
          when 'BON'.
          when 'PAY'.
*            SELECT SINGLE PERNR FROM PA0000 INTO TEST_PERNR WHERE PERNR = WA_UPLOAD-PERNR. "USRID1.
*            IF SY-SUBRC = 0.
**             LOOP AT IT_0416 INTO WA_0416 WHERE PERNR = WA_UPLOAD-PERNR.
*              IF WA_UPLOAD-LGA01 IS NOT INITIAL." infotype 0416 Quota Number Compensated
*                CLEAR: ACTION_TYPE.
*
*                SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP
*                    INTO MASSN  WHERE TABNAME = 'PA0000'
*                    AND PPL_SFT = WA_UPLOAD-MASSN AND FIELD = 'MASSN'.
*                IF SY-SUBRC <> 0 . CLEAR:MASSN. ENDIF.
*                ACTION_TYPE = WA_UPLOAD-MASSN.
*
*                FORMAT COLOR 3 INTENSIFIED OFF.
*                WRITE : / 'Action:', ACTION_TYPE.
*
*                PERFORM UPDATE_IT008.
*                WRITE: /'-------------------------------------------------------------------------------------------------------------'.
*              ENDIF.
**              ENDLOOP.
**            Since now no need of approval from SAP , approved data will come from PS in File
**            CALL TRANSACTION 'HRPBSIN_LVENC_INFTY' AND SKIP FIRST SCREEN .
*            ELSE.
*              FORMAT COLOR 6 INTENSIFIED OFF.
*              WRITE : / 'Employee is not Yet Hired',WA_UPLOAD-PERNR.
*              WRITE: /'-------------------------------------------------------------------------------------------------------------'.
*            ENDIF.
          when 'RPC'.
            select single pernr from pa0000 into test_pernr where pernr = wa_upload-pernr. "USRID1.
            if sy-subrc = 0.

              if wa_upload-plans is not initial.
                perform chk_position.
              endif.
              perform map_data.
              format color 3 intensified off.
              write : / 'Action:', action_type.
              perform reporting_chng.
              write: /'-------------------------------------------------------------------------------------------------------------'.
            endif.
          when 'XFR'.
          when '' or 'DTA'.
*            refer recording ZCOPY21 in QA for dependant data
*            SELECT SINGLE PERNR FROM PA0000 INTO TEST_PERNR WHERE PERNR = WA_UPLOAD-PERNR. "USRID1.
*            IF SY-SUBRC = 0.
*              IF WA_UPLOAD-PLANS IS NOT INITIAL.
*                PERFORM CHK_POSITION.
*              ENDIF.
*              PERFORM MAP_DATA.
*              FORMAT COLOR 3 INTENSIFIED OFF.
*              WRITE : / 'Action:', ACTION_TYPE.
*              PERFORM OTHER.
*              CLEAR: BDCDATA , BDCDATA[] .
*              REFRESH BDCDATA.
*            ENDIF.
          when others.
        endcase.
      endif.
*      ENDON.


    endloop.
  else.
    format color 6 intensified off.
    write: / 'Error while reading file , Please check format'.
  endif.



*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_upload .
  call function 'TEXT_CONVERT_XLS_TO_SAP'
    exporting
*     *I_FIELD_SEPERATOR   =
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw
      i_filename           = v_filename
    tables
      i_tab_converted_data = i_upload
    exceptions
      conversion_failed    = 1
      others               = 2.

  if sy-subrc = 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
endform.                    " F_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_data .
*  DATA:zemailbukrs TYPE pa0001-bukrs.
*EXPORT I_UPLOAD from I_UPLOAD TO MEMORY ID 'ZHRBDC'.
*
*SUBMIT Z6HR014C_INFOTYPE_008_UPL_PPL
*  WITH p_pernr = '6000081'.
* AB25072017 INSERT START...
  select * from tsad3t
           into table it_tsad3t
          where langu eq sy-langu.
* AB25072017 INSERT END...

** Check for E-mail id is " @modi.com "  - added by NK on 14.11.2017 - start
  i_upload_tmp[] = i_upload[].
  i_upload_fin[] = i_upload[].
BREAK 10106.
  loop at i_upload into wa_upload WHERE massn = 'HIR'.

*    IF wa_upload-massn = 'HIR'.
*     zemailbukrs = wa_upload-bukrs.
*    ENDIF.

    if wa_upload-usrid3 is not initial.
      " Changes for email check, IRDK930682, check for pattern -icc@modi.com in email
*      SPLIT wa_upload-usrid3 AT '@' INTO gv_mail gv_modi.
      gv_modi = wa_upload-usrid3.
      translate gv_modi to lower case.

      if wa_upload-bukrs <> '801'.  " for other company than indoreagens.com
      if gv_modi np '*@indofil.com' .
*        if gv_modi np '*@indoreagens.com'."
        " End change - IRDK930682
        delete i_upload_fin[] where pernr = wa_upload-pernr.
        if gv_pernr1 ne wa_upload-pernr.
          format color 6 intensified off.
          write : / 'ERROR:', 'Employee', wa_upload-pernr, 'E-mail id is not indofil.com'.
        endif.
*        ELSE.
*
*        endif.
      endif.
      ELSE. " @indoreagens.com
      if gv_modi np '*@indoreagens.com' .
*        if gv_modi np '*@indoreagens.com'."
        " End change - IRDK930682
        delete i_upload_fin[] where pernr = wa_upload-pernr.
        if gv_pernr1 ne wa_upload-pernr.
          format color 6 intensified off.
          write : / 'ERROR:', 'Employee', wa_upload-pernr, 'E-mail id is not @indoreagens.com'.
        endif.
*        ELSE.
*
*        endif.
      endif.
      endif.
    endif.
    gv_pernr1 = wa_upload-pernr.

  endloop.
  refresh: i_upload.
  i_upload[] = i_upload_fin[].
** Check for E-mail id is " @modi.com "  - added by NK on 14.11.2017 - end
endform.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  RUN_BDC_PA04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form run_bdc_pa40 .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  if wa_upload-gbdat is not initial.
    clear: gbdat, dd,mm ,yyyy.
    split wa_upload-gbdat at '/' into mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into gbdat.

  endif.

  perform bdc_dynpro      using 'SAPMP50A'       '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'RP50G-PERNR'    ''.

  perform bdc_dynpro      using 'SAPMP50A'       '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'T529T-MNTXT(01)'.
  perform bdc_field       using 'BDC_OKCODE'     '=PICK'.
  perform bdc_field       using 'RP50G-SELEC(01)'  'X'.

  perform bdc_dynpro      using 'MP000000'       '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'PSPAR-PERSK'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'PSPAR-PERNR'    wa_upload-usrid1.        "' 6000020'.  "Personnel Number           1 refer
  perform bdc_field       using 'P0000-BEGDA'    begda.         "'01.04.2012'." Start Date                2
  perform bdc_field       using 'P0000-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0000-MASSN'    massn.                   "'I1'.             "Action Type         XXXX   3
  perform bdc_field       using 'P0000-MASSG'    massg.                   "'08'.             "Reason for Action   XXXX   4
  perform bdc_field       using 'PSPAR-PLANS'    plans_num.                   "'99999999'. " JOB(plans pOSITION)       5   XXXX
  perform bdc_field       using 'PSPAR-WERKS'    wa_upload-werks.         "'IN01'.      "Personnel Area             6
  perform bdc_field       using 'PSPAR-PERSG'    wa_upload-persg.         "'M'.         "Employee Group             7
  perform bdc_field       using 'PSPAR-PERSK'    wa_upload-persk."WA_UPLOAD-PERSK.         "'M1'.        "Employee Subgroup          8

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'PSPAR-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
  perform bdc_field       using 'PSPAR-PERNR'    wa_upload-usrid1.        "' 6000020'."Personnel Number
  perform bdc_field       using 'P0000-BEGDA'    begda.         "'01.04.2012'." Start Date
  perform bdc_field       using 'P0000-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0000-MASSN'    massn.                   "'I1'. "Action Type         XXXX
  perform bdc_field       using 'P0000-MASSG'    massg.                   "'08'."Reason for Action   XXXX
  perform bdc_field       using 'PSPAR-PLANS'    plans_num.                   "'99999999'." JOB(plans pOSITION)
  perform bdc_field       using 'PSPAR-WERKS'    wa_upload-werks. "'IN01'.
  perform bdc_field       using 'PSPAR-PERSG'    wa_upload-persg."'M'.
  perform bdc_field       using 'PSPAR-PERSK'    wa_upload-persk."WA_UPLOAD-PERSK. "'M1'.

  perform bdc_dynpro      using 'MP000100'       '2000'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0001-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0001-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0001-BTRTL'    wa_upload-btrtl."'AN01'.     "  Personnel Subarea  ????????? 9
  perform bdc_field       using 'P0001-KOSTL'    wa_upload-kostl."
  perform bdc_field       using 'P0001-ABKRS'    wa_upload-abkrs."'IN'.       "Payroll Area                10
  perform bdc_field       using 'P0001-PLANS'    plans_num."'99999999'.

  perform bdc_field       using 'P0001-SACHP'    'HRA'.
  perform bdc_field       using 'P0001-SACHZ'    'TIA'.
  perform bdc_field       using 'P0001-SACHA'    'PYA'.


  if wa_upload-mstbr is  not initial.

    data: first(02), second(05).

    first = wa_upload-mstbr(02).
    if first = '60'.
      clear ex_mstbr.
      select single pernr from pa0000 into ex_mstbr where pernr = wa_upload-mstbr+02(05).
      if sy-subrc = 0.
        wa_upload-mstbr = ex_mstbr.
      endif.
    endif.

*    SELECT SINGLE PLANS FROM PA0001 INTO REPORT_POS WHERE PERNR = WA_UPLOAD-MSTBR
*      AND ENDDA = '99991231'.
  endif.



  perform bdc_field       using 'P0001-MSTBR'    wa_upload-mstbr."'6001278'.  "Supervisor Area             11
  perform bdc_field       using 'P0001-ORGEH'    wa_upload-orgeh."'50000083'. "Organizational Unit         12
  perform bdc_field       using 'BDC_CURSOR'     'P0001-ZZLOCATION_KEY'.
  perform bdc_field       using 'P0001-ZZSTATE'  state."'13'.       "Region (State, Province, County)   13   XXXX
  perform bdc_field       using 'P0001-ZZLOCATION_KEY' zzlocation_key."'013'. " Location  key          14         XXXX

  perform bdc_dynpro      using 'MP000100'       '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
  perform bdc_field       using 'P0001-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0001-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0001-BTRTL'    wa_upload-btrtl. "'AN01'.
  perform bdc_field       using 'P0001-KOSTL'    wa_upload-kostl."
  perform bdc_field       using 'P0001-ABKRS'    wa_upload-abkrs."'IN'.
  perform bdc_field       using 'P0001-PLANS'    plans_num."'99999999'.
  perform bdc_field       using 'P0001-MSTBR'    wa_upload-mstbr. "'6001278'.
  perform bdc_field       using 'P0001-ORGEH'    wa_upload-orgeh."'50000083'.
  perform bdc_field       using 'P0001-ZZSTATE'  state.     "'13'.
  perform bdc_field       using 'P0001-ZZLOCATION_KEY'  zzlocation_key. " '013'.

  perform bdc_dynpro      using 'MP000200' '2044'.
  perform bdc_field       using 'BDC_CURSOR'     'P0002-FAMST'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0002-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0002-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0002-ANRED'    anred."'1'.       "Initials                         15      XXXX
  perform bdc_field       using 'P0002-NACHN'    wa_upload-nachn."'Reddy'.   "Last Name                        16
  perform bdc_field       using 'P0002-VORNA'    wa_upload-vorna."'Nimmakayala Pratap'. "First Name            17
  perform bdc_field       using 'P0002-MIDNM'    wa_upload-midnm."'M'.                  "Second Name           18
  perform bdc_field       using 'P0002-GBDAT'    gbdat."'31.07.1959'.          " Date of Birth       19
  perform bdc_field       using 'P0002-GBORT'    wa_upload-gbort."' Kadapa'.             "Birthplace           20
  perform bdc_field       using 'P0002-GESCH'    gesch."'1'.                   "Gender Key           21   XXXX
  perform bdc_field       using 'P0002-GBLND'    gblnd."'IN'.                  "Country of Birth     22   XXXX
  perform bdc_field       using 'P0002-NATIO'    'IN'.                  "Nationality          23   XXXX
  perform bdc_field       using 'P0002-SPRSL'    'EN'.
  perform bdc_field       using 'P0002-FAMST'    famst."'1'.                   "Marital Status Key   24   XXXX
  perform bdc_dynpro      using 'MP000200' '2044'.
  perform bdc_field       using 'BDC_CURSOR'     'P0002-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
  perform bdc_field       using 'P0002-BEGDA'    begda."'31.07.1959'.
  perform bdc_field       using 'P0002-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0002-ANRED'    anred.     "'1'.
  perform bdc_field       using 'P0002-NACHN'    wa_upload-nachn."'Reddy'.
  perform bdc_field       using 'P0002-VORNA'    wa_upload-vorna."'Nimmakayala Pratap'.
  perform bdc_field       using 'P0002-MIDNM'    wa_upload-midnm."'M'.
  perform bdc_field       using 'P0002-GBDAT'    gbdat."'31.07.1959'.
  perform bdc_field       using 'P0002-GBORT'    wa_upload-gbort."' Kadapa'.
  perform bdc_field       using 'P0002-GESCH'    gesch.     "'1'.
  perform bdc_field       using 'P0002-GBLND'    gblnd."'IN'.
  perform bdc_field       using 'P0002-NATIO'    'IN'.
  perform bdc_field       using 'P0002-SPRSL'    'EN'.
  perform bdc_field       using 'P0002-FAMST'    famst.     "'1'.

  clear : state_tmp.
  data: flg type i.
  clear: flg.

  flg = 0.

  read table i_upload into wa_upload06 with key usrid1 = wa_upload-usrid1 anssa = 'MAIL' massn = 'HIR'.
  if sy-subrc = 0.
    flg = 1.
    select single sap  from z6hr_ppl_2_sap    into state_tmp  where tabname = 'PA0006' and ppl_sft = wa_upload06-state.
    if sy-subrc <> 0 . clear:state_tmp. endif.

    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-NUM01'.
    perform bdc_field       using 'BDC_OKCODE'     '/00'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0006-ANSSA'    '01'. "ANSSA                 "  Address Record Type    25 XXXX
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'1625 TRICHY ROAD'.    "2nd Address Line         26
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'1625 TRICHY ROAD'.    "Street and House Number  27
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz."'641018'.              "Postal Code              28
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'COIMBATORE'.          " City                    29
    perform bdc_field       using 'P0006-STATE'    state_tmp."'22'.                  " Region (State, Province, County)???????? XXXX
    perform bdc_field       using 'P0006-LAND1'    'IN'."WA_UPLOAD-LAND1."'IN'.                  "Country Key              30
    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'1625 TRICHY ROAD'.
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'1625 TRICHY ROAD'.
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz. "'641018'.
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'COIMBATORE'.
    perform bdc_field       using 'P0006-STATE'    state_tmp. "'22'.
    perform bdc_field       using 'P0006-LAND1'    'IN'."WA_UPLOAD-LAND1."'IN'.
  endif.
  clear : wa_upload06.
  read table i_upload into wa_upload06 with key usrid1 = wa_upload-usrid1 anssa = 'HOME' massn = 'HIR'.
  if sy-subrc = 0.
    if flg = 1.
      flg = 2.
    elseif flg = 0.
      flg = 1.
    endif.

    select single sap  from z6hr_ppl_2_sap    into state_tmp  where tabname = 'PA0006' and ppl_sft = wa_upload06-state.
    if sy-subrc <> 0 . clear:state_tmp. endif.

    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-STATE'.
    perform bdc_field       using 'BDC_OKCODE'     '/00'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0006-ANSSA'    '02'.                  "  Address Record Type     XXXX  (IN CASE THEY PROVIDE IN FILE)
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'test permenant add2nd'.  "
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'street pemaanant'.
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz. "'234567'.
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'city_perm'.
    perform bdc_field       using 'P0006-STATE'    state_tmp. "'22'.
    perform bdc_field       using 'P0006-LAND1'    'IN'.
    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'test permenant add2nd'.
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'street pemaanant'.
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz. "'234567'.
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'city_perm'.
    perform bdc_field       using 'P0006-STATE'    state_tmp. "'22'.
    perform bdc_field       using 'P0006-LAND1'    'IN'.

    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-STATE'.
    perform bdc_field       using 'BDC_OKCODE'     '/00'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0006-ANSSA'    '02'.                  "  Address Record Type     XXXX  (IN CASE THEY PROVIDE IN FILE)
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'test permenant add2nd'.  "
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'street pemaanant'.
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz. "'234567'.
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'city_perm'.
    perform bdc_field       using 'P0006-STATE'    state_tmp. "'22'.
    perform bdc_field       using 'P0006-LAND1'    'IN'.
    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_CURSOR'     'P0006-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
    perform bdc_field       using 'P0006-BEGDA'    begda."'01.04.2012'.
    perform bdc_field       using 'P0006-ENDDA'    '31.12.9999'.
    perform bdc_field       using 'P0006-LOCAT'    wa_upload06-locat."'test permenant add2nd'.
    perform bdc_field       using 'P0006-STRAS'    wa_upload06-stras."'street pemaanant'.
    perform bdc_field       using 'P0006-PSTLZ'    wa_upload06-pstlz. "'234567'.
    perform bdc_field       using 'P0006-ORT01'    wa_upload06-ort01."'city_perm'.
    perform bdc_field       using 'P0006-STATE'    state_tmp. "'22'.
    perform bdc_field       using 'P0006-LAND1'    'IN'.

    perform bdc_dynpro      using 'MP000600' '2005'.
    perform bdc_field       using 'BDC_OKCODE'                              '/ENXT'.
    perform bdc_field       using 'BDC_CURSOR'                              'P0006-BEGDA'.

  endif.
  clear : wa_upload06.




* sKEEP ADDRESS TYPES BASED ON DATA PROVIDED IN FILE
*  IF flg = 0.
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*  ELSEIF flg = 1.
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*
*  ELSEIF flg = 2.
*    PERFORM bdc_dynpro      USING 'MP000600' '2005'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/ENXT'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0006-BEGDA'.
*
*  ENDIF.


*  PERFORM BDC_DYNPRO      USING 'MP000600' '2005'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/ENXT'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0006-BEGDA'.

*  PERFORM BDC_DYNPRO      USING 'MP000900' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/00'.
*  PERFORM BDC_FIELD       USING 'P0009-BEGDA'    BEGDA."'01.04.2012'.
*  PERFORM BDC_FIELD       USING 'P0009-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0009-BNKSA'    BNKSA."'0'.                               " Type of Bank Details Record   31  XXXX
**  PERFORM BDC_FIELD       USING 'Q0009-EMFTX'    'Mr. Nimmakayala Pratap M Reddy'. " DEFAULT COME
*  PERFORM BDC_FIELD       USING 'P0009-BANKS'    'IN'.                              " Bank Country key              32 XXXX
*  PERFORM BDC_FIELD       USING 'P0009-BANKL'    WA_UPLOAD-BANKL."'HDFC0000009'.                     " Bank Keys                     33
*  PERFORM BDC_FIELD       USING 'P0009-BANKN'    WA_UPLOAD-BANKN."'2761050036055'.                   "Bank account number            34
*  PERFORM BDC_FIELD       USING 'P0009-ZLSCH'    WA_UPLOAD-ZLSCH."'F'.                               "Payment Method                 35 XXXX
*  PERFORM BDC_FIELD       USING 'P0009-WAERS'    WA_UPLOAD-WAERS."'INR'.           " DEFAULT INR
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0009-ZZRTGS_NEFT_IFSC'.
*  PERFORM BDC_FIELD       USING 'P0009-ZZRTGS_NEFT_IFSC'   WA_UPLOAD-ZZRTGS_NEFT_IFSC."    '12345670'.          "RTGS/NEFT/IFSC                 36

  perform bdc_dynpro      using 'MP000900' '2000'.
  perform bdc_field       using 'BDC_CURSOR'      'P0009-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'      '/ENXT'."'=UPD'.
  perform bdc_field       using 'P0009-BEGDA'     begda."'01.04.2012'.
  perform bdc_field       using 'P0009-ENDDA'     '31.12.9999'.
  perform bdc_field       using 'P0009-BNKSA'     bnksa.    "'0'.
*  PERFORM BDC_FIELD       USING 'Q0009-EMFTX'     'Mr. Nimmakayala Pratap M Reddy'.
  perform bdc_field       using 'P0009-BANKS'     'IN'.
  perform bdc_field       using 'P0009-BANKL'     wa_upload-bankl."'HDFC0000009'.
  perform bdc_field       using 'P0009-BANKN'     wa_upload-bankn."'2761050036055'.
  perform bdc_field       using 'P0009-ZLSCH'     wa_upload-zlsch."'F'.
  perform bdc_field       using 'P0009-WAERS'     wa_upload-waers."'INR'.
  perform bdc_field       using 'P0009-ZZRTGS_NEFT_IFSC'   wa_upload-zzrtgs_neft_ifsc."   '12345670'.
  perform bdc_dynpro      using 'SAPLSPO1' '0200'.
  perform bdc_field       using 'BDC_OKCODE'     '=YES'.


  perform bdc_dynpro      using 'SAPMSSY0' '0120'.
  perform bdc_field       using 'BDC_CURSOR'                              '04/04'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_dynpro      using 'MP002800' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/ENXT'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0028-EXDAT'.


*  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'      '04/03'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'      '=CANC'.
  perform bdc_dynpro      using 'MP001700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'      'P0017-PTZUO'.
  perform bdc_field       using 'BDC_OKCODE'      '/00'.
  perform bdc_field       using 'P0017-BEGDA'     begda."'01.04.2012'.
  perform bdc_field       using 'P0017-ENDDA'     '31.12.9999'.
  perform bdc_field       using 'P0017-ERKLA'     wa_upload-erkla."'3'.           "Reimbursement Group for Meals/Accommodations: Statutory  37
  perform bdc_field       using 'P0017-ERGRU'     wa_upload-ergru."'1'.           "Reimbursement Group for Meals/Accomm. - Enterprise-Specific 38
  perform bdc_field       using 'P0017-SPEBE'     wa_upload-spebe."'C'.           "Employee Grouping for Travel Expense Type 39
*  PERFORM BDC_FIELD       USING 'P0017-PTZUO'     'OTH'. " iF THEY PROVIDE IN FILE.
  perform bdc_dynpro      using 'MP001700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'      'P0017-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'      '=UPD'.
  perform bdc_field       using 'P0017-BEGDA'     begda."'01.04.2012'.
  perform bdc_field       using 'P0017-ENDDA'     '31.12.9999'.
  perform bdc_field       using 'P0017-ERKLA'     wa_upload-erkla. "'3'.
  perform bdc_field       using 'P0017-ERGRU'     wa_upload-ergru. "'1'.
  perform bdc_field       using 'P0017-SPEBE'     wa_upload-spebe."'C'.
*  PERFORM BDC_FIELD       USING 'P0017-PTZUO'    'OTH'.  " iF THEY PROVIDE IN FILE.
  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0007-SCHKZ'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0007-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0007-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'    schkz."'INDOFIL'.   " Work Schedule Rule                       40   XXXX
*  PERFORM BDC_FIELD       USING 'P0007-ZTERF'    '7'.         " default not in file system will take auto
*  PERFORM BDC_FIELD       USING 'P0007-EMPCT'    '  100.00'.  "  not in file system will take auto
  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
  perform bdc_field       using 'P0007-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0007-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'    schkz."'INDOFIL'.
**  PERFORM BDC_FIELD       USING 'P0007-ZTERF'    '7'.
**  PERFORM BDC_FIELD       USING 'P0007-EMPCT'    '  100.00'.
**  PERFORM BDC_FIELD       USING 'P0007-ARBST'    '    8.50'.
**  PERFORM BDC_FIELD       USING 'P0007-WKWDY'    '    5.00'.
  perform bdc_dynpro      using 'SAPMSSY0' '0120'.
  perform bdc_field       using 'BDC_CURSOR'     '04/03'.
  perform bdc_field       using 'BDC_OKCODE'     '=CANC'.
  perform bdc_dynpro      using 'MP002300' '2000'.
  perform bdc_field       using 'BDC_OKCODE'     '/ENXT'.
  perform bdc_field       using 'BDC_CURSOR'     'P0023-BEGDA'.
  perform bdc_dynpro      using 'SAPLSPO1' '0200'.
  perform bdc_field       using 'BDC_OKCODE'     '=YES'.

  perform bdc_dynpro      using 'MP010500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0105-USRID'.
  perform bdc_field       using 'BDC_OKCODE'     '=UPD'.
  perform bdc_field       using 'P0105-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0105-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0105-USRID'    wa_upload-usrid1."SY-UNAME."'10106'.       " user id

**  PERFORM BDC_DYNPRO      USING 'MP010500' '2000'.
**  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/ENXT'.
**  PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0105-USRID'.
**  PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0200'.
**  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '=YES'.

  perform bdc_dynpro      using 'MP010500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0105-USRID'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0105-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0105-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'    ''."'CELL'.        " field not provide in file  not present  in recording added by ps
  break 10106.
  if wa_upload-fanat is initial.
    wa_upload-fanat = '9999999999'.
  else. " Added IRDK930682, remove non-numeric chars from cell no
    replace all occurrences of regex '[^[:digit:] ]' in wa_upload-fanat with ''.
    condense wa_upload-fanat.
  endif.
  perform bdc_field       using 'P0105-USRID'    wa_upload-fanat."'9999999999'."'9224412346'.  " field not provide in file
  perform bdc_dynpro      using 'MP010500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'    'P0105-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'    '=UPD'.
  perform bdc_field       using 'P0105-BEGDA'   begda."'01.04.2012'.
  perform bdc_field       using 'P0105-ENDDA'   '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'   WA_UPLOAD-USRTY2." 'CELL'.        " field not provide in file  not present  in recording added by ps
  perform bdc_field       using 'P0105-USRID'   wa_upload-fanat."'9224412346'.

*  PERFORM BDC_DYNPRO      USING 'MP010500' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0105-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/ENXT'.
*  PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '=YES'.


  perform bdc_dynpro      using 'MP010500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'    'P0105-USRID_LONG'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0105-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0105-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'    WA_UPLOAD-USRTY3.
  perform bdc_field       using 'P0105-USRID_LONG'    wa_upload-usrid3."'1234PS@GMAIL.COM'.  " Communication ID/Number

  perform bdc_dynpro      using 'MP010500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'    'P0105-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'    '=UPD'.
  perform bdc_field       using 'P0105-BEGDA'   begda."'01.04.2012'.
  perform bdc_field       using 'P0105-ENDDA'   '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'   WA_UPLOAD-USRTY3." 'CELL'.        " field not provide in file  not present  in recording added by ps
  perform bdc_field       using 'P0105-USRID_LONG'   wa_upload-usrid3."'1234PS@GMAIL.COM'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'    'UPD'.

*  PERFORM BDC_DYNPRO      USING 'MP010500' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'    'P0105-USRID'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/00'.
*  PERFORM BDC_FIELD       USING 'P0105-BEGDA'    BEGDA."'01.04.2012'.
*  PERFORM BDC_FIELD       USING 'P0105-ENDDA'    '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'    WA_UPLOAD-USRTY3."'10'
*  PERFORM BDC_FIELD       USING 'P0105-USRID'    WA_UPLOAD-USRID3."'1234PS@GMAIL.COM'.  " Communication ID/Number
*  PERFORM BDC_DYNPRO      USING 'MP010500' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'    'P0105-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'    '=UPD'.
*  PERFORM BDC_FIELD       USING 'P0105-BEGDA'   BEGDA."'01.04.2012'.
*  PERFORM BDC_FIELD       USING 'P0105-ENDDA'   '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0105-USRTY'   WA_UPLOAD-USRTY3." '10'.        " field not provide in file  not present  in recording added by ps
*  PERFORM BDC_FIELD       USING 'P0105-USRID'   WA_UPLOAD-USRID3."'1234PS@GMAIL.COM''.


*  PERFORM BDC_DYNPRO      USING 'MP010500' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/ENXT'.
*  PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '=YES'.
  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'     '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'     'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'     '/EBCK'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.
  endif.

*  READ TABLE T_MSG INTO W_MSG WITH KEY MSGTYP = 'E'.
  select single pernr from pa0001 into test_pernr1 where pernr = wa_upload-pernr. "USRID1.
  if sy-subrc = 0.

    format color 5 intensified off.
    write : / text-003 , wa_upload-usrid1 ."'Hiring Action SUCCESSFUL for User ID', WA_UPLOAD-USRID1.
**********************************************************************************************************
*    *Perform bdc_transaction for 'PA30' infotype 21 Family members details.
**********************************************************************************************************
    clear: bdcdata , bdcdata[] .
    refresh bdcdata.


    clear : wa_upload21.
    loop at i_upload into wa_upload21 where usrid1 = wa_upload-usrid1
      and ( famsa = 'SP'
            or famsa = 'B'
            or famsa = 'FA'
            or famsa = 'FL' or famsa = 'ML' or famsa = 'MO'
            or famsa = 'SI' )." AND MASSN = 'HIR'.

      clear: dd, mm, yyyy.

      if wa_upload21-fgbdt is not initial.
        clear: fgbdt, dd,mm ,yyyy.
        split wa_upload21-fgbdt at '/' into mm dd yyyy.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = dd
          importing
            output = dd.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = mm
          importing
            output = mm.

        concatenate dd '.' mm '.' yyyy into fgbdt.

      endif.
      clear : famsa.
      select single sap  from z6hr_ppl_2_sap    into famsa  where tabname = 'PA0021' and ppl_sft = wa_upload21-famsa.
      if sy-subrc <> 0 . clear:famsa. endif.

*      IF FAMSA = 1.

*       RUN BDC FOR PA30 21 INFOTYPE FOR SUBTYPE 1.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
      perform bdc_field       using 'RP50G-PERNR'                              wa_upload21-usrid1. "'6000020'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
      perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
      perform bdc_field       using 'RP50G-SUBTY'                              famsa. "'1'.
      perform bdc_dynpro      using 'MP002100' '2000'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0021-FANAT'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'P0021-BEGDA'                              begda."'01.04.2012'.
      perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
      perform bdc_field       using 'P0021-FANAM'                              wa_upload21-favor."FANAM."'Reddy'.
      perform bdc_field       using 'P0021-FAVOR'                              wa_upload21-favor."'ABCD'.

      if wa_upload21-famsa = 'FA' or wa_upload21-famsa = 'FL' or wa_upload21-famsa = 'S'
        or wa_upload21-famsa = 'B'.
        wa_upload21-fasex = 'M'.
      elseif wa_upload21-famsa = 'ML' or wa_upload21-famsa = 'MO' or wa_upload21-famsa = 'SI'
        or wa_upload21-famsa = 'D'.
        wa_upload21-fasex = 'F'.
      endif.

      if wa_upload21-famsa <> 'SP'.
        if wa_upload21-fasex = 'F'.
          perform bdc_field       using 'Q0021-GESC2'                              'X'.
        elseif wa_upload21-fasex = 'M'.
          perform bdc_field       using 'Q0021-GESC1'                              'X'.
        endif.
      endif.

      perform bdc_field       using 'P0021-FGBDT'                              fgbdt."'29.03.1987'.
      perform bdc_field       using 'P0021-FGBOT'                              wa_upload21-fgbot."'mumbai'.
      perform bdc_field       using 'P0021-FANAT'                              'IN'.
      perform bdc_dynpro      using 'MP002100' '2000'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0021-BEGDA'.
      perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
      perform bdc_field       using 'P0021-BEGDA'                             begda." '01.04.2012'.
      perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
      perform bdc_field       using 'P0021-FANAM'                             wa_upload21-favor."FANAM."'Reddy'.
      perform bdc_field       using 'P0021-FAVOR'                             wa_upload21-favor." 'ABCD'.
*    PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
*      IF WA_UPLOAD21-FASEX = 'F'.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
*      ELSEIF WA_UPLOAD21-FASEX = 'M'.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
*      ENDIF.

      perform bdc_field       using 'P0021-FGBDT'                              fgbdt."'29.03.1987'.
      perform bdc_field       using 'P0021-FGBOT'                              wa_upload21-fgbot."'mumbai'.
      perform bdc_field       using 'P0021-FANAT'                              'IN'.


      if bdcdata is not initial.
        call transaction 'PA30' using bdcdata mode tran_mode update 'S'
         messages into t_msg2 .
        read table t_msg2 into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0 .
          format color 5 intensified off.
          write : / text-004 , famsa , 'User ID' , wa_upload-usrid1. ."'Record Uploaded SUCESSFULLY for InfoType 0021 ,Subtype-', FAMSA , 'User ID' , WA_UPLOAD-USRID1.
        else.
          format color 6 intensified off.
          write :/ 'Error Occurred in Infotype 0021 Subtype -', famsa , 'USER ID:' , wa_upload-usrid1.
        endif.
      endif.
      clear:t_msg, w_msg ,t_msg2.
      clear: bdcdata , bdcdata[] .
      refresh bdcdata.
*      wa_upload-massn = 'HIR'.
*      modify I_UPLOAD from wa_upload transporting massn.

    endloop.



*    IF BDCDATA IS NOT INITIAL.
*      CALL TRANSACTION 'PA30' USING BDCDATA MODE TRAN_MODE UPDATE 'S'
*       MESSAGES INTO T_MSG2 .
*      READ TABLE T_MSG2 INTO W_MSG WITH KEY MSGTYP = 'E'.
*      IF SY-SUBRC <> 0 .
*        FORMAT COLOR 5 INTENSIFIED OFF.
*        WRITE : / TEXT-004 , FAMSA , 'User ID' , WA_UPLOAD-USRID1. ."'Record Uploaded SUCESSFULLY for InfoType 0021 ,Subtype-', FAMSA , 'User ID' , WA_UPLOAD-USRID1.
*      ELSE.
*        FORMAT COLOR 6 INTENSIFIED OFF.
*        WRITE :/ 'Error Occurred in Infotype 0021 Subtype -', FAMSA , 'USER ID:' , WA_UPLOAD-USRID1.
*      ENDIF.
*    ENDIF.
*    CLEAR:T_MSG, W_MSG ,T_MSG2.

**       ELSEIF FAMSA = 2.
**       RUN BDC FOR PA30 21 INFOTYPE FOR SUBTYPE 2 Child .

    clear: bdcdata , bdcdata[] .
    refresh bdcdata.
    clear: wa_upload21.
    loop at i_upload into wa_upload21 where usrid1 = wa_upload-usrid1
      and famsa is not initial and ( famsa = 'C'
                                    or famsa = 'D'
                                    or famsa = 'S' )." AND MASSN = 'HIR'.

      clear: dd, mm, yyyy.

      if wa_upload21-fgbdt is not initial.
        clear: fgbdt, dd,mm ,yyyy.
        split wa_upload21-fgbdt at '/' into mm dd yyyy.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = dd
          importing
            output = dd.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = mm
          importing
            output = mm.

        concatenate dd '.' mm '.' yyyy into fgbdt.

      endif.
      clear : famsa.
      select single sap  from z6hr_ppl_2_sap    into famsa  where tabname = 'PA0021' and ppl_sft = wa_upload21-famsa.
      if sy-subrc <> 0 . clear:famsa. endif.


      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload21-usrid1." '6000020'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
      perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
      perform bdc_field       using 'RP50G-SUBTY'                              famsa. "'2'.
      perform bdc_dynpro      using 'MP002100' '2040'.
      perform bdc_field       using 'BDC_CURSOR'                              'Q0021-OBJTX'.
      perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
      perform bdc_field       using 'P0021-BEGDA'                              begda."'01.12.2012'.
      perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
*          perform bdc_field       using 'P0021-OBJPS'                              '01'. "Child no.
      perform bdc_field       using 'P0021-FANAM'                              wa_upload21-favor."FANAM."'Reddy'.
      perform bdc_field       using 'P0021-FAVOR'                              wa_upload21-favor."'PS'.
*      IF WA_UPLOAD21-FASEX = 'F'.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
*      ELSEIF WA_UPLOAD21-FASEX = 'M'.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
*      ENDIF.
      perform bdc_field       using 'P0021-KDBSL'                              wa_upload-kdbsl."'Y'.
      perform bdc_field       using 'P0021-FGBDT'                              fgbdt."'01.12.2012'.
      perform bdc_field       using 'P0021-KDZUL'                              wa_upload-kdzul."'Y'.
      perform bdc_field       using 'P0021-FGBOT'                              wa_upload21-fgbot. "'mumbai2'.
      perform bdc_field       using 'P0021-KDGBR'                              wa_upload-kdgbr."'Y'.
      perform bdc_field       using 'P0021-FANAT'                              'IN'.


      if bdcdata is not initial.
        call transaction 'PA30' using bdcdata mode tran_mode update 'S'
            messages into t_msg2 .

        read table t_msg2 into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0.
          format color 5 intensified off.
          write : /'Record Uploaded SUCESSFULLY for InfoType 0021 ,Subtype-', famsa , 'User ID' , wa_upload-usrid1.
        else.
          format color 6 intensified off.
          write :/ 'Error Occurred in Infotype 0021 Subtype -', famsa , 'USER ID:' , wa_upload-usrid1.
        endif.

      endif.
      clear:t_msg, w_msg ,t_msg2.
      clear: bdcdata , bdcdata[] .
      refresh bdcdata.
*      wa_upload-massn = 'HIR'.
*      modify I_UPLOAD from wa_upload transporting massn.

    endloop.

*    IF BDCDATA IS NOT INITIAL.
*      CALL TRANSACTION 'PA30' USING BDCDATA MODE TRAN_MODE UPDATE 'S'
*          MESSAGES INTO T_MSG2 .
*
*      READ TABLE T_MSG2 INTO W_MSG WITH KEY MSGTYP = 'E'.
*      IF SY-SUBRC <> 0.
*        FORMAT COLOR 5 INTENSIFIED OFF.
*        WRITE : /'Record Uploaded SUCESSFULLY for InfoType 0021 ,Subtype-', FAMSA , 'User ID' , WA_UPLOAD-USRID1.
*      ELSE.
*        FORMAT COLOR 6 INTENSIFIED OFF.
*        WRITE :/ 'Error Occurred in Infotype 0021 Subtype -', FAMSA , 'USER ID:' , WA_UPLOAD-USRID1.
*
*      ENDIF.
*
*    ENDIF.


**********************************************************************************************************
*EXPORT I_UPLOAD to MEMORY ID 'Z008'.

*  ENDIF.
*  **************************************************************************************************************************

*   Call infotype 008 BDC

*  **************************************************************************************************************************

*    PERFORM UPDATE_IT008.

    clear: it_pa0008, wa_pa0008, begda , i_upload008 , wa_upload008.

*I_UPLOAD008
    clear : trfar,trfgb, trfgr.
    data: lga01_code type z6hr_ppl_2_sap-sap.

    loop at i_upload into wa_upload08 where usrid1 = wa_upload-usrid1 and lga01 is not initial
      and ( lga01 = 'I_BAS' or lga01 = 'I_CAR' or lga01 = 'I_CONV' or lga01 = 'I_EDUA' "OR LGA01 = 'I_ENTA'
          or lga01 = 'I_HRA' or lga01 = 'I_OTHA' or lga01 = 'I_SPLA')." AND MASSN = 'HIR'. "OR LGA01 = 'I_GRAT'

*      CLEAR : TRFAR,TRFGB, TRFGR.
      select single sap  from z6hr_ppl_2_sap    into trfar  where tabname = 'PA0008' and ppl_sft = wa_upload-trfar.
*      IF SY-SUBRC <> 0 . CLEAR:TRFAR. ENDIF.

      select single sap  from z6hr_ppl_2_sap    into trfgb  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgb.
*      IF SY-SUBRC <> 0 . CLEAR:TRFGB. ENDIF.

      select single sap  from z6hr_ppl_2_sap    into trfgr  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgr.
*      IF SY-SUBRC <> 0 . CLEAR:TRFGR. ENDIF.

      clear: dd, mm, yyyy.

      if wa_upload-begda is not initial.
        clear: begda, dd,mm ,yyyy.
        split wa_upload-begda at '/' into mm dd yyyy.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = dd
          importing
            output = dd.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = mm
          importing
            output = mm.

        concatenate dd '.' mm '.' yyyy into begda.

      endif.

      move begda to wa_upload008-begda.
      move wa_upload08-pernr to wa_upload008-pernr.
      wa_upload008-endda = '31.12.9999'.
      wa_upload008-preas = ''. "Reason for Changing Master Data
      move trfar to wa_upload008-trfar. "Pay scale type
      move trfgb to wa_upload008-trfgb. "Pay Scale Area
      move trfgr to wa_upload008-trfgr. "Pay Scale Group
      move trfst to wa_upload008-trfst. "Pay Scale Level

      move wa_upload08-lga01 to wa_upload008-lga01.
      move wa_upload08-bet01 to wa_upload008-bet01.
      move wa_upload08-waers to wa_upload008-waers.

      clear: lga01_code.

      select single sap from z6hr_ppl_2_sap
        into lga01_code where tabname = 'PA0008'
                                     and field = 'LGA01'
                                     and ppl_sft =  wa_upload08-lga01.

      if sy-subrc <> 0 . clear: lga01_code. endif.
      condense  lga01_code.
      wa_upload008-lga01_code = lga01_code.
      if wa_upload008-bet01 > 0.
        append wa_upload008 to i_upload008.
      endif.
*      wa_upload-massn = 'HIR'.
*      modify I_UPLOAD from wa_upload transporting massn.
    endloop.

    sort i_upload008 by lga01_code.

    data: code type i.
    code = 0.

    loop at i_upload008 into wa_upload008 where pernr = wa_upload-pernr. "USRID1.

      code = code + 1.

      move wa_upload008-pernr to wa_pa0008-pernr.
      move wa_upload008-begda to wa_pa0008-begda.
      wa_pa0008-endda = '31.12.9999'.
      wa_pa0008-preas = ''. "Reason for Changing Master Data
      move trfar to wa_pa0008-trfar. "Pay scale type
      move trfgb to wa_pa0008-trfgb. "Pay Scale Area
      move trfgr to wa_pa0008-trfgr. "Pay Scale Group
      move wa_upload008-trfst to wa_pa0008-trfst. "Pay Scale Level

      on change of wa_pa0008-pernr .
        append wa_pa0008 to it_pa0008.
      endon.

      case code."WA_UPLOAD008-LGA01.
        when 1."'I_BAS'.                                         "X 10    "1001
          move wa_upload008-lga01 to wa_pa0008-lga01.
          move wa_upload008-bet01 to wa_pa0008-bet01.
        when 2."'I_HRA'."X 5                                              "1004
          move wa_upload008-lga01 to wa_pa0008-lga02.
          move wa_upload008-bet01 to wa_pa0008-bet02.
        when 3."'I_EDUA'."X 3                                            "1005
          move wa_upload008-lga01 to wa_pa0008-lga03.
          move wa_upload008-bet01 to wa_pa0008-bet03.
        when 4."'I_CONV'.                                        "X 11   "1006
          move wa_upload008-lga01 to wa_pa0008-lga04.
          move wa_upload008-bet01 to wa_pa0008-bet04.
        when 5."'I_SPLA'."X 8                                            "1007
          move wa_upload008-lga01 to wa_pa0008-lga05.
          move wa_upload008-bet01 to wa_pa0008-bet05.
        when 6."'I_CAR'."X 2                                             "1008
          move wa_upload008-lga01 to wa_pa0008-lga06.
          move wa_upload008-bet01 to wa_pa0008-bet06.
        when 7."'I_OTHA'.                                        "X 13   "1013
          move wa_upload008-lga01 to wa_pa0008-lga07.
          move wa_upload008-bet01 to wa_pa0008-bet07.
        when 8."'I_ENTA'.                                        "X 12   "1020
          move wa_upload008-lga01 to wa_pa0008-lga08.
          move wa_upload008-bet01 to wa_pa0008-bet08.
        when 9."'I_GRAT'."X 4                                            "3617
          move wa_upload008-lga01 to wa_pa0008-lga09.
          move wa_upload008-bet01 to wa_pa0008-bet09.
      endcase.

      if wa_upload008-lga01 is not initial.
        modify it_pa0008 from wa_pa0008 index 1 transporting lga01 bet01
                                                             lga02 bet02
                                                             lga03 bet03
                                                             lga04 bet04
                                                             lga05 bet05
                                                             lga06 bet06
                                                             lga07 bet07
                                                             lga08 bet08
                                                             lga09 bet09
                                                             lga10 bet10
                                                             lga11 bet11
                                                             lga12 bet12
                                                             lga13 bet13
                                                             lga14 bet14
                                                             lga15 bet15
                                                             lga16 bet16
                                                             lga17 bet17
                                                             lga18 bet18
                                                             lga19 bet19
                                                             lga20 bet20.
      endif.

    endloop.

    export it_pa0008 from it_pa0008 to memory id 'ZHRBDC'.

    submit z6hr014c_infotype_008_upl_ppl
      with p_pernr = wa_upload-pernr                        "USRID1
      exporting list to memory
    and return.

    import ret from memory id 'ZHR_MSG'.
    if not ret[] is initial.
      loop at ret into wa_ret.        .
        if wa_ret-type = 'E'.
          format color 6 intensified off.
          write : /'ERROR',wa_ret-message ,wa_upload-usrid1.
        else.
          format color 5 intensified off.
          write : /'SUCESS',wa_ret-message ,wa_upload-usrid1.
        endif.
      endloop.
    else.
      format color 5 intensified off.
      write : /'Records SUCESSFULLY UPDATED for Infotype 0008 UserID: ', wa_upload-usrid1.
    endif.

    clear: t_msg2 , w_msg.
**********************************************************************************************************
**********************************************************************************************************
*code for infotype 0589 "0015 for I_LTA ***********************************************************************************
**********************************************************************************************************
**********************************************************************************************************
    data : p0589 like p0589." , P0589 LIKE P0589.
    data : p0015begda type p0589-begda.
    data : return like bapireturn1.
    data : key like bapipakey.
    data : returne like bapireturn1 .
    data: temp_bet01 type pa0008-bet01, "   " Wage Type Amount for Payments
          temp_bet02 type pa0008-bet01,
          temp_bet03 type pa0008-bet01,
          temp_bet04 type pa0008-bet01,
          temp_bet05 type pa0008-bet01.


    read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_LTA'  massn = 'HIR'.
    if sy-subrc = 0.
      clear : wa_upload15.
*      IF WA_UPLOAD15-BET01 > 0.
      clear: p0589 , return,key, returne , p0015begda.
*    mm/dd/yyyy

      if wa_upload-begda is not initial.
        clear: begda, dd,mm ,yyyy.
        split wa_upload-begda at '/' into mm dd yyyy.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = dd
          importing
            output = dd.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = mm
          importing
            output = mm.

        concatenate dd '.' mm '.' yyyy into begda.

      endif.

      concatenate begda+06(04) begda+03(02) begda(02) into p0015begda  .

      p0589-pernr = wa_upload-pernr.                        "USRID1 .
      p0589-begda = p0015begda.
      p0589-endda = '99991231'.
      p0589-lga01 = '1101'. "I_LTA
      p0589-lga02 = '1102'. "I_MED
      p0589-lga03 = '1116'. "I_TEL
      p0589-lga04 = '1020'. "I_ENTA
      p0589-lga05 = '1018'."'1216'. "I_FURN
      p0589-preas = '01'.
      p0589-waehi = 'INR'.

      read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_LTA' massn = 'HIR'.
      if sy-subrc = 0.
        if wa_upload15-bet01 > 0.
          temp_bet01 = wa_upload15-bet01.
          p0589-bet01 = temp_bet01.
        endif.
      endif.
      clear: wa_upload15.

      read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_MED' massn = 'HIR'.
      if sy-subrc = 0.
        if wa_upload15-bet01 > 0.
          temp_bet02 = wa_upload15-bet01.
          p0589-bet02 = temp_bet02.
        endif.
      endif.
      clear: wa_upload15.

      read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_TEL' massn = 'HIR'.
      if sy-subrc = 0.
        if wa_upload15-bet01 > 0.
          temp_bet03 = wa_upload15-bet01 * 12.
          p0589-bet03 = temp_bet03.
        endif.
      endif.
      clear: wa_upload15.

      read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_ENTA' massn = 'HIR'.
      if sy-subrc = 0.
        if wa_upload15-bet01 > 0.
          temp_bet04 = wa_upload15-bet01.
          p0589-bet04 = temp_bet04.
        endif.
      endif.
      clear: wa_upload15.

      read table i_upload into wa_upload15 with key usrid1 = wa_upload-usrid1  lga01 = 'I_FURN' massn = 'HIR'.
      if sy-subrc = 0.
        if wa_upload15-bet01 > 0.
          temp_bet05 = wa_upload15-bet01.
          p0589-bet05 = temp_bet05.
        endif.
      endif.
      clear: wa_upload15.



      call function 'BAPI_EMPLOYEE_ENQUEUE'
        exporting
          number = p0589-pernr
        importing
          return = returne.


      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty         = '0589'
          number        = p0589-pernr
          subtype       = p0589-subty
          objectid      = p0589-objps
          lockindicator = p0589-sprps
          validityend   = p0589-endda
          validitybegin = p0589-begda
          recordnumber  = p0589-seqnr
          record        = p0589
          operation     = 'INS'
          tclas         = 'A'
          dialog_mode   = '0'
        importing
          return        = return
          key           = key.

      if return is not initial.
        format color 6 intensified off.
        write :/ 'Error Occurred in Infotype 0589 , FOR' , wa_upload-usrid1.
      else.
        format color 5 intensified off.
        write :/ 'Records SUCESSFULLY UPDATED for Infotype 0589,  FOR' ,  wa_upload-usrid1.
      endif.


      call function 'BAPI_EMPLOYEE_DEQUEUE'
        exporting
          number = p0589-pernr.


    endif.

    clear wa_upload15.

*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.
**********************************************************************************************************
*  Update infotype 0015 for I_med***********************************************************************************
**********************************************************************************************************
*    READ TABLE I_UPLOAD INTO WA_UPLOAD15 WITH KEY USRID1 = WA_UPLOAD-USRID1  LGA01 = 'I_MED' MASSN = 'HIR'.
*    IF SY-SUBRC = 0.
*      IF WA_UPLOAD15-BET01 > 0.
*        CLEAR: P0589 , RETURN,KEY, RETURNE.
*
*        P0589-PERNR = WA_UPLOAD-USRID1 .
*        P0589-BEGDA = P0015BEGDA.
*        P0589-ENDDA = '99991231'.
*        P0589-LGA02 = '1102'.
*        P0589-PREAS = '01'.
*        P0589-WAEHI = WA_UPLOAD15-WAERS.
*        P0589-BET02 = WA_UPLOAD15-BET01.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR
*          IMPORTING
*            RETURN = RETURNE.
*
*
*        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*          EXPORTING
*            INFTY         = '0589'
*            NUMBER        = P0589-PERNR
*            SUBTYPE       = P0589-SUBTY
*            OBJECTID      = P0589-OBJPS
*            LOCKINDICATOR = P0589-SPRPS
*            VALIDITYEND   = P0589-ENDDA
*            VALIDITYBEGIN = P0589-BEGDA
*            RECORDNUMBER  = P0589-SEQNR
*            RECORD        = P0589
*            OPERATION     = 'INS'
*            TCLAS         = 'A'
*            DIALOG_MODE   = '0'
*          IMPORTING
*            RETURN        = RETURN
*            KEY           = KEY.
*
*        IF RETURN IS NOT INITIAL.
*          FORMAT COLOR 6 INTENSIFIED OFF.
*          WRITE :/ 'Error Occurred in Infotype 0589 Subtype - Medical Reimbursement FOR' , WA_UPLOAD-USRID1.
*        ELSE.
*          FORMAT COLOR 5 INTENSIFIED OFF.
*          WRITE :/ 'Records SUCESSFULLY UPDATED for Infotype 0589 Subtype - Medical Reimbursement FOR', WA_UPLOAD-USRID1.
*        ENDIF.
*
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR.
*      ENDIF.
*
*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.
**********************************************************************************************************
*  Update infotype 0015 for I_VIP***********************************************************************************
**********************************************************************************************************


*    DATA : P0015 LIKE P0015.
*    DATA : P0015BEGDA1 TYPE P0015-BEGDA.
*    DATA : RETURN2 LIKE BAPIRETURN1.
*    DATA : KEY2 LIKE BAPIPAKEY.
*    DATA : RETURNE2 LIKE BAPIRETURN1 .
*
*
*    READ TABLE I_UPLOAD INTO WA_UPLOAD15 WITH KEY USRID1 = WA_UPLOAD-USRID1  LGA01 = 'I_VIP' MASSN = 'HIR'.
*    IF SY-SUBRC = 0.
*
*      IF WA_UPLOAD15-BET01 > 0.
*        CLEAR: P0015 , RETURN2,KEY2, RETURNE2.
*
*        P0015-PERNR = WA_UPLOAD-USRID1 .
*        P0015-BEGDA = P0015BEGDA1.
*        P0015-ENDDA = '99991231'.
*        P0015-LGArt = '1106'.
*        P0015-PREAS = '01'.
*        P0015-WAErs = WA_UPLOAD15-WAERS.
*        P0015-BETRG = WA_UPLOAD15-BET01.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
*          EXPORTING
*            NUMBER = P0015-PERNR
*          IMPORTING
*            RETURN = RETURNE.
*
*
*        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*          EXPORTING
*            INFTY         = '0015'
*            NUMBER        = P0015-PERNR
*            SUBTYPE       = P0015-SUBTY
*            OBJECTID      = P0015-OBJPS
*            LOCKINDICATOR = P0015-SPRPS
*            VALIDITYEND   = P0015-ENDDA
*            VALIDITYBEGIN = P0015-BEGDA
*            RECORDNUMBER  = P0015-SEQNR
*            RECORD        = P0015
*            OPERATION     = 'INS'
*            TCLAS         = 'A'
*            DIALOG_MODE   = '0'
*          IMPORTING
*            RETURN        = RETURN
*            KEY           = KEY.
*
*        IF RETURN IS NOT INITIAL.
*          FORMAT COLOR 6 INTENSIFIED OFF.
*          WRITE :/ 'Error Occurred in Infotype 0015 Subtype - Performance Incentive FOR', WA_UPLOAD-USRID1.
*        ELSE.
*          FORMAT COLOR 5 INTENSIFIED OFF.
*          WRITE :/ 'Records SUCESSFULLY UPDATED for Infotype 0015 Subtype - Performance Incentive FOR' , WA_UPLOAD-USRID1.
*        ENDIF.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
*          EXPORTING
*            NUMBER = P0015-PERNR.
*      ENDIF.
*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.

**********************************************************************************************************
*  Update infotype 0014 for I_TEL***********************************************************************************
**********************************************************************************************************


    data : p0014 like p0014.
*    DATA : P0014BEGDA1 TYPE P0014-BEGDA.
    data : return3 like bapireturn1.
    data : key3 like bapipakey.
    data : returne3 like bapireturn1 .


    read table i_upload into wa_upload14 with key usrid1 = wa_upload-usrid1  lga01 = 'I_TEL' massn = 'HIR'.
    if sy-subrc = 0.

      if wa_upload14-bet01 > 0.
        clear: p0014 , return3,key3, returne3.

        p0014-pernr = wa_upload-pernr.                      "USRID1 .
        p0014-begda = p0015begda.
        p0014-endda = '99991231'.
        p0014-lgart = '1116'.
        p0014-preas = '01'.
        p0014-waers = wa_upload14-waers.
        p0014-betrg = wa_upload14-bet01.

        call function 'BAPI_EMPLOYEE_ENQUEUE'
          exporting
            number = p0014-pernr
          importing
            return = returne.


        call function 'HR_INFOTYPE_OPERATION'
          exporting
            infty         = '0014'
            number        = p0014-pernr
            subtype       = p0014-subty
            objectid      = p0014-objps
            lockindicator = p0014-sprps
            validityend   = p0014-endda
            validitybegin = p0014-begda
            recordnumber  = p0014-seqnr
            record        = p0014
            operation     = 'INS'
            tclas         = 'A'
            dialog_mode   = '0'
          importing
            return        = return
            key           = key.

        if return is not initial.
          format color 6 intensified off.
          write :/ 'Error Occurred in Infotype 0014 Subtype - Telephone Reimbursement FOR', wa_upload-usrid1.
        else.
          format color 5 intensified off.
          write :/ 'Records SUCESSFULLY UPDATED for Infotype 0014 Subtype - Telephone Reimbursement FOR' , wa_upload-usrid1.
        endif.

        call function 'BAPI_EMPLOYEE_DEQUEUE'
          exporting
            number = p0014-pernr.
      endif.
    else.
      clear wa_upload14.
    endif.



******************************
* upload montholy Medical Allowance in 14 infotype
******************************

    clear: wa_upload14.
    read table i_upload into wa_upload14 with key usrid1 = wa_upload-usrid1  lga01 = 'I_MED' massn = 'HIR'.
    if sy-subrc = 0.

      if wa_upload14-bet01 > 0.
        clear: p0014 , return3,key3, returne3.

        p0014-pernr = wa_upload-pernr.                      "USRID1 .
        p0014-begda = p0015begda.
        p0014-endda = '99991231'.
        p0014-lgart = '1102'.
        p0014-preas = '01'.
        p0014-waers = wa_upload14-waers.
        p0014-betrg = wa_upload14-bet01.

        call function 'BAPI_EMPLOYEE_ENQUEUE'
          exporting
            number = p0014-pernr
          importing
            return = returne.


        call function 'HR_INFOTYPE_OPERATION'
          exporting
            infty         = '0014'
            number        = p0014-pernr
            subtype       = p0014-subty
            objectid      = p0014-objps
            lockindicator = p0014-sprps
            validityend   = p0014-endda
            validitybegin = p0014-begda
            recordnumber  = p0014-seqnr
            record        = p0014
            operation     = 'INS'
            tclas         = 'A'
            dialog_mode   = '0'
          importing
            return        = return
            key           = key.

        if return is not initial.
          format color 6 intensified off.
          write :/ 'Error Occurred in Infotype 0014 Subtype - Medical Allowance FOR', wa_upload-usrid1.
        else.
          format color 5 intensified off.
          write :/ 'Records SUCESSFULLY UPDATED for Infotype 0014 Subtype - Medical Allowance FOR' , wa_upload-usrid1.
        endif.

        call function 'BAPI_EMPLOYEE_DEQUEUE'
          exporting
            number = p0014-pernr.
      endif.
    else.
      clear wa_upload14.
    endif.







**********************************************************************************************************
**  Update infotype 0015 for I_BON***********************************************************************************
***********************************************************************************************************
*    data: P0015 like P0015.
*
*    READ TABLE I_UPLOAD INTO WA_UPLOAD15 WITH KEY USRID1 = WA_UPLOAD-USRID1 LGA01 = 'I_BON' MASSN = 'HIR'.
*    IF SY-SUBRC = 0.
*      IF WA_UPLOAD15-BET01 > 0.
*        CLEAR: P0015 , RETURN,KEY, RETURNE.
*
*        P0015-PERNR = WA_UPLOAD-USRID1 .
*        P0015-BEGDA = P0015BEGDA.
*        P0015-ENDDA = '99991231'.
*        P0015-LGART = '1203'.
*        P0015-PREAS = '01'.
*        P0015-WAERS = WA_UPLOAD15-WAERS.
*        P0015-BETRG = WA_UPLOAD15-BET01.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
*          EXPORTING
*            NUMBER = P0015-PERNR
*          IMPORTING
*            RETURN = RETURNE.
*
*
*        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*          EXPORTING
*            INFTY         = '0015'
*            NUMBER        = P0015-PERNR
*            SUBTYPE       = P0015-SUBTY
*            OBJECTID      = P0015-OBJPS
*            LOCKINDICATOR = P0015-SPRPS
*            VALIDITYEND   = P0015-ENDDA
*            VALIDITYBEGIN = P0015-BEGDA
*            RECORDNUMBER  = P0015-SEQNR
*            RECORD        = P0015
*            OPERATION     = 'INS'
*            TCLAS         = 'A'
*            DIALOG_MODE   = '0'
*          IMPORTING
*            RETURN        = RETURN
*            KEY           = KEY.
*
*        IF RETURN IS NOT INITIAL.
*          FORMAT COLOR 6 INTENSIFIED OFF.
*          WRITE :/ 'Error Occurred in Infotype 0015 Subtype - Bonus FOR' , WA_UPLOAD-USRID1.
*        ELSE.
*          FORMAT COLOR 5 INTENSIFIED OFF.
*          WRITE :/ 'Records SUCESSFULLY UPDATED for Infotype 0015 Subtype - Bonus FOR' , WA_UPLOAD-USRID1.
*        ENDIF.
*
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
*          EXPORTING
*            NUMBER = P0015-PERNR.
*      ENDIF.
*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.
**********************************************************************************************************
**  Update infotype 0015 for I_FURN***********************************************************************************
***********************************************************************************************************
*    READ TABLE I_UPLOAD INTO WA_UPLOAD15 WITH KEY USRID1 = WA_UPLOAD-USRID1  LGA01 = 'I_FURN' MASSN = 'HIR'.
*    IF SY-SUBRC = 0.
*      IF WA_UPLOAD15-BET01 > 0.
*
*        CLEAR: P0589 , RETURN,KEY, RETURNE.
*
*        P0589-PERNR = WA_UPLOAD-USRID1 .
*        P0589-BEGDA = P0015BEGDA.
*        P0589-ENDDA = '99991231'.
*        P0589-LGA04 = '1216'.
*        P0589-PREAS = '01'.
*        P0589-WAEHI = WA_UPLOAD15-WAERS.
*        P0589-BET04 = WA_UPLOAD15-BET01.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR
*          IMPORTING
*            RETURN = RETURNE.
*
*
*
*        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*          EXPORTING
*            INFTY         = '0589'
*            NUMBER        = P0589-PERNR
*            SUBTYPE       = P0589-SUBTY
*            OBJECTID      = P0589-OBJPS
*            LOCKINDICATOR = P0589-SPRPS
*            VALIDITYEND   = P0589-ENDDA
*            VALIDITYBEGIN = P0589-BEGDA
*            RECORDNUMBER  = P0589-SEQNR
*            RECORD        = P0589
*            OPERATION     = 'INS'
*            TCLAS         = 'A'
*            DIALOG_MODE   = '0'
*          IMPORTING
*            RETURN        = RETURN
*            KEY           = KEY.
*
*        IF RETURN IS NOT INITIAL.
*          FORMAT COLOR 6 INTENSIFIED OFF.
*          WRITE :/ 'Error Occurred in Infotype 0589 Subtype - Hard Furnishing' , WA_UPLOAD-USRID1.
*        ELSE.
*          FORMAT COLOR 5 INTENSIFIED OFF.
*          WRITE :/ 'Records SUCESSFULLY UPDATED for Infotype 0589 Subtype - Hard Furnishing FOR:', WA_UPLOAD-USRID1.
*        ENDIF.
*
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR.
*      ENDIF.
*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.

**********************************************************************************************************
*  Update infotype 0589 for I_ENTA***********************************************************************************
**********************************************************************************************************
*    READ TABLE I_UPLOAD INTO WA_UPLOAD15 WITH KEY USRID1 = WA_UPLOAD-USRID1  LGA01 = 'I_ENTA' MASSN = 'HIR'.
*    IF SY-SUBRC = 0.
*      IF WA_UPLOAD15-BET01 > 0.
*        CLEAR: P0589 , RETURN,KEY, RETURNE.
*
*        P0589-PERNR = WA_UPLOAD-USRID1 .
*        P0589-BEGDA = P0015BEGDA.
*        P0589-ENDDA = '99991231'.
*        P0589-LGA04 = '1020'.
*        P0589-PREAS = '01'.
*        P0589-WAEHI = WA_UPLOAD15-WAERS.
*        P0589-BET04 = WA_UPLOAD15-BET01.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR
*          IMPORTING
*            RETURN = RETURNE.
*
*
*        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*          EXPORTING
*            INFTY         = '0589'
*            NUMBER        = P0589-PERNR
*            SUBTYPE       = P0589-SUBTY
*            OBJECTID      = P0589-OBJPS
*            LOCKINDICATOR = P0589-SPRPS
*            VALIDITYEND   = P0589-ENDDA
*            VALIDITYBEGIN = P0589-BEGDA
*            RECORDNUMBER  = P0589-SEQNR
*            RECORD        = P0589
*            OPERATION     = 'INS'
*            TCLAS         = 'A'
*            DIALOG_MODE   = '0'
*          IMPORTING
*            RETURN        = RETURN
*            KEY           = KEY.
*
*        IF RETURN IS NOT INITIAL.
*          FORMAT COLOR 6 INTENSIFIED OFF.
*          WRITE :/ 'Error Occurred in Infotype 0589 Subtype - Entertainment Allowance FOR' , WA_UPLOAD-USRID1.
*        ELSE.
*          FORMAT COLOR 5 INTENSIFIED OFF.
*          WRITE :/ 'Records SUCESSFULLY UPDATED for Infotype 0589 Subtype - Entertainment Allowance FOR', WA_UPLOAD-USRID1.
*        ENDIF.
*
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
*          EXPORTING
*            NUMBER = P0589-PERNR.
*      ENDIF.
*
*    ELSE.
*      CLEAR WA_UPLOAD15.
*    ENDIF.


**********************************************************************************************************
*    BDC fot transaction PT60 ****************************************************************************
**********************************************************************************************************
*    PERFORM PT60.

    clear: bdcdata , bdcdata[] .
    refresh bdcdata.
    clear: enddate.
    concatenate '31.12.' begda+06(04) into enddate.

    perform bdc_dynpro      using 'RPTIME00' '1000'.
    perform bdc_field       using 'BDC_CURSOR'                              'TESTOPT1'.
    perform bdc_field       using 'BDC_OKCODE'                              '=ONLI'.
    perform bdc_field       using 'PNPPERNR-LOW'                            wa_upload-usrid1."  ' 6000080'.
    perform bdc_field       using 'SCHEMA'                                  'ZM04'.
    perform bdc_field       using 'VAR_EDT'                                 'SAP&TEDT'.
    perform bdc_field       using 'BEGDATE'                                  begda."'21.01.2013'.
    perform bdc_field       using 'ENDDATE'                                  enddate."'31.12.2013'.
*    PERFORM BDC_FIELD       USING 'TESTOPT1'                                 'X'.

    perform bdc_dynpro      using 'SAPMSSY0' '0120'.
    perform bdc_field       using 'BDC_OKCODE'                              '=BACC'.

    perform bdc_dynpro      using 'SAPLSPO1' '0500'.
    perform bdc_field       using 'BDC_OKCODE'                              '=OPT1'.
*
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'TEXT_TAB1-TEXTZEILE(02)'.
    perform bdc_dynpro      using 'RPTIME00' '1000'.
    perform bdc_field       using 'BDC_OKCODE'                              '/EE'.
    perform bdc_field       using 'BDC_CURSOR'                              'PNPPERNR-LOW'.

    clear: t_msg2 , w_msg.
    if bdcdata is not initial.
      call transaction 'PT60' using bdcdata mode tran_mode update 'S'
          messages into t_msg2 .

      read table t_msg2 into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : /'Absence Quotas Uploaded SUCESSFULLY for User ID' , wa_upload-usrid1.
      else.
        format color 6 intensified off.
        write :/ 'Error Occurred in Absence Quotas  FOR USER ID:' , wa_upload-usrid1.
      endif.

    endif.

    write: /'-------------------------------------------------------------------------------------------------------------'.

  else.
    format color 6 intensified off.
    write : /'ERROR in Hiring Action for User ID', wa_upload-usrid1.
    clear: t_msg , w_msg.
  endif.

* RUN TCODE PT60 AND UPDATE LEAVE QUOTA
* RUN BDC FOR 2001 INFOTYPE PRG NAME ZHR_BDC_PA302001 FOR TEST .
* AGAIN RUN PT60 AFTER COMPLETION OF 11 MONTHS
* ZTEST_PA40_BDC for entire pa40 recording
* Z_PA30_BDC_0021_SPOUSE for infptype 21 and sub type spouce
* Z_PA30_BDC_0021_CHILD
* write code for bdc 0021 and incorporate 008 pgm logic
* CHECK FOR MULTIPLE ADDRESS

endform.                    " RUN_BDC_PA04
*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form map_data .

  clear: massn,massg,zzlocation_key,anred,gesch,gblnd,famst, anssa, state,schkz, bnksa,plans_num ,
   awart ,famsa, fasex , trfar, trfgb ,trfgr , action_type , trfst." , PERSK. PLANS_CHAR

  select single sap  from z6hr_ppl_2_sap
    into massn  where tabname = 'PA0000'
    and ppl_sft = wa_upload-massn and field = 'MASSN'.
  if sy-subrc <> 0 . clear:massn. endif.
  action_type = wa_upload-massn.
*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP
*    INTO MASSG
*    WHERE TABNAME = 'PA0000'
*    AND PPL_SFT = WA_UPLOAD-MASSG AND FIELD = 'MASSG'.
*  IF SY-SUBRC <> 0 . CLEAR:MASSG. ENDIF.

  select single massg
    from z6hr_ps_sap_act
    into massg
    where massn = massn
    and ps_massg = wa_upload-massg.
  if sy-subrc <> 0 . clear:massg. endif.

  select single sap  from z6hr_ppl_2_sap
    into zzlocation_key
    where tabname = 'PA0001'
    and ppl_sft = wa_upload-zzlocation_key and field = 'ZZLOCATION_KEY'.
  if sy-subrc <> 0 . clear:zzlocation_key. endif.
  gv_title = wa_upload-anred.
  translate  wa_upload-anred to upper case.
  select single sap  from z6hr_ppl_2_sap
    into anred  where tabname = 'PA0002' and ppl_sft = wa_upload-anred and field = 'ANRED'.
  if sy-subrc <> 0 . clear:anred. endif.

  if wa_upload-gesch is not initial.
    if wa_upload-gesch = 'M'.
      gesch = 1.
    elseif wa_upload-gesch = 'F'.
      gesch = 2.
    endif.
  endif.

  select single sap  from z6hr_ppl_2_sap
    into gblnd  where tabname = 'PA0002'
    and ppl_sft = wa_upload-gblnd and field = 'GBLND'.
  if sy-subrc <> 0 . clear:gblnd. endif.

*

*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO PERSK  WHERE TABNAME = 'PA0001' AND PPL_SFT = WA_UPLOAD-PERSK.
*  IF SY-SUBRC <> 0 . CLEAR:PERSK. ENDIF.


*  TRANSLATE  WA_UPLOAD-NATIO TO UPPER CASE.
*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO NATIO  WHERE TABNAME = 'PA0002' AND PPL_SFT = WA_UPLOAD-NATIO.
*  IF SY-SUBRC <> 0 . CLEAR:NATIO. ENDIF.

  select single sap  from z6hr_ppl_2_sap
    into famst  where tabname = 'PA0002'
    and ppl_sft = wa_upload-famst
    and field = 'FAMST'.
  if sy-subrc <> 0 . clear:famst. endif.


  select single sap  from z6hr_ppl_2_sap
    into anssa  where tabname = 'PA0006'
    and ppl_sft = wa_upload-anssa
    and field = 'ANSSA'.
  if sy-subrc <> 0 . clear:anssa. endif.

*SELECT SINGLE sap  FROM z6hr_ppl_2_sap    INTO ZZSTATE  WHERE tabname = 'PA0001' AND PPL_SFT = WA_UPLOAD-ZZSTATE.
  select single sap  from z6hr_ppl_2_sap
    into state  where tabname = 'PA0006'
    and ppl_sft = wa_upload-state
    and field = 'ZZSTATE'.
  if sy-subrc <> 0 . clear:state. endif.

  if  wa_upload-schkz is not initial .
    select single sap  from z6hr_ppl_2_sap
      into schkz  where tabname = 'PA0007'
      and ppl_sft = wa_upload-schkz
      and field = 'SCHKZ'.
  elseif  wa_upload-schkz is initial.

    schkz = 'INDOFIL'.

  endif.

  if sy-subrc <> 0 . clear:schkz. endif.

  select single sap  from z6hr_ppl_2_sap
    into bnksa  where tabname = 'PA0009'
    and ppl_sft = wa_upload-bnksa
    and field = 'BNKSA'.
  if sy-subrc <> 0 . clear:bnksa. endif.

*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO ZLSCH  WHERE TABNAME = 'PA0009' AND PPL_SFT = WA_UPLOAD-ZLSCH.
*  IF SY-SUBRC <> 0 . CLEAR:ZLSCH. ENDIF.

*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO BANKS  WHERE TABNAME = 'PA0009' AND PPL_SFT = WA_UPLOAD-BANKS.
*  IF SY-SUBRC <> 0 . CLEAR:BANKS. ENDIF.

*  SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP
*    INTO PLANS_CHAR
*    WHERE TABNAME = 'PA0001'
*    AND PPL_SFT = WA_UPLOAD-PLANS
*    AND FIELD = 'PLANS'.

  select single plans from z6hr_ps_sap_pos into plans_num
    where pernr = wa_upload-pernr
    and ps_plans = wa_upload-plans.

  if sy-subrc <> 0 . clear: plans_num. endif."PLANS_CHAR ,


  select single sap  from z6hr_ppl_2_sap
    into awart  where tabname = 'PA2001'
    and ppl_sft = wa_upload-awart
    and field = 'AWART'.
  if sy-subrc <> 0 . clear:awart. endif.


  select single sap  from z6hr_ppl_2_sap
    into trfst
    where tabname = 'PA0008'
    and field = 'TRFST'
    and ppl_sft = wa_upload-trfst.
  if sy-subrc <> 0 .
    clear:trfst.
  else.
    if trfst = 'MT'.
      clear: trfst.
    endif .
  endif.



  if wa_upload-mstbr is not initial.
    first = wa_upload-mstbr(02).
    if first = '60'.
      clear ex_mstbr.
      select single pernr from pa0000 into ex_mstbr where pernr = wa_upload-mstbr+02(05).
      if sy-subrc = 0.
        wa_upload-mstbr = ex_mstbr.

      endif.
    endif.
  endif.
*LGA01 TYPE PA0008-LGA01."CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008




*MASSN(03)," TYPE PA0000-MASSN,"(2)(CHAR)  Action Type                XXXX Mapped in table
*MASSG(03)," TYPE PA0000-MASSG,"(2)(Char)  Reason for Action          XXXX  Mapped in table
*ZZLOCATION_KEY(11)," TYPE PA0001-ZZLOCATION_KEY,"(3)(CHAR)  Location  key  XXXX Mapped in table
*ANRED(03)," TYPE PA0002-ANRED,")(CHAR)  Initials                           XXXX  T522G mapped
*GESCH TYPE PA0002-GESCH,"(1)(CHAR)  Gender Key                            XXXX IF  M = 1 IF F = 2 " by logic hard code
*GBLND TYPE PA0002-GBLND,"(3) (CHAR)  Country of Birth                     XXXX T005 IF LANDK = IND SELECT LAND1 SPRAS = EN " by logic hard code
*NATIO TYPE PA0002-NATIO,"(3)(CHAR)  Nationality                           XXXX T005" by logic hard code
*FAMST TYPE PA0002-FAMST,"(1)(CHAR)  Marital Status Key                    XXXX " by logic hard code
*  0  Single
*  1  Marr.
*  2  Wid.
*  3  Div.
*ANSSA TYPE PA0006-ANSSA,"(4)(CHAR)  Address Record Type                   XXXX NNED TO MAP IN TABLE T591A DONE FOR 2 ENTRIES IN ird
*ZZSTATE TYPE PA0001-STATE,"(3)(CHAR)  Region (State, Province, County)     XXXX NEED TO MAP IN TABLE DONE
*STATE TYPE PA0006-STATE,"(3)(CHAR)  Region (State, Province, County)     XXXX NEED TO MAP IN TABLE DONE IN IRD NEED TO DO IN QA
*SCHKZ(11)," TYPE PA0007-SCHKZ,"(8)(CHAR)  Work Schedule Rule               XXXX NEED TO MAP IN TABLE DONE
*BNKSA TYPE PA0009-BNKSA,"(4)(CHAR)  Type of Bank Details Record           XXXX t591a table need to map DONE IN IRD
*ZLSCH TYPE PA0009-ZLSCH,"(1)(CHAR)  Payment Method                        XXXX T042Z table need to map Done in IRD
*BANKS TYPE PA0009-BANKS,"(3)(CHAR)  Bank Country key                      XXXX done in IRD
*PLANS
*AWART
*TRFAR TYPE PA0008-TRFAR,"CHAR(02)    "Pay scale type                      XXXX                INFOTYPE 008
*TRFGB TYPE PA0008-TRFGB,"CHAR(02)    "Pay Scale Area                      XXXX                INFOTYPE 008
*TRFGR TYPE PA0008-TRFGR,"CHAR(08)   " Pay Scale Group                     XXXX                INFOTYPE 008
*TRFST TYPE PA0008-TRFST,"CHAR(02)   " Pay Scale Level                     XXXX                INFOTYPE 008
*LGA01 TYPE PA0008-LGA01."CHAR(04)   " Wage Type                           XXXX                INFOTYPE 008


endform.                    " MAP_DATA
*&---------------------------------------------------------------------*
*&      Form  TERMINATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form termination .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.
  zzbegda = begda.
  clear: dd, mm, yyyy.

  if wa_upload-gbdat is not initial.
    clear: gbdat, dd,mm ,yyyy.
    split wa_upload-gbdat at '/' into mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into gbdat.

  endif.

  perform bdc_dynpro      using 'SAPMP50A'          '2000'.
  perform bdc_field       using 'BDC_CURSOR'        'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'        '/00'.
  perform bdc_field       using 'RP50G-PERNR'       ''.

  perform bdc_dynpro      using 'SAPMP50A'          '2000'.
  perform bdc_field       using 'BDC_CURSOR'        'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'        '/00'.
  perform bdc_field       using 'RP50G-PERNR'       wa_upload-pernr."USRID1."''.
  perform bdc_field       using 'RP50G-SELEC(10)'   'X'.
  perform bdc_field       using 'BDC_SUBSCR'        'SAPMP50A                                0800SUBSCR_HEADER'.

  perform bdc_dynpro      using 'SAPMP50A'          '2000'.
  perform bdc_field       using 'BDC_CURSOR'        'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'        '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'       wa_upload-pernr."USRID1."''.
  perform bdc_field       using 'BDC_SUBSCR'        '/1PAPAXX/HDR_23006A                     0100SUBSCR_HEADER'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-MASSG'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda." '01.02.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. " 'I7'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '01'.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0001-BEGDA'                             begda." '01.02.2013'.
  perform bdc_field       using 'P0001-ENDDA'                             '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-BTRTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. "'AN01'.
*  ENDIF.

  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.
*  IF WA_UPLOAD-ABKRS IS NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs."'IN'.
*  ENDIF.
*  IF PLANS_NUM IS NOT INITIAL.
  perform bdc_field       using 'P0001-PLANS'                             '99999999'."PLANS_NUM."
*  ENDIF.

*perform bdc_field       using 'P0001-STELL'                             '50001402'.


  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh."'50000215'.
*  ENDIF.

  perform bdc_field       using 'BDC_SUBSCR'        'MP000100                                0100SUB0001'.
  perform bdc_field       using 'BDC_SUBSCR'        'ZP000100                                0200SUBSCREEN_T582C'.

*  PERFORM BDC_FIELD       USING 'P0001-ZZSTATE'      STATE.
*  PERFORM BDC_FIELD       USING 'P0001-ZZLOCATION_KEY ' ZZLOCATION_KEY.
*

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0001-BEGDA'                              begda."'01.02.2013'.
  perform bdc_field       using 'P0001-ENDDA'                              '31.12.9999'.
  if wa_upload-btrtl is not initial.
    perform bdc_field       using 'P0001-BTRTL'                              wa_upload-btrtl. "'AN01'.
  endif.
  if wa_upload-abkrs is not initial.
    perform bdc_field       using 'P0001-ABKRS'                              wa_upload-abkrs."'IN'.
  endif.
*  IF PLANS_NUM IS NOT INITIAL.
  perform bdc_field       using 'P0001-PLANS'                              '99999999'."PLANS_NUM.
*  ENDIF.

*  PERFORM BDC_FIELD       USING 'P0001-STELL'                              '50001402'.
  if wa_upload-orgeh is not initial.
    perform bdc_field       using 'P0001-ORGEH'                               wa_upload-orgeh."'50000215'.
  endif.

  perform bdc_field       using 'BDC_SUBSCR'        'MP000100                                0100SUB0001'.
  perform bdc_field       using 'BDC_SUBSCR'        'ZP000100                                0200SUBSCREEN_T582C'.


  perform bdc_dynpro      using 'MP010500' '3000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0105-BEGDA(01)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=DLIM'.
  perform bdc_field       using 'RP50M-BEGDA'                              begda."'01.02.2013'.
  perform bdc_field       using 'RP50M-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'RP50M-SUBTY'                              '0001'.
  perform bdc_field       using 'RP50M-ABGRD'                              begda."'01.02.2013'.
  perform bdc_field       using 'RP50M-PAGEA'                              '  1'.
  perform bdc_field       using 'RP50M-SELE2(01)'                              'X'.
  perform bdc_field       using 'BDC_SUBSCR'        'SAPMP50A_CE                             0100SUBSCREEN_EMPL'.

  perform bdc_dynpro      using 'MP010500' '3000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0105-BEGDA(01)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=DLIM'.
  perform bdc_field       using 'RP50M-BEGDA'                              begda."'01.02.2013'.
  perform bdc_field       using 'RP50M-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'RP50M-SUBTY'                              '0010'.
  perform bdc_field       using 'RP50M-ABGRD'                              begda."'01.02.2013'.
  perform bdc_field       using 'RP50M-PAGEA'                              '  1'.
  perform bdc_field       using 'RP50M-SELE2(01)'                              'X'.
  perform bdc_field       using 'BDC_SUBSCR'        'SAPMP50A_CE                             0100SUBSCREEN_EMPL'.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

****************************Code for recording in DEV.


*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'        'RP50G-PERNR'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              ''.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'T529T-MNTXT(10)'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=PICK'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                             WA_UPLOAD-USRID1." '6000140'.
*  PERFORM BDC_FIELD       USING 'RP50G-SELEC(10)'                              'X'.
*  PERFORM BDC_DYNPRO      USING 'MP000000' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0000-MASSG'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              'UPD'.
*  PERFORM BDC_FIELD       USING 'P0000-BEGDA'                             BEGDA." '01.02.2013'.
*  PERFORM BDC_FIELD       USING 'P0000-MASSN'                             MASSN. " 'I7'.
*  PERFORM BDC_FIELD       USING 'P0000-MASSG'                             MASSG. " '01'.
*  PERFORM BDC_DYNPRO      USING 'MP000100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0001-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'P0001-BEGDA'                             BEGDA." '01.02.2013'.
*  PERFORM BDC_FIELD       USING 'P0001-ENDDA'                             '31.12.9999'.
*
*  IF WA_UPLOAD-BTRTL IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-BTRTL'                             WA_UPLOAD-BTRTL. "'AN01'.
*  ENDIF.
*  IF WA_UPLOAD-ABKRS IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-ABKRS'                             WA_UPLOAD-ABKRS."'IN'.
*  ENDIF.
*  IF PLANS_NUM IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-PLANS'                             PLANS_NUM."'99999999'.
*  ENDIF.
*
**perform bdc_field       using 'P0001-STELL'                             '50001402'.
*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-ORGEH'                             WA_UPLOAD-ORGEH."'50000215'.
*  ENDIF.
*  PERFORM BDC_DYNPRO      USING 'MP000100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0001-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=UPD'.
*  PERFORM BDC_FIELD       USING 'P0001-BEGDA'                              BEGDA."'01.02.2013'.
*  PERFORM BDC_FIELD       USING 'P0001-ENDDA'                              '31.12.9999'.
*  IF WA_UPLOAD-BTRTL IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-BTRTL'                              WA_UPLOAD-BTRTL. "'AN01'.
*  ENDIF.
*  IF WA_UPLOAD-ABKRS IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-ABKRS'                              WA_UPLOAD-ABKRS."'IN'.
*  ENDIF.
*  IF PLANS_NUM IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-PLANS'                              PLANS_NUM."'99999999'.
*  ENDIF.
*
**  PERFORM BDC_FIELD       USING 'P0001-STELL'                              '50001402'.
*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
*    PERFORM BDC_FIELD       USING 'P0001-ORGEH'                               WA_UPLOAD-ORGEH."'50000215'.
*  ENDIF.
*
*  PERFORM BDC_DYNPRO      USING 'MP010500' '3000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0105-BEGDA(01)'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=DLIM'.
*  PERFORM BDC_FIELD       USING 'RP50M-BEGDA'                              BEGDA."'01.02.2013'.
*  PERFORM BDC_FIELD       USING 'RP50M-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'RP50M-SUBTY'                              '0001'.
*  PERFORM BDC_FIELD       USING 'RP50M-ABGRD'                              BEGDA."'01.02.2013'.
*  PERFORM BDC_FIELD       USING 'RP50M-PAGEA'                              '  1'.
*  PERFORM BDC_FIELD       USING 'RP50M-SELE2(01)'                              'X'.
*  PERFORM BDC_DYNPRO      USING 'MP010500' '3000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0105-BEGDA(01)'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=DLIM'.
*  PERFORM BDC_FIELD       USING 'RP50M-BEGDA'                              BEGDA."'01.02.2013'.
*  PERFORM BDC_FIELD       USING 'RP50M-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'RP50M-SUBTY'                              '0010'.
*  PERFORM BDC_FIELD       USING 'RP50M-ABGRD'                              BEGDA."'01.02.2013'.
*  PERFORM BDC_FIELD       USING 'RP50M-PAGEA'                              '  1'.
*  PERFORM BDC_FIELD       USING 'RP50M-SELE2(01)'                              'X'.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/EBCK'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .
  endif.

*  FORMAT COLOR 3 INTENSIFIED OFF.
*  WRITE : / 'Action:', ACTION_TYPE.

  loop at t_msg into w_msg where msgtyp = 'E'.
    condense w_msg-msgv1 no-gaps.
    format color 6 intensified off.
    condense wa_upload-pernr no-gaps.
    condense w_msg-msgv1 no-gaps.
    write : / 'ERROR:', w_msg-msgv1 ,',in Separation ACTION  ' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
  endloop.

  read table t_msg into w_msg with key msgtyp = 'E'.
  if sy-subrc <> 0.
    format color 5 intensified off.
    write : / text-005 , wa_upload-pernr."USRID1 ."'Separation Process Complete for Employee:', WA_UPLOAD-USRID1.
*  ELSE.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR:' , W_MSG-MSGV1 ,'For:' , WA_UPLOAD-pernr."USRID1 ."'.ERROR
  endif.
  clear: t_msg , w_msg.



endform.                    " TERMINATION
*&---------------------------------------------------------------------*
*&      Form  CONFIRMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

form confirmation .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).
*  DATA: P01BEGDA(10).
*  DATA: PA0001BEGDA TYPE SY-DATUM.
*  DATA: PA0001DATE TYPE SY-DATUM.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

  endif.

  clear: dd, mm, yyyy.

*  CALL FUNCTION 'OIL_GET_NEXT_MONTH'
*    EXPORTING
*      I_DATE = PA0001DATE
*    IMPORTING
*      E_DATE = PA0001BEGDA.
*
*  CONCATENATE  '01.' PA0001BEGDA+04(02) '.' PA0001BEGDA(04) INTO P01BEGDA.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1." '6000144'.
  perform bdc_field       using 'RP50G-SELEC(03)'                              'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'PSPAR-PERSG'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda." '01.08.2012'.
  perform bdc_field       using 'P0000-MASSN'                             massn. " 'I3'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '01'.
  if wa_upload-plans is not initial.
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-werks is initial.
    select single werks
      from pa0001 into wa_upload-werks
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-WERKS IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. " 'IN01'.
*  ENDIF.

  if wa_upload-persg is initial.
    select single persg
      from pa0001 into wa_upload-persg
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-PERSG IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSG'                             wa_upload-persg." 'M'.
*  ENDIF.


  if wa_upload-persk is initial.
    select single persk
      from pa0001 into wa_upload-persk
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-PERSK IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSK'                             wa_upload-persk. " 'M8'.
*  ENDIF.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.

*  CALL FUNCTION 'OIL_GET_NEXT_MONTH'
*    EXPORTING
*      I_DATE        = begda
*   IMPORTING
*     E_DATE        = PA0001BEGDA
*            .
  perform bdc_field       using 'P0001-BEGDA'                             begda." '01.08.2012'.
  perform bdc_field       using 'P0001-ENDDA'                              '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. " 'AN01'.

  if wa_upload-kostl is initial.
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  perform bdc_field       using 'P0001-KOSTL'                             wa_upload-kostl. " 'AN01'.

  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*    IF WA_UPLOAD-ABKRS IS NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs." 'IN'.
*    ENDIF.



  if wa_upload-plans is not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*    IF WA_UPLOAD-ORGEH IS NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh."." '99999999'.
*    ENDIF.

  if wa_upload-state is initial.
    select single zzstate
      from pa0001 into state
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*    IF WA_UPLOAD-STATE IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZSTATE'                           state. "   '13'.
*    ENDIF.

  if wa_upload-zzlocation_key is initial.
    select single zzlocation_key
      from pa0001 into zzlocation_key
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*    IF WA_UPLOAD-ZZLOCATION_KEY IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZLOCATION_KEY'                    zzlocation_key. "  '013'.
*    ENDIF.

  if wa_upload-schkz is initial.
    select single schkz
      from pa0007 into schkz
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0007-SCHKZ'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0007-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0007-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'    schkz."'INDOFIL'.   " Work Schedule Rule


*    IF WA_UPLOAD-SCHKZ IS NOT INITIAL.
  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '01.08.2012'.
  perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'                             schkz."'INDOFIL'.
*  PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'. " default not in file system will take auto
*  PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.""  not in file system will take auto
*  PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
*  PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.
*    ENDIF.

  perform bdc_dynpro      using 'SAPLRPBS' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '=ESC'.

  perform bdc_dynpro      using 'MP200000' '2251'.
  perform bdc_field       using 'BDC_OKCODE'                              '/ENXT'.
  perform bdc_field       using 'BDC_CURSOR'                              'P2006-KTART(01)'.


  perform bdc_dynpro      using 'SAPLSPO1' '0200'.
  perform bdc_field       using 'BDC_OKCODE'                              '=YES'.

  perform bdc_dynpro      using 'MP001600' '2006'.
  perform bdc_field       using 'BDC_OKCODE'                              '/ENXT'.
  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.

    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      condense wa_upload-pernr no-gaps.
      condense w_msg-msgv1 no-gaps.
      write : / 'ERROR:', w_msg-msgv1 ,',In Confirmation ACTION For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.
*W_MSG-MSGV1

    read table t_msg into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : / text-009 , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
*  ELSEIF SY-SUBRC = 0.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR: In Confirmation ACTION For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
    endif.

    perform update_it008.
*    PERFORM UPDATE_IT589.
    perform pt60.
  endif.
  clear: t_msg , w_msg.
endform.                    " CONFIRMATION
*&---------------------------------------------------------------------*
*&      Form  PROMOTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form promotion .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             ''.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1. "'6000144'.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'T529T-MNTXT(04)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1. "'6000144'.
  perform bdc_field       using 'RP50G-SELEC(07)'                         'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'PSPAR-PERSK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0000-BEGDA'                             begda."'01.01.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. "'I6'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '03'.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num." '99999999'.
  endif.


*  IF WA_UPLOAD-ORGEH IS INITIAL.
*    SELECT SINGLE ORGEH
*      FROM PA0001 INTO WA_UPLOAD-ORGEH
*      WHERE PERNR = WA_UPLOAD-PERNR
*      AND ENDDA = '99991231'.
*  ENDIF.

  if wa_upload-werks is initial.
    select single werks
      from pa0001 into wa_upload-werks
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-WERKS IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. " 'IN01'.
*  ENDIF.

  if wa_upload-persg is initial.
    select single persg
      from pa0001 into wa_upload-persg
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-PERSG IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSG'                             wa_upload-persg." 'M'.
*  ENDIF.

  if wa_upload-persk is initial.
    select single persk
      from pa0001 into wa_upload-persk
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-PERSK IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSK'                             wa_upload-persk. " 'M7'.
*  ENDIF.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda."'01.01.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. "'I6'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '03'.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-werks is not initial.
    perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. " 'IN01'.
  endif.

  if wa_upload-persg is not initial.
    perform bdc_field       using 'PSPAR-PERSG'                             wa_upload-persg." 'M'.
  endif.

  if wa_upload-persk is not initial.
    perform bdc_field       using 'PSPAR-PERSK'                             wa_upload-persk. " 'M7'.
  endif.


  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0001-BEGDA'                             begda."  '01.01.2013'.
  perform bdc_field       using 'P0001-ENDDA'                             '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-BTRTL IS  NOT INITIAL.
  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. "'AN01'.
*  ENDIF.


  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ABKRS IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs."'IN'.
*  ENDIF.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '99999999'.
  endif.


  if wa_upload-mstbr is not initial.
    perform bdc_field       using 'P0001-MSTBR'                             wa_upload-mstbr." '6001049'.
  endif.

  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh." '50000084'.
*  ENDIF.
*  PERFORM BDC_FIELD       USING 'P0001-ZZSTATE'                           STATE."   '13'.
*  PERFORM BDC_FIELD       USING 'P0001-ZZLOCATION_KEY'                    ZZLOCATION_KEY." '013'.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda."  '01.01.2013'.
  perform bdc_field       using 'P0001-ENDDA'                             '31.12.9999'.

  if wa_upload-btrtl is not initial.
    perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. "'AN01'.
  endif.

  if wa_upload-abkrs is not initial.
    perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs."'IN'.
  endif.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-mstbr is not initial.
    perform bdc_field       using 'P0001-MSTBR'                             wa_upload-mstbr." '6001049'.
  endif.

  if wa_upload-orgeh is not initial.
    perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh." '50000084'.
  endif.

  if wa_upload-schkz is initial.
    select single schkz
      from pa0007 into schkz
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.



  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '01.01.2013'.
  perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'                             schkz."'INDOFIL'.
*  PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
*  PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.
*  PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
*  PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.

  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '01.01.2013'.
  perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'                             schkz."'INDOFIL'.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.

    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR: in Promotion ACTION  For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.

    read table t_msg into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : / text-010 , wa_upload-pernr."USRID1 ."'PROMOTION Action Successful for User ID', WA_UPLOAD-USRID1.
*  ELSEIF SY-SUBRC = 0.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR: in Promotion ACTION  For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
      perform update_it017.

    endif.

*    PERFORM UPDATE_IT008.
*    PERFORM UPDATE_IT589.
  endif.
  clear: t_msg , w_msg.


endform.                    " PROMOTION
*&---------------------------------------------------------------------*
*&      Form  INTERCOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form intercom .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).
  data: p01begda(10).
  data: pa0001begda type sy-datum.
  data: pa0001date type sy-datum.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.

    concatenate yyyy mm dd into pa0001date.
  endif.

  clear: dd, mm, yyyy.
  call function 'OIL_GET_NEXT_MONTH'
    exporting
      i_date = pa0001date
    importing
      e_date = pa0001begda.

  concatenate  '01.' pa0001begda+04(02) '.' pa0001begda(04) into p01begda.


  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'T529T-MNTXT(05)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1." '6000142'.
  perform bdc_field       using 'RP50G-SELEC(05)'                              'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-MASSG'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. " 'I4'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '01'.
  if wa_upload-plans is  not initial.
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-werks is initial.
    select single werks
      from pa0001 into wa_upload-werks
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-WERKS IS  NOT INITIAL.
  perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. " 'IN01'.
*  ENDIF.


  if wa_upload-persg is initial.
    select single persg
      from pa0001 into wa_upload-persg
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-PERSG IS  NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSG'                             wa_upload-persg." 'M'.
*  ENDIF.


  if wa_upload-persk is initial.
    select single persk
      from pa0001 into wa_upload-persk
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-PERSK IS  NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSK'                             wa_upload-persk. " 'M8'.
*  ENDIF.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0001-ENDDA'                              '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-BTRTL IS  NOT INITIAL.
  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. " 'AN01'.
*  ENDIF.


  if wa_upload-kostl is initial.
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-KOSTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-KOSTL'                             wa_upload-kostl. " ''.
*  ENDIF.

  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-ABKRS IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs."  'IN'.
*  ENDIF.

  if wa_upload-plans is  not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-mstbr is  not initial.
    perform bdc_field       using 'P0001-MSTBR'                             wa_upload-mstbr." .
  endif.

  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ORGEH IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh." '99999999'.
*  ENDIF.

  if wa_upload-state is initial.
    select single zzstate
      from pa0001 into state
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-STATE IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ZZSTATE'                           state." '99999999'.
*  ENDIF.

  if wa_upload-zzlocation_key is initial.
    select single zzlocation_key
      from pa0001 into zzlocation_key
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ZZLOCATION_KEY IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ZZLOCATION_KEY'                    zzlocation_key." '99999999'.
*  ENDIF.

  if wa_upload-schkz is initial.
    select single schkz
      from pa0007 into schkz
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-SCHKZ IS NOT INITIAL.
  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0007-ENDDA'                             '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'                             schkz." 'INDOFIL'.
*  PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
*  PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.
*  PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
*  PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.
*  ENDIF.


  perform bdc_dynpro      using 'MP000600' '2005'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0006-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0006-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
  if wa_upload-locat is not initial.
    perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat."  'test comm inter comtest c'.
  endif.
  if wa_upload-stras is not initial.
    perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'test'.
  endif.
  if wa_upload-pstlz is not initial.
    perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '452154'.
  endif.

  if wa_upload-ort01 is not initial.
    perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'test comm inter com'.
  endif.

  if wa_upload-land1 is not initial.
    perform bdc_field       using 'P0006-LAND1'                             'IN'.
  endif.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.



    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      condense wa_upload-pernr no-gaps.
      condense w_msg-msgv1 no-gaps.

      write : / 'ERROR:',w_msg-msgv1 ,',in Transfer -Inter Company ACTION For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.

    read table t_msg into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : / text-011 , wa_upload-pernr."USRID1 ."'Transfer -Inter Company Successful for User ID', WA_UPLOAD-USRID1.
*  ELSEIF SY-SUBRC = 0.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR: in Transfer -Inter Company ACTION For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
    endif.

    perform update_it008.
*    PERFORM UPDATE_IT589.
  endif.

  clear: t_msg , w_msg.

endform.                    " INTERCOM
*&---------------------------------------------------------------------*
*&      Form  INTRACOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form intracom .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'T529T-MNTXT(06)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1. "'6000115'.
  perform bdc_field       using 'RP50G-SELEC(06)'                         'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-MASSG'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda."'02.02.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. "'I5'.
  perform bdc_field       using 'P0000-MASSG'                             massg. "'01'.
  if wa_upload-plans is not initial .
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num."'50000279'.
  endif.


  if wa_upload-werks is initial.
    select single werks
      from pa0001 into wa_upload-werks
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  if wa_upload-persg is initial.
    select single persg
      from pa0001 into wa_upload-persg
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  if wa_upload-persk is initial.
    select single persk
      from pa0001 into wa_upload-persk
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.



*  IF WA_UPLOAD-WERKS IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. "'IN01'.
*  ENDIF.
*  IF WA_UPLOAD-PERSG IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSG'                            wa_upload-persg." 'M'.
*  ENDIF.
*  IF WA_UPLOAD-PERSK IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSK'                            wa_upload-persk. " 'M2'.
*  ENDIF.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda."'02.02.2013'.
  perform bdc_field       using 'P0001-ENDDA'                             '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.
  if wa_upload-kostl is initial.
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.
  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-BTRTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. " 'AN01'.
*  ENDIF.

*  IF WA_UPLOAD-KOSTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-KOSTL'                             wa_upload-kostl. " ''.
*  ENDIF.

*  IF WA_UPLOAD-ABKRS IS  NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                              wa_upload-abkrs."'IN'.
*  ENDIF.

  if wa_upload-mstbr is not initial.
    perform bdc_field       using 'P0001-MSTBR'                            wa_upload-mstbr." '6001049'.
  endif.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '50000279'.
  endif.

  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh." '50000253'.
*  ENDIF.

  if wa_upload-state is initial.
    select single zzstate
      from pa0001 into state
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.



*  IF WA_UPLOAD-STATE IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZSTATE'                           state. "   '13'.
*  ENDIF.


  if wa_upload-zzlocation_key is initial.
    select single zzlocation_key
      from pa0001 into zzlocation_key
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ZZLOCATION_KEY IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZLOCATION_KEY'                    zzlocation_key. "   '013'.
*  ENDIF.

  if wa_upload-schkz is initial.
    select single schkz
      from pa0007 into schkz
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-SCHKZ IS NOT INITIAL.

  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.


  perform bdc_field       using 'P0007-SCHKZ'                             schkz." 'INDOFIL'.
*  ENDIF.

*  PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
*  PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.
*  PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
*  PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.


  perform bdc_dynpro      using 'MP000600' '2005'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0006-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0006-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
  if wa_upload-locat is not initial.
    perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat. " 'A 302,'.
  endif.

  if wa_upload-stras is not initial.
    perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'Sunil Nagar, nandivli Road'.
  endif.

  if wa_upload-pstlz is not initial.
    perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '421201'.
  endif.

  if wa_upload-ort01 is not initial.
    perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'Dombivli (East)'.
  endif.
  if wa_upload-state is not initial.
    perform bdc_field       using 'P0006-STATE'                             state. " '13'.
  endif.
  if wa_upload-land1 is not initial.
    perform bdc_field       using 'P0006-LAND1'                             'IN'.
  endif.

  perform bdc_dynpro      using 'MP000600' '2005'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0006-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0006-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
  if wa_upload-locat is not initial.
    perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat. " 'A 302,'.
  endif.

  if wa_upload-stras is not initial.
    perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'Sunil Nagar, nandivli Road'.
  endif.

  if wa_upload-pstlz is not initial.
    perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '421201'.
  endif.
  if wa_upload-ort01 is not initial.
    perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'Dombivli (East)'.
  endif.
  if wa_upload-state is not initial.
    perform bdc_field       using 'P0006-STATE'                             state. " '13'.
  endif.
  if wa_upload-land1 is not initial.
    perform bdc_field       using 'P0006-LAND1'                             'IN'.
  endif.
  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.


  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.


    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR: in Transfer -Intra Company ACTION For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.

    read table t_msg into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : / text-012 , wa_upload-pernr."USRID1 ."'Transfer -Intra Company Successful for User ID', WA_UPLOAD-USRID1.
*  ELSEIF SY-SUBRC = 0.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR: in Transfer -Intra Company ACTION For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
    endif.

*    PERFORM UPDATE_IT008.
    perform update_it008_inter.
*    PERFORM UPDATE_IT589.
    perform update_po13.

  endif.

  clear: t_msg , w_msg.

endform.                    " INTRACOM
*&---------------------------------------------------------------------*
*&      Form  TRAINEE_ABSORPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form trainee_absorption .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.

  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'T529T-MNTXT(03)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."USRID1." '6000143'.
  perform bdc_field       using 'RP50G-SELEC(04)'                         'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'PSPAR-PERSK'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0000-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0000-MASSN'                             massn. " 'I8'.
  perform bdc_field       using 'P0000-MASSG'                             massg. " '01'.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'PSPAR-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-werks is initial.
    select single werks
      from pa0001 into wa_upload-werks
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-WERKS IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-WERKS'                             wa_upload-werks. " 'IN01'.
*  ENDIF.

  if wa_upload-persg is initial.
    select single persg
      from pa0001 into wa_upload-persg
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-PERSG IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSG'                             wa_upload-persg." 'T'.
*  ENDIF.

  if wa_upload-persk is initial.
    select single persk
      from pa0001 into wa_upload-persk
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-PERSK IS NOT INITIAL.
  perform bdc_field       using 'PSPAR-PERSK'                             wa_upload-persk. " 'T2'.
*  ENDIF.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0001-ENDDA'                              '31.12.9999'.

  if wa_upload-btrtl is initial.
    select single btrtl
      from pa0001 into wa_upload-btrtl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-BTRTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-BTRTL'                             wa_upload-btrtl. " 'AN01'.
*  ENDIF.

  if wa_upload-kostl is initial.
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-KOSTL IS NOT INITIAL.
  perform bdc_field       using 'P0001-KOSTL'                             wa_upload-kostl. " 'AN01'.
*  ENDIF.

  if wa_upload-abkrs is initial.
    select single abkrs
      from pa0001 into wa_upload-abkrs
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ABKRS IS NOT INITIAL.
  perform bdc_field       using 'P0001-ABKRS'                             wa_upload-abkrs." 'IN'.
*  ENDIF.

  if wa_upload-plans is not initial.
    perform bdc_field       using 'P0001-PLANS'                             plans_num." '99999999'.
  endif.

  if wa_upload-orgeh is initial.
    select single orgeh
      from pa0001 into wa_upload-orgeh
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.


*  IF WA_UPLOAD-ORGEH IS NOT INITIAL.
  perform bdc_field       using 'P0001-ORGEH'                             wa_upload-orgeh." '99999999'.
*  ENDIF.


  if wa_upload-state is initial.
    select single zzstate
      from pa0001 into state
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-STATE IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZSTATE'                           state. "   '13'.
*  ENDIF.

  if wa_upload-zzlocation_key is initial.
    select single zzlocation_key
      from pa0001 into zzlocation_key
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

*  IF WA_UPLOAD-ZZLOCATION_KEY IS NOT INITIAL.
  perform bdc_field       using 'P0001-ZZLOCATION_KEY'                    zzlocation_key. "  '013'.
*  ENDIF.

  if wa_upload-schkz is initial.
    select single schkz
      from pa0007 into schkz
      where pernr = wa_upload-pernr
      and endda = '99991231'.
  endif.

  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'     'P0007-SCHKZ'.
  perform bdc_field       using 'BDC_OKCODE'     '/00'.
  perform bdc_field       using 'P0007-BEGDA'    begda."'01.04.2012'.
  perform bdc_field       using 'P0007-ENDDA'    '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'    schkz."'INDOFIL'.   " Work Schedule Rule


  perform bdc_dynpro      using 'MP000700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0007-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0007-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0007-SCHKZ'                             schkz.": 'INDOFIL'.


*  IF WA_UPLOAD-SCHKZ IS NOT INITIAL .

*  ENDIF.

**  PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
**  PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.
**  PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
**  PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.

  perform bdc_dynpro      using 'MP001600' '2006'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0016-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
  perform bdc_field       using 'P0016-BEGDA'                             begda." '02.02.2013'.
  perform bdc_field       using 'P0016-ENDDA'                              '31.12.9999'.
  data: temp_persg type pa0001-persg, temp_endda type pa0001-endda.
  clear: temp_persg.
  select single persg endda from pa0001 into (temp_persg , temp_endda )
    where pernr = wa_upload-pernr
    and endda = '99991231'.
  if sy-subrc = 0.
    if temp_persg = 'T'.
      perform bdc_field       using 'P0016-CTTYP'                             'I2'. "Contract Type
    elseif temp_persg = 'N'.
      perform bdc_field       using 'P0016-CTTYP'                             'I1'. "Contract Type
    endif.
  endif.

  clear : temp_persg.

*  PERFORM BDC_DYNPRO      USING 'MP001900' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/ENXT'.
  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action:', ACTION_TYPE.




    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR: in Trainee Absorption ACTION For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.

    read table t_msg into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : / text-013 , wa_upload-pernr."USRID1 ."'Trainee Absorption is Successful for User ID', WA_UPLOAD-USRID1.
*  ELSEIF SY-SUBRC = 0.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    WRITE : / 'ERROR: in Trainee Absorption ACTION For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
    endif.

    perform update_it008.
    perform update_it589.
    perform update_it017.

    perform pt60.
  endif.
  clear: t_msg , w_msg.


endform.                    " TRAINEE_ABSORPTION
*&---------------------------------------------------------------------*
*&      Form  CHK_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form chk_position .

  data: temp_plans type pa0001-plans.

  clear: temp_plans.

*  SELECT SINGLE SAP FROM Z6HR_PPL_2_SAP INTO TEMP_PLANS WHERE  PPL_SFT = WA_UPLOAD-PLANS AND TABNAME = 'PA0001'.
  select single plans
    from z6hr_ps_sap_pos
    into temp_plans
    where pernr = wa_upload-pernr
    and ps_plans = wa_upload-plans.

  if sy-subrc <> 0.
*    IF WA_UPLOAD-MASSN = 'HIR'.
    perform create_position.
*    PERFORM CREATE_NEW_POS.
*    ELSE.
*
*    ENDIF.
  else.
    if wa_upload-massn = 'XEC'.
      delete from z6hr_ps_sap_pos where pernr = wa_upload-pernr and plans = temp_plans and ps_plans = wa_upload-plans.
      commit work.
      perform create_position.
    endif.
  endif.


endform.                    " CHK_POSITION
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXISTING_EMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_existing_emp .
  data: first(02), second(05).

  first = wa_upload-pernr(02).

  if first = '60'.
    clear ex_pernr.
    select single pernr from pa0000 into ex_pernr where pernr = wa_upload-pernr+02(05).
    if sy-subrc = 0.
      wa_upload-pernr = ex_pernr.
    endif.
  endif.

endform.                    " CHECK_EXISTING_EMP
*&---------------------------------------------------------------------*
*&      Form  UPDATE_2001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_2001 .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), enddate(10) , fgbdt(10).

  if  wa_upload-begda2001 is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda2001 at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  if  wa_upload-endda2001 is not initial.
    split wa_upload-endda2001 at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into enddate.
  endif.


  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000002'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000002'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_field       using 'RP50G-BEGDA'                             begda." '14.02.2013'.
  perform bdc_field       using 'RP50G-ENDDA'                             enddate." '15.02.2013'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
  perform bdc_field       using 'RP50G-CHOIC'                             '2001'." 'Absences'.
  perform bdc_field       using 'RP50G-SUBTY'                             awart." 'CL'.

  perform bdc_dynpro      using 'MP200000' '2001'.
  perform bdc_field       using 'BDC_CURSOR'                              'P2001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P2001-BEGDA'                             begda." '14.02.2013'.
  perform bdc_field       using 'P2001-ENDDA'                             enddate." '15.02.2013'.

  perform bdc_dynpro      using 'MP200000' '2001'.
  perform bdc_field       using 'BDC_CURSOR'                              'P2001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P2001-BEGDA'                             begda." '14.02.2013'.
  perform bdc_field       using 'P2001-ENDDA'                             enddate." '15.02.2013'.
*perform bdc_field       using 'P2001-STDAZ'                              '   16.00'.

  clear: t_msg.
  if bdcdata is not initial.
    call transaction 'PA30' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action: Absence Data'.

    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.

      write : / 'ERROR in Absence Data Updation ' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.

    if t_msg is not initial.
      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / text-014 , wa_upload-pernr."USRID1 ."'Absence data updated Successfully for User ID', WA_UPLOAD-USRID1.
      endif.
      clear: t_msg , w_msg.
    endif.
  endif.

  clear: bdcdata , bdcdata[].
  refresh bdcdata.

endform.                    " UPDATE_2001
*&---------------------------------------------------------------------*
*&      Form  UPDATE_0416
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_0416 .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), enddate(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.


  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                              ''.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '6011663'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
  perform bdc_field       using 'RP50G-CHOIC'                             '0416'." 'Time Quota Compensation'.
  perform bdc_field       using 'RP50G-SUBTY'                              '1000'.
  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '6011663'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_field       using 'RP50G-CHOIC'                              'Time Quota Compensation'.
  perform bdc_field       using 'RP50G-SUBTY'                              '1000'.
  perform bdc_dynpro      using 'MP041600' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0416-NUMBR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0416-BEGDA'                             begda." '14.02.2013'.
  perform bdc_field       using 'P0416-NUMBR'                             wa_upload-numbr." '           4'.
  perform bdc_dynpro      using 'MP041600' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0416-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0416-BEGDA'                              begda."'14.02.2013'.
  perform bdc_field       using 'P0416-NUMBR'                             wa_upload-numbr."'     4.00000'.
*perform bdc_transaction using 'PA30'.

  clear: t_msg.
  if bdcdata is not initial.
    call transaction 'PA30' using bdcdata mode tran_mode update 'S'
      messages into t_msg .

*    FORMAT COLOR 3 INTENSIFIED OFF.
*    WRITE : / 'Action: Leave Encashment'.

    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.

      write : / 'ERROR in Leave Encashment Data Updation ' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.
    if t_msg is not initial.
      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / text-015 , wa_upload-pernr."USRID1 ."'Leave Encashment updated Successfully for User ID', WA_UPLOAD-USRID1.
      endif.
    endif.
    clear: t_msg , w_msg.
  endif.

  clear: bdcdata , bdcdata[].
  refresh bdcdata.
endform.                    " UPDATE_0416
*&---------------------------------------------------------------------*
*&      Form  CREATE_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_position .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10).
  data: para type tpara-paramid value 'PON',
        prog type sy-repid.
  data:  ctumode like ctu_params-dismode.


  clear:bdcdata, bdcdata[].
  refresh: bdcdata.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             ''.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(01)'.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'MARKFELD(01)'                              'X'.


  perform bdc_dynpro      using 'MP100000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1000-STEXT'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1000-BEGDA'                             begda." '14.02.2013'.
  perform bdc_field       using 'P1000-ENDDA'                             '31.12.9999'.
  perform bdc_field       using 'P1000-SHORT'                             wa_upload-description2(12)." 'TESt MANG'.
  perform bdc_field       using 'P1000-STEXT'                             wa_upload-description2." 'MANAGER'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.

  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PO13' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .

  endif.

  get parameter id para field prog.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.


  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             prog."''.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.


  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             prog." '50002032'.
  perform bdc_field       using 'BDC_CURSOR'                              'PPHDR-BEGDA'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda." '18.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'MARKFELD(02)'                              'X'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                             begda." '18.02.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '003'.
  perform bdc_field       using 'P1001-SCLAS'                              'O'.
  perform bdc_field       using 'P1001-SOBID'                             wa_upload-orgeh."'50000096'.
  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                             begda." '18.02.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '003'.
  perform bdc_field       using 'P1001-SCLAS'                              'O'.
  perform bdc_field       using 'P1001-SOBID'                             wa_upload-orgeh." '50000096'.
  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
*perform bdc_field       using 'PM0D1-SEARK'                              '50002032'.

  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PO13' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .

  endif.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.
  data: pos_pernr type pa0001-pernr,
        pos_prog  type pa0001-plans.

  pos_pernr = wa_upload-pernr.
  pos_prog = prog.

  wa_z6hr_ps_sap_pos-pernr = pos_pernr.
  wa_z6hr_ps_sap_pos-plans = pos_prog.
  wa_z6hr_ps_sap_pos-ps_plans = wa_upload-plans.
  wa_z6hr_ps_sap_pos-descr = wa_upload-description2.

*  IF WA_UPLOAD-MASSN = 'RDG'.
*    DELETE FROM Z6HR_PS_SAP_POS WHERE PERNR = WA_UPLOAD-PERNR.
*    COMMIT WORK.
*    INSERT Z6HR_PS_SAP_POS FROM WA_Z6HR_PS_SAP_POS.
*    plans_num = PROG.
*  ELSE.
  insert z6hr_ps_sap_pos from wa_z6hr_ps_sap_pos.
*  ENDIF.
  commit work.

  clear: bdcdata , bdcdata[] , wa_z6hr_ps_sap_pos , pos_pernr , pos_prog.
  refresh bdcdata.

endform.                    " CREATE_POSITION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_it008 .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  clear: it_pa0008, wa_pa0008, begda , i_upload008 , wa_upload008.

*I_UPLOAD008

  data: lga01_code type z6hr_ppl_2_sap-sap.
  clear : trfar,trfgb, trfgr.
  loop at i_upload into wa_upload08 where usrid1 = wa_upload-usrid1 and lga01 is not initial
    and ( lga01 = 'I_BAS' or lga01 = 'I_CAR' or lga01 = 'I_CONV' or lga01 = 'I_EDUA' "OR LGA01 = 'I_ENTA'
        or lga01 = 'I_HRA' or lga01 = 'I_OTHA' or lga01 = 'I_SPLA') and massn = action_type."'TAB'."'TAB'. "OR LGA01 = 'I_GRAT'

*    CLEAR : TRFAR,TRFGB, TRFGR.
    if wa_upload-trfar  is initial .
      select single trfar from pa0008 into wa_upload-trfar
      where pernr = test_pernr
      and endda =  '99991231'.
      if sy-subrc <> 0 . clear : wa_upload-trfar. endif.
      trfar = wa_upload-trfar.
    else.
      select single sap  from z6hr_ppl_2_sap    into trfar  where tabname = 'PA0008' and ppl_sft = wa_upload-trfar.
*      IF SY-SUBRC <> 0 . CLEAR:TRFAR. ENDIF.
    endif.

    if wa_upload-trfgb  is initial .

      select single trfgb from pa0008 into  wa_upload-trfgb
        where pernr = test_pernr
        and endda =  '99991231'.
      if sy-subrc <> 0 . clear : wa_upload-trfgb . endif.
      trfgb = wa_upload-trfgb.
    else.
      select single sap  from z6hr_ppl_2_sap    into trfgb  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgb.
*      IF SY-SUBRC <> 0 . CLEAR:TRFGB. ENDIF.
    endif.

*    SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO TRFAR  WHERE TABNAME = 'PA0008' AND PPL_SFT = WA_UPLOAD-TRFAR.
*    IF SY-SUBRC <> 0 . CLEAR:TRFAR. ENDIF.
*
*    SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO TRFGB  WHERE TABNAME = 'PA0008' AND PPL_SFT = WA_UPLOAD-TRFGB.
*    IF SY-SUBRC <> 0 . CLEAR:TRFGB. ENDIF.

    select single sap  from z6hr_ppl_2_sap    into trfgr  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgr.
    if sy-subrc <> 0 . clear:trfgr. endif.

    select single sap  from z6hr_ppl_2_sap
    into trfst
    where tabname = 'PA0008'
    and field = 'TRFST'
    and ppl_sft = wa_upload-trfst.
    if sy-subrc <> 0 .
      clear:trfst.
    else.
      if trfst = 'MT'.
        clear: trfst.
      endif .
    endif.

    clear: dd, mm, yyyy.

    if wa_upload-begda is not initial.
      clear: begda, dd,mm ,yyyy.
      split wa_upload-begda at '/' into mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into begda.

    endif.

    move begda to wa_upload008-begda.
    move wa_upload08-pernr to wa_upload008-pernr.
    wa_upload008-endda = '31.12.9999'.
    wa_upload008-preas = ''. "Reason for Changing Master Data
    move trfar to wa_upload008-trfar. "Pay scale type
    move trfgb to wa_upload008-trfgb. "Pay Scale Area
    move trfgr to wa_upload008-trfgr. "Pay Scale Group
    move trfst to wa_upload008-trfst. "Pay Scale Level

    move wa_upload08-lga01 to wa_upload008-lga01.
    move wa_upload08-bet01 to wa_upload008-bet01.
    move wa_upload08-waers to wa_upload008-waers.

    clear: lga01_code.

    select single sap from z6hr_ppl_2_sap
      into lga01_code where tabname = 'PA0008'
                                   and field = 'LGA01'
                                   and ppl_sft =  wa_upload08-lga01.

    if sy-subrc <> 0 . clear: lga01_code. endif.
    condense  lga01_code.
    wa_upload008-lga01_code = lga01_code.
    if wa_upload008-bet01 > 0.
      append wa_upload008 to i_upload008.
    endif.
  endloop.

  sort i_upload008 by lga01_code.

  data: code type i.
  code = 0.

  loop at i_upload008 into wa_upload008 where pernr = wa_upload-pernr. "USRID1.

    code = code + 1.

    move wa_upload008-pernr to wa_pa0008-pernr.
    move wa_upload008-begda to wa_pa0008-begda.
    wa_pa0008-endda = '31.12.9999'.
    wa_pa0008-preas = ''. "Reason for Changing Master Data
    move trfar to wa_pa0008-trfar. "Pay scale type
    move trfgb to wa_pa0008-trfgb. "Pay Scale Area
    move trfgr to wa_pa0008-trfgr. "Pay Scale Group
    move wa_upload008-trfst to wa_pa0008-trfst. "Pay Scale Level

*      ON CHANGE OF WA_UPLOAD008-PERNR .
    if sy-tabix = 1.
      append wa_pa0008 to it_pa0008.
    endif.
*      ENDON.

    case code."WA_UPLOAD008-LGA01.
      when 1."'I_BAS'.                                         "X 10    "1001
        move wa_upload008-lga01 to wa_pa0008-lga01.
        move wa_upload008-bet01 to wa_pa0008-bet01.
      when 2."'I_HRA'."X 5                                              "1004
        move wa_upload008-lga01 to wa_pa0008-lga02.
        move wa_upload008-bet01 to wa_pa0008-bet02.
      when 3."'I_EDUA'."X 3                                            "1005
        move wa_upload008-lga01 to wa_pa0008-lga03.
        move wa_upload008-bet01 to wa_pa0008-bet03.
      when 4."'I_CONV'.                                        "X 11   "1006
        move wa_upload008-lga01 to wa_pa0008-lga04.
        move wa_upload008-bet01 to wa_pa0008-bet04.
      when 5."'I_SPLA'."X 8                                            "1007
        move wa_upload008-lga01 to wa_pa0008-lga05.
        move wa_upload008-bet01 to wa_pa0008-bet05.
      when 6."'I_CAR'."X 2                                             "1008
        move wa_upload008-lga01 to wa_pa0008-lga06.
        move wa_upload008-bet01 to wa_pa0008-bet06.
      when 7."'I_OTHA'.                                        "X 13   "1013
        move wa_upload008-lga01 to wa_pa0008-lga07.
        move wa_upload008-bet01 to wa_pa0008-bet07.
      when 8."'I_ENTA'.                                        "X 12   "1020
        move wa_upload008-lga01 to wa_pa0008-lga08.
        move wa_upload008-bet01 to wa_pa0008-bet08.
      when 9."'I_GRAT'."X 4                                            "3617
        move wa_upload008-lga01 to wa_pa0008-lga09.
        move wa_upload008-bet01 to wa_pa0008-bet09.
    endcase.

    if wa_upload008-lga01 is not initial.
      modify it_pa0008 from wa_pa0008 index 1 transporting lga01 bet01
                                                           lga02 bet02
                                                           lga03 bet03
                                                           lga04 bet04
                                                           lga05 bet05
                                                           lga06 bet06
                                                           lga07 bet07
                                                           lga08 bet08
                                                           lga09 bet09
                                                           lga10 bet10
                                                           lga11 bet11
                                                           lga12 bet12
                                                           lga13 bet13
                                                           lga14 bet14
                                                           lga15 bet15
                                                           lga16 bet16
                                                           lga17 bet17
                                                           lga18 bet18
                                                           lga19 bet19
                                                           lga20 bet20.
    endif.

  endloop.

  if it_pa0008 is not initial.
    export it_pa0008 from it_pa0008 to memory id 'ZHRBDC'.

    submit z6hr014c_infotype_008_upl_ppl
      with p_pernr = wa_upload-pernr                        "USRID1
      exporting list to memory
    and return.

    import ret from memory id 'ZHR_MSG'.
    if not ret[] is initial.
      loop at ret into wa_ret.        .
        if wa_ret-type = 'E'.
          format color 6 intensified off.
          write : /'ERROR',wa_ret-message ,wa_upload-pernr. "USRID1.
        else.
          format color 5 intensified off.
          write : /'SUCESS',wa_ret-message ,wa_upload-pernr. "USRID1.
        endif.
      endloop.
    else.
      format color 5 intensified off.
      write : /'Records SUCESSFULLY UPDATED for Infotype 0008 UserID: ', wa_upload-pernr. "USRID1.
    endif.
  else.
    format color 6 intensified off.
    write : /'ERROR:in updating Infotype 008 For: ',wa_upload-pernr. "USRID1.
  endif.
  clear: t_msg2 , w_msg.
endform.                    " UPDATE_IT008
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT589
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_it589 .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).


  data : p0589 like p0589." , P0589 LIKE P0589.
  data : p0015begda type p0589-begda.
  data : return like bapireturn1.
  data : key like bapipakey.
  data : returne like bapireturn1 .
  data: temp_bet01 type pa0008-bet01, "   " Wage Type Amount for Payments
        temp_bet02 type pa0008-bet01,
        temp_bet03 type pa0008-bet01,
        temp_bet04 type pa0008-bet01,
        temp_bet05 type pa0008-bet01.


  read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_LTA'  massn = action_type."'HIR'.
  if sy-subrc = 0.
    clear : wa_upload15.
*      IF WA_UPLOAD15-BET01 > 0.
    clear: p0589 , return,key, returne , p0015begda.
*    mm/dd/yyyy

    if wa_upload-begda is not initial.
      clear: begda, dd,mm ,yyyy.
      split wa_upload-begda at '/' into mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into begda.

    endif.

    concatenate begda+06(04) begda+03(02) begda(02) into p0015begda  .

    p0589-pernr = wa_upload-pernr.                          "USRID1 .
    p0589-begda = p0015begda.
    p0589-endda = '99991231'.
    p0589-lga01 = '1101'. "I_LTA
    p0589-lga02 = '1102'. "I_MED
    p0589-lga03 = '1116'. "I_TEL
    p0589-lga04 = '1020'. "I_ENTA
    p0589-lga05 = '1018'."'1216'. "I_FURN
    p0589-preas = '01'.
    p0589-waehi = 'INR'.

    read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_LTA' massn = action_type." 'HIR'.
    if sy-subrc = 0.
      if  wa_upload15-bet01 > 0.
        temp_bet01 = wa_upload15-bet01.
        p0589-bet01 = temp_bet01.
      endif.
    endif.
    clear: wa_upload15.

    read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_MED' massn = action_type."'HIR'.
    if sy-subrc = 0.
      if  wa_upload15-bet01 > 0.
        temp_bet02 = wa_upload15-bet01.
        p0589-bet02 = temp_bet02.
      endif.
    endif.
    clear: wa_upload15.

    read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_TEL' massn = action_type."'HIR'.
    if sy-subrc = 0.
      if  wa_upload15-bet01 > 0.
        temp_bet03 = wa_upload15-bet01 * 12.
        p0589-bet03 = temp_bet03.
      endif.
    endif.
    clear: wa_upload15.

    read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_ENTA' massn = action_type."'HIR'.
    if sy-subrc = 0.
      if  wa_upload15-bet01 > 0.
        temp_bet04 = wa_upload15-bet01.
        p0589-bet04 = temp_bet04.
      endif.
    endif.
    clear: wa_upload15.

    read table i_upload into wa_upload15 with key pernr = wa_upload-pernr  lga01 = 'I_FURN' massn = action_type."'HIR'.
    if sy-subrc = 0.
      if  wa_upload15-bet01 > 0.
        temp_bet05 = wa_upload15-bet01.
        p0589-bet05 = temp_bet05.
      endif.
    endif.
    clear: wa_upload15.



    call function 'BAPI_EMPLOYEE_ENQUEUE'
      exporting
        number = p0589-pernr
      importing
        return = returne.


    call function 'HR_INFOTYPE_OPERATION'
      exporting
        infty         = '0589'
        number        = p0589-pernr
        subtype       = p0589-subty
        objectid      = p0589-objps
        lockindicator = p0589-sprps
        validityend   = p0589-endda
        validitybegin = p0589-begda
        recordnumber  = p0589-seqnr
        record        = p0589
        operation     = 'INS'
        tclas         = 'A'
        dialog_mode   = '0'
      importing
        return        = return
        key           = key.

    if return is not initial.
      format color 6 intensified off.
      write :/ 'Error Occurred in Infotype 0589 , FOR' , wa_upload-pernr. "usrid1 .
    else.
      format color 5 intensified off.
      format color 5 intensified off.
      write :/ 'Records SUCESSFULLY UPDATED for Infotype 0589,  FOR' ,  wa_upload-pernr. "USRID1 .
    endif.


    call function 'BAPI_EMPLOYEE_DEQUEUE'
      exporting
        number = p0589-pernr.


  endif.

  clear wa_upload15.

**********************************************************************************************************
*  Update infotype 0014 for I_TEL***********************************************************************************
**********************************************************************************************************


  data : p0014 like p0014.
*    DATA : P0014BEGDA1 TYPE P0014-BEGDA.
  data : return3 like bapireturn1.
  data : key3 like bapipakey.
  data : returne3 like bapireturn1 .


  read table i_upload into wa_upload14 with key pernr = wa_upload-pernr  lga01 = 'I_TEL' massn = action_type."'HIR'.
  if sy-subrc = 0.

    if wa_upload14-bet01 > 0.
      clear: p0014 , return3,key3, returne3.

      p0014-pernr = wa_upload-pernr.                        "USRID1 .
      p0014-begda = p0015begda.
      p0014-endda = '99991231'.
      p0014-lgart = '1116'.
      p0014-preas = '01'.
      p0014-waers = wa_upload14-waers.
      p0014-betrg = wa_upload14-bet01.

      call function 'BAPI_EMPLOYEE_ENQUEUE'
        exporting
          number = p0014-pernr
        importing
          return = returne.


      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty         = '0014'
          number        = p0014-pernr
          subtype       = p0014-subty
          objectid      = p0014-objps
          lockindicator = p0014-sprps
          validityend   = p0014-endda
          validitybegin = p0014-begda
          recordnumber  = p0014-seqnr
          record        = p0014
          operation     = 'INS'
          tclas         = 'A'
          dialog_mode   = '0'
        importing
          return        = return
          key           = key.

      if return is not initial.
        format color 6 intensified off.
        write :/ 'Error Occurred in Infotype 0014 Subtype - Telephone Reimbursement FOR', wa_upload-pernr. "USRID1.
      else.
        format color 5 intensified off.
        write :/ 'Records SUCESSFULLY UPDATED for Infotype 0014 Subtype - Telephone Reimbursement FOR' , wa_upload-pernr. "USRID1.
      endif.

      call function 'BAPI_EMPLOYEE_DEQUEUE'
        exporting
          number = p0014-pernr.
    endif.
  else.
    clear wa_upload14.
  endif.

endform.                    " UPDATE_IT589
*&---------------------------------------------------------------------*
*&      Form  PT60
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pt60 .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  clear: bdcdata , bdcdata[] .
  refresh bdcdata.
  clear: enddate.
  concatenate '31.12.' begda+06(04) into enddate.

  perform bdc_dynpro      using 'RPTIME00' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'TESTOPT1'.
  perform bdc_field       using 'BDC_OKCODE'                              '=ONLI'.
  perform bdc_field       using 'PNPPERNR-LOW'                            wa_upload-pernr."USRID1."  ' 6000080'.
  perform bdc_field       using 'SCHEMA'                                  'ZM04'.
  perform bdc_field       using 'VAR_EDT'                                 'SAP&TEDT'.
  perform bdc_field       using 'BEGDATE'                                  begda."'21.01.2013'.
  perform bdc_field       using 'ENDDATE'                                  enddate."'31.12.2013'.
*    PERFORM BDC_FIELD       USING 'TESTOPT1'                                 'X'.

  perform bdc_dynpro      using 'SAPMSSY0' '0120'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACC'.

  perform bdc_dynpro      using 'SAPLSPO1' '0500'.
  perform bdc_field       using 'BDC_OKCODE'                              '=OPT1'.
*
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'TEXT_TAB1-TEXTZEILE(02)'.
  perform bdc_dynpro      using 'RPTIME00' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EE'.
  perform bdc_field       using 'BDC_CURSOR'                              'PNPPERNR-LOW'.

  clear: t_msg2 , w_msg.
  if bdcdata is not initial.
    call transaction 'PT60' using bdcdata mode tran_mode update 'S'
        messages into t_msg2 .

    read table t_msg2 into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0.
      format color 5 intensified off.
      write : /'Absence Quotas Uploaded SUCESSFULLY for User ID' , wa_upload-pernr. "USRID1.
    else.
      format color 6 intensified off.
      write :/ 'Error Occurred in Absence Quotas  FOR USER ID:' , wa_upload-pernr. "USRID1.
    endif.

  endif.

  write: /'-------------------------------------------------------------------------------------------------------------'.

endform.                                                    " PT60
*&---------------------------------------------------------------------*
*&      Form  OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form other .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  data : p0007 like p0007.
  data : p0017 like p0017.
  data : p0105 like p0105.

  data : p0007begda type p0007-begda.
  data : p0105begda type p0105-begda.
  data : return like bapireturn1.
  data : key like bapipakey.
  data : returne like bapireturn1 .

*  DATA: P01BEGDA(10).
*  DATA: PA0001BEGDA TYPE SY-DATUM.
*  DATA: PA0001DATE TYPE SY-DATUM.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

  endif.
  clear: p0007begda , p0105begda.
  concatenate begda+06(04) begda+03(02) begda(02) into p0007begda  .
  concatenate begda+06(04) begda+03(02) begda(02) into p0105begda  .
  clear: dd, mm, yyyy.

  if  wa_upload-gbdat is not initial.
    clear: gbdat, dd,mm ,yyyy.
    split wa_upload-gbdat at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into gbdat.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

  endif.

  clear: dd, mm, yyyy.

**  **************************************************************************************************
** if changes made in IT 001
**  **************************************************************************************************
*  IF WA_UPLOAD-BTRTL IS NOT INITIAL OR "(4)(CHAR)  Personnel Subarea
*      WA_UPLOAD-PERSG IS NOT INITIAL OR"(1)(CHAR)  Employee Group
*      WA_UPLOAD-PERSK IS NOT INITIAL OR"(2)(CHAR)  Employee Subgroup
*      WA_UPLOAD-ABKRS IS NOT INITIAL OR"(2)(char)  Payroll Area
*      WA_UPLOAD-KOSTL IS NOT INITIAL OR"(10)(CHAR)  Cost Center
*      WA_UPLOAD-ORGEH IS NOT INITIAL OR"(8)(NUMC)  Organizational Unit
*      WA_UPLOAD-DESCRIPTION1 IS NOT INITIAL OR
*      WA_UPLOAD-PLANS IS NOT INITIAL OR" TYPE PA0001-PLANS,"(8)(NUMC)  Job(Position)                    XXXX
*      WA_UPLOAD-DESCRIPTION2 IS NOT INITIAL OR
*      WA_UPLOAD-MSTBR IS NOT INITIAL OR"(8)(CHAR)  Supervisor Area
**      WA_UPLOAD-STATE IS NOT INITIAL OR " region
*      WA_UPLOAD-ZZLOCATION_KEY IS NOT INITIAL ." TYPE PA0001-ZZLOCATION_KEY,"(3)(CHAR)  Location  key  XXXX
*
*    PERFORM BDC_DYNPRO      USING 'SAPMP50A'        '1000'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'      '/00'.
*    PERFORM BDC_FIELD       USING 'RP50G-PERNR'     WA_UPLOAD-PERNR. "'7996'.
*    PERFORM BDC_FIELD       USING 'RP50G-TIMR6'     'X'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'      'RP50G-CHOIC'.
*    PERFORM BDC_FIELD       USING 'RP50G-CHOIC'     '001'.
*
*    PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=MOD'.
*    PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              WA_UPLOAD-PERNR. " '7996'.
*    PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*    PERFORM BDC_FIELD       USING 'RP50G-CHOIC'                              'Organizational Assignment'.
*
*    PERFORM BDC_DYNPRO      USING 'MP000100'       '2000'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0001-ORGEH'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/00'.
*    PERFORM BDC_FIELD       USING 'P0001-BEGDA'    BEGDA."'01.04.2012'.
*    PERFORM BDC_FIELD       USING 'P0001-ENDDA'    '31.12.9999'.
*    IF WA_UPLOAD-BTRTL IS INITIAL.
*      SELECT SINGLE BTRTL
*        FROM PA0001 INTO WA_UPLOAD-BTRTL
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*    PERFORM BDC_FIELD       USING 'P0001-BTRTL'    WA_UPLOAD-BTRTL."'AN01'.     "  Personnel Subarea  ????????? 9
*
*    IF WA_UPLOAD-KOSTL IS INITIAL.
*      SELECT SINGLE KOSTL
*        FROM PA0001 INTO WA_UPLOAD-KOSTL
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*    PERFORM BDC_FIELD       USING 'P0001-KOSTL'    WA_UPLOAD-KOSTL."
*
*    IF WA_UPLOAD-ABKRS IS INITIAL.
*      SELECT SINGLE ABKRS
*        FROM PA0001 INTO WA_UPLOAD-ABKRS
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*    PERFORM BDC_FIELD       USING 'P0001-ABKRS'    WA_UPLOAD-ABKRS."'IN'.       "Payroll Area                10
*
*    IF WA_UPLOAD-PLANS IS NOT INITIAL.
*      PERFORM BDC_FIELD       USING 'P0001-PLANS'    PLANS_NUM."'99999999'.
*    ENDIF.
*
*    IF WA_UPLOAD-MSTBR IS INITIAL.
*      SELECT SINGLE MSTBR
*        FROM PA0001 INTO WA_UPLOAD-MSTBR
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*
*
*    PERFORM BDC_FIELD       USING 'P0001-MSTBR'    WA_UPLOAD-MSTBR."'6001278'.  "Supervisor Area             11
*
*    IF WA_UPLOAD-ORGEH IS INITIAL.
*      SELECT SINGLE ORGEH
*        FROM PA0001 INTO WA_UPLOAD-ORGEH
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*
*    PERFORM BDC_FIELD       USING 'P0001-ORGEH'    WA_UPLOAD-ORGEH."'50000083'. "Organizational Unit         12
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0001-ZZLOCATION_KEY'.
*
*    IF WA_UPLOAD-STATE IS INITIAL.
*      SELECT SINGLE ZZSTATE
*        FROM PA0001 INTO STATE
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*    PERFORM BDC_FIELD       USING 'P0001-ZZSTATE'  STATE."'13'.       "Region (State, Province, County)   13   XXXX
*
*
*    IF WA_UPLOAD-ZZLOCATION_KEY IS INITIAL.
*      SELECT SINGLE ZZLOCATION_KEY
*        FROM PA0001 INTO ZZLOCATION_KEY
*        WHERE PERNR = WA_UPLOAD-PERNR
*        AND ENDDA = '99991231'.
*    ENDIF.
*
*    PERFORM BDC_FIELD       USING 'P0001-ZZLOCATION_KEY' ZZLOCATION_KEY."'013'. " Location  key          14         XXXX
*
*    PERFORM BDC_DYNPRO      USING 'MP000100'       '2000'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0001-BEGDA'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '=UPD'.
*    PERFORM BDC_FIELD       USING 'P0001-BEGDA'    BEGDA."'01.04.2012'.
*    PERFORM BDC_FIELD       USING 'P0001-ENDDA'    '31.12.9999'.
*    PERFORM BDC_FIELD       USING 'P0001-BTRTL'    WA_UPLOAD-BTRTL. "'AN01'.
*    PERFORM BDC_FIELD       USING 'P0001-KOSTL'    WA_UPLOAD-KOSTL."
*    PERFORM BDC_FIELD       USING 'P0001-ABKRS'    WA_UPLOAD-ABKRS."'IN'.
*    IF WA_UPLOAD-PLANS IS NOT INITIAL.
*      PERFORM BDC_FIELD       USING 'P0001-PLANS'    PLANS_NUM."'99999999'.
*    ENDIF.
*    PERFORM BDC_FIELD       USING 'P0001-MSTBR'    WA_UPLOAD-MSTBR. "'6001278'.
*    PERFORM BDC_FIELD       USING 'P0001-ORGEH'    WA_UPLOAD-ORGEH."'50000083'.
*
*    PERFORM BDC_FIELD       USING 'P0001-ZZSTATE'  STATE.   "'13'.
*    PERFORM BDC_FIELD       USING 'P0001-ZZLOCATION_KEY'  ZZLOCATION_KEY. " '013'.
*
*    PERFORM BDC_DYNPRO      USING 'MP000100'       '2000'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/EBCK'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0001-BEGDA'.
*
*    IF BDCDATA IS NOT INITIAL.
*      CALL TRANSACTION 'PA30' USING BDCDATA MODE TRAN_MODE UPDATE 'S'
*        MESSAGES INTO T_MSG .
*
*      FORMAT COLOR 3 INTENSIFIED OFF.
*      WRITE : / 'IT 001 change Action for Employee:', WA_UPLOAD-PERNR.
*
*      LOOP AT T_MSG INTO W_MSG WHERE MSGTYP = 'E'.
*        FORMAT COLOR 6 INTENSIFIED OFF.
*        CONDENSE WA_UPLOAD-PERNR NO-GAPS.
*        CONDENSE W_MSG-MSGV1 NO-GAPS.
*        WRITE : / 'ERROR:', W_MSG-MSGV1 ,',In IT 001 Change Action For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
*      ENDLOOP.
*
*      READ TABLE T_MSG INTO W_MSG WITH KEY MSGTYP = 'E'.
*      IF SY-SUBRC <> 0.
*        FORMAT COLOR 5 INTENSIFIED OFF.
*        WRITE : / 'Changes Made Successfully for IT 001 :' , WA_UPLOAD-PERNR."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.
*  CLEAR: T_MSG , W_MSG.
*  CLEAR: BDCDATA , BDCDATA[] .
*  REFRESH BDCDATA.

*  **************************************************************************************************
* if changes made in IT 002
*  **************************************************************************************************

  if wa_upload-anred is not initial or " TYPE PA0002-ANRED,")(CHAR)  Initials                           XXXX
  wa_upload-nachn is not initial or"(40)(CHAR)  Last Name
  wa_upload-midnm is not initial or"(40)(CHAR)  Second Name
  wa_upload-vorna is not initial or"(40)(CHAR)  First Name
  wa_upload-gesch is not initial or"(1)(CHAR)  Gender Key                            XXXX
*  WA_UPLOAD-GBDAT IS NOT INITIAL OR" TYPE PA0002-GBDAT,"( 8)(DATS)  Date of Birth
  wa_upload-gblnd is not initial or"(3) (CHAR)  Country of Birth                     XXXX
  wa_upload-gbort is not initial or"(40)(CHAR)  Birthplace
*  WA_UPLOAD-NATIO IS NOT INITIAL OR"(3)(CHAR)  Nationality                           XXXX
  wa_upload-famst is not initial."(1)(CHAR)  Marital Status Key                    XXXX

    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'RP50G-PERNR'                              ''.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.

    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000020'.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.

    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'RP50G-PERNR'                              wa_upload-pernr."   '7000020'.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.
    perform bdc_field       using 'BDC_CURSOR'                              'RP50G-CHOIC'.
    perform bdc_field       using 'RP50G-CHOIC'                              '002'.

    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
    perform bdc_field       using 'BDC_OKCODE'                              '=MOD'.
    perform bdc_field       using 'RP50G-PERNR'                              wa_upload-pernr."  '7000020'.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.
    perform bdc_field       using 'RP50G-CHOIC'                              'Personal Data'.

    perform bdc_dynpro      using 'MP000200' '2044'.
    perform bdc_field       using 'BDC_CURSOR'                              'P0002-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'P0002-BEGDA'                             gbdat."BEGDA." '18.02.2012'.
    perform bdc_field       using 'P0002-ENDDA'                              '31.12.9999'.
    if wa_upload-anred is not initial.
      perform bdc_field       using 'P0002-ANRED'                             anred. " '4'.
    endif.

    if wa_upload-nachn is initial.

      select single nachn
       from pa0002 into wa_upload-nachn
       where pernr = wa_upload-pernr
       and endda = '99991231'.

    endif.
    perform bdc_field       using 'P0002-NACHN'                             wa_upload-nachn." 'Deshmukh'.

    if  wa_upload-vorna is initial.
      select single vorna
    from pa0002 into wa_upload-vorna
    where pernr = wa_upload-pernr
    and endda = '99991231'.

    endif.
    perform bdc_field       using 'P0002-VORNA'                             wa_upload-vorna." 'Neelam'.
*    PERFORM BDC_FIELD       USING 'P0002-INITS'                             WA_UPLOAD- 'Mrs.'.

    if  wa_upload-midnm is initial.
      select single midnm
    from pa0002 into wa_upload-midnm
    where pernr = wa_upload-pernr
    and endda = '99991231'.

    endif.

    perform bdc_field       using 'P0002-MIDNM'                             wa_upload-midnm." 'Ketan'.
    if wa_upload-gbdat is not initial.
      perform bdc_field       using 'P0002-GBDAT'                             gbdat." '09.01.1987'.
    endif.

    if  wa_upload-gbort is initial.
      select single gbort
    from pa0002 into wa_upload-gbort
    where pernr = wa_upload-pernr
    and endda = '99991231'.

    endif.


    perform bdc_field       using 'P0002-GBORT'                             wa_upload-gbort." 'murbad'.
    if wa_upload-gesch is not initial.
      perform bdc_field       using 'P0002-GESCH'                             gesch. "  '2'.
    endif.

    perform bdc_field       using 'P0002-NATIO'                             'IN'.
    perform bdc_field       using 'P0002-SPRSL'                             'EN'.
    if wa_upload-famst is not initial.

      perform bdc_field       using 'P0002-FAMST'                             famst. " '1'.

    endif.

    perform bdc_dynpro      using 'MP000200' '2044'.
    perform bdc_field       using 'BDC_CURSOR'                              'P0002-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
*    PERFORM BDC_FIELD       USING 'P0002-BEGDA'                             BEGDA." '18.02.2012'.
    perform bdc_field       using 'P0002-ENDDA'                              '31.12.9999'.
    if wa_upload-anred is not initial.
      perform bdc_field       using 'P0002-ANRED'                             anred. " '4'.
    endif.

    perform bdc_field       using 'P0002-NACHN'                             wa_upload-nachn." 'Deshmukh'.
    perform bdc_field       using 'P0002-VORNA'                             wa_upload-vorna." 'Neelam'.
*    PERFORM BDC_FIELD       USING 'P0002-INITS'                              'Mrs.'.
    perform bdc_field       using 'P0002-MIDNM'                             wa_upload-midnm." 'Ketan'.

    if wa_upload-gbdat is not initial.
      perform bdc_field       using 'P0002-GBDAT'                             gbdat ."'09.01.1987'.
    endif.

    perform bdc_field       using 'P0002-GBORT'                             wa_upload-gbort." 'murbad'.
    if wa_upload-gesch is not initial.
      perform bdc_field       using 'P0002-GESCH'                             gesch. " '2'.
    endif.
    perform bdc_field       using 'P0002-NATIO'                              'IN'.
    perform bdc_field       using 'P0002-SPRSL'                              'EN'.
    if wa_upload-famst is not initial.
      perform bdc_field       using 'P0002-FAMST'                             famst. " '1'.
    endif.
*    PERFORM BDC_DYNPRO      USING 'MP000200' '2044'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'     '/EBCK'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'     'P0002-BEGDA'.

    if bdcdata is not initial.
      call transaction 'PA30' using bdcdata mode tran_mode update 'S'
        messages into t_msg .

      format color 3 intensified off.
      write : / 'IT 002 change Action for Employee:', wa_upload-pernr.

      loop at t_msg into w_msg where msgtyp = 'E'.
        format color 6 intensified off.
        condense wa_upload-pernr no-gaps.
        condense w_msg-msgv1 no-gaps.
        write : / 'ERROR:', w_msg-msgv1 ,',In IT 002 Change Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
      endloop.

      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / 'Changes Made Successfully for IT 002 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
      endif.

    endif.

  endif.

  clear: t_msg , w_msg.
  clear: bdcdata , bdcdata[] .
  refresh bdcdata.

*  **************************************************************************************************
* if changes made in IT 006
*  **************************************************************************************************
  data: temp_anssa type pa0006-anssa.
  clear : temp_anssa.
  if anssa is not initial.
    select single anssa from pa0006
      into temp_anssa
      where pernr = wa_upload-pernr
      and endda = '99991231'
      and anssa = anssa.
    if sy-subrc = 0.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'RP50G-PERNR'                              ''.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.

      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000020'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
      perform bdc_field       using 'RP50G-CHOIC'                              'Addresses'.
      perform bdc_field       using 'RP50G-SUBTY'                              anssa. "'01'.

      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
      perform bdc_field       using 'BDC_OKCODE'                              '=MOD'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr."  '7000020'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'RP50G-CHOIC'                              'Addresses'.
      perform bdc_field       using 'RP50G-SUBTY'                             anssa. " '01'.

      perform bdc_dynpro      using 'MP000600' '2005'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0006-LAND1'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'P0006-BEGDA'                             begda." '01.01.2013'.
      perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
      if wa_upload-locat is  not initial.
        perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat."  'gjkasdlgj_test'.
      endif.
      if wa_upload-stras is  not initial.
        perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'kasjgkla_test'.
      endif.
      if wa_upload-pstlz is  not initial.
        perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '789455'.
      endif.
      if wa_upload-ort01 is  not initial.
        perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'bhiwandi'.
      endif.
      if wa_upload-state is  not initial.
        perform bdc_field       using 'P0006-STATE'                             state. " '06'.
      endif.
      perform bdc_field       using 'P0006-LAND1'                              'IN'.

      perform bdc_dynpro      using 'MP000600' '2005'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0006-BEGDA'.
      perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
      perform bdc_field       using 'P0006-BEGDA'                             begda." '01.01.2013'.
      perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
      if wa_upload-locat is  not initial.
        perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat." 'gjkasdlgj_test'.
      endif.
      if wa_upload-stras is  not initial.
        perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'kasjgkla_test'.
      endif.
      if wa_upload-pstlz is  not initial.
        perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '789455'.
      endif.
      if wa_upload-ort01 is  not initial.
        perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'bhiwandi'.
      endif.
      if wa_upload-state is  not initial.
        perform bdc_field       using 'P0006-STATE'                             state. " '06'.
      endif.
      perform bdc_field       using 'P0006-LAND1'                              'IN'.

      if bdcdata is not initial.

        call transaction 'PA30' using bdcdata mode tran_mode update 'S'
          messages into t_msg .

        format color 3 intensified off.
        write : / 'IT 006 change Action for Employee:', wa_upload-pernr.

        loop at t_msg into w_msg where msgtyp = 'E'.
          format color 6 intensified off.
          condense wa_upload-pernr no-gaps.
          condense w_msg-msgv1 no-gaps.
          write : / 'ERROR:', w_msg-msgv1 ,',In IT 006 Change Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
        endloop.

        read table t_msg into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0.
          format color 5 intensified off.
          write : / 'Changes Made Successfully for IT 006 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
        endif.

      endif.

      clear: t_msg , w_msg.
      clear: bdcdata , bdcdata[] .
      refresh bdcdata.

    else.

      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'RP50G-PERNR'                              ''.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000018'.
      perform bdc_field       using 'BDC_CURSOR'                              'T582S-ITEXT(04)'.
      perform bdc_field       using 'RP50G-SELEC(04)'                          'X'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'RP50G-CHOIC'                              'Addresses'.
      perform bdc_field       using 'RP50G-SUBTY'                              anssa. "'02'.
      perform bdc_dynpro      using 'MP000600' '2005'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0006-STATE'.
      perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
      perform bdc_field       using 'P0006-BEGDA'                             begda." '05.03.2013'.
      perform bdc_field       using 'P0006-ENDDA'                              '31.12.9999'.
      if wa_upload-locat is not initial.
        perform bdc_field       using 'P0006-LOCAT'                             wa_upload-locat." 'perm Addr'.
      endif.
      if wa_upload-stras is not initial.
        perform bdc_field       using 'P0006-STRAS'                             wa_upload-stras." 'perm Addr'.
      endif.
      if wa_upload-pstlz is not initial.
        perform bdc_field       using 'P0006-PSTLZ'                             wa_upload-pstlz. " '456123'.
      endif.
      if wa_upload-ort01 is not initial.
        perform bdc_field       using 'P0006-ORT01'                             wa_upload-ort01." 'Mumbai'.
      endif.
      if wa_upload-state is not initial.
        perform bdc_field       using 'P0006-STATE'                             state. " '13'.
      endif.
      perform bdc_field       using 'P0006-LAND1'                              'IN'.

      if bdcdata is not initial.
        call transaction 'PA30' using bdcdata mode tran_mode update 'S'
          messages into t_msg .

        format color 3 intensified off.
        write : / 'IT 006 CREATE Action for Employee:', wa_upload-pernr.

        loop at t_msg into w_msg where msgtyp = 'E'.
          format color 6 intensified off.
          condense wa_upload-pernr no-gaps.
          condense w_msg-msgv1 no-gaps.
          write : / 'ERROR:', w_msg-msgv1 ,',In IT 006 CREATE Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
        endloop.

        read table t_msg into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0.
          format color 5 intensified off.
          write : / 'Address type Successfully created for IT 006 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
        endif.

      endif.

      clear: t_msg , w_msg.
      clear: bdcdata , bdcdata[] .
      refresh bdcdata.

    endif.
  endif.
*  **************************************************************************************************
* if changes made in IT 007
*  **************************************************************************************************
  data: temp_pernr type pa0007-pernr.
  data: oper type pspar-actio.

  if wa_upload-schkz is not initial.

    select single pernr
      from pa0007
      into temp_pernr
      where pernr = wa_upload-pernr
      and endda = '99991231'.
    if sy-subrc = 0.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000027'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.

      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
      perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000027'.
      perform bdc_field       using 'RP50G-TIMR6'                              'X'.
      perform bdc_field       using 'BDC_CURSOR'                              'RP50G-CHOIC'.
      perform bdc_field       using 'RP50G-CHOIC'                              '7'.

      perform bdc_dynpro      using 'MP000700' '2000'.
      perform bdc_field       using 'BDC_CURSOR'                              'P0007-SCHKZ'.
      perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
      perform bdc_field       using 'P0007-BEGDA'                             begda." '08.04.2013'.
      perform bdc_field       using 'P0007-ENDDA'                              '31.12.9999'.
      perform bdc_field       using 'P0007-SCHKZ'                             schkz." 'INDO'.
*      PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
*      PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.


*      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*      PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              ''.
*      PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*      PERFORM BDC_FIELD       USING 'RP50G-PERNR'                             WA_UPLOAD-PERNR." '7000027'.
*      PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-CHOIC'.
*      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'                              '7'.
*
*      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=INS'.
*      PERFORM BDC_FIELD       USING 'RP50G-PERNR'                             WA_UPLOAD-PERNR." '7000027'.
*      PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'                              'Planned Working Time'.
*
*      PERFORM BDC_DYNPRO      USING 'MP000700' '2000'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0007-SCHKZ'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              'UPD'.
*      PERFORM BDC_FIELD       USING 'P0007-BEGDA'                             BEGDA." '18.02.2012'.
*      PERFORM BDC_FIELD       USING 'P0007-ENDDA'                              '31.12.9999'.
*      PERFORM BDC_FIELD       USING 'P0007-SCHKZ'                             SCHKZ." 'INDO'.
**      PERFORM BDC_FIELD       USING 'P0007-ZTERF'                              '7'.
**      PERFORM BDC_FIELD       USING 'P0007-EMPCT'                              '  100.00'.
**      PERFORM BDC_FIELD       USING 'P0007-ARBST'                              '    8.50'.
**      PERFORM BDC_FIELD       USING 'P0007-WKWDY'                              '    5.00'.
**      PERFORM BDC_TRANSACTION USING 'PA30'.
      if bdcdata is not initial.
        call transaction 'PA30' using bdcdata mode tran_mode update 'S'
          messages into t_msg .

        format color 3 intensified off.
        write : / 'IT 007 Change Action for Employee:', wa_upload-pernr.

        loop at t_msg into w_msg where msgtyp = 'E'.
          format color 6 intensified off.
          condense wa_upload-pernr no-gaps.
          condense w_msg-msgv1 no-gaps.
          write : / 'ERROR:', w_msg-msgv1 ,',In IT 007 Change Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
        endloop.

        read table t_msg into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0.
          format color 5 intensified off.
          write : / 'Records SUCESSFULLY UPDATED for Infotype 0007,  FOR' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
        endif.

      endif.

    endif.
  endif.

  clear: t_msg , w_msg.
  clear: bdcdata , bdcdata[] .
  refresh bdcdata.

*  **************************************************************************************************
* if changes made in IT 008
*  **************************************************************************************************
  if wa_upload-lga01 is not initial.
*    PERFORM UPDATE_IT008.
  endif.

*  **************************************************************************************************
* if changes made in IT 0017
*  **************************************************************************************************

  if wa_upload-erkla is not initial or"  Reimbursement Group for Meals/Accommodations: Statutory
  wa_upload-ergru is not initial or" Reimbursement Group for Meals/Accomm. - Enterprise-Specific
  wa_upload-spebe is not initial." Employee Grouping for Travel Expense Type

    clear: p0017 , return,key, returne .
    p0017-pernr = wa_upload-pernr.
    p0017-begda = p0007begda.
    p0017-endda = '99991231'.
    p0017-erkla = wa_upload-erkla.
    p0017-ergru = wa_upload-ergru.
    p0017-spebe = wa_upload-spebe.


    clear :temp_pernr , oper.

    select single pernr
      from pa0017
      into temp_pernr
      where pernr = wa_upload-pernr
      and endda = '99991231'.
    if sy-subrc = 0.
      oper = 'MOD'.
    else.
      oper = 'INS'.
    endif.

    call function 'BAPI_EMPLOYEE_ENQUEUE'
      exporting
        number = p0017-pernr
      importing
        return = returne.


    call function 'HR_INFOTYPE_OPERATION'
      exporting
        infty         = '0017'
        number        = p0017-pernr
        subtype       = p0017-subty
        objectid      = p0017-objps
        lockindicator = p0017-sprps
        validityend   = p0017-endda
        validitybegin = p0017-begda
        recordnumber  = p0017-seqnr
        record        = p0017
        operation     = oper "'MOD'
        tclas         = 'A'
        dialog_mode   = '0'
      importing
        return        = return
        key           = key.

    if return is not initial.
      format color 6 intensified off.
      write :/ 'Error Occurred in Infotype 0017 , FOR' , wa_upload-pernr.
    else.
      format color 5 intensified off.
      write :/ 'Records SUCESSFULLY UPDATED for Infotype 0017,  FOR' ,  wa_upload-pernr.
    endif.


    call function 'BAPI_EMPLOYEE_DEQUEUE'
      exporting
        number = p0017-pernr.

  endif.


*  **************************************************************************************************
* if changes made in IT 0021
*  **************************************************************************************************
  clear: t_msg , w_msg.
  clear: bdcdata , bdcdata[] .
  refresh bdcdata.


  if wa_upload-famsa is not initial or
    wa_upload-favor is not initial or
    wa_upload-fanam is not initial or
    wa_upload-fasex is not initial or
    wa_upload-fgbdt is not initial or
    wa_upload-fgbot is not initial or
*    WA_UPLOAD-FANAT IS NOT INITIAL OR
    wa_upload-kdbsl is not initial or"(CHAR)(2) Allowance Authorization
    wa_upload-kdzul is not initial or"(CHAR(2)  Child Allowances
    wa_upload-kdgbr is not initial."(CHAR)(2) Child Allowance Entitlement


    clear : wa_upload21.


    if wa_upload-fgbdt is not initial.
      clear: fgbdt, dd,mm ,yyyy.
      split wa_upload-fgbdt at '/' into mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into fgbdt.
    endif.

    select single sap  from z6hr_ppl_2_sap
      into famsa  where tabname = 'PA0021' and ppl_sft = wa_upload-famsa.
    if sy-subrc <> 0 . clear:famsa. endif.

    select single pernr
      from pa0021
      into wa_upload21-pernr
      where pernr = wa_upload-pernr
      and subty = famsa.
    if sy-subrc = 0. "change IT 0021
      if famsa = '1' or famsa = '5' or
         famsa = '3' or famsa = '7' or
         famsa = '8' or famsa = '4' or famsa = '6'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                              ''.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000018'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '3'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

        if wa_upload-fgbot = 'ADDED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
        elseif wa_upload-fgbot = 'UPDATED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=MOD'.
        endif.

        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000018'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '3'.

        perform bdc_dynpro      using 'MP002100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P0021-FINIT'.
        perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
        perform bdc_field       using 'P0021-BEGDA'                             begda." '18.02.2012'.
        perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
        if wa_upload-fanam is not initial .
          perform bdc_field       using 'P0021-FANAM'                             wa_upload-fanam." 'Shetty'.
        endif.
        if wa_upload-favor is not initial.
          perform bdc_field       using 'P0021-FAVOR'                             wa_upload-favor." 'HOSHIAR CHAND'.
        endif.


        if wa_upload-famsa = 'FA'
          or wa_upload-famsa = 'FL'
          or wa_upload-famsa = 'S'
          or wa_upload-famsa = 'B'.

          wa_upload-fasex = 'M'.
        elseif wa_upload-famsa = 'ML'
          or wa_upload-famsa = 'MO'
          or wa_upload-famsa = 'SI'
          or wa_upload-famsa = 'D'.
          wa_upload-fasex = 'F'.
        endif.

        if wa_upload-famsa <> 'SP'.
          if wa_upload-fasex = 'F'.
            perform bdc_field       using 'Q0021-GESC2'                              'X'.
          elseif wa_upload-fasex = 'M'.
            perform bdc_field       using 'Q0021-GESC1'                              'X'.
          endif.
        endif.

*        PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
        if wa_upload-fgbdt is not initial.
          perform bdc_field       using 'P0021-FGBDT'                             fgbdt." '05.02.1946'.
        endif.
        if wa_upload-fgbot is not initial.
          perform bdc_field       using 'P0021-FGBOT'                             wa_upload-fgbot." 'shahapur'.
        endif.
        if wa_upload-fanat is not initial.
          perform bdc_field       using 'P0021-FANAT'                             'IN'."WA_UPLOAD-FANAT.
        endif.

        if bdcdata is not initial.
          call transaction 'PA30' using bdcdata mode tran_mode update 'S'
            messages into t_msg .

          format color 3 intensified off.
          write : / 'IT 0021 Change Action for Employee:', wa_upload-pernr , 'SUBTYPE: ' , famsa.

          loop at t_msg into w_msg where msgtyp = 'E'.
            format color 6 intensified off.
            condense wa_upload-pernr no-gaps.
            condense w_msg-msgv1 no-gaps.
            write : / 'ERROR:', w_msg-msgv1 ,',In IT 0021 Change Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
          endloop.

          read table t_msg into w_msg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            format color 5 intensified off.
            write : / 'Record Change Successfully IT 0021 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
          endif.

        endif.

        clear: t_msg , w_msg.
        clear: bdcdata , bdcdata[] .
        refresh bdcdata.


      elseif famsa = 2.

        if wa_upload-famsa = 'S'.
          wa_upload-fasex = 'M'.
        elseif wa_upload-famsa = 'D'.
          wa_upload-fasex = 'F'.
        endif.



        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                              ''.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000015'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '2'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.

        if wa_upload-fgbot = 'ADDED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
        elseif wa_upload-fgbot = 'UPDATED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=COP'.
        endif.

        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000015'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '2'.

*        PERFORM BDC_DYNPRO      USING 'MP002100' '2040'.
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0021-KDGBR'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*        PERFORM BDC_FIELD       USING 'P0021-BEGDA'                             BEGDA." '18.02.2012'.
*        PERFORM BDC_FIELD       USING 'P0021-ENDDA'                              '31.12.9999'.
**        PERFORM BDC_FIELD       USING 'P0021-OBJPS'                              '01'.
*        IF WA_UPLOAD-FANAM IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-FANAM'                              WA_UPLOAD-FANAM."'kUMAR'.
*        ENDIF.
*        IF WA_UPLOAD-FAVOR IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-FAVOR'                              WA_UPLOAD-FAVOR."'CHOTA'.
*        ENDIF.
*
*        IF WA_UPLOAD-FASEX = 'F'.
*            PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
*        ELSEIF WA_UPLOAD-FASEX = 'M'.
*            PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
*        ENDIF.
*
**        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              ''.
**        PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
*        IF WA_UPLOAD-KDBSL IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-KDBSL'                              WA_UPLOAD-KDBSL."'Y'.
*        ENDIF.
*        IF WA_UPLOAD-FGBDT IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-FGBDT'                              FGBDT."'01.01.2011'.
*        ENDIF.
*        IF WA_UPLOAD-KDZUL IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-KDZUL'                              WA_UPLOAD-KDZUL."'Y'.
*        ENDIF.
*
*        IF WA_UPLOAD-FGBOT IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-FGBOT'                              WA_UPLOAD-FGBOT."'shahapur'.
*        ENDIF.
*
*        IF WA_UPLOAD-KDGBR IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-KDGBR'                              WA_UPLOAD-KDGBR."'N'.
*        ENDIF.
*
*        IF WA_UPLOAD-FANAT IS NOT INITIAL.
*          PERFORM BDC_FIELD       USING 'P0021-FANAT'                              'IN'.
*        ENDIF.

        perform bdc_dynpro      using 'MP002100' '2040'.
        perform bdc_field       using 'BDC_CURSOR'                              'P0021-BEGDA'.
        perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
        perform bdc_field       using 'P0021-BEGDA'                             begda." '18.02.2012'.
        perform bdc_field       using 'P0021-ENDDA'                             '31.12.9999'.
*        PERFORM BDC_FIELD       USING 'P0021-OBJPS'                              '01'.
        if wa_upload-fanam is not initial.
          perform bdc_field       using 'P0021-FANAM'                              wa_upload-fanam."'kUMAR'.
        endif.
        if wa_upload-favor is not initial.
          perform bdc_field       using 'P0021-FAVOR'                              wa_upload-favor."'CHOTA'.
        endif.

*        PERFORM BDC_FIELD       USING 'Q0021-GESC1'                              'X'.
        if wa_upload-kdbsl is not initial.
          perform bdc_field       using 'P0021-KDBSL'                              wa_upload-kdbsl."'Y'.
        endif.

        if wa_upload-fgbdt is not initial.
          perform bdc_field       using 'P0021-FGBDT'                              fgbdt."'01.01.2011'.
        endif.
        if wa_upload-kdzul is not initial.
          perform bdc_field       using 'P0021-KDZUL'                              wa_upload-kdzul."'Y'.
        endif.
        if wa_upload-fgbot is not initial.
          perform bdc_field       using 'P0021-FGBOT'                              wa_upload-fgbot."'shahapur'.
        endif.

        if wa_upload-kdgbr is not initial.
          perform bdc_field       using 'P0021-KDGBR'                              wa_upload-kdgbr."'N'.
        endif.
        if wa_upload-fanat is not initial.
          perform bdc_field       using 'P0021-FANAT'                              'IN'.
        endif.

        if bdcdata is not initial.
          call transaction 'PA30' using bdcdata mode tran_mode update 'S'
            messages into t_msg .

          format color 3 intensified off.
          write : / 'IT 0021 Change Action for Employee:', wa_upload-pernr , 'SUBTYPE: ' , famsa.

          loop at t_msg into w_msg where msgtyp = 'E'.
            format color 6 intensified off.
            condense wa_upload-pernr no-gaps.
            condense w_msg-msgv1 no-gaps.
            write : / 'ERROR:', w_msg-msgv1 ,',In IT 0021 Change Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
          endloop.

          read table t_msg into w_msg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            format color 5 intensified off.
            write : / 'Records Change Successfully for IT 0021 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
          endif.

        endif.

        clear: t_msg , w_msg.
        clear: bdcdata , bdcdata[] .
        refresh bdcdata.

      endif.
    else. " create IT 0021
      if famsa = '1' or famsa = '5' or
           famsa = '3' or famsa = '7' or
           famsa = '8' or famsa = '4' or famsa = '6'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                              ''.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000018'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '4'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000018'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa . " '4'.

        perform bdc_dynpro      using 'MP002100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P0021-FANAM'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'P0021-BEGDA'                             begda." '06.03.2013'.
        perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
        if wa_upload-fanam is not initial.
          perform bdc_field       using 'P0021-FANAM'                             wa_upload-fanam." 'Shetty'.
        endif.
        if wa_upload-favor is not initial.
          perform bdc_field       using 'P0021-FAVOR'                             wa_upload-favor." 'Meena'.
        endif.

        if wa_upload-famsa = 'FA' or wa_upload-famsa = 'FL' or wa_upload-famsa = 'S'
        or wa_upload-famsa = 'B'.
          wa_upload-fasex = 'M'.
        elseif wa_upload-famsa = 'ML' or wa_upload-famsa = 'MO' or wa_upload-famsa = 'SI'
          or wa_upload-famsa = 'D'.
          wa_upload-fasex = 'F'.
        endif.

        if wa_upload-famsa <> 'SP'.
          if wa_upload-fasex = 'F'.
            perform bdc_field       using 'Q0021-GESC2'                              'X'.
          elseif wa_upload-fasex = 'M'.
            perform bdc_field       using 'Q0021-GESC1'                              'X'.
          endif.
        endif.

*        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.

        if wa_upload-fgbdt is not initial.
          perform bdc_field       using 'P0021-FGBDT'                              fgbdt."'05.02.1941'.
        endif.
        if wa_upload-fgbot is not initial.
          perform bdc_field       using 'P0021-FGBOT'                              wa_upload-fgbot."'test'.
        endif.
        if wa_upload-fanat is not initial.
          perform bdc_field       using 'P0021-FANAT'                              'IN'.
        endif.

        perform bdc_dynpro      using 'MP002100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P0021-BEGDA'.
        perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
        perform bdc_field       using 'P0021-BEGDA'                             begda." '06.03.2013'.
        perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
        if wa_upload-fanam is not initial.
          perform bdc_field       using 'P0021-FANAM'                             wa_upload-fanam." 'Shetty'.
        endif.
        if wa_upload-favor is not initial.
          perform bdc_field       using 'P0021-FAVOR'                             wa_upload-favor." 'Meena'.
        endif.
        if wa_upload-famsa = 'FA' or wa_upload-famsa = 'FL' or wa_upload-famsa = 'S'
        or wa_upload-famsa = 'B'.
          wa_upload-fasex = 'M'.
        elseif wa_upload-famsa = 'ML' or wa_upload-famsa = 'MO' or wa_upload-famsa = 'SI'
          or wa_upload-famsa = 'D'.
          wa_upload-fasex = 'F'.
        endif.

        if wa_upload-famsa <> 'SP'.
          if wa_upload-fasex = 'F'.
            perform bdc_field       using 'Q0021-GESC2'                              'X'.
          elseif wa_upload-fasex = 'M'.
            perform bdc_field       using 'Q0021-GESC1'                              'X'.
          endif.
        endif.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
        if wa_upload-fgbdt is not initial.
          perform bdc_field       using 'P0021-FGBDT'                             fgbdt." '05.02.1941'.
        endif.
        if wa_upload-fgbot is not initial.
          perform bdc_field       using 'P0021-FGBOT'                             wa_upload-fgbot." 'test'.
        endif.
        if wa_upload-fanat is not initial.
          perform bdc_field       using 'P0021-FANAT'                              'IN'.
        endif.

        if bdcdata is not initial.
          call transaction 'PA30' using bdcdata mode tran_mode update 'S'
            messages into t_msg .

          format color 3 intensified off.
          write : / 'IT 0021 Create Action for Employee:', wa_upload-pernr , 'SUBTYPE: ' , famsa.

          loop at t_msg into w_msg where msgtyp = 'E'.
            format color 6 intensified off.
            condense wa_upload-pernr no-gaps.
            condense w_msg-msgv1 no-gaps.
            write : / 'ERROR:', w_msg-msgv1 ,',In IT 0021 Create Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
          endloop.

          read table t_msg into w_msg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            format color 5 intensified off.
            write : / 'Record Creatted Successfully for IT 0021 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
          endif.

        endif.

        clear: t_msg , w_msg.
        clear: bdcdata , bdcdata[] .
        refresh bdcdata.

      elseif famsa = '2'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                              ''.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'RP50G-PERNR'                              wa_upload-pernr. "'7000016'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-SUBTY'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                              famsa. "'2'.

        perform bdc_dynpro      using 'SAPMP50A' '1000'.
        perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
        if wa_upload-fgbot = 'ADDED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=INS'.
        elseif wa_upload-fgbot = 'UPDATED'.
          perform bdc_field       using 'BDC_OKCODE'                              '=MOD'.
        endif.

        perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '7000016'.
        perform bdc_field       using 'RP50G-TIMR6'                              'X'.
        perform bdc_field       using 'RP50G-CHOIC'                              'Family Member/Dependents'.
        perform bdc_field       using 'RP50G-SUBTY'                             famsa. " '2'.

        perform bdc_dynpro      using 'MP002100' '2040'.
        perform bdc_field       using 'BDC_CURSOR'                              'P0021-FANAT'.
        perform bdc_field       using 'BDC_OKCODE'                              'UPD'.
        perform bdc_field       using 'P0021-BEGDA'                             begda." '06.03.2013'.
        perform bdc_field       using 'P0021-ENDDA'                              '31.12.9999'.
*        PERFORM BDC_FIELD       USING 'P0021-OBJPS'                              '01'.
        if  wa_upload-fanam is not initial.
          perform bdc_field       using 'P0021-FANAM'                             wa_upload-fanam." 'Neelambari'.
        endif.
        if  wa_upload-favor is not initial.
          perform bdc_field       using 'P0021-FAVOR'                             wa_upload-favor." 'MUNNU'.
        endif.
*        PERFORM BDC_FIELD       USING 'Q0021-GESC2'                              'X'.
        if  wa_upload-kdbsl is not initial.
          perform bdc_field       using 'P0021-KDBSL'                             wa_upload-kdbsl." 'Y'.
        endif.
        if  wa_upload-fgbdt is not initial.
          perform bdc_field       using 'P0021-FGBDT'                             fgbdt." '01.04.2011'.
        endif.
        if  wa_upload-kdzul is not initial.
          perform bdc_field       using 'P0021-KDZUL'                             wa_upload-kdzul." 'Y'.
        endif.
        if  wa_upload-fgbot is not initial.
          perform bdc_field       using 'P0021-FGBOT'                             wa_upload-fgbot." 'BADLAPUR'.
        endif.
        if  wa_upload-kdgbr is not initial.
          perform bdc_field       using 'P0021-KDGBR'                             wa_upload-kdgbr." 'Y'.
        endif.
        if  wa_upload-fanat is not initial.
          perform bdc_field       using 'P0021-FANAT'                              'IN'.
        endif.

        if bdcdata is not initial.
          call transaction 'PA30' using bdcdata mode tran_mode update 'S'
            messages into t_msg .

          format color 3 intensified off.
          write : / 'IT 0021 Create Action for Employee:', wa_upload-pernr , 'SUBTYPE: ' , famsa.

          loop at t_msg into w_msg where msgtyp = 'E'.
            format color 6 intensified off.
            condense wa_upload-pernr no-gaps.
            condense w_msg-msgv1 no-gaps.
            write : / 'ERROR:', w_msg-msgv1 ,',In IT 0021 Create Action For:' , wa_upload-pernr."USRID1 ."'.ERROR
          endloop.

          read table t_msg into w_msg with key msgtyp = 'E'.
          if sy-subrc <> 0.
            format color 5 intensified off.
            write : / 'Records Created Successfully for IT 0021 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
          endif.

        endif.

        clear: t_msg , w_msg.
        clear: bdcdata , bdcdata[] .
        refresh bdcdata.



      endif.

    endif.





  endif.

  if wa_upload-gbdat is not initial .
    data: zgbdat type pa0002-gbdat.
    clear: zgbdat, dd,mm ,yyyy.
    split wa_upload-gbdat at '/' into mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into zgbdat.


    clear: t_msg , w_msg.
    clear: bdcdata , bdcdata[] .
    refresh bdcdata.

    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'RP50G-PERNR'                               wa_upload-pernr. "'6010336'.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.
    perform bdc_dynpro      using 'SAPMP50A' '1000'.
    perform bdc_field       using 'BDC_OKCODE'                              '=MOD'.
    perform bdc_field       using 'RP50G-PERNR'                               wa_upload-pernr. "'6010336'.
    perform bdc_field       using 'BDC_CURSOR'                              'T582S-ITEXT(03)'.
    perform bdc_field       using 'RP50G-SELEC(03)'                          'X'.
    perform bdc_field       using 'RP50G-TIMR6'                              'X'.
    perform bdc_dynpro      using 'MP000200' '2044'.
    perform bdc_field       using 'BDC_CURSOR'                              'P0002-GBDAT'.
    perform bdc_field       using 'BDC_OKCODE'                              '/00'.
    perform bdc_field       using 'P0002-BEGDA'                              zgbdat."'30.12.1982'.
*    perform bdc_field       using 'P0002-ENDDA'                              '31.12.9999'.
*    perform bdc_field       using 'P0002-ANRED'                              '1'.
*    perform bdc_field       using 'P0002-NACHN'                              'Vaghela'.
*    perform bdc_field       using 'P0002-VORNA'                              'Dharmesh'.
    perform bdc_field       using 'P0002-GBDAT'                              zgbdat."'30.12.1982'.
*    perform bdc_field       using 'P0002-GESCH'                              '1'.
*    perform bdc_field       using 'P0002-NATIO'                              'IN'.
*    perform bdc_field       using 'P0002-SPRSL'                              'EN'.
*    perform bdc_field       using 'P0002-FAMST'                              '0'.
    perform bdc_dynpro      using 'MP000200' '2044'.
    perform bdc_field       using 'BDC_CURSOR'                              'P0002-BEGDA'.
    perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
    perform bdc_field       using 'P0002-BEGDA'                              zgbdat."'30.12.1982'.
*    perform bdc_field       using 'P0002-ENDDA'                              '31.12.9999'.
*    perform bdc_field       using 'P0002-ANRED'                              '1'.
*    perform bdc_field       using 'P0002-NACHN'                              'Vaghela'.
*    perform bdc_field       using 'P0002-VORNA'                              'Dharmesh'.
    perform bdc_field       using 'P0002-GBDAT'                              zgbdat."'30.12.1982'.
*    perform bdc_field       using 'P0002-GESCH'                              '1'.
*    perform bdc_field       using 'P0002-NATIO'                              'IN'.
*    perform bdc_field       using 'P0002-SPRSL'                              'EN'.
*    perform bdc_field       using 'P0002-FAMST'                              '0'.


    if bdcdata is not initial.
      call transaction 'PA30' using bdcdata mode tran_mode update 'S'
        messages into t_msg .

      format color 3 intensified off.
      write : / 'IT 002 UPLOAD DATE OF BIRTH FOR :', wa_upload-pernr .

      loop at t_msg into w_msg where msgtyp = 'E'.
        format color 6 intensified off.
        condense wa_upload-pernr no-gaps.
        condense w_msg-msgv1 no-gaps.
        write : / 'ERROR:', w_msg-msgv1 ,',In IT 002 While uploading DOB For:' , wa_upload-pernr."USRID1 ."'.ERROR
      endloop.

      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / 'Records Created Successfully for IT 002 :' , wa_upload-pernr."USRID1 ."'Confirmation Action Successful for User ID', WA_UPLOAD-USRID1.
      endif.

    endif.

    clear: t_msg , w_msg.
    clear: bdcdata , bdcdata[] .
    refresh bdcdata.


  endif.


*  **************************************************************************************************
* if changes made in IT 0105
*  **************************************************************************************************
  if wa_upload-usrid3 is not initial ."OR WA_UPLOAD-USRTY1 is not initial.
    data: zemail type pa0105-usrid_long.

    select single usrid_long
      from pa0105
      into zemail
      where pernr =  wa_upload-pernr
      and endda >= sy-datum " '99991231'
      and subty = '0010'.

    if zemail <> wa_upload-usrid3.

      data: rev_usrid3 type s_upload-usrid3.
      data: iner_mail(8).
      call function 'STRING_REVERSE'
        exporting
          string  = wa_upload-usrid3
          lang    = 'E'
        importing
          rstring = rev_usrid3
*     EXCEPTIONS
*         TOO_SMALL       = 1
*         OTHERS  = 2
        .

      iner_mail = rev_usrid3(08)."

      if iner_mail = 'moc.idom'. " Check email ID is internal

        select single begda from pa0105 into p0105-begda
          where pernr = wa_upload-pernr
            and subty = '0010'
            and endda >= sy-datum." '99991231'.

        if sy-subrc = 0.
          p0105-pernr = wa_upload-pernr.

          call function 'BAPI_EMPLOYEE_ENQUEUE'
            exporting
              number = p0105-pernr
            importing
              return = returne.

          call function 'BAPI_EMPLCOMM_DELIMIT'
            exporting
              employeenumber = p0105-pernr
              subtype        = '0010'
              objectid       = '' " P0105-OBJPS
              lockindicator  = ''
              validitybegin  = p0105-begda
              validityend    = '99991231' "P0105BEGDA
              recordnumber   = '000'
              delimit_date   = p0105begda
*             NOCOMMIT       =
            importing
              return         = return.
*   EMPLCOMMKEY          = KEY.
          .

          call function 'BAPI_EMPLOYEE_DEQUEUE'
            exporting
              number = p0105-pernr.
        endif.

        clear: p0105 , return,key, returne .
        p0105-pernr = wa_upload-pernr.
        p0105-begda = p0105begda.
        p0105-endda = '99991231'.
        p0105-subty = '0010'.
        p0105-aedtm = sy-datum.
        p0105-uname = sy-uname.
        p0105-usrid_long = wa_upload-usrid3.
**    P0105-SPRPS = SY-LANGU.
**    P0105-USRTY = '0010'.

        clear :temp_pernr , oper.

        select single pernr
          from pa0105
          into temp_pernr
          where pernr = wa_upload-pernr
          and subty = '0010'
          and endda >= sy-datum ."'99991231'.
*    IF SY-SUBRC = 0.
*      OPER = 'MOD'.
*    ELSE.
        oper = 'INS'.
*    ENDIF.

        call function 'BAPI_EMPLOYEE_ENQUEUE'
          exporting
            number = p0105-pernr
          importing
            return = returne.

        call function 'HR_INFOTYPE_OPERATION'
          exporting
            infty         = '0105'
            number        = p0105-pernr
            subtype       = p0105-subty
            objectid      = p0105-objps
            lockindicator = '' "P0105-SPRPS
            validityend   = p0105-endda
            validitybegin = p0105-begda
            recordnumber  = p0105-seqnr
            record        = p0105
            operation     = oper "'MOD'
            tclas         = 'A'
            dialog_mode   = '0'
          importing
            return        = return
            key           = key.

        if return is not initial.
          format color 6 intensified off.
          write :/ 'Error Occurred in Infotype 0105 - EMAIL , FOR' , wa_upload-pernr.
        else.
          format color 5 intensified off.
          write :/ 'Records SUCESSFULLY UPDATED for Infotype 0105 - EMAIL,  FOR' ,  wa_upload-pernr.
        endif.

        call function 'BAPI_EMPLOYEE_DEQUEUE'
          exporting
            number = p0105-pernr.

      endif.
    endif.
  endif.

  if wa_upload-fanat is not initial ."OR MOBILE NUMBER.

    data: zfanat type pa0105-usrid.

    select single usrid
      from pa0105
      into zfanat
      where pernr =  wa_upload-pernr
      and endda >= sy-datum " '99991231'
      and subty = 'CELL'.

    if zfanat <> wa_upload-fanat .

      clear: p0105 , return,key, returne .
      p0105-pernr = wa_upload-pernr.
      p0105-begda = p0105begda.
      p0105-endda = '99991231'.
      p0105-subty = 'CELL'.
      p0105-aedtm = sy-datum.
      p0105-uname = sy-uname.
      p0105-usrid = wa_upload-fanat.
**    P0105-SPRPS = SY-LANGU.
**    P0105-USRTY = '0010'.

      clear :temp_pernr , oper.

      select single pernr
        from pa0105
        into temp_pernr
        where pernr = wa_upload-pernr
        and subty = 'CELL'
        and endda >= sy-datum ." '99991231'.
*    IF SY-SUBRC = 0.
*      OPER = 'MOD'.
*    ELSE.
      oper = 'INS'.
*    ENDIF.

      call function 'BAPI_EMPLOYEE_ENQUEUE'
        exporting
          number = p0105-pernr
        importing
          return = returne.

      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty         = '0105'
          number        = p0105-pernr
          subtype       = p0105-subty
          objectid      = p0105-objps
          lockindicator = '' "P0105-SPRPS
          validityend   = p0105-endda
          validitybegin = p0105-begda
          recordnumber  = p0105-seqnr
          record        = p0105
          operation     = oper "'MOD'
          tclas         = 'A'
          dialog_mode   = '0'
        importing
          return        = return
          key           = key.

      if return is not initial.
        format color 6 intensified off.
        write :/ 'Error Occurred in Infotype 0105 - CONTACT NO  , FOR' , wa_upload-pernr.
      else.
        format color 5 intensified off.
        write :/ 'CONTACT NUMBER SUCESSFULLY UPDATED for Infotype 0105,  FOR' ,  wa_upload-pernr.
      endif.

      call function 'BAPI_EMPLOYEE_DEQUEUE'
        exporting
          number = p0105-pernr.

    endif.
  endif.



endform.                    " OTHER
*&---------------------------------------------------------------------*
*&      Form  Assignto_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assignto_pos_hir .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10).
  data: para type tpara-paramid value 'PON',
        prog type sy-repid.
  data:  ctumode like ctu_params-dismode.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.
  zzbegda = begda.
  clear: dd, mm, yyyy.


********************************REPORTS TO*********************************************
  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             ''.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                              plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'MARKFELD(02)'                              'X'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.

  data: report_pos type p1001-sobid, kokrs type pa0001-kokrs.
  clear: report_pos ,kokrs.


  if wa_upload-mstbr is initial.
    select single mstbr from pa0001 into wa_upload-mstbr where pernr = wa_upload-pernr
      and  endda = '99991231'.
    if sy-subrc = 0.
      if wa_upload-mstbr is  not initial.
        select single plans from pa0001 into report_pos where pernr = wa_upload-mstbr
          and endda = '99991231'.
      endif.
    endif.
*  ELSE.
*    FIRST = WA_UPLOAD-MSTBR(02).
*    IF FIRST = '60'.
*      CLEAR EX_MSTBR.
*      SELECT SINGLE PERNR FROM PA0000 INTO EX_MSTBR WHERE PERNR = WA_UPLOAD-MSTBR+02(05).
*      IF SY-SUBRC = 0.
*        WA_UPLOAD-MSTBR = EX_MSTBR.
*        IF WA_UPLOAD-MSTBR IS  NOT INITIAL.
*        SELECT SINGLE PLANS FROM PA0001 INTO REPORT_POS WHERE PERNR = WA_UPLOAD-MSTBR
*          AND ENDDA = '99991231'.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
  endif.

  if wa_upload-mstbr is  not initial.

    data: first(02), second(05).

    first = wa_upload-mstbr(02).
    if first = '60'.
      clear ex_mstbr.
      select single pernr from pa0000 into ex_mstbr where pernr = wa_upload-mstbr+02(05).
      if sy-subrc = 0.
        wa_upload-mstbr = ex_mstbr.
      endif.
    endif.

    select single plans from pa0001 into report_pos where pernr = wa_upload-mstbr
      and endda = '99991231'.
  endif.


  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

********************************POSITION holder *********************************************

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '008'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '008'.
  perform bdc_field       using 'P1001-SCLAS'                              'P'.
  perform bdc_field       using 'P1001-SOBID'                              wa_upload-pernr. "'6000760'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '008'.
  perform bdc_field       using 'P1001-SCLAS'                              'P'.
  perform bdc_field       using 'P1001-SOBID'                              wa_upload-pernr. "'6000760'.

********************************Cost cente *********************************************

  if wa_upload-kostl is initial. " other than hiring action
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.

    if sy-subrc <> 0.
      clear: wa_upload-kostl.
    endif.
  endif.

  data: k_plans type pa0001-plans, sobid type hrp1001-sobid.
  clear: k_plans , sobid.

  if plans_num is initial.
    select single plans kostl
     from pa0001 into (k_plans ,wa_upload-kostl)
     where pernr = wa_upload-pernr
     and endda = '99991231'.

    if sy-subrc = 0.
      select single sobid from hrp1001 into sobid
        where objid = k_plans
        and otype = 'S'
        and plvar = '01'
        and rsign = 'A'
        and relat = '011'
        and endda = '99991231'
        and sclas = 'K'.
      if sy-subrc <> 0.
        clear:  sobid.
      endif.
    else.
      clear: k_plans .
    endif.
  else.
    concatenate wa_upload-kostl '1000' into sobid.
  endif.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                              plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.

*  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              '17/08'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=PICK'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                             '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.
  perform bdc_field       using 'P1001-SCLAS'                              'K'.
  perform bdc_field       using 'P1001-SOBID'                              sobid."'01001010501000'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                              begda." '12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.
  perform bdc_field       using 'P1001-SCLAS'                              'K'.
  perform bdc_field       using 'P1001-SOBID'                              sobid."'01001010501000'.

  perform bdc_dynpro      using 'MP100100' '5010'.
  perform bdc_field       using 'BDC_CURSOR'                              'PKEYK-KOSTL'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'PKEYK-KOSTL'                             wa_upload-kostl." '100101050'.
  perform bdc_field       using 'PKEYK-KOKRS'                              '1000'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*perform bdc_transaction using 'PO13'.


  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PO13' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .

  endif.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.
  data: pos_pernr type pa0001-pernr,
        pos_prog  type pa0001-plans.

  pos_pernr = wa_upload-pernr.
  pos_prog = prog.

*  WA_Z6HR_PS_SAP_POS-PERNR = POS_PERNR.
*  WA_Z6HR_PS_SAP_POS-PLANS = POS_PROG.
*  WA_Z6HR_PS_SAP_POS-PS_PLANS = WA_UPLOAD-PLANS.
*  WA_Z6HR_PS_SAP_POS-DESCR = WA_UPLOAD-DESCRIPTION2.

*  INSERT Z6HR_PS_SAP_POS FROM WA_Z6HR_PS_SAP_POS.

*  COMMIT WORK.

  clear: bdcdata , bdcdata[] , wa_z6hr_ps_sap_pos , pos_pernr , pos_prog.
  refresh bdcdata.

endform.                    " CREATE_NEW_POS
*&---------------------------------------------------------------------*
*&      Form  REPORTING_CHNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reporting_chng .
  data : dd(02), mm(02),yyyy(04).
  data: begda(10).
  data: para type tpara-paramid value 'PON',
        prog type sy-repid.
  data:  ctumode like ctu_params-dismode.

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

*  ********************************REPORTS TO CHANGES IN PO13*********************************************
  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             ''.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                              plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'MARKFELD(02)'                              'X'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.

  data: report_pos type p1001-sobid, kokrs type pa0001-kokrs.
  clear: report_pos ,kokrs.

  if wa_upload-mstbr is  not initial.
    select single plans from pa0001 into report_pos where pernr = wa_upload-mstbr
      and endda = '99991231'.
  endif.


  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*perform bdc_transaction using 'PO13'.


  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PO13' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .

    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR in "Reporting To" data Updation in Tcode PO13 ' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.
    if t_msg is not initial.
      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / text-018 , wa_upload-pernr."USRID1 ."'Reporting to Changes Successfully for User ID', WA_UPLOAD-USRID1.
      endif.
    endif.
    clear: t_msg , w_msg.

  endif.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.

*********************************REPORTS TO CHANGES IN PA30*********************************************

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '6011666'.
  perform bdc_field       using 'RP50G-TIMR6'                             'X'.

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INS'."'=MOD'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '6011666'.
  perform bdc_field       using 'BDC_CURSOR'                              'T582S-ITEXT(02)'.
  perform bdc_field       using 'RP50G-SELEC(02)'                         'X'.
  perform bdc_field       using 'RP50G-TIMR6'                             'X'.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-MSTBR'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda." '06.09.2013'.
  perform bdc_field       using 'P0001-ENDDA'                             '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0001-BTRTL'                              'HY01'.
*  PERFORM BDC_FIELD       USING 'P0001-KOSTL'                              '100105515'.
*  PERFORM BDC_FIELD       USING 'P0001-ABKRS'                              'IN'.
*  PERFORM BDC_FIELD       USING 'P0001-PLANS'                              '50002151'.
  perform bdc_field       using 'P0001-MSTBR'                              wa_upload-mstbr."'00000123'.
*  PERFORM BDC_FIELD       USING 'P0001-ORGEH'                              '50000100'.
*  PERFORM BDC_FIELD       USING 'P0001-ZZSTATE'                            '01'.
*  PERFORM BDC_FIELD       USING 'P0001-ZZLOCATION_KEY'                     '011'.

  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                            'P0001-BEGDA'.

  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PA30' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .


    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR in "Reporting" data Updation in Tcode PA30' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.
    if t_msg is not initial.
      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / text-021 , wa_upload-pernr."USRID1 ."'Reporting to Changes Successfully for User ID', WA_UPLOAD-USRID1.
      endif.
    endif.
    clear: t_msg , w_msg.

  endif.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.

endform.                    " REPORTING_CHNG
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PO13
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_po13 .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.
********************************REPORTS TO*********************************************

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'PPHDR-PLVAR'                             '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             ''.
  perform bdc_field       using 'PM0D1-TIMR6'                             'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                             begda."'14.02.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                              plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'MARKFELD(02)'                              'X'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.

  data: report_pos type p1001-sobid, kokrs type pa0001-kokrs.
  clear: report_pos ,kokrs.


  if wa_upload-mstbr is initial.
    select single mstbr from pa0001 into wa_upload-mstbr where pernr = wa_upload-pernr
      and  endda = '99991231'.
    if sy-subrc = 0.
      if wa_upload-mstbr is  not initial.
        select single plans from pa0001 into report_pos where pernr = wa_upload-mstbr
          and endda = '99991231'.
      endif.
    endif.
*  ELSE.
*    FIRST = WA_UPLOAD-MSTBR(02).
*    IF FIRST = '60'.
*      CLEAR EX_MSTBR.
*      SELECT SINGLE PERNR FROM PA0000 INTO EX_MSTBR WHERE PERNR = WA_UPLOAD-MSTBR+02(05).
*      IF SY-SUBRC = 0.
*        WA_UPLOAD-MSTBR = EX_MSTBR.
*        IF WA_UPLOAD-MSTBR IS  NOT INITIAL.
*        SELECT SINGLE PLANS FROM PA0001 INTO REPORT_POS WHERE PERNR = WA_UPLOAD-MSTBR
*          AND ENDDA = '99991231'.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
  endif.

  if wa_upload-mstbr is  not initial.


    data: first(02), second(05).

    first = wa_upload-mstbr(02).
    if first = '60'.
      clear ex_mstbr.
      select single pernr from pa0000 into ex_mstbr where pernr = wa_upload-mstbr+02(05).
      if sy-subrc = 0.
        wa_upload-mstbr = ex_mstbr.
      endif.
    endif.

    select single plans from pa0001 into report_pos where pernr = wa_upload-mstbr
      and endda = '99991231'.
  endif.


  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '002'.
  perform bdc_field       using 'P1001-SCLAS'                              'S'.
  perform bdc_field       using 'P1001-SOBID'                              report_pos."'50002048'.

********************************Cost cente *********************************************

  if wa_upload-kostl is initial. " other than hiring action
    select single kostl
      from pa0001 into wa_upload-kostl
      where pernr = wa_upload-pernr
      and endda = '99991231'.

    if sy-subrc <> 0.
      clear: wa_upload-kostl.
    endif.
  endif.

  data: k_plans type pa0001-plans, sobid type hrp1001-sobid.
  clear: k_plans , sobid.

  if plans_num is initial.
    select single plans kostl
     from pa0001 into (k_plans ,wa_upload-kostl)
     where pernr = wa_upload-pernr
     and endda = '99991231'.

    if sy-subrc = 0.
      select single sobid from hrp1001 into sobid
        where objid = k_plans
        and otype = 'S'
        and plvar = '01'
        and rsign = 'A'
        and relat = '011'
        and endda = '99991231'
        and sclas = 'K'.
      if sy-subrc <> 0.
        clear:  sobid.
      endif.
    else.
      clear: k_plans .
    endif.
  else.
    concatenate wa_upload-kostl '1000' into sobid.
  endif.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                              plans_num." '50002141'.
  perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
  perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
  perform bdc_field       using 'PPHDR-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-RSIGN'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.

*  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              '17/08'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=PICK'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P1001-BEGDA'                               begda."'12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                             '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.
  perform bdc_field       using 'P1001-SCLAS'                              'K'.
  perform bdc_field       using 'P1001-SOBID'                              sobid."'01001010501000'.

  perform bdc_dynpro      using 'MP100100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P1001-BEGDA'                              begda." '12.09.2013'.
  perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P1001-RSIGN'                              'A'.
  perform bdc_field       using 'P1001-RELAT'                              '011'.
  perform bdc_field       using 'P1001-SCLAS'                              'K'.
  perform bdc_field       using 'P1001-SOBID'                              sobid."'01001010501000'.

  perform bdc_dynpro      using 'MP100100' '5010'.
  perform bdc_field       using 'BDC_CURSOR'                              'PKEYK-KOSTL'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'PKEYK-KOSTL'                             wa_upload-kostl." '100101050'.
  perform bdc_field       using 'PKEYK-KOKRS'                              '1000'.

  perform bdc_dynpro      using 'SAPMH5A0' '5100'.
  perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
  perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
  perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50002141'.
*perform bdc_transaction using 'PO13'.



*
*  PERFORM BDC_DYNPRO      USING 'SAPMH5A0' '5100'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'PM0D1-SEARK'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'PPHDR-PLVAR'                              '01'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             PLANS_NUM ." '50001204'.
*  PERFORM BDC_FIELD       USING 'PM0D1-TIMR6'                              'X'.
*  PERFORM BDC_FIELD       USING 'PPHDR-BEGDA'                             BEGDA." '01.01.1800'.
*  PERFORM BDC_FIELD       USING 'PPHDR-ENDDA'                              '31.12.9999'.
*
*  PERFORM BDC_DYNPRO      USING 'SAPMH5A0' '5100'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=INSE'.
*  PERFORM BDC_FIELD       USING 'PPHDR-PLVAR'                              '01'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             PLANS_NUM." '50001204'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
*  PERFORM BDC_FIELD       USING 'PM0D1-TIMR6'                              'X'.
*  PERFORM BDC_FIELD       USING 'PPHDR-BEGDA'                             BEGDA." '01.01.1800'.
*  PERFORM BDC_FIELD       USING 'PPHDR-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'MARKFELD(02)'                              'X'.
*
*  PERFORM BDC_DYNPRO      USING 'MP100100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P1001-RELAT'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'P1001-BEGDA'                             BEGDA." '19.09.2013'.
*  PERFORM BDC_FIELD       USING 'P1001-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P1001-RSIGN'                              'A'.
*  PERFORM BDC_FIELD       USING 'P1001-RELAT'                              '011'.
*
*  PERFORM BDC_DYNPRO      USING 'MP100100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P1001-SOBID'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'P1001-BEGDA'                             BEGDA." '19.09.2013'.
*  PERFORM BDC_FIELD       USING 'P1001-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P1001-RSIGN'                              'A'.
*  PERFORM BDC_FIELD       USING 'P1001-RELAT'                              '011'.
*  PERFORM BDC_FIELD       USING 'P1001-SCLAS'                              'K'.
*
*  DATA: T_SOBID TYPE HRP1001-SOBID .
*
*  CONCATENATE WA_UPLOAD-KOSTL '1000' INTO T_SOBID.
*
*  PERFORM BDC_FIELD       USING 'P1001-SOBID'                             T_SOBID." '01001055151000'.
*
*  PERFORM BDC_DYNPRO      USING 'MP100100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P1001-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=UPD'.
*  PERFORM BDC_FIELD       USING 'P1001-BEGDA'                             BEGDA." '19.09.2013'.
*  PERFORM BDC_FIELD       USING 'P1001-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P1001-RSIGN'                              'A'.
*  PERFORM BDC_FIELD       USING 'P1001-RELAT'                              '011'.
*  PERFORM BDC_FIELD       USING 'P1001-SCLAS'                              'K'.
*  PERFORM BDC_FIELD       USING 'P1001-SOBID'                              T_SOBID."'01001055151000'.
*
*  PERFORM BDC_DYNPRO      USING 'MP100100' '5010'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'PKEYK-KOSTL'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=UPD'.
*  PERFORM BDC_FIELD       USING 'PKEYK-KOSTL'                             WA_UPLOAD-KOSTL." '100105515'.
*  PERFORM BDC_FIELD       USING 'PKEYK-KOKRS'                             '1000'.
*
*  PERFORM BDC_DYNPRO      USING 'SAPMH5A0' '5100'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=BACK'.
*  PERFORM BDC_FIELD       USING 'PPHDR-PLVAR'                              '01'.
*  PERFORM BDC_FIELD       USING 'PM0D1-SEARK'                             PLANS_NUM." '50001204'.




  clear: t_msg.
  if bdcdata is not initial.

    call transaction 'PO13' using bdcdata
          mode tran_mode
          update 'S'
          messages into t_msg .


    loop at t_msg into w_msg where msgtyp = 'E'.
      format color 6 intensified off.
      write : / 'ERROR in COST CENTER Updation in Tcode PO13' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
    endloop.
    if t_msg is not initial.
      read table t_msg into w_msg with key msgtyp = 'E'.
      if sy-subrc <> 0.
        format color 5 intensified off.
        write : / text-019 , wa_upload-pernr."USRID1 ."'COST CENTER Updated Successfully for User ID', WA_UPLOAD-USRID1.
      endif.
    endif.
    clear: t_msg , w_msg.

  endif.

  clear:bdcdata, bdcdata[].
  refresh: bdcdata.

  data: temp_sobid type hrp1001-sobid,
        temp_orgeh type pa0001-orgeh.

  clear: temp_sobid , temp_orgeh.

  if wa_upload-orgeh is not initial.
    select single sobid
    from hrp1001
      into temp_sobid
      where objid = plans_num
      and rsign = 'A'
      and otype = 'S'
      and plvar = '01'
      and relat = '003'
      and endda = '99991231'
      and sclas = 'O'.

    if sy-subrc = 0.
      temp_orgeh = temp_sobid(08).
      if wa_upload-orgeh <> temp_orgeh.

        perform bdc_dynpro      using 'SAPMH5A0' '5100'.
        perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
        perform bdc_field       using 'PM0D1-SEARK'                             plans_num ." '50001204'.
        perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
        perform bdc_field       using 'PPHDR-BEGDA'                             begda." '01.01.1800'.
        perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.

        perform bdc_dynpro      using 'SAPMH5A0' '5100'.
        perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
        perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
        perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50001204'.
        perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(02)'.
        perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
        perform bdc_field       using 'PPHDR-BEGDA'                             begda." '01.01.1800'.
        perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
        perform bdc_field       using 'MARKFELD(02)'                              'X'.

        perform bdc_dynpro      using 'MP100100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P1001-RELAT'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'P1001-BEGDA'                             begda." '19.09.2013'.
        perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
        perform bdc_field       using 'P1001-RSIGN'                              'A'.
        perform bdc_field       using 'P1001-RELAT'                              '003'.

        perform bdc_dynpro      using 'MP100100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P1001-SOBID'.
        perform bdc_field       using 'BDC_OKCODE'                              '/00'.
        perform bdc_field       using 'P1001-BEGDA'                             begda." '19.09.2013'.
        perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
        perform bdc_field       using 'P1001-RSIGN'                              'A'.
        perform bdc_field       using 'P1001-RELAT'                              '003'.
        perform bdc_field       using 'P1001-SCLAS'                              'O'.
        perform bdc_field       using 'P1001-SOBID'                              wa_upload-orgeh.

        perform bdc_dynpro      using 'MP100100' '2000'.
        perform bdc_field       using 'BDC_CURSOR'                              'P1001-BEGDA'.
        perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
        perform bdc_field       using 'P1001-BEGDA'                             begda." '19.09.2013'.
        perform bdc_field       using 'P1001-ENDDA'                              '31.12.9999'.
        perform bdc_field       using 'P1001-RSIGN'                              'A'.
        perform bdc_field       using 'P1001-RELAT'                              '003'.
        perform bdc_field       using 'P1001-SCLAS'                              'O'.
        perform bdc_field       using 'P1001-SOBID'                              wa_upload-orgeh.


        perform bdc_dynpro      using 'SAPMH5A0' '5100'.
        perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
        perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
        perform bdc_field       using 'PM0D1-SEARK'                             plans_num." '50001204'.

        clear: t_msg.

        if bdcdata is not initial.

          call transaction 'PO13' using bdcdata
                mode tran_mode
                update 'S'
                messages into t_msg .


          loop at t_msg into w_msg where msgtyp = 'E'.
            format color 6 intensified off.
            write : / 'ERROR in ORG UNIT Updation in Tcode PO13' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
          endloop.
          if t_msg is not initial.
            read table t_msg into w_msg with key msgtyp = 'E'.
            if sy-subrc <> 0.
              format color 5 intensified off.
              write : / text-020 , wa_upload-pernr."USRID1 ."'Org Unit Updated Successfully for User ID', WA_UPLOAD-USRID1.
            endif.
          endif.
          clear: t_msg , w_msg.

        endif.

        clear:bdcdata, bdcdata[].
        refresh: bdcdata.

      endif.
    endif.
  endif.
endform.                    " UPDATE_PO13
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT008_INTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_it008_inter .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).

  clear: it_pa0008, wa_pa0008, begda , i_upload008 , wa_upload008.

*I_UPLOAD008

  data: lga01_code type z6hr_ppl_2_sap-sap.

  loop at i_upload into wa_upload08 where usrid1 = wa_upload-usrid1 and lga01 is not initial
    and ( lga01 = 'I_BAS' or lga01 = 'I_CAR' or lga01 = 'I_CONV' or lga01 = 'I_EDUA' "OR LGA01 = 'I_ENTA'
        or lga01 = 'I_HRA' or lga01 = 'I_OTHA' or lga01 = 'I_SPLA') and massn = action_type."'TAB'."'TAB'. "OR LGA01 = 'I_GRAT'

    clear : trfar,trfgb, trfgr.
    if wa_upload-trfar  is initial .
      select single trfar from pa0008 into wa_upload-trfar
      where pernr = test_pernr
      and endda =  '99991231'.
      if sy-subrc <> 0 . clear : wa_upload-trfar. endif.
      trfar = wa_upload-trfar.
    else.
      select single sap  from z6hr_ppl_2_sap    into trfar  where tabname = 'PA0008' and ppl_sft = wa_upload-trfar.
      if sy-subrc <> 0 . clear:trfar. endif.
    endif.

    if wa_upload-trfgb  is initial .

      select single trfgb from pa0008 into  wa_upload-trfgb
        where pernr = test_pernr
        and endda =  '99991231'.
      if sy-subrc <> 0 . clear : wa_upload-trfgb . endif.
      trfgb = wa_upload-trfgb.
    else.
      select single sap  from z6hr_ppl_2_sap    into trfgb  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgb.
      if sy-subrc <> 0 . clear:trfgb. endif.
    endif.

*    SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO TRFAR  WHERE TABNAME = 'PA0008' AND PPL_SFT = WA_UPLOAD-TRFAR.
*    IF SY-SUBRC <> 0 . CLEAR:TRFAR. ENDIF.
*
*    SELECT SINGLE SAP  FROM Z6HR_PPL_2_SAP    INTO TRFGB  WHERE TABNAME = 'PA0008' AND PPL_SFT = WA_UPLOAD-TRFGB.
*    IF SY-SUBRC <> 0 . CLEAR:TRFGB. ENDIF.

    select single sap  from z6hr_ppl_2_sap    into trfgr  where tabname = 'PA0008' and ppl_sft = wa_upload-trfgr.
    if sy-subrc <> 0 . clear:trfgr. endif.

    select single sap  from z6hr_ppl_2_sap
    into trfst
    where tabname = 'PA0008'
    and field = 'TRFST'
    and ppl_sft = wa_upload-trfst.
    if sy-subrc <> 0 .
      clear:trfst.
    else.
      if trfst = 'MT'.
        clear: trfst.
      endif .
    endif.

    clear: dd, mm, yyyy.

    if wa_upload-begda is not initial.
      clear: begda, dd,mm ,yyyy.
      split wa_upload-begda at '/' into mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into begda.

    endif.

    move begda to wa_upload008-begda.
    move wa_upload08-pernr to wa_upload008-pernr.
    wa_upload008-endda = '31.12.9999'.
    wa_upload008-preas = ''. "Reason for Changing Master Data
    move trfar to wa_upload008-trfar. "Pay scale type
    move trfgb to wa_upload008-trfgb. "Pay Scale Area
    move trfgr to wa_upload008-trfgr. "Pay Scale Group
    move trfst to wa_upload008-trfst. "Pay Scale Level

    move wa_upload08-lga01 to wa_upload008-lga01.
    move wa_upload08-bet01 to wa_upload008-bet01.
    move wa_upload08-waers to wa_upload008-waers.

    clear: lga01_code.

    select single sap from z6hr_ppl_2_sap
      into lga01_code where tabname = 'PA0008'
                                   and field = 'LGA01'
                                   and ppl_sft =  wa_upload08-lga01.

    if sy-subrc <> 0 . clear: lga01_code. endif.
    condense  lga01_code.
    wa_upload008-lga01_code = lga01_code.
    if wa_upload008-bet01 > 0.
      append wa_upload008 to i_upload008.
    endif.
  endloop.

  sort i_upload008 by lga01_code.

  data: code type i.
  code = 0.

  loop at i_upload008 into wa_upload008 where pernr = wa_upload-pernr. "USRID1.

    code = code + 1.

    move wa_upload008-pernr to wa_pa0008-pernr.
    move wa_upload008-begda to wa_pa0008-begda.
    wa_pa0008-endda = '31.12.9999'.
    wa_pa0008-preas = ''. "Reason for Changing Master Data
*    MOVE TRFAR TO WA_PA0008-TRFAR. "Pay scale type
*    MOVE TRFGB TO WA_PA0008-TRFGB. "Pay Scale Area
*    MOVE TRFGR TO WA_PA0008-TRFGR. "Pay Scale Group
    move wa_upload008-trfar to wa_pa0008-trfar. "Pay scale type
    move wa_upload008-trfgb to wa_pa0008-trfgb. "Pay Scale Area
    move wa_upload008-trfgr to wa_pa0008-trfgr. "Pay Scale Group


    move wa_upload008-trfst to wa_pa0008-trfst. "Pay Scale Level

*      ON CHANGE OF WA_UPLOAD008-PERNR .
    if sy-tabix = 1.
      append wa_pa0008 to it_pa0008.
    endif.
*      ENDON.

    case code."WA_UPLOAD008-LGA01.
      when 1."'I_BAS'.                                         "X 10    "1001
        move wa_upload008-lga01 to wa_pa0008-lga01.
        move wa_upload008-bet01 to wa_pa0008-bet01.
      when 2."'I_HRA'."X 5                                              "1004
        move wa_upload008-lga01 to wa_pa0008-lga02.
        move wa_upload008-bet01 to wa_pa0008-bet02.
      when 3."'I_EDUA'."X 3                                            "1005
        move wa_upload008-lga01 to wa_pa0008-lga03.
        move wa_upload008-bet01 to wa_pa0008-bet03.
      when 4."'I_CONV'.                                        "X 11   "1006
        move wa_upload008-lga01 to wa_pa0008-lga04.
        move wa_upload008-bet01 to wa_pa0008-bet04.
      when 5."'I_SPLA'."X 8                                            "1007
        move wa_upload008-lga01 to wa_pa0008-lga05.
        move wa_upload008-bet01 to wa_pa0008-bet05.
      when 6."'I_CAR'."X 2                                             "1008
        move wa_upload008-lga01 to wa_pa0008-lga06.
        move wa_upload008-bet01 to wa_pa0008-bet06.
      when 7."'I_OTHA'.                                        "X 13   "1013
        move wa_upload008-lga01 to wa_pa0008-lga07.
        move wa_upload008-bet01 to wa_pa0008-bet07.
      when 8."'I_ENTA'.                                        "X 12   "1020
        move wa_upload008-lga01 to wa_pa0008-lga08.
        move wa_upload008-bet01 to wa_pa0008-bet08.
      when 9."'I_GRAT'."X 4                                            "3617
        move wa_upload008-lga01 to wa_pa0008-lga09.
        move wa_upload008-bet01 to wa_pa0008-bet09.
    endcase.

    if wa_upload008-lga01 is not initial.
      modify it_pa0008 from wa_pa0008 index 1 transporting lga01 bet01
                                                           lga02 bet02
                                                           lga03 bet03
                                                           lga04 bet04
                                                           lga05 bet05
                                                           lga06 bet06
                                                           lga07 bet07
                                                           lga08 bet08
                                                           lga09 bet09
                                                           lga10 bet10
                                                           lga11 bet11
                                                           lga12 bet12
                                                           lga13 bet13
                                                           lga14 bet14
                                                           lga15 bet15
                                                           lga16 bet16
                                                           lga17 bet17
                                                           lga18 bet18
                                                           lga19 bet19
                                                           lga20 bet20.
    endif.

  endloop.

  if it_pa0008 is not initial.
    export it_pa0008 from it_pa0008 to memory id 'ZHRBDC'.

    submit z6hr014c_infotype_008_upl_ppl
      with p_pernr = wa_upload-pernr                        "USRID1
      exporting list to memory
    and return.

    import ret from memory id 'ZHR_MSG'.
    if not ret[] is initial.
      loop at ret into wa_ret.        .
        if wa_ret-type = 'E'.
          format color 6 intensified off.
          write : /'ERROR',wa_ret-message ,wa_upload-pernr. "USRID1.
        else.
          format color 5 intensified off.
          write : /'SUCESS',wa_ret-message ,wa_upload-pernr. "USRID1.
        endif.
      endloop.
    else.
      format color 5 intensified off.
      write : /'Records SUCESSFULLY UPDATED for Infotype 0008 UserID: ', wa_upload-pernr. "USRID1.
    endif.
  else.
    format color 6 intensified off.
    write : /'ERROR:in updating Infotype 008 For: ',wa_upload-pernr. "USRID1.
  endif.
  clear: t_msg2 , w_msg.

endform.                    " UPDATE_IT008_INTER
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT017
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_it017 .
*  DATA : P0017 LIKE P0017.
  data : p0007begda type p0007-begda.
  data: begda(10).
  data : dd(02), mm(02),yyyy(04).
  data : return like bapireturn1.
  data : key like bapipakey.
  data: temp_pernr type pa0007-pernr.
  data: 17_begda type pa0017-begda.
  data: oper type pspar-actio.
  data : returne like bapireturn1 .
  data:infty_tab type infty_tab.


*  **************************************************************************************************
* if changes made in IT 0017
*  **************************************************************************************************

  if wa_upload-erkla is not initial and"  Reimbursement Group for Meals/Accommodations: Statutory
  wa_upload-ergru is not initial and" Reimbursement Group for Meals/Accomm. - Enterprise-Specific
  wa_upload-spebe is not initial." Employee Grouping for Travel Expense Type


    if  wa_upload-begda is not initial.
      clear: begda, dd,mm ,yyyy.
      split wa_upload-begda at '/' into  mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into begda.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

    endif.


    concatenate begda+06(04) begda+03(02) begda(02) into p0007begda  .

    clear: p0017 , return,key .
    p0017-pernr = wa_upload-pernr.
    p0017-begda = p0007begda.
    p0017-endda = '99991231'.
    p0017-erkla = wa_upload-erkla.
    p0017-ergru = wa_upload-ergru.
    p0017-spebe = wa_upload-spebe.


    clear :temp_pernr , oper ,17_begda.
    break 10106.
    select single pernr begda
      from pa0017
      into (temp_pernr , 17_begda)
      where pernr = wa_upload-pernr
      and endda = '99991231'.
    if sy-subrc = 0.
      oper = 'MOD'.
      p0017-begda = 17_begda.

      perform modify_it17.

    else.
      oper = 'INS'.


      call function 'BAPI_EMPLOYEE_ENQUEUE'
        exporting
          number = p0017-pernr
        importing
          return = returne.

      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty         = '0017'
          number        = p0017-pernr
          subtype       = p0017-subty
          objectid      = p0017-objps
          lockindicator = p0017-sprps
          validityend   = p0017-endda
          validitybegin = p0017-begda
          recordnumber  = p0017-seqnr
          record        = p0017
          operation     = oper "'MOD'
          tclas         = 'A'
          dialog_mode   = '0'
        importing
          return        = return
          key           = key.

      if return is not initial.
        format color 6 intensified off.
        write :/ 'Error Occurred in Infotype 0017 , FOR' , wa_upload-pernr.
      else.
        format color 5 intensified off.
        write :/ 'Records SUCESSFULLY UPDATED for Infotype 0017,  FOR' ,  wa_upload-pernr.
      endif.


      call function 'BAPI_EMPLOYEE_DEQUEUE'
        exporting
          number = p0017-pernr.

    endif.

  endif.

endform.                    " UPDATE_IT017
*&---------------------------------------------------------------------*
*&      Form  MODIFY_IT17
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_it17 .
  data: begda(10).
  data : dd(02), mm(02),yyyy(04).

  clear: bdcdata , bdcdata[].
  refresh bdcdata.

  if  p0017-begda is not initial.

*     CONCATENATE P0017-BEGDA+06(02) '.' P0017-BEGDA+04(02) '.' P0017-BEGDA(04) INTO BEGDA  .
    if  wa_upload-begda is not initial.
      clear: begda, dd,mm ,yyyy.
      split wa_upload-begda at '/' into  mm dd yyyy.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = dd
        importing
          output = dd.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = mm
        importing
          output = mm.

      concatenate dd '.' mm '.' yyyy into begda.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

    endif.
  endif.

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '143'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-CHOIC'.
  perform bdc_field       using 'RP50G-CHOIC'                              '17'.
  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
  perform bdc_field       using 'BDC_OKCODE'                              '=INS'."'=MOD'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '143'.
  perform bdc_field       using 'RP50G-TIMR6'                              'X'.
  perform bdc_field       using 'RP50G-CHOIC'                              'Travel Privileges'.
  perform bdc_dynpro      using 'MP001700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0017-SPEBE'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0017-BEGDA'                              begda."'01.11.2013'.
  perform bdc_field       using 'P0017-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0017-ERKLA'                              wa_upload-erkla."'8'.
  perform bdc_field       using 'P0017-ERGRU'                              wa_upload-ergru."'2'.
  perform bdc_field       using 'P0017-SPEBE'                              wa_upload-spebe."'B'.
  perform bdc_dynpro      using 'MP001700' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0017-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0017-BEGDA'                              begda."'01.11.2013'.
  perform bdc_field       using 'P0017-ENDDA'                              '31.12.9999'.
  perform bdc_field       using 'P0017-ERKLA'                              wa_upload-erkla."'8'.
  perform bdc_field       using 'P0017-ERGRU'                              wa_upload-ergru."'2'.
  perform bdc_field       using 'P0017-SPEBE'                              wa_upload-spebe."'B'.


  if bdcdata is not initial.
    call transaction 'PA30' using bdcdata mode tran_mode update 'S'
      messages into t_msg2 .

    read table t_msg2 into w_msg with key msgtyp = 'E'.
    if sy-subrc <> 0 .
      format color 5 intensified off.
      write : / text-023 , 'User ID' , wa_upload-usrid1. ."'Record Uploaded SUCESSFULLY for InfoType 0021 ,Subtype-', FAMSA , 'User ID' , WA_UPLOAD-USRID1.
    else.
      format color 6 intensified off.
      write :/ 'Error Occurred in Infotype 0017 Subtype -', 'USER ID:' , wa_upload-usrid1.
    endif.
  endif.
  clear:t_msg2.
  clear: bdcdata , bdcdata[] .
  refresh bdcdata.

endform.                    " MODIFY_IT17
*&---------------------------------------------------------------------*
*&      Form  REDESIGNATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form redesignation .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10), gbdat(10) , fgbdt(10).


  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.

*    CONCATENATE YYYY MM DD INTO PA0001DATE.

  endif.

  clear: dd, mm, yyyy.

*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              WA_UPLOAD-PERNR. " '10038'.
*  PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=COP'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              WA_UPLOAD-PERNR. " '10038'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'T582S-ITEXT(02)'.
*  PERFORM BDC_FIELD       USING 'RP50G-SELEC(02)'                          'X'.
*  PERFORM BDC_FIELD       USING 'RP50G-TIMR6'                              'X'.
*  PERFORM BDC_DYNPRO      USING 'MP000100' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'P0001-BEGDA'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '=UPD'.
*  PERFORM BDC_FIELD       USING 'P0001-BEGDA'                             BEGDA." '01.04.2014'.
*  PERFORM BDC_FIELD       USING 'P0001-ENDDA'                              '31.12.9999'.
**  PERFORM BDC_FIELD       USING 'P0001-BTRTL'                             'TH01'.
**  PERFORM BDC_FIELD       USING 'P0001-ABKRS'                              'TM'.
**  PERFORM BDC_FIELD       USING 'P0001-PLANS'                              '50000133'.
**  PERFORM BDC_FIELD       USING 'P0001-ORGEH'                              '50000102'.
*
*
*  IF BDCDATA IS NOT INITIAL.
*    CALL TRANSACTION 'PA30' USING BDCDATA MODE TRAN_MODE UPDATE 'S'
*      MESSAGES INTO T_MSG .
*  ENDIF.
*
*  LOOP AT T_MSG INTO W_MSG WHERE MSGTYP = 'E'.
*    CONDENSE W_MSG-MSGV1 NO-GAPS.
*    FORMAT COLOR 6 INTENSIFIED OFF.
*    CONDENSE WA_UPLOAD-PERNR NO-GAPS.
*    CONDENSE W_MSG-MSGV1 NO-GAPS.
*    WRITE : / 'ERROR:', W_MSG-MSGV1 ,',in Redesignation ACTION  ' ,'For:' , WA_UPLOAD-PERNR."USRID1 ."'.ERROR
*  ENDLOOP.
*
*  READ TABLE T_MSG INTO W_MSG WITH KEY MSGTYP = 'E'.
*  IF SY-SUBRC <> 0.
*    FORMAT COLOR 5 INTENSIFIED OFF.
*    WRITE : / TEXT-024 , WA_UPLOAD-PERNR.
*  ENDIF.
*  CLEAR: T_MSG , W_MSG.



  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-EINDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '06011575'.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                             WA_UPLOAD-PERNR." '6011575'.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'                              'RP50G-PERNR'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'                              '/00'.
*  PERFORM BDC_FIELD       USING 'RP50G-PERNR'                              '6011575'.
*  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'T529T-MNTXT(10)'.
  perform bdc_field       using 'BDC_OKCODE'                              '=PICK'.
  perform bdc_field       using 'RP50G-PERNR'                             wa_upload-pernr." '6011575'.
  perform bdc_field       using 'RP50G-SELEC(10)'                              'X'.

  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '/00'.
  perform bdc_field       using 'P0000-BEGDA'                              begda."'19.06.2014'.
  perform bdc_field       using 'P0000-MASSN'                              'I9'.
  perform bdc_field       using 'P0000-MASSG'                              '01'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PLANS'                              PLANS_NUM."'50000275'.
*  PERFORM BDC_FIELD       USING 'PSPAR-WERKS'                              'IN01'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PERSG'                              'M'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PERSK'                              'M8'.
  perform bdc_dynpro      using 'MP000000' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0000-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0000-BEGDA'                              begda."'19.06.2014'.
  perform bdc_field       using 'P0000-MASSN'                              'I9'.
  perform bdc_field       using 'P0000-MASSG'                              '01'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PLANS'                              PLANS_NUM."'50000275'.
*  PERFORM BDC_FIELD       USING 'PSPAR-WERKS'                              'IN01'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PERSG'                              'M'.
*  PERFORM BDC_FIELD       USING 'PSPAR-PERSK'                              'M8'.
  perform bdc_dynpro      using 'MP000100' '2000'.
  perform bdc_field       using 'BDC_CURSOR'                              'P0001-BEGDA'.
  perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
  perform bdc_field       using 'P0001-BEGDA'                             begda." '19.06.2014'.
  perform bdc_field       using 'P0001-ENDDA'                              '31.12.9999'.
*  PERFORM BDC_FIELD       USING 'P0001-BTRTL'                              'AN01'.
*  PERFORM BDC_FIELD       USING 'P0001-ABKRS'                              'IN'.
*  PERFORM BDC_FIELD       USING 'P0001-PLANS'                              '50000275'.
*  PERFORM BDC_FIELD       USING 'P0001-ORGEH'                              '50000246'.
  perform bdc_dynpro      using 'SAPMP50A' '2000'.
  perform bdc_field       using 'BDC_OKCODE'                              '/EBCK'.
  perform bdc_field       using 'BDC_CURSOR'                              'RP50G-PERNR'.
*  PERFORM BDC_TRANSACTION USING 'PA40'.

  if bdcdata is not initial.
    call transaction 'PA40' using bdcdata mode tran_mode update 'S'
      messages into t_msg .
  endif.

  loop at t_msg into w_msg where msgtyp = 'E'.
    condense w_msg-msgv1 no-gaps.
    format color 6 intensified off.
    condense wa_upload-pernr no-gaps.
    condense w_msg-msgv1 no-gaps.
    write : / 'ERROR:', w_msg-msgv1 ,',in Redesignation ACTION  ' ,'For:' , wa_upload-pernr."USRID1 ."'.ERROR
  endloop.

  read table t_msg into w_msg with key msgtyp = 'E'.
  if sy-subrc <> 0.
    format color 5 intensified off.
    write : / text-024 , wa_upload-pernr.
*      LOOP AT IT_PLANS INTO WA_PLANS.
*        IF SY-TABIX = 1.
*          WA_PLANS-PS_PLANS = WA_UPLOAD-PLANS.
*          WA_PLANS-DESCR = WA_UPLOAD-DESCRIPTION2.
*
*          MODIFY Z6HR_PS_SAP_POS FROM WA_PLANS .
*        ENDIF.
*      ENDLOOP.

  endif.
  clear: t_msg , w_msg.




endform.                    " REDESIGNATION
*&---------------------------------------------------------------------*
*&      Form  CHANGE_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form change_position .

  data : dd(02), mm(02),yyyy(04).
  data: begda(10).

  if  wa_upload-begda is not initial.
    clear: begda, dd,mm ,yyyy.
    split wa_upload-begda at '/' into  mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda.
  endif.

  clear: dd, mm, yyyy.

  data: wa_pos like z6hr_ps_sap_pos.

  data: temp_plans type pa0001-plans.

  clear: temp_plans.

*  SELECT SINGLE SAP FROM Z6HR_PPL_2_SAP INTO TEMP_PLANS WHERE  PPL_SFT = WA_UPLOAD-PLANS AND TABNAME = 'PA0001'.
  select single plans
    from z6hr_ps_sap_pos
    into temp_plans
    where pernr = wa_upload-pernr
    and ps_plans = wa_upload-plans.

  if sy-subrc <> 0.

    select single plans
    from pa0001
    into temp_plans
    where pernr = wa_upload-pernr
    and endda = '99991231'.
    if sy-subrc <> 0 . clear: temp_plans . endif.

    delete from z6hr_ps_sap_pos where pernr = wa_upload-pernr.

    wa_pos-pernr = wa_upload-pernr.
    wa_pos-plans = temp_plans.
    wa_pos-ps_plans = wa_upload-plans.
    wa_pos-descr = wa_upload-description2.

    insert z6hr_ps_sap_pos from wa_pos.
    commit work.

    if temp_plans is not initial.
********** change description of existing position ***********************

      perform bdc_dynpro      using 'SAPMH5A0' '5100'.
      perform bdc_field       using 'BDC_CURSOR'                              'PM0D1-SEARK'.
      perform bdc_field       using 'BDC_OKCODE'                              '/00'.
      perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
      perform bdc_field       using 'PM0D1-SEARK'                              temp_plans."'50001627'.
      perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
      perform bdc_field       using 'PPHDR-BEGDA'                              begda ."'26.06.2014'.
      perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
      perform bdc_dynpro      using 'SAPMH5A0' '5100'.
      perform bdc_field       using 'BDC_OKCODE'                              '=INSE'.
      perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
      perform bdc_field       using 'PM0D1-SEARK'                              temp_plans."'50001627'.
      perform bdc_field       using 'BDC_CURSOR'                              'TT_T777T-ITEXT(01)'.
      perform bdc_field       using 'PM0D1-TIMR6'                              'X'.
      perform bdc_field       using 'PPHDR-BEGDA'                              begda."'26.06.2014'.
      perform bdc_field       using 'PPHDR-ENDDA'                              '31.12.9999'.
      perform bdc_field       using 'MARKFELD(01)'                              'X'.
      perform bdc_dynpro      using 'MP100000' '2000'.
      perform bdc_field       using 'BDC_CURSOR'                              'P1000-STEXT'.
      perform bdc_field       using 'BDC_OKCODE'                              '=UPD'.
      perform bdc_field       using 'P1000-BEGDA'                             begda." '01.04.2014'.
      perform bdc_field       using 'P1000-ENDDA'                              '31.12.9999'.
      perform bdc_field       using 'P1000-SHORT'                             wa_upload-description2(12)." 'test PRO'.
      perform bdc_field       using 'P1000-STEXT'                             wa_upload-description2." 'Promotion without change designation'.
      perform bdc_dynpro      using 'SAPMH5A0' '5100'.
      perform bdc_field       using 'BDC_OKCODE'                              '=BACK'.
      perform bdc_field       using 'PPHDR-PLVAR'                              '01'.
      perform bdc_field       using 'PM0D1-SEARK'                             temp_plans." '50001627'.
*  PERFORM BDC_TRANSACTION USING 'PO13'.

      clear: t_msg , w_msg.
      if bdcdata is not initial.
        call transaction 'PO13' using bdcdata mode tran_mode update 'S'
          messages into t_msg .

        loop at t_msg into w_msg where msgtyp = 'E'.
          format color 6 intensified off.
          write : / 'ERROR: in change of Position Description For:' , wa_upload-pernr."USRID1 ."'.ERROR
        endloop.
        read table t_msg into w_msg with key msgtyp = 'E'.
        if sy-subrc <> 0.
          format color 5 intensified off.
          write : / text-025 , wa_upload-pernr.
        endif.
      endif.
      clear: t_msg , w_msg.
    endif .
  endif.

  clear: bdcdata, bdcdata[].
endform.                    " CHANGE_POSITION
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form send_mail .
* Data Declarations
  data: lt_mailsubject     type sodocchgi1.
  data: lt_mailrecipients  type standard table of somlrec90 with header line.
  data: lt_mailtxt         type standard table of soli      with header line.
  data: begin of wa_rec,
          email type zfi044_emailid-email,
        end of wa_rec,
        it_rec like table of wa_rec.

  data: cdate(10).
  data: zaction(20) , zname(120), zname1 type t500p-name1.

  select email from zfi044_emailid into table it_rec where egroup = 'HR_ACTIONS'.
  concatenate sy-datum+06(02) '.' sy-datum+04(02) '.' sy-datum(04) into cdate.

  loop at it_rec into wa_rec .
* Recipients
    lt_mailrecipients-rec_type  = 'U'.
    lt_mailrecipients-receiver = wa_rec-email."'pshinde-icc@modi.com'.
    append lt_mailrecipients .
    clear lt_mailrecipients .
  endloop.

  if wa_upload-massn = 'HIR' or wa_upload-massn = 'ADD' or wa_upload-massn = 'REH'.
    zaction = 'New Hire On:'.
  elseif  wa_upload-massn = 'TER' or wa_upload-massn = 'RET' or wa_upload-massn = 'SUS'.
    zaction = 'Separation On:'.
    select single werks persk ename
      from pa0001
      into (wa_upload-werks , wa_upload-persk , zname)
      where pernr =  wa_upload-pernr
      and endda = '99991231'.

  endif.

  lt_mailsubject-obj_name = 'HR_ACTION'.
  lt_mailsubject-obj_langu = sy-langu.

*NACHN TYPE PA0002-NACHN,"(40)(CHAR)  Last Name
*MIDNM TYPE PA0002-MIDNM,"(40)(CHAR)  Second Name
*VORNA TYPE PA0002-VORNA,"(40)(CHAR)  First Name
  if zname is initial.
    condense: wa_upload-vorna , wa_upload-midnm , wa_upload-nachn .
    concatenate wa_upload-vorna wa_upload-midnm wa_upload-nachn into zname separated by space.
    condense zname.
  endif.
  concatenate zaction zzbegda 'Employee:' wa_upload-pernr into lt_mailsubject-obj_descr separated by space.
*lt_mailsubject-obj_descr = ''.

*New person joining date 08/15/2014
*Details of new Employee -
*Employee Number: 06000016
*Employee Name: Mr. vashant kumar naik
*Employee Region: Thane
*Employee Location:
*Employee Grade : W1

  concatenate zaction zzbegda into lt_mailtxt.
  append lt_mailtxt.
  clear lt_mailtxt.
  if zaction = 'New Hire On:'.
    lt_mailtxt = 'Details of new Employee -'.
    append lt_mailtxt.
    clear lt_mailtxt.
  elseif zaction = 'Separation On:'.
    lt_mailtxt = 'Details of Employee -'.
    append lt_mailtxt.
    clear lt_mailtxt.
  endif.

  concatenate 'Employee Number:' wa_upload-pernr into lt_mailtxt.
  append lt_mailtxt.
  clear lt_mailtxt.

  concatenate 'Employee Name:' zname into lt_mailtxt.
  append lt_mailtxt.
  clear lt_mailtxt.

  select single name1
    from t500p into zname1
    where persa = wa_upload-werks.

  concatenate 'Employee Region:' zname1  into lt_mailtxt.
  append lt_mailtxt.
  clear lt_mailtxt.


  concatenate 'Employee Grade:'  wa_upload-persk into lt_mailtxt.
  append lt_mailtxt.
  clear lt_mailtxt.


  if zaction = 'New Hire On:'.
    concatenate 'Joining Date :' zzbegda into lt_mailtxt separated by space.
    append lt_mailtxt.
    clear lt_mailtxt.
  elseif zaction = 'Separation On:'.
    concatenate 'Separation date:' zzbegda into lt_mailtxt separated by space.
    append lt_mailtxt.
    clear lt_mailtxt.

  endif.

  data: zemail_105 type pa0105-usrid_long.

  select single usrid_long from pa0105 into zemail_105 where pernr =  wa_upload-pernr and subty = '0010' AND endda >= sy-datum.
  if sy-subrc = 0.
    concatenate 'Email :' zemail_105 into lt_mailtxt separated by space.
    append lt_mailtxt.
    clear lt_mailtxt.
  endif.


  lt_mailtxt = '|-------------------------------------------------------------------------------------------------|'.
  append lt_mailtxt. clear lt_mailtxt.

  lt_mailtxt = ''.
  append lt_mailtxt. clear lt_mailtxt.

* Send Mail
  call function 'SO_NEW_DOCUMENT_SEND_API1'
    exporting
      document_data              = lt_mailsubject
    tables
      object_content             = lt_mailtxt
      receivers                  = lt_mailrecipients
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      others                     = 8.
  if sy-subrc eq 0.
    commit work.
*   Push mail out from SAP outbox
    submit rsconn01 with mode = 'INT' and return.

    format color 5 intensified off.
    write : / 'Mail send Successfully!!!' , wa_upload-pernr .
    write: /'-------------------------------------------------------------------------------------------------------------'.

  else.
    format color 5 intensified off.
    write : / 'Error in Sending Mail.' , wa_upload-pernr .
    write: /'-------------------------------------------------------------------------------------------------------------'.
  endif.

endform.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_INDONET_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_indonet_master .
  data: it_atr like table of zatr_user_tmap,
        wa_atr like line of it_atr.

  data: zpernr_ind type zatr_user_tmap-user_id.
  data: zterr type tvgrt-bezei.

*CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*  EXPORTING
*    INPUT         = WA_UPLOAD-PERNR
* IMPORTING
*   OUTPUT        = WA_UPLOAD-PERNR
*          .

  zpernr_ind = wa_upload-pernr.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = zpernr_ind
    importing
      output = zpernr_ind.

  select * from zatr_user_tmap into table it_atr where user_id = zpernr_ind.
  if sy-subrc = 0.

* Data Declarations
    data: lt_mailsubject     type sodocchgi1.
    data: lt_mailrecipients  type standard table of somlrec90 with header line.
    data: lt_mailtxt         type standard table of soli      with header line.
    data: begin of wa_rec,
            email type zfi044_emailid-email,
          end of wa_rec,
          it_rec like table of wa_rec.

    data: cdate(10).
    data: zaction(20) , zname(120), zname1 type t500p-name1.

    select email from zfi044_emailid into table it_rec where egroup = 'INDONET'.
    concatenate sy-datum+06(02) '.' sy-datum+04(02) '.' sy-datum(04) into cdate.

    loop at it_rec into wa_rec .
* Recipients
      lt_mailrecipients-rec_type  = 'U'.
      lt_mailrecipients-receiver = wa_rec-email."'pshinde-icc@modi.com'.
      append lt_mailrecipients .
      clear lt_mailrecipients .
    endloop.

    select single werks persk ename
      from pa0001
      into (wa_upload-werks , wa_upload-persk , zname)
      where pernr =  wa_upload-pernr
      and endda = '99991231'.

    lt_mailsubject-obj_name = 'HR_INDONET'.
    lt_mailsubject-obj_langu = sy-langu.

*NACHN TYPE PA0002-NACHN,"(40)(CHAR)  Last Name
*MIDNM TYPE PA0002-MIDNM,"(40)(CHAR)  Second Name
*VORNA TYPE PA0002-VORNA,"(40)(CHAR)  First Name
    if zname is initial.
      condense: wa_upload-vorna , wa_upload-midnm , wa_upload-nachn .
      concatenate wa_upload-vorna wa_upload-midnm wa_upload-nachn into zname separated by space.
      condense zname.
    endif.
    concatenate 'Separation on:' zzbegda 'Employee:' wa_upload-pernr into lt_mailsubject-obj_descr separated by space.
*lt_mailsubject-obj_descr = ''.

*New person joining date 08/15/2014
*Details of new Employee -
*Employee Number: 06000016
*Employee Name: Mr. vashant kumar naik
*Employee Region: Thane
*Employee Location:
*Employee Grade : W1

    concatenate zaction zzbegda into lt_mailtxt.
    append lt_mailtxt.
    clear lt_mailtxt.

    lt_mailtxt = 'Details of Employee -'.
    append lt_mailtxt.
    clear lt_mailtxt.


    concatenate 'Employee Number:' wa_upload-pernr into lt_mailtxt.
    append lt_mailtxt.
    clear lt_mailtxt.

    concatenate 'Employee Name:' zname into lt_mailtxt.
    append lt_mailtxt.
    clear lt_mailtxt.

    select single name1
      from t500p into zname1
      where persa = wa_upload-werks.

    concatenate 'Employee Region:' zname1  into lt_mailtxt.
    append lt_mailtxt.
    clear lt_mailtxt.


    concatenate 'Employee Grade:'  wa_upload-persk into lt_mailtxt.
    append lt_mailtxt.
    clear lt_mailtxt.

    concatenate 'Separation date:' zzbegda into lt_mailtxt separated by space.
    append lt_mailtxt.
    clear lt_mailtxt.

    data: zemail_105 type pa0105-usrid_long.

    select single usrid_long from pa0105 into zemail_105 where pernr =  wa_upload-pernr and subty = '0010' AND endda >= sy-datum.
    if sy-subrc = 0.
      concatenate 'Email :' zemail_105 into lt_mailtxt separated by space.
      append lt_mailtxt.
      clear lt_mailtxt.
    endif.


    concatenate 'Below Territory Records deleted from ATR' 'User-Territory Master .' into lt_mailtxt separated by space.
    append lt_mailtxt.
    clear lt_mailtxt.

    loop at it_atr into wa_atr.
      clear: zterr.
      select single bezei from tvgrt into zterr where spras = sy-langu
        and vkgrp = wa_atr-vkgrp.

      concatenate 'Territory :' wa_atr-vkgrp ',' zterr into lt_mailtxt separated by space.
      append lt_mailtxt.
      clear lt_mailtxt.

    endloop.

    lt_mailtxt = '|-------------------------------------------------------------------------------------------------|'.
    append lt_mailtxt. clear lt_mailtxt.

    lt_mailtxt = ''.
    append lt_mailtxt. clear lt_mailtxt.
    data: wa_user_m like zatr_user_m.
    select single * from zatr_user_m into wa_user_m where user_id = zpernr_ind.
    if sy-subrc = 0.
      update zatr_user_m  set status = 'X'
      where user_id = zpernr_ind.
      commit work.
    endif.



    delete from zatr_user_tmap where user_id = zpernr_ind.
    if sy-subrc = 0.

* Send Mail
      call function 'SO_NEW_DOCUMENT_SEND_API1'
        exporting
          document_data              = lt_mailsubject
        tables
          object_content             = lt_mailtxt
          receivers                  = lt_mailrecipients
        exceptions
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          others                     = 8.
      if sy-subrc eq 0.
        commit work.
*   Push mail out from SAP outbox
        submit rsconn01 with mode = 'INT' and return.

        format color 5 intensified off.
        write : / 'Mail send Successfully!!!' , wa_upload-pernr .
        write: /'-------------------------------------------------------------------------------------------------------------'.

      else.
        format color 5 intensified off.
        write : / 'Error in Sending Mail.' , wa_upload-pernr .
        write: /'-------------------------------------------------------------------------------------------------------------'.
      endif.
    endif.
  endif.

endform.                    " UPDATE_INDONET_MASTER
*&---------------------------------------------------------------------*
*&      Form  CREATE_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_user .

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1050'.

  perform bdc_field using 'BDC_OKCODE' '=COPY'.
  perform bdc_field using 'SUID_ST_BNAME-BNAME' wa_upload-pernr.

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1200'.

  perform bdc_field using 'BDC_OKCODE' '=COPY'.
  perform bdc_field using 'GV_COPY_UNAME_SRC' '1855'. "wa_table-bname1
  perform bdc_field using 'GV_COPY_UNAME_DST' wa_upload-pernr.

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1100'.

  perform bdc_field using 'BDC_OKCODE' '=ADDR'.
  concatenate wa_upload-vorna wa_upload-nachn into gv_name separated by space.
  perform bdc_field using 'SUID_ST_NODE_LOGONDATA-USERALIAS' gv_name.
  perform bdc_field using 'SUID_ST_NODE_LOGONDATA-USTYP' 'C'. "'S'  changes on 28.01.2019 as instructed by VENU
  perform bdc_field using 'SUID_ST_NODE_PASSWORD_EXT-PASSWORD' 'Init123$'.
  perform bdc_field using 'SUID_ST_NODE_PASSWORD_EXT-PASSWORD2' 'Init123$'.
  clear gv_date.
  concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4)
         into gv_date.
  perform bdc_field using 'SUID_ST_NODE_LOGONDATA-GLTGV' gv_date.
  clear gv_date.
  gv_date = '31.12.9999'.
  perform bdc_field using 'SUID_ST_NODE_LOGONDATA-GLTGB' gv_date.

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1100'.

  perform bdc_field using 'BDC_OKCODE' '=ACTG'.

  concatenate gv_title '.' into gv_title.
  read table it_tsad3t with key title_medi = gv_title.
  if sy-subrc eq 0.
    perform bdc_field using 'SUID_ST_NODE_PERSON_NAME_EXT-TITLE_MEDI' it_tsad3t-title.
  endif.

  perform bdc_field using 'SUID_ST_NODE_PERSON_NAME-NAME_LAST' wa_upload-nachn.
  perform bdc_field using 'SUID_ST_NODE_PERSON_NAME-NAME_FIRST' wa_upload-vorna.
  perform bdc_field using 'SUID_ST_NODE_COMM_DATA-MOB_NUMBER' wa_upload-fanat.
  perform bdc_field using 'SUID_ST_NODE_COMM_DATA-SMTP_ADDR' wa_upload-usrid3.

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1100'.

  perform bdc_field using 'BDC_OKCODE' '=LAW'.

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1100'.

  perform bdc_field using 'BDC_OKCODE' '=TYP'.
*  perform bdc_field using 'SUID_ST_NODE_UCLASS-LIC_TYPE' 'CE' . ""'CD'. "change in licence type mail received from venu on 26.09.2018
  perform bdc_field using 'SUID_ST_NODE_UCLASS-LIC_TYPE' 'CD' . " change as CD as mail from VENU on March 8, 2019 7:02 PM

  perform bdc_dynpro using 'SAPLSUID_MAINTENANCE' '1100'.

  perform bdc_field using 'BDC_OKCODE' '=UPD'.
*  perform bdc_field using 'SUID_ST_NODE_UCLASS-LIC_TYPE' 'CE' . "" 'CD'.
  perform bdc_field using 'SUID_ST_NODE_UCLASS-LIC_TYPE' 'CD' .  "'CE' . "" 'CD'." change as CD as mail from VENU on March 8, 2019 7:02 PM

  clear: gv_remark, t_msg[].
  call transaction 'SU01' using bdcdata
                           mode tran_mode
                         update 'S'
                       messages into t_msg.

*  IF t_msg[] IS NOT INITIAL.
*    READ TABLE t_msg INTO w_msg
*                     WITH KEY msgtyp = 'S'
*                              msgid  = '15'
*                              msgnr  = '239'.
  read table t_msg into w_msg with key msgtyp = 'E'.

  if sy-subrc <> 0.

    call function 'MESSAGE_TEXT_BUILD'
      exporting
        msgid               = w_msg-msgid
        msgnr               = w_msg-msgnr
        msgv1               = w_msg-msgv1
        msgv2               = w_msg-msgv2
        msgv3               = w_msg-msgv3
        msgv4               = w_msg-msgv4
      importing
        message_text_output = gv_remark.

    gv_remark = text-i10.
    replace '&' in gv_remark with wa_upload-pernr.
    write: / gv_remark.

    gv_user = wa_upload-pernr.

    call function 'BAPI_USER_GET_DETAIL'
      exporting
        username       = gv_user
      tables
        activitygroups = it_groups
        return         = it_return.

    loop at it_groups into wa_groups.
      wa_groups-from_dat = sy-datum.
      modify it_groups from wa_groups transporting from_dat.
    endloop.

****Below changes is to assigned new role based on grade of employee - mail from venu on Thursday, February 21, 2019 9:20 AM
    CLEAR: wa_groups.
    BREAK 10106.
    if wa_upload-persk(01) <> 'T'.
    CONCATENATE 'ZGRADE' wa_upload-persk INTO wa_groups-AGR_NAME.
    CONCATENATE 'GRADE' wa_upload-persk INTO wa_groups-AGR_TEXT .
    ELSE.
      wa_groups-AGR_NAME = 'ZGRADE5C'.
      wa_groups-AGR_TEXT = 'GRADE5C'.
    endif.
    wa_groups-FROM_DAT = sy-datum.
    wa_groups-TO_DAT = '99991231'.
    append wa_groups to it_groups.

****Below changes is to assigned new role related to fund mgmt - mail from venu on - Wed 04/11/2020 16:35-- AUTO ROLE ATTACHMENT DURING HIRING
      CLEAR: wa_groups.
      wa_groups-AGR_NAME = 'ZFM_BUDGET_SUPP_TRFR'.
      wa_groups-AGR_TEXT = 'Budget Maintain – Supplement & Transfer'.
      wa_groups-FROM_DAT = sy-datum.
      wa_groups-TO_DAT = '99991231'.
      append wa_groups to it_groups.

      CLEAR: wa_groups.
      wa_groups-AGR_NAME = 'ZFM_REPORT'.
      wa_groups-AGR_TEXT = 'ZFM_REPORT'.
      wa_groups-FROM_DAT = sy-datum.
      wa_groups-TO_DAT = '99991231'.
      append wa_groups to it_groups.

      CLEAR: wa_groups.
      wa_groups-AGR_NAME = 'ZFM_MASTER_DISPLAY'.
      wa_groups-AGR_TEXT = 'ZFM_MASTER_DISPLAY'.
      wa_groups-FROM_DAT = sy-datum.
      wa_groups-TO_DAT = '99991231'.
      append wa_groups to it_groups.



    call function 'BAPI_USER_ACTGROUPS_ASSIGN'
      exporting
        username       = gv_user
      tables
        activitygroups = it_groups
        return         = it_return.

    read table it_return into wa_return
                         with key type = 'S'
                                    id = '01'
                                number = '048'.
    if sy-subrc eq 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
    endif.

  else.

    read table t_msg into w_msg index 1.
    if sy-subrc eq 0.

      call function 'MESSAGE_TEXT_BUILD'
        exporting
          msgid               = w_msg-msgid
          msgnr               = w_msg-msgnr
          msgv1               = w_msg-msgv1
          msgv2               = w_msg-msgv2
          msgv3               = w_msg-msgv3
          msgv4               = w_msg-msgv4
        importing
          message_text_output = gv_remark.

      write: / gv_remark.

    endif.
  endif.
*  ENDIF.  "IF T_MSG[] IS NOT INITIAL.

endform.
*&---------------------------------------------------------------------*
*&      Form  DELIMIT_USER_LOGON
*&---------------------------------------------------------------------*
*       IRDK930518 => HR: S_K: ZHR_PS: Delimit user logon/roles on user separation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delimit_user_logon.
  " 1. Delimit user logon validity date
  " 2. Delimit user role end date
  data lv_sep_date type syst-datum.  " Separation Date
  data: dd(2), mm(2), yyyy(4).
  data lv_start_date(10) type c.
  data username   type bapibname-bapibname.
  data logondata  type bapilogond.
  data logondatax type bapilogonx.
  data return     type standard table of bapiret2 with header line.
  data activitygroups type standard table of bapiagr with header line.

  check wa_upload-pernr is not initial.

  " Clear data
  clear: lv_sep_date, lv_start_date, username, logondata, logondatax, activitygroups.
  clear: dd, mm, yyyy.
  refresh return.

  split wa_upload-begda at '/' into mm dd yyyy.
  lv_start_date = dd && '.' && mm && '.' && yyyy.

  " Convert date in sap format
  call function 'CONVERT_DATE_TO_INTERNAL'
    exporting
      date_external            = lv_start_date
      accept_initial_date      = abap_false
    importing
      date_internal            = lv_sep_date
    exceptions
      date_external_is_invalid = 1
      others                   = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  check lv_sep_date is not initial.

  username = wa_upload-pernr.
  shift username left deleting leading '0'.   " IRDK930680, required for bapi below since other wise query on usr02 in the bapi fails
  logondata-gltgb = lv_sep_date - 1.  " Valid upto, " 1 day prior to date of separation - IRDK930682
  logondatax-gltgb = abap_true.

  call function 'BAPI_USER_CHANGE'  " Internal commit
    exporting
      username   = username
      logondata  = logondata
      logondatax = logondatax
    tables
      return     = return.

  " Delimit user roles
  " 1. BAPI_USER_ACTGROUPS_ASSIGN deletes all user roles and adds the new ones in activitygroups
  " 2. So we first get all the current roles using BAPI_USER_GET_DETAIL, update the 'to date' field only and pass the updated table
  "    to BAPI_USER_ACTGROUPS_ASSIGN so that all the curretn roles are preserved but with changed end date
  refresh return.
  call function 'BAPI_USER_GET_DETAIL'
    exporting
      username       = username
      cache_results  = abap_true
    tables
      activitygroups = activitygroups
      return         = return.

  check activitygroups[] is not initial.
  loop at activitygroups.
    activitygroups-to_dat = lv_sep_date - 1.  " IRDK930792
    modify activitygroups transporting to_dat.
  endloop.

  refresh return.
  call function 'BAPI_USER_ACTGROUPS_ASSIGN'  " Internal Commit
    exporting
      username       = username
    tables
      activitygroups = activitygroups
      return         = return.
endform.
*&---------------------------------------------------------------------*
*&      Form  SEND_PRECONF_MAIL
*&---------------------------------------------------------------------*
*       6010859
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form send_preconf_mail .
  data: begin of wa_date,
          on_date    type sy-datum,
          mail_subj  type string,
          t_contents type standard table of solisti1,
        end of wa_date ,
        it_date like standard table of wa_date.

  data: begin of wa_rec ,
          rec type somlreci1-receiver,
        end of wa_rec ,
        it_rec like standard table of wa_rec.

  data: it_email like table of zfi044_emailid,
        wa_email like line of it_email.

  data: due_for_text(10) type c,
        months(2)        type c,
        on_b4_date       type sy-datum.

  clear: wa_date, it_date, it_email , wa_email , wa_rec , it_rec.

  data : dd(02), mm(02), yyyy(04).
  data: begda_ext(10),
        begda_int type sy-datum.

  if  wa_upload-begda is not initial.
    clear: begda_ext, dd, mm, yyyy.
    split wa_upload-begda at '/' into mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda_ext.

    clear begda_int.
    call function 'CONVERT_DATE_TO_INTERNAL'
      exporting
        date_external            = begda_ext
      importing
        date_internal            = begda_int
      exceptions
        date_external_is_invalid = 1
        others                   = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endif.

  if wa_upload-persg = 'M'. " for management
*****  get mail sending date
    wa_date-on_date = begda_int + 75. " 3 rd month " Days will be confirmed later, might have to send a few days prior to 90
    " How many days prior in each case will be confirmed by HR in each case below
    wa_date-mail_subj = wa_upload-pernr && ` ` && '1st Confirmation Pending (3 mnths)'. " this is confirmed, i.e no of months is fixed by HR

    clear: due_for_text, months.
    due_for_text = '3rd month'.
    months = '3'.
    perform construct_mail_body tables wa_date-t_contents using due_for_text months.

    append wa_date to it_date.
    clear: wa_date.
*--------------------------------------------------------------------*
    wa_date-on_date = begda_int + 150. " 5 th month
    wa_date-mail_subj = wa_upload-pernr && ` ` && '2nd Confirmation Pending (5 mnths)'.

    clear: due_for_text, months.
    refresh wa_date-t_contents.
    due_for_text = '5th month'.
    months = '5'.
    perform construct_mail_body tables wa_date-t_contents using due_for_text months.

    append wa_date to it_date.
    clear: wa_date.

  elseif wa_upload-persg = 'T'. " for trainee
******  *****  get mail sending date
    wa_date-on_date = begda_int + 90. " 3 rd month
    wa_date-mail_subj = wa_upload-pernr && ` ` && '1st Confirmation Pending (3 mnths)'.

    clear: due_for_text, months.
    refresh wa_date-t_contents.
    due_for_text = '1st'.
    months = '3'.
    perform construct_mail_body tables wa_date-t_contents using due_for_text months.

    append wa_date to it_date.
    clear: wa_date.
*--------------------------------------------------------------------*
    wa_date-on_date = begda_int + 210. " 7 th month
    wa_date-mail_subj = wa_upload-pernr && ` ` && '2nd Confirmation Pending (7 mnths)'.

    clear: due_for_text, months.
    refresh wa_date-t_contents.
    due_for_text = '2nd'.
    months = '7'.
    perform construct_mail_body tables wa_date-t_contents using due_for_text months.

    append wa_date to it_date.
    clear: wa_date.
*--------------------------------------------------------------------*
    wa_date-on_date = begda_int + 300. " 10 th month
    wa_date-mail_subj = wa_upload-pernr && ` ` && '3rd Confirmation Pending (10 mnths)'.

    clear: due_for_text, months.
    refresh wa_date-t_contents.
    due_for_text = '3rd'.
    months = '10'.
    perform construct_mail_body tables wa_date-t_contents using due_for_text months.

    append wa_date to it_date.
    clear: wa_date.
  endif.

  " Get recievers
*****************************************************************************
***** get email id of HR key user ********************
  select * from zfi044_emailid into table it_email where egroup = 'ZHR_PS_CON'.
  loop at it_email into wa_email .
    wa_rec-rec = wa_email-email .
    append wa_rec to it_rec.
    clear: wa_rec.
  endloop.
***************   get email id of line manager ***********
  select single usrid_long from pa0105
  into wa_rec-rec
  where pernr = wa_upload-mstbr
  and subty = '0010' and endda > sy-datum .
  if sy-subrc = 0.
    append wa_rec to it_rec.
    clear: wa_rec.
  endif.

  " Construct recievers table
******* send mail
  clear: p_rec , p_rec[].
  loop at it_rec into wa_rec.
    clear: p_rec.
    p_rec-sign = 'I'.
    p_rec-option = 'EQ'.
    p_rec-low = wa_rec-rec.
    append p_rec.
  endloop.

  break 10106.
  loop at it_date into wa_date .

    export wa_date-t_contents from wa_date-t_contents to memory id 'MAIL_BODY'. " imported in below program

    submit zsend_mail_future
      with p_date = wa_date-on_date
      with m_subj = wa_date-mail_subj
      with persg  = wa_upload-persg
      with p_rec in p_rec and return.

    " IHDK900610
    data: lv_result type c length 1.
    clear lv_result.
    import lv_result to lv_result from memory id 'EMAIL_RES'. " exported from above program
    if lv_result eq 'F'.
      data(lv_failure) = abap_true. " do not reset this flag in the loop, this allows failure checks after the loop
    endif.
    free memory id: 'MAIL_BODY', 'EMAIL_RES'.
    " End IHDK900610

    clear wa_date.
    refresh wa_date-t_contents[].
  endloop.

  " IHDK900610
  if lv_failure eq abap_false.
    format color 5 intensified off.
    write: / 'Pre-confirmation email scheduled successfully'.
    format reset.
  endif.
  clear lv_failure.

endform.

                                                            " 6010859
form construct_mail_body tables t_contents structure solisti1
                         using due_for_text
                               months.
  data: w_contents type solisti1.
  data: name(100) type c.
  data: begda_int type sy-datum.
  data: conf_date_int type sy-datum.
  data: conf_date_ext(10) type c.
  data : dd(02), mm(02), yyyy(04).
  data: begda_ext(10).

  if  wa_upload-begda is not initial.
    clear: begda_ext, dd, mm, yyyy.
    split wa_upload-begda at '/' into mm dd yyyy.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = dd
      importing
        output = dd.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = mm
      importing
        output = mm.

    concatenate dd '.' mm '.' yyyy into begda_ext.
  endif.

  clear name.
  name = |{ wa_upload-anred } { wa_upload-vorna } { wa_upload-midnm } { wa_upload-nachn }|.
  condense name.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">Dear Manager, </span></p>'.
  append w_contents to t_contents.

*clear w_contents.
*w_contents-line = '<p><span style="color: black;">&nbsp;</span></p>'.
*append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">We would like to inform you that your team member' && ` ` && name && ` ` &&
  'is due for' && ` ` && due_for_text && ` ` && 'confirmation appraisal.</span></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">The details are as below:</span></p>'.
  append w_contents to t_contents.

*clear w_contents.
*w_contents-line = '<p><span style="color: black;">&nbsp;</span></p>'.
*append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<table style="width: 522.9pt; border-collapse: collapse;" width="1046">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<tbody>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<tr style="height: 15.25pt;">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 71.6pt; border: solid windowtext 1.0pt; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="143">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Employee ID</span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.5in; border: solid windowtext 1.0pt; border-left: none; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="216">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Employee name</span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 41.8pt; border: solid windowtext 1.0pt; border-left: none; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="84">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Grade </span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 103.5pt; border: solid windowtext 1.0pt; border-left: none; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="207">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Designation</span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.25in; border: solid windowtext 1.0pt; border-left: none; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="180">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Date of Joining</span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.5in; border: solid windowtext 1.0pt; border-left: none; background: #F2F2F2; padding: 0in 5.4pt 0in 5.4pt; height: 15.25pt;" width="216">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p style="text-align: center;"><strong><span style="color: black;">Date of Confirmation</span></strong></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</tr>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<tr style="height: 15.75pt;">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 71.6pt; border: solid windowtext 1.0pt; border-top: none; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="143">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p>' && wa_upload-pernr && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.5in; border-top: none; border-left: none; border-bottom: solid windowtext 1.0pt; border-right: solid windowtext 1.0pt; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="216">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p>' && name && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 41.8pt; border-top: none; border-left: none; border-bottom: solid windowtext 1.0pt; border-right: solid windowtext 1.0pt; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="84">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p>' && wa_upload-persk && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 103.5pt; border-top: none; border-left: none; border-bottom: solid windowtext 1.0pt; border-right: solid windowtext 1.0pt; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="207">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p>' && wa_upload-description2 && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.25in; border-top: none; border-left: none; border-bottom: solid windowtext 1.0pt; border-right: solid windowtext 1.0pt; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="180">'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p>' && begda_ext && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<td style="width: 1.5in; border-top: none; border-left: none; border-bottom: solid windowtext 1.0pt; border-right: solid windowtext 1.0pt; padding: 0in 5.4pt 0in 5.4pt; height: 15.75pt;" width="216">'.
  append w_contents to t_contents.

  clear begda_int.
  call function 'CONVERT_DATE_TO_INTERNAL'
    exporting
      date_external            = begda_ext
    importing
      date_internal            = begda_int
    exceptions
      date_external_is_invalid = 1
      others                   = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  clear conf_date_int.
  call function 'MONTH_PLUS_DETERMINE'
    exporting
      months  = months
      olddate = begda_int
    importing
      newdate = conf_date_int.

  clear conf_date_ext.
  call function 'CONVERT_DATE_TO_EXTERNAL'
    exporting
      date_internal            = conf_date_int
    importing
      date_external            = conf_date_ext
    exceptions
      date_internal_is_invalid = 1
      others                   = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  clear w_contents.
  w_contents-line = '<p>' && conf_date_ext && '</p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</td>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</tr>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</tbody>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '</table>'.
  append w_contents to t_contents.

*clear w_contents.
*w_contents-line = '<p><span style="color: black;">&nbsp;</span></p>'.
*append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">We would request you to initiate the performance' && ` ` &&
  'discussion with your team member and submit the form (attached) on or before' && ` ` && conf_date_ext && ` ` && '.</span></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">&nbsp;</span></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">Regards,</span></p>'.
  append w_contents to t_contents.

  clear w_contents.
  w_contents-line = '<p><span style="color: black;">HR Operations Team </span></p>'.
  append w_contents to t_contents.
endform.
*&---------------------------------------------------------------------*
*& Form SEND_INDUCTION_EMAIL
*&---------------------------------------------------------------------*
*& IHDK900606: HR: S_K: ZHR_PS: Add induction/welcome email: 13.2.19; 6010859
*&---------------------------------------------------------------------*
*&      --> WA_UPLOAD
*&---------------------------------------------------------------------*
form send_induction_email using p_wa_upload like wa_upload.
  " get recipients
  select single pernr from pa0001 into @data(lv_pernr) where pernr = @p_wa_upload-pernr and endda > @sy-datum.
  check sy-subrc = 0. " employee created successfully
  " get hr key users
  select * from zfi044_emailid into table @data(lt_hr_email) where egroup eq 'ZHR_INDUCT'.  " hr key users

  " get manager
  select single usrid_long from pa0105
    into @data(lv_manager_email)
    where pernr = @p_wa_upload-mstbr
    and   subty = '0010' and endda ge @sy-datum.  " IHDK900728

  " get spoc
  select param2 as email
    from z6mma_params
    into table @data(lt_spoc_email)
    where progname = 'ZHR_INDUCT_SPOC'
    and   param1   = @p_wa_upload-orgeh.

  data: lt_recipient type standard table of zfi_s_vp_recipient,
        ls_recipient type zfi_s_vp_recipient.

  check p_wa_upload-usrid3 is not initial.
  clear ls_recipient.
  ls_recipient-recipient = condense( to_upper( p_wa_upload-usrid3 ) ).
  append ls_recipient to lt_recipient.

  if lv_manager_email is not initial.
    clear ls_recipient.
    ls_recipient-copy      = abap_true.
    ls_recipient-recipient = condense( to_upper( lv_manager_email ) ).
    append ls_recipient to lt_recipient.
  endif.

  if lt_hr_email is not initial.
    loop at lt_hr_email into data(ls_hr_email).
      clear ls_recipient.
      ls_recipient-blind_copy = abap_true.  " IHDK900637
      ls_recipient-recipient = condense( to_upper( ls_hr_email-email ) ).
      append ls_recipient to lt_recipient.
      clear ls_hr_email.
    endloop.
  endif.

  if lt_spoc_email is not initial.
    loop at lt_spoc_email into data(ls_spoc_email).
      clear ls_recipient.
      ls_recipient-copy = abap_true.
      ls_recipient-recipient = condense( to_upper( ls_spoc_email-email ) ).
      append ls_recipient to lt_recipient.
      clear ls_spoc_email.
    endloop.
  endif.

  " IHDK900899
  split condense( to_upper( wa_upload-induct_bcc ) ) at ',' into table data(lt_bcc).

  if lt_bcc is not initial.
    loop at lt_bcc into data(ls_bcc).
      clear ls_recipient.
      ls_recipient-blind_copy = abap_true.
      ls_recipient-recipient = condense( to_upper( ls_bcc ) ).
      append ls_recipient to lt_recipient.
      clear ls_bcc.
    endloop.
  endif.
  " End IHDK900899

  check lt_recipient is not initial.
  data(lv_hr_gm_email) = condense( to_upper( 'sraj@indofil.com' ) ).
  new zcl_email( )->send_induction_email(
    exporting
      iv_salutation = conv #( condense( |{ p_wa_upload-anred }. { p_wa_upload-vorna }| ) )
      iv_sender     = conv #( lv_hr_gm_email )
      it_recipient  = lt_recipient
    importing
      ev_sent       = data(lv_sent) ).  " IHDK900610

  " IHDK900610
  if lv_sent eq abap_true.
    format color 5 intensified off.
    write: / 'Induction/Welcome email sent successfully'.
    format reset.
  endif.
endform.
