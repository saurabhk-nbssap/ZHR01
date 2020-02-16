*----------------------------------------------------------------------*
*  INCLUDE PCEPFIN4                                                    *
*XXXNTnote number  <date>   Note<note number>:<short description>
*RBSNT794651       10032004 Note794651: Blank form is not printed when
*                                       no valid employees are selected
*----------------------------------------------------------------------*

form print_module1.

  if disp_flg_lot lt 1.
    formname = layout.
    lang = langu.
  endif.

***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****

  if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****

    perform open_form using lang.
* Page1
    perform start_form using formname lang 'PAGE1'.
***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****
  endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****


  loop at final_tab.
    perform get_address using final_tab-tstad.
    perform convert_to_scriptvar using 'name1' sadr-name1 .
    perform convert_to_scriptvar using 'name2' sadr-name2 .
    perform convert_to_scriptvar using 'stras' sadr-stras .
    perform convert_to_scriptvar using 'pstlz' sadr-pstlz+0(6) .
    perform convert_to_scriptvar using 'ort01' sadr-ort01 .
    perform convert_to_scriptvar using 'fybegda' fybegda(4).
    perform convert_to_scriptvar using 'fyendda' fyendda(4).

    perform curr_to_char using 'pf_rate' final_tab-pf_rate.

    perform convert_to_scriptvar using 'pfmonth' pf_month_year.
    if final_tab-pfrfn is not initial.                      "PKT1152442
      perform convert_to_scriptvar using 'pfref' final_tab-pfrfn.
    else.
      perform convert_to_scriptvar using 'pfref' final_tab-pfref.
    endif.
*epf a/c
    perform curr_to_char using 'tot_pf_basis' final_tab-tot_pf_basis.
    perform curr_to_char using 'tot_pf_ee_contr'
                                final_tab-tot_pf_ee_contr.
    perform curr_to_char using 'tot_pf_er_contr'
                                final_tab-tot_pf_er_contr.

    perform curr_to_char using 'tot_pf_admin_chgs'
                                final_tab-tot_pf_admin_chgs.

*pension a/c
    perform curr_to_char using 'tot_pen_basis' final_tab-tot_pen_basis .
    perform curr_to_char using 'tot_pen_er_contr'
                                final_tab-tot_pen_er_contr.

*edli a/c
    perform curr_to_char using 'tot_edli_basis' final_tab-tot_edli_basis.

    perform curr_to_char using 'tot_edli_er_contr'
                                final_tab-tot_edli_er_contr.

    perform curr_to_char using 'tot_edli_admin_chgs'
                                final_tab-tot_edli_admin_chgs.

    perform convert_to_scriptvar using 'tot_no_emp'
                                        final_tab-tot_no_emp.
*  No of subscribers for EPF
    perform convert_to_scriptvar using 'no_last_month'
                           final_tab-no_last_month.
    perform convert_to_scriptvar using 'no_new_this_month'
                           final_tab-no_new_this_month.
    perform convert_to_scriptvar using 'no_left_service'
                           final_tab-no_left_service.
    perform convert_to_scriptvar using 'total_members'
                           final_tab-total_members.

*  No of sunscribers for pension fund
    perform convert_to_scriptvar using 'no_last_month_pen'
                           final_tab-no_last_month_pen.
    perform convert_to_scriptvar using 'no_new_this_mon_pen'
                           final_tab-no_new_this_month_pen.
    perform convert_to_scriptvar using 'no_left_service_pen'
                           final_tab-no_left_service_pen.
    perform convert_to_scriptvar using 'total_members_pen'
                           final_tab-total_members_pen.

*  No of subscribers for EDLI
    perform convert_to_scriptvar using 'no_last_month_edli'
                           final_tab-no_last_month_edli.
    perform convert_to_scriptvar using 'no_new_this_mon_edli'
                           final_tab-no_new_this_month_edli.
    perform convert_to_scriptvar using 'no_left_service_edli'
                           final_tab-no_left_service_edli.
    perform convert_to_scriptvar using 'total_members_edli'
                           final_tab-total_members_edli.

***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****

    gv_fybegda = fybegda(4).
    gv_fyendda = fyendda(4).
    gv_pfmonth = pf_month_year.
    gv_bankname = bankname.
    gv_bankaddr = bankaddr.
    gv_bankcity = bankcity.

***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****

  endloop.

***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****
  if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****


    perform write_form using '' 'APPEND' 'BODY' 'VAR1'.

    perform end_form.
    perform close_form.

***** START OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****
  endif.

  if p_pdf = 'X'.
    gv_fpname = formname.
    perform pdf_display.
  endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '6-1-2005' *****



endform.                    "PRINT_MODULE1

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
form display_list.

  data: repid like sy-repid,
          title(50) type c.

  refresh mn_tab.
  loop at main_tab.
* AB08022017 INSERT START...
    read table itab1 with key pernr = main_tab-pernr.
    if sy-subrc eq 0.
      call function 'PYXX_READ_PAYROLL_RESULT'
        exporting
*         CLUSTERID                          =
          employeenumber                     = main_tab-pernr
          sequencenumber                     = itab1-seqnr
*         READ_ONLY_BUFFER                   = ' '
          read_only_international            = 'X'
*         ARC_GROUP                          = ' '
*         CHECK_READ_AUTHORITY               = 'X'
*         FILTER_CUMULATIONS                 = 'X'
*         CLIENT                             =
*       IMPORTING
*         VERSION_NUMBER_PAYVN               =
*         VERSION_NUMBER_PCL2                =
        changing
          payroll_result                     = result
        exceptions
          illegal_isocode_or_clusterid       = 1
          error_generating_import            = 2
          import_mismatch_error              = 3
          subpool_dir_full                   = 4
          no_read_authority                  = 5
          no_record_found                    = 6
          versions_do_not_match              = 7
          error_reading_archive              = 8
          error_reading_relid                = 9
          others                             = 10.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        rt1 = result-inter-rt.
* Start IHDK904177
        clear wa_rt.
        read table rt1 into wa_rt with key lgart = '/4MI'.
        IF sy-subrc = 0.
          main_tab-tearn = wa_rt-betrg.

* start IHDK904232
        clear wa_rt.
        read table rt1 into wa_rt with key lgart = '1116'.
        IF sy-subrc = 0.
          main_tab-tearn = main_tab-tearn + wa_rt-betrg.
        ENDIF.
* Start IHDK904343
        IF main_tab-pf_ee_arrear < 0.
          main_tab-pf_ee_arrear = main_tab-tearn - main_tab-pf_ee_arrear.
        ENDIF.
* ENd IHDK904343
* End IHDK904232
        ELSE.
*  End IHDK904177
        clear  wa_rt.  " IHDK904177
        read table rt1 into wa_rt with key lgart = '/101'.
        if sy-subrc eq 0.
          main_tab-tearn = wa_rt-betrg.
        endif.
        ENDIF. " IHDK904177
      endif.
* AB08022017 INSERT END...
* AB08022017 COMMENT START...
**    Gross total
*    main_tab-tearn = main_tab-basic + main_tab-pf_rate +
*    main_tab-pf_ee_contr + main_tab-pf_er_vpf + main_tab-pf_er_contr +
*    main_tab-pf_admin_chgs + main_tab-pen_er_contr + main_tab-edli_er_contr
*    + main_tab-edli_admin_chgs.
* AB08022017 COMMENT END...
*      modify main_tab transporting tearn . -IHDK904343
      modify main_tab transporting tearn pf_ee_arrear. "+IHDK904343
    endif.
  endloop.

  " IHDK902336
  select pernr, bukrs
    from pa0001
    into table @data(lt_emp_comp)
    for all entries in @main_tab
    where pernr eq @main_tab-pernr
    and begda le @sy-datum and endda ge @sy-datum.  " IHDK902522

  loop at main_tab.
    try.
        main_tab-bukrs = lt_emp_comp[ pernr = main_tab-pernr ]-bukrs. " IHDK902336
      catch cx_sy_itab_line_not_found.
    endtry.
    move-corresponding main_tab to mn_tab.
    modify main_tab transporting bukrs. " IHDK902472
    append mn_tab.
  endloop.

  " IHDK902336
  delete main_tab where bukrs not in s_bukrs.
  delete mn_tab where bukrs not in s_bukrs.
********* added by nk on 12.01.2017

  loop at mn_tab.
*    Gross total
    mn_tab-tearn = mn_tab-basic + mn_tab-pf_rate +
    mn_tab-pf_ee_contr + mn_tab-pf_er_vpf + mn_tab-pf_er_contr +
    mn_tab-pf_admin_chgs + mn_tab-pen_er_contr + mn_tab-edli_er_contr
    + mn_tab-edli_admin_chgs.
    modify mn_tab transporting tearn.
  endloop.
********* added by nk on 12.01.2017

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = 'Z6HR032R_HINCEPF0'
      i_internal_tabname     = 'MN_TAB'
      i_inclname             = 'Z6HR032I_PCEPFIN2'
    changing
      ct_fieldcat            = fieldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  read table fieldcat with key fieldname = 'PERNR'.
  fieldcat-seltext_m = 'Employee No.'(058).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 13.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EEPFN'.
  fieldcat-seltext_m = 'Employee PF No.'(057).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 16.
  modify fieldcat index sy-tabix.
*****Added for UAN field by NK on 20.12.2016******
  read table fieldcat with key fieldname = 'ICNUM'.
  fieldcat-seltext_m = 'UAN No.'.
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 30.
  modify fieldcat index sy-tabix.
*****end for UAN field by NK on 20.12.2016******

  read table fieldcat with key fieldname = 'BASIC'.
  if sy-subrc = 0.
    fieldcat-seltext_m = 'Basic Pay'.
    fieldcat-ddictxt = 'M'.
    fieldcat-outputlen = 16.
    modify fieldcat index sy-tabix.
  endif.

  read table fieldcat with key fieldname = 'BASIC_ARREAR'.
  fieldcat-seltext_m = 'Basic Pay Arrears'.
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 18.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_RATE'.
  fieldcat-seltext_m = 'PF Rate'(043).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 8.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_EE_CONTR'.
  fieldcat-seltext_m = 'EePFCont'(045).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_EE_ARREAR'.
  fieldcat-seltext_m = 'EE PF Arrears'(103).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 14.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_ER_CONTR'.
  fieldcat-seltext_m = 'ErPFCont'(046).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_ER_ARREAR'.
  fieldcat-seltext_m = 'ER PF Arrears'(104).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 14.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_ER_VPF'.
  fieldcat-seltext_m = 'VPF Cont' .
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_VPF_ARREAR'.
  fieldcat-seltext_m = 'VPF Arrears'(105).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 12.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PF_ADMIN_CHGS'.
  fieldcat-seltext_m = 'PFAdmChg'(047).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PEN_ER_CONTR'.
  fieldcat-seltext_m = 'ErPenCon'(049).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'PEN_ER_ARREAR'.
  fieldcat-seltext_m = 'ER Pen Arrears'(106).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 16.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EDLI_ER_CONTR'.
  fieldcat-seltext_m = 'EDLI ErCo'(051).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 10.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EDLI_ADMIN_CHGS'.
  fieldcat-seltext_m = 'EDLI Adm'(052).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'TEARN'.
  fieldcat-seltext_m = 'Gross Salary'.
*  fieldcat-seltext_m = 'Total Earnings'(053).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 15.
*  fieldcat-reftab   = 'PA0008'.
*  fieldcat-reffield = 'BET01'.
  modify fieldcat index sy-tabix.

  repid = sy-repid.
  refresh g_itab_fcode.
  move 'CORC' to g_itab_fcode-fcode.
  append g_itab_fcode.
  move 'AMBC' to g_itab_fcode-fcode.
  append g_itab_fcode.

*  PERFORM HR_LIST TABLES MAIN_TAB FLD_NAM
*                  USING TITLE HEADLINE.
*
*  CHECK SY-SUBRC = 0.

  clear form_name.
  form_name = 'HR_LIST'.

  perform display_alv_grid in program hincalv0
                        tables main_tab fieldcat g_itab_fcode
                        using repid 'PF Form 12A'(015)
                        'PF Form 12A Results'(016).

  if sy-batch = 'X'.
*   For output in SAP Script
    perform error_cases.
    perform print_module.
  endif.

endform.                               " DISPLAY_LIST

*&---------------------------------------------------------------------*
*&      Form  ERROR_CASES
*&---------------------------------------------------------------------*
form error_cases.
  describe table hr_error lines num.
  if num > 0.
    call function 'HR_DISPLAY_ERROR_LIST'
    exporting
         no_popup         = ' '
         no_print         = ' '
*         NO_IMG           = ' '
*         LINESIZE         = SY-LINSZ
     tables
          error            = hr_error
         exceptions
              invalid_linesize = 1
              others           = 2.
  else.
    message s361(hrpadin01).
*   There are no errors
    if sy-batch = 'X'.
      write: text-ner.
    endif.

  endif.

endform.                               " ERROR_CASES

*&---------------------------------------------------------------------*
*&      Form  DISPLAY5
*&---------------------------------------------------------------------*
form display5.

  data: repid like sy-repid,
        title(50) type c.

  refresh frm5_tab.
  loop at form5_tab.
    move-corresponding form5_tab to frm5_tab.
    append frm5_tab.
  endloop.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = 'HINCEPF0'
      i_internal_tabname     = 'FRM5_TAB'
      i_inclname             = 'PCEPFIN2'
    changing
      ct_fieldcat            = fieldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.


  read table fieldcat with key fieldname = 'PERNR'.
  fieldcat-seltext_m = 'Employee No.'(058).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 13.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EEPFN'.
  fieldcat-seltext_m = 'Employee PF No.'(057).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 16.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'ENAME'.
  fieldcat-seltext_m = 'Emp-Name'(060).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'DOB'.
  fieldcat-seltext_m = 'DateOfBirth'(062).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 12.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'DOJPF'.
  fieldcat-seltext_m = 'DateOfPFJn'(063).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 11.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'GENDER'.
  fieldcat-seltext_m = 'Sex'(064).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 4.
  modify fieldcat index sy-tabix.

  repid = sy-repid.
  refresh g_itab_fcode.
  move 'CORC' to g_itab_fcode-fcode.
  append g_itab_fcode.
  move 'AMBC' to g_itab_fcode-fcode.
  append g_itab_fcode.


*  PERFORM HR_LIST TABLES FORM5_TAB FLD_NAM
*                  USING TITLE HEADLINE.

  clear form_name.
  form_name = 'HR_FORM5'.

* Appending blank record when selection is empty to print blank form
  if form5_tab[] is initial.                            "RBSNT794651
    move space to form5_tab-pernr.
    move space to form5_tab-dob.
    move space to form5_tab-dojpf.
    move space to form5_tab-prev_service.
    append form5_tab.
  endif.

  perform display_alv_grid in program hincalv0
                        tables form5_tab fieldcat g_itab_fcode
                        using repid 'PF Form 5'(066)
                        'PF Form 5 Results'(067).

  if sy-batch = 'X'.
*   For output in SAP Script
    perform error_cases.
    perform print_module.
  endif.

endform.                                                    " DISPLAY5

*&---------------------------------------------------------------------*
*&      Form  DISPLAY10
*&---------------------------------------------------------------------*
form display10.

  data: repid like sy-repid,
          title(50) type c.
  refresh frm10_tab.
  loop at form10_tab.
    move-corresponding form10_tab to frm10_tab.
    append frm10_tab.
  endloop.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = 'HINCEPF0'
      i_internal_tabname     = 'FRM10_TAB'
      i_inclname             = 'PCEPFIN2'
    changing
      ct_fieldcat            = fieldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.


  read table fieldcat with key fieldname = 'PERNR'.
  fieldcat-seltext_m = 'Employee No.'(058).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 13.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EEPFN'.
  fieldcat-seltext_m = 'Employee PF No.'(057).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 16.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'ENAME'.
  fieldcat-seltext_m = 'Emp-Name'(060).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'DOL'.
  fieldcat-seltext_m = 'DateOfLv'(068).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 9.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'REASON_LEAVE'.
  fieldcat-seltext_m = 'ReasonForLv'(069).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 12.
  modify fieldcat index sy-tabix.

  repid = sy-repid.
  refresh g_itab_fcode.
  move 'CORC' to g_itab_fcode-fcode.
  append g_itab_fcode.
  move 'AMBC' to g_itab_fcode-fcode.
  append g_itab_fcode.

*  PERFORM HR_LIST TABLES FORM10_TAB FLD_NAM
*                  USING TITLE HEADLINE.
*
*  CHECK SY-SUBRC = 0.

  clear form_name.
  form_name = 'HR_FORM10'.

* Appending blank record when selection is empty to print blank form
  if form10_tab[] is initial.                           "RBSNT794651
    move space to form10_tab-ename.
    move space to form10_tab-dol.
    append form10_tab.
  endif.

  perform display_alv_grid in program hincalv0
                        tables form10_tab fieldcat g_itab_fcode
                        using repid 'PF Form 10'(070)
                        'PF Form 10 Results'(020).
  if sy-batch = 'X'.
*   For output in SAP Script
    perform error_cases.
    perform print_module.
  endif.

endform.                                                    " DISPLAY10

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM5
*&---------------------------------------------------------------------*
form print_form5.

  data: srno type i.

  if disp_flg_lot lt 1.
    formname = layout.
    lang = langu.
  endif.

  sort form5_tab ascending.            " BY PFREF PERNR.

  loop at form5_tab.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


      perform open_form using lang.
* page
      perform start_form using formname lang 'PAGE1'.
***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


    perform get_address using form5_tab-tstad.
    perform convert_to_scriptvar using 'name1' sadr-name1 .
    perform convert_to_scriptvar using 'name2' sadr-name2 .
    perform convert_to_scriptvar using 'stras' sadr-stras .
    perform convert_to_scriptvar using 'pstlz' sadr-pstlz+0(6) .
    perform convert_to_scriptvar using 'ort01' sadr-ort01 .

    perform convert_to_scriptvar using 'pfmonth' pf_month_year.
    if form5_tab-pfrfn is not initial.                      "PKT1152442
      perform convert_to_scriptvar using 'pfref' form5_tab-pfrfn.
    else.
      perform convert_to_scriptvar using 'pfref' form5_tab-pfref.
    endif.
***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****
    gv_name1 = sadr-name1.
    gv_stras = sadr-stras.
    gv_ort01 = sadr-ort01.
    gv_pstlz = sadr-pstlz+0(6).
    gv_pfmonth = pf_month_year.


    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

      perform write_form using '' 'APPEND' 'BODY' 'HDDT2'.
      perform write_form using '' 'APPEND' 'BODY' 'HDDT3'.
      perform write_form using '' 'APPEND' 'BODY' 'HDDT4'.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****
    endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


    srno = srno + 1.
    perform convert_to_scriptvar using 'srno' srno.

    perform convert_to_scriptvar using 'eepfn' form5_tab-eepfn.
    perform convert_to_scriptvar using 'ename' form5_tab-ename.
    perform convert_to_scriptvar using 'fath_name' form5_tab-fath_name.
    perform convert_to_scriptvar using 'dob' form5_tab-dob.
    perform convert_to_scriptvar using 'gender' form5_tab-gender.
    perform convert_to_scriptvar using 'dojpf' form5_tab-dojpf.
    perform convert_to_scriptvar using 'prev_service'
                               form5_tab-prev_service.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


      perform write_form using 'DETAIL' 'APPEND' 'BODY' 'MAIN'.

      perform write_form using '' 'APPEND' 'BODY' 'FTDT'.
***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  endloop.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    perform end_form.
    perform close_form.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  endif.

  if p_pdf = 'X'.

    gv_fpname = formname.

    perform pdf_display.

  endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

endform.                               " PRINT_FORM5

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM10
*&---------------------------------------------------------------------*
form print_form10.

  data: srno type i.

  if disp_flg_lot lt 1.
    formname = layout.
    lang = langu.
  endif.

  sort form10_tab.                     " ASCENDING BY PFREF PERNR.

  loop at form10_tab.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


      perform open_form using lang.
* page
      perform start_form using formname lang 'PAGE1'.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    endif.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


    perform get_address using form10_tab-tstad.
    perform convert_to_scriptvar using 'name1' sadr-name1 .
    perform convert_to_scriptvar using 'name2' sadr-name2 .
    perform convert_to_scriptvar using 'stras' sadr-stras .
    perform convert_to_scriptvar using 'pstlz' sadr-pstlz+0(6) .
    perform convert_to_scriptvar using 'ort01' sadr-ort01 .

    perform convert_to_scriptvar using 'pfmonth' pf_month_year.
    if form10_tab-pfrfn is not initial.                     "PKT1152442
      perform convert_to_scriptvar using 'pfref' form10_tab-pfrfn.
    else.
      perform convert_to_scriptvar using 'pfref' form10_tab-pfref.
    endif.
***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****
    gv_name1 = sadr-name1.
    gv_stras = sadr-stras.
    gv_ort01 = sadr-ort01.
    gv_pstlz = sadr-pstlz+0(6).
    gv_pfmonth = pf_month_year.
    if form10_tab-pfrfn is not initial.                     "PKT1152442
      gv_pfref = form10_tab-pfrfn.
    else.
      gv_pfref = form10_tab-pfref.
    endif.

    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

      perform write_form using '' 'APPEND' 'BODY' 'HDDT2'.
      perform write_form using '' 'APPEND' 'BODY' 'HDDT3'.
      perform write_form using '' 'APPEND' 'BODY' 'HDDT4'.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


    srno = srno + 1.
    perform convert_to_scriptvar using 'srno' srno.
    perform convert_to_scriptvar using 'eepfn' form10_tab-eepfn.
    perform convert_to_scriptvar using 'ename' form10_tab-ename.
    perform convert_to_scriptvar using 'fath_name' form10_tab-fath_name.
    perform convert_to_scriptvar using 'dol' form10_tab-dol.
    perform convert_to_scriptvar using 'reason_leave'
                                     form10_tab-reason_leave.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****



      perform write_form using 'DETAIL' 'APPEND' 'BODY' 'MAIN'.

      perform write_form using '' 'APPEND' 'BODY' 'FTDT'.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  endloop.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  if p_script = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

    perform end_form.
    perform close_form.

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

  endif.

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****


  if p_pdf = 'X'.

    gv_fpname = formname.

    perform pdf_display.

  endif.
***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****




endform.                               " PRINT_FORM10

*&---------------------------------------------------------------------*
*&      Form  SHOW_FORMNAMES
*&---------------------------------------------------------------------*
form show_formnames.

  types: begin of forms,
            frmnm type pinpf-frmnm,
            frmds type pinpf-frmds,
          end of forms.

  data: formnames type forms occurs 0 with header line.

  clear formnames.
  refresh formnames.

  move 'FORM5' to formnames-frmnm.
  move 'Return of Employees qualifying for membership'(022)
        to formnames-frmds.
  append formnames.

  move 'FORM10' to formnames-frmnm.
  move 'Return of Members leaving the service'(023) to formnames-frmds.
  append formnames.

  move 'FORM12A' to formnames-frmnm.
  move 'Monthly contribution statement'(024) to formnames-frmds.
  append formnames.

  clear formnames.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
*         DDIC_STRUCTURE   = ' '
            retfield         = 'FRMNM'
*         PVALKEY          = ' '
            dynpprog         = 'HINCEPF0'
            dynpnr           = '1000'
            dynprofield      = 'REPONAME'
*         STEPL            = 0
           window_title     = 'Legal Forms-EPF'(010)
*         VALUE            = ' '
            value_org        = 'S'
*         MULTIPLE_CHOICE  = ' '
*         DISPLAY          = ' '
*         CALLBACK_PROGRAM = ' '
*         CALLBACK_FORM    = ' '
       tables
            value_tab        = formnames
*         FIELD_TAB       =
*         RETURN_TAB       =
*         DYNPFLD_MAPPING  =
*    EXCEPTIONS
*         PARAMETER_ERROR  = 1
*         NO_VALUES_FOUND  = 2
*         OTHERS           = 3
            .
  if sy-subrc <> 0.                                         "#EC *
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               " SHOW_FORMNAMES

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
form display_results.

  enddate = pn-endda.

  if disp_flg_lot ge 1.
    perform re596f using sname enddate.

*    perform (      ) in program (    ) using .........

    perform (t596f-modna) in program (t596f-pgmna)
                          changing formname.

    perform get_output_type using    enddate
                            changing formname
                                     p_script.
    if p_script is initial.
      p_pdf = 'X'.
    else.
      clear p_pdf.
    endif.
  endif.

*  perform pf12a_99 changing formname.

  case reponame.
    when 'FORM12A'.
      perform display_list.
    when 'FORM5'.
      perform display5.
    when 'FORM10'.
      perform display10.
  endcase.

endform.                               " DISPLAY_RESULTS


*&---------------------------------------------------------------------*
*&      Form  HR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA_TAB  text
*      -->P_FLD_TAB  text
*      -->P_TITLE  text
*      -->P_HEADLINE  text
*----------------------------------------------------------------------*
form hr_list tables   p_data_tab
                      p_fld_tab
             using    p_title
                      p_headline.

  call function 'HR_DISPLAY_BASIC_LIST'
    exporting
      basic_list_title     = p_title
      file_name            = '        '
      head_line1           = p_headline
      lay_out              = 0
      dyn_pushbutton_text1 = 'Print Errors'(017)
      dyn_pushbutton_text2 = 'Print Form'(018)
      additional_options   = ' '
    importing
      return_code          = ret_cd
    tables
      data_tab             = p_data_tab
      fieldname_tab        = p_fld_tab
    exceptions
      download_problem     = 1
      no_data_tab_entries  = 2
      table_mismatch       = 3
      print_problems       = 4
      others               = 5.

  case sy-subrc.
    when 1.
      message e171(pn).
*  Problem in downloading
    when 3.
      message e112(hrpadin01).
*  There is mismatch between the field table and data table
    when 4.
      message e113(hrpadin01).
*  Error due to print problems
    when 5.
    when others.
  endcase.

endform.                    " HR_LIST


*&--------------------------------------------------------------------*
*&      Form  print_module
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form print_module.
  case form_name.
    when 'HR_LIST'.
      perform print_module1.
    when 'HR_FORM5'.
      perform print_form5.
    when 'HR_FORM10'.
      perform print_form10.
  endcase.
endform.                    "print_module
*&---------------------------------------------------------------------*
*&      Form  PDF_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pdf_display .

  try.

      call function 'FP_FUNCTION_MODULE_NAME'
        exporting
          i_name           = gv_fpname
        importing
          e_funcname       = gv_fm_name
          e_interface_type = gv_e_interface_type.
      .

    catch cx_root into gv_w_cx_root.
      gv_mesg = gv_w_cx_root->get_text( ).
      message gv_mesg type 'E'.

  endtry.


  call function 'FP_JOB_OPEN'
    changing
      ie_outputparams = gv_fp_outputparams
    exceptions
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      others          = 5.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


***** APPENDS DATA OF DISP_BODY TABLE *****
***** TO GT_OUTPUT_TAB TABLE *****

  if gv_fpname = 'HR_IN_EPF010_99M'.

    loop at form10_tab.
      move-corresponding form10_tab to gs_output_tab.
      gs_output_tab-srno = sy-tabix.
      append gs_output_tab to gt_output_tab.
      clear gs_output_tab.
    endloop.

  elseif gv_fpname = 'HR_IN_EPF005_99M'.
    clear gt_output_tab.
    loop at form5_tab .
      move-corresponding form5_tab to gs_output_tab.
      if form5_tab-pfrfn is not initial.                    "PKT1152442
        gs_output_tab-pfref = form5_tab-pfrfn .
      else.
        gs_output_tab-pfref = form5_tab-pfref .
      endif.
      gs_output_tab-srno = sy-tabix.
      append gs_output_tab to gt_output_tab.
    endloop.

  elseif gv_fpname = 'HR_IN_EPF12A_99M'.

    loop at final_tab .
      move-corresponding final_tab to gs_output12a_tab.
      if final_tab-pfrfn is not initial.                    "PKT1152442
        gs_output12a_tab-pfref = final_tab-pfrfn .
      else.
        gs_output12a_tab-pfref = final_tab-pfref .
      endif.

      write final_tab-pf_rate currency curr to
                                   gs_output12a_tab-pf_rate.

      write final_tab-tot_pf_basis currency curr to
                                   gs_output12a_tab-tot_pf_basis.

      write final_tab-tot_pf_ee_contr currency curr to
                                   gs_output12a_tab-tot_pf_ee_contr.

      write final_tab-tot_pf_er_contr currency curr to
                                   gs_output12a_tab-tot_pf_er_contr.

      write final_tab-tot_pf_admin_chgs currency curr to
                                   gs_output12a_tab-tot_pf_admin_chgs.

      write final_tab-tot_pen_basis currency curr to
                                   gs_output12a_tab-tot_pen_basis.

      write final_tab-tot_pen_er_contr currency curr to
                                   gs_output12a_tab-tot_pen_er_contr.

      write final_tab-tot_edli_basis currency curr to
                                   gs_output12a_tab-tot_edli_basis.

      write final_tab-tot_edli_er_contr currency curr to
                                   gs_output12a_tab-tot_edli_er_contr.

      write final_tab-tot_edli_admin_chgs currency curr to
                                   gs_output12a_tab-tot_edli_admin_chgs.

      move final_tab-tot_no_emp to gs_output12a_tab-tot_no_emp1.
      move t500c-waers to gs_t500c-waers.
      append gs_output12a_tab to gt_output12a_tab.
    endloop.

  endif.
  gv_name1 = sadr-name1.
  gv_stras = sadr-stras.
  gv_ort01 = sadr-ort01.
  gv_pstlz = sadr-pstlz+0(6).

***** CALLS THE CREATED FUNCTION MODULE *****

  call function gv_fm_name
    exporting
      gs_name1       = gv_name1
      gs_stras       = gv_stras
      gs_ort01       = gv_ort01
      gs_pstlz       = gv_pstlz
      gs_repodate    = repodate
      gs_pfmonth     = gv_pfmonth
      gs_begda       = gv_fybegda
      gs_fyendda     = gv_fyendda
      gs_bankname    = gv_bankname
      gs_bankaddr    = gv_bankaddr
      gs_bankcity    = gv_bankcity
      t500c          = gs_t500c
      gs_pfref       = gv_pfref
      disp_body12a   = gt_output12a_tab
      disp_body      = gt_output_tab
    exceptions
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      others         = 4.


  call function 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
   exceptions
      usage_error          = 1
      system_error         = 2
      internal_error       = 3
      others               = 4
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  refresh : gt_output12a_tab,
            gt_output_tab.


endform.                    " PDF_DISPLAY
