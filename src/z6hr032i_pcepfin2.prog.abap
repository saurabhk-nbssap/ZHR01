*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN2                                                   *
*----------------------------------------------------------------------*
*  Data declaration

data: begin of it occurs 100.
        include structure pc207.
data: end of it.

data: begin of prt occurs 10.
        include structure pc207.
data: end of prt.

data: begin of pcrt occurs 10.
        include structure pc208 .
data: end of pcrt .


data: begin of pepf occurs 10.
        include structure pc2_in07.
data: end of pepf.

data: begin of lepf occurs 10.
        include structure pc2_in07.
data: end of lepf.

data: begin of temp_rgdir occurs 10.
        include structure pc261.
data: end of temp_rgdir.


data: last_month_begda like pn-begda,
      last_month_endda like pn-endda,
      last_month(2),
      last_result,
      next_month_begda like pn-begda,
      toggle_flag,
      num type i,
      title like sy-title,
      headline(20),
      ret_cd like sy-subrc,
      emp_name like pernr-ename,
      e_name like pernr-ename,
      formname(30),
      form5_tab_entry,
      form10_tab_entry,
      lang like sy-langu value 'E',
      nbegda like sy-datum,
      nendda like sy-datum,
      edliper(6) type n,
      acc_factor like t511k-kwert.

data: pf_basis like pc207-lgart value '/3FB',
      edli_basis like pc207-lgart value '/3FL',
      pen_basis like pc207-lgart value '/3FC'.

data: char_date(9) type c,
      pf_month(3) type c,
      pf_year(4) type c,
      pf_mon(2) type c,
      pf_day(2) type c,
      sep(3) type c value ' - ',
      pf_month_year(10) type c,
      fybegda like pn-begda,
      fyendda like pn-endda,
      begda1800 like pn-begda value '18000101',
      endda9999 like pn-endda value '99991231'.

data: sname like t596f-sname,
      enddate like sy-datum,
      subty(4) type c.

"Anees
types : begin of i_t549q.
        include type t549q.
types : fpper(9) type c.
types : end of i_t549q.
data : v_fpper type hrpy_rgdir-fpper,
       v_flper type hrpy_rgdir-inper.
data : v_lines type i.
data : wa_t549q type i_t549q,
       it_t549q type table of i_t549q .
"End

data: begin of main_tab occurs 10,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm, "pc2_in07-eepfn, " IHDK902030
        eepnn like pc2_in07-eepnm, "pc2_in07-eepnn,
        icnum like pa0185-icnum," add UAN by NK on 20.12.2016
        basic like pc207-betrg,
        basic_arrear like pc207-betrg,    " Naga 16-11-2019
        pf_rate like pc2_in07-eectr,
        pf_ee_contr like pc207-betrg,
        pf_ee_arrear like pc207-betrg,   " Naga 16-11-2019
        pf_er_vpf   like pc207-betrg,
        pf_vpf_arrear like  pc207-betrg, " Naga 16-11-2019
        pf_er_contr like pc207-betrg,
        pf_er_arrear like  pc207-betrg,  " Naga 16-11-2019
        pf_admin_chgs like pc207-betrg,
        pen_er_contr like pc207-betrg,
        pen_er_arrear like  pc207-betrg, " Naga 16-11-2019
        edli_er_contr like pc207-betrg,
        edli_admin_chgs like pc207-betrg,
        edli_basis like pc207-betrg,
        pf_basis like pc207-betrg,
        pen_basis like pc207-betrg,
        pfref like pc2_in07-pfref,
        pfrfn like pc2_in07-pfrfn,                          "PKT1152442
        pnflg like pc2_in07-pnflg,
        payed like pc2_in07-payed,
        last_mon_pnflg like pc2_in07-pnflg,
        last_mon_payed like pc2_in07-payed,
        member_last_month type i,
        new_this_month type i,
        left_service type i,
        tearn type pa0008-bet01," Total Earnings
        bukrs type pa0001-bukrs,  " IHDK902336
      end of main_tab.

data: tot_no_emp type i.

* AB08022017 INSERT START...
data : begin of itab1 occurs 0 ,
         pernr like hrpy_rgdir-pernr,
         seqnr like hrpy_rgdir-seqnr,
         fpper like hrpy_rgdir-fpper,
       end of itab1.
data : p_fpper type hrpy_rgdir-fpper,
       p_pabrj type pabrj,
       p_pabrp type pabrp,
       result  type pay99_result,
       rt1     type pay99_international-rt,
       wa_rt   type line of pay99_international-rt.
* AB08022017 INSERT END...

data: begin of final_tab occurs 10,
        pf_rate like pc2_in07-eectr,

        tot_pf_basis like pc207-betrg,
        tot_pf_ee_contr like pc207-betrg,
        tot_pf_er_contr like pc207-betrg,
        tot_pf_vpf like pc207-betrg,
        tot_pf_admin_chgs like pc207-betrg,
        basic like pc207-betrg,

        tot_pen_basis like pc207-betrg,
        tot_pen_er_contr like pc207-betrg,

        tot_edli_basis like pc207-betrg,
        tot_edli_er_contr like pc207-betrg,
        tot_edli_admin_chgs like pc207-betrg,
        pfref like pc2_in07-pfref,
        pfrfn like pc2_in07-pfrfn,                          "PKT1152442
        tot_no_emp type i,

        no_last_month type i,
        no_new_this_month type i,
        no_left_service type i,
        total_members type i,

        no_last_month_pen type i,
        no_new_this_month_pen type i,
        no_left_service_pen type i,
        total_members_pen type i,

        no_last_month_edli type i,
        no_new_this_month_edli type i,
        no_left_service_edli type i,
        total_members_edli type i,

        tstad like t7inf1-tstad,
    end of final_tab.

data: begin of form5_tab occurs 10,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm,  "pc2_in07-eepfn,  " IHDK902030
        ename like pernr-ename,
        dob like p0002-gbdat,
        dojpf like pc2_in07-begda,
        gender like p0002-gesch,
        tstad like t7inf1-tstad,
        fath_name like pernr-ename,
        prev_service type i,
        pfref like pc2_in07-pfref,
        pfrfn like pc2_in07-pfrfn,                          "PKT1152442
      end of form5_tab.

data: begin of form10_tab occurs 10,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm,  "pc2_in07-eepfn,  " IHDK902030
        ename like pernr-ename,
        dol like pc2_in07-endda,
        reason_leave(24) type c,
        tstad like t7inf1-tstad,
        fath_name like pernr-ename,
        pfref like pc2_in07-pfref,
        pfrfn like pc2_in07-pfrfn,                          "PKT1152442
      end of form10_tab.

data: begin of hr_error occurs 10.
        include structure hrerror.
data: end of hr_error.

data: begin of fld_nam occurs 10,
            field1(20),
            field2(12),
            field3(12),
      end of fld_nam.

data : service_days type i,
       service_months type p decimals 2,
       reason(24) type c,
       disp_flg_lot type i value 1,
       negtv type i value -1,
       found.
data:  conv_date(10).
data:  form_name(20) type c.
type-pools: slis.
data: fieldcat type slis_t_fieldcat_alv with header line,
      begin of g_itab_fcode occurs 10,
       fcode like rsmpe-func,
      end   of g_itab_fcode.
data: begin of mn_tab occurs 0,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm, "pc2_in07-eepfn, " IHDK902030
        eepnn like pc2_in07-eepnm, "pc2_in07-eepnn,
        icnum like pa0185-icnum," add UAN by NK on 20.12.2016
        basic like pc207-betrg,
        basic_arrear like pc207-betrg,    " Naga 16-11-2019
        pf_rate like pc2_in07-eectr,
        pf_ee_contr like pc207-betrg,
        pf_ee_arrear like pc207-betrg,   " Naga 16-11-2019
         pf_er_vpf  like pc207-betrg,
        pf_vpf_arrear like  pc207-betrg, " Naga 16-11-2019
        pf_er_contr like pc207-betrg,
        pf_er_arrear like  pc207-betrg,  " Naga 16-11-2019
        pf_admin_chgs like pc207-betrg,
        pen_er_contr like pc207-betrg,
        pen_er_arrear like  pc207-betrg, " Naga 16-11-2019
        edli_er_contr like pc207-betrg,
        edli_admin_chgs like pc207-betrg,
        tearn like pc207-betrg,"type pa0008-bet01," Total Earnings
        bukrs like pa0001-bukrs,  " IHDK902336
       end of mn_tab.
data: begin of frm5_tab occurs 10,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm,  "pc2_in07-eepfn,  " IHDK902030
        ename like pernr-ename,
        dob like p0002-gbdat,
        dojpf like pc2_in07-begda,
        gender like p0002-gesch,
      end of frm5_tab.
data: begin of frm10_tab occurs 10,
        pernr like pernr-pernr,
        eepfn like pc2_in07-eepfm,  "pc2_in07-eepfn,  " IHDK902030
        ename like pernr-ename,
        dol like pc2_in07-endda,
        reason_leave(24) type c,
      end of frm10_tab.


***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

***** DATA DECLARATION PART *****

data:   gv_fp_outputparams    type  sfpoutputparams,
        gv_fpname             type  fpname,
        gv_fm_name            type  funcname,
        gv_e_interface_type   type  fpinterfacetype,
        gv_w_cx_root type ref to cx_root,
        gv_mesg type string.


data:   gv_slno type i,
        gv_pfmonth type pernr-geber,
        gv_name1 type sadr-name1,
        gv_stras type sadr-stras,
        gv_ort01 type sadr-ort01,
        gv_pstlz type sadr-pstlz,
        gs_t500c  type t500c,
*        gv_pfref type pc2_in07-pfref.
        gv_pfref type pc2_in07-pfrfn.

data:   gt_output_tab type standard table of hcm_hincepf0_pdf."Internal
"table
data:   gs_output_tab type hcm_hincepf0_pdf. " Structure

***** FOR PDF FORM 12A *****

data:   gt_output12a_tab type standard table of hcm_hincepf0_12a_pdf.

data:   curr(2).

data:   gs_output12a_tab type hcm_hincepf0_12a_pdf. "Structure

data:   gv_fybegda type pa0001-ename,
        gv_fyendda type pa0001-ename,
        gv_bankname type rpcepfin-bknam,
        gv_bankaddr type rpcepfin-bkadd,
        gv_bankcity type rpcepfin-bkcty.
* Begin of AFY change - L4HK040454
data:
  afy_switch   type pin_reval,
  htext_switch type c,
  paydate      type sy-datum,
  date         type sy-datum.
* End of AFY change - L4HK040454
"Anees
data: tmp_3f1 type betrg,
      tmp_3f2 type betrg,
      tmp_3f3 type betrg,
      tmp_3f4 type betrg,
      tmp_1001 type betrg,
      tmp_3602 type betrg,
      tmp_3603 type betrg,
      basic type pc207-betrg,
      tmp_146  TYPE betrg,
      tmp_111  TYPE betrg,
      tmp_zfa  TYPE betrg,
      tmp_5201 TYPE betrg,
      tmp_5202 TYPE betrg,
      tmp_zf3  TYPE betrg,
      tmp_zf4  TYPE betrg.

data: seltab type table of rsparams,
      seltab_wa like line of seltab.

data : begin of wa_z6hra_wage_grp,
       lgart       type z6hra_wage_grp-lgart,
       zwage_grp   type z6hra_wage_grp-zwage_grp,
       zwage_txt   type z6hra_wage_grp-zwage_txt,
       seq         type z6hra_wage_grp-seq,
       zzindicator type z6hra_wage_grp-zzindicator,
       end of wa_z6hra_wage_grp.

data : it_z6hra_wage_grp like standard table of wa_z6hra_wage_grp.
***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****
* Start Code: Added by CS on 14.10.2015 for Authorization. ***
* read_report_line_too_long abap dump  ~ commented by nk on 20.12.2016
*DATA: lv_persa_auth_flg TYPE c VALUE '',"Auth.FlagforPersonal Area
*      lv_persk_auth_flg TYPE c VALUE '',"Auth.FlagforEmp Subgroup
*      lv_abkrs_auth_flg TYPE c VALUE ''."Auth.FlagforPayroll Area
* End Code: Added by CS on 14.10.2015 for Authorization. ***
