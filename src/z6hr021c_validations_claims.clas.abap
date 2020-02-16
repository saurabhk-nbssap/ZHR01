class Z6HR021C_VALIDATIONS_CLAIMS definition
  public
  final
  create public .

*"* public components of class Z6HR021C_VALIDATIONS_CLAIMS
*"* do not include other source files here!!!
public section.

  interfaces IF_AC_VALIDATE_REQUEST .
  interfaces IF_BADI_INTERFACE .
protected section.
*"* protected components of class Z6HR021C_VALIDATIONS_CLAIMS
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR021C_VALIDATIONS_CLAIMS
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR021C_VALIDATIONS_CLAIMS IMPLEMENTATION.


METHOD if_ac_validate_request~validate_request_data.

  TYPES : BEGIN OF s_2001 ,
         pernr TYPE persno,
         subty TYPE subty,
         begda TYPE begda,
         endda TYPE endda,
         kaltg TYPE kaltg,
         gjahr TYPE gjahr,
          END OF s_2001.
  TYPES :  BEGIN OF s_0015 ,
         pernr TYPE persno,
         subty TYPE subty,
         begda TYPE begda,
         endda TYPE endda,
         lgart TYPE lgart,
         gjahr TYPE gjahr,
          END OF s_0015.
  DATA : wa_return TYPE bapiret2.
  DATA : lv_kaltg TYPE ps2001-kaltg.
  TYPES : BEGIN OF s_0416 ,
         pernr TYPE persno,
         subty TYPE subty,
         begda TYPE begda,
         endda TYPE endda,
         kaltg TYPE kaltg,
         gjahr TYPE gjahr,
          END OF s_0416.
  DATA : lv_dat01 TYPE pa0041-dat01.
  DATA : it_2001 TYPE  TABLE OF s_2001.
  DATA : wa_2001 TYPE s_2001.
  DATA : it_0015 TYPE  TABLE OF s_0015,
         wa_0015 TYPE s_0015.
  DATA : pre_year TYPE gjahr.
  DATA : lgart TYPE lgart.

* BADI check
  IF ist_trnsd-retyp EQ 'SLTA' AND ( ist_trnsd-rqtyp EQ 'CL' OR ist_trnsd-rqtyp EQ 'AD' ).

    SELECT SINGLE dat01 FROM pa0041 INTO lv_dat01 WHERE pernr EQ  ist_trnsd-pernr
                                             AND begda LE sy-datum
                                             AND endda GE sy-datum
                                             AND dar01 EQ 'I1'.


      IF sy-subrc EQ 0.

        CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
          EXPORTING
            months  = 11
            olddate = lv_dat01
          IMPORTING
            newdate = lv_dat01.

        IF ist_trnsd-rqcdt LT  lv_dat01.
                  CLEAR wa_return.
            wa_return-type = 'E'.
            wa_return-id = 'ZHR01'.
            wa_return-number = '000'.
            wa_return-message = 'You are not Eligible for Reimbursement Type SLTA'.
            APPEND wa_return TO ctb_errtb.
            CLEAR wa_return.
        ELSE.
          SELECT *  FROM pa2001 INTO CORRESPONDING FIELDS OF TABLE it_2001
                         WHERE pernr EQ ist_trnsd-pernr
                           AND subty EQ 'PL'
                           AND kaltg GE 4
                           AND begda EQ cst_haedc-jbgdt
                           AND endda EQ cst_haedc-jendt.


          LOOP AT it_2001 INTO wa_2001.
            wa_2001-gjahr = wa_2001-endda+0(4).
            MODIFY it_2001 FROM wa_2001.
            CLEAR wa_2001.
          ENDLOOP.

READ TABLE it_2001 INTO wa_2001 WITH KEY pernr = ist_trnsd-pernr            "checking for current year
                                         subty = 'PL'
                                         gjahr = sy-datum+0(4).
IF sy-subrc NE 0.
                 CLEAR wa_return.
                 wa_return-type = 'E'.
                 wa_return-id = 'ZHR01'.
                 wa_return-number = '000'.
                 wa_return-message = 'Can not apply LTA for previous year PL'.
                 APPEND wa_return TO ctb_errtb.
                 CLEAR wa_return.
ELSE.

          READ TABLE it_2001 INTO wa_2001 WITH KEY pernr = ist_trnsd-pernr            "checking for current year
                                                   subty = 'PL'
                                                   begda = cst_haedc-jbgdt
                                                   endda = cst_haedc-jendt.

            IF sy-subrc NE 0.
                 CLEAR wa_return.
                 wa_return-type = 'E'.
                 wa_return-id = 'ZHR01'.
                 wa_return-number = '000'.
                 wa_return-message = 'No approved PL found'.
                 APPEND wa_return TO ctb_errtb.
                 CLEAR wa_return.
            ENDIF.
        ENDIF.
        ENDIF.
     ENDIF.
  ENDIF.
  ENDMETHOD.
ENDCLASS.
