*----------------------------------------------------------------------*
***INCLUDE PCSANIN3 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_INFOTYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_infotypes.

  rp_provide_from_last p0000 space pn-begda pn-endda.

  IF pnp-sw-found EQ 0.
    MESSAGE s089(hrpadin01) WITH '0000' pernr-pernr pn-begda pn-endda.
*   There is no infotype & for personnel no & from period & to &
    PERFORM build_error TABLES hr_error
                        USING space sy-msgid sy-msgno
                        sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
    REJECT.
  ENDIF.

  rp_provide_from_last p0001 space pn-begda pn-endda.

  IF pnp-sw-found EQ 0.
    MESSAGE s089(hrpadin01) WITH '0001' pernr-pernr pn-begda pn-endda.
*   There is no infotype & for personnel no & from period & to &
    PERFORM build_error TABLES hr_error
                        USING space sy-msgid sy-msgno
                        sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
    REJECT.
  ENDIF.

ENDFORM.                               " READ_INFOTYPES

*&---------------------------------------------------------------------*
*&      Form  FILL_DISP_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_disp_body.

  DATA  : tempct LIKE pc207-betrg, tgrsal LIKE pc207-betrg.
  DATA: experience TYPE  komp-anz_monate, financial_end TYPE dats.
  CLEAR : disp_body.

  MOVE pernr-pernr TO disp_body-pernr.
  PERFORM read_name USING sy-langu CHANGING disp_body-ename.

  READ TABLE tsan WITH KEY sanid = sanid.    "MDSNT1001718

  IF sy-subrc EQ 0.
*"Anees
*  LOOP AT CRT WHERE cumty = 'Y'.
*    CASE CRT-LGART.
*      WHEN '3616'.
*        TEMPCT = TEMPCT + CRT-BETRG.
*    ENDCASE.
*  ENDLOOP.
*"End
*    LOOP AT TRT .
*
*      CASE TRT-LGART.
*
**        WHEN TSAN-MONWT.
**           TEMPCT = TEMPCT + TRT-BETRG.
*
*        WHEN TSAN-ANNWT.
*           TEMPCT = TEMPCT + TRT-BETRG.
*
*
*      ENDCASE.
*
*    ENDLOOP.

    LOOP AT tcrt.
      CASE tcrt-lgart.
        WHEN '1001'.
          IF tcrt-cumty EQ 'Y'.
            tgrsal = tgrsal + tcrt-betrg.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    disp_body-zzicnum = tsan-icnum.
    SELECT SINGLE gbdat FROM pa0002 INTO disp_body-zzdtob WHERE pernr = disp_body-pernr
                                                            AND begda LE sy-datum
                                                            AND endda GE sy-datum.

*    ** below query commented as mail received from Kamlesh patil on 01.04.2019 Date of joining is comming incorrect example employee 107 and 1033
***    new logic to fetch date of join is to select first date of table pa0001, oldest start date from this table.
*       SELECT SINGLE begda FROM pa0000 INTO disp_body-zzdtoj WHERE pernr = disp_body-pernr.

    SELECT SINGLE min( begda ) from pa0001 INTO disp_body-zzdtoj WHERE pernr = disp_body-pernr.



*    select single gbdat from pa0002 into disp_body-zzdtol where pernr = disp_body-pernr
*                                                            and subty = 'I7'
*                                                            and begda le sy-datum
*                                                            and endda ge sy-datum.



    SELECT SINGLE begda FROM pa0302 INTO disp_body-zzdtol WHERE pernr = disp_body-pernr
                                                          AND massn = 'I7'.
    SELECT SINGLE kostl FROM pa0001 INTO disp_body-kostl WHERE pernr = disp_body-pernr
                                                          AND begda LE sy-datum
                                                          AND endda GE sy-datum.
    IF sy-subrc EQ 0.
      SELECT SINGLE ktext FROM cskt INTO disp_body-ktext
        WHERE kostl = disp_body-kostl.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = disp_body-kostl
        IMPORTING
          output = disp_body-kostl.
    ENDIF.

  ENDIF.
  "Anees
  IF disp_body-zzdtoj IS NOT INITIAL.

    financial_end = pnpendda.
    IF financial_end IS INITIAL.
      financial_end = '20110331'.
    ENDIF.
    CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
      EXPORTING
        i_datum_bis             = financial_end
        i_datum_von             = disp_body-zzdtoj
*     I_KZ_INCL_BIS           = ' '
*     I_KZ_VOLLE_MONATE       = 'X'
      IMPORTING
        e_monate                = experience    "Experience in months
              .

    experience = experience / 12.               "Experience in years

    IF experience < 5.
      tempct = tgrsal * 10 / 100.
    ELSEIF experience >= 5 AND experience < 10.
      tempct = tgrsal * 125 / 1000.
    ELSEIF experience >= 10.
      tempct = tgrsal * 15 / 100.
    ENDIF.
  ENDIF.
  "End

  IF tempct EQ 0.
    MOVE tempct TO disp_body-empct.
    MOVE tgrsal TO disp_body-grsal.
*     PERFORM LEAD_SPC_OR_SPC USING '8' CHANGING DISP_BODY-EMPCT.
*     PERFORM LEAD_SPC_OR_SPC USING '8' CHANGING DISP_BODY-GRSAL.
    APPEND disp_body.
  ELSE.
    MOVE tempct TO disp_body-empct.
    MOVE tgrsal TO disp_body-grsal.
*     PERFORM LEAD_SPC_OR_SPC USING '8' CHANGING DISP_BODY-EMPCT.
*     PERFORM LEAD_SPC_OR_SPC USING '8' CHANGING DISP_BODY-GRSAL.
    APPEND disp_body.
  ENDIF.

ENDFORM.                              " FILL_DISP_BODY

*&---------------------------------------------------------------------*
*&      Form  IMPORT_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_results.

  CLEAR : trgdir, tsan, trt.
  REFRESH : trgdir, tsan, trt.

  cd-key-pernr = pernr-pernr.
  rp-imp-c2-cu.

  CHECK rp-imp-cd-subrc EQ 0.

* Read IN Cluster.
  LOOP AT rgdir WHERE fpbeg >= pn-begda AND fpend <= pn-endda.
    MOVE-CORRESPONDING rgdir TO trgdir.
    APPEND trgdir.
  ENDLOOP.

* Read the last record.
  SORT trgdir BY seqnr DESCENDING.
  READ TABLE trgdir INDEX 1.
  rx-key-seqno = trgdir-seqnr.
  rx-key-pernr = pernr-pernr.
  rp-imp-c2-in.

  CHECK rp-imp-in-subrc EQ 0.

  tsan[] = san[].
  trt[]  = rt[].
  tcrt[] = crt[].

ENDFORM.                               " IMPORT_RESULTS
