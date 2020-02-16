*&---------------------------------------------------------------------*
*&  Include           ZXPADU02
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(TCLAS) LIKE  PSPAR-TCLAS
*"     VALUE(INNNN) LIKE  PRELP STRUCTURE  PRELP
*"     VALUE(PSAVE) LIKE  PRELP STRUCTURE  PRELP
*"     VALUE(IPSYST) LIKE  PSYST STRUCTURE  PSYST
*"     VALUE(I001P) LIKE  T001P STRUCTURE  T001P
*"     VALUE(I503) LIKE  T503 STRUCTURE  T503
*"  EXPORTING
*"     VALUE(INNNN) LIKE  PRELP STRUCTURE  PRELP
*"     VALUE(SHOW_DATA_AGAIN)
*"  CHANGING
*"     VALUE(IPREF) LIKE  PREF STRUCTURE  PREF
*"  EXCEPTIONS
*"      ERROR_OCCURED
*"----------------------------------------------------------------------
* data declaration in ZXPADTOP     "Global Data
TABLES : lfa1,lfb1.
  TYPES: i0000 TYPE p0000,
        i0001 TYPE p0001,

        i0019 TYPE p0019.
  DATA :     i2001 LIKE p2001.
  DATA : it_0019 TYPE TABLE OF i0019.
  DATA : st_0019 TYPE p0019.
  DATA : gs_lfa1 TYPE lfa1,
         gs_lfb1 TYPE lfb1,
         lifnr   TYPE lfa1-lifnr.

  DATA : v_persg TYPE p0001-persg.
  CASE innnn-infty.


    WHEN '2001'.
      CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
        EXPORTING
          prelp = innnn
        IMPORTING
          pnnnn = i2001.

      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          tclas                 = 'A'
          pernr                 = i2001-pernr
          infty                 = '0019'
          begda                 = sy-datum
*         ENDDA                 = sy-datum
*         BYPASS_BUFFER         = ' '
*         LEGACY_MODE           = ' '
*       IMPORTING
*         SUBRC                 =
        TABLES
          infty_tab             = it_0019
       EXCEPTIONS
         infty_not_found       = 1
         OTHERS                = 2
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.

        CASE i2001-subty.

          WHEN 'PL'.

            SELECT SINGLE persg FROM pa0001 INTO v_persg
                                WHERE pernr EQ i2001-pernr
                                  AND begda LE i2001-begda
                                  AND endda GE i2001-begda.
            IF NOT v_persg IS INITIAL.
              CASE v_persg.
                WHEN 'M' OR 'D' OR 'N'.

                  READ TABLE it_0019 INTO st_0019 WITH KEY subty = 'I8'.
                  IF sy-subrc EQ 0.
                    IF i2001-begda LT st_0019-termn.
                      MESSAGE e016(rp) WITH 'Leave'  'not allowed'.

**       S-Message for infotype 0008 (see psyst-msgtp).

                    ENDIF.
                  ENDIF.
                WHEN 'S' OR 'W'.

                  READ TABLE it_0019 INTO st_0019 WITH KEY subty = 'I9'.
                  IF sy-subrc EQ 0.
                    IF i2001-begda LT st_0019-termn.
                      MESSAGE e016(rp) WITH 'Leave'  'not allowed'.

**       S-Message for infotype 0008 (see psyst-msgtp).

                    ENDIF.
                  ENDIF.

              ENDCASE.


            ENDIF.

          WHEN 'SLA'.

            SELECT SINGLE persg FROM pa0001 INTO v_persg
                                WHERE pernr EQ i2001-pernr
                                  AND begda LE i2001-begda
                                  AND endda GE i2001-begda.
            IF NOT v_persg IS INITIAL.
              CASE v_persg.
                WHEN 'S'.

                  READ TABLE it_0019 INTO st_0019 WITH KEY subty = 'I8'.
                  IF sy-subrc EQ 0.
                    IF i2001-begda LT st_0019-termn.
                      MESSAGE e016(rp) WITH 'Leave'  'not allowed'.

**       S-Message for infotype 0008 (see psyst-msgtp).

                    ENDIF.
                  ENDIF.
              ENDCASE.
            ENDIF.
        ENDCASE.

      ENDIF.

*      IF P0008-LGA01 EQ '1234' AND IPSYST-PERSK EQ 'DE'.
*        MESSAGE S016(RP) WITH 'wage type' I0008-LGA01 'not allowed'
*                         RAISING ERROR_OCCURD.
**       S-Message for infotype 0008 (see psyst-msgtp).
*      ENDIF.
  ENDCASE.

******************* Added By Rahul to add Payment BLock during Employee Separation ****************
IF sy-tcode = 'PA40'.
  IF ipsyst-massn = 'I7'."Separation Code.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input   = innnn-pernr
     IMPORTING
       OUTPUT   = lifnr.

***    BREAK IBM_AMS.
    SELECT SINGLE * FROM lfa1 INTO gs_lfa1
      WHERE lifnr = lifnr.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM lfb1 INTO gs_lfb1
      WHERE lifnr = lifnr.
      IF sy-SUBRC = 0.
        UPDATE lfB1 SET ZAHLS = 'B' "Block Payment.
        WHERE lifnr = gs_lfb1-lifnr
        AND   bukrs = gs_lfb1-bukrs.
        CLEAR : gs_lfb1 , gs_lfa1.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
******************* Added By Rahul to add Payment BLock during Employee Separation ****************
