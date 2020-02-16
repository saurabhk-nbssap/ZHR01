*----------------------------------------------------------------------*
*   INCLUDE ZP000220                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  MOD_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODULE_PBO_0002 OUTPUT.

  if p0002-KONFE = '10'.
    loop at screen.
      if screen-NAME = 'P0002-ZZREG_OTHERS'.
        screen-active = '1'.
        modify screen.
      endif.
    endloop.
  elseif p0002-KONFE ne '10'.
    loop at screen.
      if screen-NAME = 'P0002-ZZREG_OTHERS'.
        screen-active = '0'.
        modify screen.
      endif.
    endloop.
  endif.
ENDMODULE.                 " MOD_PBO  OUTPUT
