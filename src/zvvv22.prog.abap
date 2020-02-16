*&---------------------------------------------------------------------*
*& Report  ZVVV22
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZVVV22.

**data: i_ekko type
* DATA : i_ekko LIKE ekko OCCURS 0 WITH HEADER LINE.
*
*
*READ TABLE i_ekko  binarysearch
*write:i_ekko-ebeln.


DATA: spfli_tab TYPE SORTED TABLE OF spfli
                WITH UNIQUE KEY carrid connid,
      spfli_key LIKE LINE OF spfli_tab.

FIELD-SYMBOLS <spfli> TYPE spfli.

SELECT *
       FROM spfli
       INTO TABLE spfli_tab
       WHERE carrid = 'LH'.


spfli_key-carrid = 'LH'.
spfli_key-connid = '0400'.

READ TABLE spfli_tab FROM spfli_key assigning <spfli>.

write :  spfli_key-carrid,
      spfli_key-connid.

IF sy-subrc = 0.
  ...
ENDIF.

...

READ TABLE spfli_tab
           WITH TABLE KEY carrid = 'LH' connid = '2402'
           ASSIGNING <spfli>.


write:spfli_key-carrid.
*write :spfli_key-connid.

IF sy-subrc = 0.
  ...
ENDIF.
