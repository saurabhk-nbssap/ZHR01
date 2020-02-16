*&---------------------------------------------------------------------*
*& Include          ZXP02U01
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(DATE) LIKE  P0001-BEGDA
*"             VALUE(PNR) LIKE  P0001-PERNR
*"       EXPORTING
*"             VALUE(NAME) LIKE  P0001-ENAME
*"             VALUE(PERSA) LIKE  P0001-WERKS
*"             VALUE(BUKRS) LIKE  P0001-BUKRS
*"             VALUE(KOSTL) LIKE  P0001-KOSTL
*"             VALUE(MOLGA) LIKE  T500P-MOLGA
*"             VALUE(RET)
*"       EXCEPTIONS
*"              PERSON_UNKNOWN
*"              PERSON_NOT_ACTIVE
*"              DATA_FAULT
*"              SAP_CHECK
*"----------------------------------------------------------------------

BREAK 10106.
