*----------------------------------------------------------------------*
*   INCLUDE ZP000900                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP000900 MESSAGE-ID RP.
TABLES: P0009.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0009 DEFAULT P0009.

DATA: PSAVE LIKE P0009.
