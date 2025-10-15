*----------------------------------------------------------------------*
***INCLUDE LZMMT0034F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZMMT0034-CPUDT = SY-DATUM.
  ZMMT0034-CPUTM = SY-UZEIT.
  ZMMT0034-USNAM = SY-UNAME.
ENDFORM.                    "FETCH_VALUE
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_PORTO INPUT.
  DATA: TL_RETURN_TAB2 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
             TL_DSELC2      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_PORTO OCCURS 0,
          DS_PORTO TYPE ZNOM_TRANSPORTE-DS_PORTO,
         END OF TL_PORTO.

  SELECT DISTINCT DS_PORTO
     FROM  ZNOM_TRANSPORTE INTO TABLE TL_PORTO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DS_PORTO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZMMT0034'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_PORTO
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.
ENDMODULE.                 " SEARCH_PORTO  INPUT
