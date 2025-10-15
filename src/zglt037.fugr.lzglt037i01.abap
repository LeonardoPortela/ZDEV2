*----------------------------------------------------------------------*
***INCLUDE LZGLT037I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DEPTO INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
              TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_DEP OCCURS 0,
          DEP_RESP  TYPE ZIMP_CAD_IMPOSTO-DEP_RESP,
          TEXT1     TYPE T012T-TEXT1,
         END OF TL_DEP.
  REFRESH TL_DEP.
  CLEAR TL_DEP.

  SELECT DEP_RESP DEP_RESP_DESC
    FROM ZIMP_CAD_DEPTO
    INTO TABLE TL_DEP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DEP_RESP'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_APROVADOR-DEP_RESP'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DEP
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_APROV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_APROV INPUT.
  DATA: TL_RETURN_TAB2 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
              TL_DSELC2      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_USR OCCURS 0,
          BNAME   TYPE V_USR_NAME-BNAME,
          NAME_TEXT  TYPE V_USR_NAME-NAME_TEXT,
         END OF TL_USR.

  SELECT BNAME  NAME_TEXT
     FROM  V_USR_NAME INTO TABLE TL_USR.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BNAME'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_APROVADOR-APROVADOR'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_USR
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.
ENDMODULE.                 " SEARCH_APROV  INPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZAR OUTPUT.
ZGLT037BUKRS = TEXT-A01.
ZGLT037BUKRS_ATE = TEXT-A02.
ZGLT037DEP_RESP = TEXT-A03.
ZGLT037PGT_FORN = TEXT-A04.
ZGLT037WAERS = TEXT-A05.
ZGLT037NIVEL = TEXT-A06.
ZGLT037APROVADOR = TEXT-A07.
ZGLT037VALOR_DE = TEXT-A08.
ZGLT037VALOR_ATE = TEXT-A09.
ZGLT037DATA_ATUAL = TEXT-A10.
ZGLT037HORA_ATUAL = TEXT-A11.
ZGLT037USUARIO = TEXT-A12.
ENDMODULE.                 " INICIALIZAR  OUTPUT
