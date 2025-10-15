*----------------------------------------------------------------------*
***INCLUDE LZIMP_APROVADORI01 .
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

  select dep_resp dep_resp_desc
    from zimp_cad_depto
    into table tl_dep.

*  TL_DEP-DEP_RESP = '01'.
*  TL_DEP-TEXT1    = 'Tributos Indiretos'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '02'.
*  TL_DEP-TEXT1    = 'Tributos Diretos'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '03'.
*  TL_DEP-TEXT1    = 'Recursos Humanos'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '04'.
*  TL_DEP-TEXT1    = 'Jurídico'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '05'.
*  TL_DEP-TEXT1    = 'Patrimônio'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '06'.
*  TL_DEP-TEXT1    = 'Sustentabilidade'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '07'.
*  TL_DEP-TEXT1    = 'Tributário'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '08'.
*  TL_DEP-TEXT1    = 'AMAGGI & LDCommodities'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '09'.
*  TL_DEP-TEXT1    = 'Contabilidade'.
*  APPEND TL_DEP.

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
