*----------------------------------------------------------------------*
***INCLUDE LZINV_APROVADORI01 .
*----------------------------------------------------------------------*
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
      DYNPROFIELD     = 'ZINV_APROVADOR-APROVADOR'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_USR
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.
ENDMODULE.                 " SEARCH_APROV  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_TIPO INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
              TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_TIP OCCURS 0,
          TIPO      TYPE ZINV_APROVADOR-TIPO,
          TEXT1     TYPE T012T-TEXT1,
         END OF TL_TIP.
  REFRESH TL_TIP.
  CLEAR TL_TIP.


  TL_TIP-TIPO     = '01'.
  TL_TIP-TEXT1    = 'INVOICE - Terceiros'.
  APPEND TL_TIP.

  TL_TIP-TIPO     = '02'.
  TL_TIP-TEXT1    = 'Performance'.
  APPEND TL_TIP.

  TL_TIP-TIPO     = '03'.
  TL_TIP-TEXT1    = 'Adiantamento'.
  APPEND TL_TIP.

    TL_TIP-TIPO     = '04'.
  TL_TIP-TEXT1    = 'INVOICE - Grupo'.
  APPEND TL_TIP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'TIPO'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZINV_APROVADOR-TIPO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TIP
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO_OPERACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_TIPO_OPERACAO INPUT.
  DATA: TL_RETURN_TAB_TP TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
         TL_DSELC_TP      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_TIPOO OCCURS 0,
          TP_OPERACAO      TYPE ZFIT0043-TP_OPERACAO,
          DS_OPERACAO      TYPE ZFIT0043-DS_OPERACAO,
          STATUS_CTB       TYPE ZFIT0043-STATUS_CTB,
          LIQUIDAR         TYPE ZFIT0043-LIQUIDAR,
         END OF TL_TIPOO.

  SELECT TP_OPERACAO DS_OPERACAO STATUS_CTB LIQUIDAR
    FROM ZFIT0043
    INTO TABLE TL_TIPOO
     where spras = sy-langu.

  SORT TL_TIPOO BY TP_OPERACAO.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'TP_OPERACAO'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0043-TP_OPERACAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TIPOO
      RETURN_TAB      = TL_RETURN_TAB_TP
      DYNPFLD_MAPPING = TL_DSELC_TP.

*  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*    EXPORTING
*      FUNCTIONCODE           = '=ENT'
*    EXCEPTIONS
*      FUNCTION_NOT_SUPPORTED = 1
*      OTHERS                 = 2.
ENDMODULE.                 " SEARCH_TIPO_OPERACAO  INPUT
