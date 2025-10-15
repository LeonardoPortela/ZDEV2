*----------------------------------------------------------------------*
***INCLUDE LZPMR0004F02 .
*----------------------------------------------------------------------*
MODULE VALIDATE_VALUE INPUT.
  DATA WL_T370 TYPE T370U.

  IF  ZPMR0004-ORDEM_EQUIP IS NOT INITIAL
  AND ZPMR0004-ORDEM_CC    IS NOT INITIAL.
    CLEAR: ZPMR0004-ORDEM_EQUIP,
           ZPMR0004-ORDEM_CC.
    MESSAGE 'Não pode ser definidos os dois tipo de ordem para uma categoria.' TYPE 'E'.
  ENDIF.

  IF ZPMR0004-EQTYP IS NOT INITIAL.
    SELECT SINGLE TYPTX
      FROM T370U
      INTO WL_T370
      WHERE EQTYP = ZPMR0004-EQTYP
       AND  SPRAS = SY-LANGU.

    IF SY-SUBRC IS NOT INITIAL.
      CLEAR ZPMR0004-EQTYP.
      MESSAGE 'Categoria de equipamento não existe.' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.                    "validate_value

*&---------------------------------------------------------------------*
*&      Module  BUSCA_CATEGORIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_CATEGORIA INPUT.
  DATA: TL_T370   TYPE TABLE OF T370U      WITH HEADER LINE,
        TL_RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC  TYPE TABLE OF DSELC      WITH HEADER LINE.

  SELECT *
    FROM T370U
    INTO TABLE TL_T370
    WHERE SPRAS = SY-LANGU.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'EQTYP'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'EQTYP'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_T370
      RETURN_TAB      = TL_RETURN
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.                 " BUSCA_CATEGORIA  INPUT
