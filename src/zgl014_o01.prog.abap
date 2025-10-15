*&---------------------------------------------------------------------*
*&  Include           ZGL014_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR  'Z001'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIAR_TELA  OUTPUT
*&------------------------------------ ---------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIAR_TELA OUTPUT.

  IF WG_ACAO IS INITIAL.
    CONCATENATE ICON_HEADER  TEXT-B01 INTO WG_MENSLAN.
    WGLT034-LOTE = TEXT-L01.
    WGLT034-DESCR_LOTE = TEXT-L02.
    WGLT034-BUKRS = TEXT-L03.
    WGZGLT034-USNAM = TEXT-L04.
    WGLT034-DEP_RESP = TEXT-L05.
    WG_ACAO = C_SEARCH.

    REFRESH: TG_FIELDS.
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR1'
                                  C_1       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

    SELECT SINGLE BNAME
      FROM USER_ADDR
      INTO VG_BNAME
      WHERE BNAME      = SY-UNAME
      AND   DEPARTMENT = 'Amaggi Holanda'.
    IF SY-SUBRC = 0.
      WG_ZGLT034-BUKRS = '0201'.
    ENDIF.

  ENDIF.
ENDMODULE.                 " INICIAR_TELA  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD. "'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
