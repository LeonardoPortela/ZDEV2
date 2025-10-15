*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9002.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.

  SET PF-STATUS 'PF9002'.
  SET TITLEBAR 'TL9002'.

  IF WA_ADD_NFE_9002-N55_CHAVE_ACESSO IS NOT INITIAL.
    PERFORM BUSCAR_INDO_NOTA_DIGITADA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002_EXIT INPUT.
  WA_ADD_NFE_9002-CK_INCLUIR = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_INDO_NOTA_DIGITADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCAR_INDO_NOTA_DIGITADA .

  DATA: QTD TYPE I,
        NFE TYPE REF TO ZCL_NFE_INBOUND.

  QTD = STRLEN( WA_ADD_NFE_9002-N55_CHAVE_ACESSO ).

  WA_ADD_NFE_9002-CK_INCLUIR = ABAP_FALSE.

  IF QTD NE 44.
    MESSAGE S034 WITH WA_ADD_NFE_9002-N55_CHAVE_ACESSO.
    EXIT.
  ENDIF.

  TRY .
      CREATE OBJECT NFE
        EXPORTING
          I_CHAVE_NFE = WA_ADD_NFE_9002-N55_CHAVE_ACESSO.

      TRY .
          NFE->SET_INFO_SAP( ).
        CATCH ZCX_NFE_INBOUND_EXCEPTION.
        CATCH ZCX_CADASTRO.
        CATCH ZCX_PEDIDO_COMPRA_EXCEPTION.
      ENDTRY.

      DATA(INFO_NOTA) = NFE->GET_INFO_NOTA( ).
      NFE->FREE( ).
      CLEAR: NFE.
      WA_ADD_NFE_9002-BRANCH     = INFO_NOTA-NFE_BASE-F_TOMADORA.
      WA_ADD_NFE_9002-BUKRS      = INFO_NOTA-NFE_BASE-E_TOMADORA.
      WA_ADD_NFE_9002-DOCNUM_NFE = INFO_NOTA-NFE_BASE-DOCNUM_NFE.
      WA_ADD_NFE_9002-PARID      = INFO_NOTA-NFE_BASE-P_EMISSOR.
      SELECT SINGLE STCD3 INTO WA_ADD_NFE_9002-PARID_IE
        FROM LFA1
       WHERE LIFNR EQ INFO_NOTA-NFE_BASE-P_EMISSOR.
      WA_ADD_NFE_9002-NFTOT      = INFO_NOTA-NFE_BASE-VL_TOTAL.
      WA_ADD_NFE_9002-DT_EMISSAO = INFO_NOTA-NFE_BASE-DT_EMISSAO.
      WA_ADD_NFE_9002-NUMERO     = INFO_NOTA-NFE_BASE-NUMERO.
      WA_ADD_NFE_9002-SERIE      = INFO_NOTA-NFE_BASE-SERIE.
      WA_ADD_NFE_9002-NTGEW      = 0.

      LOOP AT INFO_NOTA-NFE_BASE-ITENS INTO DATA(WA_ITEM).
        TRANSLATE WA_ITEM-PROD_UND_COMERCI TO UPPER CASE.
        CASE WA_ITEM-PROD_UND_COMERCI.
          WHEN 'KG'.
            ADD WA_ITEM-PROD_QTD_COMERCI TO WA_ADD_NFE_9002-NTGEW.
          WHEN 'TO'.
            WA_ITEM-PROD_QTD_COMERCI = WA_ITEM-PROD_QTD_COMERCI * 1000.
            ADD WA_ITEM-PROD_QTD_COMERCI TO WA_ADD_NFE_9002-NTGEW.
        ENDCASE.
        WA_ADD_NFE_9002-CFOP = WA_ITEM-PROD_CFOP.
      ENDLOOP.

      SELECT SINGLE BUTXT INTO WA_ADD_NFE_9002-BUTXT
        FROM T001
       WHERE BUKRS EQ INFO_NOTA-NFE_BASE-E_TOMADORA.

      SELECT SINGLE NAME INTO WA_ADD_NFE_9002-NAME
        FROM J_1BBRANCH
       WHERE BUKRS EQ INFO_NOTA-NFE_BASE-E_TOMADORA
         AND BRANCH EQ INFO_NOTA-NFE_BASE-F_TOMADORA.

      SELECT SINGLE NAME1 INTO WA_ADD_NFE_9002-NAME1
        FROM LFA1
       WHERE LIFNR EQ WA_ADD_NFE_9002-PARID.

      WA_ADD_NFE_9002-CK_INCLUIR = ABAP_TRUE.

    CATCH ZCX_NFE_INBOUND_EXCEPTION INTO DATA(EX_NFE_INBOUND_EXCEPTION).
      IF NFE IS NOT INITIAL.
        NFE->FREE( ).
      ENDIF.
      EX_NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).
      IF NFE IS NOT INITIAL.
        NFE->FREE( ).
      ENDIF.
      EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
      IF EX_CADASTRO->MSGID = 'ZNFE_DISTRI' AND EX_CADASTRO->MSGNO = 103.
        MESSAGE I035.
      ENDIF.
  ENDTRY.

  "WA_ADD_NFE_9002-.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.
      CLEAR: OK_CODE.
      PERFORM BUSCAR_INDO_NOTA_DIGITADA.

      IF CK_ALTERADO_CHAVE EQ ABAP_TRUE.
        CK_ALTERADO_CHAVE = ABAP_FALSE.
        EXIT.
      ENDIF.

      IF WA_ADD_NFE_9002-CK_INCLUIR EQ ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CHAVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_INFO_CHAVE INPUT.
  CK_ALTERADO_CHAVE = ABAP_TRUE.
ENDMODULE.
