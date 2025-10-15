*----------------------------------------------------------------------*
***INCLUDE ZFIR0044_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: W_ANSWER,
        TL_ZFIT0065 TYPE TABLE OF ZFIT0065 WITH HEADER LINE,
        WL_ZFIT0065 TYPE ZFIT0065.

  DATA: VBUKRS      TYPE ZFIT0065-BUKRS,
        VNRO_CTO    TYPE ZFIT0065-NRO_CTO,
        VNRO_PAR    TYPE ZFIT0065-NRO_PAR.


  CASE OK-CODE.
    WHEN 'COPY'.
      IF WG_CADCTO-BUKRS IS NOT INITIAL AND WG_CADCTO-NRO_CTO IS NOT  INITIAL.
        SELECT  *
             FROM ZFIT0065
             INTO TABLE TL_ZFIT0065
             WHERE BUKRS    EQ WG_CADCTO-BUKRS
             AND   NRO_CTO  EQ WG_CADCTO-NRO_CTO.
        IF TL_ZFIT0065[] IS NOT INITIAL.
          SORT  TL_ZFIT0065 BY NRO_PAR DESCENDING.
          READ TABLE TL_ZFIT0065 INDEX 1.

          SELECT SINGLE *
               FROM ZFIT0065
               INTO WL_ZFIT0065
               WHERE BUKRS    EQ WG_CADCTO-BUKRS
               AND   NRO_CTO  EQ WG_CADCTO-NRO_CTO
               AND   NRO_PAR  EQ TL_ZFIT0065-NRO_PAR.

          IF SY-SUBRC = 0.
            VBUKRS      = WG_CADCTO-BUKRS.
            VNRO_CTO    = WG_CADCTO-NRO_CTO.
            VNRO_PAR    = WG_CADCTO-NRO_PAR.
            MOVE-CORRESPONDING WL_ZFIT0065 TO WG_CADCTO.
            WG_CADCTO-BUKRS   = VBUKRS.
            WG_CADCTO-NRO_CTO = VNRO_CTO.
            WG_CADCTO-NRO_PAR = VNRO_PAR.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
         TEXT_QUESTION               = 'Confirma a exclusão do contrato?'
         TEXT_BUTTON_1               = 'Sim'(001)
         ICON_BUTTON_1               = 'ICON_OKAY '
         TEXT_BUTTON_2               = 'Não'(002)
         ICON_BUTTON_2               = 'ICON_CANCEL'
         DEFAULT_BUTTON              = '1'
         DISPLAY_CANCEL_BUTTON       = ' '
*         USERDEFINED_F1_HELP         = ' '
         START_COLUMN                = 25
         START_ROW                   = 6
*         POPUP_TYPE                  =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        ANSWER                      = W_ANSWER
*       TABLES
*         PARAMETER                   =
      EXCEPTIONS
        TEXT_NOT_FOUND              = 1
        OTHERS                      = 2.
      .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        PERFORM ELIMINAR_CONTRATO.
      ENDIF.
    WHEN C_SEARCH.
      CLEAR XMODIF.
      PERFORM BUSCA_DADOS.
      PERFORM VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = ''
          I_REPID       = SY-REPID
          I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
    WHEN C_SAVE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CLEAR WG_ACAO.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                           'GR2'
                              C_0       "INPUT 1     NO INPUT 0
                              C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_0       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
    WHEN C_ADD.
      IF WG_CADCTO-BUKRS IS NOT INITIAL AND WG_CADCTO-NRO_CTO IS NOT INITIAL AND WG_ACAO NE C_DISPLA.
        CLEAR XMODIF.
        WG_ACAO = C_ATUALI.
        PERFORM LIMPA_CAMPOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0


      ELSEIF WG_ACAO = C_DISPLA.
        WG_ACAO = C_ADD.
        PERFORM LIMPA_CAMPOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        LEAVE TO SCREEN 100.
      ENDIF.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_CADCTO-NRO_CTO IS NOT INITIAL.
        IF WG_ACAO = C_MODIF.
          CLEAR WG_ACAO.
          REFRESH: TG_FIELDS.
          PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
          PERFORM TRATA_CAMPOS USING SPACE
                                    'GR1'
                                     C_1       "INPUT 1     NO INPUT 0
                                     C_0.      "INVISIBLE 1 VISIBLE 0
        ELSE.
          WG_ACAO = C_MODIF.
          REFRESH: TG_FIELDS.
          PERFORM TRATA_CAMPOS USING SPACE
                                     'GR2'
                                        C_1       "INPUT 1     NO INPUT 0
                                        C_0.      "INVISIBLE 1 VISIBLE 0
          PERFORM TRATA_CAMPOS USING SPACE
                                    'GR1'
                                     C_0       "INPUT 1     NO INPUT 0
                                     C_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.
      ENDIF.
    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_EXIT.

      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_OPER INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_OPER OCCURS 0,
          COD_OPER      TYPE ZFIT0063-COD_OPER,
          DESCR         TYPE ZFIT0063-DESCR,
         END OF TL_OPER.

  SELECT COD_OPER DESCR
    FROM ZFIT0063
    INTO TABLE TL_OPER.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'COD_OPER'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0063-COD_OPER'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_OPER
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

  IF TL_RETURN_TAB IS NOT INITIAL.
    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_OPER WITH KEY COD_OPER = TL_RETURN_TAB-FIELDVAL.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_OPER = TL_OPER-DESCR.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_POS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_POS INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_POS OCCURS 0,
          POSICAO       TYPE ZFIT0065-POSICAO,
          DESCR         TYPE ZFIT0063-DESCR,
         END OF TL_POS.

  REFRESH TL_POS.

  TL_POS-POSICAO = 'C'.
  TL_POS-DESCR   = 'Compra'.
  APPEND TL_POS.

  TL_POS-POSICAO = 'V'.
  TL_POS-DESCR   = 'Venda'.
  APPEND TL_POS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'POSICAO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-POSICAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_POS
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

  IF TL_RETURN_TAB IS NOT INITIAL.
    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_POS WITH KEY POSICAO = TL_RETURN_TAB-FIELDVAL.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_POS = TL_POS-DESCR.
    ENDIF.
  ENDIF.
ENDMODULE.                 " SEARCH_POS  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_TIPO INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_TIPO OCCURS 0,
          TP_OPERACAO   TYPE ZFIT0065-TP_OPERACAO,
          DESCR         TYPE ZFIT0063-DESCR,
         END OF TL_TIPO.

  REFRESH TL_TIPO.

  TL_TIPO-TP_OPERACAO = 'O'.
  TL_TIPO-DESCR       = 'Operacional'.
  APPEND TL_TIPO.

  TL_TIPO-TP_OPERACAO = 'F'.
  TL_TIPO-DESCR       = 'Financeiro'.
  APPEND TL_TIPO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'TP_OPERACAO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-TP_OPERACAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TIPO
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

  IF TL_RETURN_TAB IS NOT INITIAL.
    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_TIPO WITH KEY TP_OPERACAO = TL_RETURN_TAB-FIELDVAL.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_TP = TL_TIPO-DESCR.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TAXA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_TAXA INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_TAXA OCCURS 0,
          TP_TX_ATIVA   TYPE ZFIT0065-TP_TX_ATIVA,
          DESCR         TYPE ZFIT0063-DESCR,
         END OF TL_TAXA.

  REFRESH TL_TAXA.

  TL_TAXA-TP_TX_ATIVA = '1'.
  TL_TAXA-DESCR       = 'Composto - 252'.
  APPEND TL_TAXA.

  TL_TAXA-TP_TX_ATIVA = '2'.
  TL_TAXA-DESCR       = 'Simples - 360'.
  APPEND TL_TAXA.

  TL_TAXA-TP_TX_ATIVA = '3'.
  TL_TAXA-DESCR       = 'Composto - 360'.
  APPEND TL_TAXA.

  TL_TAXA-TP_TX_ATIVA = '4'.
  TL_TAXA-DESCR       = 'Simples - 365'.
  APPEND TL_TAXA.

  TL_TAXA-TP_TX_ATIVA = '5'.
  TL_TAXA-DESCR       = 'Composto - 365'.
  APPEND TL_TAXA.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'TP_TX_ATIVA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-TP_TX_ATIVA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TAXA
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

  IF TL_RETURN_TAB IS NOT INITIAL.
    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_TAXA WITH KEY TP_TX_ATIVA = TL_RETURN_TAB-FIELDVAL.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_ATIVA = TL_TAXA-DESCR.
    ENDIF.
  ENDIF.
ENDMODULE.                 " SEARCH_TAXA  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TAXAP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_TAXAP INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_TAXAP OCCURS 0,
          TP_TX_PASSIVA TYPE ZFIT0065-TP_TX_PASSIVA,
          DESCR         TYPE ZFIT0063-DESCR,
         END OF TL_TAXAP.

  REFRESH TL_TAXAP.

  TL_TAXAP-TP_TX_PASSIVA = '1'.
  TL_TAXAP-DESCR       = 'Compostos - 252'.
  APPEND TL_TAXAP.

  TL_TAXAP-TP_TX_PASSIVA = '2'.
  TL_TAXAP-DESCR       = 'Simples - 360'.
  APPEND TL_TAXAP.

  TL_TAXAP-TP_TX_PASSIVA = '3'.
  TL_TAXAP-DESCR       = 'Composto - 360'.
  APPEND TL_TAXAP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'TP_TX_PASSIVA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-TP_TX_PASSIVA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TAXAP
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

  IF TL_RETURN_TAB IS NOT INITIAL.
    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_TAXAP WITH KEY TP_TX_PASSIVA  = TL_RETURN_TAB-FIELDVAL.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_PASSIVA = TL_TAXAP-DESCR.
    ENDIF.
  ENDIF.


ENDMODULE.                 " SEARCH_TAXAP  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_CTO INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_CTO OCCURS 0,
          NRO_CTO        TYPE ZFIT0065-NRO_CTO,
          NRO_PAR        TYPE ZFIT0065-NRO_PAR,
          BANCO          TYPE ZFIT0065-BANCO,
          POSICAO        TYPE ZFIT0065-POSICAO,
          NATUREZA_CTO   TYPE ZFIT0065-NATUREZA_CTO,
          TP_OPERACAO    TYPE ZFIT0065-TP_OPERACAO,
          DT_INICIO_CTO  TYPE ZFIT0065-DT_INICIO_CTO,
          DT_FIM_CTO     TYPE ZFIT0065-DT_FIM_CTO,
          VLR_CTO_R      TYPE ZFIT0065-VLR_CTO_R,
          VLR_CTO_US     TYPE ZFIT0065-VLR_CTO_US,
         END OF TL_CTO.

  DATA: L_DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
  REFRESH L_DYNPFIELDS.
  CLEAR   L_DYNPFIELDS.

  IF WG_CADCTO-BUKRS IS  INITIAL.
    L_DYNPFIELDS-FIELDNAME  = 'WG_CADCTO-BUKRS'.
    APPEND L_DYNPFIELDS.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME     = SY-REPID
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = L_DYNPFIELDS.
    READ TABLE L_DYNPFIELDS INDEX 1.
    MOVE L_DYNPFIELDS-FIELDVALUE TO WG_CADCTO-BUKRS.
  ENDIF.

  SELECT NRO_CTO NRO_PAR BANCO POSICAO NATUREZA_CTO TP_OPERACAO DT_INICIO_CTO DT_FIM_CTO VLR_CTO_R VLR_CTO_US
    FROM ZFIT0065
    INTO TABLE TL_CTO
    WHERE BUKRS = WG_CADCTO-BUKRS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'NRO_CTO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-NRO_CTO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_CTO
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
*
*  IF TL_RETURN_TAB IS NOT INITIAL.
*    READ TABLE TL_RETURN_TAB INDEX 1.
*    READ TABLE TL_CTO WITH KEY NRO_CTO = TL_RETURN_TAB-FIELDVAL.
*    IF SY-SUBRC = 0.
*      WG_CADCTO-NRO_PAR = TL_CTO-NRO_PAR.
*    ENDIF.
*  ENDIF.

ENDMODULE.                 " SEARCH_CTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BAN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BAN INPUT.
  REFRESH: TL_RETURN_TAB,TL_DSELC.

  DATA: BEGIN OF TL_BAN OCCURS 0,
          BANCO         TYPE ZFIT0068-BANCO,
         END OF TL_BAN.

  SELECT BANCO
    FROM ZFIT0068
    INTO TABLE TL_BAN.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BANCO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0065-BANCO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_BAN
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.                 " SEARCH_BAN  INPUT
