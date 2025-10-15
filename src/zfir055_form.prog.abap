*----------------------------------------------------------------------*
***INCLUDE ZFIR055_FORM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ACTIVE  text
*----------------------------------------------------------------------*

FORM CHANGE_ROWS  USING    P_ACTIVE.

  DATA: LS_EDIT TYPE LVC_S_STYL,
        LT_EDIT TYPE LVC_T_STYL.

  IF WA_ALV IS NOT INITIAL.

    LOOP AT IT_CR INTO WA_CR.

      CLEAR: WA_CR-FIELD_STYLE, LS_EDIT, LT_EDIT.

      LS_EDIT-FIELDNAME = 'DT_VENCIMENTO'.
      IF ( WA_CR-LOTE         IS INITIAL ) AND
         ( WA_CR-DOC_LCTO     IS INITIAL ) AND
         ( WA_CR-DOC_CONTABIL IS INITIAL ) AND
         ( P_ACTIVE IS NOT INITIAL ).
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 10.
      INSERT LS_EDIT INTO TABLE LT_EDIT.

      LS_EDIT-FIELDNAME = 'VALOR'.
      IF ( WA_CR-LOTE         IS INITIAL ) AND
         ( WA_CR-DOC_LCTO     IS INITIAL ) AND
         ( WA_CR-DOC_CONTABIL IS INITIAL ) AND
         ( P_ACTIVE IS NOT INITIAL ).
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.

      INSERT LINES OF LT_EDIT INTO TABLE WA_CR-FIELD_STYLE.

      MODIFY IT_CR FROM WA_CR TRANSPORTING FIELD_STYLE.

      CLEAR: WA_CR.

    ENDLOOP.

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDIF.


  IF OBJ_ALV_EV_VENDA IS NOT INITIAL.

    LOOP AT IT_EVENTOS_VENDA INTO WA_EVENTOS_VENDA.

      REFRESH: IT_ZFIT0101.
      SELECT *
        FROM ZFIT0101
        INTO TABLE IT_ZFIT0101
       WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
         AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
         AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
         AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA
         AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV.

      CLEAR: WA_EVENTOS_VENDA-FIELD_STYLE, LS_EDIT, LT_EDIT.

      LS_EDIT-FIELDNAME = 'COD_EV'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 10.
      INSERT LS_EDIT INTO TABLE LT_EDIT.

      LS_EDIT-FIELDNAME = 'VALOR'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.

       LS_EDIT-FIELDNAME = 'COND_PGTO'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.


      LS_EDIT-FIELDNAME = 'FORMA_PGTO'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.


      LS_EDIT-FIELDNAME = 'QTD_PARCELAS'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.


      LS_EDIT-FIELDNAME = 'DATA_VENC'.
      IF IT_ZFIT0101[] IS INITIAL.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_EDIT-STYLE2 = SPACE.
      LS_EDIT-STYLE3 = SPACE.
      LS_EDIT-STYLE4 = SPACE.
      LS_EDIT-MAXLEN = 15.
      INSERT LS_EDIT INTO TABLE LT_EDIT.


      INSERT LINES OF LT_EDIT INTO TABLE WA_EVENTOS_VENDA-FIELD_STYLE.

      MODIFY IT_EVENTOS_VENDA FROM  WA_EVENTOS_VENDA TRANSPORTING FIELD_STYLE.

      CLEAR: WA_EVENTOS_VENDA.

    ENDLOOP.

    CALL METHOD OBJ_ALV_EV_VENDA->REFRESH_TABLE_DISPLAY.


  ENDIF.



ENDFORM.                    " CHANGE_ROWS
*&---------------------------------------------------------------------*
*&      Form  SALVAR_PARCELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALVAR_PARCELAS .

  DATA: VL_VALOR_TOT_EV TYPE ZFIT0104-VALOR,
        VL_MSG_EXIBIR   TYPE STRING,
        VL_MSG_AUX01    TYPE STRING,
        VL_MSG_AUX02    TYPE STRING,
        VL_MSG_AUX03    TYPE STRING.

  CLEAR: WA_CR.

  IT_CR_AUX[] = IT_CR.

  LOOP AT IT_CR INTO WA_CR WHERE DOC_CONTABIL IS INITIAL
                             AND LOTE         IS INITIAL
                             AND DOC_LCTO     IS INITIAL.

    CLEAR: VL_VALOR_TOT_EV.

    CLEAR WA_ZFIT0104.
    SELECT SINGLE *
      INTO WA_ZFIT0104
      FROM ZFIT0104
     WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
       AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA
       AND COD_EV      EQ WA_CR-COD_EV.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    "Verifica se parcelas estão totalizando com o valor do evento.
    CLEAR: VL_VALOR_TOT_EV.
    LOOP AT IT_CR_AUX INTO WA_CR_AUX WHERE COD_EV = WA_CR-COD_EV.
      ADD WA_CR_AUX-VALOR TO VL_VALOR_TOT_EV.
      CLEAR: WA_CR_AUX.
    ENDLOOP.

    IF VL_VALOR_TOT_EV NE WA_ZFIT0104-VALOR.
      CLEAR: VL_MSG_EXIBIR, VL_MSG_AUX01.
      VL_MSG_AUX01 = WA_CR-TIPO.
      VL_MSG_AUX02 = VL_VALOR_TOT_EV.
      VL_MSG_AUX03 = WA_ZFIT0104-VALOR.
      CONCATENATE 'Total ( R$' VL_MSG_AUX02 ') das parcelas do tipo:' VL_MSG_AUX01
                  ', não totalizam o valor do evento ( R$' VL_MSG_AUX03 ')!'
             INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

      ROLLBACK WORK.
      MESSAGE VL_MSG_EXIBIR TYPE 'S'.
      RETURN.
    ENDIF.

  ENDLOOP.

  LOOP AT IT_CR_AUX INTO WA_CR_AUX WHERE DOC_CONTABIL IS INITIAL
                                     AND LOTE         IS INITIAL
                                     AND DOC_LCTO     IS INITIAL.

    CLEAR: WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
       AND NRO_PARC    = WA_CR_AUX-NRO_PARCELA
       AND TIPO        = WA_CR_AUX-TIPO
       AND COD_EV      = WA_CR_AUX-COD_EV
       AND BELNR       = ''.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os dados da parcelas!' TYPE 'S'.
      RETURN.
    ENDIF.

    WA_ZFIT0101-VALOR     = WA_CR_AUX-VALOR.
    WA_ZFIT0101-DATA_VENC = WA_CR_AUX-DT_VENCIMENTO.

    MODIFY ZFIT0101 FROM WA_ZFIT0101.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os dados da parcelas!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: WA_CR_AUX.

  ENDLOOP.

  OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).
  PERFORM CHANGE_ROWS USING ''.
  CLEAR: VG_MODIFY_PARC, WL_DESACTIVE.
ENDFORM.                    " SALVAR_PARCELAS

*&---------------------------------------------------------------------*
*&      Form  DELETAR_EVENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETAR_EVENTO .

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_EV_VENDA->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS IS NOT INITIAL.

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_EVENTOS_VENDA INTO WA_EVENTOS_VENDA INDEX WA_SEL_ROWS-INDEX.

    CLEAR: WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
       AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA
       AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV.

    IF SY-SUBRC = 0.
      ROLLBACK WORK.
      MESSAGE 'Já existe parcelas geradas! Operação não permitida!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR WA_ZFIT0104.
    SELECT SINGLE *
      INTO WA_ZFIT0104
      FROM ZFIT0104
     WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
       AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA
       AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV.

    DELETE ZFIT0104 FROM WA_ZFIT0104.

    COMMIT WORK.

  ENDLOOP.

  OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).

  CALL METHOD OBJ_ALV_EV_VENDA->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " DELETAR_EVENTO

*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_DADOS .

   REFRESH: IT_EVENTOS_VENDA, IT_CR, IT_ZFIT0101.

   CLEAR: WA_TERRENO, WA_VENDA, WA_TOPO, WA_EVENTOS_VENDA.


ENDFORM.                    " LIMPA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_EVENTOS_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVAR_EVENTOS_VENDA .

  CHECK IT_EVENTOS_VENDA IS NOT INITIAL.
  CHECK WA_TERRENO IS NOT INITIAL.

  LOOP AT IT_EVENTOS_VENDA INTO WA_EVENTOS_VENDA.

    CLEAR: WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
       AND COD_EV      = WA_EVENTOS_VENDA-COD_EV.

    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    CLEAR: WA_ZFIT0104.
    MOVE-CORRESPONDING WA_EVENTOS_VENDA TO WA_ZFIT0104.

    MOVE WA_TERRENO-EMPRESA     TO WA_ZFIT0104-EMPRESA.
    MOVE WA_TERRENO-LOTEAMENTO  TO WA_ZFIT0104-LOTEAMENTO.
    MOVE WA_TERRENO-NRO_TERRENO TO WA_ZFIT0104-NRO_TERRENO.
    MOVE WA_TERRENO-NRO_QUADRA  TO WA_ZFIT0104-NRO_QUADRA.

    MODIFY ZFIT0104 FROM WA_ZFIT0104.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar os eventos da venda!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " GRAVAR_EVENTOS_VENDA


*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM F_IMPRIME_SMART  USING    P_WA_SAIDA.

  DATA: VDOCNUM TYPE J_1BNFDOC-DOCNUM,
        VL_FORM      TYPE TDSFNAME    ,
        VL_NAME      TYPE RS38L_FNAM  ,
        VDV_NOSSO(1),
        VAR_TOTAL_TRIB  TYPE J_1BTAXVAL,
        VAR_TOTAL_TRIB2 TYPE J_1BTAXVAL,
        VAR_TOTAL_DESON TYPE J_1BTAXVAL,
        VAR_TOTAL_DESC  TYPE J_1BTAXVAL,
        VAR_TOTAL_OUTROS TYPE J_1BTAXVAL,
        VAR_SUFRAMA     TYPE C.

  "FORM
  DATA: LS_CONTROL            TYPE SSFCTRLOP,
        LS_OPTIONS            TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        LS_XSFPARAM_LINE      TYPE SSFXSFP.

  FREE: I_OTF, I_TLINE, V_BIN_FILESIZE, I_RECORD, WA_BUFFER.

  VL_FORM = 'ZFI_BOLETO_BRA'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORM
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*  Impresora
  LS_CONTROL-NO_DIALOG = 'X'. "Evita la pantalla de opciones de salida del formulario
  LS_OPTIONS-TDDEST   = 'LOCL'.
  LS_OPTIONS-TDIMMED  = 'X'.
  LS_OPTIONS-TDNEWID  = 'X'.
  LS_OPTIONS-TDNOARCH = 'X'.
  LS_OPTIONS-TDNOPREV = 'X'.

  LS_CONTROL-PREVIEW = SPACE.
  LS_CONTROL-DEVICE  = 'PRINTER'.
  LS_CONTROL-GETOTF  = 'X'.

  CLEAR:JOB_OUTPUT_INFO.
  CALL FUNCTION VL_NAME
    EXPORTING
      USER_SETTINGS      = ' '
      CONTROL_PARAMETERS = LS_CONTROL
      OUTPUT_OPTIONS     = LS_OPTIONS
      WA_SAIDA           = P_WA_SAIDA
    IMPORTING
      JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

    I_OTF[] = JOB_OUTPUT_INFO-OTFDATA[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
        MAX_LINEWIDTH         = 132
      IMPORTING
        BIN_FILESIZE          = V_BIN_FILESIZE
      TABLES
        OTF                   = I_OTF
        LINES                 = I_TLINE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.

    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
        EXPORTING
          I_OTF                    = I_OTF[]
        EXCEPTIONS
          CONVERT_OTF_TO_PDF_ERROR = 1
          CNTL_ERROR               = 2
          OTHERS                   = 3.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*&      Form  LANCAR_JUROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LANCAR_JUROS .

  FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
  UNASSIGN <SAIDA>.

  CALL METHOD WA_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK NOT IT_SEL_ROWS IS INITIAL.

  IF ( LINES( IT_SEL_ROWS ) NE 1 ).
    MESSAGE S836(SD) WITH TEXT-007.
    EXIT.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
  READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

  IF <SAIDA>-TIPO NE 'CR'.
    MESSAGE S836(SD) WITH TEXT-009.
    RETURN.
  ENDIF.

  CLEAR WA_ZFIT0101_AUX.
  CALL SCREEN 0105 STARTING AT 07 05 ENDING AT 40 7.

ENDFORM.                    " LANCAR_JUROS

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING    I_ROW_ID     TYPE LVC_S_ROW
                                    I_COLUMN_ID  TYPE LVC_S_COL
                                    IS_ROW_NO    TYPE LVC_S_ROID.

  DATA: OPT     TYPE CTU_PARAMS,
        VL_LOTE TYPE ZGLT034-LOTE.

  CLEAR: WA_CR.

  CASE I_COLUMN_ID.

    WHEN: 'DOC_CONTABIL'.
      READ TABLE IT_CR INTO WA_CR INDEX I_ROW_ID.

      CHECK WA_CR-DOC_CONTABIL IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD WA_CR-DOC_CONTABIL.
      SET PARAMETER ID 'BUK' FIELD WA_CR-EMPRESA.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN: 'DOC_COMPENSACAO'.
      READ TABLE IT_CR INTO WA_CR INDEX I_ROW_ID.

      CHECK WA_CR-DOC_COMPENSACAO IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD WA_CR-DOC_COMPENSACAO.
      SET PARAMETER ID 'BUK' FIELD WA_CR-EMPRESA.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'DOC_LCTO'.
      READ TABLE IT_CR INTO WA_CR INDEX I_ROW_ID.

      CHECK WA_CR-DOC_LCTO IS NOT INITIAL.

      CLEAR: VL_LOTE.
      SET PARAMETER ID 'BLN' FIELD WA_CR-DOC_LCTO.
      SET PARAMETER ID 'LOT' FIELD VL_LOTE.
      CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

    WHEN 'LOTE'.
      READ TABLE IT_CR INTO WA_CR INDEX I_ROW_ID.
      CHECK WA_CR-LOTE IS NOT INITIAL.

      SET PARAMETER ID 'LOT' FIELD WA_CR-LOTE.
      CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.


  ENDCASE.

ENDFORM.                    "HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  CALCULA_COD_BARRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULA_COD_BARRAS USING P_VALOR P_VENCTO P_NOSSO CHANGING VDV_NOSSO
                                                                P_WA_SAIDA TYPE ZFI_BOLETO.

  " Variáveis para o código de barras
  DATA: VAR_COD_BARRAS(44),
        VAR_COD_BARRAS_COP(44),
        VAR_COD_BARRAS_INV(43),
        VAR_COD_BARRAS_FIM(44),
        VAR_BANCO(03) VALUE '001',
        VAR_MOEDA(01) VALUE '9',
        VAR_DIG05(01) VALUE ' ',
        VAR_FATOR(04),
        VAR_VALOR(10) TYPE N,
        VAR_ZEROS(06) VALUE '000000',                     "campo livre
        VAR_NOSSO_NUMERO(17) ,   "campo livre
        VAR_NOSSO_NUMERO_COP(17) ,                        "campo livre
        VAR_NOSSO_NUMERO_INV(17) ,                        "campo livre
        "VDV_NOSSO(1),
        VAR_CARTEIRA(02) VALUE '17'.                      "campo livre

  " Variáveis para a linha digitável
  DATA:  VAR_LINHA_DIG(54),
         VCAMPO1(9),
         VCAMPO1_FIM(11),
         VCAMPO1_INV(9),
         VCAMPO1_COP(9),
         VDV1(1),
         VCAMPO2(10),
         VCAMPO2_FIM(12),
         VCAMPO2_INV(10),
         VCAMPO2_COP(10),
         VDV2(1),
         VCAMPO3(10),
         VCAMPO3_FIM(12),
         VCAMPO3_INV(10),
         VCAMPO3_COP(10),
         VDV3(1),
         VCAMPO4(1),
         VCAMPO5(14),
         XCALC TYPE I,
         VCALC TYPE I,
         CCALC(2) TYPE N,
         VTOT_CAMPO_2(2) TYPE N,
         VTOT_CAMPO_3(3) TYPE N,
         VDEZENA TYPE I.

  DATA : DATA_BASE TYPE SY-DATUM VALUE '19971007',
         XCONTADOR TYPE I,
         XPOSICAO  TYPE I,
         XMULTI    TYPE I,
         XTOTAL    TYPE I,
         XRESTO    TYPE I,
         XDVCOD    TYPE I,
         CDVCOD(1).



  VAR_NOSSO_NUMERO = P_NOSSO.

  VAR_FATOR = P_VENCTO - DATA_BASE.
  VAR_VALOR = P_VALOR * 100.

  CONCATENATE VAR_BANCO VAR_MOEDA VAR_DIG05 VAR_FATOR VAR_VALOR VAR_ZEROS VAR_NOSSO_NUMERO VAR_CARTEIRA INTO VAR_COD_BARRAS.

  VAR_COD_BARRAS_COP = VAR_COD_BARRAS.

  " Inverte codigo de barras
  XCONTADOR = 0.
  WHILE VAR_COD_BARRAS_COP NE SPACE.
    ADD 1 TO XCONTADOR.
    XPOSICAO = 43 - XCONTADOR.
    VAR_COD_BARRAS_INV+XPOSICAO(1) = VAR_COD_BARRAS_COP(1).
    SHIFT VAR_COD_BARRAS_COP.
  ENDWHILE.

  " Calculo Digito DV
  XMULTI = 2.
  XTOTAL = 0.
  WHILE VAR_COD_BARRAS_INV NE SPACE.
    XPOSICAO = VAR_COD_BARRAS_INV(1).
    IF XMULTI = 10.
      XMULTI = 2.
    ENDIF.
    XTOTAL = XTOTAL + ( XMULTI * XPOSICAO ).
    ADD 1 TO XMULTI.
    SHIFT VAR_COD_BARRAS_INV.
  ENDWHILE.

  XRESTO = XTOTAL MOD 11.

  XDVCOD = 11 - XRESTO.

  IF XDVCOD = 0 OR XDVCOD = 10 OR XDVCOD = 11.
    XDVCOD = 1.
  ENDIF.

  CDVCOD = XDVCOD.
  CONCATENATE VAR_COD_BARRAS+0(4) CDVCOD VAR_COD_BARRAS+4(39)  INTO VAR_COD_BARRAS_FIM.

  " LINHA DIGITAVEL.
  CONCATENATE VAR_BANCO VAR_MOEDA VAR_COD_BARRAS_FIM+19(5) INTO VCAMPO1.
  VCAMPO2 = VAR_COD_BARRAS_FIM+24(10).
  VCAMPO3 = VAR_COD_BARRAS_FIM+34(10).
  VCAMPO4 = CDVCOD.
  CONCATENATE VAR_FATOR VAR_VALOR INTO VCAMPO5.


  " Inverte campo1
  VCAMPO1_COP = VCAMPO1.
  XCONTADOR = 0.
  WHILE VCAMPO1_COP NE SPACE.
    ADD 1 TO XCONTADOR.
    XPOSICAO = 9 - XCONTADOR.
    VCAMPO1_INV+XPOSICAO(1) = VCAMPO1_COP(1).
    SHIFT VCAMPO1_COP.
  ENDWHILE.

  " Calculo Digito DV Campo1
  XMULTI = 2.
  XTOTAL = 0.
  WHILE VCAMPO1_INV NE SPACE.
    XPOSICAO = VCAMPO1_INV(1).
    IF XMULTI = 0.
      XMULTI = 2.
    ENDIF.
    XCALC = ( XMULTI * XPOSICAO ).
    CCALC = XCALC .
    XCALC = 0.
    WHILE CCALC NE SPACE.
      VCALC = CCALC(1).
      XCALC = XCALC + VCALC.
      SHIFT CCALC.
    ENDWHILE.
    XTOTAL = XTOTAL + XCALC.
    SUBTRACT 1 FROM XMULTI.
    SHIFT VCAMPO1_INV.
  ENDWHILE.

  IF XTOTAL LT 100.
    VTOT_CAMPO_2 = XTOTAL.
    IF  XTOTAL < 10.
      VDEZENA = 10.
    ELSE.
      VDEZENA = VTOT_CAMPO_2+0(1).
      VDEZENA = ( VDEZENA + 1 ) * 10.
    ENDIF.
  ELSE.
    VTOT_CAMPO_3 = XTOTAL.
    VDEZENA = VTOT_CAMPO_3+0(2).
    VDEZENA = ( VDEZENA + 1 ) * 10.
  ENDIF.

  XDVCOD = VDEZENA - XTOTAL.
  IF XDVCOD = 10.
    VDV1 = 0.
  ELSE.
    VDV1 = XDVCOD.
  ENDIF.

  " Inverte campo2
  VCAMPO2_COP = VCAMPO2.
  XCONTADOR = 0.
  WHILE VCAMPO2_COP NE SPACE.
    ADD 1 TO XCONTADOR.
    XPOSICAO = 10 - XCONTADOR.
    VCAMPO2_INV+XPOSICAO(1) = VCAMPO2_COP(1).
    SHIFT VCAMPO2_COP.
  ENDWHILE.

  " Calculo Digito DV Campo2
  XMULTI = 2.
  XTOTAL = 0.
  WHILE VCAMPO2_INV NE SPACE.
    XPOSICAO = VCAMPO2_INV(1).
    IF XMULTI = 0.
      XMULTI = 2.
    ENDIF.
    XCALC = ( XMULTI * XPOSICAO ).
    CCALC = XCALC .
    XCALC = 0.
    WHILE CCALC NE SPACE.
      VCALC = CCALC(1).
      XCALC = XCALC + VCALC.
      SHIFT CCALC.
    ENDWHILE.
    XTOTAL = XTOTAL + XCALC.
    SUBTRACT 1 FROM XMULTI.
    SHIFT VCAMPO2_INV.
  ENDWHILE.

  IF XTOTAL LT 100.
    VTOT_CAMPO_2 = XTOTAL.
    IF  XTOTAL < 10.
      VDEZENA = 10.
    ELSE.
      VDEZENA = VTOT_CAMPO_2+0(1).
      VDEZENA = ( VDEZENA + 1 ) * 10.
    ENDIF.
  ELSE.
    VTOT_CAMPO_3 = XTOTAL.
    VDEZENA = VTOT_CAMPO_3+0(2).
    VDEZENA = ( VDEZENA + 1 ) * 10.
  ENDIF.

  XDVCOD = VDEZENA - XTOTAL.
  IF XDVCOD = 10.
    VDV2 = 0.
  ELSE.
    VDV2 = XDVCOD.
  ENDIF.

  " Inverte campo3
  VCAMPO3_COP = VCAMPO3.
  XCONTADOR = 0.
  WHILE VCAMPO3_COP NE SPACE.
    ADD 1 TO XCONTADOR.
    XPOSICAO = 10 - XCONTADOR.
    VCAMPO3_INV+XPOSICAO(1) = VCAMPO3_COP(1).
    SHIFT VCAMPO3_COP.
  ENDWHILE.

  " Calculo Digito DV Campo3
  XMULTI = 2.
  XTOTAL = 0.
  WHILE VCAMPO3_INV NE SPACE.
    XPOSICAO = VCAMPO3_INV(1).
    IF XMULTI = 0.
      XMULTI = 2.
    ENDIF.
    XCALC = ( XMULTI * XPOSICAO ).
    CCALC = XCALC .
    XCALC = 0.
    WHILE CCALC NE SPACE.
      VCALC = CCALC(1).
      XCALC = XCALC + VCALC.
      SHIFT CCALC.
    ENDWHILE.
    XTOTAL = XTOTAL + XCALC.
    SUBTRACT 1 FROM XMULTI.
    SHIFT VCAMPO3_INV.
  ENDWHILE.

  IF XTOTAL LT 100.
    VTOT_CAMPO_2 = XTOTAL.
    IF  XTOTAL < 10.
      VDEZENA = 10.
    ELSE.
      VDEZENA = VTOT_CAMPO_2+0(1).
      VDEZENA = ( VDEZENA + 1 ) * 10.
    ENDIF.
  ELSE.
    VTOT_CAMPO_3 = XTOTAL.
    VDEZENA = VTOT_CAMPO_3+0(2).
    VDEZENA = ( VDEZENA + 1 ) * 10.
  ENDIF.

  XDVCOD = VDEZENA - XTOTAL.
  IF XDVCOD = 10.
    VDV3 = 0.
  ELSE.
    VDV3 = XDVCOD.
  ENDIF.

  " monta linha digitável.
  CONCATENATE VCAMPO1+0(5) '.' VCAMPO1+5(4) VDV1 INTO  VCAMPO1_FIM.
  CONCATENATE VCAMPO2+0(5) '.' VCAMPO2+5(5) VDV2 INTO  VCAMPO2_FIM.
  CONCATENATE VCAMPO3+0(5) '.' VCAMPO3+5(5) VDV3 INTO  VCAMPO3_FIM.
  CONCATENATE VCAMPO1_FIM VCAMPO2_FIM VCAMPO3_FIM VCAMPO4 VCAMPO5 INTO VAR_LINHA_DIG SEPARATED BY SPACE.


  " Inverte NOSSO NUMERO
  VAR_NOSSO_NUMERO_COP = VAR_NOSSO_NUMERO.
  XCONTADOR = 0.
  WHILE VAR_NOSSO_NUMERO_COP NE SPACE.
    ADD 1 TO XCONTADOR.
    XPOSICAO = 17 - XCONTADOR.
    VAR_NOSSO_NUMERO_INV+XPOSICAO(1) = VAR_NOSSO_NUMERO_COP(1).
    SHIFT VAR_NOSSO_NUMERO_COP.
  ENDWHILE.

  " Calculo Digito NOSSO NUMERO
  XMULTI = 9.
  XTOTAL = 0.
  WHILE VAR_NOSSO_NUMERO_INV NE SPACE.
    XPOSICAO = VAR_NOSSO_NUMERO_INV(1).
    IF XMULTI = 1.
      XMULTI = 9.
    ENDIF.
    XCALC = ( XMULTI * XPOSICAO ).
    XTOTAL = XTOTAL + XCALC.
    SUBTRACT 1 FROM XMULTI.
    SHIFT VAR_NOSSO_NUMERO_INV.
  ENDWHILE.

  XRESTO = XTOTAL MOD 11.

  CLEAR VDV_NOSSO.
  IF XRESTO LT 10.
    VDV_NOSSO = XRESTO.
  ELSEIF XRESTO EQ 10.
    VDV_NOSSO = 'X'.
  ENDIF.

  P_WA_SAIDA-VAR_COD_BARRAS_FIM = VAR_COD_BARRAS_FIM.
  P_WA_SAIDA-VAR_LINHA_DIG = VAR_LINHA_DIG.

ENDFORM.                    " CALCULA_COD_BARRAS
*&---------------------------------------------------------------------*
*&      Form  SEARCH_QUADRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_QUADRA .

    "TYPES
    TYPES: BEGIN OF TY_QUADRA,
             NRO_QUADRA      TYPE ZFIT0099-NRO_QUADRA,
             NRO_TERRENO     TYPE ZFIT0099-NRO_TERRENO,
             ENDERECO        TYPE ZFIT0099-ENDERECO,
           END OF TY_QUADRA.

    DATA: LT_QUADRA TYPE TABLE OF TY_QUADRA,
          LS_QUADRA TYPE TY_QUADRA,
          LT_MAP    TYPE TABLE OF DSELC,
          LS_MAP    TYPE DSELC,
          LT_RETURN TYPE TABLE OF DDSHRETVAL,
          LS_RETURN TYPE DDSHRETVAL,
          LS_STABLE TYPE LVC_S_STBL.

    IF WA_TOPO-LOTEAMENTO IS INITIAL.
      MESSAGE 'Selecione um Loteamento!' TYPE 'S'.
      EXIT.
    ENDIF.

    "LOAD F4 DATA
    SELECT NRO_QUADRA NRO_TERRENO ENDERECO
      INTO TABLE LT_QUADRA
      FROM ZFIT0099
     WHERE LOTEAMENTO = WA_TOPO-LOTEAMENTO.

    SORT LT_QUADRA BY NRO_QUADRA.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0001'.
    LS_MAP-DYFLDNAME = 'NRO_QUADRA'.
    APPEND LS_MAP TO LT_MAP.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0002'.
    LS_MAP-DYFLDNAME = 'NRO_TERRENO'.
    APPEND LS_MAP TO LT_MAP.

    " CALL SEARCH HELP POPUP FUNCTION
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'NRO_TERRENO'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = LT_QUADRA
        DYNPFLD_MAPPING = LT_MAP
        RETURN_TAB      = LT_RETURN
      EXCEPTIONS
        PARAMETER_ERROR = 1
        NO_VALUES_FOUND = 2
        OTHERS          = 3.


    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
    IF LS_RETURN IS NOT INITIAL.
      WA_TOPO-NRO_QUADRA = LS_RETURN-FIELDVAL.
    ENDIF.

    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0002'.
    IF LS_RETURN IS NOT INITIAL.
      WA_TOPO-NRO_TERRENO = LS_RETURN-FIELDVAL.
    ENDIF.





ENDFORM.                    " SEARCH_QUADRA
