*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9007.
*----------------------------------------------------------------------*
TABLES: ZDE_ZSDT0001CG_VALIDA,
        ZDE_ZSDT0001CG_VALIDA_NOTA.

CLASS LCL_EVENT_HANDLER_9008 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

TYPES: BEGIN OF TY_VALIDA_ALV.
        INCLUDE STRUCTURE ZDE_ZSDT0001CG_VALIDA_NOTA.
TYPES:   LINE_COLOR(4) TYPE C, "Used to store row color attributes
         COLOR_CELL    TYPE LVC_T_SCOL,  " Cell color
         STYLE         TYPE LVC_T_STYL,
         END OF TY_VALIDA_ALV.

DATA: LC_QTD_NOTAS            TYPE I,
      LC_SUB_9007             TYPE SY-DYNNR,
      CK_VERIFICOU_TIPO_FRETE TYPE CHAR01,
      IT_VALIDA_NOTAS         TYPE TABLE OF TY_VALIDA_ALV,
      WA_VALIDA_NOTAS         TYPE TY_VALIDA_ALV.

DATA: CK_VALIDACAO_CONFERENCIA TYPE C LENGTH 1.
DATA: CK_VALIDACAO_SAIDA_AUTOM TYPE C LENGTH 1.
DATA: CK_EFETUAR_SAIDA_AUTOM   TYPE C LENGTH 1.

DATA: CTL_ALV_9008         TYPE REF TO CL_GUI_ALV_GRID,
      EVENT_HANDLER_9008   TYPE REF TO LCL_EVENT_HANDLER_9008,
      CTL_CONTAINER_9008   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_VARIANT_9008      TYPE DISVARIANT,
      IT_FIELDCATALOG_9008 TYPE LVC_T_FCAT,
      GS_LAYOUT_9008       TYPE LVC_S_LAYO.

DATA: SPLITTER_9010    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_PICTURE_9010 TYPE REF TO CL_GUI_CONTAINER,
      PICTURE_9010     TYPE REF TO CL_GUI_PICTURE.

CLASS LCL_EVENT_HANDLER_9008 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_9008 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_9007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9007 OUTPUT.

  DATA: IT_PF_9007 TYPE TABLE OF SY-UCOMM.

  CASE PTIPCA.
    WHEN ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.

      TRY .
          OBJETO->GET_CK_SAIDA_AUTOMATICA( ).
        CATCH ZCX_CARGA.
          APPEND 'FATURAMENT' TO IT_PF_9007.
          CK_VALIDACAO_SAIDA_AUTOM = ABAP_TRUE.
      ENDTRY.

      IF CK_VERIFICOU_TIPO_FRETE = ABAP_FALSE.
        CK_VERIFICOU_TIPO_FRETE = ABAP_TRUE.

        TRY .
            OBJETO->GET_INFO_ALV_APRESENTACAO(
              IMPORTING
                E_APRESENTACAO = DATA(INFO_CONFERENCIA)
            ).

            READ TABLE INFO_CONFERENCIA-ORDEM_VENDA INTO DATA(WA_ORDEM_VENDA) INDEX 1.
            IF SY-SUBRC IS INITIAL.
              ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
                                 )->SET_ORDEM_VENDA( I_VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA
                                 )->GET_TIPO_FRETE( IMPORTING  E_TIPO_FRETE = DATA(E_TIPO_FRETE)
                                 ).
              IF E_TIPO_FRETE NE ZIF_CARGA=>ST_TP_FRETE_CIF.
                APPEND 'FATURAMENT' TO IT_PF_9007.
              ENDIF.
            ELSE.
              APPEND 'FATURAMENT' TO IT_PF_9007.
            ENDIF.

          CATCH ZCX_ORDEM_CARREGAMENTO INTO EX_ORDEM.    "
            EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
            EXIT.
          CATCH ZCX_CARGA INTO EX_CARGA.    "
            EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
            EXIT.
        ENDTRY.
      ENDIF.

    WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_OPUS.
      APPEND 'FATURAMENT' TO IT_PF_9007.
    WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_ENT_FOB.
      APPEND 'FATURAMENT' TO IT_PF_9007.
  ENDCASE.

  LOOP AT SCREEN.
    SPLIT SCREEN-NAME AT '-' INTO: DATA(STR1) DATA(STR2).
    IF STR1 EQ 'ZDE_ZSDT0001CG_VALIDA' AND STR2 = 'NR_PERC_CAR'.

      SELECT SINGLE * INTO @DATA(WA_MARA)
        FROM MARA
       WHERE MATNR EQ @INFO_CONFERENCIA-CARGA-ID_PRODUTO.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_MARA-MATKL
        IMPORTING
          OUTPUT = WA_MARA-MATKL.

      IF WA_MARA-MATKL EQ '700170'. "Milho
        SCREEN-INPUT = 1.
      ELSE.
        SCREEN-INPUT = 0.
      ENDIF.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

  SET PF-STATUS 'PF9007' EXCLUDING IT_PF_9007.
  SET TITLEBAR 'TL9007'.

  IF LC_QTD_NOTAS NE 1.
    LC_SUB_9007 = '9008'.
  ELSE.
    LC_SUB_9007 = '9009'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9007_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9007_EXIT INPUT.
  CK_VALIDACAO_CONFERENCIA = ABAP_FALSE.
  CK_VERIFICOU_TIPO_FRETE  = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9007 INPUT.

  DATA: LC_CHECK_INVALIDO TYPE C LENGTH 1.

  CASE OK_CODE.

    WHEN 'FATURAMENT'.
      PERFORM FATURAMENTO_9011.

    WHEN 'VALIDAR'.
      TRY .
          OBJETO->GET_INFO_ALV_APRESENTACAO(
            IMPORTING
              E_APRESENTACAO         = INFO_CONFERENCIA
          ).
        CATCH ZCX_ORDEM_CARREGAMENTO INTO EX_ORDEM.    "
          EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
          EXIT.
        CATCH ZCX_CARGA INTO EX_CARGA.    "
          EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
          EXIT.
      ENDTRY.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "193  Peso Bruto inválido!
      "194  Peso Tara inválido!
      "195  Peso SubTotal inválido!

      LC_CHECK_INVALIDO = ABAP_FALSE.

      IF INFO_CONFERENCIA-CARGA-NM_PESO_BRUTO NE ZDE_ZSDT0001CG_VALIDA-NM_PESO_BRUTO.
        LC_CHECK_INVALIDO = ABAP_TRUE.
        MESSAGE S193 DISPLAY LIKE 'E'.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NM_PESO_TARA NE ZDE_ZSDT0001CG_VALIDA-NM_PESO_TARA.
        MESSAGE S194 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NM_PESO_SUBTOTAL NE ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL.
        MESSAGE S195 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "196  Percentual Umidade inválido!
      "198  Percentual Impureza inválido!
      "200  Percentual Avariado inválido!
      "202  Percentual Ardido inválido!
      "204  Percentual Quebrado inválido!
      "206  Percentual Esverdeado inválido!

      IF INFO_CONFERENCIA-CARGA-NR_PERC_UMI NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_UMI.
        MESSAGE S196 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_IMP NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP.
        MESSAGE S198 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_AVA NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_AVA.
        MESSAGE S200 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_ARD NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_ARD.
        MESSAGE S202 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_QUE NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_QUE.
        MESSAGE S204 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_ESV NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_ESV.
        MESSAGE S206 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_PERC_CAR NE ZDE_ZSDT0001CG_VALIDA-NR_PERC_CAR.
        MESSAGE S248 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "197  Peso Umidade inválido!
      "199  Peso Impureza inválido!
      "201  Peso Avariado inválido!
      "203  Peso Ardido inválido!
      "205  Peso Quebrado inválido!
      "207  Peso Esverdeado inválido!

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_UMI NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_UMI.
        MESSAGE S197 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_IMP NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_IMP.
        MESSAGE S199 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_AVA NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_AVA.
        MESSAGE S201 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_ARD NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_ARD.
        MESSAGE S203 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_QUE NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_QUE.
        MESSAGE S205 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_ESV NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_ESV.
        MESSAGE S207 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      IF INFO_CONFERENCIA-CARGA-NR_QTDE_CAR NE ZDE_ZSDT0001CG_VALIDA-NR_QTDE_CAR.
        MESSAGE S249 DISPLAY LIKE 'E'.
        LC_CHECK_INVALIDO = ABAP_TRUE.
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      LOOP AT INFO_CONFERENCIA-NOTAS ASSIGNING FIELD-SYMBOL(<FS_NOTAS>).
        CONDENSE <FS_NOTAS>-NM_SERIE NO-GAPS.
      ENDLOOP.

      "Documentos Fiscais
      LOOP AT IT_VALIDA_NOTAS INTO WA_VALIDA_NOTAS.

        CONDENSE WA_VALIDA_NOTAS-NM_SERIE NO-GAPS.

        READ TABLE INFO_CONFERENCIA-NOTAS INTO DATA(WA_NOTAS)
        WITH KEY NR_NOTA          = WA_VALIDA_NOTAS-NR_NOTA
                 NM_SERIE         = WA_VALIDA_NOTAS-NM_SERIE
                 NR_FORNECEDOR_IE = WA_VALIDA_NOTAS-NR_FORNECEDOR_IE.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE S219 WITH WA_VALIDA_NOTAS-NR_NOTA WA_VALIDA_NOTAS-NM_SERIE WA_VALIDA_NOTAS-NR_FORNECEDOR_IE DISPLAY LIKE 'E'.
          LC_CHECK_INVALIDO = ABAP_TRUE.
        ELSE.

          IF WA_VALIDA_NOTAS-ID_ENTRADA NE WA_NOTAS-ID_ENTRADA.
            MESSAGE S220 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-ID_MOD_FISCAL NE WA_NOTAS-ID_MOD_FISCAL.
            MESSAGE S221 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-DT_EMISSAO NE WA_NOTAS-DT_EMISSAO.
            MESSAGE S222 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-NR_CHAVE_NFE NE WA_NOTAS-NR_CHAVE_NFE.
            MESSAGE S223 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-NR_QUANTIDADE NE WA_NOTAS-NR_QUANTIDADE.
            MESSAGE S224 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-NR_VALOR NE WA_NOTAS-NR_VALOR.
            MESSAGE S225 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.

          IF WA_VALIDA_NOTAS-DT_VENCIMENTO_FORM NE WA_NOTAS-DT_VENCIMENTO_FORM.
            MESSAGE S226 DISPLAY LIKE 'E'.
            LC_CHECK_INVALIDO = ABAP_TRUE.
          ENDIF.
        ENDIF.

      ENDLOOP.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      CHECK LC_CHECK_INVALIDO EQ ABAP_FALSE.

      IF CK_DADO_FRETE_9011 EQ ABAP_FALSE.
        READ TABLE INFO_CONFERENCIA-ORDEM_VENDA INTO WA_ORDEM_VENDA INDEX 1.
        IF SY-SUBRC IS INITIAL.

          ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
                             )->SET_ORDEM_VENDA( I_VBELN = WA_ORDEM_VENDA-NR_ORDEM_VENDA
                             )->GET_TIPO_FRETE( IMPORTING  E_TIPO_FRETE = E_TIPO_FRETE
                             ).

          IF E_TIPO_FRETE NE ZIF_CARGA=>ST_TP_FRETE_CIF.
            CK_DADO_FRETE_9011 = ABAP_TRUE.
            CK_ERRO_FRETE_9011 = ABAP_FALSE.
          ENDIF.
        ELSE.
          CK_DADO_FRETE_9011 = ABAP_TRUE.
          CK_ERRO_FRETE_9011 = ABAP_FALSE.
        ENDIF.
      ENDIF.

      IF CK_DADO_FRETE_9011 EQ ABAP_FALSE.
        PERFORM FATURAMENTO_9011.
      ENDIF.

      CHECK CK_VALIDACAO_SAIDA_AUTOM EQ ABAP_TRUE.
      CHECK CK_ERRO_FRETE_9011 EQ ABAP_FALSE.

      CK_VALIDACAO_CONFERENCIA = ABAP_TRUE.
      CK_VERIFICOU_TIPO_FRETE  = ABAP_FALSE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_01_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_01_VAL INPUT.
  PERFORM PERC_RESULTADO USING '1' ZDE_ZSDT0001CG_VALIDA-NR_PERC_UMI
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_UMI.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_02_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_02_VAL INPUT.

  PERFORM PERC_RESULTADO USING '2' ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_IMP.

  PERFORM PERC_RESULTADO USING '1' ZDE_ZSDT0001CG_VALIDA-NR_PERC_UMI
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_UMI.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_03_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_03_VAL INPUT.
  PERFORM PERC_RESULTADO USING '3' ZDE_ZSDT0001CG_VALIDA-NR_PERC_AVA
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_AVA.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_04_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_04_VAL INPUT.
  PERFORM PERC_RESULTADO USING '4' ZDE_ZSDT0001CG_VALIDA-NR_PERC_ARD
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_ARD.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_05_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_05_VAL INPUT.
  PERFORM PERC_RESULTADO USING '5' ZDE_ZSDT0001CG_VALIDA-NR_PERC_QUE
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_QUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_06_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_06_VAL INPUT.
  PERFORM PERC_RESULTADO USING '6' ZDE_ZSDT0001CG_VALIDA-NR_PERC_ESV
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_ESV.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_07_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERC_07_VAL INPUT.
  PERFORM PERC_RESULTADO USING '7' ZDE_ZSDT0001CG_VALIDA-NR_PERC_CAR
                                   ZDE_ZSDT0001CG_VALIDA-NR_PERC_IMP
                                   ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL
                          CHANGING ZDE_ZSDT0001CG_VALIDA-NR_QTDE_CAR.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_SUBTOTAL2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_SUBTOTAL2 INPUT.

  DATA: OBJ_RECEBIMENTO TYPE REF TO ZIF_CARGA.

  TRY .

      CASE PTIPCA.
        WHEN ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.
          CREATE OBJECT OBJ_RECEBIMENTO TYPE ZCL_CARGA_RECEBIMENTO.
        WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_OPUS.
          CREATE OBJECT OBJ_RECEBIMENTO TYPE ZCL_CARGA_SAIDA_OPUS.
        WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_ENT_FOB.
          CREATE OBJECT OBJ_RECEBIMENTO TYPE ZCL_CARGA_SAIDA.
      ENDCASE.

      OBJETO->GET_CALCULAR_SUBTOTAL(
         EXPORTING
           I_PESO_BRUTO = ZDE_ZSDT0001CG_VALIDA-NM_PESO_BRUTO
           I_PESO_TARA  = ZDE_ZSDT0001CG_VALIDA-NM_PESO_TARA
        IMPORTING
          E_PESO_SUBTOTAL = DATA(E_PESO_SUBTOTAL2) ).

      CLEAR: OBJ_RECEBIMENTO.

      IF ZDE_ZSDT0001CG_VALIDA-NM_PESO_SUBTOTAL NE E_PESO_SUBTOTAL2.
        MESSAGE E063.
      ENDIF.

    CATCH ZCX_CARGA INTO EX_CARGA.
      CLEAR: OBJ_RECEBIMENTO.
  ENDTRY.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9008  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9008 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF CTL_CONTAINER_9008 IS INITIAL.

    CREATE OBJECT CTL_CONTAINER_9008
      EXPORTING
        CONTAINER_NAME = 'ALV_NOTAS'.

    PERFORM FILL_IT_FIELDCATALOG_9008.

    "Hints
    PERFORM FILL_IT_HINTS_9008.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_9008.

    CREATE OBJECT CTL_ALV_9008
      EXPORTING
        I_PARENT          = CTL_CONTAINER_9008
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    GS_LAYOUT_9008-SEL_MODE   = 'A'.
    GS_LAYOUT_9008-INFO_FNAME = 'LINE_COLOR'.
    GS_LAYOUT_9008-STYLEFNAME = 'STYLE'.
    GS_LAYOUT_9008-CTAB_FNAME = 'COLOR_CELL'.
    GS_LAYOUT_9008-ZEBRA      = ABAP_FALSE.
    GS_LAYOUT_9008-NO_TOOLBAR = ABAP_TRUE.
    GS_LAYOUT_9008-NO_ROWMARK = ABAP_TRUE.


*    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = CTL_ALV.
*
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR CTL_ALV.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR CTL_ALV.

    CALL METHOD CTL_ALV_9008->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_9008
        IS_VARIANT      = GS_VARIANT_9008
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE          = 'A'
        "IT_EXCEPT_QINFO      = IT_EXCEPT_QINFO
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_9008
        IT_OUTTAB       = IT_VALIDA_NOTAS[].

    CREATE OBJECT EVENT_HANDLER_9008.
    SET HANDLER EVENT_HANDLER_9008->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_9008.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_9008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_9008
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  LOOP AT IT_VALIDA_NOTAS ASSIGNING FIELD-SYMBOL(<FS_VALIDA_NOTA>).
    CLEAR: <FS_VALIDA_NOTA>-LINE_COLOR.
  ENDLOOP.

  READ TABLE IT_VALIDA_NOTAS INDEX ROW_ID ASSIGNING <FS_VALIDA_NOTA>.

  CASE FIELDNAME.
    WHEN 'ID_NUMERO'.
      <FS_VALIDA_NOTA>-LINE_COLOR = CS_LINE_COLOR_SELECIONADA.
      MOVE-CORRESPONDING <FS_VALIDA_NOTA> TO ZDE_ZSDT0001CG_VALIDA_NOTA.

      GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
      GS_ALV_REFRES_COND-COL = ABAP_TRUE.

      IF CTL_ALV_9008 IS NOT INITIAL.
        CALL METHOD CTL_ALV_9008->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE      = GS_ALV_REFRES_COND
            I_SOFT_REFRESH = ABAP_TRUE.
      ENDIF.

      CALL METHOD CL_GUI_CFW=>FLUSH.

      LEAVE TO SCREEN 9007.

  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_9008 .

  CLEAR: IT_FIELDCATALOG_9008[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZSDT0001CG_VALIDA_NOTA'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_9008.

  DELETE IT_FIELDCATALOG_9008 WHERE FIELDNAME NE 'ID_NUMERO'.

  LOOP AT IT_FIELDCATALOG_9008 ASSIGNING FIELD-SYMBOL(<FS_CAT>).
    <FS_CAT>-TABNAME = 'ZDE_ZSDT0001CG_VALIDA_NOTA'.
    CASE <FS_CAT>-FIELDNAME.
      WHEN 'ID_NUMERO'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 1.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS_9008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_HINTS_9008 .

*  DATA: IT_DD07V        TYPE TABLE OF DD07V WITH HEADER LINE,
*        WA_EXCEPT_QINFO LIKE LINE OF IT_EXCEPT_QINFO,
*        LC_TP_STATUS    TYPE ZDE_STATUS_CARGA,
*        LC_ICO_CARGA    TYPE CHAR04.
**
*  CLEAR: IT_EXCEPT_QINFO[].
**
*  "Informações Documento
*  CALL FUNCTION 'GET_DOMAIN_VALUES'
*    EXPORTING
*      DOMNAME    = 'ZDM_STATUS_CARGA'
*    TABLES
*      VALUES_TAB = IT_DD07V.
*
*  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
*    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
*    LC_TP_STATUS = CONV #( IT_DD07V-DOMVALUE_L ).
*    PERFORM SETA_ICONE_STATUS USING LC_TP_STATUS CHANGING LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-VALUE = LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
*    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_ZSDT0001CG_ALV'.
*    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_CARGA'.
*    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_9008 .

  GS_VARIANT_9008-REPORT      = SY-REPID.
  GS_VARIANT_9008-HANDLE      = '9008'.
  GS_VARIANT_9008-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_9008-USERNAME    = ABAP_FALSE.
  GS_VARIANT_9008-VARIANT     = ABAP_FALSE.
  GS_VARIANT_9008-TEXT        = ABAP_FALSE.
  GS_VARIANT_9008-DEPENDVARS  = ABAP_FALSE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9009 OUTPUT.

*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL IS INITIAL.
    ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL = ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
  ENDIF.

  IF ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA IS NOT INITIAL.
    SELECT SINGLE DS_ENTRADA INTO ZDE_ZSDT0001CG_VALIDA_NOTA-DS_ENTRADA
      FROM ZSDT0001TETX
     WHERE ID_ENTRADA EQ ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA.


  ENDIF.

  CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
    WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
      DATA(LC_NOTA_PROPRIA) = ABAP_FALSE.

      SELECT SINGLE * INTO @DATA(WA_ENTRADA)
        FROM ZSDT0001TE
       WHERE ID_ENTRADA EQ @ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA
         AND ID_EMPRESA EQ @ZDE_ZSDT0001CG_ALV-ID_BUKRS
         AND CK_NFE     EQ @ABAP_TRUE.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO @DATA(WA_J_1BAA)
          FROM J_1BAA
         WHERE NFTYPE EQ @WA_ENTRADA-CT_NOTA.

        IF WA_J_1BAA-FORM IS NOT INITIAL.
          LC_NOTA_PROPRIA = ABAP_TRUE.
        ENDIF.
      ENDIF.

  ENDCASE.

  LOOP AT SCREEN.
    SPLIT SCREEN-NAME AT '-' INTO: DATA(STR1V) DATA(STR2V).
    CASE STR2V.
      WHEN 'ID_ENTRADA'.
      WHEN 'DS_ENTRADA'.
        SCREEN-INPUT = 0.
      WHEN 'ID_MOD_FISCAL'.

      WHEN 'NR_FORNECEDOR_IE'.

      WHEN 'NR_NOTA'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            SCREEN-INPUT = 0.
        ENDCASE.

      WHEN 'NM_SERIE'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            SCREEN-INPUT = 0.
        ENDCASE.

      WHEN 'DT_EMISSAO'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            SCREEN-INPUT = 0.
        ENDCASE.

      WHEN 'NR_CHAVE_NFE'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
            SCREEN-INPUT = 0.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            IF LC_NOTA_PROPRIA EQ ABAP_TRUE.
              SCREEN-INPUT = 0.
            ENDIF.
        ENDCASE.

      WHEN 'NR_QUANTIDADE'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            IF LC_NOTA_PROPRIA EQ ABAP_FALSE.
              SCREEN-INPUT = 0.
            ENDIF.
        ENDCASE.

      WHEN 'NR_VALOR'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            IF LC_NOTA_PROPRIA EQ ABAP_FALSE.
              SCREEN-INPUT = 0.
            ENDIF.
        ENDCASE.

      WHEN 'DT_VENCIMENTO_FORM'.

        CASE ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
          WHEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
            SCREEN-INPUT = 0.
            ZDE_ZSDT0001CG_VALIDA_NOTA-DT_VENCIMENTO_FORM = ZDE_ZSDT0001CG_VALIDA_NOTA-DT_EMISSAO.
        ENDCASE.

      WHEN 'ID_FORNECEDOR'.
        SCREEN-INPUT = 0.
      WHEN 'DS_FORNECEDOR'.
        SCREEN-INPUT = 0.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CH_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CH_VAL INPUT.
  PERFORM ATUALIZA_DADOS.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_ENTRA2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ENTRA2 INPUT.

  IF ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA IS NOT INITIAL.
    SELECT SINGLE DS_ENTRADA INTO ZDE_ZSDT0001CG_VALIDA_NOTA-DS_ENTRADA
      FROM ZSDT0001TETX
     WHERE ID_ENTRADA EQ ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_MODEL2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MODEL2 INPUT.

  IF ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL NE ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO AND ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL NE ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
    MESSAGE E055.
  ENDIF.

  IF ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL EQ ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
    CLEAR: ZDE_ZSDT0001CG_VALIDA_NOTA-NR_CHAVE_NFE.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_FORNE2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_FORNE2 INPUT.

  TRY .
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
         )->SET_PARCEIRO_IE( I_INSC_ESTATUAL = CONV #( ZDE_ZSDT0001CG_VALIDA_NOTA-NR_FORNECEDOR_IE )
         )->GET_NAME( IMPORTING E_NAME = ZDE_ZSDT0001CG_VALIDA_NOTA-DS_FORNECEDOR
         )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = ZDE_ZSDT0001CG_VALIDA_NOTA-ID_FORNECEDOR
         ).
    CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.
      EX_PARCEIROS->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9009 INPUT.

  CASE OK_CODE.
    WHEN 'CHNFE2'.
      CLEAR: WA_ADD_NFE_9002.
      CALL SCREEN 9002 STARTING AT 40 10.
      IF WA_ADD_NFE_9002-CK_INCLUIR EQ ABAP_TRUE.
        ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL      = ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
        ZDE_ZSDT0001CG_VALIDA_NOTA-DT_EMISSAO         = WA_ADD_NFE_9002-DT_EMISSAO.
        ZDE_ZSDT0001CG_VALIDA_NOTA-DS_FORNECEDOR      = WA_ADD_NFE_9002-NAME1.
        ZDE_ZSDT0001CG_VALIDA_NOTA-ID_FORNECEDOR      = WA_ADD_NFE_9002-PARID.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NR_FORNECEDOR_IE   = WA_ADD_NFE_9002-PARID_IE.
        ZDE_ZSDT0001CG_VALIDA_NOTA-DT_VENCIMENTO_FORM = WA_ADD_NFE_9002-DT_EMISSAO.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NR_NOTA            = WA_ADD_NFE_9002-NUMERO.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NM_SERIE           = WA_ADD_NFE_9002-SERIE.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NR_VALOR           = WA_ADD_NFE_9002-NFTOT.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NR_QUANTIDADE      = WA_ADD_NFE_9002-NTGEW.
        ZDE_ZSDT0001CG_VALIDA_NOTA-NR_CHAVE_NFE       = WA_ADD_NFE_9002-N55_CHAVE_ACESSO.
        PERFORM ATUALIZA_DADOS.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9010 INPUT.

  CASE SCREEN_TELA.
    WHEN 9007.
      CALL SCREEN 9007 STARTING AT 50 08.
    WHEN 9001.
      CALL SCREEN 9001 STARTING AT 50 08.
  ENDCASE.

  PICTURE_9010->FREE( ).
  CLEAR: PICTURE_9010.
  CTL_PICTURE_9010->FREE( ).
  CLEAR: CTL_PICTURE_9010.
  SPLITTER_9010->FREE( ).
  CLEAR: SPLITTER_9010.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9010 OUTPUT.

  SUPPRESS DIALOG.

  CASE SCREEN_TELA.
    WHEN 9007.
      SET TITLEBAR 'TL9007'.
    WHEN 9001.
      SET TITLEBAR 'TL9001'.
  ENDCASE.

  IF SPLITTER_9010 IS INITIAL.

    CREATE OBJECT SPLITTER_9010
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_9010->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_PICTURE_9010.

    CREATE OBJECT PICTURE_9010
      EXPORTING
        PARENT = CTL_PICTURE_9010
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE_9010->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE_9010->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB USING PICTURE_9010.

  ENDIF.

  LEAVE TO LIST-PROCESSING.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_DADOS .
  READ TABLE IT_VALIDA_NOTAS WITH KEY ID_NUMERO = ZDE_ZSDT0001CG_VALIDA_NOTA-ID_NUMERO ASSIGNING FIELD-SYMBOL(<FS_VALIDA>).
  IF SY-SUBRC IS INITIAL.
    <FS_VALIDA>-ID_ENTRADA          = ZDE_ZSDT0001CG_VALIDA_NOTA-ID_ENTRADA.
    <FS_VALIDA>-DS_ENTRADA          = ZDE_ZSDT0001CG_VALIDA_NOTA-DS_ENTRADA.
    <FS_VALIDA>-ID_MOD_FISCAL       = ZDE_ZSDT0001CG_VALIDA_NOTA-ID_MOD_FISCAL.
    <FS_VALIDA>-NR_FORNECEDOR_IE    = ZDE_ZSDT0001CG_VALIDA_NOTA-NR_FORNECEDOR_IE.
    <FS_VALIDA>-NR_NOTA             = ZDE_ZSDT0001CG_VALIDA_NOTA-NR_NOTA.
    <FS_VALIDA>-NM_SERIE            = ZDE_ZSDT0001CG_VALIDA_NOTA-NM_SERIE.
    <FS_VALIDA>-DT_EMISSAO          = ZDE_ZSDT0001CG_VALIDA_NOTA-DT_EMISSAO.
    <FS_VALIDA>-NR_CHAVE_NFE        = ZDE_ZSDT0001CG_VALIDA_NOTA-NR_CHAVE_NFE.
    <FS_VALIDA>-NR_QUANTIDADE       = ZDE_ZSDT0001CG_VALIDA_NOTA-NR_QUANTIDADE.
    <FS_VALIDA>-NR_VALOR            = ZDE_ZSDT0001CG_VALIDA_NOTA-NR_VALOR.
    <FS_VALIDA>-DT_VENCIMENTO_FORM  = ZDE_ZSDT0001CG_VALIDA_NOTA-DT_VENCIMENTO_FORM.
    <FS_VALIDA>-ID_FORNECEDOR       = ZDE_ZSDT0001CG_VALIDA_NOTA-ID_FORNECEDOR.
    <FS_VALIDA>-DS_FORNECEDOR       = ZDE_ZSDT0001CG_VALIDA_NOTA-DS_FORNECEDOR.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FATURAMENTO_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FATURAMENTO_9011 .

  TRY .
      OBJETO->GET_CK_SAIDA_AUTOMATICA( ).
      CALL SCREEN 9011 STARTING AT 55 09.
      CK_DADO_FRETE_9011 = ABAP_TRUE.
    CATCH ZCX_CARGA.
      EXIT.
  ENDTRY.

ENDFORM.
