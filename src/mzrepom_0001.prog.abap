*----------------------------------------------------------------------*
***INCLUDE MZREPOM_0001.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_HANDLER_0001 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK_0001 FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER_0001 TYPE REF TO LCL_EVENT_HANDLER_0001.

DATA: CTL_ALV_0001    TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0001    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0001     TYPE LVC_S_LAYO,
      GS_VAR_0001     TYPE DISVARIANT,
      IT_CATALOG_0001 TYPE LVC_T_FCAT,
      IT_EXCLUDE_0001 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0001 LIKE LINE OF IT_EXCLUDE_0001.

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER_0001 IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK_0001.
    PERFORM HANDLE_DOUBLE_CLICK_0001 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: GS_SCROLL_COL_0001 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0001 TYPE LVC_S_ROID.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  CASE OK_CODE.
    WHEN OK_NOVO.
      CLEAR OK_CODE.
      PERFORM NOVA_ROTA.
    WHEN OK_EDITAR.
      CLEAR OK_CODE.
      PERFORM EDITAR_ROTA.
    WHEN OK_ABRIR.
      CLEAR OK_CODE.
      PERFORM ABRIR_ROTA.
    WHEN OK_SOLICITAR.
      CLEAR OK_CODE.
      PERFORM SOLICITAR_ROTA.
    WHEN OK_CONSULTAR.
      CLEAR OK_CODE.
      PERFORM CONSULTAR_ROTA.
    WHEN OK_PESQUISAR.
      CLEAR OK_CODE.
      PERFORM PESQUISAR_ROTAS.
    WHEN OK_CARTAO_PED.
      CLEAR OK_CODE.
      PERFORM VERIFICA_CARTAO.
    WHEN OK_ATIVA.
      CLEAR OK_CODE.
      PERFORM ATIVAR_ROTA.
    WHEN OK_DESATIVA.
      CLEAR OK_CODE.
      PERFORM DESATIVAR_ROTA.
    WHEN OK_EXCLUIR.
      CLEAR OK_CODE.
      PERFORM EXCLUIR_ROTA.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF CTL_CON_0001 IS INITIAL.

    CREATE OBJECT CTL_CON_0001
      EXPORTING
        CONTAINER_NAME = 'ALV_0001'.

    CREATE OBJECT CTL_ALV_0001
      EXPORTING
        I_PARENT = CTL_CON_0001.

    PERFORM FILL_IT_FIELDCATALOG_0001.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0001.
*   Set layout parameters for ALV grid

    GS_LAY_0001-SEL_MODE   = 'A'.
    GS_LAY_0001-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0001
        IS_VARIANT           = GS_VAR_0001
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0001
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0001
        IT_OUTTAB            = IT_ZLEST0121_ALV[].

    CREATE OBJECT EVENT_HANDLER_0001.
    SET HANDLER EVENT_HANDLER_0001->HANDLE_DOUBLE_CLICK_0001 FOR CTL_ALV_0001.
    CALL METHOD CTL_ALV_0001->REFRESH_TABLE_DISPLAY.
  ELSE.
    CALL METHOD CTL_ALV_0001->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0001->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0001
      ES_ROW_NO   = GS_SCROLL_ROW_0001.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  NOVA_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NOVA_ROTA .

  DATA: CK_GRAVOU TYPE CHAR01.

  CALL FUNCTION 'Z_REPOM_CADASTRO_ROTA'
    EXPORTING
      I_CONSULTA = ABAP_FALSE
    IMPORTING
      I_GRAVOU   = CK_GRAVOU.

  IF CK_GRAVOU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_ROTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_ROTAS .

  CK_FILTRAR = ABAP_FALSE.

  CALL SCREEN 1001 STARTING AT 5 5.

  IF CK_FILTRAR EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS_BANCO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR_ROTA .

  DATA: CK_GRAVOU TYPE CHAR01.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CALL FUNCTION 'Z_REPOM_CADASTRO_ROTA'
    EXPORTING
      I_CONSULTA = ABAP_FALSE
      I_ID_ROTA  = IT_ZLEST0121_SEL-ID_ROTA
    IMPORTING
      I_GRAVOU   = CK_GRAVOU.

  IF CK_GRAVOU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_0001 INPUT.

  CALL METHOD CTL_ALV_0001->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0001
      ES_ROW_NO   = GS_SCROLL_ROW_0001.

ENDMODULE.                 " GET_SCROLL_INFO_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_0001 INPUT.
  PERFORM POPULA_SELECAO_0001.
ENDMODULE.                 " GET_SELECTED_ROWS_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULA_SELECAO_0001 .

  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV_0001->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_ZLEST0121_SEL[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZLEST0121_ALV INDEX WA_SELECTED_ROWS-INDEX.
    MOVE-CORRESPONDING IT_ZLEST0121_ALV TO IT_ZLEST0121_SEL.
    APPEND IT_ZLEST0121_SEL.
  ENDLOOP.

ENDFORM.                    " POPULA_SELECAO_0001

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0001 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0001> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0121_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0001.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0001 ASSIGNING <FS_CAT_0001>.
    CASE <FS_CAT_0001>-FIELDNAME.
      WHEN 'MANDT'.
        <FS_CAT_0001>-NO_OUT = ABAP_TRUE.
      WHEN 'ID_ROTA'.
        <FS_CAT_0001>-OUTPUTLEN = 6.
      WHEN 'BUKRS'.
        <FS_CAT_0001>-OUTPUTLEN = 6.
      WHEN 'BUTXT'.
        <FS_CAT_0001>-OUTPUTLEN = 20.
      WHEN 'BRANCH'.
        <FS_CAT_0001>-OUTPUTLEN = 6.
      WHEN 'NAME'.
        <FS_CAT_0001>-OUTPUTLEN = 20.
      WHEN 'CD_PAIS'.
        <FS_CAT_0001>-NO_OUT = ABAP_TRUE.
      WHEN 'CD_CID_ORIGEM'.
        <FS_CAT_0001>-OUTPUTLEN = 10.
      WHEN 'DS_CID_ORIGEM'.
        <FS_CAT_0001>-OUTPUTLEN = 30.
      WHEN 'CD_CID_DESTINO'.
        <FS_CAT_0001>-OUTPUTLEN = 10.
      WHEN 'DS_CID_DESTINO'.
        <FS_CAT_0001>-OUTPUTLEN = 30.
      WHEN 'TP_PROC_TRANSP'.
        <FS_CAT_0001>-OUTPUTLEN = 05.
      WHEN 'TP_IDA_VOLTA'.
        <FS_CAT_0001>-OUTPUTLEN = 05.
      WHEN 'DS_OBSERVACAO'.
        <FS_CAT_0001>-OUTPUTLEN = 40.
      WHEN 'TP_ST_ROTA'.
        <FS_CAT_0001>-OUTPUTLEN = 05.
      WHEN 'CK_ATIVO'.
        <FS_CAT_0001>-OUTPUTLEN = 05.
      WHEN 'US_ULTIMO_AJUSTE'.
        <FS_CAT_0001>-OUTPUTLEN = 12.
      WHEN 'DT_ULTIMO_AJUSTE'.
        <FS_CAT_0001>-OUTPUTLEN = 08.
      WHEN 'HR_ULTIMO_AJUSTE'.
        <FS_CAT_0001>-OUTPUTLEN = 08.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0001

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0001 .

  GS_VAR_0001-REPORT      = SY-REPID.
  GS_VAR_0001-HANDLE      = '0001'.
  GS_VAR_0001-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0001-USERNAME    = ABAP_FALSE.
  GS_VAR_0001-VARIANT     = ABAP_FALSE.
  GS_VAR_0001-TEXT        = ABAP_FALSE.
  GS_VAR_0001-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0001

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_ROTAS_BANCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_ROTAS_BANCO .

  DATA: IT_T001         TYPE TABLE OF T001 WITH HEADER LINE,
        IT_J_1BBRANCH   TYPE TABLE OF J_1BBRANCH WITH HEADER LINE,
        IT_J_1BTXJURT_O TYPE TABLE OF J_1BTXJURT WITH HEADER LINE,
        IT_J_1BTXJURT_D TYPE TABLE OF J_1BTXJURT WITH HEADER LINE.

  CLEAR: IT_ZLEST0121_ALV[], IT_ZLEST0121_ALV, IT_ZLEST0121[], IT_ZLEST0121.

  SELECT * INTO TABLE IT_ZLEST0121
    FROM ZLEST0121
   WHERE ID_ROTA        IN PIDROTA
     AND BUKRS          IN PBUKRS
     AND BRANCH         IN PBRANCH
     AND CD_CID_ORIGEM  IN PORIGEM
     AND CD_CID_DESTINO IN PDESTIN
     AND TP_PROC_TRANSP IN PPROCTR
     AND TP_IDA_VOLTA   IN PIDAVOL
     AND DS_OBSERVACAO  IN POBSERV.

  IF IT_ZLEST0121[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_T001
      FROM T001
       FOR ALL ENTRIES IN IT_ZLEST0121
     WHERE BUKRS EQ IT_ZLEST0121-BUKRS.

    SORT IT_T001 BY BUKRS.

    SELECT * INTO TABLE IT_J_1BBRANCH
      FROM J_1BBRANCH
       FOR ALL ENTRIES IN IT_ZLEST0121
     WHERE BUKRS  EQ IT_ZLEST0121-BUKRS
       AND BRANCH EQ IT_ZLEST0121-BRANCH.

    SORT IT_J_1BBRANCH BY BUKRS BRANCH.

    SELECT * INTO TABLE IT_J_1BTXJURT_O
      FROM J_1BTXJURT
       FOR ALL ENTRIES IN IT_ZLEST0121
     WHERE SPRAS EQ SY-LANGU
       AND COUNTRY EQ IT_ZLEST0121-CD_PAIS
       AND TAXJURCODE EQ IT_ZLEST0121-CD_CID_ORIGEM.

    SORT IT_J_1BTXJURT_O BY COUNTRY TAXJURCODE.

    SELECT * INTO TABLE IT_J_1BTXJURT_D
      FROM J_1BTXJURT
       FOR ALL ENTRIES IN IT_ZLEST0121
     WHERE SPRAS EQ SY-LANGU
       AND COUNTRY EQ IT_ZLEST0121-CD_PAIS
       AND TAXJURCODE EQ IT_ZLEST0121-CD_CID_DESTINO.

    SORT IT_J_1BTXJURT_D BY COUNTRY TAXJURCODE.

  ENDIF.

  LOOP AT IT_ZLEST0121.
    CLEAR: IT_ZLEST0121_ALV.
    MOVE-CORRESPONDING IT_ZLEST0121 TO IT_ZLEST0121_ALV.

    READ TABLE IT_T001 WITH KEY BUKRS = IT_ZLEST0121-BUKRS BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0121_ALV-BUTXT = IT_T001-BUTXT.
    ENDIF.

    READ TABLE IT_J_1BBRANCH WITH KEY BUKRS  = IT_ZLEST0121-BUKRS
                                      BRANCH = IT_ZLEST0121-BRANCH BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0121_ALV-NAME = IT_J_1BBRANCH-NAME.
    ENDIF.

    READ TABLE IT_J_1BTXJURT_O WITH KEY COUNTRY    = IT_ZLEST0121-CD_PAIS
                                        TAXJURCODE = IT_ZLEST0121-CD_CID_ORIGEM BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0121_ALV-DS_CID_ORIGEM = IT_J_1BTXJURT_O-TEXT.
    ENDIF.

    READ TABLE IT_J_1BTXJURT_D WITH KEY COUNTRY    = IT_ZLEST0121-CD_PAIS
                                        TAXJURCODE = IT_ZLEST0121-CD_CID_DESTINO BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0121_ALV-DS_CID_DESTINO = IT_J_1BTXJURT_D-TEXT.
    ENDIF.

    APPEND IT_ZLEST0121_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ABRIR_ROTA .

  DATA: CK_GRAVOU TYPE CHAR01.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CALL FUNCTION 'Z_REPOM_CADASTRO_ROTA'
    EXPORTING
      I_CONSULTA = ABAP_TRUE
      I_ID_ROTA  = IT_ZLEST0121_SEL-ID_ROTA
    IMPORTING
      I_GRAVOU   = CK_GRAVOU.

  IF CK_GRAVOU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SOLICITAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SOLICITAR_ROTA .

  DATA: OBJ_ROTA     TYPE REF TO ZCL_REPOM_ROTA,
        CK_SOLICITOU TYPE CHAR01,
        LC_ERROS     TYPE ZDE_REPOM_ERROS_T,
        WA_ERRO      TYPE ZDE_REPOM_ERROS.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CREATE OBJECT OBJ_ROTA
    EXPORTING
      I_ID_ROTA = IT_ZLEST0121_SEL-ID_ROTA.

  "OBJ_ROTA->CK_SALVAR_XML_LOCAL = ABAP_TRUE.

  CALL METHOD OBJ_ROTA->SOLICITAR_ROTA
    IMPORTING
      E_ERROS                    = LC_ERROS
    RECEIVING
      I_SOLICITOU                = CK_SOLICITOU
    EXCEPTIONS
      SERVICO_NAO_ENCONTRADO     = 1
      HTTP_COMMUNICATION_FAILURE = 2
      HTTP_INVALID_STATE         = 3
      HTTP_PROCESSING_FAILED     = 4
      HTTP_INVALID_TIMEOUT       = 5
      ERRO                       = 6
      OTHERS                     = 7.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.

  IF CK_SOLICITOU EQ ABAP_FALSE.
    LOOP AT LC_ERROS INTO WA_ERRO.
      MESSAGE W017 WITH WA_ERRO-ERRO_CODIGO WA_ERRO-ERRO_DESCRICAO.
    ENDLOOP.
  ENDIF.

  CLEAR: OBJ_ROTA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONSULTAR_ROTA .

  DATA: OBJ_ROTA    TYPE REF TO ZCL_REPOM_ROTA,
        CK_CONSULTA TYPE CHAR01,
        LC_ERROS    TYPE ZDE_REPOM_ERROS_T,
        WA_ERRO     TYPE ZDE_REPOM_ERROS.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CREATE OBJECT OBJ_ROTA
    EXPORTING
      I_ID_ROTA = IT_ZLEST0121_SEL-ID_ROTA.

  "OBJ_ROTA->CK_SALVAR_XML_LOCAL = ABAP_TRUE.

  CALL METHOD OBJ_ROTA->CONSULTAR_ROTA
    IMPORTING
      E_ERROS                    = LC_ERROS
    RECEIVING
      I_CONSULTOU                = CK_CONSULTA
    EXCEPTIONS
      SERVICO_NAO_ENCONTRADO     = 1
      HTTP_COMMUNICATION_FAILURE = 2
      HTTP_INVALID_STATE         = 3
      HTTP_PROCESSING_FAILED     = 4
      HTTP_INVALID_TIMEOUT       = 5
      ERRO                       = 6
      OTHERS                     = 7.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.

  IF CK_CONSULTA EQ ABAP_FALSE.
    LOOP AT LC_ERROS INTO WA_ERRO.
      MESSAGE W017 WITH WA_ERRO-ERRO_CODIGO WA_ERRO-ERRO_DESCRICAO.
    ENDLOOP.
  ENDIF.

  CLEAR: OBJ_ROTA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_CARTAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_CARTAO .

  CALL SCREEN 9999 STARTING AT 05 05.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_0001  USING P_ROW TYPE LVC_S_ROW.

  DATA: LC_ROW TYPE LVC_T_ROW.

  CLEAR: IT_ZLEST0121_SEL[].

  IF P_ROW-ROWTYPE IS INITIAL.

    APPEND P_ROW TO LC_ROW.

    CALL METHOD CTL_ALV_0001->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_ZLEST0121_ALV INDEX P_ROW-INDEX INTO IT_ZLEST0121_ALV.
    APPEND IT_ZLEST0121_ALV TO IT_ZLEST0121_SEL.

    PERFORM ABRIR_ROTA.

  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK_0001

*&---------------------------------------------------------------------*
*&      Form  ATIVAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATIVAR_ROTA .

  DATA: OBJ_ROTA TYPE REF TO ZCL_REPOM_ROTA.

  DATA: CK_GRAVOU TYPE CHAR01.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CREATE OBJECT OBJ_ROTA
    EXPORTING
      I_ID_ROTA = IT_ZLEST0121_SEL-ID_ROTA.

  CALL METHOD OBJ_ROTA->ATIVAR_ROTA
    RECEIVING
      I_ATIVOU = CK_GRAVOU.

  CLEAR: OBJ_ROTA.

  IF CK_GRAVOU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS_BANCO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESATIVAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESATIVAR_ROTA .

  DATA: OBJ_ROTA TYPE REF TO ZCL_REPOM_ROTA.

  DATA: CK_GRAVOU TYPE CHAR01.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CREATE OBJECT OBJ_ROTA
    EXPORTING
      I_ID_ROTA = IT_ZLEST0121_SEL-ID_ROTA.

  CALL METHOD OBJ_ROTA->DESATIVAR_ROTA
    RECEIVING
      I_DESATIVOU = CK_GRAVOU.

  CLEAR: OBJ_ROTA.

  IF CK_GRAVOU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS_BANCO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_ROTA .

  DATA: OBJ_ROTA TYPE REF TO ZCL_REPOM_ROTA.

  DATA: CK_EXCLUIU TYPE CHAR01.

  IF IT_ZLEST0121_SEL[] IS INITIAL.
    MESSAGE S016.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0121_SEL INDEX 1.

  CREATE OBJECT OBJ_ROTA
    EXPORTING
      I_ID_ROTA = IT_ZLEST0121_SEL-ID_ROTA.

  CALL METHOD OBJ_ROTA->EXCLUIR_REGISTRO
    RECEIVING
      I_EXCLUIU = CK_EXCLUIU.

  CLEAR: OBJ_ROTA.

  IF CK_EXCLUIU EQ ABAP_TRUE.
    PERFORM PESQUISAR_ROTAS_BANCO.
    PERFORM ATUALIZAR_TELA.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZAR_TELA .

*  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.
*
*  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
*  GS_ALV_REFRES_COND-COL = ABAP_TRUE.
*
*  CALL METHOD CTL_ALV_0001->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE      = GS_ALV_REFRES_COND
*      I_SOFT_REFRESH = ABAP_TRUE.
*
*  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.
