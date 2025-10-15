*----------------------------------------------------------------------*
***INCLUDE ZSDR0195_COPY_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form update_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM UPDATE_ALV.

**<<<------"167326 - NMS - INI------>>>
  TABLES: ZDE_SD_NFE_E_F.

  DATA: VL_DOCNUM_FLOTE TYPE ZDOCNUM_FLOTE.

  IF CKB_VINC IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
    " Recriar o ALV após atualização
    FREE: GT_VINC,
          GT_VINC_AUX,
          GT_NOVOS_DOC.
**<<<------"167326 - NMS - INI------>>>
  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  CALL METHOD ZCL_IM_CL_FLUXO_EXPORTACAO=>RETURN_NFE_VINC_F_LOTE
    EXPORTING
      I_DOCNUM    = R_DOCNUM
      I_DATA      = S_EMIS[]
    RECEIVING
      R_VINCFLOTE = DATA(LT_VINC_AUX).

  DELETE LT_VINC_AUX WHERE CANCEL = ABAP_TRUE.
**<<<------"167326 - NMS - INI------>>>
  IF NOT CKB_VINC IS INITIAL.
    LOOP AT LT_VINC_AUX INTO DATA(EL_VINC_AUX).
      DATA(VL_TABIX) = SY-TABIX.
      READ TABLE GT_VINC TRANSPORTING NO FIELDS WITH KEY DOCNUM_FLOTE = EL_VINC_AUX-DOCNUM_FLOTE
                                                         DOCNUM_EPROD = EL_VINC_AUX-DOCNUM_EPROD
                                                         ID_VINC      = EL_VINC_AUX-ID_VINC.

      IF NOT SY-SUBRC IS INITIAL.
        DELETE LT_VINC_AUX INDEX VL_TABIX.

      ENDIF.

    ENDLOOP.
* Recriar o ALV após atualização
    FREE: GT_VINC,
          GT_VINC_AUX.

    IF SY-UCOMM EQ 'SAVE'.
      FREE GT_NOVOS_DOC.

    ENDIF.

  ELSE.
    SELECT DOCNUM_FLOTE, DOCNUM_EPROD, QTD_VINC_ORIG
      FROM ZSDT_DOCNUM_LOG
      INTO TABLE @DATA(LT_LOG)
      FOR ALL ENTRIES IN @LT_VINC_AUX
      WHERE DOCNUM_FLOTE EQ @LT_VINC_AUX-DOCNUM_FLOTE
        AND DOCNUM_EPROD EQ @LT_VINC_AUX-DOCNUM_EPROD.

    IF SY-SUBRC IS INITIAL.
      SORT LT_LOG BY DOCNUM_FLOTE DOCNUM_EPROD.

    ENDIF.

  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  MOVE-CORRESPONDING LT_VINC_AUX TO GT_VINC.
  MOVE-CORRESPONDING LT_VINC_AUX TO GT_VINC_AUX.
**<<<------"167326 - NMS - INI------>>>
  LOOP AT GT_VINC ASSIGNING FIELD-SYMBOL(<FS_VINC>).
    IF NOT CKB_VINC IS INITIAL.
      IF VL_DOCNUM_FLOTE EQ <FS_VINC>-DOCNUM_FLOTE.
        <FS_VINC>-QTD_VINC_ORIG = ZDE_SD_NFE_E_F-MENGE.

      ELSE.
        VL_DOCNUM_FLOTE = <FS_VINC>-DOCNUM_FLOTE.
        DATA(TL_NOTAS) = ZCL_IM_CL_FLUXO_EXPORTACAO=>GET_NFE_E_F( I_DOCNUM = VL_DOCNUM_FLOTE ).
        READ TABLE TL_NOTAS INTO ZDE_SD_NFE_E_F WITH KEY DOCNUM = VL_DOCNUM_FLOTE.

        IF SY-SUBRC IS INITIAL.
          <FS_VINC>-QTD_VINC_ORIG = ZDE_SD_NFE_E_F-MENGE.

        ENDIF.

      ENDIF.

    ELSE.
      READ TABLE LT_LOG ASSIGNING FIELD-SYMBOL(<FS_LOG>) WITH KEY DOCNUM_FLOTE = <FS_VINC>-DOCNUM_FLOTE
                                                                  DOCNUM_EPROD = <FS_VINC>-DOCNUM_EPROD
                                                         BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        <FS_VINC>-QTD_VINC_ORIG = <FS_LOG>-QTD_VINC_ORIG.

      ELSE.
        <FS_VINC>-QTD_VINC_ORIG = <FS_VINC>-QTD_VINC.

      ENDIF.

    ENDIF.

  ENDLOOP.
**<<<------"167326 - NMS - FIM------>>>
  LO_ALV->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL ).
  CL_GUI_CFW=>FLUSH( ).

ENDFORM.

FORM CHANGE_DOCNUM.

  DATA: LV_ERRO     TYPE C,
        LT_BSID_AUX TYPE TRTY_BSID.

  DATA: LS_BSEG   TYPE BSEG,
        LT_ERRDOC TYPE TPIT_T_ERRDOC,
        LT_FNAME  TYPE TPIT_T_FNAME,
        LT_BUZTAB TYPE TPIT_T_BUZTAB.

  DATA: LS_API     TYPE REF TO IF_SALV_GUI_OM_EXTEND_GRID_API,
        LS_EDIT    TYPE REF TO IF_SALV_GUI_OM_EDIT_RESTRICTED,
        LV_GRBEW   TYPE LVC_FNAME,
        LV_NOTE    TYPE LVC_FNAME,
        LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL,
        LV_LINES   TYPE SY-TABIX.

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  DATA: LT_ROWS    TYPE SALV_T_ROW.

  LR_SELECTIONS = LO_ALV->GET_SELECTIONS( ).
  LT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).
  DESCRIBE TABLE LT_ROWS LINES LV_LINES.
  IF LV_LINES > 1.
    MESSAGE S000(Z01) WITH 'Favor selecionar apenas uma linha!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF LT_ROWS IS NOT INITIAL.

    LS_API = LO_ALV->EXTENDED_GRID_API( ).
    LS_EDIT = LS_API->EDITABLE_RESTRICTED( ).

    CALL METHOD LS_EDIT->SET_T_CELLTAB_COLUMNNAME
      EXPORTING
        T_CELLTAB_COLUMNNAME = 'CELLTAB'.

    LS_EDIT->VALIDATE_CHANGED_DATA( ).
    LO_ALV->REFRESH( ).

    LS_CELLTAB-FIELDNAME = 'DOCNUM_EPROD'.
    LS_CELLTAB-STYLE     = '00080000'.
    INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 1.

    LS_CELLTAB-FIELDNAME = 'QTD_VINC'.
    LS_CELLTAB-STYLE     = '00080000'.
    INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 2.

    LOOP AT LT_ROWS  ASSIGNING FIELD-SYMBOL(<FS_ROWS>).
      READ TABLE GT_VINC ASSIGNING FIELD-SYMBOL(<FS_VINC>) INDEX <FS_ROWS>.
      IF SY-SUBRC IS INITIAL.

        FREE: <FS_VINC>-CELLTAB.

        <FS_VINC>-CELLTAB = LT_CELLTAB.
**<<<------"167326 - NMS - INI------>>>
        IF NOT CKB_VINC IS INITIAL.
          APPEND INITIAL LINE TO GT_NOVOS_DOC ASSIGNING FIELD-SYMBOL(<FS_NOVOS_DOC>).
          APPEND INITIAL LINE TO GT_VINC_AUX  ASSIGNING FIELD-SYMBOL(<FS_VINC_AUX>).
          MOVE-CORRESPONDING <FS_VINC> TO: <FS_NOVOS_DOC>,
                                           <FS_VINC_AUX>.
* Variável de identificação de alteração de Vinculo Criado Manualmente.
          VG_ALT_VINC = ABAP_ON.

        ELSE.
          VG_ALT_VINC = ABAP_OFF.

        ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_altera_docnum_vinculado
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_ALTERA_DOCNUM_VINCULADO .

  FREE: GT_CANCELADAS,
        GT_NOVOS_DOC.
**<<<------"167326 - NMS - INI------>>>
  IF CKB_VINC IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
    IF S_FLOTE IS NOT INITIAL.
      R_DOCNUM = S_FLOTE[].
    ELSEIF S_PROD IS NOT INITIAL.
      R_DOCNUM = S_PROD[].
    ENDIF.

    CALL METHOD ZCL_IM_CL_FLUXO_EXPORTACAO=>RETURN_NFE_VINC_F_LOTE
      EXPORTING
        I_DOCNUM    = R_DOCNUM
        I_DATA      = S_EMIS[]
      RECEIVING
        R_VINCFLOTE = DATA(LT_VINC_AUX).

    DELETE LT_VINC_AUX WHERE CANCEL = ABAP_TRUE.
**<<<------"167326 - NMS - INI------>>>
  ELSE.
    APPEND INITIAL LINE TO LT_VINC_AUX.
    CLEAR TG_DOCS.

  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  IF LT_VINC_AUX IS NOT INITIAL.

    MOVE-CORRESPONDING LT_VINC_AUX TO GT_VINC.

    SORT LT_VINC_AUX BY DOCNUM_FLOTE DOCNUM_EPROD.
    DELETE ADJACENT DUPLICATES FROM LT_VINC_AUX COMPARING DOCNUM_FLOTE DOCNUM_EPROD.

    IF LT_VINC_AUX IS NOT INITIAL.

      SELECT DOCNUM_FLOTE, DOCNUM_EPROD, QTD_VINC_ORIG
        FROM ZSDT_DOCNUM_LOG
        INTO TABLE @DATA(LT_LOG)
        FOR ALL ENTRIES IN @LT_VINC_AUX
        WHERE DOCNUM_FLOTE = @LT_VINC_AUX-DOCNUM_FLOTE
          AND DOCNUM_EPROD = @LT_VINC_AUX-DOCNUM_FLOTE.
      IF SY-SUBRC IS INITIAL.
        SORT LT_LOG BY DOCNUM_FLOTE DOCNUM_EPROD.
      ENDIF.

      LOOP AT GT_VINC ASSIGNING FIELD-SYMBOL(<FS_VINC>).
        READ TABLE LT_LOG ASSIGNING FIELD-SYMBOL(<FS_LOG>)
        WITH KEY DOCNUM_FLOTE = <FS_VINC>-DOCNUM_FLOTE
                 DOCNUM_EPROD = <FS_VINC>-DOCNUM_EPROD
        BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <FS_VINC>-QTD_VINC_ORIG = <FS_LOG>-QTD_VINC_ORIG.
        ELSE.
          <FS_VINC>-QTD_VINC_ORIG = <FS_VINC>-QTD_VINC.
        ENDIF.

      ENDLOOP.

    ENDIF.

    MOVE-CORRESPONDING GT_VINC TO GT_VINC_AUX.

    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = LO_ALV
          CHANGING
            T_TABLE      = GT_VINC ).

        " Configurações de colunas
        LT_COLUMNS = LO_ALV->GET_COLUMNS( ).
        LT_COLUMNS->SET_COLUMN_POSITION( EXPORTING COLUMNNAME = 'QTD_VINC_ORIG' POSITION = 7 ).

        DATA(LO_COLUMN) = LT_COLUMNS->GET_COLUMN( 'MANDT' ).
        LO_COLUMN->SET_VISIBLE( ABAP_FALSE ).

        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'QTD_VINC_ORIG' ).
        LO_COLUMN->SET_MEDIUM_TEXT('Qtd Vinc Orig').
        LO_COLUMN->SET_LONG_TEXT('Qtd Vinc Orig').

        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'DOCNUM_REF' ).
        LO_COLUMN->SET_SHORT_TEXT('Docnum REF').

        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'VINCULADA_XML' ).
        LO_COLUMN->SET_MEDIUM_TEXT('Nota Ref XML').
        LO_COLUMN->SET_OUTPUT_LENGTH( 12 ).


        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'VINC_VIRTUAL' ).
        LO_COLUMN->SET_MEDIUM_TEXT('Vinc. Virtual').
        LO_COLUMN->SET_OUTPUT_LENGTH( 12 ).

        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'CANCEL' ).
        LO_COLUMN->SET_SHORT_TEXT('Cancelado').
        LO_COLUMN->SET_OUTPUT_LENGTH( 10 ).
**<<<------"167326 - NMS - INI------>>>
        LO_COLUMN = LT_COLUMNS->GET_COLUMN( 'MANUAL' ).
        LO_COLUMN->SET_SHORT_TEXT('Vin Manual').
        LO_COLUMN->SET_OUTPUT_LENGTH( 10 ).
**<<<------"167326 - NMS - FIM------>>>
        " Adiciona funções padrão (Exportar, Filtro, etc.)
        LO_FUNCTIONS = LO_ALV->GET_FUNCTIONS( ).
        LO_FUNCTIONS->SET_ALL( ABAP_TRUE ).

        LR_SELECTIONS = LO_ALV->GET_SELECTIONS( ).

        LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

        " Define o GUI Status personalizado
        LO_ALV->SET_SCREEN_STATUS(
          PFSTATUS      = 'SALV_STANDARD'   " Nome do seu GUI Status
          REPORT        = SY-REPID         " Report atual
          SET_FUNCTIONS = LO_ALV->C_FUNCTIONS_ALL ).

        GO_API = LO_ALV->EXTENDED_GRID_API( ).
        GO_EDIT = GO_API->EDITABLE_RESTRICTED( ).

        CALL METHOD GO_EDIT->SET_T_CELLTAB_COLUMNNAME
          EXPORTING
            T_CELLTAB_COLUMNNAME = 'CELLTAB'.
**<<<------"167326 - NMS - INI------>>>
        IF NOT CKB_VINC IS INITIAL.
          DATA: LT_CELLTAB TYPE LVC_T_STYL,
                LS_CELLTAB TYPE LVC_S_STYL.

          LS_CELLTAB-FIELDNAME = 'DOCNUM_EPROD'.
          LS_CELLTAB-STYLE     = '00080000'.
          INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 1.

          LS_CELLTAB-FIELDNAME = 'DOCNUM_FLOTE'.
          LS_CELLTAB-STYLE     = '00080000'.
          INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 2.

          LS_CELLTAB-FIELDNAME = 'QTD_VINC'.
          LS_CELLTAB-STYLE     = '00080000'.
          INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 3.

          READ TABLE GT_VINC ASSIGNING <FS_VINC> INDEX 1.
          <FS_VINC>-CELLTAB = LT_CELLTAB.

        ENDIF.
**<<<------"167326 - NMS - FIM------>>>
        DATA(LR_EVENTS) = LO_ALV->GET_EVENT( ).

        CREATE OBJECT GR_EVENTS.

        SET HANDLER: GR_EVENTS->ON_USER_COMMAND   FOR LR_EVENTS.

        DATA(MO_LISTENER) = NEW LCL_LISTENER( ).
        GO_EDIT->SET_LISTENER( MO_LISTENER ).

        " Exibe o ALV
        LO_ALV->DISPLAY( ).

      CATCH CX_SALV_MSG INTO DATA(LX_MSG).
        MESSAGE LX_MSG->GET_TEXT( ) TYPE 'E'.
    ENDTRY.

  ELSE.

    MESSAGE S000(Z01) WITH 'Nenhum registro encontrado!' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING I_UCOMM TYPE SALV_DE_FUNCTION.

  CASE I_UCOMM.
    WHEN 'REFRESH'.
      " Lógica para atualizar o ALV
      PERFORM UPDATE_ALV.
    WHEN 'CHANGE_DOC'.
      " Lógica para alterar o DOCNUM
      PERFORM CHANGE_DOCNUM.
    WHEN 'SAVE'.
**<<<------"167326 - NMS - INI------>>>
      IF NOT CKB_VINC IS INITIAL.
        MOVE-CORRESPONDING GT_VINC TO GT_VINC_AUX.

      ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      PERFORM F_GRAVA_ALTERACOES.
    WHEN 'NOVA_LINHA'.
      PERFORM F_NOVA_LINHA.
**<<<------"167326 - NMS - INI------>>>
    WHEN 'EXC_LINHA'.
* Excluí Linha e delete registro de formação de lote selecionado.
      PERFORM ZF_DELETE_LINE.
**<<<------"167326 - NMS - FIM------>>>
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_grava_alteracoes
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GRAVA_ALTERACOES .
  DATA: LT_LOG           TYPE TABLE OF ZSDT_DOCNUM_LOG,
        LS_LOG           TYPE ZSDT_DOCNUM_LOG,
        LV_ANSWER        TYPE C,
        LV_QTD_TOTAL     TYPE ZQTD_VINC,
        LV_QTD_VINC_ORIG TYPE ZQTD_VINC.

  DATA: LOC_GRID TYPE REF TO CL_GUI_ALV_GRID. "<<<------"167326 - NMS------>>>

  SORT GT_VINC_AUX BY DOCNUM_EPROD DOCNUM_FLOTE.
**<<<------"167326 - NMS - INI------>>>
  IF NOT CKB_VINC IS INITIAL.
* Verifica linhas duplicada em "Criar Vínculo".
    PERFORM ZF_CHECK_LINHA_DUPLICADA USING SY-ABCDE+4(1). "E - Erro
* Verifica linhas já criadas em "Criar Vínculo".
    PERFORM ZF_CHECK_LINHA_CRIADA USING SY-ABCDE+4(1). "E - Erro
*** Implementação do processo de Checagem de algteração de dados e acionamento do "Botão
*** Salvar" sem dar o <ENTER> para efetivaçãop dos dados na tela.
  ENDIF.
* Gera uma cópia do ALV em processamento.
  IF LOC_GRID IS INITIAL.
* Busca o instanciamento do ALV em processamento.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = LOC_GRID.

  ENDIF.
* Verifica se o instanciamento do ALV em processamento foi carregado.
  IF NOT LOC_GRID IS INITIAL.
* Executa a validação de Mudança de Dados do ALV em processamento carregado.
    CALL METHOD LOC_GRID->CHECK_CHANGED_DATA.
* verifica se ocorreu algum erro ao verificar os dados alterados quando acionado o botão de salvar.
    IF NOT VG_ERR_SAVE IS INITIAL.
      CLEAR VG_ERR_SAVE.
      EXIT.

    ENDIF.
* Verifica se há dados novos.
    IF NOT GT_NOVOS_DOC[] IS INITIAL.
      DATA(GT_NOVOS_DOC_AUX) = GT_NOVOS_DOC.
      CLEAR GT_NOVOS_DOC.
* Move o dado atualizado para a TI de Documentos Novos.
      MOVE-CORRESPONDING GT_VINC TO GT_NOVOS_DOC.
      LOOP AT GT_NOVOS_DOC ASSIGNING FIELD-SYMBOL(<FS_NOVOS_DOC2>).
        READ TABLE GT_NOVOS_DOC_AUX INTO DATA(EL_NOVOS_DOC_AUX) WITH KEY DOCNUM_FLOTE = <FS_NOVOS_DOC2>-DOCNUM_FLOTE
                                                                         DOCNUM_EPROD = <FS_NOVOS_DOC2>-DOCNUM_EPROD
                                                                BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          <FS_NOVOS_DOC2>-DOC_EPROD_ANT = EL_NOVOS_DOC_AUX-DOC_EPROD_ANT.
          <FS_NOVOS_DOC2>-QTD_VINC_ANT  = EL_NOVOS_DOC_AUX-QTD_VINC_ANT.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  SELECT *
    FROM ZSDT_DOCNUM_LOG
    INTO TABLE @DATA(LT_LOG_AUX)
    FOR ALL ENTRIES IN @GT_VINC_AUX
    WHERE DOCNUM_FLOTE = @GT_VINC_AUX-DOCNUM_FLOTE
      AND DOCNUM_EPROD = @GT_VINC_AUX-DOCNUM_EPROD
      AND CANCEL       = @ABAP_FALSE.
  IF SY-SUBRC IS INITIAL.
    SORT LT_LOG BY DOCNUM_FLOTE DOCNUM_EPROD.
  ENDIF.

  IF GT_NOVOS_DOC IS INITIAL.

    MESSAGE S000(Z01) WITH 'Nenhum ajuste realizado!' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.

    LOOP AT GT_NOVOS_DOC ASSIGNING FIELD-SYMBOL(<FS_NOVOS_DOC>).
      LV_QTD_TOTAL = LV_QTD_TOTAL + <FS_NOVOS_DOC>-QTD_VINC.
    ENDLOOP.

  ENDIF.

  LOOP AT GT_NOVOS_DOC ASSIGNING <FS_NOVOS_DOC>.
    READ TABLE GT_VINC_AUX ASSIGNING FIELD-SYMBOL(<FS_VINC_AUX>)
    WITH KEY DOCNUM_EPROD = <FS_NOVOS_DOC>-DOC_EPROD_ANT
             DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE
    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      READ TABLE LT_LOG_AUX ASSIGNING FIELD-SYMBOL(<FS_LOG_AUX>)
      WITH KEY DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE
               DOCNUM_EPROD = <FS_VINC_AUX>-DOCNUM_EPROD
      BINARY SEARCH.
**<<<------"167326 - NMS - INI------>>>
      IF <FS_VINC_AUX>-MANUAL IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
        IF SY-SUBRC IS INITIAL.
          LV_QTD_VINC_ORIG = <FS_LOG_AUX>-QTD_VINC_ORIG.
        ELSE.
          LV_QTD_VINC_ORIG = <FS_VINC_AUX>-QTD_VINC.
        ENDIF.
**<<<------"167326 - NMS - INI------>>>
      ELSE.
        LV_QTD_VINC_ORIG = <FS_VINC_AUX>-QTD_VINC.

      ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      IF LV_QTD_TOTAL < LV_QTD_VINC_ORIG.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TEXT_QUESTION  = 'Quantidade vinculada atual, diferente da quantidade original, Deseja continuar mesmo assim?'
            TEXT_BUTTON_1  = 'Sim'
            TEXT_BUTTON_2  = 'Não'
          IMPORTING
            ANSWER         = LV_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC = 0.
          IF LV_ANSWER <> 1.
            EXIT.
          ENDIF.
        ENDIF.
      ELSEIF LV_QTD_TOTAL > LV_QTD_VINC_ORIG.

          MESSAGE S000(Z01) WITH 'Quantidade vinculada total é maior do que' 'a quantidade vinculada original!' DISPLAY LIKE 'E'.
          RETURN.
**<<<------"167326 - NMS - INI------>>>
*      ELSE.
*        MESSAGE s000(z01) WITH 'Nenhum ajuste realizado!' DISPLAY LIKE 'E'.
*        RETURN.
***<<<------"167326 - NMS - FIM------>>>
      ENDIF.
**<<<------"167326 - NMS - INI------>>>
          IF <FS_VINC_AUX>-MANUAL IS INITIAL.
            DATA(VL_MANUAL) = CKB_VINC.

          ELSE.
            VL_MANUAL = <FS_VINC_AUX>-MANUAL.

          ENDIF.
**<<<------"167326 - NMS - FIM------>>>
          ZCL_IM_CL_FLUXO_EXPORTACAO=>MANUT_FILA(
            I_DOCNUM       = <FS_VINC_AUX>-DOCNUM_EPROD
            I_DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE
            I_OPERACAO     = 'M'
            I_CANCEL       = ABAP_TRUE
            I_SALDO_VINC   = <FS_VINC_AUX>-QTD_VINC
            I_MANUAL       = VL_MANUAL "<<<------"167326 - NMS------>>>
          ).

          IF <FS_VINC_AUX>-VINCULADA_XML IS NOT INITIAL.

            LS_LOG-DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE.
            LS_LOG-DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD.
            LS_LOG-DOCNUM_PROD_VINC_XML = <FS_VINC_AUX>-DOCNUM_EPROD.
            LS_LOG-QTD_VINC_ORIG        = <FS_VINC_AUX>-QTD_VINC.
            LS_LOG-MANUAL               = VL_MANUAL. "<<<------"167326 - NMS------>>>

            MODIFY ZSDT_DOCNUM_LOG FROM LS_LOG.
          ELSE.

            UPDATE ZSDT_DOCNUM_LOG SET CANCEL    = ABAP_TRUE
                                       US_CANCEL = SY-UNAME
                                       DT_CANCEL = SY-DATUM
                                       HR_CANCEL = SY-UZEIT
                                 WHERE DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE
                                   AND DOCNUM_EPROD = <FS_VINC_AUX>-DOCNUM_EPROD.

            READ TABLE LT_LOG_AUX ASSIGNING <FS_LOG_AUX>
            WITH KEY DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE
                     DOCNUM_EPROD = <FS_VINC_AUX>-DOCNUM_EPROD
            BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.

              LS_LOG-DOCNUM_FLOTE = <FS_VINC_AUX>-DOCNUM_FLOTE.
              LS_LOG-DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD.
              LS_LOG-DOCNUM_PROD_VINC_XML = <FS_LOG_AUX>-DOCNUM_PROD_VINC_XML.
              LS_LOG-QTD_VINC_ORIG        = <FS_LOG_AUX>-QTD_VINC_ORIG.
              LS_LOG-MANUAL               = VL_MANUAL. "<<<------"167326 - NMS------>>>

              MODIFY ZSDT_DOCNUM_LOG FROM LS_LOG.

            ENDIF.
          ENDIF.

        ELSE.

          LS_LOG-DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE.
          LS_LOG-DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD.

          READ TABLE LT_LOG_AUX ASSIGNING <FS_LOG_AUX> INDEX 1.
          IF SY-SUBRC IS INITIAL.
            LS_LOG-DOCNUM_PROD_VINC_XML = <FS_LOG_AUX>-DOCNUM_PROD_VINC_XML.
          ELSE.
            READ TABLE GT_NOVOS_DOC ASSIGNING FIELD-SYMBOL(<FS_NOVOS_DOC_AUX>) INDEX 1.
            IF SY-SUBRC IS INITIAL.
              LS_LOG-DOCNUM_PROD_VINC_XML = <FS_NOVOS_DOC_AUX>-DOC_EPROD_ANT.
            ENDIF.
          ENDIF.
**<<<------"167326 - NMS - INI------>>>
          IF NOT CKB_VINC IS INITIAL.
* Verifica a variável de identificação de alteração de Vinculo Criado Manualmente.
            IF VG_ALT_VINC IS INITIAL.
              LS_LOG-QTD_VINC_ORIG = <FS_NOVOS_DOC>-QTD_VINC.

            ELSE.
              LS_LOG-QTD_VINC_ORIG = <FS_LOG_AUX>-QTD_VINC_ORIG.

            ENDIF.

            IF LS_LOG-MANUAL IS INITIAL.
              VL_MANUAL = CKB_VINC.

            ELSE.
              VL_MANUAL = <FS_LOG_AUX>-MANUAL.

            ENDIF.

            LS_LOG-MANUAL = VL_MANUAL.

          ELSE.
* Verificas se o Vinculo foi criado manualmente.
            IF NOT <FS_NOVOS_DOC>-MANUAL IS INITIAL.
              MOVE <FS_NOVOS_DOC>-MANUAL TO: LS_LOG-MANUAL,
                                             VL_MANUAL.

              READ TABLE GT_VINC INTO DATA(EL_VINC) WITH KEY DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE
                                                             DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD.

              IF SY-SUBRC IS INITIAL.
                LS_LOG-QTD_VINC_ORIG = EL_VINC-QTD_VINC_ORIG.

              ENDIF.

            ENDIF.

          ENDIF.
**<<<------"167326 - NMS - FIM------>>>
          MODIFY ZSDT_DOCNUM_LOG FROM LS_LOG.

        ENDIF.

        ZCL_IM_CL_FLUXO_EXPORTACAO=>MANUT_FILA(
          I_DOCNUM       = <FS_NOVOS_DOC>-DOCNUM_EPROD
          I_DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE
          I_OPERACAO     = 'M'
          I_CANCEL       = ABAP_FALSE
          I_SALDO_VINC   = <FS_NOVOS_DOC>-QTD_VINC
          I_MANUAL       = VL_MANUAL "<<<------"167326 - NMS------>>>
        ).
**<<<------"167326 - NMS - INI------>>>
* Ajusta o Campo ID_VINC do relatório ALV para exibição sobente do registro que foi criado.
        CALL METHOD ZCL_IM_CL_FLUXO_EXPORTACAO=>RETURN_NFE_VINC_F_LOTE
          EXPORTING
            I_DOCNUM    = R_DOCNUM
            I_DATA      = S_EMIS[]
          RECEIVING
            R_VINCFLOTE = DATA(LT_VINC_AUX).

        IF NOT LT_VINC_AUX[] IS INITIAL.
          SORT LT_VINC_AUX BY DOCNUM_FLOTE DOCNUM_EPROD ASCENDING ID_VINC DESCENDING.

          READ TABLE LT_VINC_AUX INTO DATA(EL_VINC2) WITH KEY DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE
                                                              DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD
                                                              QTD_VINC     = <FS_NOVOS_DOC>-QTD_VINC.

          IF SY-SUBRC IS INITIAL.
            READ TABLE GT_VINC ASSIGNING FIELD-SYMBOL(<FS_VINC>) WITH KEY DOCNUM_FLOTE = <FS_NOVOS_DOC>-DOCNUM_FLOTE
                                                                          DOCNUM_EPROD = <FS_NOVOS_DOC>-DOCNUM_EPROD
                                                                          ID_VINC      = <FS_NOVOS_DOC>-ID_VINC.

            IF SY-SUBRC IS INITIAL.
              <FS_VINC>-ID_VINC = EL_VINC2-ID_VINC.

            ENDIF.

          ENDIF.

        ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      ENDLOOP.
**<<<------"167326 - NMS - INI------>>>
      IF CKB_VINC IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
        MESSAGE S000(Z01) WITH 'Alterações realizadas com sucesso!'.
**<<<------"167326 - NMS - INI------>>>
      ELSE.
        IF VG_ALT_VINC IS INITIAL.
          MESSAGE S000(Z01) WITH 'Vínculo criado com sucesso!'.

        ELSE.
          MESSAGE S000(Z01) WITH 'Alterações realizadas com sucesso!'.
* Limpa a variável de identificação de alteração de Vinculo Criado Manualmente.
          CLEAR VG_ALT_VINC.

        ENDIF.

      ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      PERFORM UPDATE_ALV.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_nova_linha
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_NOVA_LINHA .

  DATA: LS_API     TYPE REF TO IF_SALV_GUI_OM_EXTEND_GRID_API,
        LS_EDIT    TYPE REF TO IF_SALV_GUI_OM_EDIT_RESTRICTED,
        LV_GRBEW   TYPE LVC_FNAME,
        LV_NOTE    TYPE LVC_FNAME,
        LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL,
        LV_LINES   TYPE SY-TABIX.

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  DATA: LT_ROWS    TYPE SALV_T_ROW.

**<<<------"167326 - NMS - INI------>>>
* Verifica se o vículo criado está sendo alterado.
  IF NOT VG_ALT_VINC IS INITIAL.
    MESSAGE E000(Z01) WITH 'Salve primeiro a Alteração do Vínculo Criado.'.

  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  LR_SELECTIONS = LO_ALV->GET_SELECTIONS( ).
  LT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).
**<<<------"167326 - NMS - INI------>>>
*  IF gt_novos_doc IS NOT INITIAL.
  IF GT_NOVOS_DOC IS NOT INITIAL OR
     CKB_VINC     IS NOT INITIAL.
**<<<------"167326 - NMS - FIM------>>>
    LS_API = LO_ALV->EXTENDED_GRID_API( ).
    LS_EDIT = LS_API->EDITABLE_RESTRICTED( ).

    CALL METHOD LS_EDIT->SET_T_CELLTAB_COLUMNNAME
      EXPORTING
        T_CELLTAB_COLUMNNAME = 'CELLTAB'.

    LS_EDIT->VALIDATE_CHANGED_DATA( ).
    LO_ALV->REFRESH( ).

    LS_CELLTAB-FIELDNAME = 'DOCNUM_EPROD'.
    LS_CELLTAB-STYLE     = '00080000'.
    INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 1.

    LS_CELLTAB-FIELDNAME = 'DOCNUM_FLOTE'.
    LS_CELLTAB-STYLE     = '00080000'.
    INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 2.

    LS_CELLTAB-FIELDNAME = 'QTD_VINC'.
    LS_CELLTAB-STYLE     = '00080000'.
    INSERT LS_CELLTAB INTO LT_CELLTAB INDEX 3.

    APPEND INITIAL LINE TO GT_VINC ASSIGNING FIELD-SYMBOL(<FS_VINC>).

    <FS_VINC>-CELLTAB = LT_CELLTAB.

    READ TABLE GT_NOVOS_DOC ASSIGNING FIELD-SYMBOL(<FS_NOVOS_DOC>) INDEX 1.
    IF SY-SUBRC IS INITIAL.

      <FS_VINC>-QTD_VINC_ORIG = <FS_NOVOS_DOC>-QTD_VINC_ANT.

    ENDIF.

  ELSE.

    MESSAGE S000(Z01) WITH 'Necessário alterar um registro antes!' DISPLAY LIKE 'E'.

  ENDIF.
ENDFORM.
**<<<------"167326 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_delete_line
*&---------------------------------------------------------------------*
*& Excluí Linha e delete registro de formação de lote selecionado
*&---------------------------------------------------------------------*
FORM ZF_DELETE_LINE .

  TABLES ZSDT_DOCNUM_LOG.

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  DATA: LT_ROWS TYPE SALV_T_ROW.

  DATA: RL_DOCNUM TYPE RANGE OF ZDOCNUM_EPROD.

  DATA: VL_ANSWER.

  LR_SELECTIONS = LO_ALV->GET_SELECTIONS( ).
  LT_ROWS       = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF     LINES( LT_ROWS ) GT 1.
    MESSAGE S000(Z01) WITH 'Favor selecionar apenas uma linha!' DISPLAY LIKE 'E'.
    RETURN.

  ELSEIF LINES( LT_ROWS ) EQ 0.
    MESSAGE S000(Z01) WITH 'Favor selecionar uma linha!' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  CHECK LT_ROWS IS NOT INITIAL.

  LOOP AT LT_ROWS ASSIGNING FIELD-SYMBOL(<FS_ROWS>).
    READ TABLE GT_VINC INTO DATA(EL_VINC) INDEX <FS_ROWS>.
* Verifica se é linha vazia.
    IF EL_VINC-DOCNUM_FLOTE IS INITIAL AND
       EL_VINC-DOCNUM_EPROD IS INITIAL.
      DELETE GT_VINC INDEX <FS_ROWS>.

      LO_ALV->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL ).
      CL_GUI_CFW=>FLUSH( ).

      IF NOT CKB_VINC IS INITIAL.
* Atualiza as TIs GT_NOVOS_DOC e GT_VINC.
        PERFORM ZF_ATUALIZA_NOVOS_VINC USING EL_VINC-DOCNUM_FLOTE
                                             EL_VINC-DOCNUM_EPROD.

      ENDIF.

      EXIT.

    ELSE.
      SELECT SINGLE * FROM ZSDT_DOCNUM_LOG WHERE DOCNUM_FLOTE EQ EL_VINC-DOCNUM_FLOTE
                                             AND DOCNUM_EPROD EQ EL_VINC-DOCNUM_EPROD.

        IF SY-SUBRC IS INITIAL.
          IF EL_VINC-ID_VINC IS INITIAL.
            DATA(VL_DELE) = ABAP_ON.

          ELSE.
            VL_DELE = ABAP_OFF.

          ENDIF.

        ELSE.
          VL_DELE = ABAP_ON.

        ENDIF.

        IF NOT CKB_VINC IS INITIAL AND
           NOT VL_DELE  IS INITIAL.
          DELETE GT_VINC INDEX <FS_ROWS>.

          LO_ALV->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL ).
          CL_GUI_CFW=>FLUSH( ).
* Atualiza as TIs GT_NOVOS_DOC e GT_VINC.
          PERFORM ZF_ATUALIZA_NOVOS_VINC USING EL_VINC-DOCNUM_FLOTE
                                               EL_VINC-DOCNUM_EPROD.
          CLEAR VL_DELE.

          EXIT.

        ENDIF.

      ENDIF.
* Função de POPUP de pergunta de prosseguimento de processo.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja EXCLUIR o Documento selecionado?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = VL_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF VL_ANSWER NE '1'.
        EXIT.

      ENDIF.

      CASE ZSDT_DOCNUM_LOG-MANUAL.
        WHEN ABAP_OFF.
* Verifica se a NF-e do Produtor já está vinculada.
          FREE: RL_DOCNUM.
          APPEND VALUE #( SIGN   = ZCL_LES_UTILS=>IF_STAB_CONSTANTS~MC_SIGN_INCLUDE
                          OPTION = ZCL_LES_UTILS=>IF_STAB_CONSTANTS~MC_OPTION_EQUAL
                          LOW    = EL_VINC-DOCNUM_EPROD
                        ) TO RL_DOCNUM.

          DATA(LS_VINCULO) = ZCL_IM_CL_FLUXO_EXPORTACAO=>CHECK_NFE_FILA_VINCULO(  I_DOCNUM = R_DOCNUM[] ).

          IF LS_VINCULO-ZSDTVINC_P_FLOTE IS NOT INITIAL.
            CLEAR LS_FORMACAO_LOTE.
            " MESSAGE 'NF-e já Vinculada, operação não permitida' TYPE 'I'.

            " Função DE POPUP DE PERGUNTA DE PROSSEGUIMENTO DE PROCESSO.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                TITLEBAR              = 'Confirmação'
                TEXT_QUESTION         = 'Lançemento automático deseja excluir?'
                TEXT_BUTTON_1         = 'Sim'
                TEXT_BUTTON_2         = 'Não'
                DEFAULT_BUTTON        = '1'
                DISPLAY_CANCEL_BUTTON = ''
              IMPORTING
                ANSWER                = VL_ANSWER
              EXCEPTIONS
                TEXT_NOT_FOUND        = 1
                OTHERS                = 2.

            IF VL_ANSWER NE '1'.
              EXIT.

            ENDIF.

            ZCL_IM_CL_FLUXO_EXPORTACAO=>MANUT_FILA( I_DOCNUM       = EL_VINC-DOCNUM_EPROD
                                                    I_DOCNUM_FLOTE = EL_VINC-DOCNUM_FLOTE
                                                    I_OPERACAO     = ZCL_IM_CL_FLUXO_EXPORTACAO=>LC_CONTANTE-OPERACAO-MANUTENCAO
                                                    I_CANCEL       = ABAP_TRUE
                                                    I_SALDO_VINC   = EL_VINC-QTD_VINC
                                                    I_MANUAL       = ABAP_OFF
                                                    ).

            UPDATE ZSDT_DOCNUM_LOG
               SET CANCEL    = ABAP_TRUE
                   US_CANCEL = SY-UNAME
                   DT_CANCEL = SY-DATUM
                   HR_CANCEL = SY-UZEIT
            WHERE DOCNUM_FLOTE = EL_VINC-DOCNUM_FLOTE
              AND DOCNUM_EPROD = EL_VINC-DOCNUM_EPROD
              AND CANCEL       = ABAP_FALSE.

            DELETE GT_VINC INDEX <FS_ROWS>.

            IF NOT CKB_VINC IS INITIAL.
* Atualiza as TIs GT_NOVOS_DOC e GT_VINC.
              PERFORM ZF_ATUALIZA_NOVOS_VINC USING EL_VINC-DOCNUM_FLOTE
                                                   EL_VINC-DOCNUM_EPROD.

            ENDIF.

            LO_ALV->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL ).
            CL_GUI_CFW=>FLUSH( ).



            EXIT.

          ENDIF.

        WHEN ABAP_ON.
          CLEAR SY-SUBRC.

        WHEN OTHERS.
*     Do nothing
      ENDCASE.

      IF SY-SUBRC IS INITIAL.
        IF EL_VINC-MANUAL IS INITIAL.
          DATA(VL_MANUAL) = CKB_VINC.

        ELSE.
          VL_MANUAL = EL_VINC-MANUAL.

        ENDIF.

        ZCL_IM_CL_FLUXO_EXPORTACAO=>MANUT_FILA( I_DOCNUM       = EL_VINC-DOCNUM_EPROD
                                                I_DOCNUM_FLOTE = EL_VINC-DOCNUM_FLOTE
                                                I_OPERACAO     = ZCL_IM_CL_FLUXO_EXPORTACAO=>LC_CONTANTE-OPERACAO-MANUTENCAO
                                                I_CANCEL       = ABAP_TRUE
                                                I_SALDO_VINC   = EL_VINC-QTD_VINC
                                                I_MANUAL       = VL_MANUAL
                                                ).

        UPDATE ZSDT_DOCNUM_LOG
           SET CANCEL    = ABAP_TRUE
               US_CANCEL = SY-UNAME
               DT_CANCEL = SY-DATUM
               HR_CANCEL = SY-UZEIT
        WHERE DOCNUM_FLOTE = EL_VINC-DOCNUM_FLOTE
          AND DOCNUM_EPROD = EL_VINC-DOCNUM_EPROD
          AND CANCEL       = ABAP_FALSE.

        DELETE GT_VINC INDEX <FS_ROWS>.

        IF NOT CKB_VINC IS INITIAL.
* Atualiza as TIs GT_NOVOS_DOC e GT_VINC.
          PERFORM ZF_ATUALIZA_NOVOS_VINC USING EL_VINC-DOCNUM_FLOTE
                                               EL_VINC-DOCNUM_EPROD.

        ENDIF.

        LO_ALV->REFRESH( REFRESH_MODE = IF_SALV_C_REFRESH=>FULL ).
        CL_GUI_CFW=>FLUSH( ).

      ENDIF.

    ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_check_linha_duplicada
*&---------------------------------------------------------------------*
*& Verifica linhas duplicada em "Criar Vínculo"
*&---------------------------------------------------------------------*
*& --> UV_TPMSG - Tipo de mensagem
*&---------------------------------------------------------------------*
FORM ZF_CHECK_LINHA_DUPLICADA USING UV_TPMSG TYPE C.

  DATA(TL_DOCS_AUX) = TG_DOCS.
  DATA(LV_LIN) = LINES( TG_DOCS ).
  SORT TL_DOCS_AUX BY DOCNUM_FLOTE DOCNUM_EPROD ROW_ID.
  DELETE ADJACENT DUPLICATES FROM TL_DOCS_AUX COMPARING DOCNUM_FLOTE DOCNUM_EPROD.
  DATA(LV_LIN2) = LINES( TL_DOCS_AUX ).

  IF LV_LIN GT LV_LIN2.
    CASE UV_TPMSG.
      WHEN SY-ABCDE+4(1).  "E - Erro
        MESSAGE E000(Z01) WITH 'Há linhas repetidas. Excluir uma delas!!!'.

      WHEN SY-ABCDE+18(1). "S - Sucesso
        MESSAGE S000(Z01) WITH 'Há linhas repetidas. Excluir uma delas!!!' DISPLAY LIKE 'E'.

      WHEN OTHERS.
*     Do nothing
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_check_linha_criada
*&---------------------------------------------------------------------*
*& Verifica linhas já criadas em "Criar Vínculo"
*&---------------------------------------------------------------------*
*& --> UV_TPMSG - Tipo de mensagem
*&---------------------------------------------------------------------*
FORM ZF_CHECK_LINHA_CRIADA USING UV_TPMSG TYPE C.

* Verifica se o vículo criado não está sendo alterado.
  CHECK VG_ALT_VINC IS INITIAL.

  CHECK NOT TG_DOCS[]      IS INITIAL AND
        NOT GT_NOVOS_DOC[] IS INITIAL.

  SELECT * FROM ZSDT_DOCNUM_LOG
    INTO TABLE @DATA(TL_DOCNUM_LOG)
    FOR ALL ENTRIES IN @TG_DOCS
  WHERE DOCNUM_FLOTE EQ @TG_DOCS-DOCNUM_FLOTE
    AND DOCNUM_EPROD EQ @TG_DOCS-DOCNUM_EPROD
    AND CANCEL       EQ @ABAP_FALSE.

    IF SY-SUBRC IS INITIAL.
      LOOP AT TL_DOCNUM_LOG INTO DATA(EL_DOCNUM_LOG).
        READ TABLE GT_NOVOS_DOC TRANSPORTING NO FIELDS WITH KEY DOCNUM_FLOTE = EL_DOCNUM_LOG-DOCNUM_FLOTE
                                                                DOCNUM_EPROD = EL_DOCNUM_LOG-DOCNUM_EPROD.

        IF SY-SUBRC IS INITIAL.
          DATA(VL_TEXTO) = |Doc. FLote: { EL_DOCNUM_LOG-DOCNUM_FLOTE } e Doc. EProd: { EL_DOCNUM_LOG-DOCNUM_EPROD }.|.

          CASE UV_TPMSG.
            WHEN SY-ABCDE+4(1).  "E - Erro
              MESSAGE E000(Z01) WITH 'NF-e já Vinculada, operação não permitida.' VL_TEXTO.

            WHEN SY-ABCDE+18(1). "S - Sucesso
              MESSAGE S000(Z01) WITH 'NF-e já Vinculada, operação não permitida.' VL_TEXTO DISPLAY LIKE 'E'.

            WHEN OTHERS.
*     Do nothing
          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_atualiza_novos_vinc
*&---------------------------------------------------------------------*
*& Atualiza as TIs GT_NOVOS_DOC e GT_VINC.
*&---------------------------------------------------------------------*
*& --> UV_DOCNUM_FLOTE Docnum Formação de Lote
*& --> UV_DOCNUM_EPROD Docnum Nota Entrada Produtor
*&---------------------------------------------------------------------*
FORM ZF_ATUALIZA_NOVOS_VINC USING UV_DOCNUM_FLOTE TYPE ZDOCNUM_FLOTE
                                  UV_DOCNUM_EPROD TYPE ZDOCNUM_EPROD.

  CONSTANTS: CL_ID_VINC TYPE ZID_VINC VALUE '00'.

  READ TABLE GT_NOVOS_DOC TRANSPORTING NO FIELDS WITH KEY DOCNUM_FLOTE = UV_DOCNUM_FLOTE
                                                          DOCNUM_EPROD = UV_DOCNUM_EPROD
                                                          ID_VINC      = CL_ID_VINC.

  IF SY-SUBRC IS INITIAL.
    DELETE GT_NOVOS_DOC INDEX SY-TABIX.

  ENDIF.

  CLEAR TG_DOCS.
  LOOP AT GT_VINC INTO DATA(EL_VINC).
    APPEND INITIAL LINE TO TG_DOCS ASSIGNING FIELD-SYMBOL(<FS_DOCS>).
    MOVE-CORRESPONDING EL_VINC TO <FS_DOCS>.
    <FS_DOCS>-ROW_ID = SY-TABIX.

  ENDLOOP.

ENDFORM.
**<<<------"167326 - NMS - FIM------>>>
**<<<------"169312 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*&      Form  zf_cria_botao
*&---------------------------------------------------------------------*
*&      Monta o boptão da tela de seleção
*&---------------------------------------------------------------------*
FORM ZF_CRIA_BOTAO .

*  Estrutura para descrever o botão
  DATA: EL_BUTTON TYPE SMP_DYNTXT.

*** Cadastro Aprovador
*  Nome do Botão
  EL_BUTTON-TEXT      = 'Cadastro Aprovador'.
*  Ícone do Botão
  EL_BUTTON-ICON_ID   = ICON_ADD_ROW.
*  Texto que aparecerá ao lado do ícone (pode ser vazio)
  EL_BUTTON-ICON_TEXT = 'Cadastro Aprovadores'.
*  Quickinfo (aparece quando o user passar o mouse sobre o botao)
  EL_BUTTON-QUICKINFO = 'Tela de Cadastro Aprovador'.
*  Associa essas propriedades com a função 1
  SSCRFIELDS-FUNCTXT_01 = EL_BUTTON.

*** Cockpit Aprovação
*  Nome do Botão
  EL_BUTTON-TEXT      = 'Cockpit Aprovação'.
*  Ícone do Botão
  EL_BUTTON-ICON_ID   = ICON_CHANGE_ORDER.
*  Texto que aparecerá ao lado do ícone (pode ser vazio)
  EL_BUTTON-ICON_TEXT = 'Cockpit Aprovação'.
*  Quickinfo (aparece quando o user passar o mouse sobre o botao)
  EL_BUTTON-QUICKINFO = 'Tela de Cockpit Aprovação'.
*  Associa essas propriedades com a função 1
  SSCRFIELDS-FUNCTXT_02 = EL_BUTTON.

ENDFORM.                    " zf_cria_botao
**<<<------"169312 - NMS - FIM------>>>
