*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 10/12/2021                                              &*
*& Descrição: API Integrações abastecimentos                          &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zpmr0074 MESSAGE-ID zcarga.

"Tabelas
TABLES: zpmt0058, equz, aufk, sscrfields.

TYPES BEGIN OF ty_zpmt0058.
INCLUDE STRUCTURE zpmt0058.
TYPES des_status(255).
*DATA des_status(255).
TYPES END OF ty_zpmt0058.

"Contantes
CONSTANTS: gco_bukrs TYPE bukrs VALUE '0001'.

TYPES: BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.

"Tabela Interna Global
DATA: git_t001       TYPE TABLE OF t001,
      git_zpm0058    TYPE TABLE OF zpmt0058,
      it_zpm0058     TYPE TABLE OF zpmt0058,
      t_zpm0058      TYPE TABLE OF zpmt0058,
      w_zpmt0058     TYPE zpmt0058,
      w_zpmt0060     TYPE zpmt0060,
      zequtx         TYPE equtx,
      zposi_contador TYPE imrc_cntrc,
      git_zpmt0061   TYPE TABLE OF zpmt0061 WITH HEADER LINE,
      git_filtro     TYPE zif_screen_linha_filtro_t,
      editor         TYPE REF TO cl_gui_textedit,
      c_editor       TYPE REF TO cl_gui_custom_container,
      txtopen        TYPE c,
      it_editor      TYPE TABLE OF ty_editor,
      wa_editor      TYPE ty_editor,
      p_sim          TYPE char1,
      p_nao          TYPE char1.

"Work Área
DATA: gwa_t001 TYPE t001.

"Variáveis
DATA: gva_bukrs TYPE bukrs.
DATA: icon_proc TYPE string.

*** Anexos:
DATA: t_anexos     TYPE TABLE OF bdn_con,
      l_obj_key    TYPE sibflporb-instid,
      l_lines      TYPE i,
      anexo_obj    TYPE REF TO cl_gos_manager,
      l_ip_mode    TYPE sgs_rwmod,
      l_ip_service TYPE sgs_srvnam,
      w_bor        TYPE borident.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lines                       TYPE sy-tabix,
      wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row.

"Ranges
RANGES:     gra_bukrs FOR t001-bukrs.

"Field-Symbol
FIELD-SYMBOLS: <gfs_t001> TYPE t001.


" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .


    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
    CASE e_column_id.
      WHEN 'STATUS_PROCESSAMENTO'.
        PERFORM fm_log_erros USING e_row_id
                             e_column_id.
    ENDCASE.



    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_ucomm.

    CASE sy-ucomm.
      WHEN 'PROCESSAR'.

        CALL METHOD gob_gui_alv_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.

        ELSE.
          LOOP AT it_selected_rows INTO wa_selected_rows.
            READ TABLE git_zpm0058 INTO DATA(wa_saida) INDEX wa_selected_rows-index.
            APPEND wa_saida TO it_zpm0058.
            CLEAR: wa_saida.
          ENDLOOP.
        ENDIF.

        IF it_zpm0058 IS NOT INITIAL.
          FREE: it_zpm0058.
          it_zpm0058 = git_zpm0058[].
          PERFORM fm_processar.
        ENDIF.


      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: rb_cons RADIOBUTTON GROUP g1 USER-COMMAND abc DEFAULT 'X'. "Consumo.
    SELECTION-SCREEN COMMENT 05(15) TEXT-002 FOR FIELD rb_cons.

    SELECTION-SCREEN POSITION 34.
    SELECTION-SCREEN COMMENT 20(13) TEXT-003 FOR FIELD rb_trans. "Transferencia
    PARAMETERS: rb_trans RADIOBUTTON GROUP g1 .
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: rb_pend RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND abc.
    SELECTION-SCREEN COMMENT 05(15) TEXT-005 FOR FIELD rb_pend.

    SELECTION-SCREEN POSITION 30.
    SELECTION-SCREEN COMMENT 20(10) TEXT-006 FOR FIELD rb_proc.
    PARAMETERS: rb_proc RADIOBUTTON GROUP g2 .
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-007.
  SELECT-OPTIONS: p_iwerk FOR equz-iwerk OBLIGATORY,
                  p_data  FOR aufk-aedat.

SELECTION-SCREEN: END OF BLOCK b3.


SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.

  icon_proc = icon_oo_interface && 'Importar dados sistema UNIDATA'.
  sscrfields-functxt_01 = icon_proc .

AT SELECTION-SCREEN. "PAI
  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.
      PERFORM fm_importa_dados_unidata.
  ENDCASE.


AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_at_selection_screen.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_seleciona_empresa .

*  CONSTANTS: lco_bukrs TYPE bukrs VALUE '0001'.
*  DATA: lit_t001 TYPE TABLE OF t001.
*  DATA: lwa_t001 TYPE t001.
*  FIELD-SYMBOLS: <lfs_t001> TYPE t001.
*  RANGES: lra_bukrs FOR t001-bukrs.
*
*  SELECT * INTO TABLE @lit_t001
*    FROM t001
*  WHERE bukrs NE @space.
*
*  git_t001[] = lit_t001[].
*
*  SORT lit_t001 BY bukrs.
*  READ TABLE lit_t001 INTO lwa_t001 WITH KEY bukrs = '0001' BINARY SEARCH.
*
*  LOOP AT lit_t001 INTO DATA(waaaaa).
*
*    READ TABLE git_t001
*    WITH KEY bukrs = waaaaa-bukrs
*    INTO DATA(wa_teste_t001).
*    IF sy-subrc IS INITIAL.
*      "ATRIBUIR
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  DATA:   lva_tipo_movimento TYPE zpmt0058-movi_tipo_movimento.
  RANGES: rg_werks  FOR zpmt0058-poco_centro,
          rg_lgort  FOR zpmt0058-poco_deposito,
          rg_status FOR zpmt0058-status_processamento .

  CLEAR: rg_status, rg_lgort, rg_werks.
  FREE: rg_status[], rg_lgort[], rg_werks[].
  IF rb_proc IS NOT INITIAL.
    rg_status-sign = 'I'.
    rg_status-option = 'EQ'.
    rg_status-low  = 'S'.
    APPEND rg_status.

  ELSE.
    rg_status-sign = 'I'.
    rg_status-option = 'EQ'.
    rg_status-low  = ''.
    APPEND rg_status.

    rg_status-sign = 'I'.
    rg_status-option = 'EQ'.
    rg_status-low  = 'E'.
    APPEND rg_status.
  ENDIF.

  SELECT * FROM zpmt0061
  INTO TABLE git_zpmt0061
    WHERE uname = sy-uname.

  IF git_zpmt0061[] IS NOT INITIAL.

    CLEAR: rg_werks.
    LOOP AT git_zpmt0061.
      rg_werks-sign = 'I'.
      rg_werks-option = 'EQ'.
      rg_werks-low  = git_zpmt0061-werks.
      APPEND rg_werks.

      IF git_zpmt0061-lgort IS NOT INITIAL.
        rg_lgort-sign = 'I'.
        rg_lgort-option = 'EQ'.
        rg_lgort-low  = git_zpmt0061-lgort.
        APPEND rg_lgort.
      ENDIF.

    ENDLOOP.


    IF rb_cons IS NOT INITIAL.
      SELECT * FROM zpmt0058
        INTO TABLE git_zpm0058
         WHERE poco_centro   IN p_iwerk
             AND data_inicio IN p_data
             AND poco_codigo_destino_aux = '' "consumo é nulo
             AND status_processamento IN rg_status
             AND poco_deposito IN rg_lgort.
    ELSE.
      SELECT * FROM zpmt0058
      INTO TABLE git_zpm0058
       WHERE poco_centro   IN p_iwerk
           AND data_inicio IN p_data
           AND poco_codigo_destino_aux <> ''
           AND status_processamento IN rg_status
           AND poco_deposito IN rg_lgort.
    ENDIF.

    IF git_zpm0058[] IS INITIAL.
      MESSAGE |Informações não encontrada !| TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT git_zpm0058 INTO DATA(lwa_zpm0058) WHERE poco_centro NOT IN rg_werks.
      MESSAGE |Usuário sem acesso a filial { lwa_zpm0058-poco_centro } na transação ZPM0083!| TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDLOOP.

    DELETE git_zpm0058 WHERE poco_centro NOT IN rg_werks.

    IF git_zpm0058 IS INITIAL.
*      MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSE.
    MESSAGE 'Usuário não cadastrado na transação ZPM0083!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DELETE git_zpm0058 WHERE movi_volume EQ 0.
  SORT git_zpm0058 BY data_fim hora_fim.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN rb_pend.
*        IF ( screen-name =  'P_DATA-LOW'  OR
*          screen-name   =  'P_DATA-HIGH' OR
*          screen-name   =  '%_P_DATA_%_APP_%-TEXT' OR
*          screen-name   =  '%_P_DATA_%_APP_%-OPTI_PUSH' OR
*          screen-name   =  '%_P_DATA_%_APP_%-TO_TEXT' OR
*          screen-name   =  '%_P_DATA_%_APP_%-VALU_PUSH' ).
*          screen-input  =  '0'.
*          screen-active =  '0'.
*          REFRESH p_data.
*          MODIFY SCREEN.
*        ENDIF.
      WHEN rb_proc.
*        IF ( screen-name =  'P_DATA-LOW'  OR  screen-name = 'P_DATA-HIGH').
*          screen-input = '1'.
*          screen-active  = '1'.
*          MODIFY SCREEN.
*        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.

  IF rb_proc IS NOT INITIAL.
    APPEND 'PROCESSAR' TO fcode.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR 'TB0100' WITH 'Integração Abastecimentos'.

  PERFORM fm_criar_objetos.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'PROCESSAR'.
*      IF git_zpm0058 IS NOT INITIAL.
      PERFORM fm_processar.
*      ENDIF.

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos.

  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

  PERFORM fm_cria_fieldcat.


  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'API Integrações abastecimentos'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.



    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = git_zpm0058
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat.

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  git_fcat = VALUE lit_fieldcat_aux(
( fieldname ='STATUS_PROCESSAMENTO'               coltext = 'Status Processamento'                                                         col_opt = 'X' no_zero = '' hotspot = 'X' )
*( fieldname ='DES_STATUS'                         coltext = 'Descrição status'                                                             col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_CODIGO'                        coltext = 'ID abastecimento'                                                             col_opt = 'X' no_zero = '' )
( fieldname ='POCO_CODIGO'                        coltext = 'Cód posto/comboio abast.real'                                                 col_opt = 'X' no_zero = '' )
( fieldname ='POCO_DESCRICAO'                     coltext = 'Desc. deposito'                                                               col_opt = 'X' no_zero = '' )
( fieldname ='POCO_CODIGO_AUX'                    coltext = 'Cód. aux. deposito'                                                           col_opt = 'X' no_zero = '' )
( fieldname ='POCO_DEPOSITO'                      coltext = 'Depósito origem                                             '                 col_opt = 'X' no_zero = '' )
( fieldname ='POCO_CODIGO_DESTINO'                coltext = 'Depósito Destino'                                                             col_opt = 'X' no_zero = '' )
( fieldname ='POCO_CENTRO'                        coltext = 'Centro                                                    '                   col_opt = 'X' no_zero = '' )
*( fieldname ='POCO_CGC'                           coltext = 'CGC do posto/comboio'                                                         col_opt = 'X' no_zero = '' )
*( fieldname ='TAFI_CODIGO'                        coltext = 'Códi tanques físicos do posto/comb onde abast. foi realizado'                 col_opt = 'X' no_zero = '' )
*( fieldname ='BICO_NUMERO'                        coltext = 'Código do bico'                                                               col_opt = 'X' no_zero = '' )
*( fieldname ='BICO_CODIGO_AUX'                    coltext = 'Código auxiliar do bico'                                                      col_opt = 'X' no_zero = '' )
( fieldname ='DATA_FIM'                            coltext = 'Data abastecimento'                                                           col_opt = 'X' no_zero = '' )
( fieldname ='HORA_FIM'                            coltext = 'Hora abastecimento'                                                          col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_DURACAO'                       coltext = 'Duração do abastecimento'                                                     col_opt = 'X' no_zero = '' )
( fieldname ='EMPR_CODIGO'                        coltext = 'Empresa                                  '                                    col_opt = 'X' no_zero = '' )
( fieldname ='EMPR_DESCRICAO'                     coltext = 'Desc. empresa'                                                                col_opt = 'X' no_zero = '' )
( fieldname ='EMPR_DESC_ABREV'                    coltext = 'Descrição abrev. empresa'                                                     col_opt = 'X' no_zero = '' )
( fieldname ='VEIC_CODIGOFROTA'                   coltext = 'Frota'                                                                        col_opt = 'X' no_zero = '' )
( fieldname ='VEIC_PLACA'                         coltext = 'Placa'                                                                         col_opt = 'X' no_zero = '' )
*( fieldname ='VEIC_TIPO'                          coltext = 'Tipo Veic. 0 - Proprio / 1 - Terceiro'                                        col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_ENCERRANTE_INICIAL'            coltext = 'Encerrante inicial do bico do abastecimento'                                  col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_ENCERRANTE_FINAL'              coltext = 'Encerrante final do bico do abastecimento'                                    col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_VOLUME'                        coltext = 'Quant. produto'                                                                col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_TOTALIZADOR_VEICULO'           coltext = 'Contador veiculo                            '                                  col_opt = 'X' no_zero = '' )
( fieldname ='MOVI_TOT_VEIC_ANT'                  coltext = 'Contador anterior'                                                             col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_TOTALIZADOR_RODADO'            coltext = 'Totalizador (km/hora) rodado do veículo abastecido'                           col_opt = 'X' no_zero = '' )
*( fieldname ='VEIC_TIPO_TOTALIZADOR'              coltext = 'Tp contador. '' Nao utiliza / 1 - hori / 2 - Hod'                          col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_DESEMPENHO'                    coltext = 'Desempenho do veículo abastecido'                                             col_opt = 'X' no_zero = '' )
*( fieldname ='MOVE_CONSUMO_MEDIO'                 coltext = 'Consumo médio do veículo abastecido'                                          col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_MODO'                          coltext = 'Modo terminal abastecimento:0 - Manu / 1 - Aut / 2 - ManuInt'                 col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_FORMA_ID'                      coltext = 'Id Frentista'                              col_opt = 'X' no_zero = '' )
( fieldname ='USUA_LOGIN'                         coltext = 'Nome do frentista'                                                             col_opt = 'X' no_zero = '' )
( fieldname ='USUA_NOME'                          coltext = 'Usuario'                                                                       col_opt = 'X' no_zero = '' )
( fieldname ='PROD_CODIGO_AUX'                    coltext = 'Codigo material'                                                               col_opt = 'X' no_zero = '' )
( fieldname ='PROD_DISPLAY'                       coltext = 'Desc. material'                                                                col_opt = 'X' no_zero = '' )
*( fieldname ='PROD_CODIGO_AUX'                    coltext = 'Tipo do produto: C - Comb. G - Graxa. l -Lubrif.'                             col_opt = 'X' no_zero = '' )
*( fieldname ='PROD_TIPO'                          coltext = 'Código do centro de custo'                                                    col_opt = 'X' no_zero = '' )
*( fieldname ='CECU_CODIGO'                        coltext = 'Centro de custo'                                                 col_opt = 'X' no_zero = '' )
*( fieldname ='CECU_DESCRICAO'                     coltext = 'Tipo do Movimento'                                                            col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_TIPO_MOVIMENTO'                coltext = 'SIG ordem do abastecimento'                                                   col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_SIG_ORDEM'                     coltext = 'Elemento PEP do abastecimento'                                                col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_ELEMENTO_PEP'                  coltext = 'Conta Razão do abastecimento'                                                 col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_CONTA_RAZAO'                   coltext = 'Número do contrato do veículo'                                                col_opt = 'X' no_zero = '' )
*( fieldname ='CONT_NUMERO'                        coltext = 'Diagrama de rede do abastecimento'                                            col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_DIAGRAMA_REDE'                 coltext = 'Reserva do abastecimento'                                                     col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_RESERVA'                       coltext = 'Código do motorista (quando configurado)'                                     col_opt = 'X' no_zero = '' )
*( fieldname ='MOTO_CODIGO'                        coltext = 'Nome do motorista (quando configurado)'                                       col_opt = 'X' no_zero = '' )
*( fieldname ='MOTO_NOME'                          coltext = 'Código do modelo do veículo'                                                  col_opt = 'X' no_zero = '' )
*( fieldname ='VEMO_CODIGO'                        coltext = 'Código modelo do veículo'                                               col_opt = 'X' no_zero = '' )
( fieldname ='VEMO_DESCRICAO'                     coltext = 'modelo do veículo'                                                             col_opt = 'X' no_zero = '' )
*( fieldname ='VEMA_CODIGO'                        coltext = 'Descrição da marca do veículo'                                                col_opt = 'X' no_zero = '' )
( fieldname ='VEMA_DESCRICAO'                     coltext = 'Marca do veículo'                                                              col_opt = 'X' no_zero = '' )
*( fieldname ='GERE_CODIGO'                        coltext = 'Nome da gerência (quando configurado)'                                        col_opt = 'X' no_zero = '' )
( fieldname ='VEIC_EQUIP_SAP'                     coltext = 'Código do equipamento no SAP'                                                 col_opt = 'X' no_zero = '' )
( fieldname ='VEIC_FROTA_SAP'                     coltext = 'Código da frota no SAP'                                                       col_opt = 'X' no_zero = '' )
*( fieldname ='VEIC_CLASSE'                        coltext = 'Código da classe no SAP'                                                      col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_AREA'                          coltext = 'Código da área do SAP'                                                        col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_PRECO_UNITARIO'                coltext = 'Preço unitário do produto'                                                    col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_PRECO_TOTAL'                   coltext = 'Preço total do abastecimento'                                                 col_opt = 'X' no_zero = '' )
*( fieldname ='EMPR_CODIGO_AUXI'                   coltext = 'Código Auxiliar da empresa proprietária do veículo'                           col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_COMPARTIMENTO'                 coltext = 'Número do compartimento lubrificado'                                          col_opt = 'X' no_zero = '' )
*( fieldname ='COMP_DESCRICAO'                     coltext = 'Descrição do compartimento lubrificando'                                      col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_TROCA_LUB'                     coltext = 'troca ou complemento de lubrificante'                                         col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_NUMERO_OS'                     coltext = 'número da ordem de serviço'                                                   col_opt = 'X' no_zero = '' )
*( fieldname ='CAMA_CODIGO'                        coltext = 'código da Causa de Manutenção'                                                col_opt = 'X' no_zero = '' )
*( fieldname ='CAMA_DESCRICAO'                     coltext = 'descrição da Causa de Manutenção'                                             col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_FRASCO'                        coltext = 'código do frasco da amostra da lubrificação'                                  col_opt = 'X' no_zero = '' )
*( fieldname ='GEPO_LATITUDE'                      coltext = 'Latitude do abastecimento'                                                    col_opt = 'X' no_zero = '' )
*( fieldname ='GEPO_LONGITUDE'                     coltext = 'Longitude do abastecimento'                                                   col_opt = 'X' no_zero = '' )
*( fieldname ='GEPO_ALTITUDE'                      coltext = 'Altitude do abastecimento'                                                    col_opt = 'X' no_zero = '' )
*( fieldname ='GEPO_DENTRO_CERCA'                  coltext = 'Flag que indica se o abastecimento geolocalizado'                             col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_ONLINE'                        coltext = 'Flag que indica se o abastecimento foi realizado online'                      col_opt = 'X' no_zero = '' )
*( fieldname ='OPER_CODIGO'                        coltext = 'Código da operação agrícola (quando configurado)'                             col_opt = 'X' no_zero = '' )
*( fieldname ='OPER_DESCRICAO'                     coltext = 'Descrição da operação agrícola (quando configurado)'                          col_opt = 'X' no_zero = '' )
*( fieldname ='MOVI_SAFRA'                         coltext = 'Código da safra (quando configurado)'                                         col_opt = 'X' no_zero = '' )
*( fieldname ='SAFR_DESCRICAO'                     coltext = 'Descrição da safra (quando configurado)'                                      col_opt = 'X' no_zero = '' )

).
*  DATA: lc_col_pos  TYPE lvc_colpos.
*  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
*  CLEAR: git_fcat.
*
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name = 'ZDE_DATA_COMBOIO_ABASTECIMENTO'
*    CHANGING
*      ct_fieldcat      = git_fcat.
*
*  LOOP AT git_fcat ASSIGNING <fs_cat>.
*
*
*  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros.

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_PROCESSAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_processar .

  DATA: integ_comb TYPE REF TO zcl_integ_comb.
  CREATE OBJECT integ_comb.

  FREE: it_zpm0058.

  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  DESCRIBE TABLE it_selected_rows LINES lines.

  IF ( lines IS INITIAL ).
    MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.

  ELSE.
    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE git_zpm0058 INTO DATA(wa_saida) INDEX wa_selected_rows-index.
      IF sy-subrc EQ 0.
        APPEND wa_saida TO it_zpm0058.
*        CLEAR: wa_saida.
      ENDIF.
      CLEAR: wa_saida.
    ENDLOOP.


    IF it_zpm0058 IS NOT INITIAL.

*      it_zpm0058 = git_zpm0058[].

      "FF #158521 - inicio
      PERFORM f_valida_quant_combustivel TABLES it_zpm0058.
      "FF #158521 - fim


      IF rb_cons IS NOT INITIAL.

        TRY .
            zcl_integ_comb=>zif_integracao_comb~get_instance(
              )->set_processa_consumo( i_comboio = it_zpm0058   ).

          CATCH zcx_integracao INTO DATA(ex_integra).
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_error INTO DATA(ex_error).    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

*    FREE: git_zpm0058.
*    git_zpm0058 = integ_comb->zif_integracao_comb~at_comboio.

      ELSE.
        TRY .
            zcl_integ_comb=>zif_integracao_comb~get_instance(
              )->set_processa_transferencia( i_comboio = git_zpm0058   ).

          CATCH zcx_integracao INTO ex_integra.
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_error INTO ex_error.    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM fm_start_of_selection.

  CALL METHOD gob_gui_alv_grid->refresh_table_display.

ENDFORM.

"#158521 - Fabricio - inicio
FORM f_valida_quant_combustivel TABLES p_it_zpm0058 STRUCTURE zpmt0058.

  DATA: lv_qtd_abastec TYPE dec_16_02_s,
        lv_equnr       TYPE equnr.

  CHECK p_it_zpm0058[] IS NOT INITIAL.

  SELECT * FROM zpmt0060
  INTO TABLE @DATA(lt_0060)
  FOR ALL ENTRIES IN @p_it_zpm0058
  WHERE movi_codigo = @p_it_zpm0058-movi_codigo.

  IF sy-subrc <> 0.
    CLEAR lt_0060[].
  ENDIF.

  LOOP AT p_it_zpm0058 ASSIGNING FIELD-SYMBOL(<fs_0058>).

    lv_equnr = |{ condense( <fs_0058>-veic_frota_sap ) ALPHA = IN }|. "FF #190270

    SELECT SINGLE eqtyp, eqart, herst, typbz
    FROM equi
    INTO @DATA(ls_equi)
    WHERE equnr = @lv_equnr.

    IF sy-subrc = 0.

      IF (  ls_equi-eqtyp EQ '1'
         OR ls_equi-eqtyp EQ '2'
         OR ls_equi-eqtyp EQ '3'
         OR ls_equi-eqtyp EQ '4'
         OR ls_equi-eqtyp EQ 'A' ).

        DATA(lo_pm_data_equipament) = NEW zcl_pm_data_equipament( ).

        lv_qtd_abastec  = <fs_0058>-movi_volume.

        lo_pm_data_equipament->zif_pm_data_equipament~valida_capacidade_combustivel(

          EXPORTING
            i_cod_classe           = ls_equi-eqart
            i_fabricante           = ls_equi-herst
            i_modelo               = ls_equi-typbz
            i_qtd_abastec          = lv_qtd_abastec
          IMPORTING
            e_tq_comb              = DATA(lv_tq_comb)
            e_dentro_da_tolerancia = DATA(lv_dentro_tolerancia) ).

        IF lv_dentro_tolerancia <> abap_true.

          READ TABLE lt_0060 WITH KEY movi_codigo = <fs_0058>-movi_codigo INTO DATA(ls_0060).
          IF sy-subrc = 0.

            ls_0060-tipo_msg = 'E'.
            ls_0060-message = |Verificar parâmetros de TQ Abast transação ZPM0017|.

            MODIFY zpmt0060 FROM ls_0060.
            COMMIT WORK.
            CLEAR:  ls_0060.

            UPDATE zpmt0058 SET status_processamento = 'E'
              WHERE movi_codigo = <fs_0058>-movi_codigo.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
"#158521 - Fabricio - fim

*&---------------------------------------------------------------------*
*&      Form  FM_LOG_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_log_erros  USING e_row TYPE lvc_s_row
                          e_column_id TYPE  lvc_s_col.

  DATA: lit_zpmt0060      TYPE TABLE OF zpmt0060,
        linha_selecionada TYPE slis_selfield,
        _exit             TYPE c,
        lwa_zpmt0060      TYPE zpmt0060.


  TYPES: BEGIN OF ty_itab ,
           name(90) TYPE c,
         END OF ty_itab.

  DATA: lva_msg_alv TYPE char80,
        li_tab_msg  TYPE TABLE OF ty_itab,
        lwa_tab_msg TYPE  ty_itab,
        zcontador   TYPE imrc_cntrc,
        t_zpmt0060  TYPE TABLE OF zpmt0060.

  CLEAR: w_zpmt0058, w_zpmt0060.


  DATA: integ_comb TYPE REF TO zcl_integ_comb.
  CREATE OBJECT integ_comb.

  READ TABLE git_zpm0058 INTO w_zpmt0058 INDEX e_row-index.

  CHECK sy-subrc = 0.

*  IF lwa_zpm0058-status_processamento = 'E'.
*    lwa_tab_msg-name    = '------------------------MENSAGEM ERRO---------------------------------------'.
*    APPEND lwa_tab_msg TO li_tab_msg  .
*    CLEAR lwa_tab_msg.
*  ELSE.
*    lwa_tab_msg-name    = '------------------------MENSAGEM SUCESSO------------------------------------'.
*    APPEND lwa_tab_msg TO li_tab_msg  .
*    CLEAR lwa_tab_msg.
*  ENDIF.

  CLEAR: w_zpmt0060.
  SELECT SINGLE *
     FROM zpmt0060
     INTO w_zpmt0060
  WHERE  movi_codigo  =  w_zpmt0058-movi_codigo.


  IF w_zpmt0060-znumber NE 2.
    APPEND w_zpmt0060 TO t_zpmt0060.



*    DATA: itab TYPE TABLE OF trtab WITH HEADER LINE.
*    DATA:  msg_lote TYPE char80.
*
**  LOOP AT lit_zpmt0060 INTO lwa_zpmt0060.
*
*    itab-line = w_zpmt0060-message .
*    APPEND itab.
*    CLEAR itab.
**  ENDLOOP.
*
*    msg_lote = |ID Abastecimento { w_zpmt0060-movi_codigo }|.
**  CONCATENATE 'ID Abastecimento ' lwa_zpm0058-movi_codigo INTO msg_lote SEPARATED BY space.
*
*    CALL FUNCTION 'LAW_SHOW_POPUP_WITH_TEXT'
*      EXPORTING
*        titelbar         = msg_lote
*      TABLES
*        list_tab         = itab[]
*      EXCEPTIONS
*        action_cancelled = 1
*        OTHERS           = 2.

    IF ( t_zpmt0060 IS NOT INITIAL ).
*      DELETE T_ORDEM WHERE WARPL NE WA_PLANOS-WARPL.

      DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(

      ( fieldname = 'MOVI_CODIGO     '        seltext_m = 'ID Documento'  outputlen = '07' )
      ( fieldname = 'VEIC_CODIGOFROTA'        seltext_m = 'Equipamento '  outputlen = '15' )
      ( fieldname = 'TIPO_MSG        '        seltext_m = 'Tipo de msg '  outputlen = '10' )
      ( fieldname = 'ZNUMBER         '        seltext_m = 'Nº msg      '  outputlen = '08' )
       ( fieldname = 'MESSAGE        '        seltext_m = 'Desc.msg    '  outputlen = '60' ) ).

      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title     = 'Log de processamento'
          i_selection = 'X'
          i_tabname   = 't_zpmt0060'
          i_zebra     = 'X'
          it_fieldcat = tl_fieldcat
        IMPORTING
          es_selfield = linha_selecionada
          e_exit      = _exit
        TABLES
          t_outtab    = t_zpmt0060.
    ENDIF.

  ELSE.

    IF w_zpmt0058-veic_frota_sap IS NOT INITIAL.
      integ_comb->zif_integracao_comb~at_epto = |{ w_zpmt0058-veic_frota_sap ALPHA = IN }|.
    ELSE.
      integ_comb->zif_integracao_comb~at_epto = |{ w_zpmt0058-veic_codigofrota ALPHA = IN }|.
    ENDIF.



    "Seleciona descrição do veiculo.
    SELECT SINGLE eqktx FROM eqkt INTO w_zpmt0058-gere_nome WHERE equnr EQ integ_comb->zif_integracao_comb~at_epto.

    "Selecionar pontos de medição equipamento.
    integ_comb->zif_integracao_comb~set_ponto_medicao( ).

    LOOP AT integ_comb->zif_integracao_comb~at_ponto_medicao  ASSIGNING FIELD-SYMBOL(<w_dimpt>) WHERE indtr NE abap_true AND ( atnam EQ 'HORIMETRO' ) OR ( atnam EQ 'ODOMETRO' ).

      zcontador =  w_zpmt0058-movi_totalizador_rodado.
      IF sy-subrc EQ 0.
        zcl_int_sappm_autotrac=>m_check_pont_med(
          EXPORTING
            i_date  = w_zpmt0058-data_inicio
            i_time  = w_zpmt0058-hora_inicio
            i_point = <w_dimpt>-point         " Ponto de medição
          IMPORTING
            e_value = zcontador          " Unidade de medida ao entrar documento
        ).

      ENDIF.
    ENDLOOP.

    CONDENSE zcontador.
    REPLACE '.' IN zcontador WITH ',' .
    REPLACE '.' IN zcontador WITH ',' .

    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = zcontador
      IMPORTING
        num             = w_zpmt0058-movi_tot_veic_ant
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0.
      CALL SCREEN 0200 STARTING AT 5 5 ENDING AT 100 23.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      DATA: w_text TYPE char72.
      DATA: integ_comb TYPE REF TO zcl_integ_comb.
      CREATE OBJECT integ_comb.

****   Verifica se o campo observação foi preenchida.

      CLEAR: wa_editor.
      FREE:  t_zpm0058.
      CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
      IF it_editor IS INITIAL.
        MESSAGE TEXT-008 TYPE 'I' DISPLAY LIKE 'E'.
        txtopen = abap_true.
      ELSE.
*
        LOOP AT it_editor ASSIGNING FIELD-SYMBOL(<w_text>).
          w_text = |{ w_text } { <w_text>-line }|.

        ENDLOOP.

        DATA(p_sub_cont) = ' '.
        IF p_sim EQ abap_true.
          p_sub_cont = abap_true.
        ELSE.
          p_sub_cont = ' '.
        ENDIF.

        TRY .
            "Processa documento.
            integ_comb->zif_integracao_comb~regist_justif(
              EXPORTING
                i_line     =  w_text         " Texto descritivo - linha de 72 caracteres
                w_zpmt0058 =  w_zpmt0058     " Informações integradas SAP x UNIDATA
                i_odometro =  CONV #( zposi_contador ) " Posição do contador
            ).

            w_zpmt0058-movi_totalizador_veiculo = w_zpmt0058-movi_totalizador_veiculo.
            APPEND w_zpmt0058 TO t_zpm0058.

            MODIFY zpmt0058 FROM w_zpmt0058.
            COMMIT WORK.
            CLEAR: w_zpmt0058.


            zcl_integ_comb=>zif_integracao_comb~get_instance(
              )->set_processa_consumo( i_comboio = t_zpm0058   ).


            "Atualiza tabela da ALV.
            LOOP AT git_zpm0058 ASSIGNING FIELD-SYMBOL(<ls_zpm0058>).
              READ TABLE integ_comb->zif_integracao_comb~at_comboio INTO DATA(ws_zpmt0058) WITH KEY movi_codigo = <ls_zpm0058>-movi_codigo.
              IF sy-subrc EQ 0.
                <ls_zpm0058>-status_processamento = ws_zpmt0058-status_processamento.
                <ls_zpm0058>-movi_totalizador_veiculo = ws_zpmt0058-movi_totalizador_veiculo.
              ENDIF.
            ENDLOOP.

          CATCH zcx_integracao INTO DATA(ex_integra).
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_error INTO DATA(ex_error).    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

        PERFORM fm_start_of_selection.

        CALL METHOD gob_gui_alv_grid->refresh_table_display.

        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  PERFORM caixa_txt_obs.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM caixa_txt_obs .

  txtopen = abap_true.

  IF NOT c_editor IS INITIAL AND NOT editor IS INITIAL.
    CALL METHOD c_editor->free( ).
    CALL METHOD editor->free( ).
  ENDIF.

  CREATE OBJECT: c_editor EXPORTING container_name = 'C_EDITOR', "CONTAINER
                   editor EXPORTING parent         = c_editor.

  CALL METHOD editor->set_toolbar_mode( toolbar_mode = editor->false ).
  CALL METHOD editor->set_statusbar_mode( statusbar_mode = editor->false ).

  CASE txtopen.
    WHEN 'X'.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
    WHEN OTHERS.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
  ENDCASE.

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.


  FREE: txtopen, it_editor.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TIT0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_IMPORTA_DADOS_UNIDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_importa_dados_unidata .
  "Processar programa.
  SUBMIT zpmr0075 AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_IMPORTAR_ANEXOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_importar_anexos .

  l_obj_key = 'ZPM0085'.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZPM0085_ANEXO'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  l_ip_mode     = 'E'.
  l_ip_service  = COND #( WHEN l_lines = 0 THEN 'PCATTA_CREA'
                                           ELSE 'VIEW_ATTA' ).
  w_bor-objkey  = l_obj_key. "l_chave.
  w_bor-objtype = 'ZPM0085_ANEX'.

  anexo_obj->set_rw_mode( ip_mode = l_ip_mode ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = l_ip_service
      is_object          = w_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  WAIT UP TO 2 SECONDS.

  COMMIT WORK.

ENDFORM.
