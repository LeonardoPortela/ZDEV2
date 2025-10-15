*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Subcontratação da  frota  própria                       *
* Transação..: ZLES0181                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zlesr0142.

*----------------------------------------------------------------------*
* Tabelas -------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: zlest0194.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
TYPES: BEGIN OF y_report,
         selecionar    TYPE zde_selecionar,
         icon          TYPE zde_status,
         dt_chegada    TYPE zde_dt_chegada,
         dt_emissao    TYPE j_1bdocdat,
         chave_xml_cte TYPE zde_chave_doc_e,
         peso_saida    TYPE zde_peso_saida2,
         meins         TYPE zde_meins,
         id_ctr        TYPE zsdt0244-id_ctr,
         ano           TYPE zsdt0244-ano,
         tarifa        TYPE zsdt0244-tarifa,
         unid_tarifa   TYPE zsdt0244-unid_tarifa,
         valor_cte     TYPE zde_vlr15_02,
         produto       TYPE zde_produto,
         dt_mov_ov     TYPE j_1bdocdat,
         ov_sub        TYPE zde_ov_sub,
         fat_sub       TYPE zde_fat_sub,
         status_cte    TYPE c LENGTH 20,
         docnum_sub    TYPE zde_docnum_sub2,
         mensagem      TYPE zde_msg,
         celltab       TYPE lvc_t_styl, "BUG - 89694 - CBRAND
       END OF  y_report.


TYPES: BEGIN OF y_outtab_mess,
         status TYPE char4,
         msg    TYPE char200,
       END   OF y_outtab_mess.
*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA: t_zlest0194        TYPE TABLE OF zlest0194,
      t_fieldcat         TYPE lvc_t_fcat,
      t_report           TYPE TABLE OF y_report, "zsds025,
      t_zib_cte_dist_n55 TYPE TABLE OF zib_cte_dist_n55.

DATA: t_zcte_ciot        TYPE TABLE OF zcte_ciot,
      t_zcte_identifica  TYPE TABLE OF zcte_identifica,
      t_zcte_doc_ant     TYPE TABLE OF zcte_doc_ant,
      t_zcte_obs_gerais  TYPE TABLE OF zcte_obs_gerais,
      t_zcte_parceiros   TYPE TABLE OF zcte_parceiros,
      t_zcte_trans       TYPE TABLE OF zcte_trans,
      t_zcte_info_nota   TYPE TABLE OF zcte_info_nota,
      t_zcte_motorista   TYPE TABLE OF zcte_motorista,
      t_zib_cte_dist_ter TYPE TABLE OF zib_cte_dist_ter,
      t_cte_dist_n01     TYPE TABLE OF zib_cte_dist_n01,
      t_outtab_mess      TYPE TABLE OF y_outtab_mess,
      gr_table           TYPE REF TO cl_salv_table.

DATA: ls_celltab TYPE lvc_s_styl,
      lt_celltab TYPE TABLE OF lvc_s_styl.

DATA: columns TYPE REF TO cl_salv_columns_table,
      column  TYPE REF TO cl_salv_column.


*----------------------------------------------------------------------*
* Estruturas internas -------------------------------------------------*
*----------------------------------------------------------------------*
DATA: wa_toolbar     TYPE stb_button,
      wa_layout      TYPE lvc_s_layo,
      wa_outtab_mess TYPE  y_outtab_mess.

*----------------------------------------------------------------------*
* Variaveis -----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  v_ucomm  TYPE sy-ucomm,
  v_bahns  TYPE lfa1-bahns,
  v_okcode TYPE sy-ucomm.

*---------------------------------------------------------------------
*
* Classes locais (Definição)
*
*---------------------------------------------------------------------
DATA: ctl_alv  TYPE REF TO cl_gui_alv_grid.

CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.

* Link visualização
    METHODS :
      handle_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS. "LCL_GRID_EVENT DEFINITION

*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event IMPLEMENTATION.

  METHOD handle_hotspot.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_toolbar.
*...Barra de Ferramentas
    PERFORM f_toolbar_grid CHANGING e_object.
  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command USING e_ucomm.
  ENDMETHOD. "handle_command_grid

ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION

DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container,
      lcl_event         TYPE REF TO lcl_grid_event.

*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_dt    FOR zlest0194-dt_emissao OBLIGATORY,
                  s_chave FOR zlest0194-chave_xml_cte.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2  WITH FRAME TITLE TEXT-042.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_r1 RADIOBUTTON GROUP 1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(10)  TEXT-002.

    PARAMETERS: r_r2 RADIOBUTTON GROUP 1.
    SELECTION-SCREEN COMMENT 16(10) TEXT-003.

    PARAMETERS: r_r3 RADIOBUTTON GROUP 1.
    SELECTION-SCREEN COMMENT 29(10) TEXT-044.
  SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-043.
  PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD'.
SELECTION-SCREEN END OF BLOCK bl2.

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  IF t_report[] IS NOT INITIAL.
    CALL SCREEN 8000.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_command  USING    p_e_ucomm.

  CASE v_ucomm.

    WHEN 'GERAR'.

      PERFORM f_gerar.
      PERFORM f_atualiza_zlest0194.
      PERFORM f_grava_tabelas.

*      PERFORM f_seleciona_dados.
      PERFORM f_atualiza_alv.

*      WAIT UP TO 10 SECONDS.
*      PERFORM f_atualiza_zlest0194.
*      PERFORM f_grava_tabelas.
*      PERFORM f_call_alv.

    WHEN 'ESTORNAR'.

      PERFORM f_estornar.
      PERFORM f_atualiza_zlest0194.
      PERFORM f_deleta_tabelas.

      DATA(t_erro) = t_report.
      DELETE t_erro WHERE icon       <> icon_red_light.
*                      AND mensagem   <> text-041.

      PERFORM f_seleciona_dados.

      LOOP AT t_report ASSIGNING FIELD-SYMBOL(<fs_report>).

        READ TABLE t_erro INTO DATA(w_erro) WITH KEY   chave_xml_cte =   <fs_report>-chave_xml_cte.
        IF sy-subrc IS INITIAL.
          <fs_report>-icon       = icon_red_light.
          <fs_report>-mensagem   = w_erro-mensagem.
        ENDIF.

      ENDLOOP.

      PERFORM f_atualiza_alv.

    WHEN 'ATUALIZAR'.
*
      PERFORM f_atualiza_zlest0194.
      PERFORM f_grava_tabelas.

      PERFORM f_seleciona_dados.
      PERFORM f_atualiza_alv.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  REFRESH: t_zlest0194, t_report.

  CASE abap_true.
    WHEN r_r1.       "Pendente

      SELECT *
        FROM zlest0194
        INTO TABLE t_zlest0194
       WHERE dt_emissao    IN s_dt
         AND chave_xml_cte IN s_chave
         AND ov_sub     EQ space.

    WHEN r_r2.   "Gerado

      SELECT *
        FROM zlest0194
        INTO TABLE t_zlest0194
       WHERE dt_emissao    IN s_dt
         AND chave_xml_cte IN s_chave
         AND ov_sub        NE space.

      IF t_zlest0194[] IS NOT INITIAL.

        SELECT docnum, cancel, code, action_requ FROM j_1bnfe_active
          INTO TABLE @DATA(t_active)
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE docnum = @t_zlest0194-docnum_sub.

      ENDIF.

    WHEN r_r3.

      SELECT * FROM zlest0194
        INTO TABLE t_zlest0194
        WHERE dt_emissao    IN s_dt
          AND chave_xml_cte IN s_chave.

      IF t_zlest0194[] IS NOT INITIAL.

        DATA(t_zlest0194_aux) = t_zlest0194.

        DELETE t_zlest0194_aux WHERE docnum_sub IS INITIAL.

        IF t_zlest0194_aux[] IS NOT INITIAL.

          SELECT docnum cancel code action_requ FROM j_1bnfe_active
            INTO TABLE t_active
            FOR ALL ENTRIES IN t_zlest0194_aux
            WHERE docnum = t_zlest0194_aux-docnum_sub.

        ENDIF.

      ENDIF.


    WHEN OTHERS.
  ENDCASE.

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH TEXT-004 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  "Busca dados do contrato
  SELECT * FROM zsdt0244
    INTO TABLE @DATA(t_zsdt0244)
    FOR ALL ENTRIES IN @t_zlest0194
    WHERE id_ctr = @t_zlest0194-id_ctr
      AND ano    = @t_zlest0194-ano.

  LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>).

    APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).

    <fs_report>-dt_chegada    = <fs_zlest0194>-dt_descarga.
    <fs_report>-dt_emissao    = <fs_zlest0194>-dt_emissao.
    <fs_report>-chave_xml_cte = <fs_zlest0194>-chave_xml_cte.
    <fs_report>-valor_cte     = <fs_zlest0194>-valor_prestacao.
    <fs_report>-peso_saida    = <fs_zlest0194>-qt_carga_cte.
    <fs_report>-meins         = <fs_zlest0194>-meins.
    <fs_report>-dt_mov_ov     = <fs_zlest0194>-dt_mov_ov.
    <fs_report>-produto       = <fs_zlest0194>-ds_prod_pred.
    <fs_report>-ov_sub        = <fs_zlest0194>-ov_sub.
    <fs_report>-fat_sub       = <fs_zlest0194>-fat_sub.
    <fs_report>-docnum_sub    = <fs_zlest0194>-docnum_sub.

    "Dados do contrato
    <fs_report>-id_ctr      = <fs_zlest0194>-id_ctr.
    <fs_report>-ano         = <fs_zlest0194>-ano.

    READ TABLE t_zsdt0244 INTO DATA(w_zsdt0244) WITH KEY id_ctr = <fs_report>-id_ctr
                                                         ano    = <fs_report>-ano.
    IF sy-subrc IS INITIAL.
      <fs_report>-tarifa      = w_zsdt0244-tarifa .
      <fs_report>-unid_tarifa = w_zsdt0244-unid_tarifa.
    ENDIF.

    <fs_report>-docnum_sub    = <fs_zlest0194>-docnum_sub.

*    IF  <fs_report>-docnum_sub IS NOT INITIAL
*        AND <fs_report>-fat_sub  IS NOT INITIAL
*          AND <fs_report>-ov_sub  IS NOT INITIAL
*            AND <fs_report>-dt_mov_ov IS NOT INITIAL.
*
*      <fs_report>-icon      = icon_green_light.
*      <fs_report>-mensagem  = text-035.
*
*    ENDIF.

    READ TABLE t_active INTO DATA(w_active) WITH KEY docnum = <fs_zlest0194>-docnum_sub.

    IF sy-subrc IS INITIAL.
* COMPLETED
***********************************************************************
      IF w_active-action_requ CA 'C'.

        <fs_report>-status_cte = icon_complete.

        IF w_active-cancel IS INITIAL.
          <fs_report>-icon = icon_green_light.
        ENDIF.

        IF w_active-cancel IS NOT INITIAL.
          <fs_report>-icon = icon_red_light.
        ENDIF.

* ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
***********************************************************************
      ELSEIF w_active-action_requ CA '145'.
        <fs_report>-status_cte = icon_alert.
* IN PROCESS - USER ACTION REQUIRED
***********************************************************************
      ELSEIF w_active-action_requ CA '2367890'.
        <fs_report>-status_cte = icon_warning.
* IN PROCESS - NO USER ACTION REQUIRED
***********************************************************************
      ELSE.
        <fs_report>-status_cte = icon_activity.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM f_toolbar_grid  CHANGING  p_object TYPE REF TO cl_alv_event_toolbar_set.


  CLEAR wa_toolbar.
  MOVE: 'ATUALIZAR' TO wa_toolbar-function ,
  icon_refresh TO wa_toolbar-icon ,
*  TEXT-009 TO WA_TOOLBAR-TEXT ,
  space TO wa_toolbar-disabled .
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CASE abap_true.
    WHEN r_r1.

      CLEAR wa_toolbar.
      MOVE: 'GERAR' TO wa_toolbar-function ,
      icon_import_all_requests TO wa_toolbar-icon ,
      TEXT-005 TO wa_toolbar-text ,
      space TO wa_toolbar-disabled .
      APPEND wa_toolbar TO p_object->mt_toolbar.

    WHEN r_r2.

      CLEAR wa_toolbar.
      MOVE: 'ESTORNAR' TO wa_toolbar-function ,
      icon_storno TO wa_toolbar-icon ,
      TEXT-006 TO wa_toolbar-text ,
      space TO wa_toolbar-disabled .
      APPEND wa_toolbar TO p_object->mt_toolbar.

    WHEN r_r3.

      CLEAR wa_toolbar.
      MOVE: 'GERAR' TO wa_toolbar-function ,
      icon_import_all_requests TO wa_toolbar-icon ,
      TEXT-005 TO wa_toolbar-text ,
      space TO wa_toolbar-disabled .
      APPEND wa_toolbar TO p_object->mt_toolbar.

      CLEAR wa_toolbar.
      MOVE: 'ESTORNAR' TO wa_toolbar-function ,
      icon_storno TO wa_toolbar-icon ,
      TEXT-006 TO wa_toolbar-text ,
      space TO wa_toolbar-disabled .
      APPEND wa_toolbar TO p_object->mt_toolbar.

    WHEN OTHERS.

  ENDCASE.

  DELETE p_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                  OR function = '&LOCAL&INSERT_ROW'
                                  OR function = '&LOCAL&DELETE_ROW'
                                  OR function = '&LOCAL&COPY_ROW'
                                  OR function = '&LOCAL&CUT'
                                  OR function = '&LOCAL&COPY'
                                  OR function = '&LOCAL&PASTE'
                                  OR function = '&REFRESH'
                                  OR function = '&CHECK'
                                  OR function = '&GRAPH'
                                  OR function = '&INFO'
                                  OR function = '&LOCAL&UNDO'
                                  OR function = '&MB_VIEW'
                                  OR function = '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click  USING    p_index  TYPE any
                                       p_column TYPE any.


  READ TABLE t_report INTO DATA(w_report) INDEX p_index.

  CASE p_column.
    WHEN 'OV_SUB'.
      SET PARAMETER ID 'AUN' FIELD w_report-ov_sub.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    WHEN 'FAT_SUB'.
      SET PARAMETER ID 'VF' FIELD w_report-fat_sub.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
    WHEN 'DOCNUM_SUB'.

      SELECT SINGLE docnum, bukrs FROM j_1bnfdoc
        INTO @DATA(w_doc)
        WHERE docnum = @w_report-docnum_sub.

      IF sy-subrc IS INITIAL.
*        SET PARAMETER ID 'JEF' FIELD w_doc-docnum.
*        SET PARAMETER ID 'BUK' FIELD w_doc-bukrs.
*        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD w_doc-docnum.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD w_doc-bukrs.
        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
      ENDIF.
*      SET PARAMETER ID 'JEF' FIELD w_report-docnum_sub.
*      CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
    WHEN 'MENSAGEM'. "BUG - 89694 - CBRAND
      PERFORM display_mensagem.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8000 OUTPUT.

*  CASE abap_true.
*    WHEN r_r1.
*      SET PF-STATUS 'PF_9000'.
*      SET TITLEBAR 'TITULO_9000'.
*    WHEN r_r2.
*      SET PF-STATUS 'PF_8000'.
*      SET TITLEBAR 'TITULO_8000'.
*    WHEN OTHERS.
*  ENDCASE.

  SET PF-STATUS 'PF_8000'.
  SET TITLEBAR 'TITULO_8000'.
  PERFORM f_alv_geradas.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8000 INPUT.

  CASE v_okcode.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_GERADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_geradas .

  DATA: wa_variant TYPE disvariant.

  IF lcl_alv IS BOUND.
    CALL METHOD lcl_alv->free.
    CLEAR lcl_alv.
  ENDIF.

  IF lcl_container_alv IS BOUND.
    CALL METHOD lcl_container_alv->free.
    CLEAR lcl_container_alv.
  ENDIF.

  CREATE OBJECT lcl_container_alv
    EXPORTING
      container_name              = 'ALV2'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc IS INITIAL.

    CREATE OBJECT lcl_alv
      EXPORTING
        i_parent          = lcl_container_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


    CREATE OBJECT lcl_event.


* Incluir a hOTSPOT
    SET HANDLER lcl_event->handle_hotspot FOR lcl_alv.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER lcl_event->handle_toolbar FOR lcl_alv.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER lcl_event->handle_command_grid FOR lcl_alv.

    REFRESH t_fieldcat.
    PERFORM zf_montar_fieldcat CHANGING t_report t_fieldcat.
    PERFORM zf_ajuste_campos   CHANGING t_fieldcat.

    wa_layout-zebra       = abap_true.       "Código Zebrado
    "wa_layout-cwidth_opt  = abap_true.  "Ajusta tamanho na coluna *BUG - 89694 - CBRAND
    wa_layout-box_fname   = abap_true.   "

    wa_layout-stylefname = 'CELLTAB'. "BUG - 89694 - CBRAND

    CASE abap_true.
      WHEN r_r1.
        wa_layout-grid_title  = 'Pendentes'.
      WHEN r_r2.
        wa_layout-grid_title  = 'Geradas'.
      WHEN r_r3.
        wa_layout-grid_title  = 'Visão Geral'.
      WHEN OTHERS.
    ENDCASE.

    wa_layout-sel_mode   = 'A'.

    wa_variant-report   = sy-repid.
    wa_variant-variant  = p_layout.

    CALL METHOD lcl_alv->set_table_for_first_display
      EXPORTING
        is_variant      = wa_variant
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = t_report[]
        it_fieldcatalog = t_fieldcat[].

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_REPORT  text
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DEFINIR_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_definir_hotspot  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <fs_fcat> LIKE LINE OF pt_fieldcat.

  LOOP AT pt_fieldcat ASSIGNING <fs_fcat>.
    IF <fs_fcat>-fieldname = 'OV_SUB' OR
       <fs_fcat>-fieldname = 'FAT_SUB' OR
       <fs_fcat>-fieldname = 'DOCNUM_SUB'.
      <fs_fcat>-hotspot = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_campos CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

*----------------------
*  Descrição dos campos do ALV da Tela 9000
*----------------------
    CASE <fs_fcat>-fieldname.
      WHEN 'STATUS'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'Status'.
      WHEN 'SELECIONAR'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'OV_SUB'.
        <fs_fcat>-hotspot = 'X'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'OV. SubContr.'.
        <fs_fcat>-outputlen = 14.
      WHEN 'FAT_SUB'.
        <fs_fcat>-hotspot = 'X'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'Fatura SubContr.'.
        <fs_fcat>-outputlen = 16.
      WHEN 'DOCNUM_SUB'.
        <fs_fcat>-hotspot = 'X'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'Documento SubContr.'.
        <fs_fcat>-outputlen = 20.
      WHEN 'VALOR_CTE'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'Valor CTe Contr.'.
        <fs_fcat>-outputlen = 16.
      WHEN 'MENSAGEM'.
        <fs_fcat>-outputlen = 100.
      WHEN 'ICON'.
        " <fs_fcat>-no_out = abap_true.
      WHEN 'DT_CHEGADA'.
        <fs_fcat>-outputlen = 12.
      WHEN 'DT_EMISSAO'.
        <fs_fcat>-outputlen = 12.
      WHEN 'CHAVE_XML_CTE'.
        <fs_fcat>-outputlen = 44.
      WHEN 'PESO_SAIDA'.
        <fs_fcat>-outputlen = 15.
      WHEN 'MEINS'.
        <fs_fcat>-outputlen = 05.
      WHEN 'ID_CTR'.
        <fs_fcat>-outputlen = 12.
      WHEN 'ANO'.
        <fs_fcat>-outputlen = 04.
      WHEN 'TARIFA'.
        <fs_fcat>-outputlen = 12.
      WHEN 'UNID_TARIFA'.
        <fs_fcat>-outputlen = 18.
      WHEN 'PRODUTO'.
        <fs_fcat>-outputlen = 50.
      WHEN 'DT_MOV_OV'.
        <fs_fcat>-outputlen = 12.
      WHEN 'STATUS_CTE'.
        CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.
        <fs_fcat>-coltext = <fs_fcat>-coltext = 'Status CTE.'.
        <fs_fcat>-outputlen = 16.
      WHEN 'CELLTAB'.        "lvc_t_styl, "BUG - 89694 - CBRAND
        <fs_fcat>-no_out = abap_true.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar .

  DATA: vl_vbeln         TYPE bapivbeln-vbeln,
        vl_fat           TYPE vbak-vbeln,
        vl_refkey        TYPE j_1bnflin-refkey,
        vl_dstcat        TYPE zlest0030-dstcat,
        vl_emissor_ordem TYPE zlest0194-reme_cod_forn,
        vl_auart_zssf    TYPE auart VALUE 'ZSSF',
        vl_liquido       TYPE zlest0194-valor_prestacao,
        vl_icms          TYPE zsdt0244-tarifa,
        vl_pis           TYPE zlest0194-valor_prestacao,
        vl_cofins        TYPE zlest0194-valor_prestacao,
        vl_ag_frete      TYPE lifnr,
        wa_header        TYPE bapisdhd1,
        wa_item          TYPE bapisditm,
        wa_schedules     TYPE bapischdl,
        wa_partner       TYPE bapiparnr,
        wa_conditions    TYPE bapicond,
        v_msg_aux        TYPE string,
        v_msg_aux1       TYPE char200,
        t_return         TYPE TABLE OF bapiret2,
        t_item           TYPE TABLE OF bapisditm,
        t_partner        TYPE TABLE OF bapiparnr,
        t_schedules      TYPE TABLE OF bapischdl,
        t_conditions     TYPE TABLE OF bapicond,
        t_bapiparex      TYPE TABLE OF bapiparex.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    CLEAR: vl_icms, vl_pis, vl_cofins, vl_liquido, vl_dstcat,
       vl_emissor_ordem, vl_ag_frete, wa_header, wa_item,
       wa_schedules, wa_partner, wa_conditions, t_return,
       t_item, t_partner, t_schedules, t_conditions, t_bapiparex.

    READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>)
                                         WITH KEY chave_xml_cte = <fs_report>-chave_xml_cte.

    CHECK sy-subrc IS INITIAL.
*----------------------------------------------------------------------*
* Geração da Ordem de Venda de Subcontratação
*----------------------------------------------------------------------*
    IF <fs_report>-ov_sub IS INITIAL.

      SELECT SINGLE *
        FROM j_1bbranch
       WHERE stcd1 = @<fs_zlest0194>-pl_cav_prop_cnpj
        INTO @DATA(wa_j_1bbranch).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-014 TEXT-050 "TEXT-050 será sempre fazio, utilizado com parâmetro apenas
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0244
       WHERE id_ctr     = @<fs_zlest0194>-id_ctr
         AND bukrs      = @wa_j_1bbranch-bukrs
        INTO @DATA(wa_zsdt0169_seq).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-015 space"text-016
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0022
       WHERE auart = @vl_auart_zssf
        INTO @DATA(wa_zsdt0022).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-017 TEXT-018
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte = @<fs_report>-chave_xml_cte
        INTO @DATA(wa_zib_cte_dist_ter).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-019 TEXT-020
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      TRY.
          zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
                    EXPORTING
                      i_tipo_agente          = '3'                  "-CS2022000236 - 25.02.2022 - JT - inicio
                      i_bukrs                = wa_j_1bbranch-bukrs  "-CS2022000236 - 25.02.2022 - JT - inicio
                      i_placa                = <fs_zlest0194>-placa_cav
                      i_uf_origem_mercadoria = wa_zib_cte_dist_ter-inicio_uf
                    IMPORTING
                      e_agente_frete         = vl_ag_frete ).
        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.

      IF vl_ag_frete IS INITIAL.
        PERFORM f_mensagens USING   TEXT-033 TEXT-034
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_ag_frete
        IMPORTING
          output = vl_ag_frete.

*----------------------------------------------------------------------
      "Busca Expedidor
*----------------------------------------------------------------------
      IF wa_zib_cte_dist_ter-exped_cpf IS NOT INITIAL.
        SELECT SINGLE *
          FROM lfa1
          WHERE stcd2 = @wa_zib_cte_dist_ter-exped_cpf
           AND stcd3 = @wa_zib_cte_dist_ter-exped_ie "FF #178888 - ins
          INTO @DATA(wa_lfa1).

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-021 TEXT-022
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF wa_zib_cte_dist_ter-exped_cnpj IS NOT INITIAL.

        CLEAR wa_lfa1.
        SELECT SINGLE * FROM lfa1
              INTO wa_lfa1
          WHERE stcd1 = wa_zib_cte_dist_ter-exped_cnpj
   AND stcd3 = wa_zib_cte_dist_ter-exped_ie. "FF #178888 - ins.


        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-021 TEXT-022
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.
      ENDIF.

*----------------------------------------------------------------------
      "Busca Recebedor
*----------------------------------------------------------------------
      IF wa_zib_cte_dist_ter-receb_cpf  IS NOT INITIAL.
        SELECT SINGLE *
        FROM kna1
        WHERE stcd2 = @wa_zib_cte_dist_ter-receb_cpf
        AND stcd3 = @wa_zib_cte_dist_ter-receb_ie "FF #178888 - ins.
        INTO @DATA(wa_kna1).

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-023 TEXT-024
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF wa_zib_cte_dist_ter-receb_cnpj  IS NOT INITIAL.
        CLEAR wa_kna1.
        SELECT SINGLE *
        FROM kna1
        WHERE stcd1 = @wa_zib_cte_dist_ter-receb_cnpj
         AND stcd3 = @wa_zib_cte_dist_ter-receb_ie "FF #178888 - ins.
         INTO @wa_kna1.

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-023 TEXT-024
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0008
       WHERE auart      = 'ZSSF'
         AND vkaus      = 'T'
         AND uf_centro  = @wa_zib_cte_dist_ter-inicio_uf
         AND uf_cliente = @wa_zib_cte_dist_ter-termino_uf
         AND mwsk1      = 'SD'
         AND ownpr      = @space
        INTO @DATA(wa_zsdt0008).

      IF sy-subrc NE 0.

        v_msg_aux1 = |ZSSF/T/{ wa_zib_cte_dist_ter-inicio_uf }/{ wa_zib_cte_dist_ter-termino_uf }/SD.|.

        v_msg_aux = 'Não encontrado parâmetro ZSDT0011!'.
        v_msg_aux = v_msg_aux && | Tp.OV.: ZSSF |.
        v_msg_aux = v_msg_aux && | / Utilização: T|.
        v_msg_aux = v_msg_aux && | / UF Emissor: { wa_zib_cte_dist_ter-inicio_uf }|.
        v_msg_aux = v_msg_aux && | / UF Receptor: { wa_zib_cte_dist_ter-termino_uf }|.
        v_msg_aux = v_msg_aux && | / Tp.IVA: SD|.
        v_msg_aux = v_msg_aux && | / produção: "Desabilitada"!|.

        PERFORM f_mensagens USING  'Não encontrado parâmetro ZSDT0011!'
                                   v_msg_aux1
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        MESSAGE v_msg_aux TYPE 'I'.

        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM j_1btxsdc
       WHERE taxcode =  @wa_zsdt0008-j_1btxsdc
        INTO @DATA(wa_j_1btxsdc).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-027 TEXT-050
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      IF wa_j_1btxsdc-icms = abap_true.
        SELECT SINGLE *
          FROM j_1btxic1
         WHERE land1    = 'BR'
           AND shipfrom	= @wa_zib_cte_dist_ter-inicio_uf
           AND shipto   = @wa_zib_cte_dist_ter-termino_uf
          INTO @DATA(wa_j_1btxic1).

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-028 TEXT-050
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.

        IF sy-subrc IS INITIAL.
          vl_icms = ( <fs_zlest0194>-valor_prestacao * wa_j_1btxic1-rate ) / 100.
        ENDIF.
      ENDIF.

      IF wa_j_1btxsdc-pis = abap_true.
        SELECT SINGLE *
          FROM j_1btxpis
         WHERE country   = 'BR'
           AND gruop     = '72'
           AND value     = @vl_ag_frete+6(4)
           AND validfrom >= @<fs_report>-dt_emissao
           AND validto   <= @<fs_report>-dt_emissao
           INTO @DATA(wa_j_1btxpis).

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-029 TEXT-050
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.

        IF sy-subrc IS INITIAL.
          vl_pis =  ( <fs_zlest0194>-valor_prestacao * wa_j_1btxpis-rate ) / 100.
        ENDIF.
      ENDIF.

      IF wa_j_1btxsdc-cofins = abap_true.
        SELECT SINGLE *
         FROM j_1btxcof
        WHERE country   = 'BR'
          AND gruop     = '71'
          AND value     = @vl_ag_frete+6(4)
          AND validfrom >= @<fs_report>-dt_emissao
          AND validto   <= @<fs_report>-dt_emissao
         INTO @DATA(wa_j_1btxcof).

        IF sy-subrc NE 0.
          PERFORM f_mensagens USING   TEXT-030 TEXT-050
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.

        IF sy-subrc IS INITIAL.
          vl_cofins =  ( <fs_zlest0194>-valor_prestacao * wa_j_1btxcof-rate ) / 100.
        ENDIF.
      ENDIF.

*Valor  liquido da  OV
      vl_liquido  = ( <fs_zlest0194>-valor_prestacao - vl_icms - vl_pis - vl_cofins ).


      IF wa_zib_cte_dist_ter-inicio_uf = wa_zib_cte_dist_ter-termino_uf.
        vl_dstcat = '0'.
      ELSE.
        vl_dstcat = '1'.
      ENDIF.

      vl_emissor_ordem  = <fs_zlest0194>-emit_cod_cliente.

      SELECT SINGLE kdgrp
        FROM knvv
       WHERE kunnr = @vl_emissor_ordem
         AND vkorg = '0001'
         AND vtweg = '10'
         AND spart = '08'
        INTO @DATA(vl_industry).

      IF vl_industry IS INITIAL.

        PERFORM f_mensagens USING   TEXT-012 TEXT-013
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONCATENATE <fs_report>-mensagem '-' vl_emissor_ordem
        INTO <fs_report>-mensagem SEPARATED BY space.
        CONTINUE.
      ENDIF.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_industry
        IMPORTING
          output = vl_industry.

      SELECT SINGLE *
        FROM zlest0030
       WHERE direct     = '2'
         AND dstcat     = @vl_dstcat
         AND industry   = @vl_industry
         AND tpparceiro = '0'
        INTO @DATA(wa_zlest0030_cfop).

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-031 TEXT-032
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

*-CS2022000236 - 06.03.2022 - JT - inicio
      SELECT SINGLE regio
        INTO @DATA(l_regio)
        FROM lfa1
       WHERE lifnr = @vl_ag_frete.

      IF sy-subrc NE 0.
        PERFORM f_mensagens USING   TEXT-045 TEXT-022
                         CHANGING: <fs_report>-icon
                                   <fs_report>-mensagem.
        CONTINUE.
      ENDIF.

      IF l_regio <> wa_zib_cte_dist_ter-inicio_uf.

        "13.11.2023 AJUSTE CFOP ZLES0181 PSA         "wa_zlest0030_cfop-cfop = wa_zlest0030_cfop-cfop_uf_emit_dif_prest.
        SELECT SINGLE cfop_uf_emit_dif_prest
         FROM zlest0030
        WHERE direct     = '2'
          AND dstcat     = @vl_dstcat
          AND industry   = @vl_industry
          AND tpparceiro = '0'
          AND tdlnr = @vl_ag_frete
         INTO @wa_zlest0030_cfop-cfop.

        IF wa_zlest0030_cfop-cfop IS INITIAL.


          PERFORM f_mensagens USING   TEXT-050 TEXT-051
                           CHANGING: <fs_report>-icon
                                     <fs_report>-mensagem.
          CONTINUE.
        ENDIF.

      ELSE. "13.11.2023 AJUSTE CFOP ZLES0181 PSA

        SELECT SINGLE cfop
          FROM zlest0030
         WHERE direct     = '2'
           AND dstcat     = @vl_dstcat
           AND industry   = @vl_industry
           AND tpparceiro = '0'
           AND tdlnr = @vl_ag_frete
          INTO @wa_zlest0030_cfop-cfop.

      ENDIF.
*-CS2022000236 - 06.03.2022 - JT - fim

* Dados Cabeçalho
      wa_header-sales_org  =  '0001'.               "(organização de venda)
      wa_header-doc_type   = 'ZSSF'.                "(tipo de ordem)
      wa_header-distr_chan = '10'.                  "(canal)
      wa_header-division   = '08'.                   "(setor de atividade)
      wa_header-curr_iso   = 'BRL'.                 "(código moeda)
      wa_header-currency   = 'BRL'.                 "(código moeda do documento)
      wa_header-created_by = sy-uname.              "(usuário)
      wa_header-pmnttrms   = wa_zsdt0169_seq-zterm.    "(condição de pagamento)
      wa_header-purch_no_c = <fs_zlest0194>-placa_cav.   "(Nr. do pedido do cliente)
      wa_header-price_date = <fs_zlest0194>-dt_descarga. "(Data de determinação do preço)
      wa_header-exchg_rate = '1.00000'.

      wa_item-itm_number  = '000010'.
      wa_item-target_qty  = '1'.
      wa_item-plant       = vl_ag_frete+6(4).
*---> 12/05/2023 - Migração S4 - DG
* IT_GO_ITEM-MATERIAL      = IT_LQUAY-MATNR.
      DATA(v_len) = strlen( wa_zsdt0022-matnr ).
      IF v_len > 18.
        wa_item-material_long = wa_zsdt0022-matnr.
      ELSE.
        wa_item-material      = wa_zsdt0022-matnr.
      ENDIF.
*<--- 12/05/2023 - Migração S4 - DG

      wa_item-material    = wa_zsdt0022-matnr.
      wa_item-gross_wght  = <fs_zlest0194>-qt_carga_cte.
      wa_item-net_weight  = <fs_zlest0194>-qt_carga_cte.
      wa_item-untof_wght  = <fs_zlest0194>-meins.
      wa_item-sd_taxcode  = wa_zsdt0008-j_1btxsdc.
      wa_item-taxlawiss   = space.
      wa_item-dlvschduse  = 'T'.
      wa_item-cfop_long   = wa_zlest0030_cfop-cfop.
      APPEND wa_item TO t_item.

      wa_schedules-itm_number = '000010'.
      wa_schedules-req_qty    = '1'.
      APPEND wa_schedules TO t_schedules.

* Emissor da Ordem
      CLEAR: wa_partner.
      wa_partner-partn_role = 'AG'.
      wa_partner-partn_numb = vl_emissor_ordem.
      APPEND wa_partner TO t_partner.

* Local de entrega
      CLEAR: wa_partner.
      wa_partner-partn_role = 'LR'.
      wa_partner-partn_numb = <fs_zlest0194>-dest_cod_cliente.
      APPEND wa_partner TO t_partner.

* Ponto de coleta
      CLEAR: wa_partner.
      wa_partner-partn_role = 'PC'.
      wa_partner-partn_numb = <fs_zlest0194>-reme_cod_forn.
      APPEND wa_partner TO t_partner.

* Remetente da Mercadoria
      CLEAR: wa_partner.
      wa_partner-partn_role = 'RM'.
      wa_partner-partn_numb = <fs_zlest0194>-reme_cod_forn.
      APPEND wa_partner TO t_partner.

* Remetente
      CLEAR: wa_partner.
      wa_partner-partn_role = 'T1'.
      wa_partner-partn_numb = <fs_zlest0194>-reme_cod_forn.
      APPEND wa_partner TO t_partner.

* Expedidor
      CLEAR: wa_partner.
      wa_partner-partn_role = 'T2'.
*      wa_partner-partn_numb = wa_lfa1-lifnr. "FF #178888 - del
      wa_partner-partn_numb = <fs_zlest0194>-exped_cod_forn."FF #178888 - ins
      APPEND wa_partner TO t_partner.

* Recebedor de Mercadoria
      CLEAR: wa_partner.
      wa_partner-partn_role = 'T3'.
*      wa_partner-partn_numb = wa_kna1-kunnr. "FF #178888 - del
      wa_partner-partn_numb = <fs_zlest0194>-receb_cod_cliente."FF #178888 - ins
      APPEND wa_partner TO t_partner.


* Destinatário da Carga
      CLEAR: wa_partner.
      wa_partner-partn_role = 'T4'.
      wa_partner-partn_numb = <fs_zlest0194>-dest_cod_cliente.
      APPEND wa_partner TO t_partner.

*** US #182910 - MMSILVA - 18.06.2025 - Ini ***
* Recebedor da Mercadoria
      CLEAR: wa_partner.
      wa_partner-partn_role = 'WE'.
      wa_partner-partn_numb = <fs_zlest0194>-dest_cod_cliente.
      APPEND wa_partner TO t_partner.
*** US #182910 - MMSILVA - 18.06.2025 - Fim ***

      wa_conditions-itm_number 	 = '000010'.
      wa_conditions-cond_type  	 = 'PR00'.
      wa_conditions-cond_value   = vl_liquido.
      wa_conditions-currency   	 = 'BRL'.
      APPEND wa_conditions TO t_conditions.

      CLEAR: vl_vbeln.
      CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          order_header_in     = wa_header
        IMPORTING
          salesdocument       = vl_vbeln
        TABLES
          return              = t_return
          order_items_in      = t_item
          order_partners      = t_partner
          order_schedules_in  = t_schedules
          order_conditions_in = t_conditions
          extensionin         = t_bapiparex.

      IF vl_vbeln IS NOT INITIAL.

        <fs_report>-icon      = icon_green_light.
        <fs_report>-mensagem  = TEXT-035.
        <fs_report>-ov_sub    = vl_vbeln.
        <fs_report>-dt_mov_ov = sy-datum.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ELSE.
        CLEAR: t_outtab_mess.
        LOOP AT t_return ASSIGNING FIELD-SYMBOL(<fs_return>) WHERE type EQ 'E'.

          PERFORM f_mensagens_bapi USING  <fs_return>
                                CHANGING: <fs_report>-icon
                                          <fs_report>-mensagem.


        ENDLOOP.

      ENDIF.
*** BUG - 89694 - Inicio - CBRAND
      IF t_outtab_mess IS INITIAL.
        REFRESH: lt_celltab.
        CLEAR: lt_celltab.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_hotspot_no.
        ls_celltab-fieldname = 'MENSAGEM'.
        APPEND ls_celltab TO lt_celltab.
        CLEAR: <fs_report>-celltab.
        INSERT LINES OF lt_celltab INTO TABLE <fs_report>-celltab.
      ELSE.
        REFRESH: lt_celltab.
        CLEAR: lt_celltab.
        <fs_report>-icon  = icon_red_light.
        <fs_report>-mensagem  = 'Houve um erro ao gerar O.V. Clique aqui para visualizar'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_hotspot.
        ls_celltab-fieldname = 'MENSAGEM'.
        APPEND ls_celltab TO lt_celltab.
        CLEAR: <fs_report>-celltab.
        INSERT LINES OF lt_celltab INTO TABLE <fs_report>-celltab.

      ENDIF.
*** BUG - 89694 - Fim - CBRAND
    ELSE.

      vl_vbeln = <fs_report>-ov_sub.

    ENDIF.


*----------------------------------------------------------------------*
* Geração da Fatura de Subcontratação
*----------------------------------------------------------------------*
    IF vl_vbeln IS NOT INITIAL AND <fs_report>-fat_sub IS INITIAL.

      DO 5 TIMES.
        SELECT SINGLE *
          FROM vbak INTO @DATA(lwa_vbak)
         WHERE vbeln = @vl_vbeln.

        IF sy-subrc EQ 0.
          EXIT.
        ELSE.
          WAIT UP TO 2 SECONDS.
        ENDIF.
      ENDDO.

      CLEAR: t_return, vl_fat, vl_refkey.
      CALL FUNCTION 'ZSD_FAT_ZTRO'
        EXPORTING
          p_sales  = vl_vbeln
        IMPORTING
          p_fat    = vl_fat
          t_return = t_return.

      IF vl_fat IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vl_fat
          IMPORTING
            output = vl_fat.

        <fs_report>-fat_sub = vl_fat.

        DO 5 TIMES.
          SELECT SINGLE *
            FROM j_1bnflin
           WHERE reftyp = 'BI'
             AND refkey = @vl_fat
            INTO @DATA(wa_j_1bnflin).

          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO 2 SECONDS.
          ENDIF.
        ENDDO.

        IF sy-subrc IS INITIAL.
          <fs_report>-docnum_sub = wa_j_1bnflin-docnum.
        ENDIF.


      ELSE.

        "Fatura criada mais sem documento Eletrônico
        READ TABLE t_return INTO DATA(w_return) WITH KEY type   = 'S'
                                                         id     = 'VF'
                                                         number = '050'.
        IF sy-subrc IS INITIAL.

          <fs_report>-fat_sub = w_return-message_v1.

        ELSE.
          CLEAR: t_outtab_mess.

          DELETE t_return WHERE id = 'V4'.
          LOOP AT t_return ASSIGNING <fs_return>.

            PERFORM f_mensagens_bapi USING     <fs_return>
                                     CHANGING: <fs_report>-icon
                                               <fs_report>-mensagem.

          ENDLOOP.
        ENDIF.

*** BUG - 89694 - Inicio - CBRAND
        IF t_outtab_mess IS INITIAL.
          REFRESH: lt_celltab.
          CLEAR: lt_celltab.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_hotspot_no.
          ls_celltab-fieldname = 'MENSAGEM'.
          APPEND ls_celltab TO lt_celltab.
          CLEAR: <fs_report>-celltab.
          INSERT LINES OF lt_celltab INTO TABLE <fs_report>-celltab.
        ELSE.
          REFRESH: lt_celltab.
          CLEAR: lt_celltab.
          <fs_report>-icon     = icon_red_light.
          <fs_report>-mensagem = 'Houve um erro ao gerar Fatura. Clique aqui para visualizar'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_hotspot.
          ls_celltab-fieldname = 'MENSAGEM'.
          APPEND ls_celltab TO lt_celltab.

          CLEAR: <fs_report>-celltab.
          INSERT LINES OF lt_celltab INTO TABLE <fs_report>-celltab.
        ENDIF.
*** BUG - 89694 - Fim - CBRAND

      ENDIF.

    ENDIF.

*----------------------------------------------------------------------*
* Geração da Fatura de Subcontratação
*----------------------------------------------------------------------*

  ENDLOOP.

*  IF t_report[] IS NOT INITIAL.
*
*    DATA(t_report_aux) = t_report.
*    DELETE t_report_aux WHERE docnum_sub IS INITIAL.
*
*    SELECT docnum, cancel, code, action_requ FROM j_1bnfe_active
*      INTO TABLE @DATA(t_active)
*      FOR ALL ENTRIES IN @t_report
*      WHERE docnum = @t_report-docnum_sub.
*
*  ENDIF.
*
*  LOOP AT t_report ASSIGNING <fs_report>.
*
*    READ TABLE t_active INTO DATA(w_active) WITH KEY docnum = <fs_report>-docnum_sub.
*
*    CHECK sy-subrc IS INITIAL.
*
** COMPLETED
************************************************************************
*    IF w_active-action_requ CA 'C'.
*
*      <fs_report>-status_cte = icon_complete.
*
*      IF w_active-cancel IS INITIAL.
*        <fs_report>-icon = icon_green_light.
*      ENDIF.
*
*      IF w_active-cancel IS NOT INITIAL.
*        <fs_report>-icon = icon_red_light.
*      ENDIF.
*
** ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
************************************************************************
*    ELSEIF w_active-action_requ CA '145'.
*      <fs_report>-status_cte = icon_alert.
** IN PROCESS - USER ACTION REQUIRED
************************************************************************
*    ELSEIF w_active-action_requ CA '2367890'.
*      <fs_report>-status_cte = icon_warning.
** IN PROCESS - NO USER ACTION REQUIRED
************************************************************************
*    ELSE.
*      <fs_report>-status_cte = icon_activity.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_014  text
*      -->P_TEXT_050  text
*      <--P_<FS_REPORT>_ICON  text
*      <--P_<FS_REPORT>_MENSAGEM  text
*----------------------------------------------------------------------*
FORM f_mensagens  USING  p_text1   TYPE char200
                        p_text2   TYPE char200
              CHANGING  p_status  TYPE char4
                        p_msg     TYPE char200.

  CONCATENATE p_text1 p_text2 INTO p_msg SEPARATED BY space.
  p_status = icon_red_light.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGENS_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_RETURN>  text
*      <--P_<FS_REPORT>_ICON  text
*      <--P_<FS_REPORT>_MENSAGEM  text
*----------------------------------------------------------------------*
FORM f_mensagens_bapi  USING    p_return TYPE bapiret2
                       CHANGING p_status TYPE char4
                                p_msg    TYPE char200.


  p_msg = p_return-message.
  p_status = icon_red_light.

*** BUG - 89694 - Inicio - CBRAND
*  wa_outtab_mess-status  = p_status.
*  wa_outtab_mess-msg     = p_return-message.

  wa_outtab_mess-status  = p_status.
  wa_outtab_mess-msg     = p_msg.

  APPEND wa_outtab_mess TO t_outtab_mess.
  CLEAR: wa_outtab_mess.

*** BUG - 89694 - Fim - CBRAND
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.

  CASE v_okcode.
    WHEN 'BACK' OR 'CANC'.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ZLEST0194
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_zlest0194 .

  DATA: vl_awkey  TYPE awkey,
        vl_refkey TYPE j_1bnflin-refkey.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.

    READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>)
                                         WITH KEY chave_xml_cte = <fs_report>-chave_xml_cte.
    IF sy-subrc IS INITIAL.


*----------------------------------------------------------------------*
* Verifica preenchimento da docnum de  subcontratação
*----------------------------------------------------------------------*
      IF <fs_report>-docnum_sub IS NOT INITIAL.

        SELECT SINGLE *
          FROM j_1bnfdoc
         WHERE docnum = @<fs_report>-docnum_sub
          INTO @DATA(wa_j_1bnfdoc).

        IF wa_j_1bnfdoc-cancel IS NOT INITIAL.

          CLEAR: <fs_report>-docnum_sub, <fs_zlest0194>-docnum_sub.

        ELSE.

          <fs_zlest0194>-nr_cte_sub     = wa_j_1bnfdoc-nfenum.
          <fs_zlest0194>-serie_cte_sub  = wa_j_1bnfdoc-series.

          SELECT SINGLE *
            FROM j_1bnfe_active
           WHERE docnum = @<fs_report>-docnum_sub
            INTO @DATA(wa_j_1bnfe_active).

          IF sy-subrc IS INITIAL.

            CONCATENATE wa_j_1bnfe_active-regio  wa_j_1bnfe_active-nfyear  wa_j_1bnfe_active-nfmonth
                        wa_j_1bnfe_active-stcd1  wa_j_1bnfe_active-model   wa_j_1bnfe_active-serie
                        wa_j_1bnfe_active-nfnum9 wa_j_1bnfe_active-docnum9 wa_j_1bnfe_active-cdv
            INTO <fs_zlest0194>-chave_cte_sub.
            CONDENSE <fs_zlest0194>-chave_cte_sub NO-GAPS.

          ENDIF.

        ENDIF.

      ENDIF.


* COMPLETED
***********************************************************************
      IF wa_j_1bnfe_active-action_requ CA 'C'.

        <fs_report>-status_cte = icon_complete.

        IF wa_j_1bnfe_active-cancel IS INITIAL.
          <fs_report>-icon = icon_green_light.
        ENDIF.

        IF wa_j_1bnfe_active-cancel IS NOT INITIAL.
          <fs_report>-icon = icon_red_light.
        ENDIF.

* ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
***********************************************************************
      ELSEIF wa_j_1bnfe_active-action_requ CA '145'.
        <fs_report>-status_cte = icon_alert.
* IN PROCESS - USER ACTION REQUIRED
***********************************************************************
      ELSEIF wa_j_1bnfe_active-action_requ CA '2367890'.
        <fs_report>-status_cte = icon_warning.
* IN PROCESS - NO USER ACTION REQUIRED
***********************************************************************
      ELSE.
        CLEAR <fs_report>-status_cte.
      ENDIF.
*----------------------------------------------------------------------*
* Verifica preenchimento da ordem de venda
*----------------------------------------------------------------------*
      IF <fs_report>-docnum_sub IS INITIAL.
        CLEAR: <fs_zlest0194>-ov_sub, <fs_zlest0194>-bukrs_ov, <fs_zlest0194>-branch_ov, <fs_zlest0194>-dt_mov_ov, <fs_zlest0194>-kunnr_ov, <fs_zlest0194>-kunnr_cnpj,
               <fs_zlest0194>-fat_sub, <fs_zlest0194>-nr_cte_sub, <fs_zlest0194>-serie_cte_sub, <fs_zlest0194>-chave_cte_sub,
               <fs_zlest0194>-belnr_vf, <fs_zlest0194>-gjahr_vf,
               <fs_report>-docnum_sub, <fs_report>-fat_sub, <fs_report>-ov_sub, <fs_report>-dt_mov_ov.
      ENDIF.

      IF <fs_report>-ov_sub IS NOT INITIAL.

        SELECT SINGLE *
          FROM vbak
         WHERE vbeln = @<fs_report>-ov_sub
          INTO @DATA(wa_vbak).

        IF sy-subrc IS INITIAL.

          <fs_zlest0194>-bukrs_ov  = wa_vbak-bukrs_vf.
          <fs_zlest0194>-kunnr_ov  = wa_vbak-kunnr.
          <fs_zlest0194>-dt_mov_ov = wa_vbak-audat.

          SELECT SINGLE *
            FROM vbap
           WHERE vbeln = @wa_vbak-vbeln
            INTO @DATA(wa_vbap).

          IF sy-subrc IS INITIAL.
            <fs_zlest0194>-branch_ov = wa_vbap-werks.
          ENDIF.

          SELECT SINGLE *
            FROM kna1
           WHERE kunnr = @wa_vbak-kunnr
             AND land1 = 'BR'
            INTO @DATA(wa_kna1).

          IF sy-subrc IS INITIAL.
            <fs_zlest0194>-kunnr_cnpj = wa_kna1-stcd1.
          ENDIF.
        ENDIF.

      ENDIF.

      IF <fs_zlest0194>-bukrs_ov  IS NOT INITIAL AND
         <fs_zlest0194>-dt_mov_ov IS NOT INITIAL AND
         <fs_zlest0194>-fat_sub   IS NOT INITIAL.

        CLEAR: vl_awkey.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_zlest0194>-fat_sub
          IMPORTING
            output = vl_awkey.

        SELECT SINGLE *
          FROM bkpf
         WHERE bukrs = @<fs_zlest0194>-bukrs_ov
           AND gjahr = @<fs_zlest0194>-dt_mov_ov(4)
           AND awkey = @vl_awkey+10(10)
          INTO @DATA(wa_bkpf).

        IF sy-subrc IS INITIAL.
          <fs_zlest0194>-belnr_vf = wa_bkpf-belnr.
          <fs_zlest0194>-gjahr_vf = wa_bkpf-gjahr.
        ENDIF.
      ENDIF.

      <fs_zlest0194>-ov_sub     = <fs_report>-ov_sub.
      <fs_zlest0194>-dt_mov_ov  = <fs_report>-dt_mov_ov.
      <fs_zlest0194>-fat_sub    = <fs_report>-fat_sub.
      <fs_zlest0194>-docnum_sub = <fs_report>-docnum_sub.

    ENDIF.

  ENDLOOP.

  LOOP AT t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194_aux>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_zlest0194_aux>-fat_sub
      IMPORTING
        output = <fs_zlest0194_aux>-fat_sub.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_zlest0194_aux>-ov_sub
      IMPORTING
        output = <fs_zlest0194_aux>-ov_sub.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_zlest0194_aux>-docnum_sub
      IMPORTING
        output = <fs_zlest0194_aux>-docnum_sub.
  ENDLOOP.

  MODIFY zlest0194 FROM TABLE t_zlest0194.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_alv .

  DATA: l_stable       TYPE lvc_s_stbl, "Estrutura para refresh do ALV
        l_soft_refresh TYPE c. "Campo para refresh do ALV


*...Fixa posição da linha no ALV
  l_stable-row = abap_true.
  l_stable-col = abap_true.
  l_soft_refresh = abap_true.

*...Atualiza o ALV
  CALL METHOD lcl_alv->refresh_table_display
    EXPORTING
      is_stable      = l_stable
      i_soft_refresh = l_soft_refresh
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar .

  DATA: vl_cancelled  TYPE bapivbrkout-cancelled,
        vl_doc_number TYPE j_1bdocnum,
        vl_nr_fat     TYPE c LENGTH 10,
        t_success     TYPE TABLE OF bapivbrksuccess,
        t_return      TYPE TABLE OF bapireturn1.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

*&---------------------------------------------------------------------*
    " Verifica cancelamento de notas númeradas
*&---------------------------------------------------------------------*
    "Cabeçalho de Nota Fiscal
    SELECT SINGLE *
      FROM j_1bnfdoc
     WHERE docnum = @<fs_report>-docnum_sub
      INTO @DATA(wa_j_1bnfdoc).

    IF (  sy-subrc IS INITIAL AND wa_j_1bnfdoc-docnum IS NOT INITIAL AND wa_j_1bnfdoc-nfenum IS NOT INITIAL ).


      SELECT SINGLE *
        FROM j_1bnfe_active
       WHERE docnum = @<fs_report>-docnum_sub
        INTO @DATA(wa_j_1bnfe_active_sub).

      CHECK sy-subrc EQ 0.

      DATA(_solicita_estorno_zcte) = abap_true.
      IF wa_j_1bnfe_active_sub-docsta  EQ '1' AND
         wa_j_1bnfe_active_sub-scssta  EQ '2' AND
         wa_j_1bnfe_active_sub-action_requ IS NOT INITIAL .
        _solicita_estorno_zcte = abap_false.
      ENDIF.

      IF wa_j_1bnfe_active_sub-docsta  EQ '2' AND
         wa_j_1bnfe_active_sub-scssta  EQ '4' AND
         wa_j_1bnfe_active_sub-action_requ IS NOT INITIAL .
        _solicita_estorno_zcte = abap_false.
      ENDIF.

      IF wa_j_1bnfe_active_sub-docsta  EQ '3' AND
         wa_j_1bnfe_active_sub-scssta  EQ '0' AND
         wa_j_1bnfe_active_sub-action_requ IS NOT INITIAL .
        _solicita_estorno_zcte = abap_false.
      ENDIF.

      "FF #178888 - inicio
      IF wa_j_1bnfe_active_sub-docsta  EQ '1' AND
         wa_j_1bnfe_active_sub-scssta  EQ '0' AND
         wa_j_1bnfe_active_sub-action_requ IS NOT INITIAL .
        _solicita_estorno_zcte = abap_false.
      ENDIF.
      "FF #178888 - fim

      IF _solicita_estorno_zcte EQ abap_true.
        <fs_report>-icon       = icon_red_light.
        <fs_report>-mensagem   = TEXT-041.
        CONTINUE.
      ENDIF.

    ENDIF.

    IF  wa_j_1bnfdoc-cancel IS NOT INITIAL.
      <fs_report>-docnum_sub = space.
    ENDIF.

*&---------------------------------------------------------------------*
    "Efetua o cancelamento da fatura
*&---------------------------------------------------------------------*
    CLEAR: vl_cancelled, vl_doc_number, t_success, t_return, vl_nr_fat.

    IF <fs_report>-fat_sub IS NOT INITIAL.

      vl_nr_fat = <fs_report>-fat_sub.
      UNPACK vl_nr_fat TO vl_nr_fat.

*>--- S4 MIGRATION 07/07/2023 - LM
*      CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*        EXPORTING
*          billingdoc_number       = vl_nr_fat
*        IMPORTING
*          billingdoc_is_cancelled = vl_cancelled.

      DATA: vl_bill_doc              TYPE bapivbrksuccess-bill_doc,
            ls_billingdocumentdetail TYPE bapivbrkout.

      vl_bill_doc = CONV #( vl_nr_fat ).

      CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
        EXPORTING
          billingdocument       = vl_bill_doc
        IMPORTING
          billingdocumentdetail = ls_billingdocumentdetail.

      vl_cancelled = ls_billingdocumentdetail-cancelled.
*<--- S4 MIGRATION 07/07/2023 - LM

*&---------------------------------------------------------------------*
      "Se a fatura foi estornada com sucesso efetiva a ação
*&---------------------------------------------------------------------*
      IF vl_cancelled IS NOT INITIAL.

        <fs_report>-fat_sub = space.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ENDIF.

    ENDIF.

*&---------------------------------------------------------------------*
    "Se a fatura não foi cancelada e encontrou o número
    "efetua o cancelamento via Z
*&---------------------------------------------------------------------*
    IF ( vl_cancelled IS INITIAL ) AND vl_nr_fat IS NOT INITIAL.

      "Cancela fatura
      CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
        EXPORTING
          billingdocument = vl_nr_fat "<fs_report>-fat_sub
        TABLES
          return          = t_return
          success         = t_success.

      IF sy-subrc EQ 0.

        <fs_report>-fat_sub     = space.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

*&---------------------------------------------------------------------*
        " Verifica cancelamento de notas númeradas
*&---------------------------------------------------------------------*
        IF wa_j_1bnfdoc-docnum IS NOT INITIAL AND wa_j_1bnfdoc-nfenum IS INITIAL.
          CLEAR <fs_report>-docnum_sub.
        ENDIF.

      ENDIF.

    ENDIF.

*&---------------------------------------------------------------------*
    " Verifica cancelamento de notas não númeradas
*&---------------------------------------------------------------------*
    IF <fs_report>-docnum_sub IS INITIAL.
      <fs_report>-ov_sub     = space.
    ELSE.
      <fs_report>-icon       = icon_red_light.
      <fs_report>-mensagem   = TEXT-041.
      CONTINUE.
    ENDIF.

    IF <fs_report>-ov_sub  IS INITIAL.
      <fs_report>-dt_mov_ov = space.
    ENDIF.

    IF <fs_report>-docnum_sub = '0000000000'
      AND <fs_report>-fat_sub  IS INITIAL
        AND <fs_report>-ov_sub  IS INITIAL.

      CLEAR: <fs_report>-status_cte.
      <fs_report>-icon       = icon_green_light.
      <fs_report>-mensagem   = TEXT-036.

    ENDIF.

  ENDLOOP.

*  IF t_report[] IS NOT INITIAL.
*
*    DATA(t_report_aux) = t_report.
*    DELETE t_report_aux WHERE docnum_sub IS INITIAL.
*
**&---------------------------------------------------------------------*
*    " Verifica status atual
**&---------------------------------------------------------------------*
*    SELECT docnum, cancel, code, action_requ FROM j_1bnfe_active
*      INTO TABLE @DATA(t_active)
*      FOR ALL ENTRIES IN @t_report
*      WHERE docnum = @t_report-docnum_sub.
*
*  ENDIF.
*
*  LOOP AT t_report ASSIGNING <fs_report>.
*
*    READ TABLE t_active INTO DATA(w_active) WITH KEY docnum = <fs_report>-docnum_sub.
*
*    CHECK sy-subrc IS INITIAL.
*
** COMPLETED
************************************************************************
*    IF w_active-action_requ CA 'C'.
*
*
*      <fs_report>-status_cte = icon_complete.
*
*      IF w_active-cancel IS INITIAL.
*        <fs_report>-icon = icon_green_light.
*      ENDIF.
*
*      IF w_active-cancel IS NOT INITIAL.
*        <fs_report>-icon = icon_red_light
  .
*      ENDIF.
*
** ALERT: NF-e ERROR/INCONSISTENCY - USER ACTION REQUIRED
************************************************************************
*    ELSEIF w_active-action_requ CA '145'.
*      <fs_report>-status_cte = icon_alert.
** IN PROCESS - USER ACTION REQUIRED
************************************************************************
*    ELSEIF w_active-action_requ CA '2367890'.
*      <fs_report>-status_cte = icon_warning.
** IN PROCESS - NO USER ACTION REQUIRED
************************************************************************
*    ELSE.
*      <fs_report>-status_cte = icon_activity.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_tabelas .


  DATA: vl_awkey  TYPE awkey,
        vl_refkey TYPE j_1bnflin-refkey.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_limpa_tabelas.

  PERFORM f_seleciona_zib_cte_dist_n55.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report INTO DATA(wa_report) INDEX w_row_no-row_id.

    IF wa_report-ov_sub IS NOT INITIAL AND wa_report-docnum_sub IS NOT INITIAL.

      READ TABLE t_zlest0194 INTO DATA(wa_zlest0194)
                                           WITH KEY chave_xml_cte = wa_report-chave_xml_cte.

      IF sy-subrc IS INITIAL.

        PERFORM f_carrega_zcte_trans USING: wa_zlest0194
                                            wa_report.

        PERFORM f_carrega_zcte_parceiros USING: wa_zlest0194
                                                wa_report.

        PERFORM f_carrega_zcte_motorista USING: wa_zlest0194
                                                wa_report.

        PERFORM f_carrega_zcte_info_nota USING: wa_zlest0194
                                                wa_report.

        PERFORM f_carrega_zcte_ciot USING: wa_zlest0194
                                           wa_report.

        PERFORM f_carrega_zcte_identifica USING: wa_zlest0194
                                                 wa_report.

        PERFORM f_carrega_zcte_doc_ant USING: wa_zlest0194
                                               wa_report.

        PERFORM f_carrega_zcte_obs_gerais USING: wa_zlest0194
                                                 wa_report.

        PERFORM f_carrega_zib_cte_dist_ter USING: wa_zlest0194
                                                  wa_report.

      ENDIF.

      COMMIT WORK.

    ENDIF.

    CLEAR: wa_zlest0194, wa_report.

*    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.
*
*    IF <fs_report>-ov_sub IS NOT INITIAL AND <fs_report>-docnum_sub IS NOT INITIAL.
*
*      READ TABLE t_zlest0194 ASSIGNING FIELD-SYMBOL(<fs_zlest0194>)
*                                           WITH KEY chave_xml_cte = <fs_report>-chave_xml_cte.
*
*      PERFORM f_carrega_zcte_trans USING: <fs_zlest0194>
*                                          <fs_report>.
*
*      PERFORM f_carrega_zcte_parceiros USING: <fs_zlest0194>
*                                              <fs_report>.
*
*      PERFORM f_carrega_zcte_motorista USING: <fs_zlest0194>
*                                              <fs_report>.
*
*      PERFORM f_carrega_zcte_info_nota USING: <fs_zlest0194>
*                                              <fs_report>.
*
*      PERFORM f_carrega_zcte_ciot USING: <fs_zlest0194>
*                                         <fs_report>.
*
*      PERFORM f_carrega_zcte_identifica USING: <fs_zlest0194>
*                                               <fs_report>.
*
*      PERFORM f_carrega_zcte_doc_ant USING: <fs_zlest0194>
*                                             <fs_report>.
*
*      PERFORM f_carrega_zcte_obs_gerais USING: <fs_zlest0194>
*                                               <fs_report>.
*
*      PERFORM f_carrega_zib_cte_dist_ter USING: <fs_zlest0194>
*                                                <fs_report>.
*
*    ENDIF.

  ENDLOOP.

  REFRESH: t_zcte_trans, t_zcte_parceiros, t_zcte_motorista,
           t_zcte_info_nota, t_zcte_ciot, t_zcte_identifica,
           t_zcte_doc_ant, t_zcte_obs_gerais, t_zib_cte_dist_ter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_trans   USING: p_zlest0194 TYPE zlest0194
                                   p_report    TYPE y_report. "zsds025.

  DATA: wa_zlest0002 TYPE zlest0002,
        wa_lfa1      TYPE lfa1.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_trans
    INTO @DATA(l_cte_trans)
       WHERE docnum = @p_report-docnum_sub
         AND  pc_veiculo = @p_zlest0194-placa_cav.

  CHECK sy-subrc IS NOT INITIAL.

  CLEAR: wa_lfa1.
  SELECT SINGLE *
    FROM lfa1
    INTO wa_lfa1
   WHERE lifnr = p_zlest0194-pl_cav_prop_cod.

  IF p_zlest0194-placa_cav IS NOT INITIAL.

    CLEAR: wa_zlest0002.
    SELECT SINGLE *
      FROM zlest0002
      INTO wa_zlest0002
     WHERE pc_veiculo = p_zlest0194-placa_cav.

    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_zcte_trans  ASSIGNING FIELD-SYMBOL(<fs_zcte_trans_1>).
      <fs_zcte_trans_1>-docnum         = p_report-docnum_sub.
      <fs_zcte_trans_1>-pc_veiculo     = p_zlest0194-placa_cav.
      <fs_zcte_trans_1>-qtd_eixo       = wa_zlest0002-qt_eixo.
      <fs_zcte_trans_1>-proprietario   = p_zlest0194-pl_cav_prop_cod.
      <fs_zcte_trans_1>-country        = wa_zlest0002-country.
      <fs_zcte_trans_1>-taxjurcode     = wa_zlest0002-taxjurcode.
      <fs_zcte_trans_1>-spras          = wa_zlest0002-spras.
      <fs_zcte_trans_1>-cd_cidade      = wa_zlest0002-cd_cidade.
      <fs_zcte_trans_1>-cd_uf          = wa_zlest0002-cd_uf.
      <fs_zcte_trans_1>-agregado       = wa_zlest0002-agregado.
      <fs_zcte_trans_1>-cd_renavam     = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_1>-tp_veiculo     = p_zlest0194-pl_cav_tp_veic.
      <fs_zcte_trans_1>-tp_rodado      = wa_zlest0002-tp_rodado.
      <fs_zcte_trans_1>-tp_carroceria2 = wa_zlest0002-tp_carroceria2.
      <fs_zcte_trans_1>-tara           = wa_zlest0002-tara.
      <fs_zcte_trans_1>-cap_kg         = wa_zlest0002-cap_kg.
      <fs_zcte_trans_1>-cap_m3         = wa_zlest0002-cap_m3.
      <fs_zcte_trans_1>-prop_cnpj      = p_zlest0194-pl_cav_prop_cnpj.
      <fs_zcte_trans_1>-prop_nome      = wa_lfa1-name1.
      <fs_zcte_trans_1>-prop_rntrc     = wa_lfa1-bahns.
      <fs_zcte_trans_1>-prop_uf        = wa_lfa1-regio.
      <fs_zcte_trans_1>-prop_ie        = wa_lfa1-stcd3.
      <fs_zcte_trans_1>-telefone       = wa_lfa1-telf1.
    ENDIF.
  ENDIF.

  IF p_zlest0194-placa_car1 IS NOT INITIAL.
    CLEAR: wa_zlest0002.
    SELECT SINGLE *
      FROM zlest0002
      INTO wa_zlest0002
     WHERE pc_veiculo = p_zlest0194-placa_car1.

    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_zcte_trans  ASSIGNING FIELD-SYMBOL(<fs_zcte_trans_2>).
      <fs_zcte_trans_2>-docnum         = p_report-docnum_sub.
      <fs_zcte_trans_2>-pc_veiculo     = p_zlest0194-placa_car1.
      <fs_zcte_trans_2>-qtd_eixo       = wa_zlest0002-qt_eixo.
      <fs_zcte_trans_2>-proprietario   = p_zlest0194-pl_cav_prop_cod.
      <fs_zcte_trans_2>-country        = wa_zlest0002-country.
      <fs_zcte_trans_2>-taxjurcode     = wa_zlest0002-taxjurcode.
      <fs_zcte_trans_2>-spras          = wa_zlest0002-spras.
      <fs_zcte_trans_2>-cd_cidade      = wa_zlest0002-cd_cidade.
      <fs_zcte_trans_2>-cd_uf          = wa_zlest0002-cd_uf.
      <fs_zcte_trans_2>-agregado       = wa_zlest0002-agregado.
      <fs_zcte_trans_2>-cd_renavam     = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_2>-tp_veiculo     = p_zlest0194-pl_cav_tp_veic.
      <fs_zcte_trans_2>-tp_rodado      = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_2>-tp_carroceria2 = wa_zlest0002-tp_carroceria2.
      <fs_zcte_trans_2>-tara           = wa_zlest0002-tara.
      <fs_zcte_trans_2>-cap_kg         = wa_zlest0002-cap_kg.
      <fs_zcte_trans_2>-cap_m3         = wa_zlest0002-cap_m3.
      <fs_zcte_trans_2>-prop_cnpj      = p_zlest0194-pl_cav_prop_cnpj.
      <fs_zcte_trans_2>-prop_nome      = wa_lfa1-name1.
      <fs_zcte_trans_2>-prop_rntrc     = wa_lfa1-bahns.
      <fs_zcte_trans_2>-prop_uf        = wa_lfa1-regio.
      <fs_zcte_trans_2>-prop_ie        = wa_lfa1-stcd3.
      <fs_zcte_trans_2>-telefone       = wa_lfa1-telf1.
    ENDIF.
  ENDIF.

  IF p_zlest0194-placa_car2 IS NOT INITIAL.
    CLEAR: wa_zlest0002.
    SELECT SINGLE *
      FROM zlest0002
      INTO wa_zlest0002
     WHERE pc_veiculo = p_zlest0194-placa_car2.

    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_zcte_trans  ASSIGNING FIELD-SYMBOL(<fs_zcte_trans_3>).
      <fs_zcte_trans_3>-docnum         = p_report-docnum_sub.
      <fs_zcte_trans_3>-pc_veiculo     = p_zlest0194-placa_car2.
      <fs_zcte_trans_3>-qtd_eixo       = wa_zlest0002-qt_eixo.
      <fs_zcte_trans_3>-proprietario   = p_zlest0194-pl_cav_prop_cod.
      <fs_zcte_trans_3>-country        = wa_zlest0002-country.
      <fs_zcte_trans_3>-taxjurcode     = wa_zlest0002-taxjurcode.
      <fs_zcte_trans_3>-spras          = wa_zlest0002-spras.
      <fs_zcte_trans_3>-cd_cidade      = wa_zlest0002-cd_cidade.
      <fs_zcte_trans_3>-cd_uf          = wa_zlest0002-cd_uf.
      <fs_zcte_trans_3>-agregado       = wa_zlest0002-agregado.
      <fs_zcte_trans_3>-cd_renavam     = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_3>-tp_veiculo     = p_zlest0194-pl_cav_tp_veic.
      <fs_zcte_trans_3>-tp_rodado      = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_3>-tp_carroceria2 = wa_zlest0002-tp_carroceria2.
      <fs_zcte_trans_3>-tara           = wa_zlest0002-tara.
      <fs_zcte_trans_3>-cap_kg         = wa_zlest0002-cap_kg.
      <fs_zcte_trans_3>-cap_m3         = wa_zlest0002-cap_m3.
      <fs_zcte_trans_3>-prop_cnpj      = p_zlest0194-pl_cav_prop_cnpj.
      <fs_zcte_trans_3>-prop_nome      = wa_lfa1-name1.
      <fs_zcte_trans_3>-prop_rntrc     = wa_lfa1-bahns.
      <fs_zcte_trans_3>-prop_uf        = wa_lfa1-regio.
      <fs_zcte_trans_3>-prop_ie        = wa_lfa1-stcd3.
      <fs_zcte_trans_3>-telefone       = wa_lfa1-telf1.
    ENDIF.
  ENDIF.

  IF p_zlest0194-placa_car3 IS NOT INITIAL.
    CLEAR: wa_zlest0002.
    SELECT SINGLE *
      FROM zlest0002
      INTO wa_zlest0002
     WHERE pc_veiculo = p_zlest0194-placa_car3.

    IF sy-subrc IS INITIAL.

      APPEND INITIAL LINE TO t_zcte_trans  ASSIGNING FIELD-SYMBOL(<fs_zcte_trans_4>).
      <fs_zcte_trans_4>-docnum         = p_report-docnum_sub.
      <fs_zcte_trans_4>-pc_veiculo     = p_zlest0194-placa_car3.
      <fs_zcte_trans_4>-qtd_eixo       = wa_zlest0002-qt_eixo.
      <fs_zcte_trans_4>-proprietario   = p_zlest0194-pl_cav_prop_cod.
      <fs_zcte_trans_4>-country        = wa_zlest0002-country.
      <fs_zcte_trans_4>-taxjurcode     = wa_zlest0002-taxjurcode.
      <fs_zcte_trans_4>-spras          = wa_zlest0002-spras.
      <fs_zcte_trans_4>-cd_cidade      = wa_zlest0002-cd_cidade.
      <fs_zcte_trans_4>-cd_uf          = wa_zlest0002-cd_uf.
      <fs_zcte_trans_4>-agregado       = wa_zlest0002-agregado.
      <fs_zcte_trans_4>-cd_renavam     = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_4>-tp_veiculo     = p_zlest0194-pl_cav_tp_veic.
      <fs_zcte_trans_4>-tp_rodado      = wa_zlest0002-cd_renavam.
      <fs_zcte_trans_4>-tp_carroceria2 = wa_zlest0002-tp_carroceria2.
      <fs_zcte_trans_4>-tara           = wa_zlest0002-tara.
      <fs_zcte_trans_4>-cap_kg         = wa_zlest0002-cap_kg.
      <fs_zcte_trans_4>-cap_m3         = wa_zlest0002-cap_m3.
      <fs_zcte_trans_4>-prop_cnpj      = p_zlest0194-pl_cav_prop_cnpj.
      <fs_zcte_trans_4>-prop_nome      = wa_lfa1-name1.
      <fs_zcte_trans_4>-prop_rntrc     = wa_lfa1-bahns.
      <fs_zcte_trans_4>-prop_uf        = wa_lfa1-regio.
      <fs_zcte_trans_4>-prop_ie        = wa_lfa1-stcd3.
      <fs_zcte_trans_4>-telefone       = wa_lfa1-telf1.
    ENDIF.
  ENDIF.

  IF t_zcte_trans[] IS NOT INITIAL.
    MODIFY zcte_trans FROM TABLE t_zcte_trans.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_PARCEIROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_parceiros USING: p_zlest0194 TYPE zlest0194
                                     p_report    TYPE y_report. "zsds025.

  DATA: wa_j_1bnfdoc  TYPE j_1bnfdoc,
        wa_j_1bbranch TYPE j_1bbranch,
        wa_adrc       TYPE adrc.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_parceiros
    INTO @DATA(l_cte_parc)
       WHERE docnum = @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bnfdoc
    INTO wa_j_1bnfdoc
   WHERE docnum = p_zlest0194-docnum_sub.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM j_1bbranch
      INTO wa_j_1bbranch
     WHERE bukrs  = wa_j_1bnfdoc-bukrs
       AND branch = wa_j_1bnfdoc-branch.

    SELECT SINGLE *
      FROM adrc
      INTO wa_adrc
     WHERE addrnumber = wa_j_1bbranch-adrnr.

    IF p_zlest0194-reme_cod_forn IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
       WHERE lifnr = @p_zlest0194-reme_cod_forn
        INTO @DATA(wa_lfa1_2).

      IF wa_lfa1_2-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc
         WHERE addrnumber = @wa_lfa1_2-adrnr
          INTO @DATA(wa_adrc_2).
      ENDIF.
    ENDIF.


    IF p_zlest0194-dest_cod_cliente IS NOT INITIAL.
      SELECT SINGLE *
        FROM kna1
       WHERE kunnr = @p_zlest0194-dest_cod_cliente
        INTO @DATA(wa_kna1).

      IF wa_kna1-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc
         WHERE addrnumber = @wa_kna1-adrnr
          INTO @DATA(wa_adrc_3).
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO t_zcte_parceiros  ASSIGNING FIELD-SYMBOL(<fs_zcte_parceiros>).
    <fs_zcte_parceiros>-docnum        = p_report-docnum_sub.
    <fs_zcte_parceiros>-emit_codigo   = wa_j_1bnfdoc-branch.
    <fs_zcte_parceiros>-emit_cnpj     = wa_j_1bbranch-stcd1.
    <fs_zcte_parceiros>-emit_ie       = wa_j_1bbranch-state_insc.
    <fs_zcte_parceiros>-emit_xnome    = wa_adrc-name1.
    <fs_zcte_parceiros>-emit_xfant    = wa_adrc-name1.
    <fs_zcte_parceiros>-emit_xlgr     = wa_adrc-street.
    <fs_zcte_parceiros>-emit_nro      = wa_adrc-house_num1.
    <fs_zcte_parceiros>-emit_xbairro  = wa_adrc-city2.
    <fs_zcte_parceiros>-emit_cmun     = wa_adrc-taxjurcode+3(12).
    <fs_zcte_parceiros>-emit_xmun     = wa_adrc-city1.
    <fs_zcte_parceiros>-emit_cep      = wa_adrc-post_code1.
    <fs_zcte_parceiros>-emit_uf       = wa_adrc-region.
    <fs_zcte_parceiros>-emit_fone     = wa_adrc-tel_number.
    <fs_zcte_parceiros>-reme_codigo   = p_zlest0194-reme_cod_forn.
    <fs_zcte_parceiros>-reme_cnpj     = p_zlest0194-reme_cnpj.
    <fs_zcte_parceiros>-reme_cpf      = p_zlest0194-reme_cpf.
    <fs_zcte_parceiros>-reme_ie       = p_zlest0194-reme_ie.
    <fs_zcte_parceiros>-reme_xnome    = p_zlest0194-reme_rsocial.
    <fs_zcte_parceiros>-reme_xfant    = p_zlest0194-reme_rsocial.
    <fs_zcte_parceiros>-reme_fone     = wa_adrc_2-tel_number.
    <fs_zcte_parceiros>-reme_xlgr     = wa_adrc_2-street.
    <fs_zcte_parceiros>-reme_nro      = wa_adrc_2-house_num1.
    <fs_zcte_parceiros>-reme_xbairro  = wa_adrc_2-city2.
    <fs_zcte_parceiros>-reme_cmun     = wa_adrc_2-taxjurcode+3(12).
    <fs_zcte_parceiros>-reme_xmun     = wa_adrc_2-city1.
    <fs_zcte_parceiros>-reme_cep      = wa_adrc_2-post_code1.
    <fs_zcte_parceiros>-reme_uf       = wa_adrc_2-region.
    <fs_zcte_parceiros>-dest_codigo   = p_zlest0194-dest_cod_cliente.
    <fs_zcte_parceiros>-dest_cnpj     = p_zlest0194-dest_cnpj.
    <fs_zcte_parceiros>-dest_cpf      = p_zlest0194-dest_cpf.
    <fs_zcte_parceiros>-dest_ie       = p_zlest0194-dest_ie.
    <fs_zcte_parceiros>-dest_xnome    = p_zlest0194-dest_rsocial.
    <fs_zcte_parceiros>-dest_xfant    = p_zlest0194-dest_rsocial.
    <fs_zcte_parceiros>-dest_fone     = wa_adrc_3-tel_number.
    <fs_zcte_parceiros>-dest_xlgr     = wa_adrc_3-street.
    <fs_zcte_parceiros>-dest_nro      = wa_adrc_3-house_num1.
    <fs_zcte_parceiros>-dest_xbairro  = wa_adrc_3-city2.
    <fs_zcte_parceiros>-dest_cmun     = wa_adrc_3-taxjurcode+3(12).
    <fs_zcte_parceiros>-dest_xmun     = wa_adrc_3-city1.
    <fs_zcte_parceiros>-dest_cep      = wa_adrc_3-post_code1.
    <fs_zcte_parceiros>-dest_uf       = wa_adrc_3-region.

  ENDIF.


  IF t_zcte_parceiros[] IS NOT INITIAL.
    MODIFY zcte_parceiros   FROM TABLE t_zcte_parceiros.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_MOTORISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_motorista   USING: p_zlest0194 TYPE zlest0194
                                       p_report    TYPE y_report. "zsds025.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_motorista
    INTO @DATA(l_cte_motorista)
       WHERE docnum =  @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.

  IF p_zlest0194-motorista IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = @p_zlest0194-motorista
      INTO @DATA(wa_lfa1).

    APPEND INITIAL LINE TO t_zcte_motorista  ASSIGNING FIELD-SYMBOL(<fs_zcte_motorista>).
    <fs_zcte_motorista>-docnum = p_zlest0194-docnum_sub.
    <fs_zcte_motorista>-lifnr  = p_zlest0194-motorista.
    <fs_zcte_motorista>-xnome  = wa_lfa1-name1.
    <fs_zcte_motorista>-cpf    = p_zlest0194-mot_cpf.

  ENDIF.

  IF t_zcte_motorista[] IS NOT INITIAL.
    MODIFY zcte_motorista   FROM TABLE t_zcte_motorista.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_INFO_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_info_nota USING: p_zlest0194 TYPE zlest0194
                                     p_report    TYPE y_report. "zsds025.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_info_nota
    INTO @DATA(l_cte_info_nota)
       WHERE docnum         = @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.


  READ TABLE t_zib_cte_dist_n55 ASSIGNING FIELD-SYMBOL(<fs_zib_cte_dist_n55>)
                                              WITH KEY cd_chave_cte = p_zlest0194-chave_xml_cte.

  IF sy-subrc IS INITIAL.
    IF <fs_zib_cte_dist_n55>-docnum_nfe IS NOT INITIAL.

      SELECT SINGLE *
        FROM j_1bnfdoc
       WHERE docnum = @<fs_zib_cte_dist_n55>-docnum_nfe
        INTO @DATA(wa_j_1bnfdoc).

      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
          FROM j_1bnflin
         WHERE docnum = @wa_j_1bnfdoc-docnum
          INTO @DATA(wa_j_1bnflin).

        SELECT SINGLE *
          FROM j_1bnfe_active
         WHERE docnum = @wa_j_1bnfdoc-docnum
          INTO @DATA(wa_j_1bnfe_active).

        APPEND INITIAL LINE TO t_zcte_info_nota  ASSIGNING FIELD-SYMBOL(<fs_zcte_info_nota>).
        <fs_zcte_info_nota>-nfe            = wa_j_1bnfdoc-nfe.
        <fs_zcte_info_nota>-modelo         = wa_j_1bnfdoc-model.
        <fs_zcte_info_nota>-serie          = wa_j_1bnfdoc-series.
        <fs_zcte_info_nota>-numero         = wa_j_1bnfdoc-nfenum..
        <fs_zcte_info_nota>-cliente        = wa_j_1bnfdoc-parid.
        <fs_zcte_info_nota>-docnum         = p_zlest0194-docnum_sub.
        <fs_zcte_info_nota>-partyp         = wa_j_1bnfdoc-partyp.
        <fs_zcte_info_nota>-docnum_nf      = wa_j_1bnfdoc-docnum.
        <fs_zcte_info_nota>-dtemissao      = wa_j_1bnfdoc-pstdat.
        <fs_zcte_info_nota>-vl_produtos    = wa_j_1bnfdoc-nftot.
        <fs_zcte_info_nota>-vl_nota_fiscal = wa_j_1bnfdoc-nftot.
        <fs_zcte_info_nota>-material       = wa_j_1bnflin-matnr.
        <fs_zcte_info_nota>-quantidade     = wa_j_1bnfdoc-brgew.
        <fs_zcte_info_nota>-unidade        = wa_j_1bnfdoc-shpunt.
        <fs_zcte_info_nota>-cfop           = wa_j_1bnflin-cfop.
        <fs_zcte_info_nota>-docnum9        = wa_j_1bnfe_active-docnum9.
        <fs_zcte_info_nota>-cdv            = wa_j_1bnfe_active-cdv.

        CONCATENATE wa_j_1bnfe_active-regio   wa_j_1bnfe_active-nfyear
                    wa_j_1bnfe_active-nfmonth wa_j_1bnfe_active-stcd1
                    wa_j_1bnfe_active-model   wa_j_1bnfe_active-serie
                    wa_j_1bnfe_active-nfnum9  wa_j_1bnfe_active-docnum9
                    wa_j_1bnfe_active-cdv
        INTO <fs_zcte_info_nota>-chave.
        CONDENSE <fs_zcte_info_nota>-chave NO-GAPS.
      ENDIF.

* Docnum NFE em branco
    ELSE.

      APPEND INITIAL LINE TO t_zcte_info_nota  ASSIGNING FIELD-SYMBOL(<fs_zcte_info_nota_2>).

      CONCATENATE <fs_zib_cte_dist_n55>-n55_chave_acesso+20(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+21(1)
      INTO <fs_zcte_info_nota_2>-modelo.
      CONDENSE <fs_zcte_info_nota_2>-modelo NO-GAPS.

      CONCATENATE <fs_zib_cte_dist_n55>-n55_chave_acesso+22(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+23(1)
      <fs_zib_cte_dist_n55>-n55_chave_acesso+24(1) INTO <fs_zcte_info_nota_2>-serie.
      CONDENSE <fs_zcte_info_nota_2>-serie NO-GAPS.


      CONCATENATE <fs_zib_cte_dist_n55>-n55_chave_acesso+25(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+26(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+27(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+28(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+29(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+30(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+31(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+32(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+33(1)
                  INTO <fs_zcte_info_nota_2>-numero.
      CONDENSE <fs_zcte_info_nota_2>-numero NO-GAPS.

      <fs_zcte_info_nota_2>-vl_produtos    = <fs_zib_cte_dist_n55>-zvlr_mercadoria.
      <fs_zcte_info_nota_2>-vl_nota_fiscal = <fs_zib_cte_dist_n55>-zvlr_mercadoria.
      <fs_zcte_info_nota_2>-docnum         = p_zlest0194-docnum_sub.

      CONCATENATE <fs_zib_cte_dist_n55>-n55_chave_acesso+34(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+35(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+36(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+37(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+38(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+39(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+40(1) <fs_zib_cte_dist_n55>-n55_chave_acesso+41(1)
                  <fs_zib_cte_dist_n55>-n55_chave_acesso+42(1)
                  INTO <fs_zcte_info_nota_2>-docnum9.
      CONDENSE <fs_zcte_info_nota_2>-docnum9 NO-GAPS.

      <fs_zcte_info_nota_2>-cdv   = <fs_zib_cte_dist_n55>-n55_chave_acesso+43(1).
      <fs_zcte_info_nota_2>-chave = <fs_zib_cte_dist_n55>-n55_chave_acesso.
    ENDIF.


* Inicio - FA - 14.10.2025 - ZLES0181 -  Erro ao gera ct-e de subcontratação
  ELSE.

    READ TABLE t_cte_dist_n01
     ASSIGNING FIELD-SYMBOL(<fs_cte_dist_n01>)
     WITH KEY cd_chave_cte = p_zlest0194-chave_xml_cte.
    IF sy-subrc IS INITIAL.

      IF <fs_cte_dist_n01>-docnum_nf IS NOT INITIAL.

        SELECT SINGLE *
          FROM j_1bnfdoc
           INTO wa_j_1bnfdoc
         WHERE docnum = <fs_cte_dist_n01>-docnum_nf.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE *
            FROM j_1bnflin
            INTO wa_j_1bnflin
           WHERE docnum = wa_j_1bnfdoc-docnum.

          SELECT SINGLE *
            INTO wa_j_1bnfe_active
            FROM j_1bnfe_active
           WHERE docnum = wa_j_1bnfdoc-docnum.

          APPEND INITIAL LINE TO t_zcte_info_nota  ASSIGNING <fs_zcte_info_nota>.
          <fs_zcte_info_nota>-nfe            = wa_j_1bnfdoc-nfe.
          <fs_zcte_info_nota>-modelo         = wa_j_1bnfdoc-model.
          <fs_zcte_info_nota>-serie          = wa_j_1bnfdoc-series.
          <fs_zcte_info_nota>-numero         = wa_j_1bnfdoc-nfenum..
          <fs_zcte_info_nota>-cliente        = wa_j_1bnfdoc-parid.
          <fs_zcte_info_nota>-docnum         = p_zlest0194-docnum_sub.
          <fs_zcte_info_nota>-partyp         = wa_j_1bnfdoc-partyp.
          <fs_zcte_info_nota>-docnum_nf      = wa_j_1bnfdoc-docnum.
          <fs_zcte_info_nota>-dtemissao      = wa_j_1bnfdoc-pstdat.
          <fs_zcte_info_nota>-vl_produtos    = wa_j_1bnfdoc-nftot.
          <fs_zcte_info_nota>-vl_nota_fiscal = wa_j_1bnfdoc-nftot.
          <fs_zcte_info_nota>-material       = wa_j_1bnflin-matnr.
          <fs_zcte_info_nota>-quantidade     = wa_j_1bnfdoc-brgew.
          <fs_zcte_info_nota>-unidade        = wa_j_1bnfdoc-shpunt.
          <fs_zcte_info_nota>-cfop           = wa_j_1bnflin-cfop.
          <fs_zcte_info_nota>-docnum9        = wa_j_1bnfe_active-docnum9.
          <fs_zcte_info_nota>-cdv            = wa_j_1bnfe_active-cdv.

          CONCATENATE wa_j_1bnfe_active-regio   wa_j_1bnfe_active-nfyear
                      wa_j_1bnfe_active-nfmonth wa_j_1bnfe_active-stcd1
                      wa_j_1bnfe_active-model   wa_j_1bnfe_active-serie
                      wa_j_1bnfe_active-nfnum9  wa_j_1bnfe_active-docnum9
                      wa_j_1bnfe_active-cdv
          INTO <fs_zcte_info_nota>-chave.
          CONDENSE <fs_zcte_info_nota>-chave NO-GAPS.
        ENDIF.

* Docnum NFE em branco
      ELSE.

        APPEND INITIAL LINE TO t_zcte_info_nota ASSIGNING <fs_zcte_info_nota_2>.

        <fs_zcte_info_nota_2>-modelo = <fs_cte_dist_n01>-n01_modelo_nf.

        CONDENSE <fs_zcte_info_nota_2>-modelo NO-GAPS.

        <fs_zcte_info_nota_2>-serie = <fs_cte_dist_n01>-n01_numr_serie.
        CONDENSE <fs_zcte_info_nota_2>-serie NO-GAPS.


        <fs_zcte_info_nota_2>-numero = <fs_cte_dist_n01>-n01_nr_nf.
        CONDENSE <fs_zcte_info_nota_2>-numero NO-GAPS.


        <fs_zcte_info_nota_2>-vl_produtos    = <fs_cte_dist_n01>-n01_vl_produtos.
        <fs_zcte_info_nota_2>-vl_nota_fiscal = <fs_cte_dist_n01>-n01_vl_nota.

        <fs_zcte_info_nota_2>-dtemissao = <fs_cte_dist_n01>-n01_data_emissao.


        <fs_zcte_info_nota_2>-vl_bc     = <fs_cte_dist_n01>-n01_vl_base_icms.
        <fs_zcte_info_nota_2>-vl_icms   = <fs_cte_dist_n01>-n01_vl_icms.
        <fs_zcte_info_nota_2>-vl_bc_st  = <fs_cte_dist_n01>-n01_vl_bicms_st.
        <fs_zcte_info_nota_2>-vl_st     = <fs_cte_dist_n01>-n01_vl_icms_st .

        <fs_zcte_info_nota_2>-cfop = <fs_cte_dist_n01>-n01_codg_cfop.
        <fs_zcte_info_nota_2>-peso_fiscal   = <fs_cte_dist_n01>-n01_vl_peso.

      ENDIF.

    ENDIF.

* Fim - FA - 14.10.2025 - ZLES0181 -  Erro ao gera ct-e de subcontratação

  ENDIF.

  IF t_zcte_info_nota[] IS NOT INITIAL.
    MODIFY zcte_info_nota   FROM TABLE t_zcte_info_nota.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_CIOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_ciot USING: p_zlest0194 TYPE zlest0194
                                p_report    TYPE y_report. "zsds025.

  DATA: vl_werks_lifnr TYPE lifnr,
        vl_cd_ciot     TYPE zcte_ciot-cd_ciot.

  DATA: lt_result    TYPE match_result_tab,
        wa_result    TYPE match_result,
        vl_file      TYPE lfa1-gbort,
        vl_lines     TYPE i,
        vl_file_name TYPE char50.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_ciot
    INTO @DATA(l_cte_ciot)
       WHERE docnum = @p_report-docnum_sub.

  IF sy-subrc IS NOT INITIAL.

    CLEAR: v_bahns.
    IF p_report-docnum_sub IS NOT INITIAL.
      SELECT SINGLE *
          FROM j_1bnfdoc
         WHERE docnum = @p_report-docnum_sub
          INTO @DATA(wa_j_1bnfdoc).

      IF wa_j_1bnfdoc-branch IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_j_1bnfdoc-branch
          IMPORTING
            output = vl_werks_lifnr.

        SELECT SINGLE *
          FROM lfa1
         WHERE lifnr = @vl_werks_lifnr
          INTO @DATA(wa_lfa1_1).

        IF sy-subrc IS INITIAL.
          v_bahns = wa_lfa1_1-bahns.
        ENDIF.
      ENDIF.
    ENDIF.


    IF p_zlest0194-ov_sub IS NOT INITIAL.

      SELECT SINGLE *
        FROM vbap
       WHERE vbeln = @p_zlest0194-ov_sub
        INTO @DATA(wa_vbap_1).

      IF wa_vbap_1-werks IS NOT INITIAL.
        CLEAR: vl_werks_lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbap_1-werks
          IMPORTING
            output = vl_werks_lifnr.

        SELECT SINGLE *
          FROM lfa1
         WHERE lifnr = @vl_werks_lifnr
          INTO @DATA(wa_lfa1_2).

        IF wa_lfa1_2-adrnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM adrc
           WHERE addrnumber = @wa_lfa1_2-adrnr
            INTO @DATA(wa_adrc_1).
        ENDIF.
      ENDIF.

      SELECT SINGLE *
        FROM vbpa
       WHERE vbeln = @p_zlest0194-ov_sub
         AND parvw = 'T4'
        INTO @DATA(wa_vbpa_1).

      IF wa_vbpa_1-kunnr IS NOT INITIAL.

        SELECT SINGLE *
          FROM kna1
         WHERE kunnr = @wa_vbpa_1-kunnr
          INTO @DATA(wa_kna1_1).

        IF wa_kna1_1-adrnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM adrc
           WHERE addrnumber = @wa_kna1_1-adrnr
            INTO @DATA(wa_adrc_2).
        ENDIF.
      ENDIF.


      SELECT SINGLE *
        FROM vbpa
       WHERE vbeln = @p_zlest0194-ov_sub
         AND parvw = 'T1'
        INTO @DATA(wa_vbpa_2).

      IF wa_vbpa_2-lifnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM lfa1
         WHERE lifnr = @wa_vbpa_2-lifnr
          INTO @DATA(wa_lfa1_3).

        IF wa_lfa1_3-adrnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM adrc
           WHERE addrnumber = @wa_lfa1_3-adrnr
            INTO @DATA(wa_adrc_3).
        ENDIF.
      ENDIF.


      SELECT SINGLE *
        FROM vbpa
       WHERE vbeln = @p_zlest0194-ov_sub
         AND parvw = 'LR'
        INTO @DATA(wa_vbpa_3).

      IF wa_vbpa_3-kunnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM kna1
         WHERE kunnr = @wa_vbpa_3-kunnr
          INTO @DATA(wa_kna1_2).

        IF wa_kna1_2-adrnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM adrc
           WHERE addrnumber = @wa_kna1_2-adrnr
            INTO @DATA(wa_adrc_6).
        ENDIF.
      ENDIF.
    ENDIF.


    IF p_zlest0194-pl_cav_prop_cod IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
       WHERE lifnr = @p_zlest0194-pl_cav_prop_cod
        INTO @DATA(wa_lfa1_4).

      IF wa_lfa1_4-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc
         WHERE addrnumber = @wa_lfa1_4-adrnr
          INTO @DATA(wa_adrc_4).
      ENDIF.
    ENDIF.


    IF p_zlest0194-motorista IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1
       WHERE lifnr = @p_zlest0194-motorista
        INTO @DATA(wa_lfa1_5).

      IF wa_lfa1_5-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc
         WHERE addrnumber = @wa_lfa1_5-adrnr
          INTO @DATA(wa_adrc_5).
      ENDIF.
    ENDIF.

    SELECT MAX( cd_ciot )
      FROM zcte_ciot
      INTO vl_cd_ciot.

    SELECT SINGLE *
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte = @p_report-chave_xml_cte
      INTO @DATA(wa_zib_cte_dist_ter).

    APPEND INITIAL LINE TO t_zcte_ciot  ASSIGNING FIELD-SYMBOL(<fs_zcte_ciot>).
    <fs_zcte_ciot>-docnum           = p_report-docnum_sub.
    <fs_zcte_ciot>-cd_ciot          = vl_cd_ciot + 1.
    CONDENSE <fs_zcte_ciot>-cd_ciot NO-GAPS.
    <fs_zcte_ciot>-st_ciot          = '9'.
    <fs_zcte_ciot>-rntrc            = wa_lfa1_1-bahns.
    <fs_zcte_ciot>-uf_origem        = wa_zib_cte_dist_ter-inicio_uf.
    <fs_zcte_ciot>-municipio_origem = wa_zib_cte_dist_ter-inicio_ibge.
    <fs_zcte_ciot>-dt_origem        = p_zlest0194-dt_emissao.
    <fs_zcte_ciot>-uf_termin        = wa_zib_cte_dist_ter-termino_uf.
    <fs_zcte_ciot>-municipio_termin = wa_zib_cte_dist_ter-termino_ibge.
    <fs_zcte_ciot>-emissor          = wa_vbap_1-werks.
    <fs_zcte_ciot>-ct_codigo        = wa_vbap_1-werks.
    <fs_zcte_ciot>-ct_nome          = wa_lfa1_2-name1.
    <fs_zcte_ciot>-ct_razao         = wa_lfa1_2-name1.
    <fs_zcte_ciot>-ct_cnpj          = wa_lfa1_2-stcd1.
    <fs_zcte_ciot>-ct_cpf           = wa_lfa1_2-stcd2.
    <fs_zcte_ciot>-ct_logradouro    = wa_lfa1_2-stras.
    <fs_zcte_ciot>-ct_numero        = wa_adrc_1-house_num1.
    <fs_zcte_ciot>-ct_bairro        = wa_lfa1_2-ort02.
    <fs_zcte_ciot>-ct_uf            = wa_lfa1_2-regio.
    <fs_zcte_ciot>-ct_municipio     = wa_lfa1_2-txjcd+3(12).
    <fs_zcte_ciot>-ct_cep           = wa_lfa1_2-pstlz.
    <fs_zcte_ciot>-ct_fone          = wa_lfa1_2-telf1.
    <fs_zcte_ciot>-dt_codigo        = wa_vbpa_1-kunnr.
    <fs_zcte_ciot>-dt_nome          = wa_kna1_1-name1.
    <fs_zcte_ciot>-dt_razao         = wa_kna1_1-name1.
    <fs_zcte_ciot>-dt_cnpj          = wa_kna1_1-stcd1.
    <fs_zcte_ciot>-dt_cpf           = wa_kna1_1-stcd2.
    <fs_zcte_ciot>-dt_logradouro    = wa_kna1_1-stras.
    <fs_zcte_ciot>-dt_numero        = wa_adrc_2-house_num1.
    <fs_zcte_ciot>-dt_bairro        = wa_kna1_1-ort02.
    <fs_zcte_ciot>-dt_uf            = wa_kna1_1-regio.
    <fs_zcte_ciot>-dt_municipio     = wa_kna1_1-txjcd+3(12).
    <fs_zcte_ciot>-dt_cep           = wa_kna1_1-pstlz.
    <fs_zcte_ciot>-dt_fone          = wa_kna1_1-telf1.
    <fs_zcte_ciot>-quantidade       = wa_vbap_1-brgew.
    <fs_zcte_ciot>-unidade          = wa_vbap_1-gewei.
    <fs_zcte_ciot>-vlr_frete        = wa_vbap_1-netpr.
    <fs_zcte_ciot>-vlr_saldo        = <fs_zcte_ciot>-vlr_frete.
    <fs_zcte_ciot>-moeda            = wa_vbap_1-waerk.
    <fs_zcte_ciot>-vlr_unit_merc    = p_zlest0194-vl_total_merc / p_zlest0194-qt_carga_cte.
    <fs_zcte_ciot>-unid_vlr_merc    = p_zlest0194-meins.
    <fs_zcte_ciot>-vlr_unit_frete   = ( p_zlest0194-valor_prestacao / p_zlest0194-qt_carga_cte ) * 1000.
    <fs_zcte_ciot>-unid_vlr_frete   = 'TO'.

    <fs_zcte_ciot>-rm_codigo         = wa_vbpa_2-lifnr.
    <fs_zcte_ciot>-rm_nome          = wa_lfa1_3-name1.
    <fs_zcte_ciot>-rm_razao         = wa_lfa1_3-name1.
    <fs_zcte_ciot>-rm_cnpj          = wa_lfa1_3-stcd1.
    <fs_zcte_ciot>-rm_cpf           = wa_lfa1_3-stcd2.
    <fs_zcte_ciot>-rm_logradouro    = wa_lfa1_3-stras.
    <fs_zcte_ciot>-rm_numero        = wa_adrc_3-house_num1.
    <fs_zcte_ciot>-rm_bairro        = wa_lfa1_3-ort02.
    <fs_zcte_ciot>-rm_uf            = wa_lfa1_3-regio.
    <fs_zcte_ciot>-rm_municipio     = wa_lfa1_3-txjcd+3(12).
    <fs_zcte_ciot>-rm_cep           = wa_lfa1_3-pstlz.
    <fs_zcte_ciot>-rm_fone          = wa_lfa1_3-telf1.
    <fs_zcte_ciot>-tr_codigo        = p_zlest0194-pl_cav_prop_cod.
    <fs_zcte_ciot>-tr_nome          = wa_lfa1_4-name1.
    <fs_zcte_ciot>-tr_razao         = wa_lfa1_4-name1.
    <fs_zcte_ciot>-tr_cnpj          = wa_lfa1_4-stcd1.
    <fs_zcte_ciot>-tr_cpf           = wa_lfa1_4-stcd2.
    <fs_zcte_ciot>-tr_logradouro    = wa_lfa1_4-stras.
    <fs_zcte_ciot>-tr_numero        = wa_adrc_4-house_num1.
    <fs_zcte_ciot>-tr_bairro        = wa_lfa1_4-ort02.
    <fs_zcte_ciot>-tr_uf            = wa_lfa1_4-regio.
    <fs_zcte_ciot>-tr_municipio     = wa_lfa1_4-txjcd+3(12).
    <fs_zcte_ciot>-tr_cep           = wa_lfa1_4-pstlz.
    <fs_zcte_ciot>-tr_fone          = wa_lfa1_4-telf1.
    <fs_zcte_ciot>-mt_codigo        = p_zlest0194-motorista.
    <fs_zcte_ciot>-mt_nome          = wa_lfa1_5-name1.
    <fs_zcte_ciot>-mt_razao         = wa_lfa1_5-name1.
    <fs_zcte_ciot>-mt_cpf           = wa_lfa1_5-stcd2.
    <fs_zcte_ciot>-mt_logradouro    = wa_lfa1_5-stras.
    <fs_zcte_ciot>-mt_numero        = wa_adrc_5-house_num1.
    <fs_zcte_ciot>-mt_bairro        = wa_lfa1_5-ort02.
    <fs_zcte_ciot>-mt_uf            = wa_lfa1_5-regio.
    <fs_zcte_ciot>-mt_municipio     = wa_lfa1_5-txjcd+3(12).
    <fs_zcte_ciot>-mt_cep           = wa_lfa1_5-pstlz.
    <fs_zcte_ciot>-mt_rg            = wa_lfa1_5-stcd3.
    <fs_zcte_ciot>-mt_cnh           = wa_lfa1_5-stcd4.
    <fs_zcte_ciot>-mt_uf_rg         = wa_lfa1_5-gbort(2).
    <fs_zcte_ciot>-mt_fone          = wa_lfa1_5-telf1.
    <fs_zcte_ciot>-mt_nomemae       = wa_lfa1_5-profs.
    <fs_zcte_ciot>-mt_sexo          = wa_lfa1_5-sexkz.
    <fs_zcte_ciot>-mt_dt_nascimento = wa_lfa1_5-gbdat.

    vl_file = wa_lfa1_4-gbort.
    FIND ALL OCCURRENCES OF '/' IN vl_file RESULTS lt_result.
    DESCRIBE TABLE lt_result LINES vl_lines.
    READ TABLE lt_result INTO wa_result
    INDEX vl_lines.
    wa_result-offset = wa_result-offset + 1.
    vl_file_name = vl_file+wa_result-offset.

    <fs_zcte_ciot>-mt_org_rg        = vl_file_name.  "( após  o traço / ).
    <fs_zcte_ciot>-lr_codigo        = wa_vbpa_3-kunnr.
    <fs_zcte_ciot>-lr_nome          = wa_kna1_2-name1.
    <fs_zcte_ciot>-lr_razao         = wa_kna1_2-name1.
    <fs_zcte_ciot>-lr_cnpj          = wa_kna1_2-stcd1.
    <fs_zcte_ciot>-lr_cpf           = wa_kna1_2-stcd2.
    <fs_zcte_ciot>-lr_logradouro    = wa_kna1_2-stras.
    <fs_zcte_ciot>-lr_numero        = wa_adrc_6-house_num1.
    <fs_zcte_ciot>-lr_bairro        = wa_kna1_2-ort02.
    <fs_zcte_ciot>-lr_uf            = wa_kna1_2-regio.
    <fs_zcte_ciot>-lr_municipio     = wa_kna1_2-txjcd+3(12).
    <fs_zcte_ciot>-lr_cep           = wa_kna1_2-pstlz.
    <fs_zcte_ciot>-lr_fone          = wa_kna1_2-telf1.

  ENDIF.

  IF t_zcte_ciot[] IS NOT INITIAL.
    MODIFY zcte_ciot        FROM TABLE t_zcte_ciot.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_IDENTIFICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_identifica USING: p_zlest0194 TYPE zlest0194
                                      p_report    TYPE y_report. "zsds025.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_identifica
    INTO @DATA(l_cte_identifica)
       WHERE docnum = @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.

  IF p_zlest0194-docnum_sub IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bnfdoc
     WHERE docnum = @p_zlest0194-docnum_sub
      INTO @DATA(wa_j_1bnfdoc).

    IF sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM j_1bnflin
       WHERE docnum = @p_zlest0194-docnum_sub
        INTO @DATA(wa_j_1bnflin).

      SELECT SINGLE *
        FROM j_1bbranch
       WHERE bukrs  = @wa_j_1bnfdoc-bukrs
         AND branch = @wa_j_1bnfdoc-branch
        INTO @DATA(wa_j_1bbranch).

      IF wa_j_1bbranch-adrnr  IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc
         WHERE addrnumber = @wa_j_1bbranch-adrnr
          INTO @DATA(wa_adrc).

        IF wa_adrc-taxjurcode IS NOT INITIAL.
          SELECT SINGLE *
            FROM j_1btxjurt
           WHERE taxjurcode =  @wa_adrc-taxjurcode
             AND country    = 'BR'
             AND spras      = @sy-langu
            INTO @DATA(wa_j_1btxjurt_1).
        ENDIF.
      ENDIF.

      IF wa_j_1bnfdoc-cte_strt_lct IS NOT INITIAL.
        SELECT SINGLE *
          FROM j_1btxjurt
         WHERE taxjurcode =  @wa_j_1bnfdoc-cte_strt_lct
           AND country    = 'BR'
           AND spras      = @sy-langu
          INTO @DATA(wa_j_1btxjurt_2).
      ENDIF.
    ENDIF.

    SELECT SINGLE *
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte = @p_report-chave_xml_cte
      INTO @DATA(wa_zib_cte_dist_ter).

    APPEND INITIAL LINE TO t_zcte_identifica ASSIGNING FIELD-SYMBOL(<fs_zcte_identifica>).
    <fs_zcte_identifica>-docnum         = p_report-docnum_sub.
    <fs_zcte_identifica>-emissor        = wa_j_1bnfdoc-branch.
    <fs_zcte_identifica>-cfop           = wa_j_1bnflin-cfop.
    <fs_zcte_identifica>-cfotxt         = wa_j_1bnfdoc-natop.
    <fs_zcte_identifica>-dhemi          = wa_j_1bnfdoc-credat.
    <fs_zcte_identifica>-hremi          = wa_j_1bnfdoc-cretim.
    <fs_zcte_identifica>-tpcte          = '0'.
    <fs_zcte_identifica>-modal          = '01'.
    <fs_zcte_identifica>-tpserv         = '1'.
    <fs_zcte_identifica>-rodo_rntrc     = v_bahns.
    <fs_zcte_identifica>-ufenv          = wa_adrc-region.
    <fs_zcte_identifica>-cmunenv        = wa_adrc-taxjurcode+3(12).
    <fs_zcte_identifica>-nmunenv        = wa_j_1btxjurt_1-text.
    <fs_zcte_identifica>-ufini          = wa_zib_cte_dist_ter-inicio_uf.
    <fs_zcte_identifica>-cmunini        = wa_zib_cte_dist_ter-inicio_ibge.
    <fs_zcte_identifica>-nmunini        = wa_zib_cte_dist_ter-inicio_muni.
    <fs_zcte_identifica>-uffim          = wa_zib_cte_dist_ter-termino_uf.
    <fs_zcte_identifica>-cmunfim        = wa_zib_cte_dist_ter-termino_ibge.
    <fs_zcte_identifica>-nmunfim        = wa_zib_cte_dist_ter-termino_muni.
    <fs_zcte_identifica>-toma           = p_zlest0194-cd_tomador.
    <fs_zcte_identifica>-spras          = 'PT'.
    <fs_zcte_identifica>-country        = wa_j_1bnfdoc-land1.
    <fs_zcte_identifica>-waers          = wa_j_1bnfdoc-waerk.
    <fs_zcte_identifica>-vtprest        = wa_j_1bnfdoc-nftot.
    <fs_zcte_identifica>-vrec           = wa_j_1bnfdoc-nftot.
    <fs_zcte_identifica>-rodo_dt_inicio = wa_j_1bnfdoc-docdat.

  ENDIF.

  IF t_zcte_identifica[] IS NOT INITIAL.
    MODIFY zcte_identifica  FROM TABLE t_zcte_identifica.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_DOC_ANT
*&---------------------------------------------------------------------*
FORM f_carrega_zcte_doc_ant  USING: p_zlest0194 TYPE zlest0194
                                    p_report    TYPE y_report. "zsds025.

  DATA: lva_regio TYPE kna1-regio.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_doc_ant
    INTO @DATA(l_cte_doc_ant)
       WHERE docnum = @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.

  IF p_zlest0194-emit_cod_forn IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = @p_zlest0194-emit_cod_forn
      INTO @DATA(wa_lfa1).

    lva_regio = wa_lfa1-regio.
  ELSE.
    SELECT SINGLE *
      FROM kna1
     WHERE kunnr = @p_zlest0194-emit_cod_cliente
      INTO @DATA(wa_kna1).

    lva_regio = wa_kna1-regio.
  ENDIF.

*** BUG - 83992 - Inicio - CBRAND
*** Estava gerando XML sem UF
  IF lva_regio IS INITIAL.
    MESSAGE i000(z_mm) WITH 'Estado do Emissor (Fornecedor/Cliente)' 'não informado!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
*** BUG - 83992 - Inicio - CBRAND

  SELECT SINGLE *
    FROM j_1bnfe_active
   WHERE docnum = @p_zlest0194-docnum_sub
    INTO @DATA(wa_j_1bnfe_active).

  IF sy-subrc IS INITIAL.

    APPEND INITIAL LINE TO t_zcte_doc_ant ASSIGNING FIELD-SYMBOL(<fs_zcte_doc_ant>).

    CONCATENATE wa_j_1bnfe_active-regio  wa_j_1bnfe_active-nfyear  wa_j_1bnfe_active-nfmonth
                wa_j_1bnfe_active-stcd1  wa_j_1bnfe_active-model   wa_j_1bnfe_active-serie
                wa_j_1bnfe_active-nfnum9 wa_j_1bnfe_active-docnum9 wa_j_1bnfe_active-cdv
                INTO <fs_zcte_doc_ant>-cd_chave_cte.
    CONDENSE <fs_zcte_doc_ant>-cd_chave_cte NO-GAPS.

    <fs_zcte_doc_ant>-c57_chave_acesso = p_zlest0194-chave_xml_cte.
    <fs_zcte_doc_ant>-docnum           = p_zlest0194-docnum_sub.
    <fs_zcte_doc_ant>-emit_ant_cnpj    = p_zlest0194-emit_cnpj.
    <fs_zcte_doc_ant>-emit_ant_ie      = p_zlest0194-emit_ie.
    <fs_zcte_doc_ant>-emit_ant_uf      = lva_regio.
    <fs_zcte_doc_ant>-emit_ant_nome    = p_zlest0194-emit_rsocial.

  ENDIF.

  IF t_zcte_doc_ant[] IS NOT INITIAL.
    MODIFY zcte_doc_ant     FROM TABLE t_zcte_doc_ant.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZCTE_OBS_GERAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zcte_obs_gerais USING: p_zlest0194 TYPE zlest0194
                                      p_report    TYPE y_report. "zsds025.

  "Verifica se já foi adicionado
  SELECT SINGLE docnum FROM zcte_obs_gerais
    INTO @DATA(l_cte_obs_gerais)
       WHERE docnum =  @p_zlest0194-docnum_sub.

  CHECK sy-subrc IS NOT INITIAL.

  IF p_zlest0194-motorista IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = @p_zlest0194-motorista
      INTO @DATA(wa_lfa1).
  ENDIF.

  IF p_zlest0194-placa_cav IS NOT INITIAL.
    SELECT SINGLE *
      FROM zcte_trans
     WHERE docnum     = @p_zlest0194-docnum_sub
       AND pc_veiculo = @p_zlest0194-placa_cav
      INTO @DATA(wa_zcte_trans_1).
  ENDIF.

  IF p_zlest0194-placa_car1 IS NOT INITIAL.
    SELECT SINGLE *
     FROM zcte_trans
    WHERE docnum     = @p_zlest0194-docnum_sub
      AND pc_veiculo = @p_zlest0194-placa_car1
     INTO @DATA(wa_zcte_trans_2).
  ENDIF.

  IF p_zlest0194-placa_car2 IS NOT INITIAL.
    SELECT SINGLE *
     FROM zcte_trans
    WHERE docnum     = @p_zlest0194-docnum_sub
      AND pc_veiculo = @p_zlest0194-placa_car2
     INTO @DATA(wa_zcte_trans_3).
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfdoc
   WHERE docnum = @p_zlest0194-docnum_sub
    INTO @DATA(wa_j_1bnfdoc).

  IF sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zcte_info_nota
     WHERE docnum = @p_zlest0194-docnum_sub
      INTO @DATA(wa_zcte_info_nota).

    SELECT SINGLE *
      FROM j_1bnflin
     WHERE docnum = @wa_j_1bnfdoc-docnum
      INTO @DATA(wa_j_1bnflin).

    IF wa_j_1bnflin-taxlw1 IS NOT INITIAL.
      SELECT SINGLE *
        FROM j_1batl1t
       WHERE langu  = @sy-langu
         AND taxlaw = @wa_j_1bnflin-taxlw1
        INTO @DATA(wa_j_1batl1t).
    ENDIF.

* Linha 1
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_1>).
    <fs_zcte_obs_gerais_1>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_1>-linha  = '000001'.
    CONCATENATE 'SUBCONTR.SERV DE TRANSP.POR'  p_zlest0194-emit_rsocial 'IE:' p_zlest0194-emit_ie
    'CNPJ:' p_zlest0194-emit_cnpj INTO <fs_zcte_obs_gerais_1>-texto SEPARATED BY space.

* Linha 2
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_2>).
    <fs_zcte_obs_gerais_2>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_2>-linha  = '000002'.
    CONCATENATE 'CONF.DACTE N.' p_zlest0194-numr_cte 'Motorista:' wa_lfa1-name1 'CPF:' p_zlest0194-mot_cpf
     INTO <fs_zcte_obs_gerais_2>-texto SEPARATED BY space.

* Linha 3
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_3>).
    <fs_zcte_obs_gerais_3>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_3>-linha  = '000003'.
    CONCATENATE 'Placas:' p_zlest0194-placa_cav 'Renavam:' wa_zcte_trans_1-cd_renavam 'RNTRC:' wa_zcte_trans_1-prop_rntrc
    INTO <fs_zcte_obs_gerais_3>-texto SEPARATED BY space.

* Linha 4
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_4>).
    <fs_zcte_obs_gerais_4>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_4>-linha  = '000004'.
    CONCATENATE '/Carreta:' p_zlest0194-placa_car1 ',Renavam:' wa_zcte_trans_2-cd_renavam
     INTO <fs_zcte_obs_gerais_4>-texto SEPARATED BY space.

* Linha 5
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_5>).
    <fs_zcte_obs_gerais_5>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_5>-linha  = '000005'.
    CONCATENATE '/Carreta2:' p_zlest0194-placa_car2 ',Renavam:' wa_zcte_trans_3-cd_renavam
     INTO <fs_zcte_obs_gerais_5>-texto SEPARATED BY space.

* Linha 6
    APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_6>).
    <fs_zcte_obs_gerais_6>-docnum = wa_j_1bnfdoc-docnum.
    <fs_zcte_obs_gerais_6>-linha  = '000006'.
    CONCATENATE 'NFe(s):' wa_zcte_info_nota-nfe
     INTO <fs_zcte_obs_gerais_6>-texto SEPARATED BY space.

* Linha 7
    IF wa_j_1batl1t-line1 IS NOT INITIAL.
      APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_7>).
      <fs_zcte_obs_gerais_7>-docnum = wa_j_1bnfdoc-docnum.
      <fs_zcte_obs_gerais_7>-linha  = '000007'.
      <fs_zcte_obs_gerais_7>-texto  = wa_j_1batl1t-line1.
    ENDIF.

* Linha 8
    IF wa_j_1batl1t-line2 IS NOT INITIAL.
      APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_8>).
      <fs_zcte_obs_gerais_8>-docnum = wa_j_1bnfdoc-docnum.
      <fs_zcte_obs_gerais_8>-linha  = '000008'.
      <fs_zcte_obs_gerais_8>-texto  = wa_j_1batl1t-line2.
    ENDIF.

* Linha 9
    IF wa_j_1batl1t-line3 IS NOT INITIAL.
      APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_9>).
      <fs_zcte_obs_gerais_9>-docnum = wa_j_1bnfdoc-docnum.
      <fs_zcte_obs_gerais_9>-linha  = '000009'.
      <fs_zcte_obs_gerais_9>-texto  = wa_j_1batl1t-line3.
    ENDIF.

* Linha 10
    IF wa_j_1batl1t-line4 IS NOT INITIAL.
      APPEND INITIAL LINE TO t_zcte_obs_gerais ASSIGNING FIELD-SYMBOL(<fs_zcte_obs_gerais_10>).
      <fs_zcte_obs_gerais_10>-docnum = wa_j_1bnfdoc-docnum.
      <fs_zcte_obs_gerais_10>-linha  = '000010'.
      <fs_zcte_obs_gerais_10>-texto  = wa_j_1batl1t-line4.
    ENDIF.

  ENDIF.

  IF t_zcte_obs_gerais[] IS NOT INITIAL.
    MODIFY zcte_obs_gerais  FROM TABLE t_zcte_obs_gerais.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ZIB_CTE_DIST_TER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZLEST0194>  text
*      -->P_<FS_REPORT>  text
*----------------------------------------------------------------------*
FORM f_carrega_zib_cte_dist_ter USING:  p_zlest0194 TYPE zlest0194
                                        p_report    TYPE y_report. "zsds025.

  READ TABLE t_zib_cte_dist_ter ASSIGNING FIELD-SYMBOL(<fs_zib_cte_dist_ter>)
                                              WITH KEY cd_chave_cte = p_report-chave_xml_cte.

  IF sy-subrc IS INITIAL.
    <fs_zib_cte_dist_ter>-docnum_cte_sub = p_report-docnum_sub.
  ENDIF.

  IF t_zib_cte_dist_ter[] IS NOT INITIAL.
    MODIFY zib_cte_dist_ter FROM TABLE t_zib_cte_dist_ter.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_limpa_tabelas .
  CLEAR: t_zcte_trans, t_zcte_parceiros, t_zcte_motorista, t_zib_cte_dist_n55,
         t_zcte_info_nota, t_zcte_ciot,  t_zcte_identifica, t_zib_cte_dist_ter.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ZIB_CTE_DIST_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_zib_cte_dist_n55 .

  DATA: t_report_aux TYPE TABLE OF y_report. "zsds025.

  DATA: vl_awkey  TYPE awkey,
        vl_refkey TYPE j_1bnflin-refkey.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_limpa_tabelas.

  REFRESH: t_report_aux.
  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.

    IF <fs_report>-ov_sub IS NOT INITIAL.
      APPEND <fs_report> TO t_report_aux.
    ENDIF.

  ENDLOOP.

*  t_report_aux[] = t_report[].
*  DELETE t_report_aux WHERE selecionar NE abap_true.

  IF t_report_aux[] IS NOT INITIAL.

    SELECT *
      FROM zib_cte_dist_n55
      INTO TABLE t_zib_cte_dist_n55
       FOR ALL ENTRIES IN t_report_aux
     WHERE cd_chave_cte = t_report_aux-chave_xml_cte.


    SELECT *
      FROM zib_cte_dist_ter
      INTO TABLE t_zib_cte_dist_ter
       FOR ALL ENTRIES IN t_report_aux
     WHERE cd_chave_cte = t_report_aux-chave_xml_cte.


    SELECT *
     FROM zib_cte_dist_n01
     INTO TABLE t_cte_dist_n01
     FOR ALL ENTRIES IN t_report_aux
     WHERE cd_chave_cte = t_report_aux-chave_xml_cte.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DELETA_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_deleta_tabelas .

  DATA: w_ter TYPE zib_cte_dist_ter.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD lcl_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( t_row_no ) > 1.
*    MESSAGE i000(z_mm) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
*    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX w_row_no-row_id.
*  LOOP AT t_report ASSIGNING FIELD-SYMBOL(<fs_report>) WHERE selecionar = abap_true.

    IF <fs_report>-ov_sub IS NOT INITIAL.

      DELETE FROM zcte_trans       WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_parceiros   WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_motorista   WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_info_nota   WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_ciot        WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_identifica  WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_doc_ant     WHERE docnum = <fs_report>-docnum_sub.
      DELETE FROM zcte_obs_gerais  WHERE docnum = <fs_report>-docnum_sub.
      COMMIT WORK.

      IF <fs_report>-docnum_sub IS NOT INITIAL.

        SELECT  * FROM zib_cte_dist_ter
          INTO TABLE @DATA(t_ter)
          WHERE docnum_cte_sub = @<fs_report>-docnum_sub.

        IF t_ter[] IS NOT INITIAL.

          MODIFY TABLE t_ter FROM w_ter TRANSPORTING docnum_cte_sub.
          MODIFY zib_cte_dist_ter FROM TABLE t_ter.
          COMMIT WORK.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

  "Elimina linha do ALV Gerados, onde todos os documentos foram estornados
  IF r_r1 IS NOT INITIAL OR r_r2 IS NOT INITIAL.
    DELETE t_report WHERE ov_sub IS INITIAL AND fat_sub IS INITIAL  AND docnum_sub IS INITIAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_mensagem .
  DATA: lr_content TYPE REF TO cl_salv_form_element.
  DATA: lr_selections TYPE REF TO cl_salv_selections.

  cl_salv_table=>factory(
    EXPORTING
      list_display = 'X'
    IMPORTING
      r_salv_table = gr_table
    CHANGING
      t_table      = t_outtab_mess ).

  columns = gr_table->get_columns( ).

  PERFORM change_column_name.


  gr_table->set_screen_popup(
    start_column = 1
    end_column   = 100
    start_line   = 1
    end_line     = 20 ).

  "lr_selections = gr_table->get_selections( ).
  "lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  gr_table->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COLUMN_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_column_name .
  DATA not_found TYPE REF TO cx_salv_not_found.
  TRY.
      column = columns->get_column( 'STATUS' ).
      column->set_short_text( 'Status' ).
      column->set_medium_text( 'Status' ).
      column->set_long_text( 'Status' ).

      column = columns->get_column( 'MSG' ).
      column->set_short_text( 'Mensagem.' ).
      column->set_medium_text( 'Mensagem' ).
      column->set_long_text( 'Mensagem' ).

    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.
ENDFORM.
