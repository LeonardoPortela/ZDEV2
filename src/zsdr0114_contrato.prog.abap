*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Contratos Prestação Serviços Fretes                     *
* Transação..:                                                         *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
*11.12.20|JALEXANDRE |         | Desenvolvimento Inicial              *
*----------------------------------------------------------------------*
REPORT zsdr0114_contrato.

TABLES: t001, kna1, zsdt0244.

*-------------------------------------------------------------------
"Declaração de Variáveis
*-------------------------------------------------------------------
DATA: v_ucomm  TYPE sy-ucomm,
      v_valida TYPE c,
      v_salvou TYPE c.

*-------------------------------------------------------------------
"Declaração de tipos
*-------------------------------------------------------------------
TYPES: BEGIN OF ty_tela.
         INCLUDE TYPE zsdt0244.
         TYPES: wgbez TYPE t023t-wgbez60.
TYPES: embarcado_unid    TYPE c LENGTH 3.
TYPES: desc_cliente      TYPE kna1-name1.
TYPES: cliente_endereco  TYPE kna1-stras.
TYPES: cliente_local     TYPE kna1-ort01.
TYPES: cliente_uf        TYPE kna1-regio.
TYPES: desc_empresa      TYPE t001-butxt.
TYPES: desc_unidade      TYPE c LENGTH 10.
TYPES: desc_uni_tar      TYPE c LENGTH 10.
TYPES: desc_coleta       TYPE lfa1-name1.
TYPES: desc_entrega      TYPE kna1-name1.
TYPES: cliente_tela      TYPE kunnr.
TYPES: desc_zterm        TYPE text1_052.
TYPES: desc_rem          TYPE lfa1-name1.
TYPES: desc_dest         TYPE kna1-name1.

TYPES: END OF ty_tela.

TYPES: BEGIN OF y_filtros,
         parametro  TYPE string,
         valor      TYPE string,
         direita    TYPE string,
         parametro2 TYPE string,
         valor2     TYPE string,
       END OF y_filtros.

TYPES: BEGIN OF y_saida,
         selecionar     TYPE  zde_selecionar,
         bukrs          TYPE  bukrs,
         desc_empresa   TYPE  butxt,
         id_ctr         TYPE  zde_id_ctr,
         ano            TYPE  zde_ano,
         status         TYPE  zdemm_status_id_ctr,
         status_desc    TYPE  c LENGTH 10,
         id_cliente     TYPE  kunnr,
         desc_cliente   TYPE  name1,
         cliente_cnpj   TYPE  zde_cliente_cnpj,
         cliente_cpf    TYPE  zde_cliente_cpf,
         cliente_ie     TYPE  zde_cliente_ie, "BUG SOLTO 161256 - MMSILVA - 20.12.2024
         gr_mercadoria  TYPE  matkl,
         wgbez          TYPE  t023t-wgbez60,
         quantidade     TYPE  zde_qtd_total,
         unid_quant     TYPE  zde_unid_quant,
         tolerancia     TYPE  zde_ctr_toler,
         total_embarque TYPE  zde_ctr_saldo,
         unid_embarque  TYPE 	zde_unid_quant,
         tarifa         TYPE  zde_valor_tarifa,
         unid_tarifa    TYPE  zde_unid_tarifa,
         zterm          TYPE  dzterm,
         desc_zterm     TYPE  text1_052,
         pc_codigo      TYPE  lifnr,
         desc_coleta    TYPE 	name1,
         pc_cnpj        TYPE  zde_pc_cnpj,
         pc_cpf         TYPE  zde_pc_cpf,
         pc_ie          TYPE  zde_pc_ie,
         pc_endereco    TYPE  zde_endereco,
         pc_local       TYPE  zde_local,
         pc_uf          TYPE  zde_uf,
         pc_zona        TYPE  zde_pc_zona,
         lr_codigo      TYPE  kunnr,
         desc_entrega   TYPE  name1,
         lr_cnpj        TYPE  zde_lr_cnpj,
         lr_cpf         TYPE  zde_lr_cpf,
         lr_ie          TYPE  zde_lr_ie,
         lr_endereco    TYPE  zde_endereco,
         lr_local       TYPE  zde_local,
         lr_uf          TYPE  zde_uf,
         lr_zona        TYPE  zde_lr_zona,
         route          TYPE  route,
         reme_codigo    TYPE  lifnr,
         reme_cnpj      TYPE  zde_cnpj_rem,
         reme_cpf       TYPE  zde_cpf_rem,
         reme_ie        TYPE  zde_ie_rem,
         reme_endereco  TYPE  zde_endereco,
         reme_local     TYPE  zde_local,
         reme_uf        TYPE  zde_uf,
         dest_codigo    TYPE  kunnr,
         dest_cnpj      TYPE  zde_cnpj_dest,
         dest_cpf       TYPE  zde_cpf_dest,
         dest_ie        TYPE  zde_ie_dest,
         dest_endereco  TYPE  zde_endereco,
         dest_local     TYPE  zde_local,
         dest_uf        TYPE  zde_uf,
         observacao     TYPE  bapi_msg,
       END OF y_saida.
*-------------------------------------------------------------------
"Declaração de Tabela Interna
*-------------------------------------------------------------------
DATA: t_filtro    TYPE TABLE OF y_filtros,
      t_fieldcat2 TYPE lvc_t_fcat,
      t_saida     TYPE TABLE OF y_saida, "zsds022.
      t_dd07t     TYPE TABLE OF dd07t.


*---------------------------------------------------------------------
"Declaração de Estruturas
*---------------------------------------------------------------------
DATA: wa_tela     TYPE ty_tela,
      wa_tela_old TYPE ty_tela.

*---------------------------------------------------------------------
"Definição de Classes
*---------------------------------------------------------------------
CLASS lcl_event_receiver DEFINITION.
* seção publica
  PUBLIC SECTION.
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
"Implementação de Classe
*---------------------------------------------------------------------
CLASS lcl_event_receiver IMPLEMENTATION.
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

*---------------------------------------------------------------------
"Declaração de Classes
*---------------------------------------------------------------------
DATA: v_grid           TYPE REF TO cl_gui_alv_grid,         "#EC NEEDED
      v_event_receiver TYPE REF TO lcl_event_receiver,      "#EC NEEDED
      v_titulo         TYPE sy-title.

*&---------------------------------------------------------------------*
*Tela de Parâmetros 1
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

"Campo para todas as opções
SELECT-OPTIONS: s_bu_4  FOR t001-bukrs DEFAULT '0001' NO INTERVALS NO-EXTENSION     MODIF ID all.

"Campos da opção Listar Contratos
SELECT-OPTIONS: s_ku_4  FOR kna1-kunnr MODIF ID lst,
                s_ano_4 FOR zsdt0244-ano   NO INTERVALS NO-EXTENSION     MODIF ID lst,
                s_id_4 FOR zsdt0244-id_ctr NO INTERVALS NO-EXTENSION     MODIF ID lst.

"Campos da opção Criar contrato novo
SELECT-OPTIONS:  s_ku_2  FOR kna1-kunnr NO INTERVALS NO-EXTENSION MODIF ID nov,
                 s_dt_2  FOR sy-datum DEFAULT sy-datum NO INTERVALS NO-EXTENSION   NO-DISPLAY. "  MODIF ID nov

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK bl0  WITH FRAME TITLE text-042.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(22) text-t01.
SELECTION-SCREEN POSITION 2.
PARAMETERS: r_lst RADIOBUTTON GROUP 1 USER-COMMAND mt DEFAULT 'X'.

SELECTION-SCREEN COMMENT 32(33) text-t02.
SELECTION-SCREEN POSITION 29.
PARAMETERS: r_nov RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl0.


SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD'.
SELECTION-SCREEN END OF BLOCK bl2.


**&---------------------------------------------------------------------*
**Inicialização
**&---------------------------------------------------------------------*
INITIALIZATION.

**&---------------------------------------------------------------------*
**Tratar campos da tela seleção
**&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM zf_monta_tela.


**&---------------------------------------------------------------------*
**Início do processamento
**&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_limpar_variaveis.
  PERFORM zf_validar_campos.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_tela .

  LOOP AT SCREEN.

    IF r_lst IS NOT INITIAL.

      IF screen-group1 EQ 'NOV'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.
      REFRESH: s_dt_2, s_ku_2.
    ENDIF.

    IF r_nov IS NOT INITIAL.

      IF screen-group1 EQ 'LST'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.
      REFRESH: s_ano_4, s_ku_4.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_CONTRATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_contratos .

  SELECT  *  FROM zsdt0244
    INTO TABLE @DATA(t_editar)
    WHERE bukrs      IN @s_bu_4
     AND id_ctr      IN @s_id_4
      AND id_cliente IN @s_ku_4
      AND ano        IN @s_ano_4. "sy-datum(4).

  IF t_editar[] IS INITIAL.
    MESSAGE s000(z_les) WITH text-004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "DD: textos p/valores fixos dom.(depend.idioma)
  IF t_dd07t IS INITIAL.

    SELECT * FROM dd07t
    INTO TABLE t_dd07t
      WHERE domname    = 'ZDOMM_STATUS_ID_CTR'
        AND ddlanguage = 'P'.

  ENDIF.

  REFRESH: t_saida.
  IF t_editar[] IS NOT INITIAL.
    SORT t_editar BY   bukrs id_ctr ano. "BUG - 83992 - CBRAND

    "BUG SOLTO 161256 - MMSILVA - 20.12.2024 - Inicio
    LOOP AT t_editar ASSIGNING FIELD-SYMBOL(<fs_editar>).
      IF <fs_editar>-cliente_ie IS INITIAL.

        DATA: wa_stcd3 TYPE kna1-stcd3.

        SELECT SINGLE stcd3
          FROM kna1
          WHERE lifnr = @<fs_editar>-id_cliente
          INTO @wa_stcd3.

        <fs_editar>-cliente_ie = wa_stcd3.

        CLEAR wa_stcd3.
      ENDIF.
    ENDLOOP.
    "BUG SOLTO 161256 - MMSILVA - 20.12.2024 - Fim

    MOVE-CORRESPONDING t_editar[] TO t_saida[].

    LOOP AT t_saida  ASSIGNING FIELD-SYMBOL(<fs_saida>).

      CLEAR wa_tela.
      MOVE-CORRESPONDING <fs_saida> TO wa_tela.

      PERFORM zf_atualiza_tela .

      MOVE-CORRESPONDING wa_tela TO <fs_saida>.

      READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY valpos = <fs_saida>-status.
      IF sy-subrc IS INITIAL.
        <fs_saida>-status_desc = w_dd07t-ddtext.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.

  SET PF-STATUS 'PF_0900'.
  SET TITLEBAR 'TITULO_0900'.

  PERFORM f_exibe_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA: wa_variant TYPE disvariant,
        wa_layout  TYPE lvc_s_layo.

  wa_layout-zebra      = abap_true.  "Código Zebrado
  wa_layout-cwidth_opt = abap_true.  "Ajusta tamanho na coluna
  wa_layout-box_fname  = abap_true.  "
  wa_layout-sel_mode   = 'A'.        "

  v_titulo  = sy-title.

  wa_variant-report   = sy-repid.
  wa_variant-variant  = p_layout.

  "Criar tela padrão Amaggi
  IF v_grid IS INITIAL.

    REFRESH t_fieldcat2.
    PERFORM zf_montar_fieldcat     CHANGING t_saida t_fieldcat2.
    PERFORM zf_ajuste_descr_campos CHANGING t_fieldcat2.


    PERFORM zf_split_screen        CHANGING v_grid.

    CREATE OBJECT v_event_receiver.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER v_event_receiver->handle_toolbar FOR v_grid.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER v_event_receiver->handle_command_grid FOR v_grid.

    CALL METHOD v_grid->set_table_for_first_display
      EXPORTING
        is_variant      = wa_variant
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = t_saida[]
        it_fieldcatalog = t_fieldcat2[].

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_command  USING    p_e_ucomm.

  CASE v_ucomm.

    WHEN 'CRIAR'.
      CLEAR  wa_tela.
      CALL SCREEN 0700.
    WHEN 'VISUALIZAR'.
      PERFORM f_valida_linhas_editar.
    WHEN 'LISTARCTE'.
      PERFORM f_listar_cte_atribuidos.
    WHEN 'ATUALIZAR'.
      PERFORM zf_buscar_contratos.
  ENDCASE.

  IF v_grid IS NOT INITIAL.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_DESCRICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_descricao .

*----------------------------------------------------------------------*
* Dados Empresa
*----------------------------------------------------------------------*
* Descrição da Empresa
  IF wa_tela-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt FROM t001
      INTO (wa_tela-desc_empresa)
      WHERE bukrs = wa_tela-bukrs.
  ENDIF.

*----------------------------------------------------------------------*
*Dados do Cliente
*----------------------------------------------------------------------*
* Descrição Cliente
  IF wa_tela-id_cliente IS NOT INITIAL.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 FROM kna1
      INTO ( wa_tela-desc_cliente, wa_tela-cliente_cnpj, wa_tela-cliente_cpf, wa_tela-cliente_ie )
      WHERE kunnr = wa_tela-id_cliente.

  ENDIF.

* Descrição ZTERM
  IF wa_tela-zterm IS NOT INITIAL.

    SELECT SINGLE text1 FROM t052u
      INTO (wa_tela-desc_zterm)
      WHERE spras = sy-langu
        AND zterm = wa_tela-zterm.

  ENDIF.

* Denominações para grupos de mercadoria
  IF wa_tela-gr_mercadoria IS NOT INITIAL.
    SELECT SINGLE wgbez60 FROM t023t
      INTO ( wa_tela-wgbez )
      WHERE spras = sy-langu
       AND matkl = wa_tela-gr_mercadoria.
  ENDIF.

*----------------------------------------------------------------------*
* Dados da Coleta
*----------------------------------------------------------------------*
* Descrição do local de coleta
  IF wa_tela-pc_codigo IS NOT INITIAL.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 FROM lfa1
      INTO ( wa_tela-desc_coleta, wa_tela-pc_cnpj, wa_tela-pc_cpf, wa_tela-pc_ie )
      WHERE lifnr = wa_tela-pc_codigo. "kunnr = wa_tela-id_cliente.
  ENDIF.

*----------------------------------------------------------------------*
* Dados do Local de entrega
*----------------------------------------------------------------------*
  IF wa_tela-lr_codigo IS NOT INITIAL.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 FROM kna1
  INTO ( wa_tela-desc_entrega, wa_tela-lr_cnpj, wa_tela-lr_cpf, wa_tela-lr_ie )
  WHERE kunnr = wa_tela-lr_codigo. "wa_tela-id_cliente.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_LINHAS_EDITAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_valida_linhas_editar .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( t_row_no ) > 1.
    MESSAGE i000(z_mm) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR: wa_tela.
  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.
    MOVE-CORRESPONDING <fs_saida> TO wa_tela.

    "Preenche campos de descrição
    PERFORM zf_atualiza_tela .

  ENDLOOP.

  CALL SCREEN 0700.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM f_toolbar_grid  CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA:       wa_toolbar TYPE stb_button.

  CLEAR wa_toolbar.
  MOVE: 'ATUALIZAR' TO wa_toolbar-function,
  icon_refresh TO wa_toolbar-icon,
  '' TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.


  CLEAR wa_toolbar.
  MOVE: 'VISUALIZAR' TO wa_toolbar-function,
  icon_display TO wa_toolbar-icon,
  '' TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'LISTARCTE' TO wa_toolbar-function,
  icon_transport TO wa_toolbar-icon,
  '' TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.

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
*&      Module  STATUS_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0700 OUTPUT.

  DATA: ld_field   TYPE vrm_id,
        it_listbox TYPE vrm_values,
        wa_listbox LIKE LINE OF it_listbox,
        l_valpos   TYPE n LENGTH 4.

  SET PF-STATUS 'PF_0700'.
  SET TITLEBAR 'TITULO_700'.

  IF v_ucomm = 'VISUALIZAR'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  IF v_ucomm = 'EDITAR'.
    LOOP AT SCREEN.
      IF screen-group2 = 'UPD'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF v_ucomm = 'CRIAR'.
    LOOP AT SCREEN.
      IF screen-group1 = 'CRE'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
* ListBox - STATUS
*----------------------------------------------------------------------*
  "DD: textos p/valores fixos dom.(depend.idioma)
  IF t_dd07t IS INITIAL.

    SELECT * FROM dd07t
    INTO TABLE t_dd07t
      WHERE domname    = 'ZDOMM_STATUS_ID_CTR'
        AND ddlanguage = 'P'.

  ENDIF.

  IF t_dd07t[] IS INITIAL.

    LOOP AT t_dd07t INTO DATA(wa_dd07t).
      IF wa_dd07t-valpos IS NOT INITIAL.
        wa_listbox-key   = wa_dd07t-valpos.
        wa_listbox-text  = wa_dd07t-ddtext.
        APPEND wa_listbox TO it_listbox.
      ENDIF.
    ENDLOOP.

    ld_field = 'WA_TELA-STATUS'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = ld_field
        values = it_listbox.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0700 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.
    WHEN 'EDITAR'.
      IF v_ucomm = sy-ucomm.
        v_ucomm = 'VISUALIZAR'.
      ELSE.
        PERFORM zf_verificar_mod_contrato .
        v_ucomm = sy-ucomm.
      ENDIF.
    WHEN 'SALVAR'.
      PERFORM zf_novo_contrato.
    WHEN 'CRIAR'.
      PERFORM zf_novo_contrato.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_EDITAR_zsdt0244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_editar_zsdt0244 .

  CLEAR: v_salvou.
  IF wa_tela-quantidade IS INITIAL.
    MESSAGE e000(z_les) WITH text-010." DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  wa_tela-mandt = sy-mandt.
  MODIFY zsdt0244 FROM wa_tela.
  COMMIT WORK.
  MESSAGE s000(z_les) WITH text-009. " DISPLAY LIKE 'S'.
  v_salvou = abap_true.

  READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WITH KEY selecionar = abap_true.

  IF sy-subrc IS INITIAL.
    <fs_saida>-quantidade = wa_tela-quantidade.
  ENDIF.

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
  CALL METHOD v_grid->refresh_table_display
    EXPORTING
      is_stable      = l_stable
      i_soft_refresh = l_soft_refresh
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_GRID  text
*----------------------------------------------------------------------*
FORM zf_split_screen  CHANGING p_alv TYPE REF TO cl_gui_alv_grid.

  DATA: l_header       TYPE REF TO cl_gui_splitter_container,
        l_picture      TYPE REF TO cl_gui_picture,
        l_dg_dyndoc_id TYPE REF TO cl_dd_document,
        l_html         TYPE REF TO  cl_gui_html_viewer,
        l_split        TYPE REF TO  cl_gui_splitter_container.

  DATA: t_text_table_f  TYPE sdydo_text_table,
        t_text_table_v  TYPE sdydo_text_table,
        t_text_table_f2 TYPE sdydo_text_table,
        t_text_table_v2 TYPE sdydo_text_table.

  CHECK l_split IS INITIAL.

  l_split = NEW #( parent = cl_gui_container=>screen0 rows = 2 columns = 1 ).

  DATA(v_container_html) = l_split->get_container( EXPORTING row = 1 column = 1 ).

*----------------------------------------------------------------------*
* Header
*----------------------------------------------------------------------*
  l_header = NEW #( parent = v_container_html rows = 1 columns = 2 ).

  DATA(l_header_texto) = l_header->get_container( EXPORTING row = 1 column = 1 ).

  l_header->set_column_width( EXPORTING id = 1 width = 40 ).

  "Logo
  DATA(l_header_logo) = l_header->get_container( EXPORTING row = 1 column = 2 ).

  l_picture = NEW #( parent = l_header_logo ).

  DATA: l_url TYPE char255.

  PERFORM zf_buscar_imagem_url USING 'LOGO_NOVO' CHANGING l_url.

  l_picture->load_picture_from_url( EXPORTING url = l_url ).

  l_picture->set_display_mode( EXPORTING display_mode = l_picture->display_mode_fit_center ).

*----------------------------------------------------------------------*
* Item
*----------------------------------------------------------------------*

  DATA(v_item_grid) = l_split->get_container( EXPORTING row = 2 column = 1 ).

  l_split->set_row_height( EXPORTING id = 1 height = 15 ).

  p_alv = NEW #( i_parent = v_item_grid ).

  l_dg_dyndoc_id = NEW #( style = 'ALV_TO_HTML' background_color = 7 ).
  l_dg_dyndoc_id->initialize_document( ).

  l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 1 border = '0' width = '100%' IMPORTING table = DATA(table_element) ).

*----------------------------------------------------------------------*
* Preencher Titulo
*----------------------------------------------------------------------*
  IF v_titulo IS NOT INITIAL.
    table_element->add_column( IMPORTING column = DATA(column) ).
    table_element->set_column_style( EXPORTING col_no = 1 sap_style = cl_dd_document=>heading sap_align = 'CENTER' ).
    column->add_text( EXPORTING text = CONV #( v_titulo ) sap_style = 'HEADING' ).
  ENDIF.

*----------------------------------------------------------------------*
* Mostra dados adicionais
*----------------------------------------------------------------------*
  IF t_filtro[] IS NOT INITIAL.

    l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 4 border = '0' width = '100%' IMPORTING table = DATA(table_element_linhas) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_1) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_2) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_3) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_4) ).

    table_element_linhas->set_column_style( EXPORTING col_no = 1 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 2 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).
    table_element_linhas->set_column_style( EXPORTING col_no = 3 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 4 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).

    LOOP AT t_filtro INTO DATA(w_filtro).

      APPEND w_filtro-parametro TO t_text_table_f.
      APPEND w_filtro-valor TO t_text_table_v.

      APPEND w_filtro-parametro2 TO t_text_table_f2.
      APPEND w_filtro-valor2 TO t_text_table_v2.

      column_1->add_text( EXPORTING text_table = t_text_table_f  fix_lines = abap_true ).
      column_2->add_text( EXPORTING text_table = t_text_table_v  fix_lines = abap_true ).
      column_3->add_text( EXPORTING text_table = t_text_table_f2 fix_lines = abap_true ).
      column_4->add_text( EXPORTING text_table = t_text_table_v2 fix_lines = abap_true ).

      CLEAR: t_text_table_f[], t_text_table_v[], t_text_table_f2[], t_text_table_v2[].

    ENDLOOP.

  ENDIF.

  l_html = NEW #( parent = l_header_texto ).

  l_dg_dyndoc_id->merge_document( ).

  l_dg_dyndoc_id->html_control = l_html.

  l_dg_dyndoc_id->display_document( EXPORTING reuse_control = 'X' parent = l_header_texto ).


  CALL METHOD l_split->set_row_height
    EXPORTING
      id     = 1
      height = 20.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_IMAGEM_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1029   text
*      <--P_L_URL  text
*----------------------------------------------------------------------*
FORM zf_buscar_imagem_url   USING    i_nome_logo
                         CHANGING r_url.


  TYPES: BEGIN OF ty_graphic_table,
           line(255) TYPE x,
         END OF ty_graphic_table.

  DATA: graphic_table TYPE TABLE OF ty_graphic_table.

  DATA: l_graphic_xstr TYPE xstring.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = i_nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  DATA(graphic_size) = xstrlen( l_graphic_xstr ).
  DATA(l_graphic_conv) = graphic_size.
  DATA(l_graphic_offs) = 0.
  WHILE l_graphic_conv > 255.
    APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(255) ) TO graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(l_graphic_conv) ) TO graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = r_url.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_BUSCAR_DESCRICAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_buscar_descricao INPUT.
*  PERFORM ZF_BUSCAR_DESCRICAO.

  IF wa_tela <> wa_tela_old.
    PERFORM zf_atualiza_tela.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_atualiza_tela .

*----------------------------------------------------------------------*
* Dados Empresa
*----------------------------------------------------------------------*
* Descrição da Empresa
  IF wa_tela-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt FROM t001
      INTO (wa_tela-desc_empresa)
      WHERE bukrs = wa_tela-bukrs.
  ENDIF.

*----------------------------------------------------------------------*
* Dados do Cliente
*----------------------------------------------------------------------*
  IF wa_tela-id_cliente IS NOT INITIAL.

    SELECT SINGLE * "STCD1, STCD2, NAME1
      FROM kna1
     WHERE kunnr = @wa_tela-id_cliente
      INTO @DATA(wa_kna1_c).

    IF r_nov = 'X'.
      wa_tela-desc_cliente      = wa_kna1_c-name1.
      wa_tela-cliente_cnpj      = wa_kna1_c-stcd1.
      wa_tela-cliente_cpf       = wa_kna1_c-stcd2.
      wa_tela-cliente_endereco  = wa_kna1_c-stras.
      wa_tela-cliente_local     = wa_kna1_c-ort01.
*    wa_tela-cliente__zona      = wa_kna1_2-lzone.
      wa_tela-cliente_uf        = wa_kna1_c-regio.
      wa_tela-cliente_ie        = wa_kna1_c-stcd3. "BUG SOLTO 161256 - MMSILVA - 20.12.2024

    ELSE.
      wa_tela-desc_cliente      = wa_kna1_c-name1.
      wa_tela-cliente_endereco  = wa_kna1_c-stras.
      wa_tela-cliente_local     = wa_kna1_c-ort01.
*    wa_tela-cliente__zona      = wa_kna1_2-lzone.
      wa_tela-cliente_uf        = wa_kna1_c-regio.
    ENDIF.

  ENDIF.

  IF wa_tela-gr_mercadoria IS NOT INITIAL.

    SELECT SINGLE wgbez
      FROM t023t
     WHERE spras = @sy-langu
       AND matkl = @wa_tela-gr_mercadoria
      INTO @DATA(vl_wgbez).

    wa_tela-wgbez = vl_wgbez.
  ENDIF.

*----------------------------------------------------------------------*
* Dados do Ponto de Coleta
*----------------------------------------------------------------------*
  IF wa_tela-pc_codigo IS NOT INITIAL.

    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = @wa_tela-pc_codigo
      INTO @DATA(wa_lfa1).

    IF r_nov = 'X'.

      wa_tela-desc_coleta = wa_lfa1-name1.
      wa_tela-pc_cpf      = wa_lfa1-stcd2.
      wa_tela-pc_cnpj     = wa_lfa1-stcd1.
      wa_tela-pc_ie       = wa_lfa1-stcd3. "BUG SOLTO 161256 - MMSILVA - 20.12.2024
      wa_tela-pc_endereco = wa_lfa1-stras.
      wa_tela-pc_local    = wa_lfa1-ort01.
      wa_tela-pc_zona     = wa_lfa1-lzone.
      wa_tela-pc_uf       = wa_lfa1-regio.

    ELSE.
      wa_tela-desc_coleta = wa_lfa1-name1.
      IF wa_tela-pc_ie IS INITIAL.
        wa_tela-pc_ie = wa_lfa1-stcd3.
      ENDIF.
    ENDIF.
  ENDIF.


*----------------------------------------------------------------------*
* Dados do Local de entrega
*----------------------------------------------------------------------*
  IF wa_tela-lr_codigo IS NOT INITIAL.

    SELECT SINGLE *
      FROM kna1
     WHERE kunnr = @wa_tela-lr_codigo
      INTO @DATA(wa_kna1_2).

    IF r_nov = 'X'.
      wa_tela-desc_entrega = wa_kna1_2-name1.
      wa_tela-lr_cpf       = wa_kna1_2-stcd2.
      wa_tela-lr_cnpj      = wa_kna1_2-stcd1.
      wa_tela-lr_ie        = wa_kna1_2-stcd3. "BUG SOLTO 161256 - MMSILVA - 20.12.2024
      wa_tela-lr_endereco  = wa_kna1_2-stras.
      wa_tela-lr_local     = wa_kna1_2-ort01.
      wa_tela-lr_zona      = wa_kna1_2-lzone.
      wa_tela-lr_uf        = wa_kna1_2-regio.

    ELSE.
      wa_tela-desc_entrega = wa_kna1_2-name1.
      IF wa_tela-lr_ie IS INITIAL.
        wa_tela-lr_ie = wa_kna1_2-stcd3.
      ENDIF.
    ENDIF.

  ENDIF.

  IF wa_lfa1-lzone IS NOT INITIAL AND wa_kna1_2-lzone IS NOT INITIAL AND r_nov = 'X'.

    SELECT SINGLE *
      FROM trolz
     WHERE aland = 'BR'
       AND azone = @wa_lfa1-lzone
       AND lland = 'BR'
       AND lzone = @wa_kna1_2-lzone
       INTO @DATA(wa_trolz).

    wa_tela-route = wa_trolz-route.
  ENDIF.

  IF wa_tela-zterm IS NOT INITIAL.

    SELECT SINGLE *
      FROM t052u
     WHERE spras = @sy-langu
       AND zterm = @wa_tela-zterm
      INTO @DATA(wa_t052u).

    wa_tela-desc_zterm = wa_t052u-text1.

  ENDIF.

*&---------------------------------------------------------------------*
  "Mantém a unida de embarque igual a unidade de descarga
*&---------------------------------------------------------------------*
  wa_tela-unid_embarque = wa_tela-unid_quant.

*** US - 82085 - Inicio - CBRAND
*----------------------------------------------------------------------*
* Seleção de dados para Remetente
*----------------------------------------------------------------------*
  IF wa_tela-reme_codigo IS NOT INITIAL.

    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = @wa_tela-reme_codigo
      INTO @DATA(wa_lfa1_reme).

    IF r_nov = 'X'.
      wa_tela-desc_rem      = wa_lfa1_reme-name1.
      wa_tela-reme_cpf      = wa_lfa1_reme-stcd2.
      wa_tela-reme_cnpj     = wa_lfa1_reme-stcd1.
      wa_tela-reme_ie       = wa_lfa1_reme-stcd3. "BUG SOLTO 161256 - MMSILVA - 20.12.2024
      wa_tela-reme_endereco = wa_lfa1_reme-stras.
      wa_tela-reme_local    = wa_lfa1_reme-ort01.
      wa_tela-reme_uf       = wa_lfa1_reme-regio.
    ELSE.
      wa_tela-desc_rem      = wa_lfa1_reme-name1.
      IF wa_tela-reme_ie IS INITIAL.
        wa_tela-reme_ie = wa_lfa1_reme-stcd3.
      ENDIF.
    ENDIF.

  ENDIF.
*----------------------------------------------------------------------*
* Seleção de dados para Destinatário
*----------------------------------------------------------------------*
  IF wa_tela-dest_codigo IS NOT INITIAL.
    SELECT SINGLE *
      FROM kna1
     WHERE kunnr = @wa_tela-dest_codigo
      INTO @DATA(wa_kna1_dest).

    IF r_nov = 'X'.
      wa_tela-desc_dest      = wa_kna1_dest-name1.
      wa_tela-dest_cpf       = wa_kna1_dest-stcd2.
      wa_tela-dest_cnpj      = wa_kna1_dest-stcd1.
      wa_tela-dest_ie        = wa_kna1_dest-stcd3. "BUG SOLTO 161256 - MMSILVA - 20.12.2024
      wa_tela-dest_endereco  = wa_kna1_dest-stras.
      wa_tela-dest_local     = wa_kna1_dest-ort01.
      wa_tela-dest_uf        = wa_kna1_dest-regio.
    ELSE.
      wa_tela-desc_dest      = wa_kna1_dest-name1.
      IF wa_tela-dest_ie IS INITIAL.
        wa_tela-dest_ie = wa_kna1_dest-stcd3.
      ENDIF.
    ENDIF.
  ENDIF.

*** US - 82085 - Fim - CBRAND

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_NOVO_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_novo_contrato .

  DATA: lv_nr_range_nr TYPE inri-nrrangenr,
        lv_object      TYPE inri-object,
        lv_quantity    TYPE inri-quantity,
        lv_number      TYPE i,
        lv_erro        TYPE c.

*----------------------------------------------------------------------*
* Efetuar Validação
*----------------------------------------------------------------------*
  CLEAR:  lv_erro, lv_nr_range_nr, lv_object, lv_quantity, lv_number.
  PERFORM zf_mensagem_validacao USING lv_erro.

  IF lv_erro IS INITIAL.

    IF wa_tela-id_ctr IS INITIAL.
*----------------------------------------------------------------------*
* Gerar Número de contrato
*----------------------------------------------------------------------*
      lv_nr_range_nr = '01'.
      lv_object      = 'ZSD_CTR_FP'.
      lv_quantity    = '00000000000000000001'.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = lv_nr_range_nr
          object                  = lv_object
          quantity                = lv_quantity
        IMPORTING
          number                  = lv_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      wa_tela-id_ctr = lv_number.
      CONDENSE wa_tela-id_ctr NO-GAPS.

    ENDIF.

    IF wa_tela-id_ctr  IS NOT INITIAL AND wa_tela <> wa_tela_old.

      DATA: wa_zsdt0244     TYPE zsdt0244.

      IF wa_tela-status = 0000.
        wa_tela-status = '0001'.
      ENDIF.
      MOVE-CORRESPONDING wa_tela TO wa_zsdt0244.

*----------------------------------------------------------------------*
* Dados do Modificador
*----------------------------------------------------------------------*
      wa_zsdt0244-usnam        = sy-uname.
      wa_zsdt0244-data_atual   = sy-datum.
      wa_zsdt0244-hora_atual   = sy-uzeit.
      MODIFY zsdt0244 FROM wa_zsdt0244.
      COMMIT WORK.


      v_ucomm = 'VISUALIZAR'.
      "PERFORM zf_buscar_contratos. -BUG - 83992 - Buscava todos os contratos novamente.

      MESSAGE s000(z_les) WITH text-009 DISPLAY LIKE 'S'.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MENSAGEM_VALIDACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA(L_ERRO)  text
*----------------------------------------------------------------------*
FORM zf_mensagem_validacao  USING p_erro.

*----------------------------------------------------------------------*
* "Dados do Cliente
*----------------------------------------------------------------------*
  IF wa_tela <> wa_tela_old.

    SELECT SINGLE *
       FROM knb1
      WHERE kunnr = @wa_tela-id_cliente
        AND bukrs = @wa_tela-bukrs
       INTO @DATA(wa_knb1).

    IF sy-subrc NE 0.
      MESSAGE s000(z_les) WITH text-007 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

*----------------------------------------------------------------------*
* "Dados do contrato
*----------------------------------------------------------------------*

    IF wa_tela-id_cliente    IS INITIAL.
      MESSAGE s000(z_les) WITH 'ID do cliente' 'não informado!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF wa_tela-gr_mercadoria IS INITIAL.
      MESSAGE s000(z_les) WITH 'Grupo de mercadoria' 'não informado!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF   wa_tela-quantidade    IS INITIAL.
      MESSAGE s000(z_les) WITH 'Quantidade' 'não informado!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF wa_tela-unid_quant    IS INITIAL.
      MESSAGE s000(z_les) WITH 'Unidade da quantidade' 'não informada!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
*      RETURN.
    ENDIF.
    IF wa_tela-tarifa         IS INITIAL.
      MESSAGE s000(z_les) WITH 'Tarifa' 'não informada!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.
    IF wa_tela-unid_tarifa   IS INITIAL.
      MESSAGE s000(z_les) WITH 'Unidade da tarifa' 'não informada!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.
    IF wa_tela-zterm         IS INITIAL.
      MESSAGE s000(z_les) WITH 'Condição Receb' 'não informada!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.
    IF wa_tela-pc_codigo     IS INITIAL.
      MESSAGE s000(z_les) WITH 'Cód Ponto Coleta' 'não informado!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.
    IF wa_tela-lr_codigo     IS INITIAL.
      MESSAGE s000(z_les) WITH 'Cód Local Entrega' 'não informado!'
      DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

  ELSE.
*  não ocorreu alteração do contrato
*    p_erro = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICAR_MOD_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_verificar_mod_contrato .
  CLEAR: wa_tela_old.
  wa_tela_old = wa_tela.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_validar_campos .

*----------------------------------------------------------------------*
* "Dados do contrato
*----------------------------------------------------------------------*
  IF s_bu_4[] IS INITIAL.
    MESSAGE s000(z_les) WITH 'Código da Empresa Prest. Serviço não informado!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
  "Listar Contratos
*----------------------------------------------------------------------*
  IF r_lst IS NOT INITIAL.

    IF s_id_4[] IS INITIAL.

      IF s_ku_4[] IS INITIAL.
        MESSAGE s000(z_les) WITH 'Código do cliente não informado!'
        DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    " Lista Contratos Criados
    CLEAR v_ucomm.
    PERFORM zf_buscar_contratos.
    IF t_saida[] IS NOT INITIAL.
      CALL SCREEN 0900.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
  " Criar Novo Contrato
*----------------------------------------------------------------------*
  IF r_nov IS NOT INITIAL.

    IF s_ku_2[] IS INITIAL.
      MESSAGE s000(z_les) WITH 'Código do cliente obrigatório!'
      DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*----------------------------------------------------------------------*
* "Dados do Cliente
*----------------------------------------------------------------------*
    SELECT SINGLE *
       FROM knb1
      WHERE bukrs IN @s_bu_4
        AND kunnr IN @s_ku_2
       INTO @DATA(wa_knb1).

    IF sy-subrc NE 0.
      MESSAGE s000(z_les) WITH text-007 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.


    " Criar Novo Contrato
    v_ucomm = 'CRIAR'.
    wa_tela-bukrs      = s_bu_4-low.
    wa_tela-id_cliente = s_ku_2-low.
    wa_tela-ano        = sy-datum(4).
    wa_tela-status     = '0001'.

    "Preenche campos de descrição
    PERFORM zf_atualiza_tela .

    CALL SCREEN 0700.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_limpar_variaveis .

  CLEAR: wa_tela, wa_tela_old, v_ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FIELDCAT2  text
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
*&      Form  ZF_AJUSTE_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FIELDCAT2  text
*----------------------------------------------------------------------*
FORM zf_ajuste_descr_campos  CHANGING p_t_fieldcat2  TYPE lvc_t_fcat.

  LOOP AT p_t_fieldcat2 ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
    CASE <fs_fieldcat>-fieldname.
      WHEN 'SELECIONAR'.
        <fs_fieldcat>-no_out  = abap_true.
        <fs_fieldcat>-tech    = abap_true.
      WHEN 'STATUS'.
        <fs_fieldcat>-no_out  = abap_true.
        <fs_fieldcat>-tech    = abap_true.
      WHEN 'STATUS_DESC'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Status'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'DESC_EMPRESA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Empresa'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'DESC_CLIENTE'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Cliente'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'ZTERM'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Cond.Receb'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'DESC_ZTERM'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Receb.'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'WGBEZ'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Grp. Merc.'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.

*----------------------------------------------------------------------*
        "Campos PONTO DE COLETA
*----------------------------------------------------------------------*
      WHEN 'PC_CODIGO'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Ponto Coleta'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'DESC_COLETA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Ponto Coleta'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_CNPJ'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'CNPJ PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_CPF'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'CPF PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_ENDERECO'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Endereço PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_LOCAL'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Cidade PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_UF'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'UF PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'PC_ZONA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Zona Transp. PC'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.

*----------------------------------------------------------------------*
        "CAMPOS LOCAL DE ENTREGA
*----------------------------------------------------------------------*
      WHEN 'LR_CODIGO'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Local Entrega'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'DESC_ENTREGA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Descrição Local Entrega'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_CNPJ'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'CNPJ LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_CPF'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'CPF LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_ENDERECO'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Endereço LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_LOCAL'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Cidade LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_UF'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'UF LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'LR_ZONA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Zona Transp. LR'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'UNID_QUANT'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Unid.Qtde'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'UNID_TARIFA'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Unid.Tarifa'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN 'UNID_EMBARQUE'.
        <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l = 'Unid.Emb.'.
        <fs_fieldcat>-reptext   = <fs_fieldcat>-coltext   = <fs_fieldcat>-scrtext_l.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LISTAR_CTE_ATRIBUIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_listar_cte_atribuidos .


  DATA:
    l_columns   TYPE REF TO cl_salv_columns_table,
    l_column    TYPE REF TO cl_salv_column,
    t_table     TYPE REF TO cl_salv_table,
    l_functions TYPE REF TO cl_salv_functions,
    l_display   TYPE REF TO cl_salv_display_settings,
    l_message   TYPE REF TO cx_salv_msg,
    l_not_found TYPE REF TO cx_salv_not_found.

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF lines( t_row_no ) < 1.
    MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( t_row_no ) > 1.
    MESSAGE i000(z_mm) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR: wa_tela.
  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF sy-subrc IS INITIAL.

      SELECT id_ctr, ano, chave_xml_cte, numr_cte, placa_cav, numr_serie,
        dt_emissao, qt_carga_cte, meins, frota
        FROM zlest0194 INTO TABLE @DATA(t_lista_cte)
        WHERE id_ctr = @<fs_saida>-id_ctr.

      IF t_lista_cte[] IS INITIAL.
        MESSAGE s000(z_mm) WITH 'Contrato não possui CT-e atríbuido' .
        RETURN.

      ELSE.

*---------------------------------------------------------------------
* Abrir ALV
*---------------------------------------------------------------------
        TRY.

            cl_salv_table=>factory( IMPORTING r_salv_table = t_table
                                     CHANGING t_table      = t_lista_cte ).

            TRY.
*---------------------------------------------------------------------
* Buscar campos
*---------------------------------------------------------------------
                l_columns = t_table->get_columns( ).

*---------------------------------------------------------------------
* Otimizar campos
*---------------------------------------------------------------------
                l_columns->set_optimize( ).

*---------------------------------------------------------------------
* Ajuste descrição de campos
*---------------------------------------------------------------------
                l_column = l_columns->get_column( 'NUMR_CTE' ).
                l_column->set_short_text( 'Nr. CT-e' ).
                l_column->set_medium_text( 'Nr. CT-e' ).
                l_column->set_long_text( 'Nr. CT-e' ).

              CATCH cx_salv_not_found INTO l_not_found.
                " error handling
            ENDTRY.

*---------------------------------------------------------------------
* Ajuste funções
*---------------------------------------------------------------------
            l_functions = t_table->get_functions( ).
            l_functions->set_all( abap_true ).

*---------------------------------------------------------------------
* Título do ALV
*---------------------------------------------------------------------
            l_display = t_table->get_display_settings( ).
            l_display->set_list_header( 'Lista de CTe atribuidos ao Contrato' ).

            IF t_table IS BOUND.
              t_table->display( ).
            ENDIF.

          CATCH cx_salv_msg INTO l_message.
            " error handling
        ENDTRY.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
