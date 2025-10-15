*  ----------------------------------------------------------------------*
*   ID........:                                                          *
*   Programa..: ZPPR019                                                  *
*   Tipo......: R - Report                                               *
*   Transação.:                                                          *
*   Descrição.: Aprovação de Alteração Lista Técnica                     *
*   Autor.....: JBARBOSA                                                 *
*   Data......: 09.09.2020                                               *
*  ----------------------------------------------------------------------*
*                       Controle de Alterações                           *
*  ----------------------------------------------------------------------*
*   Data       | Change     | Autor        | Alteração                   *
*  ----------------------------------------------------------------------*
*   09.09.20   |            |JBARBOSA      | Codificação Inicial         *
*  ----------------------------------------------------------------------*
  REPORT zppr019.

  TABLES: mast, zppt0024, stpo, zppt0025.

*  &---------------------------------------------------------------------*
*  &     Declaração de Tipos
*  &---------------------------------------------------------------------*
  TYPES: BEGIN OF y_saida,
           icon(5)         TYPE c,
           status(10)      TYPE  c, "ZDEPP_STAT_APROV,
           cod_alt         TYPE zdepp_cod_list,
           werks           TYPE  werks_d,
           matnr           TYPE  matnr,
           stlan           TYPE  stlan,
           posnr           TYPE  posnr,
           idnrk           TYPE  idnrk,
           maktx           TYPE  maktx,
           tipo            TYPE stpob-vbkz,
           qtd_antiga      TYPE zdepp_qtd_antiga,
           qtd_atual       TYPE zdepp_qtd_atual,
           status_email(5) TYPE c,
           icon_email(5)   TYPE c,
           aenam           TYPE  aenam,
           aedat           TYPE  aedat,
           hr_alteracao    TYPE  zdepp_hr_alt,

           aprovador       TYPE  zdepp_aprovador,
           dt_aprovacao    TYPE  zdepp_dt_aprov,
           hr_aprovacao    TYPE  zdepp_hr_aprov,
           color_line(4)   TYPE c,
           color_cell      TYPE lvc_t_scol,
         END OF y_saida.
*  &---------------------------------------------------------------------*
*  &     ALV
*  &---------------------------------------------------------------------*
  CLASS lcl_event_receiver DEFINITION DEFERRED.

*  &---------------------------------------------------------------------*
*   Declaração de Instância de Métodos
*  &---------------------------------------------------------------------*
  DATA: v_event_receiver  TYPE REF TO lcl_event_receiver.

*  &---------------------------------------------------------------------*
*  &  Declaração de Container
*  &---------------------------------------------------------------------*
  DATA: v_container_h TYPE REF TO cl_gui_container,
        v_container_i TYPE REF TO cl_gui_container.

*  &---------------------------------------------------------------------*
*  &  Declaração de GRID
*  &---------------------------------------------------------------------*
  DATA: v_grid     TYPE REF TO cl_gui_alv_grid.

*  &---------------------------------------------------------------------*
*  &  Docking
*  &---------------------------------------------------------------------*
  DATA: v_docking  TYPE REF TO cl_gui_docking_container,
        v_splitter TYPE REF TO cl_gui_splitter_container.

  DATA: t_zppt0024 TYPE TABLE OF zppt0024,
        t_zppt0025 TYPE TABLE OF zppt0025,
        t_saida    TYPE TABLE OF y_saida.

  DATA: t_coltab2 TYPE lvc_t_scol.

*  &---------------------------------------------------------------------*
*   Declaração de Ranges
*  &---------------------------------------------------------------------*
  DATA: r_status TYPE RANGE OF zppt0025-status WITH HEADER LINE.

*  &---------------------------------------------------------------------*
*   Declaração de Estrutura
*  &---------------------------------------------------------------------*
  DATA: w_disvariant TYPE disvariant,
        w_layout     TYPE lvc_s_layo,
        w_saida      LIKE LINE OF t_saida.

*  &---------------------------------------------------------------------*
*   Declaração de Variáveis
*  &---------------------------------------------------------------------*
  DATA: v_info TYPE c.
*  ----------------------------------------------------------------------*
*   Definição de Icones
*  ----------------------------------------------------------------------*
  CONSTANTS: BEGIN OF gc_icones,
               icon_green_light(5)   TYPE c VALUE '@08@',
               icon_yellow_light(5)  TYPE c VALUE '@09@',
               icon_red_light(5)     TYPE c VALUE '@0A@',
               icon_system_okay(5)   TYPE c VALUE '@2K@',
               icon_system_cancel(5) TYPE c VALUE '@2O@',
             END OF gc_icones.

  CONSTANTS: BEGIN OF gc,
               amarelo TYPE lvc_s_colo-col      VALUE '3',
               padrao  TYPE lvc_s_colo-col      VALUE '2',
               verde   TYPE lvc_s_colo-col      VALUE '5',
               int     TYPE lvc_s_colo-int      VALUE '1',
               inv     TYPE lvc_s_colo-inv      VALUE '0',
             END OF gc.

*  ----------------------------------------------------------------------*
*   Parâmetros de Seleção
*  ----------------------------------------------------------------------*

  SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

  SELECT-OPTIONS: s_werks  FOR mast-werks OBLIGATORY,
                  s_stlan  FOR mast-stlan,
                  s_matnr  FOR mast-matnr,
                  s_idnrk  FOR stpo-idnrk,
                  s_codalt FOR zppt0025-cod_alt,
                  s_aedat  FOR sy-datum,
                  s_aprov  FOR zppt0024-aprovador NO INTERVALS NO-EXTENSION.

  SELECTION-SCREEN BEGIN OF LINE.

  SELECTION-SCREEN COMMENT 1(8) text-005.

  SELECTION-SCREEN POSITION 33.
  PARAMETERS: r_opcao1   RADIOBUTTON GROUP g1 DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 35(10) text-003. " FOR FIELD  r_opcao1.

  SELECTION-SCREEN POSITION 52.
  PARAMETERS: r_opcao2 RADIOBUTTON GROUP g1.
  SELECTION-SCREEN COMMENT 54(10) text-002. " FOR FIELD r_opcao2.

  SELECTION-SCREEN POSITION 73.
  PARAMETERS: r_opcao3 RADIOBUTTON GROUP g1.
  SELECTION-SCREEN COMMENT 75(10) text-006. " FOR FIELD r_opcao2.

  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN: END OF BLOCK  bl1.

  SELECTION-SCREEN: BEGIN OF BLOCK bl2 WITH FRAME TITLE text-004.

  SELECTION-SCREEN: END OF BLOCK  bl2.

  START-OF-SELECTION.

*    IF r_opcao3 IS INITIAL.
*
*      IF s_stlan[] IS INITIAL.
*        DATA(l_erro) = 'X'.
*        MESSAGE s000(zppr) WITH 'Campo Util.LisTéc. obrigatório' DISPLAY LIKE 'E'.
*      ENDIF.
*
*    ENDIF.
*
*    IF l_erro IS INITIAL.
    PERFORM zf_buscar_dados.
*    ENDIF.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCA_DADOS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_buscar_dados .

    PERFORM zf_buscar_objetos_modificados.
    PERFORM zf_preparar_saida.

    IF t_saida[] IS NOT INITIAL.
      CALL SCREEN 9000.
    ENDIF.

  ENDFORM.


*  ----------------------------------------------------------------------*
*         CLASS lcl_event_receiver DEFINITION
*  ----------------------------------------------------------------------*
*         Definição da classe lcl_event_receiver
*  ----------------------------------------------------------------------*
  CLASS lcl_event_receiver DEFINITION.

    PUBLIC SECTION.

      CLASS-METHODS:

        handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
          IMPORTING e_row_id e_column_id,

        handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
          IMPORTING e_ucomm,

        handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
          IMPORTING e_object
                    e_interactive.

  ENDCLASS.                    "lcl_event_receiver DEFINITION

*  ----------------------------------------------------------------------*
*         CLASS lcl_event_receiver IMPLEMENTATION
*  ----------------------------------------------------------------------*
*         Implementação da classe lcl_event_receiver
*  ----------------------------------------------------------------------*
  CLASS lcl_event_receiver IMPLEMENTATION.

    METHOD handle_hotspot_click.
      PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
    ENDMETHOD.                    "handle_hotspot_click


    METHOD handle_user_command.

      PERFORM zf_buscar_aprovador.

      CASE e_ucomm.
        WHEN 'APROVAR'.
          PERFORM zf_efetuar_aprovacao.
        WHEN 'CANCAPROV'.
          PERFORM zf_cancelar_aprovacao.
        WHEN 'EMAIL'.
          PERFORM zf_reenvio_email.
        WHEN 'INFO'.
          PERFORM zf_expand_legenda.
        WHEN 'CADEMAIL'.
          PERFORM zf_cad_email.
        WHEN OTHERS.
      ENDCASE.

      CALL METHOD v_grid->refresh_table_display.

    ENDMETHOD.                    "handle_user_command

    METHOD handle_toolbar.

      PERFORM zf_adiciona_botoes_header USING e_object.
      PERFORM zf_elimina_botoes_header  USING e_object.

    ENDMETHOD.                    "handle_toolbar

  ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*  &---------------------------------------------------------------------*
*  &      Form  ZF_ADICIONA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
  FORM zf_adiciona_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*   Add Button
    DATA: w_toolbar  TYPE stb_button.

*  ----------------------------------------------------------------------*
*   legendas
    CLEAR w_toolbar.
    MOVE 3 TO w_toolbar-butn_type.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    MOVE 'INFO'                 TO w_toolbar-function.
    MOVE icon_information       TO w_toolbar-icon.
    MOVE '0 '                   TO w_toolbar-butn_type.
    MOVE 'Legendas'(028)        TO w_toolbar-quickinfo.
    APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Aprovar
    CLEAR w_toolbar.

    MOVE 'APROVAR'                 TO w_toolbar-function.
    MOVE icon_set_state            TO w_toolbar-icon.
    MOVE '0 '                      TO w_toolbar-butn_type.
    MOVE 'Aprovar'(008)            TO w_toolbar-quickinfo.
    MOVE 'Aprovar'(008)            TO w_toolbar-text.
    IF r_opcao1 IS INITIAL.
      MOVE r_opcao2 TO w_toolbar-disabled.
    ENDIF.
    APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Cancelar Aprovação
    CLEAR w_toolbar.

    MOVE 'CANCAPROV'                 TO w_toolbar-function.
    MOVE icon_cancel                 TO w_toolbar-icon.
    MOVE '0 '                        TO w_toolbar-butn_type.
    MOVE 'Cancelar Aprovação'(009)   TO w_toolbar-quickinfo.
    MOVE 'Cancelar Aprovação'(009)   TO w_toolbar-text.
    MOVE r_opcao1 TO w_toolbar-disabled.
    APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Reenviar e-mail
    CLEAR w_toolbar.

    MOVE 'EMAIL'                    TO w_toolbar-function.
    MOVE icon_mail                 TO w_toolbar-icon.
    MOVE '0 '                      TO w_toolbar-butn_type.
    MOVE 'Reenvio E-mail'(010)     TO w_toolbar-quickinfo.
    MOVE 'Reenvio E-mail'(010)     TO w_toolbar-text.
    APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Cancelar Aprovação
    CLEAR w_toolbar.

    MOVE 'CADEMAIL'                 TO w_toolbar-function.
    MOVE icon_display              TO w_toolbar-icon.
    MOVE '0 '                        TO w_toolbar-butn_type.
    MOVE 'Consultar aprovador'(030)   TO w_toolbar-quickinfo.
    MOVE 'Consultar Aprovador'(030)   TO w_toolbar-text.
    IF r_opcao2 IS INITIAL.
      MOVE r_opcao2 TO w_toolbar-disabled.
    ENDIF.
    APPEND w_toolbar TO e_object->mt_toolbar.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_ELIMINA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
  FORM zf_elimina_botoes_header  USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*      elimina itens desnecessarios da barra do container
    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
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
                                   OR function = '&MB_VARIANT'
*                                   OR FUNCTION =  '&MB_EXPORT'
                                   OR function =  '&MB_SUM'
                                   OR function =  '&MB_SUBTOT'
                                   OR function =  '&PRINT_BACK'.
  ENDFORM.

*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCAR_APROVADOR
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_buscar_aprovador .

*   Ligação material - lista técnica
    IF t_zppt0025[] IS NOT INITIAL.

      REFRESH: t_zppt0024.
      SELECT * FROM zppt0024
        INTO TABLE t_zppt0024
        FOR ALL ENTRIES IN t_zppt0025
        WHERE werks = t_zppt0025-werks.

    ENDIF.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCAR_OBJETOS_MODIFICADOS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_buscar_objetos_modificados .

*  &------------------------------------------
*   Define status
*  &------------------------------------------
    REFRESH: r_status.

    r_status-sign   = 'I'.
    r_status-option = 'EQ'.

    CASE abap_true.
      WHEN r_opcao1.
        r_status-low    = 'P'.
        APPEND r_status.
      WHEN r_opcao2.
        r_status-low    = 'A'.
        APPEND r_status.
      WHEN r_opcao3.

        r_status-low    = 'A'.
        APPEND r_status.

        r_status-low    = 'P'.
        APPEND r_status.

        r_status-low    = 'R'.
        APPEND r_status.

      WHEN OTHERS.
    ENDCASE.

*  &------------------------------------------
*   Ligação material - lista técnica
*  &------------------------------------------
    REFRESH: t_zppt0025.

    SELECT * FROM zppt0025
    INTO TABLE t_zppt0025
    WHERE cod_alt IN s_codalt
      AND matnr IN s_matnr
      AND werks IN s_werks
      AND stlan IN s_stlan
      AND idnrk IN s_idnrk
      AND aedat IN s_aedat
      AND aprovador IN s_aprov
      AND status IN r_status.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(zppr) WITH 'Nenhum registro encontrado.' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_SAIDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_preparar_saida .

*   DD-R/3: valores dos domínios
    SELECT * FROM dd07t
      INTO TABLE @DATA(t_dd07t)
      WHERE domname = 'ZDOPP_STAT_APROV'.

    SELECT matnr, maktx FROM makt
      INTO TABLE @DATA(t_makt)
      FOR ALL ENTRIES IN @t_zppt0025
      WHERE matnr = @t_zppt0025-idnrk.

    PERFORM zf_define_cor_colunas.

    REFRESH: t_saida.
    LOOP AT t_zppt0025 INTO DATA(w_zppt0025).

      MOVE-CORRESPONDING w_zppt0025 TO w_saida.

*   Texto do Status
      READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY domvalue_l = w_saida-status.
      IF sy-subrc IS INITIAL.
        w_saida-status = w_dd07t-ddtext.
      ENDIF.

      CASE w_zppt0025-status.
        WHEN 'A'.
          w_saida-icon   = gc_icones-icon_green_light.
        WHEN 'P'.
          w_saida-icon   = gc_icones-icon_yellow_light.
        WHEN 'R'.
          w_saida-icon   = gc_icones-icon_red_light.
        WHEN OTHERS.
      ENDCASE.

      READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_zppt0025-idnrk.
      IF sy-subrc IS INITIAL.
        w_saida-maktx = w_makt-maktx.
      ENDIF.

      IF w_zppt0025-status_email = 'S'.
        w_saida-icon_email   = gc_icones-icon_system_okay.
      ELSE.
        w_saida-icon_email   = gc_icones-icon_system_cancel.
      ENDIF.

      w_saida-color_cell = t_coltab2.

      APPEND w_saida TO t_saida.

    ENDLOOP.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Module  ZM_STATUS_9000  OUTPUT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
  MODULE zm_status_9000 OUTPUT.
    SET PF-STATUS 'ZGPP_STATUS_9000'.
    SET TITLEBAR  'ZUPP_TITULO'.

    PERFORM zf_split_screen.
    PERFORM zf_preparar_alv.

  ENDMODULE.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_SPLIT_SCREEN
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_split_screen .

    CLEAR: v_docking, v_splitter, v_container_h, v_container_i.
    CREATE OBJECT v_docking
      EXPORTING
        repid = sy-repid
        dynnr = sy-dynnr
        ratio = '95'.

*   Create a splitter with 2 rows and 1 column
    CREATE OBJECT v_splitter
      EXPORTING
        parent  = v_docking
        rows    = 2
        columns = 1.

*  * Upper Container
    CALL METHOD v_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = v_container_h.

*  * Lower Container
    CALL METHOD v_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = v_container_i.

*  * Upper Container height

    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    PERFORM zf_preparar_header.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_preparar_header .

    DATA: l_document  TYPE REF TO cl_dd_document,
          l_doctable  TYPE REF TO cl_dd_table_element,
          l_column1   TYPE REF TO cl_dd_area,
          l_column2   TYPE REF TO cl_dd_area,
          l_text(255) TYPE c.  "Text

    CREATE OBJECT l_document.
*  ************ - Titulo Legenda
    CLEAR l_text.
    CALL METHOD l_document->add_gap
      EXPORTING
        width = 30.

    CALL METHOD l_document->add_text
      EXPORTING
        text         = 'Legenda'(023)
        sap_emphasis = 'STRONG'.

    CALL METHOD l_document->new_line.

*  *************L1 - Legenda
    CALL METHOD l_document->add_gap
      EXPORTING
        width = 30.

    CALL METHOD l_document->add_icon
      EXPORTING
        sap_icon = 'ICON_GREEN_LIGHT'.

    CALL METHOD l_document->add_text
      EXPORTING
        text = 'Aprovada'(024).

    CALL METHOD l_document->new_line.

*  *************L3 - Aguardando aprovação
    CALL METHOD l_document->add_gap
      EXPORTING
        width = 30.

    CALL METHOD l_document->add_icon
      EXPORTING
        sap_icon = 'ICON_YELLOW_LIGHT'.

    CALL METHOD l_document->add_text
      EXPORTING
        text = 'Aguardando aprovação'(025).

    CALL METHOD l_document->new_line.

*  *************L4 - Email enviado com sucesso
    CALL METHOD l_document->add_gap
      EXPORTING
        width = 30.

    CALL METHOD l_document->add_icon
      EXPORTING
        sap_icon = 'ICON_SYSTEM_OKAY'.

    CALL METHOD l_document->add_text
      EXPORTING
        text = 'Email enviado com sucesso'(026).

    CALL METHOD l_document->new_line.

*  *************L5 - Erro no envio do mail
    CALL METHOD l_document->add_gap
      EXPORTING
        width = 30.

    CALL METHOD l_document->add_icon
      EXPORTING
        sap_icon = 'ICON_SYSTEM_CANCEL'.

    CALL METHOD l_document->add_text
      EXPORTING
        text = 'Erro envio de Email'(027).

    CALL METHOD l_document->new_line.

*  ********************************************
    CALL METHOD l_document->display_document
      EXPORTING
        parent = v_container_h.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_ALV
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_preparar_alv .

    DATA: t_fcat TYPE lvc_t_fcat,
          t_sort TYPE lvc_t_sort.

    DATA: w_variant TYPE disvariant,
          w_layout  TYPE lvc_s_layo.

    w_layout-cwidth_opt = 'X'.
    w_layout-zebra      = 'X'.
    w_layout-info_fname = 'COLOR_LINE'.
    w_layout-ctab_fname = 'COLOR_CELL'.

*    W_LAYOUT-STYLEFNAME = 'CELLTAB'.
*    W_LAYOUT-SEL_MODE   = 'A'.

    w_variant-report    = sy-repid.
*    W_VARIANT-VARIANT   = P_LAYOUT.

    PERFORM zf_montar_fieldcat CHANGING t_saida t_fcat.
    PERFORM zf_ajuste_fieldcat CHANGING t_fcat.
*    PERFORM ZF_AJUSTE_SAIDA.

    IF v_grid IS INITIAL.

      CREATE OBJECT v_grid
        EXPORTING
          i_parent          = v_container_i
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      CREATE OBJECT v_event_receiver.
      SET HANDLER v_event_receiver->handle_hotspot_click FOR v_grid.
      SET HANDLER v_event_receiver->handle_toolbar       FOR v_grid.
      SET HANDLER v_event_receiver->handle_user_command  FOR v_grid.

      w_layout-sel_mode = 'A'.

      CALL METHOD v_grid->set_table_for_first_display
        EXPORTING
          is_variant                    = w_variant
          i_save                        = 'A'
          is_layout                     = w_layout
        CHANGING
          it_fieldcatalog               = t_fcat
          it_outtab                     = t_saida
          it_sort                       = t_sort
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*     RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
      CALL METHOD v_grid->set_toolbar_interactive.

    ELSE.
      CALL METHOD v_grid->refresh_table_display.
    ENDIF.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_MONTAR_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        <--P_T_SAIDA  text
*        <--P_T_FCAT  text
*  ----------------------------------------------------------------------*
  FORM zf_montar_fieldcat CHANGING pt_tabela   TYPE ANY TABLE
                                    pt_fieldcat TYPE lvc_t_fcat.

    DATA:
      l_columns      TYPE REF TO cl_salv_columns_table,
      l_aggregations TYPE REF TO cl_salv_aggregations,
      l_salv_table   TYPE REF TO cl_salv_table,
      l_data         TYPE REF TO data.
    FIELD-SYMBOLS:
      <f_table>      TYPE STANDARD TABLE.

*   Cria uma estrutura com o mesmo layout da tabela de saída
    CREATE DATA l_data LIKE pt_tabela.
    ASSIGN l_data->* TO <f_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*   Monta a estrutura dinâmica no objeto l_salv_table
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = l_salv_table
          CHANGING
            t_table      = <f_table> ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
        RETURN.
    ENDTRY.

*   Recupera as colunas e dados internos
    l_columns      = l_salv_table->get_columns( ).
    l_aggregations = l_salv_table->get_aggregations( ).

*   Monta o fieldcat
    pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                     r_aggregations = l_aggregations ).

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Module  ZM_EXIT  INPUT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
  MODULE zm_exit INPUT.

    CASE sy-ucomm.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'CANC'.
        LEAVE TO SCREEN 0.
      WHEN 'EXIT'.
        LEAVE PROGRAM.
      WHEN OTHERS.
    ENDCASE.

  ENDMODULE.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_EFETUAR_APROVACAO
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_efetuar_aprovacao .

    DATA: t_index_rows TYPE lvc_t_row,                      "#EC NEEDED
          t_row_no     TYPE lvc_t_roid,
          t_delete     TYPE TABLE OF zcot0013,
          w_row_no     LIKE LINE OF t_row_no,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c,

          w_0025       TYPE zppt0025.

    REFRESH: t_zppt0025.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    DESCRIBE TABLE t_row_no LINES l_linhas.
    IF l_linhas = 0.
      "Selecionar uma linha.
      MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF l_linhas > 1.
      "Selecionar apenas uma linha
      MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.


    LOOP AT t_row_no INTO w_row_no.

      CLEAR w_saida.
      READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

      CHECK sy-subrc IS INITIAL AND <fs_saida>-status <> 'A'.

      IF <fs_saida>-cod_alt IS NOT INITIAL.

        REFRESH: t_zppt0025.
        SELECT * FROM zppt0025
          INTO TABLE @DATA(t_zppt0025)
          WHERE cod_alt = @<fs_saida>-cod_alt.

      ENDIF.

      IF <fs_saida>-werks IS NOT INITIAL.

        REFRESH: t_zppt0024.
        SELECT * FROM zppt0024
          INTO TABLE t_zppt0024
          WHERE werks = <fs_saida>-werks.

      ENDIF.

      LOOP AT t_zppt0025 ASSIGNING FIELD-SYMBOL(<fs_0025>).

        READ TABLE t_zppt0024 INTO DATA(w_zppt0024) WITH KEY werks     = <fs_0025>-werks
                                                             aprovador = sy-uname
                                                             stlan     = <fs_0025>-stlan.

        IF sy-subrc IS INITIAL.

          <fs_0025>-status       = 'A'.
          <fs_0025>-aprovador    = sy-uname.
          <fs_0025>-dt_aprovacao = sy-datum.
          <fs_0025>-hr_aprovacao = sy-uzeit.

        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'para o centro.' 'Transação ZPP0021' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF t_zppt0025[] IS NOT INITIAL.
      MODIFY zppt0025 FROM TABLE t_zppt0025.
      COMMIT WORK.
    ENDIF.

    PERFORM zf_buscar_objetos_modificados.
    PERFORM zf_preparar_saida.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_AJUSTE_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        <--P_T_FCAT  text
*  ----------------------------------------------------------------------*
  FORM zf_ajuste_fieldcat  CHANGING pt_fcat TYPE lvc_t_fcat.

    LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

      <fs_fcat>-coltext = <fs_fcat>-reptext.

      CASE <fs_fcat>-fieldname.
        WHEN 'MATNR'.
          <fs_fcat>-coltext = <fs_fcat>-reptext = 'Lista técnica'.
          <fs_fcat>-hotspot = 'X'.
        WHEN 'IDNRK'.
          <fs_fcat>-coltext = <fs_fcat>-reptext = 'Componente'.
        WHEN 'TIPO'.
          <fs_fcat>-coltext = <fs_fcat>-reptext = 'Tipo'.
        WHEN 'STATUS'.
          <fs_fcat>-coltext = <fs_fcat>-reptext = 'Status'.
        WHEN 'STATUS_EMAIL'.
          <fs_fcat>-no_out = 'X'.
        WHEN 'ICON_EMAIL'.
          <fs_fcat>-coltext = <fs_fcat>-reptext = 'Email'.
        WHEN 'STATUS'.
          <fs_fcat>-drdn_hndl  = '1'.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CANCELAR_APROVACAO
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_cancelar_aprovacao .

    DATA: t_index_rows TYPE lvc_t_row,                      "#EC NEEDED
          t_row_no     TYPE lvc_t_roid,
          t_delete     TYPE TABLE OF zcot0013,
          w_row_no     LIKE LINE OF t_row_no,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c,

          w_0025       TYPE zppt0025.

    REFRESH: t_zppt0025.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    DESCRIBE TABLE t_row_no LINES l_linhas.
    IF l_linhas = 0.
      "Selecionar uma linha.
      MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF l_linhas > 1.
      "Selecionar apenas uma linha
      MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT t_row_no INTO w_row_no.

      CLEAR w_saida.
      READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

      CHECK sy-subrc IS INITIAL AND <fs_saida>-status <> 'P'.

      IF <fs_saida>-cod_alt IS NOT INITIAL.

        SELECT * FROM zppt0025
          INTO TABLE @DATA(t_zppt0025)
          WHERE cod_alt = @<fs_saida>-cod_alt.

      ENDIF.

      IF <fs_saida>-werks IS NOT INITIAL.

        REFRESH: t_zppt0024.
        SELECT * FROM zppt0024
          INTO TABLE t_zppt0024
          WHERE werks = <fs_saida>-werks.

      ENDIF.

      LOOP AT t_zppt0025 ASSIGNING FIELD-SYMBOL(<fs_0025>).

        READ TABLE t_zppt0024 INTO DATA(w_zppt0024) WITH KEY werks     = <fs_saida>-werks
                                                             aprovador = sy-uname
                                                             stlan     = <fs_saida>-stlan.

        IF sy-subrc IS INITIAL.
          <fs_0025>-status       = 'P'.
          CLEAR: <fs_0025>-aprovador, <fs_0025>-dt_aprovacao, <fs_0025>-hr_aprovacao.
        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador.' 'Transação ZPP0021' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF t_zppt0025[] IS NOT INITIAL.
      MODIFY zppt0025 FROM TABLE t_zppt0025.
      COMMIT WORK.
    ENDIF.

    PERFORM zf_buscar_objetos_modificados.
    PERFORM zf_preparar_saida.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_REENVIO_EMAIL
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_reenvio_email .

    DATA: t_index_rows TYPE lvc_t_row,                      "#EC NEEDED
          t_row_no     TYPE lvc_t_roid,

          w_row_no     LIKE LINE OF t_row_no,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c,

          l_enviado    TYPE sofolenti1-object_id.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    DESCRIBE TABLE t_row_no LINES l_linhas.
    IF l_linhas = 0.
      "Selecionar uma linha.
      MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF l_linhas > 1.
      "Selecionar apenas uma linha
      MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT t_row_no INTO w_row_no.

      CLEAR w_saida.
      READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

      CHECK sy-subrc IS INITIAL.

      IF <fs_saida>-cod_alt IS NOT INITIAL.
        SELECT * FROM zppt0025
          INTO TABLE @DATA(t_lista)
          WHERE cod_alt = @<fs_saida>-cod_alt.
      ENDIF.

*   Email cadastrados para controle de Modif. Lista Técnica
      SELECT SINGLE * FROM zppt0024
        INTO @DATA(wl_email)
        WHERE werks = @<fs_saida>-werks.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.

*   Envia e-mail
        CALL FUNCTION 'ZFPP_EMAIL_LISTA_TECNICA'
          IMPORTING
            e_enviado = l_enviado
          TABLES
            tl_lista  = t_lista.

        LOOP AT t_lista ASSIGNING FIELD-SYMBOL(<fs_lista>).
          <fs_lista>-status_email = 'S'.
        ENDLOOP.

      ENDIF.

    ENDLOOP.

    IF t_lista[] IS NOT INITIAL.
      MODIFY zppt0025 FROM TABLE t_lista.
      COMMIT WORK.
    ENDIF.

    PERFORM zf_buscar_objetos_modificados.
    PERFORM zf_preparar_saida.


  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_EXPAND_LEGENDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_expand_legenda .

    IF v_info IS INITIAL.
      CALL METHOD v_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 27.

      v_info = 'X'.
    ELSE.
      CALL METHOD v_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 0.

      CLEAR v_info.
    ENDIF.

    CALL METHOD v_grid->refresh_table_display.


  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CAD_EMAIL
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_cad_email .

    DATA: t_index_rows TYPE lvc_t_row,                      "#EC NEEDED
          t_row_no     TYPE lvc_t_roid,
          w_row_no     LIKE LINE OF t_row_no,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c,

          go_alv       TYPE REF TO cl_salv_table,
          go_functions TYPE REF TO cl_salv_functions,
          go_display   TYPE REF TO cl_salv_display_settings.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    DESCRIBE TABLE t_row_no LINES l_linhas.
    IF l_linhas = 0.
      "Selecionar uma linha.
      MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF l_linhas > 1.
      "Selecionar apenas uma linha
      MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT t_row_no INTO w_row_no.

      CLEAR w_saida.
      READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

      IF sy-subrc IS INITIAL.

        REFRESH: t_zppt0024.
        SELECT * FROM zppt0024
          INTO TABLE t_zppt0024
          WHERE werks = <fs_saida>-werks
            AND stlan = <fs_saida>-stlan.

        IF t_zppt0024[] IS NOT INITIAL.
          cl_salv_table=>factory( IMPORTING r_salv_table = go_alv CHANGING t_table = t_zppt0024 ).

*          go_functions = go_alv->get_functions( ).
*          go_functions->set_all( abap_true ).

          go_display = go_alv->get_display_settings( ).
          go_display->set_list_header( 'Aprovadores cadastros' ).

          IF go_alv IS BOUND.
            go_alv->display( ).
          ENDIF.
        ELSE.
          MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado para Centro' <fs_saida>-werks 'Util.LisTéc' <fs_saida>-stlan.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CS03
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_cs03 USING p_saida TYPE y_saida.

    SET PARAMETER ID: 'MAT' FIELD p_saida-matnr,
                      'WRK' FIELD p_saida-werks,
                      'CSV' FIELD p_saida-stlan.
    CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_HANDLE_HOTSPOT_CLICK
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_ROW_ID_INDEX  text
*        -->P_E_COLUMN_ID  text
*  ----------------------------------------------------------------------*
  FORM zf_handle_hotspot_click    USING    p_index  TYPE any
                                         p_column TYPE any.

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX p_index.

    CHECK sy-subrc IS INITIAL.

    CASE p_column.
      WHEN 'MATNR'.
        PERFORM zf_cs03 USING <fs_saida>.
*      WHEN 'STLNR'.
*        PERFORM ZF_CS03  USING <FS_SAIDA>.
    ENDCASE.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_AJUSTE_SAIDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_ajuste_saida .

    LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      <fs_saida>-color_cell = t_coltab2.
    ENDLOOP.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_DEFINE_COR_COLUNAS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_define_cor_colunas .

    DATA: w_col   TYPE lvc_s_scol,
          w_color TYPE lvc_s_colo.

    REFRESH: t_coltab2.
*  ------------------------------------------------
*   Cor dos dados de alteração
*  ------------------------------------------------
    w_color-col   = gc-verde.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'AENAM'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

    w_color-col   = gc-verde.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'AEDAT'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

    w_color-col   = gc-verde.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'HR_ALTERACAO'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

*  ------------------------------------------------
*  cor dos dados de aprovação
*  ------------------------------------------------

    w_color-col   = gc-amarelo.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'APROVADOR'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

    w_color-col   = gc-amarelo.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'DT_APROVACAO'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

    w_color-col   = gc-amarelo.
    w_color-int   = gc-int.
    w_color-inv   = gc-inv.
    w_col-fname   = 'HR_APROVACAO'.  "Nome da coluna
    w_col-color   = w_color.
    APPEND w_col TO t_coltab2.

  ENDFORM.
