*  ----------------------------------------------------------------------*
*   ID........:                                                          *
*   Programa..: ZPMR0072                                                 *
*   Tipo......: R - Report                                               *
*   Transação.:                                                          *
*   Descrição.: Parâmetros de Impressão automática do Selo SAP           *
*   Autor.....: JBARBOSA                                                 *
*   Data......: 04.05.2021                                               *
*  ----------------------------------------------------------------------*
*                       Controle de Alterações                           *
*  ----------------------------------------------------------------------*
*   Data       | Change     | Autor        | Alteração                   *
*  ----------------------------------------------------------------------*
*   04.05.21   |            |JBARBOSA      | Codificação Inicial         *
*  ----------------------------------------------------------------------*
*   07.06.24   |            |VRIENZO       | 138080-Alt imagem de rotulo *

  REPORT zpmr0072.

  TABLES: zpmt0054,
          zpmt0055.

*  &---------------------------------------------------------------------*
*  &     Declaração de Tipos
*  &---------------------------------------------------------------------*
  TYPES: BEGIN OF ty_saida,
           werks       TYPE werks_d,
           safra       TYPE zdepm_safra,
           matnr       TYPE matnr,
           maktx       TYPE makt-maktx,
           imagem      TYPE zimagem,
           imagem_desc TYPE char30,
           id_lote     TYPE zdepm_id_lote,
           lote        TYPE zdepm_lote,
           ciclo       TYPE zdepm_ciclo,
           ciclo_sem   TYPE zdepm_ciclo_sem,
           ciclo_desc  TYPE char30,
           dia         TYPE zdepm_dia,
           validade    TYPE zdepm_validade,
           bloquear    TYPE zdepm_flag,
           usname      TYPE zdepm_uname,
           data        TYPE zdepm_datum,
           hora        TYPE zdepm_uzeit,
           imprimir    TYPE zdepm_imprimir,
           sel_line    TYPE c,
         END OF ty_saida.

  TYPES: BEGIN OF ty_field_screen_key,
           field_screen TYPE string.
  TYPES END OF ty_field_screen_key.

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

  DATA: t_zpmt0054 TYPE TABLE OF zpmt0054,
        t_saida    TYPE TABLE OF ty_saida.

  DATA: t_coltab2 TYPE lvc_t_scol.

*  &---------------------------------------------------------------------*
*   Declaração de Estrutura
*  &---------------------------------------------------------------------*
  DATA: w_disvariant TYPE disvariant,
        w_layout     TYPE lvc_s_layo,
        w_saida      LIKE LINE OF t_saida.

*  &---------------------------------------------------------------------*
*   Declaração de Variáveis
*  &---------------------------------------------------------------------*
  DATA: v_info     TYPE c,
        v_ok9000   TYPE sy-ucomm,
        v_ok9100   TYPE sy-ucomm,
        v_operacao TYPE c LENGTH 20.
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
  SELECT-OPTIONS: s_werks FOR zpmt0054-werks
                               NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_safra FOR zpmt0054-safra
                               NO-EXTENSION NO INTERVALS OBLIGATORY.
  SELECTION-SCREEN: END OF BLOCK  bl1.

  START-OF-SELECTION.

    PERFORM zf_buscar_dados.

*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCA_DADOS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
  FORM zf_buscar_dados.
    CALL SCREEN 9000.
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

      CASE e_ucomm.
        WHEN 'INS'.
          PERFORM zf_adicionar_linha.
        WHEN 'CHG'.
          PERFORM zf_modificar_linha.
        WHEN 'DEL'.
          PERFORM zf_eliminar_linha.
        WHEN OTHERS.
      ENDCASE.

      CALL METHOD v_grid->refresh_table_display.

    ENDMETHOD.                    "handle_user_command

    METHOD handle_toolbar.
      PERFORM zf_elimina_botoes_header  USING e_object.
      PERFORM zf_adiciona_botoes_header USING e_object.
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

    DATA: w_toolbar  TYPE stb_button.

    CLEAR w_toolbar.
    MOVE 3 TO w_toolbar-butn_type.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    MOVE 'INS'                  TO w_toolbar-function.
    MOVE icon_create            TO w_toolbar-icon.
    MOVE '0 '                   TO w_toolbar-butn_type.
    MOVE 'Incluir'              TO w_toolbar-quickinfo.
    MOVE ''                     TO w_toolbar-text.
    APPEND w_toolbar            TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    MOVE 'CHG'                  TO w_toolbar-function.
    MOVE icon_change            TO w_toolbar-icon.
    MOVE '0 '                   TO w_toolbar-butn_type.
    MOVE 'Alterar'   TO w_toolbar-quickinfo.
    MOVE ''                     TO w_toolbar-text.
    APPEND w_toolbar            TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    MOVE 'DEL'                  TO w_toolbar-function.
    MOVE icon_delete            TO w_toolbar-icon.
    MOVE '0 '                   TO w_toolbar-butn_type.
    MOVE 'DEL'                  TO w_toolbar-quickinfo.
    MOVE ''                     TO w_toolbar-text.
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
*                                   OR function = '&MB_VARIANT'
*                                   OR FUNCTION =  '&MB_EXPORT'
                                   OR function =  '&MB_SUM'
                                   OR function =  '&MB_SUBTOT'
                                   OR function =  '&PRINT_BACK'.
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

    DATA: t_idd07v TYPE TABLE OF dd07v,
          w_idd07v TYPE dd07v.

    REFRESH: t_zpmt0054, t_saida.

    "Buscar parâmetros
    SELECT * FROM zpmt0054
    INTO TABLE t_zpmt0054
    WHERE werks IN s_werks
      AND safra IN s_safra.

*------------------------------------------
*-- ao entrar, atualiza lote de todos
*------------------------------------------
    LOOP AT t_zpmt0054 INTO DATA(w_0054).

      CHECK w_0054-bloquear IS INITIAL.

      CALL FUNCTION 'ZSD_CALCULAR_LOTE_SELO'
        EXPORTING
          i_data_ref               = sy-datum
          i_zpmt0054               = w_0054
        IMPORTING
          e_lote                   = w_0054-lote
        EXCEPTIONS
          documento_nao_autorizado = 1
          OTHERS                   = 2.

      UPDATE zpmt0054 SET lote  = w_0054-lote
                    WHERE werks = w_0054-werks
                      AND safra = w_0054-safra
                      AND matnr = w_0054-matnr.
      COMMIT WORK.
    ENDLOOP.

    "Buscar parâmetros
    SELECT *
      FROM zpmt0054
      INTO TABLE t_zpmt0054
     WHERE werks IN s_werks
       AND safra IN s_safra.
*------------------------------------------

    "Buscar descrição de material
    IF t_zpmt0054[] IS NOT INITIAL.
      SELECT matnr, maktx FROM makt
        INTO TABLE @DATA(t_makt)
        FOR ALL ENTRIES IN @t_zpmt0054
        WHERE matnr = @t_zpmt0054-matnr
          AND spras = @sy-langu.
    ENDIF.

    REFRESH: t_saida. CLEAR w_saida.
    LOOP AT t_zpmt0054 INTO DATA(w_zpmt0054).

      MOVE-CORRESPONDING w_zpmt0054 TO w_saida.

      READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_zpmt0054-matnr.
      IF sy-subrc IS INITIAL.
        w_saida-maktx = w_makt-maktx.
      ENDIF.

*---------------------------------
* imagem
*---------------------------------
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZIMAGEM'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = t_idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      CLEAR w_idd07v.
      READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = w_zpmt0054-imagem.

      w_saida-imagem_desc = w_idd07v-ddtext.

*---------------------------------
* ciclo
*---------------------------------
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZDEPM_CICLO_SEM'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = t_idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      CLEAR w_idd07v.
      READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = w_zpmt0054-ciclo_sem.

      w_saida-ciclo_desc = w_idd07v-ddtext.

      APPEND w_saida TO t_saida.

    ENDLOOP.

  ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Module  ZM_STATUS_9000  OUTPUT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
  MODULE zm_status_9000 OUTPUT.
    SET PF-STATUS 'PF9000'.
    SET TITLEBAR  'TIT9000'.

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
        rows    = 1
        columns = 1.

    "Item
    CALL METHOD v_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = v_container_i.

    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 0.

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

*    w_layout-cwidth_opt = 'X'.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.

    w_variant-report    = sy-repid.

    PERFORM zf_montar_fieldcat CHANGING t_saida t_fcat.
    PERFORM zf_ajuste_fieldcat CHANGING t_fcat.

    IF v_grid IS INITIAL.

      PERFORM zf_preparar_saida.

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

    ELSE.
      CALL METHOD v_grid->refresh_table_display.
    ENDIF.

    CALL METHOD v_grid->set_toolbar_interactive.

    CALL METHOD v_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
    LEAVE TO SCREEN 0.
  ENDMODULE.
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
*      <fs_fcat>-edit = abap_true.

      CASE <fs_fcat>-fieldname.
        WHEN 'WERKS'.
          <fs_fcat>-outputlen = 6.
          CLEAR <fs_fcat>-edit.
          <fs_fcat>-f4availabl = 'X'.
          <fs_fcat>-ref_table  = 'T001'.
          <fs_fcat>-ref_field  = 'WERKS'.
        WHEN 'SAFRA'.
          <fs_fcat>-outputlen = 6.
          CLEAR <fs_fcat>-edit.
        WHEN 'MATNR'.
          <fs_fcat>-ref_table  = 'MARA'.
          <fs_fcat>-ref_field  = 'MATNR'.
        WHEN 'MAKTX'.
          CLEAR <fs_fcat>-edit.
        WHEN 'ID_LOTE'.
          <fs_fcat>-outputlen = 8.
        WHEN 'LOTE'.
        WHEN 'IMAGEM'.
          <fs_fcat>-outputlen = 6.
          <fs_fcat>-coltext   = 'Selo'.
          <fs_fcat>-ref_table = 'ZPMT0054'.
          <fs_fcat>-ref_field = 'IMAGEM'.
          <fs_fcat>-f4availabl = 'X'.
          <fs_fcat>-no_out     = 'X'.
*         <fs_fcat>-scrtext_l = 'Selo'.
*         <fs_fcat>-scrtext_m = 'Selo'.
*         <fs_fcat>-scrtext_s = 'Selo'.
        WHEN 'IMAGEM_DESC'.
          <fs_fcat>-outputlen = 20.
          <fs_fcat>-coltext   = 'Selo'.
          <fs_fcat>-f4availabl = ''.
        WHEN 'CICLO_DESC'.
          <fs_fcat>-outputlen = 20.
          <fs_fcat>-coltext   = 'Ciclo'.
          <fs_fcat>-f4availabl = ''.
*         <fs_fcat>-outputlen = 10.
        WHEN 'DIA'.
          <fs_fcat>-outputlen = 15.
          <fs_fcat>-coltext   = 'Par.Dt.Produção'.
        WHEN 'VALIDADE'.
          <fs_fcat>-outputlen = 10.
        WHEN 'BLOQUEAR'.
          <fs_fcat>-outputlen = 10.
          <fs_fcat>-checkbox = abap_true.
        WHEN 'USNAME'.
          CLEAR <fs_fcat>-edit.
        WHEN 'DATA'.
          CLEAR <fs_fcat>-edit.
        WHEN 'HORA'.
          CLEAR <fs_fcat>-edit.
        WHEN 'IMPRIMIR'.
          <fs_fcat>-outputlen = 10.
          <fs_fcat>-checkbox = abap_true.
        WHEN OTHERS.
          <fs_fcat>-no_out = abap_true.
          <fs_fcat>-tech  = abap_true.
      ENDCASE.

    ENDLOOP.

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

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM zf_adicionar_linha .

    DATA: t_index_rows TYPE lvc_t_row,
          t_row_no     TYPE lvc_t_roid,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c.

    CLEAR w_saida.
    w_saida-werks  = s_werks-low.
    w_saida-safra  = s_safra-low.
    w_saida-usname = sy-uname.
    w_saida-data   = sy-datum.
    w_saida-hora   = sy-uzeit.

    CALL SCREEN 9100 STARTING AT 10 05.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINAR_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM zf_eliminar_linha .

    DATA: t_index_rows TYPE lvc_t_row,
          t_row_no     TYPE lvc_t_roid,

          l_linhas     TYPE sy-tabix,
          l_answer     TYPE c.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    IF lines( t_row_no ) < 1.
      MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.

    ELSE.

      LOOP AT t_row_no INTO DATA(w_row_no).
        READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.
        IF <fs_saida> IS ASSIGNED.
          <fs_saida>-sel_line = 'X'.
        ENDIF.
      ENDLOOP.

      DELETE t_saida WHERE sel_line <> 'X'.

      "Parâmetros de Impressão automática do Selo SAP
      SELECT * FROM zpmt0054
        INTO TABLE @DATA(t_zpmt0054)
        FOR ALL ENTRIES IN @t_saida
        WHERE werks = @t_saida-werks
          AND safra = @t_saida-safra
          AND matnr = @t_saida-matnr.

      IF sy-subrc IS INITIAL.
*----------------------------------------
*---- LOG
*----------------------------------------
        LOOP AT t_zpmt0054 INTO DATA(w_zpmt0054).
          MOVE-CORRESPONDING w_zpmt0054 TO zpmt0055.
          MOVE sy-datum                 TO zpmt0055-data_reg.
          MOVE sy-uzeit                 TO zpmt0055-hora_reg.
          MOVE sy-uname                 TO zpmt0055-user_reg.
          MOVE 'E'                      TO zpmt0055-atividade.
          MODIFY zpmt0055.
        ENDLOOP.

        DELETE zpmt0054 FROM TABLE t_zpmt0054.
        COMMIT WORK.
      ENDIF.

      PERFORM zf_preparar_saida.
      MESSAGE 'Registro(s) eliminado(s) com sucesso' TYPE 'S'.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE zm_user_command INPUT.

    DATA: l_werks TYPE zpmt0055-werks,
          l_safra TYPE zpmt0055-safra.


    CASE v_ok9000.
      WHEN 'EXIT' OR 'CANC' OR 'BACK'.
        LEAVE TO SCREEN 0.

      WHEN '&LOG'.
        READ TABLE s_werks INDEX 1.
        READ TABLE s_safra INDEX 1.

        l_werks = s_werks-low.
        l_safra = s_safra-low.

        CALL FUNCTION 'ZSD_CADASTRO_LOG_SELO'
          EXPORTING
            i_werks        = l_werks
            i_safra        = l_safra
          EXCEPTIONS
            nao_existe_log = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          MESSAGE s000(z_mm) WITH 'Não ha Log a ser exibido' DISPLAY LIKE 'W'.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.
  ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE zm_status_9100 OUTPUT.

    SET PF-STATUS 'PF9100'.
*>>>Begin- Stefanini - 07.06.2024 - Vitor Rienzo
*    IF v_operacao = 'CHANGE'.
*
*      LOOP AT SCREEN.
*        IF screen-name = 'W_SAIDA-MATNR'.
*          screen-input = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*
*    ENDIF.
*<<<End- Stefanini - 07.06.2024 - Vitor Rienzo
  ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE zm_user_command_9100 INPUT.

    CASE v_ok9100.
      WHEN 'CONFIRM'.
        PERFORM zf_salvar_registros.
      WHEN 'CANCEL'.
      WHEN OTHERS.
    ENDCASE.

  ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM zf_salvar_registros .

    DATA: w_zpmt0054 TYPE zpmt0054,
          l_msg      TYPE bapiret2-message.

    IF w_saida-werks IS NOT INITIAL AND w_saida-matnr IS NOT INITIAL.

      SELECT SINGLE matnr FROM marc
        INTO @DATA(l_matnr)
         WHERE matnr = @w_saida-matnr
           AND werks = @w_saida-werks.

      IF sy-subrc IS NOT INITIAL.

        CLEAR l_msg.
        CONCATENATE 'Material'
                    w_saida-matnr
                    'não encontrado para o centro'
                     w_saida-werks INTO l_msg SEPARATED BY space.

        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    IF v_operacao IS INITIAL.

      "Verifica se o registro já existe.
      SELECT SINGLE * FROM zpmt0054
        INTO w_zpmt0054
        WHERE werks   = w_saida-werks
          AND safra   = w_saida-safra
          AND matnr   = w_saida-matnr.
*         AND id_lote = w_saida-id_lote.

      IF sy-subrc IS NOT INITIAL.

        CLEAR w_zpmt0054.
        w_saida-usname  = sy-uname.
        w_saida-data    = sy-datum.
        w_saida-hora    = sy-uzeit.

        MOVE-CORRESPONDING w_saida TO w_zpmt0054.
        MODIFY zpmt0054 FROM w_zpmt0054.
        COMMIT WORK.

        MESSAGE 'Registro(s) gravado(s) com sucesso' TYPE 'S'.
        PERFORM zf_preparar_saida.
      ELSE.
        MESSAGE 'Já existe uma entrada com os dados informado(s)' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ELSE.

*--------------------------------------
*---- LOG
*--------------------------------------
      SELECT SINGLE *
               FROM zpmt0054
               INTO @DATA(w_zpmt0054_alt)
              WHERE werks = @w_saida-werks
                AND safra = @w_saida-safra
                AND matnr = @w_saida-matnr.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING w_zpmt0054_alt TO zpmt0055.
        MOVE sy-datum                     TO zpmt0055-data_reg.
        MOVE sy-uzeit                     TO zpmt0055-hora_reg.
        MOVE sy-uname                     TO zpmt0055-user_reg.
        MOVE 'A'                          TO zpmt0055-atividade.
        MODIFY zpmt0055.
      ENDIF.

      CLEAR w_zpmt0054.
      MOVE-CORRESPONDING w_saida TO w_zpmt0054.
      MODIFY zpmt0054 FROM w_zpmt0054.
      COMMIT WORK.

      MESSAGE 'Registro(s) alterado(s) com sucesso' TYPE 'S'.
      PERFORM zf_preparar_saida.

    ENDIF.

    CLEAR v_operacao.
    LEAVE TO SCREEN 0.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MODIFICAR_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM zf_modificar_linha .

    DATA: t_index_rows TYPE lvc_t_row,
          t_row_no     TYPE lvc_t_roid,

          l_linhas     TYPE sy-tabix,
          l_lines      TYPE sy-tfill,
          l_answer     TYPE c.

    CALL METHOD v_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_index_rows
        et_row_no     = t_row_no.

    IF lines( t_row_no ) > 1.
      MESSAGE i000(z_mm) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
      RETURN.

    ELSE.

      LOOP AT t_row_no INTO DATA(w_row_no).
        CLEAR w_saida.
        READ TABLE t_saida INTO w_saida INDEX w_row_no-row_id.
      ENDLOOP.

      v_operacao = 'CHANGE'.
      CALL SCREEN 9100 STARTING AT 10 05.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_ID_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE zm_id_lote INPUT.

    DATA: l_week         TYPE scal-week,
          l_week2        TYPE scal-week,
          l_date_monday  TYPE sy-datum,
          l_date_sunday  TYPE sy-datum,
          l_atual(3)     TYPE c,
          l_sem_atual(2) TYPE c,
          l_tabix        TYPE sy-tabix,
          l_subrc        TYPE sy-subrc,
          l_msg          TYPE bapiret2-message,
          w_zpmt0054     TYPE zpmt0054,
          w_zpmt0054_aux TYPE zpmt0054.

    IF w_saida-werks IS NOT INITIAL AND w_saida-matnr IS NOT INITIAL.

      SELECT SINGLE matnr FROM marc
        INTO @DATA(l_matnr)
         WHERE matnr = @w_saida-matnr
           AND werks = @w_saida-werks.

      IF sy-subrc IS NOT INITIAL.

        CLEAR l_msg.
        CONCATENATE 'Material'
                    w_saida-matnr
                    'não encontrado para o centro'
                     w_saida-werks INTO l_msg SEPARATED BY space.

        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    IF w_saida-ciclo_sem IS NOT INITIAL.

      CLEAR w_zpmt0054.

      CASE w_saida-ciclo_sem.
        WHEN '1'.
          w_saida-ciclo = 7.
        WHEN '2'.
          w_saida-ciclo = 15.
        WHEN '3'.
          w_saida-ciclo = 21.
        WHEN '4'.
          w_saida-ciclo = 30.
      ENDCASE.

      READ TABLE t_zpmt0054 INTO w_zpmt0054_aux
                            WITH KEY werks = w_saida-werks
                                     safra = w_saida-safra
                                     matnr = w_saida-matnr.
      l_subrc = sy-subrc.
      l_tabix = sy-tabix.

      IF  w_zpmt0054_aux-bloquear = abap_false.
        w_zpmt0054-id_lote   = w_saida-id_lote.
        w_zpmt0054-lote      = w_saida-lote.
        w_zpmt0054-ciclo     = w_saida-ciclo.
        w_zpmt0054-ciclo_sem = w_saida-ciclo_sem.
        w_zpmt0054-dia       = w_saida-dia.
        w_zpmt0054-validade  = w_saida-validade.

        IF w_saida-ciclo_sem <> w_zpmt0054_aux-ciclo_sem.
          CLEAR w_zpmt0054-lote.
        ENDIF.

        w_zpmt0054_aux-ciclo_sem = w_saida-ciclo_sem.

        IF l_subrc = 0.
          MODIFY t_zpmt0054   FROM w_zpmt0054_aux INDEX l_tabix.
        ENDIF.

        CALL FUNCTION 'ZSD_CALCULAR_LOTE_SELO'
          EXPORTING
            i_werks                  = w_saida-werks
            i_safra                  = w_saida-safra
            i_matnr                  = w_saida-matnr
            i_data_ref               = sy-datum
            i_zpmt0054               = w_zpmt0054
          IMPORTING
            e_lote                   = w_saida-lote
          EXCEPTIONS
            documento_nao_autorizado = 1
            OTHERS                   = 2.

      ENDIF.
    ENDIF.

  ENDMODULE.
