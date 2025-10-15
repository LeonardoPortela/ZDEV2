*&-------------------------------------------------------------------------------------------------------*
*& Método         : MZSD_APROV_SEM_SALDO_F01 (Include)                                                   *
*& Chamado        : USER STORY 169312                                                                    *
*& Data           : 21/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 21/03/2025|DEVK9A1XAW |NSEGATIN       | Aprovar NFL sem Saldo a Vincular. Desenvolvimento inicial.    *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form zf_seleciona_dados
*&---------------------------------------------------------------------*
*& Seleciona dados para processamento
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados.

*** Busca das Solicitações de Aprovação de NFL com saldo insuficiente.
  SELECT * FROM zsdtvinc_f_aprov INTO TABLE @DATA(tl_tvinc_f_aprov) ORDER BY PRIMARY KEY.

  IF sy-subrc IS INITIAL.
    CLEAR: tg_vinc_f_apv_pen, tg_vinc_f_apv_a_r.

    LOOP AT tl_tvinc_f_aprov INTO DATA(el_tvinc_f_aprov).
* Objeto de autorização Autorização Aprovadores 1x1
      AUTHORITY-CHECK OBJECT 'ZSD_1X1_AP'
       ID 'BUKRS' FIELD el_tvinc_f_aprov-bukrs.

      IF sy-subrc IS INITIAL.
* verifica o status da solicitação de aprovação.
        CASE el_tvinc_f_aprov-status.
          WHEN space.
            APPEND el_tvinc_f_aprov TO tg_vinc_f_apv_pen.

          WHEN OTHERS.
            APPEND el_tvinc_f_aprov TO tg_vinc_f_apv_a_r.

        ENDCASE.

      ENDIF.

    ENDLOOP.

    SORT: tg_vinc_f_apv_pen BY id_aprov docnum_flote bukrs werks matnr,
          tg_vinc_f_apv_a_r BY id_aprov docnum_flote bukrs werks matnr.

  ENDIF.
*** Busca os Aprovadores das Solicitações de Aprovação de NFL com saldo insuficiente.
  SELECT * FROM zsdtvinc_aprov INTO TABLE @DATA(tl_vinc_aprov) ORDER BY PRIMARY KEY.

  IF sy-subrc IS INITIAL.
    CLEAR: tg_vinc_aprov, tg_aprovador_buk.

    LOOP AT tl_vinc_aprov INTO DATA(eg_vinc_aprov).
* Objeto de autorização Autorização Aprovadores 1x1
      AUTHORITY-CHECK OBJECT 'ZSD_1X1_AP'
       ID 'BUKRS' FIELD eg_vinc_aprov-bukrs.

      IF sy-subrc IS INITIAL.
        APPEND eg_vinc_aprov TO tg_vinc_aprov.

        IF eg_vinc_aprov-aprovador EQ sy-uname   AND
           eg_vinc_aprov-ativo     IS NOT INITIAL.
          APPEND eg_vinc_aprov TO tg_aprovador_buk.

        ENDIF.

      ENDIF.

    ENDLOOP.

    SORT tg_vinc_aprov BY aprovador.

  ENDIF.
* Verifica a ação acionada pelo usuário.
  IF sy-ucomm EQ 'SAVE'    OR
     sy-ucomm EQ 'REFRESH'.
    clg_table_pen->refresh( ).
    clg_table_apr->refresh( ).
    clg_table_a_r->refresh( ).

    CLEAR sy-ucomm.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_exibe_dados
*&---------------------------------------------------------------------*
*& Exibe dados selecionados processados
*&---------------------------------------------------------------------*
FORM zf_exibe_dados.

* Declaração de Classes.
  DATA: cll_event_handler TYPE REF TO clg_event_handler,
        cll_column        TYPE REF TO cl_salv_column_table.

  DATA: BEGIN OF el_txt_fld,
          tx_short  TYPE scrtext_s,
          tx_medium TYPE scrtext_m,
          tx_long   TYPE scrtext_l,
        END   OF el_txt_fld.

* Valida se s Classe base para geração de ALVs estão carregadas.
  IF clg_table_pen IS INITIAL AND
     clg_table_apr IS INITIAL AND
     clg_table_a_r IS INITIAL.
* Seleciona dados para processamento
    PERFORM zf_seleciona_dados.
*** Solicitação de Aprovação - Pendente. ***
* Determina o Conteiner principal a ser carregado.
    DATA(cll_container_main) = NEW cl_gui_custom_container( parent         = cl_gui_container=>default_screen
                                                            container_name = 'CSTCTR_PEN'
                                                           ).
*... Create Instance
    CALL METHOD cl_salv_table=>factory
      EXPORTING
        r_container    = cll_container_main
        container_name = 'CSTCTR_PEN'
      IMPORTING
        r_salv_table   = clg_table_pen
      CHANGING
        t_table        = tg_vinc_f_apv_pen.
*... Set PFSTATUS
    CALL METHOD clg_table_pen->get_functions
      RECEIVING
        value = DATA(cll_pfstatus).
    CALL METHOD cll_pfstatus->set_all.
* ... Implement ZEBRA.
    DATA(cll_display) = clg_table_pen->get_display_settings( ).
    cll_display->set_striped_pattern( abap_true ).
    DATA(cll_columns) = clg_table_pen->get_columns( ).
    cll_columns->set_optimize( abap_true ).
* ... Mandante.
    cll_column ?= cll_columns->get_column( 'MANDT' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... ID de Aprovação
    cll_column ?= cll_columns->get_column( 'ID_APROV' ).
    el_txt_fld-tx_short  = el_txt_fld-tx_medium = el_txt_fld-tx_long = 'ID Aprov'.
    cll_column->set_short_text( el_txt_fld-tx_short ).
    cll_column->set_medium_text( el_txt_fld-tx_medium ).
    cll_column->set_long_text( el_txt_fld-tx_long ).
* ... Aprovador
    cll_column ?= cll_columns->get_column( 'US_APROVADOR' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Data Aprovação/Reprovação
    cll_column ?= cll_columns->get_column( 'DT_APROVACAO' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Hora Aprovação/Reprovação
    cll_column ?= cll_columns->get_column( 'HR_APROVACAO' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Status
    cll_column ?= cll_columns->get_column( 'STATUS' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Motivo.
    cll_column ?= cll_columns->get_column( 'MOTIVO' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Implemente type selections rows.
    DATA(cll_selections) = clg_table_pen->get_selections( ).
    cll_selections->set_selection_mode( cl_salv_selections=>row_column ).
* ... Set Configuration of the Save Layout Variant.
    DATA(cll_layout) = clg_table_pen->get_layout( ).
    cll_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*... Display table in ALV Grid
    clg_table_pen->display( ).
*** Aprovador. ***
* Determina o Conteiner principal a ser carregado.
    cll_container_main = NEW cl_gui_custom_container( parent         = cl_gui_container=>default_screen
                                                      container_name = 'CSTCTR_APR'
                                                     ).
*... Create Instance
    CALL METHOD cl_salv_table=>factory
      EXPORTING
        r_container    = cll_container_main
        container_name = 'CSTCTR_APR'
      IMPORTING
        r_salv_table   = clg_table_apr
      CHANGING
        t_table        = tg_vinc_aprov.
*... Set PFSTATUS
    CALL METHOD clg_table_apr->get_functions
      RECEIVING
        value = cll_pfstatus.
    CALL METHOD cll_pfstatus->set_all.
* ... Implement ZEBRA.
    cll_display = clg_table_apr->get_display_settings( ).
    cll_display->set_striped_pattern( abap_true ).
    cll_columns = clg_table_apr->get_columns( ).
    cll_columns->set_optimize( abap_true ).
* ... Mandante.
    cll_column ?= cll_columns->get_column( 'MANDT' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Aprovador
    cll_column ?= cll_columns->get_column( 'APROVADOR' ).
    el_txt_fld-tx_short  = el_txt_fld-tx_medium = el_txt_fld-tx_long = 'Aprovador'.
    cll_column->set_short_text( el_txt_fld-tx_short ).
    cll_column->set_medium_text( el_txt_fld-tx_medium ).
    cll_column->set_long_text( el_txt_fld-tx_long ).
* ... Set Configuration of the Save Layout Variant.
    cll_layout = clg_table_apr->get_layout( ).
    cll_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*... Display table in ALV Grid
    clg_table_apr->display( ).
*** Solicitação de Aprovação - Aprovado/Reprovado. ***
* Determina o Conteiner principal a ser carregado.
    cll_container_main = NEW cl_gui_custom_container( parent         = cl_gui_container=>default_screen
                                                      container_name = 'CSTCTR_A_R'
                                                     ).
*... Create Instance
    CALL METHOD cl_salv_table=>factory
      EXPORTING
        r_container    = cll_container_main
        container_name = 'CSTCTR_A_R'
      IMPORTING
        r_salv_table   = clg_table_a_r
      CHANGING
        t_table        = tg_vinc_f_apv_a_r.
*... Set PFSTATUS
    CALL METHOD clg_table_a_r->get_functions
      RECEIVING
        value = cll_pfstatus.
    CALL METHOD cll_pfstatus->set_all.
* ... Implement ZEBRA.
    cll_display = clg_table_a_r->get_display_settings( ).
    cll_display->set_striped_pattern( abap_true ).
    cll_columns = clg_table_a_r->get_columns( ).
    cll_columns->set_optimize( abap_true ).
* ... Mandante.
    cll_column ?= cll_columns->get_column( 'MANDT' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... ID de Aprovação
    cll_column ?= cll_columns->get_column( 'ID_APROV' ).
    el_txt_fld-tx_short  = el_txt_fld-tx_medium = el_txt_fld-tx_long = 'ID Aprov'.
    cll_column->set_short_text( el_txt_fld-tx_short ).
    cll_column->set_medium_text( el_txt_fld-tx_medium ).
    cll_column->set_long_text( el_txt_fld-tx_long ).
* ... Motivo.
    cll_column ?= cll_columns->get_column( 'MOTIVO' ).
    cll_column->set_visible( abap_false ).
    cll_column->set_technical( abap_true ).
* ... Set Configuration of the Save Layout Variant.
    cll_layout = clg_table_a_r->get_layout( ).
    cll_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*... Set EVENTS
    DATA(cll_events) = clg_table_a_r->get_event( ).
*... Creating an instance for the event handler
    CREATE OBJECT cll_event_handler.
*... Registering handler methods to handle ALV Grid events
    SET HANDLER cll_event_handler->zm_double_click FOR cll_events .
*... Display table in ALV Grid
    clg_table_a_r->display( ).

  ENDIF.
* Verifica qual guia de Controle de tela de guias foi acionada.
  CASE eg_sbsc_9000-pressed_tab.
    WHEN cg_sb_9000-t1_pend. "Lista NFL Pendente
      eg_sbsc_9000-subscreen2 = cg_sb_9000-scr_apr.

    WHEN cg_sb_9000-t2_apre. "Lista NFL Aprovadas/Reprovadas
      eg_sbsc_9000-subscreen2 = cg_sb_9000-scr_mtv.

    WHEN OTHERS.
* Do nothing.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_textedit_motivo
*&---------------------------------------------------------------------*
*& Prepara editor de texto para Motivo Aprovado/Reprovado
*&---------------------------------------------------------------------*
*& -->PT_VINC_F_APROV   TI de exibição do ALV
*& -->UV_CONTAINER_NAME Nome do conteiner
*& -->UV_READONLY_MODE  Somente Leitura (0 = ON 0<> OFF)
*& -->UV_TOOLBAR_MODE   Barra de Ferramenta (1 = ON 0 = OFF)
*& -->UV_STATUSBAR_MODE Barra de Status (1 = ON 0 = OFF)
*& -->UV_WORDWRAP_POSIT Quantidade de caracteres em uma linha
*&---------------------------------------------------------------------*
FORM zf_textedit_motivo TABLES pt_vinc_f_aprov   STRUCTURE   zsdtvinc_f_aprov
                         USING uv_container_name TYPE        char60
                               uv_readonly_mode  TYPE        i
                               uv_toolbar_mode   TYPE        i
                               uv_statusbar_mode TYPE        i
                               uv_wordwrap_posit TYPE        i.

* Verifica se foi acionado um dos Botões de Aprovar ou Reprovar da tela de Motivo.
  IF ( sy-ucomm EQ 'APNFL'   OR  "Botão Aprovar NFL
       sy-ucomm EQ 'RPNFL' ) AND "Botão Reprovar NFL
       sy-dynnr EQ '9310'.
* Refresh e recarrega todo a classe do ALV do relatório de Pendência.
    CALL METHOD clg_table_pen->get_metadata.
* Verifica se a linha foi selecionada no ALV Grid.
    DATA(cll_selections) = clg_table_pen->get_selections( ).
    DATA(tl_sel_rows)    = cll_selections->get_selected_rows( ).

    IF     lines( tl_sel_rows ) GT 1.
      MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'W'.
      LEAVE TO SCREEN 0.

    ELSEIF lines( tl_sel_rows ) EQ 0.
      MESSAGE 'Selecione ao menus uma linha!' TYPE 'S' DISPLAY LIKE 'W'.
      LEAVE TO SCREEN 0.

    ELSE.
      READ TABLE tl_sel_rows INTO DATA(el_sel_rows) INDEX 1.
      READ TABLE tg_vinc_f_apv_pen INTO DATA(el_vinc_f_apv_pen) INDEX el_sel_rows.
      READ TABLE tg_aprovador_buk TRANSPORTING NO FIELDS WITH KEY aprovador = sy-uname
                                                                  bukrs     = el_vinc_f_apv_pen-bukrs.

      IF NOT sy-subrc IS INITIAL.
* Verifica o Status a ser salvo.
        CASE vg_status.
          WHEN sy-abcde(1).    "A - Aprovar
            DATA(vl_tp_action) = 'aprovar'.

          WHEN sy-abcde+17(1). "R - Reprovar
            vl_tp_action = 'reprovar'.

          WHEN OTHERS.
*       Do nothing
        ENDCASE.

        DATA(vl_txt) = | Aprovador em questão não pode { vl_tp_action } a empresa { el_vinc_f_apv_pen-bukrs }.|.
        MESSAGE vl_txt TYPE 'S' DISPLAY LIKE 'W'.
        LEAVE TO SCREEN 0.

      ENDIF.

    ENDIF.

  ENDIF.

  IF clg_txtedit_cstm_cntner IS BOUND.
    CALL METHOD clg_txtedit_cstm_cntner->free
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.

  ENDIF.
* Cria o Container para exibir o Editor.
  CREATE OBJECT clg_txtedit_cstm_cntner
    EXPORTING
      container_name              = uv_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc IS INITIAL.
    IF clg_editor IS BOUND.
      CLEAR: clg_editor.

    ENDIF.
* Formata o editor.
    CREATE OBJECT clg_editor
      EXPORTING
        max_number_chars           = 150
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = uv_wordwrap_posit
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
        parent                     = clg_txtedit_cstm_cntner
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.
* Exibe o Editor somente leitura
    clg_editor->set_readonly_mode( uv_readonly_mode ).
* Inibe a Barra de Tarefa do Editor
    CALL METHOD clg_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = uv_toolbar_mode.
* Inibe a Barra de Status do Editor
    CALL METHOD clg_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = uv_statusbar_mode.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form  zf_double_click
*&---------------------------------------------------------------------*
*& To implement double click in ALV Grid
*&---------------------------------------------------------------------*
*&  -->UV_ROW    Line position clic in ALV grid
*&  -->UV_COLUMN Column position clic in ALV grid
*&---------------------------------------------------------------------*
FORM zf_double_click USING uv_row    TYPE salv_de_row
                           uv_column TYPE salv_de_column.

  DATA: t_text_mtv TYPE trtexts.

* Prepara editor de texto para Motivo Aprovado/Reprovado.
  PERFORM zf_textedit_motivo TABLES tg_vinc_f_apv_a_r
                              USING 'CUSTC_TXTEDITOR2'
                                    cl_gui_textedit=>true
                                    cl_gui_textedit=>false
                                    cl_gui_textedit=>false
                                    57.
* Verifica se a guia selecionada é NFL Aprovadas/Reprovadas.
  CHECK eg_sbsc_9000-pressed_tab EQ cg_sb_9000-t2_apre. "Lista NFL Aprovadas/Reprovadas
  READ TABLE tg_vinc_f_apv_a_r INTO DATA(el_vinc_f_apv_a_r) INDEX uv_row.

  IF sy-subrc IS INITIAL.
    APPEND INITIAL LINE TO t_text_mtv ASSIGNING FIELD-SYMBOL(<fs_trtexts>).
* Indentifica qual é o ID de Aprovação que está sendo exibido na tela de Motivos.
    <fs_trtexts> = |{ el_vinc_f_apv_a_r-id_aprov ALPHA = OUT }|.
    CONCATENATE 'ID Aprov:' <fs_trtexts> INTO <fs_trtexts> SEPARATED BY space.
* Quebra o texto lido em linhas conforme o comprimento necessitado (Comprimento máximo 80 caracteres).
    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = el_vinc_f_apv_a_r-motivo
        iv_len   = 57
      IMPORTING
        et_lines = t_text_mtv.
* Exibe o conteudo carregado na tabela no editor.
    CALL METHOD clg_editor->set_text_as_r3table
      EXPORTING
        table           = t_text_mtv
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_editor_save
*&---------------------------------------------------------------------*
*& Salva o conteúdo do editor na tabela
*&---------------------------------------------------------------------*
FORM zf_editor_save.

  DATA: tl_txt_mvt          TYPE          trtexts,
        tl_vinc_f_apv_email TYPE TABLE OF zsdtvinc_f_aprov.

* Busca o texto editado no Editor do Motivo.
  CALL METHOD clg_editor->get_text_as_r3table
    IMPORTING
      table                  = tl_txt_mvt
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      error_dp_create        = 3
      potential_data_loss    = 4
      OTHERS                 = 5.

  IF tl_txt_mvt[] IS INITIAL.
    MESSAGE 'Texto de motivo é obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE SCREEN.

  ENDIF.
* Refresh e recarrega todo a classe do ALV do relatório de Pendência.
  CALL METHOD clg_table_pen->get_metadata.
* Verifica se a linha foi selecionada no ALV Grid.
  DATA(cll_selections) = clg_table_pen->get_selections( ).
  DATA(tl_sel_rows)    = cll_selections->get_selected_rows( ).

  LOOP AT tl_sel_rows INTO DATA(el_sel_row).
    READ TABLE tg_vinc_f_apv_pen ASSIGNING FIELD-SYMBOL(<fs_vinc_f_apv_pen>) INDEX el_sel_row.

    IF sy-subrc IS INITIAL.
      LOOP AT tl_txt_mvt INTO DATA(el_txt_mvt).
        IF <fs_vinc_f_apv_pen>-motivo IS INITIAL.
          <fs_vinc_f_apv_pen>-motivo = el_txt_mvt.

        ELSE.
          CONCATENATE <fs_vinc_f_apv_pen>-motivo el_txt_mvt INTO <fs_vinc_f_apv_pen>-motivo SEPARATED BY space.

        ENDIF.

      ENDLOOP.

      <fs_vinc_f_apv_pen>-us_aprovador = sy-uname.
      <fs_vinc_f_apv_pen>-dt_aprovacao = sy-datlo.
      <fs_vinc_f_apv_pen>-hr_aprovacao = sy-timlo.
      <fs_vinc_f_apv_pen>-status       = vg_status.

    ENDIF.

  ENDLOOP.

  IF NOT <fs_vinc_f_apv_pen>-motivo IS INITIAL.
    UPDATE zsdtvinc_f_aprov
       SET us_aprovador = sy-uname
           dt_aprovacao = sy-datlo
           hr_aprovacao = sy-timlo
           status       = vg_status
           motivo       = <fs_vinc_f_apv_pen>-motivo
    WHERE id_aprov     EQ <fs_vinc_f_apv_pen>-id_aprov
      AND docnum_flote EQ <fs_vinc_f_apv_pen>-docnum_flote
      AND bukrs        EQ <fs_vinc_f_apv_pen>-bukrs
      AND werks        EQ <fs_vinc_f_apv_pen>-werks
      AND matnr        EQ <fs_vinc_f_apv_pen>-matnr.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
* Verifica qual foi a ação acionada pelo usuário.
      CASE vg_status.
        WHEN sy-abcde(1).    "A - Aprovar
          MESSAGE 'Motivo e Aprovação Salvo com sucesso' TYPE 'S'.

        WHEN sy-abcde+17(1). "R - Reprovar
          MESSAGE 'Motivo e Reprovação Salvo com sucesso' TYPE 'S'.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

      APPEND <fs_vinc_f_apv_pen> TO tl_vinc_f_apv_email.
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
      zcl_im_cl_fluxo_exportacao=>email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_apv_email
                                                             i_bukrs         = <fs_vinc_f_apv_pen>-bukrs
                                                             i_status        = <fs_vinc_f_apv_pen>-status
                                                  ).

    ELSE.
      ROLLBACK WORK.
      CLEAR sy-ucomm.
      MESSAGE 'Erro ao salvar o Motivo e o Status.' TYPE 'E'.

    ENDIF.

  ENDIF.

ENDFORM.
