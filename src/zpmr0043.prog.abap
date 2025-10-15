*&--------------------------------------------------------------------&*
*&                        AMAGGI                                      &*
*&--------------------------------------------------------------------&*
*& Projeto..: SAP PM / Mobile                                                  &*
*& Autor....: Andeoson Oenning                                        &*
*& Data.....: 17/04/2019                                              &*
*& Descrição: Cadastro de usuario para acesso pm mobile               &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Código Espec.Funcional/Técnica: Anderson Oenning                   &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         10.01.2012                            &*
*&--------------------------------------------------------------------&*
REPORT zpmr0043.

TABLES: ztpm_d_usuario.

TYPES:
  BEGIN OF ty_permit.

    INCLUDE STRUCTURE zpmt0012.
TYPES: cellsty TYPE lvc_t_styl.

TYPES END OF ty_permit.

DATA: gt_usuario TYPE ztpm_d_usuario.
DATA: gw_usuario TYPE TABLE OF ztpm_d_usuario.
DATA: gt_permit  TYPE TABLE OF ty_permit.
DATA: gt_permit_aux  TYPE TABLE OF zpmt0012.
DATA: p_erro TYPE char1.
DATA: modif  TYPE char1.
DATA: tg_usrefus    TYPE TABLE OF ztpm_d_usuario WITH HEADER LINE,
      it_msg_return TYPE TABLE OF zfiwrs0002,
      gt_log        TYPE TABLE OF ztpm_d_m_log,
      gt_log_alv    TYPE TABLE OF ztpm_d_m_log.

*Parametros conteiner 01
DATA:
  wa_cont         TYPE REF TO cl_gui_custom_container,
  wa_alv          TYPE REF TO cl_gui_alv_grid,
  wa_layout       TYPE lvc_s_layo,
  wa_fcat         TYPE lvc_s_fcat,
  it_select_rows  TYPE lvc_t_row,
  it_fcat         TYPE TABLE OF lvc_s_fcat,
  it_p_rows       TYPE lvc_t_row,
  wa_select_rows  TYPE lvc_s_row,
  wa_p_rows       TYPE lvc_s_row,
  clicks          TYPE sy-tabix,
  tg_selectedcell TYPE lvc_t_cell,
  wg_selectedcell TYPE lvc_s_cell.

*Parametros conteiner 02
DATA:
  wa_cont_2        TYPE REF TO cl_gui_custom_container,
  wa_alv_2         TYPE REF TO cl_gui_alv_grid,
  wa_layout_2      TYPE lvc_s_layo,
  wa_fcat_2        TYPE lvc_s_fcat,
  it_select_rows_2 TYPE lvc_t_row,
  it_fcat_2        TYPE TABLE OF lvc_s_fcat,
  it_p_rows_2      TYPE lvc_t_row,
  wa_select_rows_2 TYPE lvc_s_row,
  wa_p_rows_2      TYPE lvc_s_row,
  clicks_2         TYPE sy-tabix,
  lt_f4            TYPE lvc_t_f4 WITH HEADER LINE,
  wa_stable_2      TYPE lvc_s_stbl,
  tl_function      TYPE ui_functions,
  wl_function      LIKE tl_function WITH HEADER LINE,
  w_fieldcatalog   TYPE lvc_s_fcat,
  lt_sort          TYPE lvc_t_sort.

*Parametros container 03
DATA:
  wa_cont_3        TYPE REF TO cl_gui_custom_container,
  wa_alv_3         TYPE REF TO cl_gui_alv_grid,
  wa_layout_3      TYPE lvc_s_layo,
  wa_fcat_3        TYPE lvc_s_fcat,
  it_select_rows_3 TYPE lvc_t_row,
  it_fcat_3        TYPE TABLE OF lvc_s_fcat,
  it_p_rows_3      TYPE lvc_t_row,
  wa_select_rows_3 TYPE lvc_s_row,
  wa_p_rows_3      TYPE lvc_s_row,
  clicks_3         TYPE sy-tabix,
  lt_f4_3          TYPE lvc_t_f4 WITH HEADER LINE,
  wa_stable_3      TYPE lvc_s_stbl,
  tl_function_3    TYPE ui_functions,
  wl_function_3    LIKE tl_function WITH HEADER LINE,
  w_fieldcatalog_3 TYPE lvc_s_fcat.

DATA:wa_toolbar    TYPE stb_button.
DATA:w_toolbar    TYPE stb_button.
*===============================================


*===============================================

CLASS events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

    CLASS-METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS events IMPLEMENTATION.

  METHOD on_toolbar.

    CLEAR w_toolbar.
    w_toolbar-function     = 'LOG'.
*    w_toolbar-icon         =  icon.
    w_toolbar-quickinfo    = 'Log de modificações'.
    w_toolbar-butn_type    = 0.
    w_toolbar-text         = 'Log de modificações'.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    w_toolbar-function     = 'MARCARTODOS'.
    w_toolbar-icon         =  '@B_MRKA@'.
    w_toolbar-quickinfo    = 'Marcar Todos'.
    w_toolbar-butn_type    = 0.
*    w_toolbar-text         = 'Marcar Todos'.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    w_toolbar-function     = 'DESMARCAR'.
    w_toolbar-icon         =  '@B_MRKD@'.
    w_toolbar-quickinfo    = 'Desmarcar todos'.
    w_toolbar-butn_type    = 0.
*    w_toolbar-text         = 'Desmarcar todos'.
    APPEND w_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
        <fs_tollbar>-function = 'INSERT_ROW'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-function = 'DELETE_ROW'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
        <fs_tollbar>-function = 'COPY_ROW'.
      ENDIF.
    ENDLOOP.
*
*    CLEAR w_toolbar.
*    w_toolbar-function     = 'BTN_DELETE'.
*    w_toolbar-icon         =  icon_delete_row.
*    w_toolbar-quickinfo    = 'Delete'.
*    w_toolbar-butn_type    = 0.
*    w_toolbar-text         = 'Delete'.
*    APPEND w_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD on_data_changed.
    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_tabix   TYPE sy-tabix,
          vl_value   TYPE lvc_value,
          wl_usrefus LIKE LINE OF tg_usrefus,
          wl_depto   TYPE zimp_cad_depto,
          lw_log     TYPE ztpm_d_m_log,
          lt_log1    TYPE TABLE OF ztpm_d_m_log,
          lt_cellsty TYPE lvc_t_styl,
          lw_cellsty TYPE lvc_s_styl.
*          WL_SAIDA   LIKE LINE OF TG_SAIDA,
*          VGRUPO     TYPE ZFIT0032-GRUPO.

    FIELD-SYMBOLS: <fs_value> TYPE any,
                   <fs_valor> TYPE iwerk.

    DATA(lt_permit) = gt_permit.
    SORT lt_permit BY pernr.
    DELETE ADJACENT DUPLICATES FROM lt_permit COMPARING pernr.
    IF lt_permit IS NOT INITIAL.
      SELECT *
        FROM ztpm_d_m_log
        INTO TABLE @DATA(lt_log)
        FOR ALL ENTRIES IN @lt_permit
        WHERE usuario = @lt_permit-pernr.
      IF sy-subrc IS INITIAL.
        SORT lt_log BY usuario transacao.
      ENDIF.
    ENDIF.

    SORT gt_log BY usuario transacao campo_modific.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_cells>).
      READ TABLE gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>) INDEX <fs_cells>-row_id.
      IF sy-subrc IS INITIAL.
        <fs_permit>-data_modific = sy-datum.
        <fs_permit>-modificado_por = sy-uname.

        READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<fs_log>)
        WITH KEY usuario   = <fs_permit>-pernr
                 transacao = <fs_permit>-transacao
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lw_log-transacao = <fs_log>-transacao.
          lw_log-criado_em = <fs_log>-criado_em.
          lw_log-criado_por = <fs_log>-criado_por.
        ELSE.
          lw_log-transacao = <fs_permit>-transacao.
          lw_log-criado_em = sy-datum.
          lw_log-criado_por = sy-uname.
        ENDIF.

        lw_log-usuario = <fs_permit>-pernr.
        lw_log-data_modific = sy-datum.
        lw_log-hora_modif = sy-uzeit.
        lw_log-modificado_por = sy-uname.
        lw_log-campo_modific = <fs_cells>-fieldname.

        READ TABLE gt_log ASSIGNING <fs_log>
        WITH KEY usuario = <fs_permit>-pernr
                 transacao = <fs_permit>-transacao
                 campo_modific = lw_log-campo_modific
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          <fs_log>-valor_novo = <fs_cells>-value.

          CALL METHOD wa_alv_2->refresh_table_display
            EXPORTING
              is_stable = wa_stable_2.
          CONTINUE.

        ENDIF.

        CASE <fs_cells>-fieldname.
          WHEN 'MODIFICAR'.
            ASSIGN COMPONENT 'MODIFICAR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'MODIFICAR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'MODIFICAR'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.


            ENDIF.
          WHEN 'EXIBIR'.
            ASSIGN COMPONENT 'EXIBIR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'EXIBIR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'EXIBIR'.
              lw_log-valor_antigo = <fs_value>.


              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.

            ENDIF.
          WHEN 'CRIAR'.
            ASSIGN COMPONENT 'CRIAR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'CRIAR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'CRIAR'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.

            ENDIF.
          WHEN 'ENCERRAR'.
            ASSIGN COMPONENT 'ENCERRAR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'ENCERRAR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'ENCERRAR'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.

            ENDIF.
          WHEN 'IWERK'.

            ASSIGN COMPONENT 'IWERK' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'IWERK'.
                lw_log-valor_novo = <fs_cells>-value.
*                <fs_value> = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'CENTRO'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.


            ENDIF.

          WHEN 'LIBERAR'.

            ASSIGN COMPONENT 'LIBERAR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'LIBERAR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'LIBERAR'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.

            ENDIF.


          WHEN 'ENCERRAR'.

            ASSIGN COMPONENT 'ENCERRAR' OF STRUCTURE <fs_permit> TO <fs_value>.
            IF <fs_value> IS ASSIGNED.

              IF <fs_cells>-fieldname = 'ENCERRAR'.
                lw_log-valor_novo = <fs_cells>-value.
              ENDIF.

              lw_log-campo_modific = 'ENCERRAR'.
              lw_log-valor_antigo = <fs_value>.

              IF lw_log-valor_antigo <> lw_log-valor_novo.
                APPEND lw_log TO gt_log.
              ENDIF.

            ENDIF.

          WHEN 'TRANSACAO'.

*            ASSIGN COMPONENT 'TRANSACAO' OF STRUCTURE <fs_permit> TO <fs_value>.
*            IF <fs_value> IS ASSIGNED.
*
*              IF <fs_cells>-fieldname = 'TRANSACAO'.
*                lw_log-valor_novo = <fs_cells>-value.
*                <fs_value> = <fs_cells>-value.
*              ENDIF.
*
*              lw_log-campo_modific = 'TRANSACAO'.
*              lw_log-valor_antigo = <fs_value>.
*
*              IF lw_log-valor_antigo <> lw_log-valor_novo.
*                APPEND lw_log TO gt_log.
*              ENDIF.

*            ENDIF.
          WHEN OTHERS.
        ENDCASE.

      ENDIF.
    ENDLOOP.

*    SORT gt_permit BY transacao.

*    CALL METHOD wa_alv_2->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable_2.

*    IF lt_log1 IS NOT INITIAL.
*      MODIFY ztpm_d_m_log FROM TABLE lt_log1.
*    ENDIF.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'PERNR'.
      READ TABLE gt_permit INTO DATA(wl_saida) INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      SELECT SINGLE pernr
      FROM ztpm_d_usuario
        INTO wl_usrefus
          WHERE pernr EQ lv_value.

      MOVE: wl_usrefus-pernr TO lv_value.

      IF ls_good-fieldname EQ 'PERNR' AND lv_value > 1.

*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'CPF_NR'
*            I_VALUE     = LV_VALUE.

      ELSE.
        MESSAGE |Usuario não cadastrado!| TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CLEAR: wl_usrefus, lv_value.





    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished.

    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_tabix   TYPE sy-tabix,
          vl_value   TYPE lvc_value,
          wl_usrefus LIKE LINE OF tg_usrefus,
          wl_depto   TYPE zimp_cad_depto,
          lw_log     TYPE ztpm_d_m_log,
          lt_log1    TYPE TABLE OF ztpm_d_m_log,
          lt_cellsty TYPE lvc_t_styl,
          lw_cellsty TYPE lvc_s_styl.
*          WL_SAIDA   LIKE LINE OF TG_SAIDA,
*          VGRUPO     TYPE ZFIT0032-GRUPO.

    FIELD-SYMBOLS: <fs_value> TYPE any,
                   <fs_valor> TYPE iwerk.


    SORT gt_log BY usuario transacao campo_modific.

    LOOP AT et_good_cells ASSIGNING FIELD-SYMBOL(<fs_cells>).
      READ TABLE gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>) INDEX <fs_cells>-row_id.
      IF sy-subrc IS INITIAL.
        <fs_permit>-data_modific = sy-datum.
        <fs_permit>-modificado_por = sy-uname.

      ENDIF.
    ENDLOOP.

    IF et_good_cells IS NOT INITIAL.
      CALL METHOD wa_alv_2->refresh_table_display
        EXPORTING
          is_stable = wa_stable_2.
    ENDIF.

  ENDMETHOD.

  METHOD on_onf4.

  ENDMETHOD.

  METHOD handle_user_command.
    DATA: w_saida        TYPE zpmt001,
          lt_stable      TYPE lvc_s_stbl,
          lt_select_rows TYPE lvc_t_row,
          lt_cellsty     TYPE lvc_t_styl,
          lw_cellsty     TYPE lvc_s_styl,
          lv_index       TYPE sy-tabix,
          lv_centro      TYPE werks_d,
          lt_fields      TYPE TABLE OF sval,
          ls_permit      TYPE ty_permit,
          lv_filename    TYPE localfile.

    CASE e_ucomm.
      WHEN 'BTN_ADD'.










*        gt_permit_aux[] = gt_permit[].
*        REFRESH: gt_permit.
*        LOOP AT gt_permit_aux INTO w_saida.
*          APPEND  w_saida TO gt_permit.
*        ENDLOOP.
*        CLEAR: w_saida.
**        MOVE ICON_UNLOCKED TO WL_SAIDA-STATUS_CONTAB.
*        APPEND w_saida TO gt_permit.
*
*        CALL METHOD wa_alv_2->refresh_table_display
*          EXPORTING
*            is_stable = wa_stable_2.

      WHEN 'BTN_DELETE'.

        DATA: p_resp,
           lv_msg TYPE bapi_msg.

        CLEAR: it_select_rows[], wa_select_rows.

        CALL METHOD wa_alv_2->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        IF it_select_rows[] IS NOT INITIAL.

*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING        "TITLEBAR = 'Confirmar'
*              TEXT_QUESTION         = 'Deseja realmente excluir a linha?'
*              TEXT_BUTTON_1         = 'Sim'
*              TEXT_BUTTON_2         = 'Não'
*              DISPLAY_CANCEL_BUTTON = ' '
*            IMPORTING
*              ANSWER                = P_RESP.
*
*          IF P_RESP = 1.

          LOOP AT it_select_rows INTO wa_select_rows.

            READ TABLE gt_permit INTO DATA(wa_s_report) INDEX wa_select_rows-index.

            DELETE FROM zpmt0012
            WHERE pernr = wa_s_report-pernr
              AND iwerk = wa_s_report-iwerk
              AND transacao =  wa_s_report-transacao.

          ENDLOOP.

          IF sy-subrc = 0.
            MESSAGE |Informações excluida com sucesso!| TYPE 'I' DISPLAY LIKE 'S'.
*              PERFORM SEL_DADOS.

*            PERFORM selec_usuario USING ''.
            CALL METHOD wa_alv_2->refresh_table_display
              EXPORTING
                is_stable = wa_stable_2.

            LEAVE TO SCREEN 0.
          ENDIF.
        ELSE.
          MESSAGE |Necessario selecionar uma linha para excluir!| TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'LOG'.

        CALL SCREEN '0400'.

      WHEN  'INSERT_ROW'.

        APPEND INITIAL LINE TO gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>).

        CALL METHOD wa_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        READ TABLE it_select_rows ASSIGNING FIELD-SYMBOL(<fs_rows>) INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE gw_usuario ASSIGNING FIELD-SYMBOL(<fs_usuarios>) INDEX <fs_rows>-index.
          IF sy-subrc IS INITIAL.
            <fs_permit>-pernr = <fs_usuarios>-pernr.
          ENDIF.
        ENDIF.


        lw_cellsty-fieldname = 'IWERK'.
        lw_cellsty-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND lw_cellsty TO lt_cellsty.
*
*        lw_cellsty-fieldname = 'PERNR'.
*        lw_cellsty-style = cl_gui_alv_grid=>mc_style_enabled.
*        APPEND lw_cellsty TO lt_cellsty.

        lw_cellsty-fieldname = 'TRANSACAO'.
        lw_cellsty-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND lw_cellsty TO lt_cellsty.

        <fs_permit>-cellsty = lt_cellsty.

        CALL METHOD wa_alv_2->refresh_table_display
          EXPORTING
            is_stable = wa_stable_2.

      WHEN  'DELETE_ROW'.

        CALL METHOD wa_alv_2->get_selected_rows
          IMPORTING
            et_index_rows = lt_select_rows.

        IF lt_select_rows[] IS NOT INITIAL.

          SORT lt_select_rows BY index DESCENDING.

          LOOP AT lt_select_rows ASSIGNING <fs_rows>.
            READ TABLE gt_permit ASSIGNING <fs_permit> INDEX <fs_rows>-index.

            DELETE FROM zpmt0012 WHERE pernr = <fs_permit>-pernr AND
                                       iwerk = <fs_permit>-iwerk AND
                                       transacao = <fs_permit>-transacao.

            DELETE gt_permit INDEX <fs_rows>-index.

          ENDLOOP.

        ENDIF.

        CALL METHOD wa_alv_2->refresh_table_display
          EXPORTING
            is_stable = wa_stable_2.

      WHEN 'MARCARTODOS'.

        CALL METHOD wa_alv_2->get_selected_rows
          IMPORTING
            et_index_rows = lt_select_rows.

        IF lt_select_rows[] IS NOT INITIAL.

          SORT lt_select_rows BY index DESCENDING.

          LOOP AT lt_select_rows ASSIGNING <fs_rows>.
            READ TABLE gt_permit ASSIGNING <fs_permit> INDEX <fs_rows>-index.

            <fs_permit>-exibir = abap_true.
            <fs_permit>-criar = abap_true.
            <fs_permit>-liberar = abap_true.
            <fs_permit>-modificar = abap_true.
            <fs_permit>-aprovar_orc = abap_true.
            <fs_permit>-encerrar = abap_true.

          ENDLOOP.

        ENDIF.

        CALL METHOD wa_alv_2->refresh_table_display
          EXPORTING
            is_stable = wa_stable_2.

      WHEN 'DESMARCAR'.

        CALL METHOD wa_alv_2->get_selected_rows
          IMPORTING
            et_index_rows = lt_select_rows.

        IF lt_select_rows[] IS NOT INITIAL.

          SORT lt_select_rows BY index DESCENDING.

          LOOP AT lt_select_rows ASSIGNING <fs_rows>.
            READ TABLE gt_permit ASSIGNING <fs_permit> INDEX <fs_rows>-index.

            <fs_permit>-exibir = abap_false.
            <fs_permit>-criar = abap_false.
            <fs_permit>-liberar = abap_false.
            <fs_permit>-modificar = abap_false.
            <fs_permit>-aprovar_orc = abap_false.
            <fs_permit>-encerrar = abap_false.

          ENDLOOP.

        ENDIF.

        CALL METHOD wa_alv_2->refresh_table_display
          EXPORTING
            is_stable = wa_stable_2.

      WHEN 'COPY_ROW'.

        CALL METHOD wa_alv_2->get_selected_rows
          IMPORTING
            et_index_rows = lt_select_rows.

        IF lt_select_rows[] IS NOT INITIAL.

          LOOP AT lt_select_rows ASSIGNING <fs_rows>.
            FREE lt_cellsty.
            READ TABLE gt_permit ASSIGNING <fs_permit> INDEX <fs_rows>-index.
            IF sy-subrc IS INITIAL.

              IF lv_centro IS INITIAL.
                lv_centro = <fs_permit>-iwerk.
              ELSEIF lv_centro <> <fs_permit>-iwerk.
                MESSAGE 'Para copia de mais de um centro, favor usar upload por arquivo!' TYPE 'E'.
              ENDIF.


            ENDIF.

          ENDLOOP.

          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
          <fs_fields>-tabname = 'ZPMT0012'.
          <fs_fields>-fieldname = 'IWERK'.

          CALL FUNCTION 'POPUP_GET_VALUES'
            EXPORTING
              popup_title     = 'Informar Centro'
            TABLES
              fields          = lt_fields
            EXCEPTIONS
              error_in_fields = 1
              OTHERS          = 2.
          IF sy-subrc = 0.

            READ TABLE lt_fields ASSIGNING <fs_fields> INDEX 1.
            IF sy-subrc IS INITIAL.
              IF <fs_fields>-value IS NOT INITIAL.

                SORT lt_select_rows BY index DESCENDING.

                LOOP AT lt_select_rows ASSIGNING <fs_rows>.
                  FREE lt_cellsty.
                  READ TABLE gt_permit ASSIGNING <fs_permit> INDEX <fs_rows>-index.

                  lv_index = <fs_rows>-index + 1.

                  ls_permit = <fs_permit>.
                  READ TABLE lt_fields ASSIGNING <fs_fields> INDEX 1.
                  IF sy-subrc IS INITIAL.
                    ls_permit-iwerk = <fs_fields>-value.
                  ENDIF.

                  lw_cellsty-fieldname = 'IWERK'.
                  lw_cellsty-style = cl_gui_alv_grid=>mc_style_enabled.

                  APPEND lw_cellsty TO lt_cellsty.

                  ls_permit-cellsty = lt_cellsty.
                  INSERT ls_permit INTO gt_permit INDEX lv_index.

                ENDLOOP.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

        CALL METHOD wa_alv_2->refresh_table_display
          EXPORTING
            is_stable = wa_stable_2.

      WHEN 'UPLOAD'.

        PERFORM f_select_file USING lv_filename.
        PERFORM f_carrega_arquivo USING lv_filename.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.


CLASS event DEFINITION.
  PUBLIC SECTION.


    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.



CLASS event IMPLEMENTATION.
*  METHOD CONSTRUCTOR.
**   Create ALV toolbar manager instance
*    CREATE OBJECT C_ALV_TOOLBARMANAGER
*      EXPORTING
*        IO_ALV_GRID = IO_ALV_GRID.
*  ENDMETHOD.                    "constructor

  METHOD on_double_click.

  ENDMETHOD.



  METHOD set_toolbar.
    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_NOVO'.
    wa_toolbar-icon         =  icon_create.
    wa_toolbar-quickinfo    = 'NOVO'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Novo'.
    APPEND wa_toolbar TO e_object->mt_toolbar.

    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_DEL'.
    wa_toolbar-icon         =  icon_delete_row.
    wa_toolbar-quickinfo    = 'EXCLUIR'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Excluir'.
    APPEND wa_toolbar TO e_object->mt_toolbar.

    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_MODIF'.
    wa_toolbar-icon         =  icon_change.
    wa_toolbar-quickinfo    = 'MODIFICAR'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Modificar'.
    APPEND wa_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

*  METHOD HANDLE_DATA_CHANGED.
**    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
*  ENDMETHOD.

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'BTN_NOVO'.

        CLEAR: modif,
               gt_usuario-pernr.

        CALL SCREEN 0200 STARTING AT 8 8.

      WHEN 'BTN_DEL'.

        DATA: p_resp,
           lv_msg TYPE bapi_msg.

        CLEAR: it_select_rows[], wa_select_rows.

        CALL METHOD wa_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        IF it_select_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING        "TITLEBAR = 'Confirmar'
              text_question         = 'Deseja realmente excluir a linha?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              display_cancel_button = ' '
            IMPORTING
              answer                = p_resp.

          IF p_resp = 1.

            LOOP AT it_select_rows INTO wa_select_rows.

              READ TABLE gw_usuario INTO DATA(wa_s_report) INDEX wa_select_rows-index.

              DELETE FROM ztpm_d_usuario
              WHERE pernr = wa_s_report-pernr
              AND cname =  wa_s_report-cname
              AND cpf_nr = wa_s_report-cpf_nr.
*              AND login = wa_s_report-login.

              DELETE FROM zpmt0012
              WHERE pernr = wa_s_report-pernr.
*              AND iwerk = wa_s_report-iwerk.

              COMMIT WORK.
            ENDLOOP.

            IF sy-subrc = 0.
              MESSAGE |Informações excluida com sucesso!| TYPE 'I' DISPLAY LIKE 'S'.
              PERFORM sel_dados.

            ENDIF.
          ENDIF.

        ELSE.
          MESSAGE |Necessario selecionar uma linha para excluir!| TYPE 'I' DISPLAY LIKE 'E'.
          PERFORM sel_dados.
        ENDIF.


      WHEN 'BTN_MODIF'.

        DATA: q_linha TYPE char1.

        CLEAR: modif, lv_msg, p_resp, it_select_rows[], wa_select_rows, q_linha.

        CALL METHOD wa_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        IF it_select_rows[] IS NOT INITIAL.

          LOOP AT it_select_rows INTO DATA(_linha).
            ADD 1 TO q_linha.
          ENDLOOP.

          IF q_linha EQ 1.

            LOOP AT it_select_rows INTO wa_select_rows.
              READ TABLE gw_usuario INTO DATA(wa_report) INDEX wa_select_rows-index.

              SELECT SINGLE *
              FROM ztpm_d_usuario
              INTO @DATA(_usuario)
              WHERE pernr EQ @wa_report-pernr.

              gt_usuario-pernr   = _usuario-pernr.
              gt_usuario-cname   = _usuario-cname.
              gt_usuario-cpf_nr  = _usuario-cpf_nr.
              modif = abap_true.
              CALL SCREEN 0200 STARTING AT 8 8.
            ENDLOOP.

          ELSE.
            MESSAGE |Selecionar somente a informação que sera alterada!| TYPE 'I' DISPLAY LIKE 'S'.
          ENDIF.
        ELSE.
          MESSAGE |É necessario selecionar a informação que sera alterada!| TYPE 'I' DISPLAY LIKE 'E'.
          PERFORM sel_dados.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


INITIALIZATION.

START-OF-SELECTION.

  PERFORM sel_dados.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'TL001'.
  SET TITLEBAR 'SET_001'.

  SORT gw_usuario BY cname.

  IF wa_cont IS INITIAL.
    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    PERFORM z_fieldcat.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    DATA(obj_even) = NEW event( ).
    SET HANDLER:obj_even->on_double_click  FOR wa_alv.
    SET HANDLER:obj_even->set_toolbar      FOR wa_alv.
    SET HANDLER:obj_even->get_ucomm        FOR wa_alv.


    CALL METHOD wa_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
      CHANGING
        it_outtab                     = gw_usuario
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL METHOD wa_alv->refresh_table_display.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_filename    TYPE localfile.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_PERM'.

*      IF SY-SUBRC IS INITIAL.
*      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
*        EXPORTING
*          ACTION    = 'U'
*          VIEW_NAME = 'ZPMT0012'.
      PERFORM fm_sel_usuario.

    WHEN 'BTN_UPLOAD'.

      PERFORM f_select_file USING lv_filename.
      PERFORM f_carrega_arquivo USING lv_filename.

    WHEN 'BTN_MODELO'.

      PERFORM f_gera_modelo_planilha.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM check_usuario  CHANGING p_erro.
  CLEAR: p_erro.

  IF gt_usuario-pernr IS NOT INITIAL.
    gt_usuario-pernr = |{ gt_usuario-pernr ALPHA = IN }|.

    SELECT SINGLE *
    FROM ztpm_d_usuario
    INTO @DATA(_usuario)
    WHERE pernr EQ @gt_usuario-pernr.

    IF _usuario-pernr IS INITIAL.

      SELECT SINGLE *
      FROM pa0002
      INTO @DATA(_pa0002)
      WHERE pernr EQ @gt_usuario-pernr.

      IF _pa0002 IS NOT INITIAL.
        gt_usuario-cname =  _pa0002-cname.

        TRANSLATE gt_usuario-cname TO UPPER CASE.

        SELECT SINGLE *
        FROM pa0465 AS a
        INTO @DATA(_pa0465)
        WHERE pernr EQ @gt_usuario-pernr
        AND  tpdoc EQ '0001'.
        gt_usuario-cpf_nr = _pa0465-cpf_nr.

      ELSE.
        MESSAGE |Crachar invalido| TYPE 'I' DISPLAY LIKE 'E'.
        p_erro = abap_true.
        CLEAR: gt_usuario-cname, gt_usuario-cpf_nr.
        EXIT.
      ENDIF.

    ELSE.
      MESSAGE | Usuario ja cadastrado! | TYPE 'I' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      CLEAR: gt_usuario-cname, gt_usuario-cpf_nr.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE | Informar o cracha do usuario! | TYPE 'I' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    EXIT.
  ENDIF.
  CLEAR:_pa0002, _pa0465, _usuario.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'TL002'.
  SET TITLEBAR 'TI002'.

  IF modif IS NOT INITIAL.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'GT_USUARIO-PERNR'.
          screen-input = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: wa_usuarios1 TYPE ztpm_d_m_usuario,
        lw_usuarios  TYPE ztpm_d_m_usuario,
        lv_string1   TYPE string,
        lt_paramtab1 TYPE TABLE OF abap_func_parmbind_tab,
        lt_prop_tab1 TYPE js_property_tab,
        ls_object1   TYPE REF TO cl_java_script,
        lv_length1   TYPE sy-tabix.


  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: gt_usuario.
      LEAVE TO SCREEN 0.
    WHEN 'ENTER'.
      PERFORM check_usuario CHANGING p_erro.

    WHEN 'BTN_CONF'.
      IF modif IS INITIAL.
        PERFORM check_usuario CHANGING p_erro.
      ENDIF.
      IF p_erro IS INITIAL.
        IF gt_usuario-login IS NOT INITIAL.
          IF  gt_usuario-login_conf IS NOT INITIAL.
            IF gt_usuario-login_conf EQ gt_usuario-login.

              DELETE gw_usuario WHERE pernr = gt_usuario-pernr.
              APPEND gt_usuario TO gw_usuario.
              MODIFY ztpm_d_usuario FROM gt_usuario.

              CALL FUNCTION 'ZPM_EXPORT_DATA_TO_PM_MOBILE'
                EXPORTING
                  view   = 'vw_d_usuario'
                  pernr  = gt_usuario-pernr
                IMPORTING
                  result = lv_string1.

              lv_length1 = strlen( lv_string1 ).

              lv_length1 = lv_length1 - 1.
              lv_string1 = lv_string1+1(lv_length1).

              /ui2/cl_json=>deserialize( EXPORTING json = lv_string1 CHANGING data = lw_usuarios ).

              REFRESH lw_usuarios-perfil.

              DATA(lt_permit1) = gt_permit.
              SORT lt_permit1 BY pernr.

              DELETE lt_permit1 WHERE pernr NE lw_usuarios-pernr.

              LOOP AT lt_permit1 ASSIGNING FIELD-SYMBOL(<fs_permit1>).

                APPEND INITIAL LINE TO lw_usuarios-perfil ASSIGNING FIELD-SYMBOL(<fs_perfil1>).
                <fs_perfil1>-iwerk     = <fs_permit1>-iwerk    .
                <fs_perfil1>-transacao = <fs_permit1>-transacao.
                <fs_perfil1>-exibir    = <fs_permit1>-exibir   .
                <fs_perfil1>-modificar = <fs_permit1>-modificar.
                <fs_perfil1>-criar     = <fs_permit1>-criar    .
                <fs_perfil1>-liberar   = <fs_permit1>-liberar  .
                <fs_perfil1>-encerrar  = <fs_permit1>-encerrar  .
                <fs_perfil1>-aprovar_orc = <fs_permit1>-aprovar_orc.

              ENDLOOP.

              TRY.

                  zcl_permissao_usuarios=>zif_permissao_usuarios~get_instance(
                        )->set_dados_usuario( i_data = lw_usuarios
                        )->post_permissao_usuario( EXPORTING i_usuario = lw_usuarios ).


                CATCH zcx_integracao INTO DATA(ex_integra1).    "
                  ex_integra1->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

                CATCH zcx_error INTO DATA(ex_error1).    "  "
                  ex_error1->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

              ENDTRY.

              COMMIT WORK.

              MESSAGE |Usuario cadastrado com sucesso| TYPE 'S'.
              CLEAR: gt_usuario.
            ELSE.
              MESSAGE |Entrar senhas idênticas!| TYPE 'I' DISPLAY LIKE 'E'.
            ENDIF.
          ELSE.
            MESSAGE |Confirmar senha!| TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          MESSAGE |Informar a senha!| TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

      SORT gw_usuario BY pernr.
      CALL METHOD wa_alv->refresh_table_display.
      LEAVE TO SCREEN 0.

    WHEN 'BTN_CANC'.
      CLEAR: gt_usuario.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_dados.

  CLEAR: gw_usuario.
  SELECT *
  FROM ztpm_d_usuario
  INTO TABLE gw_usuario.


  PERFORM selec_usuario.
  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fieldcat .

  PERFORM z_feed_fieldcat USING:
       1  'PERNR    '  'GW_USUARIO'  '30'   ' '  ' '  ' '  'Nº pessoal        '  ''  ' '  ' '  ' ',
       2  'CNAME    '  'GW_USUARIO'  '30'   ' '  ' '  ' '  'Nome usuario      '  ''  ' '  ' '  ' ',
       3  'CPF_NR   '  'GW_USUARIO'  '15'   ' '  ' '  ' '  'CPF               '  ''  ' '  ' '  ' '.


ENDFORM.

FORM z_fieldcat_2 .

  PERFORM z_feed_fieldcat_2 USING:
       1  'PERNR       '   'GT_PERMIT'  '10'   ''  ' '  ' '  'Nº pessoal  '  ''  ' '  ' '  ' '  ' '.







  READ TABLE gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>) INDEX 1.
  IF <fs_permit> IS ASSIGNED AND <fs_permit>-iwerk IS NOT INITIAL.

    PERFORM z_feed_fieldcat_2 USING:
          3  'IWERK       '   'GT_PERMIT'  '10'   ''  ' '  ' '  'Centro      '  ''  ' '  ' '  ' '  ' '.

  ELSE.

    PERFORM z_feed_fieldcat_2 USING:
       3  'IWERK       '   'GT_PERMIT'  '10'   'X'  ' '  ' '  'Centro      '  ''  ' '  ' '  ' '  ' '.

  ENDIF.

  PERFORM z_feed_fieldcat_2 USING:
       2  'TRANSACAO   '   'GT_PERMIT'  '15'   ''  ' '  ' '  'Transação   '  ''  ' '  ' '  ' '  ' ',
       4  'EXIBIR      '   'GT_PERMIT'  '10'   'X'  'X'  ' '  'Exibir      '  ''  ' '  ' '  ' '  'X',
       5  'MODIFICAR   '   'GT_PERMIT'  '15'   'X'  'X'  ' '  'Modificar   '  ''  ' '  ' '  ' '  'X',
       6  'CRIAR       '   'GT_PERMIT'  '15'   'X'  'X'  ' '  'Criar       '  ''  ' '  ' '  ' '  'X',
       7  'LIBERAR     '   'GT_PERMIT'  '15'   'X'  'X'  ' '  'Liberar     '  ''  ' '  ' '  ' '  'X',
       8  'ENCERRAR    '   'GT_PERMIT'  '15'   'X'  'X'  ' '  'Encerrar    '  ''  ' '  ' '  ' '  'X',
       09 'APROVAR_ORC '   'GT_PERMIT'  '15'   'X'  'X'  ' '  'Aprovar Orç.    '  ''  ' '  ' '  ' '  'X',
       10  'MODIFICADO_POR' 'GT_PERMIT'  '15'   ''  ''  ' '  'Modificado por'  ''  ' '  ' '  ' '  '',
       11  'DATA_MODIFIC'   'GT_PERMIT'  '15'   ''  ''  ' '  'Data modificação'  ''  ' '  ' '  ' '  ''.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0412   text
*      -->P_0413   text
*      -->P_0414   text
*      -->P_0415   text
*      -->P_0416   text
*      -->P_0417   text
*      -->P_0418   text
*      -->P_0419   text
*      -->P_0420   text
*      -->P_0421   text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM z_feed_fieldcat  USING       VALUE(p_colnum)
                                  VALUE(p_fieldname)
                                  VALUE(p_tabname)
                                  VALUE(p_len)
                                  VALUE(p_edit)
                                  VALUE(p_icon)
                                  VALUE(p_do_sum)
                                  VALUE(p_header)
                                  VALUE(p_emphasize)
                                  VALUE(p_hotspot)
                                  VALUE(p_ref_table)
                                  VALUE(p_ref_field).


  wa_fcat-col_pos     = p_colnum.
  wa_fcat-fieldname   = p_fieldname.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-outputlen   = p_len.
  wa_fcat-edit        = p_edit.
  wa_fcat-icon        = p_icon.
  wa_fcat-do_sum      = p_do_sum.
  wa_fcat-coltext     = p_header.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-ref_table   = p_ref_table.
  wa_fcat-ref_table   = p_ref_field.

  wa_layout-excp_conds    = 'X'.
  wa_layout-zebra         = 'X'.
  wa_layout-sel_mode      = 'A'.
  wa_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  wa_layout-totals_bef    = ' '.

  APPEND wa_fcat TO it_fcat.
ENDFORM.

FORM z_feed_fieldcat_2  USING       VALUE(p_colnum)
                                  VALUE(p_fieldname)
                                  VALUE(p_tabname)
                                  VALUE(p_len)
                                  VALUE(p_edit)
                                  VALUE(p_icon)
                                  VALUE(p_do_sum)
                                  VALUE(p_header)
                                  VALUE(p_emphasize)
                                  VALUE(p_hotspot)
                                  VALUE(p_ref_table)
                                  VALUE(p_ref_field)
                                  VALUE(p_checkbox).


  wa_fcat_2-col_pos     = p_colnum.
  wa_fcat_2-fieldname   = p_fieldname.
  wa_fcat_2-tabname     = p_tabname.
  wa_fcat_2-outputlen   = p_len.
  wa_fcat_2-edit        = p_edit.
  wa_fcat_2-icon        = p_icon.
  wa_fcat_2-do_sum      = p_do_sum.
  wa_fcat_2-coltext     = p_header.
  wa_fcat_2-emphasize   = p_emphasize.
  wa_fcat_2-hotspot     = p_hotspot.
  wa_fcat_2-ref_table   = p_ref_table.
  wa_fcat_2-ref_field   = p_ref_field.
  wa_fcat_2-checkbox    = p_checkbox.

  IF p_fieldname EQ 'IWERK'.
    wa_fcat_2-f4availabl  = abap_true.
    wa_fcat_2-ref_field   = 'WERKS'.
    wa_fcat_2-ref_table   = 'T001W'.
  ENDIF.


  IF p_fieldname EQ 'TRANSACAO'.
    wa_fcat_2-drdn_hndl    = '3'.
  ENDIF.


  wa_layout_2-excp_conds    = 'X'.
  wa_layout_2-zebra         = 'X'.
  wa_layout_2-sel_mode      = 'A'.
  wa_layout_2-cwidth_opt    = ' '.     "  Otimizar colunas na tela
  wa_layout_2-totals_bef    = ' '.

  IF p_fieldname EQ 'PERNR' OR
    w_fieldcatalog-f4availabl = abap_true.
  ENDIF.

  APPEND wa_fcat_2 TO it_fcat_2.
  CLEAR wa_fcat_2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'TL003'.
  SET TITLEBAR 'ST_003'.

  IF wa_cont_2 IS BOUND AND wa_alv_2 IS BOUND.
    wa_alv_2->free( ).
    wa_cont_2->free( ).
  ENDIF.








  CLEAR: wa_cont_2,
         wa_alv_2.

  REFRESH it_fcat_2.

  CREATE OBJECT wa_cont_2
    EXPORTING
      container_name              = 'CONTAINER_2'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wl_function TO tl_function.
*  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    APPEND wl_function TO tl_function.

  PERFORM z_fieldcat_2.

  CREATE OBJECT wa_alv_2
    EXPORTING
      i_parent          = wa_cont_2
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  DATA(obj_events) = NEW events( ).
  SET HANDLER:
  obj_events->handle_user_command      FOR wa_alv_2,
  obj_events->on_toolbar              FOR wa_alv_2,
  obj_events->on_data_changed_finished FOR wa_alv_2,
  obj_events->on_data_changed          FOR wa_alv_2,
  obj_events->on_onf4                  FOR wa_alv_2.

  DATA: lt_dropdown    TYPE lvc_t_drop,
        ls_dropdown    TYPE lvc_s_drop,
        lt_dropdown_al TYPE lvc_t_dral,
        ls_dropdown_al TYPE lvc_s_dral.

  wa_layout_2-stylefname = 'CELLSTY'.


  FREE lt_dropdown.

  ls_dropdown-handle = '3'.
  ls_dropdown-value = 'APONTAMENTO'.
  APPEND ls_dropdown TO lt_dropdown.

  ls_dropdown-handle = '3'.
  ls_dropdown-value = 'APONT_AGRO'. "FF - 28.03.24
  APPEND ls_dropdown TO lt_dropdown.

  ls_dropdown-handle = '3'.
  ls_dropdown-value = 'ORDEM'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '3'.
  ls_dropdown-value = 'NOTA'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '3'.
  ls_dropdown-value = 'RESERVA'.
  APPEND ls_dropdown TO lt_dropdown.

  CALL METHOD wa_alv_2->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.

  APPEND INITIAL LINE TO lt_sort ASSIGNING FIELD-SYMBOL(<fs_sort>).

  <fs_sort>-fieldname = 'TRANSACAO'.
  <fs_sort>-up      = 'X'.

  CALL METHOD wa_alv_2->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding          = tl_function
      is_layout                     = wa_layout_2
    CHANGING
      it_outtab                     = gt_permit
      it_fieldcatalog               = it_fcat_2
      it_sort                       = lt_sort
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD wa_alv_2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD wa_alv_2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD wa_alv_2->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].


  IF sy-subrc NE 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.





  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: lt_permit TYPE TABLE OF zpmt0012.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVE'.
      DATA: ws_return TYPE zfiwrs0002,
            z_object  TYPE char08 VALUE 'I_IWERK',
            z_id      TYPE char08 VALUE 'IWERK',
            lv_lines  TYPE sy-tabix,
            lv_erro   TYPE c.

      CALL METHOD wa_alv_2->check_changed_data.
      FREE: it_msg_return.
      CLEAR: ws_return.
      IF gt_permit IS NOT INITIAL.

        DATA(lt_permit2) = gt_permit.

        LOOP AT gt_permit ASSIGNING FIELD-SYMBOL(<ws_permit>).

          lt_permit2 = gt_permit.
          DELETE lt_permit2 WHERE transacao <> <ws_permit>-transacao OR iwerk <> <ws_permit>-iwerk.
          DESCRIBE TABLE lt_permit2 LINES lv_lines.
          IF lv_lines > 1.
            APPEND  VALUE #( msg = 'Não é possível salvar registros duplicados!' ) TO it_msg_return.
            EXIT.
          ENDIF.
          IF <ws_permit>-iwerk IS INITIAL. APPEND  VALUE #( msg = |Preencha o centro | ) TO it_msg_return. ENDIF.

          "Check se usuario tem permissão de acesso ao centro.
          AUTHORITY-CHECK OBJECT z_object ID z_id FIELD <ws_permit>-iwerk. "Código do Centro.
          CASE sy-subrc.
            WHEN 0.

            WHEN 4.
              APPEND  VALUE #( msg = |Usuario não tem permissão para centro { <ws_permit>-iwerk }| ) TO it_msg_return.
            WHEN 12.
              APPEND  VALUE #( msg = |Objeto de autorização { z_object } não existe!| ) TO it_msg_return.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        IF it_msg_return IS NOT INITIAL.
          READ TABLE it_msg_return INTO ws_return INDEX 1.
          MESSAGE |{ ws_return-msg }| TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.



        MOVE-CORRESPONDING gt_permit TO lt_permit.
        MODIFY zpmt0012 FROM TABLE lt_permit.
        COMMIT WORK.

      ENDIF.

      CALL METHOD wa_alv_2->refresh_table_display
        EXPORTING
          is_stable = wa_stable_2.

      DATA: wa_usuarios TYPE ztpm_d_m_usuario,
            e_data      TYPE string,
            lv_string   TYPE string,
            lt_paramtab TYPE TABLE OF abap_func_parmbind_tab,
            lt_prop_tab TYPE js_property_tab,
            ls_object   TYPE REF TO cl_java_script,
            lv_length   TYPE sy-tabix.

      FIELD-SYMBOLS: <fs_cent_trab> TYPE any.

      READ TABLE gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>) INDEX 1.
      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'ZPM_EXPORT_DATA_TO_PM_MOBILE'
          EXPORTING
            view   = 'vw_d_usuario'
            pernr  = <fs_permit>-pernr
          IMPORTING
            result = lv_string.

        lv_length = strlen( lv_string ).

        lv_length = lv_length - 1.
        lv_string = lv_string+1(lv_length).

        /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = wa_usuarios ).

        REFRESH wa_usuarios-perfil.
        LOOP AT gt_permit ASSIGNING <fs_permit>.

          APPEND INITIAL LINE TO wa_usuarios-perfil ASSIGNING FIELD-SYMBOL(<fs_perfil>).
          <fs_perfil>-iwerk     = <fs_permit>-iwerk    .
          <fs_perfil>-transacao = <fs_permit>-transacao.
          <fs_perfil>-exibir    = <fs_permit>-exibir   .
          <fs_perfil>-modificar = <fs_permit>-modificar.
          <fs_perfil>-criar     = <fs_permit>-criar    .
          <fs_perfil>-liberar   = <fs_permit>-liberar  .
          <fs_perfil>-encerrar  = <fs_permit>-encerrar  .
          <fs_perfil>-aprovar_orc = <fs_permit>-aprovar_orc.

        ENDLOOP.
      ENDIF.

      IF gt_log IS NOT INITIAL.
        MODIFY ztpm_d_m_log FROM TABLE gt_log.
        REFRESH: gt_log.
      ENDIF.

      TRY.
          "Executa a API.
          CLEAR: e_data.
          zcl_permissao_usuarios=>zif_permissao_usuarios~get_instance(
                )->set_dados_usuario( i_data = wa_usuarios
                )->post_permissao_usuario( EXPORTING i_usuario = wa_usuarios ).


        CATCH zcx_integracao INTO DATA(ex_integra).    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        CATCH zcx_error INTO DATA(ex_error).    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      ENDTRY.

      FREE: gt_permit.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELEC_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_usuario.

  SELECT *
  FROM zpmt0012
  INTO CORRESPONDING FIELDS OF TABLE gt_permit.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SEL_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_sel_usuario .
  DATA: lv_msg TYPE bapi_msg,
        lt_log TYPE TABLE OF ztpm_d_m_log,
        lw_log TYPE ztpm_d_m_log.

  FIELD-SYMBOLS: <fs_value> TYPE any.

  CLEAR: it_select_rows[], wa_select_rows.

  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.

    LOOP AT it_select_rows INTO wa_select_rows.
      READ TABLE gw_usuario INTO DATA(wa_s_report) INDEX wa_select_rows-index.
      IF sy-subrc EQ 0.

        SELECT *
        FROM zpmt0012
        INTO CORRESPONDING FIELDS OF TABLE gt_permit WHERE pernr EQ wa_s_report-pernr.


        IF gt_permit IS INITIAL.
          "Seleciona transações no SET MAGI_PM_ZPM0061.
          SELECT *
          FROM setleaf
          INTO TABLE @DATA(t_data)
          WHERE setname EQ 'MAGI_PM_ZPM0061'.

          IF sy-subrc EQ 0.
            gt_permit = VALUE #( FOR l IN t_data ( pernr = wa_s_report-pernr transacao = l-valfrom ) ).

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE gt_permit WHERE transacao EQ space.

*    LOOP AT gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>).
*
*      <fs_permit>-modificado_por = sy-uname.
*      <fs_permit>-data_modific = sy-datum.
*
*      lw_log-usuario = <fs_permit>-pernr.
*      lw_log-transacao = <fs_permit>-transacao.
*      lw_log-criado_por = sy-uname.
*      lw_log-criado_em  = sy-datum.
*      lw_log-data_modific = sy-datum.
*      lw_log-hora_modif = sy-uzeit.
*      lw_log-modificado_por = sy-uname.
*
*      ASSIGN COMPONENT 'MODIFICAR' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'MODIFICAR'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*      ASSIGN COMPONENT 'EXIBIR' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'EXIBIR'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*      ASSIGN COMPONENT 'CRIAR' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'CRIAR'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*      ASSIGN COMPONENT 'LIBERAR' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'LIBERAR'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*      ASSIGN COMPONENT 'ENCERRAR' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'ENCERRAR'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*      ASSIGN COMPONENT 'IWERK' OF STRUCTURE <fs_permit> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*
*        lw_log-campo_modific = 'IWERK'.
*        lw_log-valor_novo = <fs_value>.
*
*        APPEND lw_log TO gt_log.
*
*      ENDIF.
*
*    ENDLOOP.

    CALL SCREEN 0300. "STARTING AT 8 8.
  ELSE.
    MESSAGE |Selecione uma linha.| TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'TL004'.
  SET TITLEBAR 'Log de modificação'.

  PERFORM f_processo_log_modif.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSO_LOG_MODIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processo_log_modif .

  DATA: lv_lines TYPE sy-tabix.

  CLEAR: it_select_rows[], wa_select_rows.

  CALL METHOD wa_alv_2->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.
    DESCRIBE TABLE it_select_rows[] LINES lv_lines.
    IF lv_lines > 1.
      MESSAGE 'Favor selecionar apenas uma linha!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      READ TABLE it_select_rows ASSIGNING FIELD-SYMBOL(<fs_rows>) INDEX 1.
      IF sy-subrc IS INITIAL.

        READ TABLE gt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>) INDEX <fs_rows>-index.
        IF sy-subrc IS INITIAL.
          SELECT *
            FROM ztpm_d_m_log
            INTO TABLE gt_log_alv
            WHERE usuario   = <fs_permit>-pernr
              AND transacao = <fs_permit>-transacao.
        ENDIF.
        IF wa_cont_3 IS INITIAL.
          CREATE OBJECT wa_cont_3
            EXPORTING
              container_name              = 'CC_LOG'
            EXCEPTIONS
              cntl_error                  = 1
              cntl_system_error           = 2
              create_error                = 3
              lifetime_error              = 4
              lifetime_dynpro_dynpro_link = 5
              OTHERS                      = 6.

          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_move_row.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_paste.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_undo.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_append_row.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_copy.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_cut.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_loc_cut.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_check.
          APPEND wl_function_3 TO tl_function_3.
          wl_function_3 = cl_gui_alv_grid=>mc_fc_refresh.
          APPEND wl_function_3 TO tl_function_3.

          PERFORM z_fieldcat_3.

          CREATE OBJECT wa_alv_3
            EXPORTING
              i_parent          = wa_cont_3
            EXCEPTIONS
              error_cntl_create = 1
              error_cntl_init   = 2
              error_cntl_link   = 3
              error_dp_create   = 4
              OTHERS            = 5.

          DATA(obj_events) = NEW events( ).
          SET HANDLER:
          obj_events->handle_user_command      FOR wa_alv_3,
*          obj_events->on_toolbar              FOR wa_alv_3,
          obj_events->on_data_changed_finished FOR wa_alv_3,
          obj_events->on_data_changed          FOR wa_alv_3,
          obj_events->on_onf4                  FOR wa_alv_3.


          CALL METHOD wa_alv_3->set_table_for_first_display
            EXPORTING
              it_toolbar_excluding          = tl_function_3
              is_layout                     = wa_layout_3
            CHANGING
              it_outtab                     = gt_log_alv
              it_fieldcatalog               = it_fcat_3
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.

          CALL METHOD wa_alv_3->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD wa_alv_3->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_enter.

          CALL METHOD wa_alv_3->register_f4_for_fields
            EXPORTING
              it_f4 = lt_f4_3[].


          IF sy-subrc NE 0 .
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ELSE.
          CALL METHOD wa_alv_3->refresh_table_display
            EXPORTING
              is_stable = wa_stable_3.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.

    MESSAGE 'Favor selecionar uma linha!' TYPE 'I' DISPLAY LIKE 'E'.
    SET SCREEN '0300'.
    LEAVE SCREEN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fieldcat_3 .


  PERFORM z_feed_fieldcat_3 USING:
       1  'USUARIO'         'GT_LOG'  '10'   ''  ''  ''  'Nº pessoal      '  ''  ' '  ' '  ' ',
       3  'TRANSACAO'       'GT_LOG'  '12'   ''  ''  ''  'Transação       '  ''  ' '  ' '  ' ',
       2  'CRIADO_POR'      'GT_LOG'  '12'   ' ' ''  ''  'Criado Por      '  ''  ' '  ' '  ' ',
       4  'CRIADO_EM'       'GT_LOG'  '10'   ''  ''  ''  'Criado Em       '  ''  ' '  ' '  ' ',
       5  'DATA_MODIFIC'    'GT_LOG'  '10'   ''  ''  ''  'Modificado EM   '  ''  ' '  ' '  ' ',
       6  'HORA_MODIF'      'GT_LOG'  '8'    ''  ''  ''  'Hora modificação'  ''  ' '  ' '  ' ',
       7  'MODIFICADO_POR'  'GT_LOG'  '12'   ''  ''  ''  'Modificado Por  '  ''  ' '  ' '  ' ',
       8  'CAMPO_MODIFIC'   'GT_LOG'  '30'   ''  ''  ''  'Cmpo modificado '  ''  ' '  ' '  ' ',
       9  'VALOR_ANTIGO'    'GT_LOG'  '10'   ''  ''  ''  'Valor antigo    '  ''  ' '  ' '  ' ',
       10  'VALOR_NOVO'     'GT_LOG'  '10'   ''  ''  ''  'Valor Novo      '  ''  ' '  ' '  ' '.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0412   text
*      -->P_0413   text
*      -->P_0414   text
*      -->P_0415   text
*      -->P_0416   text
*      -->P_0417   text
*      -->P_0418   text
*      -->P_0419   text
*      -->P_0420   text
*      -->P_0421   text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM z_feed_fieldcat_3  USING     VALUE(p_colnum)
                                  VALUE(p_fieldname)
                                  VALUE(p_tabname)
                                  VALUE(p_len)
                                  VALUE(p_edit)
                                  VALUE(p_icon)
                                  VALUE(p_do_sum)
                                  VALUE(p_header)
                                  VALUE(p_emphasize)
                                  VALUE(p_hotspot)
                                  VALUE(p_ref_table)
                                  VALUE(p_ref_field).


  wa_fcat_3-col_pos     = p_colnum.
  wa_fcat_3-fieldname   = p_fieldname.
  wa_fcat_3-tabname     = p_tabname.
  wa_fcat_3-outputlen   = p_len.
  wa_fcat_3-edit        = p_edit.
  wa_fcat_3-icon        = p_icon.
  wa_fcat_3-do_sum      = p_do_sum.
  wa_fcat_3-coltext     = p_header.
  wa_fcat_3-emphasize   = p_emphasize.
  wa_fcat_3-hotspot     = p_hotspot.
  wa_fcat_3-ref_table   = p_ref_table.
  wa_fcat_3-ref_table   = p_ref_field.

  wa_layout_3-excp_conds    = 'X'.
  wa_layout_3-zebra         = 'X'.
  wa_layout_3-sel_mode      = 'A'.
  wa_layout_3-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  wa_layout_3-totals_bef    = ' '.

  APPEND wa_fcat_3 TO it_fcat_3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME text
*----------------------------------------------------------------------*
**********************************************************************
* seleciona arquivo
**********************************************************************
FORM f_select_file USING p_filename TYPE localfile.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable,
        lv_resposta TYPE char01.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO p_filename INDEX 1.

ENDFORM.

FORM f_carrega_arquivo USING p_filename TYPE localfile.

  DATA: l_erro     TYPE char1,
        l_cols     TYPE i,
        lt_tab     TYPE TABLE OF alsmex_tabline,
        w_tab      TYPE alsmex_tabline,
        lt_file    TYPE TABLE OF zpme0071,
        wa_file    TYPE zpme0071,
        lt_permit2 TYPE TABLE OF zpmt0012.

  FIELD-SYMBOLS: <fs_fld> TYPE any.

  CHECK p_filename IS NOT INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = TEXT-001.

*----------------------------------------
* upload excel
*----------------------------------------
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_filename
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 12
      i_end_row               = 30000
*     i_end_col               = 256
*     i_end_row               = 65536
    TABLES
      intern                  = lt_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE TEXT-002 && p_filename TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK l_erro IS INITIAL.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  FREE: l_erro, l_cols.

  DATA(lt_permit) = gt_permit.
  SORT lt_permit BY iwerk.

  DELETE lt_tab WHERE row = 0001.
  LOOP AT lt_tab INTO w_tab.

    l_cols = l_cols + 1.
    ASSIGN COMPONENT w_tab-col OF STRUCTURE wa_file TO <fs_fld>.

    IF w_tab-col EQ 0003.
      READ TABLE lt_permit TRANSPORTING NO FIELDS
      WITH KEY iwerk = w_tab-value
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MESSAGE TEXT-005 && w_tab-value && TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
        l_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF l_erro IS NOT INITIAL.
      EXIT.
    ENDIF.

    <fs_fld> = w_tab-value.
    AT END OF row.

      IF l_cols < 3 AND  w_tab-row = 0002.
        MESSAGE TEXT-003 && TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      APPEND wa_file TO lt_file.
      CLEAR wa_file.
      FREE l_cols.
    ENDAT.
  ENDLOOP.

  IF lt_file IS NOT INITIAL.

    LOOP AT lt_file ASSIGNING FIELD-SYMBOL(<fs_file>).
      APPEND INITIAL LINE TO lt_permit2 ASSIGNING FIELD-SYMBOL(<fs_permit>).

      MOVE-CORRESPONDING <fs_file> TO <fs_permit>.
      <fs_permit>-modificado_por = sy-uname.
      <fs_permit>-data_modific   = sy-datum.

    ENDLOOP.

    IF lt_permit2 IS NOT INITIAL.
      MODIFY zpmt0012 FROM TABLE lt_permit2.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

        MESSAGE 'Dados inseridos com sucesso!' TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_MODELO_PLANILHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gera_modelo_planilha .

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata      TYPE REF TO data,
        v_nome_arquivo TYPE char50,
        t_file         TYPE TABLE OF zpme0071.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name(50) TYPE c,
    END OF t_fieldnames.

  CONCATENATE 'PlanilhaCargaModelo_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '_' sy-uzeit INTO v_nome_arquivo.


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = v_nome_arquivo
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    APPEND INITIAL LINE TO t_file ASSIGNING FIELD-SYMBOL(<fs_file>).
    <fs_file>-pernr = '1111111111'.
    <fs_file>-transacao = 'ORDEM'.
    <fs_file>-iwerk     = '2602'.
    <fs_file>-criar     = 'X'.
    <fs_file>-encerrar  = 'X'.
    <fs_file>-exibir    = 'X'.
    <fs_file>-liberar   = 'X'.
    <fs_file>-modificar = 'X'.
    <fs_file>-aprovar_orc = 'X'.

    CONCATENATE path '.xls' INTO p_local.

    t_fieldnames-name    = 'Numero Pessoal'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Transacao'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Exibir'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Modificar'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Criar'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Liberar'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Encerrar'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Aprovar Orcamento.'.
    APPEND t_fieldnames.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = p_local
        filetype              = 'ASC'
        write_field_separator = 'X'
        "CODEPAGE            = '8404'
      TABLES
        data_tab              = t_file
        fieldnames            = t_fieldnames
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        invalid_filesize      = 3
        invalid_table_width   = 4
        invalid_type          = 5
        no_batch              = 6
        unknown_error         = 7
        OTHERS                = 8.

    IF sy-subrc = 0.
      MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
    ELSE.
      MESSAGE 'Arquivo processado com erro' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
