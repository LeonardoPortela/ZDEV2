*&----------------------------------------------------------------------------*
*&  Include           ZSDR0061_5000
*&----------------------------------------------------------------------------*
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN  |DEVK9A2HNK |01/04/2025 |Ajuste diversos no cadastro de Roteiro.&*
*&                                   |Chamado: 171563.                       &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN  |DEVK9A2RF1 |26/08/2025 |Ajst. na atribuição da Zona ao Roteiro.&*
*&                                   |Chamado: 187296.                       &*
*&---------------------------------------------------------------------------&*
DATA: it_filial        TYPE STANDARD TABLE OF ty_zsdt0132_alv.


DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      ctl_alv1           TYPE REF TO cl_gui_alv_grid,
      gs_layout          TYPE lvc_s_layo,
      it_fieldcatalog    TYPE lvc_t_fcat,
      t_new_line         TYPE REF TO data,
      it_sort            TYPE lvc_t_sort,
      it_exclude         TYPE ui_functions,
      lt_f4              TYPE lvc_t_f4 WITH HEADER LINE.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
               <fs_line>  TYPE any.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      "FF - inicio #143815
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,
      "FF - fim

      toolbar FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.
    "Separador
    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "Botão Nova Linha
*    wa_tool-function = 'NEW'.
*    wa_tool-icon     = '@17@'.
*    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "Botão Deleta Nova Linha
    wa_tool-function = 'DEL'.
    wa_tool-icon     = '@18@'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "Botão Edita Linha Antiga
    wa_tool-function = 'EDIT'.
    wa_tool-icon     = '@0Z@'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
**<<<------"171563 - NMS - INI------>>>
* Acionar os botões Nova Linha ou Edita Linha Antiga não implememntar o
* botão Criar Zona.
*    READ TABLE it_zsdt0132 TRANSPORTING NO FIELDS WITH KEY antig = space.
*    CHECK NOT sy-subrc IS INITIAL AND
*          NOT p_c      IS INITIAL.    "Cliente.
**<<<------"171563 - NMS - FIM------>>>
    "FF inicio #143815
    "Separador
    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "Botão Nova Linha
    wa_tool-function = 'ZONA'.
    wa_tool-icon     = '@AE@'.
    wa_tool-quickinfo = 'Criar Zona de Transporte'.
    wa_tool-text = 'Criar Zona'.
    APPEND wa_tool TO e_object->mt_toolbar.


    wa_tool-function = 'DEFI'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Definir Zona Padrão'.
    wa_tool-text = 'Definir Zona Padrão'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: tl_good_cells     TYPE lvc_t_modi,
          lv_value(14)      TYPE c,
          lv_value_form(14) TYPE c,
          ajuste_tel(14)    TYPE c,
          contador          TYPE i,
          ls_good           TYPE lvc_s_modi.

    DATA: obg_event TYPE REF TO lcl_event_handler.
    CREATE OBJECT obg_event.

    REFRESH: tl_good_cells.

    FIELD-SYMBOLS: <mr> TYPE tty_itens,
                   <tr> TYPE ty_zsdt0132_alv.
    ASSIGN er_data_changed->mp_mod_rows->* TO <mr>.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'TEL_NUMBER' .

      lv_value_form = ls_good-value.

      TRANSLATE ls_good-value USING '( '.
      TRANSLATE ls_good-value USING ') '.
      TRANSLATE ls_good-value USING '- '.
      CONDENSE ls_good-value NO-GAPS.

      CONDENSE lv_value.
      contador = strlen( ls_good-value ).
      IF contador LE 9 OR contador GE 12.
        MESSAGE 'Telefone Inválido' TYPE 'S' DISPLAY LIKE 'W'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TEL_NUMBER'
            i_value     = ' '.

      ELSE.
        IF contador EQ 10.
          CONCATENATE '(' ls_good-value(2) ')' ' ' ls_good-value+2(4)'-' ls_good-value+6(4)  INTO lv_value.
        ELSE.
          CONCATENATE '(' ls_good-value(2) ')' ' ' ls_good-value+2(5)'-' ls_good-value+7(4)  INTO lv_value.
        ENDIF.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TEL_NUMBER'
            i_value     = lv_value.

      ENDIF.
    ENDLOOP.

    "FF inicio #143815
    DATA lv_zone TYPE kna1-lzone.
**<<<------"171563 - NMS - INI------>>>
* Verifica o tipo do parceiro de negócio.
    CASE abap_on.
      WHEN p_c. "Cliente
        SELECT SINGLE land1 INTO @DATA(vl_land1) FROM kna1 WHERE kunnr EQ @p_kunnr.

      WHEN p_f. "Fornecedor
        SELECT SINGLE land1 INTO vl_land1 FROM lfa1 WHERE lifnr EQ p_lifnr.

      WHEN OTHERS.
* Do nothing
    ENDCASE.
**<<<------"171563 - NMS - FIM----->>>
    SELECT zone1
      FROM tzone INTO TABLE @DATA(lt_zone)
**<<<------"171563 - NMS - INI------>>>
*      WHERE land1 = 'BR'.
      WHERE land1 = @vl_land1.
**<<<------"171563 - NMS - FIM----->>>
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'LZONE'.
      lv_zone = ls_good-value.

      READ TABLE lt_zone WITH KEY zone1 = lv_zone TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        MESSAGE 'Zona Transporte inválida!' TYPE 'S' DISPLAY LIKE 'E'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'LZONE'
            i_value     = ' '.
**<<<------"171563 - NMS - INI------>>>
      ELSE.
        READ TABLE it_zsdt0132 INTO DATA(e_zsdt0132) INDEX ls_good-row_id.
        e_zsdt0132-lzone = lv_zone.
        LOOP AT e_zsdt0132-cellstyles ASSIGNING FIELD-SYMBOL(<fs_cellstyles>) WHERE fieldname EQ 'ZLATITUDE'
                                                                                 OR fieldname EQ 'ZLONGITUDE'
                                                                                 OR fieldname EQ 'Z_URL_LOCALIZACAO'.
          <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_enabled.
          DATA(vl_refresh) = 1.

        ENDLOOP.

        MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX ls_good-row_id.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.

    ENDLOOP.
    "FF fim
**<<<------"171563 - NMS - INI------>>>
    LOOP AT er_data_changed->mt_mod_cells INTO DATA(el_mod_cells) WHERE fieldname EQ 'LZONE'
                                                                    AND error     EQ abap_on.
      lv_zone = el_mod_cells-value.

      READ TABLE lt_zone WITH KEY zone1 = lv_zone TRANSPORTING NO FIELDS.

      IF NOT sy-subrc IS INITIAL.
        READ TABLE it_zsdt0132 INTO e_zsdt0132 INDEX el_mod_cells-row_id.
        e_zsdt0132-lzone = lv_zone.
        LOOP AT e_zsdt0132-cellstyles ASSIGNING <fs_cellstyles> WHERE fieldname EQ 'ZLATITUDE'
                                                                   OR fieldname EQ 'ZLONGITUDE'
                                                                   OR fieldname EQ 'Z_URL_LOCALIZACAO'.

          <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_disabled.
          vl_refresh = 2.

        ENDLOOP.

        MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX el_mod_cells-row_id.

      ENDIF.

    ENDLOOP.

    IF NOT vl_refresh IS INITIAL.
      CALL METHOD ctl_alv1->refresh_table_display.
      CHECK vl_refresh EQ 2.
      MESSAGE 'Zona Transporte inválida!' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.
**<<<------"171563 - NMS - FIM------>>>
  ENDMETHOD.

  METHOD user_command.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row,
          wa_zsdt0132      TYPE ty_zsdt0132_alv.

    DATA: el_cellstyles TYPE lvc_s_styl. "<<<------"187296 - NMS ------->>>

    DATA: vl_resp TYPE c. "<<<------"171563 - NMS------>>>

    IF e_ucomm = 'DEL'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF it_selected_rows IS INITIAL.
        MESSAGE 'Selecione ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      LOOP AT it_selected_rows INTO wa_selected_rows.


        READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.

        DATA(lv_txt1) = 'Deseja excluir o registro selecionado?'.
        DATA lv_answer TYPE c.

        CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
          EXPORTING
            diagnosetext1 = lv_txt1
*           diagnosetext2 = vl_text2
            textline1     = space
            titel         = TEXT-015
            start_column  = 25
            start_row     = 6
*           CANCEL_DISPLAY       = 'X'
          IMPORTING
            answer        = lv_answer.

        IF lv_answer = 'J'.

          IF sy-subrc IS INITIAL.

            SELECT SINGLE route
              FROM trolz
              INTO @DATA(lv_route_orig)
              WHERE azone = @wa_zsdt0132-lzone.

*          IF sy-subrc EQ 0.

            SELECT SINGLE route
              FROM trolz
              INTO @DATA(lv_route_dest)
              WHERE lzone = @wa_zsdt0132-lzone.

            IF lv_route_dest IS NOT INITIAL AND
               lv_route_orig IS NOT INITIAL.

              MESSAGE |Zona já associada ao itinerário: { lv_route_dest }| TYPE 'S' DISPLAY LIKE 'E'..

            ELSE.

              MOVE-CORRESPONDING wa_zsdt0132 TO wa_zlest0153.

              MOVE wa_zsdt0132-aland TO wa_zlest0153-land1.

              SELECT SINGLE * FROM zlest0153
                INTO @DATA(ls_153_del)
                WHERE land1 = @wa_zlest0153-land1 AND
                      lzone = @wa_zlest0153-lzone AND
                      lifnr = @wa_zlest0153-lifnr AND
                      kunnr = @wa_zlest0153-kunnr.

              IF sy-subrc EQ 0.
                DELETE zlest0153 FROM ls_153_del.
              ENDIF.

              "enqueue Tzone
              DATA(lt_msg_enq) = NEW zcl_zona_transporte( )->enqueue_tzone( i_exec = abap_on ).

              IF line_exists( lt_msg_enq[ type = 'E' ] ).
                " Obtém a primeira mensagem de erro encontrada
                DATA(ls_msg_enq) = lt_msg_enq[ type = 'E' ].
                IF sy-subrc = 0.
                  MESSAGE |Erro ao excluir zona: { ls_msg_enq-msg1 } { ls_msg_enq-msg2 }| TYPE 'S' DISPLAY LIKE 'E'.
                ENDIF.

              ELSE.

                DATA ls_tzone TYPE tzone.

                MOVE-CORRESPONDING wa_zsdt0132 TO ls_tzone.

                MOVE: wa_zsdt0132-aland TO ls_tzone-land1,
                      wa_zsdt0132-lzone TO ls_tzone-zone1.

                SELECT SINGLE *
                  FROM tzone
                  INTO @DATA(ls_tzone_del)
                  WHERE land1 = @ls_tzone-land1 AND
                        zone1 = @ls_tzone-zone1.
                IF sy-subrc EQ 0.

                  DELETE tzone FROM ls_tzone_del.

                  FREE: lt_msg_enq.


                  SELECT SINGLE *
                    FROM tzont
                    INTO @DATA(ls_tzont_del)
                    WHERE land1 = @ls_tzone-land1 AND
                          zone1 = @ls_tzone-zone1 AND
                          spras = @sy-langu.
                  IF sy-subrc EQ 0.
                    DELETE tzont FROM ls_tzont_del.
                  ENDIF.

                  "dequeue
                  lt_msg_enq = NEW zcl_zona_transporte( )->enqueue_tzone( i_exec = abap_off ).
                  MESSAGE |Registro excluído com sucesso| TYPE 'S'.

                  DELETE it_zsdt0132 INDEX wa_selected_rows-index.

                  CLEAR: wa_zsdt0132, wa_zlest0153.

                  CALL METHOD ctl_alv1->refresh_table_display.

                ENDIF.

              ENDIF.

            ENDIF.

*          ENDIF.

          ENDIF.

        ELSE.

        ENDIF.

      ENDLOOP.

    ELSEIF e_ucomm = 'EDIT'.
**<<<------"187296 - NMS - INI------>>>
*      CALL METHOD ctl_alv1->get_frontend_fieldcatalog
*        IMPORTING
*          et_fieldcatalog = it_fieldcatalog.
*
*      LOOP AT it_fieldcatalog INTO DATA(wa_fieldcat).
*
*        IF wa_fieldcat-fieldname EQ 'VTEXT' OR
*           wa_fieldcat-fieldname EQ 'ZLATITUDE' OR
*           wa_fieldcat-fieldname EQ 'ZLONGITUDE' OR
*           wa_fieldcat-fieldname EQ 'Z_URL_LOCALIZACAO'.
*
*
*          wa_fieldcat-edit = 'X'.
*
*        ENDIF.
*
*        MODIFY it_fieldcatalog FROM wa_fieldcat.
*
*      ENDLOOP.
*
*      CALL METHOD ctl_alv1->set_frontend_fieldcatalog
*        EXPORTING
*          it_fieldcatalog = it_fieldcatalog.
*
      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF it_selected_rows[] IS INITIAL.
        MESSAGE 'Selecione ao menos uma linha para alterar a Zona de transporte' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

      ELSE.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.

      ENDIF.

      READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.

      LOOP AT wa_zsdt0132-cellstyles INTO el_cellstyles WHERE fieldname EQ 'ZLATITUDE'
                                                           OR fieldname EQ 'ZLONGITUDE'
                                                           OR fieldname EQ 'Z_URL_LOCALIZACAO'.

        el_cellstyles-style = cl_gui_alv_grid=>mc_style_enabled.
        MODIFY wa_zsdt0132-cellstyles FROM el_cellstyles INDEX sy-tabix.

      ENDLOOP.

      MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index.

      CALL METHOD ctl_alv1->refresh_table_display.
**<<<------"187296 - NMS - FIM------>>>
    ELSEIF e_ucomm = 'ZONA'.

      DATA lv_numrange(9) TYPE n.
      DATA ls_153 TYPE zlest0153.
      DATA lv_erro.
      DATA lv_lzone TYPE lzone.

      TYPES:
        BEGIN OF ty_zona,
          pais              TYPE land1,
          cod_zona          TYPE char10,
**<<<------"171563 - NMS - INI------>>>
          zlatitude         TYPE zde_latitude,
          zlongitude        TYPE zde_longitude,
          z_url_localizacao TYPE zde_url_loc,
**<<<------"171563 - NMS - FIM------>>>
          desc              TYPE char20,
        END OF ty_zona.

      DATA ls_zona TYPE ty_zona.

      CLEAR lv_erro.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSD_ZONA'
        IMPORTING
          number                  = lv_numrange
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      CASE sy-subrc .
        WHEN 0.
          "Sucesso
        WHEN 1.
**<<<------"171563 - NMS - INI------>>>
*              MESSAGE |Intervalo de numeração não encontrado| TYPE 'E' DISPLAY LIKE 'E'.
          MESSAGE |Intervalo de numeração não encontrado| TYPE 'S' DISPLAY LIKE 'E'.
          MOVE abap_true TO wa_zsdt0132-antig.
*              MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index TRANSPORTING antig.
          CLEAR: wa_zsdt0132.
          CALL METHOD ctl_alv1->refresh_table_display.
          RETURN.
**<<<------"171563 - NMS - FIM------>>>
        WHEN OTHERS.
**<<<------"171563 - NMS - INI------>>>
*              MESSAGE |Erro ao gerar código da Zona| TYPE 'E' DISPLAY LIKE 'E'.
          MESSAGE |Erro ao gerar código da Zona| TYPE 'S' DISPLAY LIKE 'E'.
*              MOVE abap_true TO wa_zsdt0132-antig.
*              MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index TRANSPORTING antig.
          CLEAR: wa_zsdt0132.
          CALL METHOD ctl_alv1->refresh_table_display.
          RETURN.
**<<<------"171563 - NMS - FIM------>>>
      ENDCASE.

      ls_zona-cod_zona = wa_zsdt0132-lzone = |{ 'Z' }{ lv_numrange }|.
*          ls_zona-desc = |{ 'Roteiro' }{ wa_zsdt0132-nr_rot }|.
**<<<------"171563 - NMS - INI------>>>
* Popup dos valores de Latitude, Longitude e URL de Localização.
      PERFORM zf_popup_get_value_zone USING wa_zsdt0132-lzone
                                            wa_zsdt0132-vtext
                                            wa_zsdt0132-zlatitude
                                            wa_zsdt0132-zlongitude
                                            wa_zsdt0132-z_url_localizacao
                                            e_ucomm
                                            vl_resp.

      IF vl_resp EQ sy-abcde(1). "A - Abort (Cancelado pelo usuário)
        RETURN.

      ENDIF.


      ls_zona-zlatitude         = wa_zsdt0132-zlatitude.


*      IF ( ls_zona-zlatitude  IS NOT INITIAL AND
*           ls_zona-zlongitude IS INITIAL ) OR
*         ( ls_zona-zlongitude IS NOT INITIAL AND
*           ls_zona-zlatitude IS INITIAL ).
*
*        MESSAGE 'Campos latitute e longitude devem estar preenchidos ou zerados!' TYPE 'S' DISPLAY LIKE 'E'.
*        lv_erro = abap_on.
*
*      ENDIF.
*
*      IF lv_erro IS NOT INITIAL.
*        RETURN.
*      ENDIF.
*
*      CLEAR vg_erro.
*
*      PERFORM valida_coordenadas USING wa_zsdt0132-zlatitude
*                                                     2
*                                                     0.
*
*      IF vg_erro IS NOT INITIAL.
*        RETURN.
*      ENDIF.
*
      ls_zona-zlongitude   = wa_zsdt0132-zlongitude.    "<<<------"187296 - NMS ------->>>
*
*
*      PERFORM valida_coordenadas USING ls_zona-zlongitude
*                                                     3
*                                                     0.
*
*      IF vg_erro IS NOT INITIAL.
*        RETURN.
*      ENDIF.

      ls_zona-z_url_localizacao = wa_zsdt0132-z_url_localizacao.

      IF p_c IS NOT INITIAL.
        SELECT SINGLE land1, lzone
           FROM kna1
           INTO ( @ls_zona-pais, @lv_lzone )
           WHERE kunnr = @wa_header-kunnr.
        IF sy-subrc <> 0.
          CLEAR ls_zona-pais.
        ENDIF.

      ELSE.

        SELECT SINGLE land1, lzone
           FROM lfa1
           INTO ( @ls_zona-pais, @lv_lzone )
           WHERE lifnr = @wa_header-kunnr.
        IF sy-subrc <> 0.
          CLEAR ls_zona-pais.
        ENDIF.

      ENDIF.
      lv_lzone = wa_header-bpzon.   "<<<------"187296 - NMS ------>>>
      DATA(lt_msg) = NEW zcl_zona_transporte( )->criar_zona(
        i_pais     = ls_zona-pais
        i_cod_zona = ls_zona-cod_zona
**<<<------"171563 - NMS - INI------>>>
        i_zlatitude         = ls_zona-zlatitude
        i_zlongitude        = ls_zona-zlongitude
        i_z_url_localizacao = ls_zona-z_url_localizacao
**<<<------"171563 - NMS - FIM------>>>
        i_desc     = wa_zsdt0132-vtext
      ).

      IF line_exists( lt_msg[ type = 'E' ] ).
        " Obtém a primeira mensagem de erro encontrada
        READ TABLE lt_msg INTO DATA(ls_msg) WITH KEY type = 'E'.
        IF sy-subrc = 0.
**<<<------"171563 - NMS - INI------>>>
*              MESSAGE |Erro ao criar zona: { ls_msg-msg1 } { ls_msg-msg2 }| TYPE 'E' DISPLAY LIKE 'E'.
          MESSAGE |Erro ao criar zona: { ls_msg-msg1 } { ls_msg-msg2 }| TYPE 'S' DISPLAY LIKE 'E'.
          lv_erro = abap_on.
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.
**<<<------"171563 - NMS - INI------>>>
      ELSEIF lv_erro IS INITIAL.


        IF p_c IS NOT INITIAL.
          wa_zsdt0132-kunnr = p_kunnr.
        ELSE.
          wa_zsdt0132-lifnr = p_lifnr.
        ENDIF.

        wa_zsdt0132-aland = ls_zona-pais.
        wa_zsdt0132-usnam = sy-uname.
        wa_zsdt0132-data_atual = sy-datum.
        wa_zsdt0132-hora_atual = sy-uzeit.


        INSERT wa_zsdt0132 INTO it_zsdt0132  INDEX 1.
        MESSAGE |Registro inserido com sucesso| TYPE 'S'.
        CLEAR: wa_zsdt0132.

      ENDIF.


      IF lv_erro IS INITIAL.

        TRY.
            " Preenchendo os campos da estrutura com os valores desejados
            ls_153-land1       = ls_zona-pais.
            ls_153-lzone       = ls_zona-cod_zona.

            IF p_c IS NOT INITIAL.
              ls_153-kunnr       = wa_header-kunnr.
              DATA(vl_id_part)   = wa_header-kunnr.    "<<<------"187296 - NMS ------->>>
            ELSE.
              ls_153-lifnr       = wa_header-lifnr.    "<<<------"187296 - NMS ------->>>
              vl_id_part         = wa_header-lifnr.    "<<<------"187296 - NMS ------->>>
            ENDIF.

*            ls_153-nr_rot      = lv_nr_rot.                 "US 170612
            ls_153-us_registro = sy-uname.
            ls_153-dt_registro = sy-datum.
            ls_153-hr_registro = sy-uzeit.

            " Realizando o MODIFY na tabela transparente
            MODIFY zlest0153 FROM ls_153.

          CATCH cx_root INTO DATA(lx_root).
            " Tratamento de exceção genérica
            MESSAGE |Ocorreu uma exceção: { lx_root->get_text( ) }| TYPE 'S' DISPLAY LIKE 'E'.
            lv_erro = abap_on.

        ENDTRY.

      ENDIF.

      CLEAR: wa_zsdt0132.
**<<<------"187296 - NMS - INI------>>>
*      IF lv_erro IS INITIAL.
      IF lv_erro         IS INITIAL AND
         wa_header-bpzon IS INITIAL.
**<<<------"187296 - NMS - FIM------>>>
        PERFORM f_popup_zona_transp USING ls_zona-cod_zona
*                                          wa_header-kunnr.   "<<<------"187296 - NMS ------->>>
                                          vl_id_part          "<<<------"187296 - NMS ------->>>
*                                          space              "<<<------"187296 - NMS ------->>>
                                          e_ucomm.

      ENDIF.

      CALL METHOD ctl_alv1->get_frontend_layout
        IMPORTING
          es_layout = gs_layout.

      gs_layout-cwidth_opt = abap_true.

      CALL METHOD ctl_alv1->set_frontend_layout
        EXPORTING
          is_layout = gs_layout.

      CALL METHOD ctl_alv1->refresh_table_display.

      SET SCREEN 0.
      LEAVE TO SCREEN 5000.


    ELSEIF e_ucomm = 'DEFI'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.


      IF it_selected_rows IS INITIAL.
        MESSAGE 'Selecione ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
**<<<------"187296 - NMS - INI------>>>
      IF NOT wa_header-bpzon IS INITIAL.
        MESSAGE |A Zona padrão já está definida. Ação não permitida.| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.

      CASE abap_on.
        WHEN p_c. "Cliente
          vl_id_part = wa_header-kunnr.

        WHEN p_f. "Fornecedor
          vl_id_part = wa_header-lifnr.

        WHEN OTHERS.
*           Do nothing
      ENDCASE.
**<<<------"187296 - NMS - FIM------>>>
      LOOP AT it_selected_rows INTO wa_selected_rows.

        READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.

        IF sy-subrc IS INITIAL.

          PERFORM f_popup_zona_transp USING wa_zsdt0132-lzone
                                            vl_id_part           "<<<------"187296 - NMS ------->>>
*                                            space               "<<<------"187296 - NMS ------->>>
                                            e_ucomm.
          DATA(vl_refhesh) = abap_on.

        ENDIF.

      ENDLOOP.

    ENDIF.
    "FF fim

    DATA: wl TYPE lvc_s_stbl.

    CALL METHOD ctl_alv1->get_frontend_layout
      IMPORTING
        es_layout = gs_layout.

    gs_layout-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1->set_frontend_layout
      EXPORTING
        is_layout = gs_layout.


    CALL METHOD ctl_alv1->refresh_table_display.
**<<<------"187296 - NMS - INI------>>>
    CHECK NOT vl_refhesh IS INITIAL.
    SET SCREEN 0.
    LEAVE TO SCREEN 5000.
**<<<------"187296 - NMS - FIM------>>>
  ENDMETHOD.

  METHOD handle_button_click .

    DATA: wa_zsdt0132 TYPE ty_zsdt0132_alv.

    DATA: vl_obj_key    TYPE sibflporb-instid,
          vl_lines      TYPE i,
          anexos        TYPE TABLE OF bdn_con,
          vl_ip_mode    TYPE sgs_rwmod,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          anexo_obj     TYPE REF TO cl_gos_manager.

    DATA: vl_display_mode TYPE xfeld,
          wl_header       TYPE thead,
          wl_name         TYPE thead-tdname,
          it_texto        TYPE STANDARD TABLE OF tline,
          wa_texto        TYPE tline,
          tl_texto        TYPE catsxt_longtext_itab,
          wl_texto        TYPE LINE OF catsxt_longtext_itab.

    READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX es_row_no-row_id.

*    CASE es_col_id.
*      WHEN 'ANEXO'.
*
*        CREATE OBJECT anexo_obj TYPE cl_gos_manager.
*
*        IF wa_zsdt0132-antig EQ abap_true.
*          vl_ip_mode = 'R'.
*        ELSE.
*          vl_ip_mode = 'E'.
*        ENDIF.
*
*        IF wa_zsdt0132-anexo EQ '@1F@'.
*          vl_ip_service = 'PCATTA_CREA'.
*        ELSE.
*          vl_ip_service = 'VIEW_ATTA'.
*        ENDIF.
*
**        wa_bor-objkey = wa_zsdt0132-nr_rot.
**        wa_bor-objtype = 'ZSDR0061'.
*
*        anexo_obj->set_rw_mode( ip_mode = vl_ip_mode ).
*
*        anexo_obj->start_service_direct(
*          EXPORTING
*            ip_service         = vl_ip_service
*            is_object          = wa_bor
*          EXCEPTIONS
*            no_object          = 1
*            object_invalid     = 2
*            execution_failed   = 3
*            OTHERS             = 4 ).
*
*        COMMIT WORK.
*
*        "Contando o número de anexos p/ atualizar ícone
*        vl_obj_key = wa_zsdt0132-nr_rot.
*
*        CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*          EXPORTING
*            classname          = 'ZSDR0061'
*            objkey             = vl_obj_key
*            client             = sy-mandt
*          TABLES
*            gos_connections    = anexos
*          EXCEPTIONS
*            no_objects_found   = 1
*            internal_error     = 2
*            internal_gos_error = 3
*            OTHERS             = 4.
*
*        DESCRIBE TABLE anexos LINES vl_lines.
*
*        IF vl_lines NE 0.
*          wa_zsdt0132-anexo = '@1E@'.
*        ELSE.
*          wa_zsdt0132-anexo = '@1F@'.
*        ENDIF.
*
*        MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING anexo .
*        CLEAR: wa_bor, vl_obj_key, vl_lines, anexos.
*
*      WHEN 'TEXTO'.
*
*        IF wa_zsdt0132-antig EQ abap_true.
*          MOVE abap_true TO vl_display_mode.
*        ELSE.
*          MOVE space TO vl_display_mode.
*        ENDIF.
*
*        REFRESH: it_texto, tl_texto.
*        CLEAR: wl_texto.
*
*        wl_name = wa_zsdt0132-nr_rot.
*
*        CALL FUNCTION 'READ_TEXT'
*          EXPORTING
*            id        = 'ZROT'
*            language  = sy-langu
*            name      = wl_name
*            object    = 'ZSDROTEIRO'
*          TABLES
*            lines     = it_texto
*          EXCEPTIONS
*            id        = 1
*            language  = 2
*            name      = 3
*            not_found = 4
*            OTHERS    = 5.
*
*        IF sy-subrc IS INITIAL.
*          LOOP AT it_texto INTO wa_texto.
*            MOVE: wa_texto-tdline TO wl_texto.
*            APPEND wl_texto TO tl_texto.
*            CLEAR: wl_texto.
*          ENDLOOP.
*        ENDIF.
*
*        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
*          EXPORTING
*            im_title        = 'Roteiro'
*            im_display_mode = vl_display_mode
*          CHANGING
*            ch_text         = tl_texto.
*
*        IF sy-ucomm EQ 'CX_CONT' AND wa_zsdt0132-antig NE abap_true.  "Salvar
*
*          IF tl_texto[] IS NOT INITIAL.
*
*            MOVE '@1E@' TO wa_zsdt0132-texto.
*            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING texto.
*            CLEAR: it_texto.
*
*            LOOP AT tl_texto INTO wl_texto.
*              MOVE: '*'      TO wa_texto-tdformat,
*                    wl_texto TO wa_texto-tdline.
*              APPEND wa_texto TO it_texto.
*            ENDLOOP.
*
*            wl_header-tdname = wa_zsdt0132-nr_rot.
*            wl_header-tdobject = 'ZSDROTEIRO'.
*            wl_header-tdid     = 'ZROT'.
*            wl_header-tdspras  = sy-langu.
*
*            CALL FUNCTION 'SAVE_TEXT'
*              EXPORTING
*                header          = wl_header
*                savemode_direct = 'X'
*              TABLES
*                lines           = it_texto
*              EXCEPTIONS
*                id              = 1
*                language        = 2
*                name            = 3
*                object          = 4
*                OTHERS          = 5.
*
*            CLEAR it_texto.
*
*            CALL FUNCTION 'COMMIT_TEXT'
*              EXPORTING
*                object          = 'ZSDROTEIRO'
*                name            = wl_header-tdname
*                id              = 'ZROT'
*                language        = sy-langu
*                savemode_direct = ' '.
*
*          ELSE.
*
*            wl_name = wa_zsdt0132-nr_rot.
*
*            CALL FUNCTION 'DELETE_TEXT'
*              EXPORTING
*                id        = 'ZROT'
*                language  = sy-langu
*                name      = wl_name
*                object    = 'ZSDROTEIRO'
*              EXCEPTIONS
*                not_found = 1
*                OTHERS    = 2.
*
*            COMMIT WORK.
*
*            MOVE '@1F@' TO wa_zsdt0132-texto.
*            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING texto.
*            CLEAR: wa_zsdt0132.
*          ENDIF.
*
*        ENDIF.
*
*    ENDCASE.

    CALL METHOD ctl_alv1->refresh_table_display.

  ENDMETHOD .


  METHOD handle_double_click.


    DATA(lv_row)     = e_row.
    DATA(lv_column)  = e_column.

    CASE lv_column.

*      WHEN 'LZONE'.
*
*        READ TABLE it_zsdt0132 INTO DATA(wa) INDEX lv_row.
*        IF sy-subrc = 0 AND wa-lzone IS NOT INITIAL.
*
*          CALL TRANSACTION 'OVR1' AND SKIP FIRST SCREEN.
*
*        ENDIF.

      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.

  METHOD on_f4.
    PERFORM f4_filial USING er_event_data es_row_no e_fieldname.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5000 OUTPUT.

  SET PF-STATUS 'SG5000'.

  CASE abap_on.
    WHEN p_c. "Cliente
      DATA(vl_title) = 'Cliente'.

    WHEN p_f. "Fornecedor
      vl_title = 'Fornec.'.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  SET TITLEBAR 'TG5000' WITH vl_title.

  IF p_c EQ abap_true.
    wa_header-kunnr = p_kunnr.
**<<<------"187296 - NMS - INI------>>>
    wa_header-lzone = lzone.
    SELECT SINGLE partner supplier transpzone
      FROM ibupacustomer AS a
      INNER JOIN but020 AS b
       ON a~businesspartner EQ b~partner
      INNER JOIN adrc AS c
       ON b~addrnumber EQ c~addrnumber
      INTO ( wa_header-partn, wa_header-lifnr, wa_header-bpzon )
    WHERE customer EQ wa_header-kunnr.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE lzone FROM lfa1 INTO ( wa_header-fnzon ) WHERE lifnr EQ wa_header-lifnr.

    ENDIF.
**<<<------"187296 - NMS - FIM------>>>
  ELSEIF p_f EQ abap_true.
**<<<------"187296 - NMS - INI------>>>
*    wa_header-kunnr = p_lifnr.
    wa_header-lifnr = p_lifnr.
    wa_header-fnzon = lzone.
    SELECT SINGLE partner customer transpzone
      FROM ibpsupplier AS a
      INNER JOIN but020 AS b
       ON a~businesspartner EQ b~partner
      INNER JOIN adrc AS c
       ON b~addrnumber EQ c~addrnumber
      INTO ( wa_header-partn, wa_header-kunnr, wa_header-bpzon )
    WHERE supplier EQ wa_header-lifnr.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE lzone FROM kna1 INTO ( wa_header-lzone ) WHERE kunnr EQ wa_header-kunnr.

    ENDIF.
**<<<------"187296 - NMS - FIM------>>>
  ENDIF.
**<<<------"187296 - NMS - INI------>>>
  LOOP AT SCREEN.
    IF screen-group1 EQ 'BTE'.
      IF ( wa_header-bpzon EQ wa_header-lzone   AND
           wa_header-bpzon EQ wa_header-fnzon ) OR
           wa_header-bpzon IS INITIAL.
        screen-input = 0.
        MODIFY SCREEN.

      ENDIF.

    ENDIF.

  ENDLOOP.

  IF ( wa_header-bpzon NE wa_header-lzone   OR
       wa_header-bpzon NE wa_header-fnzon ) AND
       wa_header-bpzon IS NOT INITIAL.
* Zona Transp. BP diferente do Cliente e/ou Fornec.
* Clique no botão "Equaliza Zona" para ajustar.
    MESSAGE |{ TEXT-016 } { TEXT-017 }|  TYPE 'S' DISPLAY LIKE 'W'.

  ENDIF.

  IF NOT wa_header-bpzon IS INITIAL.
    SELECT SINGLE tz~zone1 AS lzone, tz~land1 AS aland, tz~zlatitude, tz~zlongitude, tz~z_url_localizacao,
           tx~vtext
      FROM tzone AS tz
      LEFT OUTER JOIN tzont AS tx
       ON tz~land1 EQ tx~land1 AND
          tz~zone1 EQ tx~zone1 AND
          tx~spras EQ @sy-langu
      INTO @DATA(ls_zones_vend)
    WHERE tz~zone1 EQ @wa_header-bpzon.

    IF sy-subrc IS INITIAL.
      READ TABLE it_zsdt0132 TRANSPORTING NO FIELDS WITH KEY lzone = ls_zones_vend-lzone.
      IF NOT sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO it_zsdt0132 ASSIGNING FIELD-SYMBOL(<fs_saida>).
        MOVE-CORRESPONDING ls_zones_vend TO <fs_saida>.
        <fs_saida>-antig = abap_on.
        UNASSIGN <fs_saida>.

      ENDIF.

    ENDIF.

  ENDIF.
**<<<------"187296 - NMS - FIM------>>>
  wa_header-name1 = vg_name1.
  wa_header-city  = city.
  wa_header-cep   = cep.
*  wa_header-lzone = lzone.    "<<<------"187296 - NMS ------->>>

  PERFORM bloqueia_linhas.     "<<<------"187296 - NMS ------->>>
*  PERFORM busca_info_docs.
  PERFORM carrega_alv.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5000 INPUT.

  ctl_alv1->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SAVE'.

      PERFORM validar_dados.

      PERFORM salvar_zonas.
**<<<------"187296 - NMS - INI------>>>
    WHEN 'EQUZON'. "Eqializa Zona.
* Equalizaçãop de Zona de Transporte Do BP para os Cliente e Fornecedor.
      PERFORM zf_equaliza_zona_transp.
**<<<------"187296 - NMS - FIM------>>>
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5000_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_alv .

  IF g_custom_container IS INITIAL.

    "REFRESH LT_F4.
*  DEFINE F4.
*    LT_F4-FIELDNAME = &1.
*    LT_F4-REGISTER  = &2.
*    LT_F4-GETBEFORE = &3.
*    APPEND LT_F4 .
*    CLEAR LT_F4.
*  END-OF-DEFINITION.

*  F4:
*      'CITY1'  ABAP_TRUE ABAP_TRUE.



    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER5000'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
    IF p_c EQ abap_true.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog USING:
        01 'ALAND'             ''         '3'   ' ' ' '  ' '   'X'  ' '   'País' '',
        02 'LZONE'             'ZSDT0132' '10'  ' ' 'X'  ' '   'X'  'L'   'Zona Transporte' '',
        03 'VTEXT'             ''         '10'  ' ' 'X'  ' '   'X'  'L'   'Descrição ZT' '',
        04 'ZLATITUDE'         ''         '14'  'X' ' '  ' '   ' '  'R'   'Latitude' '',
        05 'ZLONGITUDE'        ''         '15'  'X' ' '  ' '   ' '  'R'   'Longitude' '',
        06 'Z_URL_LOCALIZACAO' ''         '120' 'X' ' '  ' '   ' '  'L'   'Ender. URL Localiza' ''.

    ELSEIF p_f EQ abap_true.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog USING:
        01 'ALAND'             ''         '3'   ' ' ' '  ' '   'X'  ' '   'País' '',
        02 'LZONE'             'ZSDT0132' '10'  ' ' ' '  ' '   'X'  'L'   'Zona Transporte' '',
        03 'VTEXT'             ''         '10'  ' ' ' '  ' '   'X'  'L'   'Descrição ZT' '',
        04 'ZLATITUDE'         ''         '14'  'X' ' '  ' '   ' '  'R'   'Latitude' '',
        05 'ZLONGITUDE'        ''         '15'  'X' ' '  ' '   ' '  'R'   'Longitude' '',
        06 'Z_URL_LOCALIZACAO' ''         '120' 'X' ' '  ' '   ' '  'L'   'Ender. URL Localiza' ''.

    ENDIF.


    REFRESH lt_f4.
    lt_f4[] =
    VALUE #(
             ( fieldname = 'TRANSP_RESP' register = abap_true  getbefore = abap_true )
             ( fieldname = 'CITY1' register = abap_true  getbefore = abap_true )
             ( fieldname = 'LZONE' register = abap_true  getbefore = abap_true ) "<<<------"171563 - NMS------>>>
           ).

    "GS_LAYOUT-SEL_MODE   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
*    gs_layout-cwidth_opt = 'X'.

*    PERFORM sort USING 'NR_ROT' CHANGING it_sort.
    PERFORM excluir_botoes.
    "PERFORM REGISTRAR_F4.

    CREATE OBJECT ctl_alv1
      EXPORTING
        i_parent = g_custom_container.           "ALV Lote

*    CALL METHOD ctl_alv1->register_f4_for_fields
*      EXPORTING
*        it_f4 = lt_f4[].

    SET HANDLER:
     lcl_event_handler=>handle_double_click FOR ctl_alv1,   "FF #143815
     lcl_event_handler=>toolbar             FOR ctl_alv1,
     lcl_event_handler=>user_command        FOR ctl_alv1,
     lcl_event_handler=>handle_button_click FOR ctl_alv1,
     lcl_event_handler=>on_data_changed     FOR ctl_alv1.
*     lcl_event_handler=>on_f4               FOR ctl_alv1.



    CALL METHOD ctl_alv1->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_zsdt0132
        it_sort              = it_sort.

    CALL METHOD ctl_alv1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.

    CALL METHOD ctl_alv1->get_frontend_layout
      IMPORTING
        es_layout = gs_layout.

    gs_layout-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1->set_frontend_layout
      EXPORTING
        is_layout = gs_layout.

    CALL METHOD ctl_alv1->refresh_table_display.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog  TABLES p_it_fieldacatalog_lote STRUCTURE lvc_s_fcat
                           USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_len)
                                 VALUE(p_edit)
                                 VALUE(p_f4)
                                 VALUE(p_lzero)
                                 VALUE(p_opt)
                                 VALUE(p_just)
                                 VALUE(p_header)
                                 VALUE(p_checkbox).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-outputlen  = p_len.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-checktable = p_tabname.
  wa_fieldcatalog-lzero      = p_lzero.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-just       = p_just.
  wa_fieldcatalog-checkbox   = p_checkbox.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog_lote.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
FORM sort USING p_fieldname
               CHANGING p_it_sort TYPE lvc_t_sort.

  DATA wa_sort TYPE lvc_s_sort.

  wa_sort-spos = '1' .
  wa_sort-fieldname = p_fieldname.
  wa_sort-up = 'X' . "A to Z
  wa_sort-down = space .
  APPEND wa_sort TO p_it_sort.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS
*&---------------------------------------------------------------------*
FORM bloqueia_linhas.

  DATA: vl_cont     TYPE i,
        it_celltab  TYPE lvc_t_styl,
        el_celltab  TYPE lvc_s_styl,
        wa_zsdt0132 TYPE ty_zsdt0132_alv.

  LOOP AT it_zsdt0132 INTO wa_zsdt0132.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_zsdt0132-cellstyles.

    IF wa_zsdt0132-antig IS INITIAL.
      IF wa_zsdt0132-lzone IS INITIAL.
        el_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

      ELSE.
        el_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

      ENDIF.

    ELSE.
      el_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

    ENDIF.

    el_celltab-fieldname = 'ZLATITUDE'.
    APPEND el_celltab TO it_celltab.

    el_celltab-fieldname = 'ZLONGITUDE'.
    APPEND el_celltab TO it_celltab.

    el_celltab-fieldname = 'Z_URL_LOCALIZACAO'.
    APPEND el_celltab TO it_celltab.

    PERFORM fill_celltab USING wa_zsdt0132-antig
                         CHANGING it_celltab.

    INSERT LINES OF it_celltab INTO TABLE wa_zsdt0132-cellstyles.
    MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX vl_cont.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab  USING    p_antig
                   CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_antig EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  wa_celltab-fieldname = 'LZONE'.
  wa_celltab-style     = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

  wa_celltab-fieldname = 'ALAND'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT wa_celltab INTO TABLE p_it_celltab.

  wa_celltab-fieldname = 'VTEXT'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT wa_celltab INTO TABLE p_it_celltab.

ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
FORM excluir_botoes.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO it_exclude.
  CLEAR ls_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_INFO_DOCS
*&---------------------------------------------------------------------*
FORM busca_info_docs .

  DATA: wa_zsdt0132 TYPE ty_zsdt0132_alv.

  DATA: wl_name    TYPE thead-tdname,
        it_texto   TYPE STANDARD TABLE OF tline,
        vl_obj_key TYPE sibflporb-instid,
        vl_lines   TYPE i,
        anexos     TYPE TABLE OF bdn_con.

  LOOP AT it_zsdt0132 INTO wa_zsdt0132.

    "Atualiza Ícone de Texto
    REFRESH: it_texto.
    CLEAR: wl_name.
    wl_name = wa_zsdt0132-nr_rot.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZROT'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZSDROTEIRO'
      TABLES
        lines                   = it_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF it_texto IS INITIAL.
      wa_zsdt0132-texto = '@1F@'.
    ELSE.
      wa_zsdt0132-texto = '@1E@'.
    ENDIF.

    "Atualiza Ícone de Anexo
    vl_obj_key = wa_zsdt0132-nr_rot.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZSDR0061'
        objkey             = vl_obj_key
        client             = sy-mandt
      TABLES
        gos_connections    = anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    DESCRIBE TABLE anexos LINES vl_lines.

    IF vl_lines NE 0.
      wa_zsdt0132-anexo = '@1E@'.
    ELSE.
      wa_zsdt0132-anexo = '@1F@'.
    ENDIF.

    MODIFY it_zsdt0132 FROM wa_zsdt0132 TRANSPORTING texto anexo.
    CLEAR: it_texto, vl_obj_key, vl_lines, anexos.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_ROTEIROS
*&---------------------------------------------------------------------*
FORM salvar_roteiros .

  DATA: wa_zsdt0132     TYPE ty_zsdt0132_alv,
        wa_zsdt0132_aux TYPE zsdt0132.

  LOOP AT it_zsdt0132 INTO wa_zsdt0132 WHERE antig NE abap_true.

    wa_zsdt0132_aux-nr_rot     = wa_zsdt0132-nr_rot.

    IF p_c EQ abap_true.
      wa_zsdt0132_aux-kunnr      = p_kunnr.
**<<<------"171563 - NMS - INI------>>>
      SELECT SINGLE land1 INTO @DATA(vl_land1) FROM kna1 WHERE kunnr EQ @p_kunnr.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * FROM tzone INTO @DATA(el_zone) WHERE land1 EQ @vl_land1
                                                         AND zone1 EQ @wa_zsdt0132-lzone.

        IF sy-subrc IS INITIAL.
          DATA(el_zone2) = el_zone.
          CLEAR el_zone2.
          MOVE-CORRESPONDING wa_zsdt0132 TO el_zone2.
          MOVE: sy-mandt          TO el_zone2-mandt,
                vl_land1          TO el_zone2-land1,
                wa_zsdt0132-lzone TO el_zone2-zone1.

          IF el_zone2 NE el_zone.
* Atualiza Zona de transporte "RGA
*            DATA(tl_mesg) = NEW zcl_zona_transporte( )->atualiza_zona( EXPORTING i_tzone = el_zone2 ).

          ENDIF.

        ENDIF.

      ENDIF.
**<<<------"171563 - NMS - FIM------>>>
    ELSEIF p_f EQ abap_true.
      wa_zsdt0132_aux-lifnr      = p_lifnr.
    ENDIF.

    wa_zsdt0132_aux-rot_desc   = wa_zsdt0132-rot_desc.
    wa_zsdt0132_aux-tel_number = wa_zsdt0132-tel_number.
    wa_zsdt0132_aux-lzone      = wa_zsdt0132-lzone.         "FF #143815
    wa_zsdt0132_aux-rot_obs    = wa_zsdt0132-rot_obs.
    wa_zsdt0132_aux-city1      = wa_zsdt0132-city1.
    wa_zsdt0132_aux-uf         = wa_zsdt0132-uf.
    wa_zsdt0132_aux-marca      = wa_zsdt0132-marca.
    wa_zsdt0132_aux-status     = wa_zsdt0132-status.
    wa_zsdt0132_aux-usnam      = sy-uname.
    wa_zsdt0132_aux-data_atual = sy-datum.
    wa_zsdt0132_aux-hora_atual = sy-uzeit.
    wa_zsdt0132_aux-armazem = wa_zsdt0132-armazem.
    wa_zsdt0132_aux-transportadora = wa_zsdt0132-transportadora.
    wa_zsdt0132_aux-transp_resp = wa_zsdt0132-transp_resp.

    IF wa_zsdt0132-status EQ 'I'.
      wa_zsdt0132_aux-usnam_i      = sy-uname.
      wa_zsdt0132_aux-data_atual_i = sy-datum.
      wa_zsdt0132_aux-hora_atual_i = sy-uzeit.
    ENDIF.

    MODIFY zsdt0132 FROM wa_zsdt0132_aux.

    wa_zsdt0132-antig = 'X'.
    MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX sy-tabix.
**<<<------"171563 - NMS - INI------>>>
*    IF NOT tl_mesg[] IS INITIAL.   /// comentado RGA
*      READ TABLE tl_mesg INTO DATA(el_mesg) INDEX 1.
*      DATA(vl_txt) = |{ TEXT-004 } e { el_mesg-msg1 } { el_mesg-msg2 }|.
*      MESSAGE vl_txt TYPE el_mesg-type.
*      CLEAR: tl_mesg, el_mesg, vl_txt.
*
*    ELSE.
***<<<------"171563 - NMS - FIM------>>>
*      MESSAGE TEXT-004 TYPE 'S'.
***<<<------"171563 - NMS - INI------>>>
*    ENDIF.
**<<<------"171563 - NMS - FIM------>>>
  ENDLOOP.

  CALL METHOD ctl_alv1->get_frontend_layout
    IMPORTING
      es_layout = gs_layout.

  gs_layout-cwidth_opt = abap_true.

  CALL METHOD ctl_alv1->set_frontend_layout
    EXPORTING
      is_layout = gs_layout.

  CALL METHOD ctl_alv1->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_dados.

  DATA: tl_cell TYPE lvc_t_ceno. "**<<<------"171563 - NMS------>>>

  DATA: wa_zsdt0132 TYPE ty_zsdt0132_alv,
        wa_twspr    TYPE twspr.
**<<<------"171563 - NMS - INI------>>>
  DATA: vl_oblig TYPE c,
        vl_armaz TYPE c,
        vl_marca TYPE c.
**<<<------"171563 - NMS - FIM------>>>
  LOOP AT it_zsdt0132 INTO wa_zsdt0132 WHERE antig NE abap_true.
**<<<------"171563 - NMS - INI------>>>
    DATA(vl_tabix) = sy-tabix.
    DATA(vl_tbxax) = sy-tabix.
    CLEAR vl_tbxax.
**<<<------"171563 - NMS - FIM------>>>
    IF p_c EQ abap_true.
      SELECT SINGLE land1 INTO @DATA(vl_land1) FROM kna1 WHERE kunnr EQ @p_kunnr.

      SELECT zone1 FROM tzone
       INTO TABLE @DATA(lt_zone)
      WHERE land1 EQ @vl_land1.

      IF sy-subrc IS INITIAL.
        READ TABLE lt_zone WITH KEY zone1 = wa_zsdt0132-lzone TRANSPORTING NO FIELDS.

        IF NOT sy-subrc IS INITIAL.
          LOOP AT wa_zsdt0132-cellstyles ASSIGNING FIELD-SYMBOL(<fs_cellstyles>) WHERE fieldname EQ 'ZLATITUDE'
                                                                                    OR fieldname EQ 'ZLONGITUDE'
                                                                                    OR fieldname EQ 'Z_URL_LOCALIZACAO'.

            <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_disabled.
            DATA(vl_zona) = abap_on.

            IF vl_tbxax NE vl_tabix.
              APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
              <fs_cell>-col_id = 7.
              <fs_cell>-row_id = vl_tbxax = vl_tabix.

            ENDIF.

          ENDLOOP.

          MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX vl_tabix.

        ENDIF.

      ENDIF.

      IF wa_zsdt0132-status     IS INITIAL OR
         wa_zsdt0132-rot_desc   IS INITIAL OR
         wa_zsdt0132-rot_obs    IS INITIAL OR
         wa_zsdt0132-tel_number IS INITIAL OR
         wa_zsdt0132-city1      IS INITIAL.
**<<<------"171563 - NMS - INI------>>>
*        MESSAGE TEXT-011 TYPE 'E'.
        IF vl_zona IS INITIAL.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 2
                                                  vl_tabix
                                                  wa_zsdt0132-rot_desc
                                         CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 3
                                                  vl_tabix
                                                  wa_zsdt0132-rot_obs
                                         CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 6
                                                  vl_tabix
                                                  wa_zsdt0132-tel_number
                                         CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 13
                                                  vl_tabix
                                                  wa_zsdt0132-status
                                         CHANGING vl_oblig.

        ENDIF.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.
      "      PERFORM F_VALIDA_TELEFONE USING WA_ZSDT0132-TEL_NUMBER.

    ELSEIF p_f EQ abap_true.

      IF wa_zsdt0132-status   IS INITIAL OR
         wa_zsdt0132-rot_desc IS INITIAL OR
         wa_zsdt0132-city1    IS INITIAL OR
         wa_zsdt0132-uf       IS INITIAL OR
         wa_zsdt0132-marca    IS INITIAL.
**<<<------"171563 - NMS - INI------>>>
*        MESSAGE TEXT-005 TYPE 'E'.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 2
                                                vl_tabix
                                                wa_zsdt0132-rot_desc
                                       CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 3
                                                vl_tabix
                                                wa_zsdt0132-city1
                                       CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 4
                                                vl_tabix
                                                wa_zsdt0132-marca
                                       CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 5
                                                vl_tabix
                                                wa_zsdt0132-uf
                                       CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 8
                                                vl_tabix
                                                wa_zsdt0132-status
                                       CHANGING vl_oblig.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.
**<<<------"171563 - NMS - INI------>>>
      IF vl_oblig IS INITIAL.
**<<<------"171563 - NMS - FIM------>>>
        IF wa_zsdt0132-transportadora IS NOT INITIAL.
**<<<------"171563 - NMS - INI------>>>
*          IF wa_zsdt0132-armazem IS INITIAL.
*            MESSAGE TEXT-010 TYPE 'E'.
*          ENDIF.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 9
                                                  vl_tabix
                                                  wa_zsdt0132-armazem
                                         CHANGING vl_armaz.
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.
**<<<------"171563 - NMS - INI------>>>
        IF vl_armaz IS INITIAL.
          CLEAR wa_twspr.
**<<<------"171563 - NMS - FIM------>>>
          SELECT SINGLE *
            FROM twspr
            INTO wa_twspr
            WHERE wrkst EQ wa_zsdt0132-marca.
**<<<------"171563 - NMS - INI------>>>
*          IF sy-subrc IS NOT INITIAL.
*            MESSAGE TEXT-009 TYPE 'E'.
*
*          ENDIF.
* Verifica se o campo não está preenchido.
          PERFORM zf_check_field_initial   TABLES tl_cell
                                            USING 5
                                                  vl_tabix
                                                  wa_twspr-wrkst
                                         CHANGING vl_marca.

        ENDIF.

      ENDIF.
**<<<------"171563 - NMS - FIM------>>>
    ENDIF.

  ENDLOOP.
**<<<------"171563 - NMS - INI------>>>
  IF NOT vl_zona IS INITIAL.
    DELETE tl_cell WHERE col_id NE 7.
* Verifica se há celulas da Grid ALV que foram criticadas.
    IF NOT tl_cell[] IS INITIAL.
* Marca as celulas da Grid ALV que foram criticadas.
      ctl_alv1->set_selected_cells_id( EXPORTING it_cells = tl_cell ).

    ENDIF.

    MESSAGE 'Zona Transporte inválida!' TYPE 'E'.

  ENDIF.
* Verifica se há celulas da Grid ALV que foram criticadas.
  IF NOT tl_cell[] IS INITIAL.
* Marca as celulas da Grid ALV que foram criticadas.
    ctl_alv1->set_selected_cells_id( EXPORTING it_cells = tl_cell ).

    CASE abap_on.
      WHEN p_c. "Cliente
* Preencher os Campos Obrigatórios (Descrição Roteiro / Observação / Telefone / Status).
        MESSAGE TEXT-011 TYPE 'E'.

      WHEN p_f. "Fornecedor
        CASE abap_on.
          WHEN vl_oblig. "Valida campos obrigatórios
* Preencher os Campos Obrigatórios (Nome / Municipio / UF / Marca / Roteiro / Status).
            MESSAGE TEXT-005 TYPE 'E'.

          WHEN vl_armaz. "Valida Armazem
* Local de Embarque só pode ser "Transportadora" se também for "Armazém"!
            MESSAGE TEXT-010 TYPE 'E'.

          WHEN vl_marca. "Valida Marca
* Marca não existe
            MESSAGE TEXT-009 TYPE 'E'.

          WHEN OTHERS.
*       Do nothing
        ENDCASE.

      WHEN OTHERS.
* Do nothing
    ENDCASE.

  ENDIF.
**<<<------"171563 - NMS - FIM------>>>
ENDFORM.

FORM f4_filial USING: er_event_data TYPE REF TO cl_alv_event_data
                              es_row_no TYPE lvc_s_roid
                              e_fieldname TYPE lvc_fname.

  DATA: it_ret_5420       TYPE STANDARD TABLE OF ddshretval,
        it_f4_5420        TYPE STANDARD TABLE OF ty_f4_filial,
        it_f4_5420_cidade TYPE STANDARD TABLE OF ty_f4_grid,
        it_fe_grid        TYPE STANDARD TABLE OF ty_f4_grid,
        it_tvkbt          TYPE STANDARD TABLE OF tvkbt,
        it_kna1           TYPE STANDARD TABLE OF kna1,
        it_lfa1           TYPE STANDARD TABLE OF lfa1,
        wa_tvkbt          TYPE tvkbt,
        wa_kna1           TYPE kna1,
        wa_lfa1           TYPE lfa1.

  DATA: wa_f4_5420        TYPE ty_f4_filial,
        wa_f4_5420_cidade TYPE ty_f4_grid,
        wa_frete_5420     TYPE ty_zsdt0132_alv,
        wa_ret            TYPE ddshretval,
        wa_modi           TYPE lvc_s_modi.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  DATA : it_fmap TYPE STANDARD TABLE OF dselc,
         wa_fmap TYPE dselc.

  READ TABLE it_zsdt0132 INTO wa_frete_5420 INDEX es_row_no-row_id.
  IF sy-subrc IS INITIAL AND wa_frete_5420-antig EQ abap_false.
    CASE e_fieldname.
      WHEN 'TRANSP_RESP'.
        wa_fmap-fldname = 'FILIAL_RESP'.
        wa_fmap-dyfldname = 'Transp. Resp.'.
        APPEND wa_fmap TO it_fmap.

        SELECT *
          FROM tvkbt
          INTO TABLE it_tvkbt.

        LOOP AT it_tvkbt INTO wa_tvkbt.
          IF wa_tvkbt-vkbur(1) = 'T'.
            wa_f4_5420-filial_resp = wa_tvkbt-vkbur.
            wa_f4_5420-bezei = wa_tvkbt-bezei.
            APPEND wa_f4_5420 TO it_f4_5420.
            CLEAR: wa_f4_5420.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'FILIAL_RESP'
            window_title    = 'Lista de Pedidos'(002)
            value_org       = 'S'
            dynprofield     = 'FILIAL_RESP'
          TABLES
            value_tab       = it_f4_5420
            return_tab      = it_ret_5420
            dynpfld_mapping = it_fmap
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'TRANSP_RESP'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
      WHEN 'CITY1'.

        wa_fmap-fldname = 'ORT01'.
        wa_fmap-dyfldname = 'Município'.
        APPEND wa_fmap TO it_fmap.

        CASE abap_true.
          WHEN p_c.
            SELECT *
              FROM kna1
              WHERE kunnr = @wa_header-kunnr
              INTO TABLE @it_kna1.

            LOOP AT it_kna1 INTO wa_kna1.

              wa_f4_5420_cidade-cep = wa_kna1-pstlz.
              wa_f4_5420_cidade-uf = wa_kna1-regio.
              wa_f4_5420_cidade-cidade = wa_kna1-ort01.
              APPEND wa_f4_5420_cidade TO it_f4_5420_cidade.
              CLEAR: wa_f4_5420_cidade.
            ENDLOOP.

          WHEN p_f.

*            SELECT *
*            FROM LFA1
*            WHERE LIFNR = @WA_HEADER-KUNNR
*            INTO TABLE @IT_LFA1.
*
*            LOOP AT IT_LFA1 INTO WA_LFA1.
*
*              WA_F4_5420_CIDADE-CEP = WA_LFA1-PSTLZ.
*              WA_F4_5420_CIDADE-UF = WA_LFA1-REGIO.
*              WA_F4_5420_CIDADE-CIDADE = WA_LFA1-ORT01.
*              APPEND WA_F4_5420_CIDADE TO IT_F4_5420_CIDADE.
*              CLEAR: WA_F4_5420_CIDADE.
*
*            ENDLOOP.


        ENDCASE.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ORT01'
            window_title    = 'Lista de Pedidos'(002)
            value_org       = 'S'
            dynprofield     = 'ORT01'
          TABLES
            value_tab       = it_f4_5420_cidade
            return_tab      = it_ret_5420
            dynpfld_mapping = it_fmap
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'CITY1'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
          READ TABLE it_f4_5420_cidade INTO DATA(uf) INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'UF'.
          wa_modi-value     = uf-uf.
          APPEND wa_modi TO <itab>.
        ELSE.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'CITY1'.
          wa_modi-value     = ' '.
          APPEND wa_modi TO <itab>.
          "READ TABLE IT_F4_5420_CIDADE INTO data(UF_limpo) INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'UF'.
          wa_modi-value     = ' '.
          APPEND wa_modi TO <itab>.

        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
**<<<------"171563 - NMS - INI------>>>
      WHEN 'LZONE'.
        wa_fmap-fldname   = 'LAND1'.
        wa_fmap-dyfldname = 'País/reg.'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'ZONE1'.
        wa_fmap-dyfldname = 'Zona transporte'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'VTEXT'.
        wa_fmap-dyfldname = 'Descrição'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'ZLATITUDE'.
        wa_fmap-dyfldname = 'Latitude'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'ZLONGITUDE'.
        wa_fmap-dyfldname = 'Longitude'.
        APPEND wa_fmap TO it_fmap.

*        wa_fmap-fldname   = 'Z_URL_LOCALIZACAO'.
*        wa_fmap-dyfldname = 'Ender. URL'.
*        APPEND wa_fmap TO it_fmap.

        SELECT a~land1, b~zone1, vtext, zlatitude, zlongitude "z_url_localizacao
          FROM kna1 AS a
          INNER JOIN tzont AS b
           ON a~land1 EQ b~land1
          INNER JOIN tzone AS c
            ON b~land1 EQ c~land1 AND
               b~zone1 EQ c~zone1
          INTO TABLE @DATA(lt_zone)
        WHERE kunnr EQ @p_kunnr.

        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'ZONE1'
              window_title    = 'Lista de Pedidos'(002)
              value_org       = 'S'
              dynprofield     = 'ZONE1'
            TABLES
              value_tab       = lt_zone
              return_tab      = it_ret_5420
              dynpfld_mapping = it_fmap
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.

          IF sy-subrc IS INITIAL.
            ASSIGN er_event_data->m_data->* TO <itab>.

            READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
            IF sy-subrc IS INITIAL.
              wa_modi-row_id    = es_row_no-row_id.
              wa_modi-fieldname = e_fieldname.
              wa_modi-value     = wa_ret-fieldval.
            ENDIF.

            APPEND wa_modi TO <itab>.

          ENDIF.

        ENDIF.
**<<<------"171563 - NMS - FIM------>>>
    ENDCASE.
  ENDIF.
**<<<------"171563 - NMS - INI------>>>
  CHECK e_fieldname EQ 'LZONE'.
* Inibe a execução da rotina padrão do F4.
  er_event_data->m_event_handled = abap_on.
**<<<------"171563 - NMS - FIM------>>>
ENDFORM.
**<<<------"187296 - NMS - INI------>>>
**&---------------------------------------------------------------------*
**& Form f_monta_shdb
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_monta_shdb USING p_pais
*                        p_cod_zona
*                        p_desc.
*
*  PERFORM f_preenche_bdc USING:
*
*          'X'    'SAPL080V'       '0180',
*          ' '    'BDC_CURSOR'     'V_TZONE-VTEXT(01)',
*          ' '    'BDC_OKCODE'     '=NEWL'.
*
*  PERFORM f_preenche_bdc USING:
*        'X'    'SAPL080V'       '0180',
*        ' '    'BDC_CURSOR'     'V_TZONE-VTEXT(01)',
*        ' '    'BDC_OKCODE'     '/00',
*        ' '    'V_TZONE-LAND1(01)'  p_pais,
*        ' '    'V_TZONE-ZONE1(01)'  p_cod_zona,
*        ' '    'V_TZONE-VTEXT(01)'  p_desc.
*
*  PERFORM f_preenche_bdc USING:
*        'X'    'SAPL080V'       '0180',
*        ' '    'BDC_CURSOR'     'V_TZONE-LAND1(02)',
*        ' '    'BDC_OKCODE'     '=SAVE'.
*
*
*ENDFORM.
*
*FORM f_preenche_bdc USING dynbegin
*                           name
*                           value.
*
*  IF dynbegin = 'X'.
*    MOVE: name      TO st_bdcdata-program,
*          value     TO st_bdcdata-dynpro,
*          dynbegin  TO st_bdcdata-dynbegin.
*    APPEND st_bdcdata TO it_bdcdata.
*  ELSE.
*
*    MOVE: name  TO st_bdcdata-fnam,
*          value TO st_bdcdata-fval.
*    APPEND st_bdcdata TO it_bdcdata.
*
*  ENDIF.
*
*  CLEAR st_bdcdata.
*
*ENDFORM.
*
*FORM f_call_ovr1 CHANGING p_erro.
*
*  CALL TRANSACTION 'OVR1' USING it_bdcdata
*                            MODE  vg_mode
*                            UPDATE vg_s
*                            MESSAGES INTO it_msg.
*
*  IF line_exists( it_msg[ msgtyp = 'E' ] ).
*    p_erro = 'X'.
*    MESSAGE 'Erro ao criar Zona de transporte' TYPE 'E'.
*  ELSE.
*    MESSAGE 'Zona de transporte criada com sucesso!' TYPE 'S'.
*  ENDIF.
*
*ENDFORM.
**<<<------"187296 - NMS - FIM------>>>
FORM f_popup_zona_transp USING p_lzone
                               p_kunnr
*                              uv_lzone_empty   "<<<------"187296 - NMS ------->>>
                               p_ucomm.

  DATA: tl_bp TYPE cvis_ei_extern_t.

  DATA: el_bp TYPE cvis_ei_extern.

  DATA: vl_text1 TYPE text132.

  CONSTANTS: cl_task_update TYPE bus_ei_object_task VALUE 'U'.

  DATA: lv_answer.

  DATA wa_zsdt0153 TYPE zlest0153.
**<<<------"187296 - NMS - INI------>>>
*  DATA:
*
*    customerno    TYPE bapikna103-customer,
*    pi_salesorg   LIKE bapikna102-salesorg,
*    pi_distr_chan LIKE bapikna102-distr_chan,
*    pi_division   LIKE bapikna102-division,
*    lt_return     LIKE bapireturn1.

*  DATA:
*    ls_return_detail        LIKE bapireturn1,
*    ls_personaldatax        LIKE bapikna101_1x,
*    ls_personaldata         LIKE bapikna101_1,
*    ls_personaldatachg      LIKE bapikna101_1,
*    ls_opt_personaldata     LIKE bapikna105,
*    ls_opt_personaldatax    LIKE bapikna105x,
*    ls_personaldata_new     LIKE bapikna101_1,
*    ls_opt_personaldata_new LIKE bapikna105,
*    ls_companydata          LIKE bapikna106,
*    ls_companydatachg       LIKE bapikna106,
*    ls_opt_companydata      LIKE bapikna105,
*    ls_companydatax         LIKE bapikna106x,
*    lv_consumer_flag        LIKE bapikna107-consumer_flag,
*    ls_addresstype_no       LIKE bapikna109.
**<<<------"187296 - NMS - FIM------>>>

**<<<------"171563 - NMS - INI------>>>
**<<<------"187296 - NMS - INI------>>>
** Verifica se a Zona de Transporte está em branco ou diferente.
*  CASE uv_lzone_empty.
*    WHEN abap_on.  "Zona de Transporte branco
** Zona de Transporte do cadastro do cliente em branco.
*      vl_text1 = TEXT-012.
*
*    WHEN abap_off. "Zona de Transporte diferente
** Zona de Transporte do cadastro do cliente diferente.
*      vl_text1 = TEXT-016.
*
*    WHEN OTHERS.
**   Do nothing
*  ENDCASE.
**<<<------"187296 - NMS - FIM------>>>
  IF p_ucomm = 'DEFI'.

    IF p_c IS NOT INITIAL.

      CLEAR: vl_text1.

      SELECT SINGLE lzone FROM kna1
        INTO @DATA(lv_zone)
        WHERE kunnr = @p_kunnr.

    ELSE.

      SELECT SINGLE lzone FROM lfa1
        INTO @lv_zone
        WHERE lifnr = @p_lifnr.

    ENDIF.

    IF lv_zone IS NOT INITIAL.

      IF lv_zone NE p_lzone.

        SELECT COUNT(*)
          FROM zlest0153
          WHERE lzone = @lv_zone
          AND   kunnr = @p_kunnr.

        IF sy-subrc NE 0.

          wa_zsdt0153-land1 = 'BR'.
          wa_zsdt0153-lzone = lv_zone.
*            LIFNR
          wa_zsdt0153-kunnr = p_kunnr.
          wa_zsdt0153-lifnr = p_lifnr.
          wa_zsdt0153-us_registro = sy-uname.
          wa_zsdt0153-dt_registro = sy-datum.
          wa_zsdt0153-hr_registro = sy-uzeit.
*
          MODIFY zlest0153 FROM wa_zsdt0153.

        ENDIF.


        vl_text1 = |Deseja substituir a zona { lv_zone }|.
        DATA(vl_text2) = | pela { p_lzone } |.


      ELSE.

        MESSAGE |Zona já definida como padrão| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ELSE.

      vl_text1 = |Deseja definir a zona { p_lzone } como padrão|.

    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1 = vl_text1
        diagnosetext2 = vl_text2
        textline1     = space
        titel         = TEXT-015
        start_column  = 25
        start_row     = 6
*       CANCEL_DISPLAY       = 'X'
      IMPORTING
        answer        = lv_answer.

    CLEAR: vl_text1, vl_text2.


  ELSEIF p_ucomm EQ 'ZONA'.


    vl_text1 = |Deseja definir a zona { p_lzone } como padrão|.

    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1 = vl_text1
        diagnosetext2 = vl_text2
        textline1     = space
        titel         = TEXT-015
        start_column  = 25
        start_row     = 6
*       CANCEL_DISPLAY       = 'X'
      IMPORTING
        answer        = lv_answer.
**<<<------"187296 - NMS - INI------>>>
  ELSEIF p_ucomm EQ 'EQUZON'. "Eqializa Zona.
    lv_answer = 'J'. "Sim
**<<<------"187296 - NMS - FIM------>>>
  ENDIF.
**<
  IF lv_answer = 'J'. "Sim

*    wa_header-lzone = p_lzone. "<<<------"171563 - NMS------>>>
*    customerno = p_kunnr.
*
*    SELECT SINGLE * FROM knvv WHERE kunnr = @customerno INTO @DATA(ls_knvv).
*
*    pi_salesorg   = ls_knvv-vkorg.
*    pi_distr_chan = ls_knvv-vtweg.
*    pi_division   = ls_knvv-spart.
*
** Chamada da BAPI
*
*    CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL1'
*      EXPORTING
*        customerno              = customerno
*        pi_salesorg             = pi_salesorg
*        pi_distr_chan           = pi_distr_chan
*        pi_division             = pi_division
*      IMPORTING
*        pe_personaldata         = ls_personaldata
*        pe_opt_personaldata     = ls_opt_personaldata
*        pe_personaldata_new     = ls_personaldata_new
*        pe_opt_personaldata_new = ls_opt_personaldata_new
*        pe_companydata          = ls_companydata
*        pe_opt_companydata      = ls_opt_companydata
*        pe_consumer_flag        = lv_consumer_flag
*        pe_addresstype_no       = ls_addresstype_no
*        return                  = ls_return_detail.
*
*    IF ls_personaldata IS INITIAL AND ls_personaldata_new IS NOT INITIAL.
*      ls_personaldata = ls_personaldata_new.
*    ENDIF.
*
*    ls_opt_personaldata-transpzone = p_lzone. "Nova zona de transporte
*    ls_personaldata-currency = ls_knvv-waers.
*
*    IF ls_personaldata-currency IS INITIAL.
*      ls_personaldata-currency = 'BRL'.
*    ENDIF.
*
*    FIELD-SYMBOLS: <lv_field>  TYPE any,
*                   <lv_fieldx> TYPE any.
*
*    "Preenchendo os campos das estruturas X
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE ls_personaldata TO <lv_field>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <lv_field> IS NOT INITIAL.
*        ASSIGN COMPONENT sy-index OF STRUCTURE ls_personaldatax TO <lv_fieldx>.
*        <lv_fieldx> = 'X'.
*      ENDIF.
*    ENDDO.
*
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE ls_opt_personaldata TO <lv_field>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <lv_field> IS NOT INITIAL.
*        ASSIGN COMPONENT sy-index OF STRUCTURE ls_opt_personaldatax TO <lv_fieldx>.
*        <lv_fieldx> = 'X'.
*      ENDIF.
*    ENDDO.
*
*    CLEAR ls_personaldata-title_key.
*
*    CALL FUNCTION 'BAPI_CUSTOMER_CHANGEFROMDATA1'
*      EXPORTING
*        pi_personaldata      = ls_personaldata
*        pi_personaldatax     = ls_personaldatax
*        pi_opt_personaldata  = ls_opt_personaldata
*        pi_opt_personaldatax = ls_opt_personaldatax
*        customerno           = customerno
*        pi_salesorg          = pi_salesorg
*        pi_distr_chan        = pi_distr_chan
*        pi_division          = pi_division
*      IMPORTING
*        return               = lt_return.
*
** Confirmação das mudanças
***<<<------"171563 - NMS - INI------>>>
**    IF sy-subrc = 0.

*<<<------"171563 - NMS - INI------>>>
*    IF sy-subrc = 0.


    IF p_c IS NOT INITIAL.
*** Business Partner(BP) - Partner
      SELECT SINGLE * FROM ibupacustomer INTO @DATA(el_customer) WHERE customer EQ @p_kunnr.
*** GUID de um endereço de parceiro de negócios
      SELECT SINGLE b~address_guid
        FROM but021_fs AS a
        INNER JOIN but020 AS b
         ON a~addrnumber EQ b~addrnumber
        INTO @DATA(vl_aguid)
      WHERE a~partner EQ @el_customer-businesspartner
        AND adr_kind  EQ 'XXDEFAULT'.
***Header.
      el_bp-partner-header-object_task                  = cl_task_update.
      el_bp-partner-header-object_instance-bpartnerguid = el_customer-businesspartneruuid.
*** Addresses
      APPEND INITIAL LINE TO el_bp-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_addresses>).
      <fs_addresses>-task                         = cl_task_update.
      <fs_addresses>-data_key-guid                = vl_aguid.
      <fs_addresses>-data-postal-data-transpzone  = p_lzone.
      <fs_addresses>-data-postal-datax-transpzone = abap_on.
*** Business Partner(BP) - Customer
*** Header.
      el_bp-customer-header-object_task                           = cl_task_update.
      el_bp-customer-header-object_instance-kunnr                 = p_kunnr.

*** Central Data - Addresses
      el_bp-customer-central_data-address-task                    = cl_task_update.
      el_bp-customer-central_data-address-postal-data-transpzone  = p_lzone.
      el_bp-customer-central_data-address-postal-datax-transpzone = abap_on.
      el_bp-customer-central_data-address-postal-data-county = 'BR'.
      el_bp-customer-central_data-address-postal-datax-country = abap_on.
**<<<------"187296 - NMS - INI------>>>
*** Business Partner(BP) - Vendor
*** Header.
      el_bp-vendor-header-object_task                           = cl_task_update.
      el_bp-vendor-header-object_instance-lifnr                 = el_customer-supplier.
*** Central Data - Addresses
      el_bp-vendor-central_data-address-task                    = cl_task_update.
      el_bp-vendor-central_data-address-postal-data-transpzone  = p_lzone.
      el_bp-vendor-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187296 - NMS - FIM------>>>
    ELSEIF p_f IS NOT INITIAL.


      SELECT SINGLE * FROM i_businesspartnersupplier INTO @DATA(el_vendor) WHERE supplier = @p_lifnr.
*** GUID de um endereço de parceiro de negócios
      SELECT SINGLE b~address_guid
        FROM but021_fs AS a
        INNER JOIN but020 AS b
         ON a~addrnumber EQ b~addrnumber
         INTO @vl_aguid
      WHERE a~partner EQ @el_vendor-businesspartner
        AND adr_kind  EQ 'XXDEFAULT'.
*** Business Partner(BP) - Vendor
** Addresses

      el_bp-partner-header-object_task                  = cl_task_update.
      el_bp-partner-header-object_instance-bpartnerguid = el_vendor-businesspartneruuid.

      APPEND INITIAL LINE TO el_bp-partner-central_data-address-addresses ASSIGNING <fs_addresses>.
      <fs_addresses>-task                         = cl_task_update.
      <fs_addresses>-data_key-guid                = vl_aguid.
      <fs_addresses>-data-postal-data-transpzone  = p_lzone.
      <fs_addresses>-data-postal-datax-transpzone = abap_on.
*** Header.
      el_bp-vendor-header-object_task                           = cl_task_update.
      el_bp-vendor-header-object_instance-lifnr                 = p_lifnr.

*** Central Data - Addresses
      el_bp-vendor-central_data-address-task                    = cl_task_update.
      el_bp-vendor-central_data-address-postal-data-transpzone  = p_lzone.
      el_bp-vendor-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187296 - NMS - INI------>>>
*** Business Partner(BP) - Customer
*** Header.
      el_bp-customer-header-object_task                           = cl_task_update.
      el_bp-customer-header-object_instance-kunnr                 = el_vendor-customer.
*** Central Data - Addresses
      el_bp-customer-central_data-address-task                    = cl_task_update.
      el_bp-customer-central_data-address-postal-data-transpzone  = p_lzone.
      el_bp-customer-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187296 - NMS - FIM------>>>
    ENDIF.

*** Master Data - Maintain Business Partners
    cl_md_bp_maintain=>validate_single( EXPORTING
                                          i_data        = el_bp
                                        IMPORTING
                                          et_return_map = DATA(tl_return_map)
                                       ).

    IF line_exists( tl_return_map[ type = 'E' ] ) OR
       line_exists( tl_return_map[ type = 'A' ] ).
      READ TABLE tl_return_map INTO DATA(el_return_map) WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        MESSAGE el_return_map-message TYPE 'I' DISPLAY LIKE el_return_map-type.

      ELSE.
        READ TABLE tl_return_map INTO el_return_map WITH KEY type = 'A'.

        IF sy-subrc IS INITIAL.
          MESSAGE el_return_map-message TYPE 'I' DISPLAY LIKE el_return_map-type.

        ENDIF.

      ENDIF.

      COMMIT WORK.
      EXIT.

    ELSE.
* Add single BP to IMPORTING table
      INSERT el_bp INTO TABLE tl_bp.

      cl_md_bp_maintain=>maintain(
        EXPORTING
          i_data     = tl_bp
          i_test_run = abap_off
        IMPORTING
          e_return   = DATA(tl_return)
      ).

    ENDIF.

    wa_header-lzone = p_lzone.
    lzone = p_lzone.
    wa_header-fnzon = wa_header-bpzon = p_lzone.    "<<<------"187296 - NMS ------->>>
*    uv_lzone_empty  = sy-abcde+25(1). "Z
    MESSAGE |Zona de Transp. Cadastro do BP, Cliente e Fornecedor atualizado com sucesso. | TYPE 'S'.
**<<<------"171563 - NMS - FIM------>>>
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

**<<<------"171563 - NMS - INI------>>>
  ELSE.
    COMMIT WORK.
**<<<------"171563 - NMS - FIM------>>>
  ENDIF.
**<<<------"171563 - NMS - INI------>>>
  PERFORM busca_nome.
  SET SCREEN 0.
  LEAVE TO SCREEN 5000.
**<<<------"171563 - NMS - FIM------>>>

ENDFORM.
*&---------------------------------------------------------------------*
*& Form salvar_zonas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM salvar_zonas .

  DATA: wa_zsdt0132     TYPE ty_zsdt0132_alv,
        wa_zsdt0132_aux TYPE zsdt0132.


  DATA wa_zlest0153 TYPE zlest0153.

  LOOP AT it_zsdt0132 INTO wa_zsdt0132.


    CLEAR wa_zlest0153.
    MOVE-CORRESPONDING wa_zsdt0132 TO wa_zlest0153.

    MOVE wa_zsdt0132-aland TO wa_zlest0153-land1.

    IF p_c IS NOT INITIAL.
      wa_zlest0153-kunnr = p_kunnr.
    ELSE.
      wa_zlest0153-lifnr = p_lifnr.
    ENDIF.

    wa_zlest0153-us_registro = sy-uname.
    wa_zlest0153-dt_registro = sy-datum.
    wa_zlest0153-hr_registro = sy-uzeit.

    MODIFY zlest0153 FROM wa_zlest0153.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE * FROM tzone INTO @DATA(el_zone) WHERE land1 EQ @wa_zsdt0132-aland
                                                       AND zone1 EQ @wa_zsdt0132-lzone.

      IF sy-subrc IS INITIAL.
        DATA(el_zone2) = el_zone.
        CLEAR el_zone2.
        MOVE-CORRESPONDING wa_zsdt0132 TO el_zone2.
        MOVE: sy-mandt          TO el_zone2-mandt,
              wa_zsdt0132-aland TO el_zone2-land1,
              wa_zsdt0132-lzone TO el_zone2-zone1.

        IF el_zone2 NE el_zone.
          DATA(tl_mesg) = NEW zcl_zona_transporte( )->atualiza_zona( EXPORTING i_tzone = el_zone2 ).

        ENDIF.
      ENDIF.

    ENDIF.

    IF NOT tl_mesg[] IS INITIAL.
      READ TABLE tl_mesg INTO DATA(el_mesg) INDEX 1.

      IF el_mesg-type EQ sy-abcde+4(1). "E - Erro
        DATA(vl_txt) = |{ el_mesg-msg1 } { el_mesg-msg2 }|.

      ELSE.
        vl_txt = |{ TEXT-004 } e { el_mesg-msg1 } { el_mesg-msg2 }|.

      ENDIF.

      MESSAGE vl_txt TYPE el_mesg-type.
      CLEAR: tl_mesg, el_mesg, vl_txt.

    ELSE.
**<<<------"171563 - NMS - FIM------>>>
      MESSAGE TEXT-004 TYPE 'S'.
**<<<------"171563 - NMS - INI------>>>
    ENDIF.

  ENDLOOP.

  CALL METHOD ctl_alv1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  LOOP AT it_fieldcatalog INTO DATA(wa_fieldcat).

    CLEAR:  wa_fieldcat-edit.

    MODIFY it_fieldcatalog FROM wa_fieldcat.
  ENDLOOP.

  CALL METHOD ctl_alv1->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcatalog.

  CALL METHOD ctl_alv1->get_frontend_layout
    IMPORTING
      es_layout = gs_layout.

  gs_layout-cwidth_opt = abap_true.

  CALL METHOD ctl_alv1->set_frontend_layout
    EXPORTING
      is_layout = gs_layout.

  CALL METHOD ctl_alv1->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validar_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validar_dados .

**<<<------"187296 - NMS - INI------>>>
  DATA:tl_cell TYPE lvc_t_ceno.

  DATA: vl_message    TYPE spo_value,
        vl_zlatitude  TYPE spo_value,
        vl_zlongitude TYPE spo_value.
**<<<------"187296 - NMS - FIM------>>>
  LOOP AT it_zsdt0132 INTO DATA(ls_saida).

    DATA(lv_tabix) = sy-tabix.


    DATA: lv_lati_aux TYPE string.
    DATA  lv_long_aux TYPE string.
    DATA  lv_lati_n TYPE j_1bdocnum.
    DATA  lv_long_n TYPE j_1bdocnum.

    lv_lati_aux = ls_saida-zlatitude.
    lv_long_aux = ls_saida-zlongitude.

    REPLACE ALL OCCURRENCES OF '.' IN lv_lati_aux WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN lv_lati_aux WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN lv_long_aux WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN lv_long_aux WITH ''.

    CONDENSE lv_lati_aux NO-GAPS.
    CONDENSE lv_long_aux NO-GAPS.

    lv_lati_n = lv_lati_aux.
    lv_long_n = lv_long_aux.

    IF ( lv_lati_n IS NOT INITIAL AND
         lv_long_n IS INITIAL )   OR
       ( lv_long_n IS NOT INITIAL AND
         lv_lati_n IS INITIAL ).

      MESSAGE 'Campos latitute e longitude devem estar preenchidos ou zerados!' TYPE 'E'.


    ENDIF.
**<<<------"187296 - NMS - INI------>>>
*    "latitude -99.99999 a 99.99999
*    PERFORM valida_coordenadas USING ls_saida-zlatitude
*                                                     2
*                                                     lv_tabix.
*    "longitude: -180.00000 a 180.00000
*    PERFORM valida_coordenadas USING ls_saida-zlongitude
*                                                     3
*                                                     lv_tabix.
    vl_zlatitude = ls_saida-zlatitude.
    CONDENSE vl_zlatitude NO-GAPS.
    vl_zlongitude = ls_saida-zlongitude.
    CONDENSE vl_zlongitude NO-GAPS.
    CLEAR: ls_saida-zlatitude, ls_saida-zlongitude, vg_erro.

    IF vl_zlatitude  LT 0 OR
       vl_zlongitude LT 0.
      DATA(lv_lowzero) = abap_on.

    ENDIF.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
    PERFORM zf_cond_existe_lat_long_dec IN PROGRAM zsdr0061 USING    vl_zlatitude
                                                                     vl_zlongitude
                                                                     sy-abcde(1)   "A - Ambos
                                                            CHANGING ls_saida-zlatitude
                                                                     ls_saida-zlongitude
                                                                     vl_message.

    vg_erro = COND #( WHEN vl_message EQ space THEN abap_off ELSE abap_on ).

    IF NOT vg_erro IS INITIAL.
* Latitude
      APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
      <fs_cell>-col_id   = 4.
      <fs_cell>-row_id   = lv_tabix.
* Longitude
      APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
      <fs_cell>-col_id   = 5.
      <fs_cell>-row_id   = lv_tabix.
* Marca as celulas da Grid ALV que foram criticadas.
      ctl_alv1->set_selected_cells_id( EXPORTING it_cells = tl_cell ).

      MESSAGE vl_message TYPE 'E'.

    ELSE.
      IF lv_lowzero IS NOT INITIAL.
        MODIFY it_zsdt0132 FROM ls_saida INDEX lv_tabix TRANSPORTING zlatitude zlongitude.
        CLEAR lv_lowzero.
        CALL METHOD ctl_alv1->refresh_table_display.

      ENDIF.

    ENDIF.
**<<<------"187296 - NMS - FIM------>>>
  ENDLOOP.

ENDFORM.
**<<<------"187296 - NMS - INI------>>>
**&---------------------------------------------------------------------*
**& Form valida_coordenadas
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LS_SAIDA_ZLATITUDE
**&      --> P_2
**&---------------------------------------------------------------------*
*FORM valida_coordenadas  USING    p_coord
*                                  VALUE(p_dec)
*                                  p_index.
*
*  DATA: tl_cell TYPE lvc_t_ceno.
*  DATA: wl_name  TYPE thead-tdname,
*        it_texto TYPE STANDARD TABLE OF tline.
*
*
*  DATA:
*    lv_valid                TYPE abap_bool VALUE abap_true,
*    lv_has_decimal          TYPE abap_bool VALUE abap_false,
*    lv_char                 TYPE c LENGTH 1,
*    lv_start                TYPE i VALUE 0,
*    lv_count_before_decimal TYPE i VALUE 0,
*    lv_decimal_pos          TYPE i.
*
*  DATA lv_num TYPE p DECIMALS 5.
*
*
*  IF p_coord IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  CONDENSE p_coord NO-GAPS.
*
*
** Check if first character is a minus sign
*  IF p_coord+0(1) = '-'.
** Skip the minus sign for checking
*    lv_start = 1.
*  ENDIF.
*
** Check all characters
*  DO strlen( p_coord ) - lv_start TIMES.
*    DATA(lv_index) = sy-index - 1 + lv_start.
*    lv_char = p_coord+lv_index(1).
*
** Check if character is a digit or decimal point
*    IF lv_char CO '0123456789'.
** Valid digit, continue
*      IF lv_has_decimal = abap_false.
*        lv_count_before_decimal = lv_count_before_decimal + 1.
*      ENDIF.
*      CONTINUE.
*    ELSEIF lv_char = '.' AND lv_has_decimal = abap_false.
** First decimal point found
*      lv_has_decimal = abap_true.
*      lv_decimal_pos = lv_index.
*
*      DATA lv_dec TYPE string.
*      DATA lv_dec_n TYPE j_1bdocnum.
*
*      lv_num = p_coord(lv_index).
*
*      lv_dec = p_coord+3(6).
*
*      REPLACE ALL OCCURRENCES OF '.' IN lv_dec WITH ''.
*      CONDENSE lv_dec NO-GAPS.
*
*      lv_dec_n = lv_dec.
*
*
*      CONTINUE.
*    ELSE.
** Invalid character found
*      lv_valid = abap_false.
*      EXIT.
*    ENDIF.
*  ENDDO.
*
*  "somente ponto decimal
*  IF lv_has_decimal = abap_false.
*    lv_valid = abap_false.
*  ENDIF.
*
*  "somente 2 ou 3 digitos antes do decimal
*  IF lv_count_before_decimal > p_dec.
*    lv_valid = abap_false.
*  ENDIF.
*
*  "se existe ao menos um digito após decimal
*  IF lv_decimal_pos = strlen( p_coord ) - 1.
*    lv_valid = abap_false.
*  ENDIF.
*
*  IF p_dec EQ 3.
*    IF lv_num > 180 OR lv_num < -180.
*
*      lv_valid = abap_false.
*
*
*    ELSEIF lv_num = 180.
*
*      IF lv_dec_n IS NOT INITIAL.
*        lv_valid = abap_false.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.
*
*  DATA lv_txt_lati TYPE string.
*
*  lv_txt_lati = p_coord.
*  REPLACE ALL OCCURRENCES OF '.' IN lv_txt_lati WITH ''.
*  CONDENSE lv_txt_lati NO-GAPS.
*
*
*  DATA(lv_len) = strlen( p_coord ).
*
*  DATA(lv_len_dec) = strlen( p_coord ) - lv_count_before_decimal - 1.
*
*
*
*
*  IF p_coord(1) = '-'.
*
*
*    IF lv_len_dec <> 6.
*      lv_valid = abap_false.
*    ENDIF.
*
*
*    IF p_dec EQ 2.
*
*      IF lv_len > 9.
*        lv_valid = abap_false.
*      ENDIF.
*
*      IF lv_num < -90.
*        lv_valid = abap_false.
*
*      ELSEIF lv_num = -90.
*
*        IF lv_dec_n IS NOT INITIAL.
*          lv_valid = abap_false.
*        ENDIF.
*
*      ENDIF.
*
*    ELSE.
*
*      IF lv_len > 10.
*        lv_valid = abap_false.
*      ENDIF.
*
*    ENDIF.
*
*  ELSE.
*
*    IF lv_len_dec <> 5.
*      lv_valid = abap_false.
*    ENDIF.
*
*    IF p_dec EQ 2.
*
*      IF lv_len > 8.
*        lv_valid = abap_false.
*      ENDIF.
*
*      IF lv_num > 90.
*        lv_valid = abap_false.
*
*
*      ELSEIF lv_num = 90.
*
*        IF lv_dec_n IS NOT INITIAL.
*          lv_valid = abap_false.
*        ENDIF.
*
*      ENDIF.
*
**      DATA lv_max TYPE bseg-dmbtr.
**      lv_max = lv_txt_lati.
**      IF lv_max > 9999999 or lv_max < -9999999.
**        lv_valid = abap_false.
**      ENDIF.
*
*    ELSE.
*
*      IF lv_len > 9.
*        lv_valid = abap_false.
*      ENDIF.
*
*    ENDIF.
*
*
*  ENDIF.
*
*  IF p_index EQ 0.
*
*    IF lv_valid NE abap_true.
*
*      IF p_dec EQ 2.
*        MESSAGE 'Formato do campo latitude inválido. Ex:(-)90.00000!' TYPE 'S' DISPLAY LIKE 'E'.
*        vg_erro = abap_true.
*      ELSE.
*        MESSAGE 'Formato do campo longitude inválido.Ex:(-)180.00000' TYPE 'S' DISPLAY LIKE 'E'.
*        vg_erro = abap_true.
*      ENDIF.
*
*    ENDIF.
*
*  ELSE.
*    APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
** Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
*
*
*    IF p_dec EQ 2. "latitude
*      <fs_cell>-col_id   = 4.
*      <fs_cell>-row_id   = p_index.
*
*    ELSEIF p_dec EQ 3."longitude
*      <fs_cell>-col_id   = 5.
*      <fs_cell>-row_id   = p_index.
*    ENDIF.
*
*
*    IF NOT tl_cell[] IS INITIAL.
** Marca as celulas da Grid ALV que foram criticadas.
*      ctl_alv1->set_selected_cells_id( EXPORTING it_cells = tl_cell ).
*
*      IF lv_valid NE abap_true.
*
*        IF p_dec EQ 2.
*          MESSAGE 'Formato do campo latitude inválido. Ex:(-)90.00000!' TYPE 'E'.
*        ELSE.
*          MESSAGE 'Formato do campo longitude inválido.Ex:(-)180.00000' TYPE 'E'.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    FREE tl_cell.
*
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_EQUALIZA_ZONA_TRANSP
*&---------------------------------------------------------------------*
*& Equalização de Zona de Transporte Do BP para os Cliente e Fornecedor
*&---------------------------------------------------------------------*
FORM zf_equaliza_zona_transp.

  CASE abap_on.
    WHEN p_c. "Cliente
      DATA(vl_id_part) = wa_header-kunnr.

    WHEN p_f. "Fornecedor
      vl_id_part       = wa_header-lifnr.

    WHEN OTHERS.
*           Do nothing
  ENDCASE.

  PERFORM f_popup_zona_transp USING wa_header-bpzon
                                    vl_id_part
                                    sy-ucomm.

ENDFORM.
**<<<------"187296 - NMS - FIM------>>>
