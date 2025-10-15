*&----------------------------------------------------------------------------*
*&  Include           ZSDR0061_5000
*&----------------------------------------------------------------------------*
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2HNK |01/04/2025 |Ajuste diversos no cadastro de Roteiro&*
*&                                    |Chamado: 171563 e 177861.             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MVY |22/08/2025 |Ajst. na atribuição da Zona ao Roteiro&*
*&                                    |Chamado: 187199.                      &*
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
    wa_tool-function = 'NEW'.
    wa_tool-icon     = '@17@'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "Botão Deleta Nova Linha
*    WA_TOOL-FUNCTION = 'DEL'.
*    WA_TOOL-ICON     = '@18@'.
*    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
*    CLEAR WA_TOOL.
    "Botão Edita Linha Antiga
    wa_tool-function = 'EDIT'.
    wa_tool-icon     = '@0Z@'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    " Projeto Insumos 2025 - 16.06 - Inicio
*****<<<------"171563 - NMS - INI------>>>
**** Acionar os botões Nova Linha ou Edita Linha Antiga não implememntar o
**** botão Criar Zona.
    READ TABLE it_zsdt0132 TRANSPORTING NO FIELDS WITH KEY antig = space.
    CHECK NOT sy-subrc IS INITIAL.
***     AND
***          NOT p_c      IS INITIAL.    "Cliente.
*****<<<------"171563 - NMS - FIM------>>>
***    "FF inicio #143815
    " Projeto Insumos 2025 - 16.06 - Fim
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

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: tl_good_cells     TYPE lvc_t_modi,
          lv_value(500)     TYPE c,
          lv_lifnr_rot      TYPE lifnr, "*-US190444-10.09.2025-#190444-JT-inicio
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
**<<<------"182100 - NMS - INI------>>>
    DATA: vl_zlatit_in  TYPE spo_value,
          vl_zlongi_in  TYPE spo_value,
          vl_zlatit_out TYPE zde_latitude,
          vl_zlongi_out TYPE zde_longitude,
          vl_message    TYPE spo_value.
**<<<------"182100 - NMS - FIM------>>>

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      READ TABLE it_zsdt0132 INTO DATA(_0132)  INDEX ls_good-row_id.

*-US190444-10.09.2025-#190444-JT-inicio
      IF _0132-lifnr_rot IS NOT INITIAL.
        LOOP AT _0132-cellstyles  INTO DATA(_cells) WHERE fieldname = 'CITY1' OR
                                                          fieldname = 'UF'.
          _cells-style = cl_gui_alv_grid=>mc_style_disabled.
          MODIFY _0132-cellstyles FROM _cells INDEX sy-tabix.
        ENDLOOP.
        MODIFY it_zsdt0132        FROM _0132  INDEX ls_good-row_id.
      ENDIF.
*-US190444-10.09.2025-#190444-JT-fim
      APPEND _0132                  TO it_zsdt0132_safra.
    ENDLOOP.

*-US190444-10.09.2025-#190444-JT-inicio
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'LIFNR_ROT' .
      READ TABLE it_zsdt0132 INTO DATA(_zsdt0132)  INDEX ls_good-row_id.

      lv_lifnr_rot = ls_good-value.

      IF lv_lifnr_rot IS INITIAL.
        MESSAGE s024(sd) WITH 'Cod.Fornecedor Obrigatorio!' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        INTO @DATA(_lfa1)
        FROM lfa1
       WHERE lifnr = @lv_lifnr_rot.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Cod.Fornecedor Incorreto!' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        INTO @DATA(_adrc)
        FROM adrc
       WHERE addrnumber = @_lfa1-adrnr.

      IF sy-subrc = 0.
        _zsdt0132-rot_desc = _adrc-name1.
        _zsdt0132-endereco = _adrc-street && '-' && _adrc-house_num1 && '-' && _adrc-city2.
        _zsdt0132-city1    = _adrc-city1.
        _zsdt0132-uf       = _adrc-region.
      ELSE.
        _zsdt0132-rot_desc = _lfa1-name1.
        _zsdt0132-endereco = _lfa1-stras  && '-' && _lfa1-ort02.
        _zsdt0132-city1    = _lfa1-ort01.
        _zsdt0132-uf       = _lfa1-regio.
      ENDIF.

      lv_value = _zsdt0132-rot_desc.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ROT_DESC'
          i_value     = lv_value.

      lv_value = _zsdt0132-endereco.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ENDERECO'
          i_value     = lv_value.

      lv_value = _zsdt0132-city1.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CITY1'
          i_value     = lv_value.

      lv_value = _zsdt0132-uf.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'UF'
          i_value     = lv_value.

      LOOP AT _zsdt0132-cellstyles INTO DATA(_cellstyles) WHERE fieldname = 'CITY1' OR
                                                                fieldname = 'UF'.
        _cellstyles-style = cl_gui_alv_grid=>mc_style_disabled.
        MODIFY _zsdt0132-cellstyles FROM _cellstyles INDEX sy-tabix.
      ENDLOOP.

      MODIFY it_zsdt0132 FROM _zsdt0132 INDEX ls_good-row_id.
      DATA(vl_refresh) = 1.
    ENDLOOP.

    IF sy-subrc <> 0 AND p_f = abap_true.
      READ TABLE er_data_changed->mt_good_cells INTO ls_good INDEX 1.
      IF sy-subrc = 0.
        READ TABLE it_zsdt0132 INTO _zsdt0132   INDEX ls_good-row_id.

        IF _zsdt0132-lifnr_rot IS INITIAL.
          MESSAGE s024(sd) WITH 'Cod.Fornecedor Obrigatorio!' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF _zsdt0132-lifnr_rot IS NOT INITIAL.
          SELECT SINGLE *
            INTO @_lfa1
            FROM lfa1
           WHERE lifnr = @_zsdt0132-lifnr_rot.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH 'Cod.Fornecedor Incorreto!' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

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

*    SELECT zone1
    SELECT *
**<<<------"171563 - NMS - FIM----->>>
      FROM tzone INTO TABLE @DATA(lt_zone)
**<<<------"171563 - NMS - INI------>>>
*      WHERE land1 = 'BR'.
      WHERE land1 = @vl_land1.
**<<<------"171563 - NMS - FIM----->>>
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'LZONE'.
      lv_zone = ls_good-value.
**<<<------"171563 - NMS - INI------>>>
      IF lv_zone IS INITIAL.
        vl_refresh = 1.

        READ TABLE it_zsdt0132 INTO DATA(e_zsdt0132) INDEX ls_good-row_id.
        e_zsdt0132-lzone = lv_zone.
        CLEAR: e_zsdt0132-zlatitude, e_zsdt0132-zlongitude, e_zsdt0132-z_url_localizacao.
        LOOP AT e_zsdt0132-cellstyles ASSIGNING FIELD-SYMBOL(<fs_cellstyles>) WHERE fieldname EQ 'ZLATITUDE'
                                                                                 OR fieldname EQ 'ZLONGITUDE'
                                                                                 OR fieldname EQ 'Z_URL_LOCALIZACAO'.
          <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_disabled.

        ENDLOOP.

        MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX ls_good-row_id.
        CONTINUE.

      ENDIF.

*      READ TABLE lt_zone WITH KEY zone1 = lv_zone TRANSPORTING NO FIELDS.
      READ TABLE lt_zone INTO DATA(el_tzone) WITH KEY zone1 = lv_zone.
**<<<------"171563 - NMS - FIM----->>>
      IF sy-subrc <> 0.
        MESSAGE 'Zona Transporte inválida!' TYPE 'S' DISPLAY LIKE 'E'.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'LZONE'
            i_value     = ' '.
**<<<------"171563 - NMS - INI------>>>
      ELSE.
        CLEAR e_zsdt0132.
        READ TABLE it_zsdt0132 INTO e_zsdt0132 INDEX ls_good-row_id.
        e_zsdt0132-lzone             = el_tzone-zone1.
**<<<------"182100 - NMS - INI------>>>
*        e_zsdt0132-zlatitude         = el_tzone-zlongitude.
*        e_zsdt0132-zlongitude        = el_tzone-zlongitude.
        e_zsdt0132-zlatitude         = COND #( WHEN el_tzone-zlatitude EQ '       0.000000' THEN space
                                               WHEN el_tzone-zlatitude EQ '0.000000'        THEN space
                                               ELSE el_tzone-zlatitude ).
        e_zsdt0132-zlongitude        = COND #( WHEN el_tzone-zlongitude EQ '       0.000000' THEN space
                                               WHEN el_tzone-zlongitude EQ '0.000000'        THEN space
                                               ELSE el_tzone-zlongitude ).
**<<<------"182100 - NMS - FIM------>>>
        e_zsdt0132-z_url_localizacao = el_tzone-z_url_localizacao.
        vl_refresh                   = 1.
        LOOP AT e_zsdt0132-cellstyles ASSIGNING <fs_cellstyles> WHERE fieldname EQ 'ZLATITUDE'
                                                                   OR fieldname EQ 'ZLONGITUDE'
                                                                   OR fieldname EQ 'Z_URL_LOCALIZACAO'.
          <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_enabled.

        ENDLOOP.

        MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX ls_good-row_id.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.

    ENDLOOP.
**<<<------"182100 - NMS - INI------>>>
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'ZLATITUDE'
                                                           OR fieldname EQ 'ZLONGITUDE'
                                                           OR fieldname EQ 'Z_URL_LOCALIZACAO'.

      DATA(vl_tabix) = sy-tabix.    "<<<------"187199 - NMS ------->>>

      CASE ls_good-fieldname.
        WHEN 'ZLATITUDE'.
          vl_zlatit_in = ls_good-value.
          CONDENSE vl_zlatit_in NO-GAPS.
          CLEAR vl_zlongi_in.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
          PERFORM zf_cond_existe_lat_long_dec USING    vl_zlatit_in
                                                       vl_zlongi_in
                                                       sy-abcde+19(1)   "T - Latitude
                                              CHANGING vl_zlatit_out
                                                       vl_zlongi_out
                                                       vl_message.

          IF NOT vl_message IS INITIAL.
            MESSAGE vl_message TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
**<<<------"187199 - NMS - INI------>>>
          ELSE.
            ls_good-value = vl_zlatit_out.
            MODIFY er_data_changed->mt_good_cells FROM ls_good INDEX vl_tabix TRANSPORTING value.

            CLEAR e_zsdt0132.
            READ TABLE it_zsdt0132 INTO e_zsdt0132 INDEX ls_good-row_id.
            e_zsdt0132-zlatitude = vl_zlatit_out.
            MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX ls_good-row_id TRANSPORTING zlatitude.

            READ TABLE <mr> ASSIGNING FIELD-SYMBOL(<fs_mr>) WITH KEY nr_rot = e_zsdt0132-nr_rot.
            IF sy-subrc IS INITIAL.
              <fs_mr>-zlatitude = vl_zlatit_out.

            ENDIF.

            vl_refresh = 1.
**<<<------"187199 - NMS - FIM------>>>
          ENDIF.

        WHEN 'ZLONGITUDE'.
          vl_zlongi_in = ls_good-value.
          CONDENSE vl_zlongi_in NO-GAPS.
          CLEAR vl_zlatit_in.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
          PERFORM zf_cond_existe_lat_long_dec USING    vl_zlatit_in
                                                       vl_zlongi_in
                                                       sy-abcde+6(1)   "G - Longitude
                                              CHANGING vl_zlatit_out
                                                       vl_zlongi_out
                                                       vl_message.

          IF NOT vl_message IS INITIAL.
            MESSAGE vl_message TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
**<<<------"187199 - NMS - INI------>>>
          ELSE.
            ls_good-value = vl_zlongi_out.
            MODIFY er_data_changed->mt_good_cells FROM ls_good INDEX vl_tabix TRANSPORTING value.

            READ TABLE it_zsdt0132 INTO e_zsdt0132 INDEX ls_good-row_id.
            e_zsdt0132-zlongitude = vl_zlongi_out.
            MODIFY it_zsdt0132 FROM e_zsdt0132 INDEX ls_good-row_id TRANSPORTING zlongitude.

            READ TABLE <mr> ASSIGNING <fs_mr> WITH KEY nr_rot = e_zsdt0132-nr_rot.
            IF sy-subrc IS INITIAL.
              <fs_mr>-zlongitude = vl_zlongi_out.

            ENDIF.

            vl_refresh = 1.
**<<<------"187199 - NMS - FIM------>>>
          ENDIF.

        WHEN OTHERS.
* Do nothing
      ENDCASE.

    ENDLOOP.
**<<<------"182100 - NMS - FIM------>>>
    "FF fim
**<<<------"171563 - NMS - INI------>>>
    LOOP AT er_data_changed->mt_mod_cells INTO DATA(el_mod_cells) WHERE fieldname EQ 'LZONE'
                                                                    AND error     EQ abap_on.
      lv_zone = el_mod_cells-value.

      READ TABLE lt_zone WITH KEY zone1 = lv_zone TRANSPORTING NO FIELDS.

      IF NOT sy-subrc IS INITIAL.
        READ TABLE it_zsdt0132 INTO e_zsdt0132 INDEX el_mod_cells-row_id.
        e_zsdt0132-lzone = lv_zone.
* Verifica se o campo Zona de transporte está sem preencher.
        IF e_zsdt0132-lzone IS INITIAL.
          vl_refresh = 1.

        ELSE.
          vl_refresh = 2.

        ENDIF.

        LOOP AT e_zsdt0132-cellstyles ASSIGNING <fs_cellstyles> WHERE fieldname EQ 'ZLATITUDE'
                                                                   OR fieldname EQ 'ZLONGITUDE'
                                                                   OR fieldname EQ 'Z_URL_LOCALIZACAO'.
          <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_disabled.

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

    DATA: vl_resp TYPE c. "<<<------"171563 - NMS------>>>

    IF e_ucomm = 'NEW'.

      CLEAR wa_zsdt0132. ""*-US190444-10.09.2025-#190444-JT-inicio

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSD_ROT'
        IMPORTING
          number                  = wa_zsdt0132-nr_rot
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      wa_zsdt0132-status = 'A'.

      "FF - inicio #143815
      IF p_c = abap_true.

        SELECT SINGLE ort01, regio
          FROM kna1
          INTO ( @wa_zsdt0132-city1, @wa_zsdt0132-uf )
          WHERE kunnr = @wa_header-kunnr.
        "FF - fim
      ELSEIF p_f = abap_true.

        SELECT SINGLE ort01, regio
        FROM lfa1
        INTO ( @wa_zsdt0132-city1, @wa_zsdt0132-uf )
        WHERE lifnr = @wa_header-lifnr. "@wa_header-kunnr. ""*-US190444-10.09.2025-#190444-JT-inicio
      ENDIF.

      APPEND wa_zsdt0132 TO it_zsdt0132.
      PERFORM bloqueia_linhas.
      PERFORM busca_info_docs.

      "CALL METHOD CTL_ALV1->REFRESH_TABLE_DISPLAY.
      CLEAR: wa_zsdt0132.
**<<<------"171563 - NMS - INI------>>>
*    ELSEIF e_ucomm = 'DEL'. "Comentado a botão de deletar BUG26747.
*
*      CLEAR: it_selected_rows, wa_selected_rows.
*
*      CALL METHOD ctl_alv1->get_selected_rows
*        IMPORTING
*          et_index_rows = it_selected_rows.
*
*      LOOP AT it_selected_rows INTO wa_selected_rows.
*        READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.
*        IF sy-subrc IS INITIAL.
*          IF wa_zsdt0132-antig NE abap_true.
*            wa_zsdt0132-rot_desc = 'DELETE'.
*            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index..
*          ENDIF.
*          CLEAR: wa_zsdt0132.
*        ENDIF.
*      ENDLOOP.
*
*      DELETE it_zsdt0132 WHERE rot_desc EQ 'DELETE'.
*      "CALL METHOD CTL_ALV1->REFRESH_TABLE_DISPLAY.
**<<<------"171563 - NMS - FIM------>>>
    ELSEIF e_ucomm = 'EDIT'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv1->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.
**<<<------"171563 - NMS - INI------>>>
      IF it_selected_rows[] IS INITIAL.
        CASE abap_on.
          WHEN p_c. "Cliente
            DATA(vl_texto) = 'Selecione ao menos uma linha para alterar a Zona de transporte'.

          WHEN p_f. "Fornecedor
            vl_texto = 'Selecione ao menos uma linha para alterar o Roteiro'.

          WHEN OTHERS.
*         Do nothing
        ENDCASE.

        MESSAGE vl_texto TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

      ENDIF.
**<<<------"171563 - NMS - FIM------>>>
      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          MOVE abap_false TO wa_zsdt0132-antig.
          MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index..
          CLEAR: wa_zsdt0132.
        ENDIF.
      ENDLOOP.

      PERFORM bloqueia_linhas.
      "CALL METHOD CTL_ALV1->REFRESH_TABLE_DISPLAY.

      "FF inicio #143815

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

      CLEAR: it_selected_rows, wa_selected_rows.


      CALL METHOD ctl_alv1->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF it_selected_rows[] IS INITIAL.
        MESSAGE 'Selecione uma linha para a criação da Zona de transporte' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN."<<<------"171563 - NMS - FIM------>>>
      ENDIF.

      CLEAR lv_erro.

      LOOP AT it_selected_rows INTO wa_selected_rows.

        READ TABLE it_zsdt0132 INTO wa_zsdt0132 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.

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
              MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index TRANSPORTING antig.
              CLEAR: wa_zsdt0132.
              CALL METHOD ctl_alv1->refresh_table_display.
              RETURN.
**<<<------"171563 - NMS - FIM------>>>
            WHEN OTHERS.
**<<<------"171563 - NMS - INI------>>>
*              MESSAGE |Erro ao gerar código da Zona| TYPE 'E' DISPLAY LIKE 'E'.
              MESSAGE |Erro ao gerar código da Zona| TYPE 'S' DISPLAY LIKE 'E'.
              MOVE abap_true TO wa_zsdt0132-antig.
              MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index TRANSPORTING antig.
              CLEAR: wa_zsdt0132.
              CALL METHOD ctl_alv1->refresh_table_display.
              RETURN.
**<<<------"171563 - NMS - FIM------>>>
          ENDCASE.

          ls_zona-cod_zona = wa_zsdt0132-lzone = |{ 'Z' }{ lv_numrange }|.
          ls_zona-desc = |{ 'Roteiro' }{ wa_zsdt0132-nr_rot }|.
**<<<------"171563 - NMS - INI------>>>
* Popup dos valores de Latitude, Longitude e URL de Localização.
          PERFORM zf_popup_get_value_zone USING wa_zsdt0132-lzone
                                                wa_zsdt0132-zlatitude
                                                wa_zsdt0132-zlongitude
                                                wa_zsdt0132-z_url_localizacao
                                                e_ucomm
                                                vl_resp.

          IF vl_resp EQ sy-abcde(1). "A - Abort (Cancelado pelo usuário)
            RETURN.

          ENDIF.

          ls_zona-zlatitude         = wa_zsdt0132-zlatitude.
          ls_zona-zlongitude        = wa_zsdt0132-zlongitude.
          ls_zona-z_url_localizacao = wa_zsdt0132-z_url_localizacao.
**<<<------"171563 - NMS - FIM------>>>
**<<<------"182100 - NMS - INI------>>>
          CASE abap_on.
            WHEN p_c. "Cliente
**<<<------"182100 - NMS - FIM------>>>
              SELECT SINGLE land1, lzone
              FROM kna1
              INTO ( @ls_zona-pais, @lv_lzone )
              WHERE kunnr = @wa_header-kunnr.
              IF sy-subrc <> 0.
                CLEAR ls_zona-pais.
              ENDIF.
**<<<------"182100 - NMS - INI------>>>
            WHEN p_f. "Fornecedor
              SELECT SINGLE land1, lzone
              FROM lfa1
              INTO ( @ls_zona-pais, @lv_lzone )
              WHERE lifnr EQ @wa_header-lifnr.

              IF NOT sy-subrc IS INITIAL.
                CLEAR ls_zona-pais.

              ENDIF.

            WHEN OTHERS.
*           Do nothing
          ENDCASE.
          lv_lzone = wa_header-bpzon.   "<<<------"187199 - NMS ------>>>
**<<<------"182100 - NMS - FIM------>>>
**<<<------"171563 - NMS - INI------>>>
*          DATA(lv_nr_rot) = wa_zsdt0132-nr_rot.             "US 170612
*          MOVE abap_false TO wa_zsdt0132-antig.
*          MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index.
*          CLEAR: wa_zsdt0132.
*          CALL METHOD ctl_alv1->refresh_table_display.
**<<<------"171563 - NMS - FIM------>>>
          "FF #170612 - inicio

          DATA(lt_msg) = NEW zcl_zona_transporte( )->criar_zona(
            i_pais     = ls_zona-pais
            i_cod_zona = ls_zona-cod_zona
**<<<------"171563 - NMS - INI------>>>
            i_zlatitude         = ls_zona-zlatitude
            i_zlongitude        = ls_zona-zlongitude
            i_z_url_localizacao = ls_zona-z_url_localizacao
**<<<------"171563 - NMS - FIM------>>>
            i_desc     = ls_zona-desc
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
          ELSE.
            DATA(lv_nr_rot) = wa_zsdt0132-nr_rot.           "US 170612
            MOVE abap_false TO wa_zsdt0132-antig.
            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index.
            CLEAR: wa_zsdt0132.
**<<<------"171563 - NMS - FIM------>>>
          ENDIF.

*          PERFORM f_monta_shdb USING ls_zona-pais
*                                     ls_zona-cod_zona
*                                     ls_zona-desc.
*
*          PERFORM f_call_ovr1 CHANGING lv_erro.
          "FF #170612 - fim
**<<<------"171563 - NMS - INI------>>>
          IF lv_erro IS INITIAL.
**<<<------"171563 - NMS - FIM------>>>
            TRY.
                " Preenchendo os campos da estrutura com os valores desejados
                ls_153-land1       = ls_zona-pais.
                ls_153-lzone       = ls_zona-cod_zona.
* Projeto Insumos 2025 - 16.06 - Inicio
                IF p_c = abap_true.
                  ls_153-lifnr       = ''.
                  ls_153-kunnr       = wa_header-kunnr.
                  DATA(vl_id_part)   = wa_header-kunnr.   "<<<------"187199 - NMS ------>>>
                ELSEIF p_f = abap_true.
                  ls_153-lifnr       = wa_header-lifnr.   "<<<------"187199 - NMS ------>>>
                  ls_153-kunnr       = ''.
                  vl_id_part         = wa_header-lifnr.   "<<<------"187199 - NMS ------>>>
                ENDIF.
* Projeto Insumos 2025 - 16.06 - Fim

                ls_153-nr_rot      = lv_nr_rot.             "US 170612
                ls_153-us_registro = sy-uname.
                ls_153-dt_registro = sy-datum.
                ls_153-hr_registro = sy-uzeit.

                " Realizando o MODIFY na tabela transparente
                MODIFY zlest0153 FROM ls_153.

              CATCH cx_root INTO DATA(lx_root).
                " Tratamento de exceção genérica
**<<<------"171563 - NMS - INI------>>>
*              MESSAGE |Ocorreu uma exceção: { lx_root->get_text( ) }| TYPE 'E' DISPLAY LIKE 'E'.
                MESSAGE |Ocorreu uma exceção: { lx_root->get_text( ) }| TYPE 'S' DISPLAY LIKE 'E'.
                lv_erro = abap_on.
**<<<------"171563 - NMS - FIM------>>>
            ENDTRY.
**<<<------"171563 - NMS - INI------>>>
          ENDIF.
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.
      ENDLOOP.

      IF lv_erro IS INITIAL. "Salvando os dados do roteiro na ZSDT0132.
        PERFORM check_dados.
        PERFORM salvar_roteiros.
      ENDIF.
**<<<------"171563 - NMS - INI------>>>
*      IF lv_lzone IS INITIAL. "Se o cadastro do cliente estiver com a Zona em branco, abrir popup.
* Se o cadastro do cliente estiver com a Zona em branco ou diferente, abrir popup.
      IF lv_lzone         IS INITIAL   OR
         ls_zona-cod_zona NE lv_lzone.
**<<<------"171563 - NMS - FIM------>>>
        MOVE abap_true TO wa_zsdt0132-antig.
        MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX wa_selected_rows-index TRANSPORTING antig.
        CLEAR: wa_zsdt0132.
**<<<------"171563 - NMS - INI------>>>
        IF lv_erro IS INITIAL.
          IF     lv_lzone IS INITIAL.
            DATA(lv_lzone_empty) = abap_on.
**<<<------"187199 - NMS - INI------>>>
*          ELSEIF ls_zona-cod_zona NE lv_lzone.
*            CLEAR lv_lzone_empty.
*
*          ENDIF.
**<<<------"187199 - NMS - FIM------>>>
**<<<------"171563 - NMS - FIM------>>>
            PERFORM f_popup_zona_transp USING ls_zona-cod_zona
**<<<------"171563 - NMS - INI------>>>
*                                          wa_header-kunnr.
                                              vl_id_part       "<<<------"187199 - NMS ------>>>
                                              lv_lzone_empty.
**<<<------"187199 - NMS - INI------>>>
          ENDIF.
**<<<------"187199 - NMS - FIM------>>>
        ENDIF.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.

    ENDIF.
    "FF fim

*    DATA: wl TYPE lvc_s_stbl. "<<<------"171563 - NMS------>>>

    CALL METHOD ctl_alv1->get_frontend_layout
      IMPORTING
        es_layout = gs_layout.

    gs_layout-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1->set_frontend_layout
      EXPORTING
        is_layout = gs_layout.

    CALL METHOD ctl_alv1->refresh_table_display.
**<<<------"171563 - NMS - INI------>>>
    IF lv_lzone_empty EQ sy-abcde+25(1). "Z
      SET SCREEN 0.
      LEAVE TO SCREEN 5000.

    ENDIF.
**<<<------"171563 - NMS - FIM------>>>
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

    CASE es_col_id.
      WHEN 'ANEXO'.

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

        IF wa_zsdt0132-antig EQ abap_true.
          vl_ip_mode = 'R'.
        ELSE.
          vl_ip_mode = 'E'.
        ENDIF.

        IF wa_zsdt0132-anexo EQ '@1F@'.
          vl_ip_service = 'PCATTA_CREA'.
        ELSE.
          vl_ip_service = 'VIEW_ATTA'.
        ENDIF.

        wa_bor-objkey = wa_zsdt0132-nr_rot.
        wa_bor-objtype = 'ZSDR0061'.

        anexo_obj->set_rw_mode( ip_mode = vl_ip_mode ).

        anexo_obj->start_service_direct(
          EXPORTING
            ip_service       = vl_ip_service
            is_object        = wa_bor
          EXCEPTIONS
            no_object        = 1
            object_invalid   = 2
            execution_failed = 3
            OTHERS           = 4 ).

        COMMIT WORK.

        "Contando o número de anexos p/ atualizar ícone
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

        MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING anexo .
        CLEAR: wa_bor, vl_obj_key, vl_lines, anexos.

      WHEN 'TEXTO'.

        IF wa_zsdt0132-antig EQ abap_true.
          MOVE abap_true TO vl_display_mode.
        ELSE.
          MOVE space TO vl_display_mode.
        ENDIF.

        REFRESH: it_texto, tl_texto.
        CLEAR: wl_texto.

        wl_name = wa_zsdt0132-nr_rot.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id        = 'ZROT'
            language  = sy-langu
            name      = wl_name
            object    = 'ZSDROTEIRO'
          TABLES
            lines     = it_texto
          EXCEPTIONS
            id        = 1
            language  = 2
            name      = 3
            not_found = 4
            OTHERS    = 5.

        IF sy-subrc IS INITIAL.
          LOOP AT it_texto INTO wa_texto.
            MOVE: wa_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Roteiro'
            im_display_mode = vl_display_mode
          CHANGING
            ch_text         = tl_texto.

        IF sy-ucomm EQ 'CX_CONT' AND wa_zsdt0132-antig NE abap_true.  "Salvar

          IF tl_texto[] IS NOT INITIAL.

            MOVE '@1E@' TO wa_zsdt0132-texto.
            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING texto.
            CLEAR: it_texto.

            LOOP AT tl_texto INTO wl_texto.
              MOVE: '*'      TO wa_texto-tdformat,
                    wl_texto TO wa_texto-tdline.
              APPEND wa_texto TO it_texto.
            ENDLOOP.

            wl_header-tdname = wa_zsdt0132-nr_rot.
            wl_header-tdobject = 'ZSDROTEIRO'.
            wl_header-tdid     = 'ZROT'.
            wl_header-tdspras  = sy-langu.

            CALL FUNCTION 'SAVE_TEXT'
              EXPORTING
                header          = wl_header
                savemode_direct = 'X'
              TABLES
                lines           = it_texto
              EXCEPTIONS
                id              = 1
                language        = 2
                name            = 3
                object          = 4
                OTHERS          = 5.

            CLEAR it_texto.

            CALL FUNCTION 'COMMIT_TEXT'
              EXPORTING
                object          = 'ZSDROTEIRO'
                name            = wl_header-tdname
                id              = 'ZROT'
                language        = sy-langu
                savemode_direct = ' '.

          ELSE.

            wl_name = wa_zsdt0132-nr_rot.

            CALL FUNCTION 'DELETE_TEXT'
              EXPORTING
                id        = 'ZROT'
                language  = sy-langu
                name      = wl_name
                object    = 'ZSDROTEIRO'
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            COMMIT WORK.

            MOVE '@1F@' TO wa_zsdt0132-texto.
            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX es_row_no-row_id TRANSPORTING texto.
            CLEAR: wa_zsdt0132.
          ENDIF.

        ENDIF.

    ENDCASE.

    CALL METHOD ctl_alv1->refresh_table_display.

  ENDMETHOD .


  METHOD handle_double_click.


    DATA(lv_row)     = e_row.
    DATA(lv_column)  = e_column.

    CASE lv_column.

      WHEN 'LZONE'.

        READ TABLE it_zsdt0132 INTO DATA(wa) INDEX lv_row.
        IF sy-subrc = 0 AND wa-lzone IS NOT INITIAL.
**<<<------"171563 - NMS - INI------>>>
*          CALL TRANSACTION 'OVR1' AND SKIP FIRST SCREEN.
* Popup dos valores de Latitude, Longitude e URL de Localização
          PERFORM zf_popup_get_value_zone USING wa-lzone
                                                wa-zlatitude
                                                wa-zlongitude
                                                wa-z_url_localizacao
                                                sy-ucomm
                                                lv_column(1).
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.

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
**<<<------"171563 - NMS - INI------>>>
*  SET TITLEBAR 'TG5000'.
  CASE abap_on.
    WHEN p_c. "Cliente
      DATA(vl_title) = 'Cliente'.

    WHEN p_f. "Fornecedor
      vl_title = 'Fornec.'.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.
  SET TITLEBAR 'TG5000' WITH vl_title.
**<<<------"171563 - NMS - FIM------>>>
  IF p_c EQ abap_true.
    wa_header-kunnr = p_kunnr.
    "<<<------"187296 - NMS ------->>>
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
**<<<------"187199 - NMS - FIM------>>>
  ELSEIF p_f EQ abap_true.
    "<<<------"187296 - NMS ------->>>
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
**<<<------"187199 - NMS - FIM------>>>
  ENDIF.
**<<<------"187199 - NMS - INI------>>>
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
**<<<------"187199 - NMS - FIM------>>>
  wa_header-name1 = vg_name1.
  wa_header-city = city.
  wa_header-cep = cep.
*  wa_header-lzone = lzone.   "<<<------"187199 - NMS ------>>>

  PERFORM bloqueia_linhas.
  PERFORM busca_info_docs.
  PERFORM carrega_alv.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5000 INPUT.

  ctl_alv1->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM check_dados.
      PERFORM salvar_roteiros.
      PERFORM enviar_dados_safra.  "*-CS2025000249-04.07.2025-#181842-JT
**<<<------"187199 - NMS - INI------>>>
    WHEN 'EQUZON'. "Eqializa Zona.
* Equalizaçãop de Zona de Transporte Do BP para os Cliente e Fornecedor.
      PERFORM zf_equaliza_zona_transp.
**<<<------"187199 - NMS - FIM------>>>
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
        01 'NR_ROT'          'ZSDT0132' '12'   'X' ' '  'X'   ''   'L'   'Nro. Roteiro' '',
        02 'ROT_DESC'        'ZSDT0132' '80'   'X' ' '  ' '   ''   'L'   'Descrição Roteiro' '',
        03 'ROT_OBS'         'ZSDT0132' '100'  'X' ' '  ' '   ''   'L'   'Observação' '',
**<<<------"177861 - NMS - INI------>>>
*        03 'CITY1'           ''         '40'   'X' 'X'  ' '   'X'  ' '   'Município' '',
        03 'CITY1'           ''         '40'   'X' ' '  ' '   'X'  ' '   'Município' '',
**<<<------"177861 - NMS - FIM------>>>
        04 'UF'              'ZSDT0132' '3'    'X' ' '  ' '   'X'  ' '   'UF' '',
        05 'TEL_NUMBER'      ''         '30'   'X' ' '  ' '   ''   'L'   'Telefone      ' '',
        06 'LZONE'           'ZSDT0132' '10'   'X' 'X'  ' '   'X'  'L'   'Zona Transporte' '', "FF #143815
**<<<------"171563 - NMS - INI------>>>
*        07 'TEXTO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Roteiro' '',
*        08 'ANEXO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Anexo  ' '',
*        09 'STATUS'          'ZSDT0132' '6'    'X' 'X'  ' '   ''   'C'   'Status' ''.
        07 'ZLATITUDE'       'TZONE'    '14'   'X' ' '  ' '   ''   'R'   'Latitude' '',
        08 'ZLONGITUDE'      'TZONE'    '15'   'X' ' '  ' '   ''   'R'   'Longitude' '',
        09 'Z_URL_LOCALIZACAO' 'TZONE'  '25'   'X' 'X'  ' '   ''   'L'   'Ender. URL Localiza' '',
        10 'TEXTO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Roteiro' '',
        11 'ANEXO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Anexo  ' '',
        12 'STATUS'          'ZSDT0132' '6'    'X' 'X'  ' '   ''   'C'   'Status' ''.
**<<<------"171563 - NMS - FIM------>>>
*        09 'ARMAZEM'         ''         '6'    'X' ' '  ' '   ''   'C'   'Armazem' 'x',
*        10 'TRANSPORTADORA'  ''         '6'    'X' ' '  ' '   ''   'C'   'Transportadora' ' ',
*        11 'TRANSP_RESP'     ''         '6'    'X' 'X'  ' '   ''   'C'   'Transp. Resp.' ''.
    ELSEIF p_f EQ abap_true.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog USING:
        01 'NR_ROT'          'ZSDT0132' '12'   'X' ' '  'X'   ''   'L'   'Nro. Roteiro' '',
        02 'LIFNR_ROT'       'ZSDT0132' '18'   'X' ' '  'X'   ''   'L'   'Cod.Fornecedor' '',  "*-US190444-10.09.2025-#190444-JT-inicio
        03 'ROT_DESC'        'ZSDT0132' '60'   'X' ' '  ' '   'X'  'L'   'Nome para o Ponto de Coleta' '',
**<<<------"177861 - NMS - INI------>>>
*        03 'CITY1'           ' '        '40'   'X' 'X'  ' '   'X'  ' '   'Município' '',
        04 'ENDERECO'        ' '        '40'   'X' ' '  ' '   'X'  ' '   'Endereço' '',
        05 'CITY1'           ' '        '40'   'X' ' '  ' '   'X'  ' '   'Município' '',
**<<<------"177861 - NMS - FIM------>>>
        06 'UF'              'ZSDT0132' '3'    'X' 'X'  ' '   'X'  ' '   'UF' '',
        07 'MARCA'           'ZSDT0132' '40'   'X' 'X'  ' '   'X'  ' '   'Marca' '',
* Projetos Insumos 2025 - 16.06.25 - Inicio
        08 'LZONE'           'ZSDT0132' '10'   'X' 'X'  ' '   'X'  'L'   'Zona Transporte' '', "FF #143815
        09 'ZLATITUDE'       'TZONE'    '14'   'X' ' '  ' '   ''   'R'   'Latitude' '',
        10 'ZLONGITUDE'      'TZONE'    '15'   'X' ' '  ' '   ''   'R'   'Longitude' '',
        11 'Z_URL_LOCALIZACAO' 'TZONE'  '25'   'X' 'X'  ' '   ''   'L'   'Ender. URL Localiza' '',
* Projetos Insumos 2025 - 16.06.25 - Fim
        12 'TEXTO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Roteiro' '',
        13 'ANEXO'           ''         '6'    'X' ' '  ' '   ''   'C'   'Anexo  ' '',
        14 'STATUS'          'ZSDT0132' '6'    'X' 'X'  ' '   ''   'C'   'Status' '',
        15 'ARMAZEM'         ''         '6'    'X' ' '  ' '   ''   'C'   'Armazem' 'X',
        16 'TRANSPORTADORA'  ''         '6'    'X' ' '  ' '   ''   'C'   'Transportadora' ' ',
        17 'TRANSP_RESP'     ''         '6'    'X' 'X'  ' '   ''   'C'   'Transp. Resp.' ''.
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
    gs_layout-cwidth_opt = 'X'.

    PERFORM sort USING 'NR_ROT' CHANGING it_sort.
    PERFORM excluir_botoes.
    "PERFORM REGISTRAR_F4.

    CREATE OBJECT ctl_alv1
      EXPORTING
        i_parent = g_custom_container.           "ALV Lote

    CALL METHOD ctl_alv1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
     lcl_event_handler=>handle_double_click FOR ctl_alv1,   "FF #143815
     lcl_event_handler=>toolbar             FOR ctl_alv1,
     lcl_event_handler=>user_command        FOR ctl_alv1,
     lcl_event_handler=>handle_button_click FOR ctl_alv1,
     lcl_event_handler=>on_data_changed     FOR ctl_alv1,
     lcl_event_handler=>on_f4               FOR ctl_alv1.



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
**<<<------"171563 - NMS - INI------>>>
* Verifica se é a coluna Ender. URL para permitir letra minúscula.
  IF p_fieldname EQ 'Z_URL_LOCALIZACAO'.
    wa_fieldcatalog-lowercase = abap_on.

  ENDIF.
**<<<------"171563 - NMS - FIM------>>>
**<<<------"182100 - NMS - INI------>>>
  IF p_fieldname EQ 'MARCA'.
    wa_fieldcatalog-no_out = abap_on.

  ENDIF.
**<<<------"182100 - NMS - FIM------>>>
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
        el_celltab  TYPE lvc_s_styl, "<<<------"171563 - NMS------>>>
        wa_zsdt0132 TYPE ty_zsdt0132_alv.

  LOOP AT it_zsdt0132 INTO wa_zsdt0132.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_zsdt0132-cellstyles.
**<<<------"171563 - NMS - INI------>>>
    IF wa_zsdt0132-antig IS INITIAL.
      IF wa_zsdt0132-lzone IS INITIAL.
        el_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

      ELSE.
        IF sy-ucomm EQ 'SAVE'.
          el_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        ELSE.
          el_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

        ENDIF.

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
**<<<------"171563 - NMS - FIM------>>>
    "REFRESH IT_CELLTAB.
    PERFORM fill_celltab USING    wa_zsdt0132
                                  wa_zsdt0132-antig
                         CHANGING it_celltab.
    "CLEAR WA_ZSDT0132-CELLSTYLES.
    INSERT LINES OF it_celltab INTO TABLE wa_zsdt0132-cellstyles.
    MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX vl_cont.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab  USING    p_zsdt0132   TYPE ty_zsdt0132_alv  "*-US190444-10.09.2025-#190444-JT
                            p_antig
                   CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_antig EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  wa_celltab-fieldname = 'NR_ROT'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled. "Sempre disabled
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'ROT_DESC'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'ROT_OBS'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'TEL_NUMBER'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

  "FF inicio #143815
  wa_celltab-fieldname = 'LZONE'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  "FF fim

  IF p_c EQ abap_true.
**<<<------"177861 - NMS - INI------>>>
    wa_celltab-fieldname = 'ENDERECO'.
    wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_celltab INTO TABLE p_it_celltab.
**<<<------"177861 - NMS - FIM------>>>
    wa_celltab-fieldname = 'CITY1'.
    wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_celltab INTO TABLE p_it_celltab.
    wa_celltab-fieldname = 'UF'.
    wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_celltab INTO TABLE p_it_celltab.
  ELSE.
    wa_celltab-fieldname = 'LIFNR_ROT'.  "*-US190444-10.09.2025-#190444-JT-inicio
    wa_celltab-style = status.
    INSERT wa_celltab INTO TABLE p_it_celltab.
**<<<------"177861 - NMS - INI------>>>
    wa_celltab-fieldname = 'ENDERECO'.
    wa_celltab-style = status.
    INSERT wa_celltab INTO TABLE p_it_celltab.
**<<<------"177861 - NMS - FIM------>>>
    wa_celltab-fieldname = 'CITY1'.
*-US190444-10.09.2025-#190444-JT-inicio
*   wa_celltab-style = status.
    IF p_zsdt0132-lifnr_rot IS NOT INITIAL.
      wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled. "status.
    ENDIF.
    INSERT wa_celltab INTO TABLE p_it_celltab.
*-US190444-10.09.2025-#190444-JT-fim

    wa_celltab-fieldname = 'UF'.
*-US190444-10.09.2025-#190444-JT-inicio
*   wa_celltab-style = status.
    IF p_zsdt0132-lifnr_rot IS NOT INITIAL.
      wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled. "status.
    ENDIF.
    INSERT wa_celltab INTO TABLE p_it_celltab.
*-US190444-10.09.2025-#190444-JT-fim
  ENDIF.

  wa_celltab-style = status.     "*-US190444-10.09.2025-#190444-JT
  wa_celltab-fieldname = 'MARCA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'TEXTO'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_button.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'ANEXO'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_button.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'STATUS'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'ARMAZEM'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'TRANSPORTADORA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'TRANSP_RESP'.
  wa_celltab-style = status.
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

**<<<------"171563 - NMS - INI------>>>
  READ TABLE it_zsdt0132 TRANSPORTING NO FIELDS WITH KEY antig = abap_false.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE 'Não há dados para serem salvos!' TYPE 'W'.
    RETURN.

  ENDIF.
**<<<------"171563 - NMS - FIM------>>>
  LOOP AT it_zsdt0132 INTO wa_zsdt0132 WHERE antig NE abap_true.
    DATA(vl_tabix) = sy-tabix. "<<<------"171563 - NMS------>>>
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
                wa_zsdt0132-lzone TO el_zone2-zone1,
                vl_land1          TO el_zone2-land1.

          IF el_zone2 NE el_zone.
* Atualiza Zona de transporte
            DATA(tl_mesg) = NEW zcl_zona_transporte( )->atualiza_zona( EXPORTING i_tzone = el_zone2 ).

          ENDIF.

        ENDIF.

      ENDIF.
* Projeto Insumos 2025 - 16.06 - Inicio
    ELSEIF p_f EQ abap_true.
      wa_zsdt0132_aux-lifnr      = p_lifnr.
      SELECT SINGLE land1 INTO @DATA(vl_land2) FROM lfa1 WHERE lifnr EQ @p_lifnr.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * FROM tzone INTO @el_zone WHERE land1 EQ @vl_land2
                                                         AND zone1 EQ @wa_zsdt0132-lzone.

        IF sy-subrc IS INITIAL.
          DATA(el_zone3) = el_zone.
          CLEAR el_zone3.
          MOVE-CORRESPONDING wa_zsdt0132 TO el_zone3.

          MOVE: sy-mandt          TO el_zone3-mandt,
                vl_land1          TO el_zone3-land1,
                wa_zsdt0132-lzone TO el_zone3-zone1,
                vl_land2          TO el_zone3-land1.

          IF el_zone3 NE el_zone.
* Atualiza Zona de transporte
            DATA(tl_mesg3) = NEW zcl_zona_transporte( )->atualiza_zona( EXPORTING i_tzone = el_zone3 ).

          ENDIF.

        ENDIF.

      ENDIF.
* Projeto Insumos 2025 - 16.06 - Fim
    ENDIF.

    wa_zsdt0132_aux-lifnr_rot  = wa_zsdt0132-lifnr_rot. "*-US190444-10.09.2025-#190444-JT
    wa_zsdt0132_aux-rot_desc   = wa_zsdt0132-rot_desc.
    wa_zsdt0132_aux-tel_number = wa_zsdt0132-tel_number.
    wa_zsdt0132_aux-lzone      = wa_zsdt0132-lzone.         "FF #143815
    wa_zsdt0132_aux-rot_obs    = wa_zsdt0132-rot_obs.
    wa_zsdt0132_aux-endereco   = wa_zsdt0132-endereco.      "<<<------"177861 NMS------>>>
    wa_zsdt0132_aux-city1      = wa_zsdt0132-city1.
    wa_zsdt0132_aux-uf         = wa_zsdt0132-uf.
    wa_zsdt0132_aux-marca      = wa_zsdt0132-marca.
    wa_zsdt0132_aux-status     = wa_zsdt0132-status.
    wa_zsdt0132_aux-id_propriedade     = wa_zsdt0132-id_propriedade. "// WBARBOSA BUG-185648 18/07/25 AGRIQ

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
**<<<------"171563 - NMS - INI------>>>
*    MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX sy-tabix.
    MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX vl_tabix.
    CLEAR: wa_zsdt0132_aux.
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

*-CS2025000249-04.07.2025-#181842-JT-inicio
FORM enviar_dados_safra.

  DATA: lv_parceiro TYPE kna1-kunnr.

  IF     p_kunnr IS NOT INITIAL.
    lv_parceiro = p_kunnr.
  ELSEIF p_lifnr IS NOT INITIAL.
    lv_parceiro = p_lifnr.
  ENDIF.

  CHECK lv_parceiro IS NOT INITIAL.

  SORT it_zsdt0132_safra BY nr_rot.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0132_safra COMPARING nr_rot.

  LOOP AT it_zsdt0132_safra INTO DATA(_zsdt0132).

    "*-US190444-10.09.2025-#190444-JT-inicio
*   DO 3 TIMES.
    DATA(l_task_cust) = 'INTEGRAR_CUSTOMER' && _zsdt0132-nr_rot.

    CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS' STARTING NEW TASK l_task_cust
      EXPORTING
        i_nr_rot        = _zsdt0132-nr_rot
        i_somente_rota  = abap_true
      EXCEPTIONS
        erro_integracao = 1
        OTHERS          = 2.

*     IF sy-subrc = 0.
*       EXIT.
*     ELSE.
*       WAIT UP TO 3 SECONDS.
*     ENDIF.
*   ENDDO.

  ENDLOOP.

ENDFORM.
*-CS2025000249-04.07.2025-#181842-JT-fim

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
  DATA: wl_name  TYPE thead-tdname,             "**<<<------"177861 - PQ - ----->>>
        it_texto TYPE STANDARD TABLE OF tline.  "**<<<------"177861 - PQ - ----->>>

  DATA: wa_zsdt0132 TYPE ty_zsdt0132_alv,
        wa_twspr    TYPE twspr.
**<<<------"171563 - NMS - INI------>>>
  DATA: vl_oblig     TYPE c,
        vl_armaz     TYPE c,
**<<<------"182100 - NMS - INI------>>>
*        vl_marca TYPE c.
        vl_zlatit_in TYPE spo_value,
        vl_zlongi_in TYPE spo_value,
        vl_text      TYPE spo_value.
**<<<------"182100 - NMS - FIM------>>>
**<<<------"171563 - NMS - FIM------>>>
  LOOP AT it_zsdt0132 INTO wa_zsdt0132 WHERE antig NE abap_true.
**<<<------"171563 - NMS - INI------>>>
    DATA(vl_tabix) = sy-tabix.
    DATA(vl_tbxax) = sy-tabix.
    CLEAR vl_tbxax.
**<<<------"171563 - NMS - FIM------>>>
    IF p_c EQ abap_true.
**<<<------"171563 - NMS - INI------>>>
      IF NOT wa_zsdt0132-lzone IS INITIAL.
* Busca o país do Cliente.
        SELECT SINGLE land1 INTO @DATA(vl_land1) FROM kna1 WHERE kunnr EQ @p_kunnr.
**<<<------"182100 - NMS - INI------>>>
** Verifica se a Zona de Transporte está sendo usada por outro Roteiro.
*        SELECT lzone, nr_rot, kunnr FROM zsdt0132 AS a
*          INNER JOIN tzone AS b
*           ON a~lzone EQ b~zone1
*          INTO TABLE @DATA(tl_0132_aux)
*        WHERE lzone EQ @wa_zsdt0132-lzone
*          AND land1 EQ @vl_land1.
*
*        IF sy-subrc IS INITIAL.
*          READ TABLE tl_0132_aux INTO DATA(el_0132_aux) WITH KEY nr_rot = wa_zsdt0132-nr_rot.
*
*          IF NOT sy-subrc IS INITIAL.
*            READ TABLE tl_0132_aux INTO el_0132_aux WITH KEY lzone = wa_zsdt0132-lzone.
*            APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
** Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
*            <fs_cell>-col_id   = 7.
*            <fs_cell>-row_id   = vl_tabix.
*            DATA(vl_zone_used) = abap_on.
*            EXIT.
*
*          ENDIF.
*
*        ELSE.
*          LOOP AT it_zsdt0132 TRANSPORTING NO FIELDS WHERE lzone  EQ wa_zsdt0132-lzone
*                                                       AND nr_rot NE wa_zsdt0132-nr_rot.
*            APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
** Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
*            <fs_cell>-col_id = 7.
*            <fs_cell>-row_id = sy-tabix.
*            vl_zone_used = abap_on.
*            EXIT.
*
*          ENDLOOP.
*
*          IF NOT vl_zone_used IS INITIAL.
*            EXIT.
*
*          ENDIF.
*
*        ENDIF.
        DATA(vl_zone_used) = abap_off.
        DATA(vl_nr_rot)    = wa_zsdt0132-nr_rot.
        DATA(vl_idparc)    = wa_zsdt0132-kunnr.
        CLEAR: vl_nr_rot, vl_idparc.
* Verifica se a Zona de Transporte está sendo usada por outro Roteiro.
        PERFORM zf_check_zone_used TABLES   tl_cell
                                   USING    wa_zsdt0132
                                            vl_land1
                                            vl_tabix
                                   CHANGING vl_zone_used
                                            vl_nr_rot
                                            vl_idparc.

        IF NOT vl_zone_used IS INITIAL.
          EXIT.

        ENDIF.
**<<<------"182100 - NMS - FIM------>>>
        SELECT zone1 FROM tzone INTO TABLE @DATA(lt_zone) WHERE land1 EQ @vl_land1.

        IF sy-subrc IS INITIAL.
          READ TABLE lt_zone WITH KEY zone1 = wa_zsdt0132-lzone TRANSPORTING NO FIELDS.

          IF NOT sy-subrc IS INITIAL.
            LOOP AT wa_zsdt0132-cellstyles ASSIGNING FIELD-SYMBOL(<fs_cellstyles>) WHERE fieldname EQ 'ZLATITUDE'
                                                                                      OR fieldname EQ 'ZLONGITUDE'
                                                                                      OR fieldname EQ 'Z_URL_LOCALIZACAO'.

              <fs_cellstyles>-style = cl_gui_alv_grid=>mc_style_disabled.
              DATA(vl_zona) = abap_on.

              IF vl_tbxax NE vl_tabix.
                APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).   "<<<------"182100 ------->>>
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
                <fs_cell>-col_id = 7.
                <fs_cell>-row_id = vl_tbxax = vl_tabix.

              ENDIF.

            ENDLOOP.

            MODIFY it_zsdt0132 FROM wa_zsdt0132 INDEX vl_tabix.

          ENDIF.

        ENDIF.

      ENDIF.
**<<<------"171563 - NMS - FIM------>>>
      IF wa_zsdt0132-status     IS INITIAL OR
         wa_zsdt0132-rot_desc   IS INITIAL OR
         wa_zsdt0132-rot_obs    IS INITIAL OR "<<<------"171563 - NMS------>>>
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

      ELSE.
        IF (     wa_zsdt0132-zlatitude  IS INITIAL   AND
                 wa_zsdt0132-zlongitude IS INITIAL ) OR
           ( NOT wa_zsdt0132-zlatitude  IS INITIAL   AND
             NOT wa_zsdt0132-zlongitude IS INITIAL ).
* Do nothing.
        ELSE.
* Latitude
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 8.
          <fs_cell>-row_id = vl_tabix.
          DATA(vl_lati_longe) = abap_on.
* Longitude
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 9.
          <fs_cell>-row_id = vl_tabix.
          vl_lati_longe = abap_on.

        ENDIF.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.
      "      PERFORM F_VALIDA_TELEFONE USING WA_ZSDT0132-TEL_NUMBER.

    ELSEIF p_f EQ abap_true.

*-US190444-10.09.2025-#190444-JT-inicio
      IF wa_zsdt0132-lifnr_rot IS INITIAL.
        MESSAGE s024(sd) WITH 'Cod.Fornecedor Obrigatorio!' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ELSE.
        SELECT SINGLE lifnr
          INTO @DATA(_lifnr)
          FROM lfa1
         WHERE lifnr = @wa_zsdt0132-lifnr_rot.

        IF sy-subrc <> 0.
          MESSAGE s024(sd) WITH 'Cód.Fornecedor não Encontrado!' DISPLAY LIKE 'E'.
          IF sy-ucomm EQ 'SAVE'.
            CLEAR sy-ucomm.
          ENDIF.
          LEAVE SCREEN.
        ENDIF.
      ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

**<<<------"182100 - NMS - INI------>>>
* Busca o país do Fornecedor.
      SELECT SINGLE land1 INTO vl_land1 FROM lfa1 WHERE lifnr EQ p_lifnr.
      CLEAR: vl_zone_used, vl_nr_rot, vl_idparc.
* Verifica se a Zona de Transporte está sendo usada por outro Roteiro.
      PERFORM zf_check_zone_used TABLES   tl_cell
                                 USING    wa_zsdt0132
                                          vl_land1
                                          vl_tabix
                                 CHANGING vl_zone_used
                                          vl_nr_rot
                                          vl_idparc.

      IF NOT vl_zone_used IS INITIAL.
        EXIT.

      ENDIF.
**<<<------"182100 - NMS - FIM------>>>
      IF wa_zsdt0132-status   IS INITIAL OR
         wa_zsdt0132-lifnr_rot IS INITIAL OR "*-US190444-10.09.2025-#190444-JT
         wa_zsdt0132-rot_desc IS INITIAL OR
         wa_zsdt0132-endereco IS INITIAL OR "<<<------"177861 - NMS------>>>
         wa_zsdt0132-city1    IS INITIAL OR
**<<<------"182100 - NMS - INI------>>>
*         wa_zsdt0132-uf       IS INITIAL OR
*         wa_zsdt0132-marca    IS INITIAL.
         wa_zsdt0132-uf       IS INITIAL.
**<<<------"182100 - NMS - FIM------>>>
**<<<------"171563 - NMS - INI------>>>
*        MESSAGE TEXT-005 TYPE 'E'.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 2
                                                vl_tabix
                                                wa_zsdt0132-lifnr_rot
                                       CHANGING vl_oblig.

* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 3 "2 *-US190444-10.09.2025-#190444-JT
                                                vl_tabix
                                                wa_zsdt0132-rot_desc
                                       CHANGING vl_oblig.
**<<<------"177861 - NMS - INI------>>>
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 4 "3 *-US190444-10.09.2025-#190444-JT
                                                vl_tabix
                                                wa_zsdt0132-endereco
                                       CHANGING vl_oblig.
**<<<------"177861 - NMS - FIM------>>>
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 5 "4 *-US190444-10.09.2025-#190444-JT
                                                vl_tabix
                                                wa_zsdt0132-city1
                                       CHANGING vl_oblig.
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 6 "5 *-US190444-10.09.2025-#190444-JT
                                                vl_tabix
                                                wa_zsdt0132-uf
                                       CHANGING vl_oblig.
**<<<------"182100 - NMS - INI------>>>
** Verifica se o campo não está preenchido.
*        PERFORM zf_check_field_initial   TABLES tl_cell
*                                          USING 7 "6 *-US190444-10.09.2025-#190444-JT
*                                                vl_tabix
*                                                wa_zsdt0132-marca
*                                       CHANGING vl_oblig.
**<<<------"182100 - NMS - FIM------>>>
* Verifica se o campo não está preenchido.
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 13 "12       "<<<------"182100 - NMS ------>>>*-US190444-10.09.2025-#190444-JT
                                                vl_tabix
                                                wa_zsdt0132-status
                                       CHANGING vl_oblig.
**<<<------"171563 - NMS - FIM------>>>
      ENDIF.

**<<<------"182100 - NMS - INI------>>>
      IF   NOT wa_zsdt0132-lzone         IS INITIAL   AND
         ( wa_zsdt0132-zlatitude         IS INITIAL   OR
           wa_zsdt0132-zlongitude        IS INITIAL   OR
           wa_zsdt0132-z_url_localizacao IS INITIAL ).
* Latitude
        IF wa_zsdt0132-zlatitude IS INITIAL.
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 8. "7. *-US190444-10.09.2025-#190444-JT
          <fs_cell>-row_id = vl_tabix.

        ENDIF.
* Longitude
        IF wa_zsdt0132-zlongitude IS INITIAL.
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 9. "8. *-US190444-10.09.2025-#190444-JT
          <fs_cell>-row_id = vl_tabix.

        ENDIF.
* Ender. URL Localiza
        IF wa_zsdt0132-z_url_localizacao IS INITIAL.
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 10. "9. *-US190444-10.09.2025-#190444-JT
          <fs_cell>-row_id = vl_tabix.

        ENDIF.

        vl_lati_longe = abap_on.

      ELSE.
        vl_zlatit_in = wa_zsdt0132-zlatitude.
        vl_zlongi_in = wa_zsdt0132-zlongitude.
* Validar a condição de existência das cordenadas Latitude e Longitude decimais.
        PERFORM zf_cond_existe_lat_long_dec USING    vl_zlatit_in
                                                     vl_zlongi_in
                                                     sy-abcde(1)   "A - Ambos
                                            CHANGING wa_zsdt0132-zlatitude
                                                     wa_zsdt0132-zlongitude
                                                     vl_text.

        CLEAR: vl_zlatit_in, vl_zlongi_in.
        IF NOT vl_text IS INITIAL.
          vl_zlongi_in = abap_on.
* Latitude
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 8. "7. *-US190444-10.09.2025-#190444-JT
          <fs_cell>-row_id = vl_tabix.
* Longitude
          APPEND INITIAL LINE TO tl_cell ASSIGNING <fs_cell>.
* Carrega a posição da célula de linha e coluna que foi criticada para destacar na Grid do ALV.
          <fs_cell>-col_id = 9. "8. *-US190444-10.09.2025-#190444-JT
          <fs_cell>-row_id = vl_tabix.

        ENDIF.

      ENDIF.
**<<<------"182100 - NMS - FIM------>>>
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
                                            USING 13       "<<<------"182100 - NMS ------>>>
                                                  vl_tabix
                                                  wa_zsdt0132-armazem
                                         CHANGING vl_armaz.
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.
**<<<------"171563 - NMS - INI------>>>
**<<<------"182100 - NMS - INI------>>>
*        IF vl_armaz IS INITIAL.
*          CLEAR wa_twspr.
***<<<------"171563 - NMS - FIM------>>>
*          SELECT SINGLE *
*            FROM twspr
*            INTO wa_twspr
*            WHERE wrkst EQ wa_zsdt0132-marca.
***<<<------"171563 - NMS - INI------>>>
**          IF sy-subrc IS NOT INITIAL.
**            MESSAGE TEXT-009 TYPE 'E'.
**
**          ENDIF.
** Verifica se o campo não está preenchido.
*          PERFORM zf_check_field_initial   TABLES tl_cell
*                                            USING 6
*                                                  vl_tabix
*                                                  wa_twspr-wrkst
*                                         CHANGING vl_marca.
*
*        ENDIF.
**<<<------"182100 - NMS - FIM------>>>
      ENDIF.
**<<<------"171563 - NMS - FIM------>>>
**<<<------"177861 - PQ - inicio------>>>
      CLEAR: wl_name, it_texto.
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
        PERFORM zf_check_field_initial   TABLES tl_cell
                                          USING 11 "10      "<<<------"182100 - NMS ------>>> *-US190444-10.09.2025-#190444-JT
                                           vl_tabix
                                           it_texto
                                         CHANGING vl_oblig.
      ENDIF.
**<<<------"177861 - PQ - FIM------>>>
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
    ctl_alv1->refresh_table_display( ).
    CASE abap_on.
      WHEN p_c. "Cliente
        CASE abap_on.
          WHEN vl_oblig.     "Valida campos obrigatórios
* Preencher os Campos Obrigatórios (Descrição Roteiro / Observação / Telefone / Status).
**<<<------"182100 - NMS - INI------>>>
*            MESSAGE TEXT-011 TYPE 'E'.
            MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
**<<<------"182100 - NMS - FIM------>>>
          WHEN vl_zone_used. "Verifica se Zona de transporte já foi usada
**<<<------"182100 - NMS - INI------>>>
*            MESSAGE |Zona { wa_zsdt0132-lzone } já vinculada no Roteiro { wa_zsdt0132-nr_rot } do cliente { p_kunnr }. | TYPE 'E'.
            MESSAGE |Zona { wa_zsdt0132-lzone } já vinculada no Roteiro { vl_nr_rot } do cliente { vl_idparc }. | TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
**<<<------"182100 - NMS - FIM------>>>
          WHEN vl_lati_longe.
**<<<------"182100 - NMS - INI------>>>
*            MESSAGE |Os campos Latitude e Longitudo devem estar preenchidas ou em branco para salvar.| TYPE 'E'.
            MESSAGE |Os campos Latitude e Longitude devem estar preenchidas ou em branco para salvar.| TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
**<<<------"182100 - NMS - FIM------>>>
          WHEN OTHERS.
*         Do nothing
        ENDCASE.

      WHEN p_f. "Fornecedor
        CASE abap_on.
          WHEN vl_oblig. "Valida campos obrigatórios
* Preencher os Campos Obrigatórios (Nome / Municipio / UF / Marca / Roteiro / Status).
**<<<------"182100 - NMS - INI------>>>
*            MESSAGE TEXT-005 TYPE 'E'.
            MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
**<<<------"182100 - NMS - FIM------>>>
          WHEN vl_armaz. "Valida Armazem
* Local de Embarque só pode ser "Transportadora" se também for "Armazém"!
**<<<------"182100 - NMS - INI------>>>
*            MESSAGE TEXT-010 TYPE 'E'.
            MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
*          WHEN vl_marca. "Valida Marca
** Marca não existe
*            MESSAGE TEXT-009 TYPE 'E'.
          WHEN vl_zone_used. "Verifica se Zona de transporte já foi usada
*            IF wa_zsdt0132-lzone IS NOT INITIAL AND wa_zsdt0132-status NE 'I'.
*              MESSAGE |Zona { wa_zsdt0132-lzone } já vinculada no Roteiro { vl_nr_rot } do cliente { vl_idparc }. | TYPE 'S' DISPLAY LIKE 'E'.
*              IF sy-ucomm EQ 'SAVE'.
*                CLEAR sy-ucomm.
*              ENDIF.
*              LEAVE SCREEN.
*            ENDIF.

          WHEN vl_lati_longe.
            MESSAGE |Os campos Latitude, Longitude e Ender. URL Localiza devem estar preenchidos.| TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.

          WHEN vl_zlongi_in(1).
            MESSAGE vl_text TYPE 'S' DISPLAY LIKE 'E'.
            IF sy-ucomm EQ 'SAVE'.
              CLEAR sy-ucomm.

            ENDIF.

            LEAVE SCREEN.
**<<<------"182100 - NMS - FIM------>>>
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
**<<<------"177861 - NMS - INI------>>>
*      WHEN 'CITY1'.
*
*        wa_fmap-fldname = 'ORT01'.
*        wa_fmap-dyfldname = 'Município'.
*        APPEND wa_fmap TO it_fmap.
*
*        CASE abap_true.
*          WHEN p_c.
*            SELECT *
*              FROM kna1
*              WHERE kunnr = @wa_header-kunnr
*              INTO TABLE @it_kna1.
*
*            LOOP AT it_kna1 INTO wa_kna1.
*
*              wa_f4_5420_cidade-cep = wa_kna1-pstlz.
*              wa_f4_5420_cidade-uf = wa_kna1-regio.
*              wa_f4_5420_cidade-cidade = wa_kna1-ort01.
*              APPEND wa_f4_5420_cidade TO it_f4_5420_cidade.
*              CLEAR: wa_f4_5420_cidade.
*            ENDLOOP.
*
*          WHEN p_f.
*
**            SELECT *
**            FROM LFA1
**            WHERE LIFNR = @WA_HEADER-KUNNR
**            INTO TABLE @IT_LFA1.
**
**            LOOP AT IT_LFA1 INTO WA_LFA1.
**
**              WA_F4_5420_CIDADE-CEP = WA_LFA1-PSTLZ.
**              WA_F4_5420_CIDADE-UF = WA_LFA1-REGIO.
**              WA_F4_5420_CIDADE-CIDADE = WA_LFA1-ORT01.
**              APPEND WA_F4_5420_CIDADE TO IT_F4_5420_CIDADE.
**              CLEAR: WA_F4_5420_CIDADE.
**
**            ENDLOOP.
*
*
*        ENDCASE.
*
*        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*          EXPORTING
*            retfield        = 'ORT01'
*            window_title    = 'Lista de Pedidos'(002)
*            value_org       = 'S'
*            dynprofield     = 'ORT01'
*          TABLES
*            value_tab       = it_f4_5420_cidade
*            return_tab      = it_ret_5420
*            dynpfld_mapping = it_fmap
*          EXCEPTIONS
*            parameter_error = 1
*            no_values_found = 2
*            OTHERS          = 3.
*
*        IF sy-subrc = 0.
*          ASSIGN er_event_data->m_data->* TO <itab>.
*          READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
*          wa_modi-row_id   = es_row_no-row_id.
*          wa_modi-fieldname = 'CITY1'.
*          wa_modi-value     = wa_ret-fieldval.
*          APPEND wa_modi TO <itab>.
*          READ TABLE it_f4_5420_cidade INTO DATA(uf) INDEX 1.
*          wa_modi-row_id   = es_row_no-row_id.
*          wa_modi-fieldname = 'UF'.
*          wa_modi-value     = uf-uf.
*          APPEND wa_modi TO <itab>.
*        ELSE.
*          ASSIGN er_event_data->m_data->* TO <itab>.
*          READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
*          wa_modi-row_id   = es_row_no-row_id.
*          wa_modi-fieldname = 'CITY1'.
*          wa_modi-value     = ' '.
*          APPEND wa_modi TO <itab>.
*          "READ TABLE IT_F4_5420_CIDADE INTO data(UF_limpo) INDEX 1.
*          wa_modi-row_id   = es_row_no-row_id.
*          wa_modi-fieldname = 'UF'.
*          wa_modi-value     = ' '.
*          APPEND wa_modi TO <itab>.
*
*        ENDIF.
*
*        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
**<<<------"177861 - NMS - FIM------>>>
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

        wa_fmap-fldname   = 'Z_URL_LOCALIZACAO'.
        wa_fmap-dyfldname = 'Ender. URL'.
        APPEND wa_fmap TO it_fmap.
**<<<------"182100 - NMS - INI------>>>
        CASE abap_on.
          WHEN p_c. "Cliente
**<<<------"182100 - NMS - FIM------>>>
            SELECT a~land1, b~zone1, vtext, zlatitude, zlongitude, z_url_localizacao
              FROM kna1 AS a
              INNER JOIN tzont AS b
               ON a~land1 EQ b~land1
              INNER JOIN tzone AS c
                ON b~land1 EQ c~land1 AND
                   b~zone1 EQ c~zone1
              INTO TABLE @DATA(lt_zone)
            WHERE kunnr EQ @p_kunnr.
**<<<------"182100 - NMS - INI------>>>
          WHEN p_f. "Fornecedor
            SELECT a~land1 b~zone1 vtext zlatitude zlongitude z_url_localizacao
              FROM lfa1 AS a
              INNER JOIN tzont AS b
               ON a~land1 EQ b~land1
              INNER JOIN tzone AS c
                ON b~land1 EQ c~land1 AND
                   b~zone1 EQ c~zone1
              INTO TABLE lt_zone
            WHERE lifnr EQ p_lifnr.

          WHEN OTHERS.
* Do nothing
        ENDCASE.
**<<<------"182100 - NMS - FIM------>>>
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
**<<<------"171563 - NMS - INI------>>>
              APPEND wa_modi TO <itab>.

              READ TABLE lt_zone INTO DATA(el_zone) WITH KEY zone1 = wa_ret-fieldval(10).

              IF sy-subrc IS INITIAL.
* Latitude
                wa_modi-row_id    = es_row_no-row_id.
                wa_modi-fieldname = 'ZLATITUDE'.
                IF el_zone-zlatitude EQ '       0.000000' OR
                   el_zone-zlatitude EQ '0.000000'.
                  CLEAR el_zone-zlatitude.

                ENDIF.

                wa_modi-value     = el_zone-zlatitude.
                CONDENSE wa_modi-value NO-GAPS.
                APPEND wa_modi TO <itab>.
* Longitude
                wa_modi-row_id    = es_row_no-row_id.
                wa_modi-fieldname = 'ZLONGITUDE'.
                IF el_zone-zlongitude EQ '       0.000000' OR
                   el_zone-zlongitude EQ '0.000000'.
                  CLEAR el_zone-zlongitude.

                ENDIF.

                wa_modi-value     = el_zone-zlongitude.
                CONDENSE wa_modi-value NO-GAPS.
                APPEND wa_modi TO <itab>.
* Ender. URL
                wa_modi-row_id    = es_row_no-row_id.
                wa_modi-fieldname = 'Z_URL_LOCALIZACAO'.
                wa_modi-value     = el_zone-z_url_localizacao.
                CONDENSE wa_modi-value NO-GAPS.
                APPEND wa_modi TO <itab>.

              ENDIF.
**<<<------"171563 - NMS - FIM------>>>
            ENDIF.

*            APPEND wa_modi TO <itab>. "<<<------"171563 - NMS------>>>

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
FORM f_valida_telefone USING p_phone .

  DATA: regex   TYPE REF TO cl_abap_regex,

        matcher TYPE REF TO cl_abap_matcher,

        match   TYPE c LENGTH 1.

  CREATE OBJECT regex
    EXPORTING
      pattern = '([(])([0-9]){2}([)])(([0-9]){4}|([0-9]){5})([-])([0-9]){4}'.
  "'(([(])([0-9]){2}([)])([ ])((([0-9]){8}|([0-9]){9})|(([0-9]){4}|([0-9]){5})(([-])|([ ]))([0-9]){4}))|(([0-9]){2}([ ])((([0-9]){8}|([0-9]){9})|(([0-9]){4}|([0-9]){5})(([-])|([ ]))([0-9]){4}))'.

  matcher = regex->create_matcher( text = p_phone ).

  match = matcher->match( ).

  IF match IS INITIAL.

    p_phone =  ' '.

  ELSE.
    p_phone = 'X'.
    "TRANSLATE P_PHONE "USING '- '.

    "CONDENSE P_PHONE NO-GAPS.

  ENDIF.

ENDFORM.
**<<<------"171563 - NMS - INI------>>>
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
**<<<------"171563 - NMS - FIM------>>>
FORM f_popup_zona_transp USING p_lzone
**<<<------"171563 - NMS - INI------>>>
*                               p_kunnr.
                               p_kunnr
                               uv_lzone_empty.

  DATA: tl_bp TYPE cvis_ei_extern_t.

  DATA: el_bp TYPE cvis_ei_extern.

  DATA: vl_text1 TYPE text132.

  CONSTANTS: cl_task_update TYPE bus_ei_object_task VALUE 'U'.
**<<<------"171563 - NMS - FIM------>>>
  CHECK p_kunnr IS NOT INITIAL.

  DATA: lv_answer.
**<<<------"171563 - NMS - INI------>>>
*  DATA:
*
*    customerno    TYPE bapikna103-customer,
*    pi_salesorg   LIKE bapikna102-salesorg,
*    pi_distr_chan LIKE bapikna102-distr_chan,
*    pi_division   LIKE bapikna102-division,
*    lt_return     LIKE bapireturn1.
*
*
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
**<<<------"187199 - NMS - INI------>>>
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
  IF uv_lzone_empty IS INITIAL.
    CHECK sy-ucomm EQ 'EQUZON'. "Eqializa Zona.
    lv_answer = 'J'. "Sim

  ELSE.
* Zona de Transporte do cadastro do cliente em branco.
    vl_text1 = TEXT-012.
**<<<------"187199 - NMS - FIM------>>>
**<<<------"171563 - NMS - FIM------>>>
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
*       DEFAULTOPTION = 'Y'
**<<<------"171563 - NMS - INI------>>>
*       diagnosetext1 = TEXT-012
        diagnosetext1 = vl_text1
**<<<------"171563 - NMS - FIM------>>>
*       DIAGNOSETEXT2 = ' '
*       DIAGNOSETEXT3 = ' '
        textline1     = TEXT-013
        textline2     = TEXT-014
        titel         = TEXT-015
        start_column  = 25
        start_row     = 6
*       CANCEL_DISPLAY       = 'X'
      IMPORTING
        answer        = lv_answer.
**<<<------"187199 - NMS - INI------>>>
  ENDIF.
**<<<------"187199 - NMS - FIM------>>>
  IF lv_answer = 'J'. "Sim
**<<<------"171563 - NMS - INI------>>>
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

** Confirmação das mudanças
**<<<------"171563 - NMS - INI------>>>
*    IF sy-subrc = 0.
**<<<------"182100 - NMS - INI------>>>
    CASE abap_on.
      WHEN p_c. "Cliente
**<<<------"182100 - NMS - FIM------>>>
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
**<<<------"187199 - NMS - INI------>>>
*** Business Partner(BP) - Vendor
*** Header.
        el_bp-vendor-header-object_task                           = cl_task_update.
        el_bp-vendor-header-object_instance-lifnr                 = el_customer-supplier.
*** Central Data - Addresses
        el_bp-vendor-central_data-address-task                    = cl_task_update.
        el_bp-vendor-central_data-address-postal-data-transpzone  = p_lzone.
        el_bp-vendor-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187199 - NMS - FIM------>>>
**<<<------"182100 - NMS - INI------>>>
      WHEN p_f. "Fornecedor
*** Business Partner(BP) - Partner
        SELECT SINGLE * FROM i_businesspartnersupplier INTO @DATA(el_vendor) WHERE supplier = @p_lifnr.
*** GUID de um endereço de parceiro de negócios
        SELECT SINGLE b~address_guid
          FROM but021_fs AS a
          INNER JOIN but020 AS b
           ON a~addrnumber EQ b~addrnumber
           INTO @vl_aguid
        WHERE a~partner EQ @el_vendor-businesspartner
          AND adr_kind  EQ 'XXDEFAULT'.
***Header.
        el_bp-partner-header-object_task                  = cl_task_update.
        el_bp-partner-header-object_instance-bpartnerguid = el_vendor-businesspartneruuid.
** Addresses
        APPEND INITIAL LINE TO el_bp-partner-central_data-address-addresses ASSIGNING <fs_addresses>.
        <fs_addresses>-task                         = cl_task_update.
        <fs_addresses>-data_key-guid                = vl_aguid.
        <fs_addresses>-data-postal-data-transpzone  = p_lzone.
        <fs_addresses>-data-postal-datax-transpzone = abap_on.
*** Business Partner(BP) - Vendor
*** Header.
        el_bp-vendor-header-object_task                           = cl_task_update.
        el_bp-vendor-header-object_instance-lifnr                 = p_lifnr.
*** Central Data - Addresses
        el_bp-vendor-central_data-address-task                    = cl_task_update.
        el_bp-vendor-central_data-address-postal-data-transpzone  = p_lzone.
        el_bp-vendor-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187199 - NMS - INI------>>>
*** Business Partner(BP) - Customer
*** Header.
        el_bp-customer-header-object_task                           = cl_task_update.
        el_bp-customer-header-object_instance-kunnr                 = el_vendor-customer.
*** Central Data - Addresses
        el_bp-customer-central_data-address-task                    = cl_task_update.
        el_bp-customer-central_data-address-postal-data-transpzone  = p_lzone.
        el_bp-customer-central_data-address-postal-datax-transpzone = abap_on.
**<<<------"187199 - NMS - FIM------>>>
      WHEN OTHERS.
* Do nothing
    ENDCASE.
**<<<------"182100 - NMS - FIM------>>>
*** Master Data - Maintain Business Partners
    cl_md_bp_maintain=>validate_single( EXPORTING i_data        = el_bp
                                        IMPORTING et_return_map = DATA(tl_return_map)
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
**<<<------"187199 - NMS - INI------>>>
*    wa_header-lzone = lzone = p_lzone.
    wa_header-lzone = wa_header-fnzon = wa_header-bpzon = lzone = p_lzone.
**<<<------"187199 - NMS - FIM------>>>
    uv_lzone_empty  = sy-abcde+25(1). "Z
    MESSAGE |Zona de Transp. Cadastro do BP, Cliente e Fornecedor atualizado com sucesso. | TYPE 'S'.
**<<<------"171563 - NMS - FIM------>>>
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
**<<<------"171563 - NMS - INI------>>>
*    ENDIF.
  ELSE.
    COMMIT WORK.
**<<<------"171563 - NMS - FIM------>>>
  ENDIF.
**<<<------"171563 - NMS - INI------>>>
*  PERFORM busca_nome.
*  CALL SCREEN 5000.
**<<<------"171563 - NMS - FIM------>>>
ENDFORM.
**<<<------"187199 - NMS - INI------>>>
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

  DATA(lv_lzone_empty) = abap_off.

  PERFORM f_popup_zona_transp USING wa_header-bpzon
                                    vl_id_part
                                    lv_lzone_empty.

ENDFORM.
**<<<------"187199 - NMS - FIM------>>>
