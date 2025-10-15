*&---------------------------------------------------------------------*
*& Report  ZSDR0109
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0109.

TABLES: zsdt0226.

TYPES: BEGIN OF ty_saida,
         emp_viagem      TYPE  zsdt0226-emp_viagem,
         centro_viagem   TYPE  zsdt0226-centro_viagem,
         po_embarque     TYPE  zsdt0226-po_embarque,
         po_destino      TYPE  zsdt0226-po_destino,
         emp_fat_serv    TYPE  zsdt0226-emp_fat_serv,
         centro_fat_serv TYPE  zsdt0226-centro_fat_serv,
         celltab         TYPE lvc_t_styl,
       END OF ty_saida.

DATA: it_saida    TYPE TABLE OF ty_saida,
      wa_saida    TYPE ty_saida,
      it_zsdt0226 TYPE TABLE OF zsdt0226,
      wa_zsdt0226 TYPE zsdt0226.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog    TYPE lvc_t_fcat,
      wa_fieldcatalog    TYPE lvc_s_fcat,
      tl_function        TYPE ui_functions,
      wl_function        LIKE tl_function  WITH HEADER LINE,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      gt_estilo          TYPE lvc_t_styl,
      wa_stable          TYPE lvc_s_stbl VALUE 'XX'.

DATA: tg_selectedrow TYPE lvc_t_row,
      wg_selectedrow TYPE lvc_s_row.


START-OF-SELECTION.

  PERFORM busca_dados.
  CALL SCREEN 0100.



FORM busca_dados.

  SELECT *
    FROM zsdt0226 INTO TABLE it_zsdt0226.


  LOOP AT it_zsdt0226 INTO wa_zsdt0226.

    wa_saida-emp_viagem       = wa_zsdt0226-emp_viagem.
    wa_saida-centro_viagem    = wa_zsdt0226-centro_viagem.
    wa_saida-po_embarque      = wa_zsdt0226-po_embarque.
    wa_saida-po_destino       = wa_zsdt0226-po_destino.
    wa_saida-emp_fat_serv     = wa_zsdt0226-emp_fat_serv.
    wa_saida-centro_fat_serv  = wa_zsdt0226-centro_fat_serv.

    FREE wa_saida-celltab.
    gt_estilo =  VALUE #( ( fieldname = 'EMP_VIAGEM'         style = cl_gui_alv_grid=>mc_style_disabled  )
                          ( fieldname = 'CENTRO_VIAGEM'      style = cl_gui_alv_grid=>mc_style_disabled  )
                          ( fieldname = 'PO_EMBARQUE'        style = cl_gui_alv_grid=>mc_style_disabled  )
                          ( fieldname = 'PO_DESTINO'         style = cl_gui_alv_grid=>mc_style_disabled  )
                          ( fieldname = 'EMP_FAT_SERV'       style = cl_gui_alv_grid=>mc_style_disabled  )
                          ( fieldname = 'CENTRO_FAT_SERV'    style = cl_gui_alv_grid=>mc_style_disabled  )  ).

    INSERT LINES OF gt_estilo INTO TABLE wa_saida-celltab.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida, wa_zsdt0226.
  ENDLOOP.

ENDFORM.

FORM alv.
  REFRESH it_fieldcatalog.
  PERFORM preenche_cat USING:
        'EMP_VIAGEM'               'Empresa Viagem'                 '11'     ''     ''     ''     ''   'X'   'T001'    'BUKRS'   '',
        'CENTRO_VIAGEM'            'Centro Viagem'                  '11'     ''     ''     ''     ''   'X'   'T001W'   'WERKS'   '',
        'PO_EMBARQUE'              'Porto Origem'                   '11'     ''     ''     ''     ''   'X'   'LFA1'    'LIFNR'   '',
        'PO_DESTINO'               'Porto Destino'                  '11'     ''     ''     ''     ''   'X'   'KNA1'    'KUNNR'   '',
        'EMP_FAT_SERV'             'Empresa Fat.Serv.'              '14'     ''     ''     ''     ''   'X'   'T001'    'BUKRS'   '',
        'CENTRO_FAT_SERV'          'Centro Fat.Serv.'               '14'     ''     ''     ''     ''   'X'   'T001W'   'WERKS'   ''.

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_edit)
                        VALUE(p_table)
                        VALUE(p_fieldname)
                        VALUE(p_f4).

  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.
  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-ref_table   = p_table.
  wa_fieldcatalog-ref_field   = p_fieldname.
  wa_fieldcatalog-f4availabl  = p_f4.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TL_0100'.

  PERFORM alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid IS INITIAL AND g_custom_container IS NOT  INITIAL.
      CREATE OBJECT g_grid
        EXPORTING
          i_parent          = g_custom_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    gs_layout-stylefname = 'CELLTAB'.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant
        is_layout            = gs_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_outtab            = it_saida
        it_fieldcatalog      = it_fieldcatalog.


    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: tsalvar TYPE TABLE OF zsdt0226,
        wsalvar TYPE zsdt0226.


  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVE'.

      LOOP AT it_saida INTO wa_saida.

        SELECT SINGLE *
          FROM zsdt0226 INTO @DATA(t0226)
         WHERE emp_viagem    EQ @wa_saida-emp_viagem
          AND  centro_viagem EQ @wa_saida-centro_viagem
          and  po_destino    EQ @wa_saida-po_destino
          and  po_embarque   EQ @wa_saida-po_embarque.

        IF sy-subrc <> 0.

          IF wa_saida-emp_viagem IS INITIAL.
            MESSAGE 'Empresa Viagem é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.

            SELECT SINGLE *
              FROM t001 INTO @DATA(wt001)
              WHERE bukrs EQ @wa_saida-emp_viagem.

            IF sy-subrc = 0.
              wsalvar-emp_viagem = wa_saida-emp_viagem.
            ELSE.
              MESSAGE 'Empresa informada não exite!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF wa_saida-centro_viagem IS INITIAL.
            MESSAGE 'Centro Viagem é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.

            SELECT SINGLE *
              FROM t001w INTO @DATA(wt001w)
              WHERE werks EQ @wa_saida-centro_viagem.

            IF sy-subrc = 0.
              wsalvar-centro_viagem = wa_saida-centro_viagem.
            ELSE.
              MESSAGE 'Centro informada não exite!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.


          IF wa_saida-po_embarque IS INITIAL.
            MESSAGE 'Porto Origem é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.

            SELECT SINGLE *
              FROM lfa1 INTO @DATA(wfa1)
              WHERE lifnr EQ @wa_saida-po_embarque.

            IF sy-subrc = 0.
              wsalvar-po_embarque = wa_saida-po_embarque.
            ELSE.
              MESSAGE 'Porto Origem  informada não exite!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.


          IF wa_saida-po_destino IS INITIAL.
            MESSAGE 'Porto Destino é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.
            SELECT SINGLE *
              FROM kna1 INTO @DATA(wkna1)
              WHERE kunnr EQ @wa_saida-po_destino.

            IF sy-subrc = 0.
              wsalvar-po_destino = wa_saida-po_destino.
            ELSE.
              MESSAGE 'Porto Destino informada não exite!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.


          IF wa_saida-emp_fat_serv IS INITIAL.
            MESSAGE 'Empresa Fat.Serv. é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.

            SELECT SINGLE *
              FROM t001 INTO wt001
              WHERE bukrs EQ wa_saida-emp_fat_serv.

            IF sy-subrc = 0.
              wsalvar-emp_fat_serv     = wa_saida-emp_fat_serv.
            ELSE.
              MESSAGE 'Empresa informada não exite!' TYPE 'I'                .
              EXIT.
            ENDIF.
          ENDIF.

          IF wa_saida-centro_fat_serv IS INITIAL.
            MESSAGE 'Centro Fat.Serv. é obrigatório!' TYPE 'S'.
            EXIT.
          ELSE.

            SELECT SINGLE *
              FROM t001w INTO wt001w
              WHERE werks EQ wa_saida-centro_fat_serv.

            IF sy-subrc = 0.
              wsalvar-centro_fat_serv  = wa_saida-centro_fat_serv.
            ELSE.
              MESSAGE 'Centro informada não exite!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF wsalvar-emp_viagem IS NOT INITIAL AND wsalvar-centro_viagem IS NOT INITIAL AND
             wsalvar-emp_fat_serv IS NOT INITIAL AND wsalvar-centro_fat_serv IS NOT INITIAL and
              wsalvar-po_destino is not INITIAL and  wsalvar-po_embarque is not INITIAL.
            APPEND wsalvar TO tsalvar.
          ENDIF.
        ENDIF.

        CLEAR wa_saida.
      ENDLOOP.

      CHECK tsalvar[] IS NOT INITIAL.

      MODIFY zsdt0226 FROM TABLE tsalvar.
      MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

      LOOP AT it_saida INTO wa_saida.
        FREE wa_saida-celltab.
        gt_estilo =  VALUE #( ( fieldname = 'EMP_VIAGEM'         style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'CENTRO_VIAGEM'      style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'PO_EMBARQUE'        style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'PO_DESTINO'         style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'EMP_FAT_SERV'       style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'CENTRO_FAT_SERV'    style = cl_gui_alv_grid=>mc_style_disabled  )   ).

        INSERT LINES OF gt_estilo INTO TABLE wa_saida-celltab.
        MODIFY it_saida FROM wa_saida.
      ENDLOOP.

    WHEN 'INS'.
      APPEND INITIAL LINE TO it_saida.

    WHEN 'DEL'.

      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow INTO wg_selectedrow.

          READ TABLE it_saida INTO wa_saida INDEX  wg_selectedrow-index.

          DELETE FROM zsdt0226  WHERE  emp_viagem     =  wa_saida-emp_viagem AND
                                       centro_viagem  =  wa_saida-centro_viagem.
          REFRESH it_saida[].
          CLEAR wa_saida.

          PERFORM busca_dados.
        ENDLOOP.
      ENDIF.
  ENDCASE.

ENDMODULE.
