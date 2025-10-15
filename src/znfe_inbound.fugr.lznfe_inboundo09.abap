*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO09.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_1800 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

"US INICIO #96438 RJF
MODULE status_1800 OUTPUT.

  TYPES: BEGIN OF ty_grid,
           tipo_info TYPE char30,
           conteudo  TYPE string,
         END OF ty_grid.

  DATA: it_grid TYPE TABLE OF ty_grid,
        wa_grid TYPE ty_grid.

  CASE sy-ucomm.
    WHEN 'FATURA_DOC'.
      SET PF-STATUS 'PF1801' EXCLUDING it_ucomm.
      SET TITLEBAR 'TL1800'.

      IF ( editor_18001 IS INITIAL ).

        CREATE OBJECT container_18001
          EXPORTING
            container_name              = 'LONGTEXT1'
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.

        CHECK sy-subrc IS INITIAL.

        CREATE OBJECT editor_18001
          EXPORTING
            parent                 = container_18001
            wordwrap_mode          = '2'
            wordwrap_position      = '132'
          EXCEPTIONS
            error_cntl_create      = 1
            error_cntl_init        = 2
            error_cntl_link        = 3
            error_dp_create        = 4
            gui_type_not_supported = 5
            OTHERS                 = 6.

        CHECK sy-subrc IS INITIAL.

        CLEAR: longtext_tab1.

        DATA: lv_string TYPE string.
        DATA: lv_stringc TYPE string.
        FREE: lv_string, lv_stringc.
        LOOP AT it_det ASSIGNING FIELD-SYMBOL(<fs_det>).
          lv_string = lv_string && <fs_det>-infadprod.
        ENDLOOP.

        CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
          EXPORTING
            i_string = lv_string
          TABLES
            et_table = tl_tlines1.

        LOOP AT tl_tlines1.
          longtext1 = tl_tlines1-tdline.
          APPEND longtext1 TO longtext_tab1.
        ENDLOOP.

        CALL METHOD editor_18001->set_text_as_r3table
          EXPORTING
            table           = longtext_tab1
          EXCEPTIONS
            error_dp        = 1
            error_dp_create = 2
            OTHERS          = 3.

        CALL METHOD editor_18001->set_readonly_mode
          EXPORTING
            readonly_mode = editor_18001->true.

      ENDIF.

      IF ( editor_18002 IS INITIAL ).

        CREATE OBJECT container_18002
          EXPORTING
            container_name              = 'LONGTEXT2'
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.

        CHECK sy-subrc IS INITIAL.

        CREATE OBJECT editor_18002
          EXPORTING
            parent                 = container_18002
            wordwrap_mode          = '2'
            wordwrap_position      = '132'
          EXCEPTIONS
            error_cntl_create      = 1
            error_cntl_init        = 2
            error_cntl_link        = 3
            error_dp_create        = 4
            gui_type_not_supported = 5
            OTHERS                 = 6.

        CHECK sy-subrc IS INITIAL.

        CLEAR: longtext_tab2.

*lv_infcpl,
        IF lv_infcpl IS NOT INITIAL.
          CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
            EXPORTING
              i_string = lv_infcpl
            TABLES
              et_table = tl_tlines2.
        ENDIF.

        LOOP AT tl_tlines2.
          longtext2 = tl_tlines2-tdline.
          APPEND longtext2 TO longtext_tab2.
        ENDLOOP.

        CALL METHOD editor_18002->set_text_as_r3table
          EXPORTING
            table           = longtext_tab2
          EXCEPTIONS
            error_dp        = 1
            error_dp_create = 2
            OTHERS          = 3.

        CALL METHOD editor_18002->set_readonly_mode
          EXPORTING
            readonly_mode = editor_18002->true.

*        editor_18002->set_enable( abap_false ).

      ENDIF.


      IF ( editor_18003 IS INITIAL ).

        CREATE OBJECT container_18003
          EXPORTING
            container_name              = 'LONGTEXT3'
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.

        CHECK sy-subrc IS INITIAL.

        CREATE OBJECT editor_18003
          EXPORTING
            parent                 = container_18003
            wordwrap_mode          = '2'
            wordwrap_position      = '132'
          EXCEPTIONS
            error_cntl_create      = 1
            error_cntl_init        = 2
            error_cntl_link        = 3
            error_dp_create        = 4
            gui_type_not_supported = 5
            OTHERS                 = 6.

        CHECK sy-subrc IS INITIAL.

        CLEAR: longtext_tab3.

*lv_infadfisco.

        IF lv_infadfisco IS NOT INITIAL.
          CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
            EXPORTING
              i_string = lv_infadfisco
            TABLES
              et_table = tl_tlines3.
        ENDIF.

        LOOP AT tl_tlines3.
          longtext3 = tl_tlines3-tdline.
          APPEND longtext3 TO longtext_tab3.
        ENDLOOP.

        CALL METHOD editor_18003->set_text_as_r3table
          EXPORTING
            table           = longtext_tab3
          EXCEPTIONS
            error_dp        = 1
            error_dp_create = 2
            OTHERS          = 3.

        CALL METHOD editor_18003->set_readonly_mode
          EXPORTING
            readonly_mode = editor_18003->true.
*        editor_18003->set_enable( abap_false ).

      ENDIF.

      DATA:
        gs_layout1  TYPE lvc_s_layo,
        tl_function TYPE ui_functions,
        wl_function LIKE LINE OF tl_function,
        fcat        TYPE lvc_t_fcat.

*   // Aba Dados do Contrato
*    DATA(_fieldcatalog) = me->get_fieldcatalog( EXPORTING input = '1' ).
*--------------------------------------------------------

      CLEAR fcat[].

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'ZDE_GRID_ALV'
*         i_internal_tabname = 'IT_GRID'
        CHANGING
          ct_fieldcat      = fcat.

      LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fcat>).
        CASE <fcat>-fieldname.
          WHEN 'TIPO_INFO'.
            <fcat>-edit      = abap_off.
            <fcat>-coltext   = 'Tipo Informação'.
            <fcat>-col_pos   = 1.
            <fcat>-outputlen = 30.
          WHEN 'CONTEUDO'.
            <fcat>-edit      = abap_on.
            <fcat>-col_pos   = 2.
            <fcat>-coltext   = 'Conteúdo'.
            <fcat>-outputlen = 255.
          WHEN OTHERS.
            <fcat>-no_out = abap_true.
        ENDCASE.
      ENDLOOP.

      DATA(_fieldcatalog) = fcat.

*---------------------------------------------------------
      IF custom_grid1 IS NOT INITIAL.
        CALL METHOD grid1->free.
        CALL METHOD custom_grid1->free.
      ENDIF.

      CREATE OBJECT custom_grid1
        EXPORTING
          container_name = 'CC01'.

      CREATE OBJECT grid1
        EXPORTING
          i_parent = custom_grid1.

*    SET HANDLER: me->handle_set_toolbar  FOR grid1,
*                 me->handle_user_command FOR grid1,
*                 me->handle_data_changed FOR grid1,
*                 me->handle_data_changed_finished FOR grid1.

*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    APPEND wl_function TO tl_function.

      IF _fieldcatalog IS NOT INITIAL.

        SELECT * FROM dd07t
         INTO TABLE @DATA(it_dd07t)
          WHERE domname EQ 'ZDINFO_FER'
            AND ddlanguage EQ 'P'.
        IF sy-subrc IS INITIAL AND it_dd07t[] IS NOT INITIAL.

          SELECT * FROM zmmt0089
            INTO TABLE @DATA(it_zmmt0089)
            FOR ALL ENTRIES IN @it_dd07t
            WHERE tipo_tag EQ @it_dd07t-domvalue_l.

          DATA: lv_i  TYPE i,
                lv_s  TYPE i,
                lv_sp TYPE i,
                lv_ic TYPE i,
                lv_f  TYPE i.

          FREE: wa_grid, it_grid.
          lv_stringc = lv_string && lv_infcpl && lv_infadfisco.

          IF sy-subrc IS INITIAL.
            LOOP AT it_dd07t ASSIGNING FIELD-SYMBOL(<fs_dd07t>).
              wa_grid-tipo_info = <fs_dd07t>-ddtext.

* Conteúdo
              LOOP AT it_zmmt0089 ASSIGNING FIELD-SYMBOL(<fs_0089>) WHERE tipo_tag EQ <fs_dd07t>-domvalue_l.
                CLEAR: lv_i, lv_s, lv_sp, lv_ic, lv_f.

                IF <fs_0089>-tag IS NOT INITIAL.
                  FIND <fs_0089>-tag IN lv_stringc IGNORING CASE
                                                  MATCH OFFSET lv_i.
                  IF lv_i GT 0.
                    DATA(lv_stringm) = lv_stringc+lv_i.

*                FIND space IN lv_stringm IGNORING CASE
*                                         MATCH OFFSET lv_s.
                    IF lv_stringm CA ' '.
                      lv_s = sy-fdpos + 1.
                    ENDIF.

                    IF lv_stringm CA '|'.
                      lv_sp = sy-fdpos + 1.
                    ENDIF.

                    IF lv_sp LT lv_s AND lv_sp NE 0.
                      lv_s = lv_sp.
                    ENDIF.

                    IF lv_s GT 0.
                      DATA(lv_stringic) = lv_stringm+lv_s.

                      IF <fs_0089>-tag_fim IS NOT INITIAL.
                        FIND <fs_0089>-tag_fim IN lv_stringic IGNORING CASE
                                                             MATCH OFFSET lv_ic.
                      ELSE.
*                    FIND space IN lv_stringic IGNORING CASE
*                                           MATCH OFFSET lv_ic.
                        IF lv_stringic CA ' ' OR lv_stringm CA '|'.
                          lv_ic = sy-fdpos + 1.
                        ELSE.
                          CONTINUE.
                        ENDIF.

                      ENDIF.

                      IF lv_ic IS NOT INITIAL.
                        DATA(lv_stringict) = lv_stringic(lv_ic).

                      ENDIF.

                      IF wa_grid-conteudo IS INITIAL.
                        wa_grid-conteudo = wa_grid-conteudo && lv_stringict.
                      ELSEIF lv_stringict IS NOT INITIAL.
                        CLEAR wa_grid-conteudo.
                        wa_grid-conteudo = wa_grid-conteudo && lv_stringict.
                      ELSE.
                        wa_grid-conteudo = wa_grid-conteudo && '-' && lv_stringict.
                      ENDIF.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              APPEND wa_grid TO it_grid.
              CLEAR wa_grid.
            ENDLOOP.
          ENDIF.
        ENDIF.

      ENDIF.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
*         it_toolbar_excluding = tl_function
          is_layout       = gs_layout1
          i_save          = abap_true
        CHANGING
          it_outtab       = it_grid
          it_fieldcatalog = _fieldcatalog.

*  CALL METHOD grid1->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

*  CALL METHOD grid1->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CALL METHOD grid1->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

**   // Aba Pagamentos/Recebimentos
*    DATA(_fieldcatalog1) = me->get_fieldcatalog( ).
*    DATA(_sort)          = me->get_sort( ).

    WHEN 'SALVAR'.
*Gravar texto em objeto de texto utilizando chave da tabela ZSDT0138
*SAVE_TEXT
* FREE: t_observ, t_text.
      DATA: l_id        TYPE thead-tdid,
            l_object    TYPE thead-tdobject,
            l_name_text TYPE thead-tdname,
            lwa_thead   TYPE thead,
            t_text      TYPE TABLE OF tline,
            st_text     TYPE tline,
            t_observ    TYPE TABLE OF tline.

      IF zde_nfe_dist_alv-aut_embarque(10) IS NOT INITIAL.
        SELECT *
          UP TO 1 ROWS
                 INTO @DATA(wa_zsdt0138)
                 FROM zsdt0138
                WHERE nro_sol     = @zde_nfe_dist_alv-aut_embarque(10)
          ORDER BY seq_cam.
        ENDSELECT.

        IF sy-subrc IS INITIAL.

*          l_id     = 'Z001'.
*          l_object = 'ZOBSERVAC'.
          l_id     = 'ST'.
          l_object = 'TEXT'.
          CONCATENATE wa_zsdt0138-nro_sol wa_zsdt0138-seq_cam  wa_zsdt0138-seq
                      wa_zsdt0138-filial_resp
                 INTO l_name_text.

          REFRESH t_text.
          CLEAR: t_text, lwa_thead, st_text.

          lwa_thead-tdid     = l_id.
          lwa_thead-tdobject = l_object.
          lwa_thead-tdspras  = sy-langu.
          lwa_thead-tdname   = l_name_text.

          CLEAR lv_string.
          LOOP AT it_grid ASSIGNING FIELD-SYMBOL(<fs_grid>).
            IF lv_string IS INITIAL.
              lv_string = <fs_grid>-tipo_info && | { space }| && <fs_grid>-conteudo && | { space }|.
            ELSE.
              lv_string = lv_string && | { space }| && <fs_grid>-tipo_info && | { space }|  && <fs_grid>-conteudo && | { space }|.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
            EXPORTING
              i_string = lv_string
            TABLES
              et_table = t_text.
        ENDIF.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = l_id
            language                = sy-langu
            name                    = l_name_text
            object                  = l_object
          TABLES
            lines                   = t_observ
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF t_observ IS NOT INITIAL.

          CALL FUNCTION 'DELETE_TEXT'
            EXPORTING
              id        = l_id
              language  = sy-langu
              name      = l_name_text
              object    = l_object
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = lwa_thead
              savemode_direct = abap_true
            TABLES
              lines           = t_text
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.

          IF sy-subrc IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.

        ELSE.
          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = lwa_thead
              savemode_direct = abap_true
            TABLES
              lines           = t_text
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.

          IF sy-subrc IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
        ENDIF.
      ELSE.
        lv_error = abap_true.

      ENDIF.

      IF editor_18001 IS NOT INITIAL.
        editor_18001->free( ).
      ENDIF.
      CLEAR: editor_18001.

      IF container_18001 IS NOT INITIAL.
        container_18001->free( ).
      ENDIF.
      CLEAR: container_18001.

      IF editor_18002 IS NOT INITIAL.
        editor_18002->free( ).
      ENDIF.
      CLEAR: editor_18002.

      IF container_18002 IS NOT INITIAL.
        container_18002->free( ).
      ENDIF.
      CLEAR: container_18002.

      IF editor_18003 IS NOT INITIAL.
        editor_18003->free( ).
      ENDIF.
      CLEAR: editor_18003.

      IF container_18003 IS NOT INITIAL.
        container_18003->free( ).
      ENDIF.
      CLEAR: container_18003.

      FREE: tl_tlines1, longtext_tab1, longtext1,
            tl_tlines2, longtext_tab2, longtext2,
            tl_tlines3, longtext_tab3, longtext3,
            it_det, it_grid, lv_infcpl, lv_infadfisco.

      LEAVE TO SCREEN 0.

*      IF custom_grid1 IS NOT INITIAL.
*        custom_grid1->free( ).
*      ENDIF.
*      CLEAR: custom_grid1.
*
*      IF grid1 IS NOT INITIAL.
*        grid1->free( ).
*      ENDIF.
*      CLEAR: grid1.

    WHEN OTHERS.
      IF editor_18001 IS NOT INITIAL.
        editor_18001->free( ).
      ENDIF.
      CLEAR: editor_18001.

      IF container_18001 IS NOT INITIAL.
        container_18001->free( ).
      ENDIF.
      CLEAR: container_18001.

      IF editor_18002 IS NOT INITIAL.
        editor_18002->free( ).
      ENDIF.
      CLEAR: editor_18002.

      IF container_18002 IS NOT INITIAL.
        container_18002->free( ).
      ENDIF.
      CLEAR: container_18002.

      IF editor_18003 IS NOT INITIAL.
        editor_18003->free( ).
      ENDIF.
      CLEAR: editor_18003.

      IF container_18003 IS NOT INITIAL.
        container_18003->free( ).
      ENDIF.
      CLEAR: container_18003.

      FREE: tl_tlines1, longtext_tab1, longtext1,
            tl_tlines2, longtext_tab2, longtext2,
            tl_tlines3, longtext_tab3, longtext3,
            it_det, lv_infcpl, it_grid, lv_infadfisco.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1800 INPUT.

  CASE ok_code.
    WHEN 'SALVAR'.
      IF zde_nfe_dist_alv-aut_embarque(10) IS NOT INITIAL.
        SELECT *
          UP TO 1 ROWS
                 INTO @wa_zsdt0138
                 FROM zsdt0138
                WHERE nro_sol     = @zde_nfe_dist_alv-aut_embarque(10)
          ORDER BY seq_cam.
        ENDSELECT.

        IF sy-subrc IS INITIAL.

*          l_id     = 'Z001'.
*          l_object = 'ZOBSERVAC'.
          l_id     = 'ST'.
          l_object = 'TEXT'.
          CONCATENATE wa_zsdt0138-nro_sol wa_zsdt0138-seq_cam  wa_zsdt0138-seq
                      wa_zsdt0138-filial_resp
                 INTO l_name_text.

          REFRESH t_text.
          CLEAR: t_text, lwa_thead, st_text.

          lwa_thead-tdid     = l_id.
          lwa_thead-tdobject = l_object.
          lwa_thead-tdspras  = sy-langu.
          lwa_thead-tdname   = l_name_text.

*it_grid -> t_text

          CLEAR lv_string.
          LOOP AT it_grid ASSIGNING <fs_grid>.
* concatenar dois campos lv_string
            IF lv_string IS INITIAL.
              lv_string = <fs_grid>-tipo_info && <fs_grid>-conteudo.
            ELSE.
              lv_string = lv_string && <fs_grid>-tipo_info && <fs_grid>-conteudo.
            ENDIF.

          ENDLOOP.

          CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
            EXPORTING
              i_string = lv_string
            TABLES
              et_table = t_text.

        ENDIF.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = l_id
            language                = sy-langu
            name                    = l_name_text
            object                  = l_object
          TABLES
            lines                   = t_observ
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF t_observ IS NOT INITIAL.

** delete text
          CALL FUNCTION 'DELETE_TEXT'
            EXPORTING
              id        = l_id
              language  = sy-langu
              name      = l_name_text
              object    = l_object
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = lwa_thead
              savemode_direct = abap_true
            TABLES
              lines           = t_text
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.

        ELSE.
          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = lwa_thead
              savemode_direct = abap_true
            TABLES
              lines           = t_text
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.

        ENDIF.

        IF editor_18001 IS NOT INITIAL.
          editor_18001->free( ).
        ENDIF.
        CLEAR: editor_18001.

        IF container_18001 IS NOT INITIAL.
          container_18001->free( ).
        ENDIF.
        CLEAR: container.

        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      IF editor_18001 IS NOT INITIAL.
        editor_18001->free( ).
      ENDIF.
      CLEAR: editor_18001.

      IF container_18001 IS NOT INITIAL.
        container_18001->free( ).
      ENDIF.
      CLEAR: container.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
"U.S FIM #96438 RJF
