
    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container.
    CLASS lcl_report DEFINITION DEFERRED.
    DATA: lo_report TYPE REF TO lcl_report.

    CLASS lcl_listener DEFINITION.
      PUBLIC SECTION.
        INTERFACES :
          if_salv_gui_om_edit_strct_lstr.
    ENDCLASS.

    CLASS lcl_listener IMPLEMENTATION.
      METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.
        o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified) ).
      ENDMETHOD.
    ENDCLASS.


    CLASS lcl_report DEFINITION.
      PUBLIC SECTION .
        METHODS:
          get_data,
          generate_output,
          set_HANDLER
            CHANGING co_alV    TYPE REF TO cl_salv_table
                     co_report TYPE REF TO lcl_report,
          set_refresh
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_columns_build
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_pf_status
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_layout
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_edit_alv
            CHANGING
              co_alV TYPE REF TO cl_salv_table,
          on_toolbar
            FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
              e_object
              e_interactive
              sender.

        METHODS: on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
          on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
          on_change_data FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    ENDCLASS.
    CLASS lcl_report IMPLEMENTATION.
      METHOD set_refresh.
        lo_report->get_data( ).
        co_alv->refresh( ).
      ENDMETHOD.

      METHOD on_change_data.

        DATA: it_changed TYPE STANDARD TABLE OF zfise46 INITIAL SIZE 0.
        "CLEAR: it_changed,wa_saida.

        DATA(inserted_tab) = er_data_changed->mt_inserted_rows.
        DATA(deleted_tab)  = er_data_changed->mt_deleted_rows.

        FIELD-SYMBOLS: <itab>        TYPE ANY TABLE,
                       <struct>      TYPE any,
                       <it_mod_rows> TYPE ANY TABLE,
                       <wa_mod_rows> TYPE any.


        DATA: lt_good_cells TYPE lvc_t_modi,
              ls_good_cell  TYPE lvc_s_modi.


        ASSIGN er_data_changed->mp_mod_rows->* TO <it_mod_rows>.
        MOVE-CORRESPONDING <it_mod_rows> TO it_changed.

        lt_good_cells = er_data_changed->mt_good_cells.

      ENDMETHOD.

      METHOD set_columns_build .
*
*...Get all the Columns
        DATA: lo_cols   TYPE REF TO cl_salv_columns,
              lo_column TYPE REF TO cl_salv_column.

        lo_cols = co_alv->get_columns( ).
        lo_cols->set_optimize( abap_false ).


        DATA: lo_cols_ref  TYPE        salv_t_column_ref,
              lo_cols_list TYPE REF TO cl_salv_column_list,
              lo_col_list  LIKE LINE OF lo_cols_ref.

        lo_cols = o_alv->get_columns( ).
        "lo_cols_c1->set_optimize( abap_false ).
        DATA: ls_ddic_f4_ref TYPE salv_s_ddic_reference.

        lo_cols_ref    = lo_cols->get( ).

        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CLEAR: ls_ddic_f4_ref.
              CASE lo_col_list-columnname.
                WHEN 'BUKRS' .
                  lo_cols_list->set_short_text( 'Empresa' ).
                  lo_cols_list->set_medium_text( 'Empresa' ).
                  lo_cols_list->set_long_text( 'Empresa' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'WERKS'.
                  lo_cols_list->set_short_text( 'Filial' ).
                  lo_cols_list->set_medium_text( 'Filial' ).
                  lo_cols_list->set_long_text( 'Filial' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'BUTXT'.
                  lo_cols_list->set_short_text( 'Nm.Empresa' ).
                  lo_cols_list->set_medium_text( 'Nome Empresa' ).
                  lo_cols_list->set_long_text( 'Nome Empresa' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '20' ).
                WHEN 'NAME1'.
                  lo_cols_list->set_short_text( 'Nm.Filial' ).
                  lo_cols_list->set_medium_text( 'Nome da Filial' ).
                  lo_cols_list->set_long_text( 'Nome da Filial' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '20' ).
                WHEN 'PERIODO'.
                  lo_cols_list->set_short_text( 'Mês\Ano' ).
                  lo_cols_list->set_medium_text( 'Mês\Ano' ).
                  lo_cols_list->set_long_text( 'Mês\Ano' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                  lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
                WHEN 'SEQITEM' .
                  lo_cols_list->set_short_text( 'Seq.' ).
                  lo_cols_list->set_medium_text( 'Seq.' ).
                  lo_cols_list->set_long_text( 'Seq.' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '04' ).
                WHEN 'STATUS_LANC'.
                  lo_cols_list->set_short_text( 'Status' ).
                  lo_cols_list->set_medium_text( 'Status' ).
                  lo_cols_list->set_long_text( 'Status' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  lo_cols_list->set_output_length( '06' ).
                  lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
                WHEN 'LOTE'.
                  lo_cols_list->set_short_text( 'Lote' ).
                  lo_cols_list->set_medium_text( 'Lote' ).
                  lo_cols_list->set_long_text( 'Lote' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'DT_DOC'.
                  lo_cols_list->set_short_text( 'Dt.Doc.' ).
                  lo_cols_list->set_medium_text( 'Data Doc.' ).
                  lo_cols_list->set_long_text( 'Data Documento' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'DT_LANC'.
                  lo_cols_list->set_short_text( 'Dt.Lanc.' ).
                  lo_cols_list->set_medium_text( 'Data Lanc.' ).
                  lo_cols_list->set_long_text( 'Data Lançamento' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'SAKNR'.
                  lo_cols_list->set_short_text( 'Ct.Razão' ).
                  lo_cols_list->set_medium_text( 'Conta Razão' ).
                  lo_cols_list->set_long_text( 'Conta Razão' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                  ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0219'  field = 'SAKNR').
                  lo_cols_list->set_ddic_reference( value = ls_ddic_f4_ref ).
                  lo_cols_list->set_f4( if_salv_c_bool_sap=>true ).
                WHEN 'TXT50'.
                  lo_cols_list->set_short_text( 'DescRazão ' ).
                  lo_cols_list->set_medium_text( 'Descrição Razão' ).
                  lo_cols_list->set_long_text( 'Descrição Razão' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'KOSTL'.
                  lo_cols_list->set_short_text( 'Cent.Cust.' ).
                  lo_cols_list->set_medium_text( 'Centro Custo' ).
                  lo_cols_list->set_long_text( 'Centro de Custo' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                  ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0218'  field = 'KOSTL').
                  lo_cols_list->set_ddic_reference( value = ls_ddic_f4_ref ).
                  lo_cols_list->set_f4( if_salv_c_bool_sap=>true ).
                WHEN 'AUFNR'.
                  lo_cols_list->set_short_text( 'Ordem' ).
                  lo_cols_list->set_medium_text( 'Ordem' ).
                  lo_cols_list->set_long_text( 'Ordem' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN'DESC_FORNEC' .
                  lo_cols_list->set_short_text( 'For.Desc.' ).
                  lo_cols_list->set_medium_text( 'Forne Descrição' ).
                  lo_cols_list->set_long_text( 'Fornecedor (Descrição)' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '25' ).

                WHEN'DESC_DESP_REC' .
                  lo_cols_list->set_short_text( 'Desc.D\C' ).
                  lo_cols_list->set_medium_text( 'Desc.Despesa\Receita' ).
                  lo_cols_list->set_long_text( 'Descrição Despesa\Receita' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '30' ).

                WHEN'NR_DOC' .
                  lo_cols_list->set_short_text( 'Nro.Doc.' ).
                  lo_cols_list->set_medium_text( 'Nro.Doc.' ).
                  lo_cols_list->set_long_text( 'Nro.Doc.' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).

                WHEN'DMBTR'.
                  lo_cols_list->set_short_text( 'Valor BRL' ).
                  lo_cols_list->set_medium_text( 'Valor BRL' ).
                  lo_cols_list->set_long_text( 'Valor BRL' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).

                WHEN'ESTORNO'.
                  lo_cols_list->set_short_text( 'Estornado' ).
                  lo_cols_list->set_medium_text( 'Estornado' ).
                  lo_cols_list->set_long_text( 'Estornado' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  lo_cols_list->set_output_length( '9' ).

                WHEN'D_C' .
                  lo_cols_list->set_short_text( 'D\C' ).
                  lo_cols_list->set_medium_text( 'D\C' ).
                  lo_cols_list->set_long_text( 'D\C' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  lo_cols_list->set_output_length( '3' ).
                  ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0217'  field = 'D_C').
                  lo_cols_list->set_ddic_reference( value = ls_ddic_f4_ref ).
                  lo_cols_list->set_f4( if_salv_c_bool_sap=>true ).

                WHEN 'DOC_CONT' .
                  lo_cols_list->set_short_text( 'Doc.Cont.').
                  lo_cols_list->set_medium_text( 'Doc.Contabil' ).
                  lo_cols_list->set_long_text( 'Doc.Contabil' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                  lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
                WHEN OTHERS.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              ENDCASE.
            ENDLOOP.
          CATCH cx_salv_not_found.
        ENDTRY.
        lo_cols->set_column_position( columnname ='BUKRS' position = 1 ).
        lo_cols->set_column_position( columnname ='BUTXT' position = 2 ).
        lo_cols->set_column_position( columnname ='WERKS' position = 3 ).
        lo_cols->set_column_position( columnname ='NAME1' position = 4 ).
        lo_cols->set_column_position( columnname ='PERIODO' position = 5 ).
        lo_cols->set_column_position( columnname ='STATUS' position = 6 ).
        lo_cols->set_column_position( columnname ='STATUS_LANC' position = 7 ).
        lo_cols->set_column_position( columnname ='DOC_CONT' position = 8 ).
        lo_cols->set_column_position( columnname ='SEQITEM' position = 9 ).
        lo_cols->set_column_position( columnname ='DT_DOC' position = 10 ).
        lo_cols->set_column_position( columnname ='DT_LANC' position = 11 ).
        lo_cols->set_column_position( columnname ='SAKNR' position = 12 ).
        lo_cols->set_column_position( columnname ='TXT50' position = 13 ).
        lo_cols->set_column_position( columnname ='D_C' position = 14 ).
        lo_cols->set_column_position( columnname ='KOSTL' position = 15 ).
        lo_cols->set_column_position( columnname ='AUFNR' position = 16 ).
        lo_cols->set_column_position( columnname ='DESC_FORNEC' position = 17 ).
        lo_cols->set_column_position( columnname ='DESC_DESP_REC' position = 18 ).
        lo_cols->set_column_position( columnname ='NR_DOC' position = 19 ).
        lo_cols->set_column_position( columnname ='DMBTR' position = 20 ).
        lo_cols->set_column_position( columnname ='ESTORNO' position = 21 ).
        lo_cols->set_column_position( columnname ='LOTE' position = 22 ).

      ENDMETHOD.

      METHOD set_edit_alv.
        DATA:ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
             ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.

        DATA: lv_ref_table  TYPE REF TO cl_abap_tabledescr,
              lv_ref_struct TYPE REF TO cl_abap_structdescr.

        ls_api = o_alv->extended_grid_api( ).
        ls_edit = ls_api->editable_restricted( ).
        lv_ref_table  ?= cl_abap_tabledescr=>describe_by_data( it_saida ).
        lv_ref_struct ?= lv_ref_table->get_table_line_type( ).
        DATA(lt_details)   = lv_ref_struct->components.

        LOOP AT lt_details ASSIGNING FIELD-SYMBOL(<_details>).
          ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname              = <_details>-name
              all_cells_input_enabled = abap_true
          ).

        ENDLOOP.

        DATA(mo_listener) = NEW lcl_listener( ).
        ls_edit->set_listener( mo_listener ).
        ls_edit->validate_changed_data(
    ).

        o_alv->refresh( ).
      ENDMETHOD.

      METHOD get_data.

        FREE: it_saida.
        CLEAR: wa_saida.

        SELECT
          a~bukrs,
          b~butxt,
          a~werks,
          c~name1,
          a~monat,
          a~gjahr,
          a~seqitem,
          a~dt_doc,
          a~dt_lanc,
          a~saknr,
          a~txt50,
          a~d_c,
          a~kostl,
          a~aufnr,
          a~desc_fornec,
          a~desc_desp_rec,
          a~nr_doc,
          a~dmbtr,
          a~estorno,
          a~lote,
          a~status,
          a~obj_key
        FROM
          zfit0217 AS a
          LEFT JOIN t001 AS b ON a~bukrs = b~bukrs
          LEFT JOIN t001w AS c ON a~werks = c~werks
                WHERE 1 = 1
          "AND a~bukrs IN @p_bukrs
          AND a~werks IN @p_werks
          AND a~gjahr IN @p_gjahr
          AND a~monat IN @p_monat
          INTO TABLE @DATA(it_sap).

        IF it_sap IS INITIAL.
          MESSAGE 'Não foram encontrados dados para esta selecão!' TYPE 'I'.
          SET SCREEN 0.
          LEAVE SCREEN.
          EXIT.
        ENDIF.

        LOOP AT it_sap ASSIGNING FIELD-SYMBOL(<_move_lanc>).
          MOVE-CORRESPONDING <_move_lanc> TO wa_saida.
          wa_saida-periodo = |{ <_move_lanc>-monat }/{ <_move_lanc>-gjahr }|.
          CASE <_move_lanc>-status.
            WHEN 'A'.
              wa_saida-status_LANC = '@S_TL_G@'.
              IF <_move_lanc>-obj_key IS NOT INITIAL.
                SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<_move_lanc>-obj_key INTO @DATA(l_belnr).
                IF sy-subrc = 0.
                  wa_saida-doc_cont = l_belnr.
                ELSE.
                  SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<_move_lanc>-obj_key INTO @DATA(l_err).
                  IF sy-subrc = 0.
                    wa_saida-status_LANC = '@S_TL_R@'.
                  ELSE.
                    wa_saida-status_LANC = '@AH@'.
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN 'L'.
              wa_saida-status_LANC = '@S_TL_Y@'.
            WHEN 'R'.
              wa_saida-status_LANC = '@S_NONO@'.
            WHEN OTHERS.
              wa_saida-status_LANC = '@OUTLIG@'.
          ENDCASE.
          APPEND wa_saida TO it_saida.
          CLEAR wa_saida.
        ENDLOOP.

        FREE:it_sap.
        CLEAR: it_sap, it_sap[],wa_saida.

      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        lo_functions->set_default( abap_true ).

        TRY.
            lo_functions->add_function( name     = 'RECLASSIFICAR_LANC'
                                        icon     = '@9A@'
                                        text     = 'Reclassificar Lançamento'
                                        tooltip  = 'Reclassificar Lançamento'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

            lo_functions->add_function( name     = 'ELIMINAR_LANC'
                                        icon     = '@02@'
                                        text     = 'Eliminar Lançamento'
                                        tooltip  = 'Eliminar Lançamento'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

          CATCH cx_root.

        ENDTRY.


      ENDMETHOD.

      METHOD on_user_command.

        DATA: lo_selections TYPE REF TO cl_salv_selections.
        DATA lt_rows TYPE salv_t_row.
        DATA lt_columns TYPE salv_t_column.
        DATA lt_cells TYPE salv_t_cell.
        DATA qtd_rows TYPE int4.

        FREE: lt_rows.
        CLEAR: qtd_rows.

        lo_selections = o_alv->get_selections( ).
        lt_rows = lo_selections->get_selected_rows( ).
        qtd_rows = lines( lt_rows ).


        CASE e_salv_function.

                WHEN 'RECLASSIFICAR_LANC'.

        IF qtd_rows > 0.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index1>).
            READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_recla_lanc>) INDEX <_index1>.
            IF <_recla_lanc>-status = '' OR <_recla_lanc>-status = 'L'.

              DATA:_dt  TYPE d,
                   _ndt TYPE d.
              CLEAR: _dt,_ndt.
              _dt = |{ <_recla_lanc>-gjahr }{ <_recla_lanc>-monat }01|.

              CALL FUNCTION 'HR_PSD_DATES_ADD_MONTHS'
                EXPORTING
                  v_date   = _dt
                  v_months = 1
                IMPORTING
                  e_date   = _ndt.
              IF sy-subrc = 0.
                DATA: _ano TYPE gjahr,
                      _mes TYPE monat.

                _ano = _ndt+0(4).
                _mes = _ndt+4(2).
                UPDATE zfit0217 SET lote = ''  gjahr = _ano monat = _mes
                WHERE bukrs = <_recla_lanc>-bukrs AND werks = <_recla_lanc>-werks AND monat = <_recla_lanc>-monat AND gjahr = <_recla_lanc>-gjahr AND seqitem = <_recla_lanc>-seqitem.
                COMMIT WORK.
              ENDIF.

            ENDIF.
          ENDLOOP.
          lo_report->get_data( ).
          CALL METHOD set_refresh CHANGING co_alv = o_alv.
        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

      WHEN 'ELIMINAR_LANC'.

        IF qtd_rows > 0.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index2>).
            READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del_lanc>) INDEX <_index2>.
            IF <_del_lanc>-status = '' OR <_del_lanc>-status = 'L'.
              DATA wa_zfit0217 TYPE zfit0217.
              CLEAR: wa_zfit0217.
              MOVE-CORRESPONDING <_del_lanc> TO wa_zfit0217.
              DELETE zfit0217 FROM wa_zfit0217.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
          lo_report->get_data( ).
          CALL METHOD set_refresh CHANGING co_alv = o_alv.
        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

        "CALL METHOD set_refresh CHANGING co_alv = o_alv.

          WHEN 'GRAVAR'.

            IF it_saida IS NOT INITIAL.
              LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_read>).
                "MODIFY zmmt0185 FROM <_read>.
              ENDLOOP.
            ELSE.
              MESSAGE 'Teste' TYPE 'I'.
            ENDIF.

          WHEN 'DELETE_ROW'.

            IF qtd_rows > 0.
              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
                READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
                "DELETE zmmt0185 FROM <_del>.
              ENDLOOP.

            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

            CALL METHOD set_refresh CHANGING co_alv = o_alv.

          WHEN 'REFRESH_ROW'.
            CALL METHOD set_refresh CHANGING co_alv = o_alv.
          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD on_toolbar.

        DATA : mt_toolbar TYPE stb_button.

        CLEAR mt_toolbar.
        mt_toolbar-butn_type = '3'.   "separator
        APPEND mt_toolbar TO e_object->mt_toolbar.

        LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
          "3 DESABILITA E 0 HABILITA
          IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
            <fs_tollbar>-butn_type = '3'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
            "<fs_tollbar>-butn_type = '3'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
            <fs_tollbar>-butn_type = '3'.
          ENDIF.
          IF <fs_tollbar>-function EQ '&REFRESH'.
            <fs_tollbar>-function = 'REFRESH_ROW'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
            <fs_tollbar>-function = 'DELETE_ROW'.
          ENDIF.
        ENDLOOP.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

      ENDMETHOD.

      METHOD generate_output.

        container_main = NEW cl_gui_custom_container(
          parent         = cl_gui_container=>default_screen
          "lifetime       =  cl_gui_container=>lifetime_dynpro
          container_name = 'CONTAINER'
        ).

*** Cria Splitter Container
*  CREATE OBJECT painel_control
*    EXPORTING
*      parent  = container_main
*      rows    = 1
*      columns = 1
*      align   = 70.
**
*** Exibe Painel 1
*  CALL METHOD painel_control->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = painel1.

        DATA: lx_msg TYPE REF TO cx_salv_msg.


        TRY.
            cl_salv_table=>factory(
              EXPORTING
                r_container    = container_main
                container_name = 'CONTAINER'
              IMPORTING
                r_salv_table   = o_alv
              CHANGING
                t_table        = it_saida ).
          CATCH cx_salv_msg INTO lx_msg.
        ENDTRY.

        CALL METHOD set_pf_status
          CHANGING
            co_alv = o_alv.

        CALL METHOD set_layout
          CHANGING
            co_alv = o_alv.

        CALL METHOD set_HANDLER
          CHANGING
            co_alv    = o_alv
            co_report = lo_report.

        CALL METHOD me->set_columns_build
          CHANGING
            co_alv = o_alv.

        DATA lr_selections        TYPE REF TO cl_salv_selections.
* Enable cell selection mode
        lr_selections = o_alv->get_selections( ).
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        o_alv->display( ).

*    CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      CHANGING
*        co_alv = o_alv.


      ENDMETHOD.

      METHOD set_HANDLER.
*
*...HotSpot
        DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
              lo_col_tab  TYPE REF TO cl_salv_column_table,
              lo_events   TYPE REF TO cl_salv_events_table.

        lo_cols_tab = co_alv->get_columns( ).
        lo_events = co_alv->get_event( ).

*   event handler
        SET HANDLER co_report->on_link_click FOR lo_events.
        SET HANDLER co_report->on_user_command FOR lo_events.
        SET HANDLER co_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.
        SET HANDLER co_report->on_change_data FOR ALL INSTANCES ACTIVATION 'X'.
        "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
      ENDMETHOD.

      METHOD on_link_click.

*    DATA: wa_saida TYPE zpme0087.
*    CLEAR: wa_saida.
*
*    IF column = ''.
*      "READ TABLE lo_report->it_saida INTO wa_saida INDEX row.
*
*    ENDIF.

      ENDMETHOD.


      METHOD set_layout.
*
        DATA: lo_layout  TYPE REF TO cl_salv_layout,
              lf_variant TYPE slis_vari,
              ls_key     TYPE salv_s_layout_key.
*   get layout object
        lo_layout = co_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
        ls_key-report = sy-repid.
        lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
        lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
        lf_variant = 'DEFAULT'.
        lo_layout->set_initial_layout( lf_variant ).
      ENDMETHOD.

    ENDCLASS.
