
      types: begin of ty_ana01,
               arbpl  type crhd-arbpl,
               werks  type afru-werks,
               hrsper type p decimals 2,
               hrsapo type p decimals 2,
               hrsdis type p decimals 2,
               icon   type char15,
               arbid  type objektid,
               color  type lvc_t_scol,
             end of ty_ana01,

             begin of ty_ana02,
               nome(30)  type c,
               hrsper    type p decimals 2,
               hrsapo    type p decimals 2,
               hrsdis    type p decimals 2,
               ueberlast type p decimals 2,
               qtdhrdisp type p decimals 2,
               auart     type aufk-auart,
               tphr(12)  type c,
               pernr     type afru-pernr,
               color     type lvc_t_scol,
             end of ty_ana02,

             begin of ty_prep_ana03,
               nome(30)  type c,
               hrsper    type p decimals 2,
               hrsapo    type p decimals 2,
               hrsdis    type p decimals 2,
               ueberlast type p decimals 2,
               qtdhrdisp type p decimals 2,
               auart     type aufk-auart,
               tphr(12)  type c,
               pernr     type afru-pernr,
             end of ty_prep_ana03,

             begin of ty_ana03,
               tphr(12) type c,
               hrsper   type p decimals 2,
               hrsapo   type p decimals 2,
               hrsdis   type p decimals 2,
               color    type lvc_t_scol,
             end of ty_ana03.

      data:
        it_ana01      type standard table of ty_ana01 initial size 0,
        wa_ana01      type ty_ana01,
        it_ana02      type standard table of ty_ana02 initial size 0,
        wa_ana02      type ty_ana02,
        it_prep_ana03 type standard table of ty_prep_ana03 initial size 0,
        it_ana03      type standard table of ty_ana03 initial size 0,
        wa_ana03      type ty_ana03,
        o_alv01       type ref to cl_salv_table,
        o_alv02       type ref to cl_salv_table,
        o_alv03       type ref to cl_salv_table,
        dtstart(10)   type c,
        dtend(10)     type c.


      data: container_01 type ref to cl_gui_custom_container,
            container_02 type ref to cl_gui_custom_container,
            container_03 type ref to cl_gui_custom_container.

      class lcl_report definition deferred.
      data: lo_report type ref to lcl_report.
      class lcl_report definition.
        public section .
          methods:
            get_data_c1,
            generate_output1,
            generate_output2,
            generate_output3,
            on_link_click_c1 for event link_click of cl_salv_events_table importing row column,
            on_link_click_c2 for event link_click of cl_salv_events_table importing row column.
      endclass.
      class lcl_report implementation.

        method get_data_c1.

          free: it_ana01.
          move-corresponding it_reg to it_ana01.

        endmethod.

        method generate_output1.
          "DATA: o_alv    TYPE REF TO cl_salv_table.

          container_01 = new cl_gui_custom_container(
            parent         = cl_gui_container=>default_screen
            container_name = 'C1'
          ).

          data: lx_msg type ref to cx_salv_msg.


          try.
              cl_salv_table=>factory(
                exporting
                  r_container    = container_01
                  container_name = 'C1'
                importing
                  r_salv_table   = o_alv01
                changing
                  t_table        = it_ana01 ).
            catch cx_salv_msg into lx_msg.
          endtry.

**********************************************************************set_pf_status
          data: lo_functions type ref to cl_salv_functions_list.
          lo_functions = o_alv01->get_functions( ).
          lo_functions->set_all( abap_true ).
          "lo_functions->set_default( abap_true ).

*    TRY.
*        lo_functions->add_function( name     = 'GRAVAR'
*                                    icon     = '@2L@'
*                                    text     = 'Gravar'
*                                    tooltip  = 'Gravar em Tabela'
*                                    position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CATCH cx_root.
*
*    ENDTRY.
********************************************************************** Handler and Hotspot

          data: lo_cols_tab type ref to cl_salv_columns_table,
                lo_col_tab  type ref to cl_salv_column_table,
                lo_events   type ref to cl_salv_events_table.

          lo_cols_tab = o_alv01->get_columns( ).

          try.
              lo_cols_tab->set_color_column( 'COLOR' ).
            catch cx_salv_not_found.
          endtry.

          lo_events = o_alv01->get_event( ).

          try.
              lo_col_tab ?= lo_cols_tab->get_column( 'ARBPL' ).
              lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
            catch cx_salv_not_found.                    "#EC NO_HANDLER
          endtry.


          set handler lo_report->on_link_click_c1 for lo_events.


**********************************************************************Make Columns
          data: lo_column type ref to cl_salv_column.
          data: lo_cols type ref to cl_salv_columns.
          lo_cols = o_alv01->get_columns( ).
          "lo_cols->set_optimize( abap_false ).

          try.

              lo_column ?= lo_cols->get_column( 'ARBPL' ).
              lo_column->set_short_text( 'C.Trab' ).
              lo_column->set_medium_text( 'Centro Trabalho' ).
              lo_column->set_long_text( 'Centro Trabalho' ).
              lo_column->set_optimized( abap_false ).
              lo_column->set_alignment( if_salv_c_alignment=>centered ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'WERKS' ).
              lo_column->set_short_text( 'Centro' ).
              lo_column->set_medium_text( 'Centro' ).
              lo_column->set_long_text( 'Centro' ).
              lo_column->set_optimized( abap_false ).
              lo_column->set_alignment( if_salv_c_alignment=>centered ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSPER' ).
              lo_column->set_short_text( 'HrPlan' ).
              lo_column->set_medium_text( 'Hr Planejada' ).
              lo_column->set_long_text( 'Hr Planejada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSAPO' ).
              lo_column->set_short_text( 'HrApont' ).
              lo_column->set_medium_text( 'Qtd Hr Apontada' ).
              lo_column->set_long_text( 'Qtd Hr Apontada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSDIS' ).
              lo_column->set_short_text( 'HrDisp' ).
              lo_column->set_medium_text( 'Qtd Hr Disponivel' ).
              lo_column->set_long_text( 'Qtd Hr Disponivel' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'ICON' ).
              lo_column->set_short_text( 'Status' ).
              lo_column->set_medium_text( 'Status' ).
              lo_column->set_long_text( 'Status' ).
              lo_column->set_optimized( abap_false ).
              lo_column->set_alignment( if_salv_c_alignment=>centered ).
              lo_column->set_output_length( '5' ).

              lo_column = lo_cols->get_column( 'ARBID' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).

              lo_column = lo_cols->get_column( 'COLOR' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).


            catch cx_salv_not_found.
          endtry.

          lo_cols->set_column_position( columnname = 'WERKS' position = 01 ).
          lo_cols->set_column_position( columnname = 'ARBPL' position = 02 ).
          lo_cols->set_column_position( columnname = 'HRSPER' position = 03 ).
          lo_cols->set_column_position( columnname = 'HRSAPO' position = 04 ).
          lo_cols->set_column_position( columnname = 'HRSDIS' position = 05 ).
          lo_cols->set_column_position( columnname = 'ICON' position = 06 ).


**********************************************************************
          data lr_display_settings  type ref to cl_salv_display_settings.
          data l_title              type lvc_title.
          l_title = |Centro de Trabalho|.
          lr_display_settings = o_alv01->get_display_settings( ).
          lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
          lr_display_settings->set_list_header( l_title ).
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

          "Enable Zebra Layout
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
          lr_selections = o_alv01->get_selections( ).
          lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

          o_alv01->display( ).

        endmethod.

        method generate_output2.
          "DATA: o_alv    TYPE REF TO cl_salv_table.
          container_02 = new cl_gui_custom_container(
            parent         = cl_gui_container=>default_screen
            container_name = 'C2'
          ).

          data: lx_msg type ref to cx_salv_msg.


          try.
              cl_salv_table=>factory(
                exporting
                  r_container    = container_02
                  container_name = 'C2'
                importing
                  r_salv_table   = o_alv02
                changing
                  t_table        = it_ana02 ).
            catch cx_salv_msg into lx_msg.
          endtry.

**********************************************************************set_pf_status
          data: lo_functions type ref to cl_salv_functions_list.
          lo_functions = o_alv02->get_functions( ).
          lo_functions->set_all( abap_true ).
          "lo_functions->set_default( abap_true ).

*    TRY.
*        lo_functions->add_function( name     = 'GRAVAR'
*                                    icon     = '@2L@'
*                                    text     = 'Gravar'
*                                    tooltip  = 'Gravar em Tabela'
*                                    position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CATCH cx_root.
*
*    ENDTRY.
********************************************************************** Handler and Hotspot

          data: lo_cols_tab type ref to cl_salv_columns_table,
                lo_col_tab  type ref to cl_salv_column_table,
                lo_events   type ref to cl_salv_events_table.

          lo_cols_tab = o_alv02->get_columns( ).

          try.
              lo_cols_tab->set_color_column( 'COLOR' ).
            catch cx_salv_not_found.
          endtry.

          lo_events = o_alv02->get_event( ).

          try.
              lo_col_tab ?= lo_cols_tab->get_column( 'NOME' ).
              lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
            catch cx_salv_not_found.                    "#EC NO_HANDLER
          endtry.


          set handler lo_report->on_link_click_c2 for lo_events.

**********************************************************************Make Columns
          data: lo_column type ref to cl_salv_column.
          data: lo_cols type ref to cl_salv_columns.
          lo_cols = o_alv02->get_columns( ).
          "lo_cols->set_optimize( abap_false ).

          try.

              lo_column ?= lo_cols->get_column( 'NOME' ).
              lo_column->set_short_text( 'Nome' ).
              lo_column->set_medium_text( 'Nome' ).
              lo_column->set_long_text( 'Nome' ).
              lo_column->set_optimized( abap_false ).
              lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '30' ).

              lo_column ?= lo_cols->get_column( 'HRSPER' ).
              lo_column->set_short_text( 'HrPlan' ).
              lo_column->set_medium_text( 'Hr Planejada' ).
              lo_column->set_long_text( 'Hr Planejada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSAPO' ).
              lo_column->set_short_text( 'HrApont' ).
              lo_column->set_medium_text( 'Hr Apontada' ).
              lo_column->set_long_text( 'Hr Apontada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSDIS' ).
              lo_column->set_short_text( 'HrDisp' ).
              lo_column->set_medium_text( 'Hr Disponivel' ).
              lo_column->set_long_text( 'Hr Disponivel' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).


              lo_column = lo_cols->get_column( 'UEBERLAST' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).
              lo_column = lo_cols->get_column( 'QTDHRDISP' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).
              lo_column = lo_cols->get_column( 'AUART' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).
              lo_column = lo_cols->get_column( 'TPHR' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).
              lo_column = lo_cols->get_column( 'PERNR' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).
              lo_column = lo_cols->get_column( 'COLOR' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).


            catch cx_salv_not_found.
          endtry.

          lo_cols->set_column_position( columnname = 'NOME' position = 01 ).
          lo_cols->set_column_position( columnname = 'HRSPER' position = 02 ).
          lo_cols->set_column_position( columnname = 'HRSAPO' position = 03 ).
          lo_cols->set_column_position( columnname = 'HRSDIS' position = 04 ).


**********************************************************************
          data lr_display_settings  type ref to cl_salv_display_settings.
          data l_title              type lvc_title.
          l_title = |Funcionários|.
          lr_display_settings = o_alv02->get_display_settings( ).
          lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
          lr_display_settings->set_list_header( l_title ).
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

          "Enable Zebra Layout
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
          lr_selections = o_alv02->get_selections( ).
          lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

          o_alv02->display( ).
        endmethod.

        method generate_output3.
          "DATA: o_alv    TYPE REF TO cl_salv_table.
          container_03 = new cl_gui_custom_container(
            parent         = cl_gui_container=>default_screen
            container_name = 'C3'
          ).

          data: lx_msg type ref to cx_salv_msg.


          try.
              cl_salv_table=>factory(
                exporting
                  r_container    = container_03
                  container_name = 'C3'
                importing
                  r_salv_table   = o_alv03
                changing
                  t_table        = it_ana03 ).
            catch cx_salv_msg into lx_msg.
          endtry.

**********************************************************************set_pf_status
          data: lo_functions type ref to cl_salv_functions_list.
          lo_functions = o_alv03->get_functions( ).
          lo_functions->set_all( abap_FALSE ).
          "lo_functions->set_default( abap_true ).

*    TRY.
*        lo_functions->add_function( name     = 'GRAVAR'
*                                    icon     = '@2L@'
*                                    text     = 'Gravar'
*                                    tooltip  = 'Gravar em Tabela'
*                                    position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CATCH cx_root.
*
*    ENDTRY.


          data: lo_cols_tab type ref to cl_salv_columns_table,
                lo_col_tab  type ref to cl_salv_column_table,
                lo_events   type ref to cl_salv_events_table.

          lo_cols_tab = o_alv03->get_columns( ).

          try.
              lo_cols_tab->set_color_column( 'COLOR' ).
            catch cx_salv_not_found.
          endtry.

*          lo_events = o_alv03->get_event( ).
*
*          TRY.
*              lo_col_tab ?= lo_cols_tab->get_column( 'ARBPL' ).
*              lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
*            CATCH cx_salv_not_found.                    "#EC NO_HANDLER
*          ENDTRY.
*
*
*          SET HANDLER lo_report->on_link_click_c1 FOR lo_events.


**********************************************************************Make Columns


          data: lo_column type ref to cl_salv_column.
          data: lo_cols type ref to cl_salv_columns.
          lo_cols = o_alv03->get_columns( ).
          "lo_cols->set_optimize( abap_false ).

          try.

**            lo_column = lo_cols->get_column( 'BASE_DIFAL_SAP' ).
**            lo_column->set_visible( if_salv_c_bool_sap=>false ).

              lo_column ?= lo_cols->get_column( 'TPHR' ).
              lo_column->set_short_text( 'Tipo' ).
              lo_column->set_medium_text( 'Tipo' ).
              lo_column->set_long_text( 'Tipo' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '12' ).

              lo_column ?= lo_cols->get_column( 'HRSPER' ).
              lo_column->set_short_text( 'HrPlan' ).
              lo_column->set_medium_text( 'Hr Planejada' ).
              lo_column->set_long_text( 'Hr Planejada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSAPO' ).
              lo_column->set_short_text( 'HrApont' ).
              lo_column->set_medium_text( 'Hr Apontada' ).
              lo_column->set_long_text( 'Hr Apontada' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column ?= lo_cols->get_column( 'HRSDIS' ).
              lo_column->set_short_text( 'HrDisp' ).
              lo_column->set_medium_text( 'Hr Disponivel' ).
              lo_column->set_long_text( 'Hr Disponivel' ).
              lo_column->set_optimized( abap_false ).
              "lo_column->set_alignment( if_salv_c_alignment=>left ).
              lo_column->set_output_length( '6' ).

              lo_column = lo_cols->get_column( 'COLOR' ).
              lo_column->set_visible( if_salv_c_bool_sap=>false ).

            catch cx_salv_not_found.
          endtry.

          lo_cols->set_column_position( columnname = 'TPHR' position = 01 ).
          lo_cols->set_column_position( columnname = 'HRSPER' position = 02 ).
          lo_cols->set_column_position( columnname = 'HRSAPO' position = 03 ).
          lo_cols->set_column_position( columnname = 'HRSDIS' position = 04 ).


**********************************************************************
          data lr_display_settings  type ref to cl_salv_display_settings.
          data l_title              type lvc_title.
          l_title = |Apontamentos|.
          lr_display_settings = o_alv03->get_display_settings( ).
          lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
          lr_display_settings->set_list_header( l_title ).
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

          "Enable Zebra Layout
          lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
          data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
          lr_selections = o_alv03->get_selections( ).
          lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

          o_alv03->display( ).
        endmethod.

        method on_link_click_c1.

          if column = 'ARBPL'.

            free: it_ana02, it_ana03,it_prep_ana03, it_saida_3, it_saida_4.
            clear: wa_ana01,wa_ana02,wa_ana03.

            read table it_ana01 index row into wa_ana01.
            data: ls_color               type lvc_s_scol.

            loop at it_ana01 assigning field-symbol(<fs_ana01>).
              if sy-tabix = row.
                ls_color-color-col = col_total.
                ls_color-color-int = 0.
                ls_color-color-inv = 0.
                append ls_color to <fs_ana01>-color.
                clear:ls_color.
              else.
                clear:<fs_ana01>-color.
              endif.
            endloop.

            o_alv01->refresh( ).
            o_alv02->refresh( ).
            o_alv03->refresh( ).


            read table it_reg into data(ls_reg) with key werks = wa_ana01-werks arbid = wa_ana01-arbid.

            call method lcl_dados=>generate_it_saida_3
              exporting
                i_werks = ls_reg-werks
                i_arbid = ls_reg-arbid.

            read table it_saida_3 into data(ls_saida_3) index 1.

            if sy-subrc = 0.

              read table it_crhd into data(ls_crhd) with key arbpl = ls_saida_3-arbpl werks = ls_saida_3-werks objid = ls_saida_3-arbid.

              read table it_kako into data(ls_kako) with key kapid = ls_crhd-kapid werks = ls_crhd-werks.

              call method lcl_dados=>generate_it_saida_4
                exporting
                  i_werks = ls_saida_3-werks
                  i_arbid = ls_saida_3-arbid.

              data: _ueberlast type p decimals 2.
              clear: wa_ana02.
              free: it_ana02.

              sort it_saida_4 by pernr ascending.

              data: _it_ana02 type standard table of ty_ana02 initial size 0.
              free: _it_ana02.
              clear: wa_ana02.

              loop at it_saida_4 into data(ls_saida_4).
                wa_ana02-pernr = ls_saida_4-pernr.
                wa_ana02-auart = ls_saida_4-auart.
                if ls_saida_4-auart+0(2) = 'HR'.
                  wa_ana02-tphr = 'PRODUTIVA'.
                else.
                  wa_ana02-tphr = 'IMPRODUTIVA'.
                endif.

                wa_ana02-nome = ls_saida_4-sname.
                wa_ana02-hrsapo = ls_saida_4-ismnw.

*               >>>>> Inicio ajuste referente IR209395 / AOENNING
                if ls_reg-hrsper > 0 and ls_kako-aznor > 0.
                  wa_ana02-hrsper = ( ls_reg-hrsper / ls_kako-aznor ).
                endif.


                clear:_ueberlast.
                if ls_kako-ueberlast > 0.
                  _ueberlast = ls_kako-ueberlast.
                  wa_ana02-ueberlast = _ueberlast / 100.
                endif.

                if wa_ana02-hrsapo > 0 and   wa_ana02-ueberlast > 0.
                  wa_ana02-qtdhrdisp = wa_ana02-hrsapo * wa_ana02-ueberlast.
                endif.
*                >>>>> Fim ajuste referente IR209395 / AOENNING


                wa_ana02-hrsdis = wa_ana02-hrsper - wa_ana02-hrsapo.
                append wa_ana02 to _it_ana02.
                clear: wa_ana02.
              endloop.

              sort _it_ana02 by  nome tphr ascending.

              data: hrapo     type p decimals 2,
                    sum_hrapo type p decimals 2,
                    hrdis     type p decimals 2,
                    sum_hrdis type p decimals 2,
                    qtd_lines type i.
              clear: hrapo,sum_hrapo,hrdis,sum_hrdis,qtd_lines,wa_ana02.
              loop at _it_ana02 assigning field-symbol(<fs_ana02_gp>) group by ( pernr = <fs_ana02_gp>-pernr ).
                wa_ana02-pernr = <fs_ana02_gp>-pernr.
                clear:qtd_lines.
                loop at _it_ana02 assigning field-symbol(<fs_ana02>) where pernr = <fs_ana02_gp>-pernr.
                  clear: hrapo,hrdis.
                  hrapo = <fs_ana02>-hrsapo.
                  sum_hrapo = hrapo + sum_hrapo.
                  hrdis = <fs_ana02>-hrsdis.
                  sum_hrdis = hrdis + sum_hrdis.
                  qtd_lines = qtd_lines + 1.
                endloop.
                wa_ana02-nome = <fs_ana02_gp>-nome.
                wa_ana02-hrsper = <fs_ana02_gp>-hrsper.
                wa_ana02-hrsapo = sum_hrapo.
                wa_ana02-hrsdis = wa_ana02-hrsper - sum_hrapo."sum_hrdis / qtd_lines.
                append wa_ana02 to it_ana02.
                clear: hrapo,sum_hrapo,hrdis,sum_hrdis,qtd_lines,wa_ana02,wa_ana02.
              endloop.

              o_alv02->refresh( ).


              move-corresponding _it_ana02 to it_prep_ana03.

              free: _it_ana02.

            endif.

          endif.
        endmethod.

        method on_link_click_c2.

          if column = 'NOME'.

            data: hrapo     type p decimals 2,
                  sum_hrapo type p decimals 2.
            free: it_ana03.
            clear: hrapo,sum_hrapo,wa_ana03.

            read table it_ana02 index row into data(ls_ana02).

            data: ls_color               type lvc_s_scol.

            loop at it_ana02 assigning field-symbol(<fs_ana02>).
              if sy-tabix = row.
                ls_color-color-col = col_total.
                ls_color-color-int = 0.
                ls_color-color-inv = 0.
                append ls_color to <fs_ana02>-color.
                clear:ls_color.
              else.
                clear:<fs_ana02>-color.
              endif.
            endloop.

            o_alv02->refresh( ).

            sort it_prep_ana03 by pernr ascending tphr ascending.
            clear:ls_color.
            loop at it_prep_ana03 assigning field-symbol(<fs_prep_ana03_gp>) where pernr = ls_ana02-pernr
              group by ( pernr = <fs_prep_ana03_gp>-pernr tphr = <fs_prep_ana03_gp>-tphr ).
              clear: sum_hrapo.
              loop at it_prep_ana03 assigning field-symbol(<fs_prep_ana03>) where pernr = <fs_prep_ana03_gp>-pernr and tphr = <fs_prep_ana03_gp>-tphr.
                clear: hrapo.
                hrapo = <fs_prep_ana03>-hrsapo.
                sum_hrapo = hrapo + sum_hrapo.
              endloop.
              wa_ana03-tphr = <fs_prep_ana03>-tphr.
              wa_ana03-hrsper = <fs_prep_ana03>-hrsper.
              clear: wa_ana03-hrsapo.
              move sum_hrapo to wa_ana03-hrsapo.
              wa_ana03-hrsdis = wa_ana03-hrsper - wa_ana03-hrsapo."sum_hrdis / qtd_lines.
              append wa_ana03 to it_ana03.
              clear: hrapo,sum_hrapo,wa_ana03,wa_ana03.
            endloop.

            loop at it_ana03 assigning field-symbol(<fs_ana03>).
              if <fs_ana03>-tphr = 'PRODUTIVA'.
                ls_color-color-col = col_positive.
                ls_color-color-int = 0.
                ls_color-color-inv = 0.
                append ls_color to <fs_ana03>-color.
              elseif <fs_ana03>-tphr = 'IMPRODUTIVA'.
                ls_color-color-col = col_negative.
                ls_color-color-int = 0.
                ls_color-color-inv = 0.
                append ls_color to <fs_ana03>-color.
              else.
                clear: <fs_ana03>-color.
              endif.
            endloop.
            o_alv03->refresh( ).
          endif.
          o_alv03->refresh( ).
        endmethod.

      endclass.
