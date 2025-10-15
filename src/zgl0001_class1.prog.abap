

    CLASS lcl_report1 DEFINITION DEFERRED.
    DATA: lo_report1 TYPE REF TO lcl_report1.

    CLASS lcl_listener1 DEFINITION.
      PUBLIC SECTION.
        INTERFACES :
          if_salv_gui_om_edit_strct_lstr.
    ENDCLASS.

    CLASS lcl_listener1 IMPLEMENTATION.
      METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.
        o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified) ).
      ENDMETHOD.
    ENDCLASS.


    CLASS lcl_report1 DEFINITION.
      PUBLIC SECTION .

        DATA: container_main TYPE REF TO cl_gui_custom_container,
              painel_control TYPE REF TO cl_gui_splitter_container,
              painel1        TYPE REF TO cl_gui_container,
              painel2        TYPE REF TO cl_gui_container.

        DATA: o_alv      TYPE REF TO cl_salv_table.
        METHODS:
          get_data,
          generate_output,
          set_HANDLER
            CHANGING co_alV    TYPE REF TO cl_salv_table
                     co_report TYPE REF TO lcl_report1,
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
    CLASS lcl_report1 IMPLEMENTATION.
      METHOD set_refresh.
        lo_report1->get_data( ).
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
*...Get all the Columns
        DATA: lo_cols        TYPE REF TO cl_salv_columns,
              lo_cols_ref    TYPE        salv_t_column_ref,
              lo_cols_list   TYPE REF TO cl_salv_column_list,
              lo_col_list    LIKE LINE OF lo_cols_ref,
              lo_column      TYPE REF TO cl_salv_column,
              ls_ddic_f4_ref TYPE salv_s_ddic_reference.

        lo_cols = co_alv->get_columns( ).
        "lo_cols->set_optimize( abap_false ).

        lo_cols_ref    = lo_cols->get( ).

        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list. "Esturura das Colunas como mostrar ou esconder etc...
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CLEAR: ls_ddic_f4_ref.
              CASE lo_col_list-columnname.
                WHEN 'BUKRS' .
                  lo_cols_list->set_short_text( 'Empresa' ).
                  lo_cols_list->set_medium_text( 'Empresa'  ).
                  lo_cols_list->set_long_text( 'Empresa'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'BRANCH' .
                  lo_cols_list->set_short_text( 'Filial' ).
                  lo_cols_list->set_medium_text( 'Filial'  ).
                  lo_cols_list->set_long_text( 'Filial'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'CFOP' .
                  lo_cols_list->set_short_text( 'CFOP' ).
                  lo_cols_list->set_medium_text( 'CFOP'  ).
                  lo_cols_list->set_long_text( 'CFOP'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'TP_M' .
                  lo_cols_list->set_short_text( 'Tp Mercado' ).
                  lo_cols_list->set_medium_text( 'Tipo Mercado'  ).
                  lo_cols_list->set_long_text( 'Tipo Mercado'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'NFENUM' .
                  lo_cols_list->set_short_text( 'Nr. Nota' ).
                  lo_cols_list->set_medium_text( 'Nr. Nota'  ).
                  lo_cols_list->set_long_text( 'Nr. Nota'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'DOCNUM' .
                  lo_cols_list->set_short_text( 'Nº Doc.' ).
                  lo_cols_list->set_medium_text( 'Nº Documento'  ).
                  lo_cols_list->set_long_text( 'Nº Documento'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'PARID' .
                  lo_cols_list->set_short_text( 'CodCliente' ).
                  lo_cols_list->set_medium_text( 'Cod. Cliente'  ).
                  lo_cols_list->set_long_text( 'Cod. Cliente'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'NAME1' .
                  lo_cols_list->set_short_text( 'Cliente' ).
                  lo_cols_list->set_medium_text( 'Cliente'  ).
                  lo_cols_list->set_long_text( 'Cliente'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'MATKL' .
                  lo_cols_list->set_short_text( 'Grp. Merc.' ).
                  lo_cols_list->set_medium_text( 'Grp. Mercadoria'  ).
                  lo_cols_list->set_long_text( 'Grp. Mercadoria'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'RASSC' .
                  lo_cols_list->set_short_text( 'Soc.Parc.' ).
                  lo_cols_list->set_medium_text( 'Sociedade Parceira'  ).
                  lo_cols_list->set_long_text( 'Sociedade Parceira'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SHPUNT' .
                  lo_cols_list->set_short_text( 'Un' ).
                  lo_cols_list->set_medium_text( 'Un'  ).
                  lo_cols_list->set_long_text( 'Un'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'MATNR' .
                  lo_cols_list->set_short_text( 'Produto' ).
                  lo_cols_list->set_medium_text( 'Produto'  ).
                  lo_cols_list->set_long_text( 'Produto'  ).
                  lo_cols_list->set_output_length( '20' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'MAKTX' .
                  lo_cols_list->set_short_text( 'DescrMat.' ).
                  lo_cols_list->set_medium_text( 'Descr. Material'  ).
                  lo_cols_list->set_long_text( 'Descr. Material'  ).
                  lo_cols_list->set_output_length( '25' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'PSTDAT' .
                  lo_cols_list->set_short_text( 'Dt.Lanc.' ).
                  lo_cols_list->set_medium_text( 'Dt. Lançamento'  ).
                  lo_cols_list->set_long_text( 'Dt. Lançamento'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'DOCDAT' .
                  lo_cols_list->set_short_text( 'Dt.Doc.' ).
                  lo_cols_list->set_medium_text( 'Dt. Documento'  ).
                  lo_cols_list->set_long_text( 'Dt. Documento'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'ANZPK' .
                  lo_cols_list->set_short_text( 'Qtd.' ).
                  lo_cols_list->set_medium_text( 'Quantidade'  ).
                  lo_cols_list->set_long_text( 'Quantidade'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'HSL' .
                  lo_cols_list->set_short_text( 'Valor NF' ).
                  lo_cols_list->set_medium_text( 'Valor NF'  ).
                  lo_cols_list->set_long_text( 'Valor NF'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'TSL' .
                  lo_cols_list->set_short_text( 'VlrDolar' ).
                  lo_cols_list->set_medium_text( 'Valor Dolar'  ).
                  lo_cols_list->set_long_text( 'Valor Dolar'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'KURSF' .
                  lo_cols_list->set_short_text( 'Taxa Dolar' ).
                  lo_cols_list->set_medium_text( 'Taxa Dolar'  ).
                  lo_cols_list->set_long_text( 'Taxa Dolar'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'AUBEL' .
                  lo_cols_list->set_short_text( 'Ord.Venda' ).
                  lo_cols_list->set_medium_text( 'Ordem de Venda'  ).
                  lo_cols_list->set_long_text( 'Ordem de Venda'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'REFKEY' .
                  lo_cols_list->set_short_text( 'Doc.Fat.' ).
                  lo_cols_list->set_medium_text( 'Doc. Faturamento'  ).
                  lo_cols_list->set_long_text( 'Doc. Faturamento'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'BELNR' .
                  lo_cols_list->set_short_text( 'Doc.Cont.' ).
                  lo_cols_list->set_medium_text( 'Doc. Contabil'  ).
                  lo_cols_list->set_long_text( 'Doc. Contabil'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'ORT01_P' .
                  lo_cols_list->set_short_text( 'Cidade PC' ).
                  lo_cols_list->set_medium_text( 'Cidade PC'  ).
                  lo_cols_list->set_long_text( 'Cidade PC'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'REGIO' .
                  lo_cols_list->set_short_text( 'UF' ).
                  lo_cols_list->set_medium_text( 'UF'  ).
                  lo_cols_list->set_long_text( 'UF'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'ORT01' .
                  lo_cols_list->set_short_text( 'LocEntrega' ).
                  lo_cols_list->set_medium_text( 'Local de entrega'  ).
                  lo_cols_list->set_long_text( 'Local de entrega'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'REGIO_L' .
                  lo_cols_list->set_short_text( 'UF' ).
                  lo_cols_list->set_medium_text( 'UF'  ).
                  lo_cols_list->set_long_text( 'UF'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'INCO1' .
                  lo_cols_list->set_short_text( 'Frete' ).
                  lo_cols_list->set_medium_text( 'Frete'  ).
                  lo_cols_list->set_long_text( 'Frete'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'RACCT' .
                  lo_cols_list->set_short_text( 'Contacli.' ).
                  lo_cols_list->set_medium_text( 'Conta cliente'  ).
                  lo_cols_list->set_long_text( 'Conta cliente'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_CC' .
                  lo_cols_list->set_short_text( 'CCliCutOff' ).
                  lo_cols_list->set_medium_text( 'Conta cliente CutOff'  ).
                  lo_cols_list->set_long_text( 'Conta cliente CutOff'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_R' .
                  lo_cols_list->set_short_text( 'Conta Rec.' ).
                  lo_cols_list->set_medium_text( 'Conta receita'  ).
                  lo_cols_list->set_long_text( 'Conta receita'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_RC' .
                  lo_cols_list->set_short_text( 'CRecCutOff' ).
                  lo_cols_list->set_medium_text( 'Conta receita CutOff'  ).
                  lo_cols_list->set_long_text( 'Conta receita CutOff'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_C' .
                  lo_cols_list->set_short_text( 'Conta Cust' ).
                  lo_cols_list->set_medium_text( 'Conta custo'  ).
                  lo_cols_list->set_long_text( 'Conta custo'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_P' .
                  lo_cols_list->set_short_text( 'CPPVCutOff' ).
                  lo_cols_list->set_medium_text( 'CPP ou Ver CutOff'  ).
                  lo_cols_list->set_long_text( 'Custo PP ou Ver CutOff'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_E' .
                  lo_cols_list->set_short_text( 'Conta Est.' ).
                  lo_cols_list->set_medium_text( 'Conta estoque'  ).
                  lo_cols_list->set_long_text( 'Conta estoque'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'SAKNR_EC' .
                  lo_cols_list->set_short_text( 'CEstCutOff' ).
                  lo_cols_list->set_medium_text( 'Conta estoque CutOff'  ).
                  lo_cols_list->set_long_text( 'Conta estoque CutOff'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'WAERS' .
                  lo_cols_list->set_short_text( 'Moeda' ).
                  lo_cols_list->set_medium_text( 'Moeda'  ).
                  lo_cols_list->set_long_text( 'Moeda'  ).
                  lo_cols_list->set_output_length( '06' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'UF' .
                  lo_cols_list->set_short_text( 'Mesma UF ?' ).
                  lo_cols_list->set_medium_text( 'Mesma UF ?'  ).
                  lo_cols_list->set_long_text( 'Mesma UF ?'  ).
                  lo_cols_list->set_output_length( '10' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'ROUTE' .
                  lo_cols_list->set_short_text( 'Itinerário' ).
                  lo_cols_list->set_medium_text( 'Itinerário'  ).
                  lo_cols_list->set_long_text( 'Itinerário'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'TRAZTD' .
                  lo_cols_list->set_short_text( 'TGItinerár' ).
                  lo_cols_list->set_medium_text( 'Temp.GastoItinerário'  ).
                  lo_cols_list->set_long_text( 'Tempo Gasto Itinerário'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'DOCDAT_E' .
                  lo_cols_list->set_short_text( 'DtEntrega' ).
                  lo_cols_list->set_medium_text( 'Data de entrega'  ).
                  lo_cols_list->set_long_text( 'Data de entrega'  ).
                  lo_cols_list->set_output_length( '10' ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                WHEN 'GERA' .
                  lo_cols_list->set_short_text( 'GCutOffMês' ).
                  lo_cols_list->set_medium_text( 'Gera CutOff no mês ?'  ).
                  lo_cols_list->set_long_text( 'Gera CutOff no mês ?'  ).
                  lo_cols_list->set_output_length( '12' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  "lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
                WHEN OTHERS.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              ENDCASE.
            ENDLOOP.
          CATCH cx_salv_not_found.

        ENDTRY.

        lo_cols->set_column_position( columnname =  'BUKRS' position  =   01   ).
        lo_cols->set_column_position( columnname =  'BRANCH'  position  =   02   ).
        lo_cols->set_column_position( columnname =  'CFOP'  position  =   03   ).
        lo_cols->set_column_position( columnname =  'TP_M'  position  =   04   ).
        lo_cols->set_column_position( columnname =  'NFENUM'  position  =   05   ).
        lo_cols->set_column_position( columnname =  'DOCNUM'  position  =   06   ).
        lo_cols->set_column_position( columnname =  'PARID' position  =   07   ).
        lo_cols->set_column_position( columnname =  'NAME1' position  =   08   ).
        lo_cols->set_column_position( columnname =  'MATKL' position  =   09   ).
        lo_cols->set_column_position( columnname =  'RASSC' position  =   10   ).
        lo_cols->set_column_position( columnname =  'SHPUNT'  position  =   11   ).
        lo_cols->set_column_position( columnname =  'MATNR' position  =   12   ).
        lo_cols->set_column_position( columnname =  'MAKTX' position  =   13   ).
        lo_cols->set_column_position( columnname =  'PSTDAT'  position  =   14   ).
        lo_cols->set_column_position( columnname =  'DOCDAT'  position  =   15   ).
        lo_cols->set_column_position( columnname =  'ANZPK' position  =   16   ).
        lo_cols->set_column_position( columnname =  'HSL' position  =   17   ).
        lo_cols->set_column_position( columnname =  'TSL' position  =   18   ).
        lo_cols->set_column_position( columnname =  'KURSF' position  =   19   ).
        lo_cols->set_column_position( columnname =  'AUBEL' position  =   20   ).
        lo_cols->set_column_position( columnname =  'REFKEY'  position  =   21   ).
        lo_cols->set_column_position( columnname =  'BELNR' position  =   22   ).
        lo_cols->set_column_position( columnname =  'ORT01_P' position  =   23   ).
        lo_cols->set_column_position( columnname =  'REGIO' position  =   24   ).
        lo_cols->set_column_position( columnname =  'ORT01' position  =   25   ).
        lo_cols->set_column_position( columnname =  'REGIO_L' position  =   26   ).
        lo_cols->set_column_position( columnname =  'INCO1' position  =   27   ).
        lo_cols->set_column_position( columnname =  'RACCT' position  =   28   ).
        lo_cols->set_column_position( columnname =  'SAKNR_CC'  position  =   29   ).
        lo_cols->set_column_position( columnname =  'SAKNR_R' position  =   30   ).
        lo_cols->set_column_position( columnname =  'SAKNR_RC'  position  =   31   ).
        lo_cols->set_column_position( columnname =  'SAKNR_C' position  =   32   ).
        lo_cols->set_column_position( columnname =  'SAKNR_P' position  =   33   ).
        lo_cols->set_column_position( columnname =  'SAKNR_E' position  =   34   ).
        lo_cols->set_column_position( columnname =  'SAKNR_EC'  position  =   35   ).
        lo_cols->set_column_position( columnname =  'WAERS' position  =   36   ).
        lo_cols->set_column_position( columnname =  'UF'  position  =   37   ).
        lo_cols->set_column_position( columnname =  'ROUTE' position  =   38   ).
        lo_cols->set_column_position( columnname =  'TRAZTD'  position  =   39   ).
        lo_cols->set_column_position( columnname =  'DOCDAT_E'  position  =   40   ).
        lo_cols->set_column_position( columnname =  'GERA'  position  =   41   ).

      ENDMETHOD.

      METHOD set_edit_alv.
*        DATA:ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
*             ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.
*
*        DATA: lv_ref_table  TYPE REF TO cl_abap_tabledescr,
*              lv_ref_struct TYPE REF TO cl_abap_structdescr.
*
*        ls_api = o_alv->extended_grid_api( ).
*        ls_edit = ls_api->editable_restricted( ).
*        lv_ref_table  ?= cl_abap_tabledescr=>describe_by_data( it_saida1 ).
*        lv_ref_struct ?= lv_ref_table->get_table_line_type( ).
*        DATA(lt_details)   = lv_ref_struct->components.
*
*        LOOP AT lt_details ASSIGNING FIELD-SYMBOL(<_details>).
*          ls_edit->set_attributes_for_columnname(
*            EXPORTING
*              columnname              = <_details>-name
*              all_cells_input_enabled = abap_true
*          ).
*
*        ENDLOOP.
*
*        DATA(mo_listener) = NEW lcl_listener1( ).
*        ls_edit->set_listener( mo_listener ).
*        ls_edit->validate_changed_data(
*    ).
*
*        o_alv->refresh( ).
      ENDMETHOD.

      METHOD get_data.



      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        "lo_functions->set_default( abap_true ).

        TRY.
            lo_functions->add_function( name     = 'P_CONT' "c_cont
                                        icon     = '@8W@'
                                        text     = 'Contabilização'
                                        tooltip  = 'Contabilização'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

            lo_functions->add_function( name     = 'P_MAT' "c_p_mat
                                        icon     = '@8W@'
                                        text     = 'Preço Unitário Material'
                                        tooltip  = 'Preço Unitário Material'
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

          WHEN 'GRAVAR'.

*            IF it_saida1 IS NOT INITIAL.
*              LOOP AT it_saida1 ASSIGNING FIELD-SYMBOL(<_read>).
*                "MODIFY zmmt0185 FROM <_read>.
*              ENDLOOP.
*            ELSE.
*              MESSAGE 'Teste' TYPE 'I'.
*            ENDIF.

          WHEN 'DELETE_ROW'.

*            IF qtd_rows > 0.
*              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
*                "READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
*                "DELETE zmmt0185 FROM <_del>.
*              ENDLOOP.
*
*            ELSE.
*              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
*              EXIT.
*            ENDIF.
*
*            CALL METHOD set_refresh CHANGING co_alv = o_alv.

          WHEN 'REFRESH_ROW'.
            CALL METHOD set_refresh CHANGING co_alv = o_alv.

          WHEN 'P_MAT'. "c_p_mat
            CALL SCREEN 200.
          WHEN 'P_CONT'. "c_cont
            "PERFORM f_contabilizacao.
            "IF git_saidac IS NOT INITIAL.
            CALL SCREEN 300.
            "ENDIF.
          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD on_toolbar.

        DATA : mt_toolbar TYPE stb_button.

        CLEAR mt_toolbar.
        "mt_toolbar-butn_type = '3'.   "separator
        "APPEND mt_toolbar TO e_object->mt_toolbar.

        LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
          "3 DESABILITA E 0 HABILITA
          IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
            <fs_tollbar>-butn_type = '3'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
            "<fs_tollbar>-butn_type = '3'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
            <fs_tollbar>-butn_type = '3'.
          ELSEIF <fs_tollbar>-function EQ '&REFRESH'.
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
          lifetime       = cl_gui_container=>lifetime_dynpro
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
                t_table        = it_saida1 ).
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
            co_report = lo_report1.

        CALL METHOD me->set_columns_build
          CHANGING
            co_alv = o_alv.

*        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
*        DATA l_title              TYPE lvc_title.
*        l_title = |Nome Relatório|.
*        lr_display_settings = o_alv->get_display_settings( ).
*        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*        lr_display_settings->set_list_header( l_title ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).        "Enable Zebra Layout
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
*      "READ TABLE lo_report1->it_saida INTO wa_saida INDEX row.
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
