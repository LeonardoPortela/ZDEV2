CLASS lo_report3 DEFINITION DEFERRED.
DATA: lo_report3 TYPE REF TO lo_report3.

CLASS lcl_listener3 DEFINITION.
  PUBLIC SECTION.
    INTERFACES :
      if_salv_gui_om_edit_strct_lstr.
ENDCLASS.

CLASS lcl_listener3 IMPLEMENTATION.
  METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.
    o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified) ).
  ENDMETHOD.
ENDCLASS.


CLASS lo_report3 DEFINITION.
  PUBLIC SECTION .

    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container.

    DATA: o_alv     TYPE REF TO cl_salv_table.
    METHODS:
      get_data,
      gera_doc_receita_custo IMPORTING acao TYPE sy-ucomm,
      estorna_doc_receita_custo IMPORTING acao TYPE sy-ucomm,
      rev_doc_receita_custo IMPORTING acao TYPE sy-ucomm,
      generate_output,
      set_handler
        CHANGING co_alv    TYPE REF TO cl_salv_table
                 co_report TYPE REF TO lo_report3,
*      set_refresh
*        CHANGING
*          co_alv TYPE REF TO cl_salv_table,
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
          co_alv TYPE REF TO cl_salv_table,
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
CLASS lo_report3 IMPLEMENTATION.
*  METHOD set_refresh.
*    lo_report3->get_data( ).
*    co_alv->refresh( ).
*  ENDMETHOD.

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

*...GET all the Columns
    DATA: lo_cols        TYPE REF TO cl_salv_columns,
          lo_cols_ref    TYPE        salv_t_column_ref,
          lo_cols_list   TYPE REF TO cl_salv_column_list,
          lo_col_list    LIKE LINE OF lo_cols_ref,
          lo_column      TYPE REF TO cl_salv_column,
          ls_ddic_f4_ref TYPE salv_s_ddic_reference.

    lo_cols = co_alv->get_columns( ).
    lo_cols->set_optimize( abap_false ).

    lo_cols_ref    = lo_cols->get( ).


    TRY.

        LOOP AT lo_cols_ref INTO lo_col_list.
          lo_cols_list ?= lo_col_list-r_column.
          CLEAR: ls_ddic_f4_ref.
          CASE lo_col_list-columnname.

            WHEN 'WAERS' .
              lo_cols_list->set_short_text( 'Moeda' ).
              lo_cols_list->set_medium_text( 'Moeda'  ).
              lo_cols_list->set_long_text( 'Moeda'  ).
              lo_cols_list->set_output_length( '06' ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'SAKNR_CC' .
              lo_cols_list->set_short_text( 'CCCutOff' ).
              lo_cols_list->set_medium_text( 'Conta cliente CutOff'  ).
              lo_cols_list->set_long_text( 'Conta cliente CutOff'  ).
              lo_cols_list->set_output_length( '10' ).

            WHEN 'SAKNR_RC' .
              lo_cols_list->set_short_text( 'CRCutOff' ).
              lo_cols_list->set_medium_text( 'Conta receita CutOff'  ).
              lo_cols_list->set_long_text( 'Conta receita CutOff'  ).
              lo_cols_list->set_output_length( '10' ).

            WHEN 'MATNR' .
              lo_cols_list->set_short_text( 'Produto' ).
              lo_cols_list->set_medium_text( 'Produto'  ).
              lo_cols_list->set_long_text( 'Produto'  ).
              lo_cols_list->set_output_length( '10' ).

            WHEN 'PARID' .
              lo_cols_list->set_short_text( 'CodCliente' ).
              lo_cols_list->set_medium_text( 'Cod. Cliente'  ).
              lo_cols_list->set_long_text( 'Cod. Cliente'  ).
              lo_cols_list->set_output_length( '10' ).

            WHEN 'NAME1' .
              lo_cols_list->set_short_text( 'Cliente' ).
              lo_cols_list->set_medium_text( 'Cliente'  ).
              lo_cols_list->set_long_text( 'Cliente'  ).
              lo_cols_list->set_output_length( '30' ).

            WHEN 'ANZPK' .
              lo_cols_list->set_short_text( 'Qtd' ).
              lo_cols_list->set_medium_text( 'Quantidade'  ).
              lo_cols_list->set_long_text( 'Quantidade'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'GEWEI' .
              lo_cols_list->set_short_text( 'Unid.' ).
              lo_cols_list->set_medium_text( 'Unid.'  ).
              lo_cols_list->set_long_text( 'Unid.'  ).
              lo_cols_list->set_output_length( '06' ).

            WHEN 'VLR_DOLAR' .
              lo_cols_list->set_short_text( 'Vlr USD' ).
              lo_cols_list->set_medium_text( 'Valor USD'  ).
              lo_cols_list->set_long_text( 'Valor USD'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'VLR_BRL' .
              lo_cols_list->set_short_text( 'Vlv BRL' ).
              lo_cols_list->set_medium_text( 'Valor BRL'  ).
              lo_cols_list->set_long_text( 'Valor BRL'  ).
              lo_cols_list->set_output_length( '15' ).

*            WHEN 'PRECO' .
*              lo_cols_list->set_short_text( 'Preço BRL' ).
*              lo_cols_list->set_medium_text( 'Preço BRL'  ).
*              lo_cols_list->set_long_text( 'Preço BRL'  ).
*              lo_cols_list->set_output_length( '15' ).

            WHEN 'PRECOB' .
              lo_cols_list->set_short_text( 'Preço BRL' ).
              lo_cols_list->set_medium_text( 'Preço BRL' ).
              lo_cols_list->set_long_text( 'Preço BRL' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'PRECO' .
              lo_cols_list->set_short_text( 'Preço USD' ).
              lo_cols_list->set_medium_text( 'Preço USD' ).
              lo_cols_list->set_long_text( 'Preço USD' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'C_DOLAR' .
              lo_cols_list->set_short_text( 'Custo USD' ).
              lo_cols_list->set_medium_text( 'Custo USD'  ).
              lo_cols_list->set_long_text( 'Custo USD'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'C_BRL' .
              lo_cols_list->set_short_text( 'Custo BRL' ).
              lo_cols_list->set_medium_text( 'Custo BRL'  ).
              lo_cols_list->set_long_text( 'Custo BRL'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'SAKNR_P' .
              lo_cols_list->set_short_text( 'CPPV/VCOff' ).
              lo_cols_list->set_medium_text( 'CustoPP / VerCutOff'  ).
              lo_cols_list->set_long_text( 'Custo PP ou Ver CutOff'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'SAKNR_EC' .
              lo_cols_list->set_short_text( 'CEstCutOff' ).
              lo_cols_list->set_medium_text( 'Conta estoque CutOff'  ).
              lo_cols_list->set_long_text( 'Conta estoque CutOff'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'MATKL' .
              lo_cols_list->set_short_text( 'GrpMercad' ).
              lo_cols_list->set_medium_text( 'Grp. Mercadoria'  ).
              lo_cols_list->set_long_text( 'Grp. Mercadoria'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'RASSC' .
              lo_cols_list->set_short_text( 'SCParceira' ).
              lo_cols_list->set_medium_text( 'Sociedade Parceira'  ).
              lo_cols_list->set_long_text( 'Sociedade Parceira'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'STATUS_1' .
              lo_cols_list->set_short_text( 'Status Rec' ).
              lo_cols_list->set_medium_text( 'Status Rec'  ).
              lo_cols_list->set_long_text( 'Status Rec'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'DOC_REC' .
              lo_cols_list->set_short_text( 'DocReceita' ).
              lo_cols_list->set_medium_text( 'Doc. Receita'  ).
              lo_cols_list->set_long_text( 'Doc. Receita'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'ESTORNO_1' .
              lo_cols_list->set_short_text( 'EstReceita' ).
              lo_cols_list->set_medium_text( 'Estorno Receita'  ).
              lo_cols_list->set_long_text( 'Estorno Receita'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'STATUS_2' .
              lo_cols_list->set_short_text( 'StatusCust' ).
              lo_cols_list->set_medium_text( 'Status Custo'  ).
              lo_cols_list->set_long_text( 'Status Custo'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'DOC_CUST' .
              lo_cols_list->set_short_text( 'DocCusto' ).
              lo_cols_list->set_medium_text( 'Doc. Custo'  ).
              lo_cols_list->set_long_text( 'Doc. Custo'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'ESTORNO_2' .
              lo_cols_list->set_short_text( 'EstCusto' ).
              lo_cols_list->set_medium_text( 'Estorno Custo'  ).
              lo_cols_list->set_long_text( 'Estorno Custo'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'DOC_REV_R' .
              lo_cols_list->set_short_text( 'DocRevRec' ).
              lo_cols_list->set_medium_text( 'Doc. Rev. Receita'  ).
              lo_cols_list->set_long_text( 'Doc. Rev. Receita'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'ESTORNO_3' .
              lo_cols_list->set_short_text( 'EstRevRec' ).
              lo_cols_list->set_medium_text( 'Est. Rev. Receita'  ).
              lo_cols_list->set_long_text( 'Est. Rev. Receita'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'DOC_REV_C' .
              lo_cols_list->set_short_text( 'DocRevCust' ).
              lo_cols_list->set_medium_text( 'Doc. Rev. Custo'  ).
              lo_cols_list->set_long_text( 'Doc. Rev. Custo'  ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'ESTORNO_4' .
              lo_cols_list->set_short_text( 'EstRevCust' ).
              lo_cols_list->set_medium_text( 'Est. Rev. Custo'  ).
              lo_cols_list->set_long_text( 'Est. Rev. Custo' ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'PRECO_REAL' .
              lo_cols_list->set_short_text( 'Preço BRL' ).
              lo_cols_list->set_medium_text( 'Preço BRL'  ).
              lo_cols_list->set_long_text( 'Preço BRL' ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN 'PRECO_DOLAR' .
              lo_cols_list->set_short_text( 'Preço USD' ).
              lo_cols_list->set_medium_text( 'Preço USD'  ).
              lo_cols_list->set_long_text( 'Preço USD' ).
              lo_cols_list->set_output_length( '15' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).

            WHEN OTHERS.
              lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
          ENDCASE.
        ENDLOOP.
      CATCH cx_salv_not_found.

    ENDTRY.

    lo_cols->set_column_position( columnname =  'WAERS'       position = 02   ).
    lo_cols->set_column_position( columnname =  'SAKNR_CC'    position = 03   ).
    lo_cols->set_column_position( columnname =  'SAKNR_RC'    position = 04   ).
    lo_cols->set_column_position( columnname =  'MATNR'       position = 05   ).
    lo_cols->set_column_position( columnname =  'PARID'       position = 06   ).
    lo_cols->set_column_position( columnname =  'NAME1'       position = 07   ).
    lo_cols->set_column_position( columnname =  'ANZPK'       position = 08   ).
    lo_cols->set_column_position( columnname =  'GEWEI'       position = 09   ).
    lo_cols->set_column_position( columnname =  'VLR_BRL'     position = 10   ).
    lo_cols->set_column_position( columnname =  'VLR_DOLAR'   position = 11   ).
    lo_cols->set_column_position( columnname =  'PRECO_REAL'  position = 12   ).
    lo_cols->set_column_position( columnname =  'PRECO_DOLAR' position = 14   ).
    lo_cols->set_column_position( columnname =  'C_BRL'       position = 15   ).
    lo_cols->set_column_position( columnname =  'C_DOLAR'     position = 16   ).
    lo_cols->set_column_position( columnname =  'SAKNR_P'     position = 17   ).
    lo_cols->set_column_position( columnname =  'SAKNR_EC'    position = 18   ).
    lo_cols->set_column_position( columnname =  'MATKL'       position = 19   ).
    lo_cols->set_column_position( columnname =  'RASSC'       position = 20   ).
    lo_cols->set_column_position( columnname =  'STATUS_1'    position = 21   ).
    lo_cols->set_column_position( columnname =  'DOC_REC'     position = 22   ).
    lo_cols->set_column_position( columnname =  'ESTORNO_1'   position = 23   ).
    lo_cols->set_column_position( columnname =  'STATUS_2'    position = 24   ).
    lo_cols->set_column_position( columnname =  'DOC_CUST'    position = 25   ).
    lo_cols->set_column_position( columnname =  'ESTORNO_2'   position = 26   ).
    lo_cols->set_column_position( columnname =  'DOC_REV_R'   position = 27   ).
    lo_cols->set_column_position( columnname =  'ESTORNO_3'   position = 28   ).
    lo_cols->set_column_position( columnname =  'DOC_REV_C'   position = 29   ).
    lo_cols->set_column_position( columnname =  'ESTORNO_4'   position = 30   ).

*    lo_cols->set_column_position( columnname =  'WAERS'       position  =   02   ).
*    lo_cols->set_column_position( columnname =  'SAKNR_CC'    position  =   03   ).
*    lo_cols->set_column_position( columnname =  'SAKNR_RC'    position  =   04   ).
*    lo_cols->set_column_position( columnname =  'MATNR'       position  =   05   ).
*    lo_cols->set_column_position( columnname =  'PARID'       position  =   06   ).
*    lo_cols->set_column_position( columnname =  'NAME1'       position  =   07   ).
*    lo_cols->set_column_position( columnname =  'ANZPK'       position  =   08   ).
*    lo_cols->set_column_position( columnname =  'GEWEI'       position  =   09   ).
*    lo_cols->set_column_position( columnname =  'VLR_BRL'     position  =   10   ).
*    lo_cols->set_column_position( columnname =  'VLR_DOLAR'   position  =   11   ).
**    lo_cols->set_column_position( columnname =  'PRECO'       position =   13   ).
*    lo_cols->set_column_position( columnname =  'PRECO_REAL'  position  =   12   ).
*    lo_cols->set_column_position( columnname =  'PRECO_DOLAR' position  =   12   ).
*    lo_cols->set_column_position( columnname =  'C_BRL'     position    =   14   ).
*    lo_cols->set_column_position( columnname =  'C_DOLAR'   position    =   15   ).
*    lo_cols->set_column_position( columnname =  'SAKNR_P'   position    =   16   ).
*    lo_cols->set_column_position( columnname =  'SAKNR_EC'  position    =   17   ).
*    lo_cols->set_column_position( columnname =  'MATKL'     position    =   18   ).
*    lo_cols->set_column_position( columnname =  'RASSC'     position    =   19   ).
*    lo_cols->set_column_position( columnname =  'STATUS_1'  position    =   20   ).
*    lo_cols->set_column_position( columnname =  'DOC_REC'   position    =   21   ).
*    lo_cols->set_column_position( columnname =  'ESTORNO_1' position    =   22   ).
*    lo_cols->set_column_position( columnname =  'STATUS_2'  position    =   23   ).
*    lo_cols->set_column_position( columnname =  'DOC_CUST'  position    =   24   ).
*    lo_cols->set_column_position( columnname =  'ESTORNO_2' position    =   25   ).
*    lo_cols->set_column_position( columnname =  'DOC_REV_R' position    =   26   ).
*    lo_cols->set_column_position( columnname =  'ESTORNO_3' position    =   27   ).
*    lo_cols->set_column_position( columnname =  'DOC_REV_C' position    =   28   ).
*    lo_cols->set_column_position( columnname =  'ESTORNO_4' position    =   29   ).


  ENDMETHOD.

  METHOD set_edit_alv.
    DATA:ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
         ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.

    DATA: lv_ref_table  TYPE REF TO cl_abap_tabledescr,
          lv_ref_struct TYPE REF TO cl_abap_structdescr.

    ls_api = o_alv->extended_grid_api( ).
    ls_edit = ls_api->editable_restricted( ).
    lv_ref_table  ?= cl_abap_tabledescr=>describe_by_data( it_saida3 ).
    lv_ref_struct ?= lv_ref_table->get_table_line_type( ).
    DATA(lt_details)   = lv_ref_struct->components.

    LOOP AT lt_details ASSIGNING FIELD-SYMBOL(<_details>).
      ls_edit->set_attributes_for_columnname(
        EXPORTING
          columnname              = <_details>-name
          all_cells_input_enabled = abap_true
      ).

    ENDLOOP.

    DATA(mo_listener) = NEW lcl_listener1( ).
    ls_edit->set_listener( mo_listener ).
    ls_edit->validate_changed_data(
  ).

    o_alv->refresh( ).
  ENDMETHOD.

  METHOD get_data.

    TYPES: BEGIN OF ty_preco_material,
             matnr  TYPE    acdoca-matnr,
             racct  TYPE    acdoca-racct,
             precob TYPE    acdoca-hsl,
             preco  TYPE    acdoca-hsl,
           END OF ty_preco_material.

    DATA: ls_preco_material TYPE ty_preco_material,
          lt_preco_material TYPE TABLE OF ty_preco_material.


    SELECT * FROM zglt0004
    WHERE bukrs = @p_bukrs-low
    AND poper = @p_poper
    AND gjahr = @p_ano
    INTO TABLE @DATA(it_zglt0004).


    IF it_zglt0004 IS NOT INITIAL.

      FREE: it_saida3.

      MOVE-CORRESPONDING it_zglt0004 TO it_saida3.

      FREE: it_zglt0004.

    ELSE.

      DATA: lv_data      TYPE datum,
            lv_mes(2)    TYPE n,
            lv_data_wrt  TYPE char10,
            lv_data_conv TYPE datum.

      lv_mes = ( p_budat-high+4(2) ) + 1.
      lv_data = p_budat-high(4) && lv_mes && '01'.

      WRITE lv_data TO lv_data_wrt.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = lv_data_wrt
        IMPORTING
          output = lv_data_conv.

      DATA(git_saidanx) = it_saida1.
      DATA(git_saidan) = it_saida1.
      SORT git_saidanx BY waers racct matnr matkl gera.
      DELETE git_saidanx WHERE gera <> 'SIM'.
      DELETE git_saidan WHERE gera <> 'SIM'.
      DELETE ADJACENT DUPLICATES FROM git_saidanx COMPARING nfenum docnum belnr.

      DATA: git_saidac TYPE STANDARD TABLE OF ty_saida3 INITIAL SIZE 0.
      MOVE-CORRESPONDING git_saidanx TO git_saidac.
      FREE: git_saidanx.

      DATA:
        _qtds   TYPE decfloat34,
        _qtd    TYPE decfloat34,
        _hsl    TYPE decfloat34,
        _vlrbrl TYPE decfloat34,
        _tsl    TYPE decfloat34,
        _vlrusd TYPE decfloat34.
      CLEAR: _hsl,_tsl,_qtd,_vlrbrl,_vlrusd.

      IF git_saidac IS NOT INITIAL.

        PERFORM get_dados_alv2.

        LOOP AT git_saidac ASSIGNING FIELD-SYMBOL(<fs_saidac>).

          LOOP AT git_saidan INTO DATA(gwa_saidan) WHERE waers EQ <fs_saidac>-waers
                                                     AND racct EQ <fs_saidac>-racct
                                                     AND matkl EQ <fs_saidac>-matkl
                                                     AND matnr EQ <fs_saidac>-matnr.

*            CONDENSE gwa_saidan-anzpk NO-GAPS.
            _qtd = gwa_saidan-anzpk.
            _qtds = _qtds + _qtd.

            _vlrbrl = gwa_saidan-hsl.
            _hsl = _hsl + _vlrbrl.


            _vlrusd = gwa_saidan-tsl.
            _tsl = _tsl + _vlrusd.

            CLEAR: _qtd,_vlrbrl,_vlrusd,gwa_saidan.

          ENDLOOP.

          <fs_saidac>-anzpk = _qtds.

          <fs_saidac>-vlr_brl    = _hsl.
          <fs_saidac>-vlr_dolar   = _tsl.

          READ TABLE git_saidan INTO gwa_saidan WITH KEY gera  = 'SIM' waers = <fs_saidac>-waers racct = <fs_saidac>-racct matkl = <fs_saidac>-matkl.
          <fs_saidac>-gewei       = gwa_saidan-shpunt.

          IF <fs_saidac>-status_1 IS INITIAL.
            <fs_saidac>-status_1 = icon_led_yellow.
          ENDIF.

          IF <fs_saidac>-status_2 IS INITIAL.
            <fs_saidac>-status_2 = icon_led_yellow.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <fs_saidac>-saknr_c
            IMPORTING
              output = <fs_saidac>-saknr_c.

          IF <fs_saidac>-gjahr IS INITIAL.
            <fs_saidac>-gjahr = p_ano.
          ENDIF.


*          READ TABLE it_saida2 INTO DATA(wa_saida2) WITH KEY racct = <fs_saidac>-saknr_c
*                matnr = <fs_saidac>-matnr
*                BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            <fs_saidac>-preco   = wa_saida2-preco.
*          ELSE.
*            CLEAR <fs_saidac>-preco.
*          ENDIF.

          <fs_saidac>-c_dolar   = _qtds * _tsl.
          <fs_saidac>-c_brl   = _qtds * _hsl.

          CLEAR: _hsl,_tsl,_qtd,_vlrbrl,_vlrusd,_qtds.
        ENDLOOP.

        FREE:it_saida2.

        SORT git_saidac BY bukrs waers racct matnr matkl.

        DELETE ADJACENT DUPLICATES FROM git_saidac COMPARING bukrs waers racct matnr matkl.
        DELETE git_saidac WHERE matnr IS INITIAL.

        IF git_saidac IS NOT INITIAL.
          MOVE git_saidac TO it_saida3.
          FREE: git_saidac.
        ENDIF.

      ENDIF.
    ENDIF.

    IF it_saida3 IS NOT INITIAL.

      PERFORM get_dados_alv2. "Alheiros

      SORT it_saida2 BY matnr racct.

      SORT it_saida3 BY waers ASCENDING saknr_cc
                              ASCENDING matnr
                              ASCENDING matkl
                              ASCENDING rassc ASCENDING.

      DATA(lt_saida2_aux) = it_saida2[].

      FREE it_saida2[].

      LOOP AT lt_saida2_aux INTO DATA(ls_calc_preco).
        MOVE-CORRESPONDING ls_calc_preco TO ls_preco_material.

        COLLECT ls_preco_material INTO lt_preco_material.
      ENDLOOP.


      FREE lt_saida2_aux[].

      SORT lt_preco_material BY matnr racct.

      LOOP AT it_saida3 ASSIGNING FIELD-SYMBOL(<fs_saida3>).

        READ TABLE it_saida1 INTO DATA(ls_saida1)
             WITH KEY matnr = <fs_saida3>-matnr
                      parid = <fs_saida3>-parid.

        IF sy-subrc IS INITIAL.
          <fs_saida3>-saknr_rc = ls_saida1-saknr_rc.
        ENDIF.

        READ TABLE lt_preco_material INTO ls_preco_material
             WITH KEY matnr =  <fs_saida3>-matnr
                      racct = <fs_saida3>-saknr_c
             BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF <fs_saida3>-vlr_dolar > 0
            AND <fs_saida3>-anzpk > 0.
            <fs_saida3>-preco_dolar = ls_preco_material-preco.
            <fs_saida3>-c_dolar     = <fs_saida3>-anzpk * ls_preco_material-preco.
          ENDIF.

          IF <fs_saida3>-vlr_brl > 0
            AND <fs_saida3>-anzpk > 0.
            <fs_saida3>-preco_real = ls_preco_material-precob.
            <fs_saida3>-c_brl      = <fs_saida3>-anzpk * ls_preco_material-precob.
          ENDIF.
        ENDIF.

        IF <fs_saida3>-obj_key_1 IS NOT INITIAL.
          SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<fs_saida3>-obj_key_1 INTO @DATA(_erro1).
          IF sy-subrc = 0.
            <fs_saida3>-status_1 = icon_led_red.
          ELSE.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saida3>-obj_key_1 INTO @<fs_saida3>-doc_rec.
            IF sy-subrc = 0.
              IF <fs_saida3>-estorno_1 IS INITIAL.
                <fs_saida3>-status_1 = icon_led_green.
              ELSE.
                <fs_saida3>-status_1 = icon_led_yellow.
              ENDIF.
            ELSE.
              <fs_saida3>-status_1 = icon_time_ina.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <fs_saida3>-obj_key_2 IS NOT INITIAL.
          SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<fs_saida3>-obj_key_2 INTO @DATA(_erro2).
          IF sy-subrc = 0.
            <fs_saida3>-status_2 = icon_led_red.
          ELSE.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saida3>-obj_key_2 INTO @<fs_saida3>-doc_cust.
            IF sy-subrc = 0.
              IF <fs_saida3>-estorno_2 IS INITIAL.
                <fs_saida3>-status_2 = icon_led_green.
              ELSE.
                <fs_saida3>-status_2 = icon_led_yellow.
              ENDIF.
            ELSE.
              <fs_saida3>-status_2 = icon_time_ina.
            ENDIF.
          ENDIF.

        ENDIF.

        IF <fs_saida3>-obj_key_3 IS NOT INITIAL.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saida3>-obj_key_3 INTO @<fs_saida3>-doc_rev_r.
        ENDIF.

        IF <fs_saida3>-obj_key_4 IS NOT INITIAL.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saida3>-obj_key_4 INTO @<fs_saida3>-doc_rev_c.
        ENDIF.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.

  METHOD gera_doc_receita_custo.

    TYPES: BEGIN OF ty_grupo,
             waers    TYPE waers,
             saknr_cc TYPE saknr,
           END OF ty_grupo.

    DATA: it_zib_contabil TYPE STANDARD TABLE OF zib_contabil,
          wa_zib_contabil TYPE zib_contabil,
          it_zglt0004     TYPE STANDARD TABLE OF zglt0004 INITIAL SIZE 0,
          wa_zglt0004     TYPE zglt0004,
          wa_saida3       TYPE ty_saida3,
          lv_seqsgp(6)    TYPE n,
          lv_seqgp(6)     TYPE n,
          lv_dorow(6)     TYPE n,
          vseq(6)         TYPE p,
          lv_obj_key      TYPE string,
          it_grupo        TYPE STANDARD TABLE OF ty_grupo INITIAL SIZE 0.

    SELECT * FROM zglt0004
    WHERE bukrs = @p_bukrs-low
    AND poper = @p_poper
    AND gjahr = @p_ano
    INTO TABLE @it_zglt0004.

    IF it_zglt0004 IS INITIAL.
      IF it_saida3 IS NOT INITIAL.
        MOVE-CORRESPONDING it_saida3 TO it_zglt0004.

        IF it_zglt0004 IS NOT INITIAL.
          DATA: _uuid TYPE guid_16.
          LOOP AT it_zglt0004 ASSIGNING FIELD-SYMBOL(<fs_zglt0004>).
            CLEAR: _uuid.
            CALL FUNCTION 'GUID_CREATE'
              IMPORTING
                ev_guid_16 = _uuid.
            <fs_zglt0004>-uuid = _uuid.
          ENDLOOP.
          MODIFY zglt0004 FROM TABLE it_zglt0004.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK it_zglt0004 IS NOT INITIAL.

    FREE: it_zib_contabil.
    CLEAR:wa_zib_contabil,vseq,lv_seqgp,lv_seqsgp.

    MOVE-CORRESPONDING it_zglt0004 TO it_grupo.

    SORT it_grupo.

    DELETE ADJACENT DUPLICATES FROM it_grupo COMPARING waers saknr_cc.

    CASE acao.
      WHEN 'BT_DOC_REC'.

      WHEN 'BT_DOC_CUST'.
        DELETE it_grupo WHERE waers <> 'USD'.
    ENDCASE.

    LOOP AT it_grupo INTO DATA(wa_grupo).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seqgp = vseq.
      CLEAR: lv_seqsgp.

      LOOP AT it_zglt0004 ASSIGNING <fs_zglt0004> WHERE waers = wa_grupo-waers AND saknr_cc = wa_grupo-saknr_cc.
        DATA(_obj_key)    =  'CUTOFF' && lv_seqgp && <fs_zglt0004>-bukrs && <fs_zglt0004>-gjahr.
        DATA(_gsber) = <fs_zglt0004>-bukrs+2(2) && '01'.
        CLEAR:lv_dorow.
        DO 2 TIMES.
          lv_seqsgp = lv_seqsgp + 1.
          lv_dorow = lv_dorow + 1.
          wa_zib_contabil-obj_key    =  _obj_key.
          wa_zib_contabil-seqitem    =  lv_seqsgp.
          wa_zib_contabil-gsber = _gsber.
          wa_zib_contabil-bukrs      =  <fs_zglt0004>-bukrs.
          wa_zib_contabil-interface  =  '0'.
          wa_zib_contabil-bktxt      =  'CUT OFF'.
          CONCATENATE  p_budat-high+6(2) p_budat-high+4(2) p_budat-high+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
          CONCATENATE  p_budat-high+6(2) p_budat-high+4(2) p_budat-high+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.
          wa_zib_contabil-gjahr      = <fs_zglt0004>-gjahr.
          wa_zib_contabil-monat      = p_budat-high+4(2).
          wa_zib_contabil-blart      = 'LM'.
          wa_zib_contabil-vbund          = <fs_zglt0004>-rassc.
          wa_zib_contabil-rg_atualizado  = 'N'.
          wa_zib_contabil-bupla      = <fs_zglt0004>-bukrs.
          wa_zib_contabil-sgtxt      = 'CUT OFF CLIENTE X RECEITA'.

          IF lv_dorow EQ '1'.
            wa_zib_contabil-matnr          = <fs_zglt0004>-matnr.
            wa_zib_contabil-quantity       = <fs_zglt0004>-anzpk.
            wa_zib_contabil-base_uom       = <fs_zglt0004>-gewei.
            wa_zib_contabil-prctr      = '0000009900'.
            wa_zib_contabil-hkont      = <fs_zglt0004>-saknr_rc.
            wa_zib_contabil-bschl    =  '50'.
          ELSEIF lv_dorow EQ '2'.
            wa_zib_contabil-matnr          = ''.
            wa_zib_contabil-quantity       = ''.
            wa_zib_contabil-base_uom       = ''.
            wa_zib_contabil-prctr      = ''.
            wa_zib_contabil-hkont      = <fs_zglt0004>-saknr_cc.
            wa_zib_contabil-bschl    =  '40'.
          ENDIF.

          IF <fs_zglt0004>-waers EQ 'BRL'. "AND ( acao = 'BT_DOC_REC' OR acao = 'BT_DOC_CUST' ). "Receita e Custo
            wa_zib_contabil-wrbtr      = <fs_zglt0004>-vlr_brl.
            wa_zib_contabil-waers      = <fs_zglt0004>-waers.
            wa_zib_contabil-waers_i    = <fs_zglt0004>-waers.
            wa_zib_contabil-dmbtr      = <fs_zglt0004>-vlr_brl.
            wa_zib_contabil-waers_f    = 'USD'.
            wa_zib_contabil-dmbe2      = <fs_zglt0004>-vlr_dolar.
          ELSEIF <fs_zglt0004>-waers EQ 'USD'."AND ( acao = 'BT_DOC_REC' ). "Somente Receita
            wa_zib_contabil-wrbtr      = <fs_zglt0004>-vlr_dolar.
            wa_zib_contabil-waers      = <fs_zglt0004>-waers.
            wa_zib_contabil-waers_i    = 'BRL'.
            wa_zib_contabil-dmbtr      = <fs_zglt0004>-vlr_brl.
            wa_zib_contabil-waers_f    = <fs_zglt0004>-waers.
            wa_zib_contabil-dmbe2      = <fs_zglt0004>-vlr_dolar.
          ENDIF.

          APPEND wa_zib_contabil TO it_zib_contabil.
          CLEAR: wa_zib_contabil.
        ENDDO.

        CASE acao.
          WHEN 'BT_DOC_REC'.
            <fs_zglt0004>-obj_key_1 = _obj_key.
          WHEN 'BT_DOC_CUST'.
            <fs_zglt0004>-obj_key_2 = _obj_key.
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
      CLEAR:wa_zib_contabil,vseq,lv_seqgp,lv_seqsgp.
    ENDLOOP.



    IF it_zglt0004 IS NOT INITIAL AND it_zib_contabil IS NOT INITIAL.

      MODIFY zib_contabil  FROM TABLE it_zib_contabil.
      COMMIT WORK AND WAIT.
      FREE: it_zib_contabil.

      LOOP AT it_zglt0004 ASSIGNING <fs_zglt0004> GROUP BY ( waers = <fs_zglt0004>-waers saknr_cc = <fs_zglt0004>-saknr_cc ).
        CASE acao.
          WHEN 'BT_DOC_REC'.
            UPDATE zglt0004 SET obj_key_1 = <fs_zglt0004>-obj_key_1 WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
          WHEN 'BT_DOC_CUST'.
            UPDATE zglt0004 SET obj_key_2 = <fs_zglt0004>-obj_key_2 WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
          WHEN OTHERS.
        ENDCASE.
        COMMIT WORK AND WAIT.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD estorna_doc_receita_custo.

    DATA: lv_docnum   TYPE bkpf-belnr,   " Document Number
          lv_compcode TYPE bkpf-bukrs,   " Company Code
          lv_fiscyear TYPE bkpf-gjahr,   " Fiscal Year
          lv_revtype  TYPE stgrd,        " Reversal Reason
          lt_return   TYPE TABLE OF bapiret2,  " Return Messages
          ls_return   TYPE bapiret2,
          logsys      TYPE bapiacrev-obj_sys,
          it_zglt0004 TYPE STANDARD TABLE OF zglt0004 INITIAL SIZE 0,
          wa_zglt0004 TYPE zglt0004.

    FREE:lt_return.

    SELECT * FROM zglt0004
    WHERE bukrs = @p_bukrs-low
    AND poper = @p_poper
    AND gjahr = @p_ano
    INTO TABLE @it_zglt0004.

    IF it_zglt0004 IS INITIAL.

      MOVE-CORRESPONDING it_saida3 TO it_zglt0004.

    ELSE.

      CASE acao.
        WHEN 'BT_ESTORNO_1'.
          DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_1.
        WHEN 'BT_ESTORNO_2'.
          DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_2.
          DELETE it_zglt0004 WHERE waers <> 'USD'.
        WHEN 'BT_ESTORNO_3'.
          DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_3.
        WHEN 'BT_ESTORNO_4'.
          DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_4.
          DELETE it_zglt0004 WHERE waers <> 'USD'.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.

    CLEAR: lv_docnum   ,lv_compcode ,lv_fiscyear ,lv_revtype  ,lt_return   ,ls_return,logsys.

    IF it_zglt0004 IS NOT INITIAL.

      LOOP AT it_zglt0004 ASSIGNING FIELD-SYMBOL(<fs_zglt0004>) GROUP BY ( waers = <fs_zglt0004>-waers saknr_cc = <fs_zglt0004>-saknr_cc ).

        CASE acao.
          WHEN 'BT_ESTORNO_1'.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_zglt0004>-obj_key_1 INTO @lv_docnum. "doc_rec
          WHEN 'BT_ESTORNO_2'.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_zglt0004>-obj_key_2 INTO @lv_docnum."doc_cust
          WHEN 'BT_ESTORNO_3'.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_zglt0004>-obj_key_3 INTO @lv_docnum."doc_rev_r
          WHEN 'BT_ESTORNO_4'.
            SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_zglt0004>-obj_key_4 INTO @lv_docnum."doc_rev_c
          WHEN OTHERS.
        ENDCASE.

        CALL FUNCTION 'CALL_FB08'
          EXPORTING
            i_belnr       = lv_docnum
            i_bukrs       = <fs_zglt0004>-bukrs
            i_gjahr       = <fs_zglt0004>-gjahr
            i_stgrd       = '01'
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.

        IF sy-subrc = 0.

          SELECT SINGLE stblg
          FROM bkpf INTO @DATA(lva_stblg)
                WHERE bukrs = @<fs_zglt0004>-bukrs
                AND belnr = @lv_docnum
                AND gjahr = @<fs_zglt0004>-gjahr(4).

          CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

          CASE acao.
            WHEN 'BT_ESTORNO_1'.
              UPDATE zglt0004 SET estorno_1 = lva_stblg WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
            WHEN 'BT_ESTORNO_2'.
              UPDATE zglt0004 SET estorno_2 = lva_stblg WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
            WHEN 'BT_ESTORNO_3'.
              UPDATE zglt0004 SET estorno_3 = lva_stblg WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
            WHEN 'BT_ESTORNO_4'.
              UPDATE zglt0004 SET estorno_4 = lva_stblg WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
            WHEN OTHERS.
          ENDCASE.

          COMMIT WORK AND WAIT.

        ENDIF.

        CLEAR: lv_docnum   ,lv_compcode ,lv_fiscyear ,lv_revtype  ,lt_return   ,ls_return.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD rev_doc_receita_custo.

    TYPES: BEGIN OF ty_grupo,
             waers    TYPE waers,
             saknr_cc TYPE saknr,
           END OF ty_grupo.

    DATA: lv_seq(6)   TYPE n,
          vseq(6)     TYPE p,
          it_zglt0004 TYPE STANDARD TABLE OF zglt0004 INITIAL SIZE 0,
          wa_zglt0004 TYPE zglt0004.

    SELECT * FROM zglt0004
    WHERE bukrs = @p_bukrs-low
    AND poper = @p_poper
    AND gjahr = @p_ano
    INTO TABLE @it_zglt0004.

    IF it_zglt0004 IS INITIAL.
      MOVE-CORRESPONDING it_saida3 TO it_zglt0004.
    ENDIF.

    CASE acao.
      WHEN 'BT_DOC_REV_R'.
        DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_1.
      WHEN 'BT_DOC_REV_C'.
        DELETE ADJACENT DUPLICATES FROM it_zglt0004 COMPARING obj_key_2.
        DELETE it_zglt0004 WHERE waers <> 'USD'.
    ENDCASE.

    CHECK it_zglt0004 IS NOT INITIAL.

    LOOP AT it_zglt0004 ASSIGNING FIELD-SYMBOL(<fs_zglt0004>) GROUP BY ( waers = <fs_zglt0004>-waers saknr_cc = <fs_zglt0004>-saknr_cc ).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seq = vseq.

      DATA(_obj_key)    =  'CUTOFF' && lv_seq && <fs_zglt0004>-bukrs && <fs_zglt0004>-gjahr.

      CASE acao.
        WHEN 'BT_DOC_REV_R'.
          SELECT * FROM zib_contabil WHERE obj_key EQ @<fs_zglt0004>-obj_key_1 INTO TABLE @DATA(it_zib_contabil).
        WHEN 'BT_DOC_REV_C'.
          SELECT * FROM zib_contabil WHERE obj_key EQ @<fs_zglt0004>-obj_key_2 INTO TABLE @it_zib_contabil.
        WHEN OTHERS.
      ENDCASE.

      IF it_zib_contabil IS NOT INITIAL.
        LOOP AT it_zib_contabil ASSIGNING FIELD-SYMBOL(<fs_zib_contabil>).

          IF sy-tabix = 1.
            <fs_zib_contabil>-bschl = '40'.
          ELSEIF sy-tabix = 2.
            <fs_zib_contabil>-bschl = '50'.
          ENDIF.

          <fs_zib_contabil>-obj_key = _obj_key.
          <fs_zib_contabil>-rg_atualizado = 'N'.


          DATA: lv_date     TYPE sy-datum,
                lv_new_date TYPE sy-datum.

          REPLACE ALL OCCURENCES OF '.' IN <fs_zib_contabil>-bldat WITH ''.
          DATA(_ano_old) = <fs_zib_contabil>-bldat+4(4).
          DATA(_mes_old) = <fs_zib_contabil>-bldat+2(2).
          DATA(_dia_old) = <fs_zib_contabil>-bldat+0(2).

          lv_date = |{ _ano_old }{ _mes_old }{ _dia_old }|. " Zib date
          lv_new_date = lv_date + 1.  " add 1 days
          DATA(_ano) = lv_new_date+0(4).
          DATA(_mes) = lv_new_date+4(2).
          DATA(_dia) = lv_new_date+6(2).

          <fs_zib_contabil>-bldat = |{ _dia }.{ _mes }.{ _ano }|.
          <fs_zib_contabil>-budat = |{ _dia }.{ _mes }.{ _ano }|.
          <fs_zib_contabil>-monat = |{ _mes }|.
          <fs_zib_contabil>-gjahr = |{ _ano }|.

          CASE acao.
            WHEN 'BT_DOC_REV_R'.
              <fs_zib_contabil>-xblnr = <fs_zglt0004>-obj_key_1.
            WHEN 'BT_DOC_REV_C'.
              <fs_zib_contabil>-xblnr = <fs_zglt0004>-obj_key_2.
          ENDCASE.

        ENDLOOP.

        IF it_zib_contabil IS NOT INITIAL.
          MODIFY zib_contabil  FROM TABLE it_zib_contabil.
          COMMIT WORK AND WAIT.
        ENDIF.


        FREE: it_zib_contabil.

        CASE acao.
          WHEN 'BT_DOC_REV_R'.
            UPDATE zglt0004 SET obj_key_3 = _obj_key WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
          WHEN 'BT_DOC_REV_C'.
            UPDATE zglt0004 SET obj_key_4 = _obj_key WHERE waers = <fs_zglt0004>-waers AND saknr_cc = <fs_zglt0004>-saknr_cc.
          WHEN OTHERS.
        ENDCASE.

        COMMIT WORK AND WAIT.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_pf_status.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.
    lo_functions = co_alv->get_functions( ).
    lo_functions->set_all( abap_true ). "Liberar full tollbar
    "lo_functions->set_default( abap_true ). "Liberar default tollbar

    TRY.
        IF p_cont IS NOT INITIAL.

          lo_functions->add_function( name     = 'BT_REFRESH' "c_g_rec
                                      icon     = '@B_REFR@'
                                      text     = ''
                                      tooltip  = 'Atualizar Grid'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).


          lo_functions->add_function( name     = 'BT_DOC_REC' "c_g_rec
                                      icon     = '@15@' "icon_execute_object
                                      text     = 'Gerar Doc. Receita'
                                      tooltip  = 'Gerar Documento Receita'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_DOC_CUST' "c_g_cus
                                      icon     = '@15@' "icon_execute_object
                                      text     = 'Gerar Doc. Custo'
                                      tooltip  = 'Gerar Documento Custo'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_ESTORNO_1' "c_ge_rec
                                      icon     = '@4R@' "icon_system_redo
                                      text     = 'Est. Doc. Receita'
                                      tooltip  = 'Estornar Documento Receita'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_ESTORNO_2' "c_ge_cus
                                      icon     = '@4R@' "icon_system_redo
                                      text     = 'Est. Doc. Custo'
                                      tooltip  = 'Estornar Documento Custo'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

        ENDIF.

        IF p_rev IS NOT INITIAL.
*          lo_functions->add_function( name     = 'BT_ESTORNO_2' "c_ge_cus
*                                      icon     = '@4R@' "icon_system_redo
*                                      text     = 'Est. Doc. Custo'
*                                      tooltip  = 'Estornar Documento Custo'
*                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_DOC_REV_R' "c_gr_rec
                                      icon     = '@39@' "icon_generate
                                      text     = 'Rev. Doc. Receita'
                                      tooltip  = 'Reverter Documento Receita'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_DOC_REV_C' "c_gr_cus
                                      icon     = '@39@' "icon_generate
                                      text     = 'Rev. Doc. Custo'
                                      tooltip  = 'Reverter Documento Custo'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_ESTORNO_3' "c_er_rec
                                      icon     = '@4R@' "icon_system_redo
                                      text     = 'Est. Rev. Doc. Receita'
                                      tooltip  = 'Estornar Reverter Documento Receita'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).

          lo_functions->add_function( name     = 'BT_ESTORNO_4' "c_er_cus
                                      icon     = '@4R@' "icon_system_redo
                                      text     = 'Est. Rev. Doc. Custo'
                                      tooltip  = 'Estornar Reverter Documento Custo'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).
        ENDIF.
        IF p_cont IS NOT INITIAL.
          lo_functions->add_function( name     = 'BT_LIMPAR'
                                      icon     = '@ERASER@'
                                      text     = 'Limprar'
                                      tooltip  = 'Limpar Dados'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).


          lo_functions->add_function( name     = 'BT_REPROCESSAR'
                                      icon     = '@B_OPER@'
                                      text     = 'Reprocessar'
                                      tooltip  = 'Reprocessar'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).
        ENDIF.
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

    SELECT * FROM zglt0004
     WHERE bukrs = @p_bukrs-low
     AND poper = @p_poper
     AND gjahr = @p_ano
     INTO TABLE @DATA(it_existe_zglt0004).

    CASE e_salv_function.
      WHEN 'BT_DOC_REC'." 'Gerar Documento Receita'."c_g_rec
        READ TABLE it_saida3 INTO DATA(_notexist_obj_key_1) WITH KEY obj_key_1 = ''.
        IF sy-subrc = 0.
          lo_report3->gera_doc_receita_custo( 'BT_DOC_REC' ).
          lo_report3->get_data( ).
          lo_report3->o_alv->refresh( ).
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Gerar Documento Receita!'.
        ENDIF.
      WHEN 'BT_DOC_CUST'." 'Gerar Documento Custo' c_g_cus
        READ TABLE it_saida3 INTO DATA(_notexist_obj_key_2) WITH KEY obj_key_2 = '' waers = 'USD'.
        IF sy-subrc = 0.
          lo_report3->gera_doc_receita_custo( 'BT_DOC_CUST' ).
          lo_report3->get_data( ).
          lo_report3->o_alv->refresh( ).
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Gerar Documento Custo!'.
        ENDIF.
      WHEN 'BT_DOC_REV_R'." 'Reverter Documento Custo' c_gr_rec
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_1 estorno_1.
          DELETE it_existe_zglt0004 WHERE obj_key_1 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_1 IS NOT INITIAL. "Não pode ter estorno

          DATA(qtd_doc_rev_r) = lines( it_existe_zglt0004 ).
          IF qtd_doc_rev_r > 0.
            lo_report3->rev_doc_receita_custo( 'BT_DOC_REV_R' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Reverter Documento Receita!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_DOC_REV_C'." 'Reverter Documento Custo' c_gr_cus
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_2 estorno_2 waers.
          DELETE it_existe_zglt0004 WHERE obj_key_2 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_2 IS NOT INITIAL. "Não pode ter estorno
          DELETE it_existe_zglt0004 WHERE waers <> 'USD'. "Tem que ter só essa moeda

          DATA(qtd_doc_rev_c) = lines( it_existe_zglt0004 ).
          IF qtd_doc_rev_c > 0.
            lo_report3->rev_doc_receita_custo( 'BT_DOC_REV_C' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Reverter Documento Custo!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_ESTORNO_1'." 'Estornar Documento Receita'." c_ge_cus
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_1 estorno_1.
          DELETE it_existe_zglt0004 WHERE obj_key_1 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_1 IS NOT INITIAL. "Não pode ter estorno

          DATA(qtd_estorno_1) = lines( it_existe_zglt0004 ).
          IF qtd_estorno_1 > 0.
            lo_report3->estorna_doc_receita_custo( 'BT_ESTORNO_1' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Estornar Documento Receita!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_ESTORNO_2'." 'Estornar Documento Custo' c_ge_cus
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_2 estorno_2 waers.
          DELETE it_existe_zglt0004 WHERE obj_key_2 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_2 IS NOT INITIAL. "Não pode ter estorno
          DELETE it_existe_zglt0004 WHERE waers <> 'USD'. "Tem que ter só essa moeda

          DATA(qtd_estorno_2) = lines( it_existe_zglt0004 ).
          IF qtd_estorno_2 > 0.
            lo_report3->estorna_doc_receita_custo( 'BT_ESTORNO_2' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Estornar Documento Custo!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_ESTORNO_3'."'Estornar Reverter Documento Receita' c_er_rec
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_1 estorno_1.
          DELETE it_existe_zglt0004 WHERE obj_key_1 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_1 IS NOT INITIAL. "Não pode ter estorno

          DATA(qtd_estorno_3) = lines( it_existe_zglt0004 ).
          IF qtd_estorno_3 > 0.
            lo_report3->estorna_doc_receita_custo( 'BT_ESTORNO_3' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Estornar a Reverção Documento Receita!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_ESTORNO_4'." 'Estornar Reverter Documento Custo' c_er_cus
        IF it_existe_zglt0004 IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Favor, gerar Receita ou Custo para prosseguir!'.
        ELSE.
          DELETE ADJACENT DUPLICATES FROM it_existe_zglt0004 COMPARING obj_key_2 estorno_2 waers.
          DELETE it_existe_zglt0004 WHERE obj_key_2 IS INITIAL. "Não pode ser vazio
          DELETE it_existe_zglt0004 WHERE estorno_2 IS NOT INITIAL. "Não pode ter estorno
          DELETE it_existe_zglt0004 WHERE waers <> 'USD'. "Tem que ter só essa moeda

          DATA(qtd_estorno_4) = lines( it_existe_zglt0004 ).
          IF qtd_estorno_4 > 0.
            lo_report3->estorna_doc_receita_custo( 'BT_ESTORNO_4' ).
            lo_report3->get_data( ).
            lo_report3->o_alv->refresh( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel Estornar a Reverção Documento Custo!'.
          ENDIF.
        ENDIF.
      WHEN 'BT_REPROCESSAR'.
        DATA: wa_saida3 TYPE ty_saida3.

        CLEAR: wa_saida3.
        LOOP AT it_saida3 INTO wa_saida3 WHERE status_1 = icon_led_red OR status_2 = icon_led_red.
          IF wa_saida3-status_1 IS NOT INITIAL.
            UPDATE zib_contabil SET rg_atualizado = abap_false WHERE obj_key = wa_saida3-obj_key_1.
            COMMIT WORK AND WAIT.
          ENDIF.
          IF wa_saida3-status_2 IS NOT INITIAL.
            UPDATE zib_contabil SET rg_atualizado = abap_false WHERE obj_key = wa_saida3-obj_key_2.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDLOOP.
      WHEN 'BT_LIMPAR'.

        CLEAR: wa_saida3.
        DATA(_delete) = abap_false.
        READ TABLE it_saida3 INTO wa_saida3 INDEX 1.
        IF ( wa_saida3-doc_rec IS NOT INITIAL AND wa_saida3-estorno_1 IS NOT INITIAL ) AND ( wa_saida3-doc_cust IS NOT INITIAL AND wa_saida3-estorno_2 IS NOT INITIAL ).
          _delete = abap_true.
        ELSE.
          IF ( wa_saida3-doc_rec IS NOT INITIAL AND wa_saida3-estorno_1 IS NOT INITIAL ) AND ( wa_saida3-doc_cust IS INITIAL AND wa_saida3-estorno_2 IS INITIAL ).
            _delete = abap_true.
          ENDIF.
          IF ( wa_saida3-doc_rec IS INITIAL AND wa_saida3-estorno_1 IS INITIAL ) AND ( wa_saida3-doc_cust IS NOT INITIAL AND wa_saida3-estorno_2 IS NOT INITIAL ).
            _delete = abap_true.
          ENDIF.
        ENDIF.
        IF _delete = abap_true.
          DELETE FROM zglt0004 WHERE gjahr = p_ano AND poper = p_poper AND bukrs = p_bukrs-low.
          COMMIT WORK AND WAIT.
          lo_report3->get_data( ).
          lo_report3->o_alv->refresh( ).
        ENDIF.
      WHEN 'BT_REFRESH'.
        lo_report3->get_data( ).
        lo_report3->o_alv->refresh( ).
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

*        clear mt_toolbar.
*        mt_toolbar-butn_type = '0'.   "normal Button
*        mt_toolbar-function = 'INSERT_ROW'.   "fcode
*        mt_toolbar-icon = '@B_INSR@'.
*        mt_toolbar-quickinfo = 'Inserir linha'.
*        append mt_toolbar to e_object->mt_toolbar.

  ENDMETHOD.

  METHOD generate_output.

    container_main = NEW cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      "lifetime       =  cl_gui_container=>lifetime_dynpro
      container_name = 'CONTAINER3'
    ).

*** Cria Splitter Container
*  create object painel_control
*    exporting
*      parent  = container_main
*      rows    = 1
*      columns = 1
*      align   = 70.
**
*** Exibe Painel 1
*  call method painel_control->get_container
*    exporting
*      row       = 1
*      column    = 1
*    receiving
*      container = painel1.

    DATA: lx_msg TYPE REF TO cx_salv_msg.


    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = container_main
            container_name = 'CONTAINER3'
          IMPORTING
            r_salv_table   = o_alv
          CHANGING
            t_table        = it_saida3 ).
      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.

    CALL METHOD set_pf_status
      CHANGING
        co_alv = o_alv.

    CALL METHOD set_layout
      CHANGING
        co_alv = o_alv.

    CALL METHOD set_handler
      CHANGING
        co_alv    = o_alv
        co_report = lo_report3.

    CALL METHOD me->set_columns_build
      CHANGING
        co_alv = o_alv.

*    data lr_display_settings  type ref to cl_salv_display_settings.
*    data l_title              type lvc_title.
*    l_title = |Nome Relatório|.
*    lr_display_settings = o_alv->get_display_settings( ).
*    lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*    lr_display_settings->set_list_header( l_title ).
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*    lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ). "Enable Zebra Layout
    DATA lr_selections        TYPE REF TO cl_salv_selections.
* Enable cell selection mode
    lr_selections = o_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    o_alv->display( ).

*    call method set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      changing
*        co_alv = o_alv.


  ENDMETHOD.

  METHOD set_handler.
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

    READ TABLE it_saida3 INTO DATA(wa_saida3) INDEX row.
    IF sy-subrc IS INITIAL.
      CASE column.
        WHEN 'STATUS_1' OR 'STATUS_2'.
          TYPES: BEGIN OF ty_zib_err,
                   obj_key        TYPE zib_contabil_err-obj_key,
                   dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
                   hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
                   message        TYPE zib_contabil_err-message,
                 END OF ty_zib_err.

          DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

          IF column = 'STATUS_1'.
            SELECT obj_key, dt_atualizacao, hr_atualizacao, message
            FROM zib_contabil_err INTO TABLE @lit_zib_err
            WHERE obj_key = @wa_saida3-obj_key_1.
          ENDIF.

          IF column = 'STATUS_2'.
            SELECT obj_key, dt_atualizacao, hr_atualizacao, message
            FROM zib_contabil_err APPENDING TABLE @lit_zib_err
            WHERE obj_key = @wa_saida3-obj_key_2.
          ENDIF.

          IF lit_zib_err[] IS NOT INITIAL.
            cl_demo_output=>new(
            )->begin_section( `ZIB_CONTABIL_ERR:`
            )->write_text( |Erros encontrados NA crição DO documento: \n|
            ")->WRITE_DATA( SY-DATUM
            )->write_data( lit_zib_err[]
            )->end_section(
            )->display( ).
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nenhum resultado encontrado!'.
          ENDIF.
        WHEN 'DOC_REC'.
          IF wa_saida3-doc_rec IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-doc_rec.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_CUST'.
          IF wa_saida3-doc_cust IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-doc_cust.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_REV_R'.
          IF wa_saida3-doc_rev_r IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-doc_rev_r.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_REV_C'.
          IF wa_saida3-doc_rev_c IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-doc_rev_c.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'ESTORNO_1'.
          IF wa_saida3-estorno_1 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-estorno_1.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'ESTORNO_2'.
          IF wa_saida3-estorno_2 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-estorno_2.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'ESTORNO_3'.
          IF wa_saida3-estorno_3 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-estorno_3.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'ESTORNO_4'.
          IF wa_saida3-estorno_4 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida3-estorno_4.
            SET PARAMETER ID 'BUK' FIELD wa_saida3-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida3-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD set_layout.
*
    DATA: lo_layout  TYPE REF TO cl_salv_layout,
          lf_variant TYPE slis_vari,
          ls_key     TYPE salv_s_layout_key.
*   get layout object
    lo_layout = co_alv->get_layout( ).
*   set Layout save restriction
*   1. SET Layout Key .. Unique key identifies the Differenet ALVs
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
    lf_variant = 'DEFAULT'.
    lo_layout->set_initial_layout( lf_variant ).
  ENDMETHOD.

ENDCLASS.
