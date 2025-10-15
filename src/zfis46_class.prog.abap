*&---------------------------------------------------------------------*
*& Include          ZFIS46_CLASS
*&---------------------------------------------------------------------*
    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container.

    CLASS lcl_report DEFINITION DEFERRED.
    DATA: lo_report TYPE REF TO lcl_report.
    CLASS lcl_report DEFINITION.
      PUBLIC SECTION .

        DATA: o_alv    TYPE REF TO cl_salv_table,
              it_saida TYPE STANDARD TABLE OF zfise46 INITIAL SIZE 0,
              wa_saida TYPE STANDARD TABLE OF zfise46 INITIAL SIZE 0,
              t_salv   TYPE STANDARD TABLE OF REF TO cl_salv_table.
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
          set_columns_position
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_f4
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

    DATA: event_handler  TYPE REF TO lcl_report. "AJUSTE - HYPERLINK - MMSILVA - 07.03.2025

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



*    BREAK-POINT.
*
*    LOOP AT it_changed INTO wa_saida.
*
**      SELECT SINGLE * FROM p_businessplace_vh WHERE bukrs = @wa_saida-bukrs AND branch = @wa_saida-branch INTO @DATA(lr_validacao).
**
**      IF sy-subrc <> 0.
**        MESSAGE 'Filial Não pertence a empresa!' TYPE 'I'.
***        LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<_clear>).
***          IF <_clear>-fieldname = 'BRANCH'.
***          ENDIF.
***        ENDLOOP.
**      ENDIF.
*
*    ENDLOOP.

      ENDMETHOD.

      METHOD set_columns_build .
*
*...Get all the Columns
        DATA: lo_cols   TYPE REF TO cl_salv_columns_table,
              lo_column TYPE REF TO cl_salv_column_table,
              gr_events TYPE REF TO cl_salv_events_table.

        lo_cols = co_alv->get_columns( ).
        "lo_cols->set_optimize( abap_false ).

        TRY.

*            lo_column = lo_cols->get_column( 'BASE_DIFAL_SAP' ).
*            lo_column->set_visible( if_salv_c_bool_sap=>false ).

            lo_column ?= lo_cols->get_column( 'CHAVE_NFE_SAP' ).
            lo_column->set_short_text( 'Chv.NFESAP' ).
            lo_column->set_medium_text( 'Chave NFE SAP' ).
            lo_column->set_long_text( 'Chave NFE SAP' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '44' ).


            lo_column ?= lo_cols->get_column( 'SEQ_SAP' ).
            lo_column->set_short_text( 'Seq SAP' ).
            lo_column->set_medium_text( 'Seq SAP' ).
            lo_column->set_long_text( 'Seq SAP' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'EMPRESA' ).
            lo_column->set_short_text( 'Empresa' ).
            lo_column->set_medium_text( 'Empresa' ).
            lo_column->set_long_text( 'Empresa' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '6' ).

            lo_column ?= lo_cols->get_column( 'FILIAL' ).
            lo_column->set_short_text( 'Filial' ).
            lo_column->set_medium_text( 'Filial' ).
            lo_column->set_long_text( 'Filial' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '6' ).

            lo_column ?= lo_cols->get_column( 'DOCNUM' ).
            lo_column->set_short_text( 'Docnum' ).
            lo_column->set_medium_text( 'Docnum' ).
            lo_column->set_long_text( 'Docnum' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).
            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            gr_events = co_alv->get_event( ).
            CREATE OBJECT event_handler.
            SET HANDLER event_handler->on_link_click FOR gr_events.

            lo_column ?= lo_cols->get_column( 'COD_FORNECEDOR' ).
            lo_column->set_short_text( 'Cd.Forn.' ).
            lo_column->set_medium_text( 'Cod.Fornec.' ).
            lo_column->set_long_text( 'Código Fornecedor' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'IVA_ENTRADA' ).
            lo_column->set_short_text( 'Iva Entr.' ).
            lo_column->set_medium_text( 'Iva Entrada' ).
            lo_column->set_long_text( 'Iva Entrada' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'UTILIZ_MATERIAL' ).
            lo_column->set_short_text( 'Util.Mat' ).
            lo_column->set_medium_text( 'Utiliz. Mat.' ).
            lo_column->set_long_text( 'Utilização Material' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'MATERIAL' ).
            lo_column->set_short_text( 'Material' ).
            lo_column->set_medium_text( 'Material' ).
            lo_column->set_long_text( 'Material' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DESCRICAO' ).
            lo_column->set_short_text( 'Desc.Mat.' ).
            lo_column->set_medium_text( 'Desc.Material' ).
            lo_column->set_long_text( 'Descrição Material' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'NCM' ).
            lo_column->set_short_text( 'NCM' ).
            lo_column->set_medium_text( 'NCM' ).
            lo_column->set_long_text( 'NCM' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'BASE_DIFAL' ).
            lo_column->set_short_text( 'Base Difal' ).
            lo_column->set_medium_text( 'Base Difal' ).
            lo_column->set_long_text( 'Base Difal' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ALIQ_DIFAL' ).
            lo_column->set_short_text( 'Aliq.Difal' ).
            lo_column->set_medium_text( 'Aliq.Difal' ).
            lo_column->set_long_text( 'Aliq.Difal' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VALOR_DIFAL' ).
            lo_column->set_short_text( 'Vlr Difal' ).
            lo_column->set_medium_text( 'Vlr Difal' ).
            lo_column->set_long_text( 'Vlr Difal' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DOC_CONTABIL' ).
            lo_column->set_short_text( 'Doc.Cont.' ).
            lo_column->set_medium_text( 'Doc.Contabil' ).
            lo_column->set_long_text( 'Doc. Contabil' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).
            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            gr_events = co_alv->get_event( ).
            CREATE OBJECT event_handler.
            SET HANDLER event_handler->on_link_click FOR gr_events.

            lo_column ?= lo_cols->get_column( 'DOC_ORIGEM' ).
            lo_column->set_short_text( 'Doc.Orig.' ).
            lo_column->set_medium_text( 'Doc.Origem' ).
            lo_column->set_long_text( 'Doc. Origem' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' )..

            lo_column ?= lo_cols->get_column( 'TIPO_DOC' ).
            lo_column->set_short_text( 'Doc.Tipo' ).
            lo_column->set_medium_text( 'Doc. Tipo' ).
            lo_column->set_long_text( 'Doc. Tipo' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'SIMPLES_NACIONAL' ).
            lo_column->set_short_text( 'Simp.Nac.' ).
            lo_column->set_medium_text( 'Simples Nacional' ).
            lo_column->set_long_text( 'Simples Nacional' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'NOTA_FISCAL' ).
            lo_column->set_short_text( 'NF' ).
            lo_column->set_medium_text( 'NF' ).
            lo_column->set_long_text( 'NF' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'SERIE' ).
            lo_column->set_short_text( 'Serie' ).
            lo_column->set_medium_text( 'Serie' ).
            lo_column->set_long_text( 'Serie' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DATA_EMISSAO' ).
            lo_column->set_short_text( 'Dt.Emiss.' ).
            lo_column->set_medium_text( 'Dt. Emissão' ).
            lo_column->set_long_text( 'Dt. Emissão' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DATA_LANCAMENTO' ).
            lo_column->set_short_text( 'Dt.Lanc.' ).
            lo_column->set_medium_text( 'Dt. Lançamento' ).
            lo_column->set_long_text( 'Dt. Lançamento' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'UF_EMISSOR' ).
            lo_column->set_short_text( 'UF Emissor' ).
            lo_column->set_medium_text( 'UF Emissor' ).
            lo_column->set_long_text( 'UF Emissor' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'UF_DESTINO' ).
            lo_column->set_short_text( 'UF Destino' ).
            lo_column->set_medium_text( 'UF Destino' ).
            lo_column->set_long_text( 'UF Destino' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'CFOP_ENTRADAVE_NFE' ).
            lo_column->set_short_text( 'CFOP NF' ).
            lo_column->set_medium_text( 'CFOP NF' ).
            lo_column->set_long_text( 'CFOP NF' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'QTD_SAP' ).
            lo_column->set_short_text( 'Qtd SAP' ).
            lo_column->set_medium_text( 'Qtd SAP' ).
            lo_column->set_long_text( 'Qtd SAP' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VLR_TOT_ITEM_SAP' ).
            lo_column->set_short_text( 'VlrTotSAP' ).
            lo_column->set_medium_text( 'Vlr Tot. SAP' ).
            lo_column->set_long_text( 'Vlr Tot. SAP' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ALIQ_ICMS' ).
            lo_column->set_short_text( 'Aliq.ICMS' ).
            lo_column->set_medium_text( 'Aliq.ICMS' ).
            lo_column->set_long_text( 'Aliq.ICMS' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VALOR_ICMS' ).
            lo_column->set_short_text( 'Vlr ICMS' ).
            lo_column->set_medium_text( 'Vlr ICMS' ).
            lo_column->set_long_text( 'Vlr ICMS' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'CHAVE_NFE_XML' ).
            lo_column->set_short_text( 'Chv.NFEXML' ).
            lo_column->set_medium_text( 'Chave NFE XML' ).
            lo_column->set_long_text( 'Chave NFE XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '44' ).

            lo_column ?= lo_cols->get_column( 'SEQ_XML' ).
            lo_column->set_short_text( 'Seq XML' ).
            lo_column->set_medium_text( 'Seq XML' ).
            lo_column->set_long_text( 'Seq XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'CFOP_XML' ).
            lo_column->set_short_text( 'CFOP XML' ).
            lo_column->set_medium_text( 'CFOP XML' ).
            lo_column->set_long_text( 'CFOP XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'QTD_XML' ).
            lo_column->set_short_text( 'Qtd XML' ).
            lo_column->set_medium_text( 'Qtd XML' ).
            lo_column->set_long_text( 'Qtd XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VLR_TOT_ITEM_XML' ).
            lo_column->set_short_text( 'VlrTotXML' ).
            lo_column->set_medium_text( 'Vlr Tot. XML' ).
            lo_column->set_long_text( 'Vlr Tot. XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DESCR_MAT_XML' ).
            lo_column->set_short_text( 'DescMatXML' ).
            lo_column->set_medium_text( 'Desc.Mat.XML' ).
            lo_column->set_long_text( 'Desc.Mat.XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ICMS_BASE_VALOR' ).
            lo_column->set_short_text( 'ICMSBasVlr' ).
            lo_column->set_medium_text( 'ICMS Base Vlr' ).
            lo_column->set_long_text( 'ICMS Base Vlr' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ALIQ_ICMS_XML' ).
            lo_column->set_short_text( 'Al.ICMSXML' ).
            lo_column->set_medium_text( 'Aliq. ICMS XML' ).
            lo_column->set_long_text( 'Aliq. ICMS XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'RED_BASE_XML' ).
            lo_column->set_short_text( 'RedBaseXML' ).
            lo_column->set_medium_text( 'Red Base XML' ).
            lo_column->set_long_text( 'Red Base XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VALOR_ICMS_XML' ).
            lo_column->set_short_text( 'VlrICMSXML' ).
            lo_column->set_medium_text( 'Vlr ICMS XML' ).
            lo_column->set_long_text( 'Vlr ICMS XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'DESCONTO_XML' ).
            lo_column->set_short_text( 'Desct.XML' ).
            lo_column->set_medium_text( 'Desconto XML' ).
            lo_column->set_long_text( 'Desct.XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'PROD_VL_FRETE' ).
            lo_column->set_short_text( 'Frete XML' ).
            lo_column->set_medium_text( 'Frete XML' ).
            lo_column->set_long_text( 'Frete XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'PROD_VL_OUTRO' ).
            lo_column->set_short_text( 'Outros XML' ).
            lo_column->set_medium_text( 'Outros XML' ).
            lo_column->set_long_text( 'Outros XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'PROD_VL_SEGURO' ).
            lo_column->set_short_text( 'Seguro XML' ).
            lo_column->set_medium_text( 'Seguro XML' ).
            lo_column->set_long_text( 'Seguro XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ICMS_ST_VALOR_XML' ).
            lo_column->set_short_text( 'STVLRXML' ).
            lo_column->set_medium_text( 'ICMS ST VLR XML' ).
            lo_column->set_long_text( 'ICMS ST VLR XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ICMS_CST_XML' ).
            lo_column->set_short_text( 'ICMSCSTXML' ).
            lo_column->set_medium_text( 'ICMS CST XML' ).
            lo_column->set_long_text( 'ICMS CST XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'ALIQ_IPI_XML' ).
            lo_column->set_short_text( 'AliqIPIXML' ).
            lo_column->set_medium_text( 'Aliq. IPI XML' ).
            lo_column->set_long_text( 'Aliq. IPI XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VALOR_IPI_XML' ).
            lo_column->set_short_text( 'VlrIPIXML' ).
            lo_column->set_medium_text( 'Vlr IPI XML' ).
            lo_column->set_long_text( 'Vlr IPI XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'NCM_XML' ).
            lo_column->set_short_text( 'NCM XML' ).
            lo_column->set_medium_text( 'NCM XML' ).
            lo_column->set_long_text( 'NCM XML' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'PROD_VLR_UND_TRI' ).
            lo_column->set_short_text( 'VlrUNdTri' ).
            lo_column->set_medium_text( 'Vlr UNd. Tri.' ).
            lo_column->set_long_text( 'Vlr UNd. Tri.' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'PROD_VL_DESCONTO' ).
            lo_column->set_short_text( 'VlrDesc.' ).
            lo_column->set_medium_text( 'Vlr Desconto' ).
            lo_column->set_long_text( 'Vlr Desconto' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).


            lo_column ?= lo_cols->get_column( 'DIFAL_CALC_DUPLA' ).
            lo_column->set_short_text( 'DIFALCALC' ).
            lo_column->set_medium_text( 'DIFAL CALC DUPLA' ).
            lo_column->set_long_text( 'DIFAL CALC DUPLA' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VAR_ICMS_INTERNO' ).
            lo_column->set_short_text( 'VARICMSINT' ).
            lo_column->set_medium_text( 'VAR ICMS INTERNO' ).
            lo_column->set_long_text( 'VAR ICMS INTERNO' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'VAR_ICMS_INTEREST' ).
            lo_column->set_short_text( 'VARINTREST' ).
            lo_column->set_medium_text( 'VAR ICMS INTEREST' ).
            lo_column->set_long_text( 'VAR ICMS INTEREST' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).

            lo_column ?= lo_cols->get_column( 'AJUSTE' ).
            lo_column->set_short_text( 'Ajuste' ).
            lo_column->set_medium_text( 'Ajuste' ).
            lo_column->set_long_text( 'Ajuste' ).
            lo_column->set_optimized( abap_false ).
            "lo_column->set_alignment( if_salv_c_alignment=>left ).
            lo_column->set_output_length( '10' ).



          CATCH cx_salv_not_found.
        ENDTRY.


      ENDMETHOD.

      METHOD set_edit_alv.
*    DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
*    DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.
*
*    ls_api = co_alv->extended_grid_api( ).
*    ls_edit = ls_api->editable_restricted( ).
*
*    TRY.
*
*        ls_edit->set_attributes_for_columnname( columnname              = 'BUKRS'
*                                                all_cells_input_enabled = abap_true ).
*      CATCH cx_salv_not_found.
*    ENDTRY.
*    ls_edit->validate_changed_data( ).
*    co_alv->refresh( ).
      ENDMETHOD.

      METHOD get_data.

        FREE: it_saida.

        SELECT DISTINCT "BUG #- MMSILVA - 12.06.2025
          a01~chave_nfe AS chave_nfe_sap,
                    0 AS seq_sap,
          c01~bukrs AS empresa,
          c01~branch AS filial,
          f01~docnum AS docnum,
          c01~parid AS cod_fornecedor,
          f01~mwskz AS iva_entrada,
          f01~matuse AS utiliz_material,
          f01~matnr AS material,
          f01~maktx AS descricao,
          f01~nbm AS ncm,
*          SUM( h01~base ) AS base_difal,
          h01~base AS base_difal, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
          h01~rate AS aliq_difal,
*          SUM( h01~taxval ) AS valor_difal,
          h01~taxval AS valor_difal, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
          g01~belnr AS doc_contabil,
          f01~refkey AS doc_origem,
          f01~reftyp AS tipo_doc,
          d01~crtn AS simples_nacional,
          c01~nfenum AS nota_fiscal,
          c01~series AS serie,
          c01~docdat AS data_emissao,
          c01~pstdat AS data_lancamento,
          d01~regio AS uf_emissor,
          e01~regio AS uf_destino,
          f01~cfop AS cfop_entradave_nfe,
*          SUM( f01~menge ) AS qtd_sap,
          f01~menge AS qtd_sap, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
*          SUM( f01~nfnett ) AS vlr_tot_item_sap,
          f01~nfnett AS vlr_tot_item_sap, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
          i01~rate AS aliq_icms,
          SUM( i01~taxval ) AS valor_icms "BUG # - MMSILVA - 12.06.2025
*          i01~taxval AS valor_icms "BUG # - MMSILVA - 12.06.2025
        FROM
          zib_nfe_dist_ter AS a01
          INNER JOIN zib_nfe_forn AS b01 ON b01~nu_chave = a01~chave_nfe
          AND b01~docnum <> ' '
          INNER JOIN j_1bnfdoc AS c01 ON c01~docnum = b01~docnum
          INNER JOIN lfa1 AS d01 ON d01~lifnr = c01~parid
          INNER JOIN t001w AS e01 ON e01~werks = c01~branch
          INNER JOIN j_1bnflin AS f01 ON f01~docnum = b01~docnum
          LEFT OUTER JOIN bkpf AS g01 ON g01~awkey = f01~refkey AND g01~doccat = 'INVREC' "AJUSTE - TRAZER SOMENTE ENTRADA - MMSILVA - 07.03.2025
          LEFT OUTER JOIN j_1bnfstx AS h01 ON h01~docnum = f01~docnum AND h01~taxtyp = 'ICOP'
          AND h01~itmnum = f01~itmnum
          LEFT OUTER JOIN j_1bnfstx AS i01 ON i01~docnum = f01~docnum AND i01~taxgrp = 'ICMS' AND i01~itmnum = f01~itmnum
*          AND i01~itmnum = f01~itmnum AND i01~taxval > 0 AND i01~rate > 0

          WHERE 1 = 1
          AND f01~cfop IN @p_cfop
          AND c01~bukrs IN @p_bukrs
          AND e01~werks IN @p_werks
          AND e01~regio IN @p_regio "AJUSTE - ACRESCENTADO FILTRO UF - MMSILVA - 11.03.2025
          AND substring( c01~pstdat,1,4 ) >= '2024'
*          AND substring( c01~docdat,1,4 ) >= '2024'
*          AND c01~docdat BETWEEN @p_docdat-low AND @p_docdat-high "'20230101' AND '20231231'
          AND c01~docdat IN @p_docdat "AJUSTE - RETIRAR OBRIGATORIEDADE DO CAMPO DATA DO DOCUMENTO - MMSILVA - 28.01.2025
          AND c01~pstdat BETWEEN @p_pstdat-low AND @p_pstdat-high
          AND f01~docnum IN @p_docnum
          AND d01~regio <> e01~regio
*          AND h01~taxval > 0
          AND d01~lifnr IN @p_lifnr
          AND a01~chave_nfe IN @p_chave

*          AND EXISTS (
*            SELECT
*              *
*            FROM
*              bseg
*            WHERE
*              bseg~bukrs = g01~bukrs
*              AND bseg~belnr = g01~belnr
*              AND bseg~gjahr = g01~gjahr
*              AND bseg~hkont = '0000213001'
*          )

          GROUP BY


          a01~chave_nfe,
          c01~bukrs,
          c01~branch,
          f01~docnum,
          c01~parid,
          f01~mwskz,
          f01~matuse,
          f01~matnr,
          f01~maktx,
          f01~nbm,
          g01~belnr,
          f01~refkey,
          f01~reftyp,
          d01~crtn,
          c01~nfenum ,
          c01~series,
          c01~docdat,
          c01~pstdat,
          d01~regio,
          e01~regio,
          f01~cfop,
          h01~rate,
          i01~rate,
          f01~menge,
          f01~nfnett,
          h01~base,
          h01~taxval
*          i01~taxval "BUG # - MMSILVA - 12.06.2025

          INTO TABLE @DATA(it_sap)."@it_SAIDA.

        MOVE-CORRESPONDING it_sap TO it_saida.

        SORT it_SAIDA BY chave_nfe_sap ASCENDING qtd_sap ASCENDING vlr_tot_item_sap ASCENDING.

        DATA: lr_chave TYPE RANGE OF zib_nfe_dist_ter-chave_nfe.
        lr_CHAVE[] = VALUE #( FOR var1 IN it_saida ( option = 'EQ' sign = 'I' low = var1-chave_nfe_sap ) ).

        SORT lr_chave[] BY low ASCENDING.

        DELETE ADJACENT DUPLICATES FROM lr_chave[].

        DATA seq TYPE i.

        IF lr_chave[] IS NOT INITIAL.

          LOOP AT lr_chave[] ASSIGNING FIELD-SYMBOL(<_seq_sap>).
            CLEAR: seq.
            IF <_seq_sap>-low IS NOT INITIAL.
              LOOP AT it_SAIDA ASSIGNING FIELD-SYMBOL(<_seq_sap_index>) WHERE chave_nfe_sap = <_seq_sap>-low.
                IF <_seq_sap_index> IS NOT INITIAL.
                  seq = seq + 1.
                  <_seq_sap_index>-seq_sap = seq.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          DATA: it_xml TYPE STANDARD TABLE OF zfise46_xml INITIAL SIZE 0.
          FREE: it_xml.
          SELECT "DISTINCT "BUG #175081 - MMSILVA - 25.04.2025
                        chave_nfe AS chave_nfe_XML,
            0 AS seq_xml,
            prod_cfop AS cfop_xml,
*            SUM( prod_qtd_comerci ) AS qtd_xml,
            prod_qtd_comerci AS qtd_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
            SUM( prod_vlr_total_b - prod_vl_desconto ) AS vlr_tot_item_xml,
            prod_descricao AS descr_mat_xml,
*            SUM( icms_base ) AS icms_base_valor,
            icms_base AS icms_base_valor, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
            icms_aqt AS aliq_icms_xml,
*            SUM( icms_red_base ) AS red_base_xml,
            icms_red_base AS red_base_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
*            SUM( icms_valor ) AS valor_icms_xml,
            icms_valor AS valor_icms_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
*            prod_vl_desconto AS desconto_xml,
*            SUM( prod_vl_desconto ) AS desconto_xml,
            prod_vl_desconto AS desconto_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
*            SUM( icms_st_valor ) AS icms_st_valor_xml,
            icms_st_valor AS icms_st_valor_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
            icms_cst AS icms_cst_xml,
            ipi_aqt AS aliq_ipi_xml,
*            SUM( ipi_valor ) AS valor_ipi_xml,
            ipi_valor AS valor_ipi_xml, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
            prod_ncm AS ncm_xml,
*            SUM( prod_vlr_und_tri ) AS prod_vlr_und_tri,
            prod_vlr_und_tri AS prod_vlr_und_tri, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
*            SUM( prod_vl_desconto ) AS prod_vl_desconto
            prod_vl_desconto AS prod_vl_desconto, "AJUSTE - LINHAS DUPLICADAS - MMSILVA - 28.01.2025
            prod_item AS itm_num_xml, "BUG #175081 - MMSILVA - 25.04.2025
            prod_vl_frete as item_vl_frete, "BUG  - MMSILVA - 19.05.2025
            prod_vl_seguro as item_vl_seguro, "BUG  - MMSILVA - 19.05.2025
            prod_vl_outro as item_vl_outro "BUG  - MMSILVA - 19.05.2025
          FROM zib_nfe_dist_itm AS a
          WHERE chave_nfe IN @lr_chave[]

            GROUP BY
            chave_nfe,
            prod_cfop,
            prod_descricao,
            icms_aqt,
            icms_cst,
            ipi_aqt,
            prod_ncm,
            prod_qtd_comerci,
            icms_base,
            icms_red_base,
            icms_valor,
            prod_vl_desconto,
            icms_st_valor,
            ipi_valor,
            prod_vlr_und_tri,
            prod_vl_desconto,
            prod_item, "BUG #175081 - MMSILVA - 25.04.2025
            prod_vl_frete, "BUG  - MMSILVA - 19.05.2025
            prod_vl_seguro, "BUG  - MMSILVA - 19.05.2025
            prod_vl_outro "BUG  - MMSILVA - 19.05.2025

          INTO TABLE @it_xml.

        ENDIF.

        SORT it_xml BY chave_nfe_XML ASCENDING qtd_xml ASCENDING vlr_tot_item_xml ASCENDING.

        LOOP AT lr_chave[] ASSIGNING FIELD-SYMBOL(<_seq_xml>).
          CLEAR: seq.
          IF <_seq_xml>-low IS NOT INITIAL.
            LOOP AT it_xml ASSIGNING FIELD-SYMBOL(<_seq_xml_index>) WHERE chave_nfe_XML = <_seq_xml>-low.
              IF <_seq_xml_index> IS NOT INITIAL.
                seq = seq + 1.
                <_seq_xml_index>-seq_xml = seq.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_get_values>).

          READ TABLE it_xml ASSIGNING FIELD-SYMBOL(<_get_value_xml>) WITH KEY chave_nfe_XML = <_get_values>-chave_nfe_sap seq_xml = <_get_values>-seq_SAP .
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <_get_value_xml> TO <_get_values>.
          ENDIF.


          SELECT SINGLE * FROM j_1btxic1 WHERE land1 = 'BR'
            AND shipfrom = @<_get_values>-uf_destino
            AND shipto = @<_get_values>-uf_destino
            INTO @DATA(lr_J_1BTXIC1).

          IF sy-subrc = 0.
            <_get_values>-var_icms_interno = lr_J_1BTXIC1-rate.
          ENDIF.

          DATA var_convenio TYPE P DECIMALS 2.

          SELECT SINGLE *
            FROM j_1btxic3 WHERE land1 = 'BR'
              AND shipfrom = @<_get_values>-uf_emissor
              AND shipto = @<_get_values>-uf_destino
              AND value =   @<_get_values>-ncm
              AND gruop = '41'
              INTO @DATA(lr_J_1BTXIC3).

          IF sy-subrc = 0.
            <_get_values>-var_icms_interest = lr_J_1BTXIC3-rate.
            IF <_get_values>-var_icms_interest = '3.66'.
*              <_get_values>-var_icms_interest = '3.66'.
              var_convenio = '8.8'.
            ELSEIF <_get_values>-var_icms_interest = '1.5'.
*              <_get_values>-var_icms_interest = '1.5'.
              var_convenio = '5.6'.
            ELSE.
              var_convenio = '0'.
            ENDIF.
          ELSE.
            SELECT SINGLE *
                FROM j_1btxic1 WHERE land1 = 'BR'
                  AND shipfrom = @<_get_values>-uf_emissor
                  AND shipto = @<_get_values>-uf_destino
              INTO @DATA(lr_J_1BTXIC1_2).

            IF sy-subrc = 0.
              <_get_values>-var_icms_interest = lr_J_1BTXIC1_2-rate.
            ENDIF.

          ENDIF.

          IF <_get_values>-valor_icms_xml > 0 .
            "Cálculo para NCM que está no Convênio I ou II - Ajustado por MMSILVA - 28.01.2025.
            IF var_convenio IS NOT INITIAL.
              <_get_values>-difal_calc_dupla = ( ( ( <_get_values>-vlr_tot_item_xml + <_get_values>-valor_ipi_xml + <_get_values>-prod_vl_frete + <_get_values>-prod_vl_seguro + <_get_values>-prod_vl_outro ) - <_get_values>-valor_icms_xml ) / ( 1 - (
<_get_values>-var_icms_interno /
100 ) ) * ( ( var_convenio /
100 ) / (
<_get_values>-var_icms_interno /
100 ) ) * (
<_get_values>-var_icms_interno / 100 ) ) -
<_get_values>-valor_icms_xml.
            ELSE.
              <_get_values>-difal_calc_dupla = ( ( ( <_get_values>-vlr_tot_item_xml + <_get_values>-valor_ipi_xml + <_get_values>-prod_vl_frete + <_get_values>-prod_vl_seguro + <_get_values>-prod_vl_outro ) - <_get_values>-valor_icms_xml ) / ( 1 - (
<_get_values>-var_icms_interno / 100 ) ) * ( <_get_values>-var_icms_interno / 100 ) ) -
<_get_values>-valor_icms_xml.
            ENDIF.
*            <_get_values>-difal_calc_dupla = ( <_get_values>-vlr_tot_item_xml - <_get_values>-valor_icms_xml ) / ( 1 - ( <_get_values>-var_icms_interno / 100 ) ) * ( <_get_values>-var_icms_interno / 100 ) - <_get_values>-valor_icms_xml.
          ELSE.
            <_get_values>-difal_calc_dupla = ( ( ( <_get_values>-vlr_tot_item_xml + <_get_values>-valor_ipi_xml + <_get_values>-prod_vl_frete + <_get_values>-prod_vl_seguro + <_get_values>-prod_vl_outro  ) ) / ( 1 - ( <_get_values>-var_icms_interno /
100 ) ) * ( <_get_values>-var_icms_interno / 100 ) ) - ( ( <_get_values>-vlr_tot_item_xml +
<_get_values>-valor_ipi_xml  ) / ( 1 - (
<_get_values>-var_icms_interest / 100 ) ) *
( <_get_values>-var_icms_interest / 100 ) ).
*            <_get_values>-difal_calc_dupla =
*    ( ( ( <_get_values>-vlr_tot_item_xml - ( <_get_values>-vlr_tot_item_xml *  ( <_get_values>-var_icms_interest / 100 ) ) ) / ( 1 - ( <_get_values>-var_icms_interno / 100 ) ) ) * ( <_get_values>-var_icms_interno / 100 ) )
*    - ( <_get_values>-vlr_tot_item_xml * ( <_get_values>-var_icms_interest / 100 ) ).
          ENDIF.

          <_get_values>-ajuste = <_get_values>-valor_difal - <_get_values>-difal_calc_dupla.

          DATA: vazio(1) TYPE c.
          vazio = ''.

*          SELECT SINGLE * FROM zmmt0185 WHERE replace( nbm,'.',@vazio ) = replace( @<_get_values>-ncm_xml,'.',@vazio ) INTO @DATA(lt_zmme0185).
          SELECT SINGLE * FROM zmmt0185 WHERE nbm = @<_get_values>-ncm_xml OR cfop = @<_get_values>-cfop_xml INTO @DATA(lt_zmme0185). "AJUSTE - NÃO APARECER NCM/CFOP INFORMADO NA TABELA ZMMT0185 - MMSILVA -28.01.202

*          IF sy-subrc <> 0.
*            <_get_values>-ajuste = <_get_values>-valor_difal - <_get_values>-difal_calc_dupla.
*          ENDIF.

          "AJUSTE - NÃO APARECER NCM/CFOP INFORMADO NA TABELA ZMMT0185 - MMSILVA -28.01.2025 - Inicio
          IF lt_zmme0185-nbm IS NOT INITIAL.
            DELETE it_saida WHERE ncm_xml  = lt_zmme0185-nbm.
          ENDIF.
          IF lt_zmme0185-cfop IS NOT INITIAL.
            DELETE it_saida WHERE cfop_xml = lt_zmme0185-cfop.
          ENDIF.
          "AJUSTE - NÃO APARECER NCM/CFOP INFORMADO NA TABELA ZMMT0185 - MMSILVA -28.01.2025 - Fim

          CLEAR: LR_J_1BTXIC1, LR_J_1BTXIC1_2, LR_J_1BTXIC3, VAR_CONVENIO, LT_ZMME0185.

        ENDLOOP.




      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
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


      ENDMETHOD.

      METHOD set_columns_position.

        DATA: lo_cols TYPE REF TO cl_salv_columns.


*        lo_cols->set_column_position( columnname = 'CHAVE_NFE_SAP'      position  =  1   ).
*        lo_cols->set_column_position( columnname = 'EMPRESA'            position  = 2   ).
*        lo_cols->set_column_position( columnname = 'FILIAL'             position  = 3   ).
*        lo_cols->set_column_position( columnname = 'DOCNUM'             position  =  4   ).
*        lo_cols->set_column_position( columnname = 'COD_FORNECEDOR'     position  =  5   ).
*        lo_cols->set_column_position( columnname = 'IVA_ENTRADA'        position  =  6   ).
*        lo_cols->set_column_position( columnname = 'UTILIZ_MATERIAL'    position  =  7   ).
*        lo_cols->set_column_position( columnname = 'MATERIAL_SAP'       position  =  8   ).
*        lo_cols->set_column_position( columnname = 'DESCRICAO_MAT'      position  =  9   ).
*        lo_cols->set_column_position( columnname = 'NCM_SAP'            position  = 10    ).
*        lo_cols->set_column_position( columnname = 'BASE_DIFAL'         position  = 11  ).
*        lo_cols->set_column_position( columnname = 'ALIQ_DIFAL'         position  = 12  ).
*        lo_cols->set_column_position( columnname = 'VALOR_DIFAL'        position  = 13  ).
*        lo_cols->set_column_position( columnname = 'DOC_CONTABIL'       position  = 14  ).
*        lo_cols->set_column_position( columnname = 'DOC_ORIGEM'         position  = 15  ).
*        lo_cols->set_column_position( columnname = 'TIPO_DOC'           position  = 16  ).
*        lo_cols->set_column_position( columnname = 'SIMPLES_NACIONAL'   position  = 17  ).
*        lo_cols->set_column_position( columnname = 'NOTA_FISCAL'        position  = 18  ).
*        lo_cols->set_column_position( columnname = 'SERIE'              position  = 19  ).
*        lo_cols->set_column_position( columnname = 'DATA_EMISSAO'       position  = 20  ).
*        lo_cols->set_column_position( columnname = 'DATA_LANCAMENTO'    position  = 21  ).
*        lo_cols->set_column_position( columnname = 'UF_EMISSOR'         position  = 22  ).
*        lo_cols->set_column_position( columnname = 'UF_DESTINO'         position  = 23  ).
*        lo_cols->set_column_position( columnname = 'CFOP_ENTRADAVE_NFE' position  = 24  ).
*        lo_cols->set_column_position( columnname = 'QTD_SAP'            position  = 25  ).
*        lo_cols->set_column_position( columnname = 'ALIQ_ICMS'          position  = 26  ).
*        lo_cols->set_column_position( columnname = 'VLR_ICMS'           position  = 27  ).
*        lo_cols->set_column_position( columnname = 'VALOR_ICMS'         position  = 28  ).
*        lo_cols->set_column_position( columnname = 'VLR_TOT_ITEM_SAP'   position  = 29  ).
*        lo_cols->set_column_position( columnname = 'CFOP_XML'           position  = 30  ).
*        lo_cols->set_column_position( columnname = 'QTD_XML'            position  = 31  ).
*        lo_cols->set_column_position( columnname = 'VLR_TOT_ITEM_XML'   position  = 32  ).
*        lo_cols->set_column_position( columnname = 'DESCR_MAT_XML'      position  = 33  ).
*        lo_cols->set_column_position( columnname = 'ICMS_BASE_VALOR'    position  = 34  ).
*        lo_cols->set_column_position( columnname = 'ALIQ_ICMS_XML'      position  = 35  ).
*        lo_cols->set_column_position( columnname = 'RED_BASE_XML'       position  = 36  ).
*        lo_cols->set_column_position( columnname = 'VALOR_ICMS_XML'     position  = 37  ).
*        lo_cols->set_column_position( columnname = 'DESCONTO_XML'       position  = 38  ).
*        lo_cols->set_column_position( columnname = 'ICMS_ST_VALOR_XML'  position  = 39  ).
*        lo_cols->set_column_position( columnname = 'ICMS_CST_XML'       position  = 40  ).
*        lo_cols->set_column_position( columnname = 'ALIQ_IPI_XML'       position  = 41  ).
*        lo_cols->set_column_position( columnname = 'VALOR_IPI_XML'      position  = 42  ).
*        lo_cols->set_column_position( columnname = 'NCM_XML'            position  = 43  ).
*        lo_cols->set_column_position( columnname = 'PROD_VLR_UND_TRI'   position  = 44  ).
*        lo_cols->set_column_position( columnname = 'PROD_VL_DESCONTO'   position  = 45  ).
*        lo_cols->set_column_position( columnname = 'DIFAL_CALC_DUPLA'   position  = 46  ).
*        lo_cols->set_column_position( columnname = 'VAR_ICMS_INTERNO'   position  = 47  ).
*        lo_cols->set_column_position( columnname = 'VAR_ICMS_INTEREST'  position  = 48  ).
*        lo_cols->set_column_position( columnname = 'AJUSTE'             position  = 49  ).

      ENDMETHOD.


      METHOD set_f4.
*
        DATA: lo_cols TYPE REF TO cl_salv_columns.
        lo_cols = co_alv->get_columns( ).

        DATA: lo_column TYPE REF TO cl_salv_column.
        DATA: lo_column_tab TYPE REF TO cl_salv_column_table.

*      " F4 DDIC

*    DATA lv_ddic TYPE salv_s_ddic_reference.
*
*    TRY.
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'BUKRS' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BUKRS').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'BRANCH' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BRANCH').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'NBM' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFLIN'  field = 'NBM').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*      CATCH cx_root.                                    "#EC NO_HANDLER
*    ENDTRY.


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

*    CALL METHOD set_f4
*      CHANGING
*        co_alv = o_alv.

        CALL METHOD set_columns_position
          CHANGING
            co_alv = o_alv.

        CALL METHOD me->set_columns_build
          CHANGING
            co_alv = o_alv.

        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
        DATA l_title              TYPE lvc_title.
        l_title = |Relatório Difal|.
        lr_display_settings = o_alv->get_display_settings( ).
        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
        lr_display_settings->set_list_header( l_title ).
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

        "Enable Zebra Layout
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
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

*       AJUSTE - HYPERLINK - MMSILVA - 07.03.2025 - Inicio
        DATA: wa_saida TYPE zfise46.
        CLEAR: wa_saida.

        READ TABLE lo_report->it_saida INTO wa_saida INDEX row.
        IF ( sy-subrc IS INITIAL ).
          CASE column.
            WHEN 'DOCNUM'.
              SET PARAMETER ID 'JEF' FIELD wa_saida-docnum.
              CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
              LEAVE SCREEN.
            WHEN 'DOC_CONTABIL'.
              SET PARAMETER ID 'BLN' FIELD wa_saida-doc_contabil.
              SET PARAMETER ID 'BUK' FIELD wa_saida-empresa.
              SET PARAMETER ID 'GJR' FIELD wa_saida-data_lancamento+0(4).
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              LEAVE SCREEN.
          ENDCASE.
        ENDIF.
*       AJUSTE - HYPERLINK - MMSILVA - 07.03.2025 - Fim

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
