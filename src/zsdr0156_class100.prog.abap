CLASS lcl_report_100 DEFINITION.
  PUBLIC SECTION.

    METHODS:
      constructor,
      new_100,
      read_100,
      alv_100,
      limpar_dados_100,
      get_data_01,
      set_refresh_01,
      set_cols_01,
      on_f4_01 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells ,
      on_hotspot_01 FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id e_row_id es_row_no,
      on_toolbar_01 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      on_user_command_01 FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_change_data_01 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed,
      bt_salvar_100,
      bt_editar_100,
      bt_deletar_100,
      disable_fields_100,
      enable_fields_100,
      valida_header_100,
      gerar_ov_01 IMPORTING i_rows TYPE lvc_t_row,
      estornar_ov_01 IMPORTING i_rows TYPE lvc_t_row,
      buscar_tarifa_01 CHANGING ls_ZLEST0055 TYPE zlest0055,
      buscar_taxa_01 EXPORTING ls_ZLEST0055 TYPE zlest0055 CHANGING error TYPE string,
      calcula_total_01 CHANGING error TYPE string,
      prepara_fat_01 EXPORTING linha TYPE lvc_index.

ENDCLASS.
CLASS lcl_report_100 IMPLEMENTATION.
  METHOD alv_100.

    IF main_container IS INITIAL.

      CREATE OBJECT:
      main_container EXPORTING container_name = 'C100_1',
        o_alv_01 EXPORTING i_parent = main_container.

      me->set_cols_01( ).

      CALL METHOD o_alv_01->register_f4_for_fields
        EXPORTING
          it_f4 = it_f4_01.

      SET HANDLER:
              me->on_f4_01 FOR o_alv_01,
              me->on_user_command_01 FOR o_alv_01,
              me->on_hotspot_01 FOR o_alv_01,
              me->on_toolbar_01 FOR o_alv_01,
              me->on_change_data_01 FOR o_alv_01.

      IF it_fieldcat_01 IS NOT INITIAL.

        DATA: ls_layout  TYPE lvc_s_layo. "lvc_s_layo,

        CALL METHOD o_alv_01->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter.

        CALL METHOD o_alv_01->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.

        ls_layout-stylefname = 'CELLTAB'.

        CALL METHOD o_alv_01->set_table_for_first_display
          EXPORTING
            is_layout       = ls_layout
            is_variant      = ls_variant
            "i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_01
            it_fieldcatalog = it_fieldcat_01.

      ENDIF.


    ELSE.

      CALL METHOD o_alv_01->refresh_table_display( ).

    ENDIF.
  ENDMETHOD.

  METHOD new_100.

    DATA: lv_number TYPE string.

    " Get next number using NUMBER_GET_NEXT function
    "Desta forma nunca ira ter concorrência no id!
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ_SD108'
      IMPORTING
        number                  = lv_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      " Clear the number and raise an error message
      CLEAR lv_number.
      MESSAGE e836(sd) WITH 'O intervalo de numeração não foi encontrado!'.
    ELSE.
      zsde0407_header-id_seq = |{ lv_number ALPHA = IN WIDTH = 10 }|.
    ENDIF.

    "SELECT MAX( id_seq )
    "  FROM zsdt0407
    "  INTO TABLE @DATA(lv_seq).

    "IF sy-subrc = 0.
    "  READ TABLE lv_seq INTO DATA(_seq) INDEX 1.
    "  _seq = _seq + 1.
    "ELSE.
    "  _seq = 1.
    "ENDIF.

    "CLEAR: lv_seq.

    "zsde0407_header-id_seq = |{ _seq ALPHA = IN WIDTH = 10 }|."|{ _seq PAD = '0' WIDTH = 10 ALIGN = RIGHT }|.

  ENDMETHOD.


  METHOD read_100.
    CLEAR:zsde0407_header.
    SELECT SINGLE * FROM zsdt0407 WHERE bukrs = @p_bukrs-low AND id_seq = @p_idlot-low  INTO @DATA(ls_zsdt0407).
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_zsdt0407 TO zsde0407_header.

      IF ls_zsdt0407-ztpclass = 'CO'.
        zsde0407_header-ztpclass = 'C'.
      ELSEIF ls_zsdt0407-ztpclass = 'R1'.
        zsde0407_header-ztpclass = 'R'.
      ENDIF.

      CLEAR:ls_zsdt0407.
    ENDIF.

    FREE: it_saida_01.

  ENDMETHOD.


  METHOD on_change_data_01.
    DATA: it_changed TYPE STANDARD TABLE OF zsde0407_alv INITIAL SIZE 0,
          ls_stable  TYPE lvc_s_stbl.
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

    LOOP AT lt_good_cells ASSIGNING FIELD-SYMBOL(<fs_good_cells>).
      IF <fs_good_cells>-fieldname = 'QTD_BASE'.
        CONDENSE <fs_good_cells>-value NO-GAPS.
      ENDIF.
    ENDLOOP.


    READ TABLE lt_good_cells INTO DATA(LS_good_cells) INDEX 1.
    READ TABLE it_changed INTO DATA(LS_changed) INDEX 1.

    DATA: _linha TYPE lvc_index.

    _linha = LS_good_cells-row_id.

    READ TABLE it_saida_01 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX _linha.
    IF sy-subrc IS INITIAL.
      ASSIGN COMPONENT ls_good_cells-fieldname OF STRUCTURE <fs_saida> TO FIELD-SYMBOL(<fs_campo>).
      IF <fs_campo> IS ASSIGNED.
        <fs_campo> = LS_good_cells-value.
      ENDIF.

      IF LS_good_cells IS NOT INITIAL AND ( ls_changed-qtd_base IS NOT INITIAL AND ls_changed-navio IS NOT INITIAL AND ls_changed-locoper IS NOT INITIAL
        AND ls_changed-dt_fat IS NOT INITIAL AND ls_changed-tp_ov IS NOT INITIAL AND ls_changed-moeda_neg IS NOT INITIAL  ).
        me->prepara_fat_01(
          IMPORTING
            linha = _linha
        ).

      ENDIF.

      <fs_saida>-vlr_brl = wa_saida_01-vlr_brl.
      <fs_saida>-vlr_usd = wa_saida_01-vlr_usd.
      <fs_saida>-tx_ov   = wa_saida_01-tx_ov.
      <fs_saida>-unit    = wa_saida_01-unit.
      <fs_saida>-moeda_fat  = wa_saida_01-moeda_fat.

      ls_stable-col = abap_True.
      ls_stable-row = abap_True.

      CALL METHOD o_alv_01->refresh_table_display(
        EXPORTING
          is_stable = ls_stable ).

    ENDIF.
  ENDMETHOD.

  METHOD constructor.

    CASE p_processo.
      WHEN 'NEW'.
        me->new_100( ).
        me->enable_fields_100(  ).
      WHEN 'READ'.

        IF sy-ucomm <> 'ENTER'.
          me->read_100( ).
          IF p_edit = abap_true.
            me->enable_fields_100(  ).
          ELSE.
            me->disable_fields_100(  ).
          ENDIF.
        ENDIF.

        me->get_data_01( ).
      WHEN 'GERAR'.
        me->disable_fields_100(  ).
        me->get_data_01( ).
      WHEN OTHERS.
    ENDCASE.

    me->alv_100( ).

  ENDMETHOD.

  METHOD limpar_dados_100.
    "Limpa tabelas internas
    REFRESH:
    it_f4_01,
    it_fieldcat_01,
    it_saida_01,
    it_screen_status.

    o_alv_01->refresh_table_display( ).

    "Limpa estruturas
    CLEAR:
    wa_saida_01,
    zsde0407_header,
    zsdt0225.

    CLEAR:
    p_bukrs,
    p_bukrs[],
    p_clien,
    p_clien[],
    p_erro,
    p_idlot,
    p_idlot[],
    p_matnr,
    p_matnr[],
    p_perio,
    p_perio[],
    p_safra,
    p_safra[],
    p_werks,
    p_werks[].

    o_alv_01->finalize( ).
    o_alv_01->free( ).
    main_container->finalize( ).
    main_container->free( ).

    FREE: o_alv_01,
           main_container.

    CALL METHOD cl_gui_cfw=>flush.

    LOOP AT SCREEN.
      IF screen-group1 = 'MOD' OR  "Se usar grupos de campos
         screen-group2 = 'SEL'.
        CLEAR screen-input.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_hotspot_01.




    CASE e_column_id-fieldname.
      WHEN 'ID_SEQ'.
        READ TABLE gt_saida_0300 INTO DATA(_row) INDEX e_row_id-index.

        p_idlot-low = _row-id_seq.
        p_bukrs-low = _row-bukrs.

        CALL SCREEN '0100'.
      WHEN 'NR_OV'.

        READ TABLE it_saida_01 ASSIGNING FIELD-SYMBOL(<fs_01>) INDEX e_row_id-index.

        SET PARAMETER ID 'AUN' FIELD <fs_01>-nr_ov.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM'.
        READ TABLE it_saida_01 ASSIGNING <fs_01> INDEX e_row_id-index.

        SET PARAMETER ID 'JEF' FIELD <fs_01>-docnum.
        CALL TRANSACTION 'J1B2N' AND SKIP FIRST SCREEN.


    ENDCASE.


  ENDMETHOD.

  METHOD set_refresh_01.

    DATA: ls_stable TYPE lvc_s_stbl.

    ls_stable-row = abap_true.  "Mantém a ordem das linhas
    ls_stable-col = abap_true.  "Mantém a ordem das colunas

    me->get_data_01( ).
    o_alv_01->refresh_table_display(
      EXPORTING
        is_stable = ls_stable
    ).

  ENDMETHOD.


  METHOD set_cols_01.


    DATA: lo_structdescr   TYPE REF TO cl_abap_structdescr,
          lt_components    TYPE cl_abap_structdescr=>component_table,
          ls_component     TYPE cl_abap_structdescr=>component,
          gt_fieldcat      TYPE lvc_t_fcat,
          gs_fieldcat      TYPE lvc_s_fcat,
          lo_dynamic_table TYPE REF TO data,
          lo_dynamic_line  TYPE REF TO data,
          lt_f4            TYPE STANDARD TABLE OF lvc_s_f4 INITIAL SIZE 0,
          ls_f4            TYPE lvc_s_f4.

    FIELD-SYMBOLS: <lt_table_structure> TYPE STANDARD TABLE,
                   <ls_table_structure> TYPE any.

    " Criar tabela dinâmica baseada em IT_SAIDA
    CREATE DATA lo_dynamic_table TYPE TABLE OF zsde0407_alv.
    ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

    CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
    ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

    " Obter descrição da estrutura de linha
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <ls_table_structure> ).

    lt_components = lo_structdescr->get_components( ).

    IF sy-subrc = 0.

      TYPES: BEGIN OF ty_make_cols,
               fieldname  TYPE lvc_s_fcat-fieldname,
               coltext    TYPE lvc_s_fcat-coltext,
               edit       TYPE lvc_s_fcat-edit,
               outputlen  TYPE lvc_s_fcat-outputlen,
               f4availabl TYPE lvc_s_fcat-f4availabl,
               no_out     TYPE lvc_s_fcat-no_out,
               hotspot    TYPE lvc_s_fcat-hotspot,    "Adicionado campo hotspot
               just       TYPE lvc_s_fcat-just,
               datatype   TYPE lvc_s_fcat-datatype,
               "inttype    TYPE lvc_s_fcat-inttype , "= 'N'.        "Tipo NUMC
               "edit_mask  TYPE lvc_s_fcat-edit_mask, "= '0000000000'.
               "no_zero    TYPE lvc_s_fcat-no_zero, "= ''.         "Mostra zeros
             END OF ty_make_cols.

      DATA: it_make_cols TYPE STANDARD TABLE OF ty_make_cols WITH EMPTY KEY.

      "Basta colocar na sequencia que o key será o index da coluna, não existe a necessidade de colocar a sequencia já que é a ordem!
      it_make_cols = VALUE #(
        ( fieldname =   'ID_SEQ'          coltext = 'ID Seq.'     edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'NR_OV'           coltext = 'Nrº OV'      edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_true  just = 'L')
        ( fieldname =   'FATURA'          coltext = 'Fatura'      edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'DOCNUM'          coltext = 'Doc.Num.'    edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_true  just = 'L')
        ( fieldname =   'NR_NF'           coltext = 'Nrº NF'      edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'QTD_BASE'        coltext = 'Qtd Base'    edit = abap_true    outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'NAVIO'           coltext = 'Navio'       edit = abap_true    outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'LOCOPER'         coltext = 'Local Oper.' edit = abap_true    outputlen = 10  f4availabl = abap_TRUE    no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'DT_FAT'          coltext = 'Data Fatura' edit = abap_true    outputlen = 10  f4availabl = abap_TRUE    no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'TP_OV'           coltext = 'Tipo OV'     edit = abap_true    outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'MOEDA_NEG'       coltext = 'Moeda Neg.'  edit = abap_true    outputlen = 10  f4availabl = abap_true    no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'MOEDA_FAT'       coltext = 'Moeda Fat.'  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'UNIT'            coltext = 'Vlr. Unit.'  edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'TX_OV'           coltext = 'Tax. OV'     edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'VLR_USD'         coltext = 'Vlr. USD'    edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'VLR_BRL'         coltext = 'Vlr. BRL'    edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
                        ).

      CLEAR: gs_fieldcat,ls_f4.

      FREE: gt_fieldcat,it_fieldcat_01,lt_f4.
      DATA: lr_fieldname TYPE RANGE OF abap_compname.
      "EXCLUI AS LINHAS DE GERAR O FIELDCATALOG
      lr_fieldname = VALUE #(
       ( option = 'EQ' sign = 'I' low = 'MANDT' )
       ( option = 'EQ' sign = 'I' low = abap_false )
      ).

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>) WHERE name NOT IN lr_fieldname.

        READ TABLE it_make_cols WITH KEY fieldname = <fs_components>-name INTO DATA(wa_make_cols). "table_line

        IF sy-subrc = 0.

          MOVE-CORRESPONDING wa_make_cols TO gs_fieldcat.
          gs_fieldcat-col_pos = sy-tabix.

*          IF wa_make_cols-fieldname = 'QTD_BASE' OR
*          wa_make_cols-fieldname = 'NAVIO' OR
*          wa_make_cols-fieldname = 'LOCOPER' OR
*          wa_make_cols-fieldname = 'DT_FAT' OR
*          wa_make_cols-fieldname = 'TP_OV' OR
*          wa_make_cols-fieldname = 'MOEDA_NEG'.
*            gs_fieldcat-edit = abap_true.
*          ELSE.
*            gs_fieldcat-edit = abap_false.
*          ENDIF.

          " Obter o descritor do tipo do campo
          DATA(lo_typedesc) = CAST cl_abap_typedescr( <fs_components>-type ).

*          " Preencher o datatype do field catalog dinamicamente
          CASE lo_typedesc->type_kind.
            WHEN cl_abap_typedescr=>typekind_char.
              gs_fieldcat-datatype = 'CHAR'.
            WHEN cl_abap_typedescr=>typekind_num.
              gs_fieldcat-datatype = 'NUMC'.
            WHEN cl_abap_typedescr=>typekind_packed.
              gs_fieldcat-datatype = 'CURR'. " ou 'DEC' dependendo do contexto
            WHEN cl_abap_typedescr=>typekind_date.
              gs_fieldcat-datatype = 'DATS'.
            WHEN cl_abap_typedescr=>typekind_time.
              gs_fieldcat-datatype = 'TIMS'.
            WHEN cl_abap_typedescr=>typekind_int.
              gs_fieldcat-datatype = 'INT4'.
            WHEN cl_abap_typedescr=>typekind_float.
              gs_fieldcat-datatype = 'FLTP'.
            WHEN OTHERS.
              gs_fieldcat-datatype = ''.
          ENDCASE.

          IF  wa_make_cols-f4availabl = abap_true.
            ls_f4-register = abap_true.
            ls_f4-fieldname  = <fs_components>-name.
            APPEND ls_f4 TO lt_f4.
            CLEAR:ls_f4.
          ENDIF.

        ENDIF.

        APPEND gs_fieldcat TO gt_fieldcat.
        CLEAR: gs_fieldcat.

      ENDLOOP.

      DELETE gt_fieldcat WHERE fieldname = abap_false.
      MOVE-CORRESPONDING gt_fieldcat TO it_fieldcat_01.
      MOVE-CORRESPONDING lt_f4 TO it_F4_01.

      FREE: gt_fieldcat, lt_f4.

    ENDIF.

  ENDMETHOD.

  METHOD get_data_01.
    FREE: it_saida_01.
    CLEAR:wa_saida_01.

    DATA: lt_celltab TYPE TABLE OF lvc_s_styl,
          lw_celltab TYPE lvc_s_styl,
          lt_0225 TYPE TABLE of zsdt0225.

    FREE: lt_celltab.

    lw_celltab-fieldname = 'DT_FAT'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 1.

    lw_celltab-fieldname = 'LOCOPER'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 2.

    lw_celltab-fieldname = 'MOEDA_NEG'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 3.

    lw_celltab-fieldname = 'NAVIO'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 4.

    lw_celltab-fieldname = 'QTD_BASE'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 5.

    lw_celltab-fieldname = 'TP_OV'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 5.

    SELECT * FROM zsdt0225 WHERE id_seq = @zsde0407_header-id_seq
      AND bukrs = @zsde0407_header-bukrs
      AND werks = @zsde0407_header-werks
      AND cl_codigo = @zsde0407_header-zkunnr
      AND safra = @zsde0407_header-safra
      AND cod_material = @zsde0407_header-matnr
      INTO TABLE @DATA(lt_zsdt0225).
    IF sy-subrc = 0.

      lt_0225 = lt_zsdt0225.
      SORT lt_0225 BY docnum.
      DELETE ADJACENT DUPLICATES FROM lt_0225 COMPARING docnum.

      SELECT docnum, nfnum
        FROM j_1bnfdoc
        INTO TABLE @DATA(lt_j1_bnfdoc)
        FOR ALL ENTRIES IN @lt_0225
        WHERE docnum = @lt_0225-docnum.
      IF sy-subrc IS INITIAL.
        SORT lt_j1_bnfdoc BY docnum.
      ENDIF.

      LOOP AT lt_ZSDT0225 ASSIGNING FIELD-SYMBOL(<fs_ZSDT0225>).
        MOVE-CORRESPONDING <fs_ZSDT0225> TO wa_saida_01.
        wa_saida_01-locoper = <fs_ZSDT0225>-local_operacao.

        READ TABLE lt_j1_bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j1_bnfdoc>)
        WITH KEY docnum = <fs_ZSDT0225>-docnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida_01-nr_nf = <fs_j1_bnfdoc>-nfnum.
        ENDIF.

        wa_saida_01-qtd_base = <fs_ZSDT0225>-peso_vinculado.
        wa_saida_01-dt_fat = <fs_ZSDT0225>-dt_fatura.
        wa_saida_01-tp_ov = <fs_ZSDT0225>-auart.
        wa_saida_01-moeda_neg = <fs_ZSDT0225>-waerk.
        wa_saida_01-moeda_fat = <fs_ZSDT0225>-waerk_fatura.
        wa_saida_01-unit = <fs_ZSDT0225>-netpr.
        wa_saida_01-tx_ov = <fs_ZSDT0225>-tax_dolar.
        wa_saida_01-vlr_usd = <fs_ZSDT0225>-vlr_usd.
        wa_saida_01-vlr_brl = <fs_ZSDT0225>-vlr_brl.

        IF wa_saida_01-nr_ov IS NOT INITIAL.
          wa_saida_01-celltab = lt_celltab.
        ENDIF.

        APPEND wa_saida_01 TO it_saida_01.
        CLEAR:wa_saida_01.

      ENDLOOP.

      FREE: lt_ZSDT0225.

      IF o_alv_01 IS NOT INITIAL.
        CALL METHOD o_alv_01->refresh_table_display( ).
      ENDIF.

    ENDIF.

*            w_0225-mandt          = sy-mandt.
*            w_0225-id_seq         = zsde0407_header-id_seq.
*            w_0225-lote           = abap_false.
*            w_0225-bukrs          = zsde0407_header-bukrs.
*            w_0225-werks          = zsde0407_header-werks.
*            w_0225-operacao       = abap_false.
*            w_0225-ano_viagem     = wa_saida_01-dt_fat+0(4).
*            w_0225-cl_codigo      = zsde0407_header-zkunnr.
*            w_0225-safra          = zsde0407_header-safra. "abap_false.  "*-IR194132-29.10.2024-#156085-JT
*            w_0225-cod_material   = wl_zlest0059-matnr."Matnr_Ov zsde0407_header-matnr.
*            w_0225-dt_recreg      = |{ dtnow }{ hrnow }|.
*            "w_0225-tp_class       = abap_false.
*            w_0225-nr_dco         = abap_false.
*            w_0225-po_embarque    = abap_false.
*            w_0225-po_destino     = abap_false.
*            w_0225-dt_fatura      = wa_saida_01-dt_fat.
*            w_0225-auart          = wa_saida_01-tp_ov.
*            w_0225-bukrs_serv     = zsde0407_header-bukrs.
*            w_0225-werks_serv     = zsde0407_header-werks.
*            w_0225-waerk          = wa_saida_01-moeda_neg.
*            w_0225-netpr          = wa_saida_01-unit.
*            w_0225-tax_dolar      = wa_saida_01-tx_ov.
*            w_0225-vlr_usd        = wa_saida_01-vlr_usd.
*            w_0225-vlr_brl        = wa_saida_01-vlr_brl.
*            w_0225-vkorg          = wa_zlest0055-vkorg.
*            w_0225-vtweg          = wa_zlest0055-vtweg.
*            w_0225-spart          = wa_zlest0055-spart.
*            w_0225-matnr_ov       = ls_mara-matnr.
*            w_0225-zterm          = wa_zlest0055-zterm.
*            w_0225-nr_ov          = vbeln_ov.
*            w_0225-fatura         = vbeln_fatura.
*            w_0225-docnum         = wa_j_1bnflin-docnum.
*            w_0225-waerk_fatura   = wa_saida_01-moeda_fat.
*            w_0225-navio          = wa_saida_01-navio.
*            w_0225-local_operacao = wa_saida_01-locoper.
*            w_0225-usuario        = usr_reg.
*            w_0225-data_registro  = dtnow.
*            w_0225-hora_registro  = hrnow.

  ENDMETHOD.

  METHOD on_user_command_01.
    DATA: lt_rows  TYPE lvc_t_row,
          ls_row   TYPE lvc_s_roid,
          qtd_rows TYPE int4.

    CALL METHOD o_alv_01->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.
    qtd_rows = lines( lt_rows ).

    CASE e_ucomm.
      WHEN 'ESTORNAR_OV'.
        IF qtd_rows = 0.
          MESSAGE 'Selecione ao menos uma linha para estornar!' TYPE 'I' DISPLAY LIKE 'W'.
        ELSE.
          me->estornar_ov_01( EXPORTING i_rows = lt_rows  ).
        ENDIF.

      WHEN 'GERAR_OV'.
        IF qtd_rows = 0.
          MESSAGE 'Selecione ao menos uma linha para gerar OV!' TYPE 'I' DISPLAY LIKE 'W'.
        ELSE.

          CALL METHOD o_alv_01->get_selected_rows
            IMPORTING
              et_index_rows = lt_rows.

          me->gerar_ov_01( EXPORTING i_rows = lt_rows ).
        ENDIF.

      WHEN 'DELETE_ROW'.
        IF qtd_rows > 0.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
            READ TABLE it_saida_01 ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>-index.

            DELETE FROM zsdt0225 WHERE id_seq = <_del>-id_seq
                                   AND peso_vinculado = <_del>-qtd_base
                                   AND navio          = <_del>-navio
                                   AND local_operacao = <_del>-locoper
                                   AND waerk          = <_del>-moeda_neg.
            IF sy-subrc IS INITIAL.
              COMMIT WORK.
            ENDIF.

          ENDLOOP.

          MESSAGE 'Linhas deletadas com sucesso!' TYPE 'S' DISPLAY LIKE 'E'.

          CALL METHOD me->set_refresh_01( ).
        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

      WHEN 'INSERT_ROW'.

        SELECT *
          FROM zsdt0407
          INTO @DATA(ls_0407)
          UP TO 1 ROWS
          WHERE id_seq = @zsde0407_header-id_seq.
        ENDSELECT.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE 'Necessário salvar primeiro antes de continuar' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CLEAR: wa_saida_01.
        wa_saida_01-id_seq = zsde0407_header-id_seq.
        APPEND wa_saida_01 TO it_saida_01.

        CALL METHOD o_alv_01->refresh_table_display( ).
      WHEN 'REFRESH_GRID'.
        CALL METHOD me->set_refresh_01( ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_toolbar_01.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA

      CASE <fs_tollbar>-function.
        WHEN'&REFRESH'.
          <fs_tollbar>-function = 'REFRESH_GRID'.
        WHEN OTHERS.
          <fs_tollbar>-butn_type = '3'. "Desabilita
      ENDCASE.
    ENDLOOP.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'GERAR_OV'.   "fcode
    mt_toolbar-icon = '@B_TRNS@'.
    mt_toolbar-quickinfo = 'Gerar nova OV'.
    mt_toolbar-text = 'Gerar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'INSERT_ROW'.   "fcode
    mt_toolbar-icon = '@B_INSR@'.
    mt_toolbar-quickinfo = 'Inserir linha'.
*    mt_toolbar-text = 'Estornar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'ESTORNAR_OV'.   "fcode
    mt_toolbar-icon = '@F_UNDO@'.
    mt_toolbar-quickinfo = 'Estornar OV'.
    mt_toolbar-text = 'Estornar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD on_f4_01.
    DATA: lt_return TYPE TABLE OF ddshretval,
          ls_return TYPE ddshretval.
    CLEAR: ls_return.
    FREE: lt_return.
    CLEAR wa_saida_01.

    READ TABLE it_saida_01 ASSIGNING FIELD-SYMBOL(<fs_saida_01>) INDEX es_row_no-row_id.

    IF sy-subrc = 0.

      CASE e_fieldname.
        WHEN 'MOEDA_NEG'.

          SELECT * FROM zi_les_f4_waerk INTO TABLE @DATA(lt_ZI_LES_F4_WAERK).

          IF sy-subrc = 0.
            CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                retfield        = 'MOEDA'
                value_org       = 'S'
              TABLES
                value_tab       = lt_ZI_LES_F4_WAERK
                return_tab      = lt_return
              EXCEPTIONS
                parameter_error = 1
                no_values_found = 2
                OTHERS          = 3.
            IF sy-subrc = 0.
              READ TABLE lt_return INTO ls_return INDEX 1.
              IF sy-subrc = 0.
                DATA(l_MOEDA_NEG) = ls_return-fieldval.

                IF l_MOEDA_NEG IS INITIAL.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  <fs_saida_01>-moeda_neg = l_MOEDA_NEG.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.

        WHEN 'DT_FAT'.

          DATA: lv_date TYPE sy-datum.
          " Chama o calendário padrão SAP
          CALL FUNCTION 'F4_DATE'
*             EXPORTING
*               DATE_FOR_FIRST_MONTH               = SY-DATUM
            IMPORTING
              select_date                  = lv_date
            EXCEPTIONS
              calendar_buffer_not_loadable = 1
              date_after_range             = 2
              date_before_range            = 3
              date_invalid                 = 4
              factory_calendar_not_found   = 5
              holiday_calendar_not_found   = 6
              parameter_conflict           = 7
              OTHERS                       = 8.
          IF sy-subrc = 0.
            <fs_saida_01>-dt_fat = lv_date.
          ENDIF.


        WHEN 'LOCOPER'.

          SELECT * FROM zi_f1_local_operacao_zsdr0156 INTO TABLE @DATA(lt_zhlocoper).

          IF sy-subrc = 0.
            CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                retfield        = 'LOCOPER'
                value_org       = 'S'
              TABLES
                value_tab       = lt_zhlocoper
                return_tab      = lt_return
              EXCEPTIONS
                parameter_error = 1
                no_values_found = 2
                OTHERS          = 3.
            IF sy-subrc = 0.
              READ TABLE lt_return INTO ls_return INDEX 1.
              IF sy-subrc = 0.
                DATA(l_LOCOPER) = ls_return-fieldval.

                IF l_LOCOPER IS INITIAL.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  <fs_saida_01>-locoper = l_LOCOPER.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.

      ENDCASE.

    ENDIF.

    "CALL METHOD me->o_alv_01->refresh_table_display( ).

    DATA: ls_stable TYPE lvc_s_stbl.
    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    CALL METHOD o_alv_01->refresh_table_display(
      EXPORTING
        is_stable = ls_stable
    ).

  ENDMETHOD.

  METHOD bt_salvar_100.

    DATA: wa_zsdt0407 TYPE zsdt0407.
    MOVE-CORRESPONDING zsde0407_header TO wa_zsdt0407.
    wa_zsdt0407-usuario = sy-uname.
    wa_zsdt0407-data_registro = sy-datum.
    wa_zsdt0407-hora_registro = sy-uzeit.

    IF wa_zsdt0407-ztpclass = 'C'.
      wa_zsdt0407-ztpclass = 'CO'.
    ELSEIF wa_zsdt0407-ztpclass = 'R'.
      wa_zsdt0407-ztpclass = 'R1'.
    ELSEIF wa_zsdt0407-ztpclass = 'F' OR wa_zsdt0407-ztpclass = 'Z'.
      CLEAR wa_zsdt0407-ztpclass.
    ENDIF.

    SELECT SINGLE * FROM zsdt0407 INTO @DATA(ls_zsdt0407) WHERE id_seq = @zsde0407_header-id_seq.

    IF sy-subrc <> 0.
      INSERT zsdt0407 FROM wa_zsdt0407.
      COMMIT WORK AND WAIT.
      MESSAGE 'Registro incluido com sucesso!' TYPE 'I'.
    ELSE.
      UPDATE zsdt0407 FROM wa_zsdt0407.
      COMMIT WORK AND WAIT.
      SELECT SINGLE * FROM zsdt0407 WHERE id_seq = @zsde0407_header-id_seq
      INTO CORRESPONDING FIELDS OF @zsde0407_header .

      IF zsde0407_header-ztpclass = 'CO'.
        zsde0407_header-ztpclass = 'C'.
      ELSEIF zsde0407_header-ztpclass = 'R1'.
        zsde0407_header-ztpclass = 'R'.
      ENDIF.

      MESSAGE 'Registro ATUALIZADO com sucesso!' TYPE 'I'.
      CLEAR p_cedit.
    ENDIF.

    CLEAR: p_processo.

  ENDMETHOD.

  METHOD bt_editar_100.

    IF it_saida_01 IS NOT INITIAL.
      MESSAGE | { 'Para o Sequencial :' } { zsde0407_header-id_seq } { 'existem faturamentos, não sendo permitida a ação' } | TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Método para editar dados
    p_cedit = p_cedit + 1.

    IF p_cedit = 1.
      enable_fields_100( ).
      p_edit = abap_true.
    ELSE.
      disable_fields_100( ).
      p_edit = abap_false.
      CLEAR p_cedit.
    ENDIF.

  ENDMETHOD.

  METHOD bt_deletar_100.

    DATA: lv_answer TYPE string.

    IF it_saida_01 IS NOT INITIAL.
      MESSAGE | { 'Para o Sequencial :' } { zsde0407_header-id_seq } { 'existem faturamentos, não sendo permitida a ação' } | TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = 'Tem certeza que deseja deletar?'
        text_button_1  = 'Sim'
        text_button_2  = 'Não'
      IMPORTING
        answer         = lv_answer
*     TABLES
*       PARAMETER      =
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.

      IF lv_answer = '1'.

        DELETE FROM zsdt0407 WHERE id_seq = zsde0407_header-id_seq.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
          me->limpar_dados_100( ).
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD disable_fields_100.
    CLEAR:  p_edit.
    LOOP AT SCREEN.
      " Desabilitar campos específicos por nome ou grupo de tela
      IF screen-name = 'ZSDE0407_HEADER-BUKRS' OR  "screen-name CP '*ZSDE0407_HEADER*'.
        screen-name = 'ZSDE0407_HEADER-WERKS' OR
        screen-name = 'ZSDE0407_HEADER-ZKUNNR' OR
        screen-name = 'ZSDE0407_HEADER-MATNR' OR
        screen-name = 'ZSDE0407_HEADER-SAFRA' OR
        screen-name = 'ZSDE0407_HEADER-ZTPCALC' OR
        screen-name = 'ZSDE0407_HEADER-ZTPCLASS'.
        screen-input = '0'.  " Desabilita entrada
        screen-active = '1'. " Mantém campo visível
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD enable_fields_100.
    CLEAR:  p_edit.
    " Habilitar campos específicos do ZSDE0407_HEADER
    LOOP AT SCREEN.
      " Habilitar campos específicos por nome ou grupo de tela
      IF screen-name = 'ZSDE0407_HEADER-BUKRS' OR  "screen-name CP '*ZSDE0407_HEADER*'.
        screen-name = 'ZSDE0407_HEADER-WERKS' OR
        screen-name = 'ZSDE0407_HEADER-ZKUNNR' OR
        screen-name = 'ZSDE0407_HEADER-MATNR' OR
        screen-name = 'ZSDE0407_HEADER-SAFRA' OR
        screen-name = 'ZSDE0407_HEADER-ZTPCALC' OR
        screen-name = 'ZSDE0407_HEADER-ZTPCLASS'.
        screen-input = '1'.  " Habilita entrada
        screen-active = '1'. " Mantém campo visível
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD valida_header_100 .

    CLEAR: lv_campos_preenchidos.

    IF zsde0407_header-bukrs IS NOT INITIAL.
      IF zsde0407_header-werks IS NOT INITIAL.
        IF zsde0407_header-bukrs+2(2) <> zsde0407_header-werks+0(2).
          CLEAR:zsde0407_header-werks.
          lv_campos_preenchidos = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zsde0407_header-matnr IS NOT INITIAL.
      IF zsde0407_header-matkl IS INITIAL.
        SELECT SINGLE * FROM zi_f1_matr_matkl WHERE matnr = @zsde0407_header-matnr INTO @DATA(ls_F1_MATR_MATKL).
        IF sy-subrc = 0 .
          zsde0407_header-matkl = ls_F1_MATR_MATKL-matkl.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zsde0407_header-zkunnr IS NOT INITIAL.
      IF zsde0407_header-zstcd1 IS INITIAL AND zsde0407_header-zstcd2 IS INITIAL.
        SELECT SINGLE * FROM zi_f1_cliente_fornecedor WHERE zkunnr = @zsde0407_header-zkunnr INTO @DATA(wa_F1_CLIENTE_FORNECEDOR).
        IF sy-subrc = 0.
          zsde0407_header-zstcd1 = wa_F1_CLIENTE_FORNECEDOR-zstcd1.
          zsde0407_header-zstcd2 = wa_F1_CLIENTE_FORNECEDOR-zstcd2.
          zsde0407_header-zstcd3 = wa_F1_CLIENTE_FORNECEDOR-zstcd3.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zsde0407_header-ztpcalc > 0.
      IF zsde0407_header-ztpcalcdesc IS INITIAL.
        SELECT SINGLE * FROM zi_f1_tipo_calculo_zsdr0156 WHERE ztpcalc = @zsde0407_header-ztpcalc INTO @DATA(wa_F1_TIPO_CALCULO_ZSDR0156).
        IF sy-subrc = 0.
          zsde0407_header-ztpcalcdesc = wa_F1_TIPO_CALCULO_ZSDR0156-ztpcalcdesc.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD gerar_ov_01.

    DATA: wl_header_in  TYPE bapisdhd1,
          wl_header_inx TYPE bapisdhd1x,
          wl_order_inx  TYPE bapisdh1x,
          it_return     TYPE STANDARD TABLE OF bapiret2.

    DATA: vlr_icms        TYPE zlest0061-vlr_usd,
          vlr_pis         TYPE zlest0061-vlr_usd,
          vlr_cofins      TYPE zlest0061-vlr_usd,
          vlr_iss         TYPE zlest0061-vlr_usd,
          vlr_liquido     TYPE zlest0061-vlr_usd,
          vl_validto      TYPE j_1btxiss-validto,
          vl_validfrom    TYPE j_1btxiss-validfrom,
          vl_data         TYPE c LENGTH 10,
          vbeln_ov        TYPE bapivbeln-vbeln,
          wg_mensagem(30),
          wl_erro(1),
          lv_msg          TYPE string.


    DATA: it_itemdata     TYPE STANDARD TABLE OF bapisditm,
          wa_itemdata     TYPE bapisditm,
          it_condition    TYPE STANDARD TABLE OF bapicond,
          wl_items_inx    TYPE bapisditmx,
          wa_condition    TYPE bapicond,
          wa_partner      TYPE bapiparnr,
          it_partner      TYPE STANDARD TABLE OF bapiparnr,
          it_items_inx    TYPE STANDARD TABLE OF bapisditmx,
          tl_schedules_in TYPE STANDARD TABLE OF bapischdl,
          wl_schedules_in TYPE bapischdl,
          t_return        TYPE TABLE OF bapireturn1,
          t_success       TYPE TABLE OF bapivbrksuccess,
          t_billing       TYPE TABLE OF bapivbrk,
          _itm_number     TYPE bapisditm-itm_number,
          vbeln_fatura    TYPE vbeln,
          _ref_key        TYPE j_1bnflin-refkey,
          wa_1bapn        TYPE j_1bapn,
          dtnow           TYPE sy-datum,
          hrnow           TYPE sy-uzeit,
          data_reg        TYPE timestamp,
          usr_reg         TYPE sy-uname,
          w_0225          TYPE  zsdt0225,
          _erro(1)        TYPE c.

    LOOP AT i_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
      READ TABLE it_saida_01 INTO wa_saida_01 INDEX <fs_rows>-index.
      IF sy-subrc IS INITIAL.

        IF wa_saida_01-nr_ov IS NOT INITIAL.
          MESSAGE 'Para esta linha já existe ov gerada' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        dtnow = sy-datum.
        hrnow = sy-uzeit.
        data_reg = CONV timestamp( dtnow && hrnow ).
        usr_reg = sy-uname.

        "  *----------------------
        "  * monta cabecalho
        "  *----------------------
        wl_header_in-sales_org   = wa_zlest0055-vkorg. "(org. de venda)
        wl_header_in-distr_chan  = wa_zlest0055-vtweg. "(canal distr.)
        wl_header_in-currency    = wa_zlest0055-waerk_fatura." #161385 / RG w_saida2-waerk. "(moeda.) " gw_saida2_ger_ov-waerk 12.12.16
        wl_header_in-pymt_meth   = 'P'.
        wl_header_in-division    = wa_zlest0055-spart. "(setor atividade)
        wl_header_in-doc_type    = wa_zlest0055-auart. "(tipo de ordem)
        wl_header_in-pmnttrms    = wa_zlest0055-zterm. "ZTERM.
        wl_header_in-exrate_fi   = wa_saida_01-tx_ov."(taxa dolar)
        wl_header_in-bill_date   = wa_saida_01-dt_fat.
        wl_header_in-purch_no_c  = wa_saida_01-navio.
        wl_header_in-purch_no_s  = '.'.
        wl_header_in-fix_val_dy  = wa_saida_01-dt_fat. "VALDT Data efetiva fixa
        wl_header_in-pymt_meth   = ''. "ZLSCH. Forma de pagamento
        wl_header_in-dlvschduse  = wa_zlest0055-vkaus. "VKAUS. Código de utilização
        wl_header_in-incoterms1  = 'SRV'.
        wl_header_in-incoterms2  = 'Serviço'.


        "  *------------------------------------------------
        "  * itens da OV
        "  *------------------------------------------------

        SELECT SINGLE *
        FROM zlest0059
        INTO @DATA(wl_zlest0059)
        WHERE bukrs       EQ @zsde0407_header-bukrs
        AND auart       EQ @wa_zlest0055-auart
        AND po_embarque EQ @abap_off
        AND po_destino  EQ @abap_off.

        IF sy-subrc <> 0.
          MESSAGE | { 'Material para  serviço  não localizado para tipo ov:' } { wa_zlest0055-auart } { 'abra uma FI' } | TYPE 'E' DISPLAY LIKE 'I'.
          RETURN.
        ENDIF.

        "********************************************************************** Start Impostos
        CLEAR: vlr_icms, vlr_pis, vlr_cofins, vlr_iss, vlr_liquido.

        DATA: _lifnr TYPE lifnr.

        _lifnr = |{ zsde0407_header-werks ALPHA = IN WIDTH = 10 }|.

        SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1) WHERE kunnr EQ @zsde0407_header-zkunnr.
        SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1) WHERE lifnr EQ @_lifnr.

        SELECT *
        FROM zsdt0008
       INTO TABLE @DATA(it_zsdt0008)
              WHERE auart      EQ @wa_zlest0055-auart
             AND vkaus      EQ @wa_zlest0055-vkaus
              AND uf_centro  EQ @wa_lfa1-regio
              AND uf_cliente EQ @wa_kna1-regio
              AND mwsk1      EQ 'SD'
              AND ownpr      NE 'X'.



        IF sy-subrc EQ 0.
          SELECT *
          FROM j_1btxsdc
          INTO TABLE @DATA(it_j_1btxsdc)
                FOR ALL ENTRIES IN @it_zsdt0008
                WHERE taxcode   EQ @it_zsdt0008-j_1btxsdc
                AND custusage EQ 1.

          IF sy-subrc EQ 0.
            LOOP AT it_j_1btxsdc INTO DATA(wa_j_1btxsdc).

              "CLEAR: vlr_icms, vlr_pis, vlr_cofins.

              IF wa_j_1btxsdc-icms EQ 'X'.
                SELECT SINGLE *
                FROM j_1btxic1
                INTO @DATA(wa_j_1btxic1)
                      WHERE land1    EQ 'BR'
                      AND shipfrom EQ @wa_lfa1-regio
                      AND shipto   EQ @wa_kna1-regio.

                CASE wa_zlest0055-waerk_fatura.
                  WHEN 'BRL'.
                    vlr_icms = ( wa_saida_01-vlr_brl * wa_j_1btxic1-rate  ) / 100.
                  WHEN 'USD'.
                    vlr_icms = ( wa_saida_01-vlr_usd * wa_j_1btxic1-rate  ) / 100.
                ENDCASE.
              ENDIF.

              IF wa_j_1btxsdc-pis EQ 'X'.
                DATA: wa_j_1btxpis TYPE j_1btxpis.
                SELECT SINGLE *
                FROM j_1btxpis
                INTO @wa_j_1btxpis
                WHERE country    EQ 'BR'
                AND gruop      EQ '72'
                AND value      EQ @zsde0407_header-werks
                AND validto    <= @wa_saida_01-dt_fat
                AND validfrom  >= @wa_saida_01-dt_fat.
                IF sy-subrc IS NOT INITIAL.
                  lv_msg = 'Não encontrado parâmetro J_1BTXPIS!'.
                  lv_msg = lv_msg && | '/Country:' { 'BR' } |.
                  lv_msg = lv_msg && | '/Gruop:' { '72' } |.
                  lv_msg = lv_msg && | '/value:' { zsde0407_header-werks } |.

                  MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

                CASE wa_zlest0055-waerk_fatura.
                  WHEN 'BRL'.
                    vlr_pis = ( ( wa_saida_01-vlr_brl - vlr_icms  ) *  wa_j_1btxpis-rate  ) / 100.
                  WHEN 'USD'.
                    vlr_pis = ( ( wa_saida_01-vlr_usd - vlr_icms ) *  wa_j_1btxpis-rate  ) / 100.
                ENDCASE.
              ENDIF.

              IF ( wa_j_1btxsdc-cofins EQ 'X' ).

                DATA: wa_j_1btxcof TYPE j_1btxcof.

                SELECT SINGLE *
                FROM j_1btxcof
                INTO @wa_j_1btxcof
                WHERE country   EQ 'BR'
                AND gruop     EQ '71'
                AND value     EQ @zsde0407_header-werks
                AND validto   <= @wa_saida_01-dt_fat
                AND validfrom >= @wa_saida_01-dt_fat.
                IF sy-subrc IS NOT INITIAL.
                  lv_msg = 'Não encontrado parâmetro J_1BTXCOF!'.
                  lv_msg = lv_msg && | '/Country:' { 'BR' } |.
                  lv_msg = lv_msg && | '/Gruop:' { '71' } |.
                  lv_msg = lv_msg && | '/value:' { zsde0407_header-werks } |.

                  MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

                CASE wa_zlest0055-waerk_fatura.
                  WHEN: 'BRL'.
                    vlr_cofins  = ( ( wa_saida_01-vlr_brl - vlr_icms ) * wa_j_1btxcof-rate ) / 100.
                  WHEN: 'USD'.
                    vlr_cofins  = ( ( wa_saida_01-vlr_usd - vlr_icms ) * wa_j_1btxcof-rate ) / 100.
                ENDCASE.
              ENDIF.

              IF wa_j_1btxsdc-iss EQ 'X'.
                SELECT SINGLE *
                FROM j_1btxiss
                INTO @DATA(wa_j_1btxiss)
                      WHERE country    EQ 'BR'
                      AND gruop      EQ '73'
                      AND taxjurcode EQ @wa_kna1-txjcd
                      AND value      EQ @wa_lfa1-txjcd
                      AND validto    <= @wa_saida_01-dt_fat
                      AND validfrom  >= @wa_saida_01-dt_fat.

                CASE wa_zlest0055-waerk_fatura. " #161385 / RG
                  WHEN: 'BRL'.
                    vlr_iss   = (  wa_saida_01-vlr_brl * wa_j_1btxiss-rate ) / 100.
                  WHEN: 'USD'.
                    vlr_iss    = ( wa_saida_01-vlr_usd * wa_j_1btxiss-rate ) / 100.
                ENDCASE.
              ENDIF.


              CASE wa_zlest0055-waerk_fatura.
                WHEN: 'BRL'.
                  vlr_liquido = ( wa_saida_01-vlr_brl - vlr_pis - vlr_cofins - vlr_icms - vlr_iss ).
                WHEN: 'USD'.
                  vlr_liquido = ( wa_saida_01-vlr_usd - vlr_pis - vlr_cofins - vlr_icms - vlr_iss ).
              ENDCASE.
            ENDLOOP.
          ENDIF.

        ELSE.

          lv_msg = 'Não encontrado parâmetro ZSDT0011!'.
          lv_msg = lv_msg && | 'Tp.Ov:' { wa_zlest0055-auart } |.
          lv_msg = lv_msg && | '/Utilizador:' { wa_zlest0055-vkaus } |.
          lv_msg = lv_msg && | '/UF Emissor:' { wa_lfa1-regio } |.
          lv_msg = lv_msg && | '/UF Receptor:' { wa_kna1-regio } |.
          lv_msg = lv_msg && | '/Tp. IVA: SD' |.
          lv_msg = lv_msg && | '/Produção: Habilitada:' |.


          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.

        ENDIF.

        "********************************************************************** End Impostos

        CLEAR: wa_itemdata." wl_items_inx, wl_schedules_in, wa_condition.

        SELECT SINGLE * FROM mara WHERE matnr = @wl_zlest0059-matnr INTO @DATA(ls_mara).
        SELECT SINGLE * FROM marc WHERE matnr = @ls_mara-matnr AND werks = @zsde0407_header-werks INTO @DATA(ls_marc).
        SELECT SINGLE * FROM mbew WHERE matnr = @ls_mara-matnr AND bwkey = @zsde0407_header-werks INTO @DATA(ls_mbew).

        CLEAR: _itm_number.

        _itm_number = 10.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = _itm_number
          IMPORTING
            output = wa_itemdata-itm_number.

        wa_itemdata-itm_number  = _itm_number.
        wa_itemdata-material    = wl_zlest0059-matnr. "matnr_ov
        wa_itemdata-plant       = zsde0407_header-werks.
        wa_itemdata-division    = wa_zlest0055-spart. "(setor atividade)
        wa_itemdata-target_qty  = 1.
        wa_itemdata-target_qu   = ls_mara-meins.
        wa_itemdata-sales_unit  = ls_mara-meins.
        wa_itemdata-gross_wght  = wa_saida_01-qtd_base.
        wa_itemdata-net_weight  = wa_saida_01-qtd_base.
        wa_itemdata-untof_wght  = ls_mara-gewei.
        wa_itemdata-fix_val_dy  = wa_saida_01-dt_fat.
        wa_itemdata-price_date  = wa_saida_01-dt_fat.
        wa_itemdata-ex_rate_fi  = wa_saida_01-tx_ov.
        wa_itemdata-dlvschduse  = wa_zlest0055-vkaus.
        wa_itemdata-incoterms1  = 'SRV'.
        wa_itemdata-incoterms2  = 'Serviço'.
        wa_itemdata-purch_no_c  = wa_saida_01-navio.

        "********************************************************************** Start Busca CFOP

        IF wa_lfa1-regio EQ wa_kna1-regio. "Buscra CFOP

          CLEAR: wa_1bapn.

          SELECT SINGLE *
          FROM j_1bapn
          INTO @DATA(ls_1bapn)
          WHERE direct EQ '2'
          AND dstcat EQ '0'
          AND indus3 EQ @ls_marc-indus
          AND itmtyp EQ 'ZH'
          AND ownpro EQ ' '
          AND matuse EQ @ls_mbew-mtuse
          AND indus1 EQ ' '.
        ELSE.
          SELECT SINGLE *
          FROM j_1bapn
          INTO @ls_1bapn
          WHERE direct EQ '2'
          AND dstcat EQ '1'
          AND indus3 EQ @ls_marc-indus
          AND itmtyp EQ 'ZH'
          AND ownpro EQ ' '
         AND matuse EQ @ls_mbew-mtuse
          AND indus1 EQ ' '.
        ENDIF.

        IF ls_1bapn-cfop IS INITIAL.
          CONCATENATE 'Não localizado CFOP para: '
                      wa_lfa1-regio
                      wa_kna1-regio
                      '/ tp_item: ZH'
                      '/ Util. Mat'
                      ls_mbew-mtuse
                      INTO lv_msg SEPARATED BY space.

          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        wa_itemdata-cfop_long      = ls_1bapn-cfop.
        APPEND wa_itemdata        TO it_itemdata.

        "********************************************************************** End Busca CFOP


        wl_items_inx-itm_number    = _itm_number.
        wl_items_inx-target_qty    = 'X'.
        APPEND wl_items_inx       TO it_items_inx.

        wl_schedules_in-itm_number = _itm_number.
        wl_schedules_in-req_qty    = 1.
        APPEND wl_schedules_in    TO  tl_schedules_in.

        wa_condition-itm_number    = _itm_number.
        wa_condition-cond_type     = 'PR00'.

        IF vlr_liquido IS NOT INITIAL.
          wa_condition-cond_value  =  vlr_liquido.
        ELSE.
          IF wa_saida_01-moeda_fat = 'BRL'.
            wa_condition-cond_value = wa_saida_01-vlr_brl.
          ELSE.
            wa_condition-cond_value = wa_saida_01-vlr_usd.
          ENDIF.
        ENDIF.


        wa_condition-currency      = wa_saida_01-moeda_fat.
        wa_condition-cond_unit     = ls_mara-meins.
        APPEND  wa_condition      TO it_condition.


        "  *----------------------
        "  * parceiros
        "  *----------------------
        wa_partner-partn_role = 'AG'.
        wa_partner-partn_numb = zsde0407_header-zkunnr.
        APPEND wa_partner TO it_partner.

        wa_partner-partn_role = 'RE'.
        wa_partner-partn_numb = zsde0407_header-zkunnr.
        APPEND wa_partner TO it_partner.

        wa_partner-partn_role = 'RG'.
        wa_partner-partn_numb = zsde0407_header-zkunnr.
        APPEND wa_partner TO it_partner.

        wa_partner-partn_role = 'WE'.
        wa_partner-partn_numb = zsde0407_header-zkunnr.
        APPEND wa_partner TO it_partner.

        "  *-------------------------------------------
        "  * Criar OV
        "  *-------------------------------------------
        CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
          EXPORTING
            sales_header_in     = wl_header_in
            sales_header_inx    = wl_header_inx
          IMPORTING
            salesdocument_ex    = vbeln_ov
          TABLES
            return              = it_return
            sales_items_in      = it_itemdata
            sales_items_inx     = it_items_inx
            sales_partners      = it_partner
            sales_schedules_in  = tl_schedules_in
            sales_conditions_in = it_condition.

        READ TABLE it_return INTO DATA(wa_return) WITH KEY type = 'E'.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          DATA: wl_msg_ret TYPE zfiwrs0002,
                tg_msg_ret TYPE STANDARD TABLE OF zfiwrs0002.

          LOOP AT it_return INTO wa_return WHERE type <> 'S'.
            wl_msg_ret-msg = wa_return-message.
            APPEND wl_msg_ret TO tg_msg_ret.
          ENDLOOP.

          CHECK tg_msg_ret[] IS NOT INITIAL.

          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen      = '100'
              i_show        = 'X'
              i_repid       = sy-repid
              i_pressed_tab = 'TABSTRIP-ACTIVETAB'
              i_set_field   = 'X_FIELD'
            IMPORTING
              e_messagem    = wg_mensagem
            TABLES
              it_msgs       = tg_msg_ret.


        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          SELECT *
          FROM vbuv
          INTO TABLE @DATA(tl_vbuv)
          WHERE vbeln EQ @vbeln_ov.

          IF sy-subrc IS INITIAL.

            DATA: wl_fieldname TYPE rmdi_name,
                  wl_text      TYPE rmdi_ddtxt.

            LOOP AT  tl_vbuv INTO DATA(wl_vbuv).
              CLEAR: wl_fieldname, wl_text.

              wl_fieldname = wl_vbuv-fdnam.

              CALL FUNCTION 'RM_DDIC_TEXTS_GET'
                EXPORTING
                  i_name                = wl_fieldname
                  i_type                = 'DTEL'
                  i_langu               = sy-langu
                IMPORTING
                  e_ddtxt               = wl_text
                EXCEPTIONS
                  objtype_not_supported = 1
                  illegal_input         = 2
                  OTHERS                = 3.

              IF sy-subrc <> 0.
                CONCATENATE 'Dados incompletos na O.V: ' wl_vbuv-fdnam INTO wl_msg_ret-msg SEPARATED BY space.
              ELSE.
                CONCATENATE 'Dados incompletos na O.V: ' wl_text INTO wl_msg_ret-msg SEPARATED BY space.
              ENDIF.
              APPEND wl_msg_ret TO tg_msg_ret.
            ENDLOOP.

            CHECK tg_msg_ret[] IS NOT INITIAL.

            CALL FUNCTION 'Z_DOC_CHECK_NEW'
              EXPORTING
                i_screen      = '100'
                i_show        = 'X'
                i_repid       = sy-repid
                i_pressed_tab = 'TABSTRIP-ACTIVETAB'
                i_set_field   = 'X_FIELD'
              IMPORTING
                e_messagem    = wg_mensagem
              TABLES
                it_msgs       = tg_msg_ret.

            wl_erro = 'X'.
          ENDIF.

          "  *-------------------------------------------
          "  *-- Elimina OV
          "  *-------------------------------------------
          CASE wl_erro.

            WHEN 'X'.
              REFRESH it_return.
              CLEAR: wl_order_inx .
              wl_order_inx-updateflag = 'D'.

              CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
                EXPORTING
                  salesdocument    = vbeln_ov
                  order_header_inx = wl_order_inx
                TABLES
                  return           = it_return.

              READ TABLE it_return INTO wa_return WITH KEY type = 'S'.

              IF sy-subrc = 0.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.
              ENDIF.

            WHEN OTHERS.
              FREE:t_billing.
              t_billing = VALUE #( ( ref_doc  = |{ vbeln_ov ALPHA = IN WIDTH = 10 }|
              ref_doc_ca   = 'C'
              bill_date    = wa_saida_01-dt_fat   )  ).


              "  *-------------------------------------------
              "  *------ Criar fatura
              "  *-------------------------------------------

              WAIT UP TO 3 SECONDS.

              CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
                TABLES
                  billingdatain = t_billing
                  return        = t_return
                  success       = t_success.

              IF t_success IS NOT INITIAL.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

                TRY.

                    vbeln_fatura = |{ t_success[ 1 ]-bill_doc ALPHA = IN WIDTH = 10 }|.
                    _ref_key = vbeln_fatura.
                  CATCH cx_sy_itab_line_not_found.
                ENDTRY.
              ELSE.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                wl_erro =  abap_true.
              ENDIF.

              WAIT UP TO 12 SECONDS.

              DO 12 TIMES.

                SELECT SINGLE *
                FROM j_1bnflin
                INTO @DATA(wa_j_1bnflin)
                WHERE refkey EQ @_ref_key.
                IF sy-subrc IS NOT INITIAL.
                  WAIT UP TO 1 SECONDS.
                ELSE.
                  EXIT.
                ENDIF.
              ENDDO.

              IF sy-subrc EQ 0.

                CLEAR:w_0225.
                w_0225-mandt          = sy-mandt.
                w_0225-id_seq         = zsde0407_header-id_seq.
                w_0225-lote           = abap_false.
                w_0225-bukrs          = zsde0407_header-bukrs.
                w_0225-werks          = zsde0407_header-werks.
                w_0225-operacao       = abap_false.
                w_0225-ano_viagem     = wa_saida_01-dt_fat+0(4).
                w_0225-cl_codigo      = zsde0407_header-zkunnr.
                w_0225-safra          = zsde0407_header-safra. "abap_false.  "*-IR194132-29.10.2024-#156085-JT
                w_0225-cod_material   = zsde0407_header-matnr."Matnr_Ov zsde0407_header-matnr.
                w_0225-dt_recreg      = |{ dtnow }{ hrnow }|.
                "w_0225-tp_class       = abap_false.
                w_0225-nr_dco         = abap_false.
                w_0225-po_embarque    = abap_false.
                w_0225-po_destino     = abap_false.
                w_0225-dt_fatura      = wa_saida_01-dt_fat.
                w_0225-auart          = wa_saida_01-tp_ov.
                w_0225-bukrs_serv     = zsde0407_header-bukrs.
                w_0225-werks_serv     = zsde0407_header-werks.
                w_0225-waerk          = wa_saida_01-moeda_neg.
                w_0225-peso_vinculado = wa_saida_01-qtd_base.
                w_0225-netpr          = wa_saida_01-unit.
                w_0225-tax_dolar      = wa_saida_01-tx_ov.
                w_0225-vlr_usd        = wa_saida_01-vlr_usd.
                w_0225-vlr_brl        = wa_saida_01-vlr_brl.
                w_0225-vkorg          = wa_zlest0055-vkorg.
                w_0225-vtweg          = wa_zlest0055-vtweg.
                w_0225-spart          = wa_zlest0055-spart.
                w_0225-matnr_ov       = ls_mara-matnr.
                w_0225-zterm          = wa_zlest0055-zterm.
                w_0225-nr_ov          = vbeln_ov.
                w_0225-fatura         = vbeln_fatura.
                w_0225-docnum         = wa_j_1bnflin-docnum.
                w_0225-waerk_fatura   = wa_saida_01-moeda_fat.
                w_0225-navio          = wa_saida_01-navio.
                w_0225-local_operacao = wa_saida_01-locoper.
                w_0225-usuario        = usr_reg.
                w_0225-data_registro  = dtnow.
                w_0225-hora_registro  = hrnow.

                IF zsde0407_header-ztpclass = 'C'.
                  w_0225-tp_class = 'CO'.
                ELSEIF zsde0407_header-ztpclass = 'R'.
                  w_0225-tp_class = 'R1'.
                ELSE.
                  w_0225-tp_class = zsde0407_header-ztpclass.
                ENDIF.

                MODIFY zsdt0225    FROM w_0225.
                COMMIT WORK AND WAIT.

                IF sy-subrc = 0.
                  me->get_data_01(  ).
                  o_alv_01->refresh_table_display(  ).

                  MESSAGE 'Ordem de Venda Gerada com Sucesso!'  TYPE 'S'.
                ELSE.
                  MESSAGE 'Falha ao Gerar Ordem de Venda!'  TYPE 'W'.
                ENDIF.

              ENDIF.
          ENDCASE.

        ENDIF.

      ENDIF.

    ENDLOOP.

    p_processo = 'GERAR'.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'EN'.

  ENDMETHOD.

  METHOD estornar_ov_01.
    DATA: lv_status       TYPE char1,
          lv_msg          TYPE char64,
          lv_bill_doc     TYPE bapivbrksuccess-bill_doc,
          ls_cancelled    TYPE bapivbrkout,
          ls_return       TYPE bapireturn1,
          lt_return       TYPE TABLE OF bapireturn1,
          lt_success      TYPE TABLE OF bapivbrksuccess,
          lv_doc_est      TYPE j_1bnfdoc-docnum,
          lv_doc_number   TYPE j_1bnfdoc-docnum,
          tg_msg_ret      TYPE TABLE OF zfiwrs0002,
          wl_msg_ret      TYPE zfiwrs0002,
          estornado       TYPE c,
          txt_bstkd       TYPE vbkd-bstkd,
          data_ov         TYPE c LENGTH 10,
          wg_mensagem(30),
          tl_msg          TYPE TABLE OF bdcmsgcoll,
          wl_msg          TYPE bdcmsgcoll.

    LOOP AT i_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
      READ TABLE it_saida_01 ASSIGNING FIELD-SYMBOL(<fs_saida1>) INDEX <fs_rows>-index.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE doc_znfw FROM zsdt0225
          INTO @DATA(lv_doc_znfw)
          WHERE nr_ov = @<fs_saida1>-nr_ov
            AND fatura = @<fs_saida1>-fatura
            AND docnum = @<fs_saida1>-docnum.
        IF lv_doc_znfw IS NOT INITIAL.
          MESSAGE 'Nota fiscal registrada na empresa tomadora do serviço, solicite estorno!' TYPE 'E' DISPLAY LIKE 'I'.
          EXIT.
        ELSE.

          CALL FUNCTION 'Z_CONTROLE_FECHAMES'
            EXPORTING
              i_bukrs  = zsde0407_header-bukrs
              i_data   = <fs_saida1>-dt_fat
              i_user   = sy-uname
            IMPORTING
              e_status = lv_status
              e_messa  = lv_msg
            EXCEPTIONS
              error    = 1
              OTHERS   = 2.
          IF lv_msg IS NOT INITIAL.
            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          lv_bill_doc = CONV #( <fs_saida1>-fatura ).

          CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
            EXPORTING
              billingdocument       = lv_bill_doc
            IMPORTING
              billingdocumentdetail = ls_cancelled
              return                = ls_return.


          IF ls_cancelled-cancelled IS INITIAL.
            CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
              EXPORTING
                billingdocument = <fs_saida1>-fatura
              TABLES
                return          = lt_return
                success         = lt_success.
          ENDIF.

          IF lt_success[] IS NOT INITIAL OR ls_cancelled-cancelled IS NOT INITIAL.

            SELECT SINGLE docnum
              FROM j_1bnflin
              INTO @DATA(_docnum)
             WHERE docnum EQ @<fs_saida1>-docnum.

            SELECT SINGLE candat
              FROM j_1bnfdoc
              INTO @DATA(_vcandat)
             WHERE docnum EQ @_docnum.
            IF _vcandat IS INITIAL.

              SELECT SINGLE docnum
                  FROM j_1bnfe_active  INTO @DATA(v_docnum)
               WHERE docnum EQ @_docnum
                AND  docsta EQ '1'
                AND  cancel EQ ''.

              IF sy-subrc NE 0.

                lv_doc_number = _docnum.

                CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
                  EXPORTING
                    doc_number               = lv_doc_number
                    ref_type                 = space
                    ref_key                  = space
                    can_dat                  = sy-datum
                  IMPORTING
                    doc_number               = lv_doc_est
                  EXCEPTIONS
                    document_not_found       = 1
                    cancel_not_possible      = 2
                    nf_cancel_type_not_found = 3
                    database_problem         = 4
                    docum_lock               = 5
                    nfe_cancel_simulation    = 6
                    OTHERS                   = 7.

                IF sy-subrc EQ 0.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                ELSE.
                  DATA(w_erro) = abap_true.
*                  MESSAGE 'Não foi possível cancelar a fatura' TYPE 'S' DISPLAY LIKE 'E'.
*                  RETURN.
                ENDIF.
              ELSE.
                w_erro = abap_true.
*                MESSAGE 'Não foi possível cancelar a fatura' TYPE 'S' DISPLAY LIKE 'E'.
*                RETURN.
              ENDIF.

            ENDIF.

          ENDIF.

          WAIT UP TO 2 SECONDS.

          DELETE lt_return WHERE type NE 'E' .

          IF lt_return IS INITIAL AND w_erro IS NOT INITIAL.

            LOOP AT lt_return  INTO DATA(wa).

              SELECT *
                FROM t100 INTO TABLE @DATA(it_t100)
               WHERE sprsl EQ 'PT'
                AND  arbgb EQ @wa-id
                AND  msgnr EQ @wa-number.

              CHECK it_t100 IS NOT  INITIAL.

              LOOP AT it_t100 INTO DATA(wa_t100).
                CONCATENATE <fs_saida1>-fatura '-' wa_t100-text INTO wl_msg_ret-msg SEPARATED BY space.
                APPEND wl_msg_ret TO tg_msg_ret.
              ENDLOOP.

              IF NOT ( tg_msg_ret[] IS INITIAL ).

                CALL FUNCTION 'Z_DOC_CHECK_NEW'
                  EXPORTING
                    i_screen      = '100'
                    i_show        = 'X'
                    i_repid       = sy-repid
                    i_pressed_tab = 'TABSTRIP-ACTIVETAB'
                    i_set_field   = 'X_FIELD'
                  IMPORTING
                    e_messagem    = wg_mensagem
                  TABLES
                    it_msgs       = tg_msg_ret.
              ELSE.
                estornado = 'X'.
              ENDIF.
            ENDLOOP.
          ELSE.
            estornado = 'X'.
          ENDIF.

          CASE estornado.
            WHEN 'X'.
              REFRESH: tl_msg, tl_bdc.
              CLEAR:  wl_msg, data_ov, estornado.

              CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO data_ov.
              txt_bstkd  = '.'.

              PERFORM f_preencher_dynpro USING:
                      'X' 'SAPMV45A'    '0102',
                      ''  'BDC_CURSOR'  'VBAK-VBELN',
                      ''  'BDC_OKCODE'  '/00',
                      ''  'VBAK-VBELN'  <fs_saida1>-nr_ov,

                      'X' 'SAPMV45A'  '4001',
                      '' 'BDC_OKCODE'  '/00',
                      '' 'BDC_SUBSCR'  'SAPMV45A',
                      '' 'VBKD-BSTKD'	txt_bstkd,
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'KUWEV-KUNNR'  zsde0407_header-zkunnr,
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_CURSOR'	'VBAK-FAKSK',
                      '' 'RV45A-KETDAT'	data_ov,
                      '' 'RV45A-KPRGBZ'	'D',
                      '' 'VBAK-FAKSK'	'03',
                      '' 'VBKD-PRSDT'	data_ov,
                      '' 'VBKD-ZTERM'	'Z150',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_SUBSCR'	'SAPLV45W',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_SUBSCR'	'SAPMV45A',

                      'X' 'SAPMV45A'  '4001',
                      '' 'BDC_OKCODE'	'=SICH',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'VBKD-BSTKD'	txt_bstkd,
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'KUWEV-KUNNR'  zsde0407_header-zkunnr,
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'RV45A-KETDAT'	data_ov,
                      '' 'RV45A-KPRGBZ' 'D',
                      '' 'VBAK-FAKSK'	'03',
                      '' 'VBKD-PRSDT'	data_ov,
                      '' 'VBKD-ZTERM'	'Z150',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_SUBSCR'	'SAPLV45W',
                      '' 'BDC_SUBSCR'	'SAPMV45A',
                      '' 'BDC_CURSOR'	'RV45A-MABNR(02)',
                      '' 'BDC_SUBSCR' 'SAPMV45A'.

              CALL TRANSACTION 'VA02' USING tl_bdc MODE 'S' MESSAGES INTO tl_msg.
              WAIT UP TO 2 SECONDS.

          ENDCASE.

          DELETE FROM zsdt0225
              WHERE nr_ov   = <fs_saida1>-nr_ov
                AND docnum  = <fs_saida1>-docnum
                AND fatura  = <fs_saida1>-fatura.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

            CALL METHOD o_alv_01->set_ready_for_input
              EXPORTING
                i_ready_for_input = 1.

            MESSAGE 'Estorno realizado com sucesso' TYPE 'S'.

            DELETE it_saida_01 INDEX <fs_rows>-index.

            CALL METHOD o_alv_01->refresh_table_display( ).
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD buscar_tarifa_01.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zsde0407_header-matkl
      IMPORTING
        output = zsde0407_header-matkl.

    IF zsde0407_header-ztpclass IS NOT INITIAL.
      IF zsde0407_header-ztpclass EQ 'C'.
        DATA(lv_tpclass) = 'CO'.
      ELSEIF zsde0407_header-ztpclass = 'R'.
        lv_tpclass = 'R1'.
      ENDIF.
    ENDIF.

    SELECT  * FROM zlest0055
    WHERE auart = @wa_saida_01-tp_ov
    AND matkl = @zsde0407_header-matkl
    AND kunnr = @zsde0407_header-zkunnr
    AND waerk = @wa_saida_01-moeda_neg
    AND status = 1
    AND dt_fim >= @wa_saida_01-dt_fat
    AND vkorg  = @zsde0407_header-bukrs
    AND tp_transgenia = @lv_tpclass
    INTO @ls_ZLEST0055
    UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc <> 0.
      SELECT SINGLE stcd1 FROM kna1 WHERE kunnr = @zsde0407_header-zkunnr INTO @DATA(_stcd1).
      IF sy-subrc = 0.
        DATA: _cnpj TYPE kna1-stcd1.
        CLEAR: _cnpj.
        _cnpj = |{ _stcd1 ALPHA = IN WIDTH = 14 }|.
        _cnpj = _cnpj(8) && '0001%'.

        SELECT SINGLE kunnr FROM kna1 INTO @DATA(_kunnr) WHERE stcd1 LIKE @_cnpj.
        IF sy-subrc = 0.
          SELECT SINGLE * FROM zlest0055
          WHERE auart = @wa_saida_01-tp_ov
          AND matkl = @zsde0407_header-matkl
          AND kunnr = @_kunnr
          AND waerk = @wa_saida_01-moeda_neg
          AND status = 1
          AND dt_fim >= @wa_saida_01-dt_fat
          AND vkorg  = @zsde0407_header-bukrs
          AND tp_transgenia = @lv_tpclass
          INTO @ls_zlest0055.

        ELSE.
          MESSAGE |Não localizada tarifa para tipo OV: { wa_saida_01-tp_ov }, grupo material : { zsde0407_header-matkl }, moeda { wa_saida_01-moeda_neg }. | TYPE 'I' DISPLAY LIKE 'W'.
          RETURN.
        ENDIF.

        CASE zsde0407_header-ztpcalc.
          WHEN 1.
            IF ls_zlest0055-unid_medida <> 'TO'. "TO
              MESSAGE |Não localizada tarifa para tipo OV: { wa_saida_01-tp_ov }, grupo material : { zsde0407_header-matkl }, moeda { wa_saida_01-moeda_neg }. , Un. Medida = "TO"| TYPE 'I' DISPLAY LIKE 'W'.
              CLEAR ls_zlest0055.
              RETURN.
            ENDIF.
          WHEN 2.
            IF ls_zlest0055-unid_medida <> 'UN'. "UN
              MESSAGE |Não localizada tarifa para tipo OV: { wa_saida_01-tp_ov }, grupo material : { zsde0407_header-matkl }, moeda { wa_saida_01-moeda_neg }. , Un. Medida = "UN"| TYPE 'I' DISPLAY LIKE 'W'.
              CLEAR ls_zlest0055.
              RETURN.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

      ENDIF.
    ENDIF.

    IF ls_zlest0055 IS NOT INITIAL. "Validaqção Cliente

      wa_saida_01-moeda_fat = ls_zlest0055-waerk_fatura.

      SELECT SINGLE *
      FROM knvv
      WHERE kunnr  EQ @zsde0407_header-zkunnr
      AND vkorg  EQ @ls_zlest0055-vkorg
      AND vtweg  EQ @ls_zlest0055-vtweg
      AND spart  EQ @ls_zlest0055-spart
                  INTO @DATA(ls_knvv).

      IF sy-subrc <> 0.
        MESSAGE |Cliente não expandido para org. venda { ls_zlest0055-vkorg }, canal { ls_zlest0055-vtweg } e setor { ls_zlest0055-spart }.| TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD buscar_taxa_01.

    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          vlr_ukurs       TYPE decfloat34,
          vlr_usd         TYPE decfloat34,
          vlr_brl         TYPE decfloat34,
          lv_un_conv      TYPE zlest0055-waerk.

    CREATE OBJECT obj_zcl_util_sd.

    IF zsde0407_header-ztpcalc = 1.
      IF ls_zlest0055-waerk = 'USD'.
        lv_un_conv = 'BRL'.
        vlr_usd = ( wa_saida_01-qtd_base / 1000 ) * ls_zlest0055-netpr.
      ELSE.
        lv_un_conv = 'USD'.
        vlr_usd = ( wa_saida_01-qtd_base / 1000 ) * ls_zlest0055-netpr.
      ENDIF.
    ELSEIF zsde0407_header-ztpcalc = 2.
      IF ls_zlest0055-waerk = 'USD'.
        lv_un_conv = 'BRL'.
        vlr_usd = ls_zlest0055-netpr.
      ELSE.
        lv_un_conv = 'USD'.
        vlr_usd = ls_zlest0055-netpr.
      ENDIF.
    ENDIF.

    " Busca cotação do USD para calculo do valor em USD
    obj_zcl_util_sd->set_data( EXPORTING i_data = |{ wa_saida_01-dt_fat }| ).
    obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = ls_zlest0055-kurst ).
    obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = ls_zlest0055-waerk ).
    obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = lv_un_conv ).
    obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = vlr_ukurs ).

    IF vlR_ukurs IS NOT INITIAL.

      CASE wa_saida_01-moeda_neg.
        WHEN 'BRL'.
          vlr_usd = ( vlr_brl /  ( vlr_ukurs * -1 ) ).
          wa_saida_01-tx_ov =  ( vlr_ukurs * -1 ).
          wa_saida_01-vlr_brl      = vlr_brl.
          wa_saida_01-vlr_usd      = vlr_usd.
        WHEN 'USD'.
          vlr_brl = vlr_usd * vlR_ukurs.
          wa_saida_01-tx_ov    = vlR_ukurs.
          wa_saida_01-vlr_brl      = vlr_brl.
          wa_saida_01-vlr_usd      = vlr_usd.
        WHEN OTHERS.
      ENDCASE.

    ELSE.
      error = |Taxa do câmbio não cadastrada!|.

    ENDIF.
  ENDMETHOD.

  METHOD calcula_total_01.

    IF zsde0407_header-ztpcalc = 1 AND wa_saida_01-moeda_neg = 'BRL'.
      wa_saida_01-vlr_brl =  ( ( wa_saida_01-qtd_base / 1000 ) *  wa_saida_01-unit ).
    ELSEIF zsde0407_header-ztpcalc = 1 AND wa_saida_01-moeda_neg = 'USD'.
      wa_saida_01-vlr_usd =  ( ( wa_saida_01-qtd_base / 1000 ) * wa_saida_01-unit ).
    ENDIF.

    IF zsde0407_header-ztpcalc = 2 AND wa_saida_01-moeda_neg = 'BRL'.
      wa_saida_01-vlr_brl =  wa_saida_01-unit.
    ELSEIF zsde0407_header-ztpcalc = 2 AND wa_saida_01-moeda_neg = 'USD'.
      wa_saida_01-vlr_usd =   wa_saida_01-unit.
    ENDIF.

    "error = |Messngem de erro|.

  ENDMETHOD.

  METHOD prepara_fat_01.

    CLEAR: wa_saida_01.

    READ TABLE it_saida_01 INTO wa_saida_01 INDEX linha.

    DATA: _msg         TYPE string.
    CLEAR: wa_ZLEST0055.
    me->buscar_tarifa_01( CHANGING ls_zlest0055 = wa_ZLEST0055 ).

    IF wa_ZLEST0055 IS NOT INITIAL.

      wa_saida_01-unit = wa_ZLEST0055-netpr.

      me->buscar_taxa_01(
        IMPORTING
          ls_zlest0055 = wa_ZLEST0055
        CHANGING
          error        = _msg
      ).

      IF _msg IS NOT INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH _msg.
      ELSE.
        me->calcula_total_01(
          CHANGING
            error = _msg
        ).
        IF _msg IS NOT INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH _msg.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
