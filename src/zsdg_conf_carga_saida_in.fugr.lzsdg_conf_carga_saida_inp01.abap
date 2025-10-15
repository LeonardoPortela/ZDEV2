
CLASS lcl_carga DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.

CLASS lcl_carga IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good            TYPE lvc_s_modi,
          lv_value           TYPE lvc_value,
          lv_sol             TYPE vbap-kwmeng,
          lv_roteiro         TYPE zsdt0132-nr_rot,
          lv_total           TYPE zmmt0201-qtd_total_kg,
          lv_valor_un        TYPE zmmt0201-qtd_total_kg,
          lv_tot_kg          TYPE zmmt0202-qtd_vinc_carga,
          lv_menge           TYPE bstmg,
          lv_modif           TYPE c,
          lv_value_old       TYPE lvc_value,
          ls_coluna          TYPE lvc_s_col,
          ls_linha           TYPE lvc_s_row,
          ls_row_no          TYPE lvc_s_roid,
          ls_stable          TYPE lvc_s_stbl,
          lv_total_utilizado TYPE zmmt0218-quantidade.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).
      CLEAR lv_modif.

      IF <ls_good>-value IS INITIAL.
        CONTINUE.
      ENDIF.

      CASE <ls_good>-fieldname.

        WHEN 'QUANTIDADE'.

          READ TABLE t_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para>) INDEX <ls_good>-row_id.
          CHECK sy-subrc EQ 0.

          lv_value_old = <fs_de_para>-quantidade.

          DATA(_error) = abap_false.

          <fs_de_para>-quantidade = <ls_good>-value.

          PERFORM f_valida_qtd_conf USING <fs_de_para> CHANGING _error.

          IF _error EQ abap_true.
            <fs_de_para>-quantidade = lv_value_old.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = <ls_good>-row_id
                i_fieldname = 'QUANTIDADE'
                i_value     = lv_value_old.
          ENDIF.

          DELETE er_data_changed->mt_good_cells INDEX <ls_good>-row_id.

        WHEN 'PESO_CONV'.

          READ TABLE t_notas ASSIGNING FIELD-SYMBOL(<fs_nota>) INDEX <ls_good>-row_id.
          CHECK sy-subrc IS INITIAL.

          <fs_nota>-peso_conv = <ls_good>-value.

          IF <fs_nota>-peso_conv IS INITIAL.
            CLEAR: <fs_nota>-qtd_conv, <fs_nota>-saldo_conf.
            CONTINUE.
          ENDIF.

          <fs_nota>-qtd_conv = <fs_nota>-quantidade / <fs_nota>-peso_conv.

          CLEAR: lv_total_utilizado.

          SELECT *
            FROM zsdt0420 INTO TABLE @DATA(lt_0420)
           WHERE chave_nfe = @<fs_nota>-chave_nfe
             AND prod_item = @<fs_nota>-prod_item
             AND cancel    = @space.

          LOOP AT lt_0420 ASSIGNING FIELD-SYMBOL(<fs_0420>).
            ADD <fs_0420>-quantidade TO lv_total_utilizado.
          ENDLOOP.

          <fs_nota>-saldo_conf = <fs_nota>-qtd_conv - lv_total_utilizado.

      ENDCASE.

    ENDLOOP.

    CALL METHOD go_bordero_1000->refresh_table_display( ).
    CALL METHOD go_carga_1000->refresh_table_display( ).
    CALL METHOD go_notas_1000->refresh_table_display( ).
    CALL METHOD go_de_para_1000->refresh_table_display( ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_aceite DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no.

ENDCLASS.

CLASS lcl_aceite IMPLEMENTATION.

  METHOD handle_on_button_click.

    CASE es_col_id.
      WHEN 'ACAO'.

        READ TABLE t_aceite ASSIGNING FIELD-SYMBOL(<fs_aceite>) INDEX es_row_no-row_id.
        CHECK sy-subrc IS INITIAL.

        CASE <fs_aceite>-acao_bt.
          WHEN 'BORDERO'.
            "PERFORM f_corrigir_bordero USING <fs_aceite>.
          WHEN 'ACEITE'.
            PERFORM f_realiza_aceite_fiscal USING <fs_aceite>.
          WHEN 'CANCELAR'.
            PERFORM f_cancela_aceite_fiscal USING <fs_aceite>.
          WHEN OTHERS.
        ENDCASE.


        PERFORM f_monta_dados_alv_aceite.

        IF t_aceite[] IS INITIAL.
          LEAVE TO SCREEN 0.
        ELSE.
          CALL METHOD go_aceite_1002->refresh_table_display( ).
        ENDIF.

    ENDCASE.


  ENDMETHOD.
ENDCLASS.


CLASS lcl_bordero DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.


CLASS lcl_bordero IMPLEMENTATION.

  METHOD on_f4.


    TYPES: BEGIN OF ty_f4_nfase,
             nr_fase   TYPE zmmt0102-nr_fase,
             categoria TYPE zmmt0102-categoria,
             matnr     TYPE mara-matnr,
             menge     TYPE char18, "zmmt0102-menge,
             lfimg     TYPE char18, "zsdt0134-lfimg,
             sdo_fase  TYPE char18, "zmmt0102-menge,
           END OF ty_f4_nfase.

    DATA: it_ret_7200 TYPE STANDARD TABLE OF ddshretval,
          "it_f4_7200  TYPE STANDARD TABLE OF ty_f4_nlote,
          it_f4_nfase TYPE STANDARD TABLE OF ty_f4_nfase,
          vl_cont     TYPE i,
          it_zsdt0062 TYPE STANDARD TABLE OF zsdt0062,
          it_ekbe_aux TYPE STANDARD TABLE OF ekbe,
          it_mchb     TYPE STANDARD TABLE OF mchb,
          wa_mchb     TYPE mchb,
          it_mch1     TYPE STANDARD TABLE OF mch1,
          wa_mch1     TYPE mch1,
          it_mslb     TYPE STANDARD TABLE OF mslb,
          wa_mslb     TYPE mslb,
          it_zmmt0102 TYPE STANDARD TABLE OF zmmt0102,
          wa_zmmt0102 TYPE zmmt0102.

    DATA: wa_ret       TYPE ddshretval,
          wa_modi      TYPE lvc_s_modi,
          l_lfimg      TYPE zsdt0134-lfimg,
          l_saldo_fase TYPE zsdt0134-lfimg.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA : it_fmap      TYPE STANDARD TABLE OF dselc,
           wa_fmap      TYPE dselc,
           it_field_tab TYPE TABLE OF dfies,
           wa_field_tab TYPE dfies.

    DATA: vg_charg TYPE mch1-charg.

    CASE e_fieldname.

      WHEN 'NR_FASE'.

        READ TABLE t_bordero INTO DATA(lwa_lote_7200) INDEX es_row_no-row_id.
        CHECK sy-subrc IS INITIAL.

        "READ TABLE git_7200_solicitacoes INTO lwa_solicitacao WITH KEY vbeln = lwa_lote_7200-vbeln
        "posnr = lwa_lote_7200-posnr.
        "CHECK sy-subrc EQ 0.

        FREE: it_field_tab, it_fmap, it_zmmt0102.

        CLEAR: wa_fmap.
        wa_fmap-fldname   = 'F0002'.
        wa_fmap-dyfldname = 'CATEGORIA'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'F0003'.
        wa_fmap-dyfldname = 'MATNR'.
        APPEND wa_fmap TO it_fmap.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'NR_FASE' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 1.
        wa_field_tab-offset    = 0.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'CATEGORIA' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 2.
        wa_field_tab-offset    = 40.
        wa_field_tab-reptext   = 'Categoria'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'MATNR' CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 3.
        wa_field_tab-offset    = 44.
        wa_field_tab-reptext   = 'Material'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'MATNR' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'MENGE'.
        wa_field_tab-position  = 4.
        wa_field_tab-offset    = 80.
        wa_field_tab-reptext   = 'Quant.Entrada'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZSDT0102'
                                      'MATNR'
                             CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'LFIMG'.
        wa_field_tab-position  = 5.
        wa_field_tab-offset    = 116.
        wa_field_tab-reptext   = 'Quant.Saida'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                      'MATNR'
                             CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'SDO_FASE'.
        wa_field_tab-position  = 6.
        wa_field_tab-offset    = 152.
        wa_field_tab-reptext   = 'Saldo Fase'.
        APPEND wa_field_tab  TO it_field_tab.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = lwa_lote_7200-matnr
          IMPORTING
            output = lwa_lote_7200-matnr.

        SELECT *
          FROM zmmt0102  APPENDING TABLE it_zmmt0102
         WHERE charg EQ lwa_lote_7200-lote
           AND matnr EQ lwa_lote_7200-matnr.

        LOOP AT it_zmmt0102 INTO wa_zmmt0102.
          APPEND INITIAL LINE TO it_f4_nfase ASSIGNING FIELD-SYMBOL(<fs_f4_fase>).

          CLEAR l_lfimg.

          SELECT SINGLE lfimg
            FROM zsdt0134 INTO l_lfimg
           WHERE charg   = wa_zmmt0102-charg
             AND nr_fase = wa_zmmt0102-nr_fase.

          <fs_f4_fase>-categoria   = wa_zmmt0102-categoria.
          <fs_f4_fase>-matnr       = wa_zmmt0102-matnr.
          <fs_f4_fase>-nr_fase     = wa_zmmt0102-nr_fase.
          <fs_f4_fase>-menge       = wa_zmmt0102-menge.
          <fs_f4_fase>-lfimg       = l_lfimg.
          <fs_f4_fase>-sdo_fase    = wa_zmmt0102-menge - l_lfimg.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NR_FASE'
            window_title    = 'Lista de Fase'(002)
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_nfase
            return_tab      = it_ret_7200
            dynpfld_mapping = it_fmap
            field_tab       = it_field_tab
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_7200 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'NR_FASE'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

FORM f_fieldinfo_get USING p_tabname
                           p_fieldname
                  CHANGING p_field_tab.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
      fieldname      = p_fieldname
      lfieldname     = p_fieldname
    IMPORTING
      dfies_wa       = p_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

ENDFORM.
