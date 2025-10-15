CLASS lcl_report_200 DEFINITION.

  PUBLIC SECTION.

    METHODS:
      get_data,
      generate_output,
      set_columns_build,
      set_pf_status.

    CLASS-METHODS:
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      on_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.

    TYPES: BEGIN OF ty_saida,
             hkont_ativo    TYPE zfit0231-hkont_ativo,
             DescCtAtiv     TYPE txt50,
             hkont_provisao TYPE zfit0231-hkont_provisao,
             DescCtProv     TYPE txt50,
             hkont_despesa  TYPE zfit0231-hkont_despesa,
             DescCtDesp     TYPE txt50,
             sgtxt1         TYPE zfit0231-sgtxt1,
             sgtxt2         TYPE zfit0231-sgtxt2.
    TYPES: END OF ty_saida.

    DATA: o_alv200    TYPE REF TO cl_gui_alv_grid,
          it_fieldcat TYPE lvc_t_fcat,
          it_f4       TYPE lvc_t_f4,
          it_saida    TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
          wa_saida    TYPE ty_saida.

ENDCLASS.
CLASS lcl_report_200 IMPLEMENTATION.

  METHOD on_f4.
    DATA: lt_return TYPE TABLE OF ddshretval,
          ls_return TYPE ddshretval,
          wa_saida  TYPE ty_saida.

    FREE: lt_return.
    CLEAR: wa_saida,ls_return.

    CASE e_fieldname.
      WHEN 'HKONT_ATIVO' OR 'HKONT_PROVISAO' OR 'HKONT_DESPESA'.

        DATA:ls_ska1   TYPE ska1.

        SELECT DISTINCT a~saknr,b~txt50 FROM ska1 AS a INNER JOIN skat AS b ON a~saknr = b~saknr AND a~ktopl = b~ktopl AND b~spras = 'P'
          WHERE a~ktopl = '0050' AND b~spras = 'P' AND b~txt50 IS NOT INITIAL INTO TABLE @DATA(lt_ska1).

        IF sy-subrc = 0.
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'HKONT'
              value_org       = 'S'
            TABLES
              value_tab       = lt_ska1
              return_tab      = lt_return
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.

          IF lt_return IS NOT INITIAL.
            READ TABLE lt_return INTO ls_return INDEX 1.
            IF sy-subrc = 0.
              READ TABLE lo_report_200->it_saida INTO wa_saida INDEX es_row_no-row_id.
              IF sy-subrc = 0.

                ASSIGN COMPONENT e_fieldname OF STRUCTURE wa_saida TO FIELD-SYMBOL(<fs_field>).
                IF <fs_field> IS ASSIGNED.
                  CONDENSE ls_return-fieldval NO-GAPS.
                  <fs_field> = |{ ls_return-fieldval PAD = '0' WIDTH = 10 ALIGN = RIGHT }|.
                ENDIF.

                MODIFY lo_report_200->it_saida FROM wa_saida INDEX es_row_no-row_id.
              ELSE.
                ASSIGN COMPONENT e_fieldname OF STRUCTURE wa_saida TO <fs_field>.
                IF <fs_field> IS ASSIGNED.
                  CONDENSE ls_return-fieldval NO-GAPS.
                  <fs_field> = |{ ls_return-fieldval PAD = '0' WIDTH = 10 ALIGN = RIGHT }|.
                ENDIF.

                APPEND wa_saida TO lo_report_200->it_saida.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.



        IF lo_report_200->it_saida IS NOT INITIAL.
          LOOP AT lo_report_200->it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

            IF <fs_saida>-hkont_ativo IS NOT INITIAL.
              SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_ativo AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctativ.
            ENDIF.
            IF <fs_saida>-hkont_despesa IS NOT INITIAL.
              SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_despesa AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctdesp.
            ENDIF.
            IF <fs_saida>-hkont_provisao IS NOT INITIAL.
              SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_provisao AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctprov.
            ENDIF.

          ENDLOOP.
        ENDIF.


        lo_report_200->o_alv200->refresh_table_display( ).
    ENDCASE.


  ENDMETHOD.

  METHOD set_columns_build .

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
    CREATE DATA lo_dynamic_table TYPE TABLE OF ty_saida.
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
             END OF ty_make_cols.

      DATA: it_make_cols TYPE STANDARD TABLE OF ty_make_cols WITH EMPTY KEY.

      "Basta colocar na sequencia que o key será o index da coluna, não existe a necessidade de colocar a sequencia já que é a ordem!
      it_make_cols = VALUE #(
                         ( fieldname = 'HKONT_ATIVO'    coltext = 'Conta Ativo'                 edit = abap_true  outputlen = 15      f4availabl = abap_true  )
                         ( fieldname = 'DESCCTATIV'     coltext = 'Desc. Conta Ativo'           edit = abap_false outputlen = 25      f4availabl = abap_false )
                         ( fieldname = 'HKONT_PROVISAO' coltext = 'Conta de Provisão'           edit = abap_true  outputlen = 15      f4availabl = abap_true  )
                         ( fieldname = 'DESCCTPROV'     coltext = 'Desc. Conta Provisão'        edit = abap_false outputlen = 25      f4availabl = abap_false )
                         ( fieldname = 'HKONT_DESPESA'  coltext = 'Conta de Despesa'            edit = abap_true  outputlen = 15      f4availabl = abap_true  )
                         ( fieldname = 'DESCCTDESP'     coltext = 'Desc. Conta Despesa'         edit = abap_false outputlen = 25      f4availabl = abap_false )
                         ( fieldname = 'SGTXT1'         coltext = 'Texto Padrão Provisão Ativo' edit = abap_true  outputlen = 35      f4availabl = abap_true  )
                         ( fieldname = 'SGTXT2'         coltext = 'Texto Padrão Resultado'      edit = abap_true  outputlen = 35      f4availabl = abap_true  )
                        ).
      CLEAR: gs_fieldcat,ls_f4.

      FREE: gt_fieldcat,it_fieldcat,lt_f4.

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>).

        READ TABLE it_make_cols WITH KEY fieldname = <fs_components>-name INTO DATA(wa_make_cols). "table_line

        IF sy-subrc = 0.

          MOVE-CORRESPONDING wa_make_cols TO gs_fieldcat.
          gs_fieldcat-col_pos = sy-tabix.

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

      MOVE-CORRESPONDING gt_fieldcat TO lo_report_200->it_fieldcat.
      MOVE-CORRESPONDING lt_f4 TO lo_report_200->it_F4.

      FREE: gt_fieldcat, lt_f4.

    ENDIF.

  ENDMETHOD.

  METHOD get_data.

    FREE: it_saida.

    SELECT DISTINCT *
    FROM
      zfit0231
      INTO TABLE it_sap.

    MOVE-CORRESPONDING it_sap TO lo_report_200->it_saida.

    IF lo_report_200->it_saida IS NOT INITIAL.
      LOOP AT lo_report_200->it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        IF <fs_saida>-hkont_ativo IS NOT INITIAL.
          SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_ativo AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctativ.
        ENDIF.
        IF <fs_saida>-hkont_despesa IS NOT INITIAL.
          SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_despesa AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctdesp.
        ENDIF.
        IF <fs_saida>-hkont_provisao IS NOT INITIAL.
          SELECT SINGLE txt50 FROM skat WHERE saknr = @<fs_saida>-hkont_provisao AND spras = 'P' AND ktopl = '0050' INTO @<fs_saida>-descctprov.
        ENDIF.

      ENDLOOP.
    ENDIF.

    FREE: it_sap.

  ENDMETHOD.


  METHOD set_pf_status.

*    DATA: lo_functions TYPE REF TO cl_salv_functions_list.
*    lo_functions = o_alv->get_functions( ).
*    lo_functions->set_all( abap_true ).
*    lo_functions->set_default( abap_true ).

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

  METHOD on_user_command.
    DATA: lt_rows     TYPE lvc_t_row,
          ls_row      TYPE lvc_s_roid,
          wa_zfit0231 TYPE zfit0231,
          qtd_rows    TYPE int4.

    CALL METHOD lo_report_200->o_alv200->get_selected_rows IMPORTING et_index_rows = lt_rows.
    qtd_rows = lines( lt_rows ).


    CLEAR: lo_report_200->wa_saida.

    CASE e_ucomm.

      WHEN 'SAVE_GRID'.

        IF lo_report_200->it_saida IS NOT INITIAL.
          LOOP AT lo_report_200->it_saida ASSIGNING FIELD-SYMBOL(<_save>).
            MOVE-CORRESPONDING <_save> TO wa_zfit0231.
            MODIFY zfit0231 FROM wa_zfit0231.
          ENDLOOP.

          lo_report_200->get_data( ).
          lo_report_200->o_alv200->refresh_table_display( ).

        ELSE.
          MESSAGE 'Não existem dados!' TYPE 'I'.
        ENDIF.

      WHEN 'DELETE_ROW'.

        IF qtd_rows > 0.

          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
            READ TABLE lo_report_200->it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
            MOVE-CORRESPONDING <_del> TO wa_zfit0231.
            DELETE zfit0231 FROM wa_zfit0231.
            COMMIT WORK.
          ENDLOOP.

          lo_report_200->get_data( ).
          lo_report_200->o_alv200->refresh_table_display( ).

        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

      WHEN 'INSERT_ROW'.
        APPEND lo_report_200->wa_saida TO lo_report_200->it_saida.
        lo_report_200->o_alv200->refresh_table_display( ).
      WHEN 'REFRESH_GRID'.
        lo_report_200->get_data( ).
        lo_report_200->o_alv200->refresh_table_display( ).

*      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA
      "3 DESABILITA E 0 HABILITA

      CASE <fs_tollbar>-function.
*        WHEN
*          '&LOCAL&INSERT_ROW' OR
*          '&LOCAL&APPEND' OR
*          '&LOCAL&DELETE_ROW' OR
*          '&LOCAL&COPY_ROW'.
*          <fs_tollbar>-butn_type = '3'.
        WHEN OTHERS.
          IF <fs_tollbar>-function EQ '&REFRESH'.
            <fs_tollbar>-function = 'REFRESH_GRID'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
            <fs_tollbar>-function = 'DELETE_ROW'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
            <fs_tollbar>-function = 'INSERT_ROW'.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = '&REFRESH'.   "fcode
    mt_toolbar-icon = '@42@'.
    mt_toolbar-quickinfo = 'Atualizar'.
    mt_toolbar-text = 'Atualizar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'SAVE_GRID'.   "fcode
    mt_toolbar-icon = '@F_SAVE@'.
    mt_toolbar-quickinfo = 'Salvar Grid'.
    mt_toolbar-text = 'Salvar Grid'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD generate_output.

    DATA: main_container TYPE REF TO cl_gui_custom_container.

    CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.

    CREATE OBJECT:
    main_container EXPORTING container_name = 'C200',
      lo_report_200->o_alv200 EXPORTING i_parent = main_container.

    lo_report_200->set_columns_build( ).

    CALL METHOD lo_report_200->o_alv200->register_f4_for_fields
      EXPORTING
        it_f4 = it_f4.

    SET HANDLER:
            lo_report_200->on_f4 FOR o_alv200,
            lo_report_200->on_user_command FOR o_alv200,
            lo_report_200->on_toolbar FOR o_alv200.


    IF lo_report_200->it_fieldcat IS NOT INITIAL.

      DATA: ls_variant TYPE disvariant.
      "VALUE disvariant( report = sy-repid )

      ls_variant-report = sy-repid.
      TRY.

          CALL METHOD lo_report_200->o_alv200->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout
              "i_structure_name = 'TY_SAIDA'
              is_variant      = ls_variant
              i_save          = 'A'
            CHANGING
              it_outtab       = lo_report_200->it_saida
              it_fieldcatalog = lo_report_200->it_fieldcat.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD on_link_click.

*    DATA: wa_saida TYPE zpme0087.
*    CLEAR: wa_saida.
*
*    IF column = ''.
*      "READ TABLE lo_report_200->it_saida INTO wa_saida INDEX row.
*
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
