*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Paulo Ferraz                                            &*
*& Data.....: 15.04.2024                                              &*
*& Descrição: Classificar Contas do PL                                &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT zglr078.

TABLES zglt0112.


TYPES: BEGIN OF ty_dados,
         hkont                TYPE zglt0112-hkont,
         hkont_nome           TYPE zglt0112-hkont_nome,
         grupo_contas         TYPE zglt0112-grupo_contas,
         cod_clas_bal         TYPE zglt0112-cod_clas_bal,
         descr_bal            TYPE zfied008,
         cod_clas_not2        TYPE zglt0112-cod_clas_not2,
         descr_nota           TYPE zfied032,
         tipo_reflexa_pl      TYPE zglt0112-tipo_reflexa_pl,
         desc_tipo_reflexa_pl TYPE char40,
       END OF ty_dados.

DATA: gt_contas     TYPE TABLE OF zi_contas_classif_pl,
      gt_zglt0112   TYPE TABLE OF zglt0112,
      gt_dados      TYPE TABLE OF ty_dados,
      gt_tp_reflexa TYPE TABLE OF dd07t.

DATA: go_alv TYPE REF TO cl_salv_table.

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.


    METHODS:
      main ,

      on_added_function     FOR EVENT if_salv_events_functions~added_function
        OF cl_salv_events
        IMPORTING e_salv_function,

      on_handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm
          sender.


  PRIVATE SECTION.

    METHODS:
      get_dados,
      process,
      alv,
      colunas_alv,
      botao_alv,
      status_alv,
      save_dados
        IMPORTING iv_tp_refl TYPE z_tp_lan_eq,
      editar_valor,
      seleciona_linhas.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD main.

    get_dados( ).
    process( ).
    alv( ).

  ENDMETHOD.

  METHOD get_dados.

    CLEAR: gt_contas.

    SELECT *
    FROM zi_contas_classif_pl
    INTO TABLE @gt_contas.

    IF sy-subrc = 0.

      SELECT *
      FROM zglt0112
      INTO TABLE @gt_zglt0112
      FOR ALL ENTRIES IN @gt_contas
      WHERE hkont = @gt_contas-conta.

      SORT: gt_zglt0112 BY hkont grupo_contas cod_clas_bal cod_clas_not2.

      SELECT * FROM dd07t
      INTO TABLE @gt_tp_reflexa
            WHERE ddlanguage = @sy-langu
             AND domname = 'Z_TP_LAN_EQ'.
      SORT: gt_tp_reflexa BY domvalue_l.

    ENDIF.

  ENDMETHOD.

  METHOD process.

    LOOP AT gt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      APPEND INITIAL LINE TO gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

      <fs_dados>-hkont = <fs_conta>-conta.
      <fs_dados>-hkont_nome = <fs_conta>-descconta.
      <fs_dados>-grupo_contas = <fs_conta>-grpcontas.
      <fs_dados>-cod_clas_bal = <fs_conta>-classbal.
      <fs_dados>-cod_clas_not2 = <fs_conta>-classnota.
      <fs_dados>-descr_bal = <fs_conta>-descclassbal.
      <fs_dados>-descr_nota = <fs_conta>-descclassnota.

      READ TABLE gt_zglt0112 ASSIGNING FIELD-SYMBOL(<fs_zglt0112>)
                      WITH KEY hkont = <fs_conta>-conta
                      BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_dados>-tipo_reflexa_pl = <fs_zglt0112>-tipo_reflexa_pl.

        READ TABLE gt_tp_reflexa ASSIGNING FIELD-SYMBOL(<fs_desc>)
        WITH KEY domvalue_l = <fs_dados>-tipo_reflexa_pl
              BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_dados>-desc_tipo_reflexa_pl = <fs_desc>-ddtext.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD alv.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = gt_dados.

        seleciona_linhas(  ).
        status_alv(  ).
        colunas_alv(  ).
        botao_alv(  ).

      CATCH cx_root INTO DATA(lo_error).

    ENDTRY.

    go_alv->display( ).


  ENDMETHOD.



  METHOD colunas_alv.

    DATA: lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column  TYPE REF TO cl_salv_column.
    DATA lv_reflexa_pl     TYPE lvc_fname.
    DATA ls_reflexa_pl_ref TYPE salv_s_ddic_reference.

* column name
    lv_reflexa_pl = 'TIPO_REFLEXA_PL'.

    ls_reflexa_pl_ref-table = 'ZGLT0112'.
    ls_reflexa_pl_ref-field = 'TIPO_REFLEXA_PL'.

    lr_columns = go_alv->get_columns( ).
    lr_columns->set_optimize( ).

    TRY.
        lr_column = lr_columns->get_column( 'TIPO_REFLEXA_PL' ).
        lr_column->set_long_text( 'Tipo Reflexa PL' ).
        lr_column->set_medium_text( 'Tipo Reflexa' ).
        lr_column->set_short_text( 'TipoRefl' ).

**        lr_column = lr_columns->get_column(
**          EXPORTING
**            columnname = lv_reflexa_pl
**        ).
**        lr_column->set_ddic_reference(
**          EXPORTING
**            value = ls_reflexa_pl_ref
**        ).
**
**        DATA:lr_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
**             lr_edit TYPE REF TO if_salv_gui_om_edit_restricted.
**
**
**        lr_api = go_alv->extended_grid_api( ).
**        lr_edit = lr_api->editable_restricted( ).
**
**        lr_edit->set_attributes_for_columnname(
**          EXPORTING
**            columnname              = lv_reflexa_pl
**            all_cells_input_enabled = abap_true
**        ).


      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


    TRY.
        lr_column = lr_columns->get_column( 'DESC_TIPO_REFLEXA_PL' ).
        lr_column->set_long_text( 'Descrição Tipo Reflexa PL' ).
        lr_column->set_medium_text( 'Tipo Reflexa' ).
        lr_column->set_short_text( 'DTipoRefl' ).
        lr_column->set_output_length( 40 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

  ENDMETHOD.


  METHOD botao_alv.

    DATA gr_salv_func TYPE REF TO cl_salv_functions .

* add custom handler method to the table (implementation details follows in the next step)
    DATA(lo_events) = go_alv->get_event( ).
    SET HANDLER on_added_function FOR lo_events.

    SET HANDLER on_handle_data_changed FOR ALL INSTANCES ACTIVATION 'X'.


  ENDMETHOD.


  METHOD status_alv.

    go_alv->set_screen_status(
      pfstatus      = 'STANDARD'
      report        = 'ZGLR078'
      set_functions = go_alv->c_functions_all ).

    go_alv->get_functions( )->set_all( ).

  ENDMETHOD.

  METHOD on_added_function.

    CASE e_salv_function.

      WHEN 'ZSAVE'.


        editar_valor(  ).
        go_alv->refresh(  ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD save_dados.

    DATA: ls_zglt0112 TYPE zglt0112.
    DATA: lt_dados TYPE TABLE OF ty_dados.

    DATA(lt_rows) = go_alv->get_selections( )->get_selected_rows( ).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

      READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX <fs_row>.
      IF sy-subrc = 0 .

        ls_zglt0112 = VALUE #(
            hkont = <fs_dados>-hkont
           tipo_reflexa_pl = iv_tp_refl
           grupo_contas = <fs_dados>-grupo_contas
           cod_clas_bal = <fs_dados>-cod_clas_bal
           cod_clas_not2 = <fs_dados>-cod_clas_not2
           hkont_nome = <fs_dados>-hkont_nome

     ).

        MODIFY zglt0112 FROM ls_zglt0112.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        READ TABLE gt_tp_reflexa ASSIGNING FIELD-SYMBOL(<fs_desc>)
          WITH KEY domvalue_l = iv_tp_refl
                   BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_dados>-tipo_reflexa_pl = iv_tp_refl.
          <fs_dados>-desc_tipo_reflexa_pl = <fs_desc>-ddtext.
        ENDIF.

      ENDIF..

    ENDLOOP.

    go_alv->refresh(  ).

  ENDMETHOD.

  METHOD on_handle_data_changed.

    DATA: lv_change TYPE boolean.
    DATA: lt_dados TYPE TABLE OF ty_dados.

    IF sy-ucomm IS INITIAL.

      lt_dados = gt_dados.
      DELETE lt_dados WHERE tipo_reflexa_pl IS INITIAL.
      DELETE lt_dados WHERE desc_tipo_reflexa_pl IS NOT INITIAL.

      CLEAR: lv_change.
      LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

        READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_alv>)
                  WITH KEY hkont = <fs_dados>-hkont.
        IF sy-subrc = 0.

          READ TABLE gt_tp_reflexa ASSIGNING FIELD-SYMBOL(<fs_desc>)
                  WITH KEY domvalue_l = <fs_dados_alv>-tipo_reflexa_pl
                  BINARY SEARCH.
          IF sy-subrc = 0.
            <fs_dados_alv>-desc_tipo_reflexa_pl = <fs_desc>-ddtext.
            lv_change = abap_true.
          ENDIF.

        ENDIF.

      ENDLOOP.


      IF lv_change = abap_true.
        go_alv->refresh( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD editar_valor.

    DATA: lt_field TYPE TABLE OF sval.
    DATA: lv_resposta TYPE char1.


    DATA(lt_linhas) =   go_alv->get_selections( )->get_selected_rows( ).
    IF lt_linhas IS INITIAL.

      MESSAGE e000(zequi_patri) WITH 'Seleciona pelo menos uma linha'.

    ELSE.

      APPEND INITIAL LINE TO lt_field ASSIGNING FIELD-SYMBOL(<fs_field>).
      <fs_field>-tabname = 'ZGLT0112'.
      <fs_field>-fieldname = 'TIPO_REFLEXA_PL'.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title     = 'Informar Tipo Reflexa PL'
          start_column    = '5'
          start_row       = '5'
        IMPORTING
          returncode      = lv_resposta
        TABLES
          fields          = lt_field
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.

      IF lv_resposta <> 'A'.

        READ TABLE lt_field ASSIGNING <fs_field> INDEX 1.
        IF sy-subrc = 0.
          save_dados( CONV #( <fs_field>-value ) ).
        ENDIF.

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD seleciona_linhas.

    DATA: lo_functions  TYPE REF TO cl_salv_functions_list,
          lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = go_alv->get_selections( ).
    lo_selections->set_selection_mode( cl_salv_selections=>multiple ).

  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.

  NEW lcl_report( )->main( ).
