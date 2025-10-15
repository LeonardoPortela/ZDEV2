* -------------------------------------------------------------------- *
*                         © CLIENTE                                    *
* ==================================================================== *
* Program.....: ZMMR170                                                *
* Title.......: INTEG. PEDIDO DE COMPRAS / 72762 JTS                   *
* Author......: Jeferson Silva                                         *
* Date........: 10/02/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr170  NO STANDARD PAGE HEADING.

TABLES: ekko.
TYPE-POOLS: slis, abap, icon, ixml.

TYPES: BEGIN OF ty_ekko,
         icon        TYPE icon_d,
         ebeln       TYPE ekko-ebeln,
         bukrs       TYPE ekko-bukrs,
         butxt       TYPE t001-butxt,
         bsart       TYPE ekko-bsart,
         batxt       TYPE t161t-batxt,
         aedat       TYPE ekko-aedat,
         ernam       TYPE ekko-ernam,
         lifnr       TYPE ekko-lifnr,
         name1       TYPE lfa1-name1,
         ekorg       TYPE ekko-ekorg,
         ekotx       TYPE t024e-ekotx,
         ekgrp       TYPE ekko-ekgrp,
         eknam       TYPE t024-eknam,
         icon_result TYPE icon_d,
         tt_return   TYPE bapiret2_t,
       END OF ty_ekko.

DATA: t_fcat       TYPE lvc_t_fcat,
      t_xml_header TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header,
      t_filter     TYPE zif_integracao_coupa_ped_comp=>tt_filter,
      t_ekko       TYPE STANDARD TABLE OF ty_ekko,
      go_int_ped   TYPE REF TO zcl_integracao_coupa_ped_comp.
CONSTANTS gc_service TYPE /ui2/service_name VALUE 'COUPA_INT_ENVIA_PED_COMPRA'.

*&---------------------------------------------------------------------*
* CONTAINER
*&---------------------------------------------------------------------*
DATA: o_alv  TYPE REF TO cl_gui_alv_grid,
      o_ctnr TYPE REF TO cl_gui_custom_container.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t01.
PARAMETERS: p_coupa RADIOBUTTON GROUP gp1 USER-COMMAND uc1 DEFAULT 'X',
            p_sap   RADIOBUTTON GROUP gp1.
PARAMETERS  p_test AS CHECKBOX.
PARAMETERS  p_vis  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t02.
SELECT-OPTIONS: s_cpo FOR ekko-ebeln NO INTERVALS MODIF ID mic.
PARAMETERS: p_qreg TYPE n LENGTH 5 DEFAULT 150 MODIF ID mic.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln MODIF ID mis,
                s_bukrs FOR ekko-bukrs MODIF ID mis,
                s_bsart FOR ekko-bsart MODIF ID mis,
                s_aedat FOR ekko-aedat MODIF ID mis,
                s_ernam FOR ekko-ernam MODIF ID mis,
                s_lifnr FOR ekko-lifnr MODIF ID mis,
                s_ekorg FOR ekko-ekorg MODIF ID mis,
                s_ekgrp FOR ekko-ekgrp MODIF ID mis.
SELECTION-SCREEN END OF BLOCK b1.



CLASS lcl_int_coupa_po DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:

      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id  es_row_no,

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      start_of_selection,

      coupa,

      integrao_coupa,

      sap,

      executa,

      alterar,

      alv_show CHANGING ct_tab TYPE STANDARD TABLE,

      popup_to_confirm IMPORTING iv_text                  TYPE string
                                 iv_text_button_1         TYPE string
                                 iv_text_button_2         TYPE string
                                 iv_display_cancel_button TYPE flag DEFAULT abap_false
                       RETURNING VALUE(rv_answer)         TYPE flag,

      alv_toolbar         IMPORTING iv_classif        TYPE boole_d OPTIONAL
                          RETURNING VALUE(rt_toolbar) TYPE ui_functions,

      get_fieldcatalog CHANGING  ct_table       TYPE STANDARD TABLE
                       RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

ENDCLASS.
CLASS lcl_int_coupa_po IMPLEMENTATION.

  METHOD start_of_selection.

    FREE: t_ekko, t_xml_header.

    IF ( p_coupa IS NOT INITIAL ).
      me->coupa( ).
    ELSE.
      me->sap( ).
    ENDIF.

  ENDMETHOD.
  METHOD coupa.
    DATA: lt_xml_header TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header.
    TRY.
        CREATE OBJECT go_int_ped
          EXPORTING
            i_servico = gc_service.

*    go_int_ped->busca_dados( ).
        " Buscas os dados no Coupa
        FREE: t_filter.
        IF ( p_qreg IS NOT INITIAL ).
          APPEND VALUE #( field = 'offset'
                          value = p_qreg ) TO t_filter.
        ENDIF.
        IF ( s_cpo IS NOT INITIAL ).
          LOOP AT s_cpo REFERENCE INTO DATA(lo_data).
            FREE: t_filter.
            APPEND INITIAL LINE TO t_filter REFERENCE INTO DATA(lo_wa_filter).
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lo_data->low
              IMPORTING
                output = lo_wa_filter->value.
            lo_wa_filter->field = 'po-number'.

            APPEND VALUE #( field = 'show_deleted_lines'
                         value =  'true' ) TO t_filter.

            me->integrao_coupa( ).

          ENDLOOP.
        ELSE.
          APPEND VALUE #( field = 'show_deleted_lines'
                       value =  'true' ) TO t_filter.

          me->integrao_coupa( ).
        ENDIF.
        t_xml_header = go_int_ped->get_xml_header( ).
        IF ( t_xml_header IS INITIAL ).
          MESSAGE 'Não há itens para serem processados' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        IF ( p_vis IS INITIAL OR sy-batch IS NOT INITIAL ).
          go_int_ped->determina_criacao_modificacao(
            EXPORTING
              iv_test       = p_test    " Flag geral
            CHANGING
              ct_xml_header = t_xml_header
          ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD integrao_coupa.

    go_int_ped->zif_integracao_coupa_ped_comp~set_ds_url( e_metodo   = 'GET'
                                                         it_filter  = t_filter ).
    go_int_ped->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).

    go_int_ped->realiza_quebra_xml(
*          IMPORTING
*            et_xml_header = lt_xml_header
*        et_xml_lines  =
    ).

  ENDMETHOD.
  METHOD sap.

    SELECT o~ebeln, o~bukrs, o~bsart,
           o~aedat, o~ernam, o~lifnr, o~ekorg, o~ekgrp,
           t001~butxt, t161t~batxt, lfa1~name1, t024e~ekotx, t024~eknam
      INTO TABLE @DATA(lt_ekko)
      FROM ekko AS o
      INNER JOIN t001
      ON t001~bukrs = o~bukrs
      INNER JOIN t161t
      ON  t161t~bstyp = 'F'
      AND t161t~bsart = o~bsart
      AND t161t~spras = @sy-langu
      INNER JOIN lfa1
      ON lfa1~lifnr = o~lifnr
      INNER JOIN t024e
      ON t024e~ekorg = o~ekorg
    INNER JOIN t024
      ON t024~ekgrp = o~ekgrp
      WHERE o~ebeln  IN @s_ebeln
        AND o~bukrs  IN @s_bukrs
        AND o~bsart  IN @s_bsart
        AND o~aedat  IN @s_aedat
        AND o~lifnr  IN @s_lifnr
        AND o~ekorg  IN @s_ekorg
        AND o~ekgrp  IN @s_ekgrp.

    IF ( sy-subrc IS NOT INITIAL ).
      MESSAGE 'Não há itens para serem processados' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT lt_ekko REFERENCE INTO DATA(lo_wa_ekko_local).
      APPEND INITIAL LINE TO t_ekko REFERENCE INTO DATA(lo_wa_ekko).
      MOVE-CORRESPONDING lo_wa_ekko_local->* TO lo_wa_ekko->*.
    ENDLOOP.

  ENDMETHOD.
  METHOD executa.
    DATA: lt_xml_header TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header.

    o_alv->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lt_selections) ).

    IF lt_selections IS INITIAL.
      MESSAGE 'Selecionar pelo menos uma linha'(002) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF (  me->popup_to_confirm(
      EXPORTING
        iv_text                  = CONV #( text-001 )
        iv_text_button_1         = CONV #( text-003 )
        iv_text_button_2         = CONV #( text-004 ) ) NE '1' ) .
      RETURN.
    ENDIF.

    LOOP AT lt_selections REFERENCE INTO DATA(lo_wa_selection).
      READ TABLE t_xml_header REFERENCE INTO DATA(lo_wa_xml_header) INDEX lo_wa_selection->index.
      IF ( sy-subrc IS INITIAL ).
        APPEND lo_wa_xml_header->* TO lt_xml_header.
      ENDIF.
    ENDLOOP.

    go_int_ped->determina_criacao_modificacao(
      EXPORTING
        iv_test       = p_test    " Flag geral
      CHANGING
        ct_xml_header = lt_xml_header
    ).

    LOOP AT lt_xml_header REFERENCE INTO DATA(lo_wa_xml_header_local).
      READ TABLE t_xml_header REFERENCE INTO lo_wa_xml_header WITH KEY id        = lo_wa_xml_header_local->id
                                                                       po_number = lo_wa_xml_header_local->po_number.
      IF ( sy-subrc IS INITIAL ).
        lo_wa_xml_header->* = lo_wa_xml_header_local->*.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.
  METHOD alterar.
    DATA: lt_ekko   TYPE STANDARD TABLE OF ty_ekko,
          lt_poitem TYPE STANDARD TABLE OF bapimepoitem.

    o_alv->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lt_selections) ).

    IF lt_selections IS INITIAL.
      MESSAGE 'Selecionar pelo menos uma linha'(002) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF (  me->popup_to_confirm(
      EXPORTING
        iv_text                  = CONV #( text-005 )
        iv_text_button_1         = CONV #( text-003 )
        iv_text_button_2         = CONV #( text-004 ) ) NE '1' ) .
      RETURN.
    ENDIF.

    LOOP AT lt_selections REFERENCE INTO DATA(lo_wa_selection).
      READ TABLE t_ekko REFERENCE INTO DATA(lo_wa_ekko) INDEX lo_wa_selection->index.
      IF ( sy-subrc IS INITIAL ).
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lo_wa_xml_header->po_number
*          IMPORTING
*            output = lo_wa_xml_header->po_number.
        APPEND lo_wa_ekko->* TO lt_ekko.
      ENDIF.
    ENDLOOP.

    IF ( lt_ekko IS NOT INITIAL ).
      SELECT ebeln, ebelp
        INTO TABLE @DATA(lt_ekpo)
        FROM ekpo
        FOR ALL ENTRIES IN @lt_ekko
        WHERE ebeln EQ @lt_ekko-ebeln.

    ENDIF.

    SORT lt_ekpo BY ebeln.

    LOOP AT lt_ekko REFERENCE INTO lo_wa_ekko.
      lo_wa_ekko->icon_result = '@DH@'.
      READ TABLE lt_ekpo TRANSPORTING NO FIELDS WITH KEY ebeln = lo_wa_ekko->ebeln.
      IF ( sy-subrc IS INITIAL ).
        LOOP AT lt_ekpo REFERENCE INTO DATA(lo_wa_ekpo) FROM sy-tabix.
          IF ( lo_wa_ekpo->ebeln NE lo_wa_ekko->ebeln ).
            EXIT.
          ENDIF.
          APPEND INITIAL LINE TO lt_poitem REFERENCE INTO DATA(lo_wa_poitem).
          lo_wa_poitem->po_item = lo_wa_ekpo->ebelp.
          lo_wa_poitem->calctype = 'G'.

        ENDLOOP.
      ELSE.
        APPEND INITIAL LINE TO lo_wa_ekko->tt_return REFERENCE INTO DATA(lo_wa_return).
        MESSAGE e019(06) WITH lo_wa_ekko->ebeln INTO DATA(lv_message).
        lo_wa_return->id          = sy-msgid.
        lo_wa_return->number      = sy-msgno.
        lo_wa_return->message_v1  = sy-msgv1.
        lo_wa_return->message_v2  = sy-msgv2.
        lo_wa_return->message_v3  = sy-msgv3.
        lo_wa_return->message_v4  = sy-msgv4.
        lo_wa_return->message     = lv_message.
        lo_wa_ekko->icon = '@5C@'.
        CONTINUE.
      ENDIF.
      lo_wa_ekko->tt_return = zcl_integracao_coupa_ped_comp=>bapi_po_change(
         EXPORTING
           iv_test               = p_test    " Campo de seleção
           iw_poheader           = VALUE bapimepoheader( po_number = lo_wa_ekko->ebeln )
         CHANGING
           ct_poitem             = lt_poitem  ).


      IF ( line_exists( lo_wa_ekko->tt_return[ type = 'E' ] ) ).
        lo_wa_ekko->icon = '@5C@'.
      ELSE.
        IF ( p_test IS NOT INITIAL ).
          lo_wa_ekko->icon = '@01@'.
        ELSE.
          lo_wa_ekko->icon = '@DF@'.
        ENDIF.
      ENDIF.

      FREE: lt_poitem.

    ENDLOOP.

    LOOP AT lt_ekko REFERENCE INTO DATA(lo_wa_ekko_local).
      READ TABLE t_ekko REFERENCE INTO lo_wa_ekko WITH KEY ebeln = lo_wa_ekko_local->ebeln.
      IF ( sy-subrc IS INITIAL ).
        lo_wa_ekko->* = lo_wa_ekko_local->*.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = lo_wa_xml_header->po_number
*          IMPORTING
*            output = lo_wa_xml_header->po_number.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD alv_show.
    DATA: lo_doc_container     TYPE REF TO cl_gui_docking_container.
    DATA: ls_layout   TYPE lvc_s_layo,
          ls_layout_f TYPE slis_layout_alv,
          lt_fieldcat TYPE slis_t_fieldcat_alv,
          lw_variant  TYPE disvariant.

    DATA(lt_tbexcl) = me->alv_toolbar( iv_classif = abap_true ).

    IF ( o_alv IS NOT BOUND ).
      IF cl_gui_alv_grid=>offline( ) IS INITIAL.
        CREATE OBJECT o_ctnr
          EXPORTING
            container_name = 'ALV_MAIN'.

        IF ( o_ctnr IS NOT BOUND ).
          RETURN.
        ENDIF.

        CREATE OBJECT o_alv
          EXPORTING
            i_parent = o_ctnr.
      ELSE.
* If it is in background:
        CREATE OBJECT o_alv
          EXPORTING
            i_parent = lo_doc_container.
      ENDIF.

      IF ( o_alv IS NOT BOUND ).
        RETURN.
      ENDIF.
*    ENDIF.

      t_fcat = get_fieldcatalog(
        CHANGING
          ct_table = ct_tab ).

      ls_layout-zebra      = abap_true.
      ls_layout-cwidth_opt = abap_true.
      ls_layout-smalltitle = abap_true.
      ls_layout-sel_mode   = 'A'.
      ls_layout-stylefname = 'CELLTAB'.

      lw_variant-report = sy-repid.
*      lw_variant-variant = p_varia.

      SET HANDLER handle_button_click FOR o_alv.
      SET HANDLER handle_hotspot_click FOR o_alv.

      o_alv->set_table_for_first_display(
        EXPORTING
          is_layout            = ls_layout
          it_toolbar_excluding = lt_tbexcl[]
          is_variant           = lw_variant
          i_save               = 'A'
        CHANGING
          it_outtab            = ct_tab[]
          it_fieldcatalog      = t_fcat[]
           EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

    ELSE.
      DATA: lw_stbl TYPE lvc_s_stbl.
      lw_stbl-col =
      lw_stbl-row = abap_true.
      o_alv->refresh_table_display( is_stable = lw_stbl ).
    ENDIF.


  ENDMETHOD.
  METHOD get_fieldcatalog.

    DATA: lo_fcat_alv TYPE REF TO cl_salv_table,
          lo_aggr     TYPE REF TO cl_salv_aggregations,
          lo_cols     TYPE REF TO cl_salv_columns_table,
          lt_table    TYPE REF TO data,
          lo_line     TYPE REF TO data.

    FIELD-SYMBOLS: <fs_fcat> LIKE LINE OF rt_fcat.

    TRY.
        FREE lo_fcat_alv.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_fcat_alv
          CHANGING
            t_table      = ct_table.
      CATCH cx_salv_msg INTO DATA(cx_salv).
        MESSAGE cx_salv TYPE 'E'.
    ENDTRY.

    CHECK lo_fcat_alv IS BOUND.

    lo_cols = lo_fcat_alv->get_columns( ).

    CHECK lo_cols IS BOUND.

    lo_aggr = lo_fcat_alv->get_aggregations( ).

    rt_fcat =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns = lo_cols
            r_aggregations = lo_aggr ).

    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
      CASE <fs_fieldcat>-fieldname.
        WHEN 'ICON'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Status'.
        WHEN 'ID'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Código'.
        WHEN 'CREATED_A'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Data'.
        WHEN 'STATUS'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Status'.
        WHEN 'PO_NUMBER' OR 'EBELN'.
          <fs_fieldcat>-hotspot = abap_true.
        WHEN 'CNPJ'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'CNPJ'.
          <fs_fieldcat>-no_out = abap_true.
        WHEN 'IE'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'IE'.
          <fs_fieldcat>-no_out = abap_true.
        WHEN 'TIPO_PEDIDO'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Ped Coupa'.
        WHEN 'BSART'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Ped SAP'.
        WHEN 'TIPO_FORNECEDOR'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Tipo Fornecedor'.
          <fs_fieldcat>-no_out = abap_true.
        WHEN 'PEDIDO_SAP'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Já Criado'.
        WHEN 'PRECO'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Valor Total'.
        WHEN 'PAYMENT_TERM'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Condição Pagamento'.
          <fs_fieldcat>-no_out = abap_true.
        WHEN 'ICON_RESULT'.
          <fs_fieldcat>-reptext   =
          <fs_fieldcat>-scrtext_s =
          <fs_fieldcat>-scrtext_m =
          <fs_fieldcat>-scrtext_l = 'Resultado'.
          <fs_fieldcat>-hotspot   = abap_true.
          <fs_fieldcat>-style     = cl_gui_alv_grid=>mc_style_button.
        WHEN 'TRANSMISSION' OR 'EXPORTED' OR 'REQUISITION_ID' OR 'TAXA_D_CMBIO' OR 'TAXA_FIXA' OR 'TT_RETURN'
          OR 'REQUISITION_ID' OR 'SHIPPING_TERM' OR 'ENTREGA_FUTURA' OR 'CODIO_PAGAMENTO'.
          <fs_fieldcat>-no_out = abap_true.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD alv_toolbar.
    REFRESH rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_refresh TO rt_toolbar.
    APPEND cl_gui_alv_grid=>mc_mb_view TO rt_toolbar.

    IF iv_classif = abap_true.
      APPEND cl_gui_alv_grid=>mc_fc_graph TO rt_toolbar.
      APPEND cl_gui_alv_grid=>mc_fc_info TO rt_toolbar.
    ENDIF.
  ENDMETHOD.                    "m_alv_toolbar
  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_text
*       DIAGNOSE_OBJECT       = ' '
        text_question         = iv_text
        text_button_1         = iv_text_button_1
*       ICON_BUTTON_1         = ' '
        text_button_2         = iv_text_button_2
*       ICON_BUTTON_2         = ' '
*       DEFAULT_BUTTON        = '1'
        display_cancel_button = iv_display_cancel_button
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = 25
*       START_ROW             = 6
*       POPUP_TYPE            =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
  METHOD handle_button_click.
    DATA: lw_stable TYPE lvc_s_stbl,
          lr_table  TYPE REF TO cl_salv_table.

    lw_stable-col = abap_true.
    lw_stable-row = abap_true.

    CASE es_col_id.

      WHEN 'ICON_RESULT'.

        CASE abap_true.
          WHEN p_coupa.
            DATA(lt_return) = t_xml_header[ es_row_no-row_id ]-tt_return.

          WHEN p_sap.
            lt_return = t_ekko[ es_row_no-row_id ]-tt_return.
        ENDCASE.

        IF ( lt_return IS NOT INITIAL ).
          TRY .
              cl_salv_table=>factory(
               EXPORTING
                  list_display = ''
                IMPORTING
                  r_salv_table = lr_table
                CHANGING
                  t_table      = lt_return ).

              lr_table->set_screen_popup(
                start_column = 1
                end_column   = 170
                start_line   = 1
                end_line     = 22 ).

              lr_table->display( ).
            CATCH cx_salv_msg.

          ENDTRY.
        ENDIF.

    ENDCASE.

  ENDMETHOD.
  METHOD handle_hotspot_click.

    CASE e_column_id.

      WHEN 'PO_NUMBER'.

*        CASE abap_true.
*          WHEN p_coupa.
        DATA(lv_ebeln) = t_xml_header[ e_row_id ]-po_number.
      WHEN 'EBELN'.
        lv_ebeln = t_ekko[ e_row_id ]-ebeln.

    ENDCASE.

    IF ( lv_ebeln IS NOT INITIAL ).
      SELECT SINGLE COUNT(*)
        FROM ekko
        WHERE ebeln = lv_ebeln.

      IF ( sy-subrc IS INITIAL ).
        SET PARAMETER ID 'BES' FIELD lv_ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE 'Pedido não está criado, não é possivel a navegação' TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    CASE  abap_true.
      WHEN p_coupa.
        IF  screen-group1 EQ 'MIS' .
          screen-active = 0.
        ENDIF.
      WHEN p_sap.
        IF  screen-group1 EQ 'MIC' .
          screen-active = 0.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  IF sy-batch EQ abap_true.

    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.
    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.

  ENDIF.

  DATA(o_int_coupa_po) = NEW lcl_int_coupa_po( ).

  o_int_coupa_po->start_of_selection( ).

*&---------------------------------------------------------------------*
* END-OF-SELECTION.
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.

  INCLUDE zmmr170_o01.

  INCLUDE zmmr170_i01.
