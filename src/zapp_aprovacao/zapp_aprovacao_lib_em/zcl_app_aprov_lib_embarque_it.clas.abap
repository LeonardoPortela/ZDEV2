CLASS zcl_app_aprov_lib_embarque_it DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .

    TYPES:
      BEGIN OF ty_filter,
        filters   TYPE string,
        orgvenda  TYPE RANGE OF zssd_aprov_lib_emb_item-orgvenda,
        vbeln     TYPE RANGE OF zssd_aprov_lib_emb_item-vbeln,
        posnr     TYPE RANGE OF zssd_aprov_lib_emb_item-posnr,
        seq       TYPE RANGE OF zssd_aprov_lib_emb_item-seq,
        esc_venda TYPE RANGE OF zssd_aprov_lib_emb_item-esc_venda,
      END OF ty_filter .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_filter TYPE ty_filter.

    METHODS set_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS get_paging
      IMPORTING iv_set_top  TYPE i DEFAULT 50
                iv_set_skip TYPE i DEFAULT 0
      CHANGING  ct_report   TYPE zsdtt_aprov_lib_emb_item.

ENDCLASS.

CLASS zcl_app_aprov_lib_embarque_it IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA lt_itens TYPE zsdtt_aprov_lib_emb_item.
    DATA(lv_top)     = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)    = io_request->get_paging( )->get_offset( ).
    DATA(lv_requested_fields)  = io_request->get_requested_elements( ).
    DATA(lv_sort_order)    = io_request->get_sort_elements( ).
    DATA(lv_max_rows)  = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).

    " Ao navegar pra Object Page, devemos setar como um registro .
    lv_top      = COND #( WHEN lv_top <= 0 THEN 1 ELSE lv_top ).
    lv_max_rows = COND #( WHEN lv_max_rows <= 0 THEN 1 ELSE lv_max_rows ).

* ---------------------------------------------------------------------------
* Recupera e seta filtros de seleção
* ---------------------------------------------------------------------------
    TRY.
        me->set_filters( EXPORTING it_filters = io_request->get_filter( )->get_as_ranges( ) ). "#EC CI_CONV_OK
      CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
*        lv_exp_msg = lo_ex_filter->get_longtext( ).
    ENDTRY.

    TRY.
        CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST11_APP'
          EXPORTING
            in_usuario = sy-uname
          IMPORTING
            et_item    = lt_itens.

        IF gs_filter IS NOT INITIAL.

          IF gs_filter-orgvenda IS NOT INITIAL.
            DELETE lt_itens WHERE orgvenda NOT IN gs_filter-orgvenda.
          ENDIF.

          IF gs_filter-vbeln IS NOT INITIAL.
            DELETE lt_itens WHERE vbeln NOT IN gs_filter-vbeln.
          ENDIF.

          IF gs_filter-esc_venda IS NOT INITIAL.
            DELETE lt_itens WHERE esc_venda NOT IN gs_filter-esc_venda.
          ENDIF.

          IF gs_filter-seq IS NOT INITIAL.
            DELETE lt_itens WHERE seq NOT IN gs_filter-seq.
          ENDIF.

          IF gs_filter-posnr IS NOT INITIAL.
            DELETE lt_itens WHERE posnr NOT IN gs_filter-posnr.
          ENDIF.

        ENDIF.


** ---------------------------------------------------------------------------
** Realiza ordenação de acordo com parâmetros de entrada
** ---------------------------------------------------------------------------

        DATA(lt_requested_sort) = io_request->get_sort_elements( ).
        IF lines( lt_requested_sort ) > 0.
          DATA(lt_sort) = VALUE abap_sortorder_tab( FOR ls_sort IN lt_requested_sort ( name = ls_sort-element_name descending = ls_sort-descending ) ).
          SORT lt_itens BY (lt_sort).
        ENDIF.


        io_response->set_total_number_of_records( lines( lt_itens ) ).

        get_paging(
          EXPORTING
            iv_set_top  = CONV #( lv_top )
            iv_set_skip = CONV #( lv_skip )
          CHANGING
            ct_report   = lt_itens ).


        io_response->set_data( lt_itens ).

      CATCH cx_root INTO DATA(exception).
        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

    ENDTRY.

  ENDMETHOD.

  METHOD set_filters.

* ---------------------------------------------------------------------------
* Recupera os filtros da tela do aplicativo
* ---------------------------------------------------------------------------

    FREE: gs_filter.

    IF it_filters IS NOT INITIAL.

      TRY.
          gs_filter-orgvenda   = CORRESPONDING #( it_filters[ name = 'ORGVENDA' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root).
          DATA(lv_exp_msg) = lo_root->get_longtext( ).
      ENDTRY.
      TRY.
          gs_filter-vbeln = CORRESPONDING #( it_filters[ name = 'VBELN' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-seq = CORRESPONDING #( it_filters[ name = 'SEQ' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-esc_venda = CORRESPONDING #( it_filters[ name = 'ESC_VENDA' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-posnr = CORRESPONDING #( it_filters[ name = 'POSNR' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.

    ELSE.
*      gs_filter-param_pcddat[]        = CORRESPONDING #( it_pcddat[] ).
*      gs_filter-param_bukrs[]         = CORRESPONDING #( it_bukrs[] ).
*      gs_filter-param_kunnr[]         = CORRESPONDING #( it_kunnr[] ).
*      gs_filter-param_ktokd[]         = CORRESPONDING #( it_ktokd[] ).
*      gs_filter-param_credit_sgmnt[]  = CORRESPONDING #( it_credit_sgmnt[] ).
*      gs_filter-param_pccass[]        = CORRESPONDING #( it_pccass[] ).
*      gs_filter-param_puanos[]        = CORRESPONDING #( it_puanos[] ).
    ENDIF.

  ENDMETHOD.

  METHOD get_paging.

    DATA lt_itens TYPE SORTED TABLE OF zssd_aprov_lib_emb_item
                WITH NON-UNIQUE KEY orgvenda vbeln esc_venda seq posnr.

* ----------------------------------------------------------------------
* Filtra somente os registros da paginação
* ----------------------------------------------------------------------
    lt_itens = ct_report.

    SELECT * FROM @lt_itens AS dados
         ORDER BY orgvenda, vbeln, esc_venda, seq, posnr
         INTO TABLE @ct_report
         UP TO @iv_set_top ROWS
         OFFSET @iv_set_skip.

    IF sy-subrc NE 0.
      FREE ct_report.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
