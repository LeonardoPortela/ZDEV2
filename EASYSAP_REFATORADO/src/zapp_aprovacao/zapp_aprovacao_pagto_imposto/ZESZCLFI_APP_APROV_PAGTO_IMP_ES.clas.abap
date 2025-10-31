CLASS ZESZCLFI_APP_APROV_PAGTO_IMP_ES DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

    TYPES: BEGIN OF ty_filter,
             filters TYPE string,
             lote    TYPE RANGE OF ZESZSFI_APROV_PAGTOIMP_ESTR-lote,
             empresa TYPE RANGE OF ZESZSFI_APROV_PAGTOIMP_ESTR-empresa,
             nivel   TYPE RANGE OF ZESZSFI_APROV_PAGTOIMP_ESTR-nivel,
           END OF ty_filter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_filter TYPE ty_filter.

    METHODS set_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS get_paging
      IMPORTING iv_set_top  TYPE i DEFAULT 50
                iv_set_skip TYPE i DEFAULT 0
      CHANGING  ct_report   TYPE zttfi_adi_forn_estr.


ENDCLASS.



CLASS ZESZCLFI_APP_APROV_PAGTO_IMP_ES IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA lt_itens TYPE  zfitt_aprov_pagtimp_estr.
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
        CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST05_APP'
          EXPORTING
            in_usuario = sy-uname
          IMPORTING
            e_t_estra  = lt_itens.

        IF gs_filter IS NOT INITIAL.

          IF gs_filter-empresa IS NOT INITIAL.
            DELETE lt_itens WHERE empresa NOT IN gs_filter-empresa.
          ENDIF.

          IF gs_filter-lote IS NOT INITIAL.
            DELETE lt_itens WHERE lote NOT IN gs_filter-lote.
          ENDIF.

          IF gs_filter-nivel IS NOT INITIAL.
            DELETE lt_itens WHERE nivel NOT IN gs_filter-nivel.
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

  METHOD get_paging.

    DATA lt_itens TYPE SORTED TABLE OF ZESZSFI_APROV_PAGTOIMP_ESTR WITH NON-UNIQUE KEY lote empresa nivel.

* ----------------------------------------------------------------------
* Filtra somente os registros da paginação
* ----------------------------------------------------------------------
    lt_itens = ct_report.

    SELECT * FROM @lt_itens AS dados
         ORDER BY lote , empresa , nivel
         INTO TABLE @ct_report
         UP TO @iv_set_top ROWS
         OFFSET @iv_set_skip.

    IF sy-subrc NE 0.
      FREE ct_report.
    ENDIF.


  ENDMETHOD.

  METHOD set_filters.


* ---------------------------------------------------------------------------
* Recupera os filtros da tela do aplicativo
* ---------------------------------------------------------------------------
    FREE: gs_filter.

    IF it_filters IS NOT INITIAL.

      TRY.
          gs_filter-lote   = CORRESPONDING #( it_filters[ name = 'LOTE' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root).
          DATA(lv_exp_msg) = lo_root->get_longtext( ).
      ENDTRY.
      TRY.
          gs_filter-empresa[] = CORRESPONDING #( it_filters[ name = 'EMPRESA' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-nivel[] = CORRESPONDING #( it_filters[ name = 'NIVEL' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          lv_exp_msg = lo_root->get_longtext( ).
      ENDTRY.


    ENDIF.


  ENDMETHOD.

ENDCLASS.
