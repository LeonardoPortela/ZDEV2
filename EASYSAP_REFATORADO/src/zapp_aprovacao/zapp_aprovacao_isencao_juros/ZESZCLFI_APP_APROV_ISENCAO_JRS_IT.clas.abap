CLASS ZESZCLFI_APP_APROV_ISENCAO_JRS_IT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.


    TYPES: BEGIN OF ty_filter,
             filters      TYPE string,
             simul_venda  TYPE RANGE OF ZESZSDED003,
             ov_principal TYPE RANGE OF vbeln,
             vbeln        TYPE RANGE OF zsdt0090-vbeln,
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
      CHANGING  ct_report   TYPE zfitt_aprov_isencao_juros_item.


ENDCLASS.



CLASS ZESZCLFI_APP_APROV_ISENCAO_JRS_IT IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA lt_itens TYPE  zfitt_aprov_isencao_juros_item.
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

        CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST17_APP'
          EXPORTING
            in_usuario = sy-uname
          IMPORTING
            et_docs    = lt_itens.

        IF gs_filter IS NOT INITIAL.

          IF gs_filter-simul_venda  IS NOT INITIAL.
            DELETE lt_itens WHERE simul_venda NOT IN gs_filter-simul_venda.
          ENDIF.

          IF gs_filter-ov_principal  IS NOT INITIAL.
            DELETE lt_itens WHERE ov_principal NOT IN gs_filter-ov_principal.
          ENDIF.

          IF gs_filter-vbeln  IS NOT INITIAL.
            DELETE lt_itens WHERE vbeln NOT IN gs_filter-vbeln.
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
          gs_filter-vbeln    = CORRESPONDING #( it_filters[ name = 'VBELN' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root).
          DATA(lv_exp_msg) = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-simul_venda    = CORRESPONDING #( it_filters[ name = 'SIMUL_VENDA' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          DATA(lv_exp_msg2) = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-ov_principal    = CORRESPONDING #( it_filters[ name = 'OV_PRINCIPAL' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO lo_root.
          DATA(lv_exp_msg3) = lo_root->get_longtext( ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD get_paging.

    DATA lt_itens TYPE SORTED TABLE OF ZESZSFI_APROV_ISENCAO_JUROS_IT WITH NON-UNIQUE KEY simul_venda ov_principal vbeln.

* ----------------------------------------------------------------------
* Filtra somente os registros da paginação
* ----------------------------------------------------------------------
    lt_itens = ct_report.

    SELECT * FROM @lt_itens AS dados
         ORDER BY simul_venda , ov_principal,  vbeln
         INTO TABLE @ct_report
         UP TO @iv_set_top ROWS
         OFFSET @iv_set_skip.

    IF sy-subrc NE 0.
      FREE ct_report.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
