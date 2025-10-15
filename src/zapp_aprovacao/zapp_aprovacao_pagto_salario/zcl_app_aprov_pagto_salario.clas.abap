CLASS zcl_app_aprov_pagto_salario DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

    TYPES: BEGIN OF ty_filter,
             filters TYPE string,
             lote    TYPE RANGE OF zfi_lotes_fol-lote,
             empresa TYPE RANGE OF zfi_lotes_fol-empresa,
           END OF ty_filter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_filter TYPE ty_filter.
    METHODS set_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

ENDCLASS.

CLASS zcl_app_aprov_pagto_salario IMPLEMENTATION.

  METHOD if_rap_query_provider~select.


    DATA lt_itens TYPE zfitt_aprov_pagtsal.
    DATA(top)     = io_request->get_paging( )->get_page_size( ).
    DATA(skip)    = io_request->get_paging( )->get_offset( ).
    DATA(requested_fields)  = io_request->get_requested_elements( ).
    DATA(sort_order)    = io_request->get_sort_elements( ).


* ---------------------------------------------------------------------------
* Recupera e seta filtros de seleção
* ---------------------------------------------------------------------------
    TRY.
        me->set_filters( EXPORTING it_filters = io_request->get_filter( )->get_as_ranges( ) ). "#EC CI_CONV_OK
      CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
*        lv_exp_msg = lo_ex_filter->get_longtext( ).
    ENDTRY.

    TRY.
        CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST09_APP'
          EXPORTING
            in_usuario = sy-uname
          IMPORTING
            et_lote    = lt_itens.

        IF gs_filter IS NOT INITIAL.

          IF gs_filter-lote IS NOT INITIAL.
            DELETE lt_itens WHERE lote NOT IN gs_filter-lote.
          ENDIF.

          IF gs_filter-empresa IS NOT INITIAL.
            DELETE lt_itens WHERE empresa NOT IN gs_filter-empresa.
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
          gs_filter-lote   = CORRESPONDING #( it_filters[ name = 'LOTE' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root).
          DATA(lv_exp_msg) = lo_root->get_longtext( ).
      ENDTRY.
      TRY.
          gs_filter-empresa[] = CORRESPONDING #( it_filters[ name = 'EMPRESA' ]-range ). "#EC CI_STDSEQ
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

ENDCLASS.

