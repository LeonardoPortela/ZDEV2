CLASS ZESZCL_APP_APROV_ORC_ORDEM_PM DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

    CONSTANTS gc_sup TYPE ZESZDE_TIPO_ORDEM VALUE '2'.
    CONSTANTS gc_ord TYPE ZESZDE_TIPO_ORDEM VALUE '1'.

    TYPES: BEGIN OF ty_filter,
             filters TYPE string,
             aufnr   TYPE RANGE OF zpmr0003-aufnr,
             werks   TYPE RANGE OF zpmr0003-werks,
             equnr   TYPE RANGE OF zpmr0003-equnr,
             data    TYPE RANGE OF zpmr0003-erdat,
             arbpl   TYPE RANGE OF zpmr0003-arbpl,
             tipo    TYPE RANGE OF ZESZDE_TIPO_ORDEM,
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
      CHANGING  ct_report   TYPE zpmtt_aprov_orc_ordem_pm.

ENDCLASS.



CLASS ZESZCL_APP_APROV_ORC_ORDEM_PM IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA lt_lotes TYPE  zpmtt_aprov_orc_ordem_pm.
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
        CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST08_APP'
          EXPORTING
            in_usuario = sy-uname
          IMPORTING
            et_ordens  = lt_lotes.

        IF gs_filter IS NOT INITIAL.

          IF gs_filter-aufnr IS NOT INITIAL.
            DELETE lt_lotes WHERE aufnr NOT IN gs_filter-aufnr.
          ENDIF.
          IF gs_filter-equnr IS NOT INITIAL.
            DELETE lt_lotes WHERE equnr NOT IN gs_filter-equnr.
          ENDIF.
          IF gs_filter-werks IS NOT INITIAL.
            DELETE lt_lotes WHERE werks NOT IN gs_filter-werks.
          ENDIF.
          IF gs_filter-data IS NOT INITIAL.
            DELETE lt_lotes WHERE erdat NOT IN gs_filter-data.
          ENDIF.
          IF gs_filter-arbpl IS NOT INITIAL.
            DELETE lt_lotes WHERE arbpl NOT IN gs_filter-arbpl.
          ENDIF.
          IF gs_filter-tipo IS NOT INITIAL.
            DELETE lt_lotes WHERE tipo NOT IN gs_filter-tipo.
          ENDIF.

        ENDIF.

** ---------------------------------------------------------------------------
** Realiza ordenação de acordo com parâmetros de entrada
** ---------------------------------------------------------------------------
        DATA(lt_requested_sort) = io_request->get_sort_elements( ).
        IF lines( lt_requested_sort ) > 0.
          DATA(lt_sort) = VALUE abap_sortorder_tab( FOR ls_sort IN lt_requested_sort ( name = ls_sort-element_name descending = ls_sort-descending ) ).
          SORT lt_lotes BY (lt_sort).
        ENDIF.


        io_response->set_total_number_of_records( lines( lt_lotes ) ).

        get_paging(
          EXPORTING
            iv_set_top  = CONV #( lv_top )
            iv_set_skip = CONV #( lv_skip )
          CHANGING
            ct_report   = lt_lotes ).

        io_response->set_data( lt_lotes ).

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
          gs_filter-aufnr   = CORRESPONDING #( it_filters[ name = 'AUFNR' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root).
          DATA(lv_exp_msg) = lo_root->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-werks   = CORRESPONDING #( it_filters[ name = 'WERKS' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root2).
          DATA(lv_exp_msg2) = lo_root2->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-equnr   = CORRESPONDING #( it_filters[ name = 'EQUNR' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root3).
          DATA(lv_exp_msg3) = lo_root3->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-data   = CORRESPONDING #( it_filters[ name = 'ERDAT' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root4).
          DATA(lv_exp_msg4) = lo_root4->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-arbpl   = CORRESPONDING #( it_filters[ name = 'ARBPL' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root5).
          DATA(lv_exp_msg5) = lo_root5->get_longtext( ).
      ENDTRY.

      TRY.
          gs_filter-tipo   = CORRESPONDING #( it_filters[ name = 'TIPO' ]-range ). "#EC CI_STDSEQ
        CATCH cx_root INTO DATA(lo_root6).
          DATA(lv_exp_msg6) = lo_root6->get_longtext( ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD get_paging.

    DATA lt_itens TYPE SORTED TABLE OF ZESZSPM_APROV_ORC_ORDEM_PM WITH NON-UNIQUE KEY werks equnr aufnr.

* ----------------------------------------------------------------------
* Filtra somente os registros da paginação
* ----------------------------------------------------------------------
    lt_itens = ct_report.

    SELECT * FROM @lt_itens AS dados
         ORDER BY werks, equnr, aufnr
         INTO TABLE @ct_report
         UP TO @iv_set_top ROWS
         OFFSET @iv_set_skip.

    IF sy-subrc NE 0.
      FREE ct_report.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
