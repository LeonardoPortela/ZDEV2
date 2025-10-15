FUNCTION zfpm_create_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZPME_ORDEM_PM
*"  EXPORTING
*"     REFERENCE(E_DATA_RETURN) TYPE  STRING
*"----------------------------------------------------------------------


  IF i_data IS NOT INITIAL.

    TYPES: BEGIN OF ty_selc_plan,
             warpl TYPE vimplastat-warpl,
             mptyp TYPE vimplastat-mptyp,
             strat TYPE vimplastat-strat,
             objnr TYPE vimplastat-objnr,
           END OF ty_selc_plan.

    DATA: lv_orderid          TYPE aufnr,
          lv_refnum           TYPE ifrefnum,
          lv_oper_no          TYPE objidext,
          wa_return           TYPE bapiret2,
          numero_ordem_v2     TYPE char1,
          lv_empresa          TYPE bukrs,
          _method             TYPE swo_method,
          at_orderid          TYPE aufnr VALUE 1,
          at_refnum           TYPE ifrefnum VALUE 1,
          at_seqopr           TYPE ifrefnum,
          at_oper_no          TYPE objidext,
          wa_methods          TYPE bapi_alm_order_method,
          wa_header           TYPE bapi_alm_order_headers_i,
          lwa_eq_header       TYPE alm_me_tob_header,
          wa_operation        TYPE bapi_alm_order_operation,
          wa_data_general     TYPE bapi_itob,
          wa_data_generalx    TYPE bapi_itobx,
          wa_data_specific    TYPE bapi_itob_eq_only,
          wa_data_specificx   TYPE bapi_itob_eq_onlyx,
          wa_return_bapi_eqmt TYPE bapireturn.

    DATA: it_methods   TYPE TABLE OF bapi_alm_order_method,
          it_header    TYPE TABLE OF bapi_alm_order_headers_i,
          it_operation TYPE TABLE OF bapi_alm_order_operation,
          it_return    TYPE TABLE OF bapiret2,
          t_text       TYPE TABLE OF bapi_alm_text,
          t_text_lines TYPE TABLE OF bapi_alm_text_lines,
          t_tab_text   TYPE TABLE OF string,
          w_tab_text   TYPE string,
          t_return     TYPE TABLE OF bapiret2.
*          it_selc_plan    TYPE TABLE OF ty_selc_plan,
*          it_notification TYPE STANDARD TABLE OF bapi2080_1.



    FREE: it_methods, it_header, it_operation, it_return.
    CLEAR: wa_return, wa_header.

    at_orderid = |{ at_orderid ALPHA = IN }|.
    at_refnum  = |{ at_refnum  ALPHA = IN }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.

    it_methods = VALUE bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    APPEND VALUE #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) TO it_methods.
    APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) TO it_methods.
    APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) TO it_methods.
*
*    "Buscar dados equipamento.
*    CALL FUNCTION 'ALM_ME_EQUIPMENT_GETDETAIL'
*      EXPORTING
*        i_equipment    = lva_equnr
*      IMPORTING
*        e_equi_header  = lwa_eq_header
*      TABLES
*        return         = lit_ret
*      EXCEPTIONS
*        not_successful = 1
*        OTHERS         = 2.
*
*    wa_header-order_type    = i_data-auart.
*    wa_header-funct_loc     = lwa_eq_header-tplnr.
*    wa_header-short_text    = i_data-ktext.
*    wa_header-planplant     = lwa_eq_header-swerk.
*    wa_header-loc_comp_code = lwa_eq_header-bukrs.
*    wa_header-bus_area      = bus_area.
*    wa_header-mn_wk_ctr     = mn_wk_ctr.
*    wa_header-plant         = plant.
*    wa_header-maintplant    = maintplant.
*    wa_header-loc_bus_area  = loc_bus_area.
*    wa_header-plangroup     = plangroup.
*    wa_header-equipment     = equipment.
*    wa_header-costcenter    = costcenter.
*    wa_header-start_date    = sy-datum.
*    wa_header-priority      = priority.
    APPEND wa_header TO it_header.
    CLEAR wa_header.

*    wa_operation-activity    = activity.
*    wa_operation-control_key = control_key.
*    wa_operation-description = description.
    APPEND wa_operation TO it_operation.
    CLEAR wa_operation.


*----------------------------------------------
*-- Textos
*----------------------------------------------
    DATA: i_ordem TYPE ztpm_d_m_ordem.
    IF i_ordem-ktext IS NOT INITIAL.

      CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
        EXPORTING
          i_string         = i_ordem-ktext
          i_tabline_length = 132
        TABLES
          et_table         = t_tab_text.

      DESCRIBE TABLE t_tab_text LINES DATA(l_lines).

      APPEND VALUE #( refnumber   = at_refnum
                      objecttype  = 'TEXT'
                      method      = 'CREATE'
                      objectkey   = at_orderid )
                TO it_methods.

      APPEND VALUE #( orderid     = at_orderid
                      langu       = sy-langu
                      textstart   = 1
                      textend     = l_lines )
                TO t_text.

      LOOP AT t_tab_text INTO w_tab_text.
        APPEND VALUE #( tdformat  = '*'
                        tdline    = w_tab_text )
                  TO t_text_lines.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods   = it_methods
        it_header    = it_header
        it_operation = it_operation
        return       = it_return.

*
**----------------------------------------------
**-- Executa BAPI
**----------------------------------------------
*    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
*      TABLES
*        it_methods    = t_methods
*        it_header     = t_header
*        it_operation  = t_operation
*        it_objectlist = t_objectlist
*        it_text       = t_text
*        it_text_lines = t_text_lines
*        return        = t_return.
**        it_permit     = lt_permit.

    CLEAR: wa_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 02 SECONDS.

    READ TABLE it_return INTO wa_return WITH KEY number = '112'.

*    CLEAR: me->zif_pm_data_equipament~at_numero_ordem.
*    me->zif_pm_data_equipament~at_numero_ordem = wa_return-message_v2.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = me->zif_pm_data_equipament~at_numero_ordem
*      IMPORTING
*        output = me->zif_pm_data_equipament~at_numero_ordem.

  ENDIF.


ENDFUNCTION.
