CLASS zcl_pm_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS consultar_notas_pend
      IMPORTING
        i_equnr                TYPE equi-equnr
      EXPORTING
        es_return_nota         TYPE BAPIRETURN
      RETURNING
        VALUE(rt_notification) TYPE eams_t_bapi2080_1
      RAISING
        cx_static_check.

    CLASS-METHODS consultar_ordens_pend
      IMPORTING
        i_equnr          TYPE equi-equnr
      EXPORTING
        et_return_ordens TYPE bapiret2_t
      RETURNING
        VALUE(rt_ordens) TYPE eams_t_order_listhead_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_pm_util IMPLEMENTATION.

  METHOD consultar_notas_pend.
    DATA: lv_equipamento TYPE equi-equnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_equnr
      IMPORTING
        output = lv_equipamento.

    CALL FUNCTION 'BAPI_ALM_NOTIF_LIST_EQUI'
      EXPORTING
        equipment         = lv_equipamento
        notification_date = '01011900'
      IMPORTING
        return            = es_return_nota
      TABLES
        notification      = rt_notification
      EXCEPTIONS
        OTHERS            = 1.

    DELETE rt_notification WHERE s_status CS 'MSEN'.

  ENDMETHOD.


  METHOD consultar_ordens_pend.
    DATA: tl_ranges TYPE TABLE OF bapi_alm_order_listhead_ranges,
          wl_ranges LIKE LINE OF tl_ranges.

    CLEAR wl_ranges.
    wl_ranges-field_name = 'SHOW_DOCS_WITH_FROM_DATE'.
    wl_ranges-low_value  = '10000101'.
    APPEND wl_ranges TO tl_ranges.

    CLEAR wl_ranges.
    wl_ranges-field_name = 'OPTIONS_FOR_EQUIPMENT'.
    wl_ranges-low_value  = i_equnr.
    APPEND wl_ranges TO tl_ranges.

    CLEAR wl_ranges.
    wl_ranges-field_name = 'OPTIONS_FOR_STATUS_EXCLUSIVE'.
    wl_ranges-sign       = 'I'.
    wl_ranges-option     = 'EQ'.
    wl_ranges-low_value  = 'I0045'.
    wl_ranges-high_value = 'I0046'.
    APPEND wl_ranges TO tl_ranges.

    CALL FUNCTION 'BAPI_ALM_ORDERHEAD_GET_LIST' "#EC CI_USAGE_OK[2438131]
      TABLES                                   "#EC CI_USAGE_OK[2669857]
        it_ranges = tl_ranges
        et_result = rt_ordens
        return    = et_return_ordens
      EXCEPTIONS
        OTHERS    = 1.

  ENDMETHOD.                    "CONSULTAR_ORDENS_PEND
ENDCLASS.

