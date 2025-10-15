FUNCTION zpm_atualiza_objetos_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AUFNR) TYPE  AUFK-AUFNR
*"     VALUE(I_NOTAS) TYPE  ZPMT0020_T
*"  TABLES
*"      I_OPROL STRUCTURE  BAPI_ALM_OLIST_RELATION
*"----------------------------------------------------------------------

  DATA:
    lv_obzae        TYPE objk-obzae,
    ls_header       TYPE bapi_alm_order_header_e,
    lt_return       TYPE TABLE OF bapiret2,
    lt_oprol        TYPE TABLE OF ioprol,
    lv_objnr2       TYPE caufvd-objnr,
    lt_oprol2       TYPE TABLE OF ioprol,
    lt_oprol3       TYPE TABLE OF bapi_alm_olist_relation,
    ls_header_notif TYPE bapi2080_nothdre,
    ls_caufvd       TYPE caufvd,
    lt_riwol        TYPE TABLE OF riwol,
    g_call_by_iw28  TYPE c,
    lv_obknr        TYPE objk-obknr,
    t_methods       TYPE bapi_alm_order_method_t,
    t_operation     TYPE bapi_alm_order_operation_t.

  LOOP AT i_notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

    IF sy-tabix EQ 1 AND i_oprol[] IS INITIAL.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'IOPEXT_INSERT_OL_NOTIF'
      EXPORTING
        i_aufnr     = i_aufnr
        i_qmnum     = <fs_notas>-qmnum
        i_obknr_ins = lv_obknr
        i_obzae_ins = lv_obzae.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDLOOP.


  CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
    EXPORTING
      number    = i_aufnr
    IMPORTING
      es_header = ls_header
    TABLES
      return    = lt_return.

  IF ls_header-routing_no IS NOT INITIAL AND i_oprol[] IS INITIAL.

    READ TABLE i_notas ASSIGNING <fs_notas> INDEX 1.
    IF sy-subrc IS INITIAL.

      SELECT obknr
        FROM objk
        INTO lv_obknr
        UP TO 1 ROWS
        WHERE ihnum = <fs_notas>-qmnum.
      ENDSELECT.
      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'ALM_ME_NOTIFICATION_GETDETAIL2'
          EXPORTING
            notif_no            = <fs_notas>-qmnum
          IMPORTING
            notification_header = ls_header_notif.

        APPEND INITIAL LINE TO lt_oprol ASSIGNING FIELD-SYMBOL(<fs_oprol>).
        <fs_oprol>-aufpl = ls_header-routing_no.
        <fs_oprol>-obknr = lv_obknr.
        <fs_oprol>-aplzl = '00000001'.
        <fs_oprol>-obzae = '1'.
        <fs_oprol>-vbkz  = 'I'.

        CALL FUNCTION 'IOPOL_OPROL_SAVE'
          TABLES
            ioprol_db = lt_oprol.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        APPEND VALUE #( refnumber = '000001' objecttype = 'OPERATION' method = 'CHANGE'  objectkey = i_aufnr && '0010' ) TO t_methods.
        APPEND VALUE #( refnumber = '' objecttype = '' method = 'SAVE'  objectkey = i_aufnr ) TO t_methods.

        APPEND INITIAL LINE TO t_operation ASSIGNING FIELD-SYMBOL(<fs_operation>).

        <fs_operation>-activity    = '0010'.
        <fs_operation>-description =  ls_header_notif-short_text.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods   = t_methods
            it_operation = t_operation
            return       = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      ENDIF.

    ENDIF.

  ENDIF.


ENDFUNCTION.
