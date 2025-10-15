*&---------------------------------------------------------------------*
*& Include ZSD_AJUSTAR_PESOS_NF
*&---------------------------------------------------------------------*

******************************************
* guardar pesos para calculo
******************************************
FORM f_guardar_pesos USING p_menge
                           p_rmmme TYPE rmmme.

  FIELD-SYMBOLS: <fs_nfe> TYPE any.

  DATA: w_pesos_nf    TYPE znfe_pesos_nf,
        w_nfe_inbound TYPE znfe_inbound.

*---------------------------------
* devolucao sem informar doc referencia
*---------------------------------
  IF lineitem-charg IS INITIAL.
    ASSIGN ('(SAPLZNFE_INBOUND)wa_nfe_inbound') TO <fs_nfe>.
    IF sy-subrc = 0.
      w_nfe_inbound = <fs_nfe>.
    ELSE.
      IMPORT w_nfe_inbound FROM MEMORY ID 'ZNFE_INBOUND'.
    ENDIF.

    IF sy-subrc = 0 AND w_nfe_inbound IS NOT INITIAL.
      SELECT SINGLE zeile
        INTO @DATA(_zeile)
        FROM mseg
       WHERE mblnr = @w_nfe_inbound-nfe_base-mblnr
         AND mjahr = @w_nfe_inbound-nfe_base-mjahr.

      READ TABLE w_nfe_inbound-nfe_base-itens INTO DATA(_itens) WITH KEY ebeln = x4_rseg-ebeln
                                                                 ebelp = x4_rseg-ebelp.
      IF sy-subrc = 0.
        READ TABLE w_nfe_inbound-nfe_base-lotes INTO DATA(_lotes) WITH KEY prod_item = _itens-prod_item.
        IF sy-subrc = 0.
          lineitem-charg = _lotes-charg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*---------------------------------
* Work com pesos a ajustar
*---------------------------------
  w_pesos_nf-mblnr    = COND #( WHEN x4_rseg-mblnr IS INITIAL THEN w_nfe_inbound-nfe_base-mblnr ELSE x4_rseg-mblnr ).
  w_pesos_nf-mjahr    = COND #( WHEN x4_rseg-mjahr IS INITIAL THEN w_nfe_inbound-nfe_base-mjahr ELSE x4_rseg-mjahr ).
  w_pesos_nf-mblpo    = COND #( WHEN x4_rseg-mblpo IS INITIAL THEN _zeile                       ELSE x4_rseg-mblpo ).
  w_pesos_nf-matnr    = lineitem-matnr.
  w_pesos_nf-menge    = p_menge.
  w_pesos_nf-meins    = lineitem-meins.
  w_pesos_nf-gewei    = mara-gewei.
  w_pesos_nf-brgew    = wt-brgew.
  w_pesos_nf-ntgew    = wt-ntgew.
  w_pesos_nf-umrez    = p_rmmme-umrez.
  w_pesos_nf-umren    = p_rmmme-umren.
  APPEND w_pesos_nf  TO t_pesos_nf.

ENDFORM.

******************************************
* recalcular pesos
******************************************
FORM f_ajustar_pesos.

  DATA: lc_nfe_print TYPE REF TO zcl_im_cl_nfe_print,
        lv_bag       TYPE string,
        lv_tabix     TYPE sy-tabix,
        lv_erro      TYPE char01.

  CREATE OBJECT lc_nfe_print.

  CHECK nfheader-nftype = 'ZR'.

  LOOP AT t_pesos_nf INTO DATA(_pesos_nf).
    lv_tabix = sy-tabix.

    CHECK _pesos_nf-meins = 'BAG' OR _pesos_nf-meins = 'BIG'.

    SELECT SINGLE charg
      INTO @DATA(_charg)
      FROM mseg
     WHERE mblnr = @_pesos_nf-mblnr
       AND mjahr = @_pesos_nf-mjahr
       AND zeile = @_pesos_nf-mblpo.

    CHECK sy-subrc = 0.

    CALL METHOD lc_nfe_print->recupera_atributo
      EXPORTING
        iv_nome_attr = 'PESO BAG'
        iv_matnr     = _pesos_nf-matnr
        iv_charg     = _charg
      IMPORTING
        ev_valor     = lv_bag.

    lv_bag               = zcl_util=>get_string_numeric( CONV #( lv_bag ) ).

    _pesos_nf-brgew      = _pesos_nf-menge * lv_bag * _pesos_nf-umrez / _pesos_nf-umren.
    _pesos_nf-ntgew      = _pesos_nf-menge * lv_bag * _pesos_nf-umrez / _pesos_nf-umren.

    MODIFY t_pesos_nf FROM _pesos_nf INDEX lv_tabix.
  ENDLOOP.

  IF lv_bag IS NOT INITIAL.
    FREE: wt.
    LOOP AT t_pesos_nf INTO _pesos_nf.
      wt-gewei            = _pesos_nf-gewei.
      wt-brgew            = _pesos_nf-brgew.
      wt-ntgew            = _pesos_nf-ntgew.
      COLLECT wt.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
