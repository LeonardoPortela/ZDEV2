FUNCTION Z_SALDO_CT_ACUM_MOEDA_SET.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_RACCT) TYPE  RACCT
*"     REFERENCE(I_DT_LANC) TYPE  BUDAT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_WAERS) TYPE  WAERS
*"     REFERENCE(I_RASSC) TYPE  RASSC
*"  EXPORTING
*"     REFERENCE(E_TSL) TYPE  FINS_VTCUR12
*"     REFERENCE(E_KSL) TYPE  FINS_VTCUR12
*"     REFERENCE(E_OSL) TYPE  FINS_VTCUR12
*"--------------------------------------------------------------------

  TYPES: BEGIN OF ty_saldo,
           racct TYPE racct,
           rassc TYPE rassc,
           rwcur TYPE rwcur,
           tsl   TYPE fins_vtcur12,
           ksl   TYPE fins_vtcur12,
           osl   TYPE fins_vtcur12,
         END OF ty_saldo.

  DATA: it_saldo TYPE STANDARD TABLE OF ty_saldo INITIAL SIZE 0,
        wa_saldo TYPE ty_saldo,
        lr_waers TYPE waers.
  FREE: it_saldo.
  CLEAR: lr_waers,wa_saldo.

  SELECT * FROM zi_saldo_ct_acum_moeda_pfutura( p_bukrs = @i_bukrs , p_dt_lanc = @i_dt_lanc, p_racct = @i_racct, p_waers = @I_waers , p_rassc = @i_rassc ) INTO TABLE @DATA(lt_saldo_fututo).
  SELECT * FROM zi_saldo_ct_acum_moeda_paberto( p_bukrs = @i_bukrs , p_dt_lanc = @i_dt_lanc, p_racct = @i_racct, p_waers = @I_waers , p_rassc = @i_rassc ) INTO TABLE @DATA(lt_saldo_aberto).

  APPEND LINES OF lt_saldo_fututo TO it_saldo.
  APPEND LINES OF lt_saldo_aberto TO it_saldo.
  FREE: lt_saldo_fututo,lt_saldo_aberto.

  LOOP AT it_saldo INTO wa_saldo.

    IF wa_saldo-tsl IS NOT INITIAL.
      e_tsl = e_tsl + wa_saldo-tsl.
    ENDIF.

    IF wa_saldo-ksl IS NOT INITIAL.
      e_ksl = e_ksl + wa_saldo-ksl.
    ENDIF.

    IF wa_saldo-osl IS NOT INITIAL.
      e_osl = e_osl + wa_saldo-osl.
    ENDIF.

  ENDLOOP.

  FREE: it_saldo.

ENDFUNCTION.
