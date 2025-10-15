FUNCTION zmm_me21n_gera_pedido_compras.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SEQUENCIAL) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(IW_ZSDT0158) TYPE  ZSDT0158 OPTIONAL
*"     REFERENCE(I_ROUTE) TYPE  ROUTE DEFAULT 'ITAC01'
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_EBELN) TYPE  EBELN
*"  TABLES
*"      ET_BAPIRET2 STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA lw_0158 TYPE zsdt0158.

  IF i_sequencial IS NOT INITIAL.

    SELECT SINGLE * FROM zsdt0158
      INTO lw_0158
        WHERE sequencial = i_sequencial.

  ELSEIF iw_zsdt0158 IS NOT INITIAL.

    lw_0158 = iw_zsdt0158.

  ENDIF.

  CHECK lw_0158 IS NOT INITIAL.

  PERFORM f_gera_pedido_compras
    USING i_commit
          lw_0158
          i_route
 CHANGING ev_ebeln
          et_bapiret2[].

  IF ev_ebeln IS NOT INITIAL.

    lw_0158-ebeln = ev_ebeln.

    PERFORM f_save_zsdt0187_2 USING lw_0158.

  ENDIF.

ENDFUNCTION.
