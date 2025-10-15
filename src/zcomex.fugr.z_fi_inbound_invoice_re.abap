FUNCTION z_fi_inbound_invoice_re.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_INVOICE_RE STRUCTURE  ZFI_INVOICE_RE
*"----------------------------------------------------------------------

  DATA: t_zfit0041 TYPE TABLE OF zfit0041.

  DATA : wa_invoice_re   TYPE zfi_invoice_re,
         wa_zfit0041     TYPE zfit0041.

  DATA : vl_status TYPE zfit0036-status.

  LOOP AT t_invoice_re INTO wa_invoice_re.

    clear : wa_zfit0041.

    SELECT SINGLE *
      INTO wa_zfit0041
      FROM zfit0041
     WHERE nr_re      = wa_invoice_re-nr_re
       AND nr_invoice = wa_invoice_re-nr_invoice.

    IF sy-subrc IS NOT INITIAL.

      MOVE-CORRESPONDING wa_invoice_re TO wa_zfit0041 .
      wa_zfit0041-rg_atualizado = 'N'.

      MODIFY zfit0041 FROM wa_zfit0041.

    ENDIF.

  ENDLOOP.


  COMMIT WORK.

ENDFUNCTION.
