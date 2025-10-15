"Name: \TY:CL_TAX_CALC_BR_MM\ME:CALCULATE_ICMS_COMP\SE:END\EI
ENHANCEMENT 0 Z_TAX_CALC_NCM.
*
  SELECT SINGLE *
    from marc
    into @data(WMARC)
    where matnr = @MS_KOMP-MATNR
    and   werks = @MS_KOMP-WERKS.

  "REGIO
  SELECT SINGLE *
    from lfa1
    into @DATA(WLFA1)
    where lifnr = @MS_KOMK-LIFNR.

  SELECT SINGLE *
    from T001w
    into @DATA(WT001W)
    where WERKS = @MS_KOMP-WERKS..

  "STEUC NCM
  select SINGLE *
    from J_1BTXIC3
    into @DATA(WJ_1BTXIC3)
    WHERE land1    = 'BR'
    and   SHIPFROM = @WLFA1-REGIO
    and   SHIPTO   = @WT001W-REGIO
    and   GRUOP    = '41'
    and   VALUE    = @WMARC-STEUC.

  IF sy-subrc = 0 and WJ_1BTXIC3-rate > 0.
    lv_icop_rate = WJ_1BTXIC3-rate / 100.
    ev_icms_comp =
      iv_val_incl_tax * lv_icop_rate * ms_tax_data-icmscompbase.
    ev_icms_comp_base = iv_val_incl_tax.
    ev_icms_comp_rate = lv_icop_rate.
  ENDIF.

ENDENHANCEMENT.
