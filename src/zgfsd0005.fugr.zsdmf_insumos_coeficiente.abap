FUNCTION zsdmf_insumos_coeficiente.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ICVA_KBERT) TYPE  VFPRC_ELEMENT_AMOUNT
*"     REFERENCE(IV_ICBS_KBERT) TYPE  VFPRC_ELEMENT_AMOUNT
*"     REFERENCE(IV_KWERT_ICMI) TYPE  VFPRC_ELEMENT_AMOUNT
*"     REFERENCE(IV_KWERT_ICMI_NOVO) TYPE  VFPRC_ELEMENT_AMOUNT
*"  EXPORTING
*"     REFERENCE(EV_DIF) TYPE  KWERT
*"     REFERENCE(EV_ICMS) TYPE  KWERT
*"     REFERENCE(EV_RB00_NOVO) TYPE  KWERT
*"----------------------------------------------------------------------

  ev_dif = iv_kwert_icmi - iv_kwert_icmi_novo.

  ev_icms = ( ev_dif * ( iv_icbs_kbert / 100 ) ) * ( iv_icva_kbert / 100 ).

  ev_rb00_novo = ev_icms - ev_dif.

ENDFUNCTION.
