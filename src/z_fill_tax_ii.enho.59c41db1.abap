"Name: \PR:SAPLJ_1B_NFE\FO:MAP_IMPORT_DI_ADI\SE:END\EI
ENHANCEMENT 0 Z_FILL_TAX_II.

  IF lt_xml_badi_di[] IS NOT INITIAL.

    DATA: wl_rfc_det_tax  TYPE j_1bnfe_s_rfc_det_tax.

    READ TABLE gt_rfc_tax_ii INTO DATA(wl_rfc_tax_ii) WITH KEY id = p_lineid.
    IF SY-SUBRC NE 0.
      CLEAR: wl_rfc_tax_ii.

      wl_rfc_tax_ii-id          = p_lineid.
      wl_rfc_tax_ii-v_bc        = xmli-o_vbc.
      wl_rfc_tax_ii-v_desp_adu  = xmli-o_vdespadu.
      wl_rfc_tax_ii-v_ii        = xmli-o_vii.
      wl_rfc_tax_ii-v_iof       = xmli-o_viof.

      DATA(inc_tax) = abap_false.
      LOOP AT gt_rfc_det_tax ASSIGNING FIELD-SYMBOL(<fs_rfc_det_tax>) WHERE id = p_lineid.
        <fs_rfc_det_tax>-ii_ref = p_lineid.
        inc_tax                 = abap_true.
      ENDLOOP.

      if inc_tax eq abap_false.
        CLEAR: wl_rfc_det_tax.
        wl_rfc_det_tax-id        = p_lineid.
        wl_rfc_det_tax-ii_ref    = p_lineid.
        wl_rfc_det_tax-vtottrib  = xmli-vtottrib.
      ENDIF.

      APPEND wl_rfc_tax_ii TO gt_rfc_tax_ii.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
