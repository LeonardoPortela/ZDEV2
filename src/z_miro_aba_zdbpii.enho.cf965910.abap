"Name: \TY:CL_J_1B_BLOCK_INVOICE_CHECK\IN:IF_J_1B_BLOCK_INVOICE_CHECK\ME:VERIFY_NF_DETAILS\SE:END\EI
ENHANCEMENT 0 Z_MIRO_ABA_ZDBPII.
*
*   Delivered on SAP Note 2599501.
    FIELD-SYMBOLS:
      <fs_invoice_items> TYPE LINE OF mmcr_tdrseg.
    "Check if PO is filled
    LOOP AT it_invoice_items ASSIGNING  <fs_invoice_items>.
      "PO if filled
      IF <fs_invoice_items>-ebeln IS NOT INITIAL.
        select SINGLE bsart
        into @data(v_bsart)
        from ekko
        where ebeln =  @<fs_invoice_items>-ebeln.
        IF v_bsart = 'ZDBP' or v_bsart = 'YDBP'.
          clear cs_message.
        endif.
        EXIT.
      ENDIF.
    ENDLOOP.
ENDENHANCEMENT.
