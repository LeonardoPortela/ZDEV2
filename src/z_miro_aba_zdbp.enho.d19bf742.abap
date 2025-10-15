"Name: \TY:CL_J_1B_BLOCK_INVOICE_CHECK\IN:IF_J_1B_BLOCK_INVOICE_CHECK\ME:VALIDATE_PO_FILLED\SE:END\EI
ENHANCEMENT 0 Z_MIRO_ABA_ZDBP.
*

*  if  <fs_invoice_items>-ebeln is not INITIAL.
*      select SINGLE bsart
*        into @data(v_bsart)
*        from ekko
*        where ebeln =  @<fs_invoice_items>-ebeln.
*      IF v_bsart = 'ZDBP'.
*         select SINGLE *
*           from ekpo
*           into @data(w_ekpo)
*           where ebeln = @<fs_invoice_items>-ebeln.
*        IF sy-subrc = 0.
*           if w_ekpo-knttp EQ 'K'.
*              select SINGLE *
*              from ekpo
*              into @data(w_ekpo2)
*              where ebeln = @w_ekpo-BEDNR
*              and   knttp = ' '.
*              if sy-subrc = 0.
*                  cv_tabs_used = cv_tabs_used - 1.
*                  clear cv_purchasing_doc_flag.
*              endif.
*           endif.
*        endif.
*      ENDIF.

*  endif.
ENDENHANCEMENT.
