"Name: \PR:SAPLJ1BI\FO:NF_ITEM_CREATE\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIRO_ZDBP_1.
* Neste ponto acumula os valores dos materiais lançados como debito posterior na linha do material de serviço e zera os valores dos materiais de estoque para nao sairem na nota
  data: W_CAMPO(40),
        VBSART type ekko-BSART,
        VBEDNR TYPE ekpo-BEDNR,
        vvalor type bsis-WRBTR,
        tabix  type sy-tabix,
        W_EKPO TYPE EKPO.

   clear: tabix, vvalor.


   LOOP AT x4_rseg.
      IF x4_rseg-ebeln is not INITIAL and ( x4_rseg-KNTTP = 'K' or x4_rseg-KNTTP = 'F' ).
        SELECT SINGLE bsart into VBSART from ekko where ebeln = x4_rseg-ebeln.
        if VBSART = 'ZDBP' or VBSART = 'YDBP'.
            SELECT SINGLE BEDNR INTO VBEDNR FROM EKPO WHERE EBELN = x4_rseg-ebeln.
            SELECT SINGLE * INTO W_EKPO FROM EKPO WHERE EBELN = VBEDNR.
            IF W_EKPO-KNTTP = ''.
               tabix = sy-tabix.
            Endif.
        endif.
      ENDIF.
      add  x4_rseg-wrbtr to vvalor.
   ENDLOOP.
   "atualiza linha debito posterior e zera os materiais de estoque para não sairem na nota
   if tabix gt 0.
      LOOP AT x4_rseg.
        IF x4_rseg-ebeln is not INITIAL and ( x4_rseg-KNTTP = 'K' or x4_rseg-KNTTP = 'F' ).
           x4_rseg-wrbtr = vvalor.
           x4_rseg-xlifo = 'X'.
           modify x4_rseg INDEX tabix TRANSPORTING wrbtr xlifo.
        else.
           x4_rseg-wmwst = x4_rseg-wrbtr.
           x4_rseg-wrbtr = 0.
           x4_rseg-xlifo = 'X'.
           modify x4_rseg INDEX sy-tabix TRANSPORTING wrbtr xlifo wmwst.
        ENDIF.
     ENDLOOP.
   endif.

ENDENHANCEMENT.
