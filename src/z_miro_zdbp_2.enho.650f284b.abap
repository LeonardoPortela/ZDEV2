"Name: \PR:SAPLJ1BI\FO:NF_ITEM_CREATE\SE:END\EI
ENHANCEMENT 0 Z_MIRO_ZDBP_2.
* Neste ponto desfaz tudo feito na Z_MIRO_ZDBP_1
  CLEAR : VBSART,VBEDNR,W_EKPO,tabix.

   LOOP AT x4_rseg.
      IF x4_rseg-ebeln is not INITIAL and ( x4_rseg-KNTTP = 'K' or x4_rseg-KNTTP = 'F' ).
        SELECT SINGLE bsart into VBSART from ekko where ebeln = x4_rseg-ebeln.
        if VBSART = 'ZDBP' or VBSART = 'YDBP'.
            SELECT SINGLE BEDNR INTO VBEDNR FROM EKPO WHERE EBELN = x4_rseg-ebeln.
            SELECT SINGLE * INTO W_EKPO FROM EKPO WHERE EBELN = VBEDNR.
            IF W_EKPO-KNTTP = ''.
               tabix = sy-tabix.
               exit.
            Endif.
        endif.
      ENDIF.
   ENDLOOP.
   "atualiza linha debito posterior e zera os materiais de estoque para não sairem na nota
   if tabix gt 0.
      LOOP AT x4_rseg.
        IF x4_rseg-ebeln is not INITIAL and ( x4_rseg-KNTTP = 'K' or x4_rseg-KNTTP = 'F' ).
           x4_rseg-wrbtr = 0.
           x4_rseg-xlifo = ''.
           modify x4_rseg INDEX tabix TRANSPORTING wrbtr xlifo.
        else.
           x4_rseg-wrbtr = x4_rseg-wmwst.
           x4_rseg-wmwst = 0.
           x4_rseg-xlifo = ''.
           modify x4_rseg INDEX sy-tabix TRANSPORTING wrbtr xlifo wmwst.
        ENDIF.
     ENDLOOP.
   endif.


ENDENHANCEMENT.
