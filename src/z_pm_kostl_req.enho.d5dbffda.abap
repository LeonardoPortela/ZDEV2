"Name: \FU:ME_REQUISITION_EXT\SE:BEGIN\EI
ENHANCEMENT 0 Z_PM_KOSTL_REQ.
  data: VKOSTl  type ebkn-kostl.

  "Se ainda não tem número da Ordem
  IF i_ebkn-aufnr(1) ne '%'.

    select single kostl
      into vkostl
      from aufk
      where aufnr = i_ebkn-aufnr.

    move vkostl to i_ebkn-kostl.

  ENDIF.

  IF sy-cprog = 'ZMMR047'.
    refresh ACC_TAB.
  ENDIF.

  IF sy-cprog = 'ZMMR196'.
    IF I_EBAN-preis = 0.
      I_EBAN-preis = '0.01'.
    ENDIF.
  ENDIF.

*  Removido por solictação Sr. Cleudo - Ch.128604 - MF
*  " Nunca eliminar o item  da requisição (ALRS [14:25:18] Emerson Botelho: CH 119911)
*  clear I_EBAN-loekz.
ENDENHANCEMENT.
