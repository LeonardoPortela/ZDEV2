FUNCTION zfi_vip_counterparties_bank.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LFA1) TYPE  LFA1
*"     REFERENCE(I_CREATION) TYPE  CHAR1
*"  TABLES
*"      T_LFBK STRUCTURE  LFBK
*"----------------------------------------------------------------------
  DATA: it_lfbk TYPE vmds_lfbk_t.

  DATA(obj_vip) = NEW zcl_integra_vip( ).

  IF ( i_creation = abap_true ).
    EXIT.
  ENDIF.

  IF ( i_lfa1 IS NOT INITIAL AND t_lfbk[] IS NOT INITIAL ).
**<<<------"188303 - NMS - INI------>>>
*    it_lfbk[] = CORRESPONDING #( t_lfbk[] ).
    it_lfbk[] = VALUE #( FOR lfbk IN t_lfbk ( mandt         = lfbk-mandt
                                              lifnr         = lfbk-lifnr
                                              banks         = lfbk-banks
                                              bankl         = lfbk-bankl
                                              bankn         = lfbk-bankn && lfbk-bkont
                                              bkont         = lfbk-bkont
                                              bvtyp         = lfbk-bvtyp
                                              xezer         = lfbk-xezer
                                              bkref         = lfbk-bkref
                                              koinh         = lfbk-koinh
                                              bank_guid     = lfbk-bank_guid
                                              tech_rectyp   = lfbk-tech_rectyp
                                              ebpp_accname  = lfbk-ebpp_accname
                                              ebpp_bvstatus = lfbk-ebpp_bvstatus
                                              kovon         = lfbk-kovon
                                              kobis         = lfbk-kobis ) ).
**<<<------"188303 - NMS - FIM------>>>
    obj_vip->set_counterparties_bank(
      EXPORTING
        i_lfa1 = i_lfa1     " Categoria de tabela para LFA1 (ordenada)
        t_lfbk = it_lfbk[]   " Categoria de tabela para LFBK (ordenada)
        i_creation = i_creation "Preencher com ABAP_TRUE caso criação (XD01, XK01)
    ).
  ENDIF.

ENDFUNCTION.
