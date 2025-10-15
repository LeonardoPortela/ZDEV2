************************************************************************
* A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.   *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 12.03.2008                                          *
* Tipo de prg ...: Function                                            *
* Objetivo    ...: Start das RFCs de outbound de AP e AR com as        *
*                  informações da tabela ZFIT0006                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 12.03.2008    Michely              Criação              DEVK903640   *
*                                                                      *
************************************************************************
function z_fi_start_rfc_ap_ar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  select *
    from zfit0006
    into table it_dadosrfc.

  clear vg_testeabap_debug.

  loop at it_dadosrfc into wa_dadosrfc.
*    Caso o documento seja de estorno os camposa abixo devem ser iniciais.
*    Quando iniciais chama a function de AP/AR
    if ( wa_dadosrfc-bukrs_e is initial ) and
       ( wa_dadosrfc-belnr_e is initial ) and
       ( wa_dadosrfc-gjahr_e is initial ) and
       ( wa_dadosrfc-tcode_e is initial ).
*      Testa a variavel de debug.
      if ( vg_testeabap_debug is initial ).
        call function 'Z_FI_RETURN_PAYMENT_AP_AR' in background task
          destination 'NONE'
          exporting
            i_bukrs = wa_dadosrfc-bukrs
            i_augbl = wa_dadosrfc-belnr
            i_gjahr = wa_dadosrfc-gjahr
            i_tcode = wa_dadosrfc-tcode.

      else.
        call function 'Z_FI_RETURN_PAYMENT_AP_AR' starting new task 'PAYM'
          exporting
            i_bukrs = wa_dadosrfc-bukrs
            i_augbl = wa_dadosrfc-belnr
            i_gjahr = wa_dadosrfc-gjahr
            i_tcode = wa_dadosrfc-tcode.

      endif.

    else. " Quando não vazias chama a function de estorno.
*      Testa a variavel de debug.
      if ( vg_testeabap_debug is initial ).
        call function 'Z_FI_NF_CANCEL_LINK' in background task
          destination 'NONE'
          exporting
            i_bukrs   = wa_dadosrfc-bukrs
            i_belnr   = wa_dadosrfc-belnr
            i_gjahr   = wa_dadosrfc-gjahr
            i_bukrs_e = wa_dadosrfc-bukrs_e
            i_belnr_e = wa_dadosrfc-belnr_e
            i_gjahr_e = wa_dadosrfc-gjahr_e
            i_tcode   = wa_dadosrfc-tcode_e.
      else.
        call function 'Z_FI_NF_CANCEL_LINK' starting new task 'CLINK'
          exporting
            i_bukrs   = wa_dadosrfc-bukrs
            i_belnr   = wa_dadosrfc-belnr
            i_gjahr   = wa_dadosrfc-gjahr
            i_bukrs_e = wa_dadosrfc-bukrs_e
            i_belnr_e = wa_dadosrfc-belnr_e
            i_gjahr_e = wa_dadosrfc-gjahr_e
            i_tcode   = wa_dadosrfc-tcode_e.
      endif.
    endif.
    delete from zfit0006 where bukrs = wa_dadosrfc-bukrs
                           and belnr = wa_dadosrfc-belnr
                           and gjahr = wa_dadosrfc-gjahr.
  endloop.


endfunction.
