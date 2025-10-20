function z_les_finaliza_vt.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(P_J_1BNFDOC) TYPE  J_1BNFDOC OPTIONAL
*"  EXPORTING
*"     VALUE(E_MSG_ERRO) TYPE  STRING
*"----------------------------------------------------------------------

  data: wa_headerdata       type bapishipmentheader,
        wa_headerdataaction type bapishipmentheaderaction,
        it_return           type table of bapiret2,
        wa_return           type bapiret2,
        wa_zcte_ciot        type zcte_ciot,
        wa_zcte_info_nota   type zcte_info_nota,
        wa_j_1bnfdoc        type j_1bnfdoc,
        wa_act_nota         type j_1bnfe_active,
        ref_number          like j_1binterf-xblnr.

  CLEAR: e_msg_erro.

  IF P_J_1BNFDOC IS NOT INITIAL.

    wa_j_1bnfdoc = P_J_1BNFDOC.
    sy-subrc     = 0.

  ELSE.

    call function 'Z_NFE_CTE_AUTORIZADO'
      exporting
        p_docnum       = p_docnum
      changing
        p_cabec        = wa_j_1bnfdoc
      exceptions
        cancelado      = 1
        nao_cancelado  = 2
        pendente       = 3
        nao_concluido  = 4
        nao_existe     = 5
        autorizado_uso = 6
        denegado       = 7
        others         = 8.

  ENDIF.

  if sy-subrc is initial.

    select single * into wa_zcte_ciot
      from zcte_ciot
     where docnum eq p_docnum.

    if wa_j_1bnfdoc-nfe is initial.
      call function 'J_1B_NF_NUMBER_CONDENSE'
        exporting
          nf_number  = wa_j_1bnfdoc-nfnum
          series     = wa_j_1bnfdoc-series
        importing
          ref_number = ref_number.
    else.
      call function 'J_1B_NF_NUMBER_CONDENSE'
        exporting
          series     = wa_j_1bnfdoc-series
          nf_number9 = wa_j_1bnfdoc-nfenum
        importing
          ref_number = ref_number.
    endif.

    check not wa_zcte_ciot-tknum is initial.

    wa_headerdata-shipment_num            = wa_zcte_ciot-tknum.
    wa_headerdata-external_id_1           = ref_number.
    wa_headerdata-external_id_2           = wa_zcte_ciot-nucontrato.
    wa_headerdata-status_shpmnt_end       = 'X'.
    wa_headerdataaction-external_id_1     = 'C'.
    wa_headerdataaction-external_id_2     = 'C'.
    wa_headerdataaction-status_shpmnt_end = 'C'.

    call function 'BAPI_SHIPMENT_CHANGE'
      exporting
        headerdata       = wa_headerdata
        headerdataaction = wa_headerdataaction
      tables
        return           = it_return.

    " Comita
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
    IF SY-SUBRC EQ 0.
     message id wa_return-id type 'S'
         number wa_return-number
           with wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
           into e_msg_erro.
    ENDIF.

  endif.

  select single * into wa_zcte_info_nota
    from zcte_info_nota
   where docnum_nf eq p_docnum.

  if ( sy-subrc is initial ) and ( wa_zcte_info_nota-nfe  eq 'X' ).
    select single * into wa_act_nota
      from j_1bnfe_active
     where docnum eq wa_zcte_info_nota-docnum_nf.
    if sy-subrc is initial.
      wa_zcte_info_nota-docnum9 = wa_act_nota-docnum9.
      wa_zcte_info_nota-cdv     = wa_act_nota-cdv.
      concatenate wa_act_nota-regio wa_act_nota-nfyear wa_act_nota-nfmonth wa_act_nota-stcd1
                  wa_act_nota-model wa_act_nota-serie  wa_act_nota-nfnum9 wa_act_nota-docnum9
                  wa_act_nota-cdv
             into wa_zcte_info_nota-chave.
    endif.
  endif.

endfunction.
