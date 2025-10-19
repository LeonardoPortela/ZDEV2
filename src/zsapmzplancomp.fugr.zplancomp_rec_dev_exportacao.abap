function zplancomp_rec_dev_exportacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM_EXP) TYPE  J_1BDOCNUM OPTIONAL
*"     VALUE(P_DOCNUM_REC) TYPE  J_1BDOCNUM OPTIONAL
*"  TABLES
*"      IT_VBRP STRUCTURE  VBRP OPTIONAL
*"      IT_ZDOC_EXP_RECUSA STRUCTURE  ZDOC_EXP_RECUSA OPTIONAL
*"      IT_ZDOC_NF_PRODUTOR STRUCTURE  ZDOC_NF_PRODUTOR OPTIONAL
*"      IT_ZDOC_NF_RECUSA STRUCTURE  ZDOC_EXP_REC_NF OPTIONAL
*"  EXCEPTIONS
*"      FALTA_DOCNUM
*"      SEM_FATURA_REF
*"      DIRECAO_NOTA
*"      TIPO_NOTA
*"      FATURA_ESTORNADA
*"      ORDEM_ERRADA
*"      NOTA_FISCAL_EXP
*"      NAO_PLANEJADA
*"      CANCELADO
*"      FATURA_RECUSADA
*"      NOTA_FISCAL_COMPRO
*"----------------------------------------------------------------------

*"----------------------------------------------------------------------------------------------------
*" (Busca as fatura da Nota de Recusa/Devolução) ou
*" (Busca as fatura de Recusa/Devolução possíveis, da nota fiscal de venda ) - Usuário Seleciona
*"----------------------------------------------------------------------------------------------------

  data: qtd_recusas        type i,
        wa_znom_remetente  type znom_remetente,
        wa_zdoc_exp_recusa type zdoc_exp_recusa,
        wa_zdoc_nf_recusa  type zdoc_exp_rec_nf.

  clear: it_vbrp[], it_zdoc_exp_recusa_tela[], it_zdoc_nf_produtor[], it_zdoc_exp_recusa[], it_zdoc_nf_recusa[].

  if ( p_docnum_exp is initial ) and ( p_docnum_rec is initial ).

    message e033 raising falta_docnum.

    "Partindo do Documento de Recusa/Devolução
  elseif ( p_docnum_exp is initial ) and ( not p_docnum_rec is initial ).

    "Partindo do Documento de Recusado/Devolvido
    perform busca_recusas_nf_recusa     tables it_vbrp it_zdoc_exp_recusa using p_docnum_rec.
  elseif ( not p_docnum_exp is initial ) and ( p_docnum_rec is initial ).
    "Partindo do Documento de Exportação
    perform busca_recusas_nf_exportacao tables it_vbrp it_zdoc_exp_recusa using p_docnum_exp.
  endif.

  describe table it_zdoc_exp_recusa_tela lines qtd_recusas.

  clear: wa_zdoc_exp_recusa_tela.

  "Selecionar uma recusa.
  if qtd_recusas gt 1.
    call screen 1001 starting at 5 10 ending at 100 15.
    if wa_zdoc_exp_recusa_tela is initial.
      message e043 raising cancelado.
    endif.
  else.
    read table it_zdoc_exp_recusa_tela into wa_zdoc_exp_recusa_tela index 1.
  endif.

  if not wa_zdoc_exp_recusa_tela is initial.
    read table it_zdoc_exp_recusa
          into wa_zdoc_exp_recusa
      with key id_doc_exp   = wa_zdoc_exp_recusa_tela-id_doc_exp
               vbeln_re_exp = wa_zdoc_exp_recusa_tela-vbeln_re_exp
               posnr_re_exp = wa_zdoc_exp_recusa_tela-posnr_re_exp
               vbeln_ov_rec = wa_zdoc_exp_recusa_tela-vbeln_ov_rec
               posnr_ov_rec = wa_zdoc_exp_recusa_tela-posnr_ov_rec.
    "Consulta Notas Fiscais Vinculas em Remessa (livres)
    perform consulta_notas_da_exportacao tables it_zdoc_nf_produtor
                                          using wa_zdoc_exp_recusa_tela
                                                wa_znom_remetente.

    if it_zdoc_nf_produtor[] is initial.
      "Recusa/Devolução parte empresa
      if wa_zdoc_exp_recusa_tela-nm_qtd_rec le wa_znom_remetente-nr_parte_empresa.
        modify zdoc_exp_recusa from wa_zdoc_exp_recusa.
        wa_znom_remetente-nr_parte_empresa = wa_znom_remetente-nr_parte_empresa - wa_zdoc_exp_recusa-nm_quantidade.
        modify znom_remetente from wa_znom_remetente.
        commit work.
      else.
        message e044 with wa_znom_remetente-nr_parte_empresa raising cancelado.
      endif.
    else.
      "Recusa/Devolução parte remetente
      perform vincula_nf_produtor_recusa tables it_zdoc_nf_produtor it_zdoc_nf_recusa using wa_zdoc_exp_recusa_tela.

      if it_zdoc_nf_recusa[] is initial.
        message e043 raising cancelado.
      else.
        modify zdoc_exp_recusa from wa_zdoc_exp_recusa.
        loop at it_zdoc_nf_recusa into wa_zdoc_nf_recusa.
          modify zdoc_exp_rec_nf from wa_zdoc_nf_recusa.
        endloop.
        commit work.
      endif.

    endif.

  endif.

endfunction.
