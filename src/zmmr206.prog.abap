*&---------------------------------------------------------------------*
*& Report ZMMR206
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr206.
data: e_messagem_erro type  string,
      e_sucesso       type  char01,
      e_belnr         type  re_belnr,
      e_gjahr         type  gjahr.
parameters: pchave   type zde_chave_doc_e no-display,
            pestorno type char1 no-display,
            pcpf     type string no-display.

check pchave is not initial.

call function 'ZFI_GERA_MIRO'
  exporting
    i_chave_nfe     = pchave
    i_ck_estornar   = pestorno
    i_cpf           = pcpf
  importing
    e_messagem_erro = e_messagem_erro
    e_sucesso       = e_sucesso
    e_belnr         = e_belnr
    e_gjahr         = e_gjahr.

call function 'ZDENQUEUE_NFE_INBOUND'
  exporting
    chave = pchave.

update zib_nfe_dist_ter set belnr         = e_belnr
                            gjahr         = e_gjahr
                            sucesso       = e_sucesso
                            mensagem_erro = e_messagem_erro
where chave_nfe =  pchave.

commit work.
