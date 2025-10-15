************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 14.03.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Report para rodar em job a função de start para as  *
*                  a RFC de outbound.                                  *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 14.03.2008    Michely              Criação              DEVK903656   *
* 28.03.2008    Michely              Correções            DEVK903752   *
*                                                                      *
************************************************************************

report  zrfi001_start_rfc.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
data: wa_dadosrfc       like zfit0006,
      it_dadosrfc       like standard table of wa_dadosrfc.

*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
data:
     vg_msg             type c length 20.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s00.
parameters: p_reg      type n length 6,
            c_task     as checkbox.
selection-screen end of block b0.

selection-screen begin of block b1 with frame title text-s01.
parameters: p_belnr    like bseg-belnr,
            p_bel_e    like bseg-augbl.
selection-screen end of block b1.


*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  perform f_seleciona_dados.

  loop at it_dadosrfc into wa_dadosrfc.
*    Caso o documento seja de estorno os camposa abixo devem ser iniciais.
*    Quando iniciais chama a function de AP/AR
    vg_msg = 'Processando...'.
    perform f_evita_time_out using vg_msg.
    if p_reg is not initial.
      if sy-tabix > p_reg.
        exit.
      endif.
    endif.

    if ( wa_dadosrfc-bukrs_e is initial ) and
    ( wa_dadosrfc-belnr_e is initial ) and
    ( wa_dadosrfc-gjahr_e is initial ) and
    ( wa_dadosrfc-tcode_e is initial ) and
    ( wa_dadosrfc-laufd   is initial ) and
    ( wa_dadosrfc-laufi   is initial ).
      if c_task is initial.
        call function 'Z_FI_RETURN_PAYMENT_AP_AR'
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

    else.
      if ( wa_dadosrfc-laufd   is initial ) and
      ( wa_dadosrfc-laufi   is initial ).
        " Quando não vazias chama a function de estorno.
        if c_task is initial.
          call function 'Z_FI_NF_CANCEL_LINK'
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
      else.
        if c_task is initial.
          call function 'Z_FI_APF110_RETURN_PAYMENT'
            exporting
              laufd = wa_dadosrfc-laufd
              laufi = wa_dadosrfc-laufi.
        else.
          call function 'Z_FI_APF110_RETURN_PAYMENT' starting new task 'PAYM'
            exporting
              laufd = wa_dadosrfc-laufd
              laufi = wa_dadosrfc-laufi.
        endif.
      endif.
    endif.
    wa_dadosrfc-status = 'E'.
    modify it_dadosrfc from wa_dadosrfc.
    update zfit0006 set status = 'E' where bukrs   = wa_dadosrfc-bukrs
                                       and belnr   = wa_dadosrfc-belnr
                                       and gjahr   = wa_dadosrfc-gjahr
                                       and bukrs_e = wa_dadosrfc-bukrs_e
                                       and belnr_e = wa_dadosrfc-belnr_e
                                       and gjahr_e = wa_dadosrfc-gjahr_e
                                       and laufd   = wa_dadosrfc-laufd
                                       and laufi   = wa_dadosrfc-laufi.
  endloop.




*  call function 'Z_FI_START_RFC_AP_AR2'.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Seleciona dados conforme parametros de seleção.
*----------------------------------------------------------------------*
form f_seleciona_dados.
  if p_belnr is initial and p_bel_e is initial.
    select *
      from zfit0006
      into table it_dadosrfc
     where status ne 'E'. "Busco todos os documentos diferentes de E-Enviado
*  elseif p_bel_e is initial.
*    select *
*      from zfit0006
*      into table it_dadosrfc
*     where belnr eq p_belnr. "Busco todos os documentos diferentes de E-Enviado
  else.
    select *
      from zfit0006
      into table it_dadosrfc
     where belnr   eq p_belnr
       and belnr_e eq p_bel_e. "Busco todos os documentos diferentes de E-Enviado
  endif.

endform.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  f_evita_time_out
*&---------------------------------------------------------------------*
*       Para exibir mensagem de processando
*----------------------------------------------------------------------*
form f_evita_time_out using value(p_msg).
  call function 'SAPGUI_PROGRESS_INDICATOR'
  exporting
    text = p_msg.
endform.                    " f_evita_time_out
