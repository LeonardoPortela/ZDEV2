************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 31.03.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Report para limpar tabela ZFIT0006 que sera exec.   *
*                  por um job semanal.                                 *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 31.03.2008    Michely              Criação              DEVK903774   *
*                                                                      *
************************************************************************
report  zrfi001_zfit0006_del.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
data: wa_dadosrfc       like zfit0006,
      it_dadosrfc       like standard table of wa_dadosrfc,
      vg_data           type d.


*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
*Função para calculo da data.
  call function 'RP_CALC_DATE_IN_INTERVAL'
    exporting
      date      = sy-datum
      days      = 5
      months    = 0
      signum    = '-'
      years     = 0
    importing
      calc_date = vg_data.

*Busco todos os documentos iguais a E-Enviado e com data de 5 dias a antes da atual.
  select *
    from zfit0006
    into table it_dadosrfc
   where status eq 'E'
     and data   le vg_data.

  loop at it_dadosrfc into wa_dadosrfc where status eq 'E'.
    delete from zfit0006 where bukrs   = wa_dadosrfc-bukrs
                           and belnr   = wa_dadosrfc-belnr
                           and gjahr   = wa_dadosrfc-gjahr
                           and bukrs_e = wa_dadosrfc-bukrs_e
                           and belnr_e = wa_dadosrfc-belnr_e
                           and gjahr_e = wa_dadosrfc-gjahr_e
                           and laufd   = wa_dadosrfc-laufd
                           and laufi   = wa_dadosrfc-laufi.
  endloop.
