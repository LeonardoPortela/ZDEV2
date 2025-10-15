************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 09.05.2009                                          *
* Tipo de prg ...: include                                             *
* Objetivo    ...: EXIT para estorno de Tributos a Pagar               *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 09.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
* 01.03.2012    Desenvolvedor ABAP   Melhoria             DEVK920832   *
************************************************************************
data: lv_blart type bkpf-blart.
data: wg_acccr like line of t_acccr.
data: wg_tabix type sy-tabix.

*break abap.
*check sy-tcode = 'FB08'.
if sy-tcode eq 'FB08'.
  read table t_accit index 1.

  select single blart
    into lv_blart
    from bkpf
    where bukrs = t_accit-aworg_rev(4) and
          belnr = t_accit-awref_rev    and
          gjahr = t_accit-aworg_rev+4.

  check lv_blart = 'TB'.

  update zimp_cabecalho set estorno = 'X'
    where bukrs = t_accit-aworg_rev(4) and
          belnr = t_accit-awref_rev    and
          gjahr = t_accit-aworg_rev+4.

elseif sy-tcode eq 'KO88'
*    or sy-tcode eq 'CO88'
    or sy-tcode eq 'KO8G'.
  loop at t_acccr into wg_acccr
    where waers eq 'USD'
      and kursf is not initial.

    wg_tabix = sy-tabix.

    read table t_acccr "into wg_acccr
      with key awtyp = wg_acccr-awtyp
               posnr = wg_acccr-posnr
               waers = wg_acccr-waers
               kursf = '  0.00000'.
    if sy-subrc is initial.
       wg_acccr-wrbtr = t_acccr-wrbtr.
       wg_acccr-kursf = t_acccr-kursf.
*      move: wg_acccr-kursf to t_acccr-kursf.
      modify t_acccr from wg_acccr index wg_tabix.
    endif.
  endloop.
endif.
