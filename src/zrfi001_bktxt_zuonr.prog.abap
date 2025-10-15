************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 01.04.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Report para inversão de valores do campo BSEG-ZUONR *
*                  com BKPF-BKTXT                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 01.04.2008    Michely              Criação              DEVK903794   *
* 26.05.2008    Michely              Alteração            DEVK904068   *
************************************************************************

report  zrfi001_bktxt_zuonr.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
tables: bkpf.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_bkpf,
        bukrs            like bkpf-bukrs,
        belnr            like bkpf-belnr,
        gjahr            like bkpf-gjahr,
        bktxt            like bkpf-bktxt,
      end of wa_bkpf,

      begin of wa_bseg,
        bukrs            like bseg-bukrs,
        belnr            like bseg-belnr,
        gjahr            like bseg-gjahr,
        zuonr            like bseg-zuonr,
      end of wa_bseg,

      begin of wa_doc,
        bukrs            like bseg-bukrs,
        belnr            like bseg-belnr,
        gjahr            like bseg-gjahr,
        buzei            like bseg-buzei,
      end   of wa_doc.

data: begin of wa_bseg2.
        include structure bseg.
data: end   of wa_bseg2.

data: begin of wa_bkpf2.
        include structure bkpf.
data: end   of wa_bkpf2.

data: it_bkpf            like standard table of wa_bkpf,
      it_bseg            like standard table of wa_bseg,
      it_bseg2           like standard table of wa_bseg2,
      it_bkpf2           like standard table of wa_bkpf2,
      it_doc             like standard table of wa_doc.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
parameters: p_bktx      radiobutton group tipo default 'X' user-command vlr,
            p_sgtxt     radiobutton group tipo.
selection-screen begin of block b1 with frame title text-s10.
parameters: p_bukrs    like bkpf-bukrs,
            p_gjahr    like bkpf-gjahr.
selection-screen end   of block b1.

*----------------------------------------------------------------------*
* Event at selection-Screen output
*----------------------------------------------------------------------*
* Efetuo o bloquei de determinados campos nas telas de alteração.
at selection-screen output.
  if p_sgtxt is initial.
    loop at screen.
      if ( screen-name eq 'P_BUKRS' ) or ( screen-name eq 'P_GJAHR' ).
        screen-input = 0.
        modify screen.
      endif.
    endloop.
  else.
    loop at screen.
      if ( screen-name eq 'P_BUKRS' ) or ( screen-name eq 'P_GJAHR' ).
        screen-input = 1.
        modify screen.
      endif.
    endloop.
  endif.


*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  if p_sgtxt is initial.
    perform f_seleciona_dados.
    perform f_atualiza_tabelas.
  else.
    perform f_seleciona_dadossgtxt.
    perform f_atualiza_sgtxt.
  endif.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Seleciona os dados da tabela BKPF com BKPF-BSTAT=D e os dados
*       correspondentes na tabela BSEG
*----------------------------------------------------------------------*
form f_seleciona_dados .
  select bukrs belnr gjahr bktxt
    from bkpf
    into table it_bkpf
   where bstat eq 'D'.

  select bukrs belnr gjahr zuonr
    from bseg
    into table it_bseg
     for all entries in it_bkpf
   where bukrs eq it_bkpf-bukrs
     and belnr eq it_bkpf-belnr
     and gjahr eq it_bkpf-gjahr.
endform.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  f_atualiza_tabelas
*&---------------------------------------------------------------------*
*       Atualiza as tabelas BKPF e BSEG
*----------------------------------------------------------------------*
form f_atualiza_tabelas .
  data: vl_rgatu         type n length 6,
        vl_msg           type c length 50.

  sort: it_bseg by bukrs belnr gjahr,
        it_bkpf by bukrs belnr gjahr.

  loop at it_bkpf into wa_bkpf.
    vl_rgatu = vl_rgatu + 1.
    read table it_bseg into wa_bseg with key bukrs = wa_bkpf-bukrs
                                             belnr = wa_bkpf-belnr
                                             gjahr = wa_bkpf-gjahr
                                             binary search.
    update bkpf set bktxt = wa_bseg-zuonr
              where bukrs = wa_bkpf-bukrs
                and belnr = wa_bkpf-belnr
                and gjahr = wa_bkpf-gjahr.

    update bseg set zuonr = wa_bkpf-bktxt
              where bukrs = wa_bseg-bukrs
                and belnr = wa_bseg-belnr
                and gjahr = wa_bseg-gjahr.
  endloop.

  concatenate 'Atualizados'
              vl_rgatu
              'registros na tabela BSEG e BKPF com sucesso'
              into vl_msg separated by space.

  message s000(z01) with vl_msg.

endform.                    " f_atualiza_tabelas
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOSSGTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_seleciona_dadossgtxt .
  perform f_mensagem using 'Buscando dados na BSEG...'.
  select bukrs belnr gjahr buzei
    from bseg
    into table it_doc
   where bukrs eq p_bukrs
     and gjahr eq p_gjahr
     and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
      or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
      or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSIS...'.

  select bukrs belnr gjahr buzei
    from bsis
    appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSIK...'.

  select bukrs belnr gjahr buzei
    from bsik
    appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSID...'.

  select bukrs belnr gjahr buzei
    from bsid
    appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSAS...'.

  select bukrs belnr gjahr buzei
    from bsas
    appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSAK...'.

  select bukrs belnr gjahr buzei
   from bsak
   appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  perform f_mensagem using 'Buscando dados na BSAD...'.

  select bukrs belnr gjahr buzei
   from bsad
   appending table it_doc
  where bukrs eq p_bukrs
    and gjahr eq p_gjahr
    and ( ( sgtxt eq '' )     or ( sgtxt like '%OK%' )
     or ( sgtxt like '%ok%' ) or ( sgtxt like '%Ok%' )
     or ( sgtxt like '%oK%' ) ).

  check sy-subrc eq 0.

  delete adjacent duplicates from it_doc.

  perform f_mensagem using 'Buscando dados na BKPF...'.

  select *
    from bkpf
    into table it_bkpf2
     for all entries in it_doc
   where bukrs eq it_doc-bukrs
     and belnr eq it_doc-belnr
     and gjahr eq it_doc-gjahr.

endform.                    " F_SELECIONA_DADOSSGTXT
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_SGTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_atualiza_sgtxt .
  data: vl_sgtxt         type c length 50,
        vl_rgatu         type n length 6,
        vl_msg           type c length 50,
        vl_res           type c length 1.

  sort: it_bkpf2 by bukrs belnr gjahr,
        it_bseg2 by bukrs belnr gjahr.

  clear vl_rgatu.
  loop at it_bkpf2 into wa_bkpf2.

    perform f_mensagem using 'Atualizando dados...'.

    select *
      from bseg
      into table it_bseg2
     where bukrs eq wa_bkpf2-bukrs
       and belnr eq wa_bkpf2-belnr
       and gjahr eq wa_bkpf2-gjahr.

    loop at it_bseg2 into wa_bseg2 where bukrs eq wa_bkpf2-bukrs
                                     and belnr eq wa_bkpf2-belnr
                                     and gjahr eq wa_bkpf2-gjahr.

      vl_rgatu = vl_rgatu + 1.
      call function 'Z_1B_HISTORICAL_DESCRIPTION'
        exporting
          line_bseg = wa_bseg2
          line_bkpf = wa_bkpf2
        importing
          e_sgtxt   = vl_sgtxt.

      update bseg set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsis set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsik set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsid set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsas set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsak set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

      update bsad set sgtxt = vl_sgtxt
                where bukrs = wa_bseg2-bukrs
                  and belnr = wa_bseg2-belnr
                  and gjahr = wa_bseg2-gjahr
                  and buzei = wa_bseg2-buzei.

    endloop.
  endloop.
  concatenate 'Atualizado o campo SGTXT em'
              vl_rgatu
              'registos na tabela BSEG'
              into vl_msg separated by space.

  message s000(z01) with vl_msg.
endform.                    " F_ATUALIZA_SGTXT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0354   text
*----------------------------------------------------------------------*
form f_mensagem  using    p_msg.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = p_msg.
endform.                    " F_MENSAGEM
