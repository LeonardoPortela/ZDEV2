function z_les_email_cockpit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(COD_TRANSP) TYPE  TDLNR
*"     REFERENCE(COD_POSTO) TYPE  LIFNR
*"     REFERENCE(COD_LOTE) TYPE  CHAR10
*"  EXCEPTIONS
*"      NAO_LOCALIZADA
*"      NAO_EMAIL
*"----------------------------------------------------------------------

  data: it_zlest0013      type table of zlest0013 initial size 0 with header line, "Conhecimentos
        ti_cockpit_lote   type zles_cockpit_lote_t,
        ti_cockpit_lancto type zles_cockpit_lancto_t,
        ti_cockpit_lancad type zles_cockpit_lancto_t,
        ti_cockpit_deltas type zles_cockpit_delta_t,
        ti_cockpit_acdec  type zles_cockpit_acrescdecres_t, "CSB
        ti_cockpit_logs   type table of zlest0008 initial size 0 with header line,
        s_trans           type lxhme_range_c10_t,
        s_posto           type lxhme_range_c10_t,
        s_lote            type lxhme_range_c10_t,
        w_                type lxhme_range_c10,
        vg_cod_transp     type  tdlnr,
        vg_cod_posto      type  lifnr,
        vg_cod_lote       type  char10,
        wa_cockpit_lote   type zles_cockpit_lote,
        wa_cockpit_lancto type zles_cockpit_lancto,
        wa_cockpit_acdec  type zles_cockpit_acrescdecres, "CSB
        wa_cockpit_logs   type zlest0008.

  data: peso_origem      type brgew_ap,
        peso_confirmado  type brgew_ap,
        peso_importado   type brgew_ap,
        vlrorigem        type kwert,
        vlrconfirmado    type kwert,
        vlrdiferenca     type kwert,
        vlrprogramado    type kwert.

  data: xpeso_origem      type c length 19,
        xpeso_confirmado  type c length 19,
        xpeso_importado   type c length 19,
        xvlrorigem        type c length 16,
        xvlrconfirmado    type c length 16,
        xvlrdiferenca     type c length 16,
        xvlrprogramado    type c length 16.


  "Variáveis de e-mail
  data: document_data type sodocchgi1,
        packing_list  type table of sopcklsti1 with header line,
        receivers     type table of somlreci1 with header line,
        listobject    type table of abaplist initial size 0,
        html          type table of w3html initial size 0,
        lt_emais      type table of adr6 initial size 0 with header line,
        vg_html       type string,
        vg_data       type c length 10,
        wa_lfbk       type lfbk,
        wa_bnka       type bnka.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = cod_transp
    importing
      output = vg_cod_transp.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = cod_posto
    importing
      output = vg_cod_posto.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = cod_lote
    importing
      output = vg_cod_lote.

  w_-sign   = 'I'.
  w_-option = 'EQ'.
  w_-low    = vg_cod_transp.
  w_-high   = vg_cod_transp.
  append w_ to s_trans.

  w_-low    = vg_cod_posto.
  w_-high   = vg_cod_posto.
  append w_ to s_posto.

  w_-low    = vg_cod_lote.
  w_-high   = vg_cod_lote.
  append w_ to s_lote.

  call function 'Z_LES_EMAIL'
    exporting
      v_posto  = vg_cod_posto
      v_trans  = vg_cod_transp
    tables
      lt_emais = lt_emais.

  if lt_emais[] is initial.
    message e055 raising nao_email with vg_cod_posto vg_cod_transp.
  endif.

  call function 'Z_LES_COCKPIT_AUTOMACAO_POSTO'
    exporting
      rt_trasnportador = s_trans[]
      rt_posto         = s_posto[]
      rt_lote          = s_lote[]
    tables
      t_lotes          = ti_cockpit_lote
      t_lanctos        = ti_cockpit_lancto
      t_deltas         = ti_cockpit_deltas
      t_acrdecr        = ti_cockpit_acdec. "CSB

  if ti_cockpit_lote[] is initial.
    message e054 raising nao_localizada with cod_lote.
  else.
    read table ti_cockpit_lote index 1 into wa_cockpit_lote.
  endif.

  move ti_cockpit_lancto[] to ti_cockpit_lancad[].

  delete ti_cockpit_lancto where id_origem_zles ne '13'.
  delete ti_cockpit_lancad where ( chvid ne '19' and chvid ne '20' and chvid ne '26' and chvid ne '28' ). " CSB Add 26 e 28

  select single * into wa_lfbk
    from lfbk
   where lifnr eq wa_cockpit_lote-codposto
     and bvtyp eq '0001'.

  if sy-subrc is initial.
    select single * into wa_bnka
      from bnka
     where banks eq wa_lfbk-banks
       and bankl eq wa_lfbk-bankl.
  endif.

  select * into table ti_cockpit_logs
    from zlest0008
   where lote eq vg_cod_lote.

  perform add tables html using '<tr><td>'.

  perform add tables html using '<DIV align="left">'.
  perform add tables html using '<table border="0">'.

  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Empresa:</b></font></p></td>'
              '<td><p align="left"><font color="#000000" size=3><b>' wa_cockpit_lote-dscodtrp '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Posto:</b></font></p></td>'
              '<td><p align="left"><font color="#000000" size=3><b>' wa_cockpit_lote-dscodposto '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  write wa_cockpit_lote-vencimento to vg_data.
  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Data do Pagamento:</b></font></p></td>'
              '<td><p align="right"><font color="#000000" size=3><b>' vg_data '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Lote:</b></font></p></td>'
              '<td><p align="right"><font color="#000000" size=3><b>' wa_cockpit_lote-lote '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  if ( not wa_lfbk is initial ) and ( not wa_bnka is initial ).
    concatenate '<tr>'
                '<td><p align="left"><font color="#000000" size=3><b>Banco:</b></font></p></td>'
                '<td><p align="left"><font color="#000000" size=3><b>' wa_bnka-bankl(3) '-' wa_bnka-banka
                '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.

    concatenate '<tr>'
                '<td><p align="left"><font color="#000000" size=3><b>Conta Corrente:</b></font></p></td>'
                '<td><p align="left"><font color="#000000" size=3><b>' wa_lfbk-bankn
                '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.


    if not wa_lfbk-bkont is initial.
      concatenate wa_lfbk-bankl+4(11) '-' wa_lfbk-bkont into vg_html.
    else.
      vg_html = wa_lfbk-bankl+4(11).
    endif.

    concatenate '<tr>'
                '<td><p align="left"><font color="#000000" size=3><b>Agencia:</b></font></p></td>'
                '<td><p align="left"><font color="#000000" size=3><b>' vg_html
                '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.
  endif.

  write wa_cockpit_lote-vlrorigem to xvlrprogramado.
  shift:  xvlrprogramado left deleting leading space.
  "CONCATENATE '<tr>'
  "            '<td><p align="left"><font color="#000000" size=3><b>Total Arquivo:</b></font></p></td>'
  "            '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
  "           '</tr>' INTO vg_html.
  "PERFORM add TABLES html USING vg_html.

  write wa_cockpit_lote-vlrimportado to xvlrprogramado.
  shift:  xvlrprogramado left deleting leading space.
  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Valor Importado:</b></font></p></td>'
              '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  if wa_cockpit_lote-vlrrecusado gt 0.
    write wa_cockpit_lote-vlrrecusado to xvlrprogramado.
    shift:  xvlrprogramado left deleting leading space.
  else.
    xvlrprogramado = '0,00'.
  endif.
  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Valor Recusado:</b></font></p></td>'
              '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.

  write wa_cockpit_lote-vlrrealizado to xvlrprogramado.
  shift:  xvlrprogramado left deleting leading space.

*  if ti_cockpit_acdec is not initial. " CSB
*    read table ti_cockpit_acdec index 1 into wa_cockpit_acdec.
*    if ( wa_cockpit_acdec-chvid eq '26' ).
*      multiply wa_cockpit_acdec-wrbtr by -1.
*    endif.
*
*    write:  wa_cockpit_acdec-wrbtr to xvlrdiferenca.
*    shift:  xvlrdiferenca   left deleting leading space.
*
*    xvlrprogramado = xvlrprogramado + xvlrdiferenca.
*    concatenate '<tr>'
*                '<td><p align="left"><font color="#000000" size=3><b>Valor Realizado:</b></font></p></td>'
*                '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
*                '</tr>' into vg_html.
*    perform add tables html using vg_html.
*
*    write wa_cockpit_lote-vlr_acrec_desc to xvlrprogramado.
*    shift:  xvlrprogramado left deleting leading space.
*    concatenate '<tr>'
*                '<td><p align="left"><font color="#000000" size=3><b>Acresc./Desc.:</b></font></p></td>'
*                '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
*                '</tr>' into vg_html.
*    perform add tables html using vg_html.
*
*  else. " Continua aqui
    concatenate '<tr>'
                '<td><p align="left"><font color="#000000" size=3><b>Valor Realizado:</b></font></p></td>'
                '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.

    write wa_cockpit_lote-vlr_acrec_desc to xvlrprogramado.
    shift:  xvlrprogramado left deleting leading space.
    concatenate '<tr>'
                '<td><p align="left"><font color="#000000" size=3><b>Acresc./Desc.:</b></font></p></td>'
                '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.


*  IF wa_cockpit_lote-vlrrecusado LT 0.
*    wa_cockpit_lote-vlrconfirmado = wa_cockpit_lote-vlrconfirmado + wa_cockpit_lote-vlrrecusado.
*  ENDIF.
*
*  IF wa_cockpit_lote-vlrdiferenca GT 0.
*    wa_cockpit_lote-vlrconfirmado = wa_cockpit_lote-vlrconfirmado - wa_cockpit_lote-vlrdiferenca.
*  ENDIF.

  write wa_cockpit_lote-vlr_a_pagar to xvlrprogramado.
  shift:  xvlrprogramado left deleting leading space.
  concatenate '<tr>'
              '<td><p align="left"><font color="#000000" size=3><b>Saldo a Pagar:</b></font></p></td>'
              '<td><p align="right"><font color="#000000" size=3><b>' xvlrprogramado '</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables html using vg_html.
  perform add tables html using '</table>'.
  perform add tables html using '</DIV>'.
  perform add tables html using '</td></tr>'.

  "Documentos Importados por Arquivo
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  vlrorigem       = 0.
  vlrconfirmado   = 0.
  vlrdiferenca    = 0.
  vlrprogramado   = 0.
  peso_origem     = 0.
  peso_confirmado = 0.
  peso_importado  = 0.

  if not ti_cockpit_lancto[] is initial.

    perform add tables html using '<tr><td>'.
    perform add tables html using '<BR>'.
    perform add tables html using '<DIV align=left><FONT face=Arial color=#ff0000 size=4><STRONG>Documentos Importados com Sucesso</STRONG></FONT></DIV>'.
    perform add tables html using '<DIV align=left>'.
    perform add tables html using '<FONT face=Arial color=#0000ff size=2>'.

    perform add tables html using '<table align="left" cellspacing="0" cellpadding="6" border="1">'.

    concatenate '<tr><td colspan="6" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
                'Valores processados'
                '</strong></font></td></tr>'
           into vg_html.
    perform add tables html using vg_html.

    concatenate '<tr>'
                '<td><font color="#000030" size=2><b>Histórico</b></font></td>'
                '<td><p align="right"><font color="#000030" size=2><b>DACTE</b></font></p></td>'
                '<td><p align="right"><font color="#000030" size=2><b>Carta Frete</b></font></p></td>'
                '<td><p align="right"><font color="#000030" size=2><b>Peso Importado</b></font></p></td>'
                '<td><p align="right"><font color="#000030" size=2><b>Vlr. Importado</b></font></p></td>'
                '<td><p align="right"><font color="#000030" size=2><b>Vlr. Programado</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.

    sort ti_cockpit_lancto by conhec ctafrete.

  endif.

  loop at ti_cockpit_lancto into wa_cockpit_lancto.

    write: wa_cockpit_lancto-vlrimportado    to xvlrorigem,
           wa_cockpit_lancto-vlrconfirmado   to xvlrconfirmado,
           wa_cockpit_lancto-vlrdiferenca    to xvlrdiferenca,
           wa_cockpit_lancto-vlrprogramado   to xvlrprogramado,
           wa_cockpit_lancto-peso_origem     to xpeso_origem,
           wa_cockpit_lancto-peso_confirmado to xpeso_confirmado,
           wa_cockpit_lancto-peso_importado  to xpeso_importado .

    shift:  xvlrorigem       left deleting leading space,
            xvlrconfirmado   left deleting leading space,
            xvlrdiferenca    left deleting leading space,
            xvlrprogramado   left deleting leading space,
            xpeso_origem     left deleting leading space,
            xpeso_confirmado left deleting leading space,
            xpeso_importado  left deleting leading space.

    concatenate '<tr>'
                '<td><font color="#000000" size=2>' wa_cockpit_lancto-deschvid '</font></td>'
                '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_lancto-conhec   '</font></p></td>'
                '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_lancto-ctafrete '</font></p></td>'
                '<td><p align="right"><font color="#000000" size=2>' xpeso_importado            '</font></p></td>'
                '<td><p align="right"><font color="#000000" size=2>' xvlrorigem                 '</font></p></td>'
                '<td><p align="right"><font color="#000000" size=2>' xvlrprogramado             '</font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.


    peso_origem     = peso_origem     + wa_cockpit_lancto-peso_origem.
    peso_confirmado = peso_confirmado + wa_cockpit_lancto-peso_confirmado.
    peso_importado  = peso_importado  + wa_cockpit_lancto-peso_importado.
    vlrorigem       = vlrorigem       + wa_cockpit_lancto-vlrimportado.
    vlrconfirmado   = vlrconfirmado   + wa_cockpit_lancto-vlrconfirmado.
    vlrdiferenca    = vlrdiferenca    + wa_cockpit_lancto-vlrdiferenca.
    vlrprogramado   = vlrprogramado   + wa_cockpit_lancto-vlrprogramado.

  endloop.


  if not ti_cockpit_lancto[] is initial.

    write: vlrorigem       to xvlrorigem,
           vlrconfirmado   to xvlrconfirmado,
           vlrdiferenca    to xvlrdiferenca,
           vlrprogramado   to xvlrprogramado,
           peso_origem     to xpeso_origem,
           peso_confirmado to xpeso_confirmado,
           peso_importado  to xpeso_importado .

    shift:  xvlrorigem       left deleting leading space,
            xvlrconfirmado   left deleting leading space,
            xvlrdiferenca    left deleting leading space,
            xvlrprogramado   left deleting leading space,
            xpeso_origem     left deleting leading space,
            xpeso_confirmado left deleting leading space,
            xpeso_importado  left deleting leading space.

    concatenate '<tr>'
                '<td><font color="#000000" size=2><b>Total Históricos</b></b></font></td>'
                '<td></td>'
                '<td></td>'
                '<td><p align="right"><font color="#000000" size=2><b>'                   '</b></font></p></td>'
                '<td><p align="right"><font color="#000000" size=2><b>' xvlrorigem       '</b></font></p></td>'
                '<td><p align="right"><font color="#000000" size=2><b>' xvlrprogramado   '</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.

    perform add tables html using '</table>'.
    perform add tables html using '</DIV>'.
    perform add tables html using '</td></tr>'.
  endif.

  perform add tables html using '<tr><td>'.


  " Acréscimo e Decréscimo
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  vlrprogramado   = 0.

*  if ti_cockpit_lancad[] is initial and ti_cockpit_acdec[] is not initial.
*
*    perform add tables html using '<tr><td>'.
*    perform add tables html using '<BR>'.
*    perform add tables html using '<DIV align=left><FONT face=Arial color=#ff0000 size=4><STRONG>Lançamento de Acréscimo e Decréscimo</STRONG></FONT></DIV>'.
*    perform add tables html using '<DIV align=left>'.
*    perform add tables html using '<FONT face=Arial color=#0000ff size=2>'.
*
*    perform add tables html using '<table align="left" cellspacing="0" cellpadding="4" border="1">'.
*    concatenate '<tr><td colspan="4" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
*                'Valores Lançados'
*                '</strong></font></td></tr>'
*           into vg_html.
*    perform add tables html using vg_html.
*
*    concatenate '<tr>'
*                '<td><font color="#000030" size=2><b>Histórico</b></font></td>'
*                '<td><p align="right"><font color="#000030" size=2><b>DACTE</b></font></p></td>'
*                '<td><p align="right"><font color="#000030" size=2><b>Carta Frete</b></font></p></td>'
*                '<td><p align="right"><font color="#000030" size=2><b>Vlr. Programado</b></font></p></td>'
*                '</tr>' into vg_html.
*    perform add tables html using vg_html.
*
*
*    loop at ti_cockpit_acdec into wa_cockpit_acdec.
*      if ( wa_cockpit_acdec-chvid eq '26' ).
*        multiply wa_cockpit_acdec-wrbtr by -1.
*      endif.
*
*      write:  wa_cockpit_acdec-wrbtr   to xvlrprogramado.
*      shift:  xvlrprogramado   left deleting leading space.
*      concatenate '<tr>'
*                  '<td><font color="#000000" size=2>' wa_cockpit_acdec-sgtxt '</font></td>'
*                  '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_acdec-conhecimento   '</font></p></td>'
*                  '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_lancto-ctafrete      '</font></p></td>'
*                  '<td><p align="right"><font color="#000000" size=2>' xvlrprogramado                  '</font></p></td>'
*                  '</tr>' into vg_html.
*      perform add tables html using vg_html.
*      vlrprogramado   = vlrprogramado   + wa_cockpit_acdec-wrbtr.
*    endloop.
*
*    if not ti_cockpit_acdec[] is initial.
*      write:  vlrprogramado    to xvlrprogramado.
*      shift:  xvlrprogramado   left deleting leading space.
*      concatenate '<tr>'
*                  '<td><font color="#000000" size=2><b>Total Históricos</b></b></font></td>'
*                  '<td></td>'
*                  '<td></td>'
*                  '<td><p align="right"><font color="#000000" size=2><b>' xvlrprogramado   '</b></font></p></td>'
*                  '</tr>' into vg_html.
*      perform add tables html using vg_html.
*      perform add tables html using '</table>'.
*      perform add tables html using '</DIV>'.
*      perform add tables html using '</td></tr>'.
*    endif.
*    perform add tables html using '<tr><td>'.
*
*
*  else. " CSB

    if not ti_cockpit_lancad[] is initial.
      perform add tables html using '<tr><td>'.
      perform add tables html using '<BR>'.
      perform add tables html using '<DIV align=left><FONT face=Arial color=#ff0000 size=4><STRONG>Lançamento de Acréscimo e Decréscimo</STRONG></FONT></DIV>'.
      perform add tables html using '<DIV align=left>'.
      perform add tables html using '<FONT face=Arial color=#0000ff size=2>'.

      perform add tables html using '<table align="left" cellspacing="0" cellpadding="4" border="1">'.
      concatenate '<tr><td colspan="4" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
                  'Valores Lançados'
                  '</strong></font></td></tr>'
             into vg_html.
      perform add tables html using vg_html.

      concatenate '<tr>'
                  '<td><font color="#000030" size=2><b>Histórico</b></font></td>'
                  '<td><p align="right"><font color="#000030" size=2><b>DACTE</b></font></p></td>'
                  '<td><p align="right"><font color="#000030" size=2><b>Carta Frete</b></font></p></td>'
                  '<td><p align="right"><font color="#000030" size=2><b>Vlr. Programado</b></font></p></td>'
                  '</tr>' into vg_html.
      perform add tables html using vg_html.
    endif.

    loop at ti_cockpit_lancad into wa_cockpit_lancto.
      if ( wa_cockpit_lancto-chvid eq '20' or  wa_cockpit_lancto-chvid eq '26'  or  wa_cockpit_lancto-chvid eq '28' ).
        multiply wa_cockpit_lancto-vlrprogramado by -1.
      endif.

      write: wa_cockpit_lancto-vlrprogramado   to xvlrprogramado.
      shift:  xvlrprogramado   left deleting leading space.
      concatenate '<tr>'
                  '<td><font color="#000000" size=2>' wa_cockpit_lancto-deschvid '</font></td>'
                  '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_lancto-conhec   '</font></p></td>'
                  '<td><p align="right"><font color="#000000" size=2>' wa_cockpit_lancto-ctafrete '</font></p></td>'
                  '<td><p align="right"><font color="#000000" size=2>' xvlrprogramado '</font></p></td>'
                  '</tr>' into vg_html.
      perform add tables html using vg_html.
      vlrprogramado   = vlrprogramado   + wa_cockpit_lancto-vlrprogramado.
    endloop.

    if not ti_cockpit_lancad[] is initial.
      write: vlrprogramado   to xvlrprogramado.
      shift:  xvlrprogramado   left deleting leading space.
      concatenate '<tr>'
                  '<td><font color="#000000" size=2><b>Total Históricos</b></b></font></td>'
                  '<td></td>'
                  '<td></td>'
                  '<td><p align="right"><font color="#000000" size=2><b>' xvlrprogramado   '</b></font></p></td>'
                  '</tr>' into vg_html.
      perform add tables html using vg_html.
      perform add tables html using '</table>'.
      perform add tables html using '</DIV>'.
      perform add tables html using '</td></tr>'.
    endif.
    perform add tables html using '<tr><td>'.
*  endif. " Fim CSB

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  if not ti_cockpit_logs[] is initial.
    perform add tables html using '<BR>'.
    perform add tables html using '<DIV align=left><FONT face=Arial color=#ff0000 size=4><STRONG>Log de Processamento de Importação de Arquivo</STRONG></FONT></DIV>'.
    perform add tables html using '<DIV align="left">'.
    perform add tables html using '<table align="left" cellspacing="0" cellpadding="2" border="1">'.
    concatenate '<tr><td colspan="2" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
                'Log de Processamento do Arquivo'
                '</strong></font></td></tr>'
           into vg_html.
    perform add tables html using vg_html.
    concatenate '<tr>'
                '<td><font color="#000030" size=2><b>Sequência</b></font></td>'
                '<td><font color="#000030" size=2><b>Mensagem</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.
  endif.

  sort ti_cockpit_logs by cont.

  data:
        texto   type char50,
        assunto type char50.

  texto = 'Programação. de Pag. a Postos - lote:'.

  concatenate texto wa_cockpit_lote-lote into assunto separated by space.

  loop at ti_cockpit_logs into wa_cockpit_logs.
    concatenate '<tr>'
                '<td><font color="#000000" size=2>' wa_cockpit_logs-cont  '</font></td>'
                '<td><font color="#000000" size=2>' wa_cockpit_logs-msgv1 '</font></td>'
                '</tr>' into vg_html.
    perform add tables html using vg_html.
  endloop.

  if not ti_cockpit_logs[] is initial.
    perform add tables html using '</table>'.
    perform add tables html using '</DIV>'.
  endif.
  perform add tables html using '</td></tr>'.

  perform add tables html using '<tr><td>'.
  perform add tables html using '<BR>'.
  concatenate '<DIV align=left><FONT face=Arial color=#000030 size=1><STRONG>'
              '<p align=center>'
              'Contato via e-mail: <a href="mailto:pagamento.fretes@grupomaggi.com.br">pagamento.fretes@grupomaggi.com.br</a>'
              '</p>'
              '</STRONG></FONT></DIV>'
              into vg_html.
  perform add tables html using vg_html.
  perform add tables html using '</td></tr>'.

  document_data-obj_name  = 'ProcArqC'.
  document_data-obj_descr = assunto.
  document_data-no_change = 'X'.

  packing_list-head_start = 1.
  packing_list-head_num   = 60.
  packing_list-body_start = 1.
  packing_list-body_num   = 999.
  packing_list-doc_type   = 'HTM'.
  append packing_list.

  loop at lt_emais.
    receivers-receiver = lt_emais-smtp_addr.
    receivers-rec_type = 'U'.
    receivers-express  = 'X'.
    append receivers.
  endloop.

  if not receivers[] is initial.
    call function 'ZHTML_ENVIA_EMAIL_CP'
      exporting
        i_titulo      = 'Cockpit - Pagamento a Postos'
        document_data = document_data
      tables
        packing_list  = packing_list
        html          = html
        receivers     = receivers.
  endif.


endfunction.
