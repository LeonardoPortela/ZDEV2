function zplancomp_email_excucao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WA_ZNOM_REMETENTE) TYPE  ZNOM_REMETENTE
*"  TABLES
*"      IT_HTML STRUCTURE  W3HTML
*"----------------------------------------------------------------------

  data: vg_html                type string,
        xnumero                type c length 15,
        wa_znom_reme_notas     type znom_reme_notas,
        it_j_1bnfdoc           type table of j_1bnfdoc with header line,
        it_j_1bnflin           type table of j_1bnflin with header line,
        it_znom_reme_notas     type table of znom_reme_notas with header line,
        it_znom_reme_notas_aux type table of znom_reme_notas with header line.

  select * into table it_znom_reme_notas
    from znom_reme_notas
   where id_nomeacao_tran eq wa_znom_remetente-id_nomeacao_tran
     and id_empresa       eq wa_znom_remetente-id_empresa
     and id_filial        eq wa_znom_remetente-id_filial
     and id_material      eq wa_znom_remetente-id_material
     and id_remetente     eq wa_znom_remetente-id_remetente.

  if not it_znom_reme_notas[] is initial.

    clear: it_znom_reme_notas_aux[].
    move it_znom_reme_notas[] to it_znom_reme_notas_aux[].
    sort it_znom_reme_notas_aux by docnum.
    delete adjacent duplicates from it_znom_reme_notas_aux comparing docnum.

    select * into table it_j_1bnfdoc
      from j_1bnfdoc
       for all entries in it_znom_reme_notas_aux
     where docnum eq it_znom_reme_notas_aux-docnum.

    clear: it_znom_reme_notas_aux[].
    move it_znom_reme_notas[] to it_znom_reme_notas_aux[].
    sort it_znom_reme_notas_aux by docnum itmnum.
    delete adjacent duplicates from it_znom_reme_notas_aux comparing docnum itmnum.

    select * into table it_j_1bnflin
      from j_1bnflin
       for all entries in it_znom_reme_notas_aux
     where docnum eq it_znom_reme_notas_aux-docnum
       and itmnum eq it_znom_reme_notas_aux-itmnum.

    vg_html = '<tr><td width="35%" colspan="2" align="center" bgcolor="FFFFFF"><font color="#FFFFFF" size=2><strong></strong></font></td>'.
    perform add tables it_html using vg_html.
    vg_html = '<td width="35%" colspan="4" align="center" bgcolor="FFFFFF">'.
    perform add tables it_html using vg_html.

    "Tabela de notas fiscais de entrada do produtor ********************************************
    perform add tables it_html using '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

    "Modelo
    "Número
    "Série
    "Quantidade
    concatenate '<tr bgcolor="B0C4DE">'
                '<td width="15%"><p align="center"><font color="#000030" size=1><b>Modelo</b></font></p></td>'
                '<td width="20%"><p align="center"><font color="#000030" size=1><b>Número</b></font></p></td>'
                '<td width="20%"><p align="center"><font color="#000030" size=1><b>Série</b></font></p></td>'
                '<td width="45%"><p align="center"><font color="#000030" size=1><b>Quantidade(kg)</b></font></p></td>'
                '</tr>' into vg_html.
    perform add tables it_html using vg_html.
    loop at it_znom_reme_notas into wa_znom_reme_notas.
      read table it_j_1bnfdoc with key docnum = wa_znom_reme_notas-docnum.
      read table it_j_1bnflin with key docnum = wa_znom_reme_notas-docnum
                                       itmnum = wa_znom_reme_notas-itmnum.
      concatenate '<tr><td width="15%"><p align="left"><font color="#000030" size=1><b>' it_j_1bnfdoc-model '</b></font></p></td>' into vg_html.
      if it_j_1bnfdoc-nfe is initial.
        concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>' it_j_1bnfdoc-nfnum '</b></font></p></td>' into vg_html.
      else.
        concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>' it_j_1bnfdoc-nfenum '</b></font></p></td>' into vg_html.
      endif.
      concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>' it_j_1bnfdoc-series '</b></font></p></td>' into vg_html.
      write: wa_znom_reme_notas-nr_quantidade to xnumero.
      shift:  xnumero left deleting leading space.
      concatenate vg_html '<td width="20%"><p align="right"> <font color="#000030" size=1><b>' xnumero           '</b></font></p></td></tr>' into vg_html.
      perform add tables it_html using vg_html.
    endloop.
    perform add tables it_html using '</table>'.
    "*******************************************************************************************
    "*******************************************************************************************

    vg_html = '</td></tr>'.
    perform add tables it_html using vg_html.

  endif.

endfunction.
