*----------------------------------------------------------------------*
***INCLUDE LZSAPMZPLANCOMPF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ADD
*&---------------------------------------------------------------------*
*       Add código HTML - Truncar
*----------------------------------------------------------------------*

form add  tables   p_html  structure w3html
          using    p_texto type string.
  call function 'ZHTML_ADD'
    exporting
      i_texto = p_texto
    tables
      it_html = p_html.
endform.                    " ADD

*&---------------------------------------------------------------------*
*&      Form  MONTA_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HTML  text
*----------------------------------------------------------------------*
form monta_cabecalho  tables p_html structure w3html
                      using  wa_znom_transporte type znom_transporte
                             wa_znom_remetente  type zplac_nom_remetente.

  data: vg_html type string.

  "Início cabeçalho
  perform add tables p_html using '<tr><td>'.
  perform add tables p_html using '<BR>'.
  perform add tables p_html using '<DIV align=center><FONT face=Arial color="000000" size=4><STRONG>Programação de Embarque</STRONG></FONT></DIV>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '<FONT face=Arial color=#0000ff size=2>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '</td></tr>'.

  perform add tables p_html using '<tr><td>'.
  concatenate 'Navio:' wa_znom_transporte-ds_nome_transpor into vg_html separated by space.
  concatenate '<DIV align=left><FONT face=Arial color="000000" size=2><STRONG>' vg_html '</STRONG></FONT></DIV>' into vg_html.
*  perform add tables p_html using '</td></tr>'.

  perform add tables p_html using vg_html.

  perform add tables p_html using '</td></tr>'.

endform.                    " MONTA_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  MONTA_CABECALHO_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HTML  text
*----------------------------------------------------------------------*
form monta_cabecalho_p  tables p_html structure w3html
                        using  wa_znom_transporte type znom_transporte
                               wa_znom_remetente  type zplac_nom_remetente.

  data: vg_html type string.

  "Início cabeçalho
  perform add tables p_html using '<tr><td>'.
  perform add tables p_html using '<BR>'.
  perform add tables p_html using '<DIV align=center><FONT face=Arial color="000000" size=4><STRONG>Programação de Embarque (Performance)</STRONG></FONT></DIV>'.
  perform add tables p_html using '<DIV align=left></div>'.
  perform add tables p_html using '<FONT face=Arial color=#0000ff size=2></font>'.
  perform add tables p_html using '<DIV align=left></div>'.
  perform add tables p_html using '<DIV align=left></div>'.
  perform add tables p_html using '<DIV align=left></div>'.
  perform add tables p_html using '</td></tr>'.

  perform add tables p_html using '<tr><td>'.
  concatenate 'Navio:' wa_znom_transporte-ds_nome_transpor into vg_html separated by space.
  concatenate '<DIV align=left><FONT face=Arial color="000000" size=2><STRONG>' vg_html '</STRONG></FONT></DIV>' into vg_html.
  perform add tables p_html using '</td></tr>'.

  perform add tables p_html using vg_html.


endform.                    " MONTA_CABECALHO_P


*&---------------------------------------------------------------------*
*&      Form  MONTA_DETALHE
*&---------------------------------------------------------------------*
*       Preenche detalhes do e-mail de programação, com os remetente da
*       mercadoria
*----------------------------------------------------------------------*
form monta_detalhe  tables p_html               structure w3html
                    using  p_znom_remetente_alv type zplac_nom_remetente
                           volume_total         type j_1bnetqty
                           volume_sem           type j_1bnetqty
                    changing p_primeiro         type c.

  data: vg_html type string,
        xnumero type c length 15.

  if p_znom_remetente_alv is initial.
    write: volume_total to xnumero.
    shift: xnumero left deleting leading space.
    volume_total = 0.
    perform add tables p_html using '<tr>'.
    concatenate vg_html '<td width="20%"><p align="right"><font color="#000030" size=1><b>' xnumero '</b></font></p></td>' into vg_html.
    concatenate vg_html '<td width="80%" colspan="5" align="center" bgcolor="FFFFFF"><font color="#FFFFFF" size=2><strong></strong></font></td>' into vg_html.
    perform add tables p_html using vg_html.
    perform add tables p_html using '</tr>'.
  else.
    perform add tables p_html using '<tr>'.
    if p_znom_remetente_alv-id_remetente is initial.
      concatenate         '<td width="20%"><p align="left"><font color="#000030" size=1><b>'   p_znom_remetente_alv-name_filial '</b></font></p></td>' into vg_html.
      concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>'       p_znom_remetente_alv-stcd1_filial '</b></font></p></td>' into vg_html.
      concatenate vg_html '<td width="20%"><p align="center"><font color="#000030" size=1><b>SEM ME</b></font></p></td>' into vg_html.
      concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b></b></font></p></td>'   into vg_html.
    endif.

    if not p_znom_remetente_alv-id_remetente is initial.
      if ( volume_sem gt 0 ) or ( p_primeiro is initial ).
        if volume_total gt 0.
          write: volume_total to xnumero.
          shift:  xnumero left deleting leading space.
          volume_total = 0.
          concatenate vg_html '<td width="20%"><p align="right"><font color="#000030" size=1><b>' xnumero '</b></font></p></td>' into vg_html.
          concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b></b></font></p></td>'            into vg_html.
        else.
          vg_html = '<td width="35%" colspan="2" align="center" bgcolor="FFFFFF"><font color="#FFFFFF" size=2><strong></strong></font></td>'.
        endif.
      else.
        concatenate         '<td width="20%"><p align="left"><font color="#000030" size=1><b>' p_znom_remetente_alv-name_filial '</b></font></p></td>' into vg_html.
        concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>'     p_znom_remetente_alv-stcd1_filial '</b></font></p></td>' into vg_html.
      endif.
      concatenate vg_html '<td width="20%"><p align="left"><font color="#000030" size=1><b>'       p_znom_remetente_alv-name1  '</b></font></p></td>' into vg_html.
      concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>'       p_znom_remetente_alv-stcd1  '</b></font></p></td>' into vg_html.
    endif.

    clear: xnumero.

    write: p_znom_remetente_alv-nr_programada to xnumero.
    shift:  xnumero left deleting leading space.

    concatenate vg_html
                '<td width="10%"><p align="center"><font color="#000030" size=1><b>' p_znom_remetente_alv-uf '</b></font></p></td>'
                '<td width="20%"><p align="right"> <font color="#000030" size=1><b>' xnumero                 '</b></font></p></td>' into vg_html.
    perform add tables p_html using vg_html.
    perform add tables p_html using '</tr>'.
    p_primeiro = space.

  endif.

endform.                    " MONTA_DETALHE


*&---------------------------------------------------------------------*
*&      Form  MONTA_DETALHE_P
*&---------------------------------------------------------------------*
*       Preenche detalhes do e-mail de programação, com os remetente da
*       mercadoria
*----------------------------------------------------------------------*
form monta_detalhe_p  tables p_html               structure w3html
                       using p_nom_programacao type zplac_nom_programacao_2.

  data: vg_html type string,
        xnumero type c length 15.

  write: p_nom_programacao-nr_programada to xnumero.
  shift: xnumero left deleting leading space.

  concatenate     '<tr><td width="20%"><p align="left"><font color="#000030" size=1><b>'   p_nom_programacao-fili_name1    '</b></font></p></td>' into vg_html.
  concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>'   p_nom_programacao-fili_stcd1    '</b></font></p></td>' into vg_html.
  concatenate vg_html '<td width="20%"><p align="left"><font color="#000030" size=1><b>'   p_nom_programacao-cliente_name1 '</b></font></p></td>' into vg_html.
  concatenate vg_html '<td width="15%"><p align="left"><font color="#000030" size=1><b>'   p_nom_programacao-cliente_stcd1 '</b></font></p></td>' into vg_html.
  concatenate vg_html '<td width="10%"><p align="center"><font color="#000030" size=1><b>' p_nom_programacao-cliente_uf    '</b></font></p></td>' into vg_html.
  concatenate vg_html '<td width="20%"><p align="right"> <font color="#000030" size=1><b>' xnumero                         '</b></font></p></td></tr>' into vg_html.
  perform add tables p_html using vg_html.

endform.                    " MONTA_DETALHE_P

*&---------------------------------------------------------------------*
*&      Form  MONTA_FIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form monta_fim  tables   p_html       structure w3html
                         packing_list structure sopcklsti1
                using    document_data type sodocchgi1.

  "Fim cabeçalho
  "perform add tables p_html using '</DIV>'.
  "perform add tables p_html using '</td></tr>'.

  document_data-obj_name  = 'ProgEmbarque'.
  document_data-obj_descr = 'Programação de Embarque'.
  document_data-no_change = 'X'.

  packing_list-head_start = 1.
  packing_list-head_num   = 60.
  packing_list-body_start = 1.
  packing_list-body_num   = 99999.
  packing_list-doc_type   = 'HTM'.
  append packing_list.

endform.                    " MONTA_FIM
