function zhtml_add_rod.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(VG_TEXTO) TYPE  CLIKE OPTIONAL
*"  TABLES
*"      IT_HTML STRUCTURE  W3HTML
*"----------------------------------------------------------------------

  data: vg_text_rod type string.

  if vg_texto is initial.
    vg_text_rod = 'E-mail automático gerado pelo sistema'.
  else.
    vg_text_rod = vg_texto.
  endif.

  perform add tables it_html using '<tr><td>'.

  perform add tables it_html using '<BR>'.
  perform add tables it_html using '<BR>'.
  perform add tables it_html using '<DIV align=left>'.

  concatenate '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>' vg_text_rod '</STRONG></FONT></DIV>' into vg_text_rod.
  perform add tables it_html using vg_text_rod.

  perform add tables it_html using '<DIV align=center><FONT face=Verdana color=#3CB371 size=2><STRONG>Grupo André Maggi</STRONG></FONT></DIV>'.
  perform add tables it_html using '</DIV>'.
  perform add tables it_html using '<BR>'.

  perform add tables it_html using '</td></tr>'.

  perform add tables it_html using '</table>'.
  perform add tables it_html using '</body>'.
  perform add tables it_html using '</html>'.

endfunction.
