FUNCTION zhtml_envia_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DOCUMENT_DATA) TYPE  SODOCCHGI1
*"  TABLES
*"      PACKING_LIST STRUCTURE  SOPCKLSTI1
*"      HTML STRUCTURE  W3HTML
*"      RECEIVERS STRUCTURE  SOMLRECI1
*"----------------------------------------------------------------------

  CHECK NOT receivers[] IS INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = document_data
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = packing_list
      contents_txt               = html
      receivers                  = receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

*Exemplo de HTML de e-mail

*<html>
*<head><title>Cockpit - Pagamento a Postos</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>
*<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">
*<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>Históricos Importados</STRONG></FONT></DIV><BR>
*<FONT face=Verdana color=#0000ff size=2>
*<BR>
*<table align="center" cellspacing="0" cellpadding="10" border="1">
*<tr><td colspan="10" align="center" bgcolor="666666"><font color="#FFFFFF" size=1><strong>Valores processados</strong></font></td></tr>
*<tr><td><font color="#000030" size=1><b>Histórico</b></font></td>
*    <td><p align="right"><font color="#000030" size=1><b>Nr.CTRC</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>C.Frete</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Peso Origem</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Peso Confirmado</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Peso Importado</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Vlr. Origem</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Vlr. Confirmado</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Vlr. Diferença</b></font></p></td>
*    <td><p align="right"><font color="#000030" size=1><b>Vlr. Programado</b></font></p></td></tr>
*<tr><td><font color="#000000" size=1>Correios</font></td>
*    <td><p align="right"><font color="#000000" size=1></font></p></td>
*    <td><p align="right"><font color="#000000" size=1></font></p></td>
*    <td><p align="right"><font color="#000000" size=1>             0.000</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>             0.000</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>             0.000</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>           0.00</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>          20.00</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>           0.00</font></p></td>
*    <td><p align="right"><font color="#000000" size=1>          20.00</font></p></td></tr>
*</table>
*<BR>
*<BR>
*<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>Log de Processamento de Importação de Arquivo</STRONG></FONT></DIV>
*<BR>
*<BR>
*<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>
*<BR>
*</body>
*</html>

ENDFUNCTION.
