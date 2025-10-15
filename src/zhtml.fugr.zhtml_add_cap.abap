FUNCTION zhtml_add_cap.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TITULO) TYPE  STRING
*"  TABLES
*"      IT_HTML STRUCTURE  W3HTML
*"----------------------------------------------------------------------

  DATA: p_texto TYPE string.

  CLEAR: it_html[].

  CONCATENATE '<title>' i_titulo '</title>' INTO p_texto.
  PERFORM add TABLES it_html USING '<html>'.
  PERFORM add TABLES it_html USING '<head>'.
  PERFORM add TABLES it_html USING p_texto.
  PERFORM add TABLES it_html USING '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  PERFORM add TABLES it_html USING '<body bgcolor="#ffffff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  PERFORM add TABLES it_html USING '<table align="left" width="100%">'.

ENDFUNCTION.
