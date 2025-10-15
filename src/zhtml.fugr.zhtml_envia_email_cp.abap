function zhtml_envia_email_cp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TITULO) TYPE  STRING
*"     REFERENCE(DOCUMENT_DATA) TYPE  SODOCCHGI1
*"     VALUE(VG_TEXTO) TYPE  CLIKE OPTIONAL
*"  TABLES
*"      PACKING_LIST STRUCTURE  SOPCKLSTI1
*"      HTML STRUCTURE  W3HTML
*"      RECEIVERS STRUCTURE  SOMLRECI1
*"----------------------------------------------------------------------

  data: it_html type table of w3html initial size 0 with header line,
        wa_html type w3html.

  check not receivers[] is initial.

  call function 'ZHTML_ADD_CAP'
    exporting
      i_titulo = i_titulo
    tables
      it_html  = it_html.

  loop at html into wa_html.
    append wa_html to it_html.
  endloop.

  call function 'ZHTML_ADD_ROD'
    exporting
      vg_texto = vg_texto
    tables
      it_html  = it_html.

  call function 'ZHTML_ENVIA_EMAIL'
    exporting
      document_data = document_data
    tables
      packing_list  = packing_list
      html          = it_html
      receivers     = receivers.

endfunction.
