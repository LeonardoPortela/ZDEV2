function z_les_inbound_proc_xml_ferro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      XML STRUCTURE  ZEXML_FERRO
*"----------------------------------------------------------------------
  data st_xml type zexml_ferro.

  if not xml[] is initial.
    refresh: xml_tab.
    perform le_arquivo using xml[].
    perform transf_arquivo_tabela.

    read table xml[] into st_xml index 1.

    if st_xml-vt_vi eq 'X' and v_erro ne 'X'.
      perform zf_gerar_vt_vi.
    endif.

    if st_xml-miro eq 'X' and v_erro ne 'X'.
      perform zgerar_miro_shdb.
      perform zgerar_miro.
    endif.

    st_xml-chave_cte = st_zlest0044-chave_cte.

    st_xml-erro = v_erro.

    refresh xml.

    append st_xml to xml.
  endif.

  perform zgrava_mensagens_tab.
endfunction.
