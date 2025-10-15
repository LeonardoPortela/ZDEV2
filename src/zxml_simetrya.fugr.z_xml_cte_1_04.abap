function z_xml_cte_1_04.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(P_INICIADA) TYPE  CHAR01
*"----------------------------------------------------------------------

  data: wa_setleaf    type setleaf,
        vg_data_corte type sy-datum,
        it_setleaf  like table of wa_setleaf with header line.

  clear: it_setleaf[].

  p_iniciada = space.

  "Opter Área de contabilidade de custos
  select * into table it_setleaf
    from setleaf
   where setname eq 'DT_XML_CTE_1_04'.

  if sy-subrc is initial.
    read table it_setleaf index 1.
    vg_data_corte = it_setleaf-valfrom.
    if vg_data_corte le sy-datum.
      p_iniciada = 'X'.
    endif.
  endif.

endfunction.
