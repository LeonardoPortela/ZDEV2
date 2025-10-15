*----------------------------------------------------------------------*
***INCLUDE LZGFS_TRACE_COTTONF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_FCAT
*&---------------------------------------------------------------------*
FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.
