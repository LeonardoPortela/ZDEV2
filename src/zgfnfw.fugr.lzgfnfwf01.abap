*----------------------------------------------------------------------*
***INCLUDE LZGFNFWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
 REFRESH: tg_fieldcatalog.

  PERFORM montar_estrutura USING:
1  ' ' ' '    'IT_MSGS'   'MSG'      'Mensagem de Erro'  ' '.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------
FORM montar_estrutura USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen)     TYPE i.



  CLEAR: wg_fieldcatalog.

  wg_fieldcatalog-fieldname     = p_field.
  wg_fieldcatalog-tabname       = p_tabname.
  wg_fieldcatalog-ref_table     = p_ref_tabname.
  wg_fieldcatalog-ref_field     = p_ref_fieldname.
  wg_fieldcatalog-key           = ' '.
  wg_fieldcatalog-key_sel       = 'X'.
  wg_fieldcatalog-col_pos       = p_col_pos.
  wg_fieldcatalog-no_out        = ' '.
  wg_fieldcatalog-scrtext_l     = p_scrtext_l.
  wg_fieldcatalog-scrtext_m     = p_scrtext_l.
  wg_fieldcatalog-scrtext_s     = p_scrtext_l.
  wg_fieldcatalog-outputlen     = p_outputlen.



  IF wg_fieldcatalog-scrtext_l NE space.

    wg_fieldcatalog-selddictxt       = 'L'.

  ENDIF.

  APPEND wg_fieldcatalog TO tg_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro
