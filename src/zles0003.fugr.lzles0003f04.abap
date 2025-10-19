*----------------------------------------------------------------------*
***INCLUDE LZLES0003F04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
*
*  perform f_carregar_eventos using:
** para tira duplo click          SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
*                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
*

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout USING p_alv.

  REFRESH: estrutura, t_fieldcatalog.
  IF p_alv EQ 'LOG'.
    PERFORM montar_estrutura USING:
          1  ' '            ' '               'TG_SAIDA' 'ICON'        'Status'                 ' ' ,
          2  'ZLEST0062'   'NR_LOTE_ADM'      'TG_SAIDA' 'NR_LOTE_ADM' ' '                 ' ' ,
          3  'ZLEST0062'   'NR_LOTE'          'TG_SAIDA' 'NR_LOTE'     ' '                 ' ' ,
          4  'ZLEST0062'   'CHVID'            'TG_SAIDA' 'CHVID'       ' '                 ' ' ,
          5  'ZLEST0062'   'NUCONTRATO'       'TG_SAIDA' 'NUCONTRATO'  ' '                 ' ' ,
          6  'ZLEST0062'   'ID_TIPO'          'TG_SAIDA' 'ID_TIPO'     ' '                 ' ' ,
          8  'ZLEST0062'   'LINHA'            'TG_SAIDA' 'LINHA'       'Linha do arquivo'  ' ' ,
          7  'ZLEST0062'   'MSG_ERRO'         'TG_SAIDA' 'MSG_ERRO'    'Mensagem'  ' ' .
  ELSEIF p_alv EQ 'ARQUIVOS'.
    PERFORM montar_estrutura_oo USING:
*        1  ' '   ' '            'TG_SAIDA_ARQ' 'MARK'         ' '  ' ' ' ' ' ' ' ',
        1  ' '   ' '            'TG_SAIDA_ARQ' 'FILENAME'     'Nome do arquivo'  '80' ' ' ' ' ' '.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen).

  CLEAR wa_estrutura.

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

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.


  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_oo USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_edit)
                            value(p_sum)
                            value(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'MARK'.
    w_fieldcatalog-mark = 'X'.
  ENDIF.
  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
