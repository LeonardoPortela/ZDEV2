*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_5000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5000 INPUT.

  CASE ok_code.
    WHEN c_apcons.
      CLEAR: ok_code.
      PERFORM pesquisa_expot_acomp.
    WHEN c_csnfmt.
      CLEAR: ok_code.
      PERFORM visualiza_notas USING c_x.
    WHEN c_csnfms.
      CLEAR: ok_code.
      PERFORM visualiza_notas USING space.
    WHEN c_lancnv.
      CLEAR: ok_code.
      PERFORM lanca_nota_acomp.
    WHEN c_back OR c_exit OR c_cancel.
      CLEAR: ok_code.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_5000  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_EXPOT_ACOMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisa_expot_acomp .

  CLEAR: it_exportacoes[], it_notas[], it_export_acomp[].

  vg_quantidade = 0.
  vg_quantcompe = 0.
  vg_quantacomp = 0.

  p_direcao_1 = 2.

  CALL FUNCTION 'Z_EXPORT_TERCEIRO_ACOMP'
    EXPORTING
      p_bukrs          = t_empres
      p_werks          = t_centro
      p_matnr          = t_produt
      t_export         = t_export[]
      t_period         = t_period[]
      exportadores     = c_x
      notas_exportacao = c_x
    TABLES
      it_exportacoes   = it_exportacoes
      it_notas         = it_notas
    EXCEPTIONS
      cfops_saida      = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT it_exportacoes INTO wa_exportacoes.
    vg_quantidade = vg_quantidade + wa_exportacoes-quantidade.
    vg_quantcompe = vg_quantcompe + wa_exportacoes-quantcompe.
    vg_quantacomp = vg_quantacomp + wa_exportacoes-quantacomp.
    MOVE-CORRESPONDING wa_exportacoes TO wa_export_acomp.
    APPEND wa_export_acomp TO it_export_acomp.
  ENDLOOP.

ENDFORM.                    " PESQUISA_EXPOT_ACOMP

*&---------------------------------------------------------------------*
*&      Form  LANCA_NOTA_ACOMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lanca_nota_acomp .

  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_acomp USING vg_verifica_selecao.

  IF vg_verifica_selecao EQ 0.
    PERFORM lancar_memorandos.
    zdoc_memo_nf_exp-emissor  = wa_export_acomp-exportador.
    zdoc_memo_nf_exp-material = wa_export_acomp-produto.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_export_acomp-werks
      IMPORTING
        output = zdoc_memorando-remetente.
    zdoc_memorando-representante = wa_export_acomp-exportador.
    zdoc_memorando-direcao       = c_2.
    terceiro                     = c_x.

  ENDIF.

ENDFORM.                    " LANCA_NOTA_ACOMP

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_ACOMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_selecao_acomp  USING  vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_export_acomp.

  CALL METHOD ctl_alv_memo->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_export_acomp
          INTO wa_export_acomp
         INDEX wa_selected_rows-index.
  ENDLOOP.

  IF NOT wa_export_acomp IS INITIAL.
    vg_verifica_selecao = 0.
  ELSE.
    MESSAGE text-e04 TYPE c_e DISPLAY LIKE c_s.
  ENDIF.

*  READ TABLE it_export_acomp INTO wa_export_acomp WITH KEY mark = c_x.
*  vg_verifica_selecao = sy-subrc.
*
*  IF sy-subrc NE 0.
*    MESSAGE text-e04 TYPE c_e DISPLAY LIKE c_s.
*  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_ACOMP

**&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_EXPORT_ACP'. DO NOT CHANGE THIS LI
**&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
*MODULE tab_export_acp_change_tc_attr OUTPUT.
*  DESCRIBE TABLE it_export_acomp LINES tab_export_acp-lines.
*ENDMODULE.                    "TAB_EXPORT_ACP_CHANGE_TC_ATTR OUTPUT

**&SPWIZARD: INPUT MODUL FOR TC 'TAB_EXPORT_ACP'. DO NOT CHANGE THIS LINE
**&SPWIZARD: MARK TABLE
*MODULE tab_export_acp_mark INPUT.
*  DATA: g_tab_export_acp_wa2 LIKE LINE OF it_export_acomp.
*  IF tab_export_acp-line_sel_mode = 1
*  AND it_export_acomp-mark = c_x.
*    LOOP AT it_export_acomp INTO g_tab_export_acp_wa2
*      WHERE mark = c_x.
*      g_tab_export_acp_wa2-mark = ''.
*      MODIFY it_export_acomp
*        FROM g_tab_export_acp_wa2
*        TRANSPORTING mark.
*    ENDLOOP.
*  ENDIF.
*  MODIFY it_export_acomp
*    INDEX tab_export_acp-current_line
*    TRANSPORTING mark.
*ENDMODULE.                    "TAB_EXPORT_ACP_MARK INPUT

*&---------------------------------------------------------------------*
*&      Form  VISUALIZA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM visualiza_notas USING todas TYPE c.

  DATA: vg_verifica_selecao TYPE sy-subrc.
  DATA: concatenar TYPE char100.

  todos = todas.

  IF todas IS INITIAL.
    PERFORM verifica_selecao_acomp USING vg_verifica_selecao.
  ELSE.
    vg_verifica_selecao = 0.
  ENDIF.

  IF vg_verifica_selecao EQ 0.
    CLEAR: it_notas2[], it_fcat[].

    IF todas IS INITIAL.
      LOOP AT it_notas INTO wa_notas WHERE bukrs      = wa_export_acomp-bukrs
                                       AND werks      = wa_export_acomp-werks
                                       AND exportador = wa_export_acomp-exportador
                                       AND produto    = wa_export_acomp-produto
                                       AND unidade    = wa_export_acomp-unidade
                                      AND nr_terminal = wa_export_acomp-nr_terminal
.
        SHIFT wa_export_acomp-exportador   LEFT DELETING LEADING '0'.
        SHIFT wa_export_acomp-produto      LEFT DELETING LEADING '0'.
        SHIFT wa_export_acomp-nr_terminal  LEFT DELETING LEADING '0'.
        CONCATENATE wa_export_acomp-bukrs       '-' wa_export_acomp-werks        INTO campo_1 SEPARATED BY space.
        CONCATENATE wa_export_acomp-exportador  '-' wa_export_acomp-exportadorn  INTO campo_2 SEPARATED BY space.
        CONCATENATE wa_export_acomp-produto     '-' wa_export_acomp-produton     INTO campo_3 SEPARATED BY space.
        CONCATENATE wa_export_acomp-nr_terminal '-' wa_export_acomp-des_terminal INTO campo_4 SEPARATED BY space.



        APPEND wa_notas TO it_notas2[].
      ENDLOOP.

    ELSE.
      MOVE it_notas[] TO it_notas2[].
    ENDIF.
    IF vg_dynnr_000 = c_5050.
      IF todas IS INITIAL.
        PERFORM alv_preenche_cat USING:   "ALV ZMEMO03 INDIVIDUAL
              'STATUS'      text-v00   '03'  space  space  space,
              'DOCNUM'      text-v01   '10'  c_x    space  space,
  "            'ITMNUM'     TEXT-V02   '06'  SPACE  SPACE  SPACE,
  "            'BUKRS'      TEXT-V03   '04'  SPACE  SPACE  SPACE,
  "            'WERKS'      TEXT-V04   '04'  SPACE  SPACE  SPACE,
              'CHARG'       text-v17   '04'  space  space  space,
              'DT_EMISSAO'  text-v05   '11'  space  space  space,
              'DT_CHEGADA'  text-v21   '11'  space  space  space,

              'DT_RECEPCAO'            text-v22   '11'  space  space  space,
              'PESO_AFERIDO_RECEPCAO'  text-v23   '21'  space  space  space,

              'DUES'        text-v25   '16'  space  space  space,
              'NAVIOS'      text-v24   '18'  space  space  space,

  "            'MODELO'     TEXT-V06   '02'  SPACE  SPACE  SPACE,
  "            'SERIES'     TEXT-V07   '03'  SPACE  SPACE  SPACE,
              'NFENUM'      text-v08   '09'  space  space  space,
  "            'EXPORTADOR' TEXT-V09   '10'  SPACE  SPACE  C_X  ,
  "            'PRODUTO'    TEXT-V10   '18'  SPACE  C_X    SPACE,
  "            'PRODUTON'   TEXT-V11   '40'  SPACE  SPACE  SPACE,
  "            'NCM'        TEXT-V12   '16'  SPACE  SPACE  SPACE,
              'QUANTIDADE'  text-v13   '16'  space  space  c_x  ,
              'NUMERO_MEMO' text-v20   '06'  space  space  space,
              'NR_RECUSADO' text-v19   '05'  space  space  space,
              'QTDE_BAIXADAS' 'Qtde.Baixada'   '16'  c_x  space  c_x,
              'UNIDADE'     text-v14   '03'  space  space  space,
              'QUANTPLANE'  text-c17   '16'  space  space  c_x  ,
              'QUANTAPLAN'  text-c18   '16'  space  space  c_x  ,
              'VINCULADO'   text-v16   '16'  space  space  c_x  ,
              'SALDO'       text-v15   '16'  space  space  c_x  .

      ELSE.
        PERFORM alv_preenche_cat USING:   "ALV ZMEMO03 TODOS
              'STATUS'      text-v00   '03'  space  space  space,
              'DOCNUM'      text-v01   '10'  c_x    space  space,
"             'ITMNUM'      TEXT-V02   '06'  SPACE  SPACE  SPACE,
              'BUKRS'       text-v03   '04'  space  space  space,
              'WERKS'       text-v04   '04'  space  space  space,
              'CHARG'       text-v17   '04'  space  space  space,
              'DT_EMISSAO'  text-v05   '11'  space  space  space,
              'DT_CHEGADA'  text-v21   '11'  space  space  space,

              'DT_RECEPCAO'            text-v22   '11'  space  space  space,
              'PESO_AFERIDO_RECEPCAO'  text-v23   '21'  space  space  space,
              'DUES'                   text-v25   '16'  space  space  space,
              'NAVIOS'                 text-v24   '18'  space  space  space,

"             'MODELO'      TEXT-V06   '02'  SPACE  SPACE  SPACE,
"             'SERIES'      TEXT-V07   '03'  SPACE  SPACE  SPACE,
              'NFENUM'      text-v08   '09'  space  space  space,
              'EXPORTADOR'  text-v09   '10'  space  space  c_x  ,
              'EXPORTADORN' text-c04   ' 31' space space space,
              'PRODUTO'     text-v10   '18'  space  c_x    space,
              'PRODUTON'    text-v11   '40'  space  space  space,
"             'NCM'         TEXT-V12   '16'  SPACE  SPACE  SPACE,
              'QUANTIDADE'  text-v13   '16'  space  space  c_x  ,
              'NUMERO_MEMO' text-v20   '06'  space  space  space,
              'NR_RECUSADO' text-v19   '05'  space  space  space,
              'QTDE_BAIXADAS' 'Qtde.Baixada'   '16'  c_x  space  c_x,
              'UNIDADE'     text-v14   '03'  space  space  space,
              'QUANTPLANE'  text-c17   '16'  space  space  c_x  ,
              'QUANTAPLAN'  text-c18   '16'  space  space  c_x  ,
              'VINCULADO'   text-v16   '16'  space  space  c_x  ,
              'SALDO'       text-v15   '16'  space  space  c_x  .
      ENDIF.
    ELSE.
      IF todas IS INITIAL.
        PERFORM alv_preenche_cat USING:  "ALV ZMEMO02 INDIVIDUAL
              'STATUS'      text-v00   '03'  space  space  space,
              'DOCNUM'      text-v01   '10'  c_x    space  space,
*             'ITMNUM'      TEXT-V02   '06'  SPACE  SPACE  SPACE,
*             'BUKRS'       TEXT-V03   '04'  SPACE  SPACE  SPACE,
*             'WERKS'       TEXT-V04   '04'  SPACE  SPACE  SPACE,
              'CHARG'       text-v17   '04'  space  space  space,
              'DT_EMISSAO'  text-v05   '11'  space  space  space,
              'DT_CHEGADA'  text-v21   '11'  space  space  space,

              'DT_RECEPCAO'            text-v22   '11'  space  space  space,
              'PESO_AFERIDO_RECEPCAO'  text-v23   '21'  space  space  space,
              'DUES'                   text-v25   '16'  space  space  space,
              'NAVIOS'                 text-v24   '18'  space  space  space,

*             'MODELO'      TEXT-V06   '02'  SPACE  SPACE  SPACE,
*             'SERIES'      TEXT-V07   '03'  SPACE  SPACE  SPACE,
              'NFENUM'      text-v08   '09'  space  space  space,
*             'EXPORTADOR'  TEXT-V09   '10'  SPACE  SPACE  C_X  ,
*             'PRODUTO'     TEXT-V10   '18'  SPACE  C_X    SPACE,
*             'PRODUTON'    TEXT-V11   '40'  SPACE  SPACE  SPACE,
*             'NCM'         TEXT-V12   '16'  SPACE  SPACE  SPACE,
              'QUANTIDADE'  text-v13   '16'  space  space  c_x  ,
              'NR_RECUSADO' text-v19   '05'  space  space  space,
              'UNIDADE'     text-v14   '03'  space  space  space,
              'VINCULADO'   text-v16   '16'  space  space  c_x  ,
              'SALDO'       text-v15   '16'  space  space  c_x  .
      ELSE.
        PERFORM alv_preenche_cat USING:  "ALV ZMEMO02 TODOS
              'STATUS'      text-v00   '03'  space  space  space,
              'DOCNUM'      text-v01   '10'  c_x    space  space,
*             'ITMNUM'      TEXT-V02   '06'  SPACE  SPACE  SPACE,
              'BUKRS'       text-v03   '04'  space  space  space,
              'WERKS'       text-v04   '04'  space  space  space,
              'CHARG'       text-v17   '04'  space  space  space,
              'DT_EMISSAO'  text-v05   '11'  space  space  space,
              'DT_CHEGADA'  text-v21   '11'  space  space  space,

              'DT_RECEPCAO'            text-v22   '11'  space  space  space,
              'PESO_AFERIDO_RECEPCAO'  text-v23   '21'  space  space  space,
              'DUES'                   text-v25   '16'  space  space  space,
              'NAVIOS'                 text-v24   '18'  space  space  space,

*            'MODELO'        TEXT-V06   '02'  SPACE  SPACE  SPACE,
*            'SERIES'        TEXT-V07   '03'  SPACE  SPACE  SPACE,
              'NFENUM'       text-v08   '09'  space  space  space,
              'EXPORTADOR'   text-v09   '10'  space  space  c_x  ,
              'EXPORTADORN'  text-c04   '40'  space  space  space,
              'PRODUTO'      text-v10   '18'  space  c_x    space,
              'PRODUTON'     text-v11   '40'  space  space  space,
*             'NCM'          TEXT-V12   '16'  SPACE  SPACE  SPACE,
              'QUANTIDADE'   text-v13   '16'  space  space  c_x  ,
              'NR_RECUSADO'  text-v19   '05'  space  space  space,
              'UNIDADE'      text-v14   '03'  space  space  space,
              'VINCULADO'    text-v16   '16'  space  space  c_x  ,
              'SALDO'        text-v15   '16'  space  space  c_x  .
      ENDIF.
    ENDIF.
    vg_dynnr_ant = vg_dynnr_000.
    vg_dynnr_000 = c_7000.
  ENDIF.

ENDFORM.                    " VISUALIZA_NOTAS

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_ACOMP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_acomp OUTPUT.

  IF vg_primeiro_visual IS INITIAL.

    CREATE OBJECT ctl_cccontainer
      EXPORTING
        container_name = 'MEMO_LISTA'.

    CREATE OBJECT ctl_alv_memo
      EXPORTING
        i_parent = ctl_cccontainer.

    IF vg_dynnr_000 = c_5050.
      PERFORM z_estrutura_fieldcat_acomp USING:   "ALV ZMEMO03
            'STATUS'       text-c01 space 01 03 space space space,
            'DT_EMISSAO'   text-v05 space 02 10 space space space,
            'WERKS'        text-c02 space 03 04 space space space,
            'EXPORTADOR'   text-c03 space 04 07 space space c_x  ,
            'EXPORTADORN'  text-c04 space 05 31 space space space,
            'EXPORTCNPJ'   text-c05 space 06 18 space space space,
            'REGIAO'       text-c06 space 07 03 space space space,
            'PRODUTO'      text-c07 space 08 09 space space c_x  ,
            'PRODUTON'     text-c08 space 09 30 space space space,
            'NR_TERMINAL'  text-c22 space 10 09 space space c_x  ,
            'DES_TERMINAL' text-c21 space 10 30 space space space,
            'UNIDADE'      text-c09 space 10 03 space space space,
            'QUANTIDADE'   text-c10 space 11 16 space c_x   space,
            'QUANTPLANE'   text-c13 space 12 16 space c_x   space,
            'QUANTAPLAN'   text-c14 space 13 16 space c_x   space,
            'QUANTCOMPE'   text-c11 space 14 16 space c_x   space,
            'QUANTACOMP'   text-c12 space 15 16 space c_x   space,
            'QTDE_BAIXADAS' 'Qtde. Baixada' space 16 16 space c_x   space.
    ELSE.
      PERFORM z_estrutura_fieldcat_acomp USING:   "ALV ZMEMO02
            'STATUS'       text-c01 space 01 03 space space space,
            'DT_EMISSAO'   text-v05 space 02 10 space space space,
            'WERKS'        text-c02 space 03 04 space space space,
            'EXPORTADOR'   text-c03 space 04 07 space space c_x  ,
            'EXPORTADORN'  text-c04 space 05 31 space space space,
            'EXPORTCNPJ'   text-c05 space 06 18 space space space,
            'REGIAO'       text-c06 space 07 03 space space space,
            'PRODUTO'      text-c07 space 08 09 space space c_x  ,
            'PRODUTON'     text-c08 space 09 30 space space space,
            'UNIDADE'      text-c09 space 10 03 space space space,
            'QUANTIDADE'   text-c10 space 11 16 space c_x   space,
            'QUANTCOMPE'   text-c11 space 12 16 space c_x   space,
            'QUANTACOMP'   text-c12 space 13 16 space c_x   space.
    ENDIF.

    gs_variant_c = sy-repid. "Enable users save own LAYOUTs

    CALL METHOD ctl_alv_memo->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant_c
*       IS_LAYOUT       = WA_LAYOUT
        i_save          = 'X'
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_export_acomp[].

    vg_primeiro_visual = c_x.

  ELSE.

    CALL METHOD ctl_alv_memo->refresh_table_display.

  ENDIF.

ENDMODULE.                 " CRIA_ALV_ACOMP  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_FIELDCAT
*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat_acomp USING p_fieldname
                                      p_texto_grande
                                      p_hot
                                      p_posicao
                                      p_outputlen
                                      p_fix_column
                                      p_do_sum
                                      p_no_zero.
  CLEAR wa_fieldcatalog.
  wa_fieldcatalog-fieldname     = p_fieldname.
  wa_fieldcatalog-tabname       = 'IT_EXPORT_ACOMP'.
  wa_fieldcatalog-scrtext_l     = p_texto_grande.
  wa_fieldcatalog-scrtext_m     = p_texto_grande.
  wa_fieldcatalog-scrtext_s     = p_texto_grande.
  wa_fieldcatalog-hotspot       = p_hot.
  wa_fieldcatalog-col_pos       = p_posicao.
  wa_fieldcatalog-outputlen     = p_outputlen.
  wa_fieldcatalog-fix_column    = p_fix_column.
  wa_fieldcatalog-do_sum        = p_do_sum.
  wa_fieldcatalog-no_zero       = p_no_zero.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT
