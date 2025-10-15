*----------------------------------------------------------------------*
***INCLUDE ZSDR0040_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .

  REFRESH t_fieldcatalog.

  IF p_tp_ov IS NOT INITIAL.

    PERFORM montar_estrutura USING:
          1 'ZSDT0087'               'MATKL'   'TG_ITENS' 'MATKL'     'Grp.Mat'        '10' 'X' ' ' 'X',
          2 'ZSDT0087'               'TPSIM'   'TG_ITENS' 'TPSIM'     'Tp.Simulação'   '15' 'X' ' ' 'X',
          3 'ZSDT0087'               'INCO1'   'TG_ITENS' 'INCO1'     'Inco1'          '10' 'X' ' ' 'X',
          4 'VBAK'                   'AUART'   'TG_ITENS' 'AUART'     'Tp.Ordem'       '10' 'X' ' ' '',
          5 'ZSDT0087'               'SPART'   'TG_ITENS' 'SPART'     'Setor'          '10' 'X' ' ' ''.

  ELSE.

    PERFORM montar_estrutura USING:
          1 'VBAK'                   'AUART'   'TG_ITENS_01' 'AUART'    'Tp.Ordem'     '10' 'X' ' ' 'X',
          2 'ZSDT0087'               'INCO1'   'TG_ITENS_01' 'INCO1'    'Inco1'        '10' 'X' ' ' 'X',
          3 'ZSDT0087'               'INCO1'   'TG_ITENS_01' 'INCO2'    'Inco2'        '10' 'X' ' ' ' ',
          4 'ZSDT0087'               'INCO1'   'TG_ITENS_01' 'INCO3'    'Inco3'        '10' 'X' ' ' ' ',
          5 'ZSDT0087'               'INCO1'   'TG_ITENS_01' 'INCO4'    'Inco4'        '10' 'X' ' ' ' '.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
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

  IF p_field EQ 'INCO1' OR p_field EQ 'INCO2' OR
     p_field EQ 'INCO3' OR p_field EQ 'INCO4'.
    w_fieldcatalog-drdn_hndl  = 1.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_dropdown .
  DATA: ls_dropdown TYPE lvc_s_drop,
        lt_dropdown TYPE lvc_t_drop,
        tl_0038     TYPE TABLE OF zsdt0038 WITH HEADER LINE.


  ls_dropdown-handle = '1'.

  ls_dropdown-value = gt_values-ddtext = 'FOB'.
  gt_values-domvalue_l = 'FOB'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CPT'.
  gt_values-domvalue_l = 'CPT'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CIF'.
  gt_values-domvalue_l = 'CIF'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CFR'.
  gt_values-domvalue_l = 'CFR'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  IF p_tp_ov IS NOT INITIAL.
    CALL METHOD grid1->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.
  ELSE.

    CALL METHOD grid2->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.
  ENDIF.



ENDFORM.                    " BUILD_DROPDOWN

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_linha(6).

  REFRESH: tg_msg_ret.

  CLEAR wl_linha.

  IF p_tp_ov IS NOT INITIAL.

    LOOP AT tg_itens.
      wl_linha = sy-tabix.
      IF tg_itens-matkl IS INITIAL.
        CONCATENATE text-e01 ' Grp.Mat LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-tpsim IS INITIAL.
        CONCATENATE text-e01 ' Tip.Simulação LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.


      IF tg_itens-auart IS INITIAL.
        CONCATENATE text-e01 ' Tip.OV LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-inco1 IS INITIAL.
        CONCATENATE text-e01 ' INCO LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-spart IS INITIAL.
        CONCATENATE text-e01 ' Setor LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.

  ELSE.

    LOOP AT tg_itens_fr.
      wl_linha = sy-tabix.

      IF tg_itens_fr-auart IS INITIAL.
        CONCATENATE text-e01 ' Tip.OV LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens_fr-inco1 IS INITIAL.
        CONCATENATE text-e01 ' INCO1 LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: tl_input_zsdt0087 TYPE TABLE OF zsdt0087 WITH HEADER LINE,
        tl_save_zsdt0258  TYPE TABLE OF zsdt0258,
        wl_save_zsdt0258  TYPE zsdt0258.



  IF p_tp_ov IS NOT INITIAL.

    LOOP AT tg_itens.
      MOVE-CORRESPONDING tg_itens TO tl_input_zsdt0087.
      MOVE:
            sy-uname               TO tl_input_zsdt0087-usnam,
            sy-datum               TO tl_input_zsdt0087-data_atual,
            sy-uzeit               TO tl_input_zsdt0087-hora_atual.
      APPEND tl_input_zsdt0087.

    ENDLOOP.
    "
    DELETE FROM zsdt0087.
    MODIFY zsdt0087 FROM TABLE tl_input_zsdt0087.


    MESSAGE s836(sd) WITH 'Tipos O.V.'
                           ', criado/modificado com sucesso!'.
  ELSE.

    REFRESH tl_save_zsdt0258.


    SELECT *  FROM zsdt0258 INTO TABLE @DATA(lit_zsdt0258)
      FOR ALL ENTRIES IN @tg_itens_fr
      WHERE auart EQ @tg_itens_fr-auart.


    LOOP AT tg_itens_fr.
      CLEAR wl_save_zsdt0258.

      IF tg_itens_fr-ins EQ abap_true.

        READ TABLE lit_zsdt0258 INTO DATA(lwa_zsdt0258) WITH KEY auart =  tg_itens_fr-auart.
        IF sy-subrc EQ 0.
          REFRESH tl_save_zsdt0258.
          CLEAR wl_save_zsdt0258.

          MESSAGE i836(sd) WITH 'Tipos O.V. informada já existe!'.
          EXIT.

        ELSE.
          MOVE-CORRESPONDING tg_itens_fr TO wl_save_zsdt0258.
          APPEND wl_save_zsdt0258 TO tl_save_zsdt0258.
        ENDIF.

      ELSE.
        MOVE-CORRESPONDING tg_itens_fr TO wl_save_zsdt0258.
        APPEND wl_save_zsdt0258 TO tl_save_zsdt0258.
      ENDIF.
    ENDLOOP.

    IF tl_save_zsdt0258[] IS NOT INITIAL.

      DELETE FROM zsdt0258.
      MODIFY zsdt0258 FROM TABLE tl_save_zsdt0258.

      MESSAGE s836(sd) WITH 'Tipos O.V.'
                             ', criado/modificado com sucesso!'.
    ENDIF.

  ENDIF.
ENDFORM.                    " GRAVA_DADOS

FORM fm_busca_dados.

  IF p_tp_ov IS NOT INITIAL.

    SELECT   matkl tpsim auart inco1 spart
      FROM zsdt0087
      INTO TABLE tg_itens
      ORDER BY  matkl  tpsim auart.
  ELSEIF p_tp_fr IS NOT INITIAL.
    SELECT  auart  inco1 inco2  inco3 inco4  FROM  zsdt0258
      INTO TABLE tg_itens_fr.
  ENDIF.


ENDFORM.
