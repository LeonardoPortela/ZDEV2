*----------------------------------------------------------------------*
***INCLUDE ZFIR0045_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_0075   text
*      -->P_C_0  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM trata_campos  USING    p_field
                            p_group1
                            p_value
                            p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.

ENDFORM.                    " TRATA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
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

  IF p_field EQ 'HKONT' OR p_field EQ 'BSCHL' OR p_field EQ 'COD_OPER'." OR P_FIELD EQ 'TP_OPERACAO'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '              ' '                      'TG_ITENS' 'COD_OPER'            'Oper.'             '10' 'X' ' ' 'X',
        1 'ZFIT0063'       'DESCR'                  'TG_ITENS' 'DESCR'               'Descrição'         '10' ' ' ' ' 'X',
        1 'ZFIT0066'       'TP_OPERACAO'            'TG_ITENS' 'TP_OPERACAO'         'Tp.Operação'       '10' 'X' ' ' 'X',
        1 ' '              ' '                      'TG_ITENS' 'TP_AJUSTE'           'Tp.Ajuste'         '10' 'X' ' ' 'X',
        1 ' '              ' '                      'TG_ITENS' 'BSCHL'               'Chv.Lc'            '10' 'X' ' ' 'X',
        1 ' '              ' '                      'TG_ITENS' 'HKONT'               'Conta'             '15' 'X' ' ' 'X',
        1 'ZFIT0066'       'TXT50'                  'TG_ITENS' 'TXT50'               'Descrição'         '20' ' ' ' ' 'X',
        1 'ZFIT0066'       'SHKZG'                  'TG_ITENS' 'SHKZG'               'D/C'               '10' ' ' ' ' 'X',
        1 'ZFIT0066'       'TX_IMP'                 'TG_ITENS' 'TX_IMP'              'Tx.Imp.'           '10' 'X' ' ' 'X'.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_PAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eliminar_par .

ENDFORM.                    " ELIMINAR_PAR
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .

  DATA: tl_zfit0066 TYPE TABLE OF zfit0066 WITH HEADER LINE,
        wl_zfit0063 TYPE zfit0063,
        wl_skat     TYPE skat,
        wl_tbsl     TYPE tbsl.

  IF wg_acao EQ c_displa OR wg_acao IS INITIAL.
    SELECT *
       FROM zfit0066
       INTO TABLE tl_zfit0066.

    REFRESH: tg_itens.
    LOOP AT tl_zfit0066.
      MOVE-CORRESPONDING tl_zfit0066 TO tg_itens.

      SELECT SINGLE *
        FROM zfit0063
        INTO wl_zfit0063
        WHERE cod_oper = tg_itens-cod_oper.
      tg_itens-descr =  wl_zfit0063-descr.

      SELECT SINGLE *
         FROM skat
         INTO wl_skat
         WHERE spras EQ sy-langu AND ktopl EQ '0050'
         AND   saknr = tg_itens-hkont.
      tg_itens-txt50 =  wl_skat-txt50.

      SELECT SINGLE * FROM tbsl
        INTO wl_tbsl
        WHERE koart = 'S'
        AND   bschl = tg_itens-bschl.

      IF wl_tbsl-shkzg = 'S'.
        MOVE 'D' TO tg_itens-shkzg.
      ELSE.
        MOVE 'C' TO tg_itens-shkzg.
      ENDIF.

      APPEND tg_itens.
      CLEAR: tg_itens.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_linha(6),
        tl_tbsl           TYPE TABLE OF tbsl.

  REFRESH: tg_msg_ret.
  CLEAR: tg_msg_ret.

  LOOP AT tg_itens.
    wl_linha = sy-tabix.

*** PBI - 73761 - Inicio - CBRAND
    IF tg_itens-tp_ajuste <> 'ATIVO'   AND
       tg_itens-tp_ajuste <> 'PASSIVO' AND
       tg_itens-tp_ajuste <> 'ATIVOLP' AND
       tg_itens-tp_ajuste <> 'PASSIVOLP'.

      CONCATENATE text-e04 'LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
*** PBI - 73761 - Fim - CBRAND

    IF tg_itens-cod_oper IS INITIAL OR tg_itens-descr IS INITIAL.
      CONCATENATE text-e01 'Operação LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_itens-bschl IS INITIAL OR tg_itens-shkzg IS INITIAL.
      CONCATENATE text-e01 'Chv.Lct LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tg_itens-bschl NE '40' AND tg_itens-bschl NE '50'.
      CONCATENATE text-e03 'Chv.Lct LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_itens-hkont IS INITIAL OR tg_itens-txt50 IS INITIAL.
      CONCATENATE text-e01 'Conta LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_itens-tp_operacao IS NOT INITIAL.
      IF ( tg_itens-tp_operacao NE 'F'
       AND tg_itens-tp_operacao NE 'O'
       AND tg_itens-tp_operacao NE 'C')
        OR tg_itens-cod_oper NE 'N'.

        CONCATENATE  'Tp.operacao' 'Inválida LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

    IF tg_itens-tp_ajuste IS INITIAL..
      CONCATENATE text-e01 'Tp.Ajuste LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_itens-tx_imp LT 0 OR tg_itens-tx_imp GT 100.
      CONCATENATE text-e02 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .

  DATA:  tl_input_zfit0066 TYPE TABLE OF zfit0066 WITH HEADER LINE.

  "Grava BMF
  DELETE tg_itens WHERE cod_oper IS INITIAL.
  LOOP AT tg_itens.
    MOVE: sy-mandt                 TO tl_input_zfit0066-mandt,
          tg_itens-cod_oper        TO tl_input_zfit0066-cod_oper,
          tg_itens-tp_operacao     TO tl_input_zfit0066-tp_operacao,
          tg_itens-tp_ajuste       TO tl_input_zfit0066-tp_ajuste,
          tg_itens-bschl           TO tl_input_zfit0066-bschl,
          tg_itens-hkont           TO tl_input_zfit0066-hkont,
          tg_itens-tx_imp          TO tl_input_zfit0066-tx_imp,
          sy-uname                 TO tl_input_zfit0066-usnam,
          sy-datum                 TO tl_input_zfit0066-data_atual,
          sy-uzeit                 TO tl_input_zfit0066-hora_atual.
    APPEND tl_input_zfit0066.
  ENDLOOP.

  DELETE FROM zfit0066.
  MODIFY zfit0066 FROM TABLE tl_input_zfit0066.

  MESSAGE s836(sd) WITH 'Parâmetros'
                       ', criado/modificado com sucesso!'.
  "
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .
  CLEAR: wg_mensagem,x_field.
  "REFRESH: TG_ITENS.
ENDFORM.                    " LIMPA_CAMPOS
