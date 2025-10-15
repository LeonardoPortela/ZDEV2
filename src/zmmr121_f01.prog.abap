*----------------------------------------------------------------------*
***INCLUDE ZMMR121_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_montar_layout  USING    p_edit.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   'T156'     'BWART'       'TG_SAIDA'  'BWART'     'Tipo Movimento'         '10' p_edit ' ' 'X',
        2   ''         ''            'TG_SAIDA'  'BTEXT'     'Descrição'              '30' ''     ' ' 'X'.
  IF sy-tcode = 'ZMM0177'.
    PERFORM f_montar_estrutura USING:
        3   'ZMMT0083' 'CK_FISCAL'   'TG_SAIDA'  'CK_FISCAL' 'Copia FiscalXFisico'    '10' 'X' ' ' 'X'.
  ENDIF.
ENDFORM.

*------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_montar_estrutura  USING  p_col_pos   p_ref_tabname p_ref_fieldname p_tabname p_field
                                p_scrtext_l p_outputlen   p_edit          p_sum     p_emphasize.

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.

  w_fieldcatalog-key           = ' '.


  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos       = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen   = p_outputlen.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.
ENDFORM.                    " F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_dados .
  DATA: wl_zmmt0084 TYPE zmmt0084.

  IF wg_acao = c_add. "Novo Lançamento

    SELECT SINGLE *
      FROM zmmt0084
         INTO wl_zmmt0084
       WHERE cd_agru = wg_zmmt0083-cd_agru
       AND   code    = wg_zmmt0083-code.
    "
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wl_zmmt0084 TO wg_zmmt0083.
    ELSE.
      CLEAR: wg_zmmt0083-ds_gru_val, wg_zmmt0083-ds_gru_qtd.
      MESSAGE 'Não existe grupo cadastrado!' TYPE 'I'.
      EXIT.
    ENDIF.

    CHECK wg_zmmt0083-cd_agru IS NOT INITIAL.
    CHECK wg_zmmt0083-code    IS NOT INITIAL.

    REFRESH  tg_saida.

    wg_acao = c_modif.
    SELECT zmmt0083~bwart t156t~btext zmmt0083~entsai zmmt0083~ck_fiscal
      FROM zmmt0083
      INNER JOIN t156t
      ON  t156t~bwart = zmmt0083~bwart
      AND t156t~spras = sy-langu
*      AND T156T~SOBKZ = ''
*      AND T156T~KZBEW = 'B'
*      AND T156T~KZZUG = ''
*      AND T156T~KZVBR = ''
         INTO CORRESPONDING FIELDS OF TABLE tg_saida
       WHERE zmmt0083~cd_agru = wg_zmmt0083-cd_agru
       AND   code    = wg_zmmt0083-code.
    SORT tg_saida BY bwart.
    DELETE ADJACENT DUPLICATES FROM tg_saida COMPARING bwart.
    REFRESH: tg_fields.

    PERFORM f_trata_campos USING  space
                                  'GR1'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
  ELSEIF wg_acao = c_displa.
    CHECK wg_zmmt0083-cd_agru IS NOT INITIAL.
    REFRESH  tg_saida.

    SELECT zmmt0083~bwart t156t~btext zmmt0083~entsai zmmt0083~ck_fiscal
      FROM zmmt0083
      INNER JOIN t156t
      ON  t156t~bwart = zmmt0083~bwart
      AND t156t~spras = sy-langu
*      AND T156T~SOBKZ = ''
*      AND T156T~KZBEW = 'B'
*      AND T156T~KZZUG = ''
*      AND T156T~KZVBR = ''
         INTO CORRESPONDING FIELDS OF TABLE tg_saida
       WHERE zmmt0083~cd_agru = wg_zmmt0083-cd_agru
       AND   code = wg_zmmt0083-code.

    SORT tg_saida BY bwart.
    DELETE ADJACENT DUPLICATES FROM tg_saida COMPARING bwart.
    SELECT SINGLE *
      FROM zmmt0084
         INTO wl_zmmt0084
       WHERE cd_agru = wg_zmmt0083-cd_agru
       AND   code    = wg_zmmt0083-code.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wl_zmmt0084 TO wg_zmmt0083.
    ELSE.
      CLEAR: wg_zmmt0083-ds_gru_val, wg_zmmt0083-ds_gru_qtd.
    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_dados .
  CHECK wg_zmmt0083-cd_agru IS NOT INITIAL.

  DATA: wl_zmmt0083 TYPE zmmt0083,
        tl_zmmt0083 TYPE TABLE OF zmmt0083.

  LOOP AT tg_saida INTO wg_saida.
    wl_zmmt0083-code    = wg_zmmt0083-code.
    wl_zmmt0083-cd_agru = wg_zmmt0083-cd_agru.
    wl_zmmt0083-bwart   = wg_saida-bwart.
    wl_zmmt0083-entsai  = wg_saida-entsai.
    wl_zmmt0083-ck_fiscal  = wg_saida-ck_fiscal.

    APPEND wl_zmmt0083 TO tl_zmmt0083.
  ENDLOOP.
  DELETE FROM zmmt0083 WHERE cd_agru = wg_zmmt0083-cd_agru AND code = wg_zmmt0083-code.
  MODIFY zmmt0083 FROM TABLE tl_zmmt0083 .
  "
  MESSAGE s836(sd) WITH text-m01 wg_zmmt0083-cd_agru text-m02.
ENDFORM.

FORM f_trata_campos  USING p_field p_group1 p_value p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.

ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_GRU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_gru INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_docs OCCURS 0,
          code       TYPE zmmt0084-code,
          cd_agru    TYPE zmmt0084-cd_agru,
          ds_gru_val TYPE zmmt0084-ds_gru_val,
          ds_gru_qtd TYPE zmmt0084-ds_gru_qtd,
          entsai     TYPE zmmt0084-entsai,
          seq        TYPE zmmt0084-seq,
        END OF tl_docs.


  SELECT code cd_agru ds_gru_val ds_gru_qtd entsai seq
     FROM zmmt0084
     INTO TABLE tl_docs
     WHERE code =  wg_zmmt0083-code.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CD_AGRU'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZMMT0084-AGRU'
      value_org       = 'S'
    TABLES
      value_tab       = tl_docs
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifica_erros .
  DATA: wl_zmmt0083 TYPE zmmt0083,
        wl_zmmt0084 TYPE zmmt0084,
        wl_linha(6).

  CLEAR:    tg_msg_ret.
  REFRESH:  tg_msg_ret.

  IF wg_zmmt0083-cd_agru IS INITIAL.
    CONCATENATE  'Preencha o agrupamento ' '' INTO  tg_msg_ret-msg SEPARATED BY space.
    MOVE  'WG_ZMMT0083-CD_AGRU'          TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
    FROM zmmt0084
    INTO wl_zmmt0084
    WHERE cd_agru EQ wg_zmmt0083-cd_agru
    AND   code    EQ wg_zmmt0083-code.
    IF sy-subrc NE 0.
      CONCATENATE  'Agrupamento ' 'não existe!' INTO  tg_msg_ret-msg SEPARATED BY space.
      MOVE  'WG_ZMMT0083-CD_AGRU'          TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
  IF wg_zmmt0083-code NE 'ZMM0177'.
    LOOP AT tg_saida INTO wg_saida.
      SELECT SINGLE *
        FROM zmmt0083
        INTO wl_zmmt0083
        WHERE bwart   EQ wg_saida-bwart
        AND   cd_agru NE wg_zmmt0083-cd_agru
        AND   code    EQ wg_zmmt0083-code.
      IF sy-subrc = 0.
        wl_linha = sy-tabix.
        CONCATENATE  wg_saida-bwart 'já cadastrado em ' wl_zmmt0083-cd_agru  INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_montar_layout2   USING    p_edit.
  REFRESH t_fieldcatalog.

  PERFORM f_montar_estrutura USING:
        1   ''     ''      'TG_SAIDA2'  'CD_AGRU'     'Agrupamento'         '10' p_edit ' ' 'X',
        2   ''     ''      'TG_SAIDA2'  'DS_GRU_VAL'  'Descrição Valor'     '40' p_edit ' ' 'X',
        3   ''     ''      'TG_SAIDA2'  'DS_GRU_QTD'  'Descrição Qtd.'      '40' p_edit ' ' 'X',
        4   ''     ''      'TG_SAIDA2'  'ENTSAI'      'E/S'                 '05' p_edit ' ' 'X',
        4   ''     ''      'TG_SAIDA2'  'SEQ'         'Seq.Ex'              '07' p_edit ' ' 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifica_erros2 .
  CLEAR:    tg_msg_ret.
  REFRESH:  tg_msg_ret.

  DATA: it_dd07v TYPE TABLE OF dd07v,
        wa_dd07v TYPE dd07v.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZENTSAI'
      text       = 'X'
    TABLES
      values_tab = it_dd07v.

  LOOP AT tg_saida2 INTO wg_saida2.

    READ TABLE it_dd07v INTO wa_dd07v WITH KEY  domvalue_l  =  wg_saida2-entsai.
    IF sy-subrc NE 0.
      MESSAGE 'Campo E/S informado não existe' TYPE 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_dados2 .

  IF wg_acao = c_add. "Novo Lançamento
    wg_acao = c_modif.
    SELECT *
      FROM zmmt0084
         INTO CORRESPONDING FIELDS OF TABLE tg_saida2
      WHERE code = wg_zmmt0083-code.

  ELSEIF wg_acao = c_displa.
    SELECT *
     FROM zmmt0084
        INTO CORRESPONDING FIELDS OF TABLE tg_saida2
      WHERE code = wg_zmmt0083-code.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_dados2 .
  DATA: wl_zmmt0084 TYPE zmmt0084,
        tl_zmmt0084 TYPE TABLE OF zmmt0084.

  LOOP AT tg_saida2 INTO wg_saida2.
    wl_zmmt0084-code        = wg_zmmt0083-code.
    wl_zmmt0084-cd_agru     = wg_saida2-cd_agru.
    wl_zmmt0084-ds_gru_val  = wg_saida2-ds_gru_val.
    wl_zmmt0084-ds_gru_qtd  = wg_saida2-ds_gru_qtd.
    wl_zmmt0084-entsai      = wg_saida2-entsai.
    wl_zmmt0084-seq         = wg_saida2-seq.
    APPEND wl_zmmt0084 TO tl_zmmt0084.
  ENDLOOP.
  DELETE FROM zmmt0084 WHERE code = wg_zmmt0083-code.

  MODIFY zmmt0084 FROM TABLE tl_zmmt0084.
  "
  MESSAGE s836(sd) WITH text-m01  text-m02.
ENDFORM.
