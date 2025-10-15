FUNCTION-POOL zgl_func.                     "MESSAGE-ID ..

DATA: vl_cnt(2)           TYPE n,
      vl_valor(15)        TYPE c,
      vl_field            TYPE c LENGTH 60,
      vl_lifnr            LIKE zgl001_comp_f44-lifnr,
      vl_fordif           TYPE c LENGTH 1,
      vl_tpshdb           TYPE c LENGTH 2,
      vl_txt              TYPE c LENGTH 50,
      vl_mode             TYPE c LENGTH 1,
      vl_data             TYPE c LENGTH 8,
      vl_raz              TYPE c LENGTH 50,
      vl_mess(256)        TYPE c,
      vl_sp               TYPE c LENGTH 1,
      vl_np               TYPE c LENGTH 1,
      vl_sinal            TYPE c LENGTH 1.

DATA: BEGIN OF wa_documentos,
        mandt            LIKE zgl001_comp_f44-mandt, "Mandante
        bukrs            LIKE zgl001_comp_f44-bukrs, "Empresa
        belnr            LIKE zgl001_comp_f44-belnr, "Nº documento contabil
        buzei            LIKE zgl001_comp_f44-buzei, "Item do documento contabil
        lote             LIKE zgl001_comp_f44-lote,  "Lote Sigam
        lifnr            LIKE zgl001_comp_f44-lifnr, "Fornecedor
        waers            LIKE zgl001_comp_f44-waers, "Código moeda
        budat            LIKE zgl001_comp_f44-budat, "Dt lanc. Documento
        umskz            LIKE zgl001_comp_f44-umskz, "Codigo de Razão
        dmbtr            LIKE zgl001_comp_f44-dmbtr, "Montante em moeda interna
        processamento    LIKE zgl001_comp_f44-processamento, "P-Parcial T-Total
        acerto           LIKE zgl001_comp_f44-acerto, "Sim/Nao
        gsber            like bsik-gsber,
      END OF wa_documentos,

*      BEGIN OF wa_retorno,
*        mandt            LIKE zgl002_comp_f44-mandt,
*        bukrs            LIKE zgl002_comp_f44-bukrs,
*        lote             LIKE zgl002_comp_f44-lote,
*        status           LIKE zgl002_comp_f44-status,
*        sgtxt            LIKE zgl002_comp_f44-sgtxt,
*        data             LIKE zgl002_comp_f44-data,
*        hora             LIKE zgl002_comp_f44-hora,
*        data_comp    LIKE zgl002_comp_f44-data,
*        interface(1) TYPE c,
*      END OF wa_retorno,

      BEGIN OF wa_part_residual OCCURS 0,
        lifnr            LIKE zgl001_comp_f44-lifnr,
        name1            LIKE lfa1-name1,
        dmbtr            LIKE zgl001_comp_f44-dmbtr,
      END OF   wa_part_residual,

      BEGIN OF wa_itens_bsik,
        shkzg            LIKE bsik-shkzg,
        dmbtr            LIKE bsik-dmbtr,
      END   OF wa_itens_bsik.

DATA: it_documentos      LIKE STANDARD TABLE OF wa_documentos,
      it_docbase         LIKE STANDARD TABLE OF wa_documentos,
      wa_bdcdata         LIKE bdcdata,
      it_bdcdata         LIKE STANDARD TABLE OF wa_bdcdata,
      wa_dochead         LIKE wa_documentos,
      wa_lotes           LIKE zgl003_comp_f44,
      wa_message         LIKE bdcmsgcoll.

DATA: "it_retorno_rfc     LIKE STANDARD TABLE OF wa_retorno,
      it_retorno_rfc LIKE zgl002_comp_f44 OCCURS 0 WITH HEADER LINE,
      wa_retorno     LIKE LINE OF it_retorno_rfc,

      it_complote        LIKE STANDARD TABLE OF wa_lotes,
      it_part_residual   LIKE STANDARD TABLE OF wa_part_residual,
      it_itens_bsik      LIKE STANDARD TABLE OF wa_itens_bsik.

CONSTANTS: c_part(2)     TYPE n VALUE 12,
           c_doc(2)      TYPE n VALUE 15.
*&---------------------------------------------------------------------*
*&      Form  f_bdc_insert
*&---------------------------------------------------------------------*
*       Inserir dados das pastas Batch-Input
*----------------------------------------------------------------------*
*  -->  p1 Código da pasta BI
*  -->  p2 Nome do campo  / Nome do programa qdo Código igual a 'X'
*  -->  p3 Valor do campo / Tela do programa qdo Código igual a 'X'
*----------------------------------------------------------------------*
FORM f_bdc_insert USING value(p_codigo)
      value(p_fnam)
      value(p_fval).
  CLEAR wa_bdcdata.
  wa_bdcdata-dynbegin = p_codigo.
  IF ( p_codigo EQ 'X' ).
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
  ELSE.
    wa_bdcdata-fnam     = p_fnam.
    wa_bdcdata-fval     = p_fval.
  ENDIF.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                      " f_bdc_insert
*&---------------------------------------------------------------------*
*&      Form  f_busca_itens
*&---------------------------------------------------------------------*
*       Busca todos os itens do lançamento para contagem de linhas
*----------------------------------------------------------------------*
*      -->P_BUKRS  text
*      -->P_LIFNR  text
*      -->P_BELNR  text
*----------------------------------------------------------------------*
FORM f_busca_itens  USING    p_bukrs
                             p_lote
                             p_lifnr
                             p_belnr
"                             p_gjahr
                             p_buzei
                    CHANGING r_pos r_vlr.
  DATA: vl_buzei         LIKE bsik-buzei,
        vl_vlr           TYPE n LENGTH 4,
        vl_pos           TYPE n LENGTH 2,
        wa_doc           LIKE wa_documentos.

  vl_pos = r_pos.
  vl_vlr = r_vlr.

  IF ( p_buzei IS NOT INITIAL ) OR ( p_buzei NE 0 ).

    SELECT buzei
      INTO vl_buzei
      FROM bsik
     WHERE bukrs EQ p_bukrs
       AND lifnr EQ p_lifnr
       AND belnr EQ p_belnr
"       AND gjahr EQ p_gjahr
     ORDER BY buzei.

      READ TABLE it_documentos INTO wa_doc WITH KEY lote  = p_lote
                                                    lifnr = p_lifnr
                                                    belnr = p_belnr
"                                                    gjahr = p_gjahr
                                                    buzei = vl_buzei
                                           BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF wa_doc-processamento EQ 'P'.
          IF wa_doc-acerto IS INITIAL.
            IF wa_doc-dmbtr GT 0.
              PERFORM f_busca_sinal USING wa_doc-bukrs
                                          wa_doc-lifnr
                                          wa_doc-belnr
"                                          wa_doc-gjahr
                                          wa_doc-buzei
                                 CHANGING vl_sinal.
              vl_valor = wa_doc-dmbtr.

            ELSE.
              vl_valor = wa_doc-dmbtr * -1.
            ENDIF.
          ELSE.
            vl_valor = wa_doc-dmbtr.
            vl_sinal = ''.
          ENDIF.

          TRANSLATE vl_valor USING '.,'.

          CONCATENATE vl_valor
                      vl_sinal INTO vl_valor.

          CONCATENATE 'DF05B-PSDIF('
                      vl_pos
                      ')' INTO vl_field.
          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'	'3100',
                ' ' 'BDC_OKCODE'                '/00',
                ' ' 'BDC_SUBSCR'                'SAPDF05X',
                ' ' vl_field           vl_valor.
        ENDIF.
      ELSE.
        CONCATENATE 'DF05B-PSBET('
                    vl_pos
                    ')' INTO vl_field.

        PERFORM f_bdc_insert USING :
                    'X' 'SAPDF05X'  '3100',
                    ' ' 'BDC_OKCODE'                '=PI',
                    ' ' 'BDC_CURSOR'                vl_field,
                    ' ' 'BDC_SUBSCR'                'SAPDF05X'.
      ENDIF.

      vl_pos = vl_pos + 1.
      vl_vlr = vl_vlr + 1.

      IF vl_pos GT c_part.
        vl_pos = 1.
        PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
              ' ' 'BDC_OKCODE'                  '\00',
              ' ' 'BDC_SUBSCR'                  'SAPDF05X',
              ' ' 'RF05A-ABPOS'                  vl_vlr.
      ENDIF.
    ENDSELECT.
    r_vlr = vl_vlr.
    r_pos = vl_pos.
  ELSE.
    r_vlr = r_vlr + 1.
    r_pos = r_pos + 1.

    IF r_pos GT c_part.
      r_pos = 1.
      PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
            ' ' 'BDC_OKCODE'                  '\00',
            ' ' 'BDC_SUBSCR'                  'SAPDF05X',
            ' ' 'RF05A-ABPOS'                  r_vlr.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_busca_itens
*&---------------------------------------------------------------------*
*&      Form  f_busca_sinal
*&---------------------------------------------------------------------*
*       Identifica se o lançamento é possitivou ou negativo
*----------------------------------------------------------------------*
*      -->P_BUKRS  Empresa
*      -->P_LIFNR  Fornecedor
*      -->P_BELNR  Numero do documento
*      <--P_SINAL  Sinal (- +)
*----------------------------------------------------------------------*
FORM f_busca_sinal  USING    p_bukrs
                             p_lifnr
                             p_belnr
                             p_buzei
                    CHANGING p_sinal.
  DATA: vl_tipo          TYPE c LENGTH 1.
  SELECT SINGLE shkzg
    INTO vl_tipo
    FROM bsik
   WHERE bukrs EQ p_bukrs
     AND lifnr EQ p_lifnr
     AND belnr EQ p_belnr
     AND buzei EQ p_buzei.

  IF vl_tipo EQ 'H'.
    p_sinal = '-'.
  ELSE.
    p_sinal = ''.
  ENDIF.
ENDFORM.                    "f_busca_sinal
*&---------------------------------------------------------------------*
*&      Form  f_busca_mess
*&---------------------------------------------------------------------*
*       busca mensagem a mensagem.
*----------------------------------------------------------------------*
*      -->P_MSGID  text
*      -->P_MSGNR  text
*      -->P_MSGV1  text
*      -->P_MSGV2  text
*      -->P_MSGV3  text
*      -->P_MSGV4  text
*      <--R_MESS  text
*----------------------------------------------------------------------*
FORM f_busca_mess  USING    p_msgid
                            p_msgnr
                            p_msgv1
                            p_msgv2
                            p_msgv3
                            p_msgv4
                   CHANGING r_mess.
  sy-msgid = p_msgid.
  sy-msgno = p_msgnr.
  sy-msgv1 = p_msgv1.
  sy-msgv2 = p_msgv2.
  sy-msgv3 = p_msgv3.
  sy-msgv4 = p_msgv4.
  CALL FUNCTION 'CUTC_GET_MESSAGE'
    EXPORTING
      msg_id      = sy-msgid
      msg_no      = sy-msgno
      msg_arg1    = sy-msgv1
      msg_arg2    = sy-msgv2
      msg_arg3    = sy-msgv3
      msg_arg4    = sy-msgv4
      language    = sy-langu
    IMPORTING
      raw_message = r_mess.
ENDFORM.                    " f_busca_mess
*&---------------------------------------------------------------------*
*&      Form  f_log_retorno
*&---------------------------------------------------------------------*
*       Grava na tabela de retorno o sucesso ou o erro
*----------------------------------------------------------------------*
*  -->  p_sta     Tipo da mensagem (A W E S)
*  <--  p_mess    Mensagem
*----------------------------------------------------------------------*
FORM f_log_retorno USING p_sta p_mess.
  CLEAR wa_retorno.
  SELECT SINGLE *
    FROM zgl002_comp_f44
    INTO wa_retorno
   WHERE bukrs EQ wa_lotes-bukrs
     AND lote  EQ wa_lotes-lote.

  IF wa_retorno IS INITIAL.
    wa_retorno-mandt  = sy-mandt.
    wa_retorno-bukrs  = wa_lotes-bukrs.
    wa_retorno-lote   = wa_lotes-lote.
    wa_retorno-status = p_sta.
    wa_retorno-sgtxt  = p_mess.
    wa_retorno-data   = sy-datum.
    wa_retorno-hora   = sy-uzeit.
    INSERT into zgl002_comp_f44 values wa_retorno.
  ELSE.
    wa_retorno-status = p_sta.
    wa_retorno-sgtxt  = p_mess.
    wa_retorno-data   = sy-datum.
    wa_retorno-hora   = sy-uzeit.
    MODIFY zgl002_comp_f44 FROM wa_retorno.
  ENDIF.
  PERFORM f_del_zgl001  USING wa_lotes-bukrs
                              wa_lotes-lote.
ENDFORM.                    " f_log_retorno
*&---------------------------------------------------------------------*
*&      Form  f_del_zgl001
*&---------------------------------------------------------------------*
*       Deleta registros da tabela ZGL001_COMP_F44
*----------------------------------------------------------------------*
*      -->P_BUKRS Empresa
*      -->P_LOTE  Lote
*----------------------------------------------------------------------*
FORM f_del_zgl001  USING    p_bukrs
                            p_lote.
  DELETE FROM zgl001_comp_f44 WHERE bukrs = p_bukrs AND lote = p_lote.
ENDFORM.                    " f_del_zgl001
*&---------------------------------------------------------------------*
*&      Form  f_partida_residual
*&---------------------------------------------------------------------*
*       Gera informações para partidas residuais.
*----------------------------------------------------------------------*
*      -->P_BUKRS  Empresa
*      -->P_LIFNR  Fornecedor
*      -->P_BELNR  Numero do documento
*      -->P_VALOR  Valor parcial da baixa
*      -->P_TXT    Texto
*----------------------------------------------------------------------*
*form f_partida_residual  using    p_bukrs
*                                  p_lifnr
*                                  p_belnr
*                                  p_valor
*                                  p_txt.
*
*data: vl_valor           like bsik-dmbtr.
*  select shkzg sum( dmbtr )
*    into table it_itens_bsik
*    from bsik
*   where bukrs eq p_bukrs
*     and lifnr eq p_lifnr
*     and belnr eq p_belnr
*     and buzei eq '0001'
*   group by shkzg.
*
*  wa_part_residual-lifnr = p_lifnr.
*  wa_part_residual-name1 = p_txt.
*  clear vl_valor.
*  loop at it_itens_bsik into wa_itens_bsik.
*    if wa_itens_bsik-shkzg eq 'H'.
*      vl_valor = vl_valor + ( wa_itens_bsik-dmbtr * -1 ).
*    else.
*      vl_valor = vl_valor + wa_itens_bsik-dmbtr.
*    endif.
*  endloop.
*
*  wa_part_residual-dmbtr = ( ( vl_valor * -1 ) - p_valor ) * -1.
*  if wa_part_residual-dmbtr ne 0.
*    append wa_part_residual to it_part_residual.
*  endif.
*endform.                    " f_partida_residual

FORM F_DESCONTO_DIF USING P_DIF  TYPE BSIK-DMBTR
                          P_LOTE TYPE zgl003_comp_f44
                 CHANGING C_DESCONTO.

  data: vl_hkont_desc       TYPE bsis-hkont,
        vl_bschl_desc       TYPE bsis-bschl,
        vl_gsber_desc       TYPE bsik-gsber,
        vl_dmbtr_desc_c     type char16,
        vl_dmbtr_desc       type dmbtr,
        vl_sgtxt_desc       type bsik-sgtxt,
        r_lim_desc          LIKE RANGE OF bsik-wrbtr WITH HEADER LINE.

  C_DESCONTO = ABAP_FALSE.

  CLEAR: r_lim_desc[].
  r_lim_desc-sign   = 'I'.
  r_lim_desc-option = 'BT'.
  r_lim_desc-low    = '-5'.
  r_lim_desc-high   = '5'.
  APPEND r_lim_desc.

  CHECK ( P_DIF NE 0 ) AND ( P_DIF in r_lim_desc  ).

  vl_dmbtr_desc = P_DIF.

  vl_dmbtr_desc_c = abs( vl_dmbtr_desc ) .
  TRANSLATE vl_dmbtr_desc_c USING '.,'.
  CONDENSE vl_dmbtr_desc_c NO-GAPS.

  if vl_dmbtr_desc < 0.
    vl_hkont_desc  = '0000331102'.
    vl_bschl_desc  = '50'.
  else.
    vl_hkont_desc   = '0000431101'.
    vl_bschl_desc   = '40'.
  endif.

  LOOP AT it_documentos INTO wa_documentos WHERE bukrs = P_LOTE-bukrs AND lote =  P_LOTE-lote.
    vl_gsber_desc =  wa_documentos-gsber.
    exit.
  ENDLOOP.

  "Dar baixa a diferênça """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM f_bdc_insert USING 'X' 'SAPDF05X'   '3100'  .
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'	'=KMD'  .

  "Efetuar Baixa na Diferênça """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM f_bdc_insert USING 'X' 'SAPMF05A'    '0700'            .
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'	 '/00'             .
  PERFORM f_bdc_insert USING ''  'RF05A-NEWKO' vl_hkont_desc     .
  PERFORM f_bdc_insert USING ''  'RF05A-NEWBS' vl_bschl_desc     .
  PERFORM f_bdc_insert USING 'X' 'SAPMF05A'	   '0300'	           .
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'	 '=ZK'             .
  PERFORM f_bdc_insert USING ''  'BSEG-WRBTR'	 vl_dmbtr_desc_c   .

  if vl_dmbtr_desc < 0.
    vl_sgtxt_desc = 'Desconto obtido'.
  else.
    vl_sgtxt_desc = 'Desconto concedido'.
  endif.

  PERFORM f_bdc_insert USING '' 'BSEG-SGTXT'   vl_sgtxt_desc   .

  "Entrar com a Divisão """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM f_bdc_insert USING 'X' 'SAPLKACB'	   '0002'	           .
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'	 '=ENTE'           .
  PERFORM f_bdc_insert USING ''  'COBL-GSBER'	 vl_gsber_desc     .

  PERFORM f_bdc_insert USING 'X' 'SAPMF05A'	   '0330'	           .

  "Simular lançamento """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'	 '=BS'           .
  PERFORM f_bdc_insert USING ''  'BDC_OKCODE'  '=BU'           .

  C_DESCONTO = ABAP_TRUE.

ENDFORM.
