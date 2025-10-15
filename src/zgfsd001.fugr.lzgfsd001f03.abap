**----------------------------------------------------------------------*
****INCLUDE LZGFSD001F03.
**----------------------------------------------------------------------*
*************************************************************************
** Form  F_CRIA_TABELAS_OV
*************************************************************************
FORM f_cria_tabelas_ov USING p_doc_simulacao.

  DATA: w_zsdt0310     TYPE zsdt0310,
        w_zsdt0311     TYPE zsdt0311,
        w_zsdt0312     TYPE zsdt0312,
        w_vbak         TYPE vbak,
        w_vbap         TYPE vbap,
        w_vbkd         TYPE vbkd,
        w_lfa1         TYPE lfa1,
        t_konv         TYPE TABLE OF konv,
        w_konv         TYPE konv,
        w_makt         TYPE makt,
        w_t005u        TYPE t005u,
        w_t052u        TYPE t052u,
        w_tvtwt        TYPE tvtwt,
        w_tvakt        TYPE tvakt,
        w_ztext        TYPE ttext,
        l_erro         TYPE char1,
        l_taxa         TYPE ukurs_curr,
        l_data         TYPE datum,
        l_tenta        TYPE i,
        l_lifnr        TYPE lfa1-lifnr,
        l_total_item   TYPE vbap-netwr,
        l_seq_item     TYPE zsdt0312-seq_item,
        l_id_documento TYPE zsdt0310-id_documento,
        l_object       TYPE thead-tdobject,
        l_name         TYPE thead-tdname,
        l_id           TYPE thead-tdid,
        l_cond_pgto    TYPE dzterm,
        l_header       TYPE thead,
        t_lines        TYPE TABLE OF tline,
        w_lines        TYPE tline,
        r_vbeln        TYPE RANGE OF vbeln.

  FREE: t_konv.

*-----------------------------------
* simulador header
*-----------------------------------
  SELECT SINGLE *
    FROM zsdt0040
    INTO @DATA(w_0040)
   WHERE doc_simulacao = @p_doc_simulacao.

  CHECK sy-subrc = 0.

*-----------------------------------
* simulador itens
*-----------------------------------
  SELECT *
    FROM zsdt0041
    INTO TABLE @DATA(t_0041)
   WHERE doc_simulacao = @p_doc_simulacao.

  CHECK sy-subrc = 0.

*-----------------------------------
* data para pesquisar taxa de dolar
*-----------------------------------
  r_vbeln = VALUE #( FOR ls IN t_0041 LET s = 'I' o = 'EQ' IN sign = s option = o ( low = ls-vbeln ) ).

  SELECT MAX( erdat )
    FROM vbak
    INTO l_data
   WHERE vbeln IN r_vbeln.

*-----------------------------------
* busca taxa dolar
*-----------------------------------
  PERFORM f_busca_taxa USING l_data
                    CHANGING l_taxa.

*-----------------------------------
* verifica se todos os doctos ja estao commitados
*-----------------------------------
  DATA(t_0041_aux) = t_0041[].
  SORT t_0041_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_0041_aux
                        COMPARING vbeln.

  "BUG 104699 - BG - Deletar linhas da 41 com vbeln em branco
  DELETE t_0041_aux WHERE vbeln IS INITIAL.

  DESCRIBE TABLE t_0041_aux LINES DATA(l_lines_0041).

  l_tenta = 0.
  l_erro  = abap_off.

  DO.
    l_tenta = l_tenta + 1.
    IF l_tenta > 1000.
      l_erro = abap_true.
      EXIT.
    ENDIF.

    SELECT *
      FROM vbak
      INTO TABLE @DATA(t_vbak)
       FOR ALL ENTRIES IN @t_0041_aux
     WHERE vbeln = @t_0041_aux-vbeln.
    IF sy-subrc <> 0.
      FREE: t_vbak.
    ENDIF.

    DESCRIBE TABLE t_vbak LINES DATA(l_lines_vbak).

    IF l_lines_0041 = l_lines_vbak.
      EXIT.
    ENDIF.
    WAIT UP TO 3 SECONDS.
  ENDDO.

  CHECK l_erro = abap_false.

  SELECT *
    FROM vbap
    INTO TABLE @DATA(t_vbap)
     FOR ALL ENTRIES IN @t_0041
   WHERE vbeln = @t_0041-vbeln.
  IF sy-subrc <> 0.
    FREE: t_vbap.
  ENDIF.

  SELECT *
    FROM vbkd
    INTO TABLE @DATA(t_vbkd)
     FOR ALL ENTRIES IN @t_0041
   WHERE vbeln = @t_0041-vbeln.
  IF sy-subrc <> 0.
    FREE: t_vbap.
  ENDIF.

  SELECT SINGLE *
    FROM kna1
    INTO @DATA(w_kna1)
   WHERE kunnr = @w_0040-kunnr.
  IF sy-subrc <> 0.
    FREE: w_kna1.
  ENDIF.

  IF t_vbak[] IS NOT INITIAL.
    SELECT *
      FROM v_konv
      INTO CORRESPONDING FIELDS OF TABLE @t_konv
       FOR ALL ENTRIES IN @t_vbak
     WHERE knumv = @t_vbak-knumv
       AND kschl = 'PR00'.
  ENDIF.

  IF t_vbap[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE @DATA(t_makt)
       FOR ALL ENTRIES IN @t_vbap
     WHERE matnr = @t_vbap-matnr
       AND spras = @sy-langu.
    IF sy-subrc <> 0.
      FREE: t_makt.
    ENDIF.
  ELSE.
    FREE: t_makt.
  ENDIF.

*-----------------------------------
* checa se ficou alguma linha sem OV gerada
*-----------------------------------
  READ TABLE t_0041 INTO DATA(w_0041) WITH KEY vbeln = abap_off.

  CHECK sy-subrc <> 0.

*-----------------------------------
* recupera outras informacoes
*-----------------------------------
  SELECT SINGLE banco, agencia, conta
    INTO @DATA(w_0290)
    FROM zsdt0290
   WHERE area  = 'IN'
     AND vkorg = @w_0040-vkorg.
  IF sy-subrc <> 0.
    CLEAR w_0290.
  ENDIF.

*-----------------------------------
* gerar tabelas
*-----------------------------------
  PERFORM f_gera_seq     USING 'ZSD_ID_DOC'
                      CHANGING l_id_documento.

*-------------------------------------------------------------------------------
* Cria registros CTR - header
*-------------------------------------------------------------------------------
  CLEAR: w_zsdt0310, w_vbak, w_vbkd, w_tvtwt, w_t052u, w_ztext, w_t052u, l_cond_pgto.

  w_zsdt0310-mandt             = sy-mandt.
  w_zsdt0310-id_documento      = l_id_documento.
  w_zsdt0310-id_doc_agrupador  = 0.
  w_zsdt0310-area              = 'IN'.
  w_zsdt0310-tipo_doc          = 'CTR'.
  w_zsdt0310-nr_venda          = p_doc_simulacao.
  w_zsdt0310-vbeln             = abap_off.
  w_zsdt0310-tpsim             = w_0040-tpsim.
  w_zsdt0310-vkorg             = w_0040-vkorg.
  w_zsdt0310-vkbur             = w_0040-vkbur.
*-#146110-18.07.2024-#146110-JT-inicio
  w_zsdt0310-dt_vencimento     = w_0040-dtvencov.  "w_0040-dtpgtcult.
  IF w_zsdt0310-dt_vencimento IS INITIAL.
    w_zsdt0310-dt_vencimento   = w_0040-dtpgtcult. "w_0040-dtvencov.
  ENDIF.
*-#146110-18.07.2024-#146110-JT-fim
  w_zsdt0310-fazenda           = w_0040-fazenda.
  w_zsdt0310-area_ha           = w_0040-area_ha.
  w_zsdt0310-waerk             = w_0040-waerk.
  w_zsdt0310-kursf             = l_taxa. "w_0040-kursf.
  w_zsdt0310-tpcult            = w_0040-cultura.
  w_zsdt0310-safra             = w_0040-safra.
  w_zsdt0310-area_penhor       = w_0040-area_penhor.
  w_zsdt0310-vlrtot            = COND #( WHEN w_0040-waerk = 'BRL' THEN w_0040-vlrtot
                                                                   ELSE w_0040-vlrtot * l_taxa ).
  w_zsdt0310-vlrtot_usd        = COND #( WHEN w_0040-waerk = 'BRL' THEN 0
                                                                   ELSE w_0040-vlrtot ).
  w_zsdt0310-juros_ano        = w_0040-juros_ano. "163113 CS2019001753 Mel. na Ger. Documentos( CTR / OVD ) PSA
  w_zsdt0310-pag_prorrogado    = w_0040-pag_prorrogado.
  w_zsdt0310-tipo_doc_digital  = abap_off.
  w_zsdt0310-banco             = w_0290-banco.
  w_zsdt0310-agencia           = w_0290-agencia.
  w_zsdt0310-conta             = w_0290-conta.

*-US 130070-26.12.2023-JT-inicio
*-cond pagto
  CASE w_0040-tpsim.
    WHEN 'TS'.
      l_cond_pgto = 'I001'.
    WHEN 'AD'.
      l_cond_pgto = 'I002'.
    WHEN 'VV' OR 'VF' OR 'BN'.
      l_cond_pgto = 'I003'.
    WHEN 'TV'.
      l_cond_pgto = 'I004'.
    WHEN 'VP'.
      l_cond_pgto = 'I005'.
    WHEN 'PM'.
      l_cond_pgto = 'I006'.
  ENDCASE.

  SELECT SINGLE *
    FROM t052u
    INTO w_t052u
   WHERE spras  = sy-langu
     AND zterm  = l_cond_pgto.

  w_ztext = SWITCH #( l_cond_pgto
                      WHEN 'I003'           THEN COND #( WHEN w_0040-meio_pago = 'A' THEN 'A PRAZO COM ENTREGA POSTERIOR' ELSE 'A VISTA' ) " VV Venda a Vista
                      WHEN 'I001' OR 'I004' THEN 'A PRAZO COM ENTREGA POSTERIOR (TRC)'   " TS Troca Safra e Troca a Vista
                      WHEN 'I002'           THEN 'A PRAZO COM ENTREGA POSTERIOR (ADTO)'  " AD adiantamento
                                            ELSE w_t052u-text1 ).
  w_zsdt0310-tp_simulador      = w_ztext.
*-US 130070-26.12.2023-JT-fim

  w_zsdt0310-status            = '00'.
  w_zsdt0310-usname            = sy-uname.
  w_zsdt0310-data              = sy-datum.
  w_zsdt0310-hora              = sy-uzeit.

  MODIFY zsdt0310           FROM w_zsdt0310.

*-----------------------------------
* Cria registros CTR - cliente
*-----------------------------------
  CLEAR w_zsdt0311.

  w_zsdt0311-mandt             = sy-mandt.
  w_zsdt0311-id_documento      = l_id_documento.
  w_zsdt0311-seq_kunnr         = 1.
  w_zsdt0311-kunnr             = w_0040-kunnr.
  w_zsdt0311-tp_emitente       = 'EMITENTE'.
  MODIFY zsdt0311           FROM w_zsdt0311.

*-----------------------------------
* Cria registros CTR - itens
*-----------------------------------
  CLEAR l_seq_item.

  LOOP AT t_0041 INTO w_0041.

    CLEAR: w_zsdt0312, l_lifnr, w_lfa1, w_t005u, w_konv, w_makt,
           w_vbak,     w_vbap,  w_vbkd, w_tvakt.

    l_seq_item = l_seq_item + 1.

    TRY.
        w_vbak  = t_vbak[ vbeln = w_0041-vbeln ].
      CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
    ENDTRY.

    TRY.
        w_vbap  = t_vbap[ vbeln = w_0041-vbeln
                          matnr = w_0041-matnr ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    TRY.
        w_vbkd  = t_vbkd[ vbeln = w_0041-vbeln ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    TRY.
        w_konv  = t_konv[ knumv = w_vbak-knumv
                          kposn = w_vbap-posnr ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    TRY.
        w_makt  = t_makt[ matnr = w_vbap-matnr ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    l_lifnr = |{ w_0041-werks ALPHA = IN }|.

    SELECT SINGLE *
      FROM lfa1
      INTO w_lfa1
     WHERE lifnr = l_lifnr.

    w_zsdt0312-mandt           = sy-mandt.
    w_zsdt0312-id_documento    = l_id_documento.
    w_zsdt0312-seq_item        = l_seq_item.
    w_zsdt0312-vbeln           = w_vbap-vbeln.
    w_zsdt0312-posnr           = w_vbap-posnr.
    w_zsdt0312-tp_insumo       = COND #( WHEN w_vbap-spart = '02' THEN 'FET'
                                         WHEN w_vbap-spart = '03' THEN 'DEF'
                                         WHEN w_vbap-spart = '04' THEN 'SEM' ).
    w_zsdt0312-inco1           = w_vbkd-inco1.
    w_zsdt0312-matnr           = w_vbap-matnr.
    w_zsdt0312-desc_matnr      = w_vbap-arktx.
    w_zsdt0312-werks           = w_vbap-werks.

    IF w_vbkd-inco1 = 'FOB'.
      SELECT SINGLE *
        FROM t005u
        INTO w_t005u
       WHERE spras  = sy-langu
         AND land1  = 'BR'
         AND bland  = w_lfa1-regio.

      w_zsdt0312-endereco_entrega = w_lfa1-stras && '-' && w_lfa1-ort02 && '-' && w_lfa1-ort01 && '-' && w_t005u-bezei.
    ELSE.
      SELECT SINGLE *
        FROM t005u
        INTO w_t005u
       WHERE spras  = sy-langu
         AND land1  = 'BR'
         AND bland  = w_kna1-regio.

      w_zsdt0312-endereco_entrega = w_kna1-stras && '-' && w_kna1-ort02 && '-' && w_kna1-ort01 && '-' && w_t005u-bezei.
    ENDIF.

    TRY.
        w_zsdt0312-zmeng           = COND #( WHEN w_vbap-vrkme = w_konv-kmein THEN w_vbap-kwmeng
                                                                              ELSE w_vbap-kwmeng / w_konv-kumza ).
      CATCH cx_sy_zerodivide INTO DATA(l_zerodiv).
    ENDTRY.

*----------------------------------
*-- descricao UM
*----------------------------------
    FREE: t_lines.

    l_object = 'MATERIAL'.
    l_name   = w_vbap-matnr.
    l_id     = 'GRUN'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        object          = l_object
        name            = l_name
        id              = l_id
        language        = sy-langu
      IMPORTING
        header          = l_header
      TABLES
        lines           = t_lines
      EXCEPTIONS
        object          = 1
        id              = 2
        language        = 3
        name            = 4
        not_found       = 5
        reference_check = 6.

    TRY.
        w_zsdt0312-descricao_um = t_lines[ 1 ]-tdline.
      CATCH cx_sy_itab_line_not_found INTO l_error.
        CLEAR w_zsdt0312-descricao_um.
    ENDTRY.

*--------------------
    l_total_item = w_vbap-netwr + w_vbap-mwsbp.

    TRY.
        w_zsdt0312-vlr_unit        = COND #( WHEN w_vbap-vrkme = w_konv-kmein THEN l_total_item /   w_vbap-kwmeng
                                                                              ELSE l_total_item / ( w_vbap-kwmeng / w_konv-kumza ) ).
      CATCH cx_sy_zerodivide INTO l_zerodiv.
    ENDTRY.

    w_zsdt0312-vlr_total       = l_total_item.
    w_zsdt0312-dt_entrega      = COND #( WHEN w_vbap-spart = '02' THEN w_0040-dt_entrega_fet
                                         WHEN w_vbap-spart = '03' THEN w_0040-dt_entrega_def
                                         WHEN w_vbap-spart = '04' THEN w_0040-dt_entrega_sem ).
    w_zsdt0312-safra_aplicacao = w_0041-safra_apl.
    w_zsdt0312-valor_frete     = w_0041-vlr_frete.
    w_zsdt0312-cultura_aplicacao = w_0041-cultura_apl.

    MODIFY zsdt0312         FROM w_zsdt0312.
  ENDLOOP.

*-------------------------------------------------------------------------------
* Cria registros OVD - header
*-------------------------------------------------------------------------------
  DATA(t_0041_grp) = t_0041[].

  SORT t_0041_grp BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_0041_grp
                        COMPARING vbeln.

  LOOP AT t_0041_grp INTO w_0041.

    CLEAR: w_zsdt0310, w_vbak, w_vbkd, l_total_item.

*---------------
*-- checa se ja ha linha para a OV
*---------------
    SELECT *
      FROM zsdt0310
      INTO @DATA(w_0310)
        UP TO 1 ROWS
     WHERE nr_venda = @p_doc_simulacao
       AND vbeln    = @w_0041-vbeln
       AND tipo_doc = 'OVD'
       AND status  <> '10'.
    ENDSELECT.

    CHECK sy-subrc <> 0.

    PERFORM f_gera_seq     USING 'ZSD_ID_DOC'
                        CHANGING l_id_documento.

    LOOP AT t_vbap INTO w_vbap WHERE vbeln = w_0041-vbeln.
      l_total_item = l_total_item + w_vbap-netwr + w_vbap-mwsbp.
    ENDLOOP.

    TRY.
        w_vbak  = t_vbak[ vbeln = w_0041-vbeln ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

    TRY.
        w_vbkd  = t_vbkd[ vbeln = w_0041-vbeln ].
      CATCH cx_sy_itab_line_not_found INTO l_error.
    ENDTRY.

*-US 130070-26.12.2023-JT-inicio
    SELECT SINGLE *
      FROM tvtwt
      INTO w_tvtwt
     WHERE spras  = sy-langu
       AND vtweg  = w_vbak-vtweg.

    SELECT SINGLE *
      FROM t052u
      INTO w_t052u
     WHERE spras  = sy-langu
       AND zterm  = w_vbkd-zterm.

    w_ztext = SWITCH #( w_vbkd-zterm
                        WHEN 'I003'           THEN COND #( WHEN w_0040-meio_pago = 'A' THEN 'A PRAZO COM ENTREGA POSTERIOR' ELSE 'A VISTA' ) " VV Venda a Vista
                        WHEN 'I001' OR 'I004' THEN 'A PRAZO COM ENTREGA POSTERIOR (TRC)'   " TS Troca Safra e Troca a Vista
                        WHEN 'I002'           THEN 'A PRAZO COM ENTREGA POSTERIOR (ADTO)'  " AD adiantamento
                                              ELSE w_t052u-text1 ).
*-US 130070-26.12.2023-JT-fim

    w_zsdt0310-mandt             = sy-mandt.
    w_zsdt0310-id_documento      = l_id_documento.
    w_zsdt0310-id_doc_agrupador  = 0.
    w_zsdt0310-area              = 'IN'.
    w_zsdt0310-tipo_doc          = 'OVD'.
    w_zsdt0310-nr_venda          = p_doc_simulacao.
    w_zsdt0310-vbeln             = w_vbak-vbeln.
    w_zsdt0310-tpsim             = w_0040-tpsim.
    w_zsdt0310-vkorg             = w_vbak-vkorg.
    w_zsdt0310-vkbur             = w_vbak-vkbur.
    w_zsdt0310-dt_vencimento     = w_vbkd-valdt.
    w_zsdt0310-fazenda           = w_0040-fazenda.
    w_zsdt0310-area_ha           = abap_off.
    w_zsdt0310-waerk             = w_vbak-waerk.
    w_zsdt0310-kursf             = l_taxa. "w_vbkd-kurrf.
    w_zsdt0310-tpcult            = w_0040-cultura.
    w_zsdt0310-safra             = w_0040-safra.
    w_zsdt0310-area_penhor       = abap_off.
    w_zsdt0310-vlrtot            = COND #( WHEN w_0040-waerk = 'BRL' THEN l_total_item
                                                                     ELSE l_total_item * l_taxa ).
    w_zsdt0310-vlrtot_usd        = COND #( WHEN w_0040-waerk = 'BRL' THEN 0
                                                                     ELSE l_total_item ).
    w_zsdt0310-juros_ano        = w_0040-juros_ano. "0. "163113 CS2019001753 Mel. na Ger. Documentos( CTR / OVD ) PSA
    w_zsdt0310-pag_prorrogado    = abap_off.
    w_zsdt0310-tipo_doc_digital  = abap_off.
    w_zsdt0310-banco             = w_0290-banco.
    w_zsdt0310-agencia           = w_0290-agencia.
    w_zsdt0310-conta             = w_0290-conta.

*-US 130070-26.12.2023-JT-inicio
    w_zsdt0310-canal_distribuicao = w_tvtwt-vtext.
    w_zsdt0310-tp_simulador      = w_ztext.
    w_zsdt0310-imp_valor_unit    = abap_off.

*----------------------------------
*-observacao
*----------------------------------
    FREE: t_lines.

    l_object = 'VBBK'.
    l_name   = w_vbak-vbeln.
    l_id     = '0002'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        object          = l_object
        name            = l_name
        id              = l_id
        language        = sy-langu
      IMPORTING
        header          = l_header
      TABLES
        lines           = t_lines
      EXCEPTIONS
        object          = 1
        id              = 2
        language        = 3
        name            = 4
        not_found       = 5
        reference_check = 6.

    IF t_lines[] IS NOT INITIAL.
      DESCRIBE TABLE t_lines LINES DATA(l_lines).
      w_zsdt0310-observacao      = abap_off. "'Observações: '.
      LOOP AT t_lines INTO w_lines.
        IF sy-tabix = l_lines.
          w_zsdt0310-observacao  = w_zsdt0310-observacao && w_lines-tdline.
        ELSE.
          w_zsdt0310-observacao  = w_zsdt0310-observacao && w_lines-tdline && '-'..
        ENDIF.
      ENDLOOP.
    ENDIF.
*-US 130070-26.12.2023-JT-fim

    w_zsdt0310-status            = '00'.
    w_zsdt0310-usname            = sy-uname.
    w_zsdt0310-data              = sy-datum.
    w_zsdt0310-hora              = sy-uzeit.

    MODIFY zsdt0310           FROM w_zsdt0310.

*-----------------------------------
*-- Cria registros OVD - cliente
*-----------------------------------
    CLEAR w_zsdt0311.

    w_zsdt0311-mandt             = sy-mandt.
    w_zsdt0311-id_documento      = l_id_documento.
    w_zsdt0311-seq_kunnr         = 1.
    w_zsdt0311-kunnr             = w_vbak-kunnr.
    w_zsdt0311-tp_emitente       = 'EMITENTE'.

    MODIFY zsdt0311           FROM w_zsdt0311.

*-----------------------------------
* Cria registros CTR - itens
*-----------------------------------
    CLEAR l_seq_item.

    LOOP AT t_vbap INTO w_vbap WHERE vbeln = w_0041-vbeln.

      CLEAR: w_zsdt0312, l_lifnr, w_lfa1, w_t005u, w_konv, w_makt.

      l_seq_item = l_seq_item + 1.

      TRY.
          w_konv  = t_konv[ knumv = w_vbak-knumv
                            kposn = w_vbap-posnr ].
        CATCH cx_sy_itab_line_not_found INTO l_error.
      ENDTRY.

      TRY.
          w_makt  = t_makt[ matnr = w_vbap-matnr ].
        CATCH cx_sy_itab_line_not_found INTO l_error.
      ENDTRY.

      l_lifnr = |{ w_vbap-werks ALPHA = IN }|.

      SELECT SINGLE *
        FROM lfa1
        INTO w_lfa1
       WHERE lifnr = l_lifnr.

      w_zsdt0312-mandt           = sy-mandt.
      w_zsdt0312-id_documento    = l_id_documento.
      w_zsdt0312-seq_item        = l_seq_item.
      w_zsdt0312-vbeln           = w_vbap-vbeln.
      w_zsdt0312-posnr           = w_vbap-posnr.
      w_zsdt0312-tp_insumo       = COND #( WHEN w_vbap-spart = '02' THEN 'FET'
                                           WHEN w_vbap-spart = '03' THEN 'DEF'
                                           WHEN w_vbap-spart = '04' THEN 'SEM' ).
      w_zsdt0312-inco1           = w_vbkd-inco1.
      w_zsdt0312-matnr           = w_vbap-matnr.
      w_zsdt0312-desc_matnr      = w_vbap-arktx.
      w_zsdt0312-werks           = w_vbap-werks.

*-US 130070-26.12.2023-JT-inicio
      SELECT SINGLE *
        FROM tvakt
        INTO w_tvakt
       WHERE spras  = sy-langu
         AND auart  = w_vbak-auart.

      w_zsdt0312-tp_ov           = w_tvakt-auart && '-' && w_tvakt-bezei.
*-US 130070-26.12.2023-JT-fim

      IF w_vbkd-inco1 = 'FOB'.
        SELECT SINGLE *
          FROM t005u
          INTO w_t005u
         WHERE spras  = sy-langu
           AND land1  = 'BR'
           AND bland  = w_lfa1-regio.

        w_zsdt0312-endereco_entrega = w_lfa1-stras && '-' && w_lfa1-ort02 && '-' && w_lfa1-ort01 && '-' && w_t005u-bezei.
      ELSE.
        SELECT SINGLE *
          FROM t005u
          INTO w_t005u
         WHERE spras  = sy-langu
           AND land1  = 'BR'
           AND bland  = w_kna1-regio.

        w_zsdt0312-endereco_entrega = w_kna1-stras && '-' && w_kna1-ort02 && '-' && w_kna1-ort01 && '-' && w_t005u-bezei.
      ENDIF.

      TRY.
          w_zsdt0312-zmeng           = COND #( WHEN w_vbap-vrkme = w_konv-kmein THEN w_vbap-kwmeng
                                                                                ELSE w_vbap-kwmeng / w_konv-kumza ).
        CATCH cx_sy_zerodivide INTO l_zerodiv.
      ENDTRY.

*----------------------------------
*---- descricao UM
*----------------------------------
      FREE: t_lines.

      l_object = 'MATERIAL'.
      l_name   = w_vbap-matnr.
      l_id     = 'GRUN'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          object          = l_object
          name            = l_name
          id              = l_id
          language        = sy-langu
        IMPORTING
          header          = l_header
        TABLES
          lines           = t_lines
        EXCEPTIONS
          object          = 1
          id              = 2
          language        = 3
          name            = 4
          not_found       = 5
          reference_check = 6.

      TRY.
          w_zsdt0312-descricao_um = t_lines[ 1 ]-tdline.
        CATCH cx_sy_itab_line_not_found INTO l_error.
          CLEAR w_zsdt0312-descricao_um.
      ENDTRY.

*--------------------
      l_total_item               = w_vbap-netwr + w_vbap-mwsbp.

      TRY.
          w_zsdt0312-vlr_unit        = COND #( WHEN w_vbap-vrkme = w_konv-kmein THEN l_total_item /   w_vbap-kwmeng
                                                                                ELSE l_total_item / ( w_vbap-kwmeng / w_konv-kumza ) ).
        CATCH cx_sy_zerodivide INTO l_zerodiv.
      ENDTRY.

      w_zsdt0312-vlr_total       = l_total_item.
      w_zsdt0312-dt_entrega      = COND #( WHEN w_vbap-spart = '02' THEN w_0040-dt_entrega_fet
                                           WHEN w_vbap-spart = '03' THEN w_0040-dt_entrega_def
                                           WHEN w_vbap-spart = '04' THEN w_0040-dt_entrega_sem ).
      w_zsdt0312-safra_aplicacao = w_0041-safra_apl.
      w_zsdt0312-valor_frete     = w_0041-vlr_frete.
      w_zsdt0312-cultura_aplicacao = w_0041-cultura_apl.

      MODIFY zsdt0312         FROM w_zsdt0312.
    ENDLOOP.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* buscar taxa dolar
************************************************************************
FORM f_busca_taxa   USING p_data
                 CHANGING p_taxa.

  DATA: l_gdatu TYPE gdatu_inv.

  DATA(obj_zcl_util_sd) = NEW zcl_util_sd( ).

  FREE: p_taxa.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').

  MOVE p_data TO l_gdatu.

  obj_zcl_util_sd->set_data( l_gdatu ).
  p_taxa = obj_zcl_util_sd->taxa_cambio( ).

ENDFORM.                    " BUSCA_TAXA

************************************************************************
* gerar sequenciador
************************************************************************
FORM f_gera_seq     USING p_objeto
                 CHANGING p_seq.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = p_objeto
    IMPORTING
      number                  = p_seq
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

ENDFORM.

************************************************************************
*************************************************************************
