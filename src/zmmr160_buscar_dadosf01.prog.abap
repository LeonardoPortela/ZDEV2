*----------------------------------------------------------------------*
***INCLUDE ZMMR160_BUSCAR_DADOSF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_dados .

  REFRESH rg_docref.

  SELECT *
    FROM zfiwrt0008
     INTO TABLE it_zfiwrt0008
   WHERE bukrs  IN sempre
     AND branch IN sfilia
     AND parid  IN sparc
     AND bldat  IN sdtpos
     AND cfop   IN scfop
     AND docref EQ ' '.

  IF it_zfiwrt0008 IS NOT INITIAL.

    "NF Remessa
    SELECT *
    FROM  j_1bnfdoc
      INTO TABLE it_j_1bnfdoc
     FOR ALL ENTRIES IN it_zfiwrt0008
   WHERE docnum EQ it_zfiwrt0008-docnum.

    CLEAR rg_docref.

    LOOP AT it_zfiwrt0008 INTO wa_zfiwrt0008.
      rg_docref-sign = 'I'.
      rg_docref-option = 'EQ'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zfiwrt0008-seq_lcto
        IMPORTING
          output = rg_docref-low.

      APPEND rg_docref.
      CLEAR wa_zfiwrt0008.
    ENDLOOP.

    SELECT * INTO TABLE it_zfiwrt0008_doc
      FROM zfiwrt0008
      WHERE docref IN rg_docref.

    SELECT * FROM zfiwrt0009
      INTO TABLE it_zfiwrt0009
       FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto
      AND  matnr    IN sprod.

    SELECT *
      FROM zfiwrt0001
      INTO TABLE it_zfiwrt0001
      FOR ALL ENTRIES IN it_zfiwrt0008
    WHERE operacao EQ it_zfiwrt0008-operacao.


    IF it_zfiwrt0008_doc IS NOT INITIAL.

      SELECT * FROM zfiwrt0009
        INTO TABLE it_zfiwrt0009_doc
        FOR ALL ENTRIES IN it_zfiwrt0008_doc
          WHERE seq_lcto EQ it_zfiwrt0008_doc-seq_lcto
            AND matnr    IN sprod.

      "NF Retorno
      SELECT * INTO TABLE it_j_1bnfdoc_doc
      FROM  j_1bnfdoc
       FOR ALL ENTRIES IN it_zfiwrt0008_doc
     WHERE docnum EQ it_zfiwrt0008_doc-docnum.
    ENDIF.

    SELECT * INTO TABLE it_t001w
      FROM t001w
       FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE j_1bbranch EQ it_zfiwrt0008-branch.

    SELECT * INTO TABLE it_makt
        FROM makt
         FOR ALL ENTRIES IN it_zfiwrt0009
       WHERE matnr EQ it_zfiwrt0009-matnr .

    SELECT * INTO TABLE it_kna1
        FROM kna1
         FOR ALL ENTRIES IN it_zfiwrt0008
       WHERE kunnr EQ it_zfiwrt0008-parid .

    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE lifnr EQ it_zfiwrt0008-parid.


    PERFORM trata_dados.

    CALL SCREEN 0100.

  ELSE.
    MESSAGE  'Dados não encontrados!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.


FORM trata_dados.

  DATA: v_docref      TYPE zfiwrt0008-docref,
        v_dias        TYPE zfiwrt0001-dias,
        v_count       TYPE i,
        v_data_maior  TYPE sy-datum,
        v_data_menor  TYPE sy-datum,
        v_dias_venc   TYPE zfiwrt0001-dias,
        v_sobra       TYPE zfiwrt0009-menge,
        v_saldo_anter TYPE zfiwrt0009-menge,
        v_contador    TYPE i,
        v_dt_limite   TYPE zde_dt_limite.



  SORT: it_zfiwrt0009     BY matnr,
        it_zfiwrt0009_doc BY matnr.

  DESCRIBE TABLE it_zfiwrt0009_doc LINES DATA(v_lines).
  v_contador = 0.

  LOOP AT it_zfiwrt0009 INTO DATA(wa_0009).
    READ TABLE it_zfiwrt0009_doc INTO wa_zfiwrt0009_doc WITH KEY matnr = wa_0009-matnr.
    IF sy-subrc EQ 0.
      v_contador = v_contador + 1.
      IF v_contador = v_lines.
        READ TABLE it_zfiwrt0008_doc INTO wa_zfiwrt0008_doc WITH KEY seq_lcto = wa_zfiwrt0009_doc-seq_lcto.
        wa_zfiwrt0008_doc-v_rep_mat = 'X'.
        MODIFY it_zfiwrt0008_doc FROM wa_zfiwrt0008_doc INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.


  LOOP AT it_zfiwrt0009_doc INTO wa_zfiwrt0009_doc
    GROUP BY ( matnr = wa_zfiwrt0009_doc-matnr )
    ASCENDING
    ASSIGNING FIELD-SYMBOL(<it_zfiwrt0009_doc2>).
    DATA(v_sum_matnr) = REDUCE i( INIT x = 0 FOR wa IN it_zfiwrt0009_doc WHERE ( matnr = <it_zfiwrt0009_doc2>-matnr
                                                                         AND v_mat_lido = ' '        )  NEXT x = x + wa-menge ).
  ENDLOOP.




  LOOP AT it_zfiwrt0008 INTO wa_zfiwrt0008.

    LOOP AT it_zfiwrt0009 INTO wa_zfiwrt0009 WHERE seq_lcto = wa_zfiwrt0008-seq_lcto.

      CLEAR: wa_saida, v_count.

      PERFORM get_color_cell.

      wa_saida-branch        = wa_zfiwrt0008-branch.
      wa_saida-doc_remessa   = wa_zfiwrt0008-docnum.
      wa_saida-nf_emissao    = wa_zfiwrt0008-budat.
      wa_saida-produto_uni   = wa_zfiwrt0009-meins.
      wa_saida-nf_cfop       = wa_zfiwrt0009-cfop.
      wa_saida-nf_quantidade = wa_zfiwrt0009-menge.


      READ TABLE it_t001w INTO wa_t001w WITH KEY j_1bbranch = wa_saida-branch.
      wa_saida-branch_nome = wa_t001w-name1.

      IF wa_zfiwrt0008-parvw EQ 'AG'.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zfiwrt0008-parid.
        wa_saida-parceiro_cod  = wa_kna1-kunnr.
        wa_saida-parceiro_nome = wa_kna1-name1.
        wa_saida-parceiro_cnpj = wa_kna1-stcd1.
      ELSE.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zfiwrt0008-parid.
        wa_saida-parceiro_cod   = wa_lfa1-lifnr.
        wa_saida-parceiro_nome  = wa_lfa1-name1.
        wa_saida-parceiro_cnpj  = wa_lfa1-stcd1.
      ENDIF.


      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zfiwrt0009-matnr.
      IF sy-subrc EQ 0.
        wa_saida-produto_cod  = wa_makt-matnr.
        wa_saida-produto_nome = wa_makt-maktx.
      ENDIF.

      READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_zfiwrt0008-docnum.
      IF sy-subrc EQ 0.
        wa_saida-nf_numero         = wa_j_1bnfdoc-nfenum.
      ENDIF.

      READ TABLE it_zfiwrt0001 INTO wa_zfiwrt0001 WITH  KEY operacao = wa_zfiwrt0008-operacao.
      IF sy-subrc EQ 0.
        wa_saida-dt_limite = wa_zfiwrt0008-budat + wa_zfiwrt0001-dias.
        wa_saida-dias_ret = wa_zfiwrt0001-dias.
      ENDIF.

      IF wa_saida-dt_limite >= sy-datum.
        v_data_maior = wa_saida-dt_limite.
        v_data_menor = sy-datum.
      ELSE.
        v_data_maior = sy-datum.
        v_data_menor = wa_saida-dt_limite.
      ENDIF.
      v_dt_limite = wa_saida-dt_limite.

* ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947  - INICIO

*      wa_saida-nr_dias_vencto = v_data_maior - v_data_menor.
*
*      IF wa_saida-dt_limite < sy-datum.
*        wa_saida-nr_dias_vencto = wa_saida-nr_dias_vencto * -1.
*        wa_saida-nr_dias_vencto = |{ wa_saida-nr_dias_vencto SIGN = LEFT }|.
*      ENDIF.

* ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947  - FIM

***** Dados de Retorno.
      CLEAR: v_docref.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zfiwrt0008-seq_lcto
        IMPORTING
          output = v_docref.

      LOOP AT it_zfiwrt0008_doc INTO wa_zfiwrt0008_doc
         WHERE docref = wa_zfiwrt0008-seq_lcto.

        v_count = 1.

        READ TABLE it_zfiwrt0009_doc INTO wa_zfiwrt0009_doc WITH KEY seq_lcto  = wa_zfiwrt0008_doc-seq_lcto
                                                                     matnr     = wa_saida-produto_cod
                                                                     v_mat_lido = ' '.
        IF sy-subrc EQ 0.

          IF wa_zfiwrt0008_doc-v_rep_mat EQ 'X'.
            IF v_sobra IS INITIAL.
              v_sobra = v_sum_matnr - wa_zfiwrt0009-menge.
              wa_saida-nf_ret_quantidade = wa_zfiwrt0009-menge.
            ELSEIF v_saldo_anter = 0.
              wa_saida-nf_ret_quantidade = v_sobra.
            ENDIF.
          ELSE.
            wa_saida-nf_ret_quantidade = wa_zfiwrt0009_doc-menge.
          ENDIF.

          wa_saida-nf_ret_cfop       = wa_zfiwrt0009_doc-cfop.
          wa_saida-doc_retorno       = wa_zfiwrt0008_doc-docnum.
          wa_saida-nf_ret_emissao    = wa_zfiwrt0008_doc-bldat.
          wa_saida-nf_ret_lancame    = wa_zfiwrt0008_doc-budat.
          wa_saida-nf_saldo          = wa_saida-nf_quantidade - wa_saida-nf_ret_quantidade.
          v_saldo_anter = wa_saida-nf_saldo .

          wa_zfiwrt0009_doc-v_mat_lido = 'X'.
          MODIFY it_zfiwrt0009_doc FROM wa_zfiwrt0009_doc INDEX sy-tabix.

          READ TABLE it_j_1bnfdoc_doc INTO wa_j_1bnfdoc_doc WITH KEY docnum = wa_zfiwrt0008_doc-docnum.
          IF sy-subrc EQ 0.
            wa_saida-nf_ret_numero     = wa_j_1bnfdoc_doc-nfenum.
            wa_saida-nf_ret_entrada       = wa_j_1bnfdoc_doc-pstdat.

          ENDIF.

        ENDIF.

*        IF  wa_saida-nf_ret_numero  IS NOT INITIAL AND  wa_saida-nf_ret_emissao <= wa_saida-dt_limite.
*          wa_saida-ds_status = 'OK'.
*        ENDIF.
*
*        IF  wa_saida-nf_ret_numero  IS NOT INITIAL AND  wa_saida-nf_ret_emissao > wa_saida-dt_limite.
*          wa_saida-ds_status = 'Vencido'.
*        ENDIF.

        IF wa_saida-nf_ret_emissao  IS NOT INITIAL.
          wa_saida-nr_dias_vencto = wa_saida-dt_limite - wa_saida-nf_ret_emissao.
        ELSE.
          wa_saida-nr_dias_vencto = wa_saida-dt_limite - sy-datum.
        ENDIF.
        IF  wa_saida-nf_ret_numero > 0.
          wa_saida-ds_status = 'OK'.
        ELSEIF wa_saida-nr_dias_vencto < 0.
          wa_saida-ds_status = 'Vencido'.
        ELSE.
          wa_saida-ds_status = 'Pendente'.
        ENDIF.

        wa_saida-dat_rel = sy-datum.

        APPEND wa_saida TO it_saida.
        CLEAR: wa_zfiwrt0009_doc, wa_zfiwrt0008_doc, wa_j_1bnfdoc_doc.
      ENDLOOP.

      IF  v_count = 0.
*        IF wa_saida-nf_ret_emissao IS INITIAL AND wa_saida-dt_limite >= sy-datum.
*          wa_saida-ds_status = 'PENDENTE'.
*        ENDIF.
*
*        IF  wa_saida-nf_ret_numero  IS INITIAL AND wa_saida-dt_limite <= sy-datum.
*          wa_saida-ds_status = 'Vencido'.
*        ENDIF.
*         ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947  - INICIO

        IF wa_saida-nf_ret_emissao  IS NOT INITIAL.
          wa_saida-nr_dias_vencto = wa_saida-dt_limite - wa_saida-nf_ret_emissao.
        ELSE.
          wa_saida-nr_dias_vencto = wa_saida-dt_limite - sy-datum.
        ENDIF.

        "ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947  / Anderson Oenning
*        IF wa_saida-dt_limite < sy-datum.
*          wa_saida-nr_dias_vencto = wa_saida-nr_dias_vencto * -1.
*          wa_saida-nr_dias_vencto = |{ wa_saida-nr_dias_vencto SIGN = LEFT }|.
*        ENDIF.
        "ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947 / Anderson Oenning

        IF  wa_saida-nf_ret_numero > 0.
          wa_saida-ds_status = 'OK'.
        ELSEIF wa_saida-nr_dias_vencto < 0.
          wa_saida-ds_status = 'Vencido'.
        ELSE.
          wa_saida-ds_status = 'Pendente'.
        ENDIF.
        wa_saida-dat_rel = sy-datum.
* ZMM-CS2019000988 - RelContMercaPodTerc - BG - BUG #76947  - FIM
        APPEND wa_saida TO it_saida.
        CLEAR wa_saida.

      ENDIF.
    ENDLOOP.

    CLEAR: wa_zfiwrt0008, wa_zfiwrt0009, wa_t001w, wa_kna1, wa_lfa1, wa_makt, wa_j_1bnfdoc.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GET_COLOR_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_color_cell .

  DATA: it_color TYPE lvc_t_scol,
        lc_color TYPE lvc_s_colo,
        wa_color TYPE lvc_s_scol.

  lc_color-col = '7'.
  lc_color-int = '0'.
  lc_color-inv = '0'.

  wa_color-color = lc_color.
  wa_color-fname = 'NF_DOCNUM'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_NUMERO'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_EMISSAO'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_CFOP'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_QUANTIDADE'.
  APPEND wa_color TO it_color.

  lc_color-col = '5'.
  wa_color-color = lc_color.
  wa_color-fname = 'NF_RET_DOCNUM'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_RET_NUMERO'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_RET_EMISSAO'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_RET_LANCAME'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_RET_CFOP'.
  APPEND wa_color TO it_color.
  wa_color-fname = 'NF_RET_QUANTIDADE'.
  APPEND wa_color TO it_color.

  wa_saida-color_cell = it_color.
ENDFORM.
