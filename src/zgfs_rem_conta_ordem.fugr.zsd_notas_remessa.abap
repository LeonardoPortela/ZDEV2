FUNCTION zsd_notas_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_CHAVE_NF_VENDA) TYPE  ZDE_CHAVE_NFE
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     VALUE(E_NFNUM9) TYPE  J_1BNFNUM9
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  DATA: l_destino_ie TYPE numc11,
        l_stains     TYPE c LENGTH 18.

  FREE: ok_code,
        g_nfnum9,
        w_j_1bnfdoc,  "*-CS2024000522-18.07.2024-JT-#143588
        t_saida,
        t_set,
        r_cfop,
        g_custom_container,
        g_grid.

  CREATE OBJECT zcl_util.

  g_vbeln          = i_vbeln.
  g_chave_nf_venda = i_chave_nf_venda.

  l_seleciona      = abap_true.
  l_elimina        = abap_true.

*---------------------------------
* ler set
*---------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'ZLES0200_CFOP_CTA_ORDEM'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_set INTO w_set.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
  ENDLOOP.

*------------------------------------------
* Busca NF ja vinculada
*------------------------------------------
  SELECT *
    FROM zlest0210
      UP TO 1 ROWS
    INTO @DATA(w_0210)
    WHERE chave_nf_venda = @g_chave_nf_venda.
  ENDSELECT.

  IF sy-subrc = 0.
    w_campos_nfe = zcl_util->get_atributos_nfe( w_0210-chave_nf_cta_ordem ).
    g_nfnum9     = w_campos_nfe-nfnum9.

    IF "w_0210-ov_dummy      IS NOT INITIAL OR
       w_0210-remessa_dummy IS NOT INITIAL.
*     l_seleciona = abap_false.
      l_elimina   = abap_false.
    ENDIF.
  ELSE.
    CLEAR w_0210.  "*-CS2024000522-29.08.2024-JT-#150113-inicio
  ENDIF.

*------------------------------------------
* Buscar NFs para vinculacao
*------------------------------------------
  SELECT *
    FROM j_1bnfdoc
    INTO w_j_1bnfdoc  "*-CS2024000522-18.07.2024-JT-#143588
      UP TO 1 ROWS
   WHERE docnum = i_docnum.
  ENDSELECT.

  l_destino_ie = w_j_1bnfdoc-ie_bupla.  "*-CS2024000522-18.07.2024-JT-#143588

  SELECT *
    FROM vbpa
    INTO TABLE @DATA(t_vbpa)
   WHERE vbeln = @g_vbeln
     AND parvw = 'PC'.

  IF t_vbpa[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
       FOR ALL ENTRIES IN t_vbpa
     WHERE lifnr = t_vbpa-lifnr
       AND land1 = 'BR'.

    LOOP AT t_lfa1   INTO w_lfa1.
      w_lfa1-forne_cnpj = w_lfa1-stcd1.
      MODIFY t_lfa1  FROM w_lfa1 INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

  CONCATENATE '%' w_j_1bnfdoc-stains INTO l_stains.  "*-CS2024000522-18.07.2024-JT-#143588
  CONDENSE l_stains NO-GAPS.

  IF t_lfa1[] IS NOT INITIAL.
    SELECT zib_nfe_dist_ter~dt_emissao,
           zib_nfe_dist_ter~numero,
           zib_nfe_dist_ter~serie,
           zib_nfe_dist_ter~vl_total,
           zib_nfe_dist_ter~chave_nfe,
           zib_nfe_dist_itm~prod_descricao,
           zib_nfe_dist_itm~prod_und_comerci,
           zib_nfe_dist_itm~prod_qtd_comerci,
           zib_nfe_dist_ter~forne_cnpj,                              "*-CS2024000522-21.06.2024-JT-#143588
           zib_nfe_dist_ter~forne_ie                                 "*-CS2024000522-21.06.2024-JT-#143588
      FROM zib_nfe_dist_ter
     INNER JOIN zib_nfe_dist_itm
        ON zib_nfe_dist_itm~chave_nfe  = zib_nfe_dist_ter~chave_nfe
      INTO TABLE @DATA(t_zib_nfe)
*      FOR ALL ENTRIES IN @t_lfa1                                   "*-CS2024000522-21.06.2024-JT-#143588-comentado
     WHERE "zib_nfe_dist_ter~forne_cnpj LIKE @t_lfa1-forne_cnpj AND "*-CS2024000522-21.06.2024-JT-#143588-comentado
           zib_nfe_dist_ter~cancel     <> @abap_on
*      AND zib_nfe_dist_ter~destino_ie  = @w_jdoc-stains  "@l_destino_ie
*      AND zib_nfe_dist_ter~destino_ie  LIKE @l_stains              "*-CS2024000522-21.06.2024-JT-#143588-comentado
*      AND zib_nfe_dist_ter~forne_ie    = @t_lfa1-stcd3
       AND zib_nfe_dist_itm~prod_cfop  IN @r_cfop.

*-CS2024000522-21.06.2024-JT-#143588-inicio
    IF sy-subrc = 0.
      LOOP AT t_zib_nfe INTO DATA(w_zib_nfe_temp).
        l_tabix = sy-tabix.

        READ TABLE t_lfa1 INTO w_lfa1 WITH KEY forne_cnpj(8) = w_zib_nfe_temp-forne_cnpj(8).
        IF sy-subrc <> 0.
          DELETE t_zib_nfe INDEX l_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    FREE t_zib_nfe.
  ENDIF.
*-CS2024000522-21.06.2024-JT-#143588-fim

  LOOP AT t_zib_nfe  INTO DATA(w_zib_nfe).
    MOVE-CORRESPONDING w_zib_nfe TO w_saida.

    IF w_zib_nfe-prod_und_comerci = 'TO' OR
       w_zib_nfe-prod_und_comerci = 'TL' OR
       w_zib_nfe-prod_und_comerci = 'T'  OR
       w_zib_nfe-prod_und_comerci = 'TON'.
      w_saida-prod_qtd_comerci    = w_saida-prod_qtd_comerci * 1000.
    ENDIF.

    CHECK w_j_1bnfdoc-brgew = w_saida-prod_qtd_comerci.  "*-CS2024000522-18.07.2024-JT-#143588

*-CS2024000522-21.06.2024-JT-#143588-inicio
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = w_saida-forne_cnpj
      IMPORTING
        output = w_saida-forne_cnpj.
*-CS2024000522-21.06.2024-JT-#143588-fim

*-CS2024000522-18.07.2024-JT-#143588-inicio
    zcl_remessa_terceiro=>zif_remessa_terceiro~get_agente_frete( EXPORTING i_chave_nfe      = w_saida-chave_nfe "w_0210-chave_nf_cta_ordem
                                                                           i_vbeln          = i_vbeln
                                                                 IMPORTING e_placa          = w_saida-placa
                                                                           e_vlr_unit_frete = w_saida-vlr_unit_frete
                                                                           e_chave_cte      = w_saida-chave_cte
                                                                  CHANGING e_agente_frete   = w_saida-agente_frete ).
*-CS2024000522-18.07.2024-JT-#143588-fim

*   w_saida-dt_emissao            = w_zib_nfe-dt_emissao.
*   w_saida-numero                = w_zib_nfe-numero.
*   w_saida-serie                 = w_zib_nfe-serie.
*   w_saida-vl_total              = w_zib_nfe-vl_total.
*   w_saida-chave_nfe             = w_zib_nfe-chave_nfe.
    APPEND w_saida               TO t_saida.
  ENDLOOP.

  LOOP AT t_saida INTO w_saida.
    l_tabix = sy-tabix.

*-CS2024000522-29.08.2024-JT-#150113-inicio
*   IF w_saida-chave_nfe = w_0210-chave_nf_cta_ordem .
*     DELETE t_saida INDEX l_tabix.
*   ENDIF.
    IF w_0210 IS NOT INITIAL.
      IF w_saida-chave_nfe <> w_0210-chave_nf_cta_ordem .
        DELETE t_saida INDEX l_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

    SELECT chave_nf_cta_ordem
      FROM zlest0210
        UP TO 1 ROWS
      INTO @DATA(l_cta_ordem)
     WHERE chave_nf_cta_ordem = @w_saida-chave_nfe.
    ENDSELECT.

    IF sy-subrc = 0.
*-CS2024000522-29.08.2024-JT-#150113-inicio
      IF w_0210-chave_nf_cta_ordem IS NOT INITIAL.
        IF l_cta_ordem <> w_0210-chave_nf_cta_ordem.
          DELETE t_saida INDEX l_tabix.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSE.
        DELETE t_saida INDEX l_tabix.
      ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim
    ENDIF.
  ENDLOOP.

  READ TABLE t_saida INTO w_saida INDEX 1.

  IF sy-subrc <> 0 AND w_0210 IS INITIAL.
    RAISE not_found.
  ENDIF.

  SORT t_saida BY dt_emissao DESCENDING.

*-----------------------------
* selecao NF
*-----------------------------
  CALL SCREEN 100 STARTING AT  05  2     "25  5    *-CS2024000522-21.06.2024-JT-#143588
                    ENDING AT 185 21.    "162 18.  *-CS2024000522-21.06.2024-JT-#143588

  e_nfnum9 = g_nfnum9.

ENDFUNCTION.
