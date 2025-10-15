*&---------------------------------------------------------------------*
*&  Include           ZSDR0059_FORM
*&---------------------------------------------------------------------*

FORM f_iniciar_variaveis .

ENDFORM.

FORM f_selecionar_dados.

  DATA: wl_bapibranch TYPE bapibranch,
        vl_state_insc TYPE j_1bstains,
        v_stcd3       TYPE lfa1-stcd3.

  "Limpar Variaveis
  CLEAR: it_saida_0100[],
         tg_lfa1[],
         tg_ekko[],
         tg_ekbe[],
         tg_rbkp[],
         tg_mkpf[],
         tg_zsdt0127[],
         tg_j_1bnfdoc[],
         tg_zib_nfe_dist_ter[],
         tg_zib_nfe_dist_itm[],
         tg_zib_cte_dist_ter[],
         tg_j_1bnfe_active[].

*USER STORY 163035 - MMSILVA - 08.01.2025 - Inicio
  IF ( p_lifnr IS NOT INITIAL ) OR ( p_stcd2 IS NOT INITIAL ).
    SELECT stcd3, stcd1
      FROM lfa1
      WHERE lifnr IN @p_lifnr
      AND   stcd2 IN @p_stcd2
      AND substring( stcd3, 1,1 ) NOT BETWEEN 'A' AND 'Z'
      INTO TABLE @DATA(it_lifnr).

    IF it_lifnr IS NOT INITIAL.
      LOOP AT it_lifnr INTO DATA(wa_lifnr).
        IF ( wa_lifnr-stcd3 IS NOT INITIAL ).
          p_for_ie-sign = 'I'.
          p_for_ie-option = 'EQ'.
          p_for_ie-low = wa_lifnr-stcd3.
          APPEND p_for_ie TO p_for_ie[].
        ENDIF.

        IF wa_lifnr-stcd1 IS NOT INITIAL.
          p_cnpj-sign = 'I'.
          p_cnpj-option = 'EQ'.
          p_cnpj-low = wa_lifnr-stcd1.
          APPEND p_cnpj TO p_cnpj[].
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*USER STORY 163035 - MMSILVA - 08.01.2025 - Fim

*-US 138094-10-07-2024-#138094-RJF-inicio
  DATA: lv_for_ie TYPE p.
  LOOP AT p_for_ie ASSIGNING FIELD-SYMBOL(<fs_for_ie>).
    lv_for_ie = <fs_for_ie>-low.
    <fs_for_ie>-low = lv_for_ie.
    CONDENSE <fs_for_ie>-low NO-GAPS.
    r_st_for-sign = <fs_for_ie>-sign.
    r_st_for-option = <fs_for_ie>-option.
    r_st_for-low = <fs_for_ie>-low.
    r_st_for-high = <fs_for_ie>-high.
    APPEND r_st_for TO r_st_for.
    CLEAR r_st_for.
  ENDLOOP.
*-US 138094-10-07-2024-#138094-RJF-fim

  IF p_distri IS NOT INITIAL. "Tabelas de Distribuição
    IF ( p_modelo EQ '55' ) OR ( p_modelo IS INITIAL ).
      SELECT *
        FROM zib_nfe_dist_ter
       WHERE forne_cnpj      IN @p_cnpj
         AND ltrim( forne_ie,'0' ) IN @r_st_for "RJF
         AND dt_emissao      IN @p_data
         AND numero          IN @p_nota
         AND bukrs           IN @p_bukrs
         AND branch          IN @p_branch
         AND cancel          IN @r_st_xml
         AND model           IN @r_modelo
         AND chave_nfe       IN @p_chave
        INTO CORRESPONDING FIELDS OF TABLE @tg_zib_nfe_dist_ter.

    ENDIF.

    IF ( p_modelo EQ '57' ) OR ( p_modelo IS INITIAL ).
      SELECT *
        FROM zib_cte_dist_ter INTO CORRESPONDING FIELDS OF TABLE tg_zib_cte_dist_ter
       WHERE emit_cnpj       IN p_cnpj
         AND dt_emissao      IN p_data
         AND numr_cte        IN p_nota
         AND e_tomadora      IN p_bukrs
         AND f_tomadora      IN p_branch
         AND cancel          IN r_st_xml
         AND modelo          IN r_modelo
         AND cd_chave_cte    IN p_chave.
    ENDIF.

  ELSE.
    SELECT *
      FROM zib_nfe_forn INTO TABLE tg_zib_nfe_forn
     WHERE nu_chave_cnpj   IN p_cnpj
       AND dt_emissao      IN p_data
       AND nu_chave_numero IN p_nota
       AND st_nota         IN r_st_xml
       AND nu_chave_modelo IN r_modelo
       AND bukrs           IN p_bukrs
       AND branch          IN p_branch
       AND nu_chave        IN p_chave.

    IF tg_zib_nfe_forn[] IS NOT INITIAL.

*-US 145401-15-07-2024-#145401-RJF-início
      LOOP AT tg_zib_nfe_forn ASSIGNING FIELD-SYMBOL(<fs_zib>).
        r_chave-sign = 'I'.
        r_chave-option = 'EQ'.
        r_chave-low = <fs_zib>-nu_chave.
        APPEND r_chave TO r_chave.
        CLEAR r_chave.
      ENDLOOP.
*-US 145401-15-07-2024-#145401-RJF-fim

      SELECT *
        FROM zib_nfe_dist_ter
*        FOR ALL ENTRIES IN tg_zib_nfe_forn
*        WHERE chave_nfe = tg_zib_nfe_forn-nu_chave
        WHERE chave_nfe IN @r_chave
          AND ltrim( forne_ie,'0' ) IN @r_st_for "RJF. -US 145401-15-07-2024-#145401-RJF
        INTO CORRESPONDING FIELDS OF TABLE @tg_zib_nfe_dist_ter_aux.

      SORT tg_zib_nfe_dist_ter_aux BY chave_nfe.

      SELECT *
        FROM zib_cte_dist_ter INTO CORRESPONDING FIELDS OF TABLE tg_zib_cte_dist_ter_aux
        FOR ALL ENTRIES IN tg_zib_nfe_forn
        WHERE cd_chave_cte = tg_zib_nfe_forn-nu_chave.
      SORT tg_zib_cte_dist_ter_aux BY cd_chave_cte.

    ENDIF.
    CLEAR: tg_dados_filial[].
    tg_zib_nfe_forn_aux[] = tg_zib_nfe_forn[].
    SORT tg_zib_nfe_forn_aux BY bukrs branch.
    DELETE ADJACENT DUPLICATES FROM tg_zib_nfe_forn_aux COMPARING bukrs branch.
    DELETE tg_zib_nfe_forn_aux WHERE ( bukrs  IS INITIAL ) OR
                                     ( branch IS INITIAL ).

    LOOP AT tg_zib_nfe_forn_aux.
      CLEAR: wl_bapibranch, vl_state_insc.

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = tg_zib_nfe_forn_aux-bukrs
          branch        = tg_zib_nfe_forn_aux-branch
        IMPORTING
          branch_detail = wl_bapibranch
          e_state_insc  = vl_state_insc.

      tg_dados_filial-bukrs      = tg_zib_nfe_forn_aux-bukrs.
      tg_dados_filial-branch     = tg_zib_nfe_forn_aux-branch.
      tg_dados_filial-stcd1      = wl_bapibranch-cgc_number.
      tg_dados_filial-state_insc = vl_state_insc.
      APPEND tg_dados_filial.
    ENDLOOP.

    LOOP AT tg_zib_nfe_forn.

      CLEAR: wl_bapibranch, vl_state_insc, tg_zib_nfe_dist_ter, tg_zib_cte_dist_ter, tg_dados_filial.

      READ TABLE tg_dados_filial WITH KEY bukrs  = tg_zib_nfe_forn-bukrs
                                          branch = tg_zib_nfe_forn-branch.

      IF sy-subrc = 0.
        wl_bapibranch-cgc_number = tg_dados_filial-stcd1.
        vl_state_insc            = tg_dados_filial-state_insc.
      ENDIF.

      CASE tg_zib_nfe_forn-nu_chave_modelo.
        WHEN '55'.
          CLEAR tg_zib_nfe_dist_ter_aux.
          READ TABLE tg_zib_nfe_dist_ter_aux WITH KEY chave_nfe = tg_zib_nfe_forn-nu_chave BINARY SEARCH.
          "
          tg_zib_nfe_dist_ter-forne_cnpj    =  tg_zib_nfe_forn-nu_chave_cnpj.
          tg_zib_nfe_dist_ter-model         =  tg_zib_nfe_forn-nu_chave_modelo.
          tg_zib_nfe_dist_ter-serie         =  tg_zib_nfe_forn-nu_chave_serie.
          tg_zib_nfe_dist_ter-numero        =  tg_zib_nfe_forn-nu_chave_numero.
          tg_zib_nfe_dist_ter-dt_emissao    =  tg_zib_nfe_forn-dt_emissao.
          tg_zib_nfe_dist_ter-chave_nfe     =  tg_zib_nfe_forn-nu_chave.
          tg_zib_nfe_dist_ter-forne_ie      =  tg_zib_nfe_forn-nu_ie.
          tg_zib_nfe_dist_ter-bukrs         =  tg_zib_nfe_forn-bukrs.
          tg_zib_nfe_dist_ter-branch        =  tg_zib_nfe_forn-branch.
          tg_zib_nfe_dist_ter-destino_cnpj  =  wl_bapibranch-cgc_number.
          tg_zib_nfe_dist_ter-destino_ie    =  vl_state_insc.
          tg_zib_nfe_dist_ter-vl_total      =  tg_zib_nfe_dist_ter_aux-vl_total.
          tg_zib_nfe_dist_ter-vl_icms_total =  tg_zib_nfe_dist_ter_aux-vl_icms_total.

          IF tg_zib_nfe_forn-st_nota = '2'. "Cancelado
            tg_zib_nfe_dist_ter-cancel = 'X'.
          ENDIF.
          APPEND tg_zib_nfe_dist_ter.

        WHEN '57'.
          CLEAR tg_zib_cte_dist_ter_aux.
          READ TABLE tg_zib_cte_dist_ter_aux  WITH KEY cd_chave_cte  = tg_zib_nfe_forn-nu_chave BINARY SEARCH.
          tg_zib_cte_dist_ter-emit_cnpj     =  tg_zib_nfe_forn-nu_chave_cnpj.
          tg_zib_cte_dist_ter-modelo        =  tg_zib_nfe_forn-nu_chave_modelo.
          tg_zib_cte_dist_ter-numr_serie    =  tg_zib_nfe_forn-nu_chave_serie.
          tg_zib_cte_dist_ter-numr_cte      =  tg_zib_nfe_forn-nu_chave_numero.
          tg_zib_cte_dist_ter-dt_emissao    =  tg_zib_nfe_forn-dt_emissao.
          tg_zib_cte_dist_ter-cd_chave_cte  =  tg_zib_nfe_forn-nu_chave.
          tg_zib_cte_dist_ter-emit_ie       =  tg_zib_nfe_forn-nu_ie.
          tg_zib_cte_dist_ter-e_tomadora    =  tg_zib_nfe_forn-bukrs.
          tg_zib_cte_dist_ter-f_tomadora    =  tg_zib_nfe_forn-branch.
          tg_zib_cte_dist_ter-valor_prestacao  =  tg_zib_cte_dist_ter_aux-valor_prestacao.
          tg_zib_cte_dist_ter-valor_icms    =  tg_zib_cte_dist_ter_aux-valor_icms .

          IF tg_zib_nfe_forn-st_nota = '2'. "Cancelado
            tg_zib_cte_dist_ter-cancel = 'X'.
          ENDIF.
          APPEND tg_zib_cte_dist_ter.

      ENDCASE.
    ENDLOOP.

  ENDIF.

  CASE p_modelo.
    WHEN '55'.
      IF lines( tg_zib_nfe_dist_ter[] ) = 0.
        MESSAGE s000(z01) WITH 'Nenhum registro encontrado!'.
        STOP.
      ENDIF.
    WHEN '57'.
      IF lines( tg_zib_cte_dist_ter[] ) = 0.
        MESSAGE s000(z01) WITH 'Nenhum registro encontrado!'.
        STOP.
      ENDIF.
    WHEN OTHERS.
      IF ( lines( tg_zib_nfe_dist_ter[] ) = 0 ) AND
         ( lines( tg_zib_cte_dist_ter[] ) = 0 ).
        MESSAGE s000(z01) WITH 'Nenhum registro encontrado!'.
        STOP.
      ENDIF.
  ENDCASE.

  "Busca NF-e
  LOOP AT tg_zib_nfe_dist_ter.

    tg_zib_nfe_dist_ter-stcd1 = tg_zib_nfe_dist_ter-forne_cnpj.

    PERFORM f_tratar_ie USING tg_zib_nfe_dist_ter-forne_ie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tg_zib_nfe_dist_ter-numero
      IMPORTING
        output = tg_zib_nfe_dist_ter-numero_aux.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tg_zib_nfe_dist_ter-serie
      IMPORTING
        output = tg_zib_nfe_dist_ter-serie_aux.

    CONCATENATE tg_zib_nfe_dist_ter-numero_aux '-' tg_zib_nfe_dist_ter-serie_aux
           INTO tg_zib_nfe_dist_ter-ref_doc_no.

    MODIFY tg_zib_nfe_dist_ter.
  ENDLOOP.

  "Busca CT-e
  LOOP AT tg_zib_cte_dist_ter.

    tg_zib_cte_dist_ter-stcd1 = tg_zib_cte_dist_ter-emit_cnpj.

    PERFORM f_tratar_ie USING tg_zib_cte_dist_ter-emit_ie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tg_zib_cte_dist_ter-numr_cte
      IMPORTING
        output = tg_zib_cte_dist_ter-numr_cte_aux.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tg_zib_cte_dist_ter-numr_serie
      IMPORTING
        output = tg_zib_cte_dist_ter-numr_serie_aux.

    CONCATENATE tg_zib_cte_dist_ter-numr_cte '-' tg_zib_cte_dist_ter-numr_serie_aux
           INTO tg_zib_cte_dist_ter-ref_doc_no.

    MODIFY tg_zib_cte_dist_ter.
  ENDLOOP.

  IF tg_zib_nfe_dist_ter[] IS NOT INITIAL.

    "Busca de Fornecedores
    SELECT name1 stcd1 lifnr stcd3 stcd2
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE tg_lfa1
      FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
    WHERE stcd1 EQ tg_zib_nfe_dist_ter-stcd1.

    "Get Fornecedor por IE - Ini
    CLEAR: tg_lfa1_aux[].
    LOOP AT tg_zib_nfe_dist_ter WHERE ( ( serie GE '890' AND serie LE '899' ) OR
                                        ( serie GE '900' AND serie LE '999' ) )
                                  AND forne_ie IS NOT INITIAL.
      CLEAR: tg_lfa1.

      tg_lfa1-stcd3 = tg_zib_nfe_dist_ter-forne_ie.

      READ TABLE tg_lfa1_aux WITH KEY stcd3 = tg_lfa1-stcd3.

      IF sy-subrc EQ 0.
        tg_zib_nfe_dist_ter-lifnr = tg_lfa1_aux-lifnr.
        MODIFY tg_zib_nfe_dist_ter.
        CONTINUE.
      ENDIF.

      TRY.
          zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro_ie( i_insc_estatual = tg_lfa1-stcd3
          )->get_id_parceiro( IMPORTING e_parceiro  = tg_lfa1-lifnr ).
          APPEND tg_lfa1 TO tg_lfa1_aux.

          tg_zib_nfe_dist_ter-lifnr = tg_lfa1-lifnr.
          MODIFY tg_zib_nfe_dist_ter.

        CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
      ENDTRY.
    ENDLOOP.

    IF tg_lfa1_aux[] IS NOT INITIAL.
      SELECT name1 stcd1 lifnr stcd3 stcd2
        FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE tg_lfa1
         FOR ALL ENTRIES IN tg_lfa1_aux
       WHERE lifnr EQ tg_lfa1_aux-lifnr.
    ENDIF.
    "Get Fornecedor por IE - Fim

    "Busca Manifestos
    SELECT *
      FROM zsdt0127 INTO TABLE tg_zsdt0127
      FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE chave = tg_zib_nfe_dist_ter-chave_nfe.
    SORT  tg_zsdt0127 BY dt_atualizado hr_atualizado.

    tg_zsdt0127_aut[] = tg_zsdt0127[].
**======================================================= Atendimento IR163416 / Para atender fiscal ( João Haagsma ) para mostrar o status mesmo não estando autorizado.
*    DELETE tg_zsdt0127_aut WHERE autorizado EQ abap_false.
**=======================================================
  ENDIF.

  IF tg_zib_cte_dist_ter[] IS NOT INITIAL.

    "Busca de Fornecedores
    SELECT name1 stcd1 lifnr stcd3
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE tg_lfa1
      FOR ALL ENTRIES IN tg_zib_cte_dist_ter
    WHERE stcd1 EQ tg_zib_cte_dist_ter-stcd1.
  ENDIF.

  "Ajustar IE Fornecedores
  LOOP AT tg_lfa1.
    PERFORM f_tratar_ie USING tg_lfa1-stcd3.
    MODIFY tg_lfa1.
  ENDLOOP.
**----> CS1111038 - IR142508 --->
  SORT tg_lfa1 BY stcd1 stcd3 lifnr ASCENDING.
**<---- CS1111038 - IR142508 <---

  LOOP AT tg_zib_nfe_dist_ter.

    READ TABLE tg_lfa1 WITH KEY stcd1 = tg_zib_nfe_dist_ter-stcd1
**----> CS1111038 - IR142508 --->
                                 stcd3 = tg_zib_nfe_dist_ter-forne_ie.
*                                stcd3 = tg_zib_nfe_dist_ter-forne_ie BINARY SEARCH.
**<---- CS1111038 - IR142508 <---

    IF sy-subrc = 0.
      tg_zib_nfe_dist_ter-lifnr =  tg_lfa1-lifnr.
    ENDIF.

    MODIFY tg_zib_nfe_dist_ter.
  ENDLOOP.

  LOOP AT tg_zib_cte_dist_ter.
    READ TABLE tg_lfa1 WITH KEY stcd1 = tg_zib_cte_dist_ter-stcd1
**----> CS1111038 - IR142508 --->
                                stcd3 = tg_zib_cte_dist_ter-emit_ie.
*                               stcd3 = tg_zib_cte_dist_ter-emit_ie BINARY SEARCH.
**<---- CS1111038 - IR142508 <---

    IF sy-subrc = 0.
      tg_zib_cte_dist_ter-lifnr =  tg_lfa1-lifnr.
    ENDIF.

    MODIFY tg_zib_cte_dist_ter.
  ENDLOOP.

  "Busca Historico Pedido para NF-e/Fornecedor
  IF tg_zib_nfe_dist_ter[] IS NOT INITIAL.

*    SELECT EBELN BELNR GJAHR VGABE XBLNR
*      FROM EKBE APPENDING CORRESPONDING FIELDS OF TABLE TG_EKBE
*       FOR ALL ENTRIES IN TG_ZIB_NFE_DIST_TER
*     WHERE XBLNR EQ TG_ZIB_NFE_DIST_TER-REF_DOC_NO
*       AND VGABE IN ('1','2'). "Migo/Miro
*
*    TG_EKBE_AUX[] = TG_EKBE[].
*
*    SORT TG_EKBE_AUX BY EBELN.
*    DELETE ADJACENT DUPLICATES FROM TG_EKBE_AUX COMPARING EBELN.
*
*    SELECT EBELN LIFNR
*      FROM EKKO INTO CORRESPONDING FIELDS OF TABLE TG_EKKO
*       FOR ALL ENTRIES IN TG_EKBE_AUX
*     WHERE EBELN = TG_EKBE_AUX-EBELN.
*
*    SORT TG_ZIB_NFE_DIST_TER BY LIFNR REF_DOC_NO.
*    SORT TG_EKKO BY EBELN.
*
*    LOOP AT TG_EKBE.
*      READ TABLE TG_EKKO WITH KEY EBELN = TG_EKBE-EBELN BINARY SEARCH.
*      CHECK SY-SUBRC = 0.
*
*      READ TABLE TG_ZIB_NFE_DIST_TER WITH KEY LIFNR      = TG_EKKO-LIFNR
*                                              REF_DOC_NO = TG_EKBE-XBLNR BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        DELETE TG_EKBE.
*      ELSE.
*        TG_EKBE-LIFNR = TG_EKKO-LIFNR.
*        MODIFY TG_EKBE.
*      ENDIF.
*
*    ENDLOOP.

    SELECT a~ebeln a~belnr a~gjahr a~vgabe a~xblnr b~lifnr a~shkzg b~zterm
      APPENDING CORRESPONDING FIELDS OF TABLE tg_ekbe
      FROM ekbe AS a INNER JOIN ekko AS b ON a~ebeln = b~ebeln
      FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE a~xblnr EQ tg_zib_nfe_dist_ter-ref_doc_no
       AND a~vgabe IN ('1','2') "Migo/Miro
       AND b~lifnr EQ tg_zib_nfe_dist_ter-lifnr
      AND a~shkzg <> 'H'.

* INICIO - RRIBEIRO - IR247383 - 30.07.2025 - STEFANINI
      IF tg_ekbe IS INITIAL.
        SELECT a~ebeln a~belnr a~gjahr a~vgabe a~xblnr c~lifn2 a~shkzg b~zterm
          APPENDING CORRESPONDING FIELDS OF TABLE tg_ekbe
          FROM ekbe AS a INNER JOIN ekko AS b ON a~ebeln = b~ebeln
          INNER JOIN ekpa AS c ON c~ebeln = a~ebeln
          FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
         WHERE a~xblnr EQ tg_zib_nfe_dist_ter-ref_doc_no
           AND a~vgabe IN ('1','2') "Migo/Miro
           AND c~lifn2 EQ tg_zib_nfe_dist_ter-lifnr
          AND a~shkzg <> 'H'.
      ENDIF.
* FIM - RRIBEIRO - IR247383 - 30.07.2025 - STEFANINI
  ENDIF.

  "Busca Historico Pedido para CT-e/Fornecedor
  IF tg_zib_cte_dist_ter[] IS NOT INITIAL.

*    SELECT EBELN BELNR GJAHR VGABE XBLNR
*      FROM EKBE APPENDING CORRESPONDING FIELDS OF TABLE TG_EKBE
*       FOR ALL ENTRIES IN TG_ZIB_CTE_DIST_TER
*     WHERE XBLNR EQ TG_ZIB_CTE_DIST_TER-REF_DOC_NO
*       AND VGABE IN ('1','2'). "Migo/Miro
*
*    TG_EKBE_AUX[] = TG_EKBE[].
*
*    SORT TG_EKBE_AUX BY EBELN.
*    DELETE ADJACENT DUPLICATES FROM TG_EKBE_AUX COMPARING EBELN.
*
*    SELECT EBELN LIFNR
*      FROM EKKO INTO CORRESPONDING FIELDS OF TABLE TG_EKKO
*       FOR ALL ENTRIES IN TG_EKBE_AUX
*     WHERE EBELN = TG_EKBE_AUX-EBELN.
*
*    SORT TG_ZIB_CTE_DIST_TER BY LIFNR REF_DOC_NO.
*    SORT TG_EKKO BY EBELN.
*
*    LOOP AT TG_EKBE.
*      READ TABLE TG_EKKO WITH KEY EBELN = TG_EKBE-EBELN BINARY SEARCH.
*      CHECK SY-SUBRC = 0.
*
*      READ TABLE TG_ZIB_CTE_DIST_TER WITH KEY LIFNR      = TG_EKKO-LIFNR
*                                              REF_DOC_NO = TG_EKBE-XBLNR BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        DELETE TG_EKBE.
*      ELSE.
*        TG_EKBE-LIFNR = TG_EKKO-LIFNR.
*        MODIFY TG_EKBE.
*      ENDIF.
*    ENDLOOP.

    SELECT a~ebeln a~belnr a~gjahr a~vgabe a~xblnr b~lifnr a~shkzg b~zterm
      APPENDING CORRESPONDING FIELDS OF TABLE tg_ekbe
      FROM ekbe AS a INNER JOIN ekko AS b ON a~ebeln = b~ebeln
      FOR ALL ENTRIES IN tg_zib_cte_dist_ter
     WHERE a~xblnr EQ tg_zib_cte_dist_ter-ref_doc_no
       AND a~vgabe IN ('1','2') "Migo/Miro
       AND b~lifnr EQ tg_zib_cte_dist_ter-lifnr
      AND a~shkzg <> 'H'.
  ENDIF.

  "Busca de Miro/Migo por Historico de Pedido.
  IF tg_ekbe[] IS NOT INITIAL.
    "Migo
    SELECT b~mblnr b~mjahr b~budat
      FROM mkpf AS b APPENDING CORRESPONDING FIELDS OF TABLE tg_mkpf
       FOR ALL ENTRIES IN tg_ekbe
     WHERE mblnr EQ tg_ekbe-belnr
       AND mjahr EQ tg_ekbe-gjahr
       AND NOT EXISTS ( SELECT *
                          FROM mseg AS a
                         WHERE a~smbln = b~mblnr
                           AND a~sjahr = b~mjahr ).

    "Miro
    SELECT bukrs belnr gjahr budat lifnr xblnr zterm stblg
      FROM rbkp APPENDING CORRESPONDING FIELDS OF TABLE tg_rbkp
       FOR ALL ENTRIES IN tg_ekbe
     WHERE belnr EQ tg_ekbe-belnr
       AND gjahr EQ tg_ekbe-gjahr
       AND stblg EQ ''.           "*-CS2022000243-#76365-26.04.2022-JT-inicio
  ENDIF.

  "Busca Lctos Miros Direto / NF-e
  IF tg_zib_nfe_dist_ter[] IS NOT INITIAL.
    SELECT bukrs belnr gjahr budat lifnr xblnr zterm stblg
      FROM rbkp APPENDING CORRESPONDING FIELDS OF TABLE tg_rbkp
       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE xblnr EQ tg_zib_nfe_dist_ter-ref_doc_no
       AND lifnr EQ tg_zib_nfe_dist_ter-lifnr
       AND bukrs EQ tg_zib_nfe_dist_ter-bukrs  "*-CS2022000243-#76365-26.04.2022-JT-inicio
       AND rbstat NE '2'                       "*-IR124749-26.01.2023-RIM-SKM-inclusão
       AND stblg EQ ''.
  ENDIF.

  "Busca Lctos Miros Direto / CT-e
  IF tg_zib_cte_dist_ter[] IS NOT INITIAL.
    SELECT bukrs belnr gjahr budat lifnr xblnr zterm stblg
      FROM rbkp APPENDING CORRESPONDING FIELDS OF TABLE tg_rbkp
       FOR ALL ENTRIES IN tg_zib_cte_dist_ter
     WHERE xblnr EQ tg_zib_cte_dist_ter-ref_doc_no
       AND lifnr EQ tg_zib_cte_dist_ter-lifnr
       AND bukrs EQ tg_zib_cte_dist_ter-e_tomadora
       AND stblg EQ ''.  "*-CS2022000243-#76365-26.04.2022-JT-inicio
  ENDIF.

  SORT tg_rbkp BY belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM tg_rbkp COMPARING belnr gjahr.

  "Monta Refkey para Link com Doc. Fiscal
  LOOP AT tg_rbkp.
    CONCATENATE  tg_rbkp-belnr tg_rbkp-gjahr INTO tg_rbkp-refkey.
    MODIFY tg_rbkp.
  ENDLOOP.

  "Busca Doc. Fiscal Miro.
  IF tg_rbkp[] IS NOT INITIAL.
    SELECT a~docnum a~nftype a~doctyp a~direct a~docdat a~cancel a~nfenum
           a~series a~parid a~pstdat b~refkey a~model a~branch a~candat
      INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
      FROM j_1bnfdoc AS a INNER JOIN j_1bnflin AS b ON a~docnum = b~docnum
       FOR ALL ENTRIES IN tg_rbkp
     WHERE b~refkey EQ tg_rbkp-refkey.
  ENDIF.

  "Busca Lctos Fiscal Direto - NF-e
  IF tg_zib_nfe_dist_ter[] IS NOT INITIAL.

*Begin - RMNI - IR107676/CS1017067 - HFURUKAWA - 28/11/2022 - ZSDT0110 Erro NF-e X NFS-e
*Regra para checar se a nota fiscal selecionada pela MIRO é a mesma da tabela tg_zib_nfe_dist_ter
    LOOP AT tg_j_1bnfdoc INTO DATA(wa_doc_aux).
      DATA(lv_index) = sy-tabix.
      "Confere filial, número da NF-e, modelo e parceiro
      READ TABLE tg_zib_nfe_dist_ter TRANSPORTING NO FIELDS WITH KEY branch = wa_doc_aux-branch
                                                                     numero = wa_doc_aux-nfenum
                                                                     model  = wa_doc_aux-model
                                                                     lifnr  = wa_doc_aux-parid.
      "Caso não exista, desconsidera desde a MIRO
      IF sy-subrc <> 0.
        DELETE tg_rbkp WHERE refkey = wa_doc_aux-refkey.
        DELETE tg_j_1bnfdoc INDEX lv_index.
      ENDIF.
    ENDLOOP.
*End - RMNI - IR107676/CS1017067 - HFURUKAWA - 28/11/2022 - ZSDT0110 Erro NF-e X NFS-e

    SELECT docnum model nfnum9 serie stcd1 direct partyp bukrs branch
      FROM j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
     WHERE stcd1   EQ tg_zib_nfe_dist_ter-forne_cnpj
       AND model   EQ tg_zib_nfe_dist_ter-model
       AND serie   EQ tg_zib_nfe_dist_ter-serie
       AND nfnum9  EQ tg_zib_nfe_dist_ter-numero
       AND direct  EQ '1'
       AND partyp  EQ 'B'
       AND branch  EQ tg_zib_nfe_dist_ter-branch.

    IF ( sy-subrc = 0 ) AND ( tg_j_1bnfe_active[] IS NOT INITIAL ).
      SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
        FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
         FOR ALL ENTRIES IN tg_j_1bnfe_active
       WHERE docnum EQ tg_j_1bnfe_active-docnum.
    ENDIF.

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE nfenum  EQ tg_zib_nfe_dist_ter-numero
       AND parid   EQ tg_zib_nfe_dist_ter-lifnr
       AND branch  EQ tg_zib_nfe_dist_ter-branch
       AND model   EQ tg_zib_nfe_dist_ter-model
       AND series  EQ tg_zib_nfe_dist_ter-serie_aux
       AND direct  EQ '1'
       AND doctyp  NE '5'.

*BOC dgalvao CS0963581 - 16/02/2022
*    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
*      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
*       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
*     WHERE ( ( nfenum  EQ tg_zib_nfe_dist_ter-numero     ) OR
*             ( nfenum  EQ tg_zib_nfe_dist_ter-numero_aux ) )
*       AND parid   EQ tg_zib_nfe_dist_ter-lifnr
*       AND branch  EQ tg_zib_nfe_dist_ter-branch
*       AND model   EQ tg_zib_nfe_dist_ter-model
*       AND ( ( series  EQ tg_zib_nfe_dist_ter-serie      ) OR
*             ( series  EQ tg_zib_nfe_dist_ter-serie_aux  ) )
*       AND direct  EQ '1'
*       AND doctyp  NE '5'.

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
          FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
           FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
         WHERE nfenum  EQ tg_zib_nfe_dist_ter-numero_aux
           AND parid   EQ tg_zib_nfe_dist_ter-lifnr
           AND branch  EQ tg_zib_nfe_dist_ter-branch
           AND model   EQ tg_zib_nfe_dist_ter-model
           AND series  EQ tg_zib_nfe_dist_ter-serie
           AND direct  EQ '1'
           AND doctyp  NE '5'.


    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
          FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
           FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
         WHERE nfenum  EQ tg_zib_nfe_dist_ter-numero_aux
           AND parid   EQ tg_zib_nfe_dist_ter-lifnr
           AND branch  EQ tg_zib_nfe_dist_ter-branch
           AND model   EQ tg_zib_nfe_dist_ter-model
           AND series  EQ tg_zib_nfe_dist_ter-serie_aux
           AND direct  EQ '1'
           AND doctyp  NE '5'.

*EOC dgalvao CS0963581 - 16/02/2022

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE nfenum  EQ tg_zib_nfe_dist_ter-numero
       AND parid   EQ tg_zib_nfe_dist_ter-lifnr
       AND branch  EQ tg_zib_nfe_dist_ter-branch
       AND model   EQ tg_zib_nfe_dist_ter-model
       AND series  EQ tg_zib_nfe_dist_ter-serie
       AND direct  EQ '1'
       AND doctyp  NE '5'.

  ENDIF.

  "Busca Lctos Fiscal Direto - CT-e
  IF tg_zib_cte_dist_ter[] IS NOT INITIAL.

    SELECT docnum model nfnum9 serie stcd1 direct partyp bukrs branch
      FROM j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
     WHERE stcd1   EQ tg_zib_cte_dist_ter-emit_cnpj
       AND model   EQ tg_zib_cte_dist_ter-modelo
       AND serie   EQ tg_zib_cte_dist_ter-numr_serie
       AND nfnum9  EQ tg_zib_cte_dist_ter-numr_cte
       AND direct  EQ '1'
       AND partyp  EQ 'B'
       AND branch  EQ tg_zib_cte_dist_ter-f_tomadora.

    IF ( sy-subrc = 0 ) AND ( tg_j_1bnfe_active[] IS NOT INITIAL ).
      SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
        FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
         FOR ALL ENTRIES IN tg_j_1bnfe_active
       WHERE docnum EQ tg_j_1bnfe_active-docnum.
    ENDIF.

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN tg_zib_cte_dist_ter
     WHERE nfenum  EQ tg_zib_cte_dist_ter-numr_cte
       AND parid   EQ tg_zib_cte_dist_ter-lifnr
       AND branch  EQ tg_zib_cte_dist_ter-f_tomadora
       AND model   EQ tg_zib_cte_dist_ter-modelo
       AND series  EQ tg_zib_cte_dist_ter-numr_serie_aux
       AND direct  EQ '1'
       AND doctyp  NE '5'.

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN tg_zib_cte_dist_ter
     WHERE nfenum  EQ tg_zib_cte_dist_ter-numr_cte
       AND parid   EQ tg_zib_cte_dist_ter-lifnr
       AND branch  EQ tg_zib_cte_dist_ter-f_tomadora
       AND model   EQ tg_zib_cte_dist_ter-modelo
       AND series  EQ tg_zib_cte_dist_ter-numr_serie
       AND direct  EQ '1'
       AND doctyp  NE '5'.
  ENDIF.

  IF tg_zib_nfe_dist_ter[] IS NOT INITIAL.

    SELECT *
      FROM zib_nfe_dist_itm INTO CORRESPONDING FIELDS OF TABLE tg_zib_nfe_dist_itm
       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE chave_nfe = tg_zib_nfe_dist_ter-chave_nfe.

    SELECT *
      FROM zib_nfe_forn INTO CORRESPONDING FIELDS OF TABLE it_zib_nfe_forn
       FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
     WHERE nu_chave = tg_zib_nfe_dist_ter-chave_nfe.

    SELECT docnum nftype doctyp direct docdat cancel nfenum series parid pstdat model branch candat
      FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN it_zib_nfe_forn
      WHERE docnum  EQ it_zib_nfe_forn-docnum
       AND doctyp  NE '5'.

  ENDIF.

  DELETE tg_j_1bnfdoc WHERE candat IS NOT INITIAL.

*** PBI - 72546 - Inicio - CBRAND
  IF tg_j_1bnfdoc[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfstx INTO TABLE tg_j_1bnfstx
    FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum = tg_j_1bnfdoc-docnum
      AND taxgrp IN ( 'ICMS', 'IPI' ).
  ENDIF.
*** PBI - 72546 - Fim - CBRAND

  "SORT TG_J_1BNFDOC BY DOCNUM.
  "DELETE ADJACENT DUPLICATES FROM TG_J_1BNFDOC COMPARING DOCNUM.



  "Buscar notas de referencia.
  SELECT * FROM zib_nfe_dist_ref
  INTO TABLE tg_zib_nfe_dist_ref
  FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
  WHERE chave_nfe EQ tg_zib_nfe_dist_ter-chave_nfe.

  IF tg_zib_nfe_dist_ter IS NOT INITIAL.

    SELECT *
    FROM zib_nfe_forn
    INTO CORRESPONDING FIELDS OF TABLE tg_zib_nfe_f
    FOR ALL ENTRIES IN tg_zib_nfe_dist_ter
    WHERE nu_chave   EQ tg_zib_nfe_dist_ter-chave_nfe.
  ENDIF.

  IF tg_zib_cte_dist_ter IS NOT INITIAL.
    SELECT *
    FROM zib_nfe_forn
    APPENDING CORRESPONDING FIELDS OF TABLE tg_zib_nfe_f
    FOR ALL ENTRIES IN tg_zib_cte_dist_ter
    WHERE nu_chave   EQ tg_zib_cte_dist_ter-cd_chave_cte.
  ENDIF.

ENDFORM.

FORM f_selecionar_manifestos USING p_saida_0100 TYPE ty_saida_0100.

  DATA: tg_zsdt0127_doc  TYPE TABLE OF zsdt0127   WITH HEADER LINE.

  CLEAR: it_saida_0105[], tg_zsdt0127_doc[].

  SELECT *
    FROM zsdt0127 INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0127_doc
   WHERE chave EQ p_saida_0100-chave.

  SORT tg_zsdt0127_doc BY doc_manifesto.

  LOOP AT tg_zsdt0127_doc.
    CLEAR: wa_saida_0105.
    MOVE-CORRESPONDING tg_zsdt0127_doc TO wa_saida_0105.

    PERFORM f_get_ds_manifesto USING tg_zsdt0127_doc-cd_operacao
                            CHANGING wa_saida_0105-ds_operacao.

    APPEND wa_saida_0105 TO it_saida_0105.
  ENDLOOP.


ENDFORM.

FORM f_processar_dados .

  DATA: vl_doc_manisfesto TYPE zsdt0127-doc_manifesto,
        vl_lifnr          TYPE ty_saida_0100-lifnr,
        vl_branch         TYPE ty_saida_0100-branch,
        vl_check_op       TYPE c,
        vl_icms           TYPE j_1bnfstx-taxval,
        vl_ipi            TYPE j_1bnfstx-taxval,
        w_zsdt0127        TYPE zsdt0127,
        v_cod             TYPE zsdt0127-code,
        w_fiscal          TYPE zsdt0127_fiscal.

  SORT: tg_lfa1         BY lifnr,
        tg_zsdt0127     BY doc_manifesto,
        tg_zsdt0127_aut BY dt_atualizado hr_atualizado,
        tg_j_1bnfstx    BY docnum.

  LOOP AT tg_zib_nfe_dist_ter.

    CLEAR: wa_saida_0100, tg_lfa1, vl_doc_manisfesto, tg_zib_nfe_f.

    wa_saida_0100-chave           =  tg_zib_nfe_dist_ter-chave_nfe.
    wa_saida_0100-bukrs           =  tg_zib_nfe_dist_ter-bukrs.
    wa_saida_0100-branch          =  tg_zib_nfe_dist_ter-branch.
    wa_saida_0100-numero          =  tg_zib_nfe_dist_ter-numero.
    wa_saida_0100-serie           =  tg_zib_nfe_dist_ter-serie.
    wa_saida_0100-dt_emissao      =  tg_zib_nfe_dist_ter-dt_emissao.
    wa_saida_0100-model           =  tg_zib_nfe_dist_ter-model.
    wa_saida_0100-cancel          =  tg_zib_nfe_dist_ter-cancel.
    wa_saida_0100-vl_total        =  tg_zib_nfe_dist_ter-vl_total.
    wa_saida_0100-dest_cnpj       =  tg_zib_nfe_dist_ter-destino_cnpj.
    wa_saida_0100-dest_ie         =  tg_zib_nfe_dist_ter-destino_ie.
    wa_saida_0100-dt_vencimento   =  tg_zib_nfe_dist_ter-dt_vencimento.
*    wa_saida_0100-ctr_zterm       =  tg_zib_nfe_dist_ter-ctr_zterm.
*
*    CLEAR: wa_saida_0100-text1.
*    SELECT SINGLE text1
*      FROM t052u INTO wa_saida_0100-text1
*      WHERE zterm EQ wa_saida_0100-zterm
*      AND spras EQ sy-langu
*      AND ztagg EQ space.


    LOOP AT tg_zib_nfe_dist_itm WHERE chave_nfe = wa_saida_0100-chave.

      IF tg_zib_nfe_dist_itm-icms_aqt IS NOT INITIAL.
        wa_saida_0100-rate = tg_zib_nfe_dist_itm-icms_aqt.
      ENDIF.

      IF tg_zib_nfe_dist_itm-ipi_aqt IS NOT INITIAL.
        wa_saida_0100-rate_ipi = tg_zib_nfe_dist_itm-ipi_aqt.
      ENDIF.


**********************************************************************
* CS2023000254 ADICIONAR COLUNA XPED NA ZSDT0110 - PSA
**********************************************************************
      IF tg_zib_nfe_dist_itm-prod_pedido_comp IS NOT INITIAL.
        wa_saida_0100-pedido = tg_zib_nfe_dist_itm-prod_pedido_comp.
      ENDIF.


      IF wa_saida_0100-cfop IS INITIAL.
        wa_saida_0100-cfop = tg_zib_nfe_dist_itm-prod_cfop.
      ELSE.
        CONCATENATE wa_saida_0100-cfop ',' tg_zib_nfe_dist_itm-prod_cfop
               INTO wa_saida_0100-cfop.
      ENDIF.
    ENDLOOP.

    "Busca Migo
    SORT tg_ekbe BY belnr ASCENDING.
    LOOP AT tg_ekbe WHERE xblnr EQ tg_zib_nfe_dist_ter-ref_doc_no
                      AND vgabe = '1'
                      AND lifnr = tg_zib_nfe_dist_ter-lifnr
                      AND shkzg <> 'H'.

      wa_saida_0100-zterm    =  tg_ekbe-zterm.

**********************************************************************
* CS2023000254 ADICIONAR COLUNA XPED NA ZSDT0110 - PSA
**********************************************************************
      IF wa_saida_0100-pedido IS INITIAL.
        wa_saida_0100-pedido    =  tg_ekbe-ebeln.
      ENDIF.

**********************************************************************

      CLEAR: wa_saida_0100-text1.
      SELECT SINGLE text1
        FROM t052u INTO wa_saida_0100-text1
        WHERE zterm EQ wa_saida_0100-zterm
        AND spras EQ sy-langu
        AND ztagg EQ space.

      READ TABLE tg_mkpf WITH KEY mblnr = tg_ekbe-belnr
                                  mjahr = tg_ekbe-gjahr.
      IF sy-subrc = 0.
        wa_saida_0100-migo     =  tg_mkpf-mblnr.
        wa_saida_0100-dt_migo  =  tg_mkpf-budat.
        wa_saida_0100-gj_migo  =  tg_mkpf-mjahr.



      ENDIF.

    ENDLOOP.

    "Busca Miro
    READ TABLE tg_ekbe WITH KEY xblnr = tg_zib_nfe_dist_ter-ref_doc_no
                                vgabe = '2'
                                lifnr = tg_zib_nfe_dist_ter-lifnr.
    IF sy-subrc = 0.
      "108593 - CS2023000254 ADICIONAR COLUNA XPED NA ZSDT0110
      wa_saida_0100-ebeln     =  tg_ekbe-ebeln.
      IF tg_ekbe-shkzg <> 'H'.
        READ TABLE tg_rbkp WITH KEY belnr = tg_ekbe-belnr
                                    gjahr = tg_ekbe-gjahr.
        IF sy-subrc = 0.
          wa_saida_0100-miro     =  tg_rbkp-belnr.
          wa_saida_0100-dt_miro  =  tg_rbkp-budat.
          wa_saida_0100-gj_miro  =  tg_rbkp-gjahr.
          wa_saida_0100-stblg    =  tg_rbkp-stblg. "!*-CS2022000243-#76365-26.04.2022-JT-inicio

*          wa_saida_0100-zterm    =  tg_rbkp-zterm.
*
*          CLEAR: wa_saida_0100-text1.
*          SELECT SINGLE text1
*            FROM t052u INTO wa_saida_0100-text1
*            WHERE zterm EQ wa_saida_0100-zterm
*            AND spras EQ sy-langu
*            AND ztagg EQ space.

          READ TABLE tg_j_1bnfdoc WITH KEY refkey = tg_rbkp-refkey.
          IF sy-subrc = 0.
            wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
            wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
            wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    "Caso não definido miro
    IF ( wa_saida_0100-miro IS INITIAL ).

      "Tenta buscar direto pela Miro
      READ TABLE tg_rbkp WITH KEY xblnr = tg_zib_nfe_dist_ter-ref_doc_no
                                  lifnr = tg_zib_nfe_dist_ter-lifnr
                                  bukrs = tg_zib_nfe_dist_ter-bukrs.

      IF sy-subrc = 0.

        wa_saida_0100-miro     =  tg_rbkp-belnr.
        wa_saida_0100-dt_miro  =  tg_rbkp-budat.
        wa_saida_0100-gj_miro  =  tg_rbkp-gjahr.
        wa_saida_0100-stblg    =  tg_rbkp-stblg. "!*-CS2022000243-#76365-26.04.2022-JT-inicio

*        wa_saida_0100-zterm    =  tg_rbkp-zterm.
*
*        CLEAR: wa_saida_0100-text1.
*        SELECT SINGLE text1
*          FROM t052u INTO wa_saida_0100-text1
*          WHERE zterm EQ wa_saida_0100-zterm
*          AND spras EQ sy-langu
*          AND ztagg EQ space.

        READ TABLE tg_j_1bnfdoc WITH KEY refkey = tg_rbkp-refkey.
        IF sy-subrc = 0.
          wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
          wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
          wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
        ENDIF.

      ELSE. "Tenta buscar pelo Fiscal

        LOOP AT tg_j_1bnfdoc WHERE parid       EQ tg_zib_nfe_dist_ter-lifnr
                               AND branch      EQ tg_zib_nfe_dist_ter-branch
                               AND ( ( nfenum  EQ tg_zib_nfe_dist_ter-numero     ) OR
                                     ( nfenum  EQ tg_zib_nfe_dist_ter-numero_aux ) )
                               AND model       EQ tg_zib_nfe_dist_ter-model
                               AND ( ( series  EQ tg_zib_nfe_dist_ter-serie     ) OR
                                     ( series  EQ tg_zib_nfe_dist_ter-serie_aux ) ).

          wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
          wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
          wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
          EXIT.
        ENDLOOP.

        IF wa_saida_0100-docnum IS INITIAL.
          READ TABLE tg_j_1bnfe_active WITH KEY stcd1   = tg_zib_nfe_dist_ter-forne_cnpj
                                                model   = tg_zib_nfe_dist_ter-model
                                                serie   = tg_zib_nfe_dist_ter-serie
                                                nfnum9  = tg_zib_nfe_dist_ter-numero
                                                branch  = tg_zib_nfe_dist_ter-branch.
          IF sy-subrc = 0.
            READ TABLE tg_j_1bnfdoc WITH KEY docnum = tg_j_1bnfe_active-docnum.
            IF sy-subrc = 0.
              wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
              wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
              wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
            ENDIF.
          ENDIF.

          IF wa_saida_0100-docnum IS INITIAL.
            READ TABLE it_zib_nfe_forn WITH KEY nu_chave = tg_zib_nfe_dist_ter-chave_nfe.
            IF sy-subrc IS INITIAL.
              READ TABLE tg_j_1bnfdoc WITH KEY docnum = it_zib_nfe_forn-docnum.
              IF sy-subrc IS INITIAL.
                wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
                wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
                wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*** PBI - 72503 - CSB- inicio
    READ TABLE it_zib_nfe_forn WITH KEY nu_chave = tg_zib_nfe_dist_ter-chave_nfe.
    wa_saida_0100-docnum_ref = it_zib_nfe_forn-docnum_ref.

    IF wa_saida_0100-docnum_ref IS NOT INITIAL.

      SELECT SINGLE candat FROM j_1bnfdoc
            INTO @DATA(lva_candat)
            WHERE docnum = @it_zib_nfe_forn-docnum_ref.
      IF sy-subrc = 0.
        IF lva_candat IS NOT INITIAL.
          UPDATE zib_nfe_forn SET docnum_ref = ''
               WHERE nu_chave EQ it_zib_nfe_forn-nu_chave.
          wa_saida_0100-docnum_ref = ''.
        ENDIF.
      ENDIF.
    ENDIF.
*** PBI - 72503 - CSB - Fim


*** PBI - 72546 - Inicio - CBRAND
    CLEAR vl_icms.
    LOOP AT tg_j_1bnfstx WHERE docnum  EQ  wa_saida_0100-docnum.
      vl_icms   = vl_icms + tg_j_1bnfstx-taxval.
      IF tg_j_1bnfstx-taxtyp   EQ 'ICM1'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMX'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMO'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMN'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMF'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM4'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM3'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM2'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM1'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM0'.
*        wa_saida_0100-rate = tg_j_1bnfstx-rate.
      ENDIF.
    ENDLOOP.

*    IF vl_icms = 0.
*      wa_saida_0100-taxval  = vl_icms.
*    ELSE.

    wa_saida_0100-taxval  = tg_zib_nfe_dist_ter-vl_icms_total.
    wa_saida_0100-taxval_ipi  = tg_zib_nfe_dist_ter-vl_ipi_total.
*    ENDIF.

*    IF tg_zib_nfe_dist_ter-chave_nfe IS NOT INITIAL.
*      TRY.
*          "Buscar informações do XML NFE / GRC.
*          CLEAR: tg_xml.
*          CALL FUNCTION 'Z_DETALHAMENTO_NFE'
*            EXPORTING
*              i_chave_nfe = tg_zib_nfe_dist_ter-chave_nfe   " Chave de Documento Fiscal Eletrônico
*            IMPORTING
*              e_xml_nfe   = tg_xml   " Estrutura tipo tabela XML NF-e SEFAZ Autorizado
*            EXCEPTIONS
*              no_found    = 1
*              OTHERS      = 2.
*          IF sy-subrc EQ 0.
*            DATA(t_det)  = tg_xml-nfeproc-nfe-infnfe-det[].
*            IF t_det IS NOT INITIAL.
*              LOOP AT t_det INTO DATA(w_det).
*                w_icmstot = w_det-imposto-icms-icms00.
*              ENDLOOP.
*              wa_saida_0100-rate = w_icmstot-picms.
*            ENDIF.

    CASE tg_zib_nfe_dist_ter-cd_tipo_doc.
      WHEN 0.
        wa_saida_0100-tpnf = |0 - Entrada|.
      WHEN 1.
        wa_saida_0100-tpnf = |1 - Saida|.
      WHEN OTHERS.
    ENDCASE.


    CASE tg_zib_nfe_dist_ter-cd_fina_emissao.
      WHEN '1'.
        wa_saida_0100-finnfe  = |1 - NFe normal|.
      WHEN '2'.
        wa_saida_0100-finnfe  = |2 - NFe complementar|.

      WHEN '3'.
        wa_saida_0100-finnfe  = |3 - NFe de ajuste|.

      WHEN '4'.
        wa_saida_0100-finnfe  = |4 - Devolução de mercadoria|.
      WHEN OTHERS.
    ENDCASE.


    "Selecionar notas de referencia.
    LOOP AT tg_zib_nfe_dist_ref WHERE chave_nfe = tg_zib_nfe_dist_ter-chave_nfe.
      IF wa_saida_0100-chave_nfe_ref IS INITIAL.
        wa_saida_0100-chave_nfe_ref = tg_zib_nfe_dist_ref-chave_nfe_ref.
      ELSE.
        wa_saida_0100-chave_nfe_ref = |{ wa_saida_0100-chave_nfe_ref }, { tg_zib_nfe_dist_ref-chave_nfe_ref }| .
      ENDIF.
    ENDLOOP.


*            LOOP AT tg_xml-nfeproc-nfe-infnfe-cobr-dup ASSIGNING FIELD-SYMBOL(<ws_cob>).
*              IF <ws_cob>-dvenc IS NOT INITIAL."Data de vencimento.
*                wa_saida_0100-dt_vencimento = |{ <ws_cob>-dvenc(4) }{ <ws_cob>-dvenc+5(2) }{ <ws_cob>-dvenc+8(2) } |. "2022-03-13
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*        CATCH cx_sy_dyn_call_param_not_found.
*      ENDTRY.
*    ENDIF.

*    wa_saida_0100-taxval  = vl_icms.
*** PBI - 72546 - Fim - CBRAND

    "Filtro Status Lançamento
    IF p_lct1 IS NOT INITIAL. "Lançados
      CHECK  wa_saida_0100-docnum IS NOT INITIAL.
    ENDIF.
    IF p_lct2 IS NOT INITIAL. "Pendentes
      CHECK  wa_saida_0100-docnum IS INITIAL.
    ENDIF.

*    CASE p_st_lct.
*      WHEN '1'. "Lançados
*        CHECK  wa_saida_0100-docnum IS NOT INITIAL.
*      WHEN '2'. "Pendentes
*        CHECK  wa_saida_0100-docnum IS INITIAL.
*    ENDCASE.

    "Busca Ultimo Manifesto Autorizado
    wa_saida_0100-cd_operacao  = '000000'.

    LOOP AT tg_zsdt0127_aut WHERE chave         EQ tg_zib_nfe_dist_ter-chave_nfe
                              AND autorizado    EQ 'X'
                              AND doc_manifesto IS NOT INITIAL.

      vl_doc_manisfesto          = tg_zsdt0127_aut-doc_manifesto.
      wa_saida_0100-cd_operacao  = tg_zsdt0127_aut-cd_operacao.
    ENDLOOP.

    PERFORM f_get_ds_manifesto USING wa_saida_0100-cd_operacao
                            CHANGING wa_saida_0100-ds_operacao.

* ----> CS1102269 / IR140174 --->
    IF wa_saida_0100-cd_operacao  = '000000'.

      SELECT SINGLE * FROM zsdt0127_fiscal INTO w_fiscal
         WHERE chave EQ tg_zib_nfe_dist_ter-chave_nfe
         AND atualiza EQ 'X'.

      IF sy-subrc EQ 0.
        wa_saida_0100-cd_operacao = w_fiscal-cd_operacao.
        PERFORM f_get_ds_manifesto USING wa_saida_0100-cd_operacao
                          CHANGING wa_saida_0100-ds_operacao.
      ENDIF.
    ENDIF.
* <---- CS1102269 / IR140174 <---

    "Filtro Status Manifesto
*    IF p_st_mnf IS NOT INITIAL.
*      CHECK wa_saida_0100-cd_operacao = p_st_mnf.
*    ENDIF.

    IF r_st_mnf IS NOT INITIAL.
      LOOP AT r_st_mnf.
        CLEAR vl_check_op.
        IF wa_saida_0100-cd_operacao = r_st_mnf-low.
          vl_check_op = abap_true.
        ENDIF.
      ENDLOOP.

      CHECK vl_check_op = abap_true.
    ENDIF.


**----> CS1111038 - IR142508 --->
*    READ TABLE tg_lfa1 WITH KEY lifnr = tg_zib_nfe_dist_ter-lifnr BINARY SEARCH.
    READ TABLE tg_lfa1 WITH KEY lifnr = tg_zib_nfe_dist_ter-lifnr.
**<---- CS1111038 - IR142508 <---

    IF ( sy-subrc EQ 0 ).
      wa_saida_0100-name_forn   = tg_lfa1-name1.
      wa_saida_0100-forne_cnpj  = tg_lfa1-stcd1.
      wa_saida_0100-forne_ie    = tg_lfa1-stcd3.
      wa_saida_0100-stcd2       = tg_lfa1-stcd2. "USER STORY 163035 - MMSILVA - 08.01.2024
      wa_saida_0100-lifnr       = tg_lfa1-lifnr.
    ENDIF.

    IF wa_saida_0100-cancel IS NOT INITIAL.
      wa_saida_0100-line_color  = 'C610'.
    ENDIF.

    CLEAR: vl_lifnr, vl_branch.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_saida_0100-lifnr
      IMPORTING
        output = vl_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_saida_0100-branch
      IMPORTING
        output = vl_branch.

    CHECK vl_lifnr <> vl_branch.

*===================================Inicio CS2023000490/USER STORY 128297 / AOENNING.
    IF wa_saida_0100-docnum IS INITIAL.
      READ TABLE tg_zib_nfe_f WITH KEY nu_chave = tg_zib_nfe_dist_ter-chave_nfe.
      IF sy-subrc EQ 0.
        wa_saida_0100-docnum = tg_zib_nfe_f-docnum.
      ENDIF.
    ENDIF.
*===================================Fim CS2023000490/USER STORY 128297 / AOENNING.

    APPEND wa_saida_0100 TO it_saida_0100.

    "Verifica se Nota possui mais de uma migo. Caso sim, adicona linhas com outras migo.
    IF wa_saida_0100-migo IS NOT INITIAL.
      LOOP AT tg_ekbe WHERE xblnr = tg_zib_nfe_dist_ter-ref_doc_no
                        AND vgabe = '1'
                        AND lifnr = tg_zib_nfe_dist_ter-lifnr
                        AND belnr <> wa_saida_0100-migo
                        AND shkzg <> 'H'.

        wa_saida_0100-zterm    =  tg_ekbe-zterm.


        CLEAR: wa_saida_0100-text1.
        SELECT SINGLE text1
          FROM t052u INTO wa_saida_0100-text1
          WHERE zterm EQ wa_saida_0100-zterm
          AND spras EQ sy-langu
          AND ztagg EQ space.

        READ TABLE tg_mkpf WITH KEY mblnr = tg_ekbe-belnr
                                    mjahr = tg_ekbe-gjahr.
        IF sy-subrc = 0.
          wa_saida_0100-migo     =  tg_mkpf-mblnr.
          wa_saida_0100-dt_migo  =  tg_mkpf-budat.
          wa_saida_0100-gj_migo  =  tg_mkpf-mjahr.

          APPEND wa_saida_0100 TO it_saida_0100.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  LOOP AT tg_zib_cte_dist_ter.

    CLEAR: wa_saida_0100, tg_lfa1.

    wa_saida_0100-chave           =  tg_zib_cte_dist_ter-cd_chave_cte.
    wa_saida_0100-bukrs           =  tg_zib_cte_dist_ter-e_tomadora.
    wa_saida_0100-branch          =  tg_zib_cte_dist_ter-f_tomadora.
    wa_saida_0100-numero          =  tg_zib_cte_dist_ter-numr_cte.
    wa_saida_0100-serie           =  tg_zib_cte_dist_ter-numr_serie.
    wa_saida_0100-dt_emissao      =  tg_zib_cte_dist_ter-dt_emissao.
    wa_saida_0100-model           =  tg_zib_cte_dist_ter-modelo.
    wa_saida_0100-cancel          =  tg_zib_cte_dist_ter-cancel.
    wa_saida_0100-vl_total        =  tg_zib_cte_dist_ter-valor_prestacao.
    wa_saida_0100-cfop            =  tg_zib_cte_dist_ter-codg_cfop.


*
*    CLEAR: tg_xml_cte.
*    IF tg_zib_cte_dist_ter-cd_chave_cte IS NOT INITIAL.
*      TRY .
*          "Buscar informações do XML CTE / GRC .
*          CALL FUNCTION 'Z_DETALHAMENTO_CTE'
*            EXPORTING
*              i_chave_nfe = tg_zib_cte_dist_ter-cd_chave_cte   " Chave de Documento Fiscal Eletrônico
*            IMPORTING
*              e_xml_cte   = tg_xml_cte   " Estrutura tipo tabela XML NF-e SEFAZ Autorizado
*            EXCEPTIONS
*              no_found    = 1
*              OTHERS      = 2.
*          IF sy-subrc EQ 0.

    CASE tg_zib_cte_dist_ter-cd_tipo_cte.
      WHEN 0.
        wa_saida_0100-tpcte = |0 - CT-e Normal|.
      WHEN 1.
        wa_saida_0100-tpcte = |1 - CT-e de Compl.valores|.
      WHEN 2.
        wa_saida_0100-tpcte = |2 - CT-e de Anulação|.
      WHEN 3.
        wa_saida_0100-tpcte = |3 - CT-e de Substituição|.

      WHEN OTHERS.
    ENDCASE.

    CASE tg_zib_cte_dist_ter-cd_modal.
      WHEN 1.
        wa_saida_0100-modal = |1 - Rodoviário|.
      WHEN 2.
        wa_saida_0100-modal = |2 - Aéreo|.
      WHEN 3.
        wa_saida_0100-modal = |3 - Aquaviário|.
      WHEN 4.
        wa_saida_0100-modal = |4 - Ferroviário|.
      WHEN 5.
        wa_saida_0100-modal = |5 - Dutoviário|.
      WHEN 6.
        wa_saida_0100-modal = |6 - Multimodal|.

      WHEN OTHERS.
    ENDCASE.


    CASE tg_zib_cte_dist_ter-cd_tipo_servico.
      WHEN '0'.
        wa_saida_0100-tpserv  = |0 - Normal|.
      WHEN '1'.
        wa_saida_0100-tpserv  = |1 - Subcontratação|.
      WHEN '2'.
        wa_saida_0100-tpserv  = |2 - Redespacho|.

      WHEN '3'.
        wa_saida_0100-tpserv  = |3 - Redespacho Intermediário|.

      WHEN '4'.
        wa_saida_0100-tpserv  = |4 - Serviço Vinc.Multimodal|.
      WHEN OTHERS.
    ENDCASE.
*          ENDIF.
*        CATCH cx_sy_dyn_call_param_not_found.
*
*      ENDTRY.
*    ENDIF.

*----> CS0993761 / IR096524 --->


    SELECT * INTO w_zsdt0127 FROM zsdt0127
        WHERE chave EQ  wa_saida_0100-chave
          ORDER BY  data_emi hora_emi ASCENDING.

      PERFORM f_get_ds_manifesto USING w_zsdt0127-cd_operacao
                            CHANGING wa_saida_0100-ds_operacao.

    ENDSELECT.
*<---- CS0993761 / IR096524 <---

    "Busca Migo
    SORT tg_ekbe BY belnr ASCENDING.
    READ TABLE tg_ekbe WITH KEY xblnr = tg_zib_cte_dist_ter-ref_doc_no
                                vgabe = '1'
                                lifnr = tg_zib_cte_dist_ter-lifnr.
    IF sy-subrc = 0.
      wa_saida_0100-zterm    =  tg_ekbe-zterm.

      CLEAR: wa_saida_0100-text1.
      SELECT SINGLE text1
        FROM t052u INTO wa_saida_0100-text1
        WHERE zterm EQ wa_saida_0100-zterm
        AND spras EQ sy-langu
        AND ztagg EQ space.

      IF tg_ekbe-shkzg <> 'H'.
        READ TABLE tg_mkpf WITH KEY mblnr = tg_ekbe-belnr
                                    mjahr = tg_ekbe-gjahr.
        IF sy-subrc = 0.
          wa_saida_0100-migo     =  tg_mkpf-mblnr.
          wa_saida_0100-dt_migo  =  tg_mkpf-budat.
          wa_saida_0100-gj_migo  =  tg_mkpf-mjahr.

        ENDIF.
      ENDIF.
    ENDIF.

    "Busca Miro
    READ TABLE tg_ekbe WITH KEY xblnr = tg_zib_cte_dist_ter-ref_doc_no
                                vgabe = '2'
                                lifnr = tg_zib_cte_dist_ter-lifnr.
    IF sy-subrc = 0.
*      wa_saida_0100-zterm    =  tg_ekbe-zterm.
*
*      CLEAR: wa_saida_0100-text1.
*      SELECT SINGLE text1
*        FROM t052u INTO wa_saida_0100-text1
*        WHERE zterm EQ wa_saida_0100-zterm
*        AND spras EQ sy-langu
*        AND ztagg EQ space.

      IF tg_ekbe-shkzg <> 'H'.
        READ TABLE tg_rbkp WITH KEY belnr = tg_ekbe-belnr
                                    gjahr = tg_ekbe-gjahr.
        IF sy-subrc = 0.
          wa_saida_0100-miro     =  tg_rbkp-belnr.
          wa_saida_0100-dt_miro  =  tg_rbkp-budat.
          wa_saida_0100-gj_miro  =  tg_rbkp-gjahr.
          wa_saida_0100-stblg    =  tg_rbkp-stblg. "!*-CS2022000243-#76365-26.04.2022-JT-inicio

*          wa_saida_0100-zterm    =  tg_rbkp-zterm.
*
*          CLEAR: wa_saida_0100-text1.
*          SELECT SINGLE text1
*            FROM t052u INTO wa_saida_0100-text1
*            WHERE zterm EQ wa_saida_0100-zterm
*            AND spras EQ sy-langu
*            AND ztagg EQ space.

          READ TABLE tg_j_1bnfdoc WITH KEY refkey = tg_rbkp-refkey.
          IF sy-subrc = 0.
            wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
            wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
            wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*** PBI - 72546 - Inicio - CBRAND
    CLEAR: vl_icms.
    LOOP AT tg_j_1bnfstx WHERE docnum  EQ  wa_saida_0100-docnum.
      vl_icms   = vl_icms + tg_j_1bnfstx-taxval.
      IF tg_j_1bnfstx-taxtyp   EQ 'ICM1'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMX'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMO'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMN'
        OR tg_j_1bnfstx-taxtyp EQ 'ICMF'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM4'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM3'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM2'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM1'
        OR tg_j_1bnfstx-taxtyp EQ 'ICM0'.
        wa_saida_0100-rate = tg_j_1bnfstx-rate.
      ENDIF.

      IF tg_j_1bnfstx-taxtyp(3) EQ 'IPI'.
        wa_saida_0100-rate_ipi = tg_j_1bnfstx-rate.
      ENDIF.
    ENDLOOP.
    IF vl_icms = 0.
      wa_saida_0100-taxval  = tg_zib_cte_dist_ter-valor_icms.
    ENDIF.
*** PBI - 72546 - Fim - CBRAND


    "Caso não definido nem migo nem miro
    IF ( wa_saida_0100-miro IS INITIAL ).

      "Tenta buscar direto pela Miro
      READ TABLE tg_rbkp WITH KEY xblnr = tg_zib_cte_dist_ter-ref_doc_no
                                  lifnr = tg_zib_cte_dist_ter-lifnr
                                  bukrs = tg_zib_cte_dist_ter-e_tomadora.

      IF sy-subrc = 0.

        wa_saida_0100-miro     =  tg_rbkp-belnr.
        wa_saida_0100-dt_miro  =  tg_rbkp-budat.
        wa_saida_0100-gj_miro  =  tg_rbkp-gjahr.
        wa_saida_0100-stblg    =  tg_rbkp-stblg. "!*-CS2022000243-#76365-26.04.2022-JT-inicio

*        wa_saida_0100-zterm    =  tg_rbkp-zterm.
*
*        CLEAR: wa_saida_0100-text1.
*        SELECT SINGLE text1
*          FROM t052u INTO wa_saida_0100-text1
*          WHERE zterm EQ wa_saida_0100-zterm
*          AND spras EQ sy-langu
*          AND ztagg EQ space.

        READ TABLE tg_j_1bnfdoc WITH KEY refkey = tg_rbkp-refkey.
        IF sy-subrc = 0.
          wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
          wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
          wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
        ENDIF.

      ELSE. "Tenta buscar pelo Fiscal

        LOOP AT tg_j_1bnfdoc WHERE parid       EQ tg_zib_cte_dist_ter-lifnr
                               AND branch      EQ tg_zib_cte_dist_ter-f_tomadora
                               AND nfenum      EQ tg_zib_cte_dist_ter-numr_cte
                               AND model       EQ tg_zib_cte_dist_ter-modelo
                               AND ( ( series  EQ tg_zib_cte_dist_ter-numr_serie     ) OR
                                     ( series  EQ tg_zib_cte_dist_ter-numr_serie_aux ) ).

          wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
          wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
          wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
          EXIT.
        ENDLOOP.

        IF wa_saida_0100-docnum IS INITIAL.
          READ TABLE tg_j_1bnfe_active WITH KEY stcd1   = tg_zib_cte_dist_ter-emit_cnpj
                                                model   = tg_zib_cte_dist_ter-modelo
                                                serie   = tg_zib_cte_dist_ter-numr_serie
                                                nfnum9  = tg_zib_cte_dist_ter-numr_cte
                                                branch  = tg_zib_cte_dist_ter-f_tomadora.
          IF sy-subrc = 0.
            READ TABLE tg_j_1bnfdoc WITH KEY docnum = tg_j_1bnfe_active-docnum.
            IF sy-subrc = 0.
              wa_saida_0100-docnum  =  tg_j_1bnfdoc-docnum.
              wa_saida_0100-pstdat  =  tg_j_1bnfdoc-pstdat.
              wa_saida_0100-candat  =  tg_j_1bnfdoc-candat.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    "Filtro Status Lançamento
*    CASE p_st_lct.
*      WHEN '1'. "Lançados
*        CHECK  wa_saida_0100-docnum IS NOT INITIAL.
*      WHEN '2'. "Pendentes
*        CHECK  wa_saida_0100-docnum IS INITIAL.
*    ENDCASE.
    IF p_lct1 IS NOT INITIAL. "Lançados
      CHECK  wa_saida_0100-docnum IS NOT INITIAL.
    ENDIF.
    IF p_lct2 IS NOT INITIAL. "Pendentes
      CHECK  wa_saida_0100-docnum IS INITIAL.
    ENDIF.

**----> CS1111038 - IR142508 --->
*   READ TABLE tg_lfa1 WITH KEY lifnr = tg_zib_cte_dist_ter-lifnr BINARY SEARCH.
    READ TABLE tg_lfa1 WITH KEY lifnr = tg_zib_cte_dist_ter-lifnr.
**<---- CS1111038 - IR142508 <---

*===================================inicio cs2023000490/user story 128297 / aoenning.
    IF wa_saida_0100-docnum IS INITIAL.
      READ TABLE tg_zib_nfe_f WITH KEY nu_chave = tg_zib_cte_dist_ter-cd_chave_cte.
      IF sy-subrc EQ 0.
        wa_saida_0100-docnum = tg_zib_nfe_f-docnum.
      ENDIF.
    ENDIF.
*===================================Fim CS2023000490/USER STORY 128297 / AOENNING.


    IF ( sy-subrc EQ 0 ).
      wa_saida_0100-name_forn   = tg_lfa1-name1.
      wa_saida_0100-forne_cnpj  = tg_lfa1-stcd1.
      wa_saida_0100-forne_ie    = tg_lfa1-stcd3.
      wa_saida_0100-stcd2       = tg_lfa1-stcd2. "USER STORY 163035 - MMSILVA - 08.01.2024
      wa_saida_0100-lifnr       = tg_lfa1-lifnr.
    ENDIF.

    IF wa_saida_0100-cancel IS NOT INITIAL.
      wa_saida_0100-line_color  = 'C610'.
    ENDIF.

    "US #164021 - MMSILVA - 27.02.2025 - Inicio
    IF ( wa_saida_0100-miro IS INITIAL ) AND ( wa_saida_0100-docnum IS NOT INITIAL ).
      SELECT SINGLE docnum, refkey FROM j_1bnflin WHERE docnum EQ @wa_saida_0100-docnum INTO CORRESPONDING FIELDS OF @wa_miro_cte.

      IF sy-subrc IS INITIAL.
        wa_saida_0100-miro = wa_miro_cte-refkey+0(10).

        SELECT SINGLE budat FROM rbkp WHERE belnr EQ @wa_saida_0100-miro INTO @DATA(wa_data_cte).

        IF sy-subrc IS INITIAL.
          wa_saida_0100-dt_miro = wa_data_cte.
        ENDIF.

      ENDIF.
    ENDIF.

    IF ( wa_saida_0100-migo IS INITIAL ) AND ( wa_saida_0100-miro IS NOT INITIAL ) AND ( wa_saida_0100-docnum IS NOT INITIAL ).
      SELECT SINGLE belnr, lfbnr, vgabe, budat FROM ekbe WHERE belnr EQ @wa_saida_0100-miro INTO CORRESPONDING FIELDS OF @wa_migo_cte.

        IF sy-subrc IS INITIAL.
          IF ( wa_migo_cte-lfbnr+0(3) EQ '100' ).
            SELECT SINGLE belnr, lfbnr, vgabe, budat FROM ekbe WHERE lfbnr EQ @wa_migo_cte-lfbnr AND vgabe = '1' INTO CORRESPONDING FIELDS OF @wa_migo_cte2.

            IF sy-subrc IS INITIAL.
              wa_saida_0100-migo = wa_migo_cte2-belnr.
              wa_saida_0100-dt_migo = wa_migo_cte2-budat.
            ENDIF.

          ELSEIF ( wa_migo_cte-lfbnr+0(2) EQ '50' ).

            wa_saida_0100-migo = wa_migo_cte-lfbnr.
            wa_saida_0100-dt_migo = wa_migo_cte-budat.

          ENDIF.

        ENDIF.
    ENDIF.
    "US #164021 - MMSILVA - 27.02.2025 - Fim

    APPEND wa_saida_0100 TO it_saida_0100.

    "Verifica se Nota possui mais de uma migo. Caso sim, adicona linhas com outras migo.
    IF wa_saida_0100-migo IS NOT INITIAL.
      LOOP AT tg_ekbe WHERE xblnr = tg_zib_cte_dist_ter-ref_doc_no
                        AND vgabe = '1'
                        AND lifnr = tg_zib_cte_dist_ter-lifnr
                        AND belnr <> wa_saida_0100-migo
                        AND shkzg <> 'H'.

        wa_saida_0100-zterm    =  tg_ekbe-zterm.

        CLEAR: wa_saida_0100-text1.
        SELECT SINGLE text1
          FROM t052u INTO wa_saida_0100-text1
          WHERE zterm EQ wa_saida_0100-zterm
          AND spras EQ sy-langu
          AND ztagg EQ space.

        READ TABLE tg_mkpf WITH KEY mblnr = tg_ekbe-belnr
                                    mjahr = tg_ekbe-gjahr.
        IF sy-subrc = 0.
          wa_saida_0100-migo     =  tg_mkpf-mblnr.
          wa_saida_0100-dt_migo  =  tg_mkpf-budat.
          wa_saida_0100-gj_migo  =  tg_mkpf-mjahr.
          APPEND wa_saida_0100 TO it_saida_0100.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDLOOP.

  IF p_for_ie IS NOT INITIAL.
    DELETE it_saida_0100 WHERE forne_ie NOT IN p_for_ie. "RJF
  ENDIF.

ENDFORM.

FORM f_imprimir_dados .

  CALL SCREEN 0100.

ENDFORM.

FORM handle_hotspot_click  USING    p_screen
                                    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.


  CASE p_screen.
    WHEN '0100'.

      CLEAR: wa_saida_0100.
      CASE i_column_id.
        WHEN 'MIGO'.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id-index.

          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-migo IS NOT INITIAL ).




* ---> S4 Migration - 19/07/2023 - DG
*          SET PARAMETER ID 'MBN'  FIELD wa_saida_0100-migo.
*          SET PARAMETER ID 'MJA'  FIELD wa_saida_0100-gj_migo.
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action            = 'A04'
              i_refdoc            = 'R02'
              i_notree            = 'X'
              i_no_auth_check     = ' '
              i_deadend           = 'X'
              i_skip_first_screen = 'X'
              i_okcode            = 'OK_GO'
              i_mblnr             = wa_saida_0100-migo
              i_mjahr             = wa_saida_0100-gj_migo.
          "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG3 - DG
        WHEN 'MIRO'.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id-index.

          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-miro IS NOT INITIAL ).
          SET PARAMETER ID 'RBN'  FIELD wa_saida_0100-miro.
          SET PARAMETER ID 'GJR'  FIELD wa_saida_0100-gj_miro.
          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

        WHEN 'DOCNUM'.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id-index.

          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum IS NOT INITIAL ).
          SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      ENDCASE.

    WHEN '0101'.


  ENDCASE.


ENDFORM.                    "HANDLE_HOTSPOT_CLICK

FORM f_config_list .

*--------------------------------------------*
*  Status XML
*--------------------------------------------*
  CLEAR: it_value[].

  wa_value-key  = '1'.
  wa_value-text = 'Autorizado'.
  APPEND wa_value TO it_value.

  wa_value-key  = '2'.
  wa_value-text = 'Cancelado'.
  APPEND wa_value TO it_value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ST_XML'
      values = it_value.

*--------------------------------------------*
*  Status Manifesto
*--------------------------------------------*
*-CS2022000243-#76365-26.04.2022-JT-inicio
*  CLEAR: it_value[].
*
*  wa_value-key  = '000000'.
*  wa_value-text = 'Sem Manifesto'.
*  APPEND wa_value TO it_value.
*
*  wa_value-key  = '210210'.
*  wa_value-text = 'Ciência da Operação'.
*  APPEND wa_value TO it_value.
*
*  wa_value-key  = '210200'.
*  wa_value-text = 'Confirmação da Operação'.
*  APPEND wa_value TO it_value.
*
*  wa_value-key  = '210240'.
*  wa_value-text = 'Operação não Realizada'.
*  APPEND wa_value TO it_value.
*
*  wa_value-key  = '210220'.
*  wa_value-text = 'Desconhecimento da Operação'.
*  APPEND wa_value TO it_value.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'P_ST_MNF'
*      values = it_value.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'ZSDT0127-CD_OPERACAO'
*      values = it_value.
*-CS2022000243-#76365-26.04.2022-JT-fim

*--------------------------------------------*
*  Status Lançamento
*--------------------------------------------*
  CLEAR: it_value[].

  wa_value-key  = '1'.
  wa_value-text = 'Lançados'.
  APPEND wa_value TO it_value.

  wa_value-key  = '2'.
  wa_value-text = 'Pendentes'.
  APPEND wa_value TO it_value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ST_LCT'
      values = it_value.

*--------------------------------------------*
*  Modelo
*--------------------------------------------*
  CLEAR: it_value[].

  wa_value-key  = '55'.
  wa_value-text = 'NF-e'.
  APPEND wa_value TO it_value.

  wa_value-key  = '57'.
  wa_value-text = 'CT-e'.
  APPEND wa_value TO it_value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_MODELO'
      values = it_value.

ENDFORM.

FORM f_config_ranges .

  CLEAR: r_modelo,    r_modelo[],
         r_st_xml,    r_st_xml[].

  "----------------------------------------------------
  " Status XML
  "----------------------------------------------------
*  IF p_st_xml IS NOT INITIAL.
*
*    r_st_xml-sign   = 'I'.
*    r_st_xml-option = 'EQ'.
*
*    IF p_distri IS NOT INITIAL. "Tabelas de Distribuição
*      IF p_st_xml EQ '1'. "Autorizado
*        r_st_xml-low  = ''.
*      ELSEIF p_st_xml EQ '2'. "Cancelado
*        r_st_xml-low  = 'X'.
*      ENDIF.
*    ELSE.
*      IF p_st_xml EQ '1'. "Autorizado
*        r_st_xml-low  = '1'.
*      ELSEIF p_st_xml EQ '2'. "Cancelado
*        r_st_xml-low  = '2'.
*      ENDIF.
*    ENDIF.
*
*    r_st_xml-high = r_st_xml-low.
*
*    APPEND r_st_xml.
*  ENDIF.

  IF p_xml1 IS NOT INITIAL OR p_xml2 IS NOT INITIAL.

    r_st_xml-sign   = 'I'.
    r_st_xml-option = 'EQ'.

    IF p_distri IS NOT INITIAL. "Tabelas de Distribuição
      IF p_xml1 EQ 'X'. "Autorizado
        r_st_xml-low  = ''.
      ENDIF.

      IF p_xml2 EQ 'X'. "Cancelado
        r_st_xml-low  = 'X'.
      ENDIF.
    ELSE.

      IF p_xml1 EQ 'X'. "Autorizado
        r_st_xml-low  = '1'.
      ENDIF.

      IF p_xml2 EQ '2'. "Cancelado
        r_st_xml-low  = '2'.
      ENDIF.

    ENDIF.

    r_st_xml-high = r_st_xml-low.

    APPEND r_st_xml.
  ENDIF.

  "----------------------------------------------------
  " Modelo
  "----------------------------------------------------
  IF p_modelo IS NOT INITIAL.

    r_modelo-sign   = 'I'.
    r_modelo-option = 'EQ'.
    r_modelo-low    = p_modelo.
    r_modelo-high   = p_modelo.

    APPEND r_modelo.
  ENDIF.

  "----------------------------------------------------
  " Status Manifesto
  "----------------------------------------------------
*  IF p_st_mnf IS NOT INITIAL.
*
*    r_st_mnf-sign   = 'I'.
*    r_st_mnf-option = 'EQ'.

  IF p_mnf1 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '000000'.
    r_st_mnf-high   = '000000'.
    APPEND r_st_mnf.
  ENDIF.

  IF p_mnf2 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '210210'.
    r_st_mnf-high   = '210210'.
    APPEND r_st_mnf.
  ENDIF.

  IF p_mnf3 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.

    r_st_mnf-low    = '210200'.
    r_st_mnf-high   = '210200'.
    APPEND r_st_mnf.
  ENDIF.

  IF p_mnf4 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '210240'.
    r_st_mnf-high   = '210240'.
    APPEND r_st_mnf.
  ENDIF.

  IF p_mnf5 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '210220'.
    r_st_mnf-high   = '210220'.
    APPEND r_st_mnf.
  ENDIF.

*-CS2022000243-#76365-26.04.2022-JT-inicio
  IF p_mnf6 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '610110'.
    r_st_mnf-high   = '610110'.
    APPEND r_st_mnf.
  ENDIF.
*-CS2022000243-#76365-26.04.2022-JT-fim

  "ENDIF.

  "----------------------------------------------------
  " Status Lançamento
  "----------------------------------------------------
  "IF p_st_lct IS NOT INITIAL.

  IF p_lct1 IS NOT INITIAL.
    r_st_lct-sign   = 'I'.
    r_st_lct-option = 'EQ'.
    r_st_lct-low    = '1'.
    r_st_lct-high   = '1'.
    APPEND r_st_lct.
  ENDIF.

  IF p_lct2 IS NOT INITIAL.
    r_st_lct-sign   = 'I'.
    r_st_lct-option = 'EQ'.
    r_st_lct-low    = '2'.
    r_st_lct-high   = '2'.
    APPEND r_st_lct.
  ENDIF.

  "ENDIF.

*--------------------------------------------*
*  Status Manifesto
*--------------------------------------------*
*-CS2022000243-#76365-26.04.2022-JT-inicio
*  CLEAR: it_value[].
*
*  IF p_mnf6 = abap_true.
*    wa_value-key  = '610110'.
*    wa_value-text = 'Desacordo de Entrega de Serviços (CT-e)'.
*    APPEND wa_value TO it_value.
*  ELSE.
*    wa_value-key  = '000000'.
*    wa_value-text = 'Sem Manifesto'.
*    APPEND wa_value TO it_value.
*
*    wa_value-key  = '210210'.
*    wa_value-text = 'Ciência da Operação'.
*    APPEND wa_value TO it_value.
*
*    wa_value-key  = '210200'.
*    wa_value-text = 'Confirmação da Operação'.
*    APPEND wa_value TO it_value.
*
*    wa_value-key  = '210240'.
*    wa_value-text = 'Operação não Realizada'.
*    APPEND wa_value TO it_value.
*
*    wa_value-key  = '210220'.
*    wa_value-text = 'Desconhecimento da Operação'.
*    APPEND wa_value TO it_value.
*  ENDIF.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'P_ST_MNF'
*      values = it_value.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'ZSDT0127-CD_OPERACAO'
*      values = it_value.
*-CS2022000243-#76365-26.04.2022-JT-fim

ENDFORM.

FORM f_tratar_ie USING p_ie.

  DATA: vl_ie_num TYPE p.

  CHECK p_ie IS NOT INITIAL.

  CLEAR: vl_ie_num.

  REPLACE ALL OCCURRENCES OF  '.'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '/'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '\'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '-'  IN p_ie  WITH '' IGNORING CASE.

  CONDENSE p_ie  NO-GAPS.

  TRY.
      vl_ie_num  = p_ie.
      p_ie       = vl_ie_num.
      CONDENSE p_ie NO-GAPS.
    CATCH cx_sy_conversion_no_number.
    CATCH cx_sy_conversion_overflow.
  ENDTRY.

ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_field_catalog USING p_screen.

  CLEAR: wa_fcat, it_fcat, it_fcat[] .

  CASE p_screen.
    WHEN '0100'.

      PERFORM f_estrutura_alv USING:

        01 'ZIB_NFE_DIST_TER'  'BUKRS'         'IT_SAIDA_0100' 'BUKRS'         'Empresa               ' '07' '' '' '' '' '' '' '',
        02 'ZIB_NFE_DIST_TER'  'BRANCH'        'IT_SAIDA_0100' 'BRANCH'        'Filial                ' '07' '' '' '' '' '' '' '',
        03 'ZIB_NFE_DIST_TER'  'NUMERO'        'IT_SAIDA_0100' 'NUMERO'        'NF-e/CT-e             ' '10' '' '' '' '' '' '' '',
        04 'ZIB_NFE_DIST_TER'  'SERIE'         'IT_SAIDA_0100' 'SERIE'         'Série                 ' '05' '' '' '' 'C' '' '' '',
        05 'ZIB_NFE_DIST_TER'  'DT_EMISSAO'    'IT_SAIDA_0100' 'DT_EMISSAO'    'Dt.Emissão            ' '10' '' '' '' '' '' '' '',
        06 ''                  ''              'IT_SAIDA_0100' 'CFOP'          'CFOP                  ' '06' '' '' '' '' '' '' '',
        07 'LFA1'              'LIFNR'         'IT_SAIDA_0100' 'LIFNR'         'Cd.Fornecedor         ' '13' '' '' '' '' '' '' '',
        08 'LFA1'              'STCD1'         'IT_SAIDA_0100' 'FORNE_CNPJ'    'CNPJ Fornecedor       ' '16' '' '' '' '' '' '' '',
        07 'LFA1'              'STCD2'         'IT_SAIDA_0100' 'STCD2'         'CPF Fornecedor        ' '16' '' '' '' '' '' '' '', "USER STORY 163035 - MMSILVA - 08.01.2024
        09 'LFA1'              'STCD3'         'IT_SAIDA_0100' 'FORNE_IE'      'IE Fornecedor         ' '16' '' '' '' '' '' '' '',
        10 'LFA1'              'NAME1'         'IT_SAIDA_0100' 'NAME_FORN'     'Nome Fornecedor       ' '20' '' '' '' '' '' '' '',
        11 'ZPMT0030'          'MBLNR'         'IT_SAIDA_0100' 'MIGO'          'Migo                  ' '15' '' '' '' '' 'X' '' '',
        12 'MKPF'              'BUDAT'         'IT_SAIDA_0100' 'DT_MIGO'       'Dt.Migo               ' '15' '' '' '' '' '' '' '',
        13 'ZPMT0030'          'BELNR'         'IT_SAIDA_0100' 'MIRO'          'Miro                  ' '15' '' '' '' '' 'X' '' '',
        14 'RBKP'              'BUDAT'         'IT_SAIDA_0100' 'DT_MIRO'       'Dt.Miro               ' '15' '' '' '' '' '' '' '',
        15 'J_1BNFDOC'         'J_1BDOCNUM'    'IT_SAIDA_0100' 'DOCNUM'        'Docnum                ' '10' '' '' '' '' 'X' '' '',
        16 'J_1BNFDOC'         'J_1BDOCDAT'    'IT_SAIDA_0100' 'PSTDAT'        'Dt.Lcto               ' '10' '' '' '' '' '' '' '',
        17 'ZIB_NFE_DIST_TER'  'MODEL'         'IT_SAIDA_0100' 'MODEL'         'Modelo                ' '06' '' '' '' 'C' '' '' '',
        18 'ZIB_NFE_DIST_TER'  'CANCEL'        'IT_SAIDA_0100' 'CANCEL'        'Cancelado             ' '09' '' '' '' '' '' '' '',
        19 'ZIB_NFE_DIST_TER'  'DT_VENCIMENTO' 'IT_SAIDA_0100' 'DT_VENCIMENTO' 'Data Vencimento Nfe   ' '15' '' '' '' '' '' '' '',
        20 'EKKO'              'ZTERM'         'IT_SAIDA_0100' 'ZTERM        ' 'Cond.pagamento PC     ' '15' '' '' '' '' '' '' '',
        21 'T052U'             'TEXT1'         'IT_SAIDA_0100' 'TEXT1    '     'Desc.cond pagamento PC' '15' '' '' '' '' '' '' '',
        22 'ZIB_NFE_DIST_TER'  'VL_TOTAL'      'IT_SAIDA_0100' 'VL_TOTAL'      'Vlr. Total            ' '15' '' '' '' '' '' '' '',
        23 'J_1BNFSTX'         'TAXVAL'        'IT_SAIDA_0100' 'TAXVAL'        'ICMS                  ' '15' '' '' '' '' '' '' '',
        24 'J_1BNFSTX'         'RATE'          'IT_SAIDA_0100' 'RATE'          'Aliquota ICMS         ' '15' '' '' '' '' '' '' '',
        25 'ZSDT0127'          'CD_OPERACAO'   'IT_SAIDA_0100' 'DS_OPERACAO'   'St.Manifesto          ' '25' '' '' '' '' '' '' '',
        26 'ZIB_NFE_DIST_TER'  'CHAVE_NFE'     'IT_SAIDA_0100' 'CHAVE'         'Chave                 ' '44' '' '' '' '' '' '' '',
        27 'ZIB_NFE_FORN'      'DOCNUM_REF'    'IT_SAIDA_0100' 'DOCNUM_REF'    'Doc.Ref               ' '10' '' '' '' '' '' '' '',
        28 '                '  '     '         'IT_SAIDA_0100' 'TPNF'          'Tipo Nfe              ' '20' '' '' '' '' '' '' '',
        29 '                '  '     '         'IT_SAIDA_0100' 'FINNFE'        'Finalidade Nfe        ' '20' '' '' '' '' '' '' '',
        30 '                '  '     '         'IT_SAIDA_0100' 'TPCTE'         'Tipo Cte              ' '20' '' '' '' '' '' '' '',
        31 '                '  '     '         'IT_SAIDA_0100' 'TPSERV'        'Tipo Srv Cte          ' '20' '' '' '' '' '' '' '',
        32 '                '  '     '         'IT_SAIDA_0100' 'MODAL'         'Modal                 ' '15' '' '' '' '' '' '' '',
        33 'PEDIDO'             '     '        'IT_SAIDA_0100' 'PEDIDO'        'Pedido                ' '15' '' '' '' '' '' '' '',
        34 'CHAVE_NFE_REF'      '     '        'IT_SAIDA_0100' 'CHAVE_NFE_REF' 'Nfe Ref               ' '72' '' '' '' '' '' '' ''.





    WHEN '0105'.

      PERFORM f_estrutura_alv USING:

        01 ''             ''                  'IT_SAIDA_0105' 'DOC_MANIFESTO'  'Documento'          '10'  ''    '' ' ' ' ' ' ' ' ' ' ',
        03 ''             ''                  'IT_SAIDA_0105' 'DS_OPERACAO'    'Operação'           '25'  ''    '' ' ' ' ' ' ' ' ' ' ',

        03 'ZSDT0127'     'AUTORIZADO'        'IT_SAIDA_0105' 'AUTORIZADO'     'Autorizado'         '10'  ''    '' ' ' 'C' ' ' ' ' 'X',
        03 'ZSDT0127'     'AUTHCODE'          'IT_SAIDA_0105' 'AUTHCODE'       'Prot.'              '10'  ''    '' ' ' ' ' ' ' ' ' ' ',
        03 'ZSDT0127'     'DT_AUTHCOD'        'IT_SAIDA_0105' 'DT_AUTHCOD'     'Dt.Aut.'            '10'  ''    '' ' ' ' ' ' ' ' ' ' ',
        03 ''             ''                  'IT_SAIDA_0105' 'HR_AUTHCOD'     'Hr.Aut'             '10'  ''    '' ' ' ' ' ' ' ' ' ' ',

        04 ''             ''                  'IT_SAIDA_0105' 'JUSTIFICATIVA'  'Justificativa'      '30'  ''    '' ' ' ' ' ' ' ' ' ' ',
        05 ''             ''                  'IT_SAIDA_0105' 'USNAM'          'Usuário'            '10'  ''    '' ' ' ' ' ' ' ' ' ' ',
        06 ''             ''                  'IT_SAIDA_0105' 'DATA_EMI'       'Data Emissão'       '12'  ''    '' ' ' ' ' ' ' ' ' ' ',
        07 ''             ''                  'IT_SAIDA_0105' 'HORA_EMI'       'Hora Emissão'       '12'  ''    '' ' ' ' ' ' ' ' ' ' ',
*#127333 - 04.12.2023 - JT - inicio
        08 ''             ''                  'IT_SAIDA_0105' 'REJEICAO_ANULADA' 'Rejeição Anulada' '16'  ''    '' ' ' ' ' ' ' ' ' 'X',
        09 ''             ''                  'IT_SAIDA_0105' 'USER_ANULA'     'Usuário'            '10'  ''    '' ' ' ' ' ' ' ' ' ' ',
        10 ''             ''                  'IT_SAIDA_0105' 'DATA_ANULA'     'Data Anulação'      '12'  ''    '' ' ' ' ' ' ' ' ' ' ',
        11 ''             ''                  'IT_SAIDA_0105' 'HORA_ANULA'     'Hora Anulação'      '12'  ''    '' ' ' ' ' ' ' ' ' ' '.
*#127333 - 04.12.2023 - JT - fim

  ENDCASE.

ENDFORM.


FORM f_estrutura_alv USING  VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_just)
                            VALUE(p_hotspot)
                            VALUE(p_f4)
                            VALUE(p_checkbox).


  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-seltext     = p_scrtext_l.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-colddictxt  = 'L'.
  wa_fcat-selddictxt  = 'L'.
  wa_fcat-tipddictxt  = 'L'.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.
  wa_fcat-col_opt     = abap_true.


  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_code  USING p_screen.

  CASE p_screen.
    WHEN '0100' OR '0105'.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wa_exclude_fcode TO it_exclude_fcode.
*      wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
*      APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDCASE.

ENDFORM.

FORM f_gravar_manifesto .

  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.
  DATA: vl_doc_manifesto   TYPE zsdt0127-doc_manifesto.
  DATA: var_answer  TYPE c.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

*-CS2020001253 - 16.07.2021 - JT - inicio
*-Valida requerimento
  DATA(l_erro) = abap_false.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

    CHECK sy-subrc EQ 0.

    AUTHORITY-CHECK OBJECT 'ZLANCMANFT'  ID 'ACTVT' FIELD '03'.

    CASE sy-subrc.
*-CS2022000243-#76365-26.04.2022-JT-inicio
      WHEN 0.
        IF wa_saida_0100-docnum IS NOT INITIAL AND zsdt0127-cd_operacao = '610110'.
          l_erro = abap_true.
          MESSAGE |Lançamento de Manifesto não Permitido!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
*-CS2022000243-#76365-26.04.2022-JT-fim

      WHEN 4.
*-CS2022000243-#76365-26.04.2022-JT-inicio
        IF wa_saida_0100-docnum IS NOT INITIAL AND zsdt0127-cd_operacao = '610110'.
          l_erro = abap_true.
          MESSAGE |Lançamento de Manifesto não Permitido!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
*-CS2022000243-#76365-26.04.2022-JT-fim

*-CS2020001253 - 16.07.2021 - JT - inicio
        IF wa_saida_0100-docnum IS NOT INITIAL AND
         ( zsdt0127-cd_operacao = '210220' OR   "Desconhecimento da Operação
           zsdt0127-cd_operacao = '210240' ).   "Operação não Realizada'.
          l_erro = abap_true.
          MESSAGE |Lançamento de Manifesto não Permitido!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF wa_saida_0100-migo   IS INITIAL AND
           wa_saida_0100-miro   IS INITIAL AND
           wa_saida_0100-docnum IS INITIAL AND
         ( zsdt0127-cd_operacao = '210200' OR   "Confirmação da Operação
           zsdt0127-cd_operacao = '210210' ).   "Ciência da Operação
          l_erro = abap_true.
          MESSAGE |Lançamento de Manifesto não Permitido!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  CHECK l_erro = abap_false.
*-CS2020001253 - 16.07.2021 - JT - fim

  IF lines( it_sel_rows[] ) > 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Será gravado e enviado o manifesto para todas as notas selecionadas! Confirma operação?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ELSE.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Confirma gravação do manifesto?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDIF.

  CHECK var_answer = '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

    CHECK sy-subrc EQ 0.

    FREE: zcl_manifesto_dest.
    CREATE OBJECT zcl_manifesto_dest.

    "PERFORM F_TRATAR_IE USING WA_SAIDA_0100-DEST_IE.

    zcl_manifesto_dest->set_chave( wa_saida_0100-chave ).
    zcl_manifesto_dest->set_cd_operacao( zsdt0127-cd_operacao ).
    zcl_manifesto_dest->set_cnpj_dest( wa_saida_0100-dest_cnpj ).
    zcl_manifesto_dest->set_ie_dest( wa_saida_0100-dest_ie ).
    zcl_manifesto_dest->set_justificativa( zsdt0127-justificativa ).
    zcl_manifesto_dest->set_bukrs( wa_saida_0100-bukrs ).
    zcl_manifesto_dest->set_branch( wa_saida_0100-branch ).

    TRY.
        zcl_manifesto_dest->gravar_manifesto( RECEIVING e_doc_manifesto = vl_doc_manifesto ).

        IF ( vl_doc_manifesto IS NOT INITIAL ) AND ( lines( it_sel_rows[] ) > 1 ).
          TRY.
              zcl_manifesto_dest->enviar_manifesto( i_sem_confirmacao =  'X' ).
            CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
              ex_man_dest->published_erro( i_msgty = 'I' i_msgty_display = 'W' ).
          ENDTRY.
        ENDIF.

      CATCH zcx_manifesto_dest INTO ex_man_dest.
        ex_man_dest->published_erro( i_msgty = 'I' i_msgty_display = 'W' ).
    ENDTRY.

  ENDLOOP.

  IF lines( it_sel_rows[] ) EQ 1.
    PERFORM f_selecionar_manifestos USING wa_saida_0100.

    CLEAR: zsdt0127.

    LEAVE TO SCREEN 0105.
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

***********************************************************
* anulat rejeicao manifesto
***********************************************************
FORM f_anular_rejeicao_manifesto .

  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.
  DATA: vl_doc_manifesto   TYPE zsdt0127-doc_manifesto.

  CLEAR it_sel_rows.
  CALL METHOD obj_alv_0105->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: wa_saida_0105.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0105 INTO wa_saida_0105 INDEX wa_sel_rows-index.

  SELECT SINGLE *
    FROM zsdt0127
    INTO @DATA(w_0127)
   WHERE chave         = @wa_saida_0105-chave
     AND doc_manifesto = @wa_saida_0105-doc_manifesto.

  CHECK sy-subrc = 0.

  FREE: zcl_manifesto_dest.
  CREATE OBJECT zcl_manifesto_dest
    EXPORTING
      i_chave         = wa_saida_0105-chave
      i_doc_manifesto = wa_saida_0105-doc_manifesto.

  TRY.
      zcl_manifesto_dest->set_valida_enviar_drc( i_zsdt0127 = w_0127
                                                 i_action   = 'UNDOREJECT' ).
    CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
      ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH cx_edocument     INTO DATA(ex_edocument).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

* LEAVE TO SCREEN 0105.

ENDFORM.


FORM f_anular_conf_oper_manifesto .

  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.
  DATA: vl_doc_manifesto   TYPE zsdt0127-doc_manifesto.

  CLEAR it_sel_rows.
  CALL METHOD obj_alv_0105->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: wa_saida_0105.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0105 INTO wa_saida_0105 INDEX wa_sel_rows-index.

  SELECT SINGLE *
    FROM zsdt0127
    INTO @DATA(w_0127)
   WHERE chave         = @wa_saida_0105-chave
     AND doc_manifesto = @wa_saida_0105-doc_manifesto.

  CHECK sy-subrc = 0.

  FREE: zcl_manifesto_dest.
  CREATE OBJECT zcl_manifesto_dest
    EXPORTING
      i_chave         = wa_saida_0105-chave
      i_doc_manifesto = wa_saida_0105-doc_manifesto.

  TRY.
      zcl_manifesto_dest->set_valida_enviar_drc( i_zsdt0127 = w_0127
                                                 i_action   = 'MD_DENIAL' ).
    CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
      ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH cx_edocument     INTO DATA(ex_edocument).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

* LEAVE TO SCREEN 0105.

ENDFORM.

FORM f_emitir_manifesto .

  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.
  DATA: vl_doc_manifesto   TYPE zsdt0127-doc_manifesto.

  CLEAR it_sel_rows.
  CALL METHOD obj_alv_0105->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: wa_saida_0105.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0105 INTO wa_saida_0105 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  FREE: zcl_manifesto_dest.
  CREATE OBJECT zcl_manifesto_dest
    EXPORTING
      i_chave         = wa_saida_0105-chave
      i_doc_manifesto = wa_saida_0105-doc_manifesto.

  TRY.
      zcl_manifesto_dest->enviar_manifesto( ).
    CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
      ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

*  LEAVE TO SCREEN 0105.


ENDFORM.

FORM f_get_ds_manifesto USING p_cd_manifesto
                     CHANGING p_ds_manifesto.

  CASE p_cd_manifesto.
    WHEN '000000'.
      p_ds_manifesto  = 'Sem Manifesto'.
    WHEN '210210'.
      p_ds_manifesto  = 'Ciência da Operação'.
    WHEN '210200'.
      p_ds_manifesto  = 'Confirmação da Operação'.
    WHEN '210240'.
      p_ds_manifesto  = 'Operação não Realizada'.
    WHEN '210220'.
      p_ds_manifesto  = 'Desconhecimento da Operação'.
*-CS2022000243-#76365-26.04.2022-JT-inicio
    WHEN '610110'.
      p_ds_manifesto  = 'Desacordo de Entrega de Serviços(CT-e)'.
*-CS2022000243-#76365-26.04.2022-JT-fim
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DETALHE_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_detalhe_nfe .

  CLEAR: it_sel_rows[], wa_sel_rows.


  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  DESCRIBE TABLE it_sel_rows LINES DATA(l_lines).

  IF l_lines > 1.
    MESSAGE 'Selecionar apenas uma linha' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  CHECK sy-subrc = 0.

  READ TABLE it_saida_0100 INTO DATA(wa_saida_aux) INDEX wa_sel_rows-index.
  CHECK sy-subrc = 0.

  IF wa_saida_aux-model NE '55'.
    MESSAGE 'Documento não é um DANFE/DACTE' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'Z_SHOW_DETALHAMENTO_NFE'
    EXPORTING
      i_chave_nfe = wa_saida_aux-chave
    EXCEPTIONS
      no_found    = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE 'XML não foi localizado.' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CRIAR_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0025   text
*----------------------------------------------------------------------*
FORM f_criar_catalog  USING    VALUE(p_0025).

  PERFORM alv_preenche_cat USING:
        'BUKRS'                 'Empresa               '  '07'   ''  '' ''    '' ,
        'BRANCH'                'Filial                '  '07'   ''  '' ''    '' ,
        'NUMERO'                'NF-e/CT-e             '  '10'   ''  '' ''    '' ,
        'SERIE'                 'Série                 '  '05'   ''  '' ''    'C' ,
        'DT_EMISSAO'            'Dt.Emissão            '  '10'   ''  '' ''    '' ,
        'CFOP'                  'CFOP                  '  '44'   ''  '' ''    '' ,
        'LIFNR'                 'Cd.Fornecedor         '  '13'   ''  '' ''    '' ,
        'FORNE_CNPJ'            'CNPJ Fornecedor       '  '16'   ''  '' ''    '' ,
        'STCD2'                 'CPF Fornecedor        '  '16'   ''  '' ''    '' , "USER STORY 163035 - MMSILVA - 08.01.2024
        'FORNE_IE'              'IE Fornecedor         '  '16'   ''  '' ''    '' ,
        'NAME_FORN'             'Nome Fornecedor       '  '40'   ''  '' ''    '' ,
        'MIGO'                  'Migo                  '  '10'   'X'  '' ''   '',
        'DT_MIGO'               'Dt.Migo               '  '10'   ''  '' ''    '' ,
        'MIRO'                  'Miro                  '  '10'   'X'  '' ''   '',
        'DT_MIRO'               'Dt.Miro               '  '10'   ''  '' ''    '' ,
        'DOCNUM'                'Docnum                '  '10'   'X'  'X' ''  '',
        'PSTDAT'                'Dt.Lcto               '  '10'   ''  '' ''    '' ,
        'MODEL'                 'Modelo                '  '06'   ''  '' ''    '' ,
        'CANCEL'                'Cancelado             '  '09'   ''  '' ''    '' ,
        'DT_VENCIMENTO'         'Data Vencimento Nfe   '  '10'   ''  '' ''    '' ,
        'ZTERM        '         'Cond.pagamento PC     '  '10'   ''  '' ''    '' ,
        'TEXT1    '             'Desc.cond pagamento PC'  '10'   ''  '' ''    '' ,
        'VL_TOTAL'              'Vlr. Total            '  '10'   ''  '' ''    '' ,
        'TAXVAL'                'ICMS                  '  '10'   ''  '' ''    '' ,
        'RATE'                  'Aliquota ICMS         '  '10'   ''  '' ''    '' ,
        'TAXVAL_IPI'            'IPI                   '  '10'   ''  '' ''    '' ,
        'RATE_IPI'              'Aliquota IPI          '  '10'   ''  '' ''    '' ,
        'DS_OPERACAO'           'St.Manifesto          '  '10'   ''  '' ''    '' ,
        'CHAVE'                 'Chave                 '  '44'   ''  '' ''    '' ,
        'DOCNUM_REF'            'Doc.Ref               '  '10'   ''  '' ''    '' ,
        'TPNF'                  'Tipo Nfe              '  '15'   ''  '' ''    '' ,
        'FINNFE'                'Finalidade Nfe        '  '15'   ''  '' ''    '' ,
        'TPCTE'                 'Tipo Cte              '  '15'   ''  '' ''    '' ,
        'TPSERV'                'Tipo Srv Cte          '  '15'   ''  '' ''    '' ,
        'MODAL'                 'Modal                 '  '15'   ''  '' ''    '' ,
        'PEDIDO'                'Pedido                '  '15'   ''  '' ''    '' ,
        'CHAVE_NFE_REF'         'Nfe Ref               '  '10000'   ''  '' ''    '' .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5722   text
*      -->P_TEXT_011  text
*      -->P_5724   text
*      -->P_5725   text
*      -->P_5726   text
*      -->P_5727   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat    USING p_campo    TYPE c
                               p_desc     TYPE c
                               p_tam      TYPE c
                               p_hot      TYPE c
                               p_zero     TYPE c
                               p_convexit TYPE c
                               p_just     TYPE c.


  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-convexit  = p_convexit.
  wa_fcat-just      = p_just.
  wa_fcat-col_opt     = abap_true.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_fiscal .

*---> CS1102269 ; IR140174 --->

  TABLES: zsdt0127_fiscal.

  DATA: w_fiscal   LIKE zsdt0127_fiscal,
        w_zsdt0127 LIKE zsdt0127,
        w_user     TYPE sy-uname.

  SELECT SINGLE valfrom INTO w_user
         FROM setleaf
        WHERE setname EQ 'ZSDT0110_FISCAL'.

  IF w_user EQ sy-uname.

    SELECT * FROM zsdt0127_fiscal INTO w_fiscal
       WHERE atualiza EQ ''.

      IF sy-subrc EQ 0.

        SELECT * FROM zsdt0127 INTO w_zsdt0127
          WHERE chave EQ w_fiscal-chave.

          IF w_fiscal-authcode NE ''. w_zsdt0127-authcode = w_fiscal-authcode.ENDIF.
          IF w_fiscal-cd_operacao NE ''. w_zsdt0127-cd_operacao = w_fiscal-cd_operacao.ENDIF.
          IF w_fiscal-autorizado NE ''. w_zsdt0127-autorizado = w_fiscal-autorizado.ENDIF.

          w_fiscal-atualiza = 'X'.
          w_fiscal-usnam = sy-uname.
          w_fiscal-dt_atualizado = sy-datum.

          UPDATE zsdt0127 SET
            authcode = w_zsdt0127-authcode
            cd_operacao = w_zsdt0127-cd_operacao
            autorizado = w_zsdt0127-autorizado
          WHERE chave EQ w_fiscal-chave
            AND doc_manifesto EQ w_zsdt0127-doc_manifesto.

        ENDSELECT.

        MODIFY zsdt0127_fiscal FROM w_fiscal.
        MESSAGE s000(z01) WITH 'Atualização Fiscal efetuada'.

      ENDIF.
    ENDSELECT.

  ENDIF.

*<--- CS1102269 ; IR140174 <---

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_modifica_evento_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_modifica_evento_doc .

  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.
  DATA: vl_doc_manifesto   TYPE zsdt0127-doc_manifesto.

  DATA:tl_fields TYPE TABLE OF sval WITH HEADER LINE,
       v_ebeln   TYPE ekko-ebeln,
       lv_return TYPE vbpok-charg.

  CLEAR it_sel_rows.
  CALL METHOD obj_alv_0105->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: wa_saida_0105.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0105 INTO wa_saida_0105 INDEX wa_sel_rows-index.

  SELECT SINGLE *
    FROM zsdt0127
    INTO @DATA(w_0127)
   WHERE chave         = @wa_saida_0105-chave
     AND doc_manifesto = @wa_saida_0105-doc_manifesto.

  IF w_0127-autorizado IS NOT INITIAL.
    MESSAGE e024(sd) WITH 'Documento esta autorizado'
                          'não possivel realizar operação'.
    EXIT.
  ENDIF.

  "Preenche campo do popup.
  CLEAR tl_fields.
  tl_fields-tabname    = 'ZSDT0127'.
  tl_fields-fieldname  = 'AUTHCODE'.
  tl_fields-fieldtext  = 'Protocolo autorização'.
  tl_fields-value      =  ''.
  tl_fields-comp_tab   = 'ZSDT0127'.
  tl_fields-comp_field = 'AUTHCODE'.
  tl_fields-field_obl  = 'X'.
*  it_tab-novaluehlp = ''.
  APPEND tl_fields.

  CLEAR tl_fields.
  tl_fields-tabname    = 'ZSDT0127'.
  tl_fields-fieldname  = 'DT_AUTHCOD'.
  tl_fields-fieldtext  = 'Data autorização'.
  tl_fields-value      =  ''.
  tl_fields-comp_tab   = 'ZSDT0127'.
  tl_fields-comp_field = 'DT_AUTHCOD'.
  tl_fields-field_obl  = 'X'.
*  it_tab-novaluehlp = ''.
  APPEND tl_fields.

  CLEAR tl_fields.
  tl_fields-tabname    = 'ZSDT0127'.
  tl_fields-fieldname  = 'HR_AUTHCOD'.
  tl_fields-fieldtext  = 'Hora autorização'.
  tl_fields-value      =  ''.
  tl_fields-comp_tab   = 'ZSDT0127'.
  tl_fields-comp_field = 'HR_AUTHCOD'.
  tl_fields-field_obl  = 'X'.
*  it_tab-novaluehlp = ''.
  APPEND tl_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Informe o pedido'
    IMPORTING
      returncode      = lv_return
    TABLES
      fields          = tl_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.


  IF sy-subrc EQ 0.
    LOOP AT tl_fields.
      CASE tl_fields-fieldname.
        WHEN 'AUTHCODE'.
          w_0127-authcode = tl_fields-value.
        WHEN 'DT_AUTHCOD'.
          w_0127-dt_authcod = tl_fields-value.
        WHEN 'HR_AUTHCOD'.
          w_0127-hr_authcod = tl_fields-value.
      ENDCASE.
    ENDLOOP.

    IF w_0127-authcode IS NOT INITIAL.
      w_0127-code            = '135'.
      w_0127-autorizado      = abap_true.
      w_0127-status          = '1'.
      w_0127-msg_retorno     = 'Evento registrado e vinculado a NF-e'.
      w_0127-justificativa_auth_man   = 'Documento modificado/autorizado manualmente transação ZSDT0110'.
      w_0127-data_auth_man   = sy-datum.
      w_0127-hora_auth_man  = sy-uzeit.
      w_0127-user_auth_man   = sy-uname.

      MODIFY zsdt0127 FROM w_0127.
      COMMIT WORK.
      IF sy-subrc EQ 0.
        MESSAGE s024(sd) WITH 'Documentos ' w_0127-doc_manifesto 'modificado com sucesso!'.

        READ TABLE it_saida_0105 ASSIGNING FIELD-SYMBOL(<ws_saida>) WITH KEY chave         = wa_saida_0105-chave
                                                                             doc_manifesto = wa_saida_0105-doc_manifesto.
        IF sy-subrc EQ 0.
          <ws_saida>-authcode   = w_0127-authcode.
          <ws_saida>-autorizado = w_0127-autorizado.
          <ws_saida>-dt_authcod = w_0127-dt_authcod.
          <ws_saida>-hr_authcod = w_0127-hr_authcod.
        ENDIF.

        CALL METHOD obj_alv_0105->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE s024(sd) WITH 'Não é possivel modificar'
                          'documentos ' w_0127-doc_manifesto.

  ENDIF.
ENDFORM.
