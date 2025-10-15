"Name: \PR:SAPLJ1BF\FO:DETERMINE_STATISTICAL_TAX\SE:END\EI
ENHANCEMENT 0 Z_PEDIDO_ZUB_LAW.

  TYPES:
    BEGIN OF ty_ekko,
      werks TYPE ekpo-werks,
      reswk TYPE ekko-reswk,
    END OF ty_ekko.

  DATA: BEGIN OF litax OCCURS 50.        "NF item tax
          INCLUDE STRUCTURE j_1bnfstx.
  DATA: END OF litax.

  DATA wa_zmmt0154 TYPE zmmt0154.

  DATA: w_setleaf    TYPE setleaf,
        w_setlinet   TYPE setlinet,
        vg_setname   TYPE setleaf-setname,
        v_base       TYPE j_1bnfstx-base,
        ws_ekko      TYPE ty_ekko,
        ws_lfa1      TYPE lfa1,
        regio_e      TYPE lfa1-regio,
        regio_d      TYPE lfa1-regio,
        vwerks_e     TYPE lfa1-lifnr,
        vwerks_d     TYPE lfa1-lifnr,
        pos          TYPE i,
        v_forn_orig  TYPE lfa1-lifnr,
        v_forn_dest  TYPE lfa1-lifnr,
        v_ownpr      TYPE mbew-ownpr,
        v_mtorg      TYPE mbew-mtorg, "US #154447 - MMSILVA - 02.04.2025
        v_matkl      TYPE mara-matkl,
        v_regio_orig TYPE lfa1-regio,
        v_regio_dest TYPE lfa1-regio,
        lc_dados     TYPE zsde0185,    "*-US191846-01.10.2025-#191846-JT-inicio
        lc_retorno   TYPE zmmt0154_t,  "*-US191846-01.10.2025-#191846-JT-inicio
        wc_retorno   TYPE zmmt0154.    "*-US191846-01.10.2025-#191846-JT-inicio

*  "==================Inicio BUG IMPEDITIVO 77627 / Anderson Oenning
*  DATA: VL_NOT_VALIDA    TYPE C,
*       P_IVA            TYPE   J_1BTXSDC_,
*       VL_ICMS          TYPE ZIB_NFE_DIST_TER-VL_ICMS_TOTAL,
*       VL_DIF           TYPE WRBTR,
*       V_TOLERANCIA_DIF TYPE ZFIT0141-TOLERANCIA,
*        vl_msg        TYPE string,
*        vl_msg_01     TYPE string,
*        vl_msg_02     TYPE string,
*        vl_series     TYPE ZIB_NFE_DIST_TER-serie.
*
*
* IF SY-TCODE EQ 'MBSU'.
*
*     P_IVA = wa_nf_lin-MWSKZ.
*
**Realizar a primeira seleção
*   SELECT SINGLE * INTO @DATA(WL_ZFIWRT0027)
*        FROM ZFIWRT0027
*       WHERE BUKRS   =  @wa_nf_doc-BUKRS
*        AND LIFNR    =  @wa_nf_doc-PARID
*        AND TAXCODE  =  @P_IVA.
*
*   IF SY-SUBRC = 0.
*     VL_NOT_VALIDA = 'X'.
*   ELSE.
**Realizar a segunda seleção.
*     CLEAR:WL_ZFIWRT0027.
*     SELECT SINGLE *
*       FROM ZFIWRT0027 INTO WL_ZFIWRT0027
*      WHERE BUKRS  =  wa_nf_doc-BUKRS
*        AND LIFNR  =  wa_nf_doc-PARID
*        and TAXCODE  = ' '.
*
*     IF SY-SUBRC = 0.
*       VL_NOT_VALIDA = 'X'.
*     ELSE.
** Realizar a terceira seleção
*       CLEAR:WL_ZFIWRT0027.
*       SELECT SINGLE *
*         FROM ZFIWRT0027 INTO WL_ZFIWRT0027
*         WHERE BUKRS  =  wa_nf_doc-BUKRS
*         AND LIFNR    =  ' '
*         AND TAXCODE  =  ' '.
*       IF SY-SUBRC = 0.
*         VL_NOT_VALIDA = 'X'.
*       ENDIF.
*     ENDIF.
*   ENDIF.
*
*   IF VL_NOT_VALIDA IS INITIAL.
*
*     SELECT SINGLE STCD1
*       FROM LFA1
*       INTO @DATA(CNPJ_FORN)
*       WHERE LIFNR EQ @wa_nf_doc-PARID.
*
*
* CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wa_nf_doc-SERIES
*    IMPORTING
*      output = vl_series.
*
*     SELECT * FROM ZIB_NFE_DIST_TER
*       INTO TABLE @DATA(IT_ZIB_NFE_DIST_TER)
*        WHERE FORNE_CNPJ = @CNPJ_FORN AND
*              NUMERO = @wa_nf_doc-NFENUM AND
*              SERIE = @vl_series AND
*              DT_EMISSAO = @wa_nf_doc-DOCDAT.
*
*     IF SY-SUBRC = 0.
*       if wa_nf_stx[] is not INITIAL.
*         LOOP AT wa_nf_stx[] INTO DATA(W_LITAX) WHERE TAXTYP(3) = 'ICM'.
*          VL_ICMS = VL_ICMS + W_LITAX-TAXVAL.
*         ENDLOOP.
*
*         READ TABLE IT_ZIB_NFE_DIST_TER INTO DATA(W_ZIB_NFE_DIST_TER) INDEX 1.
*
*
*        IF VL_ICMS <> W_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL.
*          VL_DIF = VL_ICMS - W_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL.
*          VL_DIF = ABS( VL_DIF ).
*
*           SELECT SINGLE *
*             FROM ZFIT0141 INTO @DATA(WL_0141)
*             WHERE CHAVE = @W_ZIB_NFE_DIST_TER-CHAVE_NFE.
*
*          IF ( SY-SUBRC = 0 ) AND ( WL_0141-TOLERANCIA > 0 ).
*            V_TOLERANCIA_DIF = WL_0141-TOLERANCIA.
*            ENDIF.
*
*          IF ( VL_DIF > V_TOLERANCIA_DIF ) and VL_DIF > 1.
*
*             vl_msg_01 = w_zib_nfe_dist_ter-vl_icms_total.
*              vl_msg_02 = vl_dif.
*               CONCATENATE 'Valor do ICMS da MIRO diferente do valor do ICMS do XML!'
*                        'Valor XML:' vl_msg_01 '/ Diferença:'
*                             vl_msg_02 INTO vl_msg SEPARATED BY space.
*               MESSAGE vl_msg TYPE 'E'." RAISING error.
*           ENDIF.
*         ENDIF.
*       endif.
*     ENDIF.
*   ENDIF.
* ENDIF.

  "==================Fim BUG IMPEDITIVO 77627 / Anderson Oenning


*-US191846-01.10.2025-#191846-JT-inicio - COMENTADO ----------------------------
* US #154447 - MMSILVA - 02.04.2025 - Inicio
  field-symbols: <fs_bktxt> type any.
  if wa_nf_lin-xped is initial and wa_nf_doc-direct = '2'.
    assign ('(SAPMM07M)MKPF-BKTXT') to <fs_bktxt>.
    if <fs_bktxt> is assigned.
      if strlen( <fs_bktxt> ) = 10.
         wa_nf_lin-xped = <fs_bktxt>.
      endif.
    endif.
  endif.
* US #154447 - MMSILVA - 02.04.2025 - Fim
*-US191846-01.10.2025-#191846-JT-fim   - COMENTADO ----------------------------

  " CS2020000981 Criar tabela e tela para parametrização de leis fiscais pedidos transf. US 72759 - BG -- INICIO
  IF wa_nf_lin-xped IS NOT INITIAL AND wa_nf_doc-direct = '2'.
    SELECT SINGLE *
         INTO @DATA(w_ekko)
         FROM ekko
         WHERE ebeln = @wa_nf_lin-xped.

    IF w_ekko-bsart = 'ZUB'  .
      SELECT COUNT(*)
         FROM tvarvc
          WHERE name = 'MAGGI_BIODIESEL'
          AND   low  = wa_nf_lin-matnr.
      IF sy-subrc = 0.
        LOOP AT wa_nf_stx.
          wa_nf_stx-rate = 0.
          MODIFY wa_nf_stx.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF w_ekko-bsart = 'ZUB'  .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_ekko-reswk
        IMPORTING
          output = v_forn_orig.

    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_ekko-lifnr
        IMPORTING
          output = v_forn_orig.

    ENDIF.

    IF 'ZUB_ZARM_ZARS' CS w_ekko-bsart AND w_ekko-bsart is NOT INITIAL.
      SELECT SINGLE ekpo~werks
        INTO vwerks_d
        FROM ekpo
       WHERE ekpo~ebeln = wa_nf_lin-xped.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vwerks_d
        IMPORTING
          output = v_forn_dest.

      "Selecionar MBEW por BWKEY = W_EKKO-WERKS e MATNR = wa_nf_lin-matnr obter OWNPR armazenar em V_OWNPR
      SELECT SINGLE ownpr mtorg FROM mbew INTO ( v_ownpr, v_mtorg ) WHERE bwkey = wa_nf_lin-werks AND  matnr = wa_nf_lin-matnr. "US #154447 - MMSILVA - 02.04.2025 - Acrescentado "matorg" e "v_matorg"

      "selecionar MARA por  MATNR = wa_nf_lin-matnr obter MATKL e armzenar em V_MATKL
      SELECT SINGLE matkl FROM mara INTO v_matkl WHERE matnr = wa_nf_lin-matnr .

      "seleionar  LFA1 por LIFNR = V_FORN_ORIG obter REGIO e armazenar em V_REGIO_ORIG
      SELECT SINGLE regio FROM lfa1 INTO v_regio_orig WHERE lifnr = v_forn_orig.

      "seleionar  LFA1 por LIFNR = V_FORN_DEST obter REGIO e armazenar em V_REGIO_DEST
      SELECT SINGLE regio FROM lfa1 INTO v_regio_dest WHERE lifnr = v_forn_dest.

      IF 'ZARM_ZARS' CS w_ekko-bsart.
        "seleionar  LFA1 por LIFNR = V_FORN_ORIG obter REGIO e armazenar em V_REGIO_ORIG
        SELECT SINGLE regio FROM lfa1 INTO v_regio_orig WHERE lifnr = v_forn_dest.

        "seleionar  LFA1 por LIFNR = V_FORN_DEST obter REGIO e armazenar em V_REGIO_DEST
        SELECT SINGLE regio FROM lfa1 INTO v_regio_dest WHERE lifnr = v_forn_orig.
      ENDIF.

      DATA(lv_matnr)  = wa_nf_lin-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = wa_nf_lin-matnr
        IMPORTING
          output = lv_matnr.

*-US191846-01.10.2025-#191846-JT-inicio
*      select single * from  zmmt0154 into wa_zmmt0154 where
*                bsart       = w_ekko-bsart
*                and uf_orig   =  v_regio_orig
*                and uf_dest   =  v_regio_dest
*                and ownpr    = v_ownpr
*                and matnr     =  lv_matnr
*                and mtorg     = v_mtorg. "US #154447 - MMSILVA - 02.04.2025
** ----> US #154447 - MMSILVA - 02.04.2025 - Inicio - Caso MTORG esteja vazio na tabela
*      if sy-subrc is not initial.
*        select single * from  zmmt0154 into wa_zmmt0154
*          where bsart   = w_ekko-bsart
*          and uf_orig   = v_regio_orig
*          and uf_dest   = v_regio_dest
*          and ownpr     = v_ownpr
*          and matnr     = lv_matnr.
** ----> US #154447 - MMSILVA - 02.04.2025 - Fim - Caso MTORG esteja vazio na tabela
*        if sy-subrc is not initial.
*          select single * from  zmmt0154 into wa_zmmt0154 where
*                        bsart       = w_ekko-bsart
*                        and uf_orig   =  v_regio_orig
*                        and uf_dest   =  v_regio_dest
*                        and ownpr    =  v_ownpr
*                        and matkl      = v_matkl
*                        and mtorg      = v_mtorg. "US #154447 - MMSILVA - 02.04.2025
** --------> US #154447 - MMSILVA - 02.04.2025 - Inicio - Caso MTORG esteja vazio na tabela
*          if sy-subrc is not initial.
*            select single * from  zmmt0154 into wa_zmmt0154
*              where bsart   = w_ekko-bsart
*              and uf_orig   = v_regio_orig
*              and uf_dest   = v_regio_dest
*              and ownpr     = v_ownpr
*              and matkl     = v_matkl.
** --------> US #154447 - MMSILVA - 02.04.2025 - Fim - Caso MTORG esteja vazio na tabela
*            if sy-subrc is not initial.
*              concatenate 'Tipo de pedido sem parâmetro fiscal. Por gentileza criar uma FI ao departamento fiscal CHAVE: ' w_ekko-bsart   v_regio_orig  v_regio_dest  v_ownpr   wa_nf_lin-matnr into data(mensagem).
*              message: mensagem type 'E'.
*              "CHAVE: W_EKKO-BSART +  V_REGIO_ORIG + V_REGIO_DEST + V_OWNPR  + wa_nf_lin-matnr.
*            endif.
*
*          endif.
*
*          if wa_zmmt0154 is not initial.
*            wa_nf_lin-taxlw1 = wa_zmmt0154-j_1btaxlw1.
*            wa_nf_lin-taxlw2 = wa_zmmt0154-j_1btaxlw2.
*            wa_nf_lin-taxlw4 = wa_zmmt0154-j_1btaxlw4.
*            wa_nf_lin-taxlw5 = wa_zmmt0154-j_1btaxlw5.
*            wa_nf_lin-cfop       = wa_zmmt0154-cfop.
*            wa_nf_lin-mwskz       = wa_zmmt0154-mwskz.
*
*          endif.
*        endif.
*      endif.

      CLEAR: lc_dados, wa_zmmt0154.
      lc_dados-bsart-valor   = w_ekko-bsart.
      lc_dados-uf_orig-valor = v_regio_orig.
      lc_dados-uf_dest-valor = v_regio_dest.
      lc_dados-matnr-valor   = lv_matnr.
      lc_dados-werks-valor   = wa_nf_lin-werks.
      lc_dados-ownpr-valor   = v_ownpr.
      lc_dados-mtorg-valor   = v_mtorg.
      lc_dados-direcao-valor = '2'. "saida

      lc_retorno = zcl_leis_fiscais=>get_impostos( i_dados = lc_dados i_todos = abap_false ).

      READ TABLE lc_retorno INTO wa_zmmt0154 INDEX 1.

      IF NOT ( sy-subrc EQ 0 AND wa_zmmt0154-mwskz IS NOT INITIAL ).

        DATA: lva_msg_error TYPE string.

        lva_msg_error = |Não encontrado parametros com IVA na ZMM0185 para o tipo Pedido: { w_ekko-bsart } Origem: { v_regio_orig }|.
        lva_msg_error = |{ lva_msg_error } Destino: { v_regio_dest } Material: { lv_matnr } Produção Interna: { v_ownpr } Origem Material: { v_mtorg } Direção: Saida!|.
        lva_msg_error = |{ lva_msg_error } Criar FI para Departamento Fiscal|.
        MESSAGE lva_msg_error TYPE 'E'.

        "CONCATENATE 'Tipo de pedido sem parâmetro fiscal. Por gentileza criar uma FI ao departamento fiscal CHAVE: ' w_ekko-bsart   v_regio_orig  v_regio_dest  v_ownpr   wa_nf_lin-matnr INTO DATA(mensagem).
        "MESSAGE: mensagem TYPE 'E'.
      ELSE.
        wa_nf_lin-taxlw1 = wa_zmmt0154-j_1btaxlw1.
        wa_nf_lin-taxlw2 = wa_zmmt0154-j_1btaxlw2.
        wa_nf_lin-taxlw4 = wa_zmmt0154-j_1btaxlw4.
        wa_nf_lin-taxlw5 = wa_zmmt0154-j_1btaxlw5.
        wa_nf_lin-cfop   = wa_zmmt0154-cfop.
        wa_nf_lin-mwskz  = wa_zmmt0154-mwskz.
      ENDIF.
*-US191846-01.10.2025-#191846-JT-fim

    ENDIF.
  ENDIF.

*  IF wa_nf_lin-werks+0(2) = '15' and wa_nf_lin-MWSKZ = 'ZA' and wa_nf_lin-TAXLW1 = 'M51' and wa_nf_lin-XPED is not INITIAL.
*    select single bsart
*      into _bsart
*      from ekko
*      where ebeln = wa_nf_lin-XPED.
*    if _bsart = 'ZUB'  .
*       wa_nf_lin-TAXLW1 = 'IM0'.
*    endif.
*  ELSEIF wa_nf_lin-MWSKZ = 'A1' AND wa_nf_lin-XPED is not INITIAL.

* clear: WS_ekko, WS_LFA1, REGIO_E, REGIO_D, VWERKS_E,VWERKS_D.
*       SELECT SINGLE ekpo~werks, ekko~RESWK
*       INTO CORRESPONDING FIELDS OF @WS_ekko
*       from ekko
*       INNER JOIN ekpo
*       on ekpo~ebeln = ekko~ebeln
*       where ekko~ebeln = @wa_nf_lin-XPED
*       and   ekko~bsart = 'ZUB'.

*     if sy-subrc = 0.
*     " BUG 59101 - AOENNING - 31/05/2021.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WS_ekko-RESWK
*      IMPORTING
*        OUTPUT = VWERKS_e.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WS_ekko-werks
*      IMPORTING
*        OUTPUT = VWERKS_d.
*     select SINGLE REGIO FROM LFA1 INTO REGIO_E WHERE LIFNR EQ VWERKS_e.
*     select SINGLE REGIO FROM LFA1 INTO REGIO_D WHERE LIFNR EQ VWERKS_d.
*
*     if REGIO_E ne  REGIO_D.
*       vg_setname = 'MAGGI_CFOP_PEDTRANSF'. "
*     else.
*       vg_setname = 'MAGGI_CFOP_PEDTRANSF2'.
*     endif.
*     SELECT SINGLE *
*        FROM SETLEAF
*        INTO W_SETLEAF
*        WHERE SETNAME = vg_setname
*        and   valfrom = WS_ekko-werks.
*     if sy-subrc = 0.
*       SELECT SINGLE *
*            FROM SETLINET
*            INTO W_SETLINET
*            WHERE SETNAME = vg_setname
*            and   LINEID  = W_SETLEAF-LINEID.
*         SEARCH W_SETLINET-DESCRIPT FOR 'ICMS:'.
*         IF sy-subrc = 0.
*            pos = sy-fdpos + 5.
*            wa_nf_lin-TAXLW1 = W_SETLINET-DESCRIPT+pos(3).
*        ENDIF.
*     endif.
*     endif.
*  ELSEIF wa_nf_lin-XPED is not INITIAL and wa_nf_doc-direct = '1'.
*    SELECT SINGLE ekpo~werks
*      INTO @DATA(V_WERKS2)
*      from ekko
*      INNER JOIN ekpo
*      on ekpo~ebeln = ekko~ebeln
*      where ekko~ebeln = @wa_nf_lin-XPED
*      and   ekko~bsart = 'ZUB'.
*
*     if sy-subrc = 0.
*        SELECT SINGLE *
*            FROM SETLEAF
*            INTO W_SETLEAF
*            WHERE SETNAME = 'MAGGI_CFOP_PEDENTRA'
*            and   valfrom = v_werks2.
*        if sy-subrc = 0.
*           SELECT SINGLE *
*                FROM SETLINET
*                INTO W_SETLINET
*                WHERE SETNAME = 'MAGGI_CFOP_PEDENTRA'
*                and   LINEID  = W_SETLEAF-LINEID.
*             SEARCH W_SETLINET-DESCRIPT FOR 'ICMS:'.
*             IF sy-subrc = 0.
*                pos = sy-fdpos + 5.
*                wa_nf_lin-TAXLW1 = W_SETLINET-DESCRIPT+pos(3).
*            ENDIF.
*            "
*            SEARCH W_SETLINET-DESCRIPT FOR 'CFOP:'.
*            IF sy-subrc = 0.
*               pos = sy-fdpos + 5.
*               wa_nf_lin-cfop = W_SETLINET-DESCRIPT+pos(6).
*            ENDIF.
*        endif.
*     endif.
*  ENDIF.
  " CS2020000981 Criar tabela e tela para parametrização de leis fiscais pedidos transf. US 72759 - BG -- FIM
ENDENHANCEMENT.
