"Name: \FU:J_1B_IM_NF_DOCUMENT_GENERATE\SE:END\EI
ENHANCEMENT 0 Z_PEDIDO_MBSU.

  data: W_SETLEAF              TYPE SETLEAF,
        W_SETLINET             TYPE SETLINET,
        vg_setname             TYPE setleaf-setname,
        V_BASE                 type J_1BNFSTX-base,
*        WS_ekko                TYPE ty_ekko,
        WS_LFA1                TYPE LFA1,
        REGIO_E                TYPE LFA1-regio,
        REGIO_D                TYPE LFA1-regio,
        VWERKS_e               TYPE LFA1-lifnr,
        VWERKS_d               TYPE LFA1-lifnr,
        pos                    type i,
        V_FORN_ORIG            TYPE lfa1-lifnr,
        V_FORN_DEST            TYPE lfa1-lifnr,
        V_OWNPR                TYPE MBEW-OWNPR,
        V_MATKL                TYPE MARA-MATKL,
        V_REGIO_ORIG           TYPE LFA1-REGIO,
        lv_xblnr               type xblnr1,
        V_REGIO_DEST           TYPE LFA1-REGIO,
        LV_IV_ICMS             TYPE C.


  "==================Inicio BUG IMPEDITIVO 77627 / Anderson Oenning
  DATA: VL_NOT_VALIDA    TYPE C,
       P_IVA            TYPE   J_1BTXSDC_,
       VL_ICMS          TYPE ZIB_NFE_DIST_TER-VL_ICMS_TOTAL,
       VL_DIF           TYPE WRBTR,
       V_TOLERANCIA_DIF TYPE ZFIT0141-TOLERANCIA,
        vl_msg        TYPE string,
        vl_msg_01     TYPE string,
        vl_msg_02     TYPE string,
        vl_series     TYPE ZIB_NFE_DIST_TER-serie.

  DATA V835 TYPE C.
  FIELD-SYMBOLS: <FS_XMSEG_BWART> TYPE ANY.
  ASSIGN ('(SAPLMBWL)XMSEG-BWART') TO <FS_XMSEG_BWART>.

  IF ( <FS_XMSEG_BWART> IS ASSIGNED ).
    IF <FS_XMSEG_BWART> = '835'.
      V835 = 'X'.
    ENDIF.
  ENDIF.


  IF ( SY-TCODE EQ 'MBSU' OR
     SY-TCODE EQ 'MB0A' OR
     SY-TCODE EQ 'MIGO' OR V835 = 'X' ) and wa_nf_doc-direct = '1'.

    CLEAR: P_IVA, VL_ICMS.

    P_IVA = wa_nf_lin-MWSKZ.

    LOOP AT wa_nf_stx[] INTO DATA(W_LITAX) WHERE TAXTYP(3) = 'ICM'.
      ADD W_LITAX-TAXVAL TO VL_ICMS.
    ENDLOOP.

    CONCATENATE wa_nf_doc-nfenum '-'  wa_nf_doc-SERIES into lv_xblnr.

    LV_IV_ICMS = 'S'.
    EXPORT LV_IV_ICMS TO MEMORY ID 'ZIV_ICMS'.
    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        p_lifnr      = wa_nf_doc-PARID
        p_parvw      = WA_NF_DOC-PARVW
        p_nftype     = wa_nf_doc-nftype
        p_xblnr      = lv_xblnr
        p_data       = wa_nf_doc-DOCDAT
        p_werks      = wa_nf_doc-branch
        p_valor_icms = VL_ICMS
        p_iva        = P_IVA
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
 ENDIF.


* IF SY-TCODE EQ 'MBSU' or
*    SY-TCODE EQ 'MB0A'.
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
**     SELECT SINGLE STCD1
**       FROM LFA1
**       INTO @DATA(CNPJ_FORN)
**       WHERE LIFNR EQ @wa_nf_doc-PARID.
*
* CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wa_nf_doc-SERIES
*    IMPORTING
*      output = vl_series.
*
*     SELECT * FROM ZIB_NFE_DIST_TER
*       INTO TABLE @DATA(IT_ZIB_NFE_DIST_TER)
*        WHERE FORNE_CNPJ = @wa_nf_doc-cgc AND
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
ENDENHANCEMENT.
