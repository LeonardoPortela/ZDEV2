"Name: \FU:J_1B_NF_IV_SUMMING\SE:END\EI
ENHANCEMENT 0 Z_VERIFICA_ICMS_XML.


DATA: VL_NOT_VALIDA    TYPE C,
      P_IVA            TYPE   J_1BTXSDC_,
      VL_ICMS          TYPE ZIB_NFE_DIST_TER-VL_ICMS_TOTAL,
      VL_DIF           TYPE WRBTR,
      V_TOLERANCIA_DIF TYPE ZFIT0141-TOLERANCIA,
      vl_msg          TYPE string,
      vl_msg_01     TYPE string,
      vl_msg_02     TYPE string,
      lv_xblnr      TYPE  xblnr1,
      vl_series     TYPE ZIB_NFE_DIST_TER-serie,
      LV_IV_ICMS    TYPE C.


 "IF J_1BAA-FORM IS INITIAL and RBKPV-TCODE <> 'MR8M'.
 IF J_1BAA-FORM  IS INITIAL and
    I_HEADER-nfenum  is not initial AND
    ( SY-tcode <> 'MR8M' AND RBKPV-TCODE <> 'MR8M' ).

    CLEAR: P_IVA, VL_ICMS.
    LV_IV_ICMS = 'S'.
    READ TABLE i_item INTO DATA(W_ITEM) INDEX 1.
    IF SY-SUBRC = 0.
      P_IVA = W_ITEM-MWSKZ.
    ENDIF.

    LOOP AT LITAX[] INTO DATA(W_LITAX) WHERE TAXTYP(3) = 'ICM'.
      VL_ICMS = VL_ICMS + W_LITAX-TAXVAL.
    ENDLOOP.
    EXPORT LV_IV_ICMS TO MEMORY ID 'ZIV_ICMS'.
    CONCATENATE I_HEADER-nfenum '-'  I_HEADER-SERIES into lv_xblnr.

    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        p_lifnr      = I_HEADER-PARID
        p_nftype     = I_HEADER-nftype
        p_xblnr      = lv_xblnr
        p_data       = I_HEADER-DOCDAT
        p_werks      = I_HEADER-branch
        p_valor_icms = VL_ICMS
        p_iva        = P_IVA
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

 endif.
*
* IF J_1BAA-FORM IS INITIAL and RBKPV-TCODE <> 'MR8M'.
*
*
*
*   CLEAR: vl_not_valida.
*   READ TABLE i_item INTO DATA(W_ITEM) INDEX 1.
*   IF SY-SUBRC = 0.
*     P_IVA = W_ITEM-MWSKZ.
*   ENDIF.
*
**Realizar a primeira seleção
*   SELECT SINGLE * INTO @DATA(WL_ZFIWRT0027)
*        FROM ZFIWRT0027
*       WHERE BUKRS   =  @I_HEADER-BUKRS
*        AND LIFNR    =  @I_HEADER-PARID
*        AND TAXCODE  =  @P_IVA.
*
*   IF SY-SUBRC = 0.
*     VL_NOT_VALIDA = 'X'.
*   ELSE.
**Realizar a segunda seleção.
*     SELECT SINGLE *
*       FROM ZFIWRT0027 INTO WL_ZFIWRT0027
*      WHERE BUKRS  =  I_HEADER-BUKRS
*        AND LIFNR  =  I_HEADER-PARID
*        and TAXCODE  = ' '.
*
*     IF SY-SUBRC = 0.
*       VL_NOT_VALIDA = 'X'.
*     ELSE.
** Realizar a terceira seleção
*       SELECT SINGLE *
*         FROM ZFIWRT0027 INTO WL_ZFIWRT0027
*         WHERE BUKRS  =  I_HEADER-BUKRS
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
*       WHERE LIFNR EQ @I_HEADER-PARID.
*
*
* CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = I_HEADER-SERIES
*    IMPORTING
*      output = vl_series.
*
*     SELECT * FROM ZIB_NFE_DIST_TER
*       INTO TABLE @DATA(IT_ZIB_NFE_DIST_TER)
*        WHERE FORNE_CNPJ = @CNPJ_FORN AND
*              NUMERO = @I_HEADER-NFENUM AND
*              SERIE = @vl_series AND
*              DT_EMISSAO = @I_HEADER-DOCDAT.
*
*     IF SY-SUBRC = 0.
*       if LITAX[] is not INITIAL.
*         LOOP AT LITAX[] INTO DATA(W_LITAX) WHERE TAXTYP(3) = 'ICM'.
*
*          VL_ICMS = VL_ICMS + W_LITAX-TAXVAL.
*
*         ENDLOOP.
*
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

            " CS2021000618 US - 68327 - Fim   - BG

ENDENHANCEMENT.
