"Name: \PR:SAPMV45A\FO:USEREXIT_MOVE_FIELD_TO_VBKD\SE:BEGIN\EI
ENHANCEMENT 0 ZVA02_INCO1_BLOCK.

******DATA ERRO TYPE C.
******
******CASE SY-TCODE.
******    WHEN: 'VA02'.
******
******        DATA:   BEGIN OF VBKD_IT OCCURS 125.
******                INCLUDE STRUCTURE VBKDVB.
******        DATA:   END OF VBKD_IT.
******
******        DATA: VBKD_ST         TYPE VBKDVB.
******        DATA: WA_0001         TYPE ZSDT0001.
******
******      FIELD-SYMBOLS: <ICON1>    TYPE ANY,
******                     <ICON2>    TYPE ANY,
******                     <VBKD_FS>  TYPE VBKDVB.
******
******* Diferente da Empresa da Argentina
******IF VBAK-VKORG NE '0100'.
******
******      ASSIGN ('(SAPMV45A)VBKD-INCO1') TO <ICON1>.
******      ASSIGN ('(SAPMV45A)VBKD-INCO2') TO <ICON2>.
******
******      IF XVBKD-INCO1 NE <ICON1>
******      OR XVBKD-INCO2 NE <ICON2>.
*******     Verifica se existe romaneio para o Ordem de Venda
******        SELECT SINGLE * FROM ZSDT0001 INTO WA_0001 WHERE VBELN EQ VBAK-VBELN.
******          IF SY-SUBRC IS INITIAL.
******             MESSAGE E899 WITH 'Incoterms não pode ser alterado.' ' Ordem já Vinculada à Romaneio!'.
******          ENDIF.
******      ENDIF.
******ENDIF.
******
******CLEAR ERRO.
******ERRO = ZCL_SOLICITACAO_OV=>DOC_SUBSEQUENTE( I_VBELN = VBAK-VBELN
******                                            I_UCOMM = SY-UCOMM ).
******IF ERRO IS NOT INITIAL.
******  MESSAGE I836(SD) WITH 'Item não pode ser eliminado, existem Doc. contábeis vinculados!'.
******  LEAVE TO CURRENT TRANSACTION.
******ENDIF.
******
******ENDCASE.
"    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- INICIO

*      DATA:   BEGIN OF TG_VBAP OCCURS 125.
*                INCLUDE STRUCTURE VBAPVB.
*      DATA:   END OF TG_VBAP.
*
*      DATA: ST_VBAP TYPE VBAPVB,
*            V_MTART TYPE MTART,
*            V_MSG   TYPE sTRING,
*            v_tamanho TYPE i,
*            v_contador TYPE i,
*            VL_TXT01(50) TYPE C,
*              VL_TXT02(50) TYPE C,
*              VL_TXT03(50) TYPE C,
*              VL_TXT04(50) TYPE C.
*
*      REFRESH: TG_VBAP.
*      TG_VBAP[] = XVBAP[].
*
*      LOOP AT TG_VBAP INTO DATA(WA_VBAP).
*
*        SELECT SINGLE MTART FROM MARA INTO V_MTART WHERE MATNR EQ WA_VBAP-MATNR.
*
*        IF SY-SUBRC IS INITIAL.
*          IF V_MTART EQ 'ZFER' AND WA_VBAP-SPART EQ '02'. "Produto acabado e Fertilizante
*
*            CALL FUNCTION 'ZSD_VALIDACOES_OV'
*              EXPORTING
*                I_MATNR    = WA_VBAP-MATNR
*                I_WERKS    = WA_VBAP-WERKS
*              IMPORTING
*                E_MENSAGEM = V_MSG.
*
*            IF V_MSG IS NOT INITIAL.
*              v_TAMANHO = STRLEN( V_MSG ).
*              VL_TXT01 = V_MSG(50).
*              VL_TXT02 = V_MSG+50(50).
*              V_TAMANHO = V_TAMANHO - 100.
*              IF V_TAMANHO > 50.
*                VL_TXT03 = V_MSG+100(50).
*                V_TAMANHO = V_TAMANHO - 50.
*                VL_TXT04 = V_MSG+150(V_TAMANHO).
*              ELSE.
*                VL_TXT03 = V_MSG+100(V_TAMANHO).
*              ENDIF.
*              MESSAGE E836(SD) WITH VL_TXT01 VL_TXT02 VL_TXT03 VL_TXT04.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- FIM


ENDENHANCEMENT.
