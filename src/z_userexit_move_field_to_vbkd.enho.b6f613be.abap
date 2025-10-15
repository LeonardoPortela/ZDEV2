"Name: \PR:SAPMV45A\FO:USEREXIT_MOVE_FIELD_TO_VBKD\SE:BEGIN\EI
ENHANCEMENT 0 Z_USEREXIT_MOVE_FIELD_TO_VBKD.
   DATA ERRO TYPE C.

  DATA:   BEGIN OF VBKD_IT OCCURS 125.
            INCLUDE STRUCTURE VBKDVB.
          DATA:   END OF VBKD_IT.

  DATA: VBKD_ST         TYPE VBKDVB.
  DATA: WA_0001         TYPE ZSDT0001.

  FIELD-SYMBOLS: <ICON1>   TYPE ANY,
                 <ICON2>   TYPE ANY,
                 <VBKD_FS> TYPE VBKDVB.

  "CSB
  DATA: VL_LIFNR TYPE VBPA-LIFNR,
        TG_VBPA  TYPE TABLE OF VBPA,
        ST_VBPA  TYPE VBPA.

  CLEAR: ST_VBPA, VL_LIFNR.

  REFRESH TG_VBPA.

  CASE SY-TCODE.
    WHEN 'VA02' OR
         'ZSDT0087' OR
         'ZSDT0081'.   "// US-169490 WBARBOSA 24/07/2025

      IF  ( ( VBAK-AUART  EQ 'ZEXP' ) OR  ( VBAK-AUART  EQ 'ZEXI' ) OR ( VBAK-AUART  EQ 'ZEXD' ) ).

        SELECT SINGLE LIFNR
          FROM VBPA
          INTO VL_LIFNR
        WHERE  PARVW EQ 'Z1'
          AND VBELN EQ VBAK-VBELN.

        IF VL_LIFNR IS NOT INITIAL.

          TG_VBPA[] = XVBPA[].

          READ TABLE TG_VBPA INTO ST_VBPA
                     WITH KEY PARVW = 'Z1'
                      LIFNR = VL_LIFNR.

          IF SY-SUBRC <> 0.
            MESSAGE E899 WITH 'Parceiro não pode ser alterado!'.
          ENDIF.

        ENDIF.

      ENDIF.

*      Diferente da Empresa da Argentina
      IF VBAK-VKORG NE '0100'.

        ASSIGN ('(SAPMV45A)VBKD-INCO1') TO <ICON1>.
        ASSIGN ('(SAPMV45A)VBKD-INCO2') TO <ICON2>.

        IF XVBKD-INCO1 NE <ICON1>
        OR XVBKD-INCO2 NE <ICON2>.
*     Verifica se existe romaneio para o Ordem de Venda
          IF VBAK-VBELN IS NOT INITIAL.
            SELECT SINGLE * FROM ZSDT0001 INTO WA_0001 WHERE VBELN EQ VBAK-VBELN.
            IF SY-SUBRC IS INITIAL.
              MESSAGE E899 WITH 'Incoterms não pode ser alterado.' ' Ordem já Vinculada à Romaneio!'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR ERRO.
      ERRO = ZCL_SOLICITACAO_OV=>DOC_SUBSEQUENTE( I_VBELN = VBAK-VBELN
                                                  I_UCOMM = SY-UCOMM ).
      IF ERRO IS NOT INITIAL.
        MESSAGE I836(SD) WITH 'Item não pode ser eliminado, existem Doc. contábeis vinculados!'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.

  ENDCASE.
  "FIM CSB
*  VBKD-zzfield = xxxx-zzfield2.
ENDENHANCEMENT.
