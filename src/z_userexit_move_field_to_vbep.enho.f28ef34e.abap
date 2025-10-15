"Name: \PR:SAPMV45A\FO:USEREXIT_MOVE_FIELD_TO_VBEP\SE:BEGIN\EI
ENHANCEMENT 0 Z_USEREXIT_MOVE_FIELD_TO_VBEP.

*  VBEP-zzfield = xxxx-zzfield2.
* -CSB-
* Chamado 129257 - 15.09.2014 - Ao entrar na tela carrega os dados
*verificar se teve alteções nos campos abaixo

  CASE SY-TCODE.
    WHEN: 'VA02'.

      IF  ( ( VBAK-AUART  EQ 'ZEXP' ) OR ( VBAK-AUART  EQ 'ZEXI' ) OR ( VBAK-AUART  EQ 'ZEXD' ) ).

        DATA: W_CAMPO(40),
              IT_TABELA    TYPE TABLE OF VBAPVB,
              WA_IT_TABELA TYPE  VBAPVB.


        FIELD-SYMBOLS: <FS_MATNR>   TYPE ANY,
                       <FS_KWMENG>  TYPE ANY,
                       <FS_WERKS>   TYPE ANY,
                       <FS_LGORT>   TYPE ANY,
                       <FS_CHARG>   TYPE ANY,
                       <FS_PARTNER> TYPE ANY,
                       <FS_VBAP>    TYPE VBAPVB.

*        ASSIGN ('(SAPMV45A)RV45A-KWMENG') TO <FS_KWMENG>. "(QUANTIDADE) --VBEP
        ASSIGN ('(SAPMV45A)VBEP-WMENG') TO <FS_KWMENG>. "(QUANTIDADE) --VBEP

        DATA:   BEGIN OF TG_VBAP OCCURS 125.
                  INCLUDE STRUCTURE VBAPVB.
                DATA:   END OF TG_VBAP.

        DATA:   ST_VBAP        TYPE VBAPVB.

        REFRESH: TG_VBAP.

        TG_VBAP[] = XVBAP[].
        CLEAR ST_VBAP.

        LOOP AT TG_VBAP ASSIGNING <FS_VBAP>.

          ST_VBAP = <FS_VBAP>.

          IF ST_VBAP-KWMENG <> <FS_KWMENG>.
            MESSAGE E899 WITH 'Quantidade não pode ser alterada!'.
          ENDIF.

        ENDLOOP.
      ENDIF.

  ENDCASE.
* Fim chamado 129257

ENDENHANCEMENT.
