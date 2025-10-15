
TYPES: BEGIN OF TY_SKAT,
        SAKNR TYPE SKAT-SAKNR,
        TXT50 TYPE SKAT-TXT50,
       END OF TY_SKAT,

       BEGIN OF TY_039,
        CODIGO     TYPE ZGLT039-CODIGO,
        COD_NOTA   TYPE ZGLT039-COD_NOTA,
        DESCR      TYPE ZGLT039-DESCR,
        DESCR_NOTA TYPE ZGLT039-DESCR_NOTA,
       END OF TY_039,

       BEGIN OF ty_tabela,
         tipo_tab,
         data            type ZGLT044-BLDAT,
         belnr           type ZGLT045-belnr,
         CODIGO          TYPE CHAR10,
         DESCR           TYPE ZGLT044-DESCR,
         DMBTR           TYPE ZGLT044-DMBTR,
         DMBE2           TYPE ZGLT044-DMBE2,
         DMBE3           TYPE ZGLT044-DMBE3,
         TIPO            TYPE CHAR1,
         CONTRA          TYPE ZGLT044-CTA_CONTRA_PART,
         DT_VCTO         type ZGLT045-DT_VCTO,
         DT_AJUSTE       type ZGLT045-DT_AJUSTE,
         NRO_DOC_AJ      type ZGLT045-NRO_DOC_AJ,
         OBSERV          type ZGLT045-OBSERV,
         CTA_CONTRA_PART TYPE ZGLT045-CTA_CONTRA_PART,
       END OF ty_tabela,

       BEGIN OF TY_LINHA,
         TEXTO(216),
       END OF TY_LINHA.

     TYPES: TY_T_TABELA TYPE TABLE OF ty_tabela,
            TY_T_LINHAS TYPE TABLE OF TY_LINHA.








