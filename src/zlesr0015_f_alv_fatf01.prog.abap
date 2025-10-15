*----------------------------------------------------------------------*
***INCLUDE ZLESR0015_F_ALV_FATF01 .
*----------------------------------------------------------------------*
INCLUDE ZLESR0015_F_ALV_FATF01.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_NR_NF  text
*      -->P_WA_SAIDA_CNPJ  text
*----------------------------------------------------------------------*
form F_SELECIONA_DADOS2  using   p_nr_nf
                                 p_cnpj.

   data: tl_zlest0019     type table of zlest0019,
       sl_zlest0019     type zlest0019         ,
       sl_zlest0004     type zlest0004         ,
       tl_zlest0004     type table of zlest0004,
       IT_J_1BNFLIN_aux type table of J_1BNFLIN,
       it_zlest0035_aux type table of ty_zlest0035.


  refresh: it_zlest0006,
           it_zlest0004,
           it_zlest0019,
           it_zlest0003,
           it_zlest0035.


   SELECT Z~SERIE_DESPACHO
          Z~NR_DESPACHO
          Z~NR_NF
          E~CGC_REMETENTE
     FROM ZLEST0003 AS Z
     INNER JOIN ZLEST0004 AS E ON Z~SERIE_DESPACHO EQ E~SERIE_DESPACHO AND Z~NR_DESPACHO EQ  E~NR_DESPACHO
     INTO TABLE IT_ZLEST0003
     FOR ALL ENTRIES IN TL_ZLEST0004
   WHERE Z~SERIE_DESPACHO EQ TL_ZLEST0004-SERIE_DESPACHO
     AND Z~NR_DESPACHO    EQ TL_ZLEST0004-NR_DESPACHO
     AND Z~nr_nf          eq p_nr_nf
     AND E~CGC_REMETENTE  eq p_cnpj.


  SELECT *
    FROM ZLEST0004
    INTO TABLE IT_ZLEST0004
    FOR ALL ENTRIES IN IT_ZLEST0003 "IT_ZLEST0006
  WHERE SERIE_DESPACHO EQ IT_ZLEST0003-SERIE_DESPACHO
    AND NR_DESPACHO    EQ IT_ZLEST0003-NR_DESPACHO.

    "NR_FATURA   EQ IT_ZLEST0006-NR_FATURA.

  SELECT *
    FROM ZLEST0006
    INTO TABLE IT_ZLEST0006
    FOR ALL ENTRIES IN IT_ZLEST0004 "IT_ZLEST0006
  WHERE NR_FATURA EQ IT_ZLEST0004-NR_FATURA.

endform.                    " F_SELECIONA_DADOS2
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_SAIDA_FAT .

  SORT: it_zlest0006    BY nr_fatura     ,
        it_zlest0004    BY nr_fatura     ,
        it_zlest0003    BY SERIE_DESPACHO NR_DESPACHO,
        it_zlest0035    BY nr_nf cnpj    ,
        it_t001w        BY werks         ,
        IT_J_1BNFDOC    BY DOCNUM        ,
        IT_J_1BNFLIN    BY DOCNUM        ,
        IT_MAKT         BY MATNR         ,
        it_parceiro     BY vbeln         ,
        it_lfa1         BY lifnr         .

  LOOP AT  it_zlest0006 INTO wa_zlest0006.

    LOOP AT it_zlest0004 INTO wa_zlest0004 WHERE nr_fatura = wa_zlest0006-nr_fatura.

      LOOP AT it_zlest0003 INTO wa_zlest0003 WHERE SERIE_DESPACHO = WA_ZLEST0004-SERIE_DESPACHO AND NR_DESPACHO = WA_ZLEST0004-NR_DESPACHO.

         wa_saida2-NR_NF_ALL      =  WA_ZLEST0006-NR_NF_ALL.
         wa_saida2-NR_FATURA      =  WA_ZLEST0006-NR_FATURA.
         wa_saida2-serie_despacho =  WA_ZLEST0004-serie_despacho.
         wa_saida2-nr_despacho    =  WA_ZLEST0004-nr_despacho.

        APPEND wa_saida2 TO it_saida2.

        CLEAR: wa_saida    ,
               wa_zlest0003.

      ENDLOOP.

      CLEAR: wa_zlest0004.

    ENDLOOP.

    CLEAR: wa_zlest0006.

  ENDLOOP.

endform.                    " F_SAIDA_FAT




*&---------------------------------------------------------------------*
*&      Form  F_ALV_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_ALV_FAT .

endform.                    " F_ALV_FAT
