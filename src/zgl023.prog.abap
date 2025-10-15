*&---------------------------------------------------------------------*
*& Report  ZGL023
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zgl023.

TABLES: skb1, zglt049, t001.

TYPES: BEGIN OF ty_resumo,
         RBUKRS        TYPE FAGLFLEXT-RBUKRS,"140938 CS2024000424 ZGL029 - Rel. Notas do Imobilizado PSA
         racct         TYPE faglflext-racct,
         txt50         TYPE txt50_skat,
         saldo_inicial TYPE hslxx12,
         saldo_adicao  TYPE hslxx12,
         saldo_baixas  TYPE hslxx12,
         saldo_transf  TYPE hslxx12,
***     Inicio - ALX
         incorporacao  TYPE hslxx12,
***     Fim - ALX
         saldo_final   TYPE hslxx12,
       END OF ty_resumo,

       BEGIN OF ty_resumo_alv,
         color(4),
         rbukrs        TYPE faglflext-rbukrs,
         racct         TYPE faglflext-racct,
         txt50         TYPE txt50_skat,
         saldo_inicial TYPE hslxx12,
         saldo_adicao  TYPE hslxx12,
         saldo_baixas  TYPE hslxx12,
         saldo_transf  TYPE hslxx12,
***     Inicio - ALX
         incorporacao  TYPE hslxx12,
***     Fim - ALX
         saldo_final   TYPE hslxx12,
         celltab       TYPE lvc_t_styl,
         "CELLCOLOR      TYPE LVC_T_SCOL,
       END OF ty_resumo_alv.

DATA: it_skat       TYPE TABLE OF skat WITH HEADER LINE,
      it_faglflext  TYPE TABLE OF faglflext WITH HEADER LINE,
      it_resumo     TYPE SORTED TABLE OF ty_resumo WITH UNIQUE KEY rbukrs racct txt50, "140938 CS2024000424 ZGL029 - Rel. Notas do Imobilizado PSA rbukrs
      it_zglt053    TYPE TABLE OF zglt053 WITH HEADER LINE,
      "IT_T856T      TYPE TABLE OF T856T WITH HEADER LINE,
      wa_resumo     LIKE LINE OF it_resumo,
      it_resumo_alv TYPE TABLE OF ty_resumo_alv WITH HEADER LINE,
      wa_resumo_alv TYPE ty_resumo_alv.

"1 – Parâmetro
*&---------------------------------------------------------------------*
* SELECTION CRITERIA
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.
SELECT-OPTIONS: s_bukrs FOR t001-bukrs OBLIGATORY.
PARAMETER : s_exerc TYPE bkpf-gjahr OBLIGATORY,
            s_perio TYPE rfsdo-bilabmon OBLIGATORY,
            s_moeda TYPE waers OBLIGATORY DEFAULT 'BRL'.
SELECT-OPTIONS: s_conta FOR skb1-saknr,
                s_ntcl  FOR zglt049-cod_clas_bal MATCHCODE OBJECT zh_zglt039_c,
                s_nota  FOR zglt049-cod_clas_not MATCHCODE OBJECT zh_zglt039_n.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&   Event START-OF-SELECTION.
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: saldo      TYPE hslxx12,
        ls_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        c_x(1),
        it_skb1    TYPE TABLE OF skb1 WITH HEADER LINE.

  IF s_ntcl IS INITIAL AND s_nota IS INITIAL.
    SELECT * INTO TABLE it_skb1 "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1
     WHERE bukrs IN s_bukrs
       AND saknr IN s_conta.
  ELSEIF s_ntcl IS NOT INITIAL AND s_nota IS INITIAL.
    SELECT * INTO TABLE it_skb1 "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1 AS s
     WHERE bukrs IN s_bukrs
       AND saknr IN s_conta
       AND EXISTS ( SELECT *
                      FROM zglt041 AS t
                     WHERE t~bukrs EQ s~bukrs
                       AND t~saknr EQ s~saknr
                       AND t~cod_clas_bal IN s_ntcl ).
*                       AND T~GJAHR EQ S_EXERC ). "/Modificação CS2017000372
  ELSEIF s_ntcl IS INITIAL AND s_nota IS NOT INITIAL.
    SELECT * INTO TABLE it_skb1 "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1 AS s
     WHERE bukrs IN s_bukrs
       AND saknr IN s_conta
       AND EXISTS ( SELECT *
                      FROM zglt041 AS t
                     WHERE t~bukrs EQ s~bukrs
                       AND t~saknr EQ s~saknr
                       AND t~cod_clas_not2 IN s_nota ).
*                       AND T~GJAHR EQ S_EXERC ). "/Modificação CS2017000372
  ELSEIF s_ntcl IS NOT INITIAL AND s_nota IS NOT INITIAL.
    SELECT * INTO TABLE it_skb1 "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1 AS s
     WHERE bukrs IN s_bukrs
       AND saknr IN s_conta
       AND EXISTS ( SELECT *
                      FROM zglt041 AS t
                     WHERE t~bukrs EQ s~bukrs
                       AND t~saknr EQ s~saknr
                       AND t~cod_clas_bal  IN s_ntcl
                       AND t~cod_clas_not2 IN s_nota ).
*                       AND T~GJAHR EQ S_EXERC ). "/Modificação CS2017000372
  ENDIF.

  IF it_skb1[] IS NOT INITIAL.

    SELECT *
      INTO TABLE it_faglflext
      FROM faglflext
       FOR ALL ENTRIES IN it_skb1
     WHERE ryear  EQ s_exerc
       AND rbukrs IN s_bukrs
       AND rldnr  EQ '0L'
       AND rbukrs EQ it_skb1-bukrs
       AND racct  EQ it_skb1-saknr.

    SELECT *
      INTO TABLE it_skat
      FROM skat
       FOR ALL ENTRIES IN it_faglflext
     WHERE spras EQ sy-langu
       AND ktopl EQ '0050'
       AND saknr EQ it_faglflext-racct.

    SORT it_skat BY saknr.

    SELECT *
      INTO TABLE it_zglt053
      FROM zglt053.

    SORT it_zglt053 BY rmvct.

    CLEAR: it_resumo.

    LOOP AT it_faglflext.

      CLEAR: c_x.
"140938 CS2024000424 ZGL029 - Rel. Notas do Imobilizado PSA
      WA_RESUMO-RBUKRS = IT_FAGLFLEXT-RBUKRS.
      wa_resumo-racct  = it_faglflext-racct.
      READ TABLE it_skat WITH KEY saknr = it_faglflext-racct BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_resumo-txt50 = it_skat-txt50.
      ENDIF.

      IF it_faglflext-rmvct IS INITIAL.
        "100 - Implantação de Saldo
        it_faglflext-rmvct = '100'.
      ENDIF.

      wa_resumo-saldo_inicial = 0.
      wa_resumo-saldo_adicao  = 0.
      wa_resumo-saldo_baixas  = 0.
      wa_resumo-saldo_transf  = 0.
      wa_resumo-saldo_final   = 0.
      saldo                   = 0.

      PERFORM busca_saldo USING s_perio it_faglflext CHANGING saldo.

      READ TABLE it_zglt053 WITH KEY rmvct = it_faglflext-rmvct BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CASE it_zglt053-descr_abertura.
          WHEN 'S'. "Saldo Inicial
            wa_resumo-saldo_adicao = saldo.
            c_x = 'X'.
          WHEN 'A'. "Adição
            wa_resumo-saldo_adicao = saldo.
            c_x = 'X'.
          WHEN 'B'. "Baixas
            wa_resumo-saldo_baixas = saldo.
            c_x = 'X'.
          WHEN 'T'. "Transferências
            wa_resumo-saldo_transf = saldo.
            c_x = 'X'.

***     Inicio - ALX
          WHEN 'I'. "Transferências
            wa_resumo-incorporacao = saldo.
            c_x = 'X'.
***     Fim - ALX

        ENDCASE.
      ENDIF.

      IF ( c_x EQ 'X' ).

        CASE s_moeda.
          WHEN 'BRL'.
            wa_resumo-saldo_inicial = it_faglflext-hslvt.
          WHEN 'USD'.
            wa_resumo-saldo_inicial = it_faglflext-kslvt.
        ENDCASE.

        wa_resumo-saldo_final = saldo + wa_resumo-saldo_inicial.

        COLLECT wa_resumo INTO it_resumo.
CLEAR wa_resumo.
      ENDIF.

    ENDLOOP.

    CLEAR: lt_celltab.
    ls_celltab-fieldname = 'SALDO_INICIAL'.
    ls_celltab-style     = '00000120'.
    INSERT ls_celltab INTO TABLE lt_celltab.

    CLEAR: ls_celltab.
    ls_celltab-fieldname = 'SALDO_FINAL'.
    ls_celltab-style     = '00000120'.
    INSERT ls_celltab INTO TABLE lt_celltab.

    LOOP AT it_resumo INTO wa_resumo.
      CLEAR: wa_resumo_alv.
      MOVE-CORRESPONDING wa_resumo TO wa_resumo_alv.

      MOVE lt_celltab TO wa_resumo_alv-celltab.

      APPEND wa_resumo_alv TO it_resumo_alv.
    ENDLOOP.

    CALL SCREEN 0001.

  ELSE.
    MESSAGE 'Não localizado registros' TYPE 'S'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_saldo  USING    final        TYPE rfsdo-bilabmon
                           wa_faglflext TYPE faglflext
                  CHANGING saldo        TYPE hslxx12.

  FIELD-SYMBOLS: <atual> TYPE hslxx12.

  DATA: lc  TYPE c LENGTH 512,
        ms  TYPE rfsdo-bilabmon,
*---> 20.06.2023 - Migração S4 - DG
"        tms TYPE char02.
        TMS TYPE ZCHAR02.
*<--- 20.06.2023 - Migração S4 - DG

  ms = final.

  IF final EQ 1.
    CASE s_moeda.
      WHEN 'BRL'.
        saldo = wa_faglflext-hsl01.
      WHEN 'USD'.
        saldo = wa_faglflext-ksl01.
    ENDCASE.
  ELSEIF ( final GT 1 ) AND ( final LT 12 ).

    MOVE ms TO tms.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tms
      IMPORTING
        output = tms.

    CASE s_moeda.
      WHEN 'BRL'.
        CONCATENATE 'WA_FAGLFLEXT-HSL' tms INTO lc.
      WHEN 'USD'.
        CONCATENATE 'WA_FAGLFLEXT-KSL' tms INTO lc.
    ENDCASE.
    ASSIGN (lc) TO <atual>.

    ms = ms - 1.

    PERFORM busca_saldo USING ms wa_faglflext CHANGING saldo.
    saldo = <atual> + saldo.

  ELSEIF final EQ 12.
    ms = '11'.
    PERFORM busca_saldo USING ms wa_faglflext CHANGING saldo.
    CASE s_moeda.
      WHEN 'BRL'.
        saldo = saldo +
                wa_faglflext-hsl12 +
                wa_faglflext-hsl13 +
                wa_faglflext-hsl14 +
                wa_faglflext-hsl15 +
                wa_faglflext-hsl16.
      WHEN 'USD'.
        saldo = saldo +
                wa_faglflext-ksl12 +
                wa_faglflext-ksl13 +
                wa_faglflext-ksl14 +
                wa_faglflext-ksl15 +
                wa_faglflext-ksl16.
    ENDCASE.
  ENDIF.

ENDFORM.                    " BUSCA_SALDO

INCLUDE zgl023_0001.
