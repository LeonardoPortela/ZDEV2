*----------------------------------------------------------------------*
***INCLUDE LZDREF01 .
*----------------------------------------------------------------------*

TYPES: BEGIN OF tp_zgl015_dre_est04 .
TYPES: hsl     TYPE vlcur12,  "Valor em moeda interna
       ksl     TYPE vgcur12,  "Valor em moeda do Grupo de Empresas
       osl     TYPE vgcur12,  "Valor em moeda do Grupo
       msl     TYPE quan1_12, "Quantidade
       kostl   TYPE kostl,
       matnr   TYPE matnr,
       rassc   TYPE faglflexa-rassc,
       flag(1).
       INCLUDE STRUCTURE zgl015_dre_est04 .
TYPES: END OF tp_zgl015_dre_est04 .

TYPES: BEGIN OF tp_zgl015_dre_est05 .
TYPES: hsl   TYPE vlcur12,  "Valor em moeda interna
       ksl   TYPE vgcur12,  "Valor em moeda do Grupo de Empresas
       osl   TYPE vgcur12,  "Valor em moeda do Grupo
       msl   TYPE quan1_12, "Quantidade
       matnr TYPE matnr,
       rassc TYPE faglflexa-rassc.
       INCLUDE STRUCTURE zgl015_dre_est05 .
TYPES: END OF tp_zgl015_dre_est05 .

TYPES: BEGIN OF tp_zgl015_dre_est06 .
TYPES: hsl   TYPE vlcur12,  "Valor em moeda interna
       ksl   TYPE vgcur12,  "Valor em moeda do Grupo de Empresas
       osl   TYPE vgcur12,  "Valor em moeda do Grupo
       msl   TYPE quan1_12, "Quantidade
       matnr TYPE matnr,
       rassc TYPE faglflexa-rassc.
       INCLUDE STRUCTURE zgl015_dre_est06 .
TYPES: END OF tp_zgl015_dre_est06 .


CONSTANTS: vg_qtd_total TYPE i VALUE 18.

DATA: "Conta Razão
  it_zgl021_dre_dados     TYPE TABLE OF zgl021_dre_dados  WITH HEADER LINE,
  "Conta Razão/Centro de Custo
  it_zgl022_dre_dados     TYPE TABLE OF zgl022_dre_dados  WITH HEADER LINE,
  it_zgl022_dre_dados_aux TYPE TABLE OF zgl022_dre_dados  WITH HEADER LINE,
  "Conta Razão/Centro de Lucro
  it_zgl023_dre_dados     TYPE TABLE OF zgl023_dre_dados  WITH HEADER LINE,
  "Conta Razão/Grupo de Material
  it_zgl024_dre_dados     TYPE TABLE OF zgl024_dre_dados    WITH HEADER LINE,
  it_faglflexa_totcc      TYPE TABLE OF tp_zgl015_dre_est04 WITH HEADER LINE,
  it_faglflexa_totcl      TYPE TABLE OF tp_zgl015_dre_est05 WITH HEADER LINE,
  it_faglflexa_totmt      TYPE TABLE OF tp_zgl015_dre_est06 WITH HEADER LINE,
  it_faglflexa_totmt2     TYPE TABLE OF tp_zgl015_dre_est06 WITH HEADER LINE,
  vg_qtd                  TYPE i,
*     Tabelas de BPC
  it_zgl029_dre_dados     TYPE TABLE OF zgl029_dre_dados WITH HEADER LINE,
  it_zgl030_dre_acm       TYPE TABLE OF zgl030_dre_acm WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_DRE
*&---------------------------------------------------------------------*
FORM processar_dre  USING  wa_dre TYPE zgl020_dre_dados.

  DATA: it_zgl015_dre_est03     TYPE TABLE OF zgl015_dre_est03 WITH HEADER LINE,
        it_zgl015_dre_est03_aux TYPE TABLE OF zgl015_dre_est03 WITH HEADER LINE,
        it_zgl015_dre_est04     TYPE TABLE OF zgl015_dre_est04 WITH HEADER LINE,
        it_zgl015_dre_est05     TYPE TABLE OF zgl015_dre_est05 WITH HEADER LINE,
        it_zgl015_dre_est06     TYPE TABLE OF zgl015_dre_est06 WITH HEADER LINE,

        it_faglflext            TYPE TABLE OF faglflext        WITH HEADER LINE,
        it_faglflexa            TYPE TABLE OF faglflexa        WITH HEADER LINE,
        it_faglflext_total      TYPE TABLE OF faglflext        WITH HEADER LINE,
        it_csks                 TYPE TABLE OF csks             WITH HEADER LINE,
        it_bseg                 TYPE TABLE OF bseg             WITH HEADER LINE,
        it_bkpf                 TYPE TABLE OF bkpf             WITH HEADER LINE,
        it_j_1bnflin            TYPE TABLE OF j_1bnflin        WITH HEADER LINE.


  vg_qtd = 0.

  SELECT * INTO TABLE it_zgl015_dre_est03
    FROM zgl015_dre_est03
   WHERE bukrs EQ wa_dre-bukrs
     AND versn EQ wa_dre-versn.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e008(zdre) WITH wa_dre-bukrs wa_dre-versn RAISING erro.
  ENDIF.

  PERFORM muda_status USING wa_dre.
  "Atapa 1

  CLEAR: it_zgl015_dre_est03_aux[].
  MOVE it_zgl015_dre_est03[] TO it_zgl015_dre_est03_aux[].
  SORT it_zgl015_dre_est03_aux BY bukrs saknr nivel.
  DELETE ADJACENT DUPLICATES FROM it_zgl015_dre_est03_aux  COMPARING bukrs saknr nivel.

  SELECT * INTO TABLE it_zgl015_dre_est04
    FROM zgl015_dre_est04
     FOR ALL ENTRIES IN it_zgl015_dre_est03
   WHERE bukrs EQ it_zgl015_dre_est03-bukrs
     AND versn EQ it_zgl015_dre_est03-versn.

  IF NOT it_zgl015_dre_est04[] IS INITIAL.
    SELECT *
      FROM tka02 INNER JOIN csks
      ON csks~kokrs EQ tka02~kokrs
      INTO CORRESPONDING FIELDS OF TABLE it_csks
       FOR ALL ENTRIES IN it_zgl015_dre_est04
        WHERE csks~bukrs EQ it_zgl015_dre_est04-bukrs
          AND csks~kosar EQ it_zgl015_dre_est04-kosar.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_csks.
*<--- 04/07/2023 - Migração S4 - WS

    DELETE ADJACENT DUPLICATES FROM it_csks COMPARING ALL FIELDS.

*    SELECT * INTO TABLE IT_CSKS
*      FROM CSKS
*       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST04
*     WHERE KOKRS EQ 'MAGI'
*       AND KOSAR EQ IT_ZGL015_DRE_EST04-KOSAR.
  ENDIF.

  SELECT * INTO TABLE it_zgl015_dre_est05
    FROM zgl015_dre_est05
     FOR ALL ENTRIES IN it_zgl015_dre_est03
   WHERE bukrs EQ it_zgl015_dre_est03-bukrs
     AND versn EQ it_zgl015_dre_est03-versn.

  SELECT * INTO TABLE it_zgl015_dre_est06
    FROM zgl015_dre_est06
     FOR ALL ENTRIES IN it_zgl015_dre_est03
   WHERE bukrs EQ it_zgl015_dre_est03-bukrs
     AND versn EQ it_zgl015_dre_est03-versn.

  LOOP AT it_zgl015_dre_est03.
    READ TABLE it_zgl015_dre_est04 WITH KEY bukrs = it_zgl015_dre_est03-bukrs
                                            versn = it_zgl015_dre_est03-versn
                                            nivel = it_zgl015_dre_est03-nivel
                                            saknr = it_zgl015_dre_est03-saknr.
    IF sy-subrc IS INITIAL.
      DELETE it_zgl015_dre_est03_aux WHERE bukrs = it_zgl015_dre_est03-bukrs
                                       AND versn = it_zgl015_dre_est03-versn
                                       AND nivel = it_zgl015_dre_est03-nivel
                                       AND saknr = it_zgl015_dre_est03-saknr.
      CONTINUE.
    ENDIF.
    READ TABLE it_zgl015_dre_est05 WITH KEY bukrs = it_zgl015_dre_est03-bukrs
                                            versn = it_zgl015_dre_est03-versn
                                            nivel = it_zgl015_dre_est03-nivel
                                            saknr = it_zgl015_dre_est03-saknr.
    IF sy-subrc IS INITIAL.
      DELETE it_zgl015_dre_est03_aux WHERE bukrs = it_zgl015_dre_est03-bukrs
                                       AND versn = it_zgl015_dre_est03-versn
                                       AND nivel = it_zgl015_dre_est03-nivel
                                       AND saknr = it_zgl015_dre_est03-saknr.
      CONTINUE.
    ENDIF.
    READ TABLE it_zgl015_dre_est06 WITH KEY bukrs = it_zgl015_dre_est03-bukrs
                                            versn = it_zgl015_dre_est03-versn
                                            nivel = it_zgl015_dre_est03-nivel
                                            saknr = it_zgl015_dre_est03-saknr.
    IF sy-subrc IS INITIAL.
      DELETE it_zgl015_dre_est03_aux WHERE bukrs = it_zgl015_dre_est03-bukrs
                                       AND versn = it_zgl015_dre_est03-versn
                                       AND nivel = it_zgl015_dre_est03-nivel
                                       AND saknr = it_zgl015_dre_est03-saknr.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  PERFORM muda_status USING wa_dre.
  "Atapa 2

  REFRESH: it_zgl029_dre_dados.
  CLEAR: it_zgl029_dre_dados.

  " Informações de Contas ------------------------------------------------------------------------------------
  IF NOT it_zgl015_dre_est03_aux[] IS INITIAL.

    SELECT * INTO TABLE it_faglflext
      FROM faglflext
       FOR ALL ENTRIES IN it_zgl015_dre_est03_aux
     WHERE ryear  EQ wa_dre-gjahr
       AND rldnr  EQ '0L'
       AND rbukrs EQ it_zgl015_dre_est03_aux-bukrs
       AND racct  EQ it_zgl015_dre_est03_aux-saknr.

    IF sy-subrc IS INITIAL.
      PERFORM muda_status USING wa_dre.
      "Atapa 3
      PERFORM totalizar_flexa TABLES it_faglflext it_faglflext_total
                              USING wa_dre.
      PERFORM muda_status USING wa_dre.
      "Atapa 4
    ELSE.
      vg_qtd = vg_qtd + 1.
      PERFORM muda_status USING wa_dre.
      "Atapa 3
      "Atapa 4
    ENDIF.
  ELSE.
    vg_qtd = vg_qtd + 1.
    PERFORM muda_status USING wa_dre.
    "Atapa 3
    "Atapa 4
  ENDIF.

  " Informações de Contas ------------------------------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------

  " Informações de Material ----------------------------------------------------------------------------------
  CLEAR: it_faglflexa_totmt[].

  IF NOT it_zgl015_dre_est06[] IS INITIAL.

    SELECT * INTO TABLE it_faglflexa
      FROM faglflexa
       FOR ALL ENTRIES IN it_zgl015_dre_est06
     WHERE ryear  EQ wa_dre-gjahr
       AND poper  EQ wa_dre-monat
       AND rbukrs EQ it_zgl015_dre_est06-bukrs
       AND racct  EQ it_zgl015_dre_est06-saknr
       AND rldnr  EQ '0L'.

    IF  wa_dre-monat = '12'.
      SELECT * APPENDING TABLE it_faglflexa
        FROM faglflexa
         FOR ALL ENTRIES IN it_zgl015_dre_est06
        WHERE ryear  EQ wa_dre-gjahr
         AND poper  IN ('13','14','15')
         AND rbukrs EQ it_zgl015_dre_est06-bukrs
         AND racct  EQ it_zgl015_dre_est06-saknr
         AND rldnr  EQ '0L'.
    ENDIF.

    PERFORM muda_status USING wa_dre.                       "Atapa 5

    IF NOT it_faglflexa[] IS INITIAL.
      "Agrupado por Material e Grupo de mercadoria
      CLEAR: it_faglflexa_totmt[].

      LOOP AT it_zgl015_dre_est06.
        MOVE-CORRESPONDING it_zgl015_dre_est06 TO it_faglflexa_totmt.
        APPEND it_faglflexa_totmt.
      ENDLOOP.

      PERFORM totalizar_flexa_mt TABLES it_faglflexa USING wa_dre.
    ELSE.
      vg_qtd = vg_qtd + 3.
      PERFORM muda_status USING wa_dre.
      "Atapa 6 - 9
    ENDIF.
  ELSE.
    vg_qtd = vg_qtd + 4.
    PERFORM muda_status USING wa_dre.
    "Atapa 5 - 9
  ENDIF.

  " Informações de Material ----------------------------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------

  " Informações de Centro de Custo ---------------------------------------------------------------------------
  CLEAR: it_faglflexa[].

  IF NOT it_zgl015_dre_est04[] IS INITIAL.
    SELECT * INTO TABLE it_faglflexa
         FROM faglflexa
          FOR ALL ENTRIES IN it_zgl015_dre_est04
        WHERE ryear  EQ wa_dre-gjahr
          AND poper  EQ wa_dre-monat
          AND rbukrs EQ it_zgl015_dre_est04-bukrs
          AND racct  EQ it_zgl015_dre_est04-saknr
          AND rldnr  EQ '0L'.

    IF  wa_dre-monat = '12'.
      SELECT * APPENDING TABLE it_faglflexa
     FROM faglflexa
      FOR ALL ENTRIES IN it_zgl015_dre_est04
    WHERE ryear  EQ wa_dre-gjahr
      AND poper  IN ('13','14','15')
      AND rbukrs EQ it_zgl015_dre_est04-bukrs
      AND racct  EQ it_zgl015_dre_est04-saknr
      AND rldnr  EQ '0L'.
    ENDIF.

  ENDIF.
  PERFORM muda_status USING wa_dre.
  "Atapa 10

  IF NOT it_faglflexa[] IS INITIAL.

    LOOP AT it_zgl015_dre_est04.
      LOOP AT it_csks WHERE kosar EQ it_zgl015_dre_est04-kosar
                        AND bukrs EQ it_zgl015_dre_est04-bukrs.
        MOVE-CORRESPONDING it_zgl015_dre_est04 TO it_faglflexa_totcc.
        it_faglflexa_totcc-kostl = it_csks-kostl.
        APPEND it_faglflexa_totcc.
      ENDLOOP.
    ENDLOOP.

    PERFORM totalizar_flexa_cc TABLES it_faglflexa USING wa_dre.
    PERFORM muda_status USING wa_dre.
    "Atapa 11
  ELSE.
    PERFORM muda_status USING wa_dre.
    "Atapa 11
  ENDIF.

  " Informações de Centro de Custo ---------------------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------


  " Informações de Centro de Lucro ---------------------------------------------------------------------------
  CLEAR: it_faglflexa[].

  IF NOT it_zgl015_dre_est05[] IS INITIAL.
    SELECT * INTO TABLE it_faglflexa
      FROM faglflexa
       FOR ALL ENTRIES IN it_zgl015_dre_est05
     WHERE ryear  EQ wa_dre-gjahr
       AND poper  EQ wa_dre-monat
       AND rbukrs EQ it_zgl015_dre_est05-bukrs
       AND racct  EQ it_zgl015_dre_est05-saknr
       AND rldnr  EQ '0L'.

    IF  wa_dre-monat = '12'.
      SELECT * APPENDING TABLE it_faglflexa
        FROM faglflexa
         FOR ALL ENTRIES IN it_zgl015_dre_est05
       WHERE ryear  EQ wa_dre-gjahr
         AND poper  IN ('13','14','15')
         AND rbukrs EQ it_zgl015_dre_est05-bukrs
         AND racct  EQ it_zgl015_dre_est05-saknr
         AND rldnr  EQ '0L'.
    ENDIF.
    "and prctr  eq it_zgl004_dre_aux-prctr.
  ENDIF.
  PERFORM muda_status USING wa_dre.
  "Atapa 12

  IF NOT it_faglflexa[] IS INITIAL.

    LOOP AT it_zgl015_dre_est05.
      MOVE-CORRESPONDING it_zgl015_dre_est05 TO it_faglflexa_totcl.
      APPEND it_faglflexa_totcl.
    ENDLOOP.

    PERFORM totalizar_flexa_cl TABLES it_faglflexa USING wa_dre.
    PERFORM muda_status USING wa_dre.
    "Atapa 13
  ELSE.
    PERFORM muda_status USING wa_dre.
    "Atapa 13
  ENDIF.
  " Informações de Centro de Custo ---------------------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------

  DELETE FROM zgl029_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                AND  versn EQ wa_dre-versn
                                AND  monat EQ wa_dre-monat
                                AND  gjahr EQ wa_dre-gjahr.


  SORT: it_zgl015_dre_est03_aux BY bukrs versn nivel saknr.
  "Armazena Total Conta Razão
  LOOP AT it_zgl015_dre_est03.

    READ TABLE it_zgl015_dre_est03_aux WITH KEY bukrs = it_zgl015_dre_est03-bukrs
                                                versn = it_zgl015_dre_est03-versn
                                                nivel = it_zgl015_dre_est03-nivel
                                                saknr = it_zgl015_dre_est03-saknr
                                                BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE it_faglflext_total WITH KEY ryear  = wa_dre-gjahr
                                             rbukrs = it_zgl015_dre_est03-bukrs
                                             racct  = it_zgl015_dre_est03-saknr.
      IF sy-subrc IS INITIAL.
        CLEAR: it_zgl021_dre_dados.
        it_zgl021_dre_dados-bukrs = it_zgl015_dre_est03-bukrs.
        it_zgl021_dre_dados-versn = it_zgl015_dre_est03-versn.
        it_zgl021_dre_dados-monat = wa_dre-monat.
        it_zgl021_dre_dados-gjahr = wa_dre-gjahr.
        it_zgl021_dre_dados-nivel = it_zgl015_dre_est03-nivel.
        it_zgl021_dre_dados-saknr = it_zgl015_dre_est03-saknr.
        PERFORM busca_valor_flext USING it_faglflext_total wa_dre
                CHANGING it_zgl021_dre_dados-vlr_rea it_zgl021_dre_dados-vlr_dolar it_zgl021_dre_dados-vlr_dolar_conv
                         it_zgl021_dre_dados-vlr_grupo.

        MOVE-CORRESPONDING: it_zgl021_dre_dados TO it_zgl029_dre_dados.
        it_zgl029_dre_dados-vbund = it_faglflext_total-rassc.
        APPEND it_zgl021_dre_dados.
*        APPEND IT_ZGL029_DRE_DADOS.

      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE FROM zgl021_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND  versn EQ wa_dre-versn
                                 AND  monat EQ wa_dre-monat
                                 AND  gjahr EQ wa_dre-gjahr.
  IF NOT it_zgl021_dre_dados[] IS INITIAL.
    DELETE it_zgl021_dre_dados WHERE vlr_rea        EQ 0
                                 AND vlr_dolar      EQ 0
                                 AND vlr_dolar_conv EQ 0
                                 AND vlr_grupo      EQ 0.

*    DELETE IT_ZGL029_DRE_DADOS WHERE VLR_REA        EQ 0
*                                 AND VLR_DOLAR      EQ 0.
**                                 AND VLR_DOLAR_CONV EQ 0.

    MODIFY zgl021_dre_dados FROM TABLE it_zgl021_dre_dados.
*    MODIFY ZGL029_DRE_DADOS FROM TABLE IT_ZGL029_DRE_DADOS.
  ENDIF.

  PERFORM muda_status USING wa_dre.
  "Atapa 14
  " Armazena Total Conta Razão -------------------------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------


  "Armazena Total Conta Razão/Centro de Custo

*  REFRESH: IT_ZGL029_DRE_DADOS.
*  CLEAR: IT_ZGL029_DRE_DADOS.

  SORT it_faglflexa_totcc BY saknr kostl.
  LOOP AT it_zgl015_dre_est04.

    LOOP AT it_csks WHERE kosar EQ it_zgl015_dre_est04-kosar.

      READ TABLE it_faglflexa_totcc WITH KEY saknr = it_zgl015_dre_est04-saknr
                                             kostl = it_csks-kostl BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        CLEAR: it_zgl022_dre_dados.
        it_zgl022_dre_dados-bukrs          = it_zgl015_dre_est04-bukrs.
        it_zgl022_dre_dados-versn          = it_zgl015_dre_est04-versn.
        it_zgl022_dre_dados-monat          = wa_dre-monat.
        it_zgl022_dre_dados-gjahr          = wa_dre-gjahr.
        it_zgl022_dre_dados-nivel          = it_zgl015_dre_est04-nivel.
        it_zgl022_dre_dados-saknr          = it_faglflexa_totcc-saknr.
        it_zgl022_dre_dados-kostl          = it_faglflexa_totcc-kostl.
        it_zgl022_dre_dados-vlr_rea        = it_faglflexa_totcc-hsl.
        it_zgl022_dre_dados-vlr_dolar      = it_faglflexa_totcc-ksl.
        it_zgl022_dre_dados-vlr_grupo      = it_faglflexa_totcc-osl.
        it_zgl022_dre_dados-vlr_dolar_conv = it_faglflexa_totcc-ksl * wa_dre-ukurs.

*        MOVE-CORRESPONDING: IT_ZGL022_DRE_DADOS TO IT_ZGL029_DRE_DADOS.
*        MOVE: IT_FAGLFLEXA_TOTCC-RASSC TO IT_ZGL029_DRE_DADOS-VBUND.
        APPEND it_zgl022_dre_dados.
*        APPEND IT_ZGL029_DRE_DADOS.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  DELETE FROM zgl022_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND  versn EQ wa_dre-versn
                                 AND  monat EQ wa_dre-monat
                                 AND  gjahr EQ wa_dre-gjahr.
  IF NOT it_zgl022_dre_dados[] IS INITIAL.
**    teste igor
    REFRESH: it_zgl022_dre_dados_aux.
    it_zgl022_dre_dados_aux[] = it_zgl022_dre_dados[].
*    DELETE it_zgl022_dre_dados WHERE vlr_rea        EQ 0
*                                 AND vlr_dolar      EQ 0
*                                 AND vlr_dolar_conv EQ 0.

*    DELETE IT_ZGL029_DRE_DADOS WHERE VLR_REA        EQ 0
*                                 AND VLR_DOLAR      EQ 0.
**                                 AND VLR_DOLAR_CONV EQ 0.

    MODIFY zgl022_dre_dados FROM TABLE it_zgl022_dre_dados.
*    MODIFY ZGL029_DRE_DADOS FROM TABLE IT_ZGL029_DRE_DADOS.
  ENDIF.

  PERFORM muda_status USING wa_dre.
  "Atapa 15
  " Armazena Total Conta Razão/Centro de Custo ---------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------
*  REFRESH: IT_ZGL029_DRE_DADOS.
*  CLEAR: IT_ZGL029_DRE_DADOS.

  "Armazena Total Conta Razão/Centro de Lucro
  SORT it_faglflexa_totcl  BY saknr prctr.
  LOOP AT it_zgl015_dre_est05 .
    READ TABLE it_faglflexa_totcl WITH KEY saknr = it_zgl015_dre_est05-saknr
                                           prctr = it_zgl015_dre_est05-prctr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl023_dre_dados.
      it_zgl023_dre_dados-bukrs          = it_zgl015_dre_est05-bukrs.
      it_zgl023_dre_dados-versn          = it_zgl015_dre_est05-versn.
      it_zgl023_dre_dados-monat          = wa_dre-monat.
      it_zgl023_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl023_dre_dados-nivel          = it_zgl015_dre_est05-nivel.
      it_zgl023_dre_dados-saknr          = it_faglflexa_totcl-saknr.
      it_zgl023_dre_dados-prctr          = it_faglflexa_totcl-prctr.
      it_zgl023_dre_dados-vlr_rea        = it_faglflexa_totcl-hsl.
      it_zgl023_dre_dados-vlr_dolar      = it_faglflexa_totcl-ksl.
      it_zgl023_dre_dados-vlr_grupo      = it_faglflexa_totcl-osl.
      it_zgl023_dre_dados-vlr_dolar_conv = it_faglflexa_totcl-ksl * wa_dre-ukurs.

*      MOVE-CORRESPONDING: IT_ZGL023_DRE_DADOS TO IT_ZGL029_DRE_DADOS.
*      MOVE: IT_FAGLFLEXA_TOTCL-RASSC TO IT_ZGL029_DRE_DADOS-VBUND.

      APPEND it_zgl023_dre_dados.
*      APPEND IT_ZGL029_DRE_DADOS.

    ENDIF.
  ENDLOOP.

  DELETE FROM zgl023_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND  versn EQ wa_dre-versn
                                 AND  monat EQ wa_dre-monat
                                 AND  gjahr EQ wa_dre-gjahr.

  IF NOT it_zgl023_dre_dados[] IS INITIAL.
    DELETE it_zgl023_dre_dados WHERE vlr_rea        EQ 0
                                 AND vlr_dolar      EQ 0
                                 AND vlr_dolar_conv EQ 0
                                 AND vlr_grupo      EQ 0.

*    DELETE IT_ZGL029_DRE_DADOS WHERE VLR_REA        EQ 0
*                                 AND VLR_DOLAR      EQ 0.
**                                 AND VLR_DOLAR_CONV EQ 0.

    MODIFY zgl023_dre_dados FROM TABLE it_zgl023_dre_dados.
*    MODIFY ZGL029_DRE_DADOS FROM TABLE IT_ZGL029_DRE_DADOS.
  ENDIF.

  PERFORM muda_status USING wa_dre.
  "Atapa 16
  " Armazena Total Conta Razão/Centro de Lucro ---------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------
*  REFRESH: IT_ZGL029_DRE_DADOS.
*  CLEAR: IT_ZGL029_DRE_DADOS.

  "Armazena Total Conta Razão/Grupo de Material
  SORT it_faglflexa_totmt BY saknr matkl.
  LOOP AT it_zgl015_dre_est06.

    READ TABLE it_faglflexa_totmt WITH KEY saknr = it_zgl015_dre_est06-saknr
                                           matkl = it_zgl015_dre_est06-matkl BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl024_dre_dados.
      it_zgl024_dre_dados-bukrs          = it_zgl015_dre_est06-bukrs.
      it_zgl024_dre_dados-versn          = it_zgl015_dre_est06-versn.
      it_zgl024_dre_dados-monat          = wa_dre-monat.
      it_zgl024_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl024_dre_dados-nivel          = it_zgl015_dre_est06-nivel.
      it_zgl024_dre_dados-saknr          = it_faglflexa_totmt-saknr.
      it_zgl024_dre_dados-matkl          = it_faglflexa_totmt-matkl.
      it_zgl024_dre_dados-qtd_ton        = it_faglflexa_totmt-msl.
      it_zgl024_dre_dados-vlr_rea        = it_faglflexa_totmt-hsl.
      it_zgl024_dre_dados-vlr_dolar      = it_faglflexa_totmt-ksl.
      it_zgl024_dre_dados-vlr_grupo      = it_faglflexa_totmt-osl.
      it_zgl024_dre_dados-vlr_dolar_conv = it_faglflexa_totmt-ksl * wa_dre-ukurs.

*      MOVE-CORRESPONDING: IT_ZGL024_DRE_DADOS TO IT_ZGL029_DRE_DADOS.
*      MOVE: IT_FAGLFLEXA_TOTMT-RASSC TO IT_ZGL029_DRE_DADOS-VBUND.

      APPEND it_zgl024_dre_dados.
*      APPEND IT_ZGL029_DRE_DADOS.
    ENDIF.

  ENDLOOP.

  DELETE FROM zgl024_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND  versn EQ wa_dre-versn
                                 AND  monat EQ wa_dre-monat
                                 AND  gjahr EQ wa_dre-gjahr.

  IF NOT it_zgl024_dre_dados[] IS INITIAL.
    DELETE it_zgl024_dre_dados WHERE qtd_ton        EQ 0
                                 AND vlr_rea        EQ 0
                                 AND vlr_dolar      EQ 0
                                 AND vlr_dolar_conv EQ 0
                                 AND vlr_grupo      EQ 0.

*    DELETE IT_ZGL029_DRE_DADOS WHERE VLR_REA        EQ 0
*                                 AND VLR_DOLAR      EQ 0.
**                                 AND VLR_DOLAR_CONV EQ 0.
    MODIFY zgl024_dre_dados FROM TABLE it_zgl024_dre_dados.
*    MODIFY ZGL029_DRE_DADOS FROM TABLE IT_ZGL029_DRE_DADOS.
  ENDIF.


  DELETE it_zgl029_dre_dados WHERE vlr_rea        EQ 0
                               AND vlr_dolar      EQ 0
                               AND vlr_grupo      EQ 0.

  IF it_zgl029_dre_dados[] IS NOT INITIAL.
    MODIFY zgl029_dre_dados FROM TABLE it_zgl029_dre_dados.
  ENDIF.
  PERFORM muda_status USING wa_dre.                         "Atapa 17
  " Armazena Total Conta Razão/Grupo de Material ---------------------------------------------------------------
  "-----------------------------------------------------------------------------------------------------------

  PERFORM totaliza_mes TABLES it_zgl015_dre_est03_aux
                      USING wa_dre.
  PERFORM muda_status USING wa_dre.                         "Atapa 18

ENDFORM.                    " PROCESSAR_DRE

*&---------------------------------------------------------------------*
*&      Form  TOTALIZAR_FLEXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM totalizar_flexa  TABLES it_faglflext       STRUCTURE faglflext
                             it_faglflext_total STRUCTURE faglflext
                      USING wa_dre TYPE zgl020_dre_dados..

* Tabela que ira conter a totalizacao dividida pela Sociedade
  TYPES: BEGIN OF ty_saknr,
           saknr TYPE bseg-saknr,
*          MATKL TYPE MARA-MATKL,
           vbund TYPE bseg-vbund,
           ksl01 TYPE faglflext-ksl01,
           ksl02 TYPE faglflext-ksl02,
           ksl03 TYPE faglflext-ksl03,
           ksl04 TYPE faglflext-ksl04,
           ksl05 TYPE faglflext-ksl05,
           ksl06 TYPE faglflext-ksl06,
           ksl07 TYPE faglflext-ksl07,
           ksl08 TYPE faglflext-ksl08,
           ksl09 TYPE faglflext-ksl09,
           ksl10 TYPE faglflext-ksl10,
           ksl11 TYPE faglflext-ksl11,
           ksl12 TYPE faglflext-ksl12,
           ksl13 TYPE faglflext-ksl13,
           ksl14 TYPE faglflext-ksl14,
           ksl15 TYPE faglflext-ksl15,
           ksl16 TYPE faglflext-ksl16,
           hsl01 TYPE faglflext-hsl01,
           hsl02 TYPE faglflext-hsl02,
           hsl03 TYPE faglflext-hsl03,
           hsl04 TYPE faglflext-hsl04,
           hsl05 TYPE faglflext-hsl05,
           hsl06 TYPE faglflext-hsl06,
           hsl07 TYPE faglflext-hsl07,
           hsl08 TYPE faglflext-hsl08,
           hsl09 TYPE faglflext-hsl09,
           hsl10 TYPE faglflext-hsl10,
           hsl11 TYPE faglflext-hsl11,
           hsl12 TYPE faglflext-hsl12,
           osl01 TYPE faglflext-osl01,
           osl02 TYPE faglflext-osl02,
           osl03 TYPE faglflext-osl03,
           osl04 TYPE faglflext-osl04,
           osl05 TYPE faglflext-osl05,
           osl06 TYPE faglflext-osl06,
           osl07 TYPE faglflext-osl07,
           osl08 TYPE faglflext-osl08,
           osl09 TYPE faglflext-osl09,
           osl10 TYPE faglflext-osl10,
           osl11 TYPE faglflext-osl11,
           osl12 TYPE faglflext-osl12,
           osl13 TYPE faglflext-osl13,
           osl14 TYPE faglflext-osl14,
           osl15 TYPE faglflext-osl15,
           osl16 TYPE faglflext-osl16,
         END OF ty_saknr.

  DATA: vg_tabix TYPE sy-tabix,
        wa_total TYPE faglflext,
        it_saknr TYPE TABLE OF ty_saknr WITH HEADER LINE.

  CLEAR: it_faglflext_total[], it_saknr.
  REFRESH: it_saknr.

  MOVE it_faglflext[] TO it_faglflext_total[].
  SORT it_faglflext_total[] BY ryear rbukrs racct.
  DELETE ADJACENT DUPLICATES FROM it_faglflext_total COMPARING ryear rbukrs racct.

  SORT it_faglflext BY ryear rbukrs racct.
  LOOP AT it_faglflext_total.
    vg_tabix = sy-tabix.
    MOVE-CORRESPONDING it_faglflext_total TO wa_total.
    PERFORM inicializa_total USING wa_total.
    LOOP AT it_faglflext WHERE ryear  = it_faglflext_total-ryear
                           AND rbukrs = it_faglflext_total-rbukrs
                           AND racct  = it_faglflext_total-racct.
      PERFORM totaliza USING it_faglflext wa_total.
    ENDLOOP.
    MODIFY it_faglflext_total INDEX vg_tabix FROM wa_total.
  ENDLOOP.

  CLEAR: it_saknr.
  REFRESH: it_saknr.

  LOOP AT it_faglflext.

    it_saknr-saknr = it_faglflext-racct.
    it_saknr-vbund = it_faglflext-rassc.

    it_saknr-ksl01 = it_faglflext-ksl01.
    it_saknr-ksl02 = it_faglflext-ksl02.
    it_saknr-ksl03 = it_faglflext-ksl03.
    it_saknr-ksl04 = it_faglflext-ksl04.
    it_saknr-ksl05 = it_faglflext-ksl05.
    it_saknr-ksl06 = it_faglflext-ksl06.
    it_saknr-ksl07 = it_faglflext-ksl07.
    it_saknr-ksl08 = it_faglflext-ksl08.
    it_saknr-ksl09 = it_faglflext-ksl09.
    it_saknr-ksl10 = it_faglflext-ksl10.
    it_saknr-ksl11 = it_faglflext-ksl11.


    IF it_faglflext-rbukrs = '0101'. " Paraguai moeda interna
      it_saknr-hsl01 = it_faglflext-hsl01 * 100.
      it_saknr-hsl02 = it_faglflext-hsl02 * 100.
      it_saknr-hsl03 = it_faglflext-hsl03 * 100.
      it_saknr-hsl04 = it_faglflext-hsl04 * 100.
      it_saknr-hsl05 = it_faglflext-hsl05 * 100.
      it_saknr-hsl06 = it_faglflext-hsl06 * 100.
      it_saknr-hsl07 = it_faglflext-hsl07 * 100.
      it_saknr-hsl08 = it_faglflext-hsl08 * 100.
      it_saknr-hsl09 = it_faglflext-hsl09 * 100.
      it_saknr-hsl10 = it_faglflext-hsl10 * 100.
      it_saknr-hsl11 = it_faglflext-hsl11 * 100.
    ELSE.
      it_saknr-hsl01 = it_faglflext-hsl01.
      it_saknr-hsl02 = it_faglflext-hsl02.
      it_saknr-hsl03 = it_faglflext-hsl03.
      it_saknr-hsl04 = it_faglflext-hsl04.
      it_saknr-hsl05 = it_faglflext-hsl05.
      it_saknr-hsl06 = it_faglflext-hsl06.
      it_saknr-hsl07 = it_faglflext-hsl07.
      it_saknr-hsl08 = it_faglflext-hsl08.
      it_saknr-hsl09 = it_faglflext-hsl09.
      it_saknr-hsl10 = it_faglflext-hsl10.
      it_saknr-hsl11 = it_faglflext-hsl11.
    ENDIF.

    it_saknr-osl01 = it_faglflext-osl01.
    it_saknr-osl02 = it_faglflext-osl02.
    it_saknr-osl03 = it_faglflext-osl03.
    it_saknr-osl04 = it_faglflext-osl04.
    it_saknr-osl05 = it_faglflext-osl05.
    it_saknr-osl06 = it_faglflext-osl06.
    it_saknr-osl07 = it_faglflext-osl07.
    it_saknr-osl08 = it_faglflext-osl08.
    it_saknr-osl09 = it_faglflext-osl09.
    it_saknr-osl10 = it_faglflext-osl10.
    it_saknr-osl11 = it_faglflext-osl11.

* Valor Dolar
    it_saknr-ksl12 = it_faglflext-ksl12 + it_faglflext-ksl13 +
                     it_faglflext-ksl14 + it_faglflext-ksl15.

* Valor Real
    IF it_faglflext-rbukrs = '0101'. " Paraguai moeda interna
      it_saknr-hsl12 = it_faglflext-hsl12 + ( it_faglflext-hsl13 * 100 ) +
                       it_faglflext-hsl14 + ( it_faglflext-hsl15 * 100 ).
    ELSE.
      it_saknr-hsl12 = it_faglflext-hsl12 + it_faglflext-hsl13 +
                       it_faglflext-hsl14 + it_faglflext-hsl15.
    ENDIF.

* Valor Grupo
    it_saknr-osl12 = it_faglflext-osl12 + it_faglflext-osl13 +
                     it_faglflext-osl14 + it_faglflext-osl15.

    COLLECT it_saknr.
  ENDLOOP.

  SORT it_faglflext_total BY rbukrs ryear racct.
  LOOP AT it_saknr.
    READ TABLE it_faglflext_total WITH KEY rbukrs = wa_dre-bukrs
                                           ryear  = wa_dre-gjahr
                                           racct  = it_saknr-saknr BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl029_dre_dados.
      it_zgl029_dre_dados-bukrs          = wa_dre-bukrs.
      it_zgl029_dre_dados-versn          = wa_dre-versn.
      it_zgl029_dre_dados-monat          = wa_dre-monat.
      it_zgl029_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl029_dre_dados-vbund          = it_saknr-vbund.
      it_zgl029_dre_dados-saknr          = it_saknr-saknr.

      CASE wa_dre-monat.
        WHEN 01.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl01.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl01.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl01.
        WHEN 02.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl02.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl02.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl02.
        WHEN 03.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl03.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl03.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl03.
        WHEN 04.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl04.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl04.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl04.
        WHEN 05.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl05.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl05.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl05.
        WHEN 06.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl06.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl06.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl06.
        WHEN 07.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl07.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl07.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl07.
        WHEN 08.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl08.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl08.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl08.
        WHEN 09.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl09.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl09.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl09.
        WHEN 10.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl10.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl10.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl10.
        WHEN 11.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl11.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl11.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl11.
        WHEN 12.
          it_zgl029_dre_dados-vlr_rea   = it_saknr-hsl12.
          it_zgl029_dre_dados-vlr_dolar = it_saknr-ksl12.
          it_zgl029_dre_dados-vlr_grupo = it_saknr-osl12.

      ENDCASE.
      APPEND it_zgl029_dre_dados.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " TOTALIZAR_FLEXA

*&---------------------------------------------------------------------*
*&      Form  TOTALIZAR_FLEXA_MT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM totalizar_flexa_mt  TABLES it_faglflexa STRUCTURE faglflexa USING wa_dre TYPE zgl020_dre_dados.

  TYPES: BEGIN OF tb_bseg.
  TYPES: bukrs   LIKE bseg-bukrs,
         belnr   LIKE bseg-belnr,
         gjahr   LIKE bseg-gjahr,
         buzei   LIKE bseg-buzei,
         bschl   LIKE bseg-bschl,
         hkont   LIKE bseg-hkont,
         vbeln   LIKE bseg-vbeln,
         vbel2   LIKE bseg-vbel2,
         posn2   LIKE bseg-posn2,
         matnr   LIKE bseg-matnr,
         matkl   TYPE matkl,
         paobjnr TYPE bseg-paobjnr,
         kostl   TYPE bseg-kostl,
         vbund   TYPE bseg-vbund.

  TYPES: END OF tb_bseg.

  TYPES: BEGIN OF ty_acct,
           paobjnr TYPE ce4magi_acct-paobjnr,
           artnr   TYPE ce4magi_acct-artnr,
           kmmakl  TYPE ce4magi_acct-kmmakl,
         END OF ty_acct.
  TYPES: BEGIN OF ty_tka02,
           bukrs TYPE tka02-bukrs,
           kokrs TYPE tka02-kokrs,
         END OF ty_tka02.

* Tabela que ira conter a totalizacao dividida pela Sociedade
  TYPES: BEGIN OF ty_saknr,
           saknr TYPE bseg-saknr,
           matkl TYPE mara-matkl,
           vbund TYPE bseg-vbund,
           msl   TYPE faglflexa-msl,
           hsl   TYPE faglflexa-hsl,
           ksl   TYPE faglflexa-ksl,
           osl   TYPE faglflexa-osl,
         END OF ty_saknr.

  TYPES: BEGIN OF tb_bkpf.
  TYPES: bukrs   LIKE bkpf-bukrs,
         belnr   LIKE bkpf-belnr,
         gjahr   LIKE bkpf-gjahr,
         xblnr   LIKE bkpf-xblnr,
         blart   TYPE bkpf-blart,
         refkey  LIKE j_1bnflin-refkey,
         estorno.
  TYPES: END OF tb_bkpf.

  DATA: vg_tabix          TYPE sy-tabix,
        it_bseg_ft        TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux    TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_gi TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_ld TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_ar TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_bg TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_fta       TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_mara           TYPE TABLE OF mara    WITH HEADER LINE,
        it_bkpf           TYPE TABLE OF tb_bkpf WITH HEADER LINE,
        it_gi_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_ld_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_ar_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_bg_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_tka02          TYPE TABLE OF ty_tka02 WITH HEADER LINE,
        it_acct           TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_j_1bnflin      TYPE TABLE OF j_1bnflin WITH HEADER LINE,
        it_csks           TYPE TABLE OF csks WITH HEADER LINE,
        it_cvend          TYPE TABLE OF zgl015_dre_cvend WITH HEADER LINE,
        it_faglflexa_aux  TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_faglflexa_ft   TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_faglflexa_mt   TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_faglflexa_mt2  TYPE TABLE OF faglflexa WITH HEADER LINE,
        wl_count          TYPE sy-tabix,
        it_faglflexa_aux2 TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_saknr          TYPE TABLE OF ty_saknr WITH HEADER LINE.

  CLEAR: it_bseg_ft[], it_faglflexa_aux[], it_faglflexa_ft[], it_faglflexa_mt[], it_faglflexa_aux2[],
         it_saknr[].
  MOVE it_faglflexa[] TO it_faglflexa_aux[].
  DELETE it_faglflexa_aux WHERE belnr  EQ space.
  DELETE it_faglflexa_aux WHERE ryear  EQ space.
  DELETE it_faglflexa_aux WHERE rbukrs EQ space.
  DELETE it_faglflexa_aux WHERE buzei  EQ space.
  DELETE it_faglflexa_aux WHERE bschl  EQ space.
  SORT it_faglflexa_aux BY rbukrs belnr ryear buzei bschl.
  DELETE ADJACENT DUPLICATES FROM it_faglflexa_aux COMPARING rbukrs belnr ryear buzei bschl.

  MOVE it_faglflexa_aux[] TO it_faglflexa_ft[].
  MOVE it_faglflexa_aux[] TO it_faglflexa_mt[].
  MOVE it_faglflexa_aux[] TO it_faglflexa_mt2[].

  DELETE it_faglflexa_ft WHERE awtyp NE 'VBRK'.

  DELETE it_faglflexa_mt WHERE awtyp NE 'MKPF'
                           AND awtyp NE 'IDOC'.


  DELETE it_faglflexa_mt2 WHERE awtyp NE 'MLHD'
                            AND awtyp NE 'MKPF'
                            AND awtyp NE 'RMRP'
                            AND awtyp NE 'BKPF'
                            AND awtyp NE 'IDOC'
                            AND awtyp NE 'CAJO'
                            AND awtyp NE 'RMRP'.
*                             OR PRCTR NE '0000009900'.

*  APPEND LINES OF IT_FAGLFLEXA_MT2 TO IT_FAGLFLEXA_MT.

  IF it_faglflexa_mt2[] IS NOT INITIAL.
    DATA etl956c4r8364 TYPE TABLE OF bseg.
    DATA lt_fields_l956c4r3187 TYPE fagl_t_field.
    lt_fields_l956c4r3187 = VALUE #( ( line = 'BUKRS' )
     ( line = 'BELNR' )
     ( line = 'GJAHR' )
     ( line = 'BUZEI' )
     ( line = 'BSCHL' )
     ( line = 'HKONT' )
     ( line = 'VBELN' )
     ( line = 'VBEL2' )
     ( line = 'POSN2' )
     ( line = 'MATNR' )
     ( line = 'PAOBJNR' )
     ( line = 'KOSTL' )
     ( line = 'VBUND' )
     ).

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = it_faglflexa_mt2[]
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-RBUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-RYEAR|
        it_fieldlist       = lt_fields_l956c4r3187
      IMPORTING
        et_bseg            = etl956c4r8364
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( etl956c4r8364 ) > 0.
      CLEAR it_bseg_ft.
      TYPES: BEGIN OF tyl956c4r9951,
               bukrs   TYPE bseg-bukrs,
               belnr   TYPE bseg-belnr,
               gjahr   TYPE bseg-gjahr,
               buzei   TYPE bseg-buzei,
               bschl   TYPE bseg-bschl,
               hkont   TYPE bseg-hkont,
               vbeln   TYPE bseg-vbeln,
               vbel2   TYPE bseg-vbel2,
               posn2   TYPE bseg-posn2,
               matnr   TYPE bseg-matnr,
               paobjnr TYPE bseg-paobjnr,
               kostl   TYPE bseg-kostl,
               vbund   TYPE bseg-vbund,
             END OF tyl956c4r9951.
      DATA: lml956c4r7717 TYPE tyl956c4r9951,
            lwl956c4r9643 LIKE LINE OF it_bseg_ft.
      LOOP AT etl956c4r8364 REFERENCE INTO DATA(ldrl956c4r9924).
        lml956c4r7717-bukrs = ldrl956c4r9924->bukrs.
        lml956c4r7717-belnr = ldrl956c4r9924->belnr.
        lml956c4r7717-gjahr = ldrl956c4r9924->gjahr.
        lml956c4r7717-buzei = ldrl956c4r9924->buzei.
        lml956c4r7717-bschl = ldrl956c4r9924->bschl.
        lml956c4r7717-hkont = ldrl956c4r9924->hkont.
        lml956c4r7717-vbeln = ldrl956c4r9924->vbeln.
        lml956c4r7717-vbel2 = ldrl956c4r9924->vbel2.
        lml956c4r7717-posn2 = ldrl956c4r9924->posn2.
        lml956c4r7717-matnr = ldrl956c4r9924->matnr.
        lml956c4r7717-paobjnr = ldrl956c4r9924->paobjnr.
        lml956c4r7717-kostl = ldrl956c4r9924->kostl.
        lml956c4r7717-vbund = ldrl956c4r9924->vbund.
        MOVE-CORRESPONDING lml956c4r7717 TO lwl956c4r9643.
        APPEND lwl956c4r9643 TO it_bseg_ft.
      ENDLOOP.
      sy-dbcnt = lines( etl956c4r8364 ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.

*        AND KOSTL NE SPACE.

    LOOP AT it_bseg_ft WHERE kostl IS INITIAL.
      LOOP AT it_faglflexa_mt2
          WHERE rbukrs EQ it_bseg_ft-bukrs
            AND belnr  EQ it_bseg_ft-belnr
            AND gjahr  EQ it_bseg_ft-gjahr.
        APPEND it_faglflexa_mt2 TO it_faglflexa_mt.
        DELETE it_faglflexa_mt2.
      ENDLOOP.
      DELETE it_bseg_ft.
    ENDLOOP.

    IF it_bseg_ft[] IS NOT INITIAL.
      SELECT *
        FROM csks
        INTO TABLE it_csks
         FOR ALL ENTRIES IN it_bseg_ft
          WHERE kostl EQ it_bseg_ft-kostl
            AND ( kosar EQ 'V'
                OR kosar EQ 'O' ).

      SORT: it_csks BY kostl.
      IF sy-subrc IS INITIAL.
        LOOP AT it_bseg_ft.
          READ TABLE it_csks
            WITH KEY kostl = it_bseg_ft-kostl
                     BINARY SEARCH.

          IF sy-subrc IS NOT INITIAL.

            DELETE it_bseg_ft.
          ENDIF.
        ENDLOOP.

        IF it_bseg_ft[] IS NOT INITIAL.
          SELECT *
            FROM zgl015_dre_cvend
            INTO TABLE it_cvend
             FOR ALL ENTRIES IN it_bseg_ft
              WHERE kostl EQ it_bseg_ft-kostl
                AND bukrs EQ it_bseg_ft-bukrs
                AND hkont EQ it_bseg_ft-hkont.

        ENDIF .

      ENDIF.
    ENDIF.
  ENDIF.



*ENDIF.


*  REFRESH: IT_FAGLFLEXA_MT2.
*  IT_FAGLFLEXA_MT2[] = IT_FAGLFLEXA_AUX[].
*
*  DELETE IT_FAGLFLEXA_MT2 WHERE AWTYP NE 'MLHD'
*                             OR PRCTR NE '0000009900'.
*
*  APPEND LINES OF IT_FAGLFLEXA_MT2 TO IT_FAGLFLEXA_MT.


  IF NOT it_faglflexa_ft[] IS INITIAL.

    DATA etl1030c4r3591 TYPE TABLE OF bseg.
    DATA lt_fields_l1030c4r8370 TYPE fagl_t_field.
    lt_fields_l1030c4r8370 = VALUE #( ( line = 'BUKRS' )
     ( line = 'BELNR' )
     ( line = 'GJAHR' )
     ( line = 'BUZEI' )
     ( line = 'BSCHL' )
     ( line = 'HKONT' )
     ( line = 'VBELN' )
     ( line = 'VBEL2' )
     ( line = 'POSN2' )
     ( line = 'MATNR' )
     ( line = 'PAOBJNR' )
     ( line = 'KOSTL' )
     ( line = 'VBUND' )
     ).

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = it_faglflexa_ft[]
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-RBUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-RYEAR|
        it_fieldlist       = lt_fields_l1030c4r8370
      IMPORTING
        et_bseg            = etl1030c4r3591
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( etl1030c4r3591 ) > 0.
      TYPES: BEGIN OF tyl1030c4r301,
               bukrs   TYPE bseg-bukrs,
               belnr   TYPE bseg-belnr,
               gjahr   TYPE bseg-gjahr,
               buzei   TYPE bseg-buzei,
               bschl   TYPE bseg-bschl,
               hkont   TYPE bseg-hkont,
               vbeln   TYPE bseg-vbeln,
               vbel2   TYPE bseg-vbel2,
               posn2   TYPE bseg-posn2,
               matnr   TYPE bseg-matnr,
               paobjnr TYPE bseg-paobjnr,
               kostl   TYPE bseg-kostl,
               vbund   TYPE bseg-vbund,
             END OF tyl1030c4r301.
      DATA: lml1030c4r6334 TYPE tyl1030c4r301,
            lwl1030c4r7415 LIKE LINE OF it_bseg_ft.
      LOOP AT etl1030c4r3591 REFERENCE INTO DATA(ldrl1030c4r341).
        lml1030c4r6334-bukrs = ldrl1030c4r341->bukrs.
        lml1030c4r6334-belnr = ldrl1030c4r341->belnr.
        lml1030c4r6334-gjahr = ldrl1030c4r341->gjahr.
        lml1030c4r6334-buzei = ldrl1030c4r341->buzei.
        lml1030c4r6334-bschl = ldrl1030c4r341->bschl.
        lml1030c4r6334-hkont = ldrl1030c4r341->hkont.
        lml1030c4r6334-vbeln = ldrl1030c4r341->vbeln.
        lml1030c4r6334-vbel2 = ldrl1030c4r341->vbel2.
        lml1030c4r6334-posn2 = ldrl1030c4r341->posn2.
        lml1030c4r6334-matnr = ldrl1030c4r341->matnr.
        lml1030c4r6334-paobjnr = ldrl1030c4r341->paobjnr.
        lml1030c4r6334-kostl = ldrl1030c4r341->kostl.
        lml1030c4r6334-vbund = ldrl1030c4r341->vbund.
        MOVE-CORRESPONDING lml1030c4r6334 TO lwl1030c4r7415.
        APPEND lwl1030c4r7415 TO it_bseg_ft.
      ENDLOOP.
      sy-dbcnt = lines( etl1030c4r3591 ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.




    PERFORM muda_status USING wa_dre.
    "Atapa 6
  ELSE.
    PERFORM muda_status USING wa_dre.
    "Atapa 6
  ENDIF.

  IF NOT it_faglflexa_mt[] IS INITIAL.

    DATA etl1049c4r6913 TYPE TABLE OF bseg.
    DATA lt_fields_l1049c4r5615 TYPE fagl_t_field.
    lt_fields_l1049c4r5615 = VALUE #( ( line = 'BUKRS' )
     ( line = 'BELNR' )
     ( line = 'GJAHR' )
     ( line = 'BUZEI' )
     ( line = 'BSCHL' )
     ( line = 'HKONT' )
     ( line = 'VBELN' )
     ( line = 'VBEL2' )
     ( line = 'POSN2' )
     ( line = 'MATNR' )
     ( line = 'PAOBJNR' )
     ( line = 'KOSTL' )
     ( line = 'VBUND' )
     ).

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = it_faglflexa_mt[]
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-RBUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-RYEAR|
        it_fieldlist       = lt_fields_l1049c4r5615
      IMPORTING
        et_bseg            = etl1049c4r6913
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( etl1049c4r6913 ) > 0.
      TYPES: BEGIN OF tyl1049c4r2744,
               bukrs   TYPE bseg-bukrs,
               belnr   TYPE bseg-belnr,
               gjahr   TYPE bseg-gjahr,
               buzei   TYPE bseg-buzei,
               bschl   TYPE bseg-bschl,
               hkont   TYPE bseg-hkont,
               vbeln   TYPE bseg-vbeln,
               vbel2   TYPE bseg-vbel2,
               posn2   TYPE bseg-posn2,
               matnr   TYPE bseg-matnr,
               paobjnr TYPE bseg-paobjnr,
               kostl   TYPE bseg-kostl,
               vbund   TYPE bseg-vbund,
             END OF tyl1049c4r2744.
      DATA: lml1049c4r8739 TYPE tyl1049c4r2744,
            lwl1049c4r4123 LIKE LINE OF it_bseg_ft.
      LOOP AT etl1049c4r6913 REFERENCE INTO DATA(ldrl1049c4r3918).
        lml1049c4r8739-bukrs = ldrl1049c4r3918->bukrs.
        lml1049c4r8739-belnr = ldrl1049c4r3918->belnr.
        lml1049c4r8739-gjahr = ldrl1049c4r3918->gjahr.
        lml1049c4r8739-buzei = ldrl1049c4r3918->buzei.
        lml1049c4r8739-bschl = ldrl1049c4r3918->bschl.
        lml1049c4r8739-hkont = ldrl1049c4r3918->hkont.
        lml1049c4r8739-vbeln = ldrl1049c4r3918->vbeln.
        lml1049c4r8739-vbel2 = ldrl1049c4r3918->vbel2.
        lml1049c4r8739-posn2 = ldrl1049c4r3918->posn2.
        lml1049c4r8739-matnr = ldrl1049c4r3918->matnr.
        lml1049c4r8739-paobjnr = ldrl1049c4r3918->paobjnr.
        lml1049c4r8739-kostl = ldrl1049c4r3918->kostl.
        lml1049c4r8739-vbund = ldrl1049c4r3918->vbund.
        MOVE-CORRESPONDING lml1049c4r8739 TO lwl1049c4r4123.
        APPEND lwl1049c4r4123 TO it_bseg_ft.
      ENDLOOP.
      sy-dbcnt = lines( etl1049c4r6913 ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.


    IF sy-subrc IS INITIAL.

      SELECT bukrs belnr gjahr xblnr blart
        INTO TABLE it_bkpf
       FROM bkpf
        FOR ALL ENTRIES IN it_bseg_ft
      WHERE bukrs EQ it_bseg_ft-bukrs
        AND belnr EQ it_bseg_ft-belnr
        AND gjahr EQ it_bseg_ft-gjahr.

      DELETE it_bkpf WHERE xblnr IS INITIAL.
      IF it_bkpf[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr xblnr blart
         APPENDING TABLE it_bkpf
         FROM bkpf
         FOR ALL ENTRIES IN it_bkpf
          WHERE bukrs EQ it_bkpf-bukrs
            AND belnr EQ it_bkpf-xblnr+3(10)
            AND gjahr EQ it_bkpf-gjahr.
      ENDIF.

      CLEAR: vg_tabix.
      LOOP AT it_bkpf.
        vg_tabix = sy-tabix.

        CONDENSE it_bkpf-xblnr NO-GAPS.
        wl_count = strlen( it_bkpf-xblnr ).
        IF wl_count  GT '12'.
*          IT_BKPF-REFKEY = IT_BKPF-XBLNR+3(10).
          READ TABLE it_bkpf
            WITH KEY bukrs = it_bkpf-bukrs
                     belnr = it_bkpf-xblnr+3(10)
                     gjahr = it_bkpf-gjahr.

          IF sy-subrc IS INITIAL.
            it_bkpf-estorno = 'X'.
          ENDIF.

        ENDIF.
        wl_count = strlen( it_bkpf-xblnr ).
        IF wl_count LE '12'
        AND wl_count GE 10.
          SUBTRACT 10 FROM wl_count.
          it_bkpf-refkey = it_bkpf-xblnr+wl_count(10).
        ENDIF.
        MODIFY it_bkpf INDEX vg_tabix TRANSPORTING refkey estorno.
        CLEAR: vg_tabix, it_bkpf-estorno.
      ENDLOOP.

      DELETE it_bkpf WHERE refkey IS INITIAL.
      IF it_bkpf[] IS NOT INITIAL.
        SELECT * INTO TABLE it_j_1bnflin
          FROM j_1bnflin
          FOR ALL ENTRIES IN it_bkpf
        WHERE refkey EQ it_bkpf-refkey.

        PERFORM muda_status USING wa_dre.
      ENDIF.
    ENDIF.
    "Atapa 7
  ELSE.
    PERFORM muda_status USING wa_dre.
    "Atapa 7
  ENDIF.

  DELETE it_bseg_fta WHERE vbeln EQ space.

  "Ajustanto Grupo de Mercadoria por fatura
  CLEAR: it_bseg_fta[].

  IF NOT it_bseg_ft[] IS INITIAL.
    it_bseg_ft_aux[] = it_bseg_ft[].
    DELETE it_bseg_ft_aux WHERE paobjnr IS INITIAL.

    SELECT bukrs kokrs
      FROM tka02
      INTO TABLE it_tka02
       FOR ALL ENTRIES IN it_bseg_ft_aux
        WHERE bukrs EQ it_bseg_ft_aux-bukrs.

*{   INSERT         QASK900209                                        2
**---> 05/07/2023 - Migração S4 - DL
    SORT it_tka02 BY bukrs kokrs.
*<--- 05/07/2023 - Migração S4 - DL
*}   INSERT
    SORT it_bseg_ft_aux BY bukrs.

    LOOP AT it_bseg_ft_aux.
      READ TABLE it_tka02
        WITH KEY bukrs =  it_bseg_ft_aux-bukrs
                 BINARY SEARCH.

      CASE it_tka02-kokrs.
        WHEN 'MAGI'.
          APPEND it_bseg_ft_aux TO it_bseg_ft_aux_gi.
        WHEN 'MGLD'.
          APPEND it_bseg_ft_aux TO it_bseg_ft_aux_ld.
        WHEN 'MGAR'.
          APPEND it_bseg_ft_aux TO it_bseg_ft_aux_ar.
        WHEN 'MGBG'. "NAVEGAÇÕES UNIDAS TAPAJÓS
          APPEND it_bseg_ft_aux TO it_bseg_ft_aux_bg.
      ENDCASE.
    ENDLOOP.

    IF it_bseg_ft_aux_gi[] IS NOT INITIAL.
      SELECT paobjnr artnr kmmakl
        FROM ce4magi_acct
        INTO  TABLE it_gi_acct
         FOR ALL ENTRIES IN it_bseg_ft_aux_gi
          WHERE aktbo    EQ 'X'
            AND paobjnr  EQ it_bseg_ft_aux_gi-paobjnr.
    ENDIF.

    IF it_bseg_ft_aux_ld[] IS NOT INITIAL.
      SELECT paobjnr artnr kmmakl
        FROM ce4mgld_acct
        INTO  TABLE it_ld_acct
         FOR ALL ENTRIES IN it_bseg_ft_aux_ld
          WHERE aktbo    EQ 'X'
            AND paobjnr  EQ it_bseg_ft_aux_ld-paobjnr.

    ENDIF.

    IF it_bseg_ft_aux_ar[] IS NOT INITIAL.
      SELECT paobjnr artnr kmmakl
        FROM ce4mgar_acct
        INTO  TABLE it_ar_acct
         FOR ALL ENTRIES IN it_bseg_ft_aux_ar
          WHERE aktbo    EQ 'X'
            AND paobjnr  EQ it_bseg_ft_aux_ar-paobjnr.

    ENDIF.

    IF it_bseg_ft_aux_bg[] IS NOT INITIAL.
      SELECT paobjnr artnr kmmakl
        FROM ce4mgbg_acct
        INTO  TABLE it_bg_acct
         FOR ALL ENTRIES IN it_bseg_ft_aux_bg
          WHERE aktbo    EQ 'X'
            AND paobjnr  EQ it_bseg_ft_aux_bg-paobjnr.
    ENDIF.

    SORT: it_gi_acct BY paobjnr.
    SORT: it_ld_acct BY paobjnr.
    SORT: it_ar_acct BY paobjnr.
    SORT: it_bg_acct BY paobjnr.
*{   INSERT         QASK900209                                        3
*---> 05/07/2023 - Migração S4 - DL
    SORT it_tka02 BY bukrs.
*<--- 05/07/2023 - Migração S4 - DL
*}   INSERT
    LOOP AT it_bseg_ft.
      vg_tabix = sy-tabix.
      IF ( it_bseg_ft-matnr IS INITIAL ) OR ( it_bseg_ft-matkl IS INITIAL ).
        READ TABLE it_tka02
          WITH KEY bukrs = it_bseg_ft-bukrs
                   BINARY SEARCH.

        CASE it_tka02-kokrs.
          WHEN 'MAGI'.
            READ TABLE it_gi_acct INTO it_acct
              WITH KEY paobjnr = it_bseg_ft-paobjnr
                         BINARY SEARCH.
          WHEN 'MGLD'.
            READ TABLE it_ld_acct INTO it_acct
              WITH KEY paobjnr = it_bseg_ft-paobjnr
                         BINARY SEARCH.
          WHEN 'MGAR'.
            READ TABLE it_ar_acct INTO it_acct
              WITH KEY paobjnr = it_bseg_ft-paobjnr
                         BINARY SEARCH.
          WHEN 'MGBG'.
            READ TABLE it_bg_acct INTO it_acct
              WITH KEY paobjnr = it_bseg_ft-paobjnr
                         BINARY SEARCH.
          WHEN OTHERS.
            MOVE 8 TO sy-subrc.

        ENDCASE.

        IF sy-subrc IS INITIAL.
          it_bseg_ft-matnr = it_acct-artnr.
          it_bseg_ft-matkl = it_acct-kmmakl.
          MODIFY it_bseg_ft INDEX vg_tabix TRANSPORTING matnr matkl.
        ENDIF.
      ENDIF.
    ENDLOOP.

    MOVE it_bseg_ft[] TO it_bseg_fta[].
    DELETE it_bseg_fta WHERE matnr EQ space.
    SORT it_bseg_fta BY matnr.
    DELETE ADJACENT DUPLICATES FROM it_bseg_fta COMPARING matnr.

    IF NOT it_bseg_fta[] IS INITIAL.
      CLEAR: it_mara[].

      SELECT * INTO TABLE it_mara
        FROM mara
         FOR ALL ENTRIES IN it_bseg_fta
       WHERE matnr EQ it_bseg_fta-matnr.
    ENDIF.
    SORT it_mara BY matnr.
    SORT it_bkpf BY bukrs belnr gjahr blart.
    SORT it_j_1bnflin BY refkey.
    SORT it_cvend BY hkont kostl.

    LOOP AT it_bseg_ft.
      IF it_bseg_ft-matkl IS INITIAL.
        vg_tabix = sy-tabix.
        READ TABLE it_mara WITH KEY matnr = it_bseg_ft-matnr BINARY SEARCH.

        IF sy-subrc IS INITIAL AND it_bseg_ft-kostl IS INITIAL.
          it_bseg_ft-matkl = it_mara-matkl.
          MODIFY it_bseg_ft INDEX vg_tabix TRANSPORTING matkl.
        ELSEIF it_bseg_ft-matnr IS INITIAL
          AND it_bseg_ft-kostl IS INITIAL.
          READ TABLE it_bkpf
            WITH KEY bukrs = it_bseg_ft-bukrs
                     belnr = it_bseg_ft-belnr
                     gjahr = it_bseg_ft-gjahr
                     blart = 'WR'
                     BINARY SEARCH.

          IF sy-subrc IS INITIAL.
            READ TABLE it_j_1bnflin
              WITH KEY refkey = it_bkpf-refkey
                       BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              it_bseg_ft-matkl = it_j_1bnflin-matkl.
              MODIFY it_bseg_ft INDEX vg_tabix TRANSPORTING matkl.
            ENDIF.
*          ELSE.
*            READ TABLE IT_ACCT
*              WITH KEY PAOBJNR = IT_BSEG_FT-PAOBJNR
*                         BINARY SEARCH.
*            IF SY-SUBRC IS INITIAL.
*              WL_COUNT = STRLEN( IT_ACCT-ARTNR ).
*              SUBTRACT 9 FROM WL_COUNT.
*              IT_BSEG_FT-MATKL = IT_ACCT-ARTNR+WL_COUNT(9).
*              MODIFY IT_BSEG_FT INDEX VG_TABIX TRANSPORTING MATKL.
*            ENDIF.
          ENDIF.
        ELSEIF it_bseg_ft-kostl IS NOT INITIAL.
          READ TABLE it_cvend
            WITH KEY hkont = it_bseg_ft-hkont
                     kostl = it_bseg_ft-kostl
                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_bseg_ft-matkl = it_cvend-matkl.
            MODIFY it_bseg_ft INDEX vg_tabix TRANSPORTING matkl.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    ENDIF.
  ENDIF.

  PERFORM muda_status USING wa_dre.
  "Atapa 8
  "--------------------------------------------------------------------------------
  "--------------------------------------------------------------------------------

  SORT it_bseg_ft  BY bukrs belnr gjahr buzei hkont matkl.
  SORT it_bseg_fta BY bukrs belnr gjahr.


  LOOP AT it_faglflexa_totmt.

    vg_tabix = sy-tabix.

    it_faglflexa_totmt-hsl = 0.
    it_faglflexa_totmt-ksl = 0.
    it_faglflexa_totmt-msl = 0.
    it_faglflexa_totmt-osl = 0.
    CLEAR: it_faglflexa_totmt-rassc.

    LOOP AT it_faglflexa WHERE racct EQ it_faglflexa_totmt-saknr.
      IF ( it_faglflexa-awtyp EQ 'VBRK' ) OR ( it_faglflexa-awtyp EQ 'MKPF' ) OR ( it_faglflexa-awtyp EQ 'IDOC' )
        OR ( it_faglflexa-awtyp EQ 'BKPF' ) OR ( it_faglflexa-awtyp EQ 'MLHD' ) OR ( it_faglflexa-awtyp EQ 'RMRP' )
        OR ( it_faglflexa-awtyp EQ 'CAJO' ) OR ( it_faglflexa-awtyp EQ 'RMRP' ).
        READ TABLE it_bseg_ft WITH KEY bukrs = it_faglflexa-rbukrs
                                       belnr = it_faglflexa-belnr
                                       gjahr = it_faglflexa-ryear
                                       buzei = it_faglflexa-buzei
                                       hkont = it_faglflexa-racct
                                       matkl = it_faglflexa_totmt-matkl
                                       BINARY SEARCH.
        IF NOT sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.

      READ TABLE it_bkpf
          WITH KEY bukrs = it_bseg_ft-bukrs
                   belnr = it_bseg_ft-belnr
                   gjahr = it_bseg_ft-gjahr
                   blart = 'WR'
                   BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_j_1bnflin
            WITH KEY refkey = it_bkpf-refkey
                     BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF it_bkpf-estorno IS NOT INITIAL.
            MULTIPLY it_j_1bnflin-menge BY -1.
          ENDIF.
          it_faglflexa_totmt-msl = it_faglflexa_totmt-msl + it_j_1bnflin-menge.
        ENDIF.
      ELSE.
        it_faglflexa_totmt-msl = it_faglflexa_totmt-msl + it_faglflexa-msl.
      ENDIF.

      IF it_faglflexa-rbukrs = '0101'. " Paraguai moeda interna
        it_faglflexa_totmt-hsl = it_faglflexa_totmt-hsl + ( it_faglflexa-hsl * 100 ).
      ELSE.
        it_faglflexa_totmt-hsl = it_faglflexa_totmt-hsl + it_faglflexa-hsl.
      ENDIF.

      it_faglflexa_totmt-ksl = it_faglflexa_totmt-ksl + it_faglflexa-ksl.
      it_faglflexa_totmt-osl = it_faglflexa_totmt-osl + it_faglflexa-osl.
      it_faglflexa_totmt-rassc = it_bseg_ft-vbund.

    ENDLOOP.
    MODIFY it_faglflexa_totmt INDEX vg_tabix.
  ENDLOOP.

** monta tabela para o BO
  APPEND LINES OF it_faglflexa TO it_faglflexa_aux2.

  DELETE it_faglflexa_aux2 WHERE awtyp NE 'VBRK'
                             AND awtyp NE 'MKPF'
                             AND awtyp NE 'IDOC'
                             AND awtyp NE 'BKPF'
                             AND awtyp NE 'MLHD'
                             AND awtyp NE 'RMRP'
                             AND awtyp NE 'CAJO'.

*---> 04/07/2023 - Migração S4 - WS
  SORT: it_faglflexa_aux2,
        it_bseg_ft.
*<--- 04/07/2023 - Migração S4 - WS

  DELETE ADJACENT DUPLICATES FROM it_faglflexa_aux2 COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM it_bseg_ft COMPARING ALL FIELDS.

  LOOP AT it_faglflexa_aux2.
    LOOP AT it_bseg_ft WHERE bukrs = it_faglflexa_aux2-rbukrs
                         AND belnr = it_faglflexa_aux2-belnr
                         AND gjahr = it_faglflexa_aux2-ryear
                         AND buzei = it_faglflexa_aux2-buzei
                         AND hkont = it_faglflexa_aux2-racct
                         AND matkl IS NOT INITIAL.

*    IF SY-SUBRC IS INITIAL.
      READ TABLE it_bkpf
          WITH KEY bukrs = it_bseg_ft-bukrs
                   belnr = it_bseg_ft-belnr
                   gjahr = it_bseg_ft-gjahr
                   blart = 'WR'
                   BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_j_1bnflin
            WITH KEY refkey = it_bkpf-refkey
                     BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF it_bkpf-estorno IS NOT INITIAL.
            MULTIPLY it_j_1bnflin-menge BY -1.
          ENDIF.
          it_saknr-msl = it_j_1bnflin-menge.
        ENDIF.
      ELSE.
        it_saknr-msl = it_faglflexa_aux2-msl.
      ENDIF.

      IF it_faglflexa_aux2-rbukrs = '0101'. " Paraguai moeda interna
        it_saknr-hsl   = it_faglflexa_aux2-hsl * 100.
      ELSE.
        it_saknr-hsl   = it_faglflexa_aux2-hsl.
      ENDIF.

      it_saknr-ksl   = it_faglflexa_aux2-ksl.
      it_saknr-osl   = it_faglflexa_aux2-osl.
      it_saknr-vbund = it_bseg_ft-vbund.
      it_saknr-matkl = it_bseg_ft-matkl.
      it_saknr-saknr = it_bseg_ft-hkont.


      COLLECT it_saknr.
    ENDLOOP.
    CLEAR: it_saknr.
  ENDLOOP.

  SORT it_faglflexa_totmt BY saknr matkl.
  LOOP AT it_saknr.

    READ TABLE it_faglflexa_totmt WITH KEY saknr = it_saknr-saknr
                                           matkl = it_saknr-matkl BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl029_dre_dados.
      it_zgl029_dre_dados-bukrs          = wa_dre-bukrs.
      it_zgl029_dre_dados-versn          = wa_dre-versn.
      it_zgl029_dre_dados-monat          = wa_dre-monat.
      it_zgl029_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl029_dre_dados-vbund          = it_saknr-vbund.
      it_zgl029_dre_dados-saknr          = it_saknr-saknr.
      it_zgl029_dre_dados-matkl          = it_saknr-matkl.
      it_zgl029_dre_dados-qtd_ton        = it_saknr-msl.
      it_zgl029_dre_dados-vlr_rea        = it_saknr-hsl.
      it_zgl029_dre_dados-vlr_dolar      = it_saknr-ksl.
      it_zgl029_dre_dados-vlr_grupo      = it_saknr-osl.

      APPEND it_zgl029_dre_dados.
    ENDIF.
  ENDLOOP.
  PERFORM muda_status USING wa_dre.
  "Atapa 9

ENDFORM.                    " TOTALIZAR_FLEXA_MT

*&---------------------------------------------------------------------*
*&      Form  INICIALIZA_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FAGLFLEXT_TOTAL  text
*----------------------------------------------------------------------*
FORM inicializa_total  USING it_faglflext_total TYPE faglflext.

  it_faglflext_total-ksl01 = 0.
  it_faglflext_total-ksl02 = 0.
  it_faglflext_total-ksl03 = 0.
  it_faglflext_total-ksl04 = 0.
  it_faglflext_total-ksl05 = 0.
  it_faglflext_total-ksl06 = 0.
  it_faglflext_total-ksl07 = 0.
  it_faglflext_total-ksl08 = 0.
  it_faglflext_total-ksl09 = 0.
  it_faglflext_total-ksl10 = 0.
  it_faglflext_total-ksl11 = 0.
  it_faglflext_total-ksl12 = 0.
  it_faglflext_total-ksl13 = 0.
  it_faglflext_total-ksl14 = 0.
  it_faglflext_total-ksl15 = 0.
  it_faglflext_total-ksl16 = 0.

  it_faglflext_total-hsl01 = 0.
  it_faglflext_total-hsl02 = 0.
  it_faglflext_total-hsl03 = 0.
  it_faglflext_total-hsl04 = 0.
  it_faglflext_total-hsl05 = 0.
  it_faglflext_total-hsl06 = 0.
  it_faglflext_total-hsl07 = 0.
  it_faglflext_total-hsl08 = 0.
  it_faglflext_total-hsl09 = 0.
  it_faglflext_total-hsl10 = 0.
  it_faglflext_total-hsl11 = 0.
  it_faglflext_total-hsl12 = 0.
  it_faglflext_total-hsl13 = 0.
  it_faglflext_total-hsl14 = 0.
  it_faglflext_total-hsl15 = 0.
  it_faglflext_total-hsl16 = 0.

  it_faglflext_total-osl01 = 0.
  it_faglflext_total-osl02 = 0.
  it_faglflext_total-osl03 = 0.
  it_faglflext_total-osl04 = 0.
  it_faglflext_total-osl05 = 0.
  it_faglflext_total-osl06 = 0.
  it_faglflext_total-osl07 = 0.
  it_faglflext_total-osl08 = 0.
  it_faglflext_total-osl09 = 0.
  it_faglflext_total-osl10 = 0.
  it_faglflext_total-osl11 = 0.
  it_faglflext_total-osl12 = 0.
  it_faglflext_total-osl13 = 0.
  it_faglflext_total-osl14 = 0.
  it_faglflext_total-osl15 = 0.
  it_faglflext_total-osl16 = 0.

ENDFORM.                    " INICIALIZA_TOTAL

*&---------------------------------------------------------------------*
*&      Form  MUDA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DRE  text
*----------------------------------------------------------------------*
FORM muda_status  USING p_wa_dre TYPE zgl020_dre_dados.
  DATA: pc_percentual TYPE p33_procn.

  vg_qtd = vg_qtd + 1.
  pc_percentual = vg_qtd / vg_qtd_total.
  pc_percentual = pc_percentual * 100.
  p_wa_dre-status_proc = pc_percentual.
  MODIFY zgl020_dre_dados FROM p_wa_dre.
ENDFORM.                    " MUDA_STATUS

*&---------------------------------------------------------------------*
*&      Form  TOTALIZAR_FLEXA_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM totalizar_flexa_cc TABLES it_faglflexa STRUCTURE faglflexa
                         USING wa_dre TYPE zgl020_dre_dados.
* Tabela que ira conter a totalizacao dividida pela Sociedade
  TYPES: BEGIN OF ty_saknr,
           saknr TYPE bseg-saknr,
           kostl TYPE bsis-kostl,
           vbund TYPE bseg-vbund,
           msl   TYPE faglflexa-msl,
           hsl   TYPE faglflexa-hsl,
           ksl   TYPE faglflexa-ksl,
           osl   TYPE faglflexa-osl,
         END OF ty_saknr.

  DATA: vg_tabix         TYPE sy-tabix,
        it_bsis          TYPE TABLE OF bsis WITH HEADER LINE,
        it_bsis_aux      TYPE TABLE OF bsis WITH HEADER LINE,
        it_csks          TYPE TABLE OF csks WITH HEADER LINE,
        it_cvend         TYPE TABLE OF zgl015_dre_cvend WITH HEADER LINE,
        it_faglflexa_aux TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_coas          TYPE TABLE OF coas WITH HEADER LINE,
        it_saknr         TYPE TABLE OF ty_saknr WITH HEADER LINE.

  REFRESH: it_saknr.
  CLEAR: it_saknr.

  MOVE it_faglflexa[] TO it_faglflexa_aux[].
  DELETE it_faglflexa_aux WHERE belnr  EQ space.
  DELETE it_faglflexa_aux WHERE ryear  EQ space.
  DELETE it_faglflexa_aux WHERE rbukrs EQ space.
  DELETE it_faglflexa_aux WHERE buzei  EQ space.


  SELECT * INTO TABLE it_bsis
    FROM bsis
     FOR ALL ENTRIES IN it_faglflexa_aux
   WHERE bukrs EQ it_faglflexa_aux-rbukrs
     AND belnr EQ it_faglflexa_aux-belnr
     AND gjahr EQ it_faglflexa_aux-ryear
     AND buzei EQ it_faglflexa_aux-buzei
     AND kostl NE space.

  SORT it_faglflexa BY rbukrs belnr ryear buzei.
  LOOP AT it_bsis.
    READ TABLE it_faglflexa WITH KEY rbukrs = it_bsis-bukrs
                                     belnr  = it_bsis-belnr
                                     ryear  = it_bsis-gjahr
                                     buzei  = it_bsis-buzei BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      it_faglflexa-rcntr = it_bsis-kostl.
      MODIFY it_faglflexa INDEX sy-tabix TRANSPORTING rcntr.
    ENDIF.
  ENDLOOP.

  " Limpa Ordens Internas diferentes de ZSTA
  "****************************************************************
  CLEAR: it_bsis_aux[].
  MOVE it_bsis[] TO it_bsis_aux[].
  DELETE it_bsis_aux WHERE aufnr EQ space.
  SORT it_bsis_aux BY aufnr.
  DELETE ADJACENT DUPLICATES FROM it_bsis_aux COMPARING aufnr.

  IF NOT it_bsis_aux[] IS INITIAL.
    SELECT * INTO TABLE it_coas
      FROM coas
       FOR ALL ENTRIES IN it_bsis_aux
     WHERE aufnr EQ it_bsis_aux-aufnr
       AND auart NE 'ZSTA'.

  ENDIF.
  REFRESH: it_bsis_aux.
  it_bsis_aux[] = it_bsis[].
  DELETE it_bsis_aux WHERE aufnr IS NOT INITIAL.

  SORT: it_coas BY aufnr.
  LOOP AT it_bsis.
    READ TABLE it_coas
      WITH KEY aufnr = it_bsis-aufnr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      APPEND it_bsis TO it_bsis_aux.
    ENDIF.
  ENDLOOP.

  LOOP AT it_coas.
    DELETE it_bsis WHERE aufnr EQ it_coas-aufnr.
  ENDLOOP.
*  DELETE IT_BSIS WHERE AUFNR IS INITIAL.

  "****************************************************************


  IF it_bsis_aux[] IS NOT INITIAL.
    SELECT *
      FROM csks
      INTO TABLE it_csks
       FOR ALL ENTRIES IN it_bsis_aux
        WHERE kostl EQ it_bsis_aux-kostl
          AND ( kosar EQ 'V'
           OR kosar EQ 'O' ).

    SORT: it_csks BY kostl.

    LOOP AT it_bsis_aux.
      READ TABLE it_csks
        WITH KEY kostl = it_bsis_aux-kostl
                 BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        DELETE it_bsis_aux.
      ENDIF.
    ENDLOOP.

    IF it_bsis_aux[] IS NOT INITIAL.
      SELECT *
        FROM zgl015_dre_cvend
        INTO TABLE it_cvend
         FOR ALL ENTRIES IN it_bsis_aux
          WHERE kostl EQ it_bsis_aux-kostl
            AND bukrs EQ it_bsis_aux-bukrs
            AND hkont EQ it_bsis_aux-hkont.

      SORT: it_cvend BY hkont kostl bukrs.
      LOOP AT it_bsis_aux.
        READ TABLE it_cvend
          WITH KEY hkont = it_bsis_aux-hkont
                   kostl = it_bsis_aux-kostl
                   bukrs = it_bsis_aux-bukrs
                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          DELETE it_bsis_aux.
          IF wa_dre-bukrs EQ '0100'.
            DELETE TABLE it_bsis FROM it_bsis_aux.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

  APPEND LINES OF it_bsis_aux TO it_bsis.

  SORT: it_bsis        BY bukrs belnr gjahr buzei hkont kostl,
        it_faglflexa   BY racct rcntr.

  "ALRS 21.02.2014
  LOOP AT it_faglflexa_totcc.

    vg_tabix = sy-tabix.

    it_faglflexa_totcc-hsl = 0.
    it_faglflexa_totcc-ksl = 0.
    it_faglflexa_totcc-osl = 0.
    CLEAR: it_faglflexa_totcc-rassc.

    READ TABLE it_faglflexa WITH KEY racct = it_faglflexa_totcc-saknr
                                     rcntr = it_faglflexa_totcc-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT it_faglflexa WHERE racct EQ it_faglflexa_totcc-saknr
                             AND rcntr EQ it_faglflexa_totcc-kostl.

        READ TABLE it_bsis WITH KEY  bukrs = it_faglflexa-rbukrs
                                     belnr = it_faglflexa-belnr
                                     gjahr = it_faglflexa-ryear
                                     buzei = it_faglflexa-buzei
                                     hkont = it_faglflexa-racct
                                     kostl = it_faglflexa-rcntr BINARY SEARCH.
        IF NOT sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.

        IF it_faglflexa-rbukrs = '0101'. " Paraguai moeda interna
          it_faglflexa_totcc-hsl = it_faglflexa_totcc-hsl + ( it_faglflexa-hsl * 100 ).
        ELSE.
          it_faglflexa_totcc-hsl = it_faglflexa_totcc-hsl + it_faglflexa-hsl.
        ENDIF.

        it_faglflexa_totcc-ksl = it_faglflexa_totcc-ksl + it_faglflexa-ksl.
        it_faglflexa_totcc-osl = it_faglflexa_totcc-osl + it_faglflexa-osl.
        it_faglflexa_totcc-rassc = it_bsis-vbund.


      ENDLOOP.
    ENDIF.

    MODIFY it_faglflexa_totcc INDEX vg_tabix TRANSPORTING hsl ksl osl rassc.
  ENDLOOP.

  SORT: it_bsis BY bukrs hkont augbl zuonr gjahr belnr buzei kostl.
  DELETE ADJACENT DUPLICATES FROM it_bsis COMPARING bukrs hkont augbl zuonr gjahr belnr buzei kostl.
  SORT it_bsis BY bukrs belnr gjahr buzei hkont.
  LOOP AT it_faglflexa.
    READ TABLE it_bsis WITH KEY bukrs = it_faglflexa-rbukrs
                                belnr = it_faglflexa-belnr
                                gjahr = it_faglflexa-ryear
                                buzei = it_faglflexa-buzei
                                hkont = it_faglflexa-racct BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    LOOP AT it_bsis WHERE bukrs = it_faglflexa-rbukrs
                      AND belnr = it_faglflexa-belnr
                      AND gjahr = it_faglflexa-ryear
                      AND buzei = it_faglflexa-buzei
                      AND hkont = it_faglflexa-racct.

      IF it_faglflexa-rbukrs = '0101'. " Paraguai moeda interna
        it_saknr-hsl   = it_faglflexa-hsl * 100.
      ELSE.
        it_saknr-hsl   = it_faglflexa-hsl.
      ENDIF.

      it_saknr-ksl   = it_faglflexa-ksl.
      it_saknr-osl   = it_faglflexa-osl.
      it_saknr-vbund = it_bsis-vbund.
      it_saknr-kostl = it_bsis-kostl.
      it_saknr-saknr = it_bsis-hkont.

      COLLECT it_saknr.
    ENDLOOP.
    CLEAR: it_saknr.
  ENDLOOP.

  SORT it_faglflexa_totcc BY saknr kostl.
  LOOP AT it_saknr.

    READ TABLE it_faglflexa_totcc WITH KEY saknr = it_saknr-saknr
                                           kostl = it_saknr-kostl BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl029_dre_dados.
      it_zgl029_dre_dados-bukrs          = wa_dre-bukrs.
      it_zgl029_dre_dados-versn          = wa_dre-versn.
      it_zgl029_dre_dados-monat          = wa_dre-monat.
      it_zgl029_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl029_dre_dados-vbund          = it_saknr-vbund.
      it_zgl029_dre_dados-saknr          = it_saknr-saknr.
      it_zgl029_dre_dados-kostl          = it_saknr-kostl.
      it_zgl029_dre_dados-vlr_rea        = it_saknr-hsl.
      it_zgl029_dre_dados-vlr_dolar      = it_saknr-ksl.
      it_zgl029_dre_dados-vlr_grupo      = it_saknr-osl.

      APPEND it_zgl029_dre_dados.
    ENDIF.
    CLEAR: it_zgl029_dre_dados.
  ENDLOOP.

ENDFORM.                    " TOTALIZAR_FLEXA_CC

*&---------------------------------------------------------------------*
*&      Form  TOTALIZAR_FLEXA_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM totalizar_flexa_cl TABLES it_faglflexa STRUCTURE faglflexa
                         USING wa_dre TYPE zgl020_dre_dados.

* Tabela que ira conter a totalizacao dividida pela Sociedade
  TYPES: BEGIN OF ty_saknr,
           saknr TYPE bseg-saknr,
           prctr TYPE bsis-prctr,
           vbund TYPE bseg-vbund,
           msl   TYPE faglflexa-msl,
           hsl   TYPE faglflexa-hsl,
           ksl   TYPE faglflexa-ksl,
           osl   TYPE faglflexa-osl,
         END OF ty_saknr.

  TYPES: BEGIN OF ty_acct,
           paobjnr TYPE ce4magi_acct-paobjnr,
           artnr   TYPE ce4magi_acct-artnr,
         END OF ty_acct.
  TYPES: BEGIN OF ty_tka02,
           bukrs TYPE tka02-bukrs,
           kokrs TYPE tka02-kokrs,
         END OF ty_tka02.

  TYPES: BEGIN OF tb_bseg.
  TYPES: bukrs   LIKE bseg-bukrs,
         belnr   LIKE bseg-belnr,
         gjahr   LIKE bseg-gjahr,
         buzei   LIKE bseg-buzei,
         bschl   LIKE bseg-bschl,
         hkont   LIKE bseg-hkont,
         vbeln   LIKE bseg-vbeln,
         vbel2   LIKE bseg-vbel2,
         posn2   LIKE bseg-posn2,
         matnr   LIKE bseg-matnr,
         matkl   TYPE matkl,
         paobjnr TYPE bseg-paobjnr,
         kostl   TYPE bseg-kostl,
         vbund   TYPE bseg-vbund.

  TYPES: END OF tb_bseg.


  DATA: vg_tabix          TYPE sy-tabix,
        it_bsis           TYPE TABLE OF bsis WITH HEADER LINE,
        it_bsis_aux       TYPE TABLE OF bsis WITH HEADER LINE,
        it_faglflexa_aux  TYPE TABLE OF faglflexa WITH HEADER LINE,
        it_coas           TYPE TABLE OF coas WITH HEADER LINE,
        it_saknr          TYPE TABLE OF ty_saknr WITH HEADER LINE,
        it_bseg_ft        TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux    TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_gi TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_ld TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_ar TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_bseg_ft_aux_bg TYPE TABLE OF tb_bseg WITH HEADER LINE,
        it_gi_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_ld_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_ar_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_bg_acct        TYPE TABLE OF ty_acct WITH HEADER LINE,
        it_tka02          TYPE TABLE OF ty_tka02 WITH HEADER LINE,
        it_acct           TYPE TABLE OF ty_acct WITH HEADER LINE.

  REFRESH: it_saknr.
  CLEAR: it_saknr.

  MOVE it_faglflexa[] TO it_faglflexa_aux[].
  DELETE it_faglflexa_aux WHERE belnr  EQ space.
  DELETE it_faglflexa_aux WHERE ryear  EQ space.
  DELETE it_faglflexa_aux WHERE rbukrs EQ space.
  DELETE it_faglflexa_aux WHERE buzei  EQ space.

  SELECT * INTO TABLE it_bsis
    FROM bsis
     FOR ALL ENTRIES IN it_faglflexa_aux
   WHERE bukrs EQ it_faglflexa_aux-rbukrs
     AND belnr EQ it_faglflexa_aux-belnr
     AND gjahr EQ it_faglflexa_aux-ryear
     AND buzei EQ it_faglflexa_aux-buzei.

  SORT it_faglflexa BY  rbukrs belnr ryear buzei.
  LOOP AT it_bsis.
    READ TABLE it_faglflexa WITH KEY rbukrs = it_bsis-bukrs
                                     belnr  = it_bsis-belnr
                                     ryear  = it_bsis-gjahr
                                     buzei  = it_bsis-buzei BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      it_faglflexa-prctr = it_bsis-prctr.
      MODIFY it_faglflexa INDEX sy-tabix TRANSPORTING rcntr.
    ENDIF.
  ENDLOOP.

  " Limpa Ordens Internas diferentes de ZSTA
  "****************************************************************
  CLEAR: it_bsis_aux[].
  MOVE it_bsis[] TO it_bsis_aux[].
  DELETE it_bsis_aux WHERE aufnr EQ space.
  SORT it_bsis_aux BY aufnr.
  DELETE ADJACENT DUPLICATES FROM it_bsis_aux COMPARING aufnr.

  IF NOT it_bsis_aux[] IS INITIAL.
    SELECT * INTO TABLE it_coas
      FROM coas
       FOR ALL ENTRIES IN it_bsis_aux
     WHERE aufnr EQ it_bsis_aux-aufnr
       AND auart NE 'ZSTA'.

    LOOP AT it_coas.
      DELETE it_bsis WHERE aufnr EQ it_coas-aufnr.
    ENDLOOP.
  ENDIF.

  DATA etl1913c2r3749 TYPE TABLE OF bseg.
  DATA lt_fields_l1913c2r8102 TYPE fagl_t_field.
  lt_fields_l1913c2r8102 = VALUE #( ( line = 'BUKRS' )
   ( line = 'BELNR' )
   ( line = 'GJAHR' )
   ( line = 'BUZEI' )
   ( line = 'BSCHL' )
   ( line = 'HKONT' )
   ( line = 'VBELN' )
   ( line = 'VBEL2' )
   ( line = 'POSN2' )
   ( line = 'MATNR' )
   ( line = 'PAOBJNR' )
   ( line = 'KOSTL' )
   ( line = 'VBUND' )
   ).

  CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING
      it_for_all_entries = it_bsis[]
      i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
      it_fieldlist       = lt_fields_l1913c2r8102
    IMPORTING
      et_bseg            = etl1913c2r3749
    EXCEPTIONS
      not_found          = 1.
  IF sy-subrc = 0 AND lines( etl1913c2r3749 ) > 0.
    CLEAR it_bseg_ft.
    TYPES: BEGIN OF tyl1913c2r328,
             bukrs   TYPE bseg-bukrs,
             belnr   TYPE bseg-belnr,
             gjahr   TYPE bseg-gjahr,
             buzei   TYPE bseg-buzei,
             bschl   TYPE bseg-bschl,
             hkont   TYPE bseg-hkont,
             vbeln   TYPE bseg-vbeln,
             vbel2   TYPE bseg-vbel2,
             posn2   TYPE bseg-posn2,
             matnr   TYPE bseg-matnr,
             paobjnr TYPE bseg-paobjnr,
             kostl   TYPE bseg-kostl,
             vbund   TYPE bseg-vbund,
           END OF tyl1913c2r328.
    DATA: lml1913c2r5362 TYPE tyl1913c2r328,
          lwl1913c2r6677 LIKE LINE OF it_bseg_ft.
    LOOP AT etl1913c2r3749 REFERENCE INTO DATA(ldrl1913c2r1535).
      lml1913c2r5362-bukrs = ldrl1913c2r1535->bukrs.
      lml1913c2r5362-belnr = ldrl1913c2r1535->belnr.
      lml1913c2r5362-gjahr = ldrl1913c2r1535->gjahr.
      lml1913c2r5362-buzei = ldrl1913c2r1535->buzei.
      lml1913c2r5362-bschl = ldrl1913c2r1535->bschl.
      lml1913c2r5362-hkont = ldrl1913c2r1535->hkont.
      lml1913c2r5362-vbeln = ldrl1913c2r1535->vbeln.
      lml1913c2r5362-vbel2 = ldrl1913c2r1535->vbel2.
      lml1913c2r5362-posn2 = ldrl1913c2r1535->posn2.
      lml1913c2r5362-matnr = ldrl1913c2r1535->matnr.
      lml1913c2r5362-paobjnr = ldrl1913c2r1535->paobjnr.
      lml1913c2r5362-kostl = ldrl1913c2r1535->kostl.
      lml1913c2r5362-vbund = ldrl1913c2r1535->vbund.
      MOVE-CORRESPONDING lml1913c2r5362 TO lwl1913c2r6677.
      APPEND lwl1913c2r6677 TO it_bseg_ft.
    ENDLOOP.
    sy-dbcnt = lines( etl1913c2r3749 ).
  ELSE.
    sy-subrc = 4.
    sy-dbcnt = 0.
  ENDIF.


  DELETE it_bseg_ft WHERE paobjnr IS INITIAL
                      AND matnr IS INITIAL.
  SELECT bukrs kokrs
     FROM tka02
     INTO TABLE it_tka02
      FOR ALL ENTRIES IN it_bseg_ft
       WHERE bukrs EQ it_bseg_ft-bukrs.

*---> 05/07/2023 - Migração S4 - DL
  SORT it_tka02 BY bukrs kokrs.
*<--- 05/07/2023 - Migração S4 - DL

  SORT it_bseg_ft BY bukrs.
  LOOP AT it_bseg_ft.
    READ TABLE it_tka02
      WITH KEY bukrs =  it_bseg_ft-bukrs
               BINARY SEARCH.

    CASE it_tka02-kokrs.
      WHEN 'MAGI'.
        APPEND it_bseg_ft_aux TO it_bseg_ft_aux_gi.
      WHEN 'MGLD'.
        APPEND it_bseg_ft_aux TO it_bseg_ft_aux_ld.
      WHEN 'MGAR'.
        APPEND it_bseg_ft_aux TO it_bseg_ft_aux_ar.
      WHEN 'MGBG'.
        APPEND it_bseg_ft_aux TO it_bseg_ft_aux_bg.
    ENDCASE.


  ENDLOOP.

  IF it_bseg_ft_aux_gi[] IS NOT INITIAL.
    SELECT paobjnr artnr
      FROM ce4magi_acct
      INTO  TABLE it_gi_acct
       FOR ALL ENTRIES IN it_bseg_ft_aux_gi
        WHERE aktbo    EQ 'X'
          AND paobjnr  EQ it_bseg_ft_aux_gi-paobjnr
          AND artnr    NE space.
  ENDIF.

  IF it_bseg_ft_aux_ld[] IS NOT INITIAL.
    SELECT paobjnr artnr
      FROM ce4mgld_acct
      INTO  TABLE it_ld_acct
       FOR ALL ENTRIES IN it_bseg_ft_aux_ld
        WHERE aktbo    EQ 'X'
          AND paobjnr  EQ it_bseg_ft_aux_ld-paobjnr
          AND artnr    NE space.

  ENDIF.

  IF it_bseg_ft_aux_ar[] IS NOT INITIAL.
    SELECT paobjnr artnr
      FROM ce4mgar_acct
      INTO  TABLE it_ar_acct
       FOR ALL ENTRIES IN it_bseg_ft_aux_ar
        WHERE aktbo    EQ 'X'
          AND paobjnr  EQ it_bseg_ft_aux_ar-paobjnr
          AND artnr    NE space.

  ENDIF.

  IF it_bseg_ft_aux_bg[] IS NOT INITIAL.
    SELECT paobjnr artnr
      FROM ce4mgbg_acct
      INTO  TABLE it_bg_acct
       FOR ALL ENTRIES IN it_bseg_ft_aux_bg
        WHERE aktbo    EQ 'X'
          AND paobjnr  EQ it_bseg_ft_aux_bg-paobjnr
          AND artnr    NE space.

  ENDIF.

  SORT: it_gi_acct BY paobjnr.
  SORT: it_ld_acct BY paobjnr.
  SORT: it_ar_acct BY paobjnr.
  SORT: it_bg_acct BY paobjnr.

*---> 05/07/2023 - Migração S4 - DL
  SORT it_tka02 BY bukrs.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT it_bseg_ft.
    vg_tabix = sy-tabix.
    IF it_bseg_ft-matnr IS INITIAL.
      READ TABLE it_tka02
        WITH KEY bukrs = it_bseg_ft-bukrs
                 BINARY SEARCH.

      CASE it_tka02-kokrs.
        WHEN 'MAGI'.
          READ TABLE it_gi_acct INTO it_acct
            WITH KEY paobjnr = it_bseg_ft-paobjnr
                       BINARY SEARCH.
        WHEN 'MGLD'.
          READ TABLE it_ld_acct INTO it_acct
            WITH KEY paobjnr = it_bseg_ft-paobjnr
                       BINARY SEARCH.
        WHEN 'MGAR'.
          READ TABLE it_ar_acct INTO it_acct
            WITH KEY paobjnr = it_bseg_ft-paobjnr
                       BINARY SEARCH.
        WHEN 'MGBG'.
          READ TABLE it_bg_acct INTO it_acct
            WITH KEY paobjnr = it_bseg_ft-paobjnr
                       BINARY SEARCH.
        WHEN OTHERS.
          MOVE 8 TO sy-subrc.

      ENDCASE.

      IF sy-subrc IS INITIAL.
        it_bseg_ft-matnr = it_acct-artnr.
        MODIFY it_bseg_ft INDEX vg_tabix TRANSPORTING matnr.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "****************************************************************
  DELETE it_bseg_ft WHERE matnr IS INITIAL.
  SORT: it_bseg_ft BY bukrs belnr gjahr buzei hkont.
  SORT: it_bsis    BY bukrs belnr gjahr buzei hkont prctr.
  LOOP AT it_faglflexa_totcl.

    vg_tabix = sy-tabix.

    it_faglflexa_totcl-hsl = 0.
    it_faglflexa_totcl-ksl = 0.
    it_faglflexa_totcl-osl = 0.
    CLEAR: it_faglflexa_totcl-rassc.

    LOOP AT it_faglflexa WHERE racct EQ it_faglflexa_totcl-saknr
                           AND prctr EQ it_faglflexa_totcl-prctr.

      READ TABLE it_bsis WITH KEY  bukrs = it_faglflexa-rbukrs
                                   belnr = it_faglflexa-belnr
                                   gjahr = it_faglflexa-ryear
                                   buzei = it_faglflexa-buzei
                                   hkont = it_faglflexa-racct
                                   prctr = it_faglflexa-prctr BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_bseg_ft WITH KEY  bukrs = it_faglflexa-rbukrs
                                   belnr = it_faglflexa-belnr
                                   gjahr = it_faglflexa-ryear
                                   buzei = it_faglflexa-buzei
                                   hkont = it_faglflexa-racct
                                   BINARY SEARCH.
*                                   prctr = it_faglflexa-prctr.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      IF it_faglflexa-rbukrs = '0101'. " Paraguai moeda interna
        it_faglflexa_totcl-hsl = it_faglflexa_totcl-hsl + it_faglflexa-hsl * 100.
      ELSE.
        it_faglflexa_totcl-hsl = it_faglflexa_totcl-hsl + it_faglflexa-hsl.
      ENDIF.

      it_faglflexa_totcl-ksl = it_faglflexa_totcl-ksl + it_faglflexa-ksl.
      it_faglflexa_totcl-osl = it_faglflexa_totcl-osl + it_faglflexa-osl.
      it_faglflexa_totcl-rassc = it_bsis-vbund.
    ENDLOOP.

    MODIFY it_faglflexa_totcl INDEX vg_tabix.
  ENDLOOP.

  LOOP AT it_faglflexa.
    LOOP AT it_bsis WHERE bukrs = it_faglflexa-rbukrs
                      AND belnr = it_faglflexa-belnr
                      AND gjahr = it_faglflexa-ryear
                      AND buzei = it_faglflexa-buzei
                      AND hkont = it_faglflexa-racct
                      AND prctr = it_faglflexa-prctr.

      READ TABLE it_bseg_ft WITH KEY  bukrs = it_faglflexa-rbukrs
                                      belnr = it_faglflexa-belnr
                                      gjahr = it_faglflexa-ryear
                                      buzei = it_faglflexa-buzei
                                      hkont = it_faglflexa-racct
                                      BINARY SEARCH.
*                                   prctr = it_faglflexa-prctr.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      IF it_faglflexa-rbukrs = '0101'. " Paraguai moeda interna
        it_saknr-hsl   = it_faglflexa-hsl * 100.
      ELSE.
        it_saknr-hsl   = it_faglflexa-hsl.
      ENDIF.

      it_saknr-ksl   = it_faglflexa-ksl.
      it_saknr-osl   = it_faglflexa-osl.
      it_saknr-vbund = it_bsis-vbund.
      it_saknr-prctr = it_bsis-prctr.
      it_saknr-saknr = it_bsis-hkont.

      COLLECT it_saknr.
    ENDLOOP.
    CLEAR: it_saknr.
  ENDLOOP.

  LOOP AT it_saknr.

    READ TABLE it_faglflexa_totcl WITH KEY saknr = it_saknr-saknr
                                           prctr = it_saknr-prctr.
    IF sy-subrc IS INITIAL.
      CLEAR: it_zgl029_dre_dados.
      it_zgl029_dre_dados-bukrs          = wa_dre-bukrs.
      it_zgl029_dre_dados-versn          = wa_dre-versn.
      it_zgl029_dre_dados-monat          = wa_dre-monat.
      it_zgl029_dre_dados-gjahr          = wa_dre-gjahr.
      it_zgl029_dre_dados-vbund          = it_saknr-vbund.
      it_zgl029_dre_dados-saknr          = it_saknr-saknr.
      it_zgl029_dre_dados-prctr          = it_saknr-prctr.
      it_zgl029_dre_dados-vlr_rea        = it_saknr-hsl.
      it_zgl029_dre_dados-vlr_dolar      = it_saknr-ksl.
      it_zgl029_dre_dados-vlr_grupo      = it_saknr-osl.

      APPEND it_zgl029_dre_dados.
    ENDIF.
    CLEAR: it_zgl029_dre_dados.
  ENDLOOP.

ENDFORM.                    " TOTALIZAR_FLEXA_CC

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DRE  text
*----------------------------------------------------------------------*
FORM limpar_dre  USING wa_dre TYPE zgl020_dre_dados.

  DATA: wa_dre_aux TYPE zgl020_dre_dados.
  MOVE-CORRESPONDING wa_dre TO wa_dre_aux.

  CLEAR: it_zgl021_dre_dados[], it_zgl022_dre_dados[], it_zgl023_dre_dados[], it_zgl024_dre_dados[].

  DELETE FROM zgl021_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

  DELETE FROM zgl022_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

  DELETE FROM zgl023_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

  DELETE FROM zgl024_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

ENDFORM.                    " LIMPAR_DRE

*&---------------------------------------------------------------------*
*&      Form  TOTALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FAGLFLEXT  text
*      -->P_IT_FAGLFLEXT_TOTAL  text
*----------------------------------------------------------------------*
FORM totaliza  USING  it_faglflext        TYPE faglflext
                      it_faglflext_total  TYPE faglflext.

  it_faglflext_total-ksl01 = it_faglflext_total-ksl01 + it_faglflext-ksl01.
  it_faglflext_total-ksl02 = it_faglflext_total-ksl02 + it_faglflext-ksl02.
  it_faglflext_total-ksl03 = it_faglflext_total-ksl03 + it_faglflext-ksl03.
  it_faglflext_total-ksl04 = it_faglflext_total-ksl04 + it_faglflext-ksl04.
  it_faglflext_total-ksl05 = it_faglflext_total-ksl05 + it_faglflext-ksl05.
  it_faglflext_total-ksl06 = it_faglflext_total-ksl06 + it_faglflext-ksl06.
  it_faglflext_total-ksl07 = it_faglflext_total-ksl07 + it_faglflext-ksl07.
  it_faglflext_total-ksl08 = it_faglflext_total-ksl08 + it_faglflext-ksl08.
  it_faglflext_total-ksl09 = it_faglflext_total-ksl09 + it_faglflext-ksl09.
  it_faglflext_total-ksl10 = it_faglflext_total-ksl10 + it_faglflext-ksl10.
  it_faglflext_total-ksl11 = it_faglflext_total-ksl11 + it_faglflext-ksl11.

  IF it_faglflext-rbukrs = '0101'. " Paraguai moeda interna
    it_faglflext_total-hsl01 = it_faglflext_total-hsl01 + ( it_faglflext-hsl01 * 100 ).
    it_faglflext_total-hsl02 = it_faglflext_total-hsl02 + ( it_faglflext-hsl02 * 100 ).
    it_faglflext_total-hsl03 = it_faglflext_total-hsl03 + ( it_faglflext-hsl03 * 100 ).
    it_faglflext_total-hsl04 = it_faglflext_total-hsl04 + ( it_faglflext-hsl04 * 100 ).
    it_faglflext_total-hsl05 = it_faglflext_total-hsl05 + ( it_faglflext-hsl05 * 100 ).
    it_faglflext_total-hsl06 = it_faglflext_total-hsl06 + ( it_faglflext-hsl06 * 100 ).
    it_faglflext_total-hsl07 = it_faglflext_total-hsl07 + ( it_faglflext-hsl07 * 100 ).
    it_faglflext_total-hsl08 = it_faglflext_total-hsl08 + ( it_faglflext-hsl08 * 100 ).
    it_faglflext_total-hsl09 = it_faglflext_total-hsl09 + ( it_faglflext-hsl09 * 100 ).
    it_faglflext_total-hsl10 = it_faglflext_total-hsl10 + ( it_faglflext-hsl10 * 100 ).
    it_faglflext_total-hsl11 = it_faglflext_total-hsl11 + ( it_faglflext-hsl11 * 100 ).
  ELSE.
    it_faglflext_total-hsl01 = it_faglflext_total-hsl01 + it_faglflext-hsl01.
    it_faglflext_total-hsl02 = it_faglflext_total-hsl02 + it_faglflext-hsl02.
    it_faglflext_total-hsl03 = it_faglflext_total-hsl03 + it_faglflext-hsl03.
    it_faglflext_total-hsl04 = it_faglflext_total-hsl04 + it_faglflext-hsl04.
    it_faglflext_total-hsl05 = it_faglflext_total-hsl05 + it_faglflext-hsl05.
    it_faglflext_total-hsl06 = it_faglflext_total-hsl06 + it_faglflext-hsl06.
    it_faglflext_total-hsl07 = it_faglflext_total-hsl07 + it_faglflext-hsl07.
    it_faglflext_total-hsl08 = it_faglflext_total-hsl08 + it_faglflext-hsl08.
    it_faglflext_total-hsl09 = it_faglflext_total-hsl09 + it_faglflext-hsl09.
    it_faglflext_total-hsl10 = it_faglflext_total-hsl10 + it_faglflext-hsl10.
    it_faglflext_total-hsl11 = it_faglflext_total-hsl11 + it_faglflext-hsl11.
  ENDIF.

  it_faglflext_total-osl01 = it_faglflext_total-osl01 + it_faglflext-osl01.
  it_faglflext_total-osl02 = it_faglflext_total-osl02 + it_faglflext-osl02.
  it_faglflext_total-osl03 = it_faglflext_total-osl03 + it_faglflext-osl03.
  it_faglflext_total-osl04 = it_faglflext_total-osl04 + it_faglflext-osl04.
  it_faglflext_total-osl05 = it_faglflext_total-osl05 + it_faglflext-osl05.
  it_faglflext_total-osl06 = it_faglflext_total-osl06 + it_faglflext-osl06.
  it_faglflext_total-osl07 = it_faglflext_total-osl07 + it_faglflext-osl07.
  it_faglflext_total-osl08 = it_faglflext_total-osl08 + it_faglflext-osl08.
  it_faglflext_total-osl09 = it_faglflext_total-osl09 + it_faglflext-osl09.
  it_faglflext_total-osl10 = it_faglflext_total-osl10 + it_faglflext-osl10.
  it_faglflext_total-osl11 = it_faglflext_total-osl11 + it_faglflext-osl11.

  it_faglflext_total-ksl12 = it_faglflext_total-ksl12 +
                             it_faglflext-ksl12 + it_faglflext-ksl13 +
                             it_faglflext-ksl14 + it_faglflext-ksl15.

  IF it_faglflext-rbukrs = '0101'. " Paraguai moeda interna
    it_faglflext_total-hsl12 = it_faglflext_total-hsl12 +
                             ( it_faglflext-hsl12 * 100 ) + ( it_faglflext-hsl13 * 100 ) +
                             ( it_faglflext-hsl14 * 100 ) + ( it_faglflext-hsl15 * 100 ).
  ELSE.
    it_faglflext_total-hsl12 = it_faglflext_total-hsl12 +
                               it_faglflext-hsl12 + it_faglflext-hsl13 +
                               it_faglflext-hsl14 + it_faglflext-hsl15.
  ENDIF.

  it_faglflext_total-osl12 = it_faglflext_total-osl12 +
                             it_faglflext-osl12 + it_faglflext-osl13 +
                             it_faglflext-osl14 + it_faglflext-osl15.

ENDFORM.                    " TOTALIZA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_FLEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FAGLFLEXT_TOTAL  text
*      -->P_WA_DRE  text
*----------------------------------------------------------------------*
FORM busca_valor_flext  USING  it_faglflext        TYPE faglflext
                               p_dre               TYPE zgl020_dre_dados
                        CHANGING p_vlr_rea         TYPE vlcur12
                                 p_vlr_dolar       TYPE vlcur12
                                 p_vlr_dolar_conv	 TYPE vlcur12
                                 p_vlr_grupo       TYPE vlcur12.

  CASE p_dre-monat.
    WHEN 01.
      p_vlr_rea   = it_faglflext-hsl01.
      p_vlr_dolar = it_faglflext-ksl01.
      p_vlr_grupo = it_faglflext-osl01.
    WHEN 02.
      p_vlr_rea   = it_faglflext-hsl02.
      p_vlr_dolar = it_faglflext-ksl02.
      p_vlr_grupo = it_faglflext-osl02.
    WHEN 03.
      p_vlr_rea   = it_faglflext-hsl03.
      p_vlr_dolar = it_faglflext-ksl03.
      p_vlr_grupo = it_faglflext-osl03.
    WHEN 04.
      p_vlr_rea   = it_faglflext-hsl04.
      p_vlr_dolar = it_faglflext-ksl04.
      p_vlr_grupo = it_faglflext-osl04.
    WHEN 05.
      p_vlr_rea   = it_faglflext-hsl05.
      p_vlr_dolar = it_faglflext-ksl05.
      p_vlr_grupo = it_faglflext-osl05.
    WHEN 06.
      p_vlr_rea   = it_faglflext-hsl06.
      p_vlr_dolar = it_faglflext-ksl06.
      p_vlr_grupo = it_faglflext-osl06.
    WHEN 07.
      p_vlr_rea   = it_faglflext-hsl07.
      p_vlr_dolar = it_faglflext-ksl07.
      p_vlr_grupo = it_faglflext-osl07.
    WHEN 08.
      p_vlr_rea   = it_faglflext-hsl08.
      p_vlr_dolar = it_faglflext-ksl08.
      p_vlr_grupo = it_faglflext-osl08.
    WHEN 09.
      p_vlr_rea   = it_faglflext-hsl09.
      p_vlr_dolar = it_faglflext-ksl09.
      p_vlr_grupo = it_faglflext-osl09.
    WHEN 10.
      p_vlr_rea   = it_faglflext-hsl10.
      p_vlr_dolar = it_faglflext-ksl10.
      p_vlr_grupo = it_faglflext-osl10.
    WHEN 11.
      p_vlr_rea   = it_faglflext-hsl11.
      p_vlr_dolar = it_faglflext-ksl11.
      p_vlr_grupo = it_faglflext-osl11.
    WHEN 12.
      p_vlr_rea   = it_faglflext-hsl12.
      p_vlr_dolar = it_faglflext-ksl12.
      p_vlr_grupo = it_faglflext-osl12.
  ENDCASE.

  p_vlr_dolar_conv = p_vlr_dolar * p_dre-ukurs.

ENDFORM.                    " BUSCA_VALOR_FLEXT

*&---------------------------------------------------------------------*
*&      Form  TOTALIZA_MES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DRE  text
*----------------------------------------------------------------------*
FORM totaliza_mes  TABLES it_zgl015_dre_est03 STRUCTURE zgl015_dre_est03
                   USING  wa_dre TYPE zgl020_dre_dados.

** Tabela que ira conter a totalizacao dividida pela Sociedade
*  TYPES: BEGIN OF TY_SAKNR,
*          SAKNR TYPE BSEG-SAKNR,
*          KOSTL TYPE BSIS-KOSTL,
*          PRCTR TYPE BSIS-PRCTR,
*          MATKL TYPE MARA-MATKL,
*          VBUND TYPE BSEG-VBUND,
*          MSL   TYPE FAGLFLEXA-MSL,
*          HSL   TYPE FAGLFLEXA-HSL,
*          KSL   TYPE FAGLFLEXA-KSL,
*          oSL   TYPE FAGLFLEXA-oSL,
*         END OF TY_SAKNR.


  DATA: it_zgl025_dre_acm TYPE TABLE OF zgl025_dre_acm WITH HEADER LINE,
        it_zgl026_dre_acm TYPE TABLE OF zgl026_dre_acm WITH HEADER LINE,
        it_zgl027_dre_acm TYPE TABLE OF zgl027_dre_acm WITH HEADER LINE,
        it_zgl028_dre_acm TYPE TABLE OF zgl028_dre_acm WITH HEADER LINE,
        it_zgl030_dre_acm TYPE TABLE OF zgl030_dre_acm WITH HEADER LINE,
        p_monat           TYPE monat,
        vg_tabix          TYPE sy-tabix.
*        IT_SAKNR          TYPE TABLE OF TY_SAKNR WITH HEADER LINE.

*  REFRESH: IT_SAKNR.
*  CLEAR: IT_SAKNR.

  SELECT * INTO TABLE it_zgl021_dre_dados
    FROM zgl021_dre_dados
    WHERE bukrs EQ wa_dre-bukrs
      AND versn EQ wa_dre-versn
      AND monat EQ wa_dre-monat
      AND gjahr EQ wa_dre-gjahr.

  SELECT * INTO TABLE it_zgl022_dre_dados
    FROM zgl022_dre_dados
    WHERE bukrs EQ wa_dre-bukrs
      AND versn EQ wa_dre-versn
      AND monat EQ wa_dre-monat
      AND gjahr EQ wa_dre-gjahr.

  SELECT * INTO TABLE it_zgl023_dre_dados
    FROM zgl023_dre_dados
    WHERE bukrs EQ wa_dre-bukrs
      AND versn EQ wa_dre-versn
      AND monat EQ wa_dre-monat
      AND gjahr EQ wa_dre-gjahr.

  SELECT * INTO TABLE it_zgl024_dre_dados
    FROM zgl024_dre_dados
    WHERE bukrs EQ wa_dre-bukrs
      AND versn EQ wa_dre-versn
      AND monat EQ wa_dre-monat
      AND gjahr EQ wa_dre-gjahr.

  SELECT * INTO TABLE it_zgl029_dre_dados
    FROM zgl029_dre_dados
    WHERE bukrs EQ wa_dre-bukrs
      AND versn EQ wa_dre-versn
      AND monat EQ wa_dre-monat
      AND gjahr EQ wa_dre-gjahr.

  IF wa_dre-monat GT 1.
    p_monat = wa_dre-monat - 1.

    IF it_zgl015_dre_est03[] IS NOT INITIAL.
      SELECT * INTO TABLE it_zgl025_dre_acm
        FROM zgl025_dre_acm
        FOR ALL ENTRIES  IN it_zgl015_dre_est03
       WHERE bukrs EQ wa_dre-bukrs
         AND versn EQ wa_dre-versn
         AND monat EQ p_monat
         AND gjahr EQ wa_dre-gjahr
         AND nivel EQ it_zgl015_dre_est03-nivel
         AND saknr EQ it_zgl015_dre_est03-saknr.

    ENDIF.
    SELECT * INTO TABLE it_zgl026_dre_acm
      FROM zgl026_dre_acm
     WHERE bukrs EQ wa_dre-bukrs
       AND versn EQ wa_dre-versn
       AND monat EQ p_monat
       AND gjahr EQ wa_dre-gjahr.

    SELECT * INTO TABLE it_zgl027_dre_acm
      FROM zgl027_dre_acm
     WHERE bukrs EQ wa_dre-bukrs
       AND versn EQ wa_dre-versn
       AND monat EQ p_monat
       AND gjahr EQ wa_dre-gjahr.

    SELECT * INTO TABLE it_zgl028_dre_acm
      FROM zgl028_dre_acm
     WHERE bukrs EQ wa_dre-bukrs
       AND versn EQ wa_dre-versn
       AND monat EQ p_monat
       AND gjahr EQ wa_dre-gjahr.

    SELECT * INTO TABLE it_zgl030_dre_acm
      FROM zgl030_dre_acm
     WHERE bukrs EQ wa_dre-bukrs
       AND versn EQ wa_dre-versn
       AND monat EQ p_monat
       AND gjahr EQ wa_dre-gjahr.

    SORT it_zgl025_dre_acm BY nivel saknr.
    LOOP AT it_zgl021_dre_dados.
      vg_tabix = sy-tabix.
      READ TABLE it_zgl025_dre_acm WITH KEY nivel = it_zgl021_dre_dados-nivel
                                            saknr = it_zgl021_dre_dados-saknr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      LOOP AT it_zgl025_dre_acm WHERE nivel EQ it_zgl021_dre_dados-nivel
                                  AND saknr EQ it_zgl021_dre_dados-saknr.
        it_zgl021_dre_dados-vlr_rea        = it_zgl021_dre_dados-vlr_rea        + it_zgl025_dre_acm-vlr_rea       .
        it_zgl021_dre_dados-vlr_dolar      = it_zgl021_dre_dados-vlr_dolar      + it_zgl025_dre_acm-vlr_dolar     .
        it_zgl021_dre_dados-vlr_grupo      = it_zgl021_dre_dados-vlr_grupo      + it_zgl025_dre_acm-vlr_grupo     .
        it_zgl021_dre_dados-vlr_dolar_conv = it_zgl021_dre_dados-vlr_dolar_conv + it_zgl025_dre_acm-vlr_dolar_conv.
      ENDLOOP.
      MODIFY it_zgl021_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_dolar_conv vlr_grupo.
    ENDLOOP.

    SORT it_zgl021_dre_dados BY nivel  saknr.
    LOOP AT it_zgl025_dre_acm.
      READ TABLE it_zgl021_dre_dados WITH KEY nivel = it_zgl025_dre_acm-nivel
                                              saknr = it_zgl025_dre_acm-saknr.
      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl025_dre_acm TO it_zgl021_dre_dados.
        it_zgl021_dre_dados-monat = wa_dre-monat.
        APPEND it_zgl021_dre_dados.
      ENDIF.
    ENDLOOP.

    SORT it_zgl026_dre_acm BY       nivel saknr kostl.
    LOOP AT it_zgl022_dre_dados.
      vg_tabix = sy-tabix.
      READ TABLE it_zgl026_dre_acm WITH KEY nivel = it_zgl022_dre_dados-nivel
                                            saknr = it_zgl022_dre_dados-saknr
                                            kostl = it_zgl022_dre_dados-kostl BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      LOOP AT it_zgl026_dre_acm WHERE nivel EQ it_zgl022_dre_dados-nivel
                                  AND saknr EQ it_zgl022_dre_dados-saknr
                                  AND kostl EQ it_zgl022_dre_dados-kostl.
        it_zgl022_dre_dados-vlr_rea        = it_zgl022_dre_dados-vlr_rea        + it_zgl026_dre_acm-vlr_rea       .
        it_zgl022_dre_dados-vlr_dolar      = it_zgl022_dre_dados-vlr_dolar      + it_zgl026_dre_acm-vlr_dolar     .
        it_zgl022_dre_dados-vlr_grupo      = it_zgl022_dre_dados-vlr_grupo      + it_zgl026_dre_acm-vlr_grupo     .
        it_zgl022_dre_dados-vlr_dolar_conv = it_zgl022_dre_dados-vlr_dolar_conv + it_zgl026_dre_acm-vlr_dolar_conv.
      ENDLOOP.
      MODIFY it_zgl022_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_dolar_conv vlr_grupo.
    ENDLOOP.

    SORT it_zgl022_dre_dados BY nivel saknr kostl.
    LOOP AT it_zgl026_dre_acm.
      READ TABLE it_zgl022_dre_dados WITH KEY nivel = it_zgl026_dre_acm-nivel
                                              saknr = it_zgl026_dre_acm-saknr
                                              kostl = it_zgl026_dre_acm-kostl.
      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl026_dre_acm TO it_zgl022_dre_dados.
        it_zgl022_dre_dados-monat = wa_dre-monat.
        APPEND it_zgl022_dre_dados.
      ENDIF.
    ENDLOOP.

    SORT it_zgl027_dre_acm  BY nivel saknr prctr.
    LOOP AT it_zgl023_dre_dados.
      vg_tabix = sy-tabix.
      READ TABLE it_zgl027_dre_acm WITH KEY  nivel = it_zgl023_dre_dados-nivel
                                             saknr = it_zgl023_dre_dados-saknr
                                             prctr = it_zgl023_dre_dados-prctr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      LOOP AT it_zgl027_dre_acm WHERE nivel EQ it_zgl023_dre_dados-nivel
                                  AND saknr EQ it_zgl023_dre_dados-saknr
                                  AND prctr EQ it_zgl023_dre_dados-prctr.
        it_zgl023_dre_dados-vlr_rea        = it_zgl023_dre_dados-vlr_rea        + it_zgl027_dre_acm-vlr_rea       .
        it_zgl023_dre_dados-vlr_dolar      = it_zgl023_dre_dados-vlr_dolar      + it_zgl027_dre_acm-vlr_dolar     .
        it_zgl023_dre_dados-vlr_grupo      = it_zgl023_dre_dados-vlr_grupo      + it_zgl027_dre_acm-vlr_grupo     .
        it_zgl023_dre_dados-vlr_dolar_conv = it_zgl023_dre_dados-vlr_dolar_conv + it_zgl027_dre_acm-vlr_dolar_conv.
      ENDLOOP.
      MODIFY it_zgl023_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_dolar_conv vlr_grupo.
    ENDLOOP.

    SORT it_zgl023_dre_dados BY nivel saknr prctr .
    LOOP AT it_zgl027_dre_acm.
      READ TABLE it_zgl023_dre_dados WITH KEY nivel = it_zgl027_dre_acm-nivel
                                              saknr = it_zgl027_dre_acm-saknr
                                              prctr = it_zgl027_dre_acm-prctr.
      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl027_dre_acm TO it_zgl023_dre_dados.
        it_zgl023_dre_dados-monat = wa_dre-monat.
        APPEND it_zgl023_dre_dados.
      ENDIF.
    ENDLOOP.

    SORT it_zgl028_dre_acm BY nivel saknr matkl.
    LOOP AT it_zgl024_dre_dados.
      vg_tabix = sy-tabix.
      READ TABLE it_zgl028_dre_acm WITH KEY nivel = it_zgl024_dre_dados-nivel
                                            saknr = it_zgl024_dre_dados-saknr
                                            matkl = it_zgl024_dre_dados-matkl BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      LOOP AT it_zgl028_dre_acm WHERE nivel EQ it_zgl024_dre_dados-nivel
                                  AND saknr EQ it_zgl024_dre_dados-saknr
                                  AND matkl EQ it_zgl024_dre_dados-matkl.
        it_zgl024_dre_dados-vlr_rea        = it_zgl024_dre_dados-vlr_rea        + it_zgl028_dre_acm-vlr_rea       .
        it_zgl024_dre_dados-vlr_dolar      = it_zgl024_dre_dados-vlr_dolar      + it_zgl028_dre_acm-vlr_dolar     .
        it_zgl024_dre_dados-vlr_grupo      = it_zgl024_dre_dados-vlr_grupo      + it_zgl028_dre_acm-vlr_grupo     .
        it_zgl024_dre_dados-vlr_dolar_conv = it_zgl024_dre_dados-vlr_dolar_conv + it_zgl028_dre_acm-vlr_dolar_conv.
      ENDLOOP.
      MODIFY it_zgl024_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_dolar_conv vlr_grupo.
    ENDLOOP.

    SORT it_zgl024_dre_dados BY nivel saknr matkl.
    LOOP AT it_zgl028_dre_acm.
      READ TABLE it_zgl024_dre_dados WITH KEY nivel = it_zgl028_dre_acm-nivel
                                              saknr = it_zgl028_dre_acm-saknr
                                              matkl = it_zgl028_dre_acm-matkl.
      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl028_dre_acm TO it_zgl024_dre_dados.
        it_zgl024_dre_dados-monat = wa_dre-monat.
        APPEND it_zgl024_dre_dados.
      ENDIF.
    ENDLOOP.

    SORT  it_zgl030_dre_acm BY saknr kostl prctr matkl vbund.
    LOOP AT it_zgl029_dre_dados.
      vg_tabix = sy-tabix.
      IF it_zgl029_dre_dados-saknr IS NOT INITIAL
      AND it_zgl029_dre_dados-kostl IS  INITIAL
      AND it_zgl029_dre_dados-prctr IS  INITIAL
      AND it_zgl029_dre_dados-matkl IS  INITIAL.
*** Conta razão
        LOOP AT it_zgl030_dre_acm
          WHERE saknr EQ it_zgl029_dre_dados-saknr
            AND kostl IS INITIAL
            AND prctr IS INITIAL
            AND matkl IS INITIAL
            AND vbund EQ it_zgl029_dre_dados-vbund.

          it_zgl029_dre_dados-vlr_rea        = it_zgl029_dre_dados-vlr_rea        + it_zgl030_dre_acm-vlr_rea       .
          it_zgl029_dre_dados-vlr_dolar      = it_zgl029_dre_dados-vlr_dolar      + it_zgl030_dre_acm-vlr_dolar     .
          it_zgl029_dre_dados-vlr_grupo      = it_zgl029_dre_dados-vlr_grupo      + it_zgl030_dre_acm-vlr_grupo     .
*      IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV = IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV + IT_ZGL030_DRE_ACM-VLR_DOLAR_CONV.
        ENDLOOP.
        MODIFY it_zgl029_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_grupo.
      ENDIF.
      IF it_zgl029_dre_dados-saknr IS NOT INITIAL
     AND it_zgl029_dre_dados-kostl IS NOT INITIAL
     AND it_zgl029_dre_dados-prctr IS  INITIAL
     AND it_zgl029_dre_dados-matkl IS  INITIAL.
*** Centro de Custo
        LOOP AT it_zgl030_dre_acm
          WHERE saknr EQ it_zgl029_dre_dados-saknr
            AND kostl EQ it_zgl029_dre_dados-kostl
            AND prctr IS INITIAL
            AND matkl IS INITIAL
            AND vbund EQ it_zgl029_dre_dados-vbund.

          it_zgl029_dre_dados-vlr_rea        = it_zgl029_dre_dados-vlr_rea        + it_zgl030_dre_acm-vlr_rea       .
          it_zgl029_dre_dados-vlr_dolar      = it_zgl029_dre_dados-vlr_dolar      + it_zgl030_dre_acm-vlr_dolar     .
          it_zgl029_dre_dados-vlr_grupo      = it_zgl029_dre_dados-vlr_grupo      + it_zgl030_dre_acm-vlr_grupo     .
*      IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV = IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV + IT_ZGL030_DRE_ACM-VLR_DOLAR_CONV.
        ENDLOOP.
        MODIFY it_zgl029_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_grupo.
      ENDIF.
      IF it_zgl029_dre_dados-saknr IS NOT INITIAL
      AND it_zgl029_dre_dados-kostl IS  INITIAL
      AND it_zgl029_dre_dados-prctr IS NOT INITIAL
      AND it_zgl029_dre_dados-matkl IS  INITIAL.
*** Centro de lucro
        LOOP AT it_zgl030_dre_acm
          WHERE saknr EQ it_zgl029_dre_dados-saknr
            AND kostl IS INITIAL
            AND prctr EQ it_zgl029_dre_dados-prctr
            AND matkl IS INITIAL
            AND vbund EQ it_zgl029_dre_dados-vbund.

          it_zgl029_dre_dados-vlr_rea        = it_zgl029_dre_dados-vlr_rea        + it_zgl030_dre_acm-vlr_rea       .
          it_zgl029_dre_dados-vlr_dolar      = it_zgl029_dre_dados-vlr_dolar      + it_zgl030_dre_acm-vlr_dolar     .
          it_zgl029_dre_dados-vlr_grupo      = it_zgl029_dre_dados-vlr_grupo      + it_zgl030_dre_acm-vlr_grupo     .
*      IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV = IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV + IT_ZGL030_DRE_ACM-VLR_DOLAR_CONV.
        ENDLOOP.
        MODIFY it_zgl029_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_grupo.
      ENDIF.
      IF it_zgl029_dre_dados-saknr IS NOT INITIAL
      AND it_zgl029_dre_dados-kostl IS  INITIAL
      AND it_zgl029_dre_dados-prctr IS  INITIAL
      AND it_zgl029_dre_dados-matkl IS NOT INITIAL.
*** Grupo de mercadoria
        LOOP AT it_zgl030_dre_acm
          WHERE saknr EQ it_zgl029_dre_dados-saknr
            AND kostl IS INITIAL
            AND prctr IS INITIAL
            AND matkl EQ it_zgl029_dre_dados-matkl
            AND vbund EQ it_zgl029_dre_dados-vbund.

          it_zgl029_dre_dados-vlr_rea        = it_zgl029_dre_dados-vlr_rea        + it_zgl030_dre_acm-vlr_rea       .
          it_zgl029_dre_dados-vlr_dolar      = it_zgl029_dre_dados-vlr_dolar      + it_zgl030_dre_acm-vlr_dolar     .
          it_zgl029_dre_dados-vlr_grupo      = it_zgl029_dre_dados-vlr_grupo      + it_zgl030_dre_acm-vlr_grupo     .
*      IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV = IT_ZGL029_DRE_DADOS-VLR_DOLAR_CONV + IT_ZGL030_DRE_ACM-VLR_DOLAR_CONV.
        ENDLOOP.
        MODIFY it_zgl029_dre_dados INDEX vg_tabix TRANSPORTING vlr_rea vlr_dolar vlr_grupo.

      ENDIF.
    ENDLOOP.

    SORT  it_zgl029_dre_dados BY saknr kostl prctr matkl vbund.
    LOOP AT it_zgl030_dre_acm.
      READ TABLE it_zgl029_dre_dados WITH KEY saknr = it_zgl030_dre_acm-saknr
                                              kostl = it_zgl030_dre_acm-kostl
                                              prctr = it_zgl030_dre_acm-prctr
                                              matkl = it_zgl030_dre_acm-matkl
                                              vbund = it_zgl030_dre_acm-vbund BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl030_dre_acm TO it_zgl029_dre_dados.
        it_zgl029_dre_dados-monat = wa_dre-monat .
        APPEND it_zgl029_dre_dados.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT it_zgl021_dre_dados[] IS INITIAL.
    DELETE FROM zgl025_dre_acm WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl025_dre_acm FROM TABLE it_zgl021_dre_dados.
  ENDIF.

  SORT: it_zgl022_dre_dados BY bukrs versn monat gjahr nivel saknr kostl.
  LOOP AT it_zgl022_dre_dados_aux WHERE vlr_rea   IS INITIAL
                                    AND vlr_dolar IS INITIAL
                                    AND vlr_grupo IS INITIAL.
    READ TABLE it_zgl022_dre_dados
      WITH KEY bukrs = it_zgl022_dre_dados_aux-bukrs
               versn = it_zgl022_dre_dados_aux-versn
               monat = it_zgl022_dre_dados_aux-monat
               gjahr = it_zgl022_dre_dados_aux-gjahr
               nivel = it_zgl022_dre_dados_aux-nivel
               saknr = it_zgl022_dre_dados_aux-saknr
               kostl = it_zgl022_dre_dados_aux-kostl
                BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL
    OR ( it_zgl022_dre_dados-vlr_rea IS INITIAL
        AND it_zgl022_dre_dados-vlr_dolar IS INITIAL
        AND it_zgl022_dre_dados-vlr_grupo IS INITIAL ).
      DELETE it_zgl022_dre_dados_aux.
    ENDIF.
  ENDLOOP.

  IF it_zgl022_dre_dados_aux[] IS NOT INITIAL.
    DELETE FROM zgl022_dre_dados WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl022_dre_dados FROM TABLE it_zgl022_dre_dados_aux.

  ENDIF.

  IF NOT it_zgl022_dre_dados[] IS INITIAL.
    DELETE FROM zgl026_dre_acm WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl026_dre_acm FROM TABLE it_zgl022_dre_dados.
  ENDIF.

  IF NOT it_zgl023_dre_dados[] IS INITIAL.
    DELETE FROM zgl027_dre_acm WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl027_dre_acm FROM TABLE it_zgl023_dre_dados.
  ENDIF.

  IF NOT it_zgl024_dre_dados[] IS INITIAL.
    DELETE FROM zgl028_dre_acm WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl028_dre_acm FROM TABLE it_zgl024_dre_dados.
  ENDIF.

  IF NOT it_zgl029_dre_dados[] IS INITIAL.
    DELETE FROM zgl030_dre_acm WHERE bukrs EQ wa_dre-bukrs
                                 AND versn EQ wa_dre-versn
                                 AND monat EQ wa_dre-monat
                                 AND gjahr EQ wa_dre-gjahr.

    MODIFY zgl030_dre_acm FROM TABLE it_zgl029_dre_dados.
  ENDIF.

  CLEAR: it_zgl021_dre_dados[], it_zgl022_dre_dados[], it_zgl023_dre_dados[], it_zgl024_dre_dados[], it_zgl029_dre_dados[].

ENDFORM.                    " TOTALIZA_MES

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2463   text
*----------------------------------------------------------------------*
FORM mostra_texto_p  USING p_texto
                           p_total   TYPE i
                           p_posicao TYPE i.

  DATA: vmsg(100),
        p_percentage(20),
        p_perce TYPE i.

  MOVE p_texto TO vmsg.

  IF p_total NE 0.
    p_perce = ( p_posicao * 100 ) / p_total.
  ENDIF.

  WRITE p_perce TO p_percentage.

  CALL FUNCTION 'TH_REDISPATCH'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = vmsg.

ENDFORM.                    " MOSTRA_TEXTO
