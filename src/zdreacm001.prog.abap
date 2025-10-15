*&---------------------------------------------------------------------*
*& Report  ZDREACM001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdreacm001.
TABLES: zgl030_dre_acm.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR zgl030_dre_acm-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_versn FOR zgl030_dre_acm-versn NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_monat FOR zgl030_dre_acm-monat NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_gjahr FOR zgl030_dre_acm-gjahr NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM busca_dados.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA:  it_zgl029_dre_dados TYPE TABLE OF zgl029_dre_dados WITH HEADER LINE,
         it_zgl030_dre_acm TYPE TABLE OF zgl030_dre_acm WITH HEADER LINE,
         p_monat           TYPE monat,
         vg_tabix          TYPE sy-tabix.

  SELECT * INTO TABLE it_zgl029_dre_dados
      FROM zgl029_dre_dados
      WHERE bukrs EQ s_bukrs-low
        AND versn EQ s_versn-low
        AND monat EQ s_monat-low
        AND gjahr EQ s_gjahr-low.

  IF s_monat-low GT 1.

    p_monat = s_monat-low - 1.

    SELECT * INTO TABLE it_zgl030_dre_acm
      FROM zgl030_dre_acm
     WHERE bukrs EQ s_bukrs-low
       AND versn EQ s_versn-low
       AND monat EQ p_monat
       AND gjahr EQ s_gjahr-low.

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

    LOOP AT it_zgl030_dre_acm.
      READ TABLE it_zgl029_dre_dados WITH KEY saknr = it_zgl030_dre_acm-saknr
                                              kostl = it_zgl030_dre_acm-kostl
                                              prctr = it_zgl030_dre_acm-prctr
                                              matkl = it_zgl030_dre_acm-matkl
                                              vbund = it_zgl030_dre_acm-vbund.

      IF NOT sy-subrc IS INITIAL.
        MOVE-CORRESPONDING it_zgl030_dre_acm TO it_zgl029_dre_dados.
        it_zgl029_dre_dados-monat = s_monat-low .
        APPEND it_zgl029_dre_dados.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF NOT it_zgl029_dre_dados[] IS INITIAL.
    DELETE FROM zgl030_dre_acm WHERE bukrs EQ s_bukrs-low
                                 AND versn EQ s_versn-low
                                 AND monat EQ s_monat-low
                                 AND gjahr EQ s_gjahr-low.

    MODIFY zgl030_dre_acm FROM TABLE it_zgl029_dre_dados.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
