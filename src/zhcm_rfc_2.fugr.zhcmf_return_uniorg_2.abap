FUNCTION zhcmf_return_uniorg_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(PERNR) LIKE  PA0001-PERNR OPTIONAL
*"     VALUE(TIPO) TYPE  CHAR1 OPTIONAL
*"     VALUE(COD_UNIORG) TYPE  ORGEH OPTIONAL
*"     VALUE(NOME_UNIORG) TYPE  STEXT OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMS_RETURN_UNIORG_NEW
*"      T_KOSTL STRUCTURE  ZHCMS_KOSTL OPTIONAL
*"      T_AREA STRUCTURE  ZHCMES_AREA_RH OPTIONAL
*"----------------------------------------------------------------------

  DATA: r_pernr       TYPE RANGE OF pernr,
        r_uniorg      TYPE RANGE OF orgeh,
        r_nome_uniorg TYPE RANGE OF stext,
        r_cod_ccusto  TYPE RANGE OF kostl,
        r_area        TYPE RANGE OF persa.
  RANGES:   r_abkrs        FOR pa0001-abkrs.

  DATA: t_pa0001 TYPE TABLE OF pa0001.

  FREE: t_saida.

  IF pernr IS NOT INITIAL.
    FREE: r_pernr.
    r_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = pernr ) ).
  ENDIF.

  IF cod_uniorg IS NOT INITIAL.
    FREE: r_uniorg.
    r_uniorg = VALUE #( ( sign = 'I' option = 'EQ' low = cod_uniorg ) ).
  ENDIF.

  IF nome_uniorg IS NOT INITIAL.
    FREE: r_nome_uniorg.
    TRANSLATE nome_uniorg TO UPPER CASE.
    r_nome_uniorg = VALUE #( ( sign = 'I' option = 'CP'  low = |*{ nome_uniorg }*| ) ).

  ENDIF.

  IF t_kostl IS NOT INITIAL.
    FREE: r_cod_ccusto.
    r_cod_ccusto = VALUE #( FOR l IN t_kostl ( sign = 'I' option = 'EQ' low = l-kostl ) ).

    SORT r_cod_ccusto BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_cod_ccusto COMPARING low.
  ENDIF.

  IF t_area IS NOT INITIAL.
    FREE: r_area.
    r_area = VALUE #( FOR t IN t_area ( sign = 'I' option = 'EQ' low = t-area ) ).
  ENDIF.

  CASE tipo.
    WHEN '1'.

      "Seleciona.
      SELECT * FROM zhcmt_f_uniorg
        INTO CORRESPONDING FIELDS OF TABLE t_saida
        WHERE orgeh IN r_uniorg
          AND stext IN r_nome_uniorg
          AND cod_ccusto IN r_cod_ccusto
          AND cod_filial IN r_area.
    WHEN '2'.

      SELECT * FROM zv_zhcmt_pa_0031 INTO TABLE @DATA(t_pa_0031) WHERE matricula EQ @pernr AND cod_uniorg IN @r_uniorg.
      IF sy-subrc EQ 0.
        SELECT * FROM  zhcmt_f_uniorg
          INTO CORRESPONDING FIELDS OF TABLE t_saida
          FOR ALL ENTRIES IN t_pa_0031
          WHERE orgeh EQ t_pa_0031-cod_uniorg
            AND cod_ccusto IN r_cod_ccusto
            AND cod_filial IN r_area.

        IF r_nome_uniorg[] IS NOT INITIAL.
          DELETE t_saida WHERE stext NOT IN r_nome_uniorg.
        ENDIF.
      ENDIF.

      FREE: t_pa_0031.

    WHEN '3'.
      FREE: t_pa0001.
      SELECT SINGLE * FROM pa0001
        INTO @DATA(ws_pa0001)
         WHERE pernr EQ @pernr
          AND endda >= @sy-datum.

      IF sy-subrc EQ 0.

        REFRESH r_abkrs.

        IF ws_pa0001-abkrs = '10'.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = 11.
          APPEND r_abkrs.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ENDIF.

        " ELSE.
        IF ws_pa0001-abkrs = '11'.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = 10.
          APPEND r_abkrs.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ENDIF.


*** US 179213 - Inicio - CBRAND
        IF ws_pa0001-abkrs = '04'.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = '17'.
          APPEND r_abkrs.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ENDIF.
        IF ws_pa0001-abkrs = '17'.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = '04'.
          APPEND r_abkrs.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ENDIF.
*** US 179213 - Fim - CBRAND

        IF ws_pa0001-abkrs = '01'.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = 'A2'.
          APPEND r_abkrs.

          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ELSE.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = ws_pa0001-abkrs .
          APPEND r_abkrs.
        ENDIF.


* BUG - 100707 - Inicio CBRAND

        SELECT * FROM  zhcmt_f_uniorg
          INTO CORRESPONDING FIELDS OF TABLE t_saida
          WHERE orgeh      IN r_uniorg
            AND cod_ccusto IN r_cod_ccusto
            AND cod_filial IN r_area
            AND area_folha IN r_abkrs.


        IF r_nome_uniorg[] IS NOT INITIAL.
          DELETE t_saida WHERE stext NOT IN r_nome_uniorg.
        ENDIF.


*        SELECT * FROM pa0001
*        INTO TABLE t_pa0001
*        WHERE abkrs  IN r_abkrs
*          AND endda >= sy-datum
*          AND plans <> '99999999'.
*
*        DELETE ADJACENT DUPLICATES FROM t_pa0001 COMPARING orgeh.
*
*        IF t_pa0001 IS NOT INITIAL.
*
*          SELECT * FROM  zhcmt_f_uniorg
*            INTO CORRESPONDING FIELDS OF TABLE t_saida
*          FOR ALL ENTRIES IN t_pa0001
*            WHERE orgeh EQ t_pa0001-orgeh
*              AND cod_ccusto IN r_cod_ccusto
*              AND cod_filial IN r_area
*              AND area_folha EQ t_pa0001-abkrs.
*
*          IF r_uniorg[] IS NOT INITIAL.
*            DELETE t_saida WHERE orgeh NOT IN r_uniorg.
*          ENDIF.
*
*          IF r_nome_uniorg[] IS NOT INITIAL.
*            DELETE t_saida WHERE stext NOT IN r_nome_uniorg.
*          ENDIF.
*        ENDIF.
* BUG - 100707 - Inicio CBRAND

      ENDIF.

      CLEAR: ws_pa0001.
    WHEN OTHERS.
  ENDCASE.


ENDFUNCTION.
