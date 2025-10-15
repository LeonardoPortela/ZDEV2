FUNCTION z_zim01_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_MES) TYPE  MONAT
*"     REFERENCE(I_AREA) TYPE  KOKRS
*"  TABLES
*"      RESULTADO STRUCTURE  ZHCME_SYSPHERA_ZIM01
*"      T_CUSTO STRUCTURE  RSPARAMS
*"----------------------------------------------------------------------
  TYPES: ty_custo TYPE RANGE OF pa0001-kostl.

  DATA: BEGIN OF t_08 OCCURS 0.
          INCLUDE STRUCTURE zim08_rel_inv2.
        DATA: END OF t_08.

  DATA: BEGIN OF t_08_usd OCCURS 0.
          INCLUDE STRUCTURE zim08_rel_inv_us.
        DATA: END OF t_08_usd.

  DATA: BEGIN OF t_ap_inv OCCURS 0.
          INCLUDE STRUCTURE zim01_sol_ap_inv.
        DATA: END OF t_ap_inv.

  DATA: vdatai   TYPE sy-datum,
        vdataf   TYPE sy-datum,
        wa_zim01 TYPE zhcme_sysphera_zim01,
        v_kokrs  TYPE csks-kokrs.

  DATA(r_custo) = VALUE ty_custo( FOR w_custo IN t_custo[] (
                                      sign    = 'I'
                                      option  = 'EQ'
                                      low     = w_custo-low
                                      high    = w_custo-high )  ).

  vdatai = |{ i_ano }{ i_mes }01|.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vdatai
    IMPORTING
      last_day_of_month = vdataf.

  SELECT * FROM zim08_rel_inv2
         INTO TABLE t_08
         WHERE visao  EQ '01'     AND
               akostl IN r_custo  AND
               gjahr  EQ i_ano    AND
               aedat  BETWEEN vdatai AND vdataf.

  SELECT * FROM zim08_rel_inv_us
   INTO TABLE t_08_usd
    WHERE visao  EQ '01'       AND
            akostl IN r_custo  AND
            gjahr  EQ i_ano    AND
            aedat  BETWEEN vdatai AND vdataf.

  IF t_08[] IS NOT INITIAL.
    SELECT * FROM zim01_sol_ap_inv
      INTO TABLE t_ap_inv
      FOR ALL ENTRIES IN t_08
      WHERE posnr = t_08-posnr
        AND gsber = t_08-werks
       " AND ano   = i_ano
        AND solicitacao_invest NE '000000'.

    SORT t_ap_inv BY posnr gsber ASCENDING solicitacao_invest DESCENDING .
  ENDIF.

  LOOP AT t_08.
    SELECT SINGLE ltext FROM cskt INTO wa_zim01-nome_centro
          WHERE spras EQ sy-langu AND
                kokrs EQ i_area   AND
                kostl EQ t_08-akostl AND
                datbi GE sy-datum .

    SELECT SINGLE kokrs FROM csks INTO v_kokrs
         WHERE  kostl EQ t_08-akostl.

    IF v_kokrs NE i_area.
      CONTINUE.
    ENDIF.

    wa_zim01-ano            = t_08-gjahr.
    wa_zim01-mes            = t_08-aedat+4(2).
    wa_zim01-centro_custo   = t_08-akostl.

    wa_zim01-cod_conta      = ''.
    wa_zim01-nome_conta     = ''.
    wa_zim01-cod_item       = t_08-posnr.
    wa_zim01-nome_item      = 0.
    SELECT SINGLE txt50 FROM imakt
      INTO wa_zim01-nome_item
      WHERE  posnr = t_08-posnr
      AND    spras   EQ sy-langu.

    wa_zim01-cod_compra     = t_08-ebeln.
    READ TABLE t_ap_inv INTO t_ap_inv WITH KEY posnr = t_08-posnr
                                               gsber = t_08-werks BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zim01-id_sysphera    = t_ap_inv-solicitacao_invest.
    ELSE.
      wa_zim01-id_sysphera    = 0.
    ENDIF.
*---> 15/06/2023 - Migração S4 - JS
*       wa_zim01-vlr_local      = t_08-dmbtr.
      wa_zim01-vlr_local = CONV #( t_08-dmbtr ).
*<--- 15/06/2023 - Migração S4 - JS
    wa_zim01-vlr_dolar      = 0.
    COLLECT wa_zim01 INTO resultado.
  ENDLOOP.
  "
  IF t_08_usd[] IS NOT INITIAL.
    SELECT * FROM zim01_sol_ap_inv
      INTO TABLE t_ap_inv
      FOR ALL ENTRIES IN t_08_usd
      WHERE posnr = t_08_usd-posnr
        AND gsber = t_08_usd-werks
       " AND ano   = i_ano
        AND solicitacao_invest NE '000000'.

    SORT t_ap_inv BY posnr gsber ASCENDING solicitacao_invest DESCENDING .
  ENDIF.

  LOOP AT t_08_usd INTO t_08.
    SELECT SINGLE ltext FROM cskt INTO wa_zim01-nome_centro
          WHERE spras EQ sy-langu AND
                kokrs EQ i_area   AND
                kostl EQ t_08-akostl AND
                datbi GE sy-datum .

    SELECT SINGLE kokrs FROM csks INTO v_kokrs
         WHERE  kostl EQ t_08-akostl.

    IF v_kokrs NE i_area.
      CONTINUE.
    ENDIF.

    wa_zim01-ano            = t_08-gjahr.
    wa_zim01-mes            = t_08-aedat+4(2).
    wa_zim01-centro_custo   = t_08-akostl.

    wa_zim01-cod_conta      = ''.
    wa_zim01-nome_conta     = ''.
    wa_zim01-cod_item       = t_08-posnr.
    wa_zim01-nome_item      = 0.
    SELECT SINGLE txt50 FROM imakt
      INTO wa_zim01-nome_item
      WHERE  posnr = t_08-posnr
      AND    spras   EQ sy-langu.

    wa_zim01-cod_compra     = t_08-ebeln.
    READ TABLE t_ap_inv INTO t_ap_inv WITH KEY posnr = t_08-posnr
                                               gsber = t_08-werks BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zim01-id_sysphera    = t_ap_inv-solicitacao_invest.
    ELSE.
      wa_zim01-id_sysphera    = 0.
    ENDIF.
    wa_zim01-vlr_local      = 0.
*---> 15/06/2023 - Migração S4 - JS
*     wa_zim01-vlr_dolar      = t_08-dmbtr.
    wa_zim01-vlr_dolar = CONV #( t_08-dmbtr ).
*<--- 15/06/2023 - Migração S4 - JS
    COLLECT wa_zim01 INTO resultado.
  ENDLOOP.

ENDFUNCTION.
