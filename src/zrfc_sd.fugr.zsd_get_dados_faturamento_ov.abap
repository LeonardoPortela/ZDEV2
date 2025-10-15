FUNCTION zsd_get_dados_faturamento_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_CHARG_INI) TYPE  CHARG_D OPTIONAL
*"     VALUE(I_CHARG_FIM) TYPE  CHARG_D OPTIONAL
*"  TABLES
*"      T_VKORG STRUCTURE  ZSDE_VKORG
*"      T_AUART STRUCTURE  ZSDE_AUARTT
*"      T_VKBUR STRUCTURE  ZSDE_VKBUR
*"      T_WERKS STRUCTURE  ZMMST_WERKS
*"      T_VBTYP_N STRUCTURE  ZSDE_VBTYP_N
*"      T_VBTYP_V STRUCTURE  ZSDE_VBTYP_V
*"      T_SAIDA STRUCTURE  ZSDE_DADOS_FATURAMENTO
*"----------------------------------------------------------------------

  DATA: lra_erdat   TYPE RANGE OF erdat,
        lra_charg   TYPE RANGE OF charg_d,
        lra_vkorg   TYPE RANGE OF vkorg,
        lra_auart   TYPE RANGE OF auart,
        lra_vkbur   TYPE RANGE OF vkbur,
        lra_werks   TYPE RANGE OF werks_d,
        lra_vbtyp_n TYPE RANGE OF vbtypl, "*-Equalização RISE x PRD - 19.07.2023 - JT
        lra_vbtyp_v TYPE RANGE OF vbtypl, "*-Equalização RISE x PRD - 19.07.2023 - JT
        lra_vbelv   TYPE RANGE OF vbelv.

  IF i_erdat_ini IS NOT INITIAL OR i_erdat_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_erdat_ini high = i_erdat_fim ) TO lra_erdat.
  ENDIF.

  IF i_charg_ini IS NOT INITIAL OR i_charg_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_charg_ini high = i_charg_fim ) TO lra_charg.
  ENDIF.

  LOOP AT t_vkorg ASSIGNING FIELD-SYMBOL(<fs_vkorg>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_vkorg>-vkorg ) TO lra_vkorg.
  ENDLOOP.

  LOOP AT t_auart ASSIGNING FIELD-SYMBOL(<fs_auart>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_auart>-auart ) TO lra_auart.
  ENDLOOP.

  LOOP AT t_vkbur ASSIGNING FIELD-SYMBOL(<fs_vkbur>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_vkbur>-vkbur ) TO lra_vkbur.
  ENDLOOP.

  LOOP AT t_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_werks>-werks ) TO lra_werks.
  ENDLOOP.

  LOOP AT t_vbtyp_n ASSIGNING FIELD-SYMBOL(<fs_vbtyp_n>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_vbtyp_n>-vbtyp_n ) TO lra_vbtyp_n.
  ENDLOOP.

  LOOP AT t_vbtyp_v ASSIGNING FIELD-SYMBOL(<fs_vbtyp_v>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_vbtyp_v>-vbtyp_v ) TO lra_vbtyp_v.
  ENDLOOP.


  SELECT vbap~charg AS safra,
         vbfa~vbelv AS ordem_venda,
         kna1~name1 AS emissor_ordem,
         vbak~audat AS dt_emissao_ordem,
         vbfa~erdat AS data_fatura,
         vbfa~rfmng AS qtde_faturada,
         flin~docnum AS doc_num,
         fdoc~pstdat AS data_emissao,
         vbap~arktx AS denominacao,
         kna1~ort01 AS municipio,
         kna1~regio AS uf
   FROM vbak AS vbak
   INNER JOIN vbfa AS vbfa
   ON vbak~vbeln = vbfa~vbelv
   INNER JOIN vbap AS vbap
   ON vbak~vbeln = vbap~vbeln
   INNER JOIN kna1 AS kna1
   ON vbak~kunnr = kna1~kunnr
   LEFT OUTER JOIN vbrk AS vbrk
   ON vbfa~vbelv = vbrk~vbeln
   LEFT OUTER JOIN j_1bnflin AS flin
   ON vbfa~vbeln = flin~refkey
   LEFT OUTER JOIN j_1bnfdoc AS fdoc
   ON flin~docnum = fdoc~docnum
   INTO TABLE @DATA(lt_dados)
  WHERE vbak~vkorg   IN @lra_vkorg
    AND vbak~auart   IN @lra_auart
    AND vbak~vkbur   IN @lra_vkbur
    AND vbap~werks   IN @lra_werks
    AND vbfa~vbtyp_n IN @lra_vbtyp_n
    AND vbfa~vbtyp_v IN @lra_vbtyp_v
    AND vbap~charg   IN @lra_charg
    AND vbfa~erdat   IN @lra_erdat
    AND VBRK~DRAFT = @SPACE .    "*-Equalização RISE x PRD - 19.07.2023 - JT
  IF sy-subrc IS INITIAL.

    SELECT charg, vbeln, nro_sol_ov
      FROM zsdt0053
      INTO TABLE @DATA(lt_zsdt0053)
      FOR ALL ENTRIES IN @lt_dados
     WHERE vbeln EQ @lt_dados-ordem_venda.

  ENDIF.

  SORT lt_zsdt0053 BY charg vbeln.

  LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    MOVE-CORRESPONDING <fs_dados> TO <fs_saida>.

    READ TABLE lt_zsdt0053 ASSIGNING FIELD-SYMBOL(<fs_zsdt0053>)
    WITH KEY charg = <fs_saida>-safra
             vbeln = <fs_saida>-ordem_venda
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida>-numero_solicitacao = <fs_zsdt0053>-nro_sol_ov.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
