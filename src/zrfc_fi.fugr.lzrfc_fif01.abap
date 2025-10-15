*----------------------------------------------------------------------*
***INCLUDE LZRFC_FIF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_SAIDA_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DT_PRIMEIR_DIA_MES_ATUAL  text
*      -->P_I_DT_ULTIMO_DIA_MES_ANTERIOR  text
*      -->P_I_TP_PROCESSAMENTO  text
*      -->P_I_BUKRS  text
*      <--P_T_SAIDA_001  text
*----------------------------------------------------------------------*
FORM f_process_saida_001  USING  i_dt_primeiro_dia_mes_atual  TYPE erdat
                                 i_budat_ini                  TYPE budat
                                 i_budat_fim                  TYPE budat
                                 i_tp_processamento           TYPE char01
                                 i_bukrs                      TYPE zfit_bukrs
                       CHANGING  t_saida_001                  TYPE tb_zfie_saida1.

  DATA: lra_bukrs TYPE RANGE OF bukrs,
        lra_taxa  TYPE RANGE OF zfitaxctr-zid_contr,
        lv_data   TYPE tcurr-gdatu.

  LOOP AT i_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_bukrs> ) TO lra_bukrs.
  ENDLOOP.

  CONCATENATE i_dt_primeiro_dia_mes_atual+6(2) i_dt_primeiro_dia_mes_atual+4(2) i_dt_primeiro_dia_mes_atual(4) INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lv_data
    IMPORTING
      output = lv_data.

  SELECT  a~bukrs,
          t~butxt,
          a~lifnr,
          a~gjahr,
          a~belnr,
          a~buzei,
          a~bldat,
          a~augdt,
          a~waers,
          a~shkzg,
          a~dmbtr,
          a~wrbtr,
          a~hkont,
          a~vbund,
          a~xblnr
     FROM bsik AS a INNER JOIN faglflexa AS b
        ON a~bukrs = b~rbukrs
       AND a~belnr = b~belnr
       AND a~gjahr = b~gjahr
       AND a~buzei = b~buzei
       AND a~hkont = b~racct
       INNER JOIN t001 AS t
        ON a~bukrs = t~bukrs
    INTO TABLE @DATA(lt_dados1)
         WHERE a~bukrs IN @lra_bukrs
           AND b~budat BETWEEN @i_budat_ini AND @i_budat_fim
           AND b~rldnr = '0L'
           AND b~bstat in ( ' ' , 'L' ).

  CHECK sy-subrc IS INITIAL.

  DATA(lt_dados_aux) = lt_dados1.
  SORT lt_dados_aux BY hkont.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING hkont.

  SELECT *
    FROM skat
    INTO TABLE @DATA(lt_skat)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE saknr = @lt_dados_aux-hkont
      AND ktopl = '0050'
      AND spras = 'P'.
  IF sy-subrc IS INITIAL.
    SORT lt_skat BY saknr.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY waers.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING waers.

  SELECT *
    FROM tcurr
    INTO TABLE @DATA(lt_tcurr)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE kurst = 'B'
      AND fcurr = @lt_dados_aux-waers
      AND tcurr = 'USD'
      AND gdatu = @lv_data.

  IF sy-subrc IS INITIAL.
    SORT lt_tcurr BY fcurr gdatu.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY xblnr.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING xblnr.

  LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados_aux>-xblnr+4(6) ) TO lra_taxa.
  ENDLOOP.

  SELECT *
    FROM zfitaxctr
    INTO TABLE @DATA(lt_zfitaxctr)
    FOR ALL ENTRIES IN @lra_taxa
   WHERE zid_contr = @lra_taxa-low.
  IF sy-subrc IS INITIAL.
    SORT lt_zfitaxctr BY taxa.
  ENDIF.

  LOOP AT lt_dados1 ASSIGNING FIELD-SYMBOL(<fs_dados1>).

    APPEND INITIAL LINE TO t_saida_001 ASSIGNING FIELD-SYMBOL(<fs_saida_001>).
    MOVE-CORRESPONDING <fs_dados1> TO <fs_saida_001>.

    READ TABLE lt_tcurr ASSIGNING FIELD-SYMBOL(<fs_tcurr>)
    WITH KEY fcurr = <fs_dados1>-waers
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_001>-ukurs = <fs_tcurr>-ukurs.
    ENDIF.

    READ TABLE lt_skat ASSIGNING FIELD-SYMBOL(<fs_skat>)
    WITH KEY saknr = <fs_dados1>-hkont
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_001>-ds_conta_contabil = <fs_skat>-txt20.
    ENDIF.

    READ TABLE lt_zfitaxctr ASSIGNING FIELD-SYMBOL(<fs_zfitaxctr>)
    WITH KEY zid_contr = <fs_dados1>-xblnr+4(6)
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_001>-taxa_zfitaxctr = <fs_zfitaxctr>-taxa.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_SAIDA_002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DT_PRIMEIR_DIA_MES_ATUAL  text
*      -->P_I_DT_ULTIMO_DIA_MES_ANTERIOR  text
*      -->P_I_TP_PROCESSAMENTO  text
*      -->P_I_BUKRS  text
*      <--P_T_SAIDA_002  text
*----------------------------------------------------------------------*
FORM f_process_saida_002  USING  i_dt_primeiro_dia_mes_atual  TYPE erdat
                                 i_augdt_ini                  TYPE augdt
                                 i_augdt_fim                  TYPE augdt
                                 i_budat_ini                  TYPE budat
                                 i_budat_fim                  TYPE budat
                                 i_tp_processamento           TYPE char01
                                 i_bukrs                      TYPE zfit_bukrs
                       CHANGING  t_saida_002                  TYPE tb_zfie_saida1.

  DATA: lra_bukrs TYPE RANGE OF bukrs,
        lra_taxa  TYPE RANGE OF zfitaxctr-zid_contr,
        lv_data   TYPE tcurr-gdatu.

  LOOP AT i_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_bukrs> ) TO lra_bukrs.
  ENDLOOP.

  CONCATENATE i_dt_primeiro_dia_mes_atual+6(2) i_dt_primeiro_dia_mes_atual+4(2) i_dt_primeiro_dia_mes_atual(4) INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lv_data
    IMPORTING
      output = lv_data.

  SELECT  a~bukrs,
          t~butxt,
          a~lifnr,
          a~gjahr,
          a~belnr,
          a~buzei,
          a~bldat,
          a~augdt,
          a~waers,
          a~shkzg,
          a~dmbtr,
          a~wrbtr,
          a~hkont,
          a~vbund,
          a~xblnr
     FROM bsak AS a INNER JOIN faglflexa AS b
        ON a~bukrs = b~rbukrs
       AND a~belnr = b~belnr
       AND a~gjahr = b~gjahr
       AND a~buzei = b~buzei
       AND a~hkont = b~racct
       INNER JOIN t001 AS t
        ON a~bukrs = t~bukrs
    INTO TABLE @DATA(lt_dados1)
         WHERE
            a~bukrs IN @lra_bukrs
           AND a~augdt BETWEEN @i_augdt_ini AND @i_augdt_fim
           AND b~budat BETWEEN @i_budat_ini AND @i_budat_fim
           AND b~rldnr = '0L'
           AND b~bstat in ( ' ' , 'L' ).

  CHECK sy-subrc IS INITIAL.


  DATA(lt_dados_aux) = lt_dados1.
  SORT lt_dados_aux BY hkont.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING hkont.

  SELECT *
    FROM skat
    INTO TABLE @DATA(lt_skat)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE saknr = @lt_dados_aux-hkont
      AND ktopl = '0050'
      AND spras = 'P'.
  IF sy-subrc IS INITIAL.
    SORT lt_skat BY saknr.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY waers.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING waers.

  SELECT *
    FROM tcurr
    INTO TABLE @DATA(lt_tcurr)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE kurst = 'B'
      AND fcurr = @lt_dados_aux-waers
      AND tcurr = 'USD'
      AND gdatu = @lv_data.

  IF sy-subrc IS INITIAL.
    SORT lt_tcurr BY fcurr gdatu.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY xblnr.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING xblnr.

  LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados_aux>-xblnr+4(6) ) TO lra_taxa.
  ENDLOOP.

  SELECT *
    FROM zfitaxctr
    INTO TABLE @DATA(lt_zfitaxctr)
    FOR ALL ENTRIES IN @lra_taxa
   WHERE zid_contr = @lra_taxa-low.
  IF sy-subrc IS INITIAL.
    SORT lt_zfitaxctr BY taxa.
  ENDIF.

  LOOP AT lt_dados1 ASSIGNING FIELD-SYMBOL(<fs_dados1>).

    APPEND INITIAL LINE TO t_saida_002 ASSIGNING FIELD-SYMBOL(<fs_saida_002>).
    MOVE-CORRESPONDING <fs_dados1> TO <fs_saida_002>.

    READ TABLE lt_tcurr ASSIGNING FIELD-SYMBOL(<fs_tcurr>)
    WITH KEY fcurr = <fs_dados1>-waers
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_002>-ukurs = <fs_tcurr>-ukurs.
    ENDIF.

    READ TABLE lt_skat ASSIGNING FIELD-SYMBOL(<fs_skat>)
           WITH KEY saknr = <fs_dados1>-hkont
           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_002>-ds_conta_contabil = <fs_skat>-txt20.
    ENDIF.

    READ TABLE lt_zfitaxctr ASSIGNING FIELD-SYMBOL(<fs_zfitaxctr>)
    WITH KEY zid_contr = <fs_dados1>-xblnr+4(6)
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_002>-taxa_zfitaxctr = <fs_zfitaxctr>-taxa.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_SAIDA_003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DT_PRIMEIR_DIA_MES_ATUAL  text
*      -->P_I_DT_ULTIMO_DIA_MES_ANTERIOR  text
*      -->P_I_TP_PROCESSAMENTO  text
*      -->P_I_BUKRS  text
*      <--P_T_SAIDA_003  text
*----------------------------------------------------------------------*
FORM f_process_saida_003  USING  i_dt_primeiro_dia_mes_atual  TYPE erdat
                                 i_budat_ini                  TYPE budat
                                 i_budat_fim                  TYPE budat
                                 i_tp_processamento           TYPE char01
                                 i_bukrs                      TYPE zfit_bukrs
                       CHANGING  t_saida_003                  TYPE tb_zfie_saida3.

  DATA: lra_bukrs TYPE RANGE OF bukrs,
        lra_taxa  TYPE RANGE OF zfitaxctr-zid_contr,
        lv_data   TYPE tcurr-gdatu.

  LOOP AT i_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_bukrs> ) TO lra_bukrs.
  ENDLOOP.

  CONCATENATE i_dt_primeiro_dia_mes_atual+6(2) i_dt_primeiro_dia_mes_atual+4(2) i_dt_primeiro_dia_mes_atual(4) INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lv_data
    IMPORTING
      output = lv_data.

  SELECT  a~bukrs,
          t~butxt,
          a~kunnr,
          a~gjahr,
          a~belnr,
          a~buzei,
          a~bldat,
          a~augdt,
          a~waers,
          a~shkzg,
          a~dmbtr,
          a~wrbtr,
          a~hkont,
          s~txt20 AS ds_conta_contabil,
          a~vbund AS ds_sociedade_parceira,
          a~xblnr AS ds_referencia
     FROM bsid AS a INNER JOIN faglflexa AS b
        ON a~bukrs = b~rbukrs
       AND a~belnr = b~belnr
       AND a~gjahr = b~gjahr
       AND a~buzei = b~buzei
       AND a~hkont = b~racct
       INNER JOIN t001 AS t
        ON a~bukrs = t~bukrs
       LEFT JOIN skat AS s
        ON s~saknr = a~hkont
       AND s~ktopl = '0050'
       AND s~spras = 'P'
    INTO TABLE @DATA(lt_dados1)
         WHERE a~bukrs IN @lra_bukrs
           AND b~budat BETWEEN @i_budat_ini AND @i_budat_fim
           AND b~rldnr = '0L'
           AND b~bstat in ( ' ' , 'L' ).

  CHECK sy-subrc IS INITIAL.

  DATA(lt_dados_aux) = lt_dados1.
  SORT lt_dados_aux BY hkont.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING hkont.

  SELECT *
    FROM skat
    INTO TABLE @DATA(lt_skat)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE saknr = @lt_dados_aux-hkont
      AND ktopl = '0050'
      AND spras = 'P'.
  IF sy-subrc IS INITIAL.
    SORT lt_skat BY saknr.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY waers.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING waers.

  SELECT *
    FROM tcurr
    INTO TABLE @DATA(lt_tcurr)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE kurst = 'B'
      AND fcurr = @lt_dados_aux-waers
      AND tcurr = 'USD'
      AND gdatu = @lv_data.

  IF sy-subrc IS INITIAL.
    SORT lt_tcurr BY fcurr gdatu.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY ds_referencia.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING ds_referencia.

  LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados_aux>-ds_referencia+4(6) ) TO lra_taxa.
  ENDLOOP.

  SELECT *
    FROM zfitaxctr
    INTO TABLE @DATA(lt_zfitaxctr)
    FOR ALL ENTRIES IN @lra_taxa
   WHERE zid_contr = @lra_taxa-low.
  IF sy-subrc IS INITIAL.
    SORT lt_zfitaxctr BY taxa.
  ENDIF.

  LOOP AT lt_dados1 ASSIGNING FIELD-SYMBOL(<fs_dados1>).

    APPEND INITIAL LINE TO t_saida_003 ASSIGNING FIELD-SYMBOL(<fs_saida_003>).
    MOVE-CORRESPONDING <fs_dados1> TO <fs_saida_003>.

    READ TABLE lt_tcurr ASSIGNING FIELD-SYMBOL(<fs_tcurr>)
    WITH KEY fcurr = <fs_dados1>-waers.
    IF sy-subrc IS INITIAL.
      <fs_saida_003>-ukurs = <fs_tcurr>-ukurs.
    ENDIF.

    READ TABLE lt_skat ASSIGNING FIELD-SYMBOL(<fs_skat>)
    WITH KEY saknr = <fs_dados1>-hkont
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_003>-ds_conta_contabil = <fs_skat>-txt20.
    ENDIF.

    READ TABLE lt_zfitaxctr ASSIGNING FIELD-SYMBOL(<fs_zfitaxctr>)
    WITH KEY zid_contr = <fs_dados1>-ds_referencia+4(6)
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_003>-taxa_zfitaxctr = <fs_zfitaxctr>-taxa.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_SAIDA_004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DT_PRIMEIR_DIA_MES_ATUAL  text
*      -->P_I_DT_ULTIMO_DIA_MES_ANTERIOR  text
*      -->P_I_TP_PROCESSAMENTO  text
*      -->P_I_BUKRS  text
*      <--P_T_SAIDA_004  text
*----------------------------------------------------------------------*
FORM f_process_saida_004  USING  i_dt_primeiro_dia_mes_atual  TYPE erdat
                                 i_augdt_ini                  TYPE augdt
                                 i_augdt_fim                  TYPE augdt
                                 i_budat_ini                  TYPE budat
                                 i_budat_fim                  TYPE budat
                                 i_tp_processamento           TYPE char01
                                 i_bukrs                      TYPE zfit_bukrs
                       CHANGING  t_saida_004                  TYPE tb_zfie_saida3.

  DATA: lra_bukrs TYPE RANGE OF bukrs,
        lra_taxa  TYPE RANGE OF zfitaxctr-zid_contr,
        lv_data   TYPE tcurr-gdatu.

  LOOP AT i_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_bukrs> ) TO lra_bukrs.
  ENDLOOP.

  CONCATENATE i_dt_primeiro_dia_mes_atual+6(2) i_dt_primeiro_dia_mes_atual+4(2) i_dt_primeiro_dia_mes_atual(4) INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lv_data
    IMPORTING
      output = lv_data.

  SELECT  a~bukrs,
          t~butxt,
          a~kunnr,
          a~gjahr,
          a~belnr,
          a~buzei,
          a~bldat,
          a~augdt,
          a~waers,
          a~shkzg,
          a~dmbtr,
          a~wrbtr,
          a~hkont,
          a~vbund AS ds_sociedade_parceira,
          a~xblnr AS ds_referencia
     FROM bsad AS a INNER JOIN faglflexa AS b
        ON a~bukrs = b~rbukrs
       AND a~belnr = b~belnr
       AND a~gjahr = b~gjahr
       AND a~buzei = b~buzei
       AND a~hkont = b~racct
       INNER JOIN t001 AS t
        ON a~bukrs = t~bukrs
    INTO TABLE @DATA(lt_dados1)
         WHERE
            a~bukrs IN @lra_bukrs
           AND a~augdt BETWEEN @i_augdt_ini AND @i_augdt_fim
           AND b~budat BETWEEN @i_budat_ini AND @i_budat_fim
           AND b~rldnr = '0L'
           AND b~bstat in ( ' ' , 'L' ).

  CHECK sy-subrc IS INITIAL.

  DATA(lt_dados_aux) = lt_dados1.
  SORT lt_dados_aux BY hkont.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING hkont.

  SELECT *
    FROM skat
    INTO TABLE @DATA(lt_skat)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE saknr = @lt_dados_aux-hkont
      AND ktopl = '0050'
      AND spras = 'P'.
  IF sy-subrc IS INITIAL.
    SORT lt_skat BY saknr.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY waers.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING waers.

  SELECT *
    FROM tcurr
    INTO TABLE @DATA(lt_tcurr)
    FOR ALL ENTRIES IN @lt_dados_aux
    WHERE kurst = 'B'
      AND fcurr = @lt_dados_aux-waers
      AND tcurr = 'USD'
      AND gdatu = @lv_data.
  IF sy-subrc IS INITIAL.
    SORT lt_tcurr BY fcurr gdatu.
  ENDIF.

  lt_dados_aux = lt_dados1.
  SORT lt_dados_aux BY ds_referencia.
  DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING ds_referencia.

  LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados_aux>-ds_referencia+4(6) ) TO lra_taxa.
  ENDLOOP.

  SELECT *
    FROM zfitaxctr
    INTO TABLE @DATA(lt_zfitaxctr)
    FOR ALL ENTRIES IN @lra_taxa
   WHERE zid_contr = @lra_taxa-low.
  IF sy-subrc IS INITIAL.
    SORT lt_zfitaxctr BY taxa.
  ENDIF.

  LOOP AT lt_dados1 ASSIGNING FIELD-SYMBOL(<fs_dados1>).

    APPEND INITIAL LINE TO t_saida_004 ASSIGNING FIELD-SYMBOL(<fs_saida_004>).
    MOVE-CORRESPONDING <fs_dados1> TO <fs_saida_004>.

    READ TABLE lt_tcurr ASSIGNING FIELD-SYMBOL(<fs_tcurr>)
    WITH KEY fcurr = <fs_dados1>-waers
             gdatu = lv_data
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_004>-ukurs = <fs_tcurr>-ukurs.
    ENDIF.

    READ TABLE lt_skat ASSIGNING FIELD-SYMBOL(<fs_skat>)
    WITH KEY saknr = <fs_dados1>-hkont
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_004>-ds_conta_contabil = <fs_skat>-txt20.
    ENDIF.

    READ TABLE lt_zfitaxctr ASSIGNING FIELD-SYMBOL(<fs_zfitaxctr>)
    WITH KEY zid_contr = <fs_dados1>-ds_referencia+4(6)
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_004>-taxa_zfitaxctr = <fs_zfitaxctr>-taxa.
    ENDIF.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA
*&---------------------------------------------------------------------*
FORM z_partidas_aberto_saida CHANGING t_saida TYPE tb_zfi_saida.

  SORT: it_bsid      BY vbel2,
         it_kna1      BY kunnr,
         it_zsdt0041  BY vbeln,
         it_vbak      BY vbeln,
         it_tspat     BY spart.

  LOOP  AT it_bsid INTO wa_bsid.

    READ TABLE it_vbak INTO DATA(lwa_vbak) WITH KEY vbeln = wa_bsid-vbel2 BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_aging_siacorp-docvenda    = lwa_vbak-vbeln.

      READ TABLE it_tspat INTO DATA(lwa_tspat) WITH KEY spart = lwa_vbak-spart BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_aging_siacorp-setor_atividade = lwa_tspat-vtext.
      ENDIF.

    ENDIF.

    READ TABLE it_zsdt0041 INTO DATA(lwa_zsdt0041) WITH KEY vbeln = wa_bsid-vbel2 BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_aging_siacorp-doc_simulador  = lwa_zsdt0041-doc_simulacao.
    ENDIF.

    wa_aging_siacorp-bstat         = wa_bsid-bstat.
    wa_aging_siacorp-umskz         = wa_bsid-umskz.
    wa_aging_siacorp-codclientesap = wa_bsid-kunnr."CodClienteSAP
    wa_aging_siacorp-coddocumento  = wa_bsid-belnr."CodDocumento
    wa_aging_siacorp-codparcela    = '001'.
    wa_aging_siacorp-datdocumento  = wa_bsid-bldat."DatDocumento
    wa_aging_siacorp-datvencimento = wa_bsid-zfbdt + wa_bsid-zbd1t."dt vcto
    wa_aging_siacorp-bukrs         = wa_bsid-bukrs."Empresa
    wa_aging_siacorp-werks         = wa_bsid-bupla."Centro
    wa_aging_siacorp-moeda         = wa_bsid-waers."Moeda
    wa_aging_siacorp-tp_doc_sap	   = wa_bsid-blart."Tipo de Documento
    wa_aging_siacorp-conta_razao   = wa_bsid-hkont."Conta Razão


    wa_aging_siacorp-valaberto = 0.

    IF ( wa_bsid-shkzg = 'H' ).
      wa_aging_siacorp-vlr_doc_moeda_int   = wa_bsid-dmbtr * ( -1 )."Montante em moeda interna
      wa_aging_siacorp-valparcela          = wa_bsid-dmbtr * ( -1 )."Montante em moeda interna
      wa_aging_siacorp-vlr_doc_moeda_forte = wa_bsid-dmbe2 * ( -1 ).
      wa_aging_siacorp-valaberto           = wa_bsid-dmbtr * ( -1 ).
    ELSE.
      wa_aging_siacorp-vlr_doc_moeda_int   = wa_bsid-dmbtr.
      wa_aging_siacorp-valparcela          = wa_bsid-dmbtr.
      wa_aging_siacorp-vlr_doc_moeda_forte = wa_bsid-dmbe2.
      wa_aging_siacorp-valaberto           = wa_bsid-dmbtr.
    ENDIF.

    wa_aging_siacorp-dt_referencia = sy-datum.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
*    APPEND wa_aging_siacorp TO it_aging_siacorp.
    APPEND wa_aging_siacorp TO t_saida.

    CLEAR: wa_bsid,
           wa_aging_siacorp.
  ENDLOOP.

  DELETE t_saida WHERE doc_simulador NOT IN it_range_doc_simu.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA_F
*&---------------------------------------------------------------------*
FORM z_partidas_aberto_saida_f CHANGING t_saida  TYPE tb_zfi_saida.
  LOOP  AT it_bsik INTO wa_bsik.


    wa_aging_siacorp-bstat         = wa_bsik-bstat.
    wa_aging_siacorp-umskz         = wa_bsik-umskz.
    wa_aging_siacorp-codfornecsap  = wa_bsik-lifnr."CodClienteSAP
    wa_aging_siacorp-coddocumento  = wa_bsik-belnr."CodDocumento
    wa_aging_siacorp-codparcela    = '001'.
    wa_aging_siacorp-datdocumento  = wa_bsik-bldat."DatDocumento
    wa_aging_siacorp-datvencimento = wa_bsik-zfbdt + wa_bsik-zbd1t."Dt vcto
    wa_aging_siacorp-bukrs         = wa_bsik-bukrs."Empresa
    wa_aging_siacorp-werks         = wa_bsik-bupla."Centro
    wa_aging_siacorp-moeda         = wa_bsik-waers."Moeda
    wa_aging_siacorp-tp_doc_sap	   = wa_bsik-blart."Tipo de Documento
    wa_aging_siacorp-conta_razao   = wa_bsik-hkont."Conta Razão

    wa_aging_siacorp-valaberto = 0.

    IF ( wa_bsik-shkzg = 'H' ).
      wa_aging_siacorp-vlr_doc_moeda_int   = wa_bsik-dmbtr * ( -1 )."Montante em moeda interna
      wa_aging_siacorp-valparcela          = wa_bsik-dmbtr * ( -1 )."Montante em moeda interna
      wa_aging_siacorp-vlr_doc_moeda_forte = wa_bsik-dmbe2 * ( -1 ).
      wa_aging_siacorp-valaberto           = wa_bsik-dmbtr * ( -1 ).
    ELSE.
      wa_aging_siacorp-vlr_doc_moeda_int   = wa_bsik-dmbtr.
      wa_aging_siacorp-valparcela          = wa_bsik-dmbtr.
      wa_aging_siacorp-vlr_doc_moeda_forte = wa_bsik-dmbe2.
      wa_aging_siacorp-valaberto           = wa_bsik-dmbtr.
    ENDIF.

    wa_aging_siacorp-dt_referencia = sy-datum.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
*    APPEND wa_aging_siacorp TO it_aging_siacorp.
    APPEND wa_aging_siacorp TO t_saida.

    CLEAR: wa_bsik,
           wa_aging_siacorp.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_F
*&---------------------------------------------------------------------*
FORM f_select_f CHANGING t_saida TYPE tb_zfi_saida.

  DATA: lv_index     TYPE sy-tabix,
        lv_datavenci TYPE sy-datum.

  SELECT lifnr ktokk
    FROM lfa1
    INTO TABLE it_lfa1
   WHERE lifnr IN it_range_lifnr
     AND ktokk IN ('ZFNF','ZFNJ', 'ZPRJ' ).",'ZPRF','ZPRJ'

  CHECK it_lfa1[] IS NOT INITIAL.

  SELECT *
    FROM bsik AS bs
    INTO TABLE it_bsik
     FOR ALL ENTRIES IN it_lfa1
   WHERE budat IN it_range_budat
     AND lifnr EQ it_lfa1-lifnr
     AND blart NE 'VC'
     AND zuonr NE 'PERFORMANCE'.

  DELETE it_bsik WHERE bukrs NOT IN it_range_bukrs.

  CHECK it_bsik[] IS NOT INITIAL.

  LOOP AT it_bsik ASSIGNING FIELD-SYMBOL(<fs_bsik>).
    lv_index = sy-tabix.
    lv_datavenci = <fs_bsik>-zfbdt + <fs_bsik>-zbd1t.
    IF lv_datavenci NOT IN it_range_zfbdt.
      DELETE it_bsik INDEX lv_index.
    ENDIF.
  ENDLOOP.


  PERFORM: z_partidas_aberto_saida_f CHANGING t_saida[].


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_C
*&---------------------------------------------------------------------*
FORM f_select_c CHANGING t_saida TYPE tb_zfi_saida.

  DATA: lv_index     TYPE sy-tabix,
        lv_datavenci TYPE sy-datum.

  "Bsid e Bsad
*----------Clientes

  SELECT kunnr ktokd
    FROM kna1
    INTO TABLE it_kna1
   WHERE kunnr IN it_range_kunnr
     AND ktokd IN ('ZCNF','ZCNJ','ZCPF', 'ZCPJ').
*----------Fim Clientes

*----------Partidas em Aberto
  CHECK it_kna1[] IS NOT INITIAL.

  SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
         bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
         bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
         bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks bs~bstat
    FROM bsid AS bs
    INTO TABLE it_bsid
     FOR ALL ENTRIES IN  it_kna1
   WHERE budat  IN it_range_budat
     AND kunnr  EQ it_kna1-kunnr
     AND vbel2  IN it_range_vbeln
     AND blart  NE 'VC'
     AND zuonr  NE 'PERFORMANCE'.

  DELETE it_bsid WHERE bukrs NOT IN it_range_bukrs.

  CHECK it_bsid[] IS NOT INITIAL.

  LOOP AT it_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
    lv_index = sy-tabix.
    lv_datavenci = <fs_bsid>-zfbdt + <fs_bsid>-zbd1t.
    IF lv_datavenci NOT IN it_range_zfbdt.
      DELETE it_bsid INDEX lv_index.
    ENDIF.
  ENDLOOP.

  DATA(lit_bsid_aux) = it_bsid[].

  DELETE lit_bsid_aux WHERE vbel2 IS INITIAL.
  SORT lit_bsid_aux BY vbel2.
  DELETE ADJACENT DUPLICATES FROM lit_bsid_aux COMPARING vbel2.

  IF lit_bsid_aux[] IS NOT INITIAL.
    SELECT vbeln doc_simulacao
       FROM zsdt0041 INTO TABLE it_zsdt0041
         FOR ALL ENTRIES IN lit_bsid_aux
        WHERE vbeln = lit_bsid_aux-vbel2.

    SELECT vbeln spart
       FROM vbak INTO TABLE it_vbak
         FOR ALL ENTRIES IN lit_bsid_aux
        WHERE vbeln = lit_bsid_aux-vbel2.
  ENDIF.

  IF it_vbak[] IS NOT INITIAL.
    DATA(lit_vbak_aux) = it_vbak[].

    SORT lit_vbak_aux BY spart.
    DELETE ADJACENT DUPLICATES FROM lit_vbak_aux COMPARING spart.

    SELECT spart vtext
      FROM tspat INTO TABLE it_tspat
       FOR ALL ENTRIES IN lit_vbak_aux
      WHERE spart = lit_vbak_aux-spart
        AND spras = 'P'.
  ENDIF.



  PERFORM: z_partidas_aberto_saida CHANGING t_saida[].

ENDFORM.
