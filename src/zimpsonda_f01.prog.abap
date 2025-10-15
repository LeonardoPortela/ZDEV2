*&---------------------------------------------------------------------*
*& Include          ZIMPSONDA_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f4_p_cod
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_p_cod .


  TYPES: BEGIN OF ty_f4_p_cod,
           cod_imposto   TYPE zimp_lanc_impost-cod_imposto,
           descr_imposto TYPE zimp_cad_imposto-descr_imposto.
  TYPES: END OF ty_f4_p_cod.

  DATA: it_f4_p_cod         TYPE STANDARD TABLE OF ty_f4_p_cod,
        wa_f4_p_cod         TYPE ty_f4_p_cod,
        it_zimp_cad_imposto TYPE STANDARD TABLE OF zimp_cad_imposto,
        wa_zimp_cad_imposto TYPE zimp_cad_imposto,
        cont                TYPE i.

  SELECT DISTINCT cod_imposto
  FROM zimp_lanc_impost
  INTO CORRESPONDING FIELDS OF TABLE it_f4_p_cod.

  SELECT *
    FROM zimp_cad_imposto
    INTO TABLE it_zimp_cad_imposto
  FOR ALL ENTRIES IN it_f4_p_cod
  WHERE cod_imposto = it_f4_p_cod-cod_imposto.

  LOOP AT it_f4_p_cod INTO wa_f4_p_cod.
    cont = sy-tabix.
    READ TABLE it_zimp_cad_imposto INTO wa_zimp_cad_imposto WITH KEY cod_imposto = wa_f4_p_cod-cod_imposto.
    IF sy-subrc IS INITIAL.
      wa_f4_p_cod-descr_imposto = wa_zimp_cad_imposto-descr_imposto.
      MODIFY it_f4_p_cod FROM wa_f4_p_cod INDEX cont.
    ENDIF.
  ENDLOOP.

  SORT it_f4_p_cod BY cod_imposto.
  DELETE ADJACENT DUPLICATES FROM it_f4_p_cod COMPARING cod_imposto.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COD_IMPOSTO'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_COD'            " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = it_f4_p_cod   " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_p_user
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_p_user .

  TYPES: BEGIN OF ty_f4_p_user,
           usnam TYPE zimp_cad_lote-usnam.
  TYPES: END OF ty_f4_p_user.

  DATA: it_f4_p_user TYPE STANDARD TABLE OF ty_f4_p_user.

  SELECT DISTINCT usnam
  FROM zimp_cad_lote
  INTO CORRESPONDING FIELDS OF TABLE it_f4_p_user.

  SORT it_f4_p_user BY usnam.
  DELETE ADJACENT DUPLICATES FROM it_f4_p_user COMPARING usnam.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'USNAM'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_USER'            " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = it_f4_p_user   " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_p_dep
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_p_dep .

  TYPES: BEGIN OF ty_f4_p_dep,
           dep_resp      TYPE zimp_cad_lote-dep_resp,
           dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc.
  TYPES: END OF ty_f4_p_dep.

  DATA: it_f4_p_dep       TYPE STANDARD TABLE OF ty_f4_p_dep,
        wa_f4_p_dep       TYPE ty_f4_p_dep,
        it_zimp_cad_depto TYPE STANDARD TABLE OF zimp_cad_depto,
        wa_zimp_cad_depto TYPE zimp_cad_depto,
        cont              TYPE i.

  SELECT DISTINCT dep_resp
  FROM zimp_cad_lote
  INTO CORRESPONDING FIELDS OF TABLE it_f4_p_dep.

  SELECT *
    FROM zimp_cad_depto
    INTO TABLE it_zimp_cad_depto
  FOR ALL ENTRIES IN it_f4_p_dep
  WHERE dep_resp = it_f4_p_dep-dep_resp.

  LOOP AT it_f4_p_dep INTO wa_f4_p_dep.
    cont = sy-tabix.
    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = wa_f4_p_dep-dep_resp.
    IF sy-subrc IS INITIAL.
      wa_f4_p_dep-dep_resp_desc = wa_zimp_cad_depto-dep_resp_desc.
      MODIFY it_f4_p_dep FROM wa_f4_p_dep INDEX cont.
    ENDIF.
  ENDLOOP.

  SORT it_f4_p_dep BY dep_resp.
  DELETE ADJACENT DUPLICATES FROM it_f4_p_dep COMPARING dep_resp.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DEP_RESP'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_DEP'            " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = it_f4_p_dep   " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  RANGES: rg_status_lote FOR zimp_cad_lote-status_lote.

  CLEAR: rg_status_lote[].

  CASE abap_true.
    WHEN p_lot1. "Todos
    WHEN p_lot2. "Lotes Não Liberados
      APPEND VALUE #( sign = 'I' option = 'EQ' low = space ) TO rg_status_lote.
    WHEN p_lot3. "Lotes Não Aprovados
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'L' ) TO rg_status_lote.
    WHEN p_lot4. "Aprovados
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'A' ) TO rg_status_lote.
  ENDCASE.


  SELECT  *
      FROM zimp_cad_lote
      INTO TABLE it_zimp_cad_lote
      WHERE lote        IN p_lote
      AND   bukrs       IN p_bukrs
      AND   loekz = ' '
      AND   dt_venc     IN p_dtv
      AND   usnam       IN p_user
      AND   dep_resp    IN p_dep
      AND   status_lote IN rg_status_lote.

  CHECK it_zimp_cad_lote[] IS NOT INITIAL.

  SELECT *
    FROM zimp_lanc_impost
    INTO CORRESPONDING FIELDS OF TABLE it_zimp_lanc_impost
    FOR ALL ENTRIES IN it_zimp_cad_lote
    WHERE bukrs = it_zimp_cad_lote-bukrs
    AND   lote  = it_zimp_cad_lote-lote
    AND   loekz = ''
    AND cod_imposto IN p_cod.

  CHECK it_zimp_lanc_impost[] IS NOT INITIAL.

  LOOP AT it_zimp_lanc_impost INTO wa_zimp_lanc_impost.
    CONCATENATE 'ZP' wa_zimp_lanc_impost-bukrs wa_zimp_lanc_impost-doc_imposto '%' INTO vobj_key.

    SELECT SINGLE obj_key belnr bukrs gjahr
    FROM zib_contabil_chv
    INTO wa_zib_contabil_chv
    WHERE obj_key LIKE vobj_key
    AND   bukrs = wa_zimp_lanc_impost-bukrs. " P_BUKRS.
    IF sy-subrc NE 0.
      CONCATENATE 'ZIMP' wa_zimp_lanc_impost-doc_imposto '%' INTO vobj_key.
      SELECT SINGLE obj_key belnr bukrs gjahr
        FROM zib_contabil_chv
        INTO wa_zib_contabil_chv
      WHERE obj_key LIKE vobj_key
      AND   bukrs = wa_zimp_lanc_impost-bukrs. " IN P_BUKRS.
    ENDIF.

    IF sy-subrc = 0.
      wa_zimp_lanc_impost-doc_contabil = wa_zib_contabil_chv-belnr.
      wa_zimp_lanc_impost-gjahr        = wa_zib_contabil_chv-gjahr.

      MODIFY it_zimp_lanc_impost FROM wa_zimp_lanc_impost INDEX sy-tabix TRANSPORTING doc_contabil .

      SELECT SINGLE bukrs belnr gjahr budat
        FROM bkpf
        INTO wa_bkpf
        WHERE bukrs = wa_zib_contabil_chv-bukrs
      AND   belnr = wa_zib_contabil_chv-belnr
      AND   gjahr = wa_zib_contabil_chv-gjahr.

      IF sy-subrc = 0.
        wa_zimp_lanc_impost-budat = wa_bkpf-budat.
        MODIFY it_zimp_lanc_impost FROM wa_zimp_lanc_impost INDEX sy-tabix TRANSPORTING budat .
      ENDIF.

      SELECT SINGLE bukrs belnr gjahr augbl
        FROM bsak
        INTO wa_bsak
        WHERE  bukrs =  wa_zib_contabil_chv-bukrs
      AND    belnr =  wa_zib_contabil_chv-belnr
      AND    gjahr =  wa_zib_contabil_chv-gjahr.
      IF sy-subrc = 0.
        wa_zimp_lanc_impost-augbl = wa_bsak-augbl.
        MODIFY it_zimp_lanc_impost FROM wa_zimp_lanc_impost INDEX sy-tabix TRANSPORTING augbl.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT *
   FROM zimp_lanc_imp_ct
   INTO TABLE it_zimp_lanc_imp_ct
   FOR ALL ENTRIES IN it_zimp_lanc_impost
   WHERE doc_imposto = it_zimp_lanc_impost-doc_imposto
   AND   bukrs       = it_zimp_lanc_impost-bukrs.

  CHECK it_zimp_lanc_imp_ct[] IS NOT INITIAL.
  DELETE it_zimp_lanc_imp_ct WHERE valor_imp EQ 0.

  SELECT *
   FROM zimpcodimp
   INTO TABLE it_zimpcodimp.

  SELECT *
   FROM zimpcodobrig
   INTO TABLE it_zimpcodobrig.

  SELECT *
    FROM zimptpajuste
    INTO TABLE it_zimptpajuste.

  SELECT *
  FROM zimp_cad_imposto
  INTO TABLE it_zimp_imp
  WHERE cod_imposto IN p_cod.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form grava_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM grava_dados .

  LOOP AT it_zimp_lanc_impost  ASSIGNING FIELD-SYMBOL(<fs_lanc_impost>).


    wa_zimp_guia_sonda-igu_cod_matriz   = <fs_lanc_impost>-bukrs.
    wa_zimp_guia_sonda-igu_cod_filial   = <fs_lanc_impost>-gsber.
    wa_zimp_guia_sonda-igu_ind_gnre     = 'G'.
    wa_zimp_guia_sonda-igu_cod_receita  = <fs_lanc_impost>-cod_pgto.
    wa_zimp_guia_sonda-igu_ind_antecip  = 'N'.
    wa_zimp_guia_sonda-igu_ind_apuracao = 0.
    wa_zimp_guia_sonda-igu_dt_vencto    = <fs_lanc_impost>-dt_venc.
    wa_zimp_guia_sonda-igu_dt_pagto     = <fs_lanc_impost>-dt_venc.
    wa_zimp_guia_sonda-igu_cod_obrig    = 'APUR_ICMS'.
    wa_zimp_guia_sonda-igu_num_doc_arr  = <fs_lanc_impost>-doc_contabil.

    wa_zimp_lanc_sonda-iaj_cod_matriz   = <fs_lanc_impost>-bukrs.
    wa_zimp_lanc_sonda-iaj_cod_filial   = <fs_lanc_impost>-gsber.
    wa_zimp_lanc_sonda-iaj_nr_doc_arrec = <fs_lanc_impost>-cod_pgto.
*    wa_zimp_lanc_sonda-iaj_desc_compl   = <fs_lanc_impost>-observacao.
    wa_zimp_lanc_sonda-iaj_cod_obri     = 'APUR_ICMS'.

    CONCATENATE <fs_lanc_impost>-data_atual+6(2) '.' <fs_lanc_impost>-data_atual+4(2) '.' <fs_lanc_impost>-data_atual(4)
    INTO wa_zimp_lanc_sonda-iaj_data_atual.

    CONCATENATE <fs_lanc_impost>-budat+6(2) '.' <fs_lanc_impost>-budat+4(2) '.' <fs_lanc_impost>-budat(4)
    INTO wa_zimp_lanc_sonda-iaj_dt_lancto.

    CONCATENATE <fs_lanc_impost>-budat+6(2) '.' <fs_lanc_impost>-budat+4(2) '.' <fs_lanc_impost>-budat(4)
    INTO wa_zimp_lanc_sonda-iaj_dt_documento.

    CLEAR lv_ref.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = <fs_lanc_impost>-budat
        days      = '00'
        months    = '01'
        signum    = '-'
        years     = '00'
      IMPORTING
        calc_date = lv_ref.


    CONCATENATE lv_ref+4(2) '/' lv_ref(4) INTO  wa_zimp_guia_sonda-igu_referencia.

    LOOP AT it_zimp_lanc_imp_ct  ASSIGNING FIELD-SYMBOL(<fs_lanc_ct>) WHERE   bukrs = <fs_lanc_impost>-bukrs AND
                                                                              doc_imposto = <fs_lanc_impost>-doc_imposto.
      CASE <fs_lanc_ct>-cod_abertura.

        WHEN '01'.
          wa_zimp_lanc_sonda-iaj_vl_lancto  = <fs_lanc_ct>-valor_imp.
          wa_zimp_guia_sonda-igu_vl_receita = <fs_lanc_ct>-valor_imp.
        WHEN '05'.
          wa_zimp_guia_sonda-igu_vl_juros   = <fs_lanc_ct>-valor_imp.
        WHEN '04'.
          wa_zimp_guia_sonda-igu_vl_multa   = <fs_lanc_ct>-valor_imp.
        WHEN '11'.
          wa_zimp_guia_sonda-igu_vl_total   = <fs_lanc_ct>-valor_imp.

      ENDCASE.

    ENDLOOP.

    READ TABLE it_zimpcodimp ASSIGNING FIELD-SYMBOL(<fs_cod_imp>) WITH KEY cod_imposto = <fs_lanc_impost>-cod_imposto.

    IF sy-subrc IS INITIAL.
      wa_zimp_lanc_sonda-iaj_cod_imposto = <fs_cod_imp>-cod_imp_comply.
      wa_zimp_lanc_sonda-iaj_uf          = <fs_cod_imp>-uf.
      wa_zimp_guia_sonda-igu_cod_imposto = <fs_cod_imp>-cod_imp_comply.
      wa_zimp_guia_sonda-igu_uf          = <fs_cod_imp>-uf.
    ENDIF.

    READ TABLE it_zimpcodobrig  ASSIGNING FIELD-SYMBOL(<fs_cod_obrig>) WITH KEY cod_guia = <fs_lanc_impost>-cod_pgto.
    IF sy-subrc IS INITIAL.
      wa_zimp_lanc_sonda-iaj_cod_obri = <fs_cod_obrig>-cod_obrig_comply.
    ENDIF.

    wa_zimp_guia_sonda-igu_ind_automat = c_s.

    READ TABLE it_zimp_imp ASSIGNING FIELD-SYMBOL(<fs_descr>) WITH KEY cod_imposto = <fs_lanc_impost>-cod_imposto.
    IF sy-subrc IS INITIAL.
      SPLIT <fs_descr>-descr_imposto AT ' - ' INTO v1 v2.
      CONCATENATE v1 '-' wa_zimp_guia_sonda-igu_referencia INTO wa_zimp_lanc_sonda-iaj_desc_compl SEPARATED BY space.
    ENDIF.

    READ TABLE it_zimptpajuste ASSIGNING FIELD-SYMBOL(<fs_tp_ajuste>) WITH KEY cod_guia = <fs_lanc_impost>-cod_pgto.
    IF sy-subrc IS INITIAL.
      wa_zimp_lanc_sonda-iaj_cod_tp_ajust = <fs_tp_ajuste>-cod_ajuste.
    ENDIF.
    wa_zimp_lanc_sonda-iaj_cod_status = '1'.

    COLLECT wa_zimp_lanc_sonda INTO it_zimp_lanc_sonda.
    APPEND wa_zimp_guia_sonda TO it_zimp_guia_sonda.


  ENDLOOP.


  CHECK it_zimp_lanc_sonda[] IS NOT INITIAL.


  LOOP AT it_zimp_lanc_sonda ASSIGNING FIELD-SYMBOL(<fs_lanc_sonda>).
    INSERT INTO zimp_lanc_sonda VALUES <fs_lanc_sonda>.
  ENDLOOP.

  CHECK it_zimp_guia_sonda[] IS NOT INITIAL.

  LOOP AT it_zimp_guia_sonda ASSIGNING FIELD-SYMBOL(<fs_guia_sonda>).
    INSERT INTO zimp_guia_sonda VALUES <fs_guia_sonda>.
  ENDLOOP.

  MESSAGE 'Dados Gravados com Sucesso' TYPE 'S'.


ENDFORM.
