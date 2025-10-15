REPORT zlfrx001.

TABLES: /pwsati/zlftciap, /pwsati/zsati050, /pwsati/zsati010, /pwsati/zsati013.

*&---------------------------------------------------------------------*
*&      Form f_exit_zylfciapc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM f_exit_zylfciapc  TABLES itab_anla
                              itab_anlb
                              itab_anlz
                              itab_t095
                              itab_t095b
                              itab_result_liq
                     CHANGING itab_ciap TYPE /pwsati/zlftciap.

  DATA: vl_bukrs TYPE anla-bukrs,
        vl_anln1 TYPE anla-anln1,
        vl_anln2 TYPE anla-anln2,
        vl_txt50 TYPE anla-txt50,
        vl_txa50 TYPE anla-txa50.

  SELECT SINGLE
         bukrs anln1 anln2 txt50 txa50
    FROM anla
    INTO (vl_bukrs, vl_anln1, vl_anln2 , vl_txt50, vl_txa50)
   WHERE bukrs EQ itab_ciap-cod_matriz
     AND anln1 EQ itab_ciap-cod_imobilizado
     AND anln2 EQ itab_ciap-sub_num_imob.

  CHECK sy-subrc = 0.
  CHECK vl_bukrs IS NOT INITIAL.
  CHECK vl_anln1 IS NOT INITIAL.
  CHECK vl_anln2 IS NOT INITIAL.
  CHECK vl_txt50 IS NOT INITIAL.

  IF vl_txa50 IS NOT INITIAL.
    CONCATENATE vl_txt50 vl_txa50 INTO itab_ciap-funcao_bem SEPARATED BY space.
  ELSE.
    itab_ciap-funcao_bem = vl_txt50.
  ENDIF.

ENDFORM.            " f_exit_zylfciapc


*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ZYLFENTITM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB_J_1BNFDOC_UNIT_T  text
*      -->ITAB_J_1BNFLIN_T       text
*      -->ITAB_J_1BNFSTX_T       text
*      -->ITAB_J_1BINDOC_T       text
*      -->ITAB_J_1BNFDOC         text
*      -->ITAB_J_1BNFLIN         text
*      -->ZLFT_ENT_ITM           text
*----------------------------------------------------------------------*
FORM f_exit_zylfentitm TABLES itab_j_1bnfdoc_unit_t
                              itab_j_1bnflin_t
                              itab_j_1bnfstx_t
                              itab_j_1bindoc_t
                        USING itab_j_1bnfdoc TYPE j_1bnfdoc
                              itab_j_1bnflin TYPE j_1bnflin
                     CHANGING zlft_ent_itm   TYPE zlft_ent_itm.


  "DefiniÃ§Ã£o de MovimentaÃ§Ã£o Fisica do Item
  IF itab_j_1bnfdoc-model = '57'.
    zlft_ent_itm-ind_mov_fis_merc = 'N'.
  ENDIF.

ENDFORM.                    "F_EXIT_ZYLFENTITM


*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ZYLFSAT012
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB_ZSATI012  text
*----------------------------------------------------------------------*
FORM f_exit_zylfsat012 CHANGING itab_zsati012 TYPE /pwsati/zsati012.
* Ajuste -  TM - 04.23 - Inicio *
  DATA: lv_maktx TYPE j_1bnflin-maktx,
        lv_docnum TYPE j_1bnflin-docnum,
        lv_itmnum TYPE j_1bnflin-itmnum.
* Ajuste -  TM - 04.23 - Fim    *

  IF itab_zsati012-iii_tp_merc IS INITIAL.
    itab_zsati012-iii_tp_merc = '1'.
  ENDIF.

* Ajuste -  TM - 04.23 - Inicio *
  CLEAR: lv_maktx, lv_itmnum, lv_docnum.

  lv_docnum = itab_zsati012-inf_docnum+5(10).
  lv_itmnum = itab_zsati012-ini_seq.

  SELECT SINGLE maktx
  FROM j_1bnflin
  INTO lv_maktx
  WHERE docnum = lv_docnum
    AND itmnum = lv_itmnum.
  IF sy-subrc EQ 0.
    itab_zsati012-iii_dsc_imob_def =  lv_maktx.
  ENDIF.
* Ajuste -  TM - 04.23 - Fim    *

  INSERT INTO /pwsati/zsati012 VALUES itab_zsati012.

ENDFORM.            " f_exit_zylfsat012


*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ZYLFCIAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB_CIAP  text
*----------------------------------------------------------------------*
FORM f_exit_zylfciap CHANGING itab_ciap TYPE /pwsati/zlftciap.

  DATA: vl_budat  TYPE erdat,
        vl_candat TYPE j_1bnfdoc-candat,
        wl_anek   TYPE anek,
        wl_lin    TYPE j_1bnflin,
        vl_refkey TYPE j_1bnflin-refkey.

  IF ( itab_ciap-dt_ini          IS NOT INITIAL ) AND
     ( itab_ciap-cod_matriz      IS NOT INITIAL ) AND
     ( itab_ciap-cod_imobilizado IS NOT INITIAL ).

    CLEAR: vl_budat, wl_anek, wl_lin.

    CALL FUNCTION '/PWSATI/ZFORMAT_DATE_REVERSE'
      EXPORTING
        input  = itab_ciap-dt_ini
      IMPORTING
        output = vl_budat.

    SELECT SINGLE *
      FROM anek INTO wl_anek
     WHERE bukrs EQ itab_ciap-cod_matriz
       AND anln1 EQ itab_ciap-cod_imobilizado
       AND anln2 EQ itab_ciap-sub_num_imob
       AND budat EQ vl_budat
       AND tcode EQ 'MIRO'.

    IF sy-subrc NE 0.
      SELECT SINGLE *
        FROM anek INTO wl_anek
       WHERE bukrs EQ itab_ciap-cod_matriz
         AND anln1 EQ itab_ciap-cod_imobilizado
         AND anln2 EQ itab_ciap-sub_num_imob
         AND budat EQ vl_budat
         AND tcode LIKE '%MIGO%'.
    ENDIF.

    IF wl_anek IS NOT INITIAL .

      IF wl_anek-tcode EQ 'MIRO'.

        vl_refkey = wl_anek-belnr && wl_anek-gjahr.

        SELECT SINGLE a~* INTO @wl_lin
          FROM j_1bnflin AS a INNER JOIN j_1bnfdoc AS b ON a~docnum = b~docnum
         WHERE a~refkey EQ @vl_refkey
           AND b~candat EQ @vl_candat.

      ELSE.

        SELECT SINGLE *
          FROM ekbe INTO @data(_wl_ekbe)
         WHERE lfbnr = @wl_anek-belnr "Referencia MIGO
           AND vgabe = '2'. "MIRO

        IF sy-subrc EQ 0 .
          vl_refkey = _wl_ekbe-belnr && _wl_ekbe-gjahr.

          SELECT SINGLE a~* INTO @wl_lin
            FROM j_1bnflin AS a INNER JOIN j_1bnfdoc AS b ON a~docnum = b~docnum
           WHERE a~refkey EQ @vl_refkey
             AND b~candat EQ @vl_candat.
        ENDIF.

      ENDIF.

      IF wl_lin IS NOT INITIAL.
        itab_ciap-docnum   = wl_lin-docnum.
        itab_ciap-id_item  = wl_lin-itmnum.
      ENDIF.

    ENDIF.

  ENDIF.

  PERFORM migra_bc USING itab_ciap.


ENDFORM.            " f_exit_zylfciapc

*&---------------------------------------------------------------------*
*&      Form  MIGRA_BC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CIAP     text
*----------------------------------------------------------------------*
FORM migra_bc USING    p_ciap LIKE /pwsati/zlftciap.

  CLEAR: /pwsati/zsati050.

  /pwsati/zsati050-ifc_imb_andam   = p_ciap-ordem_ia.
  /pwsati/zsati050-ifc_imb_def     = p_ciap-cod_imobilizado.
  /pwsati/zsati050-ifc_sub_nr_imb  = p_ciap-sub_num_imob.
  /pwsati/zsati050-ifc_cod_matriz  = p_ciap-cod_matriz.

  CALL FUNCTION '/PWSATI/ZFORMAT_DATE_REVERSE'
    EXPORTING
      input  = p_ciap-dt_ini
    IMPORTING
      output = /pwsati/zsati050-ifc_dt_ini.

  CALL FUNCTION '/PWSATI/ZFORMAT_DATE_REVERSE'
    EXPORTING
      input  = p_ciap-dt_depr_ini
    IMPORTING
      output = /pwsati/zsati050-ifc_dt_depr_ini.


  /pwsati/zsati050-ifc_vida_util   = p_ciap-vida_util.
  /pwsati/zsati050-ifc_conta       = p_ciap-cod_cta.
  /pwsati/zsati050-ifc_conta_depr  = p_ciap-cod_cta_depr.
  /pwsati/zsati050-ifc_centro_cust = p_ciap-centro_custo.
  /pwsati/zsati050-ifc_fnc_bem     = p_ciap-funcao_bem.
  /pwsati/zsati050-ifc_dsc_imob    = p_ciap-descr_imob_def.
  /pwsati/zsati050-ifc_conta_comp  = p_ciap-cod_cta_comp.
  /pwsati/zsati050-ifc_cod_filial  = p_ciap-cod_filial.
* TS207652 23.05.2018-I:CriaÃ§Ã£o dos campos DOCNUM e ID_ITEM
*                       preenchimento via EXIT ZYLFCIAP
  /pwsati/zsati050-ifc_docnum      = p_ciap-docnum.
  /pwsati/zsati050-ifc_id_item     = p_ciap-id_item.
* TS207652 23.05.2018-F:CriaÃ§Ã£o dos campos DOCNUM e ID_ITEM
*                       preenchimento via EXIT ZYLFCIAP

  INSERT /pwsati/zsati050 FROM /pwsati/zsati050.

  CLEAR /pwsati/zsati050.

ENDFORM.                    " migra_bc


*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ZYLFSATI10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSATI010 text
*----------------------------------------------------------------------*
FORM f_exit_zylfsati10 CHANGING  p_zsati010 LIKE /pwsati/zsati010.

  DATA: tg_zsati007 TYPE TABLE OF /pwsati/zsati007 WITH HEADER LINE.

  DATA: vcfop TYPE zfit0148-cfop.

  INSERT INTO /pwsati/zsati010 VALUES /pwsati/zsati010.

  IF p_zsati010-ini_cfop IS NOT INITIAL.

    CONCATENATE  p_zsati010-ini_cfop p_zsati010-ini_cfop_dig  INTO vcfop.

    SELECT SINGLE * FROM zfit0148
      INTO @data(wa_zfit0148)
    WHERE cfop EQ @vcfop.

    IF wa_zfit0148 IS NOT INITIAL.
      /pwsati/zsati013-inf_docnum      = p_zsati010-inf_docnum.
      /pwsati/zsati013-ipt_imp_simplif = 'N'.
      /pwsati/zsati013-ipt_num_di      = '000000000000'.

      INSERT /pwsati/zsati013 FROM /pwsati/zsati013.
    ENDIF.

    IF ( vcfop EQ '2551AA' OR
         vcfop EQ '1551AA' )

       AND

      ( p_zsati010-ini_cod_iva EQ 'P1' OR
        p_zsati010-ini_cod_iva EQ 'P2' OR
        p_zsati010-ini_cod_iva EQ 'P4' ).

      CLEAR: tg_zsati007[].

      SELECT *
        FROM /pwsati/zsati007 INTO TABLE tg_zsati007
       WHERE inf_docnum EQ p_zsati010-inf_docnum
         AND ini_seq    EQ p_zsati010-ini_seq.

      LOOP AT tg_zsati007.

        CHECK ( tg_zsati007-iip_codigo EQ '01' ) AND ( tg_zsati007-iip_vlr_imposto NE 0 ).

        DELETE FROM /pwsati/zsati007 WHERE inf_docnum  = tg_zsati007-inf_docnum
                                       AND ini_seq     = tg_zsati007-ini_seq
                                       AND iip_codigo  = tg_zsati007-iip_codigo
                                       AND iip_tp_lanc = tg_zsati007-iip_tp_lanc.

        tg_zsati007-iip_tp_lanc = '3'.

        MODIFY /pwsati/zsati007 FROM tg_zsati007.
      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.                    "F_EXIT_ZYLFSATI10


*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ZYLFINV021
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_ZSATI020  text
*      -->IT_ZSATI021  text
*----------------------------------------------------------------------*
FORM f_exit_zylfinv021 TABLES  it_zsati020 STRUCTURE /pwsati/zsati020
                               it_zsati021 STRUCTURE /pwsati/zsati021.

  DATA: tg_zsati020 TYPE TABLE OF /pwsati/zsati020 WITH HEADER LINE.

  DATA: tg_mard TYPE TABLE OF mard WITH HEADER LINE,
        v_matnr TYPE mara-matnr.

  tg_zsati020[] = it_zsati020[].

  LOOP AT tg_zsati020.

    CLEAR: tg_mard[].

    v_matnr = |{ TG_ZSATI020-IIV_COD_PRODUTO alpha = in }|.

    SELECT *
      FROM mard INTO TABLE tg_mard
     WHERE matnr EQ v_matnr
       AND werks EQ tg_zsati020-iiv_area_aval.

    LOOP AT tg_mard.

      SELECT SINGLE *
        FROM zfit0150 INTO @data(_wl_zfit0150)
       WHERE class_aval EQ @tg_zsati020-iiv_classe_aval
         AND area_aval  EQ @tg_zsati020-iiv_area_aval
         AND lgort      EQ @tg_mard-lgort.

      CHECK ( sy-subrc EQ 0 ) AND ( _wl_zfit0150-parid IS NOT INITIAL ).

      IF tg_zsati020-iiv_cod_parceiro NE _wl_zfit0150-parid.
        tg_zsati020-iiv_cod_parceiro = _wl_zfit0150-parid.
      ENDIF.

      IF tg_zsati020-iiv_cod_parceiro IS NOT INITIAL.
        tg_zsati020-iiv_prop_posse = '1'.
      ENDIF.

      EXIT.
    ENDLOOP.


    MODIFY /pwsati/zsati020 FROM tg_zsati020.

  ENDLOOP.

ENDFORM.                    "F_EXIT_ZYLFINV021
