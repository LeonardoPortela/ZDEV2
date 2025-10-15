*&---------------------------------------------------------------------*
*& Report  Z_SD_SALES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_sd_sales.

TYPES: BEGIN OF ty_setleaf        ,
         operacao TYPE zfiwrt0008-operacao,
       END  OF ty_setleaf         ,

       BEGIN OF ty_zsales_siacorp,
         codclientesap         TYPE zsales_siacorp-codclientesap,
         codmes                TYPE c LENGTH 2,
         codano                TYPE c LENGTH 4,
         st_adto               TYPE c LENGTH 1,
         valvendas_moeda_int   TYPE zsales_siacorp-valvendas_moeda_int,
         valvendas_moeda_forte TYPE zsales_siacorp-valvendas_moeda_forte,
         dt_referencia         TYPE zsales_siacorp-dt_referencia,
         empresa               TYPE bukrs,
       END   OF ty_zsales_siacorp.

*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*
DATA: it_sales_siacorp     TYPE TABLE OF zsales_siacorp,
      it_sales_siacorp_aux TYPE TABLE OF ty_zsales_siacorp,
      it_setleaf           TYPE TABLE OF setleaf,
      it_setleaf_aux       TYPE TABLE OF ty_setleaf,
      it_bsid              TYPE TABLE OF bsid,
      it_bsad              TYPE TABLE OF bsad,
      it_bkpf_aux          TYPE TABLE OF bkpf,
      it_vbfa              TYPE TABLE OF vbfa,
      it_vbak              TYPE TABLE OF vbak,
      it_zfiwrt0008        TYPE TABLE OF zfiwrt0008,
      it_zib_contabil_chv  TYPE TABLE OF zib_contabil_chv,
      it_bsad_ztro         TYPE TABLE OF bsad,
      it_bsid_ztro         TYPE TABLE OF bsid.

*&---------------------------------------------------------------------*
*& Work Areas
*&---------------------------------------------------------------------*
DATA: wa_sales_siacorp     TYPE zsales_siacorp,
      wa_sales_siacorp_aux TYPE ty_zsales_siacorp,
      wa_setleaf           TYPE setleaf,
      wa_setleaf_aux       TYPE ty_setleaf,
      wa_bsid              TYPE bsid,
      wa_bsad              TYPE bsad,
      wa_bsad_ztro         TYPE bsad,
      wa_bsid_ztro         TYPE bsid,
      wa_bkpf_aux          TYPE bkpf,
      wa_vbfa              TYPE vbfa,
      wa_vbak              TYPE vbak,
      vl_teste             TYPE c LENGTH 1.


*-----------------------------------------------------------
START-OF-SELECTION.
*-----------------------------------------------------------
  REFRESH: it_sales_siacorp.


  CLEAR vl_teste.


  PERFORM : f_seleciona_dados,
            f_saida_sales .

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_SD_OUTBOUND_SALES' IN BACKGROUND TASK
*    DESTINATION 'XI_SALES_SIACORP'
*    TABLES
*      t_zsales_siacorp = it_sales_siacorp[].

  DATA lv_rfc TYPE rfcdest.

  CONSTANTS c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_SALES'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        t_zsales_siacorp = it_sales_siacorp[].
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        t_zsales_siacorp = it_sales_siacorp[].
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  COMMIT WORK.

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.
  DATA : p_dt_inicio TYPE vbak-erdat,
         p_dt_fim    TYPE vbak-erdat,
         vg_tabix    TYPE sy-tabix.

  CLEAR: p_dt_inicio.

  CONCATENATE  sy-datum(4) '0101' INTO p_dt_inicio.

  p_dt_fim = sy-datum.

  "Bsid

  SELECT *
    FROM   bsid
    INTO TABLE it_bsid
   WHERE budat >= p_dt_inicio
     AND budat <= p_dt_fim.

  DELETE it_bsid WHERE ( blart NE 'RV' AND blart NE 'SI' AND umsks IS INITIAL ).

  DELETE it_bsid WHERE ( blart EQ 'VC' ).

  DELETE it_bsid WHERE ( dmbe2 EQ '0.01' ).

  DELETE it_bsid WHERE ( umskz EQ 'F' ).

  IF it_bsid IS NOT INITIAL.

    CLEAR: it_bkpf_aux.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsid
     WHERE bukrs EQ it_bsid-bukrs
       AND belnr EQ it_bsid-belnr
       AND gjahr EQ it_bsid-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.

    IF it_bkpf_aux IS NOT INITIAL.

      CLEAR: wa_bkpf_aux.
      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsid INTO wa_bsid.

        vg_tabix = sy-tabix.

        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsid-bukrs belnr = wa_bsid-belnr BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          CLEAR: wa_bsid-belnr.
          MODIFY it_bsid INDEX vg_tabix FROM wa_bsid TRANSPORTING belnr.

        ENDIF.

      ENDLOOP.

      DELETE it_bsid WHERE belnr EQ space.

      CLEAR: wa_bsid.

    ENDIF.

  ENDIF.

  "-----------------------Bsad

  SELECT *
    FROM bsad
    INTO TABLE it_bsad
   WHERE budat >= p_dt_inicio
     AND budat <= p_dt_fim.

  DELETE it_bsad WHERE blart NE 'RV' AND blart NE 'SI'.

  IF it_bsad IS NOT INITIAL.

    CLEAR: it_bkpf_aux.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsad
     WHERE bukrs EQ it_bsad-bukrs
       AND belnr EQ it_bsad-belnr
       AND gjahr EQ it_bsad-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.


    IF it_bkpf_aux IS NOT INITIAL.

      CLEAR: wa_bkpf_aux.

      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsad INTO wa_bsad.

        vg_tabix = sy-tabix.

        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsad-bukrs belnr = wa_bsad-belnr BINARY SEARCH  .

        IF sy-subrc IS INITIAL.

          CLEAR: wa_bsad-belnr.
          MODIFY it_bsad INDEX vg_tabix FROM wa_bsad TRANSPORTING belnr.

        ENDIF.

      ENDLOOP.

      DELETE it_bsad  WHERE belnr EQ space.

      CLEAR: wa_bsad.

    ENDIF.
  ENDIF.


  " Faturas ztro
  SELECT *
    INTO TABLE it_vbfa
    FROM vbfa
   WHERE erdat >= p_dt_inicio
     AND erdat <= p_dt_fim.

  SELECT *
    INTO TABLE it_vbak
    FROM vbak
     FOR ALL ENTRIES IN it_vbfa
   WHERE vbeln  = it_vbfa-vbelv
     AND auart  = 'ZTRO'.

  SELECT *
    INTO TABLE it_setleaf
    FROM setleaf
   WHERE setname = 'MAGGI_GENF_SIACORP' .

  LOOP AT it_setleaf INTO wa_setleaf .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_setleaf-valfrom
      IMPORTING
        output = wa_setleaf_aux-operacao.

    APPEND wa_setleaf_aux TO it_setleaf_aux.

  ENDLOOP.

  SELECT *
    INTO TABLE it_zfiwrt0008
    FROM zfiwrt0008
     FOR ALL ENTRIES IN it_setleaf_aux
   WHERE operacao	=	it_setleaf_aux-operacao.

  SELECT *
    INTO TABLE it_zib_contabil_chv
    FROM zib_contabil_chv
     FOR ALL ENTRIES IN it_zfiwrt0008
   WHERE obj_key = it_zfiwrt0008-obj_key.

  SELECT *
    INTO TABLE it_bsad_ztro
    FROM bsad
     FOR ALL ENTRIES IN it_zib_contabil_chv
   WHERE bukrs  = it_zib_contabil_chv-bukrs
     AND belnr  = it_zib_contabil_chv-belnr
     AND gjahr  = it_zib_contabil_chv-gjahr
     AND budat >= p_dt_inicio
     AND budat <= p_dt_fim.

  SELECT *
    INTO TABLE it_bsid_ztro
    FROM bsid
     FOR ALL ENTRIES IN it_zib_contabil_chv
   WHERE bukrs = it_zib_contabil_chv-bukrs
     AND belnr = it_zib_contabil_chv-belnr
     AND gjahr = it_zib_contabil_chv-gjahr
     AND budat >= p_dt_inicio
     AND budat <= p_dt_fim.

ENDFORM.                    "sel


*&---------------------------------------------------------------------*
*&      Form  f_saida_sales
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_saida_sales .

  DATA: v_mes TYPE i,
        v_ano TYPE i.

  SORT: it_bsid       BY kunnr budat,
        it_bsad       BY kunnr budat,
        it_bsid_ztro  BY kunnr budat,
        it_bsad_ztro  BY kunnr budat.

  "bsad RV
  LOOP AT it_bsad INTO wa_bsad.

    CLEAR: wa_sales_siacorp_aux, v_mes, v_ano .

    v_mes   = wa_bsad-budat+4(2).
    v_ano   = wa_bsad-budat(4).

    wa_sales_siacorp_aux-empresa       = wa_bsad-bukrs.
    wa_sales_siacorp_aux-codclientesap = wa_bsad-kunnr.
    wa_sales_siacorp_aux-codmes        = v_mes.
    wa_sales_siacorp_aux-codano        = v_ano.
    wa_sales_siacorp_aux-dt_referencia = sy-datum.

    IF wa_bsad-shkzg = 'H'.
      wa_sales_siacorp_aux-valvendas_moeda_int   = ( wa_bsad-dmbtr * -1 ).
      wa_sales_siacorp_aux-valvendas_moeda_forte = ( wa_bsad-dmbe2 * -1 ).
    ELSE.
      wa_sales_siacorp_aux-valvendas_moeda_int   = wa_bsad-dmbtr.
      wa_sales_siacorp_aux-valvendas_moeda_forte = wa_bsad-dmbe2.
    ENDIF.

    COLLECT wa_sales_siacorp_aux INTO it_sales_siacorp_aux.

  ENDLOOP.

  "bsid RV
  LOOP AT it_bsid INTO wa_bsid.

    CLEAR: wa_sales_siacorp_aux, v_mes, v_ano .

    v_mes   = wa_bsid-budat+4(2).
    v_ano   = wa_bsid-budat(4).

    wa_sales_siacorp_aux-empresa       = wa_bsid-bukrs.
    wa_sales_siacorp_aux-codclientesap = wa_bsid-kunnr.
    wa_sales_siacorp_aux-codmes        = v_mes.
    wa_sales_siacorp_aux-codano        = v_ano.
    wa_sales_siacorp_aux-dt_referencia = sy-datum.

    IF wa_bsid-shkzg = 'H'.
      wa_sales_siacorp_aux-valvendas_moeda_int   = ( wa_bsid-dmbtr * -1 ).
      wa_sales_siacorp_aux-valvendas_moeda_forte = ( wa_bsid-dmbe2 * -1 ).
    ELSE.
      wa_sales_siacorp_aux-valvendas_moeda_int   = wa_bsid-dmbtr.
      wa_sales_siacorp_aux-valvendas_moeda_forte = wa_bsid-dmbe2.
    ENDIF.

    IF wa_bsid-umsks IS NOT INITIAL.
      wa_sales_siacorp_aux-st_adto := 'X'.
    ENDIF.

    COLLECT wa_sales_siacorp_aux INTO it_sales_siacorp_aux.

  ENDLOOP.

  "bsad ZTRO
  LOOP AT it_bsad_ztro INTO wa_bsad_ztro.

    CLEAR: wa_sales_siacorp_aux, v_mes, v_ano .

    v_mes   = wa_bsad_ztro-budat+4(2).
    v_ano   = wa_bsad_ztro-budat(4).

    wa_sales_siacorp_aux-empresa       = wa_bsad_ztro-bukrs.
    wa_sales_siacorp_aux-codclientesap = wa_bsad_ztro-kunnr.
    wa_sales_siacorp_aux-codmes        = v_mes.
    wa_sales_siacorp_aux-codano        = v_ano.
    wa_sales_siacorp_aux-dt_referencia = sy-datum.

    IF wa_bsad_ztro-shkzg = 'H'.
      wa_sales_siacorp_aux-valvendas_moeda_int   = ( wa_bsad_ztro-dmbtr * -1 ).
      wa_sales_siacorp_aux-valvendas_moeda_forte = ( wa_bsad_ztro-dmbe2 * -1 ).
    ELSE.
      wa_sales_siacorp_aux-valvendas_moeda_int   = wa_bsad_ztro-dmbtr.
      wa_sales_siacorp_aux-valvendas_moeda_forte = wa_bsad_ztro-dmbe2.
    ENDIF.

    COLLECT wa_sales_siacorp_aux INTO it_sales_siacorp_aux.

  ENDLOOP.

  "bsid ZTRO
  LOOP AT it_bsid_ztro INTO wa_bsid_ztro.

    CLEAR: wa_sales_siacorp_aux, v_mes, v_ano .

    v_mes   = wa_bsid_ztro-budat+4(2).
    v_ano   = wa_bsid_ztro-budat(4).

    wa_sales_siacorp_aux-empresa       = wa_bsid_ztro-bukrs.
    wa_sales_siacorp_aux-codclientesap = wa_bsid_ztro-kunnr.
    wa_sales_siacorp_aux-codmes        = v_mes.
    wa_sales_siacorp_aux-codano        = v_ano.
    wa_sales_siacorp_aux-dt_referencia = sy-datum.

    IF wa_bsid_ztro-shkzg = 'H'.
      wa_sales_siacorp_aux-valvendas_moeda_int   = ( wa_bsid_ztro-dmbtr * -1 ).
      wa_sales_siacorp_aux-valvendas_moeda_forte = ( wa_bsid_ztro-dmbe2 * -1 ).
    ELSE.
      wa_sales_siacorp_aux-valvendas_moeda_int   = wa_bsid_ztro-dmbtr.
      wa_sales_siacorp_aux-valvendas_moeda_forte = wa_bsid_ztro-dmbe2.
    ENDIF.

    COLLECT wa_sales_siacorp_aux INTO it_sales_siacorp_aux.

  ENDLOOP.

  SORT it_sales_siacorp_aux BY codclientesap codmes codano.

  LOOP AT it_sales_siacorp_aux INTO wa_sales_siacorp_aux.

    wa_sales_siacorp-empresa               =  wa_sales_siacorp_aux-empresa.
    wa_sales_siacorp-codclientesap         =  wa_sales_siacorp_aux-codclientesap.
    wa_sales_siacorp-codmes                =  wa_sales_siacorp_aux-codmes.
    wa_sales_siacorp-codano                =  wa_sales_siacorp_aux-codano.
    wa_sales_siacorp-dt_referencia         =  wa_sales_siacorp_aux-dt_referencia.
    wa_sales_siacorp-valvendas_moeda_int   =  wa_sales_siacorp_aux-valvendas_moeda_int.
    wa_sales_siacorp-valvendas_moeda_forte =  wa_sales_siacorp_aux-valvendas_moeda_forte.
    wa_sales_siacorp-st_adto               =  wa_sales_siacorp_aux-st_adto.

    APPEND wa_sales_siacorp TO it_sales_siacorp.

  ENDLOOP.

ENDFORM.                    " F_SAIDA_SALES
